#!/usr/bin/env Rscript

# ============================================================
# 08_build_death_cause_final.R
# ------------------------------------------------------------
# Objetivo:
#   Construir death_cause_final a partir de:
#   - death_cause_leaf_post_redistribution
#   - cause_master
#   - population_result (fuente externa)
#   - life_table_mortality_single_age (fuente externa)
#
# Enfoque pragmático revisado:
#   1) El factor de completitud/subreporte se estima SOLO con
#      referencia prepandemia (2018-2019).
#   2) Ese factor se arrastra a 2020-2024 para no confundir
#      exceso pandémico con subregistro.
#   3) El exceso pandémico se modela como componente separado y
#      se asigna exclusivamente a causas is_covid_related == TRUE.
#   4) Se endurece el control de duplicados y trazabilidad.
#   5) Se armoniza geografía fina de mortalidad a departamento
#      antes del merge con population_result y
#      life_table_mortality_single_age.
#
# Nota semántica importante:
#   - deaths_observed en este MVP es equivalente a
#     deaths_post_redistribution, porque este script parte de la
#     tabla canónica post-06.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "pandemic_utils.R"))
source(here("R", "spec_utils.R"))

leaf_input_override <- Sys.getenv("DCF_LEAF_INPUT_PATH", unset = "")
output_suffix <- Sys.getenv("DCF_OUTPUT_SUFFIX", unset = "")
output_suffix_path <- if (nzchar(output_suffix)) paste0("_", output_suffix) else ""

CFG <- list(
  version = "v0.4.1_pandemic_2020_2022_and_window_qc",
  dataset_id = "death_cause_final",
  table_name = "death_cause_final",
  
  years = 2018:2024,
  prepandemic_years = 2018:2019,
  pandemic_year_min = 2020L,
  pandemic_year_max = 2022L,
  
  age_min = 0L,
  age_max = 110L,
  
  min_completeness_factor = 1.0,
  max_completeness_factor = 3.0,
  min_observed_allcause_for_direct_ratio = 10,
  
  max_allowed_missing_external_prepandemic_prop = 0.005,
  hard_fail_on_missing_external_prepandemic = TRUE,
  
  verbose = TRUE,
  
  out_dir = here("data", "final", paste0("death_cause_final", output_suffix_path)),
  qc_dir  = qc_dir_path(paste0("build_death_cause_final", output_suffix_path)),
  
  leaf_candidates = c(
    if (nzchar(leaf_input_override)) leaf_input_override,
    here("data", "final", "death_cause_leaf_post_redistribution", "death_cause_leaf_post_redistribution.parquet"),
    here("data", "final", "death_cause_leaf_post_redistribution", "death_cause_leaf_post_redistribution.csv")
  ),
  
  cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.csv"),
    here("data", "final", "cause_master", "cause_master.parquet")
  ),
  
  spec_final_path = here("config", "spec_death_cause_final.yml"),
  external_yaml_path = here("config", "external_sources.yml")
)

for (d in c(CFG$out_dir, CFG$qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

write_qc <- function(dt, filename) {
  fwrite(dt, file.path(CFG$qc_dir, filename))
}

ensure_project_dirs()
ensure_catalog_files()

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

find_file_recursive <- function(root, pattern) {
  fs <- list.files(root, pattern = pattern, recursive = TRUE, full.names = TRUE)
  if (length(fs) == 0L) return(NA_character_)
  fs[1]
}

resolve_external_path <- function(external_yaml, key) {
  ys <- yaml::read_yaml(external_yaml)
  if (is.null(ys$external_datasets[[key]]$path)) {
    stop("No se encontró path para external dataset: ", key)
  }
  rel <- ys$external_datasets[[key]]$path
  normalizePath(file.path(here(), rel), winslash = "/", mustWork = FALSE)
}

detect_col <- function(dt, candidates, label) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    stop("No se encontró columna para ", label, ". Candidatas: ",
         paste(candidates, collapse = ", "))
  }
  hit[1]
}

safe_nonneg <- function(x) pmax(0, as.numeric(x))

safe_ratio <- function(num, den) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- num[ok] / den[ok]
  out
}

clip <- function(x, lower, upper) {
  pmin(pmax(x, lower), upper)
}

is_valid_external_cell <- function(population, mx, expected_allcause) {
  !is.na(population) & population > 0 &
    !is.na(mx) & mx >= 0 &
    !is.na(expected_allcause) & expected_allcause >= 0
}

harmonize_location_to_department <- function(location_id) {
  x <- sprintf("%06d", as.integer(location_id))
  as.integer(substr(x, 1, 2))
}

build_dictionary_ext <- function(dt) {
  data.table(
    variable = names(dt),
    tipo = vapply(dt, function(x) class(x)[1], character(1)),
    n = nrow(dt),
    n_missing = vapply(dt, function(x) sum(is.na(x)), integer(1)),
    n_distinct = vapply(dt, function(x) uniqueN(x), integer(1)),
    example_values = vapply(dt, function(x) {
      vals <- unique(na.omit(as.character(x)))
      paste(head(vals, 5), collapse = " | ")
    }, character(1))
  )
}

fill_factor_hierarchical <- function(fac_base) {
  x <- copy(fac_base)
  
  x[, external_status := fifelse(
    is.na(population), "missing_population",
    fifelse(is.na(mx), "missing_mx",
            fifelse(is.na(expected_allcause), "missing_expected",
                    fifelse(population <= 0 & expected_allcause <= 0, "structural_zero_population",
                            fifelse(population <= 0, "invalid_population_nonpositive",
                                    "ok"))))
  )]
  
  x[, direct_ratio_raw := safe_ratio(expected_allcause, observed_allcause)]
  x[, direct_ratio_eligible := !is.na(direct_ratio_raw) &
      is.finite(direct_ratio_raw) &
      observed_allcause >= CFG$min_observed_allcause_for_direct_ratio]
  x[, direct_ratio := fifelse(direct_ratio_eligible, direct_ratio_raw, NA_real_)]
  
  pre <- x[year_id %in% CFG$prepandemic_years]
  
  pre_loc <- pre[, .(
    factor_loc = if (all(is.na(direct_ratio))) NA_real_ else median(direct_ratio, na.rm = TRUE),
    n_years_loc = sum(!is.na(direct_ratio))
  ), by = .(location_id, sex_id, age)]
  
  pre_sex_age <- pre[, .(
    factor_sex_age = if (all(is.na(direct_ratio))) NA_real_ else median(direct_ratio, na.rm = TRUE),
    n_years_sex_age = sum(!is.na(direct_ratio))
  ), by = .(sex_id, age)]
  
  pre_age <- pre[, .(
    factor_age = if (all(is.na(direct_ratio))) NA_real_ else median(direct_ratio, na.rm = TRUE),
    n_years_age = sum(!is.na(direct_ratio))
  ), by = .(age)]
  
  x <- merge(x, pre_loc, by = c("location_id", "sex_id", "age"), all.x = TRUE, sort = FALSE)
  x <- merge(x, pre_sex_age, by = c("sex_id", "age"), all.x = TRUE, sort = FALSE)
  x <- merge(x, pre_age, by = "age", all.x = TRUE, sort = FALSE)
  
  x[, factor_method := fifelse(
    year_id %in% CFG$prepandemic_years & !is.na(direct_ratio), "direct_prepandemic",
    fifelse(!is.na(factor_loc), "backoff_loc_sex_age",
            fifelse(!is.na(factor_sex_age), "backoff_sex_age",
                    fifelse(!is.na(factor_age), "backoff_age", "fallback_1")))
  )]
  
  x[, correction_factor_preclip := fifelse(
    year_id %in% CFG$prepandemic_years & !is.na(direct_ratio), direct_ratio,
    fifelse(!is.na(factor_loc), factor_loc,
            fifelse(!is.na(factor_sex_age), factor_sex_age,
                    fifelse(!is.na(factor_age), factor_age, 1.0)))
  )]
  
  x[!is.finite(correction_factor_preclip), correction_factor_preclip := 1.0]
  x[, factor_truncated_low := correction_factor_preclip < CFG$min_completeness_factor]
  x[, factor_truncated_high := correction_factor_preclip > CFG$max_completeness_factor]
  x[, correction_factor_completeness := clip(
    correction_factor_preclip,
    CFG$min_completeness_factor,
    CFG$max_completeness_factor
  )]
  x[, factor_from_missing_external := !external_status %in% c("ok", "structural_zero_population")]
  
  x[]
}

assert_unique_factor_within_pk <- function(final_dt) {
  dup_factor <- final_dt[
    ,
    .(
      n_factor_non_na = uniqueN(correction_factor_completeness[!is.na(correction_factor_completeness)]),
      factor_values = paste(sort(unique(round(correction_factor_completeness[!is.na(correction_factor_completeness)], 10))), collapse = " | "),
      n_rows = .N
    ),
    by = .(year_id, location_id, sex_id, age, cause_concept_id)
  ][n_factor_non_na > 1]
  
  if (nrow(dup_factor) > 0L) {
    write_qc(dup_factor, "qc_pk_multiple_factors.csv")
    stop(
      "QC HARD FAIL: existen PK con más de un correction_factor_completeness distinto. ",
      "Revisar qc_pk_multiple_factors.csv"
    )
  }
  
  invisible(TRUE)
}

tryCatch({
  
  msg("Resolviendo inputs.")
  
  leaf_path  <- first_existing(CFG$leaf_candidates)
  cause_path <- first_existing(CFG$cause_candidates)
  
  if (is.na(leaf_path)) {
    leaf_path <- find_file_recursive(
      here("data"),
      "^death_cause_leaf_post_redistribution\\.(csv|parquet)$"
    )
  }
  
  if (is.na(cause_path)) {
    cause_path <- find_file_recursive(
      here("data"),
      "^cause_master\\.(csv|parquet)$"
    )
  }
  
  if (is.na(leaf_path)) stop("No encontré death_cause_leaf_post_redistribution.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  if (!file.exists(CFG$external_yaml_path)) stop("No existe external_sources.yml")
  if (!file.exists(CFG$spec_final_path)) stop("No existe spec_death_cause_final.yml")
  
  pop_path <- resolve_external_dataset_path(
    key = "population_result",
    external_yaml_path = CFG$external_yaml_path,
    must_work = FALSE
  )
  ltm_path <- resolve_external_dataset_path(
    key = "life_table_mortality_single_age",
    external_yaml_path = CFG$external_yaml_path,
    must_work = FALSE
  )
  
  if (!path_exists_with_retry(pop_path, retries = 5L, wait_sec = 2)) stop("No existe population_result en ruta externa: ", pop_path)
  if (!path_exists_with_retry(ltm_path, retries = 5L, wait_sec = 2)) stop("No existe life_table_mortality_single_age en ruta externa: ", ltm_path)
  
  msg("Leyendo leaf post-redistribution.")
  leaf <- as.data.table(read_auto(leaf_path))
  
  msg("Leyendo cause_master.")
  cm <- as.data.table(read_auto(cause_path))
  
  msg("Leyendo population_result.")
  pop <- as.data.table(read_auto(pop_path))
  
  msg("Leyendo life_table_mortality_single_age.")
  ltm <- as.data.table(read_auto(ltm_path))
  
  # ----------------------------------------------------------
  # Estandarización básica
  # ----------------------------------------------------------
  req_leaf <- c("year_id", "location_id", "sex_id", "age", "cause_term_concept_id", "deaths")
  miss_leaf <- setdiff(req_leaf, names(leaf))
  if (length(miss_leaf) > 0L) {
    stop("Faltan columnas en leaf: ", paste(miss_leaf, collapse = ", "))
  }
  
  req_cm <- c("cause_concept_id", "cause_level", "cause_name", "is_terminal", "is_covid_related")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  pop_col <- detect_col(pop, c("population"), "population")
  ltm_age_col  <- detect_col(ltm, c("age_start", "age", "exact_age"), "edad tabla mortalidad")
  ltm_rate_col <- detect_col(ltm, c("mx", "mortality_rate", "mx_interp", "mx_adjusted"), "tasa mortalidad tabla")
  ltm_loc_col  <- detect_col(ltm, c("location_id"), "location_id tabla mortalidad")
  ltm_year_col <- detect_col(ltm, c("year_id"), "year_id tabla mortalidad")
  ltm_sex_col  <- detect_col(ltm, c("sex_id"), "sex_id tabla mortalidad")
  
  leaf <- copy(leaf)[
    ,
    .(
      year_id = as.integer(year_id),
      location_id_input = as.integer(location_id),
      sex_id = as.integer(sex_id),
      age = as.integer(age),
      cause_concept_id = as.integer(cause_term_concept_id),
      deaths_post_redistribution = as.numeric(deaths)
    )
  ][
    year_id %in% CFG$years &
      age >= CFG$age_min &
      age <= CFG$age_max
  ]
  
  leaf[is.na(deaths_post_redistribution), deaths_post_redistribution := 0]
  leaf[deaths_post_redistribution < 0, deaths_post_redistribution := 0]

  qc_unresolved_leaf_negative_ids <- leaf[
    cause_concept_id <= 0 & deaths_post_redistribution > 0,
    .(
      deaths_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
      n_rows = .N
    ),
    by = .(year_id, cause_concept_id)
  ][order(year_id, cause_concept_id)]
  write_qc(qc_unresolved_leaf_negative_ids, "qc_unresolved_leaf_negative_ids.csv")

  if (nrow(qc_unresolved_leaf_negative_ids) > 0L) {
    stop(
      "QC HARD FAIL: leaf post-redistribution contiene cause_concept_id <= 0 con muertes > 0. ",
      "Esto indica masa no resuelta upstream (por ejemplo bucket -2) y no puede excluirse silenciosamente en 08. ",
      "Revisar qc_unresolved_leaf_negative_ids.csv y remediar FASE 5 antes de continuar."
    )
  }
  
  leaf[, location_id_chr6 := sprintf("%06d", location_id_input)]
  leaf[, location_id := harmonize_location_to_department(location_id_input)]
  leaf[, deaths_observed := deaths_post_redistribution]
  leaf[, deaths_input_semantic := "post_06_post_redistribution_equals_current_input_observed"]
  leaf[, location_id_semantic := "harmonized_to_department_from_fine_geo_input"]
  
  cm <- cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    is_terminal = as.logical(is_terminal),
    is_covid_related = as.logical(is_covid_related),
    is_covid_specific = if ("is_covid_specific" %in% names(cm)) as.logical(is_covid_specific) else FALSE,
    is_oprm = if ("is_oprm" %in% names(cm)) as.logical(is_oprm) else FALSE,
    target_age_start_default = if ("target_age_start_default" %in% names(cm)) {
      as.integer(target_age_start_default)
    } else NA_integer_,
    target_age_end_default = if ("target_age_end_default" %in% names(cm)) {
      as.integer(target_age_end_default)
    } else NA_integer_,
    sex_restriction_target_default = if ("sex_restriction_target_default" %in% names(cm)) {
      as.character(sex_restriction_target_default)
      } else NA_character_
  )]
  cm <- add_pandemic_component_flags(cm)
  
  pop <- pop[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    population = as.numeric(get(pop_col))
  )][year_id %in% CFG$years]
  
  ltm <- ltm[, .(
    year_id = as.integer(get(ltm_year_col)),
    location_id = as.integer(get(ltm_loc_col)),
    sex_id = as.integer(get(ltm_sex_col)),
    age = as.integer(get(ltm_age_col)),
    mx = as.numeric(get(ltm_rate_col))
  )][year_id %in% CFG$years]
  
  pop[population < 0, population := NA_real_]
  ltm[mx < 0, mx := NA_real_]
  
  cm_term <- cm[is_terminal == TRUE & cause_concept_id > 0]
  write_qc(
    cm_term[pandemic_component_class %in% c("covid_specific", "measles", "lri", "pertussis", "oprm"),
      .(
        cause_concept_id,
        cause_name,
        pandemic_component_class,
        is_pandemic_named_component,
        is_covid_specific,
        is_oprm
      )
    ][order(match(pandemic_component_class, c("covid_specific", "measles", "lri", "pertussis", "oprm")), cause_name)],
    "qc_pandemic_component_catalog.csv"
  )
  leaf <- leaf[cause_concept_id > 0]

  leaf <- merge(
    leaf,
    cm_term[, .(
      cause_concept_id,
      is_covid_related,
      is_covid_specific,
      is_oprm,
      pandemic_component_class,
      is_pandemic_named_component,
      is_pandemic_related_any
    )],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )

  leaf[is.na(is_covid_related), is_covid_related := FALSE]
  leaf[is.na(is_covid_specific), is_covid_specific := FALSE]
  leaf[is.na(is_oprm), is_oprm := FALSE]
  leaf[is.na(pandemic_component_class), pandemic_component_class := "non_pandemic"]
  leaf[is.na(is_pandemic_named_component), is_pandemic_named_component := FALSE]
  leaf[is.na(is_pandemic_related_any), is_pandemic_related_any := FALSE]
  
  # ----------------------------------------------------------
  # Armonización geográfica a departamento y re-agregación
  # ----------------------------------------------------------
  leaf <- leaf[
    ,
    .(
        deaths_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
        deaths_observed = sum(deaths_observed, na.rm = TRUE),
        deaths_input_semantic = unique(deaths_input_semantic)[1],
        location_id_semantic = unique(location_id_semantic)[1],
        is_covid_related = unique(is_covid_related)[1],
        is_covid_specific = unique(is_covid_specific)[1],
        is_oprm = unique(is_oprm)[1],
        pandemic_component_class = unique(pandemic_component_class)[1],
        is_pandemic_named_component = unique(is_pandemic_named_component)[1],
        is_pandemic_related_any = unique(is_pandemic_related_any)[1]
      ),
      by = .(year_id, location_id, sex_id, age, cause_concept_id)
    ]
  
  qc_geo_harmonization <- leaf[
    ,
    .(
      n_rows = .N,
      total_deaths_post = sum(deaths_post_redistribution, na.rm = TRUE)
    ),
    by = .(year_id, location_id)
  ][order(year_id, location_id)]
  
  qc_geo_domain <- leaf[, .(
    n_loc = uniqueN(location_id),
    min_loc = min(location_id, na.rm = TRUE),
    max_loc = max(location_id, na.rm = TRUE)
  )]
  
  write_qc(qc_geo_harmonization, "qc_geo_harmonization.csv")
  write_qc(qc_geo_domain, "qc_geo_domain.csv")
  
  if (!all(sort(unique(leaf$location_id)) %in% 1:25)) {
    write_qc(leaf[, .N, by = location_id][order(location_id)], "qc_invalid_department_ids_after_harmonization.csv")
    stop("QC HARD FAIL: location_id armonizado fuera del dominio 1:25.")
  }
  
  # ----------------------------------------------------------
  # All-cause observado y esperado
  # ----------------------------------------------------------
  msg("Construyendo all-cause observado y esperado...")
  
  obs_allcause <- leaf[
    ,
    .(observed_allcause = sum(deaths_post_redistribution, na.rm = TRUE)),
    by = .(year_id, location_id, sex_id, age)
  ]
  
  exp_allcause <- merge(
    pop,
    ltm,
    by = c("year_id", "location_id", "sex_id", "age"),
    all = FALSE
  )
  
  exp_allcause[, expected_allcause := population * mx]
  exp_allcause <- exp_allcause[, .(
    year_id, location_id, sex_id, age, population, mx, expected_allcause
  )]
  
  fac_base <- merge(
    exp_allcause,
    obs_allcause,
    by = c("year_id", "location_id", "sex_id", "age"),
    all = TRUE,
    sort = FALSE
  )
  
  fac_base[, expected_allcause := as.numeric(expected_allcause)]
  fac_base[, observed_allcause := as.numeric(observed_allcause)]
  fac_base[, valid_external_cell := is_valid_external_cell(population, mx, expected_allcause)]
  
  fac_base <- fill_factor_hierarchical(fac_base)
  fac_base[, observed_corrected_allcause := observed_allcause * correction_factor_completeness]
  
  # ----------------------------------------------------------
  # QC temprano de faltantes externos
  # ----------------------------------------------------------
  qc_missing_external_summary <- fac_base[, .(
    n_cells = .N,
    n_missing_external = sum(!external_status %in% c("ok", "structural_zero_population"), na.rm = TRUE),
    n_structural_zero_population = sum(external_status == "structural_zero_population", na.rm = TRUE),
    prop_missing_external = mean(!external_status %in% c("ok", "structural_zero_population"), na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]
  
  qc_missing_external_examples <- fac_base[
    external_status != "ok",
    .(year_id, location_id, sex_id, age, population, mx, expected_allcause, observed_allcause, external_status)
  ][order(year_id, location_id, sex_id, age)]
  
  write_qc(qc_missing_external_summary, "qc_missing_external_summary.csv")
  write_qc(qc_missing_external_examples, "qc_missing_external_examples.csv")
  
  prep_missing_prop <- fac_base[
    year_id %in% CFG$prepandemic_years,
    mean(!external_status %in% c("ok", "structural_zero_population"), na.rm = TRUE)
  ]
  
  if (isTRUE(CFG$hard_fail_on_missing_external_prepandemic) &&
      is.finite(prep_missing_prop) &&
      prep_missing_prop > CFG$max_allowed_missing_external_prepandemic_prop) {
    stop(
      "QC HARD FAIL: proporción de faltantes externos en prepandemia supera el umbral. ",
      "Revisar qc_missing_external_summary.csv y qc_missing_external_examples.csv"
    )
  }
  
  # ----------------------------------------------------------
  # Exceso pandémico
  # ----------------------------------------------------------
  msg("Calculando exceso pandémico...")
  
  fac_base[, pandemic_excess_allcause := fifelse(
    year_id >= CFG$pandemic_year_min &
      year_id <= CFG$pandemic_year_max &
      !is.na(observed_corrected_allcause) &
      !is.na(expected_allcause),
    pmax(0, observed_corrected_allcause - expected_allcause),
    0
  )]

  oprm_target_id <- cm_term[pandemic_component_class == "oprm", cause_concept_id][1]
  if (length(oprm_target_id) == 0L || is.na(oprm_target_id)) {
    stop("QC HARD FAIL: no existe una causa terminal OPRM para alojar la reasignacion pandemica.")
  }

  pandemic_target_presence <- unique(
    leaf[, .(year_id, location_id, sex_id, age, cause_concept_id, pandemic_component_class)]
  )[
    ,
    .(n_oprm_targets_present = sum(pandemic_component_class == "oprm", na.rm = TRUE)),
    by = .(year_id, location_id, sex_id, age)
  ]

  synthetic_pandemic_target_cells <- merge(
    fac_base[
      pandemic_excess_allcause > 0,
      .(year_id, location_id, sex_id, age, pandemic_excess_allcause)
    ],
    pandemic_target_presence,
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  synthetic_pandemic_target_cells[is.na(n_oprm_targets_present), n_oprm_targets_present := 0L]
  synthetic_pandemic_target_cells <- synthetic_pandemic_target_cells[
    n_oprm_targets_present <= 0
  ][
    ,
    .(
      year_id, location_id, sex_id, age,
      cause_concept_id = as.integer(oprm_target_id),
      deaths_post_redistribution = 0,
      deaths_observed = 0,
      deaths_input_semantic = "synthetic_oprm_target_zero_base",
      location_id_semantic = "harmonized_to_department_from_fine_geo_input",
      is_covid_related = TRUE,
      is_covid_specific = FALSE,
      is_oprm = TRUE,
      pandemic_component_class = "oprm",
      is_pandemic_named_component = FALSE,
      is_pandemic_related_any = TRUE,
      pandemic_excess_allcause
    )
  ]
  write_qc(
    synthetic_pandemic_target_cells[
      ,
      .(
        n_cells = .N,
        total_pandemic_excess = sum(pandemic_excess_allcause, na.rm = TRUE)
      ),
      by = .(year_id, cause_concept_id)
    ][order(year_id, cause_concept_id)],
    "qc_pandemic_synthetic_target_cells.csv"
  )
  leaf <- rbindlist(
    list(
      leaf,
      synthetic_pandemic_target_cells[, !c("pandemic_excess_allcause")]
    ),
    use.names = TRUE,
    fill = TRUE
  )
  
  # ----------------------------------------------------------
  # Distribución del exceso a causas covid/pandemia con
  # reasignación contable explícita desde otros grupos letales.
  # No se infla el total corregido all-cause: el componente
  # pandémico entra a causas elegibles covid y sale del resto
  # de causas letales no-covid dentro de la misma celda.
  # ----------------------------------------------------------
  msg("Distribuyendo exceso pandemico con definicion canonica de OPRM...")
  
  pandemic_alloc <- merge(
    leaf[, .(
      year_id, location_id, sex_id, age, cause_concept_id,
      is_covid_related, is_covid_specific, is_oprm,
      pandemic_component_class, is_pandemic_named_component,
      deaths_post_redistribution
    )],
    fac_base[, .(
      year_id, location_id, sex_id, age,
      correction_factor_completeness,
      pandemic_excess_allcause
    )],
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  
  pandemic_alloc[is.na(correction_factor_completeness), correction_factor_completeness := 1]
  pandemic_alloc[is.na(pandemic_excess_allcause), pandemic_excess_allcause := 0]
  pandemic_alloc[, base_cause_deaths_corrected := deaths_post_redistribution * correction_factor_completeness]
  
  pandemic_alloc[
    ,
    `:=`(
      pandemic_named_component_allcause = sum(base_cause_deaths_corrected[is_pandemic_named_component == TRUE], na.rm = TRUE),
      nonpandemic_source_sum = sum(base_cause_deaths_corrected[pandemic_component_class == "non_pandemic"], na.rm = TRUE),
      oprm_existing_base = sum(base_cause_deaths_corrected[pandemic_component_class == "oprm"], na.rm = TRUE),
      n_oprm_targets = sum(pandemic_component_class == "oprm", na.rm = TRUE),
      n_nonpandemic_sources = sum(pandemic_component_class == "non_pandemic", na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age)
  ]
  
  pandemic_alloc[, pandemic_named_component_capped := pmin(pandemic_excess_allcause, pandemic_named_component_allcause)]
  pandemic_alloc[, oprm_residual_allcause := pmax(0, pandemic_excess_allcause - pandemic_named_component_capped)]
  pandemic_alloc[, pandemic_reallocation_source_mode := fifelse(
    oprm_residual_allcause <= 0, "none_needed",
    fifelse(
      nonpandemic_source_sum > 0 & n_oprm_targets > 0 & oprm_residual_allcause <= (nonpandemic_source_sum + 1e-8),
      "nonpandemic_to_oprm",
      "ineligible"
    )
  )]
  pandemic_alloc[, oprm_weight := 0]
  pandemic_alloc[, noncovid_source_weight := 0]
  pandemic_alloc[, pandemic_reallocation_source_weight := 0]
  
  pandemic_alloc[
    pandemic_component_class == "oprm" & oprm_existing_base > 0,
    oprm_weight := base_cause_deaths_corrected / oprm_existing_base
  ]
  pandemic_alloc[
    pandemic_component_class == "oprm" & oprm_existing_base <= 0 & n_oprm_targets > 0,
    oprm_weight := 1 / n_oprm_targets
  ]
  
  pandemic_alloc[
    pandemic_component_class == "non_pandemic" & nonpandemic_source_sum > 0,
    noncovid_source_weight := base_cause_deaths_corrected / nonpandemic_source_sum
  ]
  qc_pandemic_reallocation_eligibility_failures <- unique(
    pandemic_alloc[
      oprm_residual_allcause > 0 &
        (
          n_oprm_targets <= 0 |
            pandemic_reallocation_source_mode == "ineligible"
        ),
      .(
        year_id, location_id, sex_id, age,
        pandemic_excess_allcause,
        pandemic_named_component_allcause,
        pandemic_named_component_capped,
        oprm_residual_allcause,
        nonpandemic_source_sum,
        n_oprm_targets,
        n_nonpandemic_sources,
        pandemic_reallocation_source_mode
      )
    ]
  )[order(year_id, location_id, sex_id, age)]
  write_qc(qc_pandemic_reallocation_eligibility_failures, "qc_pandemic_reallocation_eligibility_failures.csv")
  
  if (nrow(qc_pandemic_reallocation_eligibility_failures) > 0L) {
    stop(
      "QC HARD FAIL: existen celdas con exceso pandemico positivo que no pueden cerrarse ",
      "con la definicion canonica de OPRM. ",
      "Revisar qc_pandemic_reallocation_eligibility_failures.csv"
    )
  }
  
  pandemic_alloc[, pandemic_named_component := fifelse(
    is_pandemic_named_component == TRUE,
    base_cause_deaths_corrected,
    0
  )]
  pandemic_alloc[, oprm_residual_component := fifelse(
    pandemic_component_class == "oprm",
    oprm_residual_allcause * oprm_weight,
    0
  )]
  pandemic_alloc[, pandemic_excess_component := oprm_residual_component]
  pandemic_alloc[, noncovid_out_requested := fifelse(
    pandemic_component_class == "non_pandemic" & nonpandemic_source_sum > 0,
    oprm_residual_allcause * noncovid_source_weight,
    0
  )]
  pandemic_alloc[, pandemic_reassigned_out_component := fifelse(
    pandemic_component_class == "non_pandemic",
    pmin(base_cause_deaths_corrected, noncovid_out_requested),
    0
  )]
  pandemic_alloc[
    ,
    actual_noncovid_out_sum := sum(pandemic_reassigned_out_component[pandemic_component_class == "non_pandemic"], na.rm = TRUE),
    by = .(year_id, location_id, sex_id, age)
  ]
  pandemic_alloc[
    ,
    actual_oprm_in_sum := sum(oprm_residual_component[pandemic_component_class == "oprm"], na.rm = TRUE),
    by = .(year_id, location_id, sex_id, age)
  ]
  pandemic_alloc[, oprm_residual_unfunded := pmax(0, oprm_residual_allcause - actual_noncovid_out_sum)]
  pandemic_alloc[, pandemic_reallocation_source_weight := fifelse(
    oprm_residual_allcause > 0,
    pandemic_reassigned_out_component / oprm_residual_allcause,
    0
  )]
  
  qc_pandemic_component_noneligible <- pandemic_alloc[
    (pandemic_component_class != "oprm" & pandemic_excess_component > 1e-10) |
      (pandemic_component_class != "non_pandemic" & pandemic_reassigned_out_component > 1e-10) |
      (oprm_residual_unfunded > 1e-10)
  ][order(year_id, location_id, sex_id, age, cause_concept_id)]
  write_qc(qc_pandemic_component_noneligible, "qc_pandemic_component_noneligible.csv")
  
  if (nrow(qc_pandemic_component_noneligible) > 0L) {
    stop(
      "QC HARD FAIL: se detecto componente pandemico fuera de OPRM o salida contable fuera de causas no pandemicas. ",
      "Revisar qc_pandemic_component_noneligible.csv"
    )
  }
  
  pandemic_alloc <- pandemic_alloc[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    base_cause_deaths_corrected,
    pandemic_component_class,
    pandemic_named_component,
    pandemic_named_component_allcause,
    pandemic_named_component_capped,
    oprm_existing_base,
    oprm_weight,
    oprm_residual_allcause,
    oprm_residual_component,
    nonpandemic_source_sum,
    noncovid_source_weight,
    pandemic_reallocation_source_mode,
    pandemic_reallocation_source_weight,
    pandemic_excess_component,
    pandemic_reassigned_out_component
  )]
  
  # ----------------------------------------------------------
  # Construcción final
  # ----------------------------------------------------------
  msg("Construyendo tabla final...")
  
  final <- merge(
    leaf[, .(
      year_id, location_id, sex_id, age, cause_concept_id,
      deaths_observed, deaths_post_redistribution, deaths_input_semantic, location_id_semantic
    )],
    fac_base[, .(
      year_id, location_id, sex_id, age,
      correction_factor_completeness,
      correction_factor_preclip,
      factor_method,
      factor_truncated_low,
      factor_truncated_high,
      factor_from_missing_external,
      direct_ratio_raw,
      direct_ratio,
      direct_ratio_eligible,
      external_status,
      observed_allcause,
      expected_allcause,
      observed_corrected_allcause,
      pandemic_excess_allcause,
      population,
      mx
    )],
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  
  final <- merge(
    final,
    pandemic_alloc,
    by = c("year_id", "location_id", "sex_id", "age", "cause_concept_id"),
    all.x = TRUE,
    sort = FALSE
  )
  
  final[is.na(correction_factor_completeness), correction_factor_completeness := 1]
  final[is.na(pandemic_excess_component), pandemic_excess_component := 0]
  final[is.na(base_cause_deaths_corrected), base_cause_deaths_corrected := deaths_post_redistribution * correction_factor_completeness]
  final[is.na(pandemic_named_component), pandemic_named_component := 0]
  final[is.na(pandemic_named_component_allcause), pandemic_named_component_allcause := 0]
  final[is.na(pandemic_named_component_capped), pandemic_named_component_capped := 0]
  final[is.na(oprm_existing_base), oprm_existing_base := 0]
  final[is.na(oprm_weight), oprm_weight := 0]
  final[is.na(oprm_residual_allcause), oprm_residual_allcause := 0]
  final[is.na(oprm_residual_component), oprm_residual_component := 0]
  final[is.na(nonpandemic_source_sum), nonpandemic_source_sum := 0]
  final[is.na(noncovid_source_weight), noncovid_source_weight := 0]
  final[is.na(pandemic_reassigned_out_component), pandemic_reassigned_out_component := 0]
  final[is.na(pandemic_component_class), pandemic_component_class := "non_pandemic"]
  final[, subregistro_gain_component := pmax(0, base_cause_deaths_corrected - deaths_post_redistribution)]
  
  final[, deaths_final_net_of_pandemic := base_cause_deaths_corrected - pandemic_reassigned_out_component]
  final[, deaths_final := deaths_final_net_of_pandemic + pandemic_excess_component]
  final[, deaths_final_net_of_pandemic := safe_nonneg(deaths_final_net_of_pandemic)]
  final[, deaths_final := safe_nonneg(deaths_final)]
  final[, deaths_observed_semantic := "equals_post_redistribution_in_current_mvp"]
  final[, run_id := run_id]
  final <- merge(
    final,
    cm_term[, .(cause_concept_id, cause_name)],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # ----------------------------------------------------------
  # Hard checks sobre final
  # ----------------------------------------------------------
  qc_final_completeness_extremes <- final[
    correction_factor_completeness < CFG$min_completeness_factor |
      correction_factor_completeness > CFG$max_completeness_factor
  ][order(year_id, location_id, sex_id, age, cause_concept_id)]
  
  write_qc(qc_final_completeness_extremes, "qc_final_completeness_extremes.csv")
  
  if (any(final$correction_factor_completeness < CFG$min_completeness_factor, na.rm = TRUE)) {
    stop("QC HARD FAIL: correction_factor_completeness por debajo del mínimo.")
  }
  
  if (any(final$correction_factor_completeness > CFG$max_completeness_factor, na.rm = TRUE)) {
    stop("QC HARD FAIL: correction_factor_completeness por encima del máximo.")
  }
  
  if (nrow(fac_base[year_id %in% CFG$prepandemic_years & external_status != "ok"]) > 0L) {
    msg("Advertencia: existen celdas prepandemia sin insumos externos válidos. Revisar QC enriquecido.")
  }
  
  # ----------------------------------------------------------
  # Duplicados accidentales: endurecer control
  # ----------------------------------------------------------
  dup_pk <- final[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]
  
  if (nrow(dup_pk) > 0L) {
    write_qc(dup_pk, "qc_duplicate_pk_precollapse.csv")
    assert_unique_factor_within_pk(final)
    
    final <- final[, .(
      deaths_observed = sum(deaths_observed, na.rm = TRUE),
      deaths_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
      base_cause_deaths_corrected = sum(base_cause_deaths_corrected, na.rm = TRUE),
      deaths_final_net_of_pandemic = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
      deaths_final = sum(deaths_final, na.rm = TRUE),
      correction_factor_completeness = unique(correction_factor_completeness[!is.na(correction_factor_completeness)])[1],
      pandemic_excess_component = sum(pandemic_excess_component, na.rm = TRUE),
      pandemic_reassigned_out_component = sum(pandemic_reassigned_out_component, na.rm = TRUE),
      observed_allcause = unique(observed_allcause[!is.na(observed_allcause)])[1],
      expected_allcause = unique(expected_allcause[!is.na(expected_allcause)])[1],
      observed_corrected_allcause = unique(observed_corrected_allcause[!is.na(observed_corrected_allcause)])[1],
      pandemic_excess_allcause = unique(pandemic_excess_allcause[!is.na(pandemic_excess_allcause)])[1],
      population = unique(population[!is.na(population)])[1],
      mx = unique(mx[!is.na(mx)])[1],
      cause_name = unique(cause_name[!is.na(cause_name)])[1],
      pandemic_component_class = unique(pandemic_component_class[!is.na(pandemic_component_class)])[1],
      pandemic_named_component = sum(pandemic_named_component, na.rm = TRUE),
      pandemic_named_component_allcause = unique(pandemic_named_component_allcause[!is.na(pandemic_named_component_allcause)])[1],
      pandemic_named_component_capped = unique(pandemic_named_component_capped[!is.na(pandemic_named_component_capped)])[1],
      oprm_existing_base = unique(oprm_existing_base[!is.na(oprm_existing_base)])[1],
      oprm_weight = sum(oprm_weight, na.rm = TRUE),
      oprm_residual_allcause = unique(oprm_residual_allcause[!is.na(oprm_residual_allcause)])[1],
      oprm_residual_component = sum(oprm_residual_component, na.rm = TRUE),
      nonpandemic_source_sum = unique(nonpandemic_source_sum[!is.na(nonpandemic_source_sum)])[1],
      noncovid_source_weight = sum(noncovid_source_weight, na.rm = TRUE),
      pandemic_reallocation_source_mode = unique(pandemic_reallocation_source_mode[!is.na(pandemic_reallocation_source_mode)])[1],
      pandemic_reallocation_source_weight = sum(pandemic_reallocation_source_weight, na.rm = TRUE),
      subregistro_gain_component = sum(subregistro_gain_component, na.rm = TRUE),
      deaths_observed_semantic = unique(deaths_observed_semantic)[1],
      deaths_input_semantic = unique(deaths_input_semantic)[1],
      correction_factor_preclip = unique(correction_factor_preclip[!is.na(correction_factor_preclip)])[1],
      factor_method = unique(factor_method[!is.na(factor_method)])[1],
      factor_truncated_low = unique(factor_truncated_low[!is.na(factor_truncated_low)])[1],
      factor_truncated_high = unique(factor_truncated_high[!is.na(factor_truncated_high)])[1],
      factor_from_missing_external = unique(factor_from_missing_external[!is.na(factor_from_missing_external)])[1],
      direct_ratio_raw = unique(direct_ratio_raw[!is.na(direct_ratio_raw)])[1],
      direct_ratio = unique(direct_ratio[!is.na(direct_ratio)])[1],
      direct_ratio_eligible = unique(direct_ratio_eligible[!is.na(direct_ratio_eligible)])[1],
      external_status = unique(external_status[!is.na(external_status)])[1],
      location_id_semantic = unique(location_id_semantic)[1],
      run_id = unique(run_id)[1]
    ), by = .(year_id, location_id, sex_id, age, cause_concept_id)]
  }
  
  setcolorder(final, c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_name",
    "deaths_observed", "deaths_post_redistribution", "base_cause_deaths_corrected",
    "deaths_final_net_of_pandemic", "deaths_final",
    "correction_factor_completeness", "subregistro_gain_component",
    "pandemic_component_class", "pandemic_named_component",
    "oprm_residual_component", "pandemic_excess_component",
    "pandemic_reassigned_out_component", "run_id"
  ))
  
  # ----------------------------------------------------------
  # QC mínimo duro
  # ----------------------------------------------------------
  msg("Corriendo QC mínimo...")
  
  spec_final <- read_spec(CFG$spec_final_path)
  validate_by_spec(final[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    deaths_observed, deaths_post_redistribution, deaths_final,
    correction_factor_completeness, pandemic_excess_component, run_id
  )], spec_final)
  
  qc_factor_summary <- fac_base[, .(
    n = .N,
    min_factor = suppressWarnings(min(correction_factor_completeness, na.rm = TRUE)),
    p25_factor = suppressWarnings(quantile(correction_factor_completeness, 0.25, na.rm = TRUE)),
    median_factor = suppressWarnings(median(correction_factor_completeness, na.rm = TRUE)),
    p75_factor = suppressWarnings(quantile(correction_factor_completeness, 0.75, na.rm = TRUE)),
    max_factor = suppressWarnings(max(correction_factor_completeness, na.rm = TRUE))
  ), by = .(year_id)][order(year_id)]
  
  qc_allcause_balance <- final[
    ,
    .(
      total_post = sum(deaths_post_redistribution, na.rm = TRUE),
      total_final_net = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
      total_final = sum(deaths_final, na.rm = TRUE),
      total_pandemic_component = sum(pandemic_excess_component, na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age)
  ][order(year_id, location_id, sex_id, age)]
  
  qc_year_summary <- final[, .(
    deaths_post = sum(deaths_post_redistribution, na.rm = TRUE),
    deaths_final_net = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE),
    pandemic_excess = sum(pandemic_excess_component, na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]
  
  qc_pandemic_by_year <- final[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE),
    pandemic_excess_allcause = sum(unique(pandemic_excess_allcause[!is.na(pandemic_excess_allcause)]), na.rm = TRUE),
    pandemic_named_component_allcause = sum(unique(pandemic_named_component_allcause[!is.na(pandemic_named_component_allcause)]), na.rm = TRUE),
    oprm_residual_allcause = sum(unique(oprm_residual_allcause[!is.na(oprm_residual_allcause)]), na.rm = TRUE)
  ), by = .(year_id, location_id, sex_id, age)][
    , .(
      observed_allcause = sum(observed_allcause, na.rm = TRUE),
      expected_allcause = sum(expected_allcause, na.rm = TRUE),
      observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE),
      pandemic_excess_allcause = sum(pandemic_excess_allcause, na.rm = TRUE),
      pandemic_named_component_allcause = sum(pandemic_named_component_allcause, na.rm = TRUE),
      oprm_residual_allcause = sum(oprm_residual_allcause, na.rm = TRUE)
    ),
    by = .(year_id)
  ][order(year_id)]
  
  qc_pandemic_reallocation_balance <- final[
    ,
    .(
      total_base_corrected = sum(base_cause_deaths_corrected, na.rm = TRUE),
      total_final_net = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
      total_final = sum(deaths_final, na.rm = TRUE),
      total_pandemic_in = sum(pandemic_excess_component, na.rm = TRUE),
      total_pandemic_out = sum(pandemic_reassigned_out_component, na.rm = TRUE),
      expected_allcause = unique(expected_allcause[!is.na(expected_allcause)])[1],
      observed_corrected_allcause = unique(observed_corrected_allcause[!is.na(observed_corrected_allcause)])[1],
      pandemic_excess_allcause = unique(pandemic_excess_allcause[!is.na(pandemic_excess_allcause)])[1],
      pandemic_named_component_allcause = unique(pandemic_named_component_allcause[!is.na(pandemic_named_component_allcause)])[1],
      oprm_residual_allcause = unique(oprm_residual_allcause[!is.na(oprm_residual_allcause)])[1]
    ),
    by = .(year_id, location_id, sex_id, age)
  ]
  qc_pandemic_reallocation_balance[, target_final_net := observed_corrected_allcause - oprm_residual_allcause]
  qc_pandemic_reallocation_balance[, `:=`(
    delta_base_vs_corrected = total_base_corrected - observed_corrected_allcause,
    delta_net_vs_target = total_final_net - target_final_net,
    delta_final_vs_corrected = total_final - observed_corrected_allcause,
    delta_in_vs_out = total_pandemic_in - total_pandemic_out
  )]
  write_qc(qc_pandemic_reallocation_balance, "qc_pandemic_reallocation_balance.csv")
  
  qc_pandemic_reallocation_balance_bad <- qc_pandemic_reallocation_balance[
    abs(delta_base_vs_corrected) > 1e-6 |
      abs(delta_net_vs_target) > 1e-6 |
      abs(delta_final_vs_corrected) > 1e-6 |
      abs(delta_in_vs_out) > 1e-6
  ][order(year_id, location_id, sex_id, age)]
  write_qc(qc_pandemic_reallocation_balance_bad, "qc_pandemic_reallocation_balance_bad.csv")
  
  if (nrow(qc_pandemic_reallocation_balance_bad) > 0L) {
    stop(
      "QC HARD FAIL: la reasignación pandémica no cierra contablemente en algunas celdas. ",
      "Revisar qc_pandemic_reallocation_balance_bad.csv"
    )
  }
  
  qc_missing_external <- fac_base[
    external_status != "ok",
    .N,
    by = .(year_id, external_status)
  ][order(year_id, external_status)]
  
  qc_factor_method_summary <- fac_base[
    ,
    .N,
    by = .(year_id, factor_method)
  ][order(year_id, factor_method)]
  
  qc_factor_flags_summary <- fac_base[, .(
    n = .N,
    n_direct_eligible = sum(direct_ratio_eligible, na.rm = TRUE),
    n_truncated_low = sum(factor_truncated_low, na.rm = TRUE),
    n_truncated_high = sum(factor_truncated_high, na.rm = TRUE),
    n_from_missing_external = sum(factor_from_missing_external, na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]
  
  qc_backoff_extremes <- fac_base[
    factor_method != "direct_prepandemic",
    .(
      year_id, location_id, sex_id, age,
      observed_allcause, expected_allcause,
      correction_factor_preclip,
      correction_factor_completeness,
      factor_method,
      external_status
    )
  ][order(-correction_factor_completeness)][1:min(.N, 200)]
  
  mortality_completeness_audit <- fac_base[, .(
    year_id, location_id, sex_id, age,
    population,
    mx,
    expected_allcause,
    observed_allcause,
    direct_ratio,
    correction_factor_completeness,
    observed_corrected_allcause,
    pandemic_excess_allcause,
    run_id = run_id
  )][order(year_id, location_id, sex_id, age)]
  
  mortality_pandemic_allocation_audit <- final[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    deaths_post_redistribution,
    base_cause_deaths_corrected,
    correction_factor_completeness,
    deaths_final_net_of_pandemic,
    pandemic_reassigned_out_component,
    pandemic_named_component,
    pandemic_named_component_allcause,
    pandemic_named_component_capped,
    oprm_residual_allcause,
    oprm_residual_component,
    pandemic_excess_component,
    deaths_final,
    nonpandemic_source_sum,
    noncovid_source_weight,
    pandemic_reallocation_source_mode,
    pandemic_reallocation_source_weight,
    pandemic_component_class,
    subregistro_gain_component,
    run_id
  )][order(year_id, location_id, sex_id, age, cause_concept_id)]
  
  qc_factor_extremes <- mortality_completeness_audit[
    order(-correction_factor_completeness)
  ][1:min(.N, 200)]
  
  qc_pandemic_extremes <- final[
    ,
    .(
      total_post = sum(deaths_post_redistribution, na.rm = TRUE),
      total_final = sum(deaths_final, na.rm = TRUE),
      total_pandemic = sum(pandemic_excess_component, na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age)
  ][
    ,
    pandemic_share_over_final := fifelse(total_final > 0, total_pandemic / total_final, NA_real_)
  ][order(-pandemic_share_over_final)][1:min(.N, 200)]
  
  qc_semantic_note <- data.table(
    variable = "deaths_observed",
    semantic_note = "equals_post_redistribution_in_current_mvp",
    run_id = run_id
  )
  
  if (nrow(final[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]) > 0L) {
    stop("QC HARD FAIL: PK duplicada persiste en death_cause_final.")
  }
  
  if (any(final$deaths_final < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: deaths_final negativas.")
  }
  
  if (any(final$pandemic_excess_component < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: pandemic_excess_component negativo.")
  }
  
  if (any(final$pandemic_reassigned_out_component < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: pandemic_reassigned_out_component negativo.")
  }
  
  covid_id <- cm_term[cause_name == "COVID-19", cause_concept_id][1]
  oprm_id <- cm_term[cause_name == "Other pandemic related mortality (OPRM)", cause_concept_id][1]
  qc_covid_oprm_out_of_time_window <- final[
    (
      !is.na(covid_id) &
        cause_concept_id == covid_id &
        year_id < CFG$pandemic_year_min &
        deaths_final > 1e-10
    ) |
      (
        !is.na(oprm_id) &
          cause_concept_id == oprm_id &
          (year_id < CFG$pandemic_year_min | year_id > CFG$pandemic_year_max) &
          deaths_final > 1e-10
      ),
    .(
      year_id, location_id, sex_id, age, cause_concept_id, cause_name,
      deaths_post_redistribution, base_cause_deaths_corrected,
      pandemic_excess_component, pandemic_reassigned_out_component, deaths_final
    )
  ][order(year_id, cause_concept_id, location_id, sex_id, age)]
  write_qc(qc_covid_oprm_out_of_time_window, "qc_covid_oprm_out_of_time_window.csv")
  
  if (nrow(qc_covid_oprm_out_of_time_window) > 0L) {
    stop(
      "QC HARD FAIL: COVID-19 aparece antes de 2020 o OPRM fuera de la ventana 2020-2022. ",
      "Revisar qc_covid_oprm_out_of_time_window.csv"
    )
  }
  
  qc_pandemic_reassigned_out_by_cause <- final[
    pandemic_reassigned_out_component > 0,
    .(
      pandemic_reassigned_out_component = sum(pandemic_reassigned_out_component, na.rm = TRUE),
      deaths_final_net_of_pandemic = sum(deaths_final_net_of_pandemic, na.rm = TRUE)
    ),
    by = .(year_id, cause_concept_id, cause_name)
  ][order(year_id, -pandemic_reassigned_out_component, cause_concept_id)]
  write_qc(qc_pandemic_reassigned_out_by_cause, "qc_pandemic_reassigned_out_by_cause.csv")

  qc_sex_specific_mismatch_positive_final <- merge(
    final,
    unique(cm_term[
      !is.na(sex_restriction_target_default) & sex_restriction_target_default != "",
      .(cause_concept_id, cause_name, expected_sex = sex_restriction_target_default)
    ]),
    by = c("cause_concept_id", "cause_name"),
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(deaths_final = sum(deaths_final, na.rm = TRUE)),
    by = .(year_id, location_id, sex_id, age, cause_concept_id, cause_name, expected_sex)
  ]
  qc_sex_specific_mismatch_positive_final[
    ,
    observed_sex := fifelse(
      sex_id == 8507L, "male",
      fifelse(sex_id == 8532L, "female", "other")
    )
  ]
  qc_sex_specific_mismatch_positive_final <- qc_sex_specific_mismatch_positive_final[
    observed_sex != expected_sex & deaths_final > 1e-10
  ][order(-deaths_final, cause_name, year_id, location_id, sex_id, age)]
  write_qc(qc_sex_specific_mismatch_positive_final, "qc_sex_specific_mismatch_positive_final.csv")

  qc_age_specific_mismatch_positive_final <- merge(
    final,
    unique(cm_term[
      (!is.na(target_age_start_default) | !is.na(target_age_end_default)),
      .(
        cause_concept_id,
        cause_name,
        expected_age_start = target_age_start_default,
        expected_age_end = target_age_end_default
      )
    ]),
    by = c("cause_concept_id", "cause_name"),
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(deaths_final = sum(deaths_final, na.rm = TRUE)),
    by = .(year_id, location_id, sex_id, age, cause_concept_id, cause_name, expected_age_start, expected_age_end)
  ]
  qc_age_specific_mismatch_positive_final[
    ,
    age_mismatch := (
      (!is.na(expected_age_start) & age < expected_age_start) |
        (!is.na(expected_age_end) & age > expected_age_end)
    )
  ]
  qc_age_specific_mismatch_positive_final <- qc_age_specific_mismatch_positive_final[
    age_mismatch == TRUE & deaths_final > 1e-10
  ][order(-deaths_final, cause_name, year_id, location_id, sex_id, age)]
  write_qc(qc_age_specific_mismatch_positive_final, "qc_age_specific_mismatch_positive_final.csv")

  if (nrow(qc_sex_specific_mismatch_positive_final) > 0L) {
    msg(
      "ADVERTENCIA QC: death_cause_final conserva muertes en causas sexo-específicas incompatibles. ",
      "Revisar qc_sex_specific_mismatch_positive_final.csv. ",
      "Estas filas pueden reflejar codificación directa del input y no una fuga de redistribución."
    )
  }
  if (nrow(qc_age_specific_mismatch_positive_final) > 0L) {
    msg(
      "ADVERTENCIA QC: death_cause_final conserva muertes en causas con dominio etario incompatible. ",
      "Revisar qc_age_specific_mismatch_positive_final.csv. ",
      "Estas filas pueden reflejar codificacion directa del input y no una fuga de redistribucion."
    )
  }
  
  # ----------------------------------------------------------
  # Export
  # ----------------------------------------------------------
  msg("Exportando...")
  
  out_csv     <- file.path(CFG$out_dir, paste0(CFG$table_name, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$table_name, ".parquet"))
  out_dict    <- file.path(CFG$out_dir, paste0(CFG$table_name, "_dictionary_ext.csv"))
  
  dict_ext <- build_dictionary_ext(final)
  
  write_csv_parquet(final, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(dict_ext, out_dict)
  
  write_qc(qc_factor_summary, "qc_factor_summary.csv")
  write_qc(qc_allcause_balance, "qc_allcause_balance.csv")
  write_qc(qc_year_summary, "qc_year_summary.csv")
  write_qc(qc_pandemic_by_year, "qc_pandemic_by_year.csv")
  write_qc(qc_missing_external, "qc_missing_external.csv")
  write_qc(qc_factor_method_summary, "qc_factor_method_summary.csv")
  write_qc(qc_factor_flags_summary, "qc_factor_flags_summary.csv")
  write_qc(qc_backoff_extremes, "qc_backoff_extremes.csv")
  write_qc(mortality_completeness_audit, "mortality_completeness_audit.csv")
  write_qc(mortality_pandemic_allocation_audit, "mortality_pandemic_allocation_audit.csv")
  write_qc(qc_factor_extremes, "qc_factor_extremes.csv")
  write_qc(qc_pandemic_extremes, "qc_pandemic_extremes.csv")
  write_qc(qc_semantic_note, "qc_semantic_note.csv")
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(final),
    n_cols = ncol(final),
    notes = "CSV final death_cause_final endurecido con QC de completitud, geografía armonizada y pandemia"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_parquet,
    n_rows = nrow(final),
    n_cols = ncol(final),
    notes = "Parquet final death_cause_final endurecido con QC de completitud, geografía armonizada y pandemia"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "dictionary_ext",
    artifact_path = out_dict,
    n_rows = nrow(dict_ext),
    n_cols = ncol(dict_ext),
    notes = "Diccionario extendido death_cause_final"
  )
  
  qc_paths <- c(
    file.path(CFG$qc_dir, "qc_geo_harmonization.csv"),
    file.path(CFG$qc_dir, "qc_geo_domain.csv"),
    file.path(CFG$qc_dir, "qc_missing_external_summary.csv"),
    file.path(CFG$qc_dir, "qc_missing_external_examples.csv"),
    file.path(CFG$qc_dir, "qc_final_completeness_extremes.csv"),
    file.path(CFG$qc_dir, "qc_factor_summary.csv"),
    file.path(CFG$qc_dir, "qc_allcause_balance.csv"),
    file.path(CFG$qc_dir, "qc_year_summary.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_by_year.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_synthetic_target_cells.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_reallocation_eligibility_failures.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_component_noneligible.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_reallocation_balance.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_reallocation_balance_bad.csv"),
    file.path(CFG$qc_dir, "qc_covid_oprm_out_of_time_window.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_reassigned_out_by_cause.csv"),
    file.path(CFG$qc_dir, "qc_missing_external.csv"),
    file.path(CFG$qc_dir, "qc_factor_method_summary.csv"),
    file.path(CFG$qc_dir, "qc_factor_flags_summary.csv"),
    file.path(CFG$qc_dir, "qc_backoff_extremes.csv"),
    file.path(CFG$qc_dir, "mortality_completeness_audit.csv"),
    file.path(CFG$qc_dir, "mortality_pandemic_allocation_audit.csv"),
    file.path(CFG$qc_dir, "qc_factor_extremes.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_extremes.csv"),
    file.path(CFG$qc_dir, "qc_semantic_note.csv")
  )
  
  for (p in qc_paths[file.exists(qc_paths)]) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "qc",
      artifact_path = p,
      n_rows = tryCatch(nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(ncol(fread(p)), error = function(e) NA_integer_),
      notes = "QC 08_build_death_cause_final"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "08_build_death_cause_final completado")
  
  msg("OK -> CSV: ", out_csv)
  msg("OK -> Parquet: ", out_parquet)
  msg("OK -> Dictionary: ", out_dict)
  msg("OK -> QC dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
