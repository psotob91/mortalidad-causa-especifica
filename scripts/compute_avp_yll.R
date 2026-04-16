#!/usr/bin/env Rscript

# ============================================================
# 10_compute_avp_yll.R
# ------------------------------------------------------------
# Objetivo:
#   Calcular AVP/YLL a partir de:
#     - mortality_rate_cause_smoothed_reconciled
#     - life_table_standard_single_age
#
# Enfoque revisado:
#   - usa muertes reconciliadas del 09b
#   - AVP = deaths_smoothed_consistent * ex_standard
#   - NO vuelve a reconciliar; solo verifica consistencia
#   - recalcula tasas desde AVP absolutos / población
#   - agrega tolerancias numéricas razonables
#   - agrega auditoría de extremos
#   - usa warning y hard fail separados para plausibilidad
#
# Salidas:
#   data/final/avp_yll_cause_reconciled/
#     - avp_yll_cause_reconciled.csv
#     - avp_yll_cause_reconciled.parquet
#     - avp_yll_cause_reconciled_dictionary_ext.csv
#
# QC mínimo:
#   - no negativos
#   - no missing en ex_standard
#   - geografía: suma deptos = 9000 dentro de tolerancia
#   - jerarquía: suma hijos = padre dentro de tolerancia
#   - plausibilidad: warning y hard fail separados
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

mortality_input_override <- Sys.getenv("AVP_INPUT_MORTALITY_PATH", unset = "")
output_suffix <- Sys.getenv("AVP_OUTPUT_SUFFIX", unset = "")
output_suffix_path <- if (nzchar(output_suffix)) paste0("_", output_suffix) else ""

CFG <- list(
  version = "v0.3.0_avp_reconciled_final",
  dataset_id = "avp_yll_cause_reconciled",
  table_name = "avp_yll_cause_reconciled",
  
  input_mortality_candidates = c(
    if (nzchar(mortality_input_override)) mortality_input_override,
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  external_yaml_path = here("config", "external_sources.yml"),
  
  out_dir = here("data", "final", paste0("avp_yll_cause_reconciled", output_suffix_path)),
  qc_dir  = qc_dir_path(paste0("compute_avp_yll", output_suffix_path)),
  
  years = 2018:2024,
  base_locations = 1:25,
  national_additive_id = 9000L,
  keep_cause_levels = c(0L, 1L, 2L, 3L, 4L),
  valid_sexes = c(8507L, 8532L),
  age_min = 0L,
  age_max = 110L,
  target_standard_source = "WHO",
  target_standard_version = "GHE",
  standard_persons_sex_id = 0L,
  allow_persons_fallback_for_sex_specific = TRUE,
  
  rate_multiplier = 100000,
  max_avp_rate_warn_per_100k = 100000,
  max_avp_rate_hard_per_100k = 500000,
  min_population_for_rate_qc = 25,
  max_age_for_rate_qc = 100L,
  top_n_extremes = 200L,
  
  geo_abs_tol = 1e-8,
  geo_rel_tol = 1e-10,
  cause_abs_tol = 1e-8,
  cause_rel_tol = 1e-10,
  
  verbose = TRUE
)

for (d in c(CFG$out_dir, CFG$qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
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

resolve_external_path2 <- function(external_yaml, key) {
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

safe_rate <- function(num, den, mult = 100000) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- mult * num[ok] / den[ok]
  out
}

resolve_standard_life_table <- function(lt, cfg) {
  available_versions <- unique(lt[, .(
    standard_source,
    standard_version,
    sex_id
  )])[order(standard_source, standard_version, sex_id)]
  
  lt_target <- lt[
    standard_source == cfg$target_standard_source &
      standard_version == cfg$target_standard_version
  ]
  
  if (nrow(lt_target) == 0L) {
    stop(
      "No se encontró la tabla estándar objetivo: ",
      cfg$target_standard_source, " / ", cfg$target_standard_version
    )
  }
  
  lt_target_sex_specific <- lt_target[sex_id %in% cfg$valid_sexes]
  has_complete_sex_specific <- uniqueN(lt_target_sex_specific$sex_id) == length(cfg$valid_sexes)
  
  if (has_complete_sex_specific) {
    lt_selected <- copy(lt_target_sex_specific)
    lt_selected[, sex_resolution := "sex_specific_direct"]
  } else if (isTRUE(cfg$allow_persons_fallback_for_sex_specific) &&
             cfg$standard_persons_sex_id %in% lt_target$sex_id) {
    lt_persons <- unique(
      lt_target[sex_id == cfg$standard_persons_sex_id, .(
        age,
        ex_standard,
        standard_source,
        standard_version
      )]
    )
    lt_selected <- rbindlist(lapply(cfg$valid_sexes, function(sx) {
      copy(lt_persons)[, `:=`(sex_id = as.integer(sx), sex_resolution = "persons_fallback")]
    }), use.names = TRUE)
  } else {
    stop(
      "La tabla estándar objetivo no tiene cobertura sexo-específica completa ",
      "ni permite fallback desde Persons. Objetivo: ",
      cfg$target_standard_source, " / ", cfg$target_standard_version
    )
  }
  
  lt_selected <- unique(
    lt_selected[, .(
      sex_id,
      age,
      ex_standard,
      standard_source,
      standard_version,
      sex_resolution
    )]
  )
  
  dup_keys <- lt_selected[, .N, by = .(sex_id, age)][N > 1L]
  if (nrow(dup_keys) > 0L) {
    stop("La tabla estándar seleccionada tiene duplicados por sex_id + age.")
  }
  
  expected_grid <- CJ(
    sex_id = as.integer(cfg$valid_sexes),
    age = as.integer(cfg$age_min:cfg$age_max),
    unique = TRUE
  )
  coverage <- merge(expected_grid, lt_selected, by = c("sex_id", "age"), all.x = TRUE)
  
  list(
    available_versions = available_versions,
    selected = lt_selected,
    coverage = coverage
  )
}

rollup_metric_by_cause <- function(dt, cm, value_col) {
  stopifnot(value_col %in% names(dt))
  cm_use <- unique(cm[, .(cause_concept_id, parent_concept_id, cause_level)])
  levels_desc <- sort(unique(cm_use$cause_level), decreasing = TRUE, na.last = NA)
  out_dt <- copy(dt)
  
  for (lvl in levels_desc) {
    edges_lvl <- cm_use[
      cause_level == lvl &
        !is.na(parent_concept_id) &
        cause_concept_id != parent_concept_id,
      .(cause_concept_id, parent_concept_id)
    ]
    if (nrow(edges_lvl) == 0L) next
    
    child_sum <- merge(
      out_dt[, c("year_id", "location_id", "sex_id", "age", "cause_concept_id", value_col), with = FALSE],
      edges_lvl,
      by = "cause_concept_id",
      all = FALSE
    )[
      ,
      .(rolled_value = sum(get(value_col), na.rm = TRUE)),
      by = .(year_id, location_id, sex_id, age, parent_concept_id)
    ]
    if (nrow(child_sum) == 0L) next
    
    setnames(child_sum, c("parent_concept_id", "rolled_value"), c("cause_concept_id", value_col))
    setkeyv(out_dt, c("year_id", "location_id", "sex_id", "age", "cause_concept_id"))
    setkeyv(child_sum, c("year_id", "location_id", "sex_id", "age", "cause_concept_id"))
    out_dt[child_sum, (value_col) := get(paste0("i.", value_col))]
  }
  
  out_dt[]
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

qc_geo_hard_avp <- function(dt, base_locations, national_additive_id,
                            abs_tol = CFG$geo_abs_tol,
                            rel_tol = CFG$geo_rel_tol) {
  chk_dept <- dt[
    location_id %in% base_locations,
    .(
      avp_abs_dept = sum(avp_abs, na.rm = TRUE),
      yll_abs_dept = sum(yll_abs, na.rm = TRUE)
    ),
    by = .(year_id, sex_id, age, cause_concept_id)
  ]
  
  chk_nat <- dt[
    location_id == national_additive_id,
    .(
      avp_abs_nat = sum(avp_abs, na.rm = TRUE),
      yll_abs_nat = sum(yll_abs, na.rm = TRUE)
    ),
    by = .(year_id, sex_id, age, cause_concept_id)
  ]
  
  cmp <- merge(
    chk_dept, chk_nat,
    by = c("year_id", "sex_id", "age", "cause_concept_id"),
    all = TRUE
  )
  
  cmp[, diff_avp := avp_abs_nat - avp_abs_dept]
  cmp[, diff_yll := yll_abs_nat - yll_abs_dept]
  
  cmp[, tol_avp := pmax(abs_tol, rel_tol * pmax(abs(avp_abs_nat), abs(avp_abs_dept), 1))]
  cmp[, tol_yll := pmax(abs_tol, rel_tol * pmax(abs(yll_abs_nat), abs(yll_abs_dept), 1))]
  
  cmp[, pass_avp := !is.na(diff_avp) & abs(diff_avp) <= tol_avp]
  cmp[, pass_yll := !is.na(diff_yll) & abs(diff_yll) <= tol_yll]
  
  fwrite(cmp, file.path(CFG$qc_dir, "qc_geo_hard_compare_avp.csv"))
  
  bad <- cmp[pass_avp == FALSE | pass_yll == FALSE]
  
  if (nrow(bad) > 0L) {
    top <- bad[order(-pmax(abs(diff_avp), abs(diff_yll)))][1:min(.N, 20)]
    stop(
      "QC HARD FAIL: geografía AVP/YLL inconsistente dentro de tolerancia. ",
      "Revisar qc_geo_hard_compare_avp.csv\n",
      paste(capture.output(print(top)), collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

qc_cause_hard_avp <- function(dt, cm,
                              abs_tol = CFG$cause_abs_tol,
                              rel_tol = CFG$cause_rel_tol) {
  cm_use <- unique(cm[, .(cause_concept_id, parent_concept_id, cause_level)])
  edges <- cm_use[!is.na(parent_concept_id) & cause_concept_id != parent_concept_id]
  
  if (nrow(edges) == 0L) return(invisible(TRUE))
  
  child_sum <- merge(
    dt[, .(year_id, location_id, sex_id, age, cause_concept_id, avp_abs, yll_abs)],
    edges,
    by = "cause_concept_id",
    all.x = FALSE,
    all.y = FALSE
  )[
    ,
    .(
      child_avp_sum = sum(avp_abs, na.rm = TRUE),
      child_yll_sum = sum(yll_abs, na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age, parent_concept_id)
  ]
  
  parent_dt <- dt[, .(
    year_id, location_id, sex_id, age,
    parent_concept_id = cause_concept_id,
    parent_avp = avp_abs,
    parent_yll = yll_abs
  )]
  
  cmp <- merge(
    child_sum, parent_dt,
    by = c("year_id", "location_id", "sex_id", "age", "parent_concept_id"),
    all = FALSE
  )
  
  cmp[, diff_avp := parent_avp - child_avp_sum]
  cmp[, diff_yll := parent_yll - child_yll_sum]
  
  cmp[, tol_avp := pmax(abs_tol, rel_tol * pmax(abs(parent_avp), abs(child_avp_sum), 1))]
  cmp[, tol_yll := pmax(abs_tol, rel_tol * pmax(abs(parent_yll), abs(child_yll_sum), 1))]
  
  cmp[, pass_avp := abs(diff_avp) <= tol_avp]
  cmp[, pass_yll := abs(diff_yll) <= tol_yll]
  
  fwrite(cmp, file.path(CFG$qc_dir, "qc_cause_hard_compare_avp.csv"))
  
  bad <- cmp[pass_avp == FALSE | pass_yll == FALSE]
  
  if (nrow(bad) > 0L) {
    top <- bad[order(-pmax(abs(diff_avp), abs(diff_yll)))][1:min(.N, 20)]
    stop(
      "QC HARD FAIL: jerarquía AVP/YLL inconsistente dentro de tolerancia. ",
      "Revisar qc_cause_hard_compare_avp.csv\n",
      paste(capture.output(print(top)), collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

tryCatch({
  
  msg("Resolviendo inputs.")
  
  mort_path <- first_existing(CFG$input_mortality_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  
  if (is.na(mort_path)) stop("No encontré mortality_rate_cause_smoothed_reconciled.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  if (!file.exists(CFG$external_yaml_path)) stop("No existe external_sources.yml")
  
  lt_path <- resolve_external_dataset_path(
    key = "life_table_standard_single_age",
    external_yaml_path = CFG$external_yaml_path,
    must_work = TRUE
  )
  if (!file.exists(lt_path)) stop("No existe life_table_standard_single_age: ", lt_path)
  
  msg("Leyendo mortalidad reconciliada.")
  mort <- as.data.table(read_auto(mort_path))
  
  msg("Leyendo cause_master.")
  cm <- as.data.table(read_auto(cause_path))
  
  msg("Leyendo tabla estándar de vida restante.")
  lt <- as.data.table(read_auto(lt_path))
  
  # ----------------------------------------------------------
  # Validaciones y detección de columnas
  # ----------------------------------------------------------
  req_mort <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_level", "population", "deaths_final", "deaths_smoothed_consistent"
  )
  miss_mort <- setdiff(req_mort, names(mort))
  if (length(miss_mort) > 0L) {
    stop("Faltan columnas en mortality_rate_cause_smoothed_reconciled: ",
         paste(miss_mort, collapse = ", "))
  }
  
  req_cm <- c("cause_concept_id", "parent_concept_id", "cause_level")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  lt_age_col <- detect_col(lt, c("exact_age", "age", "age_start"), "edad tabla estándar")
  lt_sex_col <- detect_col(lt, c("sex_id"), "sex_id tabla estándar")
  lt_ex_col <- detect_col(lt, c("ex", "ex_standard", "life_expectancy_remaining"), "ex_standard")
  lt_source_col <- detect_col(lt, c("standard_source"), "standard_source")
  lt_version_col <- detect_col(lt, c("standard_version"), "standard_version")
  
  # ----------------------------------------------------------
  # Estandarización
  # ----------------------------------------------------------
  mort <- mort[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    population = as.numeric(population),
    deaths_final = as.numeric(deaths_final),
    deaths_smoothed_consistent = as.numeric(deaths_smoothed_consistent)
  )][
    year_id %in% CFG$years &
      age >= CFG$age_min &
      age <= CFG$age_max &
      cause_level %in% CFG$keep_cause_levels
  ]
  
  cm <- cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    parent_concept_id = as.integer(parent_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = if ("cause_name" %in% names(cm)) as.character(cause_name) else NA_character_,
    cause_code = if ("cause_code" %in% names(cm)) as.character(cause_code) else NA_character_,
    yll_flag = if ("yll_flag" %in% names(cm)) as.integer(yll_flag) else NA_integer_,
    yld_flag = if ("yld_flag" %in% names(cm)) as.integer(yld_flag) else NA_integer_,
    is_oprm = if ("is_oprm" %in% names(cm)) as.logical(is_oprm) else FALSE,
    is_transversal_pandemic_adjustment = if ("is_transversal_pandemic_adjustment" %in% names(cm)) {
      as.logical(is_transversal_pandemic_adjustment)
    } else FALSE,
    target_age_start_default = if ("target_age_start_default" %in% names(cm)) {
      as.integer(target_age_start_default)
    } else NA_integer_,
    target_age_end_default = if ("target_age_end_default" %in% names(cm)) {
      as.integer(target_age_end_default)
    } else NA_integer_
  )]
  
  lt <- lt[, .(
    sex_id = as.integer(get(lt_sex_col)),
    age = as.integer(get(lt_age_col)),
    ex_standard = as.numeric(get(lt_ex_col)),
    standard_source = as.character(get(lt_source_col)),
    standard_version = as.character(get(lt_version_col))
  )][
    age >= CFG$age_min &
      age <= CFG$age_max
  ]
  
  mort[deaths_final < 0 | is.na(deaths_final), deaths_final := 0]
  mort[deaths_smoothed_consistent < 0 | is.na(deaths_smoothed_consistent), deaths_smoothed_consistent := 0]
  
  mort <- mort[sex_id %in% CFG$valid_sexes]
  
  if (nrow(mort) == 0L) {
    stop("La mortalidad reconciliada quedó vacía tras filtros analíticos.")
  }
  
  # Resolver tabla estándar objetivo de forma explícita
  standard_resolution <- resolve_standard_life_table(lt, CFG)
  lt_available_versions <- standard_resolution$available_versions
  lt <- standard_resolution$selected
  qc_standard_selection_coverage <- standard_resolution$coverage[, .(
    sex_id,
    age,
    standard_source,
    standard_version,
    sex_resolution,
    ex_standard,
    has_match = !is.na(ex_standard)
  )]
  
  # ----------------------------------------------------------
  # Merge
  # ----------------------------------------------------------
  msg("Uniendo muertes reconciliadas con tabla estándar.")
  out <- merge(
    mort,
    lt,
    by = c("sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  out <- merge(
    out,
    unique(cm[, .(
      cause_concept_id,
      cause_name,
      cause_code,
      yll_flag,
      yld_flag,
      is_oprm,
      is_transversal_pandemic_adjustment,
      target_age_start_default,
      target_age_end_default,
      sex_restriction_target_default = if ("sex_restriction_target_default" %in% names(cm)) {
        as.character(sex_restriction_target_default)
      } else NA_character_
    )]),
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # ----------------------------------------------------------
  # Cálculo AVP/YLL
  # ----------------------------------------------------------
  out[, avp_yll_eligible := !is.na(yll_flag) & yll_flag == 1L &
        (!fcoalesce(is_transversal_pandemic_adjustment, FALSE) | fcoalesce(is_oprm, FALSE))]
  
  qc_avp_ineligible_positive <- out[
    avp_yll_eligible == FALSE &
      (deaths_smoothed_consistent > 0 | deaths_final > 0),
    .(
      cause_concept_id, cause_name, cause_code, cause_level,
      year_id, location_id, sex_id, age,
      yll_flag, yld_flag, is_oprm, is_transversal_pandemic_adjustment,
      deaths_final, deaths_smoothed_consistent
    )
  ][order(cause_level, cause_name, year_id, location_id, sex_id, age)]
  
  qc_avp_eligibility_summary <- out[, .(
    total_deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE),
    total_deaths_final = sum(deaths_final, na.rm = TRUE),
    n_rows = .N
  ), by = .(
    avp_yll_eligible,
    cause_level,
    cause_name,
    cause_code,
    yll_flag,
    yld_flag,
    is_oprm,
    is_transversal_pandemic_adjustment
  )][order(avp_yll_eligible, cause_level, cause_name)]
  
  out[, avp_abs := fifelse(avp_yll_eligible, deaths_smoothed_consistent * ex_standard, 0)]
  out[, yll_abs := avp_abs]
  
  out[, avp_abs_from_deaths_final := fifelse(avp_yll_eligible, deaths_final * ex_standard, 0)]
  out[, yll_abs_from_deaths_final := avp_abs_from_deaths_final]
  
  out <- rollup_metric_by_cause(out, cm, "avp_abs")
  out <- rollup_metric_by_cause(out, cm, "yll_abs")
  out <- rollup_metric_by_cause(out, cm, "avp_abs_from_deaths_final")
  out <- rollup_metric_by_cause(out, cm, "yll_abs_from_deaths_final")
  
  out[, avp_rate := safe_rate(avp_abs, population, CFG$rate_multiplier)]
  out[, yll_rate := avp_rate]
  out[, avp_rate_unit := paste0("per_", CFG$rate_multiplier)]
  
  out[, avp_method := "deaths_smoothed_consistent_x_ex_standard_rollup_from_eligible_causes"]
  out[, yll_method := avp_method]
  out[, standard_table_key := paste0(standard_source, "::", standard_version)]
  out[, run_id := run_id]
  
  setorder(out, year_id, location_id, sex_id, age, cause_level, cause_concept_id)
  
  # ----------------------------------------------------------
  # QC mínimo
  # ----------------------------------------------------------
  msg("Corriendo QC mínimo.")
  
  qc_missing_ex <- out[, .(
    n_rows = .N,
    n_missing_ex_standard = sum(is.na(ex_standard)),
    n_missing_avp_abs = sum(is.na(avp_abs)),
    n_missing_avp_rate = sum(is.na(avp_rate))
  )]
  
  qc_nonnegative <- out[, .(
    n_rows = .N,
    n_neg_deaths_smoothed_consistent = sum(deaths_smoothed_consistent < 0, na.rm = TRUE),
    n_neg_ex_standard = sum(ex_standard < 0, na.rm = TRUE),
    n_neg_avp_abs = sum(avp_abs < 0, na.rm = TRUE),
    n_neg_avp_rate = sum(avp_rate < 0, na.rm = TRUE)
  )]
  
  qc_by_level <- out[, .(
    n_rows = .N,
    total_deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE),
    total_avp_abs = sum(avp_abs, na.rm = TRUE)
  ), by = .(cause_level)][order(cause_level)]
  
  qc_year_sex <- out[, .(
    total_deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE),
    total_avp_abs = sum(avp_abs, na.rm = TRUE)
  ), by = .(year_id, sex_id)][order(year_id, sex_id)]
  
  qc_duplicate_pk <- out[, .N, by = .(
    year_id, location_id, sex_id, age, cause_concept_id
  )][N > 1]
  
  plausibility_scope <- out[
    population >= CFG$min_population_for_rate_qc &
      age <= CFG$max_age_for_rate_qc
  ]
  qc_rate_plausibility <- plausibility_scope[, .(
    n_rows = .N,
    max_avp_rate = suppressWarnings(max(avp_rate, na.rm = TRUE)),
    max_yll_rate = suppressWarnings(max(yll_rate, na.rm = TRUE)),
    p99_avp_rate = suppressWarnings(quantile(avp_rate, 0.99, na.rm = TRUE)),
    p999_avp_rate = suppressWarnings(quantile(avp_rate, 0.999, na.rm = TRUE))
  )]
  qc_rate_plausibility_scope <- data.table(
    n_rows_full = nrow(out),
    n_rows_plausibility_scope = nrow(plausibility_scope),
    min_population_for_rate_qc = CFG$min_population_for_rate_qc,
    max_age_for_rate_qc = CFG$max_age_for_rate_qc
  )
  
  qc_top_avp_rate_cells <- out[
    order(-avp_rate)
  ][1:min(.N, CFG$top_n_extremes),
    .(
      year_id, location_id, sex_id, age,
      cause_concept_id, cause_level,
      population, deaths_final, deaths_smoothed_consistent,
      ex_standard, avp_abs, avp_rate
    )]
  qc_top_avp_rate_cells_plausibility_scope <- plausibility_scope[
    order(-avp_rate)
  ][1:min(.N, CFG$top_n_extremes),
    .(
      year_id, location_id, sex_id, age,
      cause_concept_id, cause_level,
      population, deaths_final, deaths_smoothed_consistent,
      ex_standard, avp_abs, avp_rate
    )]
  
  qc_top_avp_abs_cells <- out[
    order(-avp_abs)
  ][1:min(.N, CFG$top_n_extremes),
    .(
      year_id, location_id, sex_id, age,
      cause_concept_id, cause_level,
      population, deaths_final, deaths_smoothed_consistent,
      ex_standard, avp_abs, avp_rate
    )]
  
  qc_ratio_avp_vs_deaths <- out[, .(
    n_rows = .N,
    total_deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE),
    total_avp_abs = sum(avp_abs, na.rm = TRUE),
    weighted_mean_ex = fifelse(
      sum(deaths_smoothed_consistent, na.rm = TRUE) > 0,
      sum(avp_abs, na.rm = TRUE) / sum(deaths_smoothed_consistent, na.rm = TRUE),
      NA_real_
    )
  ), by = .(year_id, sex_id)][order(year_id, sex_id)]
  
  qc_standard_table_summary <- out[, .(
    n_rows = .N,
    n_distinct_standard_key = uniqueN(standard_table_key),
    min_ex = suppressWarnings(min(ex_standard, na.rm = TRUE)),
    max_ex = suppressWarnings(max(ex_standard, na.rm = TRUE))
  )]
  
  qc_standard_versions_available <- copy(lt_available_versions)
  qc_standard_selected <- unique(lt[, .(
    standard_source,
    standard_version,
    sex_resolution
  )])[order(standard_source, standard_version, sex_resolution)]
  
  qc_weighted_ex_by_year <- out[
    cause_concept_id == 9001785 & location_id == CFG$national_additive_id,
    .(
      total_deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE),
      total_avp_abs = sum(avp_abs, na.rm = TRUE),
      weighted_mean_ex = fifelse(
        sum(deaths_smoothed_consistent, na.rm = TRUE) > 0,
        sum(avp_abs, na.rm = TRUE) / sum(deaths_smoothed_consistent, na.rm = TRUE),
        NA_real_
      )
    ),
    by = .(year_id, standard_source, standard_version)
  ][order(year_id)]

  qc_sex_specific_mismatch_positive_avp <- out[
    !is.na(sex_restriction_target_default) & sex_restriction_target_default != "",
    .(
      avp_abs = sum(avp_abs, na.rm = TRUE),
      yll_abs = sum(yll_abs, na.rm = TRUE),
      deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age, cause_concept_id, cause_name, sex_restriction_target_default)
  ]
  qc_sex_specific_mismatch_positive_avp[
    ,
    observed_sex := fifelse(
      sex_id == 8507L, "male",
      fifelse(sex_id == 8532L, "female", "other")
    )
  ]
  qc_sex_specific_mismatch_positive_avp <- qc_sex_specific_mismatch_positive_avp[
    observed_sex != sex_restriction_target_default &
      (avp_abs > 1e-10 | yll_abs > 1e-10 | deaths_smoothed_consistent > 1e-10)
  ][order(-avp_abs, cause_name, year_id, location_id, sex_id, age)]
  qc_age_specific_mismatch_positive_avp <- out[
    !is.na(target_age_start_default) | !is.na(target_age_end_default),
    .(
      avp_abs = sum(avp_abs, na.rm = TRUE),
      yll_abs = sum(yll_abs, na.rm = TRUE),
      deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE)
    ),
    by = .(
      year_id, location_id, sex_id, age, cause_concept_id, cause_name,
      target_age_start_default, target_age_end_default
    )
  ]
  qc_age_specific_mismatch_positive_avp[
    ,
    age_mismatch := (
      (!is.na(target_age_start_default) & age < target_age_start_default) |
        (!is.na(target_age_end_default) & age > target_age_end_default)
    )
  ]
  qc_age_specific_mismatch_positive_avp <- qc_age_specific_mismatch_positive_avp[
    age_mismatch == TRUE &
      (avp_abs > 1e-10 | yll_abs > 1e-10 | deaths_smoothed_consistent > 1e-10)
  ][order(-avp_abs, cause_name, year_id, location_id, sex_id, age)]
  
  # ----------------------------------------------------------
  # Export QC temprano
  # ----------------------------------------------------------
  qc_missing_ex_path <- file.path(CFG$qc_dir, "qc_missing_ex.csv")
  qc_nonnegative_path <- file.path(CFG$qc_dir, "qc_nonnegative.csv")
  qc_by_level_path <- file.path(CFG$qc_dir, "qc_by_level.csv")
  qc_year_sex_path <- file.path(CFG$qc_dir, "qc_year_sex.csv")
  qc_duplicate_pk_path <- file.path(CFG$qc_dir, "qc_duplicate_pk.csv")
  qc_rate_plausibility_path <- file.path(CFG$qc_dir, "qc_rate_plausibility.csv")
  qc_rate_plausibility_scope_path <- file.path(CFG$qc_dir, "qc_rate_plausibility_scope.csv")
  qc_top_avp_rate_cells_path <- file.path(CFG$qc_dir, "qc_top_avp_rate_cells.csv")
  qc_top_avp_rate_cells_plausibility_scope_path <- file.path(CFG$qc_dir, "qc_top_avp_rate_cells_plausibility_scope.csv")
  qc_top_avp_abs_cells_path <- file.path(CFG$qc_dir, "qc_top_avp_abs_cells.csv")
  qc_ratio_avp_vs_deaths_path <- file.path(CFG$qc_dir, "qc_ratio_avp_vs_deaths.csv")
  qc_standard_table_summary_path <- file.path(CFG$qc_dir, "qc_standard_table_summary.csv")
  qc_standard_versions_available_path <- file.path(CFG$qc_dir, "qc_standard_versions_available.csv")
  qc_standard_selected_path <- file.path(CFG$qc_dir, "qc_standard_selected.csv")
  qc_standard_selection_coverage_path <- file.path(CFG$qc_dir, "qc_standard_selection_coverage.csv")
  qc_weighted_ex_by_year_path <- file.path(CFG$qc_dir, "qc_weighted_ex_by_year.csv")
  qc_avp_ineligible_positive_path <- file.path(CFG$qc_dir, "qc_avp_ineligible_positive.csv")
  qc_avp_eligibility_summary_path <- file.path(CFG$qc_dir, "qc_avp_eligibility_summary.csv")
  qc_sex_specific_mismatch_positive_avp_path <- file.path(CFG$qc_dir, "qc_sex_specific_mismatch_positive_avp.csv")
  qc_age_specific_mismatch_positive_avp_path <- file.path(CFG$qc_dir, "qc_age_specific_mismatch_positive_avp.csv")
  
  fwrite(qc_missing_ex, qc_missing_ex_path)
  fwrite(qc_nonnegative, qc_nonnegative_path)
  fwrite(qc_by_level, qc_by_level_path)
  fwrite(qc_year_sex, qc_year_sex_path)
  fwrite(qc_duplicate_pk, qc_duplicate_pk_path)
  fwrite(qc_rate_plausibility, qc_rate_plausibility_path)
  fwrite(qc_rate_plausibility_scope, qc_rate_plausibility_scope_path)
  fwrite(qc_top_avp_rate_cells, qc_top_avp_rate_cells_path)
  fwrite(qc_top_avp_rate_cells_plausibility_scope, qc_top_avp_rate_cells_plausibility_scope_path)
  fwrite(qc_top_avp_abs_cells, qc_top_avp_abs_cells_path)
  fwrite(qc_ratio_avp_vs_deaths, qc_ratio_avp_vs_deaths_path)
  fwrite(qc_standard_table_summary, qc_standard_table_summary_path)
  fwrite(qc_standard_versions_available, qc_standard_versions_available_path)
  fwrite(qc_standard_selected, qc_standard_selected_path)
  fwrite(qc_standard_selection_coverage, qc_standard_selection_coverage_path)
  fwrite(qc_weighted_ex_by_year, qc_weighted_ex_by_year_path)
  fwrite(qc_avp_ineligible_positive, qc_avp_ineligible_positive_path)
  fwrite(qc_avp_eligibility_summary, qc_avp_eligibility_summary_path)
  fwrite(qc_sex_specific_mismatch_positive_avp, qc_sex_specific_mismatch_positive_avp_path)
  fwrite(qc_age_specific_mismatch_positive_avp, qc_age_specific_mismatch_positive_avp_path)
  
  # ----------------------------------------------------------
  # Hard checks estructurales
  # ----------------------------------------------------------
  qc_geo_hard_avp(out, CFG$base_locations, CFG$national_additive_id)
  qc_cause_hard_avp(out, cm)
  
  if (qc_nonnegative$n_neg_deaths_smoothed_consistent[1] > 0L ||
      qc_nonnegative$n_neg_ex_standard[1] > 0L ||
      qc_nonnegative$n_neg_avp_abs[1] > 0L ||
      qc_nonnegative$n_neg_avp_rate[1] > 0L) {
    stop("QC HARD FAIL: hay valores negativos en AVP/YLL.")
  }
  
  if (qc_missing_ex$n_missing_ex_standard[1] > 0L) {
    stop("QC HARD FAIL: faltan valores de ex_standard. Revisar mapeo sexo/edad.")
  }
  
  if (nrow(qc_standard_selected) != 1L ||
      qc_standard_selected$standard_source[1] != CFG$target_standard_source ||
      qc_standard_selected$standard_version[1] != CFG$target_standard_version) {
    stop(
      "QC HARD FAIL: la tabla estándar usada no coincide con el objetivo configurado (",
      CFG$target_standard_source, " / ", CFG$target_standard_version, ")."
    )
  }
  
  if (any(qc_standard_selection_coverage$has_match == FALSE)) {
    stop("QC HARD FAIL: la tabla estándar seleccionada no cubre todas las combinaciones sexo-edad requeridas.")
  }
  
  if (nrow(qc_duplicate_pk) > 0L) {
    stop("QC HARD FAIL: PK duplicada en AVP/YLL reconciliado.")
  }
  if (nrow(qc_sex_specific_mismatch_positive_avp) > 0L) {
    message(
      "ADVERTENCIA QC: AVP/YLL conserva causas sexo-específicas incompatibles. ",
      "Revisar qc_sex_specific_mismatch_positive_avp.csv. ",
      "Estas filas pueden reflejar codificación directa del input y no una fuga del cálculo de AVP."
    )
  }
  if (nrow(qc_age_specific_mismatch_positive_avp) > 0L) {
    message(
      "ADVERTENCIA QC: AVP/YLL conserva causas con dominio etario incompatible. ",
      "Revisar qc_age_specific_mismatch_positive_avp.csv. ",
      "Estas filas pueden reflejar codificacion directa del input y no una fuga del calculo de AVP."
    )
  }
  
  # ----------------------------------------------------------
  # Plausibilidad: warning y hard fail separados
  # ----------------------------------------------------------
  max_yll_rate <- qc_rate_plausibility$max_yll_rate[1]
  
  if (is.finite(max_yll_rate) && max_yll_rate > CFG$max_avp_rate_hard_per_100k) {
    stop(
      sprintf(
        "QC HARD FAIL: max yll_rate = %.4f excede el umbral duro %s. Revisar qc_top_avp_rate_cells.csv",
        max_yll_rate, CFG$max_avp_rate_hard_per_100k
      )
    )
  }
  
  if (is.finite(max_yll_rate) && max_yll_rate > CFG$max_avp_rate_warn_per_100k) {
    warning(
      sprintf(
        "QC WARNING: max yll_rate = %.4f excede el umbral de advertencia %s. Revisar qc_top_avp_rate_cells.csv",
        max_yll_rate, CFG$max_avp_rate_warn_per_100k
      )
    )
  }
  
  # ----------------------------------------------------------
  # Export final
  # ----------------------------------------------------------
  msg("Exportando AVP/YLL reconciliado.")
  out_csv <- file.path(CFG$out_dir, paste0(CFG$table_name, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$table_name, ".parquet"))
  out_dict <- file.path(CFG$out_dir, paste0(CFG$table_name, "_dictionary_ext.csv"))
  
  dict_ext <- build_dictionary_ext(out)
  
  write_csv_parquet(out, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(dict_ext, out_dict)
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(out),
    n_cols = ncol(out),
    notes = "CSV AVP/YLL reconciliado endurecido con plausibilidad y auditoría de extremos"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_parquet,
    n_rows = nrow(out),
    n_cols = ncol(out),
    notes = "Parquet AVP/YLL reconciliado endurecido con plausibilidad y auditoría de extremos"
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
    notes = "Diccionario extendido AVP/YLL reconciliado"
  )
  
  for (p in c(
    qc_missing_ex_path,
    qc_nonnegative_path,
    qc_by_level_path,
    qc_year_sex_path,
    qc_duplicate_pk_path,
    qc_rate_plausibility_path,
    qc_rate_plausibility_scope_path,
    qc_top_avp_rate_cells_path,
    qc_top_avp_rate_cells_plausibility_scope_path,
    qc_top_avp_abs_cells_path,
    qc_ratio_avp_vs_deaths_path,
    qc_standard_table_summary_path,
    qc_avp_ineligible_positive_path,
    qc_avp_eligibility_summary_path,
    qc_sex_specific_mismatch_positive_avp_path,
    qc_age_specific_mismatch_positive_avp_path,
    file.path(CFG$qc_dir, "qc_geo_hard_compare_avp.csv"),
    file.path(CFG$qc_dir, "qc_cause_hard_compare_avp.csv")
  )) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "qc",
      artifact_path = p,
      n_rows = tryCatch(nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(ncol(fread(p)), error = function(e) NA_integer_),
      notes = "QC 10_compute_avp_yll"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "10_compute_avp_yll completado")
  
  msg("OK -> CSV: ", out_csv)
  msg("OK -> Parquet: ", out_parquet)
  msg("OK -> Dictionary: ", out_dict)
  msg("OK -> QC dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
