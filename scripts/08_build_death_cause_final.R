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
  library(yaml)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

CFG <- list(
  version = "v0.2.0_mvp_hardening",
  dataset_id = "death_cause_final",
  table_name = "death_cause_final",
  
  years = 2018:2024,
  prepandemic_years = 2018:2019,
  pandemic_year_min = 2020L,
  
  age_min = 0L,
  age_max = 110L,
  
  min_completeness_factor = 1.0,
  max_completeness_factor = 3.0,
  min_observed_allcause_for_direct_ratio = 5,
  
  verbose = TRUE,
  
  out_dir = here("data", "final", "death_cause_final"),
  qc_dir  = here("data", "derived", "qc", "08_build_death_cause_final"),
  
  leaf_candidates = c(
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
  
  x[, direct_ratio := safe_ratio(expected_allcause, observed_allcause)]
  
  x[
    is.na(direct_ratio) |
      !is.finite(direct_ratio) |
      observed_allcause < CFG$min_observed_allcause_for_direct_ratio,
    direct_ratio := NA_real_
  ]
  
  # resumir prepandemia por sexo-edad-location
  pre_loc <- x[
    year_id %in% CFG$prepandemic_years,
    .(factor_loc = median(direct_ratio, na.rm = TRUE)),
    by = .(location_id, sex_id, age)
  ]
  pre_loc[!is.finite(factor_loc), factor_loc := NA_real_]
  
  pre_sex_age <- x[
    year_id %in% CFG$prepandemic_years,
    .(factor_sex_age = median(direct_ratio, na.rm = TRUE)),
    by = .(sex_id, age)
  ]
  pre_sex_age[!is.finite(factor_sex_age), factor_sex_age := NA_real_]
  
  pre_age <- x[
    year_id %in% CFG$prepandemic_years,
    .(factor_age = median(direct_ratio, na.rm = TRUE)),
    by = .(age)
  ]
  pre_age[!is.finite(factor_age), factor_age := NA_real_]
  
  x <- merge(x, pre_loc, by = c("location_id", "sex_id", "age"), all.x = TRUE, sort = FALSE)
  x <- merge(x, pre_sex_age, by = c("sex_id", "age"), all.x = TRUE, sort = FALSE)
  x <- merge(x, pre_age, by = "age", all.x = TRUE, sort = FALSE)
  
  x[, correction_factor_completeness := fifelse(
    year_id %in% CFG$prepandemic_years & !is.na(direct_ratio),
    direct_ratio,
    fifelse(!is.na(factor_loc), factor_loc,
            fifelse(!is.na(factor_sex_age), factor_sex_age,
                    fifelse(!is.na(factor_age), factor_age, 1.0)))
  )]
  
  x[!is.finite(correction_factor_completeness), correction_factor_completeness := 1.0]
  x[correction_factor_completeness < CFG$min_completeness_factor,
    correction_factor_completeness := CFG$min_completeness_factor]
  x[correction_factor_completeness > CFG$max_completeness_factor,
    correction_factor_completeness := CFG$max_completeness_factor]
  
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
    fwrite(dup_factor, file.path(CFG$qc_dir, "qc_pk_multiple_factors.csv"))
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
  
  pop_path <- resolve_external_path(CFG$external_yaml_path, "population_result")
  ltm_path <- resolve_external_path(CFG$external_yaml_path, "life_table_mortality_single_age")
  
  if (!file.exists(pop_path)) stop("No existe population_result en ruta externa: ", pop_path)
  if (!file.exists(ltm_path)) stop("No existe life_table_mortality_single_age en ruta externa: ", ltm_path)
  
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
  
  req_cm <- c("cause_concept_id", "cause_level", "is_terminal", "is_covid_related")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  pop_col <- detect_col(pop, c("population"), "population")
  ltm_age_col <- detect_col(ltm, c("age_start", "age", "exact_age"), "edad tabla mortalidad")
  ltm_rate_col <- detect_col(ltm, c("mx", "mortality_rate", "mx_interp", "mx_adjusted"), "tasa mortalidad tabla")
  ltm_loc_col <- detect_col(ltm, c("location_id"), "location_id tabla mortalidad")
  ltm_year_col <- detect_col(ltm, c("year_id"), "year_id tabla mortalidad")
  ltm_sex_col <- detect_col(ltm, c("sex_id"), "sex_id tabla mortalidad")
  
  leaf <- copy(leaf)[
    ,
    .(
      year_id = as.integer(year_id),
      location_id = as.integer(location_id),
      sex_id = as.integer(sex_id),
      age = as.integer(age),
      cause_concept_id = as.integer(cause_term_concept_id),
      deaths_post_redistribution = as.numeric(deaths),
      deaths_observed = as.numeric(deaths)
    )
  ][
    year_id %in% CFG$years &
      age >= CFG$age_min &
      age <= CFG$age_max
  ]
  
  cm <- cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    is_terminal = as.logical(is_terminal),
    is_covid_related = as.logical(is_covid_related)
  )]
  
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
  
  leaf[is.na(deaths_post_redistribution), deaths_post_redistribution := 0]
  leaf[deaths_post_redistribution < 0, deaths_post_redistribution := 0]
  leaf[is.na(deaths_observed), deaths_observed := 0]
  leaf[deaths_observed < 0, deaths_observed := 0]
  
  pop <- pop[!is.na(population)]
  pop[population < 0, population := NA_real_]
  
  ltm <- ltm[!is.na(mx)]
  ltm[mx < 0, mx := NA_real_]
  
  cm_term <- cm[is_terminal == TRUE & cause_concept_id > 0]
  leaf <- leaf[cause_concept_id > 0]
  
  leaf <- merge(
    leaf,
    cm_term[, .(cause_concept_id, is_covid_related)],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  leaf[is.na(is_covid_related), is_covid_related := FALSE]
  
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
  
  fac_base <- fill_factor_hierarchical(fac_base)
  fac_base[, observed_corrected_allcause := observed_allcause * correction_factor_completeness]
  
  # ----------------------------------------------------------
  # Exceso pandémico
  # ----------------------------------------------------------
  msg("Calculando exceso pandémico...")
  
  fac_base[, pandemic_excess_allcause := fifelse(
    year_id >= CFG$pandemic_year_min &
      !is.na(observed_corrected_allcause) &
      !is.na(expected_allcause),
    pmax(0, observed_corrected_allcause - expected_allcause),
    0
  )]
  
  # ----------------------------------------------------------
  # Distribución del exceso a causas covid/pandemia
  # ----------------------------------------------------------
  msg("Distribuyendo exceso pandémico...")
  
  covid_causes <- cm_term[is_covid_related == TRUE, cause_concept_id]
  
  pandemic_alloc <- leaf[, .(
    year_id, location_id, sex_id, age, cause_concept_id, is_covid_related,
    base_cause_deaths = deaths_post_redistribution
  )]
  
  if (length(covid_causes) == 0L) {
    warning("No hay causas terminales con is_covid_related == TRUE. pandemic_excess_component quedará en 0.")
    pandemic_alloc[, pandemic_excess_component := 0]
    pandemic_alloc[, covid_weight := 0]
    pandemic_alloc[, covid_base_sum := 0]
  } else {
    pandemic_alloc[
      ,
      covid_base_sum := sum(base_cause_deaths[is_covid_related == TRUE], na.rm = TRUE),
      by = .(year_id, location_id, sex_id, age)
    ]
    
    pandemic_alloc[
      is_covid_related == TRUE & covid_base_sum > 0,
      covid_weight := base_cause_deaths / covid_base_sum
    ]
    
    n_covid_causes <- length(covid_causes)
    pandemic_alloc[
      is_covid_related == TRUE & (is.na(covid_base_sum) | covid_base_sum <= 0),
      covid_weight := 1 / n_covid_causes
    ]
    
    pandemic_alloc[is.na(covid_weight), covid_weight := 0]
    
    pandemic_alloc <- merge(
      pandemic_alloc,
      fac_base[, .(year_id, location_id, sex_id, age, pandemic_excess_allcause)],
      by = c("year_id", "location_id", "sex_id", "age"),
      all.x = TRUE,
      sort = FALSE
    )
    
    pandemic_alloc[is.na(pandemic_excess_allcause), pandemic_excess_allcause := 0]
    pandemic_alloc[, pandemic_excess_component := pandemic_excess_allcause * covid_weight]
  }
  
  pandemic_alloc <- pandemic_alloc[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    covid_base_sum, covid_weight, pandemic_excess_component
  )]
  
  # ----------------------------------------------------------
  # Construcción final
  # ----------------------------------------------------------
  msg("Construyendo tabla final...")
  
  final <- merge(
    leaf[, .(
      year_id, location_id, sex_id, age, cause_concept_id,
      deaths_observed, deaths_post_redistribution
    )],
    fac_base[, .(
      year_id, location_id, sex_id, age,
      correction_factor_completeness,
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
  final[is.na(covid_weight), covid_weight := 0]
  final[is.na(covid_base_sum), covid_base_sum := 0]
  
  final[, deaths_final_net_of_pandemic := deaths_post_redistribution * correction_factor_completeness]
  final[, deaths_final := deaths_final_net_of_pandemic + pandemic_excess_component]
  final[, deaths_final := safe_nonneg(deaths_final)]
  final[, deaths_observed_semantic := "equals_post_redistribution_in_current_mvp"]
  final[, run_id := run_id]
  
  # ----------------------------------------------------------
  # Duplicados accidentales: endurecer control
  # ----------------------------------------------------------
  dup_pk <- final[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]
  
  if (nrow(dup_pk) > 0L) {
    fwrite(dup_pk, file.path(CFG$qc_dir, "qc_duplicate_pk_precollapse.csv"))
    assert_unique_factor_within_pk(final)
    
    final <- final[, .(
      deaths_observed = sum(deaths_observed, na.rm = TRUE),
      deaths_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
      deaths_final_net_of_pandemic = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
      deaths_final = sum(deaths_final, na.rm = TRUE),
      correction_factor_completeness = unique(correction_factor_completeness[!is.na(correction_factor_completeness)])[1],
      pandemic_excess_component = sum(pandemic_excess_component, na.rm = TRUE),
      observed_allcause = unique(observed_allcause[!is.na(observed_allcause)])[1],
      expected_allcause = unique(expected_allcause[!is.na(expected_allcause)])[1],
      observed_corrected_allcause = unique(observed_corrected_allcause[!is.na(observed_corrected_allcause)])[1],
      pandemic_excess_allcause = unique(pandemic_excess_allcause[!is.na(pandemic_excess_allcause)])[1],
      population = unique(population[!is.na(population)])[1],
      mx = unique(mx[!is.na(mx)])[1],
      covid_base_sum = unique(covid_base_sum[!is.na(covid_base_sum)])[1],
      covid_weight = unique(covid_weight[!is.na(covid_weight)])[1],
      deaths_observed_semantic = unique(deaths_observed_semantic)[1],
      run_id = unique(run_id)[1]
    ), by = .(year_id, location_id, sex_id, age, cause_concept_id)]
  }
  
  setcolorder(final, c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "deaths_observed", "deaths_post_redistribution", "deaths_final",
    "correction_factor_completeness", "pandemic_excess_component", "run_id"
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
  
  # 1) QC resumen de factores
  qc_factor_summary <- fac_base[, .(
    n = .N,
    min_factor = suppressWarnings(min(correction_factor_completeness, na.rm = TRUE)),
    p25_factor = suppressWarnings(quantile(correction_factor_completeness, 0.25, na.rm = TRUE)),
    median_factor = suppressWarnings(median(correction_factor_completeness, na.rm = TRUE)),
    p75_factor = suppressWarnings(quantile(correction_factor_completeness, 0.75, na.rm = TRUE)),
    max_factor = suppressWarnings(max(correction_factor_completeness, na.rm = TRUE))
  ), by = .(year_id)][order(year_id)]
  
  # 2) Balance all-cause por celda
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
  
  # 3) Balance anual
  qc_year_summary <- final[, .(
    deaths_post = sum(deaths_post_redistribution, na.rm = TRUE),
    deaths_final_net = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE),
    pandemic_excess = sum(pandemic_excess_component, na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]
  
  # 4) Resumen pandémico por año
  qc_pandemic_by_year <- fac_base[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE),
    pandemic_excess_allcause = sum(pandemic_excess_allcause, na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]
  
  # 5) Faltantes externos
  qc_missing_external <- fac_base[
    is.na(population) | is.na(mx) | is.na(expected_allcause),
    .N,
    by = .(year_id)
  ][order(year_id)]
  
  # 6) Auditoría de completitud por celda
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
  
  # 7) Auditoría de distribución pandémica
  mortality_pandemic_allocation_audit <- final[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    deaths_post_redistribution,
    correction_factor_completeness,
    deaths_final_net_of_pandemic,
    pandemic_excess_component,
    deaths_final,
    covid_base_sum,
    covid_weight,
    run_id
  )][order(year_id, location_id, sex_id, age, cause_concept_id)]
  
  # 8) Factores extremos
  qc_factor_extremes <- mortality_completeness_audit[
    order(-correction_factor_completeness)
  ][1:min(.N, 200)]
  
  # 9) Celdas con exceso pandémico alto
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
  
  # 10) chequeo semántico explícito
  qc_semantic_note <- data.table(
    variable = "deaths_observed",
    semantic_note = "equals_post_redistribution_in_current_mvp",
    run_id = run_id
  )
  
  # Hard checks extra
  if (nrow(final[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]) > 0L) {
    stop("QC HARD FAIL: PK duplicada persiste en death_cause_final.")
  }
  
  if (any(final$deaths_final < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: deaths_final negativas.")
  }
  
  if (any(final$pandemic_excess_component < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: pandemic_excess_component negativo.")
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
  
  fwrite(qc_factor_summary, file.path(CFG$qc_dir, "qc_factor_summary.csv"))
  fwrite(qc_allcause_balance, file.path(CFG$qc_dir, "qc_allcause_balance.csv"))
  fwrite(qc_year_summary, file.path(CFG$qc_dir, "qc_year_summary.csv"))
  fwrite(qc_pandemic_by_year, file.path(CFG$qc_dir, "qc_pandemic_by_year.csv"))
  fwrite(qc_missing_external, file.path(CFG$qc_dir, "qc_missing_external.csv"))
  fwrite(mortality_completeness_audit, file.path(CFG$qc_dir, "mortality_completeness_audit.csv"))
  fwrite(mortality_pandemic_allocation_audit, file.path(CFG$qc_dir, "mortality_pandemic_allocation_audit.csv"))
  fwrite(qc_factor_extremes, file.path(CFG$qc_dir, "qc_factor_extremes.csv"))
  fwrite(qc_pandemic_extremes, file.path(CFG$qc_dir, "qc_pandemic_extremes.csv"))
  fwrite(qc_semantic_note, file.path(CFG$qc_dir, "qc_semantic_note.csv"))
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(final),
    n_cols = ncol(final),
    notes = "CSV final death_cause_final endurecido con QC de completitud y pandemia"
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
    notes = "Parquet final death_cause_final endurecido con QC de completitud y pandemia"
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
  
  for (p in c(
    file.path(CFG$qc_dir, "qc_factor_summary.csv"),
    file.path(CFG$qc_dir, "qc_allcause_balance.csv"),
    file.path(CFG$qc_dir, "qc_year_summary.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_by_year.csv"),
    file.path(CFG$qc_dir, "qc_missing_external.csv"),
    file.path(CFG$qc_dir, "mortality_completeness_audit.csv"),
    file.path(CFG$qc_dir, "mortality_pandemic_allocation_audit.csv"),
    file.path(CFG$qc_dir, "qc_factor_extremes.csv"),
    file.path(CFG$qc_dir, "qc_pandemic_extremes.csv"),
    file.path(CFG$qc_dir, "qc_semantic_note.csv")
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