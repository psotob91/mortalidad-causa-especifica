#!/usr/bin/env Rscript

# ============================================================
# 09_build_mortality_rates.R
# ------------------------------------------------------------
# Objetivo:
#   Construir tasas de mortalidad causa-específica suavizadas
#   a partir de:
#     - death_cause_final_hierarchical
#     - population_result
#
# Enfoque revisado:
#   - Input = deaths_final ya corregidas por subreporte/pandemia
#   - 09 modela SOLO causas terminales
#   - NO vuelve a corregir subreporte
#   - Poisson GAM rápido con mgcv::bam()
#   - sexo dentro del modelo
#   - interacción sexo x edad solo en modelo principal
#   - spline de tiempo simple
#   - efecto aleatorio por región
#   - factor de fase pandémica
#   - recalibración por year_id + sex_id
#   - escalera de escape para causas raras
#   - trazabilidad formal:
#       * mortality_data_sufficiency_audit
#       * mortality_model_attempt_log
#       * mortality_model_registry
#       * mortality_model_coefficients
#       * mortality_model_smooth_terms
#       * mortality_model_fit_metrics
#       * mortality_model_recalibration_factors
#       * mortality_model_heuristic_values
#
# Fallbacks:
#   A: sex + pandemic_phase + s(age) + s(age, by=sex_male) + s(year) + re(region)
#   B: sex + pandemic_phase + s(age) + s(year) + re(region)
#   C: sex + pandemic_phase + age_band + s(year) + re(region)
#   D: sex + period + age_band + re(region)
#   E: borrow desde causa padre ya modelada
#   F: crudo
#
# Salida:
#   mortality_rate_cause_smoothed
#
# PATCH CONSERVADOR:
#   - Armoniza geografía fina de mortalidad a departamento antes del merge con población
#   - Usa pad-left a 6 dígitos y toma los dos primeros como departamento
#   - Re-agrega muertes por year_id + depto + sex_id + age + cause_concept_id
#   - Excluye location_id = 0 de la población para modelamiento departamental
#   - Hace QC duro de preservación de masa y overlap geográfico
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
  library(mgcv)
  library(parallel)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

parse_int_env <- function(name, default) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) return(as.integer(default))
  out <- suppressWarnings(as.integer(raw))
  if (length(out) != 1L || is.na(out)) return(as.integer(default))
  out
}

parse_int_vec_env <- function(name, default) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) return(as.integer(default))
  vals <- trimws(unlist(strsplit(raw, ",")))
  vals <- vals[nzchar(vals)]
  out <- suppressWarnings(as.integer(vals))
  if (length(out) == 0L || any(is.na(out))) return(as.integer(default))
  out
}

detected_cores <- suppressWarnings(parallel::detectCores(logical = FALSE))
default_bam_threads <- if (is.na(detected_cores) || detected_cores <= 1L) {
  1L
} else {
  max(1L, min(2L, detected_cores - 1L))
}

debug_years <- parse_int_vec_env("MORTALITY_DEBUG_YEARS", 2018:2024)
debug_cause_limit <- parse_int_env("MORTALITY_DEBUG_CAUSE_LIMIT", 0L)
output_suffix <- Sys.getenv("MORTALITY_OUTPUT_SUFFIX", unset = "")
output_suffix_path <- if (nzchar(output_suffix)) paste0("_", output_suffix) else ""
death_input_override <- Sys.getenv("MORTALITY_INPUT_DEATH_PATH", unset = "")

CFG <- list(
  version = "v0.4.1_terminal_audited_pandemic_window_guardrails",
  
  dataset_id = "mortality_rate_cause_smoothed",
  table_name = "mortality_rate_cause_smoothed",
  
  years = debug_years,
  sexes = c(8507L, 8532L),
  age_min = 0L,
  age_max = 110L,
  rate_multiplier = 100000,
  
  input_death_candidates = c(
    if (nzchar(death_input_override)) death_input_override,
    here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.parquet"),
    here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  external_yaml_path = here("config", "external_sources.yml"),
  
  out_dir = here("data", "final", paste0("mortality_rate_cause_smoothed", output_suffix_path)),
  qc_dir  = qc_dir_path(paste0("build_mortality_rates", output_suffix_path)),
  
  # clasificación de suficiencia
  deaths_cat_A = 500,
  deaths_cat_B = 100,
  deaths_cat_C = 20,
  mean_deaths_year_cat_A = 30,
  mean_deaths_year_cat_B = 10,
  years_cat_A = 5,
  regions_cat_A = 10,
  
  # umbral de densidad y ceros
  min_data_density_for_A = 0.60,
  min_data_density_for_B = 0.30,
  max_zero_share_for_A = 0.95,
  max_zero_share_for_B = 0.98,
  
  # bam rápido
  bam_nthreads = parse_int_env("MORTALITY_BAM_NTHREADS", default_bam_threads),
  age_k_main = 8,
  age_k_bysex = 6,
  year_k = 4,
  
  # bandas amplias del protocolo
  age_band_breaks = c(-Inf, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf),
  age_band_labels = c("0", "1-4", "5-14", "15-24", "25-34", "35-44",
                      "45-54", "55-64", "65-74", "75-84", "85+"),
  
  # periodos / fases
  period_breaks = c(2018, 2020, 2022, 2025),
  period_labels = c("2018-2019", "2020-2021", "2022-2024"),
  
  # patch geográfico
  model_location_domain = 1:25,
  min_match_rate_population = 0.95,
  mass_tolerance = 1e-6,
  
  # guardarraíles numéricos
  max_rate_per_100k = 50000,
  max_ratio_pred_vs_obs_plus1 = 100,
  validation_holdout_years = c(2022L, 2024L),
  validation_min_train_years = 4L,
  validation_min_holdout_deaths = 1e-8,
  validation_min_improvement = 0.05,

  
  debug_cause_limit = debug_cause_limit,
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

safe_nonneg <- function(x) pmax(0, x)

coalesce_chr <- function(x, y) fifelse(!is.na(x) & nzchar(x), x, y)

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

make_age_band <- function(age) {
  cut(
    age,
    breaks = CFG$age_band_breaks,
    labels = CFG$age_band_labels,
    right = FALSE,
    ordered_result = TRUE
  )
}

make_period <- function(year_id) {
  cut(
    year_id,
    breaks = CFG$period_breaks,
    labels = CFG$period_labels,
    right = FALSE,
    ordered_result = TRUE
  )
}

make_pandemic_phase <- function(year_id) {
  out <- fifelse(year_id <= 2019L, "pre",
                 fifelse(year_id <= 2021L, "pandemic", "post"))
  factor(out, levels = c("pre", "pandemic", "post"), ordered = TRUE)
}

is_valid_fit_vector <- function(x, n_expected) {
  length(x) == n_expected && all(is.finite(x)) && all(x >= 0)
}

recalibrate_within_strata <- function(dt, pred_col, obs_col,
                                      by_cols = c("year_id", "sex_id")) {
  x <- copy(dt)
  x[, pred_tmp := as.numeric(get(pred_col))]
  x[, obs_tmp  := as.numeric(get(obs_col))]
  
  sc <- x[, .(
    pred_sum = sum(pred_tmp, na.rm = TRUE),
    obs_sum  = sum(obs_tmp, na.rm = TRUE)
  ), by = by_cols]
  
  sc[, scalar := fifelse(
    is.finite(pred_sum) & pred_sum > 0 & is.finite(obs_sum) & obs_sum >= 0,
    obs_sum / pred_sum,
    NA_real_
  )]
  
  x <- merge(
    x,
    sc[, c(by_cols, "scalar"), with = FALSE],
    by = by_cols,
    all.x = TRUE,
    sort = FALSE
  )
  
  x[, pred_recal := fifelse(
    !is.na(scalar) & is.finite(scalar) & scalar >= 0,
    pred_tmp * scalar,
    pred_tmp
  )]
  
  x[]
}

extract_model_diagnostics <- function(fit, cause_concept_id, cause_name,
                                      method_code, attempt_order,
                                      formula_text, rows_train, rows_pred,
                                      warning_text, convergence_status,
                                      run_id) {
  if (is.null(fit) || inherits(fit, "fit_error")) {
    empty_coef <- data.table()
    empty_smooth <- data.table()
    empty_metrics <- data.table()
    return(list(coefficients = empty_coef, smooth_terms = empty_smooth, fit_metrics = empty_metrics, summary_text = character()))
  }
  sm <- tryCatch(summary(fit), error = function(e) NULL)
  summary_text <- tryCatch(capture.output(print(sm)), error = function(e) paste("No se pudo capturar summary():", conditionMessage(e)))
  ptab <- if (!is.null(sm) && !is.null(sm$p.table)) as.data.table(sm$p.table, keep.rownames = "term") else data.table()
  if (nrow(ptab)) {
    setnames(ptab, make.names(names(ptab), unique = TRUE))
    ptab[, `:=`(
      cause_concept_id = cause_concept_id,
      cause_name = cause_name,
      method_code = method_code,
      attempt_order = attempt_order,
      formula = formula_text,
      run_id = run_id
    )]
    setcolorder(ptab, c("cause_concept_id", "cause_name", "method_code", "attempt_order", "term", setdiff(names(ptab), c("cause_concept_id", "cause_name", "method_code", "attempt_order", "term"))))
  }
  stab <- if (!is.null(sm) && !is.null(sm$s.table)) as.data.table(sm$s.table, keep.rownames = "smooth_term") else data.table()
  if (nrow(stab)) {
    setnames(stab, make.names(names(stab), unique = TRUE))
    stab[, `:=`(
      cause_concept_id = cause_concept_id,
      cause_name = cause_name,
      method_code = method_code,
      attempt_order = attempt_order,
      formula = formula_text,
      run_id = run_id
    )]
    setcolorder(stab, c("cause_concept_id", "cause_name", "method_code", "attempt_order", "smooth_term", setdiff(names(stab), c("cause_concept_id", "cause_name", "method_code", "attempt_order", "smooth_term"))))
  }
  metrics <- data.table(
    cause_concept_id = cause_concept_id,
    cause_name = cause_name,
    method_code = method_code,
    attempt_order = attempt_order,
    formula = formula_text,
    aic = suppressWarnings(tryCatch(AIC(fit), error = function(e) NA_real_)),
    deviance = suppressWarnings(tryCatch(stats::deviance(fit), error = function(e) NA_real_)),
    null_deviance = suppressWarnings(tryCatch(fit$null.deviance %||% NA_real_, error = function(e) NA_real_)),
    deviance_explained = suppressWarnings(tryCatch(sm$dev.expl %||% NA_real_, error = function(e) NA_real_)),
    df_residual = suppressWarnings(tryCatch(stats::df.residual(fit), error = function(e) NA_real_)),
    n_observations = suppressWarnings(tryCatch(stats::nobs(fit), error = function(e) rows_train)),
    rows_train = as.integer(rows_train),
    rows_pred = as.integer(rows_pred),
    family = suppressWarnings(tryCatch(fit$family$family %||% NA_character_, error = function(e) NA_character_)),
    link = suppressWarnings(tryCatch(fit$family$link %||% NA_character_, error = function(e) NA_character_)),
    convergence_status = convergence_status,
    warning_flag = nzchar(warning_text %||% ""),
    warnings = if (nzchar(warning_text %||% "")) warning_text else NA_character_,
    run_id = run_id
  )
  list(coefficients = ptab, smooth_terms = stab, fit_metrics = metrics, summary_text = summary_text)
}

recalibration_factor_audit <- function(tmp, cause_concept_id, cause_name,
                                       method_code, attempt_order,
                                       formula_text, run_id,
                                       by_cols = c("year_id", "sex_id")) {
  if (!all(c("pred_deaths", "pred_recal", "deaths_final") %in% names(tmp))) return(data.table())
  out <- tmp[, .(
    observed_mass = sum(deaths_final, na.rm = TRUE),
    initial_prediction_mass = sum(pred_deaths, na.rm = TRUE),
    smoothed_final_mass = sum(pred_recal, na.rm = TRUE)
  ), by = by_cols]
  out[, `:=`(
    cause_concept_id = cause_concept_id,
    cause_name = cause_name,
    method_code = method_code,
    attempt_order = attempt_order,
    formula = formula_text,
    recalibration_factor = fifelse(
      is.finite(initial_prediction_mass) & initial_prediction_mass > 0,
      observed_mass / initial_prediction_mass,
      NA_real_
    ),
    final_mass_diff = smoothed_final_mass - observed_mass,
    run_id = run_id
  )]
  setcolorder(out, c("cause_concept_id", "cause_name", "method_code", "attempt_order", by_cols, setdiff(names(out), c("cause_concept_id", "cause_name", "method_code", "attempt_order", by_cols))))
  out[]
}

heuristic_value_audit <- function(base_dt, result_dt, cause_concept_id, cause_name,
                                  method_code, attempt_order, formula_text,
                                  borrowed_from_cause = NA_integer_,
                                  borrowed_from_level = NA_integer_,
                                  rationale = NA_character_,
                                  run_id) {
  data.table(
    cause_concept_id = cause_concept_id,
    cause_name = cause_name,
    method_code = method_code,
    attempt_order = attempt_order,
    heuristic_rule = formula_text,
    borrowed_from_cause = borrowed_from_cause,
    borrowed_from_level = borrowed_from_level,
    mass_input = sum(base_dt$deaths_final, na.rm = TRUE),
    mass_output = if (!is.null(result_dt)) sum(result_dt$deaths_smoothed, na.rm = TRUE) else NA_real_,
    max_abs_year_sex_diff = if (!is.null(result_dt)) {
      chk <- result_dt[, .(obs = sum(deaths_final, na.rm = TRUE), pred = sum(deaths_smoothed, na.rm = TRUE)), by = .(year_id, sex_id)]
      max(abs(chk$pred - chk$obs), na.rm = TRUE)
    } else {
      NA_real_
    },
    rationale = rationale,
    run_id = run_id
  )
}

classify_data_category <- function(total_deaths, mean_deaths_per_year,
                                   years_with_deaths, regions_with_deaths,
                                   data_density, zero_share) {
  if (!is.finite(total_deaths)) total_deaths <- 0
  if (!is.finite(mean_deaths_per_year)) mean_deaths_per_year <- 0
  if (!is.finite(years_with_deaths)) years_with_deaths <- 0
  if (!is.finite(regions_with_deaths)) regions_with_deaths <- 0
  if (!is.finite(data_density)) data_density <- 0
  if (!is.finite(zero_share)) zero_share <- 1
  
  if (total_deaths >= CFG$deaths_cat_A &&
      mean_deaths_per_year >= CFG$mean_deaths_year_cat_A &&
      years_with_deaths >= CFG$years_cat_A &&
      regions_with_deaths >= CFG$regions_cat_A &&
      data_density >= CFG$min_data_density_for_A &&
      zero_share <= CFG$max_zero_share_for_A) {
    return("A")
  }
  if (total_deaths >= CFG$deaths_cat_B &&
      mean_deaths_per_year >= CFG$mean_deaths_year_cat_B &&
      data_density >= CFG$min_data_density_for_B &&
      zero_share <= CFG$max_zero_share_for_B) {
    return("B")
  }
  if (total_deaths >= CFG$deaths_cat_C) {
    return("C")
  }
  "D"
}

choose_candidate_path_v2 <- function(audit_row, has_parent) {
  dc <- audit_row$data_category[1]
  dd <- audit_row$data_density[1]
  zs <- audit_row$zero_share[1]
  
  if (dc == "A") {
    path <- c("A", "B", "C", "D")
  } else if (dc == "B") {
    path <- c("B", "C", "D")
  } else if (dc == "C") {
    path <- c("C", "D")
  } else {
    path <- c("D")
  }
  
  if (is.finite(dd) && dd < 0.10) path <- setdiff(path, c("A", "B"))
  if (is.finite(zs) && zs > 0.995) path <- setdiff(path, c("A", "B"))
  
  if (has_parent) path <- c(path, "E")
  c(path, "F")
}

build_sufficiency_audit <- function(base_dt) {
  years_total <- uniqueN(base_dt$year_id)
  regions_total <- uniqueN(base_dt$location_id)
  sexes_total <- uniqueN(base_dt$sex_id)
  ages_total <- uniqueN(base_dt$age)
  
  rows_expected <- years_total * regions_total * sexes_total * ages_total
  rows_available <- nrow(base_dt)
  
  years_with_deaths <- uniqueN(base_dt[deaths_final > 0, year_id])
  regions_with_deaths <- uniqueN(base_dt[deaths_final > 0, location_id])
  
  yearly <- base_dt[, .(deaths_year = sum(deaths_final, na.rm = TRUE)), by = year_id]
  
  total_deaths <- sum(base_dt$deaths_final, na.rm = TRUE)
  mean_deaths_per_year <- if (nrow(yearly) > 0L) mean(yearly$deaths_year, na.rm = TRUE) else NA_real_
  max_deaths_year <- if (nrow(yearly) > 0L) max(yearly$deaths_year, na.rm = TRUE) else NA_real_
  data_density <- if (rows_expected > 0) rows_available / rows_expected else NA_real_
  zero_share <- mean(base_dt$deaths_final <= 0, na.rm = TRUE)
  
  data_category <- classify_data_category(
    total_deaths = total_deaths,
    mean_deaths_per_year = mean_deaths_per_year,
    years_with_deaths = years_with_deaths,
    regions_with_deaths = regions_with_deaths,
    data_density = data_density,
    zero_share = zero_share
  )
  
  modeling_decision <- fifelse(
    data_category == "A", "candidate_A",
    fifelse(data_category == "B", "candidate_B",
            fifelse(data_category == "C", "candidate_C", "candidate_D_or_fallback"))
  )
  
  data.table(
    cause_concept_id = unique(base_dt$cause_concept_id)[1],
    cause_name = unique(base_dt$cause_name)[1],
    cause_level = unique(base_dt$cause_level)[1],
    is_terminal = unique(base_dt$is_terminal)[1],
    total_deaths = total_deaths,
    mean_deaths_per_year = mean_deaths_per_year,
    max_deaths_year = max_deaths_year,
    years_with_deaths = years_with_deaths,
    years_total = years_total,
    regions_with_deaths = regions_with_deaths,
    regions_total = regions_total,
    rows_available = rows_available,
    rows_expected = rows_expected,
    data_density = data_density,
    zero_share = zero_share,
    data_category = data_category,
    aggregation_age = NA_character_,
    aggregation_year = NA_character_,
    aggregation_region = NA_character_,
    modeling_decision = modeling_decision,
    run_id = run_id
  )
}

fit_model_A <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      sex_f +
      pandemic_phase_f +
      s(age, k = CFG$age_k_main, bs = "cr") +
      s(age, by = sex_male, k = CFG$age_k_bysex, bs = "cr") +
      s(year_id, k = CFG$year_k, bs = "cr") +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

fit_model_B <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      sex_f +
      pandemic_phase_f +
      s(age, k = CFG$age_k_main, bs = "cr") +
      s(year_id, k = CFG$year_k, bs = "cr") +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

fit_model_C <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      sex_f +
      pandemic_phase_f +
      age_band_f +
      s(year_id, k = CFG$year_k, bs = "cr") +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

fit_model_D <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      sex_f +
      period_f +
      age_band_f +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

fit_model_A_NS <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      pandemic_phase_f +
      s(age, k = CFG$age_k_main, bs = "cr") +
      s(year_id, k = CFG$year_k, bs = "cr") +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

fit_model_C_NS <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      pandemic_phase_f +
      age_band_f +
      s(year_id, k = CFG$year_k, bs = "cr") +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

fit_model_D_NS <- function(train_dt) {
  mgcv::bam(
    deaths_final ~
      period_f +
      age_band_f +
      s(location_id, bs = "re") +
      offset(log(population)),
    data = train_dt,
    family = poisson(link = "log"),
    method = "fREML",
    discrete = TRUE,
    nthreads = CFG$bam_nthreads
  )
}

safe_predict_response <- function(fit, newdata) {
  p <- tryCatch(
    predict(fit, newdata = newdata, type = "response"),
    error = function(e) rep(NA_real_, nrow(newdata))
  )
  as.numeric(p)
}

aggregate_for_age_band <- function(dt) {
  dt[, .(
    population = sum(population, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE)
  ), by = .(
    year_id, location_id, sex_id, sex_f, sex_male,
    age_band_f, pandemic_phase_f
  )]
}

aggregate_for_age_band_period <- function(dt) {
  dt[, .(
    population = sum(population, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE)
  ), by = .(
    period_f, location_id, sex_id, sex_f, sex_male, age_band_f
  )]
}

build_cause_grid <- function(pop_grid, cause_deaths, cause_meta) {
  x <- merge(
    pop_grid,
    cause_deaths[, .(year_id, location_id, sex_id, age, deaths_final)],
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  
  x[is.na(deaths_final), deaths_final := 0]
  x[deaths_final < 0, deaths_final := 0]
  
  x[, cause_concept_id := cause_meta$cause_concept_id[1]]
  x[, cause_level := cause_meta$cause_level[1]]
  x[, cause_name := cause_meta$cause_name[1]]
  x[, parent_concept_id := cause_meta$parent_concept_id[1]]
  x[, is_terminal := cause_meta$is_terminal[1]]
  x[, sex_restriction_target_default := cause_meta$sex_restriction_target_default[1]]
  x[, target_age_start_default := cause_meta$target_age_start_default[1]]
  x[, target_age_end_default := cause_meta$target_age_end_default[1]]
  
  x[, sex_chr := fifelse(sex_id == 8507L, "male", "female")]
  x[, sex_f := factor(sex_chr, levels = c("female", "male"))]
  x[, sex_male := as.numeric(sex_chr == "male")]
  
  x[, age_band_f := factor(make_age_band(age), levels = CFG$age_band_labels, ordered = TRUE)]
  x[, period_f := factor(make_period(year_id), levels = CFG$period_labels, ordered = TRUE)]
  x[, pandemic_phase_f := make_pandemic_phase(year_id)]
  
  x
}

split_sex_eligible_grid <- function(base_dt) {
  sex_restr <- unique(base_dt$sex_restriction_target_default)
  sex_restr <- sex_restr[!is.na(sex_restr) & nzchar(sex_restr)]
  if (length(sex_restr) == 0L) {
    return(list(eligible = copy(base_dt), excluded = base_dt[0]))
  }
  sex_restr <- sex_restr[1]
  keep_idx <- if (sex_restr == "male") {
    base_dt$sex_id == 8507L
  } else if (sex_restr == "female") {
    base_dt$sex_id == 8532L
  } else {
    rep(TRUE, nrow(base_dt))
  }
  list(
    eligible = copy(base_dt[keep_idx == TRUE]),
    excluded = copy(base_dt[keep_idx == FALSE])
  )
}

split_age_eligible_grid <- function(base_dt) {
  age_start <- unique(base_dt$target_age_start_default)
  age_end <- unique(base_dt$target_age_end_default)
  age_start <- age_start[!is.na(age_start)]
  age_end <- age_end[!is.na(age_end)]
  if (length(age_start) == 0L && length(age_end) == 0L) {
    return(list(eligible = copy(base_dt), excluded = base_dt[0]))
  }
  age_start <- if (length(age_start) > 0L) age_start[1] else NA_integer_
  age_end <- if (length(age_end) > 0L) age_end[1] else NA_integer_
  keep_idx <- (is.na(age_start) | base_dt$age >= age_start) &
    (is.na(age_end) | base_dt$age <= age_end)
  list(
    eligible = copy(base_dt[keep_idx == TRUE]),
    excluded = copy(base_dt[keep_idx == FALSE])
  )
}

make_result_dt <- function(base_dt, deaths_smoothed, method_code, model_formula_used,
                           training_age_scale, training_time_scale,
                           aggregation_age, aggregation_year, aggregation_region,
                           borrowed_from_level = NA_integer_,
                           borrowed_from_cause = NA_integer_,
                           recalibration_scope = "year_sex",
                           model_status = "ok",
                           convergence_status = "ok",
                           warning_flag = FALSE,
                           data_category = NA_character_,
                           rows_available = NA_integer_,
                           rows_expected = NA_integer_,
                           data_density = NA_real_,
                           years_with_deaths = NA_integer_,
                           regions_with_deaths = NA_integer_) {
  
  out <- copy(base_dt)[, .(
    year_id, location_id, sex_id, age,
    cause_concept_id, cause_level, cause_name, parent_concept_id, is_terminal,
    population, deaths_final
  )]
  
  out[, deaths_smoothed := as.numeric(deaths_smoothed)]
  out[!is.finite(deaths_smoothed) | deaths_smoothed < 0, deaths_smoothed := 0]
  
  out[, mortality_rate_crude := safe_rate(deaths_final, population, CFG$rate_multiplier)]
  out[, mortality_rate_smoothed := safe_rate(deaths_smoothed, population, CFG$rate_multiplier)]
  out[, mortality_rate_unit := paste0("per_", CFG$rate_multiplier)]
  
  out[, model_status := model_status]
  out[, fallback_level := method_code]
  out[, model_formula_used := model_formula_used]
  out[, training_age_scale := training_age_scale]
  out[, training_time_scale := training_time_scale]
  out[, aggregation_age := aggregation_age]
  out[, aggregation_year := aggregation_year]
  out[, aggregation_region := aggregation_region]
  out[, borrowed_from_level := borrowed_from_level]
  out[, borrowed_from_cause_concept_id := borrowed_from_cause]
  out[, recalibration_scope := recalibration_scope]
  out[, convergence_status := convergence_status]
  out[, warning_flag := as.logical(warning_flag)]
  out[, data_category := data_category]
  out[, rows_available := as.integer(rows_available)]
  out[, rows_expected := as.integer(rows_expected)]
  out[, data_density := as.numeric(data_density)]
  out[, years_with_deaths := as.integer(years_with_deaths)]
  out[, regions_with_deaths := as.integer(regions_with_deaths)]
  out[, run_id := run_id]
  
  out[]
}

evaluate_attempt_result <- function(base_dt, result_dt) {
  if (is.null(result_dt)) {
    return(list(ok = FALSE, failure_reason = "null_result", max_abs_mass_diff = NA_real_))
  }
  
  if (nrow(result_dt) != nrow(base_dt)) {
    return(list(ok = FALSE, failure_reason = "row_count_mismatch", max_abs_mass_diff = NA_real_))
  }
  
  if (anyNA(result_dt$deaths_smoothed)) {
    return(list(ok = FALSE, failure_reason = "pred_missing", max_abs_mass_diff = NA_real_))
  }
  
  if (any(!is.finite(result_dt$deaths_smoothed))) {
    return(list(ok = FALSE, failure_reason = "pred_nonfinite", max_abs_mass_diff = NA_real_))
  }
  
  if (any(result_dt$deaths_smoothed < 0)) {
    return(list(ok = FALSE, failure_reason = "pred_negative", max_abs_mass_diff = NA_real_))
  }
  
  if (any(!is.finite(result_dt$mortality_rate_smoothed[!is.na(result_dt$mortality_rate_smoothed)]))) {
    return(list(ok = FALSE, failure_reason = "rate_nonfinite", max_abs_mass_diff = NA_real_))
  }
  
  max_rate <- suppressWarnings(max(result_dt$mortality_rate_smoothed, na.rm = TRUE))
  if (is.finite(max_rate) && max_rate > CFG$max_rate_per_100k) {
    return(list(ok = FALSE, failure_reason = "failed_qc_absurd_rate", max_abs_mass_diff = NA_real_))
  }
  
  obs_max <- max(base_dt$deaths_final, na.rm = TRUE)
  pred_max <- max(result_dt$deaths_smoothed, na.rm = TRUE)
  ratio_pred_obs <- pred_max / (obs_max + 1)
  if (is.finite(ratio_pred_obs) && ratio_pred_obs > CFG$max_ratio_pred_vs_obs_plus1) {
    return(list(ok = FALSE, failure_reason = "failed_qc_absurd_local_explosion", max_abs_mass_diff = NA_real_))
  }
  
  chk <- result_dt[, .(
    obs_sum = sum(deaths_final, na.rm = TRUE),
    pred_sum = sum(deaths_smoothed, na.rm = TRUE)
  ), by = .(year_id, sex_id)]
  
  chk[, diff := pred_sum - obs_sum]
  max_abs_mass_diff <- if (nrow(chk) > 0L) max(abs(chk$diff), na.rm = TRUE) else 0
  
  if (!is.finite(max_abs_mass_diff)) {
    return(list(ok = FALSE, failure_reason = "mass_check_nonfinite", max_abs_mass_diff = NA_real_))
  }
  
  if (max_abs_mass_diff > CFG$mass_tolerance) {
    return(list(ok = FALSE, failure_reason = "mass_not_preserved_within_year_sex", max_abs_mass_diff = max_abs_mass_diff))
  }
  
  list(ok = TRUE, failure_reason = NA_character_, max_abs_mass_diff = max_abs_mass_diff)
}

capture_fit_with_warnings <- function(expr) {
  warnings <- character()
  value <- tryCatch(
    withCallingHandlers(
      expr,
      warning = function(w) {
        warnings <<- c(warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) structure(list(error = conditionMessage(e)), class = "fit_error")
  )
  list(value = value, warnings = unique(warnings))
}

classify_model_warnings <- function(warnings) {
  txt <- paste(unique(na.omit(as.character(warnings))), collapse = " | ")
  if (!nzchar(txt)) return(list(severity = "none", is_critical = FALSE, note = "sin warnings"))
  low <- tolower(txt)
  non_integer_only <- grepl("non-integer x", low) &&
    !grepl("converg|invalid|nonfinite|singular|rank|not defined|error|numerically 0", low)
  if (non_integer_only) {
    return(list(
      severity = "expected_fractional_counts",
      is_critical = FALSE,
      note = "warning esperado: muertes fraccionales tras redistribucion/correccion"
    ))
  }
  critical <- grepl("converg|invalid|nonfinite|singular|rank|not defined|contrasts not defined|prediction_invalid|numerically 0", low)
  list(
    severity = if (critical) "warning_critico" else "warning_no_bloqueante",
    is_critical = critical,
    note = if (critical) "warning potencialmente critico para seleccion de modelo" else "warning no bloqueante documentado"
  )
}

poisson_deviance_score <- function(obs, pred) {
  obs <- as.numeric(obs)
  pred <- pmax(as.numeric(pred), 1e-12)
  keep <- is.finite(obs) & is.finite(pred) & obs >= 0
  if (!any(keep)) return(NA_real_)
  obs <- obs[keep]
  pred <- pred[keep]
  term <- fifelse(obs > 0, obs * log(obs / pred) - (obs - pred), pred)
  2 * sum(term, na.rm = TRUE)
}

summarise_candidate_validation <- function(base_dt, pred, path_code, warnings, failure_reason = NA_character_,
                                           validation_strategy = "temporal_holdout") {
  if (!is_valid_fit_vector(pred, nrow(base_dt))) {
    return(data.table(
      method_code = path_code, validation_strategy = validation_strategy,
      validation_status = "failed", validation_failure_reason = failure_reason %||% "prediction_invalid",
      validation_deviance = NA_real_, validation_mae = NA_real_, validation_rmse = NA_real_,
      validation_bias = NA_real_, holdout_deaths = sum(base_dt$deaths_final, na.rm = TRUE),
      warning_severity = classify_model_warnings(warnings)$severity,
      warning_is_critical = classify_model_warnings(warnings)$is_critical
    ))
  }
  obs <- base_dt$deaths_final
  diff <- pred - obs
  warn <- classify_model_warnings(warnings)
  data.table(
    method_code = path_code,
    validation_strategy = validation_strategy,
    validation_status = "ok",
    validation_failure_reason = NA_character_,
    validation_deviance = poisson_deviance_score(obs, pred),
    validation_mae = mean(abs(diff), na.rm = TRUE),
    validation_rmse = sqrt(mean(diff^2, na.rm = TRUE)),
    validation_bias = sum(diff, na.rm = TRUE),
    holdout_deaths = sum(obs, na.rm = TRUE),
    warning_severity = warn$severity,
    warning_is_critical = warn$is_critical
  )
}

fit_predict_for_validation <- function(path_code, train_dt, holdout_dt) {
  if (path_code == "A") {
    cap <- capture_fit_with_warnings(fit_model_A(train_dt))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code == "B") {
    cap <- capture_fit_with_warnings(fit_model_B(train_dt))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code == "C") {
    cap <- capture_fit_with_warnings(fit_model_C(aggregate_for_age_band(train_dt)))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code == "D") {
    cap <- capture_fit_with_warnings(fit_model_D(aggregate_for_age_band_period(train_dt)))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code == "A_NS") {
    cap <- capture_fit_with_warnings(fit_model_A_NS(train_dt))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code == "C_NS") {
    cap <- capture_fit_with_warnings(fit_model_C_NS(aggregate_for_age_band(train_dt)))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code == "D_NS") {
    cap <- capture_fit_with_warnings(fit_model_D_NS(aggregate_for_age_band_period(train_dt)))
    if (inherits(cap$value, "fit_error")) return(list(pred = NULL, warnings = cap$warnings, failure = cap$value$error))
    return(list(pred = safe_predict_response(cap$value, holdout_dt), warnings = cap$warnings, failure = NA_character_))
  }
  if (path_code %in% c("E", "F")) {
    return(list(pred = NULL, warnings = character(), failure = "fallback_not_competed_in_holdout"))
  }
  list(pred = NULL, warnings = character(), failure = "unknown_path")
}

candidate_paths_for_competition <- function(paths) {
  out <- unique(paths)
  if ("A" %in% out) out <- c(out, "A_NS")
  if ("C" %in% out) out <- c(out, "C_NS")
  if ("D" %in% out) out <- c(out, "D_NS")
  unique(out)
}

validate_candidate_paths <- function(base_dt, paths) {
  holdout_years <- intersect(CFG$validation_holdout_years, unique(base_dt$year_id))
  train_dt <- base_dt[!(year_id %in% holdout_years)]
  holdout_dt <- base_dt[year_id %in% holdout_years]
  if (uniqueN(train_dt$year_id) < CFG$validation_min_train_years ||
      nrow(holdout_dt) == 0L ||
      sum(holdout_dt$deaths_final, na.rm = TRUE) <= CFG$validation_min_holdout_deaths) {
    holdout_years <- max(unique(base_dt$year_id), na.rm = TRUE)
    train_dt <- base_dt[year_id != holdout_years]
    holdout_dt <- base_dt[year_id == holdout_years]
  }
  strategy <- paste0("temporal_holdout_years_", paste(sort(unique(holdout_dt$year_id)), collapse = "_"))
  if (uniqueN(train_dt$year_id) < CFG$validation_min_train_years || nrow(holdout_dt) == 0L) {
    return(data.table(
      method_code = paths,
      validation_strategy = "insufficient_holdout",
      validation_status = "not_run",
      validation_failure_reason = "insufficient_temporal_support",
      validation_deviance = NA_real_, validation_mae = NA_real_, validation_rmse = NA_real_,
      validation_bias = NA_real_, holdout_deaths = sum(holdout_dt$deaths_final, na.rm = TRUE),
      warning_severity = "not_evaluated", warning_is_critical = FALSE
    ))
  }
  rbindlist(lapply(paths, function(path_code) {
    fp <- fit_predict_for_validation(path_code, train_dt, holdout_dt)
    summarise_candidate_validation(holdout_dt, fp$pred, path_code, fp$warnings, fp$failure, strategy)
  }), use.names = TRUE, fill = TRUE)
}

choose_competition_winner <- function(validation_dt, path_order) {
  ok <- validation_dt[
    validation_status == "ok" &
      is.finite(validation_deviance) &
      warning_is_critical == FALSE
  ][order(validation_deviance, validation_mae, match(method_code, path_order))]
  if (nrow(ok) == 0L) {
    return(list(method_code = NA_character_, reason = "sin_candidato_validado_no_critico"))
  }
  legacy <- path_order[1]
  legacy_row <- ok[method_code == legacy][1]
  best <- ok[1]
  if (nrow(legacy_row) == 1L && is.finite(legacy_row$validation_deviance) &&
      is.finite(best$validation_deviance)) {
    improvement <- (legacy_row$validation_deviance - best$validation_deviance) / legacy_row$validation_deviance
    if (best$method_code != legacy && improvement < CFG$validation_min_improvement) {
      return(list(method_code = legacy, reason = sprintf("conserva_legacy_mejora_%.3f_menor_umbral", improvement)))
    }
  }
  list(method_code = best$method_code, reason = "mejor_validacion_temporal_no_critica")
}

prediction_diagnostics_audit <- function(tmp, cause_concept_id, cause_name,
                                         method_code, attempt_order, formula_text,
                                         run_id, by_cols) {
  if (!all(c("pred_deaths", "pred_recal", "deaths_final") %in% names(tmp))) return(data.table())
  out <- tmp[, .(
    observed = sum(deaths_final, na.rm = TRUE),
    pred_initial = sum(pred_deaths, na.rm = TRUE),
    pred_recalibrated = sum(pred_recal, na.rm = TRUE)
  ), by = by_cols]
  out[, `:=`(
    cause_concept_id = cause_concept_id,
    cause_name = cause_name,
    method_code = method_code,
    attempt_order = attempt_order,
    formula = formula_text,
    recalibration_factor = fifelse(is.finite(pred_initial) & pred_initial > 0, observed / pred_initial, NA_real_),
    diff_initial = pred_initial - observed,
    diff_recalibrated = pred_recalibrated - observed,
    resid_proxy_initial = (observed - pred_initial) / sqrt(pmax(pred_initial, 0) + 1e-6),
    resid_proxy_recalibrated = (observed - pred_recalibrated) / sqrt(pmax(pred_recalibrated, 0) + 1e-6),
    run_id = run_id
  )]
  setcolorder(out, c("cause_concept_id", "cause_name", "method_code", "attempt_order", by_cols, setdiff(names(out), c("cause_concept_id", "cause_name", "method_code", "attempt_order", by_cols))))
  out[]
}

try_fit_path <- function(path_code, base_dt, audit_row, parent_result = NULL,
                         attempt_order = NA_integer_) {
  warn_text <- character()
  conv_status <- "ok"
  cid <- unique(base_dt$cause_concept_id)[1]
  cname <- unique(base_dt$cause_name)[1]

  if (path_code %in% c("A_NS", "C_NS", "D_NS")) {
    if (path_code == "A_NS") {
      fit_cap <- capture_fit_with_warnings(fit_model_A_NS(base_dt))
      formula_text <- "pandemic_phase + s(age) + s(year) + re(region) + offset(log(pop))"
      aggregation_age <- "single_age"
      aggregation_year <- "single_year"
      aggregation_region <- "department_partial_pooling"
      rows_train <- nrow(base_dt)
    } else if (path_code == "C_NS") {
      train_ns <- aggregate_for_age_band(base_dt)
      fit_cap <- capture_fit_with_warnings(fit_model_C_NS(train_ns))
      formula_text <- "pandemic_phase + age_band + s(year) + re(region) + offset(log(pop))"
      aggregation_age <- "age_band_protocol"
      aggregation_year <- "single_year"
      aggregation_region <- "department_partial_pooling"
      rows_train <- nrow(train_ns)
    } else {
      train_ns <- aggregate_for_age_band_period(base_dt)
      fit_cap <- capture_fit_with_warnings(fit_model_D_NS(train_ns))
      formula_text <- "period + age_band + re(region) + offset(log(pop))"
      aggregation_age <- "age_band_protocol"
      aggregation_year <- "period_2018_2019__2020_2021__2022_2024"
      aggregation_region <- "department_partial_pooling"
      rows_train <- nrow(train_ns)
    }
    warn_text <- fit_cap$warnings
    if (inherits(fit_cap$value, "fit_error")) {
      return(list(result = NULL, convergence_status = "error", warnings = warn_text,
                  failure_reason = fit_cap$value$error,
                  formula = formula_text,
                  aggregation_age = aggregation_age,
                  aggregation_year = aggregation_year,
                  aggregation_region = aggregation_region,
                  rows_train = rows_train))
    }
    fit <- fit_cap$value
    pred <- safe_predict_response(fit, base_dt)
    if (!is_valid_fit_vector(pred, nrow(base_dt))) {
      return(list(result = NULL, convergence_status = "invalid_prediction", warnings = warn_text,
                  failure_reason = "prediction_invalid",
                  formula = formula_text,
                  aggregation_age = aggregation_age,
                  aggregation_year = aggregation_year,
                  aggregation_region = aggregation_region,
                  rows_train = rows_train))
    }
    tmp <- copy(base_dt)
    tmp[, pred_deaths := pred]
    tmp <- recalibrate_within_strata(tmp, "pred_deaths", "deaths_final", by_cols = c("year_id", "sex_id"))
    res <- make_result_dt(
      base_dt = base_dt,
      deaths_smoothed = tmp$pred_recal,
      method_code = path_code,
      model_formula_used = formula_text,
      training_age_scale = aggregation_age,
      training_time_scale = aggregation_year,
      aggregation_age = aggregation_age,
      aggregation_year = aggregation_year,
      aggregation_region = aggregation_region,
      model_status = "ok",
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      warning_flag = length(warn_text) > 0L,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    diag <- extract_model_diagnostics(
      fit, cid, cname, path_code, attempt_order, formula_text,
      rows_train = rows_train, rows_pred = nrow(base_dt),
      warning_text = paste(unique(warn_text), collapse = " | "),
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      run_id = run_id
    )
    recal <- recalibration_factor_audit(tmp, cid, cname, path_code, attempt_order, formula_text, run_id)
    return(list(result = res, convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
                warnings = warn_text, failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = aggregation_age,
                aggregation_year = aggregation_year,
                aggregation_region = aggregation_region,
                rows_train = rows_train,
                coefficients = diag$coefficients,
                smooth_terms = diag$smooth_terms,
                fit_metrics = diag$fit_metrics,
                recalibration_factors = recal,
                heuristic_values = data.table(),
                model_summary_text = diag$summary_text,
                prediction_diag_year_sex = prediction_diagnostics_audit(tmp, cid, cname, path_code, attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(tmp, cid, cname, path_code, attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  if (path_code == "A") {
    fit_cap <- capture_fit_with_warnings(fit_model_A(base_dt))
    warn_text <- fit_cap$warnings
    if (inherits(fit_cap$value, "fit_error")) {
      return(list(result = NULL, convergence_status = "error", warnings = warn_text,
                  failure_reason = fit_cap$value$error,
                  formula = "sex + pandemic_phase + s(age) + s(age,by=sex_male) + s(year) + re(region) + offset(log(pop))",
                  aggregation_age = "single_age",
                  aggregation_year = "single_year",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(base_dt)))
    }
    fit <- fit_cap$value
    pred <- safe_predict_response(fit, base_dt)
    if (!is_valid_fit_vector(pred, nrow(base_dt))) {
      return(list(result = NULL, convergence_status = "invalid_prediction", warnings = warn_text,
                  failure_reason = "prediction_invalid",
                  formula = "sex + pandemic_phase + s(age) + s(age,by=sex_male) + s(year) + re(region) + offset(log(pop))",
                  aggregation_age = "single_age",
                  aggregation_year = "single_year",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(base_dt)))
    }
    
    tmp <- copy(base_dt)
    tmp[, pred_deaths := pred]
    tmp <- recalibrate_within_strata(tmp, "pred_deaths", "deaths_final", by_cols = c("year_id", "sex_id"))
    
    res <- make_result_dt(
      base_dt = base_dt,
      deaths_smoothed = tmp$pred_recal,
      method_code = "A",
      model_formula_used = "sex + pandemic_phase + s(age) + s(age,by=sex_male) + s(year) + re(region) + offset(log(pop))",
      training_age_scale = "single_age",
      training_time_scale = "single_year",
      aggregation_age = "single_age",
      aggregation_year = "single_year",
      aggregation_region = "department_partial_pooling",
      model_status = "ok",
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      warning_flag = length(warn_text) > 0L,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    
    formula_text <- "sex + pandemic_phase + s(age) + s(age,by=sex_male) + s(year) + re(region) + offset(log(pop))"
    diag <- extract_model_diagnostics(
      fit, cid, cname, "A", attempt_order, formula_text,
      rows_train = nrow(base_dt), rows_pred = nrow(base_dt),
      warning_text = paste(unique(warn_text), collapse = " | "),
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      run_id = run_id
    )
    recal <- recalibration_factor_audit(tmp, cid, cname, "A", attempt_order, formula_text, run_id)
    return(list(result = res, convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
                warnings = warn_text, failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = "single_age",
                aggregation_year = "single_year",
                aggregation_region = "department_partial_pooling",
                rows_train = nrow(base_dt),
                coefficients = diag$coefficients,
                smooth_terms = diag$smooth_terms,
                fit_metrics = diag$fit_metrics,
                recalibration_factors = recal,
                heuristic_values = data.table(),
                model_summary_text = diag$summary_text,
                prediction_diag_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "A", attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "A", attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  if (path_code == "B") {
    fit_cap <- capture_fit_with_warnings(fit_model_B(base_dt))
    warn_text <- fit_cap$warnings
    if (inherits(fit_cap$value, "fit_error")) {
      return(list(result = NULL, convergence_status = "error", warnings = warn_text,
                  failure_reason = fit_cap$value$error,
                  formula = "sex + pandemic_phase + s(age) + s(year) + re(region) + offset(log(pop))",
                  aggregation_age = "single_age",
                  aggregation_year = "single_year",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(base_dt)))
    }
    fit <- fit_cap$value
    pred <- safe_predict_response(fit, base_dt)
    if (!is_valid_fit_vector(pred, nrow(base_dt))) {
      return(list(result = NULL, convergence_status = "invalid_prediction", warnings = warn_text,
                  failure_reason = "prediction_invalid",
                  formula = "sex + pandemic_phase + s(age) + s(year) + re(region) + offset(log(pop))",
                  aggregation_age = "single_age",
                  aggregation_year = "single_year",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(base_dt)))
    }
    
    tmp <- copy(base_dt)
    tmp[, pred_deaths := pred]
    tmp <- recalibrate_within_strata(tmp, "pred_deaths", "deaths_final", by_cols = c("year_id", "sex_id"))
    
    res <- make_result_dt(
      base_dt = base_dt,
      deaths_smoothed = tmp$pred_recal,
      method_code = "B",
      model_formula_used = "sex + pandemic_phase + s(age) + s(year) + re(region) + offset(log(pop))",
      training_age_scale = "single_age",
      training_time_scale = "single_year",
      aggregation_age = "single_age",
      aggregation_year = "single_year",
      aggregation_region = "department_partial_pooling",
      model_status = "ok",
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      warning_flag = length(warn_text) > 0L,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    
    formula_text <- "sex + pandemic_phase + s(age) + s(year) + re(region) + offset(log(pop))"
    diag <- extract_model_diagnostics(
      fit, cid, cname, "B", attempt_order, formula_text,
      rows_train = nrow(base_dt), rows_pred = nrow(base_dt),
      warning_text = paste(unique(warn_text), collapse = " | "),
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      run_id = run_id
    )
    recal <- recalibration_factor_audit(tmp, cid, cname, "B", attempt_order, formula_text, run_id)
    return(list(result = res, convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
                warnings = warn_text, failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = "single_age",
                aggregation_year = "single_year",
                aggregation_region = "department_partial_pooling",
                rows_train = nrow(base_dt),
                coefficients = diag$coefficients,
                smooth_terms = diag$smooth_terms,
                fit_metrics = diag$fit_metrics,
                recalibration_factors = recal,
                heuristic_values = data.table(),
                model_summary_text = diag$summary_text,
                prediction_diag_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "B", attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "B", attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  if (path_code == "C") {
    train_c <- aggregate_for_age_band(base_dt)
    fit_cap <- capture_fit_with_warnings(fit_model_C(train_c))
    warn_text <- fit_cap$warnings
    if (inherits(fit_cap$value, "fit_error")) {
      return(list(result = NULL, convergence_status = "error", warnings = warn_text,
                  failure_reason = fit_cap$value$error,
                  formula = "sex + pandemic_phase + age_band + s(year) + re(region) + offset(log(pop))",
                  aggregation_age = "age_band_protocol",
                  aggregation_year = "single_year",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(train_c)))
    }
    fit <- fit_cap$value
    pred <- safe_predict_response(fit, base_dt)
    if (!is_valid_fit_vector(pred, nrow(base_dt))) {
      return(list(result = NULL, convergence_status = "invalid_prediction", warnings = warn_text,
                  failure_reason = "prediction_invalid",
                  formula = "sex + pandemic_phase + age_band + s(year) + re(region) + offset(log(pop))",
                  aggregation_age = "age_band_protocol",
                  aggregation_year = "single_year",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(train_c)))
    }
    
    tmp <- copy(base_dt)
    tmp[, pred_deaths := pred]
    tmp <- recalibrate_within_strata(tmp, "pred_deaths", "deaths_final", by_cols = c("year_id", "sex_id"))
    
    res <- make_result_dt(
      base_dt = base_dt,
      deaths_smoothed = tmp$pred_recal,
      method_code = "C",
      model_formula_used = "sex + pandemic_phase + age_band + s(year) + re(region) + offset(log(pop))",
      training_age_scale = "age_band_protocol",
      training_time_scale = "single_year",
      aggregation_age = "age_band_protocol",
      aggregation_year = "single_year",
      aggregation_region = "department_partial_pooling",
      model_status = "ok",
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      warning_flag = length(warn_text) > 0L,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    
    formula_text <- "sex + pandemic_phase + age_band + s(year) + re(region) + offset(log(pop))"
    diag <- extract_model_diagnostics(
      fit, cid, cname, "C", attempt_order, formula_text,
      rows_train = nrow(train_c), rows_pred = nrow(base_dt),
      warning_text = paste(unique(warn_text), collapse = " | "),
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      run_id = run_id
    )
    recal <- recalibration_factor_audit(tmp, cid, cname, "C", attempt_order, formula_text, run_id)
    return(list(result = res, convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
                warnings = warn_text, failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = "age_band_protocol",
                aggregation_year = "single_year",
                aggregation_region = "department_partial_pooling",
                rows_train = nrow(train_c),
                coefficients = diag$coefficients,
                smooth_terms = diag$smooth_terms,
                fit_metrics = diag$fit_metrics,
                recalibration_factors = recal,
                heuristic_values = data.table(),
                model_summary_text = diag$summary_text,
                prediction_diag_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "C", attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "C", attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  if (path_code == "D") {
    train_d <- aggregate_for_age_band_period(base_dt)
    fit_cap <- capture_fit_with_warnings(fit_model_D(train_d))
    warn_text <- fit_cap$warnings
    if (inherits(fit_cap$value, "fit_error")) {
      return(list(result = NULL, convergence_status = "error", warnings = warn_text,
                  failure_reason = fit_cap$value$error,
                  formula = "sex + period + age_band + re(region) + offset(log(pop))",
                  aggregation_age = "age_band_protocol",
                  aggregation_year = "period_2018_2019__2020_2021__2022_2024",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(train_d)))
    }
    fit <- fit_cap$value
    pred <- safe_predict_response(fit, base_dt)
    if (!is_valid_fit_vector(pred, nrow(base_dt))) {
      return(list(result = NULL, convergence_status = "invalid_prediction", warnings = warn_text,
                  failure_reason = "prediction_invalid",
                  formula = "sex + period + age_band + re(region) + offset(log(pop))",
                  aggregation_age = "age_band_protocol",
                  aggregation_year = "period_2018_2019__2020_2021__2022_2024",
                  aggregation_region = "department_partial_pooling",
                  rows_train = nrow(train_d)))
    }
    
    tmp <- copy(base_dt)
    tmp[, pred_deaths := pred]
    tmp <- recalibrate_within_strata(tmp, "pred_deaths", "deaths_final", by_cols = c("year_id", "sex_id"))
    
    res <- make_result_dt(
      base_dt = base_dt,
      deaths_smoothed = tmp$pred_recal,
      method_code = "D",
      model_formula_used = "sex + period + age_band + re(region) + offset(log(pop))",
      training_age_scale = "age_band_protocol",
      training_time_scale = "period_2018_2019__2020_2021__2022_2024",
      aggregation_age = "age_band_protocol",
      aggregation_year = "period_2018_2019__2020_2021__2022_2024",
      aggregation_region = "department_partial_pooling",
      model_status = "ok",
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      warning_flag = length(warn_text) > 0L,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    
    formula_text <- "sex + period + age_band + re(region) + offset(log(pop))"
    diag <- extract_model_diagnostics(
      fit, cid, cname, "D", attempt_order, formula_text,
      rows_train = nrow(train_d), rows_pred = nrow(base_dt),
      warning_text = paste(unique(warn_text), collapse = " | "),
      convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
      run_id = run_id
    )
    recal <- recalibration_factor_audit(tmp, cid, cname, "D", attempt_order, formula_text, run_id)
    return(list(result = res, convergence_status = ifelse(length(warn_text) > 0L, "warning", "ok"),
                warnings = warn_text, failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = "age_band_protocol",
                aggregation_year = "period_2018_2019__2020_2021__2022_2024",
                aggregation_region = "department_partial_pooling",
                rows_train = nrow(train_d),
                coefficients = diag$coefficients,
                smooth_terms = diag$smooth_terms,
                fit_metrics = diag$fit_metrics,
                recalibration_factors = recal,
                heuristic_values = data.table(),
                model_summary_text = diag$summary_text,
                prediction_diag_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "D", attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "D", attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  if (path_code == "E") {
    if (is.null(parent_result) || nrow(parent_result) == 0L) {
      return(list(result = NULL, convergence_status = "parent_missing", warnings = character(),
                  failure_reason = "borrow_parent_unavailable",
                  formula = "borrow_parent_shape_rescaled_within_year_sex",
                  aggregation_age = "borrow_parent",
                  aggregation_year = "borrow_parent",
                  aggregation_region = "borrow_parent_shape",
                  rows_train = nrow(base_dt)))
    }
    
    par <- parent_result[, .(
      year_id, location_id, sex_id, age,
      parent_deaths_smoothed = deaths_smoothed,
      borrowed_from_level = cause_level[1],
      borrowed_from_cause = cause_concept_id[1]
    )]
    
    x <- merge(
      base_dt,
      par,
      by = c("year_id", "location_id", "sex_id", "age"),
      all.x = TRUE,
      sort = FALSE
    )
    
    x[is.na(parent_deaths_smoothed), parent_deaths_smoothed := 0]
    x[, pred_deaths := parent_deaths_smoothed]
    
    x <- recalibrate_within_strata(
      dt = x,
      pred_col = "pred_deaths",
      obs_col = "deaths_final",
      by_cols = c("year_id", "sex_id")
    )
    
    res <- make_result_dt(
      base_dt = x,
      deaths_smoothed = x$pred_recal,
      method_code = "E",
      model_formula_used = "borrow_parent_shape_rescaled_within_year_sex",
      training_age_scale = "borrow_parent",
      training_time_scale = "borrow_parent",
      aggregation_age = "borrow_parent",
      aggregation_year = "borrow_parent",
      aggregation_region = "borrow_parent_shape",
      borrowed_from_level = unique(na.omit(x$borrowed_from_level))[1],
      borrowed_from_cause = unique(na.omit(x$borrowed_from_cause))[1],
      model_status = "ok",
      convergence_status = "borrowed",
      warning_flag = FALSE,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    
    formula_text <- "borrow_parent_shape_rescaled_within_year_sex"
    recal <- recalibration_factor_audit(x, cid, cname, "E", attempt_order, formula_text, run_id)
    heur <- heuristic_value_audit(
      base_dt, res, cid, cname, "E", attempt_order, formula_text,
      borrowed_from_cause = unique(na.omit(x$borrowed_from_cause))[1],
      borrowed_from_level = unique(na.omit(x$borrowed_from_level))[1],
      rationale = "Se presto la forma de la causa padre ya modelada y se reescalo por year_id + sex_id.",
      run_id = run_id
    )
    return(list(result = res, convergence_status = "borrowed", warnings = character(),
                failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = "borrow_parent",
                aggregation_year = "borrow_parent",
                aggregation_region = "borrow_parent_shape",
                rows_train = nrow(base_dt),
                coefficients = data.table(),
                smooth_terms = data.table(),
                fit_metrics = data.table(),
                recalibration_factors = recal,
                heuristic_values = heur,
                model_summary_text = character(),
                prediction_diag_year_sex = prediction_diagnostics_audit(x, cid, cname, "E", attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(x, cid, cname, "E", attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  if (path_code == "F") {
    res <- make_result_dt(
      base_dt = base_dt,
      deaths_smoothed = base_dt$deaths_final,
      method_code = "F",
      model_formula_used = "raw_no_model",
      training_age_scale = "single_age",
      training_time_scale = "single_year",
      aggregation_age = "single_age",
      aggregation_year = "single_year",
      aggregation_region = "none_raw",
      model_status = "fallback_raw",
      convergence_status = "not_applicable",
      warning_flag = FALSE,
      data_category = audit_row$data_category[1],
      rows_available = audit_row$rows_available[1],
      rows_expected = audit_row$rows_expected[1],
      data_density = audit_row$data_density[1],
      years_with_deaths = audit_row$years_with_deaths[1],
      regions_with_deaths = audit_row$regions_with_deaths[1]
    )
    
    formula_text <- "raw_no_model"
    tmp <- copy(base_dt)
    tmp[, `:=`(pred_deaths = deaths_final, pred_recal = deaths_final)]
    recal <- recalibration_factor_audit(tmp, cid, cname, "F", attempt_order, formula_text, run_id)
    heur <- heuristic_value_audit(
      base_dt, res, cid, cname, "F", attempt_order, formula_text,
      rationale = "Fallback crudo: se conserva el valor observado porque modelar seria menos seguro.",
      run_id = run_id
    )
    return(list(result = res, convergence_status = "not_applicable", warnings = character(),
                failure_reason = NA_character_,
                formula = formula_text,
                aggregation_age = "single_age",
                aggregation_year = "single_year",
                aggregation_region = "none_raw",
                rows_train = nrow(base_dt),
                coefficients = data.table(),
                smooth_terms = data.table(),
                fit_metrics = data.table(),
                recalibration_factors = recal,
                heuristic_values = heur,
                model_summary_text = character(),
                prediction_diag_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "F", attempt_order, formula_text, run_id, c("year_id", "sex_id")),
                prediction_diag_age_year_sex = prediction_diagnostics_audit(tmp, cid, cname, "F", attempt_order, formula_text, run_id, c("year_id", "sex_id", "age"))))
  }
  
  list(result = NULL, convergence_status = "unknown", warnings = character(),
       failure_reason = "unknown_path",
       formula = NA_character_, aggregation_age = NA_character_,
       aggregation_year = NA_character_, aggregation_region = NA_character_,
       rows_train = nrow(base_dt))
}

tryCatch({
  
  msg("Resolviendo inputs.")
  death_path <- first_existing(CFG$input_death_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  
  if (is.na(death_path)) stop("No encontré death_cause_final_hierarchical.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  if (!file.exists(CFG$external_yaml_path)) stop("No existe external_sources.yml")
  
  pop_path <- resolve_external_dataset_path(
    key = "population_result",
    external_yaml_path = CFG$external_yaml_path,
    must_work = TRUE
  )
  if (!file.exists(pop_path)) stop("No existe population_result: ", pop_path)
  
  msg("Leyendo death_cause_final_hierarchical.")
  dth_raw <- as.data.table(read_auto(death_path))
  
  msg("Leyendo cause_master.")
  cm <- as.data.table(read_auto(cause_path))
  
  msg("Leyendo population_result.")
  pop <- as.data.table(read_auto(pop_path))
  
  req_dth <- c("year_id", "location_id", "sex_id", "age", "cause_concept_id", "deaths_final")
  miss_dth <- setdiff(req_dth, names(dth_raw))
  if (length(miss_dth) > 0L) {
    stop("Faltan columnas en death_cause_final_hierarchical: ", paste(miss_dth, collapse = ", "))
  }
  
  req_cm <- c("cause_concept_id", "cause_level", "cause_name", "parent_concept_id", "is_terminal")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  pop_col <- detect_col(pop, c("population"), "population")
  
  dth_raw <- dth_raw[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    deaths_final = as.numeric(deaths_final),
    cause_level = as.integer(if ("cause_level" %in% names(dth_raw)) cause_level else NA_integer_),
    cause_name = as.character(if ("cause_name" %in% names(dth_raw)) cause_name else NA_character_),
    parent_concept_id = as.integer(if ("parent_concept_id" %in% names(dth_raw)) parent_concept_id else NA_integer_),
    is_terminal = as.logical(if ("is_terminal" %in% names(dth_raw)) is_terminal else NA)
  )][
    year_id %in% CFG$years &
      sex_id %in% CFG$sexes &
      age >= CFG$age_min &
      age <= CFG$age_max
  ]
  
  cm <- cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    parent_concept_id = as.integer(parent_concept_id),
    is_terminal = as.logical(is_terminal),
    sex_restriction_target_default = if ("sex_restriction_target_default" %in% names(cm)) {
      as.character(sex_restriction_target_default)
    } else {
      NA_character_
    },
    target_age_start_default = if ("target_age_start_default" %in% names(cm)) {
      as.integer(target_age_start_default)
    } else {
      NA_integer_
    },
    target_age_end_default = if ("target_age_end_default" %in% names(cm)) {
      as.integer(target_age_end_default)
    } else {
      NA_integer_
    }
  )]
  
  pop <- pop[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    population = as.numeric(get(pop_col))
  )][
    year_id %in% CFG$years &
      sex_id %in% CFG$sexes &
      age >= CFG$age_min &
      age <= CFG$age_max
  ]
  
  dth_raw[is.na(deaths_final), deaths_final := 0]
  dth_raw[deaths_final < 0, deaths_final := 0]
  
  # reforzar metadatos desde cause_master
  dth_raw <- merge(
    dth_raw,
    cm,
    by = "cause_concept_id",
    all.x = TRUE,
    suffixes = c("", "_cm"),
    sort = FALSE
  )
  
  dth_raw[, cause_level := fifelse(is.na(cause_level), cause_level_cm, cause_level)]
  dth_raw[, cause_name := coalesce_chr(cause_name, cause_name_cm)]
  dth_raw[, parent_concept_id := fifelse(is.na(parent_concept_id), parent_concept_id_cm, parent_concept_id)]
  dth_raw[, is_terminal := fifelse(is.na(is_terminal), is_terminal_cm, is_terminal)]
  
  dth_raw[, c("cause_level_cm", "cause_name_cm", "parent_concept_id_cm", "is_terminal_cm") := NULL]
  
  if (any(is.na(dth_raw$is_terminal))) {
    bad_term <- unique(dth_raw[is.na(is_terminal), .(cause_concept_id)])
    fwrite(bad_term, file.path(CFG$qc_dir, "qc_missing_terminality.csv"))
    msg("Advertencia: hay causas sin terminalidad; se excluirán del modelamiento.")
  }
  
  # ----------------------------------------------------------
  # CONTRATO LEAF-ONLY PARA 09
  # ----------------------------------------------------------
  msg("Filtrando input a causas terminales solamente.")
  dth_raw <- dth_raw[is_terminal == TRUE & cause_concept_id > 0]
  
  if (nrow(dth_raw) == 0L) {
    stop("Tras filtrar is_terminal == TRUE, no quedan filas para modelar.")
  }
  
  # ----------------------------------------------------------
  # PATCH GEOGRÁFICO: geografía fina -> departamento
  # ----------------------------------------------------------
  msg("Armonizando geografía fina de mortalidad a departamento.")
  
  cause_meta <- unique(dth_raw[, .(
    cause_concept_id, cause_level, cause_name, parent_concept_id, is_terminal,
    sex_restriction_target_default, target_age_start_default, target_age_end_default
  )])
  
  if (CFG$debug_cause_limit > 0L) {
    cause_meta <- cause_meta[order(cause_level, cause_concept_id)][1:min(.N, CFG$debug_cause_limit)]
    dth_raw <- dth_raw[cause_concept_id %in% cause_meta$cause_concept_id]
    msg("DEBUG: limitando modelamiento a ", nrow(cause_meta), " causas terminales.")
  }
  
  mass_before_geo <- dth_raw[, sum(deaths_final, na.rm = TRUE)]
  
  # 08b ya puede entregar location_id departamental 1:25; si llega geografía fina,
  # se armoniza a departamento tomando los dos primeros dígitos del ubigeo.
  dth_raw[, location_id_dept := fifelse(
    location_id %in% CFG$model_location_domain,
    as.integer(location_id),
    as.integer(substr(sprintf("%06d", as.integer(location_id)), 1, 2))
  )]
  dth_raw[, location_id_chr6 := sprintf("%06d", as.integer(location_id))]
  
  bad_geo <- dth_raw[is.na(location_id_dept) | !(location_id_dept %in% CFG$model_location_domain)]
  if (nrow(bad_geo) > 0L) {
    fwrite(
      unique(bad_geo[, .(location_id, location_id_chr6, location_id_dept)]),
      file.path(CFG$qc_dir, "qc_bad_geo_mapping.csv")
    )
    stop("Se detectaron location_id finos que no caen en departamentos válidos 1:25.")
  }
  
  dth <- dth_raw[, .(
    deaths_final = sum(deaths_final, na.rm = TRUE)
  ), by = .(
    year_id,
    location_id = location_id_dept,
    sex_id,
    age,
    cause_concept_id
  )]
  
  dth <- merge(
    dth,
    cause_meta,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  mass_after_geo <- dth[, sum(deaths_final, na.rm = TRUE)]
  if (!isTRUE(all.equal(mass_before_geo, mass_after_geo, tolerance = CFG$mass_tolerance))) {
    stop(sprintf(
      "No se preservó masa tras armonización geográfica. Antes=%s | Después=%s",
      format(mass_before_geo, scientific = FALSE),
      format(mass_after_geo, scientific = FALSE)
    ))
  }
  
  # ----------------------------------------------------------
  # Preparar población departamental
  # ----------------------------------------------------------
  pop <- pop[!is.na(population)]
  pop[population <= 0, population := NA_real_]
  pop <- pop[location_id %in% CFG$model_location_domain]
  
  if (nrow(pop) == 0L) {
    stop("population_result quedó vacío tras restringir a departamentos 1:25.")
  }
  
  # ----------------------------------------------------------
  # QC de overlap geográfico/llaves
  # ----------------------------------------------------------
  pop_key <- unique(pop[, .(year_id, location_id, sex_id, age)])
  dth_key <- unique(dth[, .(year_id, location_id, sex_id, age)])
  
  tmp_hit <- merge(
    dth_key[, .(year_id, location_id, sex_id, age)],
    pop_key,
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    all.y = FALSE,
    sort = FALSE
  )
  
  effective_match_rate <- if (nrow(dth_key) > 0L) nrow(tmp_hit) / nrow(dth_key) else 0
  
  qc_geo_contract <- data.table(
    mass_before_geo = mass_before_geo,
    mass_after_geo = mass_after_geo,
    n_rows_dth_after_geo = nrow(dth),
    n_unique_locations_dth_after_geo = uniqueN(dth$location_id),
    n_rows_pop_dept = nrow(pop),
    n_unique_locations_pop_dept = uniqueN(pop$location_id),
    effective_match_rate = effective_match_rate
  )
  
  fwrite(qc_geo_contract, file.path(CFG$qc_dir, "qc_geo_contract.csv"))
  
  if (effective_match_rate < CFG$min_match_rate_population) {
    stop(sprintf(
      "Match rate dth vs pop insuficiente tras armonización geográfica: %.4f",
      effective_match_rate
    ))
  }
  
  # ----------------------------------------------------------
  # Grid base de exposición
  # ----------------------------------------------------------
  pop_grid <- unique(pop[, .(year_id, location_id, sex_id, age, population)])
  
  setorder(cause_meta, cause_level, cause_concept_id)
  
  result_store <- new.env(parent = emptyenv())
  
  out_list <- vector("list", nrow(cause_meta))
  audit_list <- vector("list", nrow(cause_meta))
  attempt_log_list <- list()
  registry_list <- vector("list", nrow(cause_meta))
  coefficient_list <- list()
  smooth_term_list <- list()
  fit_metric_list <- list()
  recalibration_factor_list <- list()
  heuristic_value_list <- list()
  prediction_diag_year_sex_list <- list()
  prediction_diag_age_year_sex_list <- list()
  model_validation_list <- list()
  model_summary_list <- list()
  statistical_assessment_list <- list()
  
  msg("Construyendo auditoría de suficiencia y modelando causas terminales.")
  
  for (i in seq_len(nrow(cause_meta))) {
    meta_i <- cause_meta[i]
    cid <- meta_i$cause_concept_id[1]
    clevel <- meta_i$cause_level[1]
    cname <- meta_i$cause_name[1]
    parent_id <- meta_i$parent_concept_id[1]
    
    msg(sprintf("[%s/%s] cause_concept_id=%s | L%s | %s",
                i, nrow(cause_meta), cid, clevel, cname))
    
    deaths_i <- dth[cause_concept_id == cid, .(
      year_id, location_id, sex_id, age, deaths_final
    )]
    
    base_i <- build_cause_grid(pop_grid, deaths_i, meta_i)
    sex_split_i <- split_sex_eligible_grid(base_i)
    base_i <- sex_split_i$eligible
    base_i_excluded <- sex_split_i$excluded
    age_split_i <- split_age_eligible_grid(base_i)
    base_i <- age_split_i$eligible
    base_i_excluded <- rbindlist(list(base_i_excluded, age_split_i$excluded), use.names = TRUE, fill = TRUE)
    
    audit_i <- build_sufficiency_audit(base_i)
    audit_list[[i]] <- audit_i
    
    has_parent <- !is.na(parent_id) &&
      exists(as.character(parent_id), envir = result_store, inherits = FALSE)
    
    parent_result <- if (has_parent) get(as.character(parent_id), envir = result_store) else NULL
    
    paths <- choose_candidate_path_v2(
      audit_row = audit_i,
      has_parent = has_parent
    )
    paths_comp <- candidate_paths_for_competition(paths)
    validation_i <- validate_candidate_paths(base_i, paths_comp)
    winner_i <- choose_competition_winner(validation_i, paths_comp)
    selected_preferred <- winner_i$method_code %||% NA_character_
    validation_i[, `:=`(
      cause_concept_id = cid,
      cause_name = cname,
      selected_by_competition = method_code == selected_preferred,
      competition_decision_reason = winner_i$reason,
      run_id = run_id
    )]
    model_validation_list[[length(model_validation_list) + 1L]] <- validation_i
    
    chosen <- NULL
    chosen_code <- NA_character_
    chosen_conv <- NA_character_
    chosen_warn_flag <- FALSE
    accepted_candidates <- list()
    
    for (attempt_idx in seq_along(paths_comp)) {
      pc <- paths_comp[attempt_idx]
      
      res_try <- tryCatch(
        try_fit_path(pc, base_i, audit_row = audit_i, parent_result = parent_result, attempt_order = attempt_idx),
        error = function(e) list(
          result = NULL,
          convergence_status = "error",
          warnings = character(),
          failure_reason = conditionMessage(e),
          formula = NA_character_,
          aggregation_age = NA_character_,
          aggregation_year = NA_character_,
          aggregation_region = NA_character_,
          rows_train = nrow(base_i)
        )
      )
      
      eval_try <- evaluate_attempt_result(base_i, res_try$result)
      
      attempt_log_list[[length(attempt_log_list) + 1L]] <- data.table(
        cause_concept_id = cid,
        cause_name = cname,
        attempt_order = attempt_idx,
        method_code = pc,
        attempt_status = ifelse(eval_try$ok, "accepted", "failed"),
        failure_reason = ifelse(eval_try$ok, NA_character_,
                                coalesce_chr(res_try$failure_reason, eval_try$failure_reason)),
        convergence_status = res_try$convergence_status,
        warning_flag = length(res_try$warnings) > 0L,
        warnings = if (length(res_try$warnings) > 0L) paste(unique(res_try$warnings), collapse = " | ") else NA_character_,
        rows_train = as.integer(res_try$rows_train),
        rows_pred = as.integer(nrow(base_i)),
        formula = res_try$formula,
        training_age_scale = if (!is.null(res_try$result)) unique(res_try$result$training_age_scale)[1] else NA_character_,
        training_time_scale = if (!is.null(res_try$result)) unique(res_try$result$training_time_scale)[1] else NA_character_,
        aggregation_age = res_try$aggregation_age,
        aggregation_year = res_try$aggregation_year,
        aggregation_region = res_try$aggregation_region,
        total_deaths_input = sum(base_i$deaths_final, na.rm = TRUE),
        mass_input = sum(base_i$deaths_final, na.rm = TRUE),
        mass_output = if (!is.null(res_try$result)) sum(res_try$result$deaths_smoothed, na.rm = TRUE) else NA_real_,
        max_abs_mass_diff = eval_try$max_abs_mass_diff,
        borrowed_from_cause = if (!is.null(res_try$result)) unique(res_try$result$borrowed_from_cause_concept_id)[1] else NA_integer_,
        borrowed_from_level = if (!is.null(res_try$result)) unique(res_try$result$borrowed_from_level)[1] else NA_integer_,
        run_id = run_id
      )
      if (is.data.table(res_try$coefficients) && nrow(res_try$coefficients)) {
        coefficient_list[[length(coefficient_list) + 1L]] <- copy(res_try$coefficients)
      }
      if (is.data.table(res_try$smooth_terms) && nrow(res_try$smooth_terms)) {
        smooth_term_list[[length(smooth_term_list) + 1L]] <- copy(res_try$smooth_terms)
      }
      if (is.data.table(res_try$fit_metrics) && nrow(res_try$fit_metrics)) {
        fit_metric_list[[length(fit_metric_list) + 1L]] <- copy(res_try$fit_metrics)
      }
      if (is.data.table(res_try$recalibration_factors) && nrow(res_try$recalibration_factors)) {
        recalibration_factor_list[[length(recalibration_factor_list) + 1L]] <- copy(res_try$recalibration_factors)
      }
      if (is.data.table(res_try$heuristic_values) && nrow(res_try$heuristic_values)) {
        heuristic_value_list[[length(heuristic_value_list) + 1L]] <- copy(res_try$heuristic_values)
      }
      if (length(res_try$model_summary_text %||% character())) {
        model_summary_list[[length(model_summary_list) + 1L]] <- list(
          cause_concept_id = cid,
          cause_name = cname,
          method_code = pc,
          attempt_order = attempt_idx,
          summary_text = res_try$model_summary_text
        )
      }
      
      if (eval_try$ok) {
        accepted_candidates[[pc]] <- list(
          result = res_try$result,
          code = pc,
          conv = res_try$convergence_status,
          warn_flag = length(res_try$warnings) > 0L,
          prediction_diag_year_sex = res_try$prediction_diag_year_sex,
          prediction_diag_age_year_sex = res_try$prediction_diag_age_year_sex
        )
        if (is.null(chosen)) {
          chosen <- res_try$result
          chosen_code <- pc
          chosen_conv <- res_try$convergence_status
          chosen_warn_flag <- length(res_try$warnings) > 0L
        }
        if (!is.na(selected_preferred) && identical(pc, selected_preferred)) {
          chosen <- res_try$result
          chosen_code <- pc
          chosen_conv <- res_try$convergence_status
          chosen_warn_flag <- length(res_try$warnings) > 0L
        }
      }
    }
    if (length(accepted_candidates) && !(chosen_code %in% names(accepted_candidates)) && !is.na(selected_preferred) && selected_preferred %in% names(accepted_candidates)) {
      chosen <- accepted_candidates[[selected_preferred]]$result
      chosen_code <- accepted_candidates[[selected_preferred]]$code
      chosen_conv <- accepted_candidates[[selected_preferred]]$conv
      chosen_warn_flag <- accepted_candidates[[selected_preferred]]$warn_flag
    }
    if (is.null(chosen)) {
      emergency <- try_fit_path("F", base_i, audit_row = audit_i, parent_result = NULL, attempt_order = length(paths_comp) + 1L)
      chosen <- emergency$result
      chosen_code <- "F"
      chosen_conv <- "fallback_raw_emergency"
      chosen_warn_flag <- FALSE
      
      attempt_log_list[[length(attempt_log_list) + 1L]] <- data.table(
        cause_concept_id = cid,
        cause_name = cname,
        attempt_order = length(paths_comp) + 1L,
        method_code = "F_emergency",
        attempt_status = "accepted",
        failure_reason = NA_character_,
        convergence_status = "fallback_raw_emergency",
        warning_flag = FALSE,
        warnings = NA_character_,
        rows_train = as.integer(nrow(base_i)),
        rows_pred = as.integer(nrow(base_i)),
        formula = "raw_no_model_emergency",
        training_age_scale = "single_age",
        training_time_scale = "single_year",
        aggregation_age = "single_age",
        aggregation_year = "single_year",
        aggregation_region = "none_raw",
        total_deaths_input = sum(base_i$deaths_final, na.rm = TRUE),
        mass_input = sum(base_i$deaths_final, na.rm = TRUE),
        mass_output = sum(chosen$deaths_smoothed, na.rm = TRUE),
        max_abs_mass_diff = 0,
        borrowed_from_cause = NA_integer_,
        borrowed_from_level = NA_integer_,
        run_id = run_id
      )
      if (is.data.table(emergency$coefficients) && nrow(emergency$coefficients)) {
        coefficient_list[[length(coefficient_list) + 1L]] <- copy(emergency$coefficients)
      }
      if (is.data.table(emergency$smooth_terms) && nrow(emergency$smooth_terms)) {
        smooth_term_list[[length(smooth_term_list) + 1L]] <- copy(emergency$smooth_terms)
      }
      if (is.data.table(emergency$fit_metrics) && nrow(emergency$fit_metrics)) {
        fit_metric_list[[length(fit_metric_list) + 1L]] <- copy(emergency$fit_metrics)
      }
      if (is.data.table(emergency$recalibration_factors) && nrow(emergency$recalibration_factors)) {
        recalibration_factor_list[[length(recalibration_factor_list) + 1L]] <- copy(emergency$recalibration_factors)
      }
      if (is.data.table(emergency$heuristic_values) && nrow(emergency$heuristic_values)) {
        heuristic_value_list[[length(heuristic_value_list) + 1L]] <- copy(emergency$heuristic_values)
      }
      if (is.data.table(emergency$prediction_diag_year_sex) && nrow(emergency$prediction_diag_year_sex)) {
        prediction_diag_year_sex_list[[length(prediction_diag_year_sex_list) + 1L]] <- copy(emergency$prediction_diag_year_sex)
      }
      if (is.data.table(emergency$prediction_diag_age_year_sex) && nrow(emergency$prediction_diag_age_year_sex)) {
        prediction_diag_age_year_sex_list[[length(prediction_diag_age_year_sex_list) + 1L]] <- copy(emergency$prediction_diag_age_year_sex)
      }
    } else if (chosen_code %in% names(accepted_candidates)) {
      final_diag <- accepted_candidates[[chosen_code]]
      if (is.data.table(final_diag$prediction_diag_year_sex) && nrow(final_diag$prediction_diag_year_sex)) {
        prediction_diag_year_sex_list[[length(prediction_diag_year_sex_list) + 1L]] <- copy(final_diag$prediction_diag_year_sex)
      }
      if (is.data.table(final_diag$prediction_diag_age_year_sex) && nrow(final_diag$prediction_diag_age_year_sex)) {
        prediction_diag_age_year_sex_list[[length(prediction_diag_age_year_sex_list) + 1L]] <- copy(final_diag$prediction_diag_age_year_sex)
      }
    }
    statistical_assessment_list[[length(statistical_assessment_list) + 1L]] <- data.table(
      cause_concept_id = cid,
      cause_name = cname,
      method_selected_by_competition = selected_preferred,
      method_selected_final = chosen_code,
      competition_decision_reason = winner_i$reason,
      final_verdict_statistical = fifelse(chosen_code %in% c("F", "F_emergency"), "OK_CON_NOTA_fallback_documentado",
                                          fifelse(chosen_warn_flag, "OK_CON_NOTA_warnings_documentados", "OK_modelo_validado")),
      expert_statistical_conclusion = fifelse(
        chosen_code %in% c("F", "F_emergency"),
        "Se uso fallback crudo porque no hubo un candidato GAM suficientemente seguro; se preserva masa y se documenta como decision conservadora.",
        fifelse(chosen_warn_flag,
                "El modelo seleccionado tiene warnings, pero no fueron criticos para seleccion o fueron compatibles con muertes fraccionales; se valida con cierre de masa y diagnosticos graficos.",
                "El modelo seleccionado paso la competencia conservadora sin warnings bloqueantes.")
      ),
      run_id = run_id
    )
    
    if (nrow(base_i_excluded) > 0L) {
      excluded_zero <- make_result_dt(
        base_dt = base_i_excluded,
        deaths_smoothed = rep(0, nrow(base_i_excluded)),
        method_code = "SEX0",
        model_formula_used = "sex_restriction_structural_zero",
        training_age_scale = "single_age",
        training_time_scale = "single_year",
        aggregation_age = "single_age",
        aggregation_year = "single_year",
        aggregation_region = "department_exact_zero",
        borrowed_from_level = NA_integer_,
        borrowed_from_cause = NA_integer_,
        recalibration_scope = "none",
        years_with_deaths = 0L,
        regions_with_deaths = 0L
      )
      chosen <- rbindlist(list(chosen, excluded_zero), use.names = TRUE, fill = TRUE)
    }
    
    assign(as.character(cid), chosen, envir = result_store)
    out_list[[i]] <- chosen
    
    audit_list[[i]][, `:=`(
      aggregation_age = unique(chosen$aggregation_age)[1],
      aggregation_year = unique(chosen$aggregation_year)[1],
      aggregation_region = unique(chosen$aggregation_region)[1],
      modeling_decision = paste0("selected_", chosen_code)
    )]
    
    registry_list[[i]] <- data.table(
      cause_concept_id = cid,
      cause_name = cname,
      method_selected = chosen_code,
      data_category = audit_i$data_category[1],
      years_with_deaths = audit_i$years_with_deaths[1],
      regions_with_deaths = audit_i$regions_with_deaths[1],
      rows_available = audit_i$rows_available[1],
      rows_expected = audit_i$rows_expected[1],
      data_density = audit_i$data_density[1],
      aggregation_age = unique(chosen$aggregation_age)[1],
      aggregation_year = unique(chosen$aggregation_year)[1],
      aggregation_region = unique(chosen$aggregation_region)[1],
      model_status = unique(chosen$model_status)[1],
      convergence_status = chosen_conv,
      warning_flag = chosen_warn_flag,
      total_deaths_input = sum(base_i$deaths_final, na.rm = TRUE),
      mass_input = sum(base_i$deaths_final, na.rm = TRUE),
      mass_output = sum(chosen$deaths_smoothed, na.rm = TRUE),
      borrowed_from_cause = unique(chosen$borrowed_from_cause_concept_id)[1],
      borrowed_from_level = unique(chosen$borrowed_from_level)[1],
      run_id = run_id
    )
  }
  
  msg("Combinando resultados.")
  out <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
  mortality_data_sufficiency_audit <- rbindlist(audit_list, use.names = TRUE, fill = TRUE)
  mortality_model_attempt_log <- rbindlist(attempt_log_list, use.names = TRUE, fill = TRUE)
  mortality_model_registry <- rbindlist(registry_list, use.names = TRUE, fill = TRUE)
  mortality_model_coefficients <- if (length(coefficient_list)) {
    rbindlist(coefficient_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table(
      cause_concept_id = integer(), cause_name = character(), method_code = character(),
      attempt_order = integer(), term = character(), formula = character(), run_id = character()
    )
  }
  mortality_model_smooth_terms <- if (length(smooth_term_list)) {
    rbindlist(smooth_term_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table(
      cause_concept_id = integer(), cause_name = character(), method_code = character(),
      attempt_order = integer(), smooth_term = character(), formula = character(), run_id = character()
    )
  }
  mortality_model_recalibration_factors <- if (length(recalibration_factor_list)) {
    rbindlist(recalibration_factor_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table(
      cause_concept_id = integer(), cause_name = character(), method_code = character(),
      attempt_order = integer(), year_id = integer(), sex_id = integer(),
      observed_mass = numeric(), initial_prediction_mass = numeric(),
      smoothed_final_mass = numeric(), recalibration_factor = numeric(),
      final_mass_diff = numeric(), run_id = character()
    )
  }
  mortality_model_heuristic_values <- if (length(heuristic_value_list)) {
    rbindlist(heuristic_value_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table(
      cause_concept_id = integer(), cause_name = character(), method_code = character(),
      attempt_order = integer(), heuristic_rule = character(), borrowed_from_cause = integer(),
      borrowed_from_level = integer(), mass_input = numeric(), mass_output = numeric(),
      max_abs_year_sex_diff = numeric(), rationale = character(), run_id = character()
    )
  }
  mortality_model_prediction_diagnostics_year_sex <- if (length(prediction_diag_year_sex_list)) {
    rbindlist(prediction_diag_year_sex_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table(
      cause_concept_id = integer(), cause_name = character(), method_code = character(),
      attempt_order = integer(), year_id = integer(), sex_id = integer(),
      observed = numeric(), pred_initial = numeric(), pred_recalibrated = numeric(),
      recalibration_factor = numeric(), diff_initial = numeric(), diff_recalibrated = numeric(),
      resid_proxy_initial = numeric(), resid_proxy_recalibrated = numeric(), run_id = character()
    )
  }
  mortality_model_prediction_diagnostics_age_year_sex <- if (length(prediction_diag_age_year_sex_list)) {
    rbindlist(prediction_diag_age_year_sex_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table(
      cause_concept_id = integer(), cause_name = character(), method_code = character(),
      attempt_order = integer(), year_id = integer(), sex_id = integer(), age = integer(),
      observed = numeric(), pred_initial = numeric(), pred_recalibrated = numeric(),
      recalibration_factor = numeric(), diff_initial = numeric(), diff_recalibrated = numeric(),
      resid_proxy_initial = numeric(), resid_proxy_recalibrated = numeric(), run_id = character()
    )
  }
  mortality_model_candidate_validation <- if (length(model_validation_list)) {
    rbindlist(model_validation_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table()
  }
  mortality_model_statistical_assessment <- if (length(statistical_assessment_list)) {
    rbindlist(statistical_assessment_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table()
  }
  mortality_model_fit_metrics_core <- if (length(fit_metric_list)) {
    rbindlist(fit_metric_list, use.names = TRUE, fill = TRUE)
  } else {
    data.table()
  }
  accepted_attempts <- mortality_model_attempt_log[
    attempt_status == "accepted",
    .SD[.N],
    by = cause_concept_id
  ][, .(
    cause_concept_id,
    selected_attempt_order = attempt_order,
    accepted_method_code = method_code,
    accepted_formula = formula,
    accepted_rows_train = rows_train,
    accepted_rows_pred = rows_pred,
    accepted_max_abs_mass_diff = max_abs_mass_diff,
    accepted_warnings = warnings
  )]
  mortality_model_fit_metrics <- merge(
    mortality_model_registry[, .(
      cause_concept_id, cause_name, method_selected, data_category,
      model_status, convergence_status, warning_flag,
      mass_input, mass_output, run_id
    )],
    accepted_attempts,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(mortality_model_fit_metrics_core)) {
    mortality_model_fit_metrics <- merge(
      mortality_model_fit_metrics,
      mortality_model_fit_metrics_core,
      by.x = c("cause_concept_id", "method_selected"),
      by.y = c("cause_concept_id", "method_code"),
      all.x = TRUE,
      sort = FALSE,
      suffixes = c("", "_model")
    )
  }
  mortality_model_fit_metrics[, `:=`(
    mass_diff = mass_output - mass_input,
    metric_source = fifelse(method_selected %in% c("A", "B", "C", "D"),
                            "mgcv_summary_captured_during_canonical_fit",
                            "heuristic_or_fallback_registry")
  )]
  mortality_model_warning_summary <- copy(mortality_model_fit_metrics[, .(
    cause_concept_id, cause_name, method_selected,
    warning_flag,
    warning_severity = vapply(accepted_warnings, function(x) classify_model_warnings(x)$severity, character(1)),
    warning_is_critical = vapply(accepted_warnings, function(x) classify_model_warnings(x)$is_critical, logical(1)),
    warning_note = vapply(accepted_warnings, function(x) classify_model_warnings(x)$note, character(1)),
    warning_excerpt = substr(accepted_warnings, 1, 500)
  )])
  mortality_model_coefficient_assessment <- if (nrow(mortality_model_coefficients)) {
    z_col <- intersect(c("z.value", "z", "z.value."), names(mortality_model_coefficients))[1]
    p_col <- intersect(c("Pr...z..", "Pr...t..", "p.value"), names(mortality_model_coefficients))[1]
    mortality_model_coefficients[, .(
      n_coefficients = .N,
      n_z_missing = if (length(z_col) && !is.na(z_col)) sum(is.na(get(z_col))) else NA_integer_,
      n_p_missing = if (length(p_col) && !is.na(p_col)) sum(is.na(get(p_col))) else NA_integer_,
      n_zero_se = if ("Std..Error" %in% names(.SD)) sum(is.na(Std..Error) | Std..Error == 0) else NA_integer_,
      coefficient_assessment = {
        nz <- if (length(z_col) && !is.na(z_col)) sum(is.na(get(z_col))) else 0L
        if (nz > 0L) "OK_CON_NOTA_coeficiente_no_estimable_revisar_contexto" else "OK_coeficientes_estimables"
      }
    ), by = .(cause_concept_id, cause_name, method_code)]
  } else {
    data.table()
  }
  validation_selected <- mortality_model_candidate_validation[selected_by_competition == TRUE, .(
    cause_concept_id, selected_validation_method = method_code, validation_strategy,
    validation_deviance, validation_mae, validation_rmse, validation_bias,
    validation_warning_severity = warning_severity,
    validation_warning_is_critical = warning_is_critical,
    competition_decision_reason
  )]
  mortality_model_fit_metrics_compact <- merge(
    mortality_model_fit_metrics[, .(
      cause_concept_id, cause_name, method_selected, accepted_formula,
      aic, deviance_explained, n_observations, warning_flag,
      mass_input, mass_output, mass_diff, accepted_max_abs_mass_diff
    )],
    validation_selected,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  mortality_model_fit_metrics_compact <- merge(
    mortality_model_fit_metrics_compact,
    mortality_model_warning_summary[, .(cause_concept_id, warning_severity, warning_is_critical, warning_note)],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  mortality_model_fit_metrics_compact <- merge(
    mortality_model_fit_metrics_compact,
    mortality_model_statistical_assessment[, .(cause_concept_id, final_verdict_statistical, expert_statistical_conclusion)],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  covid_specific_id <- cm[cause_name == "COVID-19" & is_terminal == TRUE, cause_concept_id][1]
  oprm_id <- cm[cause_name == "Other pandemic related mortality (OPRM)" & is_terminal == TRUE, cause_concept_id][1]
  qc_pandemic_window_zero_enforcement <- out[
    (!is.na(covid_specific_id) & cause_concept_id == covid_specific_id & year_id < 2020L & deaths_smoothed > 0) |
      (!is.na(oprm_id) & cause_concept_id == oprm_id & (year_id < 2020L | year_id > 2022L) & deaths_smoothed > 0),
    .(
      year_id, location_id, sex_id, age, cause_concept_id, cause_name,
      deaths_final, deaths_smoothed, mortality_rate_smoothed, fallback_level
    )
  ][order(year_id, cause_concept_id, location_id, sex_id, age)]
  out[
    !is.na(covid_specific_id) & cause_concept_id == covid_specific_id & year_id < 2020L,
    `:=`(deaths_smoothed = 0, mortality_rate_smoothed = 0)
  ]
  out[
    !is.na(oprm_id) & cause_concept_id == oprm_id & (year_id < 2020L | year_id > 2022L),
    `:=`(deaths_smoothed = 0, mortality_rate_smoothed = 0)
  ]
  
  setorder(out, year_id, location_id, sex_id, age, cause_level, cause_concept_id)
  
  # ----------------------------------------------------------
  # QC mínimo vital
  # ----------------------------------------------------------
  msg("Corriendo QC mínimo vital.")
  
  qc_method_summary <- mortality_model_registry[, .N, by = .(data_category, method_selected)][order(data_category, method_selected)]
  
  qc_total_preservation <- out[, .(
    crude_total = sum(deaths_final, na.rm = TRUE),
    smoothed_total = sum(deaths_smoothed, na.rm = TRUE)
  ), by = .(year_id, sex_id, cause_level)][order(cause_level, sex_id, year_id)]
  
  qc_total_preservation[, abs_diff := smoothed_total - crude_total]
  qc_total_preservation[, pct_diff := fifelse(crude_total > 0, 100 * abs_diff / crude_total, NA_real_)]
  
  qc_rate_flags <- out[, .(
    n_rows = .N,
    n_missing_crude_rate = sum(is.na(mortality_rate_crude)),
    n_missing_smoothed_rate = sum(is.na(mortality_rate_smoothed)),
    n_negative_smoothed_deaths = sum(deaths_smoothed < 0),
    n_negative_smoothed_rate = sum(mortality_rate_smoothed < 0, na.rm = TRUE),
    max_smoothed_rate = suppressWarnings(max(mortality_rate_smoothed, na.rm = TRUE))
  ), by = .(cause_level)][order(cause_level)]
  
  qc_top_borrowed <- mortality_model_registry[method_selected == "E"][order(-total_deaths_input)][1:min(.N, 200)]
  
  qc_year_totals <- out[, .(
    deaths_final_total = sum(deaths_final, na.rm = TRUE),
    deaths_smoothed_total = sum(deaths_smoothed, na.rm = TRUE)
  ), by = year_id][order(year_id)]
  
  qc_mass_preservation_by_cause <- out[, .(
    obs_sum = sum(deaths_final, na.rm = TRUE),
    pred_sum = sum(deaths_smoothed, na.rm = TRUE)
  ), by = .(cause_concept_id, year_id, sex_id)][order(cause_concept_id, year_id, sex_id)]
  qc_mass_preservation_by_cause[, diff := pred_sum - obs_sum]
  
  qc_top_absurd_rates <- out[order(-mortality_rate_smoothed)][1:min(.N, 200),
                                                              .(year_id, location_id, sex_id, age, cause_concept_id, cause_name,
                                                                deaths_final, deaths_smoothed, mortality_rate_smoothed, fallback_level)]  
 
  # ----------------------------------------------------------
  # QC de estabilidad temporal / serrucho
  # ----------------------------------------------------------
  qc_temporal_roughness <- out[
    order(cause_concept_id, location_id, sex_id, age, year_id),
    {
      d1 <- diff(mortality_rate_smoothed)
      d1 <- d1[is.finite(d1)]
      d2 <- diff(sign(d1[d1 != 0]))
      
      .(
        n_years = .N,
        mean_rate = mean(mortality_rate_smoothed, na.rm = TRUE),
        sd_rate = sd(mortality_rate_smoothed, na.rm = TRUE),
        cv_rate = fifelse(mean(mortality_rate_smoothed, na.rm = TRUE) > 0,
                          sd(mortality_rate_smoothed, na.rm = TRUE) /
                            mean(mortality_rate_smoothed, na.rm = TRUE),
                          NA_real_),
        mean_abs_diff = if (length(d1) > 0) mean(abs(d1), na.rm = TRUE) else NA_real_,
        max_abs_diff = if (length(d1) > 0) max(abs(d1), na.rm = TRUE) else NA_real_,
        n_turning_points = if (length(d2) > 0) sum(d2 != 0, na.rm = TRUE) else 0L
      )
    },
    by = .(cause_concept_id, cause_name, location_id, sex_id, age, fallback_level)
  ]
  
  qc_temporal_roughness_summary <- qc_temporal_roughness[, .(
    mean_cv_rate = mean(cv_rate, na.rm = TRUE),
    p95_cv_rate = suppressWarnings(quantile(cv_rate, 0.95, na.rm = TRUE)),
    mean_abs_diff_mean = mean(mean_abs_diff, na.rm = TRUE),
    p95_abs_diff = suppressWarnings(quantile(mean_abs_diff, 0.95, na.rm = TRUE)),
    mean_turning_points = mean(n_turning_points, na.rm = TRUE),
    p95_turning_points = suppressWarnings(quantile(n_turning_points, 0.95, na.rm = TRUE)),
    n_rows = .N
  ), by = .(cause_concept_id, cause_name, fallback_level)][order(-p95_turning_points, -p95_abs_diff)]
  
  qc_top_serrucho <- qc_temporal_roughness_summary[1:min(.N, 200)]
  qc_sex_specific_mismatch_positive_smoothed <- merge(
    out,
    unique(cm[
      !is.na(sex_restriction_target_default) & sex_restriction_target_default != "",
      .(cause_concept_id, cause_name, expected_sex = sex_restriction_target_default)
    ]),
    by = c("cause_concept_id", "cause_name"),
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(
      deaths_smoothed = sum(deaths_smoothed, na.rm = TRUE),
      deaths_final = sum(deaths_final, na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age, cause_concept_id, cause_name, expected_sex)
  ]
  qc_sex_specific_mismatch_positive_smoothed[
    ,
    observed_sex := fifelse(
      sex_id == 8507L, "male",
      fifelse(sex_id == 8532L, "female", "other")
    )
  ]
  qc_sex_specific_mismatch_positive_smoothed <- qc_sex_specific_mismatch_positive_smoothed[
    observed_sex != expected_sex & deaths_smoothed > 1e-10
  ][order(-deaths_smoothed, cause_name, year_id, location_id, sex_id, age)]
  qc_sex_specific_mismatch_positive_smoothed_path <- file.path(CFG$qc_dir, "qc_sex_specific_mismatch_positive_smoothed.csv")
  fwrite(qc_sex_specific_mismatch_positive_smoothed, qc_sex_specific_mismatch_positive_smoothed_path)
  qc_age_specific_mismatch_positive_smoothed <- merge(
    out,
    unique(cm[
      !is.na(target_age_start_default) | !is.na(target_age_end_default),
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
    .(
      deaths_smoothed = sum(deaths_smoothed, na.rm = TRUE),
      deaths_final = sum(deaths_final, na.rm = TRUE)
    ),
    by = .(year_id, location_id, sex_id, age, cause_concept_id, cause_name, expected_age_start, expected_age_end)
  ]
  qc_age_specific_mismatch_positive_smoothed <- qc_age_specific_mismatch_positive_smoothed[
    (
      (!is.na(expected_age_start) & age < expected_age_start) |
        (!is.na(expected_age_end) & age > expected_age_end)
    ) & deaths_smoothed > 1e-10
  ][order(-deaths_smoothed, cause_name, year_id, location_id, sex_id, age)]
  qc_age_specific_mismatch_positive_smoothed_path <- file.path(CFG$qc_dir, "qc_age_specific_mismatch_positive_smoothed.csv")
  fwrite(qc_age_specific_mismatch_positive_smoothed, qc_age_specific_mismatch_positive_smoothed_path)
   # hard checks básicos
  if (any(out$deaths_smoothed < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: hay deaths_smoothed negativas.")
  }
  if (any(out$mortality_rate_smoothed < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: hay mortality_rate_smoothed negativas.")
  }
  if (nrow(out[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]) > 0L) {
    stop("QC HARD FAIL: PK duplicada en mortality_rate_cause_smoothed.")
  }
  if (nrow(qc_sex_specific_mismatch_positive_smoothed) > 0L) {
    msg(
      "ADVERTENCIA QC: el suavizado conserva causas sexo-específicas incompatibles. ",
      "Revisar qc_sex_specific_mismatch_positive_smoothed.csv. ",
      "Estas filas pueden reflejar codificación directa del input y no una fuga del suavizado."
    )
  }
  if (nrow(qc_age_specific_mismatch_positive_smoothed) > 0L) {
    msg(
      "ADVERTENCIA QC: el suavizado conserva causas con dominio etario incompatible. ",
      "Revisar qc_age_specific_mismatch_positive_smoothed.csv. ",
      "Estas filas pueden reflejar codificación directa del input y no una fuga del suavizado."
    )
  }
  
  # ----------------------------------------------------------
  # Export
  # ----------------------------------------------------------
  msg("Exportando.")
  out_csv <- file.path(CFG$out_dir, paste0(CFG$table_name, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$table_name, ".parquet"))
  out_dict <- file.path(CFG$out_dir, paste0(CFG$table_name, "_dictionary_ext.csv"))
  
  write_csv_parquet(out, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(build_dictionary_ext(out), out_dict)
  
  qc_method_summary_path <- file.path(CFG$qc_dir, "qc_method_summary.csv")
  qc_total_preservation_path <- file.path(CFG$qc_dir, "qc_total_preservation.csv")
  qc_rate_flags_path <- file.path(CFG$qc_dir, "qc_rate_flags.csv")
  qc_top_borrowed_path <- file.path(CFG$qc_dir, "qc_top_borrowed.csv")
  qc_year_totals_path <- file.path(CFG$qc_dir, "qc_year_totals.csv")
  suff_audit_path <- file.path(CFG$qc_dir, "mortality_data_sufficiency_audit.csv")
  attempt_log_path <- file.path(CFG$qc_dir, "mortality_model_attempt_log.csv")
  model_registry_path <- file.path(CFG$qc_dir, "mortality_model_registry.csv")
  model_fit_metrics_path <- file.path(CFG$qc_dir, "mortality_model_fit_metrics.csv")
  model_coefficients_path <- file.path(CFG$qc_dir, "mortality_model_coefficients.csv")
  model_smooth_terms_path <- file.path(CFG$qc_dir, "mortality_model_smooth_terms.csv")
  model_recalibration_factors_path <- file.path(CFG$qc_dir, "mortality_model_recalibration_factors.csv")
  model_heuristic_values_path <- file.path(CFG$qc_dir, "mortality_model_heuristic_values.csv")
  model_prediction_diagnostics_year_sex_path <- file.path(CFG$qc_dir, "mortality_model_prediction_diagnostics_year_sex.csv")
  model_prediction_diagnostics_age_year_sex_path <- file.path(CFG$qc_dir, "mortality_model_prediction_diagnostics_age_year_sex.csv")
  model_candidate_validation_path <- file.path(CFG$qc_dir, "mortality_model_candidate_validation.csv")
  model_warning_summary_path <- file.path(CFG$qc_dir, "mortality_model_warning_summary.csv")
  model_coefficient_assessment_path <- file.path(CFG$qc_dir, "mortality_model_coefficient_assessment.csv")
  model_statistical_assessment_path <- file.path(CFG$qc_dir, "mortality_model_statistical_assessment.csv")
  model_fit_metrics_compact_path <- file.path(CFG$qc_dir, "mortality_model_fit_metrics_compact.csv")
  model_summary_dir <- file.path(CFG$qc_dir, "model_summaries_txt")
  qc_mass_by_cause_path <- file.path(CFG$qc_dir, "qc_mass_preservation_by_cause.csv")
  qc_top_absurd_rates_path <- file.path(CFG$qc_dir, "qc_top_absurd_rates.csv")
  qc_temporal_roughness_path <- file.path(CFG$qc_dir, "qc_temporal_roughness.csv")
  qc_temporal_roughness_summary_path <- file.path(CFG$qc_dir, "qc_temporal_roughness_summary.csv")
  qc_top_serrucho_path <- file.path(CFG$qc_dir, "qc_top_serrucho.csv")
  qc_pandemic_window_zero_enforcement_path <- file.path(CFG$qc_dir, "qc_pandemic_window_zero_enforcement.csv")
  
  fwrite(qc_method_summary, qc_method_summary_path)
  fwrite(qc_total_preservation, qc_total_preservation_path)
  fwrite(qc_rate_flags, qc_rate_flags_path)
  fwrite(qc_top_borrowed, qc_top_borrowed_path)
  fwrite(qc_year_totals, qc_year_totals_path)
  fwrite(mortality_data_sufficiency_audit, suff_audit_path)
  fwrite(mortality_model_attempt_log, attempt_log_path)
  fwrite(mortality_model_registry, model_registry_path)
  fwrite(mortality_model_fit_metrics, model_fit_metrics_path)
  fwrite(mortality_model_coefficients, model_coefficients_path)
  fwrite(mortality_model_smooth_terms, model_smooth_terms_path)
  fwrite(mortality_model_recalibration_factors, model_recalibration_factors_path)
  fwrite(mortality_model_heuristic_values, model_heuristic_values_path)
  fwrite(mortality_model_prediction_diagnostics_year_sex, model_prediction_diagnostics_year_sex_path)
  fwrite(mortality_model_prediction_diagnostics_age_year_sex, model_prediction_diagnostics_age_year_sex_path)
  fwrite(mortality_model_candidate_validation, model_candidate_validation_path)
  fwrite(mortality_model_warning_summary, model_warning_summary_path)
  fwrite(mortality_model_coefficient_assessment, model_coefficient_assessment_path)
  fwrite(mortality_model_statistical_assessment, model_statistical_assessment_path)
  fwrite(mortality_model_fit_metrics_compact, model_fit_metrics_compact_path)
  dir.create(model_summary_dir, recursive = TRUE, showWarnings = FALSE)
  if (length(model_summary_list)) {
    for (sm_i in model_summary_list) {
      summary_path <- file.path(
        model_summary_dir,
        sprintf("cause_%s_method_%s_attempt_%s_summary.txt", sm_i$cause_concept_id, sm_i$method_code, sm_i$attempt_order)
      )
      writeLines(c(
        paste("cause_concept_id:", sm_i$cause_concept_id),
        paste("cause_name:", sm_i$cause_name),
        paste("method_code:", sm_i$method_code),
        paste("attempt_order:", sm_i$attempt_order),
        "",
        sm_i$summary_text
      ), summary_path, useBytes = TRUE)
    }
  }
  fwrite(qc_mass_preservation_by_cause, qc_mass_by_cause_path)
  fwrite(qc_top_absurd_rates, qc_top_absurd_rates_path)
  fwrite(qc_temporal_roughness, qc_temporal_roughness_path)
  fwrite(qc_temporal_roughness_summary, qc_temporal_roughness_summary_path)
  fwrite(qc_top_serrucho, qc_top_serrucho_path)
  fwrite(qc_pandemic_window_zero_enforcement, qc_pandemic_window_zero_enforcement_path)
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(out),
    n_cols = ncol(out),
    notes = "CSV tasas suavizadas Poisson-GAM solo para causas terminales, con auditoría y fallbacks"
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
    notes = "Parquet tasas suavizadas Poisson-GAM solo para causas terminales, con auditoría y fallbacks"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "dictionary_ext",
    artifact_path = out_dict,
    n_rows = NA_integer_,
    n_cols = NA_integer_,
    notes = "Diccionario extendido mortality_rate_cause_smoothed"
  )
  
  for (p in c(
    qc_method_summary_path,
    qc_total_preservation_path,
    qc_rate_flags_path,
    qc_top_borrowed_path,
    qc_year_totals_path,
    suff_audit_path,
    attempt_log_path,
    model_registry_path,
    model_fit_metrics_path,
    model_coefficients_path,
    model_smooth_terms_path,
    model_recalibration_factors_path,
    model_heuristic_values_path,
    model_prediction_diagnostics_year_sex_path,
    model_prediction_diagnostics_age_year_sex_path,
    model_candidate_validation_path,
    model_warning_summary_path,
    model_coefficient_assessment_path,
    model_statistical_assessment_path,
    model_fit_metrics_compact_path,
    qc_mass_by_cause_path,
    qc_top_absurd_rates_path,
    qc_temporal_roughness_path,
    qc_temporal_roughness_summary_path,
    qc_top_serrucho_path,
    qc_sex_specific_mismatch_positive_smoothed_path,
    qc_pandemic_window_zero_enforcement_path,
    file.path(CFG$qc_dir, "qc_geo_contract.csv")
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
      notes = "QC 09_build_mortality_rates"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "09_build_mortality_rates completado")
  
  msg("OK -> CSV: ", out_csv)
  msg("OK -> Parquet: ", out_parquet)
  msg("OK -> Dictionary: ", out_dict)
  msg("OK -> QC dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
