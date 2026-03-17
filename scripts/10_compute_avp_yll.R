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
  library(yaml)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

CFG <- list(
  version = "v0.3.0_avp_reconciled_final",
  dataset_id = "avp_yll_cause_reconciled",
  table_name = "avp_yll_cause_reconciled",
  
  input_mortality_candidates = c(
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  external_yaml_path = here("config", "external_sources.yml"),
  
  out_dir = here("data", "final", "avp_yll_cause_reconciled"),
  qc_dir  = here("data", "derived", "qc", "10_compute_avp_yll"),
  
  years = 2018:2024,
  base_locations = 1:25,
  national_additive_id = 9000L,
  keep_cause_levels = c(0L, 1L, 2L, 3L, 4L),
  valid_sexes = c(8507L, 8532L),
  age_min = 0L,
  age_max = 110L,
  
  rate_multiplier = 100000,
  max_avp_rate_warn_per_100k = 20000,
  max_avp_rate_hard_per_100k = 100000,
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
  
  lt_path <- resolve_external_path2(CFG$external_yaml_path, "life_table_standard_single_age")
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
    cause_level = as.integer(cause_level)
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
  
  # Resolver tabla estándar por sexo + edad
  lt <- lt[
    ,
    .(
      ex_standard = ex_standard[1],
      standard_source = standard_source[1],
      standard_version = standard_version[1]
    ),
    by = .(sex_id, age)
  ]
  
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
  
  # ----------------------------------------------------------
  # Cálculo AVP/YLL
  # ----------------------------------------------------------
  out[, avp_abs := deaths_smoothed_consistent * ex_standard]
  out[, yll_abs := avp_abs]
  
  out[, avp_abs_from_deaths_final := deaths_final * ex_standard]
  out[, yll_abs_from_deaths_final := avp_abs_from_deaths_final]
  
  out[, avp_rate := safe_rate(avp_abs, population, CFG$rate_multiplier)]
  out[, yll_rate := avp_rate]
  out[, avp_rate_unit := paste0("per_", CFG$rate_multiplier)]
  
  out[, avp_method := "deaths_smoothed_consistent_x_ex_standard"]
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
  
  qc_rate_plausibility <- out[, .(
    n_rows = .N,
    max_avp_rate = suppressWarnings(max(avp_rate, na.rm = TRUE)),
    max_yll_rate = suppressWarnings(max(yll_rate, na.rm = TRUE)),
    p99_avp_rate = suppressWarnings(quantile(avp_rate, 0.99, na.rm = TRUE)),
    p999_avp_rate = suppressWarnings(quantile(avp_rate, 0.999, na.rm = TRUE))
  )]
  
  qc_top_avp_rate_cells <- out[
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
  
  # ----------------------------------------------------------
  # Export QC temprano
  # ----------------------------------------------------------
  qc_missing_ex_path <- file.path(CFG$qc_dir, "qc_missing_ex.csv")
  qc_nonnegative_path <- file.path(CFG$qc_dir, "qc_nonnegative.csv")
  qc_by_level_path <- file.path(CFG$qc_dir, "qc_by_level.csv")
  qc_year_sex_path <- file.path(CFG$qc_dir, "qc_year_sex.csv")
  qc_duplicate_pk_path <- file.path(CFG$qc_dir, "qc_duplicate_pk.csv")
  qc_rate_plausibility_path <- file.path(CFG$qc_dir, "qc_rate_plausibility.csv")
  qc_top_avp_rate_cells_path <- file.path(CFG$qc_dir, "qc_top_avp_rate_cells.csv")
  qc_top_avp_abs_cells_path <- file.path(CFG$qc_dir, "qc_top_avp_abs_cells.csv")
  qc_ratio_avp_vs_deaths_path <- file.path(CFG$qc_dir, "qc_ratio_avp_vs_deaths.csv")
  qc_standard_table_summary_path <- file.path(CFG$qc_dir, "qc_standard_table_summary.csv")
  
  fwrite(qc_missing_ex, qc_missing_ex_path)
  fwrite(qc_nonnegative, qc_nonnegative_path)
  fwrite(qc_by_level, qc_by_level_path)
  fwrite(qc_year_sex, qc_year_sex_path)
  fwrite(qc_duplicate_pk, qc_duplicate_pk_path)
  fwrite(qc_rate_plausibility, qc_rate_plausibility_path)
  fwrite(qc_top_avp_rate_cells, qc_top_avp_rate_cells_path)
  fwrite(qc_top_avp_abs_cells, qc_top_avp_abs_cells_path)
  fwrite(qc_ratio_avp_vs_deaths, qc_ratio_avp_vs_deaths_path)
  fwrite(qc_standard_table_summary, qc_standard_table_summary_path)
  
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
  
  if (nrow(qc_duplicate_pk) > 0L) {
    stop("QC HARD FAIL: PK duplicada en AVP/YLL reconciliado.")
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
    qc_top_avp_rate_cells_path,
    qc_top_avp_abs_cells_path,
    qc_ratio_avp_vs_deaths_path,
    qc_standard_table_summary_path,
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