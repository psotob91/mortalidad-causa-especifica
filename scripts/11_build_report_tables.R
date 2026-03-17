#!/usr/bin/env Rscript

# ============================================================
# 11_build_report_tables.R
# ------------------------------------------------------------
# Objetivo:
#   Construir tablas maestras y tablas reportables finales a
#   partir de:
#     - mortality_rate_cause_smoothed_reconciled
#     - avp_yll_cause_reconciled
#     - cause_master
#     - mortality_model_registry
#     - mortality_data_sufficiency_audit
#
# Productos principales:
#   - mortality_report_long
#   - avp_report_long
#   - table_method_by_cause
#
# Productos adicionales:
#   - tablas reportables ANUALES
#   - tablas top causas por año
#   - share de edad simple
#
# QC mínimo:
#   - no negativos
#   - tasas = abs / población
#   - aditividad sexo
#   - aditividad edad
#   - aditividad geográfica
#   - aditividad jerárquica de causas
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

CFG <- list(
  version = "v0.4.1_report_tables_annual_totals",
  dataset_id = "report_tables_mortality_avp",
  table_name = "report_tables_mortality_avp",
  
  input_mort_candidates = c(
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv")
  ),
  
  input_avp_candidates = c(
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.parquet"),
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  input_model_registry_candidates = c(
    here("data", "derived", "qc", "09_build_mortality_rates", "mortality_model_registry.csv")
  ),
  
  input_sufficiency_candidates = c(
    here("data", "derived", "qc", "09_build_mortality_rates", "mortality_data_sufficiency_audit.csv")
  ),
  
  years = 2018:2024,
  national_additive_id = 9000L,
  regional_scope = "regional",
  national_scope = "national",
  all_age_label = "Todas las edades",
  both_sex_id = 3L,
  both_sex_label = "Ambos",
  rate_multiplier = 100000,
  
  report_cause_levels = c(1L, 2L, 3L),
  keep_cause_levels = c(0L, 1L, 2L, 3L, 4L),
  
  age_breaks = c(-Inf, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf),
  age_labels = c("0", "1-4", "5-14", "15-24", "25-34", "35-44",
                 "45-54", "55-64", "65-74", "75-84", "85+"),
  
  top_n = 10L,
  
  qc_abs_tol = 1e-8,
  qc_rel_tol = 1e-10,
  
  out_dir_final = here("data", "final", "report_tables"),
  out_dir_tables = here("data", "derived", "tables"),
  out_dir_share = here("data", "derived", "share"),
  qc_dir = here("data", "derived", "qc", "11_build_report_tables"),
  
  verbose = TRUE
)

for (d in c(CFG$out_dir_final, CFG$out_dir_tables, CFG$out_dir_share, CFG$qc_dir)) {
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

export_csv_parquet_dict <- function(dt, stem, out_dir) {
  csv_path <- file.path(out_dir, paste0(stem, ".csv"))
  parquet_path <- file.path(out_dir, paste0(stem, ".parquet"))
  dict_path <- file.path(out_dir, paste0(stem, "_dictionary_ext.csv"))
  
  write_csv_parquet(dt, csv_path = csv_path, parquet_path = parquet_path)
  fwrite(build_dictionary_ext(dt), dict_path)
  
  list(csv = csv_path, parquet = parquet_path, dict = dict_path)
}

sex_label_fun <- function(sex_id) {
  fifelse(
    sex_id == 8507L, "Hombre",
    fifelse(sex_id == 8532L, "Mujer",
            fifelse(sex_id == CFG$both_sex_id, CFG$both_sex_label, as.character(sex_id)))
  )
}

location_scope_fun <- function(location_id) {
  fifelse(location_id == CFG$national_additive_id, CFG$national_scope, CFG$regional_scope)
}

make_age_group <- function(age) {
  cut(
    age,
    breaks = CFG$age_breaks,
    labels = CFG$age_labels,
    right = FALSE,
    ordered_result = TRUE
  )
}

add_both_sex <- function(dt_long) {
  out <- dt_long[
    sex_id %in% c(8507L, 8532L),
    .(
      population = sum(population, na.rm = TRUE),
      metric_abs = sum(metric_abs, na.rm = TRUE)
    ),
    by = .(
      year_id, location_id, location_scope,
      age_group, cause_concept_id, cause_level, cause_name, metric_type
    )
  ]
  out[, sex_id := CFG$both_sex_id]
  out[, sex_label := CFG$both_sex_label]
  out[, metric_rate := safe_rate(metric_abs, population, CFG$rate_multiplier)]
  
  setcolorder(out, names(dt_long))
  out[]
}

add_all_age_group <- function(dt_long) {
  out <- dt_long[, .(
    population = sum(population, na.rm = TRUE),
    metric_abs = sum(metric_abs, na.rm = TRUE)
  ), by = .(
    year_id, location_id, location_scope,
    sex_id, sex_label, cause_concept_id, cause_level, cause_name, metric_type
  )]
  out[, age_group := factor(CFG$all_age_label, levels = c(CFG$age_labels, CFG$all_age_label), ordered = TRUE)]
  out[, metric_rate := safe_rate(metric_abs, population, CFG$rate_multiplier)]
  
  setcolorder(out, names(dt_long))
  out[]
}

top_causes_table <- function(dt, scope = c("national", "regional"), by_sex = FALSE, metric_label = "mortality") {
  scope <- match.arg(scope)
  
  x <- copy(dt)[metric_type == metric_label]
  
  if (scope == "national") {
    x <- x[
      location_id == CFG$national_additive_id &
        as.character(age_group) == CFG$all_age_label
    ]
  } else {
    x <- x[
      location_scope == CFG$regional_scope &
        as.character(age_group) == CFG$all_age_label
    ]
  }
  
  if (by_sex) {
    x <- x[sex_label %in% c("Hombre", "Mujer", "Ambos")]
    grp <- c("year_id", "location_id", "location_scope", "sex_label")
  } else {
    x <- x[sex_label == CFG$both_sex_label]
    grp <- c("year_id", "location_id", "location_scope")
  }
  
  x <- x[cause_level %in% CFG$report_cause_levels]
  
  x[, rank_metric := frank(-metric_abs, ties.method = "dense"), by = grp]
  x[rank_metric <= CFG$top_n][order(year_id, location_id, sex_label, rank_metric)]
}

qc_rate_recalculation <- function(dt, dataset_label) {
  x <- copy(dt)
  x[, metric_rate_recalc := safe_rate(metric_abs, population, CFG$rate_multiplier)]
  x[, diff := metric_rate - metric_rate_recalc]
  x[, tol := pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(metric_rate), abs(metric_rate_recalc), 1))]
  x[, bad := abs(diff) > tol | xor(is.na(metric_rate), is.na(metric_rate_recalc))]
  x[, .(
    dataset = dataset_label,
    n_rows = .N,
    n_bad = sum(bad, na.rm = TRUE),
    max_abs_diff = suppressWarnings(max(abs(diff), na.rm = TRUE))
  )]
}

qc_sex_additivity <- function(dt, dataset_label) {
  x <- dt[sex_label %in% c("Hombre", "Mujer", "Ambos")]
  
  hm <- x[sex_label %in% c("Hombre", "Mujer"),
          .(sum_hm = sum(metric_abs, na.rm = TRUE)),
          by = .(year_id, location_id, age_group, cause_concept_id, metric_type)]
  
  both <- x[sex_label == "Ambos",
            .(both_val = sum(metric_abs, na.rm = TRUE)),
            by = .(year_id, location_id, age_group, cause_concept_id, metric_type)]
  
  chk <- merge(hm, both,
               by = c("year_id", "location_id", "age_group", "cause_concept_id", "metric_type"),
               all = FALSE)
  
  chk[, diff := both_val - sum_hm]
  chk[, tol := pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(both_val), abs(sum_hm), 1))]
  chk[, bad := abs(diff) > tol]
  
  chk[, .(
    dataset = dataset_label,
    n_rows = .N,
    n_bad = sum(bad, na.rm = TRUE),
    max_abs_diff = suppressWarnings(max(abs(diff), na.rm = TRUE))
  )]
}

qc_age_additivity <- function(dt, dataset_label) {
  x <- copy(dt)
  
  parts <- x[as.character(age_group) != CFG$all_age_label,
             .(sum_parts = sum(metric_abs, na.rm = TRUE)),
             by = .(year_id, location_id, sex_label, cause_concept_id, metric_type)]
  
  total <- x[as.character(age_group) == CFG$all_age_label,
             .(all_age_val = sum(metric_abs, na.rm = TRUE)),
             by = .(year_id, location_id, sex_label, cause_concept_id, metric_type)]
  
  chk <- merge(parts, total,
               by = c("year_id", "location_id", "sex_label", "cause_concept_id", "metric_type"),
               all = FALSE)
  
  chk[, diff := all_age_val - sum_parts]
  chk[, tol := pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(all_age_val), abs(sum_parts), 1))]
  chk[, bad := abs(diff) > tol]
  
  chk[, .(
    dataset = dataset_label,
    n_rows = .N,
    n_bad = sum(bad, na.rm = TRUE),
    max_abs_diff = suppressWarnings(max(abs(diff), na.rm = TRUE))
  )]
}

qc_geo_additivity <- function(dt, dataset_label) {
  x <- copy(dt)
  
  reg <- x[location_scope == CFG$regional_scope,
           .(sum_reg = sum(metric_abs, na.rm = TRUE)),
           by = .(year_id, sex_label, age_group, cause_concept_id, metric_type)]
  
  nat <- x[location_id == CFG$national_additive_id,
           .(nat_val = sum(metric_abs, na.rm = TRUE)),
           by = .(year_id, sex_label, age_group, cause_concept_id, metric_type)]
  
  chk <- merge(reg, nat,
               by = c("year_id", "sex_label", "age_group", "cause_concept_id", "metric_type"),
               all = FALSE)
  
  chk[, diff := nat_val - sum_reg]
  chk[, tol := pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(nat_val), abs(sum_reg), 1))]
  chk[, bad := abs(diff) > tol]
  
  chk[, .(
    dataset = dataset_label,
    n_rows = .N,
    n_bad = sum(bad, na.rm = TRUE),
    max_abs_diff = suppressWarnings(max(abs(diff), na.rm = TRUE))
  )]
}

qc_cause_additivity <- function(dt, cm, dataset_label) {
  edges <- unique(cm[!is.na(parent_concept_id) & cause_concept_id != parent_concept_id,
                     .(parent_concept_id, child_concept_id = cause_concept_id)])
  
  if (nrow(edges) == 0L) {
    return(data.table(dataset = dataset_label, n_rows = 0L, n_bad = 0L, max_abs_diff = NA_real_))
  }
  
  child_sum <- merge(
    dt[, .(year_id, location_id, sex_label, age_group, cause_concept_id, metric_type, metric_abs)],
    edges,
    by.x = "cause_concept_id",
    by.y = "child_concept_id",
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(children_sum = sum(metric_abs, na.rm = TRUE)),
    by = .(year_id, location_id, sex_label, age_group, cause_concept_id = parent_concept_id, metric_type)
  ]
  
  parent_val <- dt[, .(
    year_id, location_id, sex_label, age_group, cause_concept_id, metric_type,
    parent_val = metric_abs
  )]
  
  chk <- merge(
    child_sum, parent_val,
    by = c("year_id", "location_id", "sex_label", "age_group", "cause_concept_id", "metric_type"),
    all = FALSE
  )
  
  chk[, diff := parent_val - children_sum]
  chk[, tol := pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(parent_val), abs(children_sum), 1))]
  chk[, bad := abs(diff) > tol]
  
  chk[, .(
    dataset = dataset_label,
    n_rows = .N,
    n_bad = sum(bad, na.rm = TRUE),
    max_abs_diff = suppressWarnings(max(abs(diff), na.rm = TRUE))
  )]
}

tryCatch({
  
  msg("Resolviendo inputs.")
  
  mort_path <- first_existing(CFG$input_mort_candidates)
  avp_path  <- first_existing(CFG$input_avp_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  reg_path <- first_existing(CFG$input_model_registry_candidates)
  suff_path <- first_existing(CFG$input_sufficiency_candidates)
  
  if (is.na(mort_path)) stop("No encontré mortality_rate_cause_smoothed_reconciled.")
  if (is.na(avp_path)) stop("No encontré avp_yll_cause_reconciled.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  if (is.na(reg_path)) stop("No encontré mortality_model_registry.")
  if (is.na(suff_path)) stop("No encontré mortality_data_sufficiency_audit.")
  
  msg("Leyendo mortalidad reconciliada.")
  mort <- as.data.table(read_auto(mort_path))
  
  msg("Leyendo AVP reconciliado.")
  avp <- as.data.table(read_auto(avp_path))
  
  msg("Leyendo cause_master.")
  cm <- as.data.table(read_auto(cause_path))
  
  msg("Leyendo mortality_model_registry.")
  model_registry <- as.data.table(read_auto(reg_path))
  
  msg("Leyendo mortality_data_sufficiency_audit.")
  suff_audit <- as.data.table(read_auto(suff_path))
  
  req_mort <- c("year_id", "location_id", "sex_id", "age", "cause_concept_id",
                "cause_level", "cause_name", "population",
                "deaths_smoothed_consistent", "mortality_rate_smoothed_consistent")
  miss_mort <- setdiff(req_mort, names(mort))
  if (length(miss_mort) > 0L) {
    stop("Faltan columnas en mortality_rate_cause_smoothed_reconciled: ", paste(miss_mort, collapse = ", "))
  }
  
  req_avp <- c("year_id", "location_id", "sex_id", "age", "cause_concept_id",
               "cause_level", "population", "avp_abs", "avp_rate")
  miss_avp <- setdiff(req_avp, names(avp))
  if (length(miss_avp) > 0L) {
    stop("Faltan columnas en avp_yll_cause_reconciled: ", paste(miss_avp, collapse = ", "))
  }
  
  req_cm <- c("cause_concept_id", "cause_level", "cause_name", "parent_concept_id")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  req_reg <- c("cause_concept_id", "cause_name", "method_selected", "data_category",
               "aggregation_age", "aggregation_year", "aggregation_region",
               "years_with_deaths", "regions_with_deaths", "total_deaths_input")
  miss_reg <- setdiff(req_reg, names(model_registry))
  if (length(miss_reg) > 0L) {
    stop("Faltan columnas en mortality_model_registry: ", paste(miss_reg, collapse = ", "))
  }
  
  req_suff <- c("cause_concept_id", "rows_available", "rows_expected", "data_density")
  miss_suff <- setdiff(req_suff, names(suff_audit))
  if (length(miss_suff) > 0L) {
    stop("Faltan columnas en mortality_data_sufficiency_audit: ", paste(miss_suff, collapse = ", "))
  }
  
  mort <- mort[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    population = as.numeric(population),
    deaths_smoothed_consistent = as.numeric(deaths_smoothed_consistent),
    mortality_rate_smoothed_consistent = as.numeric(mortality_rate_smoothed_consistent)
  )][year_id %in% CFG$years & cause_level %in% CFG$keep_cause_levels]
  
  avp <- avp[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    population = as.numeric(population),
    avp_abs = as.numeric(avp_abs),
    avp_rate = as.numeric(avp_rate)
  )][year_id %in% CFG$years & cause_level %in% CFG$keep_cause_levels]
  
  cm <- unique(cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    parent_concept_id = as.integer(parent_concept_id)
  )])
  
  avp <- merge(
    avp,
    unique(cm[, .(cause_concept_id, cause_level, cause_name)]),
    by = c("cause_concept_id", "cause_level"),
    all.x = TRUE,
    sort = FALSE
  )
  
  mort[is.na(deaths_smoothed_consistent), deaths_smoothed_consistent := 0]
  avp[is.na(avp_abs), avp_abs := 0]
  
  mort[, sex_label := sex_label_fun(sex_id)]
  mort[, location_scope := location_scope_fun(location_id)]
  mort[, age_group := make_age_group(age)]
  
  avp[, sex_label := sex_label_fun(sex_id)]
  avp[, location_scope := location_scope_fun(location_id)]
  avp[, age_group := make_age_group(age)]
  
  msg("Agregando por grupo etario reportable.")
  
  mort_rep <- mort[, .(
    population = sum(population, na.rm = TRUE),
    deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE)
  ), by = .(
    year_id, location_id, location_scope, sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name
  )]
  mort_rep[, mortality_rate_smoothed_consistent := safe_rate(deaths_smoothed_consistent, population, CFG$rate_multiplier)]
  
  avp_rep <- avp[, .(
    population = sum(population, na.rm = TRUE),
    avp_abs = sum(avp_abs, na.rm = TRUE)
  ), by = .(
    year_id, location_id, location_scope, sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name
  )]
  avp_rep[, avp_rate := safe_rate(avp_abs, population, CFG$rate_multiplier)]
  
  msg("Construyendo tablas maestras largas.")
  
  mort_long <- mort_rep[, .(
    year_id, location_id, location_scope,
    sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name,
    population,
    metric_abs = deaths_smoothed_consistent,
    metric_rate = mortality_rate_smoothed_consistent
  )]
  mort_long[, metric_type := "mortality"]
  
  avp_long <- avp_rep[, .(
    year_id, location_id, location_scope,
    sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name,
    population,
    metric_abs = avp_abs,
    metric_rate = avp_rate
  )]
  avp_long[, metric_type := "avp"]
  
  mort_both <- add_both_sex(mort_long)
  mort_both[, metric_type := "mortality"]
  
  avp_both <- add_both_sex(avp_long)
  avp_both[, metric_type := "avp"]
  
  mort_long_all <- rbindlist(list(mort_long, mort_both), use.names = TRUE, fill = TRUE)
  avp_long_all  <- rbindlist(list(avp_long, avp_both), use.names = TRUE, fill = TRUE)
  
  mort_all_age <- add_all_age_group(mort_long_all)
  mort_all_age[, metric_type := "mortality"]
  
  avp_all_age <- add_all_age_group(avp_long_all)
  avp_all_age[, metric_type := "avp"]
  
  mortality_report_long <- rbindlist(list(mort_long_all, mort_all_age), use.names = TRUE, fill = TRUE)
  avp_report_long       <- rbindlist(list(avp_long_all, avp_all_age), use.names = TRUE, fill = TRUE)
  
  mortality_report_long[, run_id := run_id]
  avp_report_long[, run_id := run_id]
  
  setorder(mortality_report_long, year_id, location_id, sex_id, age_group, cause_level, cause_concept_id)
  setorder(avp_report_long, year_id, location_id, sex_id, age_group, cause_level, cause_concept_id)
  
  msg("Exportando tablas maestras largas.")
  mort_master_files <- export_csv_parquet_dict(
    mortality_report_long,
    "mortality_report_long",
    CFG$out_dir_final
  )
  avp_master_files <- export_csv_parquet_dict(
    avp_report_long,
    "avp_report_long",
    CFG$out_dir_final
  )
  
  msg("Exportando share de edad simple.")
  mort_share <- copy(mort)
  mort_share[, run_id := run_id]
  
  avp_share <- copy(avp)
  avp_share[, run_id := run_id]
  
  mort_share_files <- export_csv_parquet_dict(
    mort_share,
    "mortality_single_age_share",
    CFG$out_dir_share
  )
  avp_share_files <- export_csv_parquet_dict(
    avp_share,
    "avp_single_age_share",
    CFG$out_dir_share
  )
  
  msg("Construyendo table_method_by_cause.")
  table_method_by_cause <- merge(
    model_registry[, .(
      cause_concept_id, cause_name,
      method_selected, data_category,
      aggregation_age, aggregation_year, aggregation_region,
      total_deaths = total_deaths_input,
      years_with_deaths, regions_with_deaths
    )],
    suff_audit[, .(
      cause_concept_id, rows_available, rows_expected, data_density
    )],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  table_method_by_cause <- unique(table_method_by_cause)
  setorder(table_method_by_cause, data_category, method_selected, -total_deaths, cause_name)
  table_method_by_cause[, run_id := run_id]
  
  method_files <- export_csv_parquet_dict(
    table_method_by_cause,
    "table_method_by_cause",
    CFG$out_dir_final
  )
  
  msg("Construyendo tablas reportables anuales.")
  
  # ----------------------------------------------------------
  # NACIONAL TOTAL (sin sexo ni edad)
  # ----------------------------------------------------------
  tbl_nat_year_total_mort <- mortality_report_long[
    location_id == CFG$national_additive_id &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_nat_year_total_avp <- avp_report_long[
    location_id == CFG$national_additive_id &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # NACIONAL POR SEXO
  # ----------------------------------------------------------
  tbl_nat_year_sex_mort <- mortality_report_long[
    location_id == CFG$national_additive_id &
      as.character(age_group) == CFG$all_age_label &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_nat_year_sex_avp <- avp_report_long[
    location_id == CFG$national_additive_id &
      as.character(age_group) == CFG$all_age_label &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # NACIONAL POR EDAD
  # ----------------------------------------------------------
  tbl_nat_year_age_mort <- mortality_report_long[
    location_id == CFG$national_additive_id &
      sex_label == "Ambos" &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_nat_year_age_avp <- avp_report_long[
    location_id == CFG$national_additive_id &
      sex_label == "Ambos" &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # NACIONAL POR EDAD Y SEXO
  # ----------------------------------------------------------
  tbl_nat_year_age_sex_mort <- mortality_report_long[
    location_id == CFG$national_additive_id &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_nat_year_age_sex_avp <- avp_report_long[
    location_id == CFG$national_additive_id &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # REGIONAL TOTAL (sin sexo ni edad)
  # ----------------------------------------------------------
  tbl_reg_year_total_mort <- mortality_report_long[
    location_scope == CFG$regional_scope &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_reg_year_total_avp <- avp_report_long[
    location_scope == CFG$regional_scope &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # REGIONAL POR SEXO
  # ----------------------------------------------------------
  tbl_reg_year_sex_mort <- mortality_report_long[
    location_scope == CFG$regional_scope &
      as.character(age_group) == CFG$all_age_label &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_reg_year_sex_avp <- avp_report_long[
    location_scope == CFG$regional_scope &
      as.character(age_group) == CFG$all_age_label &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # REGIONAL POR EDAD
  # ----------------------------------------------------------
  tbl_reg_year_age_mort <- mortality_report_long[
    location_scope == CFG$regional_scope &
      sex_label == "Ambos" &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_reg_year_age_avp <- avp_report_long[
    location_scope == CFG$regional_scope &
      sex_label == "Ambos" &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # REGIONAL POR EDAD Y SEXO
  # ----------------------------------------------------------
  tbl_reg_year_age_sex_mort <- mortality_report_long[
    location_scope == CFG$regional_scope &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  tbl_reg_year_age_sex_avp <- avp_report_long[
    location_scope == CFG$regional_scope &
      sex_label %in% c("Hombre", "Mujer", "Ambos") &
      as.character(age_group) != CFG$all_age_label &
      cause_level %in% CFG$report_cause_levels
  ]
  
  # ----------------------------------------------------------
  # TOP CAUSAS POR AÑO
  # ----------------------------------------------------------
  top_nat_mort_total_year <- top_causes_table(
    dt = mortality_report_long,
    scope = "national",
    by_sex = FALSE,
    metric_label = "mortality"
  )
  
  top_nat_mort_sex_year <- top_causes_table(
    dt = mortality_report_long,
    scope = "national",
    by_sex = TRUE,
    metric_label = "mortality"
  )
  
  top_reg_mort_total_year <- top_causes_table(
    dt = mortality_report_long,
    scope = "regional",
    by_sex = FALSE,
    metric_label = "mortality"
  )
  
  top_reg_mort_sex_year <- top_causes_table(
    dt = mortality_report_long,
    scope = "regional",
    by_sex = TRUE,
    metric_label = "mortality"
  )
  
  top_nat_avp_total_year <- top_causes_table(
    dt = avp_report_long,
    scope = "national",
    by_sex = FALSE,
    metric_label = "avp"
  )
  
  top_nat_avp_sex_year <- top_causes_table(
    dt = avp_report_long,
    scope = "national",
    by_sex = TRUE,
    metric_label = "avp"
  )
  
  top_reg_avp_total_year <- top_causes_table(
    dt = avp_report_long,
    scope = "regional",
    by_sex = FALSE,
    metric_label = "avp"
  )
  
  top_reg_avp_sex_year <- top_causes_table(
    dt = avp_report_long,
    scope = "regional",
    by_sex = TRUE,
    metric_label = "avp"
  )
  
  tables_to_export <- list(
    tbl_nat_year_total_mort = tbl_nat_year_total_mort,
    tbl_nat_year_total_avp = tbl_nat_year_total_avp,
    tbl_nat_year_sex_mort = tbl_nat_year_sex_mort,
    tbl_nat_year_sex_avp = tbl_nat_year_sex_avp,
    tbl_nat_year_age_mort = tbl_nat_year_age_mort,
    tbl_nat_year_age_avp = tbl_nat_year_age_avp,
    tbl_nat_year_age_sex_mort = tbl_nat_year_age_sex_mort,
    tbl_nat_year_age_sex_avp = tbl_nat_year_age_sex_avp,
    tbl_reg_year_total_mort = tbl_reg_year_total_mort,
    tbl_reg_year_total_avp = tbl_reg_year_total_avp,
    tbl_reg_year_sex_mort = tbl_reg_year_sex_mort,
    tbl_reg_year_sex_avp = tbl_reg_year_sex_avp,
    tbl_reg_year_age_mort = tbl_reg_year_age_mort,
    tbl_reg_year_age_avp = tbl_reg_year_age_avp,
    tbl_reg_year_age_sex_mort = tbl_reg_year_age_sex_mort,
    tbl_reg_year_age_sex_avp = tbl_reg_year_age_sex_avp,
    top_nat_mort_total_year = top_nat_mort_total_year,
    top_nat_mort_sex_year = top_nat_mort_sex_year,
    top_reg_mort_total_year = top_reg_mort_total_year,
    top_reg_mort_sex_year = top_reg_mort_sex_year,
    top_nat_avp_total_year = top_nat_avp_total_year,
    top_nat_avp_sex_year = top_nat_avp_sex_year,
    top_reg_avp_total_year = top_reg_avp_total_year,
    top_reg_avp_sex_year = top_reg_avp_sex_year
  )
  
  table_files <- list()
  for (nm in names(tables_to_export)) {
    table_files[[nm]] <- export_csv_parquet_dict(
      tables_to_export[[nm]],
      nm,
      CFG$out_dir_tables
    )
  }
  
  msg("Corriendo QC final de tablas reportables.")
  
  qc_summary <- rbindlist(list(
    data.table(dataset = "mortality_report_long", n_rows = nrow(mortality_report_long), n_cols = ncol(mortality_report_long)),
    data.table(dataset = "avp_report_long", n_rows = nrow(avp_report_long), n_cols = ncol(avp_report_long)),
    data.table(dataset = "table_method_by_cause", n_rows = nrow(table_method_by_cause), n_cols = ncol(table_method_by_cause))
  ), use.names = TRUE, fill = TRUE)
  
  qc_missing <- rbindlist(list(
    mortality_report_long[, .(
      dataset = "mortality_report_long",
      n_missing_population = sum(is.na(population)),
      n_missing_metric_abs = sum(is.na(metric_abs)),
      n_missing_metric_rate = sum(is.na(metric_rate))
    )],
    avp_report_long[, .(
      dataset = "avp_report_long",
      n_missing_population = sum(is.na(population)),
      n_missing_metric_abs = sum(is.na(metric_abs)),
      n_missing_metric_rate = sum(is.na(metric_rate))
    )]
  ), use.names = TRUE, fill = TRUE)
  
  qc_negative <- rbindlist(list(
    mortality_report_long[, .(
      dataset = "mortality_report_long",
      n_negative_metric_abs = sum(metric_abs < 0, na.rm = TRUE),
      n_negative_metric_rate = sum(metric_rate < 0, na.rm = TRUE)
    )],
    avp_report_long[, .(
      dataset = "avp_report_long",
      n_negative_metric_abs = sum(metric_abs < 0, na.rm = TRUE),
      n_negative_metric_rate = sum(metric_rate < 0, na.rm = TRUE)
    )]
  ), use.names = TRUE, fill = TRUE)
  
  qc_top_rank <- rbindlist(list(
    top_nat_mort_total_year[, .(dataset = "top_nat_mort_total_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_nat_mort_sex_year[, .(dataset = "top_nat_mort_sex_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_reg_mort_total_year[, .(dataset = "top_reg_mort_total_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_reg_mort_sex_year[, .(dataset = "top_reg_mort_sex_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_nat_avp_total_year[, .(dataset = "top_nat_avp_total_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_nat_avp_sex_year[, .(dataset = "top_nat_avp_sex_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_reg_avp_total_year[, .(dataset = "top_reg_avp_total_year", bad_rank = sum(rank_metric > CFG$top_n))],
    top_reg_avp_sex_year[, .(dataset = "top_reg_avp_sex_year", bad_rank = sum(rank_metric > CFG$top_n))]
  ), use.names = TRUE, fill = TRUE)
  
  qc_duplicate_master_mort <- mortality_report_long[, .N, by = .(
    year_id, location_id, sex_id, age_group, cause_concept_id, metric_type
  )][N > 1]
  
  qc_duplicate_master_avp <- avp_report_long[, .N, by = .(
    year_id, location_id, sex_id, age_group, cause_concept_id, metric_type
  )][N > 1]
  
  qc_rate_recalc <- rbindlist(list(
    qc_rate_recalculation(mortality_report_long, "mortality_report_long"),
    qc_rate_recalculation(avp_report_long, "avp_report_long")
  ), use.names = TRUE, fill = TRUE)
  
  qc_sex_add <- rbindlist(list(
    qc_sex_additivity(mortality_report_long, "mortality_report_long"),
    qc_sex_additivity(avp_report_long, "avp_report_long")
  ), use.names = TRUE, fill = TRUE)
  
  qc_age_add <- rbindlist(list(
    qc_age_additivity(mortality_report_long, "mortality_report_long"),
    qc_age_additivity(avp_report_long, "avp_report_long")
  ), use.names = TRUE, fill = TRUE)
  
  qc_geo_add <- rbindlist(list(
    qc_geo_additivity(mortality_report_long, "mortality_report_long"),
    qc_geo_additivity(avp_report_long, "avp_report_long")
  ), use.names = TRUE, fill = TRUE)
  
  qc_cause_add <- rbindlist(list(
    qc_cause_additivity(mortality_report_long, cm, "mortality_report_long"),
    qc_cause_additivity(avp_report_long, cm, "avp_report_long")
  ), use.names = TRUE, fill = TRUE)
  
  qc_summary_path <- file.path(CFG$qc_dir, "qc_summary.csv")
  qc_missing_path <- file.path(CFG$qc_dir, "qc_missing.csv")
  qc_negative_path <- file.path(CFG$qc_dir, "qc_negative.csv")
  qc_top_rank_path <- file.path(CFG$qc_dir, "qc_top_rank.csv")
  qc_dup_mort_path <- file.path(CFG$qc_dir, "qc_duplicate_master_mort.csv")
  qc_dup_avp_path <- file.path(CFG$qc_dir, "qc_duplicate_master_avp.csv")
  qc_rate_recalc_path <- file.path(CFG$qc_dir, "qc_rate_recalculation.csv")
  qc_sex_add_path <- file.path(CFG$qc_dir, "qc_sex_additivity.csv")
  qc_age_add_path <- file.path(CFG$qc_dir, "qc_age_additivity.csv")
  qc_geo_add_path <- file.path(CFG$qc_dir, "qc_geo_additivity.csv")
  qc_cause_add_path <- file.path(CFG$qc_dir, "qc_cause_additivity.csv")
  
  fwrite(qc_summary, qc_summary_path)
  fwrite(qc_missing, qc_missing_path)
  fwrite(qc_negative, qc_negative_path)
  fwrite(qc_top_rank, qc_top_rank_path)
  fwrite(qc_duplicate_master_mort, qc_dup_mort_path)
  fwrite(qc_duplicate_master_avp, qc_dup_avp_path)
  fwrite(qc_rate_recalc, qc_rate_recalc_path)
  fwrite(qc_sex_add, qc_sex_add_path)
  fwrite(qc_age_add, qc_age_add_path)
  fwrite(qc_geo_add, qc_geo_add_path)
  fwrite(qc_cause_add, qc_cause_add_path)
  
  if (any(qc_negative$n_negative_metric_abs > 0) || any(qc_negative$n_negative_metric_rate > 0)) {
    stop("QC HARD FAIL: hay métricas negativas en tablas reportables.")
  }
  
  if (any(qc_top_rank$bad_rank > 0)) {
    stop("QC HARD FAIL: alguna tabla top causas excede el top_n.")
  }
  
  if (nrow(qc_duplicate_master_mort) > 0 || nrow(qc_duplicate_master_avp) > 0) {
    stop("QC HARD FAIL: duplicados en tablas maestras.")
  }
  
  if (any(qc_rate_recalc$n_bad > 0)) {
    stop("QC HARD FAIL: las tasas derivadas no coinciden con abs/población.")
  }
  
  if (any(qc_sex_add$n_bad > 0)) {
    stop("QC HARD FAIL: falla aditividad por sexo.")
  }
  
  if (any(qc_age_add$n_bad > 0)) {
    stop("QC HARD FAIL: falla aditividad por edad.")
  }
  
  if (any(qc_geo_add$n_bad > 0)) {
    stop("QC HARD FAIL: falla aditividad geográfica.")
  }
  
  if (any(qc_cause_add$n_bad > 0)) {
    stop("QC HARD FAIL: falla aditividad jerárquica de causas.")
  }
  
  all_artifacts <- c(
    mort_master_files$csv, mort_master_files$parquet, mort_master_files$dict,
    avp_master_files$csv, avp_master_files$parquet, avp_master_files$dict,
    method_files$csv, method_files$parquet, method_files$dict,
    mort_share_files$csv, mort_share_files$parquet, mort_share_files$dict,
    avp_share_files$csv, avp_share_files$parquet, avp_share_files$dict
  )
  
  for (obj in table_files) {
    all_artifacts <- c(all_artifacts, obj$csv, obj$parquet, obj$dict)
  }
  
  qc_files <- c(
    qc_summary_path,
    qc_missing_path,
    qc_negative_path,
    qc_top_rank_path,
    qc_dup_mort_path,
    qc_dup_avp_path,
    qc_rate_recalc_path,
    qc_sex_add_path,
    qc_age_add_path,
    qc_geo_add_path,
    qc_cause_add_path
  )
  
  for (p in all_artifacts) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = if (grepl("_dictionary_ext\\.csv$", p)) "dictionary_ext" else "final_dataset",
      artifact_path = p,
      n_rows = tryCatch(if (grepl("\\.parquet$", p)) nrow(as.data.table(read_parquet(p))) else nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(if (grepl("\\.parquet$", p)) ncol(as.data.table(read_parquet(p))) else ncol(fread(p)), error = function(e) NA_integer_),
      notes = "Salida final 11_build_report_tables anual con totales"
    )
  }
  
  for (p in qc_files) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "qc",
      artifact_path = p,
      n_rows = tryCatch(nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(ncol(fread(p)), error = function(e) NA_integer_),
      notes = "QC 11_build_report_tables"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "11_build_report_tables completado")
  
  msg("OK -> mortality_report_long: ", mort_master_files$csv)
  msg("OK -> avp_report_long: ", avp_master_files$csv)
  msg("OK -> table_method_by_cause: ", method_files$csv)
  msg("OK -> Tables dir: ", CFG$out_dir_tables)
  msg("OK -> QC dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})