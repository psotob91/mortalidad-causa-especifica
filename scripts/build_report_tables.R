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
  library(readxl)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

parse_int_vec_env <- function(name, default) {
  raw <- Sys.getenv(name, unset = "")
  if (!nzchar(raw)) return(as.integer(default))
  vals <- trimws(unlist(strsplit(raw, ",")))
  vals <- vals[nzchar(vals)]
  out <- suppressWarnings(as.integer(vals))
  if (length(out) == 0L || any(is.na(out))) return(as.integer(default))
  out
}

CFG <- list(
  version = "v0.5.0_report_tables_all_causes_annexes",
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
  
  input_normalized_candidates = c(
    here("data", "final", "death_record_normalized", "death_record_normalized.csv"),
    here("data", "final", "death_record_normalized", "death_record_normalized.parquet")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  input_model_registry_candidates = c(
    resolve_existing_qc_path("build_mortality_rates", "mortality_model_registry.csv")
  ),
  
  input_sufficiency_candidates = c(
    resolve_existing_qc_path("build_mortality_rates", "mortality_data_sufficiency_audit.csv")
  ),
  
  years = 2018:2024,
  national_additive_id = 9000L,
  regional_scope = "regional",
  national_scope = "national",
  all_age_label = "Todas las edades",
  both_sex_id = 3L,
  both_sex_label = "Ambos",
  rate_multiplier = 100000,
  
  report_cause_levels = parse_int_vec_env("MORTALITY_REPORT_CAUSE_LEVELS", c(0L, 1L, 2L, 3L, 4L)),
  top_cause_levels = parse_int_vec_env("MORTALITY_TOP_CAUSE_LEVELS", c(1L, 2L, 3L, 4L)),
  keep_cause_levels = c(0L, 1L, 2L, 3L, 4L),
  
  age_breaks = c(-Inf, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, Inf),
  age_labels = c("0", "1-4", "5-14", "15-24", "25-34", "35-44",
                 "45-54", "55-64", "65-74", "75-84", "85+"),
  
  top_n = Inf,
  
  qc_abs_tol = 1e-8,
  qc_rel_tol = 1e-10,
  
  out_dir_final = here("data", "final", "report_tables"),
  out_dir_tables = here("data", "derived", "tables"),
  out_dir_share = here("data", "derived", "share"),
  qc_dir = qc_dir_path("build_report_tables"),
  
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

clean_chr <- function(x) {
  x <- as.character(x)
  x <- trimws(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}

make_path_key <- function(level1 = NA_character_, level2 = NA_character_, level3 = NA_character_, level4 = NA_character_) {
  paste(
    fifelse(is.na(level1), "", level1),
    fifelse(is.na(level2), "", level2),
    fifelse(is.na(level3), "", level3),
    fifelse(is.na(level4), "", level4),
    sep = " || "
  )
}

read_raw_cause_catalog <- function() {
  path <- here("data", "raw", "cause_mapping", "codificacion-causas-enfermedad.xlsx")
  if (!file.exists(path)) {
    stop("No encontré data/raw/cause_mapping/codificacion-causas-enfermedad.xlsx")
  }
  
  raw <- as.data.table(readxl::read_excel(path, sheet = "OMS-Enfermedades"))
  req <- c("CÓDIGO", "NIVEL_01", "NIVEL_02", "NIVEL_03", "NIVEL_04", "cie10", "regex_r")
  miss <- setdiff(req, names(raw))
  if (length(miss) > 0L) {
    stop("Faltan columnas en codificacion-causas-enfermedad.xlsx: ", paste(miss, collapse = ", "))
  }
  
  src <- raw[, .(
    codigo = suppressWarnings(as.integer(`CÓDIGO`)),
    level_1_name = clean_chr(NIVEL_01),
    level_2_name = clean_chr(NIVEL_02),
    level_3_name = clean_chr(NIVEL_03),
    level_4_name = clean_chr(NIVEL_04),
    cie10 = clean_chr(cie10),
    icd10_regex = clean_chr(regex_r)
  )]
  src <- src[!(is.na(level_1_name) & is.na(level_2_name) & is.na(level_3_name) & is.na(level_4_name))]
  
  l1 <- unique(src[!is.na(level_1_name), .(
    cause_level = 1L,
    level_1_name,
    level_2_name = NA_character_,
    level_3_name = NA_character_,
    level_4_name = NA_character_
  )])
  l2 <- unique(src[!is.na(level_2_name), .(
    cause_level = 2L,
    level_1_name,
    level_2_name,
    level_3_name = NA_character_,
    level_4_name = NA_character_
  )])
  l3 <- unique(src[!is.na(level_3_name), .(
    cause_level = 3L,
    level_1_name,
    level_2_name,
    level_3_name,
    level_4_name = NA_character_
  )])
  l4 <- unique(src[!is.na(level_4_name), .(
    cause_level = 4L,
    level_1_name,
    level_2_name,
    level_3_name,
    level_4_name
  )])
  
  out <- rbindlist(list(l1, l2, l3, l4), use.names = TRUE, fill = TRUE)
  out[, path_key := make_path_key(level_1_name, level_2_name, level_3_name, level_4_name)]
  unique(out[, .(
    path_key,
    source_in_raw_catalog = TRUE
  )])
}

make_display_name <- function(level, cause_name) {
  lvl <- pmax(as.integer(level) - 1L, 0L)
  prefix <- ifelse(lvl > 0L, paste0(strrep("-> ", lvl)), "")
  paste0(prefix, as.character(cause_name))
}

display_cause_name <- function(x) {
  x <- as.character(x)
  map <- c(
    "Enfermedades respiratorias" = "Enfermedades respiratorias crónicas",
    "Otras causas" = "Otras causas relacionadas con la pandemia",
    "Other pandemic related mortality (OPRM)" = "Otras muertes relacionadas con la pandemia (OPRM)",
    "IV. Other /pandemic-related" = "Otras causas relacionadas con la pandemia",
    "Pandemic explicit causes" = "Causas pandémicas explícitas",
    "Pandemic residual causes" = "Causas residuales pandémicas"
  )
  hit <- match(x, names(map))
  x[!is.na(hit)] <- unname(map[hit[!is.na(hit)]])
  x
}

read_normalized_icd_codes <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    dt <- fread(path, select = "icd10_ucod_nodot")
  } else if (ext == "parquet") {
    dt <- as.data.table(read_auto(path))
    if (!"icd10_ucod_nodot" %in% names(dt)) {
      stop("El parquet de death_record_normalized no contiene icd10_ucod_nodot.")
    }
    dt <- dt[, .(icd10_ucod_nodot)]
  } else {
    stop("Extensión no soportada para death_record_normalized: ", path)
  }
  
  unique(na.omit(trimws(as.character(dt$icd10_ucod_nodot))))
}

collapse_observed_icd_codes <- function(codes, max_codes = 30L) {
  codes <- unique(sort(trimws(as.character(codes))))
  codes <- codes[nzchar(codes)]
  if (length(codes) == 0L) return(NA_character_)
  if (length(codes) <= max_codes) return(paste(codes, collapse = ", "))
  paste0(
    paste(head(codes, max_codes), collapse = ", "),
    " ... (+", length(codes) - max_codes, " más)"
  )
}

collapse_icd10_to_stem_ranges <- function(codes) {
  codes <- unique(sort(trimws(as.character(codes))))
  codes <- codes[nzchar(codes)]
  if (length(codes) == 0L) return(NA_character_)
  stems <- unique(sub("^([A-Z][0-9]{2}).*$", "\\1", gsub("\\.", "", codes)))
  stems <- stems[grepl("^[A-Z][0-9]{2}$", stems)]
  if (length(stems) == 0L) return(NA_character_)
  dt <- data.table(
    letter = substr(stems, 1, 1),
    num = as.integer(substr(stems, 2, 3))
  )[order(letter, num)]
  out <- dt[, {
    grp <- cumsum(c(1L, diff(num) != 1L))
    chunks <- split(num, grp)
    txt <- vapply(chunks, function(v) {
      if (length(v) == 1L) sprintf("%s%02d", unique(letter), v)
      else sprintf("%s%02d-%s%02d", unique(letter), min(v), unique(letter), max(v))
    }, character(1))
    .(piece = paste(txt, collapse = ", "))
  }, by = letter]
  paste(out$piece, collapse = ", ")
}

expand_icd10_stem_ranges <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return(character())
  parts <- trimws(unlist(strsplit(x, ",")))
  parts <- parts[nzchar(parts)]
  out <- character()
  for (p in parts) {
    if (grepl("^[A-Z][0-9]{2}-[A-Z][0-9]{2}$", p)) {
      left <- substr(p, 1, 3)
      right <- substr(p, 5, 7)
      if (substr(left, 1, 1) == substr(right, 1, 1)) {
        letter <- substr(left, 1, 1)
        nums <- seq.int(as.integer(substr(left, 2, 3)), as.integer(substr(right, 2, 3)))
        out <- c(out, sprintf("%s%02d", letter, nums))
      }
    } else if (grepl("^[A-Z][0-9]{2}$", p)) {
      out <- c(out, p)
    }
  }
  unique(out)
}

build_complete_cause_table <- function(dt, cause_catalog_subset, national = TRUE) {
  x <- copy(dt)
  x <- x[sex_label == CFG$both_sex_label & as.character(age_group) == CFG$all_age_label]
  if (national) {
    base <- unique(x[location_id == CFG$national_additive_id, .(
      year_id, location_id, location_name, location_scope,
      sex_id, sex_label, age_group, population
    )])
  } else {
    base <- unique(x[location_scope == CFG$regional_scope, .(
      year_id, location_id, location_name, location_scope,
      sex_id, sex_label, age_group, population
    )])
  }
  
  metric_slice <- unique(x[, .(
    year_id, location_id, cause_concept_id, cause_level, cause_name,
    metric_abs, metric_rate, metric_type
  )])
  
  base[, join_key := 1L]
  cause_catalog_subset[, join_key := 1L]
  expanded <- merge(base, cause_catalog_subset, by = "join_key", allow.cartesian = TRUE, sort = FALSE)
  expanded[, join_key := NULL]
  
  out <- merge(
    expanded,
    metric_slice,
    by = c("year_id", "location_id", "cause_concept_id", "cause_level", "cause_name"),
    all.x = TRUE,
    sort = FALSE
  )
  out[is.na(metric_abs), metric_abs := 0]
  out[is.na(metric_rate), metric_rate := 0]
  out[is.na(metric_type), metric_type := unique(x$metric_type)[1]]
  desired_order <- c(
    "year_id", "location_id", "location_name", "location_scope",
    "sex_id", "sex_label", "age_group",
    "cause_concept_id", "cause_level", "cause_name", "catalog_order",
    "population", "metric_abs", "metric_rate", "metric_type"
  )
  setcolorder(out, intersect(desired_order, names(out)))
  setorder(out, year_id, location_id, catalog_order, cause_name)
  out
}

read_location_master <- function() {
  path <- here("config", "maestro_location_dept.csv")
  if (!file.exists(path)) {
    stop("No encontré config/maestro_location_dept.csv")
  }
  
  loc <- fread(path)
  req <- c("location_id", "location_name")
  miss <- setdiff(req, names(loc))
  if (length(miss) > 0L) {
    stop("Faltan columnas en maestro_location_dept.csv: ", paste(miss, collapse = ", "))
  }
  
  unique(loc[, .(
    location_id = as.integer(location_id),
    location_name = as.character(location_name)
  )])
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
      metric_abs = sum(metric_abs, na.rm = TRUE),
      location_name = unique(location_name)[1]
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
    metric_abs = sum(metric_abs, na.rm = TRUE),
    location_name = unique(location_name)[1]
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
  
  x <- x[cause_level %in% CFG$top_cause_levels]
  
  x[, rank_metric := frank(-metric_abs, ties.method = "dense"), by = grp]
  if (is.finite(CFG$top_n)) {
    x <- x[rank_metric <= CFG$top_n]
  }
  x[order(year_id, location_id, sex_label, rank_metric)]
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
  normalized_path <- first_existing(CFG$input_normalized_candidates)
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
  
  msg("Leyendo catalogo crudo de causas.")
  raw_cause_catalog <- read_raw_cause_catalog()
  
  msg("Leyendo maestro de departamentos.")
  loc_master <- read_location_master()
  
  msg("Leyendo mortality_model_registry.")
  model_registry <- as.data.table(read_auto(reg_path))
  
  msg("Leyendo mortality_data_sufficiency_audit.")
  suff_audit <- as.data.table(read_auto(suff_path))
  
  observed_icd_codes <- NULL
  if (!is.na(normalized_path)) {
    msg("Leyendo códigos CIE-10 observados desde death_record_normalized.")
    observed_icd_codes <- read_normalized_icd_codes(normalized_path)
  } else {
    warning("No encontré death_record_normalized para enriquecer el catálogo con CIE-10 observados.")
  }
  
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
  
  cm_cols <- unique(c(
    "cause_concept_id", "cause_level", "cause_name", "parent_concept_id",
    intersect(c(
      "cause_code", "is_terminal", "is_covid_related",
      "level_1_name", "level_2_name", "level_3_name", "level_4_name",
      "cie10", "icd10_regex", "yll_flag", "yld_flag"
    ), names(cm))
  ))
  cm <- unique(cm[, ..cm_cols])
  cm[, cause_concept_id := as.integer(cause_concept_id)]
  cm[, cause_level := as.integer(cause_level)]
  cm[, parent_concept_id := as.integer(parent_concept_id)]
  
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
  
  mort <- merge(
    mort,
    loc_master,
    by = "location_id",
    all.x = TRUE,
    sort = FALSE
  )
  mort[location_id == CFG$national_additive_id, location_name := "Nacional"]
  mort[is.na(location_name), location_name := paste0("loc_", location_id)]
  
  avp <- merge(
    avp,
    loc_master,
    by = "location_id",
    all.x = TRUE,
    sort = FALSE
  )
  avp[location_id == CFG$national_additive_id, location_name := "Nacional"]
  avp[is.na(location_name), location_name := paste0("loc_", location_id)]
  
  cause_name_lookup <- unique(cm[, .(
    cause_concept_id,
    cause_name_display = display_cause_name(cause_name)
  )])
  mort <- merge(
    mort,
    cause_name_lookup,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  avp <- merge(
    avp,
    cause_name_lookup,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  mort[!is.na(cause_name_display), cause_name := cause_name_display]
  avp[!is.na(cause_name_display), cause_name := cause_name_display]
  mort[, cause_name_display := NULL]
  avp[, cause_name_display := NULL]
  
  msg("Agregando por grupo etario reportable.")
  
  mort_rep <- mort[, .(
    population = sum(population, na.rm = TRUE),
    deaths_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE),
    location_name = unique(location_name)[1]
  ), by = .(
    year_id, location_id, location_scope, sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name
  )]
  mort_rep[, mortality_rate_smoothed_consistent := safe_rate(deaths_smoothed_consistent, population, CFG$rate_multiplier)]
  
  avp_rep <- avp[, .(
    population = sum(population, na.rm = TRUE),
    avp_abs = sum(avp_abs, na.rm = TRUE),
    location_name = unique(location_name)[1]
  ), by = .(
    year_id, location_id, location_scope, sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name
  )]
  avp_rep[, avp_rate := safe_rate(avp_abs, population, CFG$rate_multiplier)]
  
  msg("Construyendo tablas maestras largas.")
  
  mort_long <- mort_rep[, .(
    year_id, location_id, location_name, location_scope,
    sex_id, sex_label,
    age_group, cause_concept_id, cause_level, cause_name,
    population,
    metric_abs = deaths_smoothed_consistent,
    metric_rate = mortality_rate_smoothed_consistent
  )]
  mort_long[, metric_type := "mortality"]
  
  avp_long <- avp_rep[, .(
    year_id, location_id, location_name, location_scope,
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

  cause_catalog <- unique(cm[, .(
    cause_concept_id,
    cause_code = if ("cause_code" %in% names(cm)) as.character(cause_code) else NA_character_,
    cause_name,
    cause_level,
    parent_concept_id,
    level_1_name = if ("level_1_name" %in% names(cm)) as.character(level_1_name) else NA_character_,
    level_2_name = if ("level_2_name" %in% names(cm)) as.character(level_2_name) else NA_character_,
    level_3_name = if ("level_3_name" %in% names(cm)) as.character(level_3_name) else NA_character_,
    level_4_name = if ("level_4_name" %in% names(cm)) as.character(level_4_name) else NA_character_,
    cie10 = if ("cie10" %in% names(cm)) as.character(cie10) else NA_character_,
    icd10_regex = if ("icd10_regex" %in% names(cm)) as.character(icd10_regex) else NA_character_,
    yll_flag = if ("yll_flag" %in% names(cm)) as.integer(yll_flag) else NA_integer_,
    yld_flag = if ("yld_flag" %in% names(cm)) as.integer(yld_flag) else NA_integer_,
    is_oprm = if ("is_oprm" %in% names(cm)) as.logical(is_oprm) else FALSE,
    is_transversal_pandemic_adjustment = if ("is_transversal_pandemic_adjustment" %in% names(cm)) {
      as.logical(is_transversal_pandemic_adjustment)
    } else FALSE
  )])
  if ("is_terminal" %in% names(cm)) {
    cause_catalog <- merge(
      cause_catalog,
      unique(cm[, .(cause_concept_id, is_terminal = as.logical(is_terminal))]),
      by = "cause_concept_id",
      all.x = TRUE,
      sort = FALSE
    )
  }
  if ("is_covid_related" %in% names(cm)) {
    cause_catalog <- merge(
      cause_catalog,
      unique(cm[, .(cause_concept_id, is_covid_related = as.logical(is_covid_related))]),
      by = "cause_concept_id",
      all.x = TRUE,
      sort = FALSE
    )
  }
  cause_catalog[, path_key := make_path_key(level_1_name, level_2_name, level_3_name, level_4_name)]
  cause_catalog <- merge(
    cause_catalog,
    raw_cause_catalog,
    by = "path_key",
    all.x = TRUE,
    sort = FALSE
  )
  cause_catalog[is.na(source_in_raw_catalog), source_in_raw_catalog := FALSE]
  for (nm in c("cause_name", "level_1_name", "level_2_name", "level_3_name", "level_4_name")) {
    cause_catalog[, (nm) := display_cause_name(get(nm))]
  }
  cause_catalog[, parent_name := as.character(cause_catalog[match(parent_concept_id, cause_concept_id), cause_name])]
  cause_catalog[, display_name := make_display_name(cause_level, cause_name)]
  cause_catalog[, hierarchy_path := apply(.SD, 1, function(v) {
    vals <- v[!is.na(v) & nzchar(v)]
    paste(vals, collapse = " > ")
  }), .SDcols = c("level_1_name", "level_2_name", "level_3_name", "level_4_name")]
  cause_catalog[, cie10_cobertura_utilizada := fifelse(
    !is.na(cie10) & nzchar(trimws(cie10)),
    cie10,
    NA_character_
  )]
  cause_catalog[, catalog_origin := fifelse(
    source_in_raw_catalog,
    "raw_catalog",
    fifelse(
      cause_level == 0L,
      "pipeline_total",
      fifelse(
        cause_concept_id %in% c(9001786L, 9001787L, 9001788L, 9001789L, 9001790L),
        "pipeline_pandemic_construct",
        "patch_or_pipeline_construct"
      )
    )
  )]
  cause_catalog[, is_considered_cause_category := source_in_raw_catalog & cause_level > 0L]
  mortality_used_ids <- unique(mort[cause_level > 0L, cause_concept_id])
  avp_used_ids <- unique(avp[cause_level > 0L, cause_concept_id])
  cause_catalog[, is_mortality_cause_category := cause_level > 0L & cause_concept_id %in% mortality_used_ids]
  cause_catalog[, is_avp_eligible_cause := cause_level > 0L & cause_concept_id %in% avp_used_ids]
  cause_catalog[cause_concept_id == 9001790L,
                nota_especial := "Otras muertes relacionadas con la pandemia (OPRM): categoria sintetica residual de modelamiento pandemico, activa solo para 2020-2022; no recibe mapeo directo CIE-10 y se estima mediante reasignacion contable del exceso pandemico."]
  cause_catalog[cause_concept_id %in% c(9001786L, 9001787L, 9001788L, 9001789L),
                nota_especial := "Rama sintetica del pipeline para separar componentes pandemicos sin doble conteo con causas respiratorias o COVID-19."]
  cause_catalog[, nota_especial := fifelse(
    cause_concept_id == 9001790L,
    "Otras muertes relacionadas con la pandemia (OPRM): categoría sintética residual de modelamiento pandémico, activa solo para 2020-2022; no recibe mapeo directo CIE-10.",
    fifelse(
      cause_concept_id %in% c(9001786L, 9001787L, 9001788L, 9001789L),
      "Rama sintética del pipeline para separar componentes pandémicos sin doble conteo con causas respiratorias o COVID-19.",
      NA_character_
    )
  )]
  
  cause_catalog[cause_concept_id == 9001790L,
                nota_especial := "Otras muertes relacionadas con la pandemia (OPRM): categoria sintetica residual de modelamiento pandemico, activa solo para 2020-2022; no recibe mapeo directo CIE-10 y se estima mediante reasignacion contable del exceso pandemico."]
  cause_catalog[cause_concept_id %in% c(9001786L, 9001787L, 9001788L, 9001789L),
                nota_especial := "Rama sintetica del pipeline para separar componentes pandemicos sin doble conteo con causas respiratorias o COVID-19."]

  if (!is.null(observed_icd_codes)) {
    regex_lookup <- cause_catalog[
      !is.na(icd10_regex) & nzchar(trimws(icd10_regex)),
      .(cause_concept_id, icd10_regex)
    ]
    
    observed_lookup <- regex_lookup[, {
      hits <- tryCatch(
        observed_icd_codes[grepl(icd10_regex[1], observed_icd_codes, perl = TRUE)],
        error = function(e) character()
      )
      .(
        n_icd10_observados = length(hits),
        icd10_observados_en_datos = collapse_observed_icd_codes(hits),
        icd10_observados_stem_ranges = collapse_icd10_to_stem_ranges(hits),
        icd10_hits_list = list(unique(sort(hits)))
      )
    }, by = .(cause_concept_id)]
    
    cause_catalog <- merge(
      cause_catalog,
      observed_lookup,
      by = "cause_concept_id",
      all.x = TRUE,
      sort = FALSE
    )

    child_edges <- unique(cause_catalog[
      !is.na(parent_concept_id) & cause_concept_id != parent_concept_id,
      .(parent_concept_id, child_concept_id = cause_concept_id)
    ])
    child_map <- split(child_edges$child_concept_id, child_edges$parent_concept_id)
    hit_map <- observed_lookup[, .(cause_concept_id, icd10_hits_list)]
    hit_map <- setNames(hit_map$icd10_hits_list, as.character(hit_map$cause_concept_id))
    memo <- new.env(parent = emptyenv())
    collect_descendant_hits <- function(id) {
      key <- as.character(id)
      if (exists(key, envir = memo, inherits = FALSE)) return(get(key, envir = memo, inherits = FALSE))
      own <- hit_map[[key]]
      if (is.null(own)) own <- character()
      kids <- child_map[[key]]
      out <- own
      if (!is.null(kids) && length(kids) > 0L) {
        for (kid in kids) out <- c(out, collect_descendant_hits(kid))
      }
      out <- unique(sort(out))
      assign(key, out, envir = memo)
      out
    }
    hier_lookup <- cause_catalog[, {
      hh <- collect_descendant_hits(cause_concept_id[1])
      .(
        n_icd10_hierarquicos = length(hh),
        icd10_hierarquicos_en_datos = collapse_observed_icd_codes(hh),
        icd10_hierarquicos_stem_ranges = collapse_icd10_to_stem_ranges(hh),
        icd10_hierarquicos_list = list(hh)
      )
    }, by = .(cause_concept_id)]
    cause_catalog <- merge(
      cause_catalog,
      hier_lookup,
      by = "cause_concept_id",
      all.x = TRUE,
      sort = FALSE
    )

    qc_cie10_hierarchy_compare <- cause_catalog[, {
      direct <- unique(sort(unlist(icd10_hits_list)))
      hier <- unique(sort(unlist(icd10_hierarquicos_list)))
      child_union <- if (!is.null(child_map[[as.character(cause_concept_id[1])]])) hier else direct
      .(
        cause_name = cause_name[1],
        cause_level = cause_level[1],
        n_direct = length(direct),
        n_hier = length(hier),
        n_missing_direct_vs_hier = length(setdiff(hier, direct)),
        n_extra_direct_vs_hier = length(setdiff(direct, hier)),
        direct_stem_ranges = collapse_icd10_to_stem_ranges(direct),
        hier_stem_ranges = collapse_icd10_to_stem_ranges(hier)
      )
    }, by = .(cause_concept_id)]
    fwrite(qc_cie10_hierarchy_compare, file.path(CFG$qc_dir, "qc_cie10_hierarchy_compare.csv"))

    rr_candidates <- c(
      here("data", "final", "redistribution_rules", "redistribution_rules.parquet"),
      here("data", "final", "redistribution_rules", "redistribution_rules.csv")
    )
    rr_path <- first_existing(rr_candidates)
    if (!is.na(rr_path)) {
      rr <- as.data.table(read_auto(rr_path))
      rr_regex <- unique(rr[!is.na(regex_r) & nzchar(trimws(regex_r)), as.character(regex_r)])
      rr_hits <- unique(unlist(lapply(rr_regex, function(rx) {
        tryCatch(observed_icd_codes[grepl(rx, observed_icd_codes, perl = TRUE)], error = function(e) character())
      })))
      rr_stems <- unique(sub("^([A-Z][0-9]{2}).*$", "\\1", gsub("\\.", "", rr_hits)))
      rr_stems <- rr_stems[grepl("^[A-Z][0-9]{2}$", rr_stems)]

      low_risk_parent_gap_ids <- c(9000380L, 9000600L, 9000940L)
      bridge_sensitive_ids <- c(9000540L, 9000820L, 9001260L)
      exact_bridge_codes <- c("D649", "G312", "G721", "E102", "E112", "E122", "E132", "E142")
      exact_specific_exception_codes <- c("Q860", "B178")

      qc_cie10_hierarchy_compare_adjusted <- qc_cie10_hierarchy_compare[, {
        miss <- expand_icd10_stem_ranges(hier_stem_ranges[1])
        dirc <- expand_icd10_stem_ranges(direct_stem_ranges[1])
        miss <- setdiff(miss, dirc)
        miss_rr <- intersect(miss, rr_stems)
        miss_unexp <- setdiff(miss, rr_stems)
        .(
          cause_name = cause_name[1],
          cause_level = cause_level[1],
          n_missing_direct_vs_hier = length(miss),
          n_missing_explicados_por_redistribucion = length(miss_rr),
          missing_explicados_por_redistribucion = if (length(miss_rr) > 0L) paste(sort(miss_rr), collapse = ", ") else NA_character_,
          n_missing_no_explicados = length(miss_unexp),
          missing_no_explicados = if (length(miss_unexp) > 0L) paste(sort(miss_unexp), collapse = ", ") else NA_character_
        )
      }, by = .(cause_concept_id)]
      fwrite(qc_cie10_hierarchy_compare_adjusted, file.path(CFG$qc_dir, "qc_cie10_hierarchy_compare_adjusted.csv"))

      qc_cie10_hierarchy_compare_classified <- cause_catalog[, {
        direct <- unique(sort(unlist(icd10_hits_list)))
        hier <- unique(sort(unlist(icd10_hierarquicos_list)))
        miss_exact <- setdiff(hier, direct)
        miss_exact_rr <- intersect(miss_exact, rr_hits)
        miss_exact_unexp <- setdiff(miss_exact, rr_hits)
        miss_stems_unexp <- unique(sub("^([A-Z][0-9]{2}).*$", "\\1", miss_exact_unexp))
        miss_stems_unexp <- miss_stems_unexp[grepl("^[A-Z][0-9]{2}$", miss_stems_unexp)]
        miss_bridge_exact <- intersect(miss_exact_unexp, exact_bridge_codes)
        mismatch_class <- fifelse(
          cause_concept_id[1] == 9001785L,
          "root_total_excluded",
          fifelse(
            length(miss_exact) == 0L,
            "no_gap",
            fifelse(
            length(miss_exact_unexp) == 0L & length(miss_exact) > 0L,
            "explained_by_redistribution",
            fifelse(
              cause_concept_id[1] %in% low_risk_parent_gap_ids,
              "possible_parent_regex_gap_low_risk",
              fifelse(
                cause_concept_id[1] %in% bridge_sensitive_ids | length(miss_bridge_exact) > 0L,
                "bridge_code_or_specific_exception_review",
                fifelse(
                  length(intersect(miss_exact_unexp, exact_specific_exception_codes)) > 0L,
                  "specific_hierarchy_exception_review",
                  fifelse(length(miss_exact_unexp) > 0L, "unclassified_review", "no_gap")
                )
              )
            )
            )
          )
        )
        impact_risk <- fifelse(
          mismatch_class == "root_total_excluded",
          "none_qc_only",
          fifelse(
            mismatch_class == "explained_by_redistribution",
            "low_documentation_only",
            fifelse(
              mismatch_class == "possible_parent_regex_gap_low_risk",
              "low_parent_regex_alignment",
              fifelse(
                mismatch_class == "bridge_code_or_specific_exception_review",
                "medium_semantic_review",
                fifelse(
                  mismatch_class == "specific_hierarchy_exception_review",
                  "medium_semantic_review",
                  fifelse(mismatch_class == "unclassified_review", "medium_manual_review", "none")
                )
              )
            )
          )
        )
        recommended_action <- fifelse(
          mismatch_class == "root_total_excluded",
          "Excluir Total del QC estricto; no corresponde interpretarlo como nodo regex analitico.",
          fifelse(
            mismatch_class == "explained_by_redistribution",
            "No tocar el cause_master; documentar que el faltante se explica por codigos fuente sujetos a redistribucion.",
            fifelse(
              mismatch_class == "possible_parent_regex_gap_low_risk",
              "Evaluar parche del regex del padre solo si no altera leaves ni genera multiple hits problematicos.",
              fifelse(
                mismatch_class == "bridge_code_or_specific_exception_review",
                "Mantener sin parchear por ahora; revisar semantica exacta del codigo puente antes de tocar el padre.",
                fifelse(
                  mismatch_class == "specific_hierarchy_exception_review",
                  "Mantener sin parchear por ahora; revisar la excepcion semantica especifica del arbol antes de tocar padre o hijo.",
                  fifelse(
                    mismatch_class == "unclassified_review",
                    "Revisar manualmente antes de cualquier cambio en el cause_master.",
                    "Sin accion."
                  )
                )
              )
            )
          )
        )
        .(
          cause_name = cause_name[1],
          cause_level = cause_level[1],
          n_missing_exact = length(miss_exact),
          n_missing_exact_explicados_por_redistribucion = length(miss_exact_rr),
          missing_exact_explicados_por_redistribucion = if (length(miss_exact_rr) > 0L) paste(sort(miss_exact_rr), collapse = ", ") else NA_character_,
          n_missing_exact_no_explicados = length(miss_exact_unexp),
          missing_exact_no_explicados = if (length(miss_exact_unexp) > 0L) paste(sort(miss_exact_unexp), collapse = ", ") else NA_character_,
          missing_stems_no_explicados = if (length(miss_stems_unexp) > 0L) paste(sort(miss_stems_unexp), collapse = ", ") else NA_character_,
          mismatch_class = mismatch_class,
          impact_risk = impact_risk,
          recommended_action = recommended_action
        )
      }, by = .(cause_concept_id)]
      fwrite(qc_cie10_hierarchy_compare_classified, file.path(CFG$qc_dir, "qc_cie10_hierarchy_compare_classified.csv"))
    }

    qc_cie10_stem_roundtrip <- cause_catalog[, {
      hier <- unique(sort(unlist(icd10_hierarquicos_list)))
      stems_in <- unique(sub("^([A-Z][0-9]{2}).*$", "\\1", hier))
      stems_in <- stems_in[grepl("^[A-Z][0-9]{2}$", stems_in)]
      txt <- collapse_icd10_to_stem_ranges(hier)
      stems_out <- expand_icd10_stem_ranges(txt)
      .(
        cause_name = cause_name[1],
        cause_level = cause_level[1],
        n_stems_in = length(unique(stems_in)),
        n_stems_out = length(unique(stems_out)),
        stem_roundtrip_ok = setequal(unique(stems_in), unique(stems_out)),
        stem_ranges = txt
      )
    }, by = .(cause_concept_id)]
    fwrite(qc_cie10_stem_roundtrip, file.path(CFG$qc_dir, "qc_cie10_stem_roundtrip.csv"))

    cause_catalog[
      (is.na(cie10_cobertura_utilizada) | !nzchar(trimws(cie10_cobertura_utilizada))) &
        !is.na(icd10_hierarquicos_en_datos) & nzchar(trimws(icd10_hierarquicos_en_datos)),
      cie10_cobertura_utilizada := icd10_hierarquicos_en_datos
    ]
    cause_catalog[
      !is.na(icd10_hierarquicos_stem_ranges) & nzchar(trimws(icd10_hierarquicos_stem_ranges)),
      cie10_cobertura_resumida := icd10_hierarquicos_stem_ranges
    ]
  } else {
    cause_catalog[, `:=`(
      n_icd10_observados = NA_integer_,
      icd10_observados_en_datos = NA_character_,
      icd10_observados_stem_ranges = NA_character_,
      n_icd10_hierarquicos = NA_integer_,
      icd10_hierarquicos_en_datos = NA_character_,
      icd10_hierarquicos_stem_ranges = NA_character_,
      cie10_cobertura_resumida = NA_character_
    )]
  }
  drop_cols <- intersect(c("icd10_hits_list", "icd10_hierarquicos_list"), names(cause_catalog))
  if (length(drop_cols) > 0L) cause_catalog[, (drop_cols) := NULL]
  setorder(
    cause_catalog,
    level_1_name, level_2_name, level_3_name, level_4_name,
    cause_level, cause_concept_id
  )
  cause_catalog[, catalog_order := .I]
  cause_catalog[, run_id := run_id]
  cause_catalog[, source_in_raw_catalog := as.logical(source_in_raw_catalog)]
  cause_catalog[, is_considered_cause_category := as.logical(is_considered_cause_category)]
  cause_catalog[, path_key := NULL]
  
  # ----------------------------------------------------------
  # NACIONAL TOTAL (sin sexo ni edad)
  # ----------------------------------------------------------
  cause_all_levels <- unique(cause_catalog[cause_level %in% CFG$report_cause_levels, .(
    cause_concept_id,
    cause_level,
    cause_name,
    catalog_order
  )])
  
  tbl_nat_year_total_mort <- build_complete_cause_table(
    mortality_report_long[cause_level %in% CFG$report_cause_levels],
    copy(cause_all_levels),
    national = TRUE
  )
  
  tbl_nat_year_total_avp <- build_complete_cause_table(
    avp_report_long[cause_level %in% CFG$report_cause_levels],
    copy(cause_all_levels),
    national = TRUE
  )
  
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
  tbl_reg_year_total_mort <- build_complete_cause_table(
    mortality_report_long[cause_level %in% CFG$report_cause_levels],
    copy(cause_all_levels),
    national = FALSE
  )
  
  tbl_reg_year_total_avp <- build_complete_cause_table(
    avp_report_long[cause_level %in% CFG$report_cause_levels],
    copy(cause_all_levels),
    national = FALSE
  )
  
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
  # TOTALES ALL-CAUSE EXPLICITOS
  # ----------------------------------------------------------
  tbl_nat_year_grand_total_mort <- mortality_report_long[
    location_id == CFG$national_additive_id &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level == 0L
  ]
  
  tbl_nat_year_grand_total_avp <- avp_report_long[
    location_id == CFG$national_additive_id &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level == 0L
  ]
  
  tbl_reg_year_grand_total_mort <- mortality_report_long[
    location_scope == CFG$regional_scope &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level == 0L
  ]
  
  tbl_reg_year_grand_total_avp <- avp_report_long[
    location_scope == CFG$regional_scope &
      sex_label == CFG$both_sex_label &
      as.character(age_group) == CFG$all_age_label &
      cause_level == 0L
  ]

  cause_l2 <- unique(cause_catalog[cause_level == 2L, .(
    cause_concept_id,
    cause_level,
    cause_name,
    catalog_order
  )])
  
  tbl_nat_year_level2_mort <- build_complete_cause_table(
    mortality_report_long,
    copy(cause_l2),
    national = TRUE
  )
  
  tbl_nat_year_level2_avp <- build_complete_cause_table(
    avp_report_long,
    copy(cause_l2),
    national = TRUE
  )
  
  tbl_reg_year_level2_mort <- build_complete_cause_table(
    mortality_report_long,
    copy(cause_l2),
    national = FALSE
  )
  
  tbl_reg_year_level2_avp <- build_complete_cause_table(
    avp_report_long,
    copy(cause_l2),
    national = FALSE
  )
  
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
    tbl_cause_hierarchy_catalog = cause_catalog,
    tbl_nat_year_grand_total_mort = tbl_nat_year_grand_total_mort,
    tbl_nat_year_grand_total_avp = tbl_nat_year_grand_total_avp,
    tbl_reg_year_grand_total_mort = tbl_reg_year_grand_total_mort,
    tbl_reg_year_grand_total_avp = tbl_reg_year_grand_total_avp,
    tbl_nat_year_level2_mort = tbl_nat_year_level2_mort,
    tbl_nat_year_level2_avp = tbl_nat_year_level2_avp,
    tbl_reg_year_level2_mort = tbl_reg_year_level2_mort,
    tbl_reg_year_level2_avp = tbl_reg_year_level2_avp,
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
  qc_cie10_hierarchy_path <- file.path(CFG$qc_dir, "qc_cie10_hierarchy_compare.csv")
  qc_cie10_hierarchy_adj_path <- file.path(CFG$qc_dir, "qc_cie10_hierarchy_compare_adjusted.csv")
  qc_cie10_stem_roundtrip_path <- file.path(CFG$qc_dir, "qc_cie10_stem_roundtrip.csv")
  
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
    qc_cause_add_path,
    qc_cie10_hierarchy_path,
    qc_cie10_hierarchy_adj_path,
    qc_cie10_stem_roundtrip_path
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
