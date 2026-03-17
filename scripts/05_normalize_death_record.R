#!/usr/bin/env Rscript

# ============================================================
# 05_normalize_death_record.R
# ------------------------------------------------------------
# Toma death_record_raw y construye death_record_normalized
# sin deduplicar registros.
#
# Objetivos:
#   - preservar 1 fila por death_id
#   - normalizar fecha, sexo, edad, ubigeos e ICD-10
#   - generar location_id provisional desde ubigeo limpio
#   - producir dataset normalizado + dictionary_ext + QC
#
# Reglas principales:
#   - sex_id: 8507 (male), 8532 (female), 0 (unknown/unmapped)
#   - age: edad simple en años cumplidos, truncada a [0,110]
#   - location_id: ubigeo residencia si válido; si no, ocurrencia; si no, 0
#   - icd10_ucod: versión limpia con punto
#
# Nota:
#   Si el spec YAML actual solo permite sex_id en {8507,8532},
#   este script relaja localmente la validación para permitir 0
#   si aparecen sexos desconocidos, sin modificar tu YAML.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(stringi)
  library(arrow)
})

# ----------------------------
# Cargar utils del proyecto
# ----------------------------
source(here("R", "io_utils.R"))
source(here("R", "spec_utils.R"))
source(here("R", "dictionary_utils.R"))
source(here("R", "qc_utils.R"))
source(here("R", "catalog_utils.R"))

# ----------------------------
# Config
# ----------------------------
CFG <- list(
  version = "v1.1.0",
  dataset_id = "death_record_sinadef_normalized",
  table_name = "death_record_normalized",
  
  input_path = here("data", "final", "death_record_raw", "death_record_raw.parquet"),
  spec_path  = here("config", "spec_death_record_normalized.yml"),
  
  out_dir = here("data", "final", "death_record_normalized"),
  qc_dir  = here("data", "derived", "qc", "05_normalize_death_record"),
  out_stem = "death_record_normalized",
  
  source_dataset = "SINADEF",
  years_allowed = 2018:2024,
  age_min = 0L,
  age_max = 110L,
  
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

# ----------------------------
# Helpers
# ----------------------------
clean_chr <- function(x) {
  y <- as.character(x)
  y <- stringi::stri_trim_both(y)
  y[y %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  y
}

normalize_sex_source_value <- function(x) {
  z <- toupper(stringi::stri_trans_general(clean_chr(x), "Latin-ASCII"))
  z <- gsub("[^A-Z0-9]", "", z)
  
  out <- fifelse(
    z %chin% c("1", "M", "MASCULINO", "HOMBRE", "VARON", "MALE"),
    "M",
    fifelse(
      z %chin% c("2", "F", "FEMENINO", "MUJER", "FEMALE"),
      "F",
      NA_character_
    )
  )
  out
}

derive_sex_id <- function(sex_std) {
  fifelse(
    sex_std == "M", 8507L,
    fifelse(sex_std == "F", 8532L, 0L)
  )
}

parse_date_multi <- function(x) {
  n <- length(x)
  out <- as.IDate(rep(NA_character_, n))
  
  if (inherits(x, "IDate")) return(as.IDate(x))
  if (inherits(x, "Date"))  return(as.IDate(x))
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.IDate(as.Date(x)))
  }
  
  x_chr <- clean_chr(x)
  
  # 1) serial Excel
  x_num <- suppressWarnings(as.numeric(x_chr))
  is_excel_serial <- !is.na(x_num) & x_num > 20000 & x_num < 80000
  if (any(is_excel_serial)) {
    out[is_excel_serial] <- as.IDate(as.Date(x_num[is_excel_serial], origin = "1899-12-30"))
  }
  
  # 2) normalización básica
  x_clean <- x_chr
  x_clean <- gsub("T", " ", x_clean, fixed = TRUE)
  x_clean <- gsub("\\s+", " ", x_clean)
  x_clean <- trimws(x_clean)
  x_clean <- gsub("\\s?(AM|PM|am|pm)$", "", x_clean)
  x_clean <- gsub("(\\d{2}:\\d{2}:\\d{2})\\.[0-9]+", "\\1", x_clean)
  
  # 3) meses abreviados en español
  x_clean2 <- x_clean
  x_clean2 <- gsub("Ene", "Jan", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Feb", "Feb", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Mar", "Mar", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Abr", "Apr", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("May", "May", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Jun", "Jun", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Jul", "Jul", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Ago", "Aug", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Sep", "Sep", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Oct", "Oct", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Nov", "Nov", x_clean2, ignore.case = TRUE)
  x_clean2 <- gsub("Dic", "Dec", x_clean2, ignore.case = TRUE)
  
  fmts <- c(
    "%Y-%m-%d",
    "%d/%m/%Y",
    "%d-%m-%Y",
    "%Y/%m/%d",
    "%d.%m.%Y",
    "%Y%m%d",
    "%d%m%Y",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%d-%m-%Y %H:%M:%S",
    "%d-%m-%Y %H:%M",
    "%m/%d/%Y",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%Y/%m/%d %H:%M:%S",
    "%Y/%m/%d %H:%M",
    "%d-%b-%y",
    "%d-%b-%Y",
    "%d-%b-%y %H:%M:%S",
    "%d-%b-%Y %H:%M:%S"
  )
  
  for (fmt in fmts) {
    idx <- is.na(out) & !is.na(x_clean2)
    if (!any(idx)) next
    
    parsed <- as.POSIXct(x_clean2[idx], format = fmt, tz = "UTC")
    ok <- !is.na(parsed)
    
    if (any(ok)) {
      out_idx <- which(idx)[ok]
      out[out_idx] <- as.IDate(as.Date(parsed[ok]))
    }
  }
  
  out
}

normalize_age_years <- function(x, age_min = 0L, age_max = 110L) {
  y <- suppressWarnings(as.numeric(x))
  age_missing_raw <- is.na(y)
  age_negative_raw <- !is.na(y) & y < 0
  age_too_high_raw <- !is.na(y) & y > 130
  
  age <- floor(y)
  age <- pmax(age_min, pmin(age_max, age))
  age[is.na(y)] <- NA_real_
  
  list(
    age = as.integer(age),
    flag_age_missing_raw = age_missing_raw,
    flag_age_negative_raw = age_negative_raw,
    flag_age_too_high_raw = age_too_high_raw
  )
}

normalize_ubigeo <- function(x) {
  z <- clean_chr(x)
  z <- gsub("[^0-9]", "", z)
  z[nchar(z) == 0] <- NA_character_
  
  idx_pad <- !is.na(z) & nchar(z) < 6
  z[idx_pad] <- sprintf("%06s", z[idx_pad])
  z <- gsub(" ", "0", z, fixed = TRUE)
  
  valid <- !is.na(z) & grepl("^[0-9]{6}$", z)
  z[!valid] <- NA_character_
  
  list(
    ubigeo = z,
    valid = valid
  )
}

normalize_icd10_nodot <- function(x) {
  z <- clean_chr(x)
  z <- toupper(z)
  z <- gsub("[[:space:]]+", "", z)
  z <- gsub("\\.", "", z)
  z <- gsub("[^A-Z0-9]", "", z)
  z[nchar(z) == 0] <- NA_character_
  z
}

add_icd10_dot <- function(x) {
  z <- clean_chr(x)
  z <- toupper(z)
  z[nchar(z) == 0] <- NA_character_
  
  has_dot <- !is.na(z) & grepl("\\.", z)
  idx <- !is.na(z) & !has_dot & nchar(z) > 3
  z[idx] <- paste0(substr(z[idx], 1, 3), ".", substr(z[idx], 4, nchar(z[idx])))
  z
}

flag_icd10_pattern <- function(nodot) {
  !is.na(nodot) & grepl("^[A-Z][0-9][0-9A-Z][0-9A-Z]{0,4}$", nodot)
}

safe_pct <- function(num, den) {
  if (is.na(den) || den <= 0) return(0)
  round(100 * num / den, 4)
}

# ----------------------------
# Cargar spec
# ----------------------------
spec <- read_spec(CFG$spec_path)

# ----------------------------
# Descubrir insumo
# ----------------------------
ensure_project_dirs()
ensure_catalog_files()

if (!file.exists(CFG$input_path)) {
  stop("No existe input_path: ", CFG$input_path)
}

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

tryCatch({
  
  # ----------------------------
  # Load raw
  # ----------------------------
  msg("Leyendo death_record_raw desde: ", CFG$input_path)
  dt_raw <- as.data.table(arrow::read_parquet(CFG$input_path))
  
  required_raw <- c(
    "death_id", "source_record_id_raw", "source_file", "year_id", "date_of_death",
    "sex_source_value", "age_value_raw", "age_unit_raw",
    "ubigeo_residence_raw", "ubigeo_occurrence_raw", "icd10_ucod_raw"
  )
  miss_raw <- setdiff(required_raw, names(dt_raw))
  if (length(miss_raw) > 0L) {
    stop("Faltan columnas esperadas en death_record_raw: ", paste(miss_raw, collapse = ", "))
  }
  
  setDT(dt_raw)
  
  # ----------------------------
  # Normalización
  # ----------------------------
  msg("Normalizando variables...")
  
  dt <- copy(dt_raw)
  
  # 1) year_id
  dt[, year_id := as.integer(year_id)]
  
  # 2) fechas
  dt[, death_date := parse_date_multi(date_of_death)]
  dt[, flag_invalid_death_date := !is.na(date_of_death) & is.na(death_date)]
  dt[, death_year_from_date := as.integer(format(death_date, "%Y"))]
  dt[, death_month := as.integer(format(death_date, "%m"))]
  dt[, death_day := as.integer(format(death_date, "%d"))]
  dt[, flag_year_date_mismatch := !is.na(death_date) & !is.na(year_id) & death_year_from_date != year_id]
  
  # QC adicional de patrones crudos de fecha
  qc_date_raw_patterns <- dt[
    ,
    .N,
    by = .(
      source_file,
      date_raw_class = vapply(date_of_death, function(v) class(v)[1], character(1)),
      sample_value = substr(as.character(date_of_death), 1, 30)
    )
  ][order(source_file, -N)]
  
  qc_date_raw_patterns <- qc_date_raw_patterns[, head(.SD, 30), by = source_file]
  
  # 3) sexo
  dt[, sex_source_value_std := normalize_sex_source_value(sex_source_value)]
  dt[, sex_id := derive_sex_id(sex_source_value_std)]
  dt[, flag_unknown_sex := sex_id == 0L]
  
  # 4) edad
  age_res <- normalize_age_years(dt$age_value_raw, age_min = CFG$age_min, age_max = CFG$age_max)
  dt[, age := age_res$age]
  dt[, flag_age_missing_raw := age_res$flag_age_missing_raw]
  dt[, flag_age_negative_raw := age_res$flag_age_negative_raw]
  dt[, flag_age_too_high_raw := age_res$flag_age_too_high_raw]
  dt[, age_value := as.numeric(age_value_raw)]
  dt[, age_unit_std := "year"]
  
  # 5) ubigeos
  ub_res <- normalize_ubigeo(dt$ubigeo_residence_raw)
  ub_occ <- normalize_ubigeo(dt$ubigeo_occurrence_raw)
  
  dt[, ubigeo_residence := ub_res$ubigeo]
  dt[, ubigeo_occurrence := ub_occ$ubigeo]
  dt[, flag_invalid_ubigeo_residence := !is.na(ubigeo_residence_raw) & is.na(ubigeo_residence)]
  dt[, flag_invalid_ubigeo_occurrence := !is.na(ubigeo_occurrence_raw) & is.na(ubigeo_occurrence)]
  
  dt[, location_source := fifelse(
    !is.na(ubigeo_residence), "residence",
    fifelse(!is.na(ubigeo_occurrence), "occurrence", "unknown")
  )]
  
  dt[, location_id := fifelse(
    !is.na(ubigeo_residence), as.integer(ubigeo_residence),
    fifelse(!is.na(ubigeo_occurrence), as.integer(ubigeo_occurrence), 0L)
  )]
  
  dt[, flag_location_id_zero := location_id == 0L]
  
  # 6) ICD-10
  dt[, icd10_ucod_nodot := normalize_icd10_nodot(icd10_ucod_raw)]
  dt[, icd10_ucod := add_icd10_dot(icd10_ucod_nodot)]
  dt[, flag_missing_icd10 := is.na(icd10_ucod_nodot)]
  dt[, flag_invalid_icd10_pattern := !flag_missing_icd10 & !flag_icd10_pattern(icd10_ucod_nodot)]
  
  # 7) dataset/run
  dt[, source_dataset := CFG$source_dataset]
  dt[, run_id := run_id]
  
  # ----------------------------
  # Dataset final normalizado
  # ----------------------------
  final_cols <- c(
    # requeridas por spec
    "death_id", "year_id", "sex_id", "age", "location_id", "icd10_ucod", "source_dataset", "run_id",
    # auxiliares útiles
    "source_record_id_raw", "source_file",
    "death_date", "death_month", "death_day", "death_year_from_date",
    "date_of_death",
    "sex_source_value", "sex_source_value_std",
    "age_value_raw", "age_value", "age_unit_raw", "age_unit_std",
    "ubigeo_residence_raw", "ubigeo_occurrence_raw",
    "ubigeo_residence", "ubigeo_occurrence",
    "location_source",
    "icd10_ucod_raw", "icd10_ucod_nodot",
    # flags QC
    "flag_invalid_death_date", "flag_year_date_mismatch",
    "flag_unknown_sex",
    "flag_age_missing_raw", "flag_age_negative_raw", "flag_age_too_high_raw",
    "flag_invalid_ubigeo_residence", "flag_invalid_ubigeo_occurrence", "flag_location_id_zero",
    "flag_missing_icd10", "flag_invalid_icd10_pattern"
  )
  
  final_dt <- dt[, ..final_cols]
  
  # ----------------------------
  # QC
  # ----------------------------
  msg("Construyendo QC...")
  
  qc_summary <- data.table(
    metric = c(
      "n_rows",
      "n_unique_death_id",
      "n_invalid_death_date",
      "n_year_date_mismatch",
      "n_unknown_sex",
      "n_age_missing_raw",
      "n_age_negative_raw",
      "n_age_too_high_raw",
      "n_invalid_ubigeo_residence",
      "n_invalid_ubigeo_occurrence",
      "n_location_id_zero",
      "n_missing_icd10",
      "n_invalid_icd10_pattern"
    ),
    value = c(
      nrow(final_dt),
      uniqueN(final_dt$death_id),
      sum(final_dt$flag_invalid_death_date, na.rm = TRUE),
      sum(final_dt$flag_year_date_mismatch, na.rm = TRUE),
      sum(final_dt$flag_unknown_sex, na.rm = TRUE),
      sum(final_dt$flag_age_missing_raw, na.rm = TRUE),
      sum(final_dt$flag_age_negative_raw, na.rm = TRUE),
      sum(final_dt$flag_age_too_high_raw, na.rm = TRUE),
      sum(final_dt$flag_invalid_ubigeo_residence, na.rm = TRUE),
      sum(final_dt$flag_invalid_ubigeo_occurrence, na.rm = TRUE),
      sum(final_dt$flag_location_id_zero, na.rm = TRUE),
      sum(final_dt$flag_missing_icd10, na.rm = TRUE),
      sum(final_dt$flag_invalid_icd10_pattern, na.rm = TRUE)
    )
  )
  
  qc_by_file <- final_dt[, .(
    n_rows = .N,
    n_invalid_death_date = sum(flag_invalid_death_date, na.rm = TRUE),
    n_year_date_mismatch = sum(flag_year_date_mismatch, na.rm = TRUE),
    n_unknown_sex = sum(flag_unknown_sex, na.rm = TRUE),
    n_age_missing_raw = sum(flag_age_missing_raw, na.rm = TRUE),
    n_invalid_ubigeo_residence = sum(flag_invalid_ubigeo_residence, na.rm = TRUE),
    n_invalid_ubigeo_occurrence = sum(flag_invalid_ubigeo_occurrence, na.rm = TRUE),
    n_location_id_zero = sum(flag_location_id_zero, na.rm = TRUE),
    n_missing_icd10 = sum(flag_missing_icd10, na.rm = TRUE),
    n_invalid_icd10_pattern = sum(flag_invalid_icd10_pattern, na.rm = TRUE)
  ), by = .(source_file, year_id)]
  
  qc_sex_map <- final_dt[, .N, by = .(sex_source_value, sex_source_value_std, sex_id)][order(-N)]
  qc_location_source <- final_dt[, .N, by = .(location_source)][order(-N)]
  
  qc_icd10_examples <- final_dt[
    flag_missing_icd10 == TRUE | flag_invalid_icd10_pattern == TRUE,
    .(
      death_id, source_file, year_id,
      icd10_ucod_raw, icd10_ucod_nodot, icd10_ucod,
      flag_missing_icd10, flag_invalid_icd10_pattern
    )
  ][1:min(.N, 2000)]
  
  qc_date_examples <- final_dt[
    flag_invalid_death_date == TRUE | flag_year_date_mismatch == TRUE,
    .(
      death_id, source_file, year_id,
      date_of_death, death_date, death_year_from_date,
      flag_invalid_death_date, flag_year_date_mismatch
    )
  ][1:min(.N, 5000)]
  
  qc_unknown_sex_examples <- final_dt[
    flag_unknown_sex == TRUE,
    .(death_id, source_file, year_id, sex_source_value, sex_source_value_std, sex_id)
  ][1:min(.N, 2000)]
  
  # ----------------------------
  # Validación contra spec
  # ----------------------------
  spec_cols <- names(spec$required_columns)
  
  if (is.null(spec_cols) || length(spec_cols) == 0L) {
    stop("El spec no contiene required_columns válidas en: ", CFG$spec_path)
  }
  
  final_dt_for_spec <- copy(final_dt[, ..spec_cols])
  
  # Relajación local de spec si hay sex_id = 0
  spec_local <- spec
  if ("sex_id" %in% names(spec_local$constraints)) {
    allowed_sex <- spec_local$constraints$sex_id$allowed_values
    if (!is.null(allowed_sex) && any(final_dt_for_spec$sex_id == 0L, na.rm = TRUE)) {
      spec_local$constraints$sex_id$allowed_values <- sort(unique(c(as.integer(allowed_sex), 0L)))
      msg("Se relajó localmente spec_local$constraints$sex_id$allowed_values para permitir 0.")
    }
  }
  
  validate_by_spec(final_dt_for_spec, spec_local)
  
  qc_pk <- qc_pk_duplicates(final_dt_for_spec, spec_local$primary_key)
  qc_missing <- qc_missing_required(final_dt_for_spec, names(spec_local$required_columns))
  
  # ----------------------------
  # Dictionary ext
  # ----------------------------
  dict_dt <- dict_from_spec(
    spec = spec_local,
    dataset_version = CFG$version,
    run_id = run_id,
    config_dir = here("config")
  )
  dict_ext_dt <- enrich_dict_with_stats(dict_dt, final_dt_for_spec)
  
  # ----------------------------
  # Export
  # ----------------------------
  out_csv     <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".parquet"))
  out_dict    <- file.path(CFG$out_dir, paste0(CFG$out_stem, "_dictionary_ext.csv"))
  
  write_csv_parquet(final_dt, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(dict_ext_dt, out_dict)
  
  qc_summary_path              <- file.path(CFG$qc_dir, "qc_summary.csv")
  qc_by_file_path              <- file.path(CFG$qc_dir, "qc_by_file_year.csv")
  qc_sex_map_path              <- file.path(CFG$qc_dir, "qc_sex_mapping.csv")
  qc_location_source_path      <- file.path(CFG$qc_dir, "qc_location_source.csv")
  qc_icd10_examples_path       <- file.path(CFG$qc_dir, "qc_icd10_examples.csv")
  qc_date_examples_path        <- file.path(CFG$qc_dir, "qc_date_examples.csv")
  qc_date_raw_patterns_path    <- file.path(CFG$qc_dir, "qc_date_raw_patterns.csv")
  qc_unknown_sex_examples_path <- file.path(CFG$qc_dir, "qc_unknown_sex_examples.csv")
  qc_missing_path              <- file.path(CFG$qc_dir, "qc_missing_required.csv")
  qc_pk_path                   <- file.path(CFG$qc_dir, "qc_pk_duplicates.csv")
  
  fwrite(qc_summary, qc_summary_path)
  fwrite(qc_by_file, qc_by_file_path)
  fwrite(qc_sex_map, qc_sex_map_path)
  fwrite(qc_location_source, qc_location_source_path)
  fwrite(qc_icd10_examples, qc_icd10_examples_path)
  fwrite(qc_date_examples, qc_date_examples_path)
  fwrite(qc_date_raw_patterns, qc_date_raw_patterns_path)
  fwrite(qc_unknown_sex_examples, qc_unknown_sex_examples_path)
  fwrite(qc_missing, qc_missing_path)
  fwrite(qc_pk, qc_pk_path)
  
  # ----------------------------
  # Catalog / provenance
  # ----------------------------
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(final_dt),
    n_cols = ncol(final_dt),
    notes = "CSV final death_record_normalized"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_parquet,
    n_rows = nrow(final_dt),
    n_cols = ncol(final_dt),
    notes = "Parquet final death_record_normalized"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "dictionary_ext",
    artifact_path = out_dict,
    n_rows = nrow(dict_ext_dt),
    n_cols = ncol(dict_ext_dt),
    notes = "Diccionario extendido del dataset normalizado"
  )
  
  for (p in c(
    qc_summary_path,
    qc_by_file_path,
    qc_sex_map_path,
    qc_location_source_path,
    qc_icd10_examples_path,
    qc_date_examples_path,
    qc_date_raw_patterns_path,
    qc_unknown_sex_examples_path,
    qc_missing_path,
    qc_pk_path
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
      notes = "QC death_record_normalized"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "05_normalize_death_record completado")
  
  msg("OK -> dataset: ", out_csv)
  msg("OK -> parquet: ", out_parquet)
  msg("OK -> dict: ", out_dict)
  msg("OK -> qc dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})