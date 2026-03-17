#!/usr/bin/env Rscript

# ============================================================
# 04_ingest_sinadef_raw.R
# ------------------------------------------------------------
# Construye death_record_raw desde archivos crudos SINADEF
# (csv / txt / xlsx), sin normalización OMOP final.
#
# Estrategia:
#   - eliminar SOLO filas completamente vacías
#   - NO eliminar duplicados exactos por contenido
#   - generar <- jerárquico de duplicación:
#       1) duplicados por source_record_id_raw
#       2) conflictos dentro de source_record_id_raw
#       3) candidatos por firma fuerte
#       4) distribución de multiplicidades por firma fuerte
#
# Salidas:
#   data/final/death_record_raw/death_record_raw.csv
#   data/final/death_record_raw/death_record_raw.parquet
#   data/final/death_record_raw/death_record_raw_dictionary_ext.csv
#
# QC:
#   data/derived/qc/04_ingest_sinadef_raw/
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(openxlsx)
  library(janitor)
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
source(here("R", "death_utils.R"))

# ----------------------------
# Config
# ----------------------------
CFG <- list(
  version = "v1.1.0",
  dataset_id = "death_record_sinadef_raw",
  table_name = "death_record_raw",
  
  input_dir = here("data", "raw", "sinadef"),
  spec_path = here("config", "spec_death_record_raw.yml"),
  
  out_dir = here("data", "final", "death_record_raw"),
  qc_dir  = here("data", "derived", "qc", "04_ingest_sinadef_raw"),
  out_stem = "death_record_raw",
  
  file_pattern = "\\.(csv|txt|xlsx)$",
  years_allowed = 2018:2024,
  
  na_tokens = c("", "NA", "N/A", "NULL", "null", "#¡NULO!", "#NULO!", "NULO"),
  warn_blank_row_pct = 0.01,
  max_qc_examples_per_file = 1000L,
  
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
  y[y %in% CFG$na_tokens] <- NA_character_
  y
}

clean_names_lower <- function(x) {
  janitor::make_clean_names(x)
}

extract_year_from_filename <- function(x) {
  y <- stringi::stri_extract_first_regex(basename(x), "(19|20)\\d{2}")
  suppressWarnings(as.integer(y))
}

first_present <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

make_file_prefix <- function(file_name) {
  stem <- tools::file_path_sans_ext(basename(file_name))
  stem <- gsub("[^A-Za-z0-9]+", "_", stem)
  toupper(stem)
}

make_synthetic_death_id <- function(year_id, row_id, file_name) {
  prefix <- paste0("SINADEF_", make_file_prefix(file_name))
  build_death_id(year_id = year_id, row_id = row_id, prefix = prefix)
}

normalize_blank_to_na_dt <- function(dt) {
  chr_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (j in chr_cols) {
    set(dt, i = NULL, j = j, value = clean_chr(dt[[j]]))
  }
  invisible(dt)
}

read_header_delim <- function(path) {
  hdr <- fread(path, nrows = 0, showProgress = FALSE, encoding = "UTF-8")
  data.table(
    original_name = names(hdr),
    clean_name = clean_names_lower(names(hdr))
  )
}

read_selected_delim <- function(path, select_names_original) {
  if (length(select_names_original) == 0L) {
    stop("No se detectó ninguna columna útil en: ", basename(path))
  }
  
  dt <- fread(
    path,
    select = select_names_original,
    showProgress = TRUE,
    encoding = "UTF-8",
    na.strings = CFG$na_tokens
  )
  setnames(dt, clean_names_lower(names(dt)))
  as.data.table(dt)
}

detect_best_sheet <- function(path, synonyms_flat) {
  sheets <- openxlsx::getSheetNames(path)
  if (length(sheets) == 1L) return(sheets[1])
  
  scores <- sapply(sheets, function(sh) {
    hdr <- tryCatch(
      openxlsx::read.xlsx(path, sheet = sh, rows = 1, colNames = TRUE),
      error = function(e) NULL
    )
    if (is.null(hdr)) return(-Inf)
    nms <- clean_names_lower(names(hdr))
    sum(nms %in% synonyms_flat)
  })
  
  sheets[which.max(scores)]
}

read_excel_all <- function(path, sheet) {
  dt <- openxlsx::read.xlsx(
    xlsxFile = path,
    sheet = sheet,
    detectDates = FALSE,
    skipEmptyRows = FALSE,
    skipEmptyCols = FALSE
  )
  setDT(dt)
  setnames(dt, clean_names_lower(names(dt)))
  dt
}

get_or_na <- function(dt, nm, n) {
  if (is.na(nm) || !nm %in% names(dt)) {
    rep(NA_character_, n)
  } else {
    clean_chr(dt[[nm]])
  }
}

safe_pct <- function(num, den) {
  if (is.na(den) || den <= 0) return(0)
  round(100 * num / den, 4)
}

# ----------------------------
# Sinónimos canónicos
# ----------------------------
name_synonyms <- list(
  source_record_id_raw = c("id", "death_id", "id_def", "id_registro", "idregistro", "id_sinadef"),
  year_id = c("anofall", "anoepi", "anio_fall", "year_id"),
  date_of_death = c("fec_fall", "fecha_fall", "fecha_def", "fec_def", "death_date"),
  sex_source_value = c("sexo", "sex", "sexo_def"),
  age_value_raw = c("edad_a", "age_years", "edad", "age"),
  age_unit_raw = c("edad_unit", "edad_u", "age_unit"),
  ubigeo_residence_raw = c("res_hab", "ubigeo", "ubigeo_res", "ubigeo_residencia", "residencia"),
  ubigeo_occurrence_raw = c("ubi_fall", "ubigeodef", "ubigeo_fall", "ubigeo_lugar_fall", "ubigeo_ocurrencia"),
  icd10_ucod_raw = c("causa_b", "cod_f", "cod_f_ok", "ucod", "icd10_ucod_raw")
)

all_synonyms_flat <- unique(unlist(name_synonyms, use.names = FALSE))

# ----------------------------
# Leer spec
# ----------------------------
spec <- read_spec(CFG$spec_path)

# ----------------------------
# Descubrir archivos
# ----------------------------
ensure_project_dirs()
ensure_catalog_files()

files <- list.files(
  path = CFG$input_dir,
  pattern = CFG$file_pattern,
  full.names = TRUE,
  ignore.case = TRUE
)

if (length(files) == 0L) {
  stop("No se encontraron archivos SINADEF en: ", CFG$input_dir)
}

file_years <- extract_year_from_filename(files)
keep_year <- !is.na(file_years) & file_years %in% CFG$years_allowed
files <- files[keep_year]
file_years <- file_years[keep_year]

if (length(files) == 0L) {
  stop("No se encontraron archivos con años válidos ", paste(CFG$years_allowed, collapse = ", "))
}

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

# ----------------------------
# Contenedores
# ----------------------------
out_list <- vector("list", length(files))
qc_file_list <- vector("list", length(files))
qc_map_list <- vector("list", length(files))
qc_missing_profile_list <- vector("list", length(files))
qc_examples_list <- vector("list", length(files))
qc_dup_source_id_list <- vector("list", length(files))
qc_dup_source_id_conflict_list <- vector("list", length(files))
qc_dup_signature_list <- vector("list", length(files))
qc_dup_signature_mult_list <- vector("list", length(files))

tryCatch({
  
  for (i in seq_along(files)) {
    f <- files[i]
    f_year <- file_years[i]
    f_name <- basename(f)
    ext <- tolower(tools::file_ext(f))
    
    msg("--------------------------------------------------")
    msg("Procesando:", f_name)
    msg("Año detectado:", f_year)
    
    # --------------------------------
    # Leer archivo
    # --------------------------------
    if (ext %in% c("csv", "txt")) {
      hdr_map <- read_header_delim(f)
      
      detected_clean <- vapply(names(name_synonyms), function(v) {
        first_present(hdr_map$clean_name, name_synonyms[[v]])
      }, character(1))
      
      detected_original <- vapply(detected_clean, function(nm_clean) {
        if (is.na(nm_clean)) return(NA_character_)
        hit <- hdr_map[clean_name == nm_clean, original_name]
        if (length(hit) == 0L) NA_character_ else hit[1]
      }, character(1))
      
      names(detected_clean) <- names(name_synonyms)
      names(detected_original) <- names(name_synonyms)
      
      need_read <- unique(na.omit(unname(detected_original)))
      dt_raw <- read_selected_delim(f, need_read)
      
    } else if (ext == "xlsx") {
      sh <- detect_best_sheet(f, all_synonyms_flat)
      msg("Hoja seleccionada:", sh)
      
      dt_raw <- read_excel_all(f, sh)
      hdr_clean <- names(dt_raw)
      
      detected_clean <- vapply(names(name_synonyms), function(v) {
        first_present(hdr_clean, name_synonyms[[v]])
      }, character(1))
      
      names(detected_clean) <- names(name_synonyms)
      detected_original <- detected_clean
      
    } else {
      stop("Extensión no soportada: ", ext)
    }
    
    normalize_blank_to_na_dt(dt_raw)
    n_rows_raw <- nrow(dt_raw)
    
    # --------------------------------
    # Mapa de columnas detectadas
    # --------------------------------
    qc_map <- data.table(
      file_name = f_name,
      variable_target = names(name_synonyms),
      column_detected = unname(detected_clean)
    )
    qc_map_list[[i]] <- qc_map
    
    # --------------------------------
    # Construcción canónica raw
    # --------------------------------
    n <- nrow(dt_raw)
    
    year_vec <- rep(f_year, n)
    if (all(is.na(year_vec))) {
      year_col <- detected_clean["year_id"]
      year_vec <- suppressWarnings(as.integer(get_or_na(dt_raw, year_col, n)))
    }
    
    age_col <- detected_clean["age_value_raw"]
    age_unit_col <- detected_clean["age_unit_raw"]
    
    age_raw_chr <- get_or_na(dt_raw, age_col, n)
    age_raw_num <- suppressWarnings(as.numeric(age_raw_chr))
    age_unit_raw <- get_or_na(dt_raw, age_unit_col, n)
    
    if (all(is.na(age_unit_raw))) {
      if (!is.na(age_col) && age_col == "edad_a") {
        age_unit_raw <- rep("YEARS", n)
      } else {
        age_unit_raw <- rep(NA_character_, n)
      }
    }
    
    source_record_id_col <- detected_clean["source_record_id_raw"]
    source_record_id_raw <- get_or_na(dt_raw, source_record_id_col, n)
    
    dt <- data.table(
      death_id = NA_character_,
      source_record_id_raw = source_record_id_raw,
      source_file = rep(f_name, n),
      year_id = as.integer(year_vec),
      date_of_death = get_or_na(dt_raw, detected_clean["date_of_death"], n),
      sex_source_value = get_or_na(dt_raw, detected_clean["sex_source_value"], n),
      age_value_raw = as.numeric(age_raw_num),
      age_unit_raw = age_unit_raw,
      ubigeo_residence_raw = get_or_na(dt_raw, detected_clean["ubigeo_residence_raw"], n),
      ubigeo_occurrence_raw = get_or_na(dt_raw, detected_clean["ubigeo_occurrence_raw"], n),
      icd10_ucod_raw = get_or_na(dt_raw, detected_clean["icd10_ucod_raw"], n)
    )
    
    # --------------------------------
    # 1) Eliminar filas completamente vacías
    # --------------------------------
    canonical_cols_for_blank <- c(
      "source_record_id_raw", "year_id", "date_of_death", "sex_source_value",
      "age_value_raw", "age_unit_raw", "ubigeo_residence_raw",
      "ubigeo_occurrence_raw", "icd10_ucod_raw"
    )
    
    blank_mat <- as.data.table(lapply(
      dt[, canonical_cols_for_blank, with = FALSE],
      function(x) {
        if (is.character(x)) is.na(x) | trimws(x) == "" else is.na(x)
      }
    ))
    
    is_all_blank <- Reduce(`&`, blank_mat)
    
    n_rows_removed_all_na <- sum(is_all_blank)
    if (n_rows_removed_all_na > 0) {
      dt <- dt[!is_all_blank]
    }
    
    pct_blank <- if (n_rows_raw > 0) n_rows_removed_all_na / n_rows_raw else 0
    warning_flag_blank_gt_1pct <- pct_blank > CFG$warn_blank_row_pct
    
    if (warning_flag_blank_gt_1pct) {
      warning(sprintf(
        "[%s] Se eliminaron %s filas completamente vacías (%.3f%%).",
        f_name, n_rows_removed_all_na, 100 * pct_blank
      ))
    }
    
    # --------------------------------
    # 2) Flags de información mínima faltante
    # --------------------------------
    dt[, flag_missing_icd10_ucod_raw := is.na(icd10_ucod_raw)]
    dt[, flag_missing_age_value_raw := is.na(age_value_raw)]
    dt[, flag_missing_sex_source_value := is.na(sex_source_value)]
    dt[, flag_missing_minimal_info := (
      flag_missing_icd10_ucod_raw |
        flag_missing_age_value_raw |
        flag_missing_sex_source_value
    )]
    
    n_rows_missing_icd10_ucod_raw <- dt[, sum(flag_missing_icd10_ucod_raw)]
    n_rows_missing_age_value_raw <- dt[, sum(flag_missing_age_value_raw)]
    n_rows_missing_sex_source_value <- dt[, sum(flag_missing_sex_source_value)]
    n_rows_missing_minimal_info <- dt[, sum(flag_missing_minimal_info)]
    
    qc_missing_profile <- dt[
      ,
      .N,
      by = .(
        file_name = source_file,
        flag_missing_icd10_ucod_raw,
        flag_missing_age_value_raw,
        flag_missing_sex_source_value,
        flag_missing_minimal_info
      )
    ][order(-N)]
    qc_missing_profile_list[[i]] <- qc_missing_profile
    
    qc_examples <- dt[
      flag_missing_minimal_info == TRUE,
      .(
        file_name = source_file,
        source_record_id_raw,
        year_id,
        date_of_death,
        sex_source_value,
        age_value_raw,
        age_unit_raw,
        ubigeo_residence_raw,
        ubigeo_occurrence_raw,
        icd10_ucod_raw
      )
    ][1:min(.N, CFG$max_qc_examples_per_file)]
    qc_examples_list[[i]] <- qc_examples
    
    # --------------------------------
    # 3) <- jerárquico de duplicación
    # --------------------------------
    
    # 3A. Duplicados por source_record_id_raw
    dt_has_id <- dt[!is.na(source_record_id_raw) & trimws(source_record_id_raw) != ""]
    
    n_rows_with_source_record_id <- nrow(dt_has_id)
    n_unique_source_record_id <- uniqueN(dt_has_id$source_record_id_raw)
    
    qc_dup_source_id <- dt_has_id[
      ,
      .N,
      by = .(file_name = source_file, source_record_id_raw)
    ][N > 1][order(-N, source_record_id_raw)]
    
    if (nrow(qc_dup_source_id) > 0) {
      qc_dup_source_id <- merge(
        qc_dup_source_id,
        dt_has_id[
          ,
          .(
            n_distinct_payload = uniqueN(
              paste(
                year_id,
                date_of_death,
                sex_source_value,
                age_value_raw,
                age_unit_raw,
                ubigeo_residence_raw,
                ubigeo_occurrence_raw,
                icd10_ucod_raw,
                sep = "||"
              )
            )
          ),
          by = .(file_name = source_file, source_record_id_raw)
        ],
        by = c("file_name", "source_record_id_raw"),
        all.x = TRUE
      )
    } else {
      qc_dup_source_id <- data.table(
        file_name = character(),
        source_record_id_raw = character(),
        N = integer(),
        n_distinct_payload = integer()
      )
    }
    
    qc_dup_source_id_list[[i]] <- qc_dup_source_id
    
    # 3B. Conflictos dentro de source_record_id_raw
    qc_dup_source_id_conflict <- qc_dup_source_id[
      n_distinct_payload > 1,
      .(file_name, source_record_id_raw, N, n_distinct_payload)
    ]
    
    if (nrow(qc_dup_source_id_conflict) > 0) {
      qc_dup_source_id_conflict <- merge(
        qc_dup_source_id_conflict,
        dt_has_id[
          ,
          .(
            year_id,
            date_of_death,
            sex_source_value,
            age_value_raw,
            age_unit_raw,
            ubigeo_residence_raw,
            ubigeo_occurrence_raw,
            icd10_ucod_raw
          ),
          by = .(file_name = source_file, source_record_id_raw)
        ],
        by = c("file_name", "source_record_id_raw"),
        all.x = TRUE
      )
    }
    
    qc_dup_source_id_conflict_list[[i]] <- qc_dup_source_id_conflict
    
    # 3C. Candidatos por firma fuerte
    strong_sig_cols <- c(
      "year_id",
      "date_of_death",
      "sex_source_value",
      "age_value_raw",
      "ubigeo_residence_raw",
      "ubigeo_occurrence_raw",
      "icd10_ucod_raw"
    )
    
    dt_strong_sig <- dt[
      complete.cases(dt[, ..strong_sig_cols])
    ]
    
    if (nrow(dt_strong_sig) > 0) {
      qc_dup_signature <- dt_strong_sig[
        ,
        .(
          N = .N,
          n_distinct_source_record_id = uniqueN(source_record_id_raw, na.rm = TRUE),
          any_missing_source_record_id = any(is.na(source_record_id_raw) | trimws(source_record_id_raw) == "")
        ),
        by = c("source_file", strong_sig_cols)
      ][N > 1][order(-N)]
    } else {
      qc_dup_signature <- data.table(
        source_file = character(),
        year_id = integer(),
        date_of_death = character(),
        sex_source_value = character(),
        age_value_raw = numeric(),
        ubigeo_residence_raw = character(),
        ubigeo_occurrence_raw = character(),
        icd10_ucod_raw = character(),
        N = integer(),
        n_distinct_source_record_id = integer(),
        any_missing_source_record_id = logical()
      )
    }
    
    qc_dup_signature_list[[i]] <- qc_dup_signature
    
    # 3D. Distribución de multiplicidades por firma fuerte
    if (nrow(qc_dup_signature) > 0) {
      qc_dup_signature_mult <- qc_dup_signature[
        ,
        .N,
        by = .(file_name = source_file, multiplicity = N)
      ][order(file_name, multiplicity)]
    } else {
      qc_dup_signature_mult <- data.table(
        file_name = character(),
        multiplicity = integer(),
        N = integer()
      )
    }
    
    qc_dup_signature_mult_list[[i]] <- qc_dup_signature_mult
    
    # --------------------------------
    # 4) Generar death_id final
    # --------------------------------
    dt[, row_in_file := .I]
    
    dt[, death_id := make_synthetic_death_id(
      year_id = year_id,
      row_id = row_in_file,
      file_name = source_file
    )]
    
    dt[, row_in_file := NULL]
    
    # --------------------------------
    # 5) Cast final
    # --------------------------------
    dt[, `:=`(
      death_id = as.character(death_id),
      source_record_id_raw = as.character(source_record_id_raw),
      source_file = as.character(source_file),
      year_id = as.integer(year_id),
      date_of_death = as.character(date_of_death),
      sex_source_value = as.character(sex_source_value),
      age_value_raw = as.numeric(age_value_raw),
      age_unit_raw = as.character(age_unit_raw),
      ubigeo_residence_raw = as.character(ubigeo_residence_raw),
      ubigeo_occurrence_raw = as.character(ubigeo_occurrence_raw),
      icd10_ucod_raw = as.character(icd10_ucod_raw)
    )]
    
    # --------------------------------
    # 6) Resumen QC por archivo
    # --------------------------------
    n_duplicate_source_record_id_groups <- nrow(qc_dup_source_id)
    n_duplicate_source_record_id_rows <- if (nrow(qc_dup_source_id) > 0) sum(qc_dup_source_id$N) else 0L
    n_conflict_source_record_id_groups <- if (nrow(qc_dup_source_id_conflict) > 0) {
      uniqueN(paste(qc_dup_source_id_conflict$file_name, qc_dup_source_id_conflict$source_record_id_raw))
    } else {
      0L
    }
    
    n_duplicate_strong_signature_groups <- nrow(qc_dup_signature)
    n_duplicate_strong_signature_rows <- if (nrow(qc_dup_signature) > 0) sum(qc_dup_signature$N) else 0L
    
    qc_file <- data.table(
      file_name = f_name,
      n_rows_raw = as.integer(n_rows_raw),
      n_rows_removed_all_na = as.integer(n_rows_removed_all_na),
      n_rows_final = as.integer(nrow(dt)),
      n_rows_missing_icd10_ucod_raw = as.integer(n_rows_missing_icd10_ucod_raw),
      n_rows_missing_age_value_raw = as.integer(n_rows_missing_age_value_raw),
      n_rows_missing_sex_source_value = as.integer(n_rows_missing_sex_source_value),
      n_rows_missing_minimal_info = as.integer(n_rows_missing_minimal_info),
      n_rows_with_source_record_id = as.integer(n_rows_with_source_record_id),
      n_unique_source_record_id = as.integer(n_unique_source_record_id),
      n_duplicate_source_record_id_groups = as.integer(n_duplicate_source_record_id_groups),
      n_duplicate_source_record_id_rows = as.integer(n_duplicate_source_record_id_rows),
      n_conflict_source_record_id_groups = as.integer(n_conflict_source_record_id_groups),
      n_duplicate_strong_signature_groups = as.integer(n_duplicate_strong_signature_groups),
      n_duplicate_strong_signature_rows = as.integer(n_duplicate_strong_signature_rows),
      pct_rows_removed_all_na = safe_pct(n_rows_removed_all_na, n_rows_raw),
      pct_rows_missing_minimal_info = safe_pct(n_rows_missing_minimal_info, nrow(dt)),
      pct_rows_with_source_record_id = safe_pct(n_rows_with_source_record_id, nrow(dt)),
      pct_rows_in_duplicate_source_record_id = safe_pct(n_duplicate_source_record_id_rows, nrow(dt)),
      pct_rows_in_duplicate_strong_signature = safe_pct(n_duplicate_strong_signature_rows, nrow(dt)),
      warning_blank_rows_gt_1pct = warning_flag_blank_gt_1pct
    )
    
    # Remover flags temporales antes de export
    dt[, c(
      "flag_missing_icd10_ucod_raw",
      "flag_missing_age_value_raw",
      "flag_missing_sex_source_value",
      "flag_missing_minimal_info"
    ) := NULL]
    
    qc_file_list[[i]] <- qc_file
    out_list[[i]] <- dt
    
    rm(
      dt_raw, dt, qc_file, qc_map, qc_missing_profile, qc_examples,
      qc_dup_source_id, qc_dup_source_id_conflict,
      qc_dup_signature, qc_dup_signature_mult
    )
    gc(verbose = FALSE)
  }
  
  # ----------------------------
  # Bind final
  # ----------------------------
  final_dt <- rbindlist(out_list, use.names = TRUE, fill = TRUE)
  qc_file_dt <- rbindlist(qc_file_list, use.names = TRUE, fill = TRUE)
  qc_map_dt  <- rbindlist(qc_map_list, use.names = TRUE, fill = TRUE)
  qc_missing_profile_dt <- rbindlist(qc_missing_profile_list, use.names = TRUE, fill = TRUE)
  qc_examples_dt <- rbindlist(qc_examples_list, use.names = TRUE, fill = TRUE)
  qc_dup_source_id_dt <- rbindlist(qc_dup_source_id_list, use.names = TRUE, fill = TRUE)
  qc_dup_source_id_conflict_dt <- rbindlist(qc_dup_source_id_conflict_list, use.names = TRUE, fill = TRUE)
  qc_dup_signature_dt <- rbindlist(qc_dup_signature_list, use.names = TRUE, fill = TRUE)
  qc_dup_signature_mult_dt <- rbindlist(qc_dup_signature_mult_list, use.names = TRUE, fill = TRUE)
  
  # ----------------------------
  # Blindaje final de death_id
  # ----------------------------
  final_dt[, row_global := .I]
  
  dup_global_id <- duplicated(final_dt$death_id) | duplicated(final_dt$death_id, fromLast = TRUE)
  
  if (any(dup_global_id)) {
    final_dt[dup_global_id, death_id := make_synthetic_death_id(
      year_id = year_id,
      row_id = row_global,
      file_name = source_file
    )]
  }
  
  final_dt[, row_global := NULL]
  
  # ----------------------------
  # Validaciones globales contra spec
  # ----------------------------
  spec_cols <- names(spec$required_columns)
  
  if (is.null(spec_cols) || length(spec_cols) == 0L) {
    stop("El spec no contiene required_columns válidas en: ", CFG$spec_path)
  }
  
  missing_spec_cols_in_final <- setdiff(spec_cols, names(final_dt))
  if (length(missing_spec_cols_in_final) > 0L) {
    stop(
      "Faltan columnas en final_dt requeridas por el spec: ",
      paste(missing_spec_cols_in_final, collapse = ", ")
    )
  }
  
  final_dt_for_spec <- copy(final_dt)
  
  extra_cols_not_in_spec <- setdiff(names(final_dt_for_spec), spec_cols)
  if (length(extra_cols_not_in_spec) > 0L) {
    msg(
      "Columnas extra no declaradas en spec que se excluirán solo para validación: ",
      paste(extra_cols_not_in_spec, collapse = ", ")
    )
    final_dt_for_spec <- final_dt_for_spec[, ..spec_cols]
  }
  
  msg("Dimensión final_dt: ", nrow(final_dt), " x ", ncol(final_dt))
  msg("Dimensión final_dt_for_spec: ", nrow(final_dt_for_spec), " x ", ncol(final_dt_for_spec))
  msg("Columnas requeridas por spec: ", paste(spec_cols, collapse = ", "))
  msg("Columnas en final_dt_for_spec: ", paste(names(final_dt_for_spec), collapse = ", "))
  
  validate_by_spec(final_dt_for_spec, spec)
  
  qc_pk <- qc_pk_duplicates(final_dt_for_spec, spec$primary_key)
  qc_missing <- qc_missing_required(final_dt_for_spec, names(spec$required_columns))
  
  qc_summary <- data.table(
    metric = c(
      "n_files",
      "n_rows_final",
      "n_unique_death_id",
      "n_missing_date_of_death",
      "n_missing_ubigeo_residence_raw",
      "n_missing_ubigeo_occurrence_raw",
      "n_missing_age_unit_raw",
      "n_missing_icd10_ucod_raw",
      "n_missing_age_value_raw",
      "n_missing_sex_source_value",
      "n_missing_minimal_info",
      "n_rows_with_source_record_id",
      "n_duplicate_source_record_id_groups",
      "n_conflict_source_record_id_groups",
      "n_duplicate_strong_signature_groups"
    ),
    value = c(
      length(files),
      nrow(final_dt),
      uniqueN(final_dt$death_id),
      sum(is.na(final_dt$date_of_death)),
      sum(is.na(final_dt$ubigeo_residence_raw)),
      sum(is.na(final_dt$ubigeo_occurrence_raw)),
      sum(is.na(final_dt$age_unit_raw)),
      sum(is.na(final_dt$icd10_ucod_raw)),
      sum(is.na(final_dt$age_value_raw)),
      sum(is.na(final_dt$sex_source_value)),
      sum(is.na(final_dt$icd10_ucod_raw) | is.na(final_dt$age_value_raw) | is.na(final_dt$sex_source_value)),
      sum(!is.na(final_dt$source_record_id_raw) & trimws(final_dt$source_record_id_raw) != ""),
      if (nrow(qc_dup_source_id_dt) > 0) uniqueN(qc_dup_source_id_dt[, .(file_name, source_record_id_raw)]) else 0L,
      if (nrow(qc_dup_source_id_conflict_dt) > 0) uniqueN(qc_dup_source_id_conflict_dt[, .(file_name, source_record_id_raw)]) else 0L,
      nrow(qc_dup_signature_dt)
    )
  )
  
  # ----------------------------
  # Dictionary ext
  # ----------------------------
  dict_dt <- dict_from_spec(
    spec = spec,
    dataset_version = CFG$version,
    run_id = run_id,
    config_dir = here("config")
  )
  
  dict_ext_dt <- enrich_dict_with_stats(dict_dt, final_dt_for_spec)
  
  # ----------------------------
  # Export principal
  # ----------------------------
  out_csv     <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".parquet"))
  out_dict    <- file.path(CFG$out_dir, paste0(CFG$out_stem, "_dictionary_ext.csv"))
  
  write_csv_parquet(final_dt, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(dict_ext_dt, out_dict)
  
  # ----------------------------
  # Export QC
  # ----------------------------
  qc_file_path                   <- file.path(CFG$qc_dir, "qc_ingest_file_summary.csv")
  qc_map_path                    <- file.path(CFG$qc_dir, "qc_column_mapping_by_file.csv")
  qc_missing_path                <- file.path(CFG$qc_dir, "qc_missing_required.csv")
  qc_pk_path                     <- file.path(CFG$qc_dir, "qc_pk_duplicates.csv")
  qc_summary_path                <- file.path(CFG$qc_dir, "qc_summary.csv")
  qc_missing_profile_path        <- file.path(CFG$qc_dir, "qc_missing_minimal_info_patterns.csv")
  qc_examples_path               <- file.path(CFG$qc_dir, "qc_missing_minimal_info_examples.csv")
  qc_dup_source_id_path          <- file.path(CFG$qc_dir, "qc_duplicates_by_source_record_id.csv")
  qc_dup_source_id_conflict_path <- file.path(CFG$qc_dir, "qc_source_record_id_conflicts.csv")
  qc_dup_signature_path          <- file.path(CFG$qc_dir, "qc_duplicate_candidates_strong_signature.csv")
  qc_dup_signature_mult_path     <- file.path(CFG$qc_dir, "qc_duplicate_signature_multiplicity.csv")
  
  fwrite(qc_file_dt, qc_file_path)
  fwrite(qc_map_dt, qc_map_path)
  fwrite(qc_missing, qc_missing_path)
  fwrite(qc_pk, qc_pk_path)
  fwrite(qc_summary, qc_summary_path)
  fwrite(qc_missing_profile_dt, qc_missing_profile_path)
  fwrite(qc_examples_dt, qc_examples_path)
  fwrite(qc_dup_source_id_dt, qc_dup_source_id_path)
  fwrite(qc_dup_source_id_conflict_dt, qc_dup_source_id_conflict_path)
  fwrite(qc_dup_signature_dt, qc_dup_signature_path)
  fwrite(qc_dup_signature_mult_dt, qc_dup_signature_mult_path)
  
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
    notes = "CSV final raw ingest SINADEF (incluye source_record_id_raw si existe)"
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
    notes = "Parquet final raw ingest SINADEF (incluye source_record_id_raw si existe)"
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
    notes = "Diccionario extendido desde spec + stats observados"
  )
  
  for (p in c(
    qc_file_path,
    qc_map_path,
    qc_missing_path,
    qc_pk_path,
    qc_summary_path,
    qc_missing_profile_path,
    qc_examples_path,
    qc_dup_source_id_path,
    qc_dup_source_id_conflict_path,
    qc_dup_signature_path,
    qc_dup_signature_mult_path
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
      notes = "QC ingest SINADEF raw"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "04_ingest_sinadef_raw completado")
  
  msg("OK -> dataset: ", out_csv)
  msg("OK -> parquet: ", out_parquet)
  msg("OK -> dict: ", out_dict)
  msg("OK -> qc dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})