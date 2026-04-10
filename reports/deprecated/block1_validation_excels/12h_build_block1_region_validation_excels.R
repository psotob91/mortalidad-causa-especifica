#!/usr/bin/env Rscript

# ============================================================
# 12h_build_block1_region_validation_excels.R
# ------------------------------------------------------------
# Genera anexos Excel regionales para validación interna
# del bloque 1:
#   - un Excel por región
#   - hoja Mortalidad
#   - hoja AVP
#
# Estructura:
#   Causa | 2018 Total Mujer Varon | 2019 ...
#
# Reglas:
#   - Mortalidad: usar metric_rate (tasa por 100 000)
#   - AVP: usar metric_abs
#   - Si no hay valor: 0
#   - Filtrar solo bloque 1
#   - Mantener orden jerárquico 2 -> 3 -> 4
#   - Sexo OMOP-like:
#       8507 = Varón
#       8532 = Mujer
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(openxlsx)
  library(here)
})

cat("\nConstruyendo anexos Excel regionales bloque 1...\n")

# ============================================================
# Helpers
# ============================================================

read_auto <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!file.exists(path)) stop("No existe: ", path)
  if (ext == "parquet") return(as.data.table(arrow::read_parquet(path)))
  if (ext == "csv") return(data.table::fread(path))
  stop("Extensión no soportada: ", path)
}

find_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

clean_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

slug_text <- function(x) {
  x <- clean_text(x)
  gsub("\\s+", "_", x)
}

fmt_num <- function(x, digits = 1) {
  out <- formatC(
    x,
    format = "f",
    digits = digits,
    big.mark = " ",
    decimal.mark = "."
  )
  out[is.na(x)] <- "0"
  out
}

# ============================================================
# Resolver cause_master
# ============================================================

read_cause_master <- function() {
  candidates <- c(
    here("data", "final", "cause_master", "cause_master.csv"),
    here("data", "final", "cause_master", "cause_master.parquet")
  )
  
  hit <- find_first_existing(candidates)
  
  if (is.na(hit)) {
    stop(
      paste0(
        "No encontré cause_master en:\n- ",
        paste(candidates, collapse = "\n- ")
      )
    )
  }
  
  cat("Leyendo cause_master desde:\n", hit, "\n")
  cm <- read_auto(hit)
  
  req <- c(
    "cause_concept_id",
    "cause_name",
    "cause_level",
    "parent_concept_id",
    "is_terminal"
  )
  miss <- setdiff(req, names(cm))
  if (length(miss) > 0) {
    stop("Faltan columnas en cause_master: ", paste(miss, collapse = ", "))
  }
  
  cm <- unique(cm[, ..req])
  cm[, cause_concept_id := as.integer(cause_concept_id)]
  cm[, parent_concept_id := as.integer(parent_concept_id)]
  cm[, cause_level := as.integer(cause_level)]
  cm
}

# ============================================================
# Resolver tablas
# ============================================================

read_table_candidate <- function(filename_stub) {
  candidates <- c(
    here("data", "derived", "tables", paste0(filename_stub, ".parquet")),
    here("data", "derived", "tables", paste0(filename_stub, ".csv")),
    here("data", "final", "tables", paste0(filename_stub, ".parquet")),
    here("data", "final", "tables", paste0(filename_stub, ".csv"))
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) stop("No encontré tabla: ", filename_stub)
  cat("Leyendo:", hit, "\n")
  read_auto(hit)
}

# ============================================================
# Inputs
# ============================================================

dir_output <- here("reports", "block1_validation_excels_regions")
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

tbl_reg_total_mort <- read_table_candidate("tbl_reg_year_total_mort")
tbl_reg_sex_mort   <- read_table_candidate("tbl_reg_year_sex_mort")
tbl_reg_total_avp  <- read_table_candidate("tbl_reg_year_total_avp")
tbl_reg_sex_avp    <- read_table_candidate("tbl_reg_year_sex_avp")

cause_master <- read_cause_master()

# ============================================================
# Sexo: armonización robusta
# ============================================================

fix_sex_names <- function(dt) {
  if ("sex_name" %in% names(dt)) {
    dt[, sex_name := as.character(sex_name)]
    return(invisible(dt))
  }
  
  if ("sex_label" %in% names(dt)) {
    dt[, sex_name := as.character(sex_label)]
    return(invisible(dt))
  }
  
  if ("sex_id" %in% names(dt)) {
    dt[, sex_id := suppressWarnings(as.integer(sex_id))]
    dt[, sex_name := fcase(
      sex_id == 8507L, "Varón",
      sex_id == 8532L, "Mujer",
      sex_id == 3L,    "Total",
      default = "Total"
    )]
  }
  
  invisible(dt)
}

normalize_sex <- function(dt) {
  if (!"sex_name" %in% names(dt)) return(dt)
  
  dt[, sex_name := clean_text(sex_name)]
  
  dt[sex_name %in% c(
    "mujer", "mujeres", "female", "femenino"
  ), sex_name := "Mujer"]
  
  dt[sex_name %in% c(
    "hombre", "hombres", "varon", "varones", "male", "masculino"
  ), sex_name := "Varón"]
  
  dt[sex_name %in% c(
    "total", "ambos", "both", "all"
  ), sex_name := "Total"]
  
  invisible(dt)
}

fix_sex_names(tbl_reg_sex_mort)
fix_sex_names(tbl_reg_sex_avp)

normalize_sex(tbl_reg_sex_mort)
normalize_sex(tbl_reg_sex_avp)

cat("\nChequeo sexos tbl_reg_sex_mort:\n")
print(tbl_reg_sex_mort[, .N, by = .(sex_id, sex_name)][order(sex_id, sex_name)])

cat("\nChequeo sexos tbl_reg_sex_avp:\n")
print(tbl_reg_sex_avp[, .N, by = .(sex_id, sex_name)][order(sex_id, sex_name)])

# ============================================================
# Detectar / armonizar columna región
# ============================================================

# ============================================================
# Resolver nombres de región desde location_id
# ------------------------------------------------------------
# Contrato del proyecto:
# - geografía regional canónica: departamentos 1:25
# - tablas pueden traer solo location_id
# - si existe config/maestro_location_dept.csv, usarlo
# ============================================================

ensure_location_id <- function(dt, dt_name) {
  if (!"location_id" %in% names(dt)) {
    stop("La tabla ", dt_name, " no tiene location_id. Revisar export de 11_build_report_tables.R")
  }
  dt[, location_id := suppressWarnings(as.integer(location_id))]
  invisible(dt)
}

ensure_location_id(tbl_reg_total_mort, "tbl_reg_year_total_mort")
ensure_location_id(tbl_reg_sex_mort,   "tbl_reg_year_sex_mort")
ensure_location_id(tbl_reg_total_avp,  "tbl_reg_year_total_avp")
ensure_location_id(tbl_reg_sex_avp,    "tbl_reg_year_sex_avp")

read_location_master <- function() {
  candidates <- c(
    here("config", "maestro_location_dept.csv"),
    here("data", "input", "maestro_location_dept.csv"),
    here("inst", "extdata", "maestro_location_dept.csv")
  )
  
  hit <- find_first_existing(candidates)
  if (is.na(hit)) {
    cat("\nNo encontré maestro_location_dept.csv. Se usarán nombres fallback loc_<id>.\n")
    return(NULL)
  }
  
  cat("\nLeyendo maestro de regiones desde:\n", hit, "\n")
  loc <- fread(hit)
  
  req <- c("location_id", "location_name")
  miss <- setdiff(req, names(loc))
  if (length(miss) > 0) {
    stop("El maestro de regiones existe pero le faltan columnas: ", paste(miss, collapse = ", "))
  }
  
  loc <- unique(loc[, .(
    location_id = as.integer(location_id),
    location_name = as.character(location_name)
  )])
  
  if ("level" %in% names(loc)) {
    loc <- loc[is.na(level) | clean_text(level) %in% c("department", "departamento", "dept")]
  }
  
  loc
}

location_master <- read_location_master()

attach_region_name <- function(dt, location_master = NULL) {
  
  x <- copy(dt)
  
  if (!is.null(location_master)) {
    x <- merge(
      x,
      location_master,
      by = "location_id",
      all.x = TRUE,
      sort = FALSE
    )
  }
  
  if (!"location_name" %in% names(x)) {
    x[, location_name := paste0("loc_", location_id)]
  } else {
    x[is.na(location_name) | trimws(location_name) == "", location_name := paste0("loc_", location_id)]
  }
  
  x[, region_name := as.character(location_name)]
  x[]
}

tbl_reg_total_mort <- attach_region_name(tbl_reg_total_mort, location_master)
tbl_reg_sex_mort   <- attach_region_name(tbl_reg_sex_mort,   location_master)
tbl_reg_total_avp  <- attach_region_name(tbl_reg_total_avp,  location_master)
tbl_reg_sex_avp    <- attach_region_name(tbl_reg_sex_avp,    location_master)

cat("\nChequeo regiones detectadas en mortalidad total:\n")
print(tbl_reg_total_mort[, .N, by = .(location_id, region_name)][order(location_id)])

cat("\nChequeo regiones detectadas en AVP total:\n")
print(tbl_reg_total_avp[, .N, by = .(location_id, region_name)][order(location_id)])

# ============================================================
# Conciliación bloque 1 a partir de nombres nivel 2
# ============================================================

cm_lvl2 <- copy(cause_master[cause_level == 2])
cm_lvl2[, cause_name_clean := clean_text(cause_name)]

match_block1 <- unique(
  rbindlist(
    list(
      cm_lvl2[grepl("digest", cause_name_clean), .(cause_concept_id, cause_name)],
      cm_lvl2[grepl("respirat", cause_name_clean), .(cause_concept_id, cause_name)],
      cm_lvl2[
        grepl("genitour", cause_name_clean) |
          grepl("urinar", cause_name_clean) |
          grepl("kidney", cause_name_clean),
        .(cause_concept_id, cause_name)
      ],
      cm_lvl2[
        grepl("skin", cause_name_clean) |
          grepl("piel", cause_name_clean),
        .(cause_concept_id, cause_name)
      ],
      cm_lvl2[
        grepl("musculo", cause_name_clean) |
          grepl("skelet", cause_name_clean),
        .(cause_concept_id, cause_name)
      ],
      cm_lvl2[grepl("congenit", cause_name_clean), .(cause_concept_id, cause_name)],
      cm_lvl2[grepl("diabet", cause_name_clean), .(cause_concept_id, cause_name)],
      cm_lvl2[
        grepl("endocr", cause_name_clean) |
          grepl("blood", cause_name_clean) |
          grepl("immune", cause_name_clean) |
          grepl("hemat", cause_name_clean),
        .(cause_concept_id, cause_name)
      ],
      cm_lvl2[grepl("neurolog", cause_name_clean), .(cause_concept_id, cause_name)],
      cm_lvl2[
        grepl("cardio", cause_name_clean) |
          grepl("circulator", cause_name_clean) |
          grepl("vascular", cause_name_clean),
        .(cause_concept_id, cause_name)
      ]
    ),
    use.names = TRUE,
    fill = TRUE
  )
)

if (nrow(match_block1) == 0) {
  stop("No logré conciliar las causas nivel 2 del bloque 1 contra cause_master.")
}

cat("\nNivel 2 conciliados para bloque 1:\n")
print(match_block1[order(cause_name)])

lvl2_ids <- unique(match_block1$cause_concept_id)

# ============================================================
# Descendientes 2 -> 3 -> 4
# ============================================================

get_descendants <- function(cm, root_ids, max_level = 4L) {
  out <- unique(as.integer(root_ids))
  frontier <- unique(as.integer(root_ids))
  
  repeat {
    kids <- cm[parent_concept_id %in% frontier & cause_level <= max_level, unique(cause_concept_id)]
    kids <- setdiff(kids, out)
    if (length(kids) == 0) break
    out <- unique(c(out, kids))
    frontier <- kids
  }
  
  unique(as.integer(out))
}

valid_causes <- get_descendants(cause_master, lvl2_ids, max_level = 4L)

cm_keep <- copy(
  cause_master[cause_concept_id %in% valid_causes & cause_level %in% 2:4]
)
setorder(cm_keep, cause_level, cause_name)

cat("\nResumen de causas retenidas:\n")
print(cm_keep[, .N, by = cause_level][order(cause_level)])

# ============================================================
# Verificar niveles presentes
# ============================================================

verify_levels_in_table <- function(dt, cm, table_name) {
  ids <- unique(dt$cause_concept_id)
  chk <- cm[cause_concept_id %in% ids, .N, by = cause_level][order(cause_level)]
  cat("\nNiveles presentes en ", table_name, ":\n", sep = "")
  print(chk)
}

verify_levels_in_table(tbl_reg_total_mort, cause_master, "tbl_reg_year_total_mort")
verify_levels_in_table(tbl_reg_total_avp,  cause_master, "tbl_reg_year_total_avp")

# ============================================================
# Filtrar bloque 1
# ============================================================

tbl_reg_total_mort <- tbl_reg_total_mort[cause_concept_id %in% valid_causes]
tbl_reg_sex_mort   <- tbl_reg_sex_mort[cause_concept_id %in% valid_causes]
tbl_reg_total_avp  <- tbl_reg_total_avp[cause_concept_id %in% valid_causes]
tbl_reg_sex_avp    <- tbl_reg_sex_avp[cause_concept_id %in% valid_causes]

# ============================================================
# Orden jerárquico
# ============================================================

make_hierarchy_order <- function(cm_sub) {
  out <- data.table()
  
  walk <- function(id, depth = 0L) {
    row <- cm_sub[cause_concept_id == id]
    if (nrow(row) == 0) return(NULL)
    
    out <<- rbind(
      out,
      data.table(
        cause_concept_id = row$cause_concept_id,
        cause_name = row$cause_name,
        cause_level = row$cause_level,
        parent_concept_id = row$parent_concept_id,
        depth = depth
      ),
      fill = TRUE
    )
    
    kids <- cm_sub[parent_concept_id == id][order(cause_name), cause_concept_id]
    if (length(kids) > 0) {
      for (k in kids) walk(k, depth + 1L)
    }
  }
  
  for (r in cm_sub[cause_level == 2][order(cause_name), cause_concept_id]) {
    walk(r, depth = 0L)
  }
  
  out[, order_id := .I]
  out
}

hier <- make_hierarchy_order(cm_keep)

# ============================================================
# Constructor de hoja
# ============================================================

build_sheet_table <- function(total_dt, sex_dt, value_col = c("metric_rate", "metric_abs")) {
  
  value_col <- match.arg(value_col)
  years <- sort(unique(total_dt$year_id))
  
  base <- copy(hier[, .(
    cause_concept_id, cause_name, cause_level,
    parent_concept_id, depth, order_id
  )])
  
  total_use <- copy(total_dt)[, .(
    cause_concept_id, year_id, value = as.numeric(get(value_col))
  )]
  total_use[, sex_group := "Total"]
  
  sex_use <- copy(sex_dt)[
    sex_name %in% c("Mujer", "Varón"),
    .(cause_concept_id, year_id, sex_group = sex_name, value = as.numeric(get(value_col)))
  ]
  
  long_dt <- rbindlist(list(total_use, sex_use), use.names = TRUE, fill = TRUE)
  long_dt <- long_dt[cause_concept_id %in% base$cause_concept_id]
  
  long_dt <- long_dt[
    ,
    .(value = sum(value, na.rm = TRUE)),
    by = .(cause_concept_id, year_id, sex_group)
  ]
  
  wide_dt <- dcast(
    long_dt,
    cause_concept_id ~ paste(year_id, sex_group, sep = "__"),
    value.var = "value",
    fill = 0
  )
  
  out <- merge(base, wide_dt, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  setorder(out, order_id)
  
  out[, Causa := paste0(strrep("   ", depth), cause_name)]
  
  keep_cols <- c("Causa")
  for (y in years) {
    keep_cols <- c(
      keep_cols,
      paste0(y, "__Total"),
      paste0(y, "__Mujer"),
      paste0(y, "__Varón")
    )
  }
  
  for (cc in keep_cols[-1]) {
    if (!cc %in% names(out)) out[, (cc) := 0]
  }
  
  out <- out[, ..keep_cols]
  
  total_row <- data.table(Causa = "Total")
  for (cc in keep_cols[-1]) {
    total_row[, (cc) := sum(out[[cc]], na.rm = TRUE)]
  }
  
  rbind(out, total_row, fill = TRUE)
}

# ============================================================
# Writer Excel
# ============================================================

write_sheet_template <- function(wb, sheet_name, dt, digits = 1) {
  
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  
  n_cols <- ncol(dt)
  years <- unique(sub("__.*$", "", names(dt)[-1]))
  
  writeData(wb, sheet_name, x = "Causa", startCol = 1, startRow = 2, colNames = FALSE)
  
  col_ptr <- 2
  for (yy in years) {
    writeData(wb, sheet_name, x = yy, startCol = col_ptr, startRow = 1, colNames = FALSE)
    mergeCells(wb, sheet_name, cols = col_ptr:(col_ptr + 2), rows = 1)
    writeData(
      wb, sheet_name,
      x = c("Total", "Mujer", "Varon"),
      startCol = col_ptr, startRow = 2,
      colNames = FALSE
    )
    col_ptr <- col_ptr + 3
  }
  
  body <- copy(dt)
  names(body) <- c("Causa", rep(c("Total", "Mujer", "Varon"), length(years)))
  
  for (j in 2:ncol(body)) {
    body[[j]] <- fmt_num(body[[j]], digits = digits)
  }
  
  writeData(wb, sheet_name, x = body, startCol = 1, startRow = 3, colNames = FALSE)
  
  style_header <- createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center",
    border = "Bottom"
  )
  
  style_year <- createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center"
  )
  
  style_body <- createStyle(valign = "center")
  
  addStyle(wb, sheet_name, style_header, rows = 2, cols = 1:n_cols, gridExpand = TRUE)
  addStyle(wb, sheet_name, style_year, rows = 1, cols = 2:n_cols, gridExpand = TRUE)
  addStyle(wb, sheet_name, style_body, rows = 3:(nrow(body) + 2), cols = 1:n_cols, gridExpand = TRUE)
  
  setColWidths(wb, sheet_name, cols = 1, widths = 45)
  setColWidths(wb, sheet_name, cols = 2:n_cols, widths = 12)
  freezePane(wb, sheet_name, firstActiveRow = 3, firstActiveCol = 2)
}

# ============================================================
# Loop por región
# ============================================================

regions <- unique(
  tbl_reg_total_mort[, .(location_id, region_name)]
)[order(location_id)]

cat("\nNúmero de regiones detectadas:", nrow(regions), "\n")

for (i in seq_len(nrow(regions))) {
  
  loc_id <- regions$location_id[i]
  r <- regions$region_name[i]
  
  cat("\nGenerando Excel región:", r, " (location_id=", loc_id, ")\n", sep = "")
  
  mort_total_r <- tbl_reg_total_mort[location_id == loc_id]
  mort_sex_r   <- tbl_reg_sex_mort[location_id == loc_id]
  avp_total_r  <- tbl_reg_total_avp[location_id == loc_id]
  avp_sex_r    <- tbl_reg_sex_avp[location_id == loc_id]
  
  if (nrow(mort_total_r) == 0 && nrow(avp_total_r) == 0) {
    cat("  Región sin datos. Se omite.\n")
    next
  }
  
  mort_reg <- build_sheet_table(
    total_dt = mort_total_r,
    sex_dt   = mort_sex_r,
    value_col = "metric_rate"
  )
  
  avp_reg <- build_sheet_table(
    total_dt = avp_total_r,
    sex_dt   = avp_sex_r,
    value_col = "metric_abs"
  )
  
  wb_reg <- createWorkbook()
  write_sheet_template(wb_reg, "Mortalidad", mort_reg, digits = 1)
  write_sheet_template(wb_reg, "AVP", avp_reg, digits = 1)
  
  fout <- file.path(
    dir_output,
    paste0(
      "region_",
      sprintf("%02d", loc_id),
      "_",
      slug_text(r),
      "_bloque1_validacion.xlsx"
    )
  )
  
  saveWorkbook(wb_reg, file = fout, overwrite = TRUE)
}

cat("\nListo.\n")
cat("Salida en:\n", dir_output, "\n")