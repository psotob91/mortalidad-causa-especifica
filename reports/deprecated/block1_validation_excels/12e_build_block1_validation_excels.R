#!/usr/bin/env Rscript

# ============================================================
# 12e_build_block1_validation_excels.R
# ------------------------------------------------------------
# Genera anexos Excel para validación interna del bloque 1
# Mortalidad y AVP
#
# Salida:
#   reports/block1_validation_excels/
#        nacional_bloque1_validacion.xlsx
#        region_<nombre>_bloque1_validacion.xlsx
#
# Cada Excel:
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
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(openxlsx)
  library(here)
})

cat("\nConstruyendo anexos Excel bloque 1...\n")

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

dir_output <- here("reports", "block1_validation_excels")
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

tbl_nat_total_mort <- read_table_candidate("tbl_nat_year_total_mort")
tbl_nat_sex_mort   <- read_table_candidate("tbl_nat_year_sex_mort")
tbl_nat_total_avp  <- read_table_candidate("tbl_nat_year_total_avp")
tbl_nat_sex_avp    <- read_table_candidate("tbl_nat_year_sex_avp")

tbl_reg_total_mort <- read_table_candidate("tbl_reg_year_total_mort")
tbl_reg_sex_mort   <- read_table_candidate("tbl_reg_year_sex_mort")
tbl_reg_total_avp  <- read_table_candidate("tbl_reg_year_total_avp")
tbl_reg_sex_avp    <- read_table_candidate("tbl_reg_year_sex_avp")

cause_master <- read_cause_master()

# ============================================================
# Sexo: armonización robusta con OMOP-like del proyecto
#   8507 = hombre/varón
#   8532 = mujer
# ============================================================

fix_sex_names <- function(dt) {
  if ("sex_name" %in% names(dt)) {
    dt[, sex_name := as.character(sex_name)]
    return(invisible(dt))
  }
  
  if ("sex_id" %in% names(dt)) {
    dt[, sex_id := suppressWarnings(as.integer(sex_id))]
    dt[, sex_name := fcase(
      sex_id == 8507L, "Varón",
      sex_id == 8532L, "Mujer",
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

fix_sex_names(tbl_nat_sex_mort)
fix_sex_names(tbl_reg_sex_mort)
fix_sex_names(tbl_nat_sex_avp)
fix_sex_names(tbl_reg_sex_avp)

normalize_sex(tbl_nat_sex_mort)
normalize_sex(tbl_reg_sex_mort)
normalize_sex(tbl_nat_sex_avp)
normalize_sex(tbl_reg_sex_avp)

# chequeo rápido de sexo
cat("\nChequeo sexos tbl_nat_sex_mort:\n")
print(tbl_nat_sex_mort[, .N, by = .(sex_id, sex_name)][order(sex_id, sex_name)])

cat("\nChequeo sexos tbl_nat_sex_avp:\n")
print(tbl_nat_sex_avp[, .N, by = .(sex_id, sex_name)][order(sex_id, sex_name)])

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

verify_levels_in_table(tbl_nat_total_mort, cause_master, "tbl_nat_year_total_mort")
verify_levels_in_table(tbl_nat_total_avp,  cause_master, "tbl_nat_year_total_avp")

# ============================================================
# Filtrar bloque 1
# ============================================================

tbl_nat_total_mort <- tbl_nat_total_mort[cause_concept_id %in% valid_causes]
tbl_nat_sex_mort   <- tbl_nat_sex_mort[cause_concept_id %in% valid_causes]
tbl_nat_total_avp  <- tbl_nat_total_avp[cause_concept_id %in% valid_causes]
tbl_nat_sex_avp    <- tbl_nat_sex_avp[cause_concept_id %in% valid_causes]

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
  
  # Por seguridad, colapsar si hubiera duplicados por causa-año-sexo
  long_dt <- long_dt[, .(value = sum(value, na.rm = TRUE)), by = .(cause_concept_id, year_id, sex_group)]
  
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
    writeData(wb, sheet_name, x = c("Total", "Mujer", "Varon"), startCol = col_ptr, startRow = 2, colNames = FALSE)
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
# Nacional
# ============================================================

cat("\nGenerando Excel nacional...\n")

mort_nat <- build_sheet_table(
  total_dt = tbl_nat_total_mort,
  sex_dt = tbl_nat_sex_mort,
  value_col = "metric_rate"
)

avp_nat <- build_sheet_table(
  total_dt = tbl_nat_total_avp,
  sex_dt = tbl_nat_sex_avp,
  value_col = "metric_abs"
)

wb_nat <- createWorkbook()
write_sheet_template(wb_nat, "Mortalidad", mort_nat, digits = 1)
write_sheet_template(wb_nat, "AVP", avp_nat, digits = 1)

saveWorkbook(
  wb_nat,
  file = file.path(dir_output, "nacional_bloque1_validacion.xlsx"),
  overwrite = TRUE
)

# ============================================================
# Regiones
# ============================================================

regions <- sort(unique(tbl_reg_total_mort$location_name))

for (r in regions) {
  cat("Generando Excel región:", r, "\n")
  
  mort_reg <- build_sheet_table(
    total_dt = tbl_reg_total_mort[location_name == r],
    sex_dt   = tbl_reg_sex_mort[location_name == r],
    value_col = "metric_rate"
  )
  
  avp_reg <- build_sheet_table(
    total_dt = tbl_reg_total_avp[location_name == r],
    sex_dt   = tbl_reg_sex_avp[location_name == r],
    value_col = "metric_abs"
  )
  
  wb_reg <- createWorkbook()
  write_sheet_template(wb_reg, "Mortalidad", mort_reg, digits = 1)
  write_sheet_template(wb_reg, "AVP", avp_reg, digits = 1)
  
  fout <- file.path(
    dir_output,
    paste0("region_", gsub(" ", "_", clean_text(r)), "_bloque1_validacion.xlsx")
  )
  
  saveWorkbook(wb_reg, file = fout, overwrite = TRUE)
}

cat("\nListo.\n")
cat("Salida en:\n", dir_output, "\n")