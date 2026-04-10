#!/usr/bin/env Rscript

# ============================================================
# 12h_build_block1_region_age_excels.R
# ------------------------------------------------------------
# Genera anexos Excel regionales para validación interna
# del bloque 1 por edad y sexo.
#
# Salida:
#   reports/block1_validation_excels_region_age/
#      region_<nombre>_bloque1_mortalidad_por_edad.xlsx
#      region_<nombre>_bloque1_avp_por_edad.xlsx
#
# Estructura de cada Excel:
#   - un sheet por año
#   - filas: causas en orden jerárquico
#   - columnas: grupos etarios
#       dentro de cada grupo:
#         Total | Mujer | Varon
#
# Reglas:
#   - Mortalidad usa metric_rate (tasa por 100 000)
#   - AVP usa metric_abs
#   - Si no hay valor: 0
#   - Filtrar solo bloque 1
#   - Mantener orden jerárquico
#   - Soportar sexos OMOP-like (8507/8532) y/o sex_label
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(openxlsx)
  library(here)
})

cat("\nConstruyendo Excels regionales por edad del bloque 1...\n")

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
  x <- gsub("[^a-z0-9\\+]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

slug_text <- function(x) {
  x <- clean_text(x)
  x <- gsub("\\s+", "_", x)
  x
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
# Config de grupos etarios reportables
# ============================================================

AGE_ORDER <- c(
  "0", "1-4", "5-14", "15-24", "25-34", "35-44",
  "45-54", "55-64", "65-74", "75-84", "85+"
)

SEX_ORDER <- c("Total", "Mujer", "Varon")

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

dir_output <- here("reports", "block1_validation_excels_region_age")
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

tbl_reg_age_sex_mort <- read_table_candidate("tbl_reg_year_age_sex_mort")
tbl_reg_age_sex_avp  <- read_table_candidate("tbl_reg_year_age_sex_avp")

cause_master <- read_cause_master()

# ============================================================
# Armonización sexo
# ============================================================

harmonize_sex <- function(dt) {
  
  if ("sex_label" %in% names(dt)) {
    dt[, sex_raw := as.character(sex_label)]
  } else if ("sex_name" %in% names(dt)) {
    dt[, sex_raw := as.character(sex_name)]
  } else if ("sex_id" %in% names(dt)) {
    dt[, sex_id := suppressWarnings(as.integer(sex_id))]
    dt[, sex_raw := fcase(
      sex_id == 8507L, "Hombre",
      sex_id == 8532L, "Mujer",
      sex_id == 3L,    "Ambos",
      default = as.character(sex_id)
    )]
  } else {
    stop("No encontré columna de sexo en la tabla.")
  }
  
  dt[, sex_std := clean_text(sex_raw)]
  dt[sex_std %in% c("ambos", "both", "total"), sex_std := "Total"]
  dt[sex_std %in% c("mujer", "mujeres", "female", "femenino"), sex_std := "Mujer"]
  dt[sex_std %in% c("hombre", "hombres", "male", "masculino", "varon", "varones"), sex_std := "Varon"]
  
  invisible(dt)
}

harmonize_sex(tbl_reg_age_sex_mort)
harmonize_sex(tbl_reg_age_sex_avp)

cat("\nChequeo sexos mortalidad:\n")
print(tbl_reg_age_sex_mort[, .N, by = .(sex_std)][order(sex_std)])

cat("\nChequeo sexos AVP:\n")
print(tbl_reg_age_sex_avp[, .N, by = .(sex_std)][order(sex_std)])

# ============================================================
# Armonización grupo etario
# ============================================================

harmonize_age_group <- function(dt) {
  
  age_col <- c("age_group", "age_group_name", "age_label")
  age_col <- age_col[age_col %in% names(dt)][1]
  
  if (is.na(age_col)) {
    stop("No encontré columna de grupo etario (age_group / age_group_name / age_label).")
  }
  
  dt[, age_group_std := as.character(get(age_col))]
  dt[, age_group_std := trimws(age_group_std)]
  
  dt <- dt[!age_group_std %in% c("Todas las edades", "All ages", "Total")]
  dt <- dt[age_group_std %in% AGE_ORDER]
  
  dt[, age_group_std := factor(age_group_std, levels = AGE_ORDER, ordered = TRUE)]
  dt[]
}

tbl_reg_age_sex_mort <- harmonize_age_group(tbl_reg_age_sex_mort)
tbl_reg_age_sex_avp  <- harmonize_age_group(tbl_reg_age_sex_avp)

cat("\nChequeo grupos etarios mortalidad:\n")
print(tbl_reg_age_sex_mort[, .N, by = .(age_group_std)][order(age_group_std)])

cat("\nChequeo grupos etarios AVP:\n")
print(tbl_reg_age_sex_avp[, .N, by = .(age_group_std)][order(age_group_std)])

# ============================================================
# Verificar variable de región
# ============================================================

detect_region_col <- function(dt) {
  candidates <- c("location_name", "region_name", "department_name", "location_label")
  hit <- candidates[candidates %in% names(dt)][1]
  if (is.na(hit)) stop("No encontré nombre de región en la tabla.")
  hit
}

REGION_COL_MORT <- detect_region_col(tbl_reg_age_sex_mort)
REGION_COL_AVP  <- detect_region_col(tbl_reg_age_sex_avp)

if (REGION_COL_MORT != REGION_COL_AVP) {
  warning("Las tablas usan distinto nombre de columna para región; se armonizará a region_name.")
}

setnames(tbl_reg_age_sex_mort, REGION_COL_MORT, "region_name")
setnames(tbl_reg_age_sex_avp,  REGION_COL_AVP,  "region_name")

tbl_reg_age_sex_mort[, region_name := as.character(region_name)]
tbl_reg_age_sex_avp[,  region_name := as.character(region_name)]

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
# Descendientes y ancestros
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

get_ancestors <- function(cm, ids) {
  out <- integer()
  parent_map <- unique(cm[, .(cause_concept_id, parent_concept_id)])
  
  for (id in unique(ids)) {
    cur <- as.integer(id)
    seen <- integer()
    
    repeat {
      par <- parent_map[cause_concept_id == cur, parent_concept_id][1]
      if (length(par) == 0 || is.na(par) || par %in% seen) break
      out <- c(out, par)
      seen <- c(seen, par)
      cur <- par
    }
  }
  
  unique(as.integer(out))
}

desc_ids <- get_descendants(cause_master, lvl2_ids, max_level = 4L)
anc_ids  <- get_ancestors(cause_master, lvl2_ids)

valid_causes <- unique(c(desc_ids, anc_ids, lvl2_ids))

cm_keep <- copy(cause_master[cause_concept_id %in% valid_causes & cause_level %in% 1:4])
setorder(cm_keep, cause_level, cause_name)

cat("\nResumen de causas retenidas:\n")
print(cm_keep[, .N, by = cause_level][order(cause_level)])

# ============================================================
# Filtrar bloque 1
# ============================================================

tbl_reg_age_sex_mort <- tbl_reg_age_sex_mort[cause_concept_id %in% valid_causes]
tbl_reg_age_sex_avp  <- tbl_reg_age_sex_avp[cause_concept_id %in% valid_causes]

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
  
  roots <- cm_sub[cause_level == min(cause_level), cause_concept_id]
  roots <- unique(roots)
  
  for (r in cm_sub[cause_concept_id %in% roots][order(cause_name), cause_concept_id]) {
    walk(r, depth = 0L)
  }
  
  out <- unique(out, by = "cause_concept_id")
  out[, order_id := .I]
  out
}

hier <- make_hierarchy_order(cm_keep)

# ============================================================
# Constructor de tabla de un año y una región
# ============================================================

build_year_region_sheet_table <- function(dt_year_region, value_col = c("metric_rate", "metric_abs")) {
  
  value_col <- match.arg(value_col)
  
  base <- copy(hier[, .(
    cause_concept_id, cause_name, cause_level,
    parent_concept_id, depth, order_id
  )])
  
  use <- copy(dt_year_region)[
    sex_std %in% c("Total", "Mujer", "Varon"),
    .(
      cause_concept_id,
      age_group_std,
      sex_std,
      value = as.numeric(get(value_col))
    )
  ]
  
  use <- use[
    ,
    .(value = sum(value, na.rm = TRUE)),
    by = .(cause_concept_id, age_group_std, sex_std)
  ]
  
  use[, age_group_std := as.character(age_group_std)]
  
  wide_dt <- dcast(
    use,
    cause_concept_id ~ paste(age_group_std, sex_std, sep = "__"),
    value.var = "value",
    fill = 0
  )
  
  out <- merge(base, wide_dt, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  setorder(out, order_id)
  
  out[, Causa := paste0(strrep("   ", depth), cause_name)]
  
  keep_cols <- c("Causa")
  for (ag in AGE_ORDER) {
    keep_cols <- c(
      keep_cols,
      paste0(ag, "__Total"),
      paste0(ag, "__Mujer"),
      paste0(ag, "__Varon")
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
# Writer de sheet
# ============================================================

write_year_sheet <- function(wb, sheet_name, dt, digits = 1) {
  
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  
  n_cols <- ncol(dt)
  
  writeData(wb, sheet_name, x = "Causa", startCol = 1, startRow = 2, colNames = FALSE)
  
  col_ptr <- 2
  for (ag in AGE_ORDER) {
    writeData(wb, sheet_name, x = ag, startCol = col_ptr, startRow = 1, colNames = FALSE)
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
  names(body) <- c("Causa", rep(c("Total", "Mujer", "Varon"), length(AGE_ORDER)))
  
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
  
  style_group <- createStyle(
    textDecoration = "bold",
    halign = "center",
    valign = "center"
  )
  
  style_body <- createStyle(valign = "center")
  
  addStyle(wb, sheet_name, style_header, rows = 2, cols = 1:n_cols, gridExpand = TRUE)
  addStyle(wb, sheet_name, style_group, rows = 1, cols = 2:n_cols, gridExpand = TRUE)
  addStyle(wb, sheet_name, style_body, rows = 3:(nrow(body) + 2), cols = 1:n_cols, gridExpand = TRUE)
  
  setColWidths(wb, sheet_name, cols = 1, widths = 45)
  setColWidths(wb, sheet_name, cols = 2:n_cols, widths = 10)
  freezePane(wb, sheet_name, firstActiveRow = 3, firstActiveCol = 2)
}

# ============================================================
# Construcción de workbook regional por métrica
# ============================================================

build_metric_workbook_region <- function(dt_region, value_col, out_file, digits = 1) {
  
  yrs <- sort(unique(dt_region$year_id))
  wb <- createWorkbook()
  
  for (yy in yrs) {
    cat("  Armando sheet año:", yy, "->", basename(out_file), "\n")
    dt_yy <- dt_region[year_id == yy]
    sheet_dt <- build_year_region_sheet_table(dt_yy, value_col = value_col)
    write_year_sheet(wb, as.character(yy), sheet_dt, digits = digits)
  }
  
  saveWorkbook(wb, file = out_file, overwrite = TRUE)
}

# ============================================================
# Loop por región
# ============================================================

regions <- sort(unique(tbl_reg_age_sex_mort$region_name))

cat("\nNúmero de regiones detectadas:", length(regions), "\n")

for (r in regions) {
  
  cat("\nGenerando Excels para región:", r, "\n")
  
  mort_r <- tbl_reg_age_sex_mort[region_name == r]
  avp_r  <- tbl_reg_age_sex_avp[region_name == r]
  
  if (nrow(mort_r) == 0 && nrow(avp_r) == 0) {
    cat("  Región sin datos. Se omite.\n")
    next
  }
  
  fout_mort <- file.path(
    dir_output,
    paste0("region_", slug_text(r), "_bloque1_mortalidad_por_edad.xlsx")
  )
  
  fout_avp <- file.path(
    dir_output,
    paste0("region_", slug_text(r), "_bloque1_avp_por_edad.xlsx")
  )
  
  if (nrow(mort_r) > 0) {
    build_metric_workbook_region(
      dt_region = mort_r,
      value_col = "metric_rate",
      out_file = fout_mort,
      digits = 1
    )
  }
  
  if (nrow(avp_r) > 0) {
    build_metric_workbook_region(
      dt_region = avp_r,
      value_col = "metric_abs",
      out_file = fout_avp,
      digits = 1
    )
  }
}

cat("\nListo.\n")
cat("Salida en:\n", dir_output, "\n")