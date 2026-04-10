#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(openxlsx)
  library(here)
})

read_auto <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!file.exists(path)) stop("No existe: ", path)
  if (ext == "parquet") return(as.data.table(arrow::read_parquet(path)))
  if (ext == "csv") return(data.table::fread(path))
  stop("Extensión no soportada: ", path)
}

find_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (length(hit) == 0L || is.na(hit)) return(NA_character_)
  hit
}

read_table_candidate <- function(filename_stub) {
  candidates <- c(
    here("data", "derived", "tables", paste0(filename_stub, ".parquet")),
    here("data", "derived", "tables", paste0(filename_stub, ".csv"))
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) stop("No encontré tabla: ", filename_stub)
  read_auto(hit)
}

fmt_num <- function(x, digits = 1) {
  out <- formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ".")
  out[is.na(x)] <- ""
  out
}

pretty_colnames <- function(dt) {
  x <- copy(dt)
  map <- c(
    year_id = "Año",
    location_id = "ID ubicación",
    location_name = "Departamento",
    location_scope = "Ámbito geográfico",
    sex_id = "ID sexo",
    sex_label = "Sexo",
    age_group = "Grupo de edad",
    cause_concept_id = "ID causa",
    cause_level = "Nivel",
    cause_name = "Causa",
    display_name = "Causa / nivel",
    parent_name = "Nivel superior inmediato",
    hierarchy_path = "Ruta jerárquica",
    cause_code = "Código de causa",
    population = "Población",
    metric_abs = "Valor absoluto",
    metric_rate = "Tasa por 100 000 hab.",
    metric_type = "Indicador",
    run_id = "ID corrida",
    catalog_origin = "Origen de la categoría",
    source_in_raw_catalog = "Proviene del catálogo base",
    is_considered_cause_category = "Se considera causa de enfermedad",
    cie10 = "CIE-10 declarado",
    cie10_cobertura_utilizada = "CIE-10 usado / inferido",
    icd10_regex = "Regex CIE-10 usado",
    is_terminal = "Es terminal",
    is_covid_related = "Relacionado con COVID-19",
    level_1_name = "Nivel 1",
    level_2_name = "Nivel 2",
    level_3_name = "Nivel 3",
    level_4_name = "Nivel 4"
  )
  keep <- names(map)[names(map) %in% names(x)]
  setnames(x, keep, unname(map[keep]))
  x
}

style_sheet <- function(wb, sheet_name, dt) {
  hdr <- createStyle(textDecoration = "bold", halign = "center", valign = "center")
  body <- createStyle(valign = "center")
  addStyle(wb, sheet_name, hdr, rows = 1, cols = 1:ncol(dt), gridExpand = TRUE)
  addStyle(wb, sheet_name, body, rows = 2:(nrow(dt) + 1), cols = 1:ncol(dt), gridExpand = TRUE)
  setColWidths(wb, sheet_name, cols = 1:ncol(dt), widths = "auto")
  freezePane(wb, sheet_name, firstActiveRow = 2)
}

write_sheet <- function(wb, sheet_name, dt, digits = 1) {
  addWorksheet(wb, sheet_name, gridLines = FALSE)
  x <- pretty_colnames(dt)
  for (nm in names(x)) {
    if (is.numeric(x[[nm]])) x[[nm]] <- fmt_num(x[[nm]], digits = digits)
  }
  writeData(wb, sheet_name, x = x)
  style_sheet(wb, sheet_name, x)
}

dir_output <- here("reports", "all_causes_validation_excels")
dir.create(dir_output, recursive = TRUE, showWarnings = FALSE)

tbl_cause_hierarchy_catalog <- read_table_candidate("tbl_cause_hierarchy_catalog")
tbl_nat_year_total_mort <- read_table_candidate("tbl_nat_year_total_mort")
tbl_nat_year_total_avp  <- read_table_candidate("tbl_nat_year_total_avp")
tbl_reg_year_total_mort <- read_table_candidate("tbl_reg_year_total_mort")
tbl_reg_year_total_avp  <- read_table_candidate("tbl_reg_year_total_avp")
tbl_nat_year_level2_mort <- read_table_candidate("tbl_nat_year_level2_mort")
tbl_nat_year_level2_avp  <- read_table_candidate("tbl_nat_year_level2_avp")
tbl_reg_year_level2_mort <- read_table_candidate("tbl_reg_year_level2_mort")
tbl_reg_year_level2_avp  <- read_table_candidate("tbl_reg_year_level2_avp")

wb_nat <- createWorkbook()
write_sheet(wb_nat, "Notas", data.table(
  Nota = c(
    "Las columnas de tasa se expresan por 100 000 habitantes.",
    "El catálogo de causas indica si la categoría proviene del catálogo base o si fue construida por el pipeline."
  )
), digits = 0)
write_sheet(wb_nat, "Catalogo_causas", tbl_cause_hierarchy_catalog, digits = 0)
write_sheet(wb_nat, "Mort_all_levels", tbl_nat_year_total_mort, digits = 1)
write_sheet(wb_nat, "AVP_all_levels", tbl_nat_year_total_avp, digits = 1)
write_sheet(wb_nat, "Mort_level2_full", tbl_nat_year_level2_mort, digits = 1)
write_sheet(wb_nat, "AVP_level2_full", tbl_nat_year_level2_avp, digits = 1)
saveWorkbook(
  wb_nat,
  file = file.path(dir_output, "nacional_todas_las_causas_validacion.xlsx"),
  overwrite = TRUE
)

wb_reg <- createWorkbook()
write_sheet(wb_reg, "Notas", data.table(
  Nota = c(
    "Las columnas de tasa se expresan por 100 000 habitantes.",
    "Las hojas regionales muestran el universo completo de categorías disponibles para el año 2024."
  )
), digits = 0)
write_sheet(wb_reg, "Mort_total_2024", tbl_reg_year_total_mort[year_id == 2024], digits = 1)
write_sheet(wb_reg, "AVP_total_2024", tbl_reg_year_total_avp[year_id == 2024], digits = 1)
write_sheet(wb_reg, "Mort_level2_2024", tbl_reg_year_level2_mort[year_id == 2024], digits = 1)
write_sheet(wb_reg, "AVP_level2_2024", tbl_reg_year_level2_avp[year_id == 2024], digits = 1)
saveWorkbook(
  wb_reg,
  file = file.path(dir_output, "regional_todas_las_causas_validacion.xlsx"),
  overwrite = TRUE
)

cat("\nListo.\n")
cat("Salida en:\n", dir_output, "\n")
