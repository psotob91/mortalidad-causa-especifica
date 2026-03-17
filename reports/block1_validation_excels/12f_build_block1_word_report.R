# #!/usr/bin/env Rscript
# 
# # ============================================================
# # 12f_build_block1_word_report.R
# # ------------------------------------------------------------
# # Informe Word simple para bloque 1:
# #   - Mortalidad y AVP
# #   - L2 y L3 por separado
# #   - ranking nacional 2024 (sin top)
# #   - tendencias 2018-2024
# #   - desagregación por sexo (2024)
# #
# # Usa:
# #   data/derived/tables/*.parquet|csv
# #   data/final/cause_master/cause_master.csv|parquet
# # ============================================================
# 
# suppressPackageStartupMessages({
#   library(data.table)
#   library(arrow)
#   library(officer)
#   library(flextable)
#   library(here)
# })
# 
# cat("\nConstruyendo informe Word bloque 1...\n")
# 
# # ============================================================
# # Helpers
# # ============================================================
# 
# read_auto <- function(path) {
#   ext <- tolower(tools::file_ext(path))
#   if (!file.exists(path)) stop("No existe: ", path)
#   if (ext == "parquet") return(as.data.table(arrow::read_parquet(path)))
#   if (ext == "csv") return(data.table::fread(path))
#   stop("Extensión no soportada: ", path)
# }
# 
# find_first_existing <- function(paths) {
#   hit <- paths[file.exists(paths)][1]
#   if (length(hit) == 0 || is.na(hit)) return(NA_character_)
#   hit
# }
# 
# read_table_candidate <- function(filename_stub) {
#   candidates <- c(
#     here("data", "derived", "tables", paste0(filename_stub, ".parquet")),
#     here("data", "derived", "tables", paste0(filename_stub, ".csv")),
#     here("data", "final", "tables", paste0(filename_stub, ".parquet")),
#     here("data", "final", "tables", paste0(filename_stub, ".csv"))
#   )
#   hit <- find_first_existing(candidates)
#   if (is.na(hit)) stop("No encontré tabla: ", filename_stub)
#   cat("Leyendo: ", hit, "\n", sep = "")
#   read_auto(hit)
# }
# 
# clean_text <- function(x) {
#   x <- tolower(trimws(as.character(x)))
#   x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
#   x <- gsub("[^a-z0-9]+", " ", x)
#   x <- gsub("\\s+", " ", x)
#   trimws(x)
# }
# 
# fmt_num <- function(x, digits = 1) {
#   out <- formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ".")
#   out[is.na(x)] <- ""
#   out
# }
# 
# fmt_int <- function(x) {
#   out <- formatC(round(x), format = "f", digits = 0, big.mark = " ", decimal.mark = ".")
#   out[is.na(x)] <- ""
#   out
# }
# 
# theme_ft <- function(ft) {
#   ft |>
#     theme_booktabs() |>
#     fontsize(size = 8, part = "all") |>
#     align(align = "center", part = "all") |>
#     align(j = 1, align = "left", part = "all") |>
#     padding(padding = 2, part = "all") |>
#     autofit()
# }
# 
# add_ft_title <- function(doc, title, ft) {
#   doc |>
#     body_add_par(title, style = "heading 3") |>
#     body_add_flextable(ft)
# }
# 
# # ============================================================
# # Cause master
# # ============================================================
# 
# read_cause_master <- function() {
#   candidates <- c(
#     here("data", "final", "cause_master", "cause_master.csv"),
#     here("data", "final", "cause_master", "cause_master.parquet")
#   )
#   hit <- find_first_existing(candidates)
#   if (is.na(hit)) {
#     stop("No encontré cause_master en data/final/cause_master/")
#   }
#   cat("Leyendo cause_master desde: ", hit, "\n", sep = "")
#   cm <- read_auto(hit)
#   
#   req <- c("cause_concept_id", "cause_name", "cause_level", "parent_concept_id", "is_terminal")
#   miss <- setdiff(req, names(cm))
#   if (length(miss) > 0) stop("Faltan columnas en cause_master: ", paste(miss, collapse = ", "))
#   
#   cm <- unique(cm[, ..req])
#   cm[, cause_concept_id := as.integer(cause_concept_id)]
#   cm[, parent_concept_id := as.integer(parent_concept_id)]
#   cm[, cause_level := as.integer(cause_level)]
#   cm
# }
# 
# # ============================================================
# # Config
# # ============================================================
# 
# CFG <- list(
#   years = 2018:2024,
#   editorial_year = 2024L,
#   out_dir = here("reports", "block1_word_report"),
#   out_file = here("reports", "block1_word_report", "informe_bloque1_mortalidad_avp.docx")
# )
# 
# dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
# 
# # ============================================================
# # Inputs
# # ============================================================
# 
# tbl_nat_total_mort <- read_table_candidate("tbl_nat_year_total_mort")
# tbl_nat_sex_mort   <- read_table_candidate("tbl_nat_year_sex_mort")
# tbl_nat_total_avp  <- read_table_candidate("tbl_nat_year_total_avp")
# tbl_nat_sex_avp    <- read_table_candidate("tbl_nat_year_sex_avp")
# 
# cause_master <- read_cause_master()
# 
# # ============================================================
# # Armonizar sexo
# # ============================================================
# 
# fix_sex_names <- function(dt) {
#   if (!"sex_name" %in% names(dt) && "sex_id" %in% names(dt)) {
#     dt[, sex_name := fcase(
#       sex_id == 8507L, "Hombre",
#       sex_id == 8532L, "Mujer",
#       default = "Total"
#     )]
#   }
#   
#   if ("sex_name" %in% names(dt)) {
#     dt[, sex_name := clean_text(sex_name)]
#     dt[sex_name %in% c("hombre", "male", "masculino", "varon"), sex_name := "Hombre"]
#     dt[sex_name %in% c("mujer", "female", "femenino"), sex_name := "Mujer"]
#     dt[sex_name %in% c("ambos", "both", "total"), sex_name := "Total"]
#   }
#   
#   invisible(dt)
# }
# 
# fix_sex_names(tbl_nat_sex_mort)
# fix_sex_names(tbl_nat_sex_avp)
# 
# # ============================================================
# # Conciliación bloque 1
# # ============================================================
# 
# cm_lvl2 <- copy(cause_master[cause_level == 2])
# cm_lvl2[, cause_name_clean := clean_text(cause_name)]
# 
# match_block1 <- unique(
#   rbindlist(
#     list(
#       cm_lvl2[grepl("digest", cause_name_clean), .(cause_concept_id, cause_name)],
#       cm_lvl2[grepl("respirat", cause_name_clean), .(cause_concept_id, cause_name)],
#       cm_lvl2[
#         grepl("genitour", cause_name_clean) |
#           grepl("urinar", cause_name_clean) |
#           grepl("kidney", cause_name_clean),
#         .(cause_concept_id, cause_name)
#       ],
#       cm_lvl2[
#         grepl("skin", cause_name_clean) |
#           grepl("piel", cause_name_clean),
#         .(cause_concept_id, cause_name)
#       ],
#       cm_lvl2[
#         grepl("musculo", cause_name_clean) |
#           grepl("skelet", cause_name_clean),
#         .(cause_concept_id, cause_name)
#       ],
#       cm_lvl2[grepl("congenit", cause_name_clean), .(cause_concept_id, cause_name)],
#       cm_lvl2[grepl("diabet", cause_name_clean), .(cause_concept_id, cause_name)],
#       cm_lvl2[
#         grepl("endocr", cause_name_clean) |
#           grepl("blood", cause_name_clean) |
#           grepl("immune", cause_name_clean) |
#           grepl("hemat", cause_name_clean),
#         .(cause_concept_id, cause_name)
#       ],
#       cm_lvl2[grepl("neurolog", cause_name_clean), .(cause_concept_id, cause_name)],
#       cm_lvl2[
#         grepl("cardio", cause_name_clean) |
#           grepl("circulator", cause_name_clean) |
#           grepl("vascular", cause_name_clean),
#         .(cause_concept_id, cause_name)
#       ]
#     ),
#     use.names = TRUE,
#     fill = TRUE
#   )
# )
# 
# if (nrow(match_block1) == 0) {
#   stop("No logré conciliar las causas nivel 2 del bloque 1.")
# }
# 
# lvl2_ids <- unique(match_block1$cause_concept_id)
# 
# get_descendants <- function(cm, root_ids, max_level = 4L) {
#   out <- unique(as.integer(root_ids))
#   frontier <- unique(as.integer(root_ids))
#   
#   repeat {
#     kids <- cm[parent_concept_id %in% frontier & cause_level <= max_level, unique(cause_concept_id)]
#     kids <- setdiff(kids, out)
#     if (length(kids) == 0) break
#     out <- unique(c(out, kids))
#     frontier <- kids
#   }
#   
#   unique(as.integer(out))
# }
# 
# valid_causes <- get_descendants(cause_master, lvl2_ids, max_level = 4L)
# 
# cm_keep <- copy(cause_master[cause_concept_id %in% valid_causes & cause_level %in% 2:4])
# 
# # ============================================================
# # Filtrar bloque y años
# # ============================================================
# 
# tbl_nat_total_mort <- tbl_nat_total_mort[
#   cause_concept_id %in% valid_causes & year_id %in% CFG$years
# ]
# tbl_nat_sex_mort <- tbl_nat_sex_mort[
#   cause_concept_id %in% valid_causes & year_id %in% CFG$years
# ]
# tbl_nat_total_avp <- tbl_nat_total_avp[
#   cause_concept_id %in% valid_causes & year_id %in% CFG$years
# ]
# tbl_nat_sex_avp <- tbl_nat_sex_avp[
#   cause_concept_id %in% valid_causes & year_id %in% CFG$years
# ]
# 
# # ============================================================
# # Garantizar nombres
# # ============================================================
# 
# if (!"cause_name" %in% names(tbl_nat_total_mort)) {
#   tbl_nat_total_mort <- merge(
#     tbl_nat_total_mort,
#     cause_master[, .(cause_concept_id, cause_name, cause_level)],
#     by = "cause_concept_id",
#     all.x = TRUE
#   )
# }
# 
# if (!"cause_name" %in% names(tbl_nat_total_avp)) {
#   tbl_nat_total_avp <- merge(
#     tbl_nat_total_avp,
#     cause_master[, .(cause_concept_id, cause_name, cause_level)],
#     by = "cause_concept_id",
#     all.x = TRUE
#   )
# }
# 
# if (!"cause_name" %in% names(tbl_nat_sex_mort)) {
#   tbl_nat_sex_mort <- merge(
#     tbl_nat_sex_mort,
#     cause_master[, .(cause_concept_id, cause_name, cause_level)],
#     by = "cause_concept_id",
#     all.x = TRUE
#   )
# }
# 
# if (!"cause_name" %in% names(tbl_nat_sex_avp)) {
#   tbl_nat_sex_avp <- merge(
#     tbl_nat_sex_avp,
#     cause_master[, .(cause_concept_id, cause_name, cause_level)],
#     by = "cause_concept_id",
#     all.x = TRUE
#   )
# }
# 
# # ============================================================
# # Helpers de tablas
# # ============================================================
# 
# make_rank_table <- function(dt, level, value_col, year = CFG$editorial_year) {
#   out <- copy(
#     dt[year_id == year & cause_level == level,
#        .(cause_name, valor = get(value_col))]
#   )
#   out <- out[order(-valor, cause_name)]
#   out[, ranking := .I]
#   setcolorder(out, c("ranking", "cause_name", "valor"))
#   out
# }
# 
# make_trend_table <- function(dt, level, value_col) {
#   long <- copy(
#     dt[cause_level == level,
#        .(cause_name, year_id, valor = get(value_col))]
#   )
#   
#   wide <- dcast(long, cause_name ~ year_id, value.var = "valor", fill = 0)
#   
#   y0 <- as.character(min(CFG$years))
#   y1 <- as.character(max(CFG$years))
#   
#   wide[, cambio_abs_2018_2024 := get(y1) - get(y0)]
#   wide[, cambio_pct_2018_2024 := fcase(
#     get(y0) == 0 & get(y1) == 0, 0,
#     get(y0) == 0 & get(y1) != 0, NA_real_,
#     default = 100 * (get(y1) - get(y0)) / get(y0)
#   )]
#   
#   setorder(wide, cause_name)
#   wide
# }
# 
# make_sex_table <- function(dt, level, value_col, year = CFG$editorial_year) {
#   long <- copy(
#     dt[year_id == year & cause_level == level & sex_name %in% c("Hombre", "Mujer"),
#        .(cause_name, sex_name, valor = get(value_col))]
#   )
#   
#   wide <- dcast(long, cause_name ~ sex_name, value.var = "valor", fill = 0)
#   if (!"Hombre" %in% names(wide)) wide[, Hombre := 0]
#   if (!"Mujer" %in% names(wide)) wide[, Mujer := 0]
#   wide[, diferencia_h_m := Hombre - Mujer]
#   setorder(wide, cause_name)
#   wide
# }
# 
# format_metric_table <- function(dt, metric = c("rate", "abs")) {
#   metric <- match.arg(metric)
#   out <- copy(dt)
#   
#   num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
#   num_cols <- setdiff(num_cols, "ranking")
#   
#   if ("ranking" %in% names(out)) {
#     out[, ranking := as.integer(ranking)]
#   }
#   
#   for (j in num_cols) {
#     if (metric == "rate") out[, (j) := fmt_num(get(j), 1)]
#     if (metric == "abs")  out[, (j) := fmt_int(get(j))]
#   }
#   
#   out
# }
# 
# ft_caption <- function(txt) {
#   flextable(data.frame(texto = txt)) |>
#     delete_part(part = "header") |>
#     fontsize(size = 9) |>
#     italic(i = 1, j = 1, italic = TRUE, part = "body") |>
#     border_remove() |>
#     autofit()
# }
# 
# # ============================================================
# # Construcción de tablas
# # ============================================================
# 
# # Mortalidad
# mort_l2_rank  <- format_metric_table(make_rank_table(tbl_nat_total_mort, 2, "metric_rate"), "rate")
# mort_l3_rank  <- format_metric_table(make_rank_table(tbl_nat_total_mort, 3, "metric_rate"), "rate")
# mort_l2_trend <- format_metric_table(make_trend_table(tbl_nat_total_mort, 2, "metric_rate"), "rate")
# mort_l3_trend <- format_metric_table(make_trend_table(tbl_nat_total_mort, 3, "metric_rate"), "rate")
# mort_l2_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_mort, 2, "metric_rate"), "rate")
# mort_l3_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_mort, 3, "metric_rate"), "rate")
# 
# # AVP
# avp_l2_rank  <- format_metric_table(make_rank_table(tbl_nat_total_avp, 2, "metric_abs"), "abs")
# avp_l3_rank  <- format_metric_table(make_rank_table(tbl_nat_total_avp, 3, "metric_abs"), "abs")
# avp_l2_trend <- format_metric_table(make_trend_table(tbl_nat_total_avp, 2, "metric_abs"), "abs")
# avp_l3_trend <- format_metric_table(make_trend_table(tbl_nat_total_avp, 3, "metric_abs"), "abs")
# avp_l2_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_avp, 2, "metric_abs"), "abs")
# avp_l3_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_avp, 3, "metric_abs"), "abs")
# 
# # ============================================================
# # Word
# # ============================================================
# 
# doc <- read_docx()
# 
# doc <- doc |>
#   body_add_par("Informe descriptivo preliminar: Mortalidad y AVP del bloque 1", style = "heading 1") |>
#   body_add_par("Perú, 2018–2024", style = "heading 2") |>
#   body_add_par(
#     "El presente documento resume tablas descriptivas nacionales para mortalidad y AVP del bloque 1, organizadas por nivel 2 y nivel 3, incluyendo rankings para 2024, tendencias 2018–2024 y desagregación por sexo para 2024.",
#     style = "Normal"
#   )
# 
# # Mortalidad
# doc <- doc |>
#   body_add_par("Mortalidad", style = "heading 1") |>
#   body_add_par("Nivel 2", style = "heading 2")
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla M1. Ranking nacional 2024 de mortalidad por nivel 2",
#   theme_ft(flextable(mort_l2_rank))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla M2. Tendencia nacional 2018–2024 de mortalidad por nivel 2",
#   theme_ft(flextable(mort_l2_trend))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla M3. Mortalidad nacional por sexo en 2024, nivel 2",
#   theme_ft(flextable(mort_l2_sex))
# )
# 
# doc <- doc |>
#   body_add_par("Nivel 3", style = "heading 2")
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla M4. Ranking nacional 2024 de mortalidad por nivel 3",
#   theme_ft(flextable(mort_l3_rank))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla M5. Tendencia nacional 2018–2024 de mortalidad por nivel 3",
#   theme_ft(flextable(mort_l3_trend))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla M6. Mortalidad nacional por sexo en 2024, nivel 3",
#   theme_ft(flextable(mort_l3_sex))
# )
# 
# # AVP
# doc <- doc |>
#   body_add_par("AVP", style = "heading 1") |>
#   body_add_par("Nivel 2", style = "heading 2")
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla A1. Ranking nacional 2024 de AVP por nivel 2",
#   theme_ft(flextable(avp_l2_rank))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla A2. Tendencia nacional 2018–2024 de AVP por nivel 2",
#   theme_ft(flextable(avp_l2_trend))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla A3. AVP nacional por sexo en 2024, nivel 2",
#   theme_ft(flextable(avp_l2_sex))
# )
# 
# doc <- doc |>
#   body_add_par("Nivel 3", style = "heading 2")
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla A4. Ranking nacional 2024 de AVP por nivel 3",
#   theme_ft(flextable(avp_l3_rank))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla A5. Tendencia nacional 2018–2024 de AVP por nivel 3",
#   theme_ft(flextable(avp_l3_trend))
# )
# 
# doc <- add_ft_title(
#   doc,
#   "Tabla A6. AVP nacional por sexo en 2024, nivel 3",
#   theme_ft(flextable(avp_l3_sex))
# )
# 
# print(doc, target = CFG$out_file)
# 
# cat("\nListo.\n")
# cat("Word generado en:\n", CFG$out_file, "\n")

#!/usr/bin/env Rscript

# ============================================================
# 12f_build_block1_word_report.R
# ------------------------------------------------------------
# Informe Word simple para bloque 1:
#   - Mortalidad y AVP
#   - L2 y L3 por separado
#   - ranking nacional 2024 (sin top)
#   - tendencias 2018-2024
#   - desagregación por sexo (2024)
#   - 4 figuras simples y robustas
#
# Usa:
#   data/derived/tables/*.parquet|csv
#   data/final/cause_master/cause_master.csv|parquet
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(officer)
  library(flextable)
  library(here)
  library(ggplot2)
  library(scales)
})

cat("\nConstruyendo informe Word bloque 1...\n")

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

read_table_candidate <- function(filename_stub) {
  candidates <- c(
    here("data", "derived", "tables", paste0(filename_stub, ".parquet")),
    here("data", "derived", "tables", paste0(filename_stub, ".csv")),
    here("data", "final", "tables", paste0(filename_stub, ".parquet")),
    here("data", "final", "tables", paste0(filename_stub, ".csv"))
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) stop("No encontré tabla: ", filename_stub)
  cat("Leyendo: ", hit, "\n", sep = "")
  read_auto(hit)
}

clean_text <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

fmt_num <- function(x, digits = 1) {
  out <- formatC(x, format = "f", digits = digits, big.mark = " ", decimal.mark = ".")
  out[is.na(x)] <- ""
  out
}

fmt_int <- function(x) {
  out <- formatC(round(x), format = "f", digits = 0, big.mark = " ", decimal.mark = ".")
  out[is.na(x)] <- ""
  out
}

theme_ft <- function(ft) {
  ft |>
    theme_booktabs() |>
    fontsize(size = 8, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = 1, align = "left", part = "all") |>
    padding(padding = 2, part = "all") |>
    autofit()
}

add_ft_title <- function(doc, title, ft) {
  doc |>
    body_add_par(title, style = "heading 3") |>
    body_add_flextable(ft)
}

save_plot <- function(plot_obj, file, width = 10, height = 6, dpi = 300) {
  ggplot2::ggsave(
    filename = file,
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    bg = "white"
  )
}

theme_fig <- function() {
  theme_minimal(base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

add_plot_title <- function(doc, title, img_path, width = 6.8, height = 4.5) {
  doc |>
    body_add_par(title, style = "heading 3") |>
    body_add_img(src = img_path, width = width, height = height)
}

# ============================================================
# Cause master
# ============================================================

read_cause_master <- function() {
  candidates <- c(
    here("data", "final", "cause_master", "cause_master.csv"),
    here("data", "final", "cause_master", "cause_master.parquet")
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) {
    stop("No encontré cause_master en data/final/cause_master/")
  }
  cat("Leyendo cause_master desde: ", hit, "\n", sep = "")
  cm <- read_auto(hit)
  
  req <- c("cause_concept_id", "cause_name", "cause_level", "parent_concept_id", "is_terminal")
  miss <- setdiff(req, names(cm))
  if (length(miss) > 0) stop("Faltan columnas en cause_master: ", paste(miss, collapse = ", "))
  
  cm <- unique(cm[, ..req])
  cm[, cause_concept_id := as.integer(cause_concept_id)]
  cm[, parent_concept_id := as.integer(parent_concept_id)]
  cm[, cause_level := as.integer(cause_level)]
  cm
}

# ============================================================
# Config
# ============================================================

CFG <- list(
  years = 2018:2024,
  editorial_year = 2024L,
  out_dir = here("reports", "block1_word_report"),
  fig_dir = here("reports", "block1_word_report", "figures"),
  out_file = here("reports", "block1_word_report", "informe_bloque1_mortalidad_avp.docx")
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Inputs
# ============================================================

tbl_nat_total_mort <- read_table_candidate("tbl_nat_year_total_mort")
tbl_nat_sex_mort   <- read_table_candidate("tbl_nat_year_sex_mort")
tbl_nat_total_avp  <- read_table_candidate("tbl_nat_year_total_avp")
tbl_nat_sex_avp    <- read_table_candidate("tbl_nat_year_sex_avp")

cause_master <- read_cause_master()

# ============================================================
# Armonizar sexo
# ============================================================

fix_sex_names <- function(dt) {
  if (!"sex_name" %in% names(dt) && "sex_id" %in% names(dt)) {
    dt[, sex_name := fcase(
      sex_id == 8507L, "Hombre",
      sex_id == 8532L, "Mujer",
      default = "Total"
    )]
  }
  
  if ("sex_name" %in% names(dt)) {
    dt[, sex_name := clean_text(sex_name)]
    dt[sex_name %in% c("hombre", "male", "masculino", "varon"), sex_name := "Hombre"]
    dt[sex_name %in% c("mujer", "female", "femenino"), sex_name := "Mujer"]
    dt[sex_name %in% c("ambos", "both", "total"), sex_name := "Total"]
  }
  
  invisible(dt)
}

fix_sex_names(tbl_nat_sex_mort)
fix_sex_names(tbl_nat_sex_avp)

# ============================================================
# Conciliación bloque 1
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
  stop("No logré conciliar las causas nivel 2 del bloque 1.")
}

lvl2_ids <- unique(match_block1$cause_concept_id)

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

cm_keep <- copy(cause_master[cause_concept_id %in% valid_causes & cause_level %in% 2:4])

# ============================================================
# Filtrar bloque y años
# ============================================================

tbl_nat_total_mort <- tbl_nat_total_mort[
  cause_concept_id %in% valid_causes & year_id %in% CFG$years
]
tbl_nat_sex_mort <- tbl_nat_sex_mort[
  cause_concept_id %in% valid_causes & year_id %in% CFG$years
]
tbl_nat_total_avp <- tbl_nat_total_avp[
  cause_concept_id %in% valid_causes & year_id %in% CFG$years
]
tbl_nat_sex_avp <- tbl_nat_sex_avp[
  cause_concept_id %in% valid_causes & year_id %in% CFG$years
]

# ============================================================
# Garantizar nombres
# ============================================================

if (!"cause_name" %in% names(tbl_nat_total_mort)) {
  tbl_nat_total_mort <- merge(
    tbl_nat_total_mort,
    cause_master[, .(cause_concept_id, cause_name, cause_level)],
    by = "cause_concept_id",
    all.x = TRUE
  )
}

if (!"cause_name" %in% names(tbl_nat_total_avp)) {
  tbl_nat_total_avp <- merge(
    tbl_nat_total_avp,
    cause_master[, .(cause_concept_id, cause_name, cause_level)],
    by = "cause_concept_id",
    all.x = TRUE
  )
}

if (!"cause_name" %in% names(tbl_nat_sex_mort)) {
  tbl_nat_sex_mort <- merge(
    tbl_nat_sex_mort,
    cause_master[, .(cause_concept_id, cause_name, cause_level)],
    by = "cause_concept_id",
    all.x = TRUE
  )
}

if (!"cause_name" %in% names(tbl_nat_sex_avp)) {
  tbl_nat_sex_avp <- merge(
    tbl_nat_sex_avp,
    cause_master[, .(cause_concept_id, cause_name, cause_level)],
    by = "cause_concept_id",
    all.x = TRUE
  )
}

# ============================================================
# Helpers de tablas
# ============================================================

make_rank_table <- function(dt, level, value_col, year = CFG$editorial_year) {
  out <- copy(
    dt[year_id == year & cause_level == level,
       .(cause_name, valor = get(value_col))]
  )
  out <- out[order(-valor, cause_name)]
  out[, ranking := .I]
  setcolorder(out, c("ranking", "cause_name", "valor"))
  out
}

make_trend_table <- function(dt, level, value_col) {
  long <- copy(
    dt[cause_level == level,
       .(cause_name, year_id, valor = get(value_col))]
  )
  
  wide <- dcast(long, cause_name ~ year_id, value.var = "valor", fill = 0)
  
  y0 <- as.character(min(CFG$years))
  y1 <- as.character(max(CFG$years))
  
  wide[, cambio_abs_2018_2024 := get(y1) - get(y0)]
  wide[, cambio_pct_2018_2024 := fcase(
    get(y0) == 0 & get(y1) == 0, 0,
    get(y0) == 0 & get(y1) != 0, NA_real_,
    default = 100 * (get(y1) - get(y0)) / get(y0)
  )]
  
  setorder(wide, cause_name)
  wide
}

make_sex_table <- function(dt, level, value_col, year = CFG$editorial_year) {
  long <- copy(
    dt[year_id == year & cause_level == level & sex_name %in% c("Hombre", "Mujer"),
       .(cause_name, sex_name, valor = get(value_col))]
  )
  
  wide <- dcast(long, cause_name ~ sex_name, value.var = "valor", fill = 0)
  if (!"Hombre" %in% names(wide)) wide[, Hombre := 0]
  if (!"Mujer" %in% names(wide)) wide[, Mujer := 0]
  wide[, diferencia_h_m := Hombre - Mujer]
  setorder(wide, cause_name)
  wide
}

format_metric_table <- function(dt, metric = c("rate", "abs")) {
  metric <- match.arg(metric)
  out <- copy(dt)
  
  num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, "ranking")
  
  if ("ranking" %in% names(out)) {
    out[, ranking := as.integer(ranking)]
  }
  
  for (j in num_cols) {
    if (metric == "rate") out[, (j) := fmt_num(get(j), 1)]
    if (metric == "abs")  out[, (j) := fmt_int(get(j))]
  }
  
  out
}

make_trend_plot_dt <- function(dt, level, value_col) {
  out <- copy(
    dt[cause_level == level,
       .(cause_name, year_id, valor = get(value_col))]
  )
  setorder(out, cause_name, year_id)
  out
}

make_rank_plot_dt <- function(dt, level, value_col, year = CFG$editorial_year) {
  out <- copy(
    dt[year_id == year & cause_level == level,
       .(cause_name, valor = get(value_col))]
  )
  out <- out[order(valor, cause_name)]
  out
}

# ============================================================
# Construcción de tablas
# ============================================================

# Mortalidad
mort_l2_rank  <- format_metric_table(make_rank_table(tbl_nat_total_mort, 2, "metric_rate"), "rate")
mort_l3_rank  <- format_metric_table(make_rank_table(tbl_nat_total_mort, 3, "metric_rate"), "rate")
mort_l2_trend <- format_metric_table(make_trend_table(tbl_nat_total_mort, 2, "metric_rate"), "rate")
mort_l3_trend <- format_metric_table(make_trend_table(tbl_nat_total_mort, 3, "metric_rate"), "rate")
mort_l2_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_mort, 2, "metric_rate"), "rate")
mort_l3_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_mort, 3, "metric_rate"), "rate")

# AVP
avp_l2_rank  <- format_metric_table(make_rank_table(tbl_nat_total_avp, 2, "metric_abs"), "abs")
avp_l3_rank  <- format_metric_table(make_rank_table(tbl_nat_total_avp, 3, "metric_abs"), "abs")
avp_l2_trend <- format_metric_table(make_trend_table(tbl_nat_total_avp, 2, "metric_abs"), "abs")
avp_l3_trend <- format_metric_table(make_trend_table(tbl_nat_total_avp, 3, "metric_abs"), "abs")
avp_l2_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_avp, 2, "metric_abs"), "abs")
avp_l3_sex   <- format_metric_table(make_sex_table(tbl_nat_sex_avp, 3, "metric_abs"), "abs")

# ============================================================
# Construcción de figuras
# ============================================================

fig_mort_l2_trend_dt <- make_trend_plot_dt(tbl_nat_total_mort, 2, "metric_rate")
fig_avp_l2_trend_dt  <- make_trend_plot_dt(tbl_nat_total_avp,  2, "metric_abs")
fig_mort_l3_rank_dt  <- make_rank_plot_dt(tbl_nat_total_mort, 3, "metric_rate")
fig_avp_l3_rank_dt   <- make_rank_plot_dt(tbl_nat_total_avp,  3, "metric_abs")

p_mort_l2_trend <- ggplot(
  fig_mort_l2_trend_dt,
  aes(x = year_id, y = valor, group = cause_name, linetype = cause_name)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  labs(
    title = "Figura M1. Tendencia nacional de mortalidad por nivel 2",
    subtitle = "Tasa por 100 000 habitantes, Perú 2018–2024",
    x = "Año",
    y = "Tasa por 100 000",
    linetype = "Causa"
  ) +
  scale_x_continuous(breaks = CFG$years) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_avp_l2_trend <- ggplot(
  fig_avp_l2_trend_dt,
  aes(x = year_id, y = valor, group = cause_name, linetype = cause_name)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  labs(
    title = "Figura A1. Tendencia nacional de AVP por nivel 2",
    subtitle = "Valores absolutos, Perú 2018–2024",
    x = "Año",
    y = "AVP",
    linetype = "Causa"
  ) +
  scale_x_continuous(breaks = CFG$years) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_mort_l3_rank <- ggplot(
  fig_mort_l3_rank_dt,
  aes(x = reorder(cause_name, valor), y = valor)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figura M2. Mortalidad nacional 2024 por nivel 3",
    subtitle = "Tasa por 100 000 habitantes",
    x = NULL,
    y = "Tasa por 100 000"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_avp_l3_rank <- ggplot(
  fig_avp_l3_rank_dt,
  aes(x = reorder(cause_name, valor), y = valor)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figura A2. AVP nacional 2024 por nivel 3",
    subtitle = "Valores absolutos",
    x = NULL,
    y = "AVP"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

file_fig_mort_l2_trend <- file.path(CFG$fig_dir, "fig_mort_l2_trend.png")
file_fig_avp_l2_trend  <- file.path(CFG$fig_dir, "fig_avp_l2_trend.png")
file_fig_mort_l3_rank  <- file.path(CFG$fig_dir, "fig_mort_l3_rank.png")
file_fig_avp_l3_rank   <- file.path(CFG$fig_dir, "fig_avp_l3_rank.png")

save_plot(p_mort_l2_trend, file_fig_mort_l2_trend, width = 8.5, height = 5.2)
save_plot(p_avp_l2_trend,  file_fig_avp_l2_trend,  width = 8.5, height = 5.2)
save_plot(p_mort_l3_rank,  file_fig_mort_l3_rank,  width = 8.0, height = 6.5)
save_plot(p_avp_l3_rank,   file_fig_avp_l3_rank,   width = 8.0, height = 6.5)

# ============================================================
# Word
# ============================================================

doc <- read_docx()

doc <- doc |>
  body_add_par("Informe descriptivo preliminar: Mortalidad y AVP del bloque 1", style = "heading 1") |>
  body_add_par("Perú, 2018–2024", style = "heading 2") |>
  body_add_par(
    "El presente documento resume tablas descriptivas nacionales para mortalidad y AVP del bloque 1, organizadas por nivel 2 y nivel 3, incluyendo rankings para 2024, tendencias 2018–2024, desagregación por sexo para 2024 y figuras ilustrativas nacionales.",
    style = "Normal"
  )

# Mortalidad
doc <- doc |>
  body_add_par("Mortalidad", style = "heading 1") |>
  body_add_par("Nivel 2", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla M1. Ranking nacional 2024 de mortalidad por nivel 2",
  theme_ft(flextable(mort_l2_rank))
)

doc <- add_ft_title(
  doc,
  "Tabla M2. Tendencia nacional 2018–2024 de mortalidad por nivel 2",
  theme_ft(flextable(mort_l2_trend))
)

doc <- add_plot_title(
  doc,
  "Figura M1. Tendencia nacional de mortalidad por nivel 2",
  file_fig_mort_l2_trend,
  width = 6.8,
  height = 4.2
)

doc <- add_ft_title(
  doc,
  "Tabla M3. Mortalidad nacional por sexo en 2024, nivel 2",
  theme_ft(flextable(mort_l2_sex))
)

doc <- doc |>
  body_add_par("Nivel 3", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla M4. Ranking nacional 2024 de mortalidad por nivel 3",
  theme_ft(flextable(mort_l3_rank))
)

doc <- add_plot_title(
  doc,
  "Figura M2. Mortalidad nacional 2024 por nivel 3",
  file_fig_mort_l3_rank,
  width = 6.8,
  height = 5.4
)

doc <- add_ft_title(
  doc,
  "Tabla M5. Tendencia nacional 2018–2024 de mortalidad por nivel 3",
  theme_ft(flextable(mort_l3_trend))
)

doc <- add_ft_title(
  doc,
  "Tabla M6. Mortalidad nacional por sexo en 2024, nivel 3",
  theme_ft(flextable(mort_l3_sex))
)

# AVP
doc <- doc |>
  body_add_par("AVP", style = "heading 1") |>
  body_add_par("Nivel 2", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla A1. Ranking nacional 2024 de AVP por nivel 2",
  theme_ft(flextable(avp_l2_rank))
)

doc <- add_ft_title(
  doc,
  "Tabla A2. Tendencia nacional 2018–2024 de AVP por nivel 2",
  theme_ft(flextable(avp_l2_trend))
)

doc <- add_plot_title(
  doc,
  "Figura A1. Tendencia nacional de AVP por nivel 2",
  file_fig_avp_l2_trend,
  width = 6.8,
  height = 4.2
)

doc <- add_ft_title(
  doc,
  "Tabla A3. AVP nacional por sexo en 2024, nivel 2",
  theme_ft(flextable(avp_l2_sex))
)

doc <- doc |>
  body_add_par("Nivel 3", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla A4. Ranking nacional 2024 de AVP por nivel 3",
  theme_ft(flextable(avp_l3_rank))
)

doc <- add_plot_title(
  doc,
  "Figura A2. AVP nacional 2024 por nivel 3",
  file_fig_avp_l3_rank,
  width = 6.8,
  height = 5.4
)

doc <- add_ft_title(
  doc,
  "Tabla A5. Tendencia nacional 2018–2024 de AVP por nivel 3",
  theme_ft(flextable(avp_l3_trend))
)

doc <- add_ft_title(
  doc,
  "Tabla A6. AVP nacional por sexo en 2024, nivel 3",
  theme_ft(flextable(avp_l3_sex))
)

print(doc, target = CFG$out_file)

cat("\nListo.\n")
cat("Word generado en:\n", CFG$out_file, "\n")