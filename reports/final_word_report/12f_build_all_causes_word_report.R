#!/usr/bin/env Rscript

# ============================================================
# 12f_build_all_causes_word_report.R
# ------------------------------------------------------------
# Informe Word descriptivo para TODAS las causas:
#   - Mortalidad y AVP
#   - Nacional: L1, L2 y L3 top
#   - Regional 2024 resumido: L1 completo y L2 top
#   - Sin sexo ni edad en región
#   - Heatmaps simples (sin mapas geográficos)
#
# Requiere tablas generadas por 11_build_report_tables.R
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

cat("\nConstruyendo informe Word de mortalidad y AVP para todas las causas...\n")

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

theme_ft_small <- function(ft) {
  ft |>
    theme_booktabs() |>
    fontsize(size = 7, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = 1, align = "left", part = "all") |>
    padding(padding = 1, part = "all") |>
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

pick_first_col <- function(dt, candidates, label) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    stop("No encontré columna para ", label, ". Candidatas: ", paste(candidates, collapse = ", "))
  }
  hit[1]
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
  if (is.na(hit)) stop("No encontré cause_master en data/final/cause_master/")
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
  regional_year = 2024L,
  top_n_l2_trend = 10L,
  top_n_l2_bar = 15L,
  top_n_l3_bar = 20L,
  top_n_reg_l2 = 10L,
  out_dir = here("reports", "all_causes_word_report"),
  fig_dir = here("reports", "all_causes_word_report", "figures"),
  out_file = here("reports", "all_causes_word_report", "informe_mortalidad_avp_todas_las_causas.docx")
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Inputs
# ============================================================

tbl_nat_total_mort <- read_table_candidate("tbl_nat_year_total_mort")
tbl_nat_total_avp  <- read_table_candidate("tbl_nat_year_total_avp")
tbl_reg_total_mort <- read_table_candidate("tbl_reg_year_total_mort")
tbl_reg_total_avp  <- read_table_candidate("tbl_reg_year_total_avp")

cause_master <- read_cause_master()

# ============================================================
# Garantizar nombres y niveles
# ============================================================

cm_lookup <- unique(cause_master[, .(cause_concept_id, cause_name, cause_level)])

ensure_cause_cols <- function(dt) {
  x <- copy(dt)
  
  if (!"cause_name" %in% names(x) || !"cause_level" %in% names(x)) {
    x <- merge(
      x,
      cm_lookup,
      by = "cause_concept_id",
      all.x = TRUE
    )
  } else {
    miss_name <- is.na(x$cause_name) | trimws(as.character(x$cause_name)) == ""
    miss_lvl  <- is.na(x$cause_level)
    if (any(miss_name) || any(miss_lvl)) {
      x <- merge(
        x,
        cm_lookup,
        by = "cause_concept_id",
        all.x = TRUE,
        suffixes = c("", "_cm")
      )
      if ("cause_name_cm" %in% names(x)) {
        x[miss_name, cause_name := cause_name_cm]
        x[, cause_name_cm := NULL]
      }
      if ("cause_level_cm" %in% names(x)) {
        x[miss_lvl, cause_level := cause_level_cm]
        x[, cause_level_cm := NULL]
      }
    }
  }
  
  x <- x[year_id %in% CFG$years]
  x <- x[cause_level %in% c(1L, 2L, 3L)]
  x
}

tbl_nat_total_mort <- ensure_cause_cols(tbl_nat_total_mort)
tbl_nat_total_avp  <- ensure_cause_cols(tbl_nat_total_avp)
tbl_reg_total_mort <- ensure_cause_cols(tbl_reg_total_mort)
tbl_reg_total_avp  <- ensure_cause_cols(tbl_reg_total_avp)

# ============================================================
# Resolver columnas clave
# ============================================================

pick_metric_col <- function(dt, preferred, alternatives = character()) {
  candidates <- c(preferred, alternatives)
  hit <- candidates[candidates %in% names(dt)][1]
  if (length(hit) == 0 || is.na(hit)) {
    stop("No encontré métrica válida. Probé: ", paste(candidates, collapse = ", "))
  }
  hit
}

mort_rate_col_nat <- pick_metric_col(
  tbl_nat_total_mort,
  preferred = "metric_rate",
  alternatives = c("rate", "mortality_rate", "rate_per_100k", "value")
)

avp_rate_col_nat <- pick_metric_col(
  tbl_nat_total_avp,
  preferred = "metric_rate",
  alternatives = c("rate", "avp_rate", "yll_rate", "rate_per_100k")
)

avp_abs_col_nat <- pick_metric_col(
  tbl_nat_total_avp,
  preferred = "metric_abs",
  alternatives = c("avp", "yll", "value", "count")
)

mort_rate_col_reg <- pick_metric_col(
  tbl_reg_total_mort,
  preferred = "metric_rate",
  alternatives = c("rate", "mortality_rate", "rate_per_100k", "value")
)

avp_rate_col_reg <- pick_metric_col(
  tbl_reg_total_avp,
  preferred = "metric_rate",
  alternatives = c("rate", "avp_rate", "yll_rate", "rate_per_100k")
)

loc_name_col_reg_mort <- pick_first_col(
  tbl_reg_total_mort,
  c("location_name", "location_label", "region_name", "departamento", "department_name"),
  "nombre de región en mortalidad regional"
)

loc_name_col_reg_avp <- pick_first_col(
  tbl_reg_total_avp,
  c("location_name", "location_label", "region_name", "departamento", "department_name"),
  "nombre de región en AVP regional"
)

# ============================================================
# Helpers de tablas
# ============================================================

make_rank_table <- function(dt, level, value_col, year = CFG$editorial_year) {
  out <- copy(
    dt[year_id == year & cause_level == level,
       .(cause_name, valor = get(value_col))]
  )
  out <- out[!is.na(cause_name)]
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
  long <- long[!is.na(cause_name)]
  
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

make_two_year_compare_table <- function(dt, level, value_col, y0 = 2018L, y1 = 2024L) {
  long <- copy(
    dt[year_id %in% c(y0, y1) & cause_level == level,
       .(cause_name, year_id, valor = get(value_col))]
  )
  wide <- dcast(long, cause_name ~ year_id, value.var = "valor", fill = 0)
  y0c <- as.character(y0)
  y1c <- as.character(y1)
  wide[, cambio_abs := get(y1c) - get(y0c)]
  wide[, cambio_pct := fcase(
    get(y0c) == 0 & get(y1c) == 0, 0,
    get(y0c) == 0 & get(y1c) != 0, NA_real_,
    default = 100 * (get(y1c) - get(y0c)) / get(y0c)
  )]
  setorder(wide, cause_name)
  wide
}

make_region_l1_wide <- function(dt, value_col, location_col, year = CFG$regional_year) {
  x <- copy(
    dt[year_id == year & cause_level == 1,
       .(region = as.character(get(location_col)), cause_name, valor = get(value_col))]
  )
  x <- x[!is.na(region) & !is.na(cause_name)]
  x <- dcast(x, region ~ cause_name, value.var = "valor", fill = 0)
  setorder(x, region)
  x
}

make_region_top_l2 <- function(dt, value_col, location_col, year = CFG$regional_year) {
  x <- copy(
    dt[year_id == year & cause_level == 2,
       .(region = as.character(get(location_col)), cause_name, valor = get(value_col))]
  )
  x <- x[!is.na(region) & !is.na(cause_name)]
  x <- x[order(region, -valor, cause_name)]
  x <- x[, .SD[1], by = region]
  setnames(x, c("region", "cause_name", "valor"), c("Región", "Causa_L2_principal", "Valor"))
  setorder(x, Región)
  x
}

format_metric_table <- function(dt, metric = c("rate", "abs")) {
  metric <- match.arg(metric)
  out <- copy(dt)
  
  num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, "ranking")
  
  if ("ranking" %in% names(out)) out[, ranking := as.integer(ranking)]
  
  for (j in num_cols) {
    if (metric == "rate") out[, (j) := fmt_num(get(j), 1)]
    if (metric == "abs")  out[, (j) := fmt_int(get(j))]
  }
  
  out
}

# ============================================================
# Helpers de figuras
# ============================================================

make_trend_plot_dt <- function(dt, level, value_col, top_n = 10L, year_ref = CFG$editorial_year) {
  keep_causes <- dt[
    year_id == year_ref & cause_level == level,
    .(valor_ref = get(value_col)),
    by = cause_name
  ][order(-valor_ref)][1:min(top_n, .N), cause_name]
  
  out <- copy(
    dt[cause_level == level & cause_name %in% keep_causes,
       .(cause_name, year_id, valor = get(value_col))]
  )
  setorder(out, cause_name, year_id)
  out
}

make_rank_plot_dt <- function(dt, level, value_col, year = CFG$editorial_year, top_n = 15L) {
  out <- copy(
    dt[year_id == year & cause_level == level,
       .(cause_name, valor = get(value_col))]
  )
  out <- out[!is.na(cause_name)]
  out <- out[order(-valor, cause_name)][1:min(top_n, .N)]
  out <- out[order(valor, cause_name)]
  out
}

make_heatmap_dt_l1 <- function(dt, value_col, location_col, year = CFG$regional_year) {
  out <- copy(
    dt[year_id == year & cause_level == 1,
       .(region = as.character(get(location_col)), cause_name, valor = get(value_col))]
  )
  out <- out[!is.na(region) & !is.na(cause_name)]
  out
}

make_heatmap_dt_l2_top <- function(dt, value_col, location_col, year = CFG$regional_year, top_n = 10L) {
  keep_causes <- dt[
    year_id == year & cause_level == 2,
    .(valor_nat = sum(get(value_col), na.rm = TRUE)),
    by = cause_name
  ][order(-valor_nat)][1:min(top_n, .N), cause_name]
  
  out <- copy(
    dt[year_id == year & cause_level == 2 & cause_name %in% keep_causes,
       .(region = as.character(get(location_col)), cause_name, valor = get(value_col))]
  )
  out <- out[!is.na(region) & !is.na(cause_name)]
  out
}

# ============================================================
# Tablas nacionales
# ============================================================

# Mortalidad
mort_l1_rank   <- format_metric_table(make_rank_table(tbl_nat_total_mort, 1, mort_rate_col_nat), "rate")
mort_l1_trend  <- format_metric_table(make_trend_table(tbl_nat_total_mort, 1, mort_rate_col_nat), "rate")
mort_l1_2y     <- format_metric_table(make_two_year_compare_table(tbl_nat_total_mort, 1, mort_rate_col_nat), "rate")

mort_l2_rank   <- format_metric_table(make_rank_table(tbl_nat_total_mort, 2, mort_rate_col_nat), "rate")
mort_l2_trend  <- format_metric_table(make_trend_table(tbl_nat_total_mort, 2, mort_rate_col_nat), "rate")

mort_l3_rank   <- format_metric_table(make_rank_table(tbl_nat_total_mort, 3, mort_rate_col_nat), "rate")

# AVP
avp_l1_rank    <- format_metric_table(make_rank_table(tbl_nat_total_avp, 1, avp_rate_col_nat), "rate")
avp_l1_trend   <- format_metric_table(make_trend_table(tbl_nat_total_avp, 1, avp_rate_col_nat), "rate")
avp_l1_2y      <- format_metric_table(make_two_year_compare_table(tbl_nat_total_avp, 1, avp_rate_col_nat), "rate")

avp_l2_rank    <- format_metric_table(make_rank_table(tbl_nat_total_avp, 2, avp_abs_col_nat), "abs")
avp_l2_trend   <- format_metric_table(make_trend_table(tbl_nat_total_avp, 2, avp_abs_col_nat), "abs")

avp_l3_rank    <- format_metric_table(make_rank_table(tbl_nat_total_avp, 3, avp_abs_col_nat), "abs")

# ============================================================
# Tablas regionales 2024
# ============================================================

reg_l1_mort_wide <- format_metric_table(
  make_region_l1_wide(tbl_reg_total_mort, mort_rate_col_reg, loc_name_col_reg_mort),
  "rate"
)

reg_l1_avp_wide <- format_metric_table(
  make_region_l1_wide(tbl_reg_total_avp, avp_rate_col_reg, loc_name_col_reg_avp),
  "rate"
)

reg_l2_top_mort <- format_metric_table(
  make_region_top_l2(tbl_reg_total_mort, mort_rate_col_reg, loc_name_col_reg_mort),
  "rate"
)

reg_l2_top_avp <- format_metric_table(
  make_region_top_l2(tbl_reg_total_avp, avp_rate_col_reg, loc_name_col_reg_avp),
  "rate"
)

# ============================================================
# Figuras nacionales
# ============================================================

fig_mort_l1_trend_dt <- make_trend_plot_dt(tbl_nat_total_mort, 1, mort_rate_col_nat, top_n = 20L)
fig_avp_l1_trend_dt  <- make_trend_plot_dt(tbl_nat_total_avp, 1, avp_rate_col_nat, top_n = 20L)

fig_mort_l2_rank_dt  <- make_rank_plot_dt(tbl_nat_total_mort, 2, mort_rate_col_nat, top_n = CFG$top_n_l2_bar)
fig_avp_l2_rank_dt   <- make_rank_plot_dt(tbl_nat_total_avp,  2, avp_abs_col_nat, top_n = CFG$top_n_l2_bar)

fig_mort_l3_rank_dt  <- make_rank_plot_dt(tbl_nat_total_mort, 3, mort_rate_col_nat, top_n = CFG$top_n_l3_bar)
fig_avp_l3_rank_dt   <- make_rank_plot_dt(tbl_nat_total_avp,  3, avp_abs_col_nat, top_n = CFG$top_n_l3_bar)

p_mort_l1_trend <- ggplot(
  fig_mort_l1_trend_dt,
  aes(x = year_id, y = valor, group = cause_name, linetype = cause_name)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  labs(
    title = "Figura M1. Tendencia nacional de mortalidad por nivel 1",
    subtitle = "Tasa por 100 000 habitantes, Perú 2018–2024",
    x = "Año",
    y = "Tasa por 100 000",
    linetype = "Causa"
  ) +
  scale_x_continuous(breaks = CFG$years) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_avp_l1_trend <- ggplot(
  fig_avp_l1_trend_dt,
  aes(x = year_id, y = valor, group = cause_name, linetype = cause_name)
) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.5) +
  labs(
    title = "Figura A1. Tendencia nacional de AVP por nivel 1",
    subtitle = "Tasa por 100 000 habitantes, Perú 2018–2024",
    x = "Año",
    y = "Tasa por 100 000",
    linetype = "Causa"
  ) +
  scale_x_continuous(breaks = CFG$years) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_mort_l2_rank <- ggplot(
  fig_mort_l2_rank_dt,
  aes(x = reorder(cause_name, valor), y = valor)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figura M2. Mortalidad nacional 2024 por nivel 2",
    subtitle = "Top causas por tasa",
    x = NULL,
    y = "Tasa por 100 000"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_avp_l2_rank <- ggplot(
  fig_avp_l2_rank_dt,
  aes(x = reorder(cause_name, valor), y = valor)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figura A2. AVP nacional 2024 por nivel 2",
    subtitle = "Top causas por AVP absoluto",
    x = NULL,
    y = "AVP"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_mort_l3_rank <- ggplot(
  fig_mort_l3_rank_dt,
  aes(x = reorder(cause_name, valor), y = valor)
) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Figura M3. Mortalidad nacional 2024 por nivel 3",
    subtitle = "Top causas por tasa",
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
    title = "Figura A3. AVP nacional 2024 por nivel 3",
    subtitle = "Top causas por AVP absoluto",
    x = NULL,
    y = "AVP"
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

# ============================================================
# Figuras regionales
# ============================================================

heat_mort_l1_dt <- make_heatmap_dt_l1(tbl_reg_total_mort, mort_rate_col_reg, loc_name_col_reg_mort)
heat_avp_l1_dt  <- make_heatmap_dt_l1(tbl_reg_total_avp,  avp_rate_col_reg,  loc_name_col_reg_avp)

heat_mort_l2_dt <- make_heatmap_dt_l2_top(tbl_reg_total_mort, mort_rate_col_reg, loc_name_col_reg_mort, top_n = CFG$top_n_reg_l2)
heat_avp_l2_dt  <- make_heatmap_dt_l2_top(tbl_reg_total_avp,  avp_rate_col_reg,  loc_name_col_reg_avp, top_n = CFG$top_n_reg_l2)

p_heat_mort_l1 <- ggplot(
  heat_mort_l1_dt,
  aes(x = cause_name, y = reorder(region, region), fill = valor)
) +
  geom_tile() +
  labs(
    title = "Figura R1. Mortalidad regional por nivel 1, 2024",
    subtitle = "Tasa por 100 000 habitantes",
    x = NULL,
    y = "Región",
    fill = "Tasa"
  ) +
  scale_fill_viridis_c(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_heat_avp_l1 <- ggplot(
  heat_avp_l1_dt,
  aes(x = cause_name, y = reorder(region, region), fill = valor)
) +
  geom_tile() +
  labs(
    title = "Figura R2. AVP regional por nivel 1, 2024",
    subtitle = "Tasa por 100 000 habitantes",
    x = NULL,
    y = "Región",
    fill = "Tasa"
  ) +
  scale_fill_viridis_c(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_heat_mort_l2 <- ggplot(
  heat_mort_l2_dt,
  aes(x = cause_name, y = reorder(region, region), fill = valor)
) +
  geom_tile() +
  labs(
    title = "Figura R3. Mortalidad regional por causas seleccionadas de nivel 2, 2024",
    subtitle = "Top causas nacionales L2. Tasa por 100 000 habitantes",
    x = NULL,
    y = "Región",
    fill = "Tasa"
  ) +
  scale_fill_viridis_c(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p_heat_avp_l2 <- ggplot(
  heat_avp_l2_dt,
  aes(x = cause_name, y = reorder(region, region), fill = valor)
) +
  geom_tile() +
  labs(
    title = "Figura R4. AVP regional por causas seleccionadas de nivel 2, 2024",
    subtitle = "Top causas nacionales L2. Tasa por 100 000 habitantes",
    x = NULL,
    y = "Región",
    fill = "Tasa"
  ) +
  scale_fill_viridis_c(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ============================================================
# Guardar figuras
# ============================================================

file_fig_mort_l1_trend <- file.path(CFG$fig_dir, "fig_mort_l1_trend.png")
file_fig_avp_l1_trend  <- file.path(CFG$fig_dir, "fig_avp_l1_trend.png")
file_fig_mort_l2_rank  <- file.path(CFG$fig_dir, "fig_mort_l2_rank.png")
file_fig_avp_l2_rank   <- file.path(CFG$fig_dir, "fig_avp_l2_rank.png")
file_fig_mort_l3_rank  <- file.path(CFG$fig_dir, "fig_mort_l3_rank.png")
file_fig_avp_l3_rank   <- file.path(CFG$fig_dir, "fig_avp_l3_rank.png")

file_fig_heat_mort_l1  <- file.path(CFG$fig_dir, "fig_heat_mort_l1.png")
file_fig_heat_avp_l1   <- file.path(CFG$fig_dir, "fig_heat_avp_l1.png")
file_fig_heat_mort_l2  <- file.path(CFG$fig_dir, "fig_heat_mort_l2.png")
file_fig_heat_avp_l2   <- file.path(CFG$fig_dir, "fig_heat_avp_l2.png")

save_plot(p_mort_l1_trend, file_fig_mort_l1_trend, width = 8.3, height = 5.0)
save_plot(p_avp_l1_trend,  file_fig_avp_l1_trend,  width = 8.3, height = 5.0)
save_plot(p_mort_l2_rank,  file_fig_mort_l2_rank,  width = 8.0, height = 5.8)
save_plot(p_avp_l2_rank,   file_fig_avp_l2_rank,   width = 8.0, height = 5.8)
save_plot(p_mort_l3_rank,  file_fig_mort_l3_rank,  width = 8.0, height = 6.5)
save_plot(p_avp_l3_rank,   file_fig_avp_l3_rank,   width = 8.0, height = 6.5)

save_plot(p_heat_mort_l1,  file_fig_heat_mort_l1,  width = 8.5, height = 6.4)
save_plot(p_heat_avp_l1,   file_fig_heat_avp_l1,   width = 8.5, height = 6.4)
save_plot(p_heat_mort_l2,  file_fig_heat_mort_l2,  width = 8.8, height = 6.8)
save_plot(p_heat_avp_l2,   file_fig_heat_avp_l2,   width = 8.8, height = 6.8)

# ============================================================
# Word
# ============================================================

doc <- read_docx()

doc <- doc |>
  body_add_par("Informe descriptivo preliminar: Mortalidad y AVP de todas las causas", style = "heading 1") |>
  body_add_par("Perú, 2018–2024", style = "heading 2") |>
  body_add_par(
    "El presente documento resume resultados descriptivos nacionales y regionales para mortalidad y AVP de todas las causas incluidas en la jerarquía analítica. El resumen nacional se organiza por niveles 1, 2 y 3, mientras que el resumen regional se presenta para 2024 sin desagregación por sexo ni edad.",
    style = "Normal"
  )

# ------------------------------------------------------------
# Mortalidad
# ------------------------------------------------------------

doc <- doc |>
  body_add_par("Mortalidad", style = "heading 1")

# L1
doc <- doc |>
  body_add_par("Nivel 1", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla M1. Ranking nacional 2024 de mortalidad por nivel 1",
  theme_ft(flextable(mort_l1_rank))
)

doc <- add_ft_title(
  doc,
  "Tabla M2. Comparación 2018 vs 2024 de mortalidad por nivel 1",
  theme_ft(flextable(mort_l1_2y))
)

doc <- add_ft_title(
  doc,
  "Tabla M3. Tendencia nacional 2018–2024 de mortalidad por nivel 1",
  theme_ft(flextable(mort_l1_trend))
)

doc <- add_plot_title(
  doc,
  "Figura M1. Tendencia nacional de mortalidad por nivel 1",
  file_fig_mort_l1_trend,
  width = 6.8,
  height = 4.2
)

# L2
doc <- doc |>
  body_add_par("Nivel 2", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla M4. Ranking nacional 2024 de mortalidad por nivel 2",
  theme_ft(flextable(mort_l2_rank))
)

doc <- add_ft_title(
  doc,
  "Tabla M5. Tendencia nacional 2018–2024 de mortalidad por nivel 2",
  theme_ft(flextable(mort_l2_trend))
)

doc <- add_plot_title(
  doc,
  "Figura M2. Mortalidad nacional 2024 por nivel 2",
  file_fig_mort_l2_rank,
  width = 6.8,
  height = 4.8
)

# L3 top
doc <- doc |>
  body_add_par("Nivel 3", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla M6. Ranking nacional 2024 de mortalidad por nivel 3",
  theme_ft(flextable(mort_l3_rank))
)

doc <- add_plot_title(
  doc,
  "Figura M3. Mortalidad nacional 2024 por nivel 3",
  file_fig_mort_l3_rank,
  width = 6.8,
  height = 5.4
)

# ------------------------------------------------------------
# AVP
# ------------------------------------------------------------

doc <- doc |>
  body_add_par("AVP", style = "heading 1")

# L1
doc <- doc |>
  body_add_par("Nivel 1", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla A1. Ranking nacional 2024 de AVP por nivel 1",
  theme_ft(flextable(avp_l1_rank))
)

doc <- add_ft_title(
  doc,
  "Tabla A2. Comparación 2018 vs 2024 de AVP por nivel 1",
  theme_ft(flextable(avp_l1_2y))
)

doc <- add_ft_title(
  doc,
  "Tabla A3. Tendencia nacional 2018–2024 de AVP por nivel 1",
  theme_ft(flextable(avp_l1_trend))
)

doc <- add_plot_title(
  doc,
  "Figura A1. Tendencia nacional de AVP por nivel 1",
  file_fig_avp_l1_trend,
  width = 6.8,
  height = 4.2
)

# L2
doc <- doc |>
  body_add_par("Nivel 2", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla A4. Ranking nacional 2024 de AVP por nivel 2",
  theme_ft(flextable(avp_l2_rank))
)

doc <- add_ft_title(
  doc,
  "Tabla A5. Tendencia nacional 2018–2024 de AVP por nivel 2",
  theme_ft(flextable(avp_l2_trend))
)

doc <- add_plot_title(
  doc,
  "Figura A2. AVP nacional 2024 por nivel 2",
  file_fig_avp_l2_rank,
  width = 6.8,
  height = 4.8
)

# L3 top
doc <- doc |>
  body_add_par("Nivel 3", style = "heading 2")

doc <- add_ft_title(
  doc,
  "Tabla A6. Ranking nacional 2024 de AVP por nivel 3",
  theme_ft(flextable(avp_l3_rank))
)

doc <- add_plot_title(
  doc,
  "Figura A3. AVP nacional 2024 por nivel 3",
  file_fig_avp_l3_rank,
  width = 6.8,
  height = 5.4
)

# ------------------------------------------------------------
# Resumen regional
# ------------------------------------------------------------

doc <- doc |>
  body_add_par("Resumen regional 2024", style = "heading 1") |>
  body_add_par(
    "Las siguientes tablas y figuras resumen la variación regional en 2024 sin desagregación por sexo ni edad.",
    style = "Normal"
  )

doc <- add_ft_title(
  doc,
  "Tabla R1. Mortalidad regional 2024 por nivel 1",
  theme_ft_small(flextable(reg_l1_mort_wide))
)

doc <- add_plot_title(
  doc,
  "Figura R1. Mortalidad regional por nivel 1, 2024",
  file_fig_heat_mort_l1,
  width = 6.8,
  height = 5.2
)

doc <- add_ft_title(
  doc,
  "Tabla R2. AVP regional 2024 por nivel 1",
  theme_ft_small(flextable(reg_l1_avp_wide))
)

doc <- add_plot_title(
  doc,
  "Figura R2. AVP regional por nivel 1, 2024",
  file_fig_heat_avp_l1,
  width = 6.8,
  height = 5.2
)

doc <- add_ft_title(
  doc,
  "Tabla R3. Principal causa de mortalidad nivel 2 por región, 2024",
  theme_ft(flextable(reg_l2_top_mort))
)

doc <- add_plot_title(
  doc,
  "Figura R3. Mortalidad regional por causas seleccionadas de nivel 2, 2024",
  file_fig_heat_mort_l2,
  width = 6.8,
  height = 5.4
)

doc <- add_ft_title(
  doc,
  "Tabla R4. Principal causa de AVP nivel 2 por región, 2024",
  theme_ft(flextable(reg_l2_top_avp))
)

doc <- add_plot_title(
  doc,
  "Figura R4. AVP regional por causas seleccionadas de nivel 2, 2024",
  file_fig_heat_avp_l2,
  width = 6.8,
  height = 5.4
)

print(doc, target = CFG$out_file)

cat("\nListo.\n")
cat("Word generado en:\n", CFG$out_file, "\n")