#!/usr/bin/env Rscript

# ============================================================
# 12f_build_all_causes_word_report.R
# ------------------------------------------------------------
# Informe Word descriptivo para TODAS las causas:
#   - Incluye total (nivel 0) y causas niveles 1 a 4
#   - Mortalidad y AVP
#   - Tablas nacionales y regionales con desglose por sexo
#   - Graficos sinteticos top 10
#   - Anexo jerarquico completo con cobertura CIE-10 y uso analitico
#
# Requiere tablas generadas por 11_build_report_tables.R
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(officer)
  library(flextable)
  library(gridExtra)
  library(grid)
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
  stop("Extension no soportada: ", path)
}

find_first_existing <- function(paths) {
  hit <- paths[file.exists(paths)][1]
  if (length(hit) == 0 || is.na(hit)) return(NA_character_)
  hit
}

read_table_candidate <- function(filename_stub) {
  candidates <- c(
    here("data", "derived", "tables", paste0(filename_stub, ".parquet")),
    here("data", "derived", "tables", paste0(filename_stub, ".csv"))
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) stop("No encontre tabla: ", filename_stub)
  cat("Leyendo: ", hit, "\n", sep = "")
  read_auto(hit)
}

read_methods_candidate <- function(filename_stub) {
  candidates <- c(
    here("data", "derived", "methods", paste0(filename_stub, ".parquet")),
    here("data", "derived", "methods", paste0(filename_stub, ".csv"))
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) stop("No encontre tabla metodologica: ", filename_stub)
  cat("Leyendo: ", hit, "\n", sep = "")
  read_auto(hit)
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

txt <- function(x) {
  x <- enc2utf8(as.character(x))
  replacements <- c(
    "ÃƒÆ’Ã‚Â¡" = "á", "ÃƒÆ’Ã‚Â©" = "é", "ÃƒÆ’Ã‚Â­" = "í", "ÃƒÆ’Ã‚Â³" = "ó", "ÃƒÆ’Ã‚Âº" = "ú",
    "ÃƒÆ’Ã‚Â" = "Á", "ÃƒÆ’Ã¢â‚¬Â°" = "É", "ÃƒÆ’Ã‚Â" = "Í", "ÃƒÆ’Ã¢â‚¬Å“" = "Ó", "ÃƒÆ’Ã…Â¡" = "Ú",
    "ÃƒÆ’Ã‚Â±" = "ñ", "ÃƒÆ’Ã¢â‚¬Ëœ" = "Ñ", "ÃƒÆ’Ã‚Â¼" = "ü", "ÃƒÆ’Ã…â€œ" = "Ü",
    "ÃƒÂ¡" = "á", "ÃƒÂ©" = "é", "ÃƒÂ­" = "í", "ÃƒÂ³" = "ó", "ÃƒÂº" = "ú",
    "ÃƒÂ" = "Á", "Ãƒâ€°" = "É", "ÃƒÂ" = "Í", "Ãƒâ€œ" = "Ó", "ÃƒÅ¡" = "Ú",
    "ÃƒÂ±" = "ñ", "Ãƒâ€˜" = "Ñ", "ÃƒÂ¼" = "ü", "ÃƒÅ“" = "Ü",
    "Ã¡" = "á", "Ã©" = "é", "Ã­" = "í", "Ã³" = "ó", "Ãº" = "ú",
    "Ã" = "Á", "Ã‰" = "É", "Ã" = "Í", "Ã“" = "Ó", "Ãš" = "Ú",
    "Ã±" = "ñ", "Ã‘" = "Ñ", "Ã¼" = "ü", "Ãœ" = "Ü",
    "â€“" = "–", "â€”" = "—", "â€˜" = "‘", "â€™" = "’",
    "â€œ" = "“", "â€" = "”", "Ã‚" = ""
  )
  for (i in seq_len(3L)) {
    for (pattern in names(replacements)) {
      x <- gsub(pattern, replacements[[pattern]], x, fixed = TRUE, useBytes = TRUE)
    }
  }
  x
}

sanitize_dt_text <- function(dt) {
  x <- copy(as.data.table(dt))
  setnames(x, txt(names(x)))
  char_cols <- names(x)[vapply(x, function(col) is.character(col) || is.factor(col), logical(1))]
  for (nm in char_cols) {
    x[, (nm) := txt(as.character(get(nm)))]
  }
  x
}

sanitize_ft <- function(ft) {
  if (!is.null(ft$body$dataset)) ft$body$dataset <- as.data.frame(sanitize_dt_text(ft$body$dataset))
  if (!is.null(ft$header$dataset)) ft$header$dataset <- as.data.frame(sanitize_dt_text(ft$header$dataset))
  ft
}

body_add_par_utf8 <- function(doc, value, style = "Normal") {
  body_add_par(doc, txt(value), style = style)
}

compress_icd10_string <- function(x) {
  if (is.na(x) || !nzchar(trimws(x))) return("")
  codes <- trimws(unlist(strsplit(x, ",")))
  codes <- codes[nzchar(codes)]
  codes <- gsub("\\.", "", codes)
  stems <- unique(sub("^([A-Z][0-9]{2}).*$", "\\1", codes))
  stems <- stems[grepl("^[A-Z][0-9]{2}$", stems)]
  if (length(stems) == 0L) return(x)

  dt <- data.table(
    stem = stems,
    letter = substr(stems, 1, 1),
    num = as.integer(substr(stems, 2, 3))
  )[order(letter, num)]

  parts <- dt[, {
    grp <- cumsum(c(1L, diff(num) != 1L))
    vals <- split(num, grp)
    out <- vapply(vals, function(v) {
      if (length(v) == 1L) {
        sprintf("%s%02d", unique(letter), v)
      } else {
        sprintf("%s%02d-%s%02d", unique(letter), min(v), unique(letter), max(v))
      }
    }, character(1))
    .(txt = paste(out, collapse = ", "))
  }, by = letter]

  paste(parts$txt, collapse = ", ")
}

theme_ft <- function(ft, size = 8) {
  label_map <- c(
    ranking = "Ranking",
    anio = "Año",
    cause_name = "Causa",
    display_name = "Categoria / nivel",
    parent_name = "Nivel superior inmediato",
    hierarchy_path = "Ruta jerÃ¡rquica",
    cause_level = "Nivel",
    valor = "Valor",
    absoluto = "Valor absoluto",
    n_muertes_estimadas = "N muertes estimadas",
    avp_estimados = "AVP estimados",
    tasa_por_100000 = "Tasa por 100 000 hab.",
    n_muertes_ambos = "N muertes estimadas (ambos)",
    n_muertes_hombre = "N muertes estimadas (hombres)",
    n_muertes_mujer = "N muertes estimadas (mujeres)",
    tasa_ambos = "Tasa ambos (por 100 000 hab.)",
    tasa_hombre = "Tasa hombres (por 100 000 hab.)",
    tasa_mujer = "Tasa mujeres (por 100 000 hab.)",
    tasa_ambos_100000 = "Tasa ambos (por 100 000 hab.)",
    tasa_hombre_100000 = "Tasa hombres (por 100 000 hab.)",
    tasa_mujer_100000 = "Tasa mujeres (por 100 000 hab.)",
    avp_ambos = "AVP estimados (ambos)",
    avp_hombre = "AVP estimados (hombres)",
    avp_mujer = "AVP estimados (mujeres)",
    tasa_avp_ambos_100000 = "Tasa AVP ambos (por 100 000 hab.)",
    tasa_avp_hombre_100000 = "Tasa AVP hombres (por 100 000 hab.)",
    tasa_avp_mujer_100000 = "Tasa AVP mujeres (por 100 000 hab.)",
    cambio_absoluto_2018_2024 = "Cambio absoluto 2018-2024",
    cambio_porcentual_2018_2024 = "Cambio porcentual 2018-2024",
    tasa = "Tasa por 100 000 hab.",
    region = "Departamento",
    location_name = "Departamento",
    age_group = "Grupo de edad",
    catalog_origin = "Origen de la categorÃ­a",
    source_in_raw_catalog = "Proviene del catÃ¡logo base",
    is_considered_cause_category = "Se considera causa de enfermedad",
    is_mortality_cause_category = "Se usa para mortalidad",
    is_avp_eligible_cause = "Se usa para cÃ¡lculo de AVP",
    cie10 = "CIE-10 declarado",
    cie10_cobertura_utilizada = "CIE-10 usado / inferido",
    icd10_regex = "Regex CIE-10 usado",
    n_icd10_observados = "N CIE-10 observados",
    icd10_observados_en_datos = "CIE-10 observados en datos",
    nota_especial = "Nota especial",
    is_terminal = "Es terminal",
    is_covid_related = "Relacionado con COVID-19",
    level_1_name = "Nivel 1",
    level_2_name = "Nivel 2",
    level_3_name = "Nivel 3",
    level_4_name = "Nivel 4"
  )
  label_map["is_avp_eligible_cause"] <- "Se usa para AVP"
  label_map["cie10_cobertura_utilizada"] <- "CIE-10 usado"
  keep_map <- label_map[names(label_map) %in% names(ft$body$dataset)]
  keep_map <- txt(keep_map)
  if (length(keep_map) > 0) {
    ft <- do.call(flextable::set_header_labels, c(list(x = ft), as.list(keep_map)))
  }
  ft |>
    theme_booktabs() |>
    font(fontname = "Arial", part = "all") |>
    fontsize(size = 6, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = 1, align = "left", part = "all") |>
    padding(padding = 2, part = "all") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1)
}

style_ft_plain <- function(ft, size = 8) {
  ft |>
    theme_booktabs() |>
    font(fontname = "Arial", part = "all") |>
    fontsize(size = 6, part = "all") |>
    align(align = "center", part = "all") |>
    align(j = 1, align = "left", part = "all") |>
    padding(padding = 2, part = "all") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1)
}

style_ft_methods <- function(ft) {
  ft |>
    theme_booktabs() |>
    font(fontname = "Arial", part = "all") |>
    fontsize(size = 6, part = "all") |>
    align(align = "left", part = "all") |>
    padding(padding = 2, part = "all") |>
    autofit() |>
    set_table_properties(layout = "autofit", width = 1)
}

build_annex_ft <- function(dt) {
  x <- sanitize_dt_text(dt)
  ft <- flextable(x)
  ft <- style_ft_plain(ft, size = 6)

  if ("CategorÃ­a / causa" %in% names(x)) {
    ft <- align(ft, j = "CategorÃ­a / causa", align = "left", part = "all")
    for (lvl in sort(unique(x$indent_level))) {
      rows_i <- which(x$indent_level == lvl)
      if (length(rows_i) > 0) {
        ft <- padding(ft, i = rows_i, j = "CategorÃ­a / causa", padding.left = 6 + (lvl - 1L) * 16, part = "body")
      }
    }
  }

  ft
}

add_note <- function(doc, text) {
  para <- fpar(
    ftext(txt(text), prop = fp_text(font.family = "Arial", font.size = 4))
  )
  doc |>
    body_add_fpar(para)
}

add_ft_via_docx <- function(doc, title, ft, note = NULL) {
  tmp <- tempfile(fileext = ".docx")
  subdoc <- read_docx() |>
    body_add_flextable(sanitize_ft(ft))
  print(subdoc, target = tmp)
  doc <- doc |>
    body_add_par_utf8(title, style = "heading 3") |>
    body_add_docx(src = tmp)
  if (!is.null(note) && nzchar(note)) {
    doc <- add_note(doc, note)
  }
  doc
}

add_native_table_title <- function(doc, title, dt, note = NULL) {
  doc <- doc |>
    body_add_par_utf8(title, style = "heading 3") |>
    body_add_table(value = as.data.frame(sanitize_dt_text(dt)), style = "table_template")
  if (!is.null(note) && nzchar(note)) {
    doc <- add_note(doc, note)
  }
  doc
}

add_ft_title <- function(doc, title, ft, landscape = FALSE, note = NULL) {
  doc <- doc |>
    body_add_par_utf8(title, style = "heading 3") |>
    body_add_flextable(sanitize_ft(ft))
  if (!is.null(note) && nzchar(note)) {
    doc <- add_note(doc, note)
  }
  doc
}

add_plot_title <- function(doc, title, img_path, width = 6.8, height = 4.5) {
  doc |>
    body_add_par_utf8(title, style = "heading 3") |>
    body_add_img(src = img_path, width = width, height = height)
}

save_table_image <- function(dt, file, font_size = 8) {
  x <- as.data.frame(copy(dt))
  wrapped_names <- vapply(names(x), function(nm) paste(strwrap(nm, width = 18), collapse = "\n"), character(1))
  names(x) <- wrapped_names
  tg <- gridExtra::tableGrob(
    x,
    rows = NULL,
    theme = gridExtra::ttheme_minimal(
      base_size = font_size,
      core = list(fg_params = list(fontsize = font_size)),
      colhead = list(fg_params = list(fontsize = font_size, fontface = "bold"))
    )
  )
  width_in <- min(max(6.5, ncol(x) * 1.15), 12)
  height_in <- min(max(1.8, 0.42 * (nrow(x) + 1)), 14)
  png(filename = file, width = width_in, height = height_in, units = "in", res = 220, bg = "white")
  grid::grid.newpage()
  grid::grid.draw(tg)
  dev.off()
  list(path = file, width = width_in, height = height_in)
}

add_table_image <- function(doc, title, dt, file, note = NULL, font_size = 8) {
  cat("Renderizando tabla como imagen: ", title, "\n", sep = "")
  img <- tryCatch(
    save_table_image(dt, file = file, font_size = font_size),
    error = function(e) {
      stop("Fallo al renderizar tabla '", title, "': ", conditionMessage(e), call. = FALSE)
    }
  )
  doc <- doc |>
    body_add_par_utf8(title, style = "heading 3") |>
    body_add_img(src = img$path, width = img$width, height = img$height)
  if (!is.null(note) && nzchar(note)) {
    doc <- add_note(doc, note)
  }
  doc
}

build_annex_ft <- function(dt) {
  x <- sanitize_dt_text(dt)
  ft <- flextable(x)
  ft <- style_ft_plain(ft, size = 6)

  col_cat <- "Categoría / causa"
  if (col_cat %in% names(x)) {
    ft <- align(ft, j = col_cat, align = "left", part = "all")
    for (lvl in sort(unique(x$indent_level))) {
      rows_i <- which(x$indent_level == lvl)
      if (length(rows_i) > 0) {
        ft <- padding(ft, i = rows_i, j = col_cat, padding.left = 6 + (lvl - 1L) * 16, part = "body")
      }
    }
  }

  ft
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
  theme_minimal(base_family = "Arial", base_size = 10) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      axis.title = element_text(size = 9),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 7),
      panel.grid.minor = element_blank(),
      legend.position = "bottom"
    )
}

wrap_label <- function(x, width = 22) {
  vapply(as.character(x), function(z) paste(strwrap(z, width = width), collapse = "\n"), character(1))
}

pick_first_col <- function(dt, candidates, label) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    stop("No encontre columna para ", label, ". Candidatas: ", paste(candidates, collapse = ", "))
  }
  hit[1]
}

add_location_label <- function(dt) {
  x <- copy(dt)
  if ("location_name" %in% names(x)) {
    x[, location_label_resolved := as.character(location_name)]
  } else if (!"location_label_resolved" %in% names(x)) {
    x[, location_label_resolved := fifelse(
      !is.na(location_id),
      fifelse(location_id == 9000L, "Nacional", sprintf("Departamento %02d", as.integer(location_id))),
      NA_character_
    )]
  }
  x
}

pick_metric_col <- function(dt, preferred, alternatives = character()) {
  candidates <- c(preferred, alternatives)
  hit <- candidates[candidates %in% names(dt)][1]
  if (length(hit) == 0L || is.na(hit)) {
    stop("No encontre metrica valida. Probe: ", paste(candidates, collapse = ", "))
  }
  hit
}

format_table_values <- function(dt, rate_cols = character(), abs_cols = character(), pct_cols = character()) {
  out <- copy(dt)
  if ("ranking" %in% names(out)) out[, ranking := as.integer(ranking)]
  for (j in intersect(rate_cols, names(out))) out[, (j) := fmt_num(get(j), 1)]
  for (j in intersect(abs_cols, names(out))) out[, (j) := fmt_int(get(j))]
  for (j in intersect(pct_cols, names(out))) out[, (j) := fmt_num(get(j), 1)]
  out
}

level_label <- function(level) {
  if (level == 0L) return("Total")
  paste("Nivel", level)
}

level_scope_note <- function(level) {
  if (level == 3L) return("Incluye categorÃ­as agrupadas y terminales de nivel 3.")
  if (level == 4L) return("Incluye causas terminales de nivel 3 y todas las causas terminales de nivel 4.")
  NULL
}

metric_label <- function(metric) {
  if (identical(metric, "mort")) return("mortalidad")
  "AVP"
}

needs_landscape <- function(dt) {
  ncol(dt) >= 10L || nrow(dt) >= 40L
}

rate_note_text <- "Nota: todas las tasas presentadas en este informe se expresan por 100 000 habitantes."

# ============================================================
# Cause master
# ============================================================

read_cause_master <- function() {
  candidates <- c(
    here("data", "final", "cause_master", "cause_master.csv"),
    here("data", "final", "cause_master", "cause_master.parquet")
  )
  hit <- find_first_existing(candidates)
  if (is.na(hit)) stop("No encontre cause_master en data/final/cause_master/")
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
  all_age_label = "Todas las edades",
  report_levels = 0:4,
  top_n_rank = Inf,
  top_n_trend = Inf,
  top_n_regional_heat = Inf,
  top_n_rank_plot = 10L,
  top_n_trend_plot = 10L,
  top_n_regional_heat_plot = 10L,
  out_dir = here("reports", "all_causes_word_report"),
  fig_dir = here("reports", "all_causes_word_report", "figures"),
  out_file = {
    forced_out <- Sys.getenv("REPORT_OUT_FILE", unset = "")
    if (nzchar(forced_out)) forced_out else here("reports", "all_causes_word_report", "informe_mortalidad_avp_todas_las_causas.docx")
  }
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$fig_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Inputs
# ============================================================

tbl_nat_total_mort <- read_table_candidate("tbl_nat_year_total_mort")
tbl_nat_total_avp  <- read_table_candidate("tbl_nat_year_total_avp")
tbl_nat_sex_mort   <- read_table_candidate("tbl_nat_year_sex_mort")
tbl_nat_sex_avp    <- read_table_candidate("tbl_nat_year_sex_avp")
tbl_reg_total_mort <- read_table_candidate("tbl_reg_year_total_mort")
tbl_reg_total_avp  <- read_table_candidate("tbl_reg_year_total_avp")
tbl_reg_sex_mort   <- read_table_candidate("tbl_reg_year_sex_mort")
tbl_reg_sex_avp    <- read_table_candidate("tbl_reg_year_sex_avp")
tbl_nat_level2_mort <- read_table_candidate("tbl_nat_year_level2_mort")
tbl_nat_level2_avp  <- read_table_candidate("tbl_nat_year_level2_avp")
tbl_reg_level2_mort <- read_table_candidate("tbl_reg_year_level2_mort")
tbl_reg_level2_avp  <- read_table_candidate("tbl_reg_year_level2_avp")
tbl_cause_hierarchy_catalog <- read_table_candidate("tbl_cause_hierarchy_catalog")
tbl_methods_demog_incompat <- read_methods_candidate("direct_demographic_incompatibility_handling_summary")
tbl_methods_direct_specific <- read_methods_candidate("direct_specific_icd_handling_summary")
tbl_methods_sensitive_cases <- read_methods_candidate("sensitive_methodological_positions_review")
cause_master <- read_cause_master()

# ============================================================
# Garantizar nombres y niveles
# ============================================================

cm_lookup <- unique(cause_master[, .(cause_concept_id, cause_name, cause_level)])

ensure_cause_cols <- function(dt) {
  x <- copy(dt)

  if (!"cause_name" %in% names(x) || !"cause_level" %in% names(x)) {
    x <- merge(x, cm_lookup, by = "cause_concept_id", all.x = TRUE)
  } else {
    miss_name <- is.na(x$cause_name) | trimws(as.character(x$cause_name)) == ""
    miss_lvl  <- is.na(x$cause_level)
    if (any(miss_name) || any(miss_lvl)) {
      x <- merge(x, cm_lookup, by = "cause_concept_id", all.x = TRUE, suffixes = c("", "_cm"))
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
  x <- x[cause_level %in% CFG$report_levels]
  x
}

tbl_nat_total_mort <- ensure_cause_cols(tbl_nat_total_mort)
tbl_nat_total_avp  <- ensure_cause_cols(tbl_nat_total_avp)
tbl_nat_sex_mort   <- ensure_cause_cols(tbl_nat_sex_mort)
tbl_nat_sex_avp    <- ensure_cause_cols(tbl_nat_sex_avp)
tbl_reg_total_mort <- ensure_cause_cols(tbl_reg_total_mort)
tbl_reg_total_avp  <- ensure_cause_cols(tbl_reg_total_avp)
tbl_reg_sex_mort   <- ensure_cause_cols(tbl_reg_sex_mort)
tbl_reg_sex_avp    <- ensure_cause_cols(tbl_reg_sex_avp)
tbl_nat_level2_mort <- ensure_cause_cols(tbl_nat_level2_mort)
tbl_nat_level2_avp  <- ensure_cause_cols(tbl_nat_level2_avp)
tbl_reg_level2_mort <- ensure_cause_cols(tbl_reg_level2_mort)
tbl_reg_level2_avp  <- ensure_cause_cols(tbl_reg_level2_avp)
tbl_reg_total_mort <- add_location_label(tbl_reg_total_mort)
tbl_reg_total_avp  <- add_location_label(tbl_reg_total_avp)
tbl_reg_sex_mort   <- add_location_label(tbl_reg_sex_mort)
tbl_reg_sex_avp    <- add_location_label(tbl_reg_sex_avp)
tbl_reg_level2_mort <- add_location_label(tbl_reg_level2_mort)
tbl_reg_level2_avp  <- add_location_label(tbl_reg_level2_avp)

# ============================================================
# Resolver columnas clave
# ============================================================

mort_rate_col_nat <- pick_metric_col(
  tbl_nat_total_mort,
  preferred = "metric_rate",
  alternatives = c("rate", "mortality_rate", "rate_per_100k", "value")
)

mort_abs_col_nat <- pick_metric_col(
  tbl_nat_total_mort,
  preferred = "metric_abs",
  alternatives = c("deaths", "count", "value_abs", "value")
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

mort_abs_col_reg <- pick_metric_col(
  tbl_reg_total_mort,
  preferred = "metric_abs",
  alternatives = c("deaths", "count", "value_abs", "value")
)

avp_rate_col_reg <- pick_metric_col(
  tbl_reg_total_avp,
  preferred = "metric_rate",
  alternatives = c("rate", "avp_rate", "yll_rate", "rate_per_100k")
)

avp_abs_col_reg <- pick_metric_col(
  tbl_reg_total_avp,
  preferred = "metric_abs",
  alternatives = c("avp", "yll", "value", "count")
)

loc_name_col_reg_mort <- pick_first_col(
  tbl_reg_total_mort,
  c("location_name", "location_label", "region_name", "departamento", "department_name", "location_label_resolved", "location_id"),
  "nombre de region en mortalidad regional"
)

loc_name_col_reg_avp <- pick_first_col(
  tbl_reg_total_avp,
  c("location_name", "location_label", "region_name", "departamento", "department_name", "location_label_resolved", "location_id"),
  "nombre de region en AVP regional"
)

# ============================================================
# Helpers de tablas
# ============================================================

make_total_trend_table <- function(dt, abs_col, rate_col, year = CFG$years) {
  out <- copy(
    dt[year_id %in% year & cause_level == 0L,
       .(year_id, absoluto = get(abs_col), tasa = get(rate_col))]
  )
  setorder(out, year_id)
  out
}

make_total_trend_table_by_sex <- function(dt, year = CFG$years, abs_label, rate_label) {
  x <- copy(
    dt[
      year_id %in% year &
        cause_level == 0L &
        as.character(age_group) == CFG$all_age_label &
        sex_label %in% c("Ambos", "Hombre", "Mujer"),
      .(year_id, sex_label, metric_abs, metric_rate)
    ]
  )
  abs_wide <- dcast(x, year_id ~ sex_label, value.var = "metric_abs", fill = 0)
  rate_wide <- dcast(x, year_id ~ sex_label, value.var = "metric_rate", fill = 0)
  out <- merge(abs_wide, rate_wide, by = "year_id", suffixes = c("_abs", "_rate"), sort = TRUE)
  setnames(
    out,
    c("Ambos_abs", "Hombre_abs", "Mujer_abs", "Ambos_rate", "Hombre_rate", "Mujer_rate"),
    c(
      paste0(abs_label, "_ambos"),
      paste0(abs_label, "_hombre"),
      paste0(abs_label, "_mujer"),
      paste0(rate_label, "_ambos_100000"),
      paste0(rate_label, "_hombre_100000"),
      paste0(rate_label, "_mujer_100000")
    ),
    skip_absent = TRUE
  )
  out
}

make_rank_table <- function(dt, level, value_col, year = CFG$editorial_year, top_n = CFG$top_n_rank) {
  out <- copy(
    dt[year_id == year & cause_level == level,
       .(cause_name, valor = get(value_col))]
  )
  out <- out[!is.na(cause_name)]
  out <- out[order(-valor, cause_name)]
  if (is.finite(top_n)) {
    out <- out[1:min(top_n, .N)]
  }
  out[, ranking := .I]
  setcolorder(out, c("ranking", "cause_name", "valor"))
  out
}

filter_level_view <- function(dt, level, include_terminal_l3_for_level4 = FALSE) {
  x <- copy(dt)
  if (!"is_terminal" %in% names(x) && "cause_concept_id" %in% names(x)) {
    x <- merge(
      x,
      unique(cause_master[, .(cause_concept_id, is_terminal)]),
      by = "cause_concept_id",
      all.x = TRUE
    )
  }
  if (level == 4L && include_terminal_l3_for_level4) {
    return(x[cause_level == 4L | (cause_level == 3L & is_terminal == TRUE)])
  }
  x[cause_level == level]
}

make_rank_table_by_sex <- function(dt, level, value_col, year = CFG$editorial_year, top_n = CFG$top_n_rank, value_prefix = "valor") {
  base_dt <- filter_level_view(dt, level, include_terminal_l3_for_level4 = (level == 4L))
  x <- copy(
    base_dt[
      year_id == year &
        as.character(age_group) == CFG$all_age_label &
        sex_label %in% c("Ambos", "Hombre", "Mujer"),
      .(cause_name, sex_label, valor = get(value_col))
    ]
  )
  wide <- dcast(x, cause_name ~ sex_label, value.var = "valor", fill = 0)
  if (!"Ambos" %in% names(wide)) wide[, Ambos := 0]
  if (!"Hombre" %in% names(wide)) wide[, Hombre := 0]
  if (!"Mujer" %in% names(wide)) wide[, Mujer := 0]
  setorder(wide, -Ambos, cause_name)
  if (is.finite(top_n)) {
    wide <- wide[1:min(top_n, .N)]
  }
  wide[, ranking := .I]
  setnames(
    wide,
    c("Ambos", "Hombre", "Mujer"),
    c(
      paste0(value_prefix, "_ambos"),
      paste0(value_prefix, "_hombre"),
      paste0(value_prefix, "_mujer")
    )
  )
  setcolorder(
    wide,
    c("ranking", "cause_name", paste0(value_prefix, "_ambos"), paste0(value_prefix, "_hombre"), paste0(value_prefix, "_mujer"))
  )
  wide
}

make_trend_table <- function(dt, level, value_col, top_n = CFG$top_n_trend, year_ref = CFG$editorial_year) {
  base_dt <- filter_level_view(dt, level, include_terminal_l3_for_level4 = (level == 4L))
  keep_causes <- base_dt[
    year_id == year_ref,
    .(valor_ref = get(value_col)),
    by = cause_name
  ][order(-valor_ref)]
  if (is.finite(top_n)) {
    keep_causes <- keep_causes[1:min(top_n, .N), cause_name]
  } else {
    keep_causes <- keep_causes[, cause_name]
  }

  long <- copy(
    base_dt[cause_name %in% keep_causes,
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
  wide
}

rename_trend_table_columns <- function(dt, metric_kind = c("mort", "avp")) {
  metric_kind <- match.arg(metric_kind)
  out <- copy(dt)
  year_cols <- intersect(as.character(CFG$years), names(out))
  if (metric_kind == "mort") {
    setnames(out, year_cols, paste0("Tasa ", year_cols, " (por 100 000 hab.)"), skip_absent = TRUE)
  } else {
    setnames(out, year_cols, paste0("AVP ", year_cols, " (N)"), skip_absent = TRUE)
  }
  setnames(
    out,
    c("cambio_abs_2018_2024", "cambio_pct_2018_2024"),
    c("Cambio absoluto 2018-2024", "Cambio porcentual 2018-2024"),
    skip_absent = TRUE
  )
  out
}

make_region_total_table <- function(dt, abs_col, rate_col, location_col, year = CFG$regional_year) {
  out <- copy(
    dt[year_id == year & cause_level == 0L,
       .(region = as.character(get(location_col)), absoluto = get(abs_col), tasa = get(rate_col))]
  )
  out <- out[!is.na(region)]
  out <- out[order(-tasa, region)]
  out[, ranking := .I]
  setcolorder(out, c("ranking", "region", "absoluto", "tasa"))
  out
}

make_region_total_table_by_sex <- function(dt, location_col, year = CFG$regional_year, abs_label, rate_label) {
  x <- copy(
    dt[
      year_id == year &
        cause_level == 0L &
        as.character(age_group) == CFG$all_age_label &
        sex_label %in% c("Ambos", "Hombre", "Mujer"),
      .(region = as.character(get(location_col)), sex_label, metric_abs, metric_rate)
    ]
  )
  abs_wide <- dcast(x, region ~ sex_label, value.var = "metric_abs", fill = 0)
  rate_wide <- dcast(x, region ~ sex_label, value.var = "metric_rate", fill = 0)
  out <- merge(abs_wide, rate_wide, by = "region", suffixes = c("_abs", "_rate"), sort = FALSE)
  if (!"Ambos_abs" %in% names(out)) out[, Ambos_abs := 0]
  if (!"Hombre_abs" %in% names(out)) out[, Hombre_abs := 0]
  if (!"Mujer_abs" %in% names(out)) out[, Mujer_abs := 0]
  if (!"Ambos_rate" %in% names(out)) out[, Ambos_rate := 0]
  if (!"Hombre_rate" %in% names(out)) out[, Hombre_rate := 0]
  if (!"Mujer_rate" %in% names(out)) out[, Mujer_rate := 0]
  out <- out[order(-Ambos_rate, region)]
  out[, ranking := .I]
  setnames(
    out,
    c("Ambos_abs", "Hombre_abs", "Mujer_abs", "Ambos_rate", "Hombre_rate", "Mujer_rate"),
    c(
      paste0(abs_label, "_ambos"),
      paste0(abs_label, "_hombre"),
      paste0(abs_label, "_mujer"),
      paste0(rate_label, "_ambos_100000"),
      paste0(rate_label, "_hombre_100000"),
      paste0(rate_label, "_mujer_100000")
    )
  )
  setcolorder(
    out,
    c("ranking", "region",
      paste0(abs_label, "_ambos"), paste0(abs_label, "_hombre"), paste0(abs_label, "_mujer"),
      paste0(rate_label, "_ambos_100000"), paste0(rate_label, "_hombre_100000"), paste0(rate_label, "_mujer_100000"))
  )
  out
}

make_heatmap_dt <- function(dt, level, value_col, location_col, year = CFG$regional_year, top_n = CFG$top_n_regional_heat) {
  base_dt <- filter_level_view(dt, level, include_terminal_l3_for_level4 = (level == 4L))
  keep_causes_dt <- base_dt[
    year_id == year,
    .(valor_nat = sum(get(value_col), na.rm = TRUE)),
    by = cause_name
  ][order(-valor_nat)]
  if (is.finite(top_n)) {
    cause_order <- keep_causes_dt[1:min(top_n, .N), cause_name]
  } else {
    cause_order <- keep_causes_dt[, cause_name]
  }

  out <- copy(
    base_dt[year_id == year & cause_name %in% cause_order,
       .(region = as.character(get(location_col)), cause_name, valor = get(value_col))]
  )
  out <- out[!is.na(region) & !is.na(cause_name)]
  region_order <- out[, .(valor_total = sum(valor, na.rm = TRUE)), by = region][order(-valor_total), region]
  out[, cause_name := factor(cause_name, levels = cause_order)]
  out[, region := factor(region, levels = rev(region_order))]
  out
}

level_rank_limit <- function(level, default_top_n) {
  Inf
}

rank_subtitle <- function(level, metric_type = c("mort", "avp")) {
  metric_type <- match.arg(metric_type)
  if (metric_type == "mort") return(paste("Top 10 de", tolower(level_label(level)), "por tasa por 100 000 habitantes"))
  paste("Top 10 de", tolower(level_label(level)), "por AVP estimados")
}

trend_subtitle <- function(level) {
  paste("Top 10 de", tolower(level_label(level)), "segÃºn el valor observado en 2024")
}

heat_subtitle <- function(level) {
  paste("Top 10 de", tolower(level_label(level)), "ordenado de mayor a menor carga regional en 2024")
}

# ============================================================
# Tablas nacionales y regionales
# ============================================================

total_mort_nat <- format_table_values(
  make_total_trend_table_by_sex(tbl_nat_sex_mort, year = CFG$years, abs_label = "n_muertes", rate_label = "tasa"),
  rate_cols = c("tasa_ambos_100000", "tasa_hombre_100000", "tasa_mujer_100000"),
  abs_cols = c("n_muertes_ambos", "n_muertes_hombre", "n_muertes_mujer")
)
setnames(total_mort_nat, "year_id", "anio")

total_avp_nat <- format_table_values(
  make_total_trend_table_by_sex(tbl_nat_sex_avp, year = CFG$years, abs_label = "avp", rate_label = "tasa_avp"),
  rate_cols = c("tasa_avp_ambos_100000", "tasa_avp_hombre_100000", "tasa_avp_mujer_100000"),
  abs_cols = c("avp_ambos", "avp_hombre", "avp_mujer")
)
setnames(total_avp_nat, "year_id", "anio")

total_mort_reg <- format_table_values(
  make_region_total_table_by_sex(tbl_reg_sex_mort, loc_name_col_reg_mort, year = CFG$regional_year, abs_label = "n_muertes", rate_label = "tasa"),
  rate_cols = c("tasa_ambos_100000", "tasa_hombre_100000", "tasa_mujer_100000"),
  abs_cols = c("n_muertes_ambos", "n_muertes_hombre", "n_muertes_mujer")
)

total_avp_reg <- format_table_values(
  make_region_total_table_by_sex(tbl_reg_sex_avp, loc_name_col_reg_avp, year = CFG$regional_year, abs_label = "avp", rate_label = "tasa_avp"),
  rate_cols = c("tasa_avp_ambos_100000", "tasa_avp_hombre_100000", "tasa_avp_mujer_100000"),
  abs_cols = c("avp_ambos", "avp_hombre", "avp_mujer")
)

pretty_total_mort_nat <- copy(total_mort_nat)
setnames(pretty_total_mort_nat,
         c("anio", "n_muertes_ambos", "n_muertes_hombre", "n_muertes_mujer", "tasa_ambos_100000", "tasa_hombre_100000", "tasa_mujer_100000"),
         c("Año", "N muertes estimadas (ambos)", "N muertes estimadas (hombres)", "N muertes estimadas (mujeres)", "Tasa ambos (por 100 000 hab.)", "Tasa hombres (por 100 000 hab.)", "Tasa mujeres (por 100 000 hab.)"),
         skip_absent = TRUE)

pretty_total_avp_nat <- copy(total_avp_nat)
setnames(pretty_total_avp_nat,
         c("anio", "avp_ambos", "avp_hombre", "avp_mujer", "tasa_avp_ambos_100000", "tasa_avp_hombre_100000", "tasa_avp_mujer_100000"),
         c("Año", "AVP estimados (ambos)", "AVP estimados (hombres)", "AVP estimados (mujeres)", "Tasa AVP ambos (por 100 000 hab.)", "Tasa AVP hombres (por 100 000 hab.)", "Tasa AVP mujeres (por 100 000 hab.)"),
         skip_absent = TRUE)

pretty_total_mort_reg <- copy(total_mort_reg)
setnames(pretty_total_mort_reg,
         c("ranking", "region", "n_muertes_ambos", "n_muertes_hombre", "n_muertes_mujer", "tasa_ambos_100000", "tasa_hombre_100000", "tasa_mujer_100000"),
         c("Ranking", "Departamento", "N muertes estimadas (ambos)", "N muertes estimadas (hombres)", "N muertes estimadas (mujeres)", "Tasa ambos (por 100 000 hab.)", "Tasa hombres (por 100 000 hab.)", "Tasa mujeres (por 100 000 hab.)"),
         skip_absent = TRUE)

pretty_total_avp_reg <- copy(total_avp_reg)
setnames(pretty_total_avp_reg,
         c("ranking", "region", "avp_ambos", "avp_hombre", "avp_mujer", "tasa_avp_ambos_100000", "tasa_avp_hombre_100000", "tasa_avp_mujer_100000"),
         c("Ranking", "Departamento", "AVP estimados (ambos)", "AVP estimados (hombres)", "AVP estimados (mujeres)", "Tasa AVP ambos (por 100 000 hab.)", "Tasa AVP hombres (por 100 000 hab.)", "Tasa AVP mujeres (por 100 000 hab.)"),
         skip_absent = TRUE)

annex_cause_catalog <- copy(
  tbl_cause_hierarchy_catalog[cause_level %in% 1:4][order(catalog_order)]
)
annex_cause_catalog[, `:=`(
  indent_level = as.integer(cause_level),
  nivel_jerarquico = fifelse(
    cause_level == 1L, as.character(level_1_name),
    fifelse(
      cause_level == 2L, as.character(level_2_name),
      fifelse(
        cause_level == 3L, as.character(level_3_name),
        as.character(level_4_name)
      )
    )
  ),
  cie10_usados = fifelse(
    !is.na(cie10_cobertura_resumida) & nzchar(cie10_cobertura_resumida),
    as.character(cie10_cobertura_resumida),
    fifelse(!is.na(cie10_cobertura_utilizada), vapply(cie10_cobertura_utilizada, compress_icd10_string, character(1)), "")
  ),
  uso_mortalidad = fifelse(as.logical(is_mortality_cause_category), "SÃ­", "No"),
  uso_avp = fifelse(as.logical(is_avp_eligible_cause), "SÃ­", "No")
)]
annex_cause_catalog <- annex_cause_catalog[, .(
  indent_level,
  `CategorÃ­a / causa` = nivel_jerarquico,
  cie10_usados,
  uso_mortalidad,
  uso_avp,
  nota_especial
)]
setnames(
  annex_cause_catalog,
  c("cie10_usados", "uso_mortalidad", "uso_avp", "nota_especial"),
  c("CÃ³digos CIE-10 usados", "Se usa para mortalidad", "Se usa para AVP", "Nota especial")
)

annex_demog_incompat <- copy(tbl_methods_demog_incompat)
annex_demog_incompat[, tipo_incompatibilidad := fifelse(
  direct_demographic_incompatibility_type == "sex_incompatible",
  "Sexo incompatible",
  fifelse(direct_demographic_incompatibility_type == "age_incompatible", "Edad incompatible", txt(as.character(direct_demographic_incompatibility_type)))
)]
annex_demog_incompat[, manejo_aplicado := fifelse(
  handling_label == "reclassify_to_all_diseases_pool",
  "Reclasificado al pool general de causas para redistribuciÃ³n",
  fifelse(handling_label == "reclassify_to_cancer_pool", "Reclasificado al pool de cÃ¡nceres no especificados para redistribuciÃ³n", txt(as.character(handling_label)))
)]
annex_demog_incompat <- annex_demog_incompat[, .(
  `Tipo de incompatibilidad` = tipo_incompatibilidad,
  `Causa original` = original_cause_name,
  `Regla de reemplazo` = replacement_source_group_name,
  `Manejo aplicado` = manejo_aplicado,
  `N muertes` = fmt_int(as.numeric(deaths)),
  `N registros` = fmt_int(as.numeric(rows))
)]

annex_direct_specific <- copy(tbl_methods_direct_specific)
annex_direct_specific[, tipo_manejo := fifelse(
  handling_type == "dynamic_year_split",
  "RedistribuciÃ³n dinÃ¡mica por aÃ±o",
  fifelse(handling_type == "replace_with_rule", "SustituciÃ³n por regla de redistribuciÃ³n", txt(as.character(handling_type)))
)]
annex_direct_specific <- annex_direct_specific[, .(
  `Patch / caso` = patch_id,
  `CÃ³digo CIE-10` = icd10_ucod_nodot,
  `Causa original` = original_cause_name,
  `Tipo de manejo` = tipo_manejo,
  `Detalle del manejo` = specific_handling_label,
  `Regla de reemplazo` = replacement_source_group_name,
  `N muertes` = fmt_int(as.numeric(deaths)),
  `N registros` = fmt_int(as.numeric(rows))
)]

annex_sensitive_cases <- copy(tbl_methods_sensitive_cases)
annex_sensitive_cases <- annex_sensitive_cases[, .(
  `Caso` = short_label,
  `CÃ³digos observados` = observed_codes,
  `N muertes observadas` = fmt_int(as.numeric(n_deaths_observed)),
  `Postura OMS` = who_position,
  `Postura Australia` = australia_position,
  `Postura del proyecto` = project_position_preferred,
  `RecomendaciÃ³n` = recommendation,
  `Estado de cambio core` = core_change_status,
  `Cambio de redistribuciÃ³n` = redistribution_change_status,
  `JustificaciÃ³n` = rationale
)]

catalog_counts <- tbl_cause_hierarchy_catalog[
  cause_level %in% 1:4,
  .(
    n_categorias = .N,
    n_causas_enfermedad = sum(is_mortality_cause_category %in% c(TRUE, "SÃ­"), na.rm = TRUE)
  ),
  by = .(cause_level)
][order(cause_level)]

catalog_summary_text <- paste(
  vapply(
    seq_len(nrow(catalog_counts)),
    function(i) {
      paste0(
        "nivel ", catalog_counts$cause_level[i], ": ",
        catalog_counts$n_categorias[i], " categorÃ­as (",
        catalog_counts$n_causas_enfermedad[i], " de enfermedad del catÃ¡logo base)"
      )
    },
    character(1)
  ),
  collapse = "; "
)

levels_core <- 1:4
level_tables <- vector("list", length(levels_core))
names(level_tables) <- paste0("L", levels_core)

for (lvl in levels_core) {
  rank_dt_mort <- tbl_nat_total_mort
  rank_dt_avp  <- tbl_nat_total_avp
  rank_dt_mort_sex <- tbl_nat_sex_mort
  rank_dt_avp_sex  <- tbl_nat_sex_avp
  reg_dt_mort  <- if (lvl == 2L) tbl_reg_level2_mort else tbl_reg_total_mort
  reg_dt_avp   <- if (lvl == 2L) tbl_reg_level2_avp else tbl_reg_total_avp
  top_n_rank_this <- level_rank_limit(lvl, CFG$top_n_rank)
  top_n_trend_this <- level_rank_limit(lvl, CFG$top_n_trend)
  top_n_heat_this <- level_rank_limit(lvl, CFG$top_n_regional_heat)
  
  level_tables[[paste0("L", lvl)]] <- list(
    mort_rank = format_table_values(
      make_rank_table_by_sex(rank_dt_mort_sex, lvl, "metric_rate", top_n = top_n_rank_this, value_prefix = "tasa"),
      rate_cols = c("tasa_ambos", "tasa_hombre", "tasa_mujer")
    ),
    mort_trend = rename_trend_table_columns(format_table_values(
      make_trend_table(rank_dt_mort, lvl, mort_rate_col_nat, top_n = top_n_trend_this),
      rate_cols = as.character(CFG$years),
      pct_cols = "cambio_pct_2018_2024",
      abs_cols = "cambio_abs_2018_2024"
    ), metric_kind = "mort"),
    avp_rank = format_table_values(
      make_rank_table_by_sex(rank_dt_avp_sex, lvl, "metric_abs", top_n = top_n_rank_this, value_prefix = "avp"),
      abs_cols = c("avp_ambos", "avp_hombre", "avp_mujer")
    ),
    avp_trend = rename_trend_table_columns(format_table_values(
      make_trend_table(rank_dt_avp, lvl, avp_abs_col_nat, top_n = top_n_trend_this),
      abs_cols = c(as.character(CFG$years), "cambio_abs_2018_2024"),
      pct_cols = "cambio_pct_2018_2024"
    ), metric_kind = "avp")
  )
}

pretty_mort_rank <- function(dt) {
  out <- copy(dt)
  setnames(out,
           c("ranking", "cause_name", "tasa_ambos", "tasa_hombre", "tasa_mujer"),
           c("Ranking", "Causa", "Tasa ambos (por 100 000 hab.)", "Tasa hombres (por 100 000 hab.)", "Tasa mujeres (por 100 000 hab.)"),
           skip_absent = TRUE)
  out
}

pretty_avp_rank <- function(dt) {
  out <- copy(dt)
  setnames(out,
           c("ranking", "cause_name", "avp_ambos", "avp_hombre", "avp_mujer"),
           c("Ranking", "Causa", "AVP estimados (ambos)", "AVP estimados (hombres)", "AVP estimados (mujeres)"),
           skip_absent = TRUE)
  out
}

# ============================================================
# Datos de figuras
# ============================================================

total_mort_nat_plot <- make_total_trend_table(tbl_nat_total_mort, mort_abs_col_nat, mort_rate_col_nat)
total_avp_nat_plot  <- make_total_trend_table(tbl_nat_total_avp, avp_abs_col_nat, avp_rate_col_nat)
total_mort_reg_plot <- make_region_total_table(tbl_reg_total_mort, mort_abs_col_reg, mort_rate_col_reg, loc_name_col_reg_mort)
total_avp_reg_plot  <- make_region_total_table(tbl_reg_total_avp, avp_abs_col_reg, avp_rate_col_reg, loc_name_col_reg_avp)

make_rank_plot_dt <- function(dt, level, value_col, top_n = CFG$top_n_rank) {
  base_dt <- filter_level_view(dt, level, include_terminal_l3_for_level4 = (level == 4L))
  out <- copy(
    base_dt[year_id == CFG$editorial_year,
       .(cause_name, valor = get(value_col))]
  )
  out <- out[order(-valor, cause_name)]
  if (is.finite(top_n)) {
    out <- out[1:min(top_n, .N)]
  }
  out[order(valor, cause_name)]
}

make_trend_plot_dt <- function(dt, level, value_col, top_n = CFG$top_n_trend) {
  base_dt <- filter_level_view(dt, level, include_terminal_l3_for_level4 = (level == 4L))
  keep <- base_dt[
    year_id == CFG$editorial_year,
    .(valor_ref = get(value_col)),
    by = cause_name
  ][order(-valor_ref)]
  if (is.finite(top_n)) {
    keep <- keep[1:min(top_n, .N), cause_name]
  } else {
    keep <- keep[, cause_name]
  }

  out <- copy(
    base_dt[cause_name %in% keep,
       .(cause_name, year_id, valor = get(value_col))]
  )
  setorder(out, cause_name, year_id)
  out
}

fig_rank_mort <- setNames(vector("list", length(levels_core)), paste0("L", levels_core))
fig_rank_avp  <- setNames(vector("list", length(levels_core)), paste0("L", levels_core))
fig_trend_mort <- setNames(vector("list", length(levels_core)), paste0("L", levels_core))
fig_trend_avp  <- setNames(vector("list", length(levels_core)), paste0("L", levels_core))
heat_mort <- setNames(vector("list", length(levels_core)), paste0("L", levels_core))
heat_avp  <- setNames(vector("list", length(levels_core)), paste0("L", levels_core))

for (lvl in levels_core) {
  key <- paste0("L", lvl)
  rank_dt_mort <- if (lvl == 2L) tbl_nat_level2_mort else tbl_nat_total_mort
  rank_dt_avp  <- if (lvl == 2L) tbl_nat_level2_avp else tbl_nat_total_avp
  reg_dt_mort  <- if (lvl == 2L) tbl_reg_level2_mort else tbl_reg_total_mort
  reg_dt_avp   <- if (lvl == 2L) tbl_reg_level2_avp else tbl_reg_total_avp
  top_n_rank_this <- CFG$top_n_rank_plot
  top_n_trend_this <- CFG$top_n_trend_plot
  top_n_heat_this <- CFG$top_n_regional_heat_plot
  
  fig_rank_mort[[key]] <- make_rank_plot_dt(rank_dt_mort, lvl, mort_rate_col_nat, top_n = top_n_rank_this)
  fig_rank_avp[[key]]  <- make_rank_plot_dt(rank_dt_avp, lvl, avp_abs_col_nat, top_n = top_n_rank_this)
  fig_trend_mort[[key]] <- make_trend_plot_dt(rank_dt_mort, lvl, mort_rate_col_nat, top_n = top_n_trend_this)
  fig_trend_avp[[key]]  <- make_trend_plot_dt(rank_dt_avp, lvl, avp_abs_col_nat, top_n = top_n_trend_this)
  heat_mort[[key]] <- make_heatmap_dt(reg_dt_mort, lvl, mort_rate_col_reg, loc_name_col_reg_mort, top_n = top_n_heat_this)
  heat_avp[[key]]  <- make_heatmap_dt(reg_dt_avp, lvl, avp_rate_col_reg, loc_name_col_reg_avp, top_n = top_n_heat_this)
}

# ============================================================
# Graficos
# ============================================================

p_total_mort_nat <- ggplot(total_mort_nat_plot, aes(x = year_id, y = tasa)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = txt("Figura T1. Mortalidad total nacional 2018-2024"),
    subtitle = txt("Tasa por 100 000 habitantes"),
    x = txt("Año"),
    y = txt("Tasa por 100 000")
  ) +
  scale_x_continuous(breaks = CFG$years) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_total_avp_nat <- ggplot(total_avp_nat_plot, aes(x = year_id, y = tasa)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2) +
  labs(
    title = txt("Figura T2. AVP total nacional 2018-2024"),
    subtitle = txt("Tasa por 100 000 habitantes"),
    x = txt("Año"),
    y = txt("Tasa por 100 000")
  ) +
  scale_x_continuous(breaks = CFG$years) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_total_mort_reg <- ggplot(total_mort_reg_plot, aes(x = reorder(region, tasa), y = tasa)) +
  geom_col() +
  coord_flip() +
  labs(
    title = txt("Figura T3. Mortalidad total regional 2024"),
    subtitle = txt("Tasa por 100 000 habitantes"),
    x = NULL,
    y = txt("Tasa por 100 000")
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

p_total_avp_reg <- ggplot(total_avp_reg_plot, aes(x = reorder(region, tasa), y = tasa)) +
  geom_col() +
  coord_flip() +
  labs(
    title = txt("Figura T4. AVP total regional 2024"),
    subtitle = txt("Tasa por 100 000 habitantes"),
    x = NULL,
    y = txt("Tasa por 100 000")
  ) +
  scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
  theme_fig()

plot_rank <- function(dt, title, subtitle, ylab) {
  ggplot(dt, aes(x = reorder(cause_name, valor), y = valor)) +
    geom_col() +
    coord_flip() +
    labs(title = txt(title), subtitle = txt(subtitle), x = NULL, y = txt(ylab)) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
    scale_x_discrete(labels = wrap_label) +
    theme_fig()
}

plot_trend <- function(dt, title, subtitle, ylab) {
  ggplot(dt, aes(x = year_id, y = valor, group = cause_name, color = cause_name)) +
    geom_line(linewidth = 0.7) +
    geom_point(size = 1.4) +
    labs(title = txt(title), subtitle = txt(subtitle), x = txt("Año"), y = txt(ylab), color = txt("Causa")) +
    scale_x_continuous(breaks = CFG$years) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".")) +
    scale_color_discrete(labels = function(x) wrap_label(x, width = 18)) +
    guides(color = guide_legend(ncol = 2)) +
    theme_fig()
}

plot_heat <- function(dt, title, subtitle, fill_lab) {
  ggplot(dt, aes(x = cause_name, y = region, fill = valor)) +
    geom_tile() +
    labs(title = txt(title), subtitle = txt(subtitle), x = NULL, y = txt("Región"), fill = txt(fill_lab)) +
    scale_fill_gradientn(
      colours = c("#ffffff", "#fee5d9", "#fcae91", "#fb6a4a", "#cb181d"),
      labels = label_number(big.mark = " ", decimal.mark = ".")
    ) +
    theme_fig() +
    scale_x_discrete(labels = function(x) wrap_label(x, width = 15)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.text.y = element_text(size = 7))
}

plot_files <- list(
  total_mort_nat = file.path(CFG$fig_dir, "fig_total_mort_nat.png"),
  total_avp_nat = file.path(CFG$fig_dir, "fig_total_avp_nat.png"),
  total_mort_reg = file.path(CFG$fig_dir, "fig_total_mort_reg.png"),
  total_avp_reg = file.path(CFG$fig_dir, "fig_total_avp_reg.png")
)

save_plot(p_total_mort_nat, plot_files$total_mort_nat, width = 8.0, height = 4.8)
save_plot(p_total_avp_nat, plot_files$total_avp_nat, width = 8.0, height = 4.8)
save_plot(p_total_mort_reg, plot_files$total_mort_reg, width = 8.2, height = 6.2)
save_plot(p_total_avp_reg, plot_files$total_avp_reg, width = 8.2, height = 6.2)

for (lvl in levels_core) {
  key <- paste0("L", lvl)
  lvl_lab <- level_label(lvl)

  p_mort_rank <- plot_rank(
    fig_rank_mort[[key]],
    paste("Figura M", lvl, ". Mortalidad nacional 2024 por", tolower(lvl_lab), sep = ""),
    rank_subtitle(lvl, "mort"),
    "Tasa por 100 000"
  )
  p_avp_rank <- plot_rank(
    fig_rank_avp[[key]],
    paste("Figura A", lvl, ". AVP nacional 2024 por", tolower(lvl_lab), sep = ""),
    rank_subtitle(lvl, "avp"),
    "AVP"
  )
  p_mort_trend <- plot_trend(
    fig_trend_mort[[key]],
    paste("Figura MT", lvl, ". Tendencia nacional de mortalidad por ", tolower(lvl_lab), sep = ""),
    trend_subtitle(lvl),
    "Tasa por 100 000"
  )
  p_avp_trend <- plot_trend(
    fig_trend_avp[[key]],
    paste("Figura AT", lvl, ". Tendencia nacional de AVP por ", tolower(lvl_lab), sep = ""),
    trend_subtitle(lvl),
    "AVP"
  )
  p_heat_mort <- plot_heat(
    heat_mort[[key]],
    paste("Figura RM", lvl, ". Mortalidad regional 2024 por", tolower(lvl_lab), sep = ""),
    heat_subtitle(lvl),
    "Tasa por 100 000"
  )
  p_heat_avp <- plot_heat(
    heat_avp[[key]],
    paste("Figura RA", lvl, ". AVP regional 2024 por", tolower(lvl_lab), sep = ""),
    heat_subtitle(lvl),
    "Tasa por 100 000"
  )

  plot_files[[paste0("mort_rank_", key)]] <- file.path(CFG$fig_dir, paste0("fig_mort_rank_", key, ".png"))
  plot_files[[paste0("avp_rank_", key)]]  <- file.path(CFG$fig_dir, paste0("fig_avp_rank_", key, ".png"))
  plot_files[[paste0("mort_trend_", key)]] <- file.path(CFG$fig_dir, paste0("fig_mort_trend_", key, ".png"))
  plot_files[[paste0("avp_trend_", key)]]  <- file.path(CFG$fig_dir, paste0("fig_avp_trend_", key, ".png"))
  plot_files[[paste0("heat_mort_", key)]] <- file.path(CFG$fig_dir, paste0("fig_heat_mort_", key, ".png"))
  plot_files[[paste0("heat_avp_", key)]]  <- file.path(CFG$fig_dir, paste0("fig_heat_avp_", key, ".png"))

  save_plot(p_mort_rank, plot_files[[paste0("mort_rank_", key)]], width = 8.0, height = 5.8)
  save_plot(p_avp_rank, plot_files[[paste0("avp_rank_", key)]], width = 8.0, height = 5.8)
  save_plot(p_mort_trend, plot_files[[paste0("mort_trend_", key)]], width = 8.3, height = if (lvl == 2L) 7.0 else 5.2)
  save_plot(p_avp_trend, plot_files[[paste0("avp_trend_", key)]], width = 8.3, height = if (lvl == 2L) 7.0 else 5.2)
  save_plot(p_heat_mort, plot_files[[paste0("heat_mort_", key)]], width = if (lvl == 2L) 10.5 else 8.8, height = if (lvl == 2L) 7.4 else 6.8)
  save_plot(p_heat_avp, plot_files[[paste0("heat_avp_", key)]], width = if (lvl == 2L) 10.5 else 8.8, height = if (lvl == 2L) 7.4 else 6.8)
}

# ============================================================
# Word
# ============================================================

doc <- read_docx()

doc <- doc |>
  body_add_par("Informe descriptivo: Mortalidad y AVP de todas las causas", style = "heading 1") |>
  body_add_par("Peru, 2018-2024", style = "heading 2") |>
  body_add_par("Indice general", style = "heading 1") |>
  body_add_toc(level = 2) |>
  body_add_break() |>
  body_add_par("IntroducciÃ³n", style = "heading 1") |>
  body_add_par(
    paste0(
      "Este informe presenta resultados nacionales y regionales de mortalidad y AVP para todas las causas incluidas en la jerarquÃ­a analÃ­tica del proyecto. ",
      "Se presenta el total y los niveles 1 a 4. ",
      "La jerarquÃ­a vigente contiene ", catalog_summary_text, ". ",
      "Dentro de esa estructura, Otras muertes relacionadas con la pandemia (OPRM) se mantiene como una categorÃ­a especial y transversal del pipeline. ",
      "Se muestra en todos los niveles para fines de lectura comparativa, pero no representa una rama etiolÃ³gica convencional: resume mortalidad residual pandÃ©mica del modelamiento, sin doble conteo con COVID-19 ni con otras causas letales."
    ),
    style = "Normal"
  ) |>
  body_add_par(
    "Las tablas del cuerpo principal muestran el universo completo de categorÃ­as en cada nivel. Las figuras se concentran en el top 10 para facilitar la lectura. Todas las tasas se expresan por 100 000 habitantes.",
    style = "Normal"
  ) |>
  body_add_par("MÃ©todos (resumen)", style = "heading 1") |>
  body_add_par(
    "La mortalidad y el AVP provienen del pipeline auditado del proyecto. Las muertes se estiman, redistribuyen y reconcilian jerarquicamente antes del cÃ¡lculo de AVP. El AVP se calcula con tabla de vida estÃ¡ndar WHO GHE y luego se agrega por sexo, edad, regiÃ³n y causa. Para los detalles metodolÃ³gicos completos, revisar el anexo metodolÃ³gico del proyecto.",
    style = "Normal"
  )

doc <- read_docx() |>
  body_add_par_utf8("Informe descriptivo: Mortalidad y AVP de todas las causas", style = "heading 1") |>
  body_add_par_utf8("Perú, 2018-2024", style = "heading 2") |>
  body_add_par_utf8("Indice general", style = "heading 1") |>
  body_add_toc(level = 2) |>
  body_add_break() |>
  body_add_par_utf8("Introducción", style = "heading 1") |>
  body_add_par_utf8(
    paste0(
      "Este informe presenta resultados nacionales y regionales de mortalidad y AVP para todas las causas incluidas en la jerarquía analítica del proyecto. ",
      "Se presenta el total y los niveles 1 a 4. ",
      "La jerarquía vigente contiene ", txt(catalog_summary_text), ". ",
      "Dentro de esa estructura, Otras muertes relacionadas con la pandemia (OPRM) se mantiene como una categoría especial y transversal del pipeline. ",
      "Se muestra en todos los niveles para fines de lectura comparativa, pero no representa una rama etiológica convencional: resume mortalidad residual pandémica del modelamiento, sin doble conteo con COVID-19 ni con otras causas letales."
    ),
    style = "Normal"
  ) |>
  body_add_par_utf8(
    "Las tablas del cuerpo principal muestran el universo completo de categorías en cada nivel. Las figuras se concentran en el top 10 para facilitar la lectura. Todas las tasas se expresan por 100 000 habitantes.",
    style = "Normal"
  ) |>
  body_add_par_utf8("Métodos (resumen)", style = "heading 1") |>
  body_add_par_utf8(
    "La mortalidad y el AVP provienen del pipeline auditado del proyecto. Las muertes se estiman, redistribuyen y reconcilian jerárquicamente antes del cálculo de AVP. El AVP se calcula con tabla de vida estándar WHO GHE y luego se agrega por sexo, edad, región y causa. El anexo metodológico de este informe resume además el manejo de incompatibilidades demográficas directas, los tratamientos específicos por CIE-10 y la postura comparada OMS/Australia/proyecto para los casos sensibles priorizados.",
    style = "Normal"
  ) |>
  body_add_par_utf8("Totales", style = "heading 1")

doc <- add_ft_title(
  doc,
  "Tabla T1. Mortalidad total nacional 2018-2024",
  style_ft_plain(flextable(pretty_total_mort_nat), size = 7),
  note = rate_note_text
)
doc <- add_plot_title(doc, "Figura T1. Mortalidad total nacional 2018-2024", plot_files$total_mort_nat, width = 6.8, height = 4.2)
doc <- add_ft_title(
  doc,
  "Tabla T2. AVP total nacional 2018-2024",
  style_ft_plain(flextable(pretty_total_avp_nat), size = 7),
  note = rate_note_text
)
doc <- add_plot_title(doc, "Figura T2. AVP total nacional 2018-2024", plot_files$total_avp_nat, width = 6.8, height = 4.2)
doc <- add_ft_title(
  doc,
  "Tabla T3. Mortalidad total regional 2024",
  style_ft_plain(flextable(pretty_total_mort_reg), size = 6),
  note = rate_note_text
)
doc <- add_plot_title(doc, "Figura T3. Mortalidad total regional 2024", plot_files$total_mort_reg, width = 6.8, height = 5.0)
doc <- add_ft_title(
  doc,
  "Tabla T4. AVP total regional 2024",
  style_ft_plain(flextable(pretty_total_avp_reg), size = 6),
  note = rate_note_text
)
doc <- add_plot_title(doc, "Figura T4. AVP total regional 2024", plot_files$total_avp_reg, width = 6.8, height = 5.0)

doc <- doc |>
  body_add_par_utf8("Mortalidad", style = "heading 1")

for (lvl in levels_core) {
  key <- paste0("L", lvl)
  doc <- doc |>
    body_add_par_utf8(level_label(lvl), style = "heading 2")
  scope_note <- level_scope_note(lvl)
  if (!is.null(scope_note)) {
    doc <- body_add_par_utf8(doc, scope_note, style = "Normal")
  }

  doc <- add_ft_title(
    doc,
    paste("Tabla M", lvl, ". Ranking nacional 2024 de mortalidad por ", tolower(level_label(lvl)), sep = ""),
    style_ft_plain(flextable(pretty_mort_rank(level_tables[[key]]$mort_rank)), size = 6),
    note = rate_note_text
  )
  doc <- add_ft_title(
    doc,
    paste("Tabla MT", lvl, ". Tendencia nacional 2018-2024 de mortalidad por ", tolower(level_label(lvl)), sep = ""),
    style_ft_plain(flextable(level_tables[[key]]$mort_trend), size = 6),
    landscape = TRUE,
    note = rate_note_text
  )
  doc <- add_plot_title(
    doc,
    paste("Figura M", lvl, ". Mortalidad nacional 2024 por", tolower(level_label(lvl)), sep = ""),
    plot_files[[paste0("mort_rank_", key)]],
    width = 6.8,
    height = 4.8
  )
  doc <- add_plot_title(
    doc,
    paste("Figura MT", lvl, ". Tendencia nacional de mortalidad por ", tolower(level_label(lvl)), sep = ""),
    plot_files[[paste0("mort_trend_", key)]],
    width = 6.8,
    height = 4.6
  )
}

doc <- doc |>
  body_add_par_utf8("AVP", style = "heading 1")

for (lvl in levels_core) {
  key <- paste0("L", lvl)
  doc <- doc |>
    body_add_par_utf8(level_label(lvl), style = "heading 2")
  scope_note <- level_scope_note(lvl)
  if (!is.null(scope_note)) {
    doc <- body_add_par_utf8(doc, scope_note, style = "Normal")
  }

  doc <- add_ft_title(
    doc,
    paste("Tabla A", lvl, ". Ranking nacional 2024 de AVP por ", tolower(level_label(lvl)), sep = ""),
    style_ft_plain(flextable(pretty_avp_rank(level_tables[[key]]$avp_rank)), size = 6)
  )
  doc <- add_ft_title(
    doc,
    paste("Tabla AT", lvl, ". Tendencia nacional 2018-2024 de AVP por ", tolower(level_label(lvl)), sep = ""),
    style_ft_plain(flextable(level_tables[[key]]$avp_trend), size = 6),
    landscape = TRUE
  )
  doc <- add_plot_title(
    doc,
    paste("Figura A", lvl, ". AVP nacional 2024 por", tolower(level_label(lvl)), sep = ""),
    plot_files[[paste0("avp_rank_", key)]],
    width = 6.8,
    height = 4.8
  )
  doc <- add_plot_title(
    doc,
    paste("Figura AT", lvl, ". Tendencia nacional de AVP por ", tolower(level_label(lvl)), sep = ""),
    plot_files[[paste0("avp_trend_", key)]],
    width = 6.8,
    height = 4.6
  )
}

doc <- doc |>
  body_add_par_utf8("Resumen regional 2024", style = "heading 1") |>
  body_add_par_utf8(
    "Se incluyen heatmaps regionales para cada nivel de causa con los nombres departamentales canÃ³nicos del proyecto. La escala de color usa un gradiente continuo de blanco a rojo, donde los tonos mÃ¡s intensos representan mayor carga observada. Las columnas se ordenan de mayor a menor carga observada en 2024 dentro del top 10 grÃ¡fico.",
    style = "Normal"
  )

for (lvl in levels_core) {
  key <- paste0("L", lvl)
  doc <- doc |>
    body_add_par_utf8(level_label(lvl), style = "heading 2")

  doc <- add_plot_title(
    doc,
    paste("Figura RM", lvl, ". Mortalidad regional 2024 por", tolower(level_label(lvl)), sep = ""),
    plot_files[[paste0("heat_mort_", key)]],
    width = 6.8,
    height = 5.3
  )
  doc <- add_plot_title(
    doc,
    paste("Figura RA", lvl, ". AVP regional 2024 por", tolower(level_label(lvl)), sep = ""),
    plot_files[[paste0("heat_avp_", key)]],
    width = 6.8,
    height = 5.3
  )
}

doc <- doc |>
  body_add_par_utf8("Anexos", style = "heading 1") |>
  body_add_par_utf8(
    "El Anexo 1 resume la lista completa de causas y subcausas del proyecto, manteniendo la jerarquÃ­a y mostrando la cobertura CIE-10 utilizada en la prÃ¡ctica, junto con la marca de uso para mortalidad y AVP. Las tablas completas de mortalidad y AVP para todos los niveles y categorÃ­as tambiÃ©n se exportan en los anexos Excel de validaciÃ³n.",
    style = "Normal"
  )

doc <- add_ft_title(
  doc,
  "Anexo 1. Catalogo completo de causas por niveles y subniveles",
  build_annex_ft(annex_cause_catalog),
  landscape = TRUE,
  note = "Nota: Otras muertes relacionadas con la pandemia (OPRM) es una categorÃ­a especial del modelamiento pandÃ©mico. Se presenta de manera transversal para mantener la comparabilidad del total entre niveles, pero no corresponde a una causa etiolÃ³gica convencional. Su cobertura CIE-10 no proviene de mapeo directo, sino de la reasignaciÃ³n contable del exceso pandÃ©mico donde corresponde."
)

doc <- add_ft_title(
  doc,
  "Anexo 2. Incompatibilidades demograficas directas y manejo aplicado",
  style_ft_methods(flextable(annex_demog_incompat)),
  note = "Nota: estos registros no modifican sexo ni edad observados. Cuando la causa directa es incompatible con la demografia registrada, se la trata como causa directa no confiable y se la redirige a un pool de redistribucion trazable."
)

doc <- add_ft_title(
  doc,
  "Anexo 3. Tratamientos especificos por codigo CIE-10",
  style_ft_methods(flextable(annex_direct_specific)),
  note = "Nota: estos casos no surgen de reglas garbage generales, sino de decisiones metodologicas puntuales documentadas y mantenidas en maestros editables para futuros años."
)

doc <- add_ft_title(
  doc,
  "Anexo 4. Casos sensibles: postura OMS, Australia y proyecto",
  style_ft_methods(flextable(annex_sensitive_cases)),
  note = "Nota: este anexo resume los casos con mayor sensibilidad metodologica para facilitar auditoria, actualizacion futura y redaccion del anexo metodologico narrativo."
)

target_out <- CFG$out_file
print_status <- tryCatch({
  print(doc, target = target_out)
  NULL
}, error = function(e) e)

if (inherits(print_status, "error")) {
  fallback_out <- file.path(
    CFG$out_dir,
    paste0(
      "informe_mortalidad_avp_todas_las_causas_",
      format(Sys.time(), "%Y%m%d_%H%M%S"),
      ".docx"
    )
  )
  cat("\nAviso: no se pudo sobreescribir el Word principal. Se generarÃ¡ una copia alternativa en:\n", fallback_out, "\n", sep = "")
  print(doc, target = fallback_out)
  target_out <- fallback_out
}

preview_annex_out <- file.path(CFG$out_dir, "preview_anexo_catalogo_causas.docx")
preview_doc <- read_docx() |>
  body_add_par_utf8("Preview. Anexo 1. Catalogo completo de causas por niveles y subniveles", style = "heading 1")
preview_doc <- add_ft_title(
  preview_doc,
  "Catalogo jerarquico de causas",
  build_annex_ft(annex_cause_catalog),
  note = "Nota: Otras muertes relacionadas con la pandemia (OPRM) es una categorÃ­a especial del modelamiento pandÃ©mico. Se presenta de manera transversal para mantener la comparabilidad del total entre niveles, pero no corresponde a una causa etiolÃ³gica convencional."
)
print(preview_doc, target = preview_annex_out)

cat("\nListo.\n")
cat("Word generado en:\n", target_out, "\n")

