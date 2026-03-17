#!/usr/bin/env Rscript

# ============================================================
# diagnostico_avp_rate_pdfs.R
# ------------------------------------------------------------
# PDFs diagnósticos SOLO para tasa AVP suavizada/reconciliada
# en Perú 2018-2024.
#
# Salidas:
#   1) avp_rate_age_all_levels.pdf
#      Un PDF único multipágina para AVP por edad.
#   2) avp_rate_time_all_levels.pdf
#      Un PDF único multipágina para AVP por año.
#
# En ambos PDFs:
#   - series por Hombre, Mujer y Ambos
#   - Ambos en color negro
#   - páginas de 4 causas por defecto
#   - niveles L4, L3, L2, L1 y L0(total)
#
# Diseño epidemiológico:
#   - cada causa se grafica en un panel independiente
#   - entre causas, el eje Y es libre
#   - en edad, los años de una misma causa comparten la misma escala Y
#
# Nota semántica importante:
#   - Este script USA la tasa AVP ya construida por el pipeline
#     (columna canónica esperada: avp_rate).
#   - No vuelve a multiplicar por 100,000.
#   - Solo recalcula tasa para "Ambos" a partir de AVP absoluto y población,
#     porque esa agregación no suele venir explícita en el dataset base.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(arrow)
  library(here)
  library(scales)
  library(stringr)
  library(patchwork)
})

# ------------------------------------------------------------
# Utils del proyecto
# ------------------------------------------------------------
source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "age_utils.R"))

# ------------------------------------------------------------
# Configuración principal
# ------------------------------------------------------------
CFG <- list(
  version = "v0.1.0_avp_rate_only_final",
  dataset_id = "diagnostic_pdfs_avp_rate_only",
  
  input_avp_candidates = c(
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.parquet"),
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  input_model_registry_candidates = c(
    here("data", "derived", "qc", "09_build_mortality_rates", "mortality_model_registry.csv")
  ),
  
  input_qc_avp_candidates = c(
    here("data", "derived", "qc", "10_compute_avp_yll", "qc_rate_plausibility.csv")
  ),
  
  out_dir = here("outputs", "diagnostic_pdfs"),
  qc_dir  = here("data", "derived", "qc", "12_diagnostic_pdfs_avp"),
  
  years = 2018:2024,
  location_id_national = 9000L,
  valid_sexes = c(8507L, 8532L),
  age_min = 0L,
  age_max = 110L,
  keep_cause_levels = c(4L, 3L, 2L, 1L, 0L),
  
  causes_per_page = 4L,
  wrap_width_cause = 22L,
  min_total_avp_abs = 100,
  
  width_landscape = 14,
  width_age_landscape = 18,
  height_landscape = 10,
  
  palette_sex = c("Hombre" = "#1f78b4", "Mujer" = "#e31a1c", "Ambos" = "#000000"),
  
  qc_rate_hard_max = 100000,
  qc_jump_ratio_warn = 3,
  qc_jump_ratio_hard = 10,
  qc_sex_ratio_warn = 20,
  qc_sex_ratio_hard = 100,
  
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

read_dt_any <- function(path) {
  if (is.na(path)) stop("Ruta no encontrada.")
  ext <- tolower(tools::file_ext(path))
  if (ext == "parquet") return(as.data.table(arrow::read_parquet(path)))
  if (ext == "csv") return(fread(path))
  stop("Formato no soportado: ", path)
}

detect_col_local <- function(dt, candidates, label, required = TRUE) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    if (isTRUE(required)) {
      stop(
        "No se encontró columna para ", label,
        ". Candidatas: ", paste(candidates, collapse = ", "),
        "\nColumnas disponibles: ", paste(names(dt), collapse = ", ")
      )
    }
    return(NA_character_)
  }
  hit[1]
}

sex_label_fun <- function(sex_id) {
  fifelse(
    sex_id == 8507L, "Hombre",
    fifelse(sex_id == 8532L, "Mujer", as.character(sex_id))
  )
}

safe_rate <- function(num, den, mult = 100000) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- mult * num[ok] / den[ok]
  out
}

wrap_cause_name <- function(x, width = 22L) {
  stringr::str_wrap(as.character(x), width = width)
}

pretty_cause_label <- function(dt, width = CFG$wrap_width_cause) {
  dt[, cause_name_wrapped := wrap_cause_name(cause_name, width = width)]
  dt[, cause_label := paste0(cause_name_wrapped, "\n[", cause_concept_id, "]")]
  dt[]
}

split_into_pages <- function(x, n_per_page = 4L) {
  if (length(x) == 0L) return(list())
  split(x, ceiling(seq_along(x) / n_per_page))
}

append_both_sexes_from_agg <- function(dt_agg, value_cols, by_cols_no_sex) {
  both <- dt_agg[, lapply(.SD, function(z) sum(z, na.rm = TRUE)), by = by_cols_no_sex, .SDcols = value_cols]
  both[, sex_label := "Ambos"]
  rbindlist(list(dt_agg, both), use.names = TRUE, fill = TRUE)
}

compose_page_with_caption <- function(plot_list, title, subtitle, caption = NULL, ncol = 2L) {
  plot_list <- Filter(Negate(is.null), plot_list)
  if (length(plot_list) == 0L) return(NULL)
  
  wrap_plots(plotlist = plot_list, ncol = ncol, guides = "collect") +
    plot_annotation(
      title = title,
      subtitle = subtitle,
      caption = caption,
      theme = theme(
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 10),
        plot.caption = element_text(size = 9, hjust = 0)
      )
    ) &
    theme(
      legend.position = "bottom",
      legend.direction = "horizontal",
      legend.box = "horizontal",
      legend.justification = "center",
      legend.title = element_text(size = 9),
      legend.text = element_text(size = 8)
    )
}

# ------------------------------------------------------------
# Preparación de datos
# ------------------------------------------------------------
prepare_dataset <- function(location_id_target = CFG$location_id_national,
                            years = CFG$years,
                            cause_levels = CFG$keep_cause_levels) {
  
  avp_path   <- first_existing(CFG$input_avp_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  reg_path   <- first_existing(CFG$input_model_registry_candidates)
  qcavp_path <- first_existing(CFG$input_qc_avp_candidates)
  
  if (is.na(avp_path)) stop("No encontré avp_yll_cause_reconciled.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  
  msg("Leyendo AVP reconciliado...")
  avp <- read_dt_any(avp_path)
  
  msg("Leyendo cause_master...")
  cm <- read_dt_any(cause_path)
  
  reg <- if (!is.na(reg_path)) fread(reg_path) else data.table()
  qcavp <- if (!is.na(qcavp_path)) fread(qcavp_path) else data.table()
  
  msg("Columnas detectadas en AVP: ", paste(names(avp), collapse = ", "))
  
  col_year   <- detect_col_local(avp, c("year_id"), "year_id")
  col_loc    <- detect_col_local(avp, c("location_id"), "location_id")
  col_sex    <- detect_col_local(avp, c("sex_id"), "sex_id")
  col_age    <- detect_col_local(avp, c("age"), "age")
  col_cid    <- detect_col_local(avp, c("cause_concept_id"), "cause_concept_id")
  col_clev   <- detect_col_local(avp, c("cause_level"), "cause_level")
  col_pop    <- detect_col_local(avp, c("population"), "population")
  
  col_avp_abs <- detect_col_local(
    avp,
    candidates = c("avp_abs", "yll_abs"),
    label = "AVP absoluto",
    required = TRUE
  )
  
  col_avp_rate <- detect_col_local(
    avp,
    candidates = c("avp_rate", "yll_rate"),
    label = "tasa AVP",
    required = TRUE
  )
  
  std_names <- c(
    year_id = col_year,
    location_id = col_loc,
    sex_id = col_sex,
    age = col_age,
    cause_concept_id = col_cid,
    cause_level = col_clev,
    population = col_pop,
    avp_abs = col_avp_abs,
    avp_rate = col_avp_rate
  )
  
  for (nm in names(std_names)) {
    if (std_names[[nm]] != nm) {
      setnames(avp, std_names[[nm]], nm)
    }
  }
  
  req_avp <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_level", "population", "avp_abs", "avp_rate"
  )
  miss_avp <- setdiff(req_avp, names(avp))
  if (length(miss_avp) > 0L) stop("Faltan columnas en avp: ", paste(miss_avp, collapse = ", "))
  
  req_cm_min <- c("cause_concept_id", "cause_level", "parent_concept_id", "is_terminal")
  miss_cm_min <- setdiff(req_cm_min, names(cm))
  if (length(miss_cm_min) > 0L) stop("Faltan columnas mínimas en cause_master: ", paste(miss_cm_min, collapse = ", "))
  
  msg("Columnas detectadas en cause_master: ", paste(names(cm), collapse = ", "))
  
  cm <- copy(cm)
  cm_cause_name_col <- detect_col_local(
    cm,
    candidates = c("cause_name", "cause_label", "name", "terminal_cause_name"),
    label = "nombre de causa en cause_master",
    required = FALSE
  )
  
  if (is.na(cm_cause_name_col)) {
    if ("cause_code" %in% names(cm)) {
      cm[, cause_name := as.character(cause_code)]
    } else {
      cm[, cause_name := paste0("cause_", cause_concept_id)]
    }
  } else if (cm_cause_name_col != "cause_name") {
    setnames(cm, cm_cause_name_col, "cause_name")
  }
  
  cm[, cause_concept_id := as.integer(cause_concept_id)]
  cm[, cause_level := as.integer(cause_level)]
  cm[, cause_name := as.character(cause_name)]
  cm[, parent_concept_id := as.integer(parent_concept_id)]
  cm[, is_terminal := as.logical(is_terminal)]
  
  if (!"is_residual" %in% names(cm)) cm[, is_residual := FALSE]
  cm[, is_residual := as.logical(is_residual)]
  
  if (!"is_covid_related" %in% names(cm)) cm[, is_covid_related := FALSE]
  cm[, is_covid_related := as.logical(is_covid_related)]
  
  cm <- unique(cm[, .(
    cause_concept_id,
    cause_level,
    cause_name,
    parent_concept_id,
    is_terminal,
    is_residual,
    is_covid_related
  )])
  
  avp[, `:=`(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    population = as.numeric(population),
    avp_abs = as.numeric(avp_abs),
    avp_rate = as.numeric(avp_rate)
  )]
  
  avp <- avp[
    location_id == location_id_target &
      year_id %in% years &
      sex_id %in% CFG$valid_sexes &
      age >= CFG$age_min & age <= CFG$age_max &
      cause_level %in% cause_levels
  ]
  
  if ("cause_name" %in% names(avp)) avp[, cause_name := NULL]
  
  avp <- merge(
    avp,
    cm[, .(cause_concept_id, cause_level, cause_name)],
    by = c("cause_concept_id", "cause_level"),
    all.x = TRUE,
    sort = FALSE
  )
  
  if (!"cause_name" %in% names(avp)) avp[, cause_name := paste0("cause_", cause_concept_id)]
  avp[is.na(cause_name) | cause_name == "", cause_name := paste0("cause_", cause_concept_id)]
  
  avp[, sex_label := sex_label_fun(sex_id)]
  avp <- pretty_cause_label(avp, width = CFG$wrap_width_cause)
  
  if (nrow(reg) > 0L) {
    keep_reg <- intersect(
      c("cause_concept_id", "method_selected", "data_category",
        "years_with_deaths", "regions_with_deaths", "total_deaths_input"),
      names(reg)
    )
    reg2 <- unique(reg[, ..keep_reg])
    avp <- merge(avp, reg2, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  }
  
  cause_stats <- avp[, .(
    cause_name = {
      zz <- na.omit(cause_name)
      if (length(zz) == 0L) paste0("cause_", cause_concept_id[1]) else zz[1]
    },
    cause_level = {
      zz <- na.omit(cause_level)
      if (length(zz) == 0L) NA_integer_ else zz[1]
    },
    total_avp_abs = sum(avp_abs, na.rm = TRUE),
    max_rate = suppressWarnings(max(avp_rate, na.rm = TRUE)),
    n_cells = .N
  ), by = cause_concept_id]
  
  cause_stats <- merge(
    cause_stats,
    cm[, .(cause_concept_id, is_covid_related)],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  cause_stats[is.na(is_covid_related), is_covid_related := FALSE]
  
  list(
    avp = avp,
    cm = cm,
    cause_stats = cause_stats,
    model_registry = reg,
    qc_avp = qcavp
  )
}

filter_causes_auto <- function(cause_stats,
                               cause_level_target,
                               include_total = FALSE,
                               min_total_avp_abs = CFG$min_total_avp_abs,
                               top_n = NULL,
                               exclude_residual = FALSE,
                               cm = NULL) {
  
  x <- copy(cause_stats)[cause_level == cause_level_target]
  x[is.na(total_avp_abs), total_avp_abs := 0]
  x[is.na(max_rate), max_rate := 0]
  x <- x[total_avp_abs >= min_total_avp_abs]
  
  if (!include_total) x <- x[cause_level != 0L]
  
  if (exclude_residual && !is.null(cm) && "is_residual" %in% names(cm)) {
    x <- merge(x, cm[, .(cause_concept_id, is_residual)], by = "cause_concept_id", all.x = TRUE)
    x <- x[is.na(is_residual) | is_residual == FALSE]
  }
  
  setorder(x, cause_level, cause_name, cause_concept_id)
  if (!is.null(top_n)) x <- x[seq_len(min(.N, top_n))]
  x[]
}

get_level_cause_ids <- function(prep, cause_level, top_n = NULL, sort_mode = c("concept_id", "name", "avp_desc")) {
  sort_mode <- match.arg(sort_mode)
  
  x <- filter_causes_auto(
    prep$cause_stats,
    cause_level_target = cause_level,
    include_total = (cause_level == 0L),
    top_n = NULL,
    cm = prep$cm
  )
  
  if (cause_level == 1L) {
    covid_l1_ids <- prep$cause_stats[cause_level == 1L & is_covid_related == TRUE, unique(cause_concept_id)]
    if (length(covid_l1_ids) > 0L) {
      x <- unique(rbindlist(list(
        x,
        prep$cause_stats[cause_concept_id %in% covid_l1_ids]
      ), use.names = TRUE, fill = TRUE), by = "cause_concept_id")
    }
  }
  
  if (sort_mode == "concept_id") {
    setorder(x, cause_concept_id)
  } else if (sort_mode == "name") {
    setorder(x, cause_name, cause_concept_id)
  } else {
    setorder(x, -total_avp_abs, cause_concept_id)
  }
  
  if (!is.null(top_n)) x <- x[seq_len(min(.N, top_n))]
  x$cause_concept_id
}

summarize_level_coverage <- function(prep) {
  out <- prep$cause_stats[, .(
    n_causes = uniqueN(cause_concept_id),
    total_avp_abs = sum(total_avp_abs, na.rm = TRUE)
  ), by = cause_level][order(-cause_level)]
  
  fwrite(out, file.path(CFG$qc_dir, "qc_diag_level_coverage.csv"))
  msg("Cobertura de causas por nivel:")
  print(out)
  out
}

# ------------------------------------------------------------
# QC automático
# ------------------------------------------------------------
compute_qc_flags <- function(prep) {
  avp <- copy(prep$avp)
  
  qc_rate <- avp[, .(
    n_negative = sum(avp_rate < 0, na.rm = TRUE),
    max_rate = suppressWarnings(max(avp_rate, na.rm = TRUE)),
    n_hard = sum(avp_rate > CFG$qc_rate_hard_max, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id)]
  
  avp_all_age <- avp[, .(
    avp_abs = sum(avp_abs, na.rm = TRUE),
    pop = sum(population, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id, year_id)]
  avp_all_age[, rate := safe_rate(avp_abs, pop, 100000)]
  setorder(avp_all_age, cause_concept_id, sex_id, year_id)
  avp_all_age[, lag_rate := shift(rate), by = .(cause_concept_id, sex_id)]
  avp_all_age[, ratio_jump := fifelse(!is.na(lag_rate) & lag_rate > 0, rate / lag_rate, NA_real_)]
  avp_all_age[, qc_jump_warn := !is.na(ratio_jump) & (ratio_jump >= CFG$qc_jump_ratio_warn | ratio_jump <= 1 / CFG$qc_jump_ratio_warn)]
  avp_all_age[, qc_jump_hard := !is.na(ratio_jump) & (ratio_jump >= CFG$qc_jump_ratio_hard | ratio_jump <= 1 / CFG$qc_jump_ratio_hard)]
  
  wide_sex <- dcast(
    avp[, .(rate = sum(avp_abs, na.rm = TRUE), pop = sum(population, na.rm = TRUE)),
        by = .(cause_concept_id, cause_name, cause_level, year_id, age, sex_label)],
    cause_concept_id + cause_name + cause_level + year_id + age ~ sex_label,
    value.var = "rate",
    fill = NA_real_
  )
  
  wide_pop <- dcast(
    avp[, .(pop = sum(population, na.rm = TRUE)),
        by = .(cause_concept_id, cause_name, cause_level, year_id, age, sex_label)],
    cause_concept_id + cause_name + cause_level + year_id + age ~ sex_label,
    value.var = "pop",
    fill = NA_real_
  )
  
  for (sx in c("Hombre", "Mujer")) {
    if (!sx %in% names(wide_sex)) wide_sex[, (sx) := NA_real_]
    if (!sx %in% names(wide_pop)) wide_pop[, (sx) := NA_real_]
  }
  
  wide_sex <- merge(
    wide_sex,
    wide_pop,
    by = c("cause_concept_id", "cause_name", "cause_level", "year_id", "age"),
    suffixes = c("_abs", "_pop"),
    all = TRUE
  )
  
  wide_sex[, rate_h := safe_rate(Hombre_abs, Hombre_pop, 100000)]
  wide_sex[, rate_m := safe_rate(Mujer_abs, Mujer_pop, 100000)]
  wide_sex[, sex_ratio_mf := fifelse(!is.na(rate_m) & rate_m > 0, rate_h / rate_m, NA_real_)]
  wide_sex[, qc_sex_warn := !is.na(sex_ratio_mf) & (sex_ratio_mf >= CFG$qc_sex_ratio_warn | sex_ratio_mf <= 1 / CFG$qc_sex_ratio_warn)]
  wide_sex[, qc_sex_hard := !is.na(sex_ratio_mf) & (sex_ratio_mf >= CFG$qc_sex_ratio_hard | sex_ratio_mf <= 1 / CFG$qc_sex_ratio_hard)]
  
  impossible_age <- avp[
    age < CFG$age_min | age > CFG$age_max |
      avp_abs < 0 |
      avp_rate < 0,
    .N,
    by = .(cause_concept_id, cause_name, cause_level)
  ]
  
  fwrite(qc_rate, file.path(CFG$qc_dir, "qc_diag_rate_flags.csv"))
  fwrite(avp_all_age[qc_jump_warn == TRUE | qc_jump_hard == TRUE], file.path(CFG$qc_dir, "qc_diag_temporal_jumps.csv"))
  fwrite(wide_sex[qc_sex_warn == TRUE | qc_sex_hard == TRUE], file.path(CFG$qc_dir, "qc_diag_sex_ratio_flags.csv"))
  fwrite(impossible_age, file.path(CFG$qc_dir, "qc_diag_impossible_age.csv"))
  
  list(
    qc_rate = qc_rate,
    qc_jump = avp_all_age,
    qc_sex = wide_sex,
    qc_impossible_age = impossible_age
  )
}

# ------------------------------------------------------------
# Gráficos
# ------------------------------------------------------------
base_theme_diag <- function() {
  theme_minimal(base_size = 10) +
    theme(
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 9),
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8),
      plot.margin = margin(4, 4, 4, 4)
    )
}

plot_age_distribution <- function(dt,
                                  cause_ids,
                                  title,
                                  subtitle = NULL) {
  
  x <- copy(dt)[cause_concept_id %in% cause_ids]
  if (nrow(x) == 0L) return(NULL)
  
  x <- x[, .(
    avp_abs = sum(avp_abs, na.rm = TRUE),
    pop = sum(population, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_label, cause_level, year_id, age, sex_label)]
  
  x <- append_both_sexes_from_agg(
    x,
    value_cols = c("avp_abs", "pop"),
    by_cols_no_sex = c("cause_concept_id", "cause_label", "cause_level", "year_id", "age")
  )
  
  x[, rate := safe_rate(avp_abs, pop, 100000)]
  x[, sex_label := factor(sex_label, levels = c("Hombre", "Mujer", "Ambos"))]
  
  plots <- lapply(cause_ids, function(cid) {
    xc <- copy(x)[cause_concept_id == cid]
    if (nrow(xc) == 0L) return(NULL)
    cause_lab <- unique(xc$cause_label)[1]
    
    ggplot() +
      geom_line(
        data = xc[sex_label == "Ambos"],
        aes(x = age, y = rate, group = sex_label, color = sex_label, linetype = sex_label),
        linewidth = 0.35, alpha = 0.55
      ) +
      geom_line(
        data = xc[sex_label == "Hombre"],
        aes(x = age, y = rate, group = sex_label, color = sex_label),
        linewidth = 0.6, alpha = 0.95
      ) +
      geom_line(
        data = xc[sex_label == "Mujer"],
        aes(x = age, y = rate, group = sex_label, color = sex_label),
        linewidth = 0.6, alpha = 0.95
      ) +
      facet_wrap(~ year_id, ncol = 1, scales = "fixed") +
      scale_color_manual(values = CFG$palette_sex, drop = FALSE, breaks = c("Hombre", "Mujer", "Ambos")) +
      scale_linetype_manual(values = c("Hombre" = "solid", "Mujer" = "solid", "Ambos" = "22"), drop = FALSE, breaks = c("Hombre", "Mujer", "Ambos")) +
      scale_x_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 110)) +
      labs(
        title = cause_lab,
        x = "Edad simple",
        y = "Tasa AVP por 100,000",
        color = "Sexo",
        linetype = "Sexo"
      ) +
      base_theme_diag()
  })
  
  compose_page_with_caption(
    plot_list = plots,
    title = title,
    subtitle = subtitle,
    caption = paste(
      "Lectura sugerida:",
      "cada columna corresponde a una causa.",
      "Dentro de cada causa, los años 2018-2024 están apilados verticalmente y comparten la misma escala Y; entre causas, la escala Y cambia.",
      "Esto prioriza la lectura de la forma edad-sexo dentro de cada enfermedad.",
      "Revise concentración de AVP en edades tempranas versus tardías, diferencias hombre-mujer, picos por lesiones o causas perinatales, y quiebres inesperados por edad o año."
    ),
    ncol = 4L
  )
}

plot_time_trend <- function(dt,
                            cause_ids,
                            title,
                            subtitle = NULL) {
  
  x <- copy(dt)[cause_concept_id %in% cause_ids]
  if (nrow(x) == 0L) return(NULL)
  
  x <- x[, .(
    avp_abs = sum(avp_abs, na.rm = TRUE),
    pop = sum(population, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_label, cause_level, sex_label, year_id)]
  
  x <- append_both_sexes_from_agg(
    x,
    value_cols = c("avp_abs", "pop"),
    by_cols_no_sex = c("cause_concept_id", "cause_label", "cause_level", "year_id")
  )
  
  x[, rate := safe_rate(avp_abs, pop, 100000)]
  x[, sex_label := factor(sex_label, levels = c("Hombre", "Mujer", "Ambos"))]
  
  plots <- lapply(cause_ids, function(cid) {
    xc <- copy(x)[cause_concept_id == cid]
    if (nrow(xc) == 0L) return(NULL)
    cause_lab <- unique(xc$cause_label)[1]
    
    ggplot() +
      geom_line(
        data = xc[sex_label == "Ambos"],
        aes(x = year_id, y = rate, group = sex_label, color = sex_label),
        linewidth = 0.7, alpha = 0.7
      ) +
      geom_point(
        data = xc[sex_label == "Ambos"],
        aes(x = year_id, y = rate, color = sex_label),
        size = 1.1, alpha = 0.7
      ) +
      geom_line(
        data = xc[sex_label == "Hombre"],
        aes(x = year_id, y = rate, group = sex_label, color = sex_label),
        linewidth = 0.9, alpha = 0.95
      ) +
      geom_point(
        data = xc[sex_label == "Hombre"],
        aes(x = year_id, y = rate, color = sex_label),
        size = 1.4, alpha = 0.95
      ) +
      geom_line(
        data = xc[sex_label == "Mujer"],
        aes(x = year_id, y = rate, group = sex_label, color = sex_label),
        linewidth = 0.9, alpha = 0.95
      ) +
      geom_point(
        data = xc[sex_label == "Mujer"],
        aes(x = year_id, y = rate, color = sex_label),
        size = 1.4, alpha = 0.95
      ) +
      scale_color_manual(values = CFG$palette_sex, drop = FALSE, breaks = c("Hombre", "Mujer", "Ambos")) +
      scale_x_continuous(breaks = sort(unique(xc$year_id))) +
      labs(
        title = cause_lab,
        x = "Año",
        y = "Tasa AVP por 100,000",
        color = "Sexo"
      ) +
      base_theme_diag()
  })
  
  compose_page_with_caption(
    plot_list = plots,
    title = title,
    subtitle = subtitle,
    caption = paste(
      "Lectura sugerida:",
      "cada recuadro usa su propia escala Y.",
      "Compare la trayectoria temporal dentro de cada causa, no entre causas.",
      "Busque saltos abruptos, inversiones inesperadas por sexo, patrones pandémicos 2020-2021 o serruchos incompatibles con la trayectoria esperada del AVP."
    ),
    ncol = 2L
  )
}

write_multipage_pdf <- function(plot_list, file, width = CFG$width_landscape, height = CFG$height_landscape) {
  plot_list <- Filter(Negate(is.null), plot_list)
  if (length(plot_list) == 0L) {
    warning("No hay plots para escribir en: ", file)
    return(invisible(FALSE))
  }
  
  grDevices::pdf(file = file, width = width, height = height, onefile = TRUE)
  on.exit(grDevices::dev.off(), add = TRUE)
  for (p in plot_list) print(p)
  invisible(TRUE)
}

build_pdf_avp_age_all_levels <- function(prep,
                                         out_file = file.path(CFG$out_dir, "avp_rate_age_all_levels.pdf"),
                                         causes_per_page = CFG$causes_per_page,
                                         top_n_per_level = NULL,
                                         sort_mode = "concept_id") {
  plot_list <- list()
  
  for (lvl in c(4L, 3L, 2L, 1L, 0L)) {
    ids <- get_level_cause_ids(prep, cause_level = lvl, top_n = top_n_per_level, sort_mode = sort_mode)
    pages <- split_into_pages(ids, causes_per_page)
    
    lev_plots <- lapply(seq_along(pages), function(i) {
      plot_age_distribution(
        dt = prep$avp,
        cause_ids = pages[[i]],
        title = paste0("AVP por edad - nivel ", lvl),
        subtitle = paste0("Nivel ", lvl, " | página ", i, " de ", length(pages),
                          " | 4 columnas por página | Y fija dentro de causa, libre entre causas")
      )
    })
    
    plot_list <- c(plot_list, lev_plots)
  }
  
  write_multipage_pdf(plot_list, out_file, width = CFG$width_age_landscape, height = CFG$height_landscape)
}

build_pdf_avp_time_all_levels <- function(prep,
                                          out_file = file.path(CFG$out_dir, "avp_rate_time_all_levels.pdf"),
                                          causes_per_page = CFG$causes_per_page,
                                          top_n_per_level = NULL,
                                          sort_mode = "concept_id") {
  plot_list <- list()
  
  for (lvl in c(4L, 3L, 2L, 1L, 0L)) {
    ids <- get_level_cause_ids(prep, cause_level = lvl, top_n = top_n_per_level, sort_mode = sort_mode)
    pages <- split_into_pages(ids, causes_per_page)
    
    lev_plots <- lapply(seq_along(pages), function(i) {
      plot_time_trend(
        dt = prep$avp,
        cause_ids = pages[[i]],
        title = paste0("AVP por año - nivel ", lvl),
        subtitle = paste0("Nivel ", lvl, " | página ", i, " de ", length(pages),
                          " | Y libre entre causas")
      )
    })
    
    plot_list <- c(plot_list, lev_plots)
  }
  
  write_multipage_pdf(plot_list, out_file)
}

# ------------------------------------------------------------
# Runner
# ------------------------------------------------------------
run_iterative_diagnostics <- function(test_mode = TRUE) {
  prep <- prepare_dataset()
  qc <- compute_qc_flags(prep)
  level_cov <- summarize_level_coverage(prep)
  
  msg("QC generado en: ", CFG$qc_dir)
  
  if (isTRUE(test_mode)) {
    msg("Modo iterativo TEST: generando PDFs únicos de AVP con subconjunto de causas.")
    
    test_l4 <- get_level_cause_ids(prep, cause_level = 4L, top_n = 8L, sort_mode = "concept_id")
    test_l3 <- get_level_cause_ids(prep, cause_level = 3L, top_n = 8L, sort_mode = "concept_id")
    msg("Primeras causas L4 TEST: ", paste(prep$cause_stats[cause_concept_id %in% test_l4][order(cause_concept_id)]$cause_name, collapse = " | "))
    msg("Primeras causas L3 TEST: ", paste(prep$cause_stats[cause_concept_id %in% test_l3][order(cause_concept_id)]$cause_name, collapse = " | "))
    msg("Causas L1 disponibles: ", paste(prep$cause_stats[cause_level == 1L][order(cause_concept_id)]$cause_name, collapse = " | "))
    msg("Causas L1 covid/pandemia detectadas: ", paste(prep$cause_stats[cause_level == 1L & is_covid_related == TRUE][order(cause_concept_id)]$cause_name, collapse = " | "))
    
    build_pdf_avp_age_all_levels(
      prep,
      out_file = file.path(CFG$out_dir, "TEST_avp_rate_age_all_levels.pdf"),
      top_n_per_level = 8L,
      sort_mode = "concept_id"
    )
    
    build_pdf_avp_time_all_levels(
      prep,
      out_file = file.path(CFG$out_dir, "TEST_avp_rate_time_all_levels.pdf"),
      top_n_per_level = 8L,
      sort_mode = "concept_id"
    )
    
    return(invisible(list(prep = prep, qc = qc, level_cov = level_cov)))
  }
  
  msg("Modo FULL: generando PDFs únicos de AVP con todas las causas.")
  
  build_pdf_avp_age_all_levels(
    prep,
    out_file = file.path(CFG$out_dir, "avp_rate_age_all_levels.pdf"),
    top_n_per_level = NULL,
    sort_mode = "concept_id"
  )
  
  build_pdf_avp_time_all_levels(
    prep,
    out_file = file.path(CFG$out_dir, "avp_rate_time_all_levels.pdf"),
    top_n_per_level = NULL,
    sort_mode = "concept_id"
  )
  
  invisible(list(prep = prep, qc = qc, level_cov = level_cov))
}

# ------------------------------------------------------------
# Ejecución
# ------------------------------------------------------------
if (identical(environment(), globalenv())) {
  message("Script cargado. Ejecuta manualmente run_iterative_diagnostics(test_mode = TRUE) o run_iterative_diagnostics(test_mode = FALSE).")
}