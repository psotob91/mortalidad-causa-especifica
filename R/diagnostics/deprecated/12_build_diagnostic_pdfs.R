#!/usr/bin/env Rscript

# ============================================================
# diagnostico_mortalidad_pdfs.R
# ------------------------------------------------------------
# PDFs diagnósticos SOLO para mortalidad suavizada
# en Perú 2018-2024.
#
# Salidas:
#   1) mortality_age_all_levels.pdf
#      Un PDF único con muchas páginas para mortalidad por edad.
#   2) mortality_time_all_levels.pdf
#      Un PDF único con muchas páginas para mortalidad por año.
#
# En ambos PDFs:
#   - series por Hombre, Mujer y Ambos
#   - Ambos en color negro
#   - eje Y libre por panel
#   - páginas de 4-5 causas
#   - ordenadas por nivel jerárquico L4, L3, L2, L1, L0(total)
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(arrow)
  library(here)
  library(scales)
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
  version = "v0.2.0_mortality_only",
  dataset_id = "diagnostic_pdfs_mortality_only",
  
  input_mort_candidates = c(
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  input_model_registry_candidates = c(
    here("data", "derived", "qc", "09_build_mortality_rates", "mortality_model_registry.csv")
  ),
  
  input_roughness_candidates = c(
    here("data", "derived", "qc", "09_build_mortality_rates", "qc_temporal_roughness.csv")
  ),
  
  out_dir = here("outputs", "diagnostic_pdfs"),
  qc_dir  = here("data", "derived", "qc", "12_diagnostic_pdfs"),
  
  years = 2018:2024,
  location_id_national = 9000L,
  valid_sexes = c(8507L, 8532L),
  age_min = 0L,
  age_max = 110L,
  keep_cause_levels = c(4L, 3L, 2L, 1L, 0L),
  
  causes_per_page = 4L,
  min_total_deaths = 25,
  rate_multiplier = 100000,
  
  width_landscape = 14,
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
  if (isTRUE(CFG$verbose)) cat(..., "
")
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L || (length(x) == 1L && is.na(x))) y else x

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
        "
Columnas disponibles: ", paste(names(dt), collapse = ", ")
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

pretty_cause_label <- function(dt) {
  if (!"cause_label" %in% names(dt)) {
    dt[, cause_label := paste0(cause_name, "
[", cause_concept_id, "]")]
  }
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

# ------------------------------------------------------------
# Preparación de datos
# ------------------------------------------------------------
prepare_dataset <- function(location_id_target = CFG$location_id_national,
                            years = CFG$years,
                            cause_levels = CFG$keep_cause_levels) {
  
  mort_path  <- first_existing(CFG$input_mort_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  reg_path   <- first_existing(CFG$input_model_registry_candidates)
  rough_path <- first_existing(CFG$input_roughness_candidates)
  
  if (is.na(mort_path)) stop("No encontré mortality_rate_cause_smoothed_reconciled.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  
  msg("Leyendo mortalidad reconciliada...")
  mort <- read_dt_any(mort_path)
  
  msg("Leyendo cause_master...")
  cm <- read_dt_any(cause_path)
  
  reg <- if (!is.na(reg_path)) fread(reg_path) else data.table()
  rough <- if (!is.na(rough_path)) fread(rough_path) else data.table()
  
  req_mort <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_level", "population", "deaths_smoothed_consistent",
    "mortality_rate_smoothed_consistent"
  )
  miss_mort <- setdiff(req_mort, names(mort))
  if (length(miss_mort) > 0L) stop("Faltan columnas en mort: ", paste(miss_mort, collapse = ", "))
  
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
  
  cm <- unique(cm[, .(
    cause_concept_id,
    cause_level,
    cause_name,
    parent_concept_id,
    is_terminal,
    is_residual
  )])
  
  mort <- mort[
    location_id == location_id_target &
      year_id %in% years &
      sex_id %in% CFG$valid_sexes &
      age >= CFG$age_min & age <= CFG$age_max &
      cause_level %in% cause_levels
  ]
  
  mort <- merge(mort, cm, by = c("cause_concept_id", "cause_level"), all.x = TRUE, sort = FALSE, suffixes = c("", "_cm"))
  if (!"cause_name" %in% names(mort) && "cause_name_cm" %in% names(mort)) {
    setnames(mort, "cause_name_cm", "cause_name")
  }
  if ("cause_name_cm" %in% names(mort) && "cause_name" %in% names(mort)) {
    mort[, cause_name := fifelse(is.na(cause_name) | cause_name == "", cause_name_cm, cause_name)]
    mort[, cause_name_cm := NULL]
  }
  if (!"cause_name" %in% names(mort)) mort[, cause_name := paste0("cause_", cause_concept_id)]
  
  mort[, sex_label := sex_label_fun(sex_id)]
  mort <- pretty_cause_label(mort)
  
  if (nrow(reg) > 0L) {
    keep_reg <- intersect(
      c("cause_concept_id", "method_selected", "data_category",
        "years_with_deaths", "regions_with_deaths", "total_deaths_input"),
      names(reg)
    )
    reg2 <- unique(reg[, ..keep_reg])
    mort <- merge(mort, reg2, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  }
  
  if (nrow(rough) > 0L) {
    keep_r <- intersect(c("cause_concept_id", "sex_id", "roughness_score"), names(rough))
    if (length(keep_r) >= 2L) {
      rough2 <- unique(rough[, ..keep_r])
      mort <- merge(mort, rough2, by = intersect(c("cause_concept_id", "sex_id"), names(rough2)), all.x = TRUE, sort = FALSE)
    }
  }
  
  cause_stats <- mort[, .(
    cause_name = {
      zz <- na.omit(cause_name)
      if (length(zz) == 0L) paste0("cause_", cause_concept_id[1]) else zz[1]
    },
    cause_level = {
      zz <- na.omit(cause_level)
      if (length(zz) == 0L) NA_integer_ else zz[1]
    },
    total_deaths = sum(deaths_smoothed_consistent, na.rm = TRUE),
    max_rate = suppressWarnings(max(mortality_rate_smoothed_consistent, na.rm = TRUE)),
    n_cells = .N
  ), by = cause_concept_id]
  
  list(
    mort = mort,
    cm = cm,
    cause_stats = cause_stats,
    model_registry = reg,
    roughness = rough
  )
}

filter_causes_auto <- function(cause_stats,
                               cause_level,
                               include_total = FALSE,
                               min_total_deaths = CFG$min_total_deaths,
                               top_n = NULL,
                               exclude_residual = FALSE,
                               cm = NULL) {
  
  x <- copy(cause_stats)[cause_level == !!cause_level]
  x[is.na(total_deaths), total_deaths := 0]
  x[is.na(max_rate), max_rate := 0]
  x <- x[total_deaths >= min_total_deaths]
  
  if (!include_total) x <- x[cause_level != 0L]
  
  if (exclude_residual && !is.null(cm) && "is_residual" %in% names(cm)) {
    x <- merge(x, cm[, .(cause_concept_id, is_residual)], by = "cause_concept_id", all.x = TRUE)
    x <- x[is.na(is_residual) | is_residual == FALSE]
  }
  
  setorder(x, cause_level, -total_deaths, cause_name)
  if (!is.null(top_n)) x <- x[seq_len(min(.N, top_n))]
  x[]
}

get_level_cause_ids <- function(prep, cause_level, top_n = NULL) {
  filter_causes_auto(
    prep$cause_stats,
    cause_level = cause_level,
    include_total = (cause_level == 0L),
    top_n = top_n,
    cm = prep$cm
  )$cause_concept_id
}

# ------------------------------------------------------------
# QC automático
# ------------------------------------------------------------
compute_qc_flags <- function(prep) {
  mort <- copy(prep$mort)
  
  qc_rate <- mort[, .(
    n_negative = sum(mortality_rate_smoothed_consistent < 0, na.rm = TRUE),
    max_rate = suppressWarnings(max(mortality_rate_smoothed_consistent, na.rm = TRUE)),
    n_hard = sum(mortality_rate_smoothed_consistent > CFG$qc_rate_hard_max, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id)]
  
  mort_all_age <- mort[, .(
    deaths = sum(deaths_smoothed_consistent, na.rm = TRUE),
    pop = sum(population, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id, year_id)]
  mort_all_age[, rate := safe_rate(deaths, pop, CFG$rate_multiplier)]
  setorder(mort_all_age, cause_concept_id, sex_id, year_id)
  mort_all_age[, lag_rate := shift(rate), by = .(cause_concept_id, sex_id)]
  mort_all_age[, ratio_jump := fifelse(!is.na(lag_rate) & lag_rate > 0, rate / lag_rate, NA_real_)]
  mort_all_age[, qc_jump_warn := !is.na(ratio_jump) & (ratio_jump >= CFG$qc_jump_ratio_warn | ratio_jump <= 1 / CFG$qc_jump_ratio_warn)]
  mort_all_age[, qc_jump_hard := !is.na(ratio_jump) & (ratio_jump >= CFG$qc_jump_ratio_hard | ratio_jump <= 1 / CFG$qc_jump_ratio_hard)]
  
  wide_sex <- dcast(
    mort[, .(rate = sum(mortality_rate_smoothed_consistent, na.rm = TRUE)), by = .(cause_concept_id, cause_name, cause_level, year_id, age, sex_label)],
    cause_concept_id + cause_name + cause_level + year_id + age ~ sex_label,
    value.var = "rate",
    fill = NA_real_
  )
  if (!"Hombre" %in% names(wide_sex)) wide_sex[, Hombre := NA_real_]
  if (!"Mujer" %in% names(wide_sex))  wide_sex[, Mujer := NA_real_]
  wide_sex[, sex_ratio_mf := fifelse(!is.na(Mujer) & Mujer > 0, Hombre / Mujer, NA_real_)]
  wide_sex[, qc_sex_warn := !is.na(sex_ratio_mf) & (sex_ratio_mf >= CFG$qc_sex_ratio_warn | sex_ratio_mf <= 1 / CFG$qc_sex_ratio_warn)]
  wide_sex[, qc_sex_hard := !is.na(sex_ratio_mf) & (sex_ratio_mf >= CFG$qc_sex_ratio_hard | sex_ratio_mf <= 1 / CFG$qc_sex_ratio_hard)]
  
  impossible_age <- mort[
    age < CFG$age_min | age > CFG$age_max |
      deaths_smoothed_consistent < 0 |
      mortality_rate_smoothed_consistent < 0,
    .N,
    by = .(cause_concept_id, cause_name, cause_level)
  ]
  
  fwrite(qc_rate, file.path(CFG$qc_dir, "qc_diag_rate_flags.csv"))
  fwrite(mort_all_age[qc_jump_warn == TRUE | qc_jump_hard == TRUE], file.path(CFG$qc_dir, "qc_diag_temporal_jumps.csv"))
  fwrite(wide_sex[qc_sex_warn == TRUE | qc_sex_hard == TRUE], file.path(CFG$qc_dir, "qc_diag_sex_ratio_flags.csv"))
  fwrite(impossible_age, file.path(CFG$qc_dir, "qc_diag_impossible_age.csv"))
  
  list(
    qc_rate = qc_rate,
    qc_jump = mort_all_age,
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
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10),
      axis.text.x = element_text(size = 8),
      axis.text.y = element_text(size = 8)
    )
}

plot_age_distribution <- function(dt,
                                  cause_ids,
                                  title,
                                  subtitle = NULL,
                                  free_y = TRUE) {
  
  x <- copy(dt)[cause_concept_id %in% cause_ids]
  if (nrow(x) == 0L) return(NULL)
  
  x <- x[, .(
    deaths = sum(deaths_smoothed_consistent, na.rm = TRUE),
    pop = sum(population, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_label, cause_level, year_id, age, sex_label)]
  
  x <- append_both_sexes_from_agg(
    x,
    value_cols = c("deaths", "pop"),
    by_cols_no_sex = c("cause_concept_id", "cause_label", "cause_level", "year_id", "age")
  )
  
  x[, rate := safe_rate(deaths, pop, CFG$rate_multiplier)]
  x[, cause_label := factor(cause_label, levels = unique(x[order(cause_level, cause_label)]$cause_label))]
  x[, year_factor := factor(year_id, levels = sort(unique(year_id)))]
  x[, sex_label := factor(sex_label, levels = c("Hombre", "Mujer", "Ambos"))]
  
  ggplot(x, aes(x = age, y = rate, color = sex_label, group = sex_label)) +
    geom_line(linewidth = 0.55, alpha = 0.95) +
    facet_grid(year_factor ~ cause_label, scales = if (free_y) "free_y" else "fixed") +
    scale_color_manual(values = CFG$palette_sex, drop = FALSE) +
    scale_x_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 110)) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Edad simple",
      y = "Tasa de mortalidad por 100,000",
      color = "Sexo"
    ) +
    base_theme_diag()
}

plot_time_trend <- function(dt,
                            cause_ids,
                            title,
                            subtitle = NULL,
                            free_y = TRUE) {
  
  x <- copy(dt)[cause_concept_id %in% cause_ids]
  if (nrow(x) == 0L) return(NULL)
  
  x <- x[, .(
    deaths = sum(deaths_smoothed_consistent, na.rm = TRUE),
    pop = sum(population, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_label, cause_level, sex_label, year_id)]
  
  x <- append_both_sexes_from_agg(
    x,
    value_cols = c("deaths", "pop"),
    by_cols_no_sex = c("cause_concept_id", "cause_label", "cause_level", "year_id")
  )
  
  x[, rate := safe_rate(deaths, pop, CFG$rate_multiplier)]
  x[, sex_label := factor(sex_label, levels = c("Hombre", "Mujer", "Ambos"))]
  
  ggplot(x, aes(x = year_id, y = rate, color = sex_label, group = sex_label)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.3) +
    facet_wrap(~ cause_label, scales = if (free_y) "free_y" else "fixed", ncol = 2) +
    scale_color_manual(values = CFG$palette_sex, drop = FALSE) +
    scale_x_continuous(breaks = sort(unique(x$year_id))) +
    labs(
      title = title,
      subtitle = subtitle,
      x = "Año",
      y = "Tasa de mortalidad por 100,000",
      color = "Sexo"
    ) +
    base_theme_diag()
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

build_pdf_mortality_age_all_levels <- function(prep,
                                               out_file = file.path(CFG$out_dir, "mortality_age_all_levels.pdf"),
                                               causes_per_page = CFG$causes_per_page,
                                               top_n_per_level = NULL) {
  plot_list <- list()
  
  for (lvl in c(4L, 3L, 2L, 1L, 0L)) {
    ids <- get_level_cause_ids(prep, cause_level = lvl, top_n = top_n_per_level)
    pages <- split_into_pages(ids, causes_per_page)
    
    lev_plots <- lapply(seq_along(pages), function(i) {
      plot_age_distribution(
        dt = prep$mort,
        cause_ids = pages[[i]],
        title = paste0("Mortalidad por edad - nivel ", lvl),
        subtitle = paste0("Nivel ", lvl, " | página ", i, " de ", length(pages))
      )
    })
    
    plot_list <- c(plot_list, lev_plots)
  }
  
  write_multipage_pdf(plot_list, out_file)
}

build_pdf_mortality_time_all_levels <- function(prep,
                                                out_file = file.path(CFG$out_dir, "mortality_time_all_levels.pdf"),
                                                causes_per_page = CFG$causes_per_page,
                                                top_n_per_level = NULL) {
  plot_list <- list()
  
  for (lvl in c(4L, 3L, 2L, 1L, 0L)) {
    ids <- get_level_cause_ids(prep, cause_level = lvl, top_n = top_n_per_level)
    pages <- split_into_pages(ids, causes_per_page)
    
    lev_plots <- lapply(seq_along(pages), function(i) {
      plot_time_trend(
        dt = prep$mort,
        cause_ids = pages[[i]],
        title = paste0("Mortalidad por año - nivel ", lvl),
        subtitle = paste0("Nivel ", lvl, " | página ", i, " de ", length(pages))
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
  
  msg("QC generado en: ", CFG$qc_dir)
  
  if (isTRUE(test_mode)) {
    msg("Modo iterativo TEST: generando PDFs únicos de mortalidad con subconjunto de causas.")
    
    build_pdf_mortality_age_all_levels(
      prep,
      out_file = file.path(CFG$out_dir, "TEST_mortality_age_all_levels.pdf"),
      top_n_per_level = 8L
    )
    
    build_pdf_mortality_time_all_levels(
      prep,
      out_file = file.path(CFG$out_dir, "TEST_mortality_time_all_levels.pdf"),
      top_n_per_level = 8L
    )
    
    return(invisible(list(prep = prep, qc = qc)))
  }
  
  msg("Modo FULL: generando PDFs únicos de mortalidad con todas las causas.")
  
  build_pdf_mortality_age_all_levels(
    prep,
    out_file = file.path(CFG$out_dir, "mortality_age_all_levels.pdf"),
    top_n_per_level = NULL
  )
  
  build_pdf_mortality_time_all_levels(
    prep,
    out_file = file.path(CFG$out_dir, "mortality_time_all_levels.pdf"),
    top_n_per_level = NULL
  )
  
  invisible(list(prep = prep, qc = qc))
}

# ------------------------------------------------------------
# Ejecución
# ------------------------------------------------------------
if (identical(environment(), globalenv())) {
  run_iterative_diagnostics(test_mode = TRUE)
}
