#!/usr/bin/env Rscript

# ============================================================
# diagnostico_mortalidad_qc_extra_bloque1_pdfs.R
# ------------------------------------------------------------
# Bloque 1 de QC complementario para mortalidad suavizada
# Perú 2018-2024
#
# Productos:
#   1) Heatmap edad-año de tasa de mortalidad (por sexo)
#   2) Ratio Hombre/Mujer por edad
#   3) Edad mediana de muerte por causa y año
#   4) Flags QC y resumen de priorización de revisión
#
# Principios:
#   - Script independiente, prudente y modular
#   - Reusa convenciones del proyecto cuando conviene
#   - No usa dplyr
#   - No implementa todavía observado vs suavizado
#   - No implementa todavía cruda vs estandarizada
#   - No implementa todavía curva acumulada por edad
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(patchwork)
  library(arrow)
  library(here)
  library(scales)
})

# ============================================================
# Carga opcional de utils del proyecto
# ============================================================
source_if_exists <- function(path) {
  if (file.exists(path)) {
    source(path)
    return(TRUE)
  }
  FALSE
}

loaded_io      <- source_if_exists(here("R", "io_utils.R"))
loaded_catalog <- source_if_exists(here("R", "catalog_utils.R"))
loaded_spec    <- source_if_exists(here("R", "spec_utils.R"))
loaded_age     <- source_if_exists(here("R", "age_utils.R"))
loaded_qc      <- source_if_exists(here("R", "qc_utils.R"))
loaded_maestro <- source_if_exists(here("R", "maestro_utils.R"))

if (!exists("read_auto")) {
  read_auto <- function(path, ...) {
    ext <- tolower(tools::file_ext(path))
    switch(
      ext,
      csv = data.table::fread(path, ...),
      parquet = arrow::read_parquet(path, as_data_frame = FALSE),
      rds = readRDS(path),
      stop("Extensión no soportada: ", ext)
    )
  }
}

if (!exists("write_csv_parquet")) {
  write_csv_parquet <- function(dt, csv_path = NULL, parquet_path = NULL) {
    if (!is.null(csv_path)) data.table::fwrite(dt, csv_path)
    if (!is.null(parquet_path)) arrow::write_parquet(dt, parquet_path)
    invisible(TRUE)
  }
}

if (!exists("ensure_project_dirs")) {
  ensure_project_dirs <- function() {
    invisible(TRUE)
  }
}

if (!exists("ensure_catalog_files")) {
  ensure_catalog_files <- function() {
    invisible(TRUE)
  }
}

if (!exists("register_run_start")) {
  register_run_start <- function(run_id, dataset_id, version) invisible(TRUE)
}

if (!exists("register_run_finish")) {
  register_run_finish <- function(run_id, status = c("success", "failed"), message = NA_character_) invisible(TRUE)
}

if (!exists("register_artifact")) {
  register_artifact <- function(dataset_id, table_name, version, run_id,
                                artifact_type, artifact_path,
                                n_rows = NA_integer_, n_cols = NA_integer_,
                                notes = NA_character_) invisible(TRUE)
}

# ============================================================
# Configuración
# ============================================================
CFG <- list(
  version = "v0.1.0_qc_extra_bloque1",
  dataset_id = "mortality_qc_extra_bloque1",
  table_name = "mortality_qc_extra_bloque1",
  
  input_mort_candidates = c(
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  output_pdf_dir = here("outputs", "diagnostic_pdfs_qc"),
  output_qc_dir  = here("data", "derived", "qc", "12_diagnostic_pdfs_extra_bloque1"),
  
  years = 2018:2024,
  location_id_national = 9000L,
  sex_ids = c(8507L, 8532L),
  age_min = 0L,
  age_max = 110L,
  cause_levels = c(1L, 2L, 3L, 4L),
  rate_multiplier = 100000,
  
  test_mode = TRUE,
  test_n_causes = 12L,
  test_seed = 20260316L,
  
  pdf_width = 12,
  pdf_height = 8,
  base_size = 11,
  summary_table_max_rows = 24L,
  summary_table_text_size = 3.2,
  heatmap_palette = c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1", "#6baed6", "#3182bd", "#08519c"),
  
  # Heurísticos QC: conservadores y trazables
  min_nonzero_deaths_for_ratio = 3,
  min_population_for_ratio = 100,
  ratio_extreme_hi = 5,
  ratio_extreme_lo = 0.2,
  ratio_serrucho_threshold = 0.75,
  ratio_crossing_threshold = 4L,
  
  heatmap_temporal_jump_log_threshold = 1.25,
  heatmap_age_jump_log_threshold = 1.35,
  heatmap_high_roughness_share_threshold = 0.15,
  age_jump_years_threshold = 10,
  
  review_high_n_flags = 3L,
  review_medium_n_flags = 1L,
  
  verbose = TRUE
)

for (d in c(CFG$output_pdf_dir, CFG$output_qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# Helpers generales
# ============================================================
msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

safe_num <- function(x) suppressWarnings(as.numeric(x))
safe_int <- function(x) suppressWarnings(as.integer(x))

safe_rate <- function(num, den, mult = CFG$rate_multiplier) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- mult * num[ok] / den[ok]
  out
}

safe_log1p <- function(x) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & is.finite(x) & x >= 0
  out[ok] <- log1p(x[ok])
  out
}

clip01 <- function(x) pmax(0, pmin(1, x))

as_chr_na <- function(x) {
  y <- as.character(x)
  y[is.na(y)] <- NA_character_
  y
}

build_dictionary_ext <- function(dt) {
  data.table(
    variable = names(dt),
    tipo = vapply(dt, function(x) class(x)[1], character(1)),
    n = nrow(dt),
    n_missing = vapply(dt, function(x) sum(is.na(x)), integer(1)),
    n_distinct = vapply(dt, function(x) uniqueN(x), integer(1)),
    example_values = vapply(dt, function(x) {
      vals <- unique(na.omit(as.character(x)))
      paste(head(vals, 5), collapse = " | ")
    }, character(1))
  )
}

sex_label_fun <- function(sex_id) {
  fifelse(
    sex_id == 8507L, "Hombre",
    fifelse(sex_id == 8532L, "Mujer", as.character(sex_id))
  )
}

pick_col <- function(nms, candidates, label) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0L) {
    stop("No se encontró columna para ", label, ". Candidatas: ", paste(candidates, collapse = ", "))
  }
  hit[1]
}

weighted_median_age <- function(age, weight) {
  ok <- !is.na(age) & !is.na(weight) & is.finite(age) & is.finite(weight) & weight > 0
  if (!any(ok)) return(NA_real_)
  
  x <- data.table(age = as.numeric(age[ok]), weight = as.numeric(weight[ok]))
  setorder(x, age)
  x[, cum_w := cumsum(weight)]
  total_w <- sum(x$weight)
  if (!is.finite(total_w) || total_w <= 0) return(NA_real_)
  x[cum_w >= 0.5 * total_w, age][1]
}

collapse_flag_text <- function(x) {
  x <- unique(na.omit(as.character(x)))
  x <- x[nzchar(x)]
  if (length(x) == 0L) return(NA_character_)
  paste(sort(x), collapse = " | ")
}

compress_cause_title <- function(cause_name, cause_id, cause_level) {
  paste0(cause_name, " | ID ", cause_id, " | L", cause_level)
}

qc_note_heatmap <- paste(
  "Qué muestra: patrón edad-año de tasas suavizadas por sexo.",
  "Esperable: gradientes relativamente continuos y cambios temporales plausibles.",
  "Señal de problema: bandas verticales/horizontales abruptas, dientes de sierra o quiebres aislados.",
  "Posibles explicaciones: cambio de captación, sparsity, sobre/suavizado insuficiente, reconciliación jerárquica, pandemia real 2020-2021.",
  "Revisar: parámetros GAM, celdas de baja información, reconciliación, ceros estructurales y factores de corrección.",
  sep = "\n"
)

qc_note_ratio <- paste(
  "Qué muestra: razón Hombre/Mujer por edad, con líneas anuales y una línea resumen.",
  "Esperable: perfil etario relativamente estable salvo causas con fuerte dimorfismo sexual.",
  "Señal de problema: ratios extremos, serrucho intenso, cruces repetidos o inversiones improbables.",
  "Posibles explicaciones: denominadores/eventos pequeños, clasificación por sexo, suavizado diferencial por sexo.",
  "Revisar: celdas con pocos eventos, población, restricciones del modelo por sexo y necesidad de truncar solo para visualización.",
  sep = "\n"
)

qc_note_median_age <- paste(
  "Qué muestra: edad mediana de muerte ponderada por muertes suavizadas.",
  "Esperable: trayectorias relativamente estables o con cambios epidemiológicamente plausibles.",
  "Señal de problema: saltos bruscos entre años o divergencias súbitas entre sexos.",
  "Posibles explicaciones: artefacto composicional, redistribución, roll-up, pandemia o cambio real en perfil etario.",
  "Revisar: composición por edad, total de muertes, coherencia con AVP y causas pandemia/COVID.",
  sep = "\n"
)

# ============================================================
# Lectura y preparación de insumos
# ============================================================
read_inputs <- function() {
  mort_path <- first_existing(CFG$input_mort_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  
  if (is.na(mort_path)) stop("No encontré mortality_rate_cause_smoothed_reconciled.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  
  msg("Leyendo mortalidad reconciliada: ", mort_path)
  mort <- as.data.table(read_auto(mort_path))
  
  msg("Leyendo cause_master: ", cause_path)
  cm <- as.data.table(read_auto(cause_path))
  
  list(mort = mort, cm = cm, mort_path = mort_path, cause_path = cause_path)
}

prepare_data <- function(mort, cm) {
  c_year  <- pick_col(names(mort), c("year_id", "year"), "year_id")
  c_loc   <- pick_col(names(mort), c("location_id", "location"), "location_id")
  c_sex   <- pick_col(names(mort), c("sex_id", "sex"), "sex_id")
  c_age   <- pick_col(names(mort), c("age", "age_start", "exact_age"), "age")
  c_ccid  <- pick_col(names(mort), c("cause_concept_id", "cause_id"), "cause_concept_id")
  c_clvl  <- pick_col(names(mort), c("cause_level"), "cause_level")
  c_pop   <- pick_col(names(mort), c("population"), "population")
  c_dsc   <- pick_col(names(mort), c("deaths_smoothed_consistent", "deaths_smoothed", "deaths_final"), "deaths_smoothed_consistent")
  c_mrsc  <- pick_col(names(mort), c("mortality_rate_smoothed_consistent", "mortality_rate_smoothed"), "mortality_rate_smoothed_consistent")
  
  c_cm_id    <- pick_col(names(cm), c("cause_concept_id"), "cause_master$cause_concept_id")
  c_cm_name  <- pick_col(names(cm), c("cause_name"), "cause_master$cause_name")
  c_cm_level <- pick_col(names(cm), c("cause_level"), "cause_master$cause_level")
  
  mort0 <- mort[, .(
    year_id = safe_int(get(c_year)),
    location_id = safe_int(get(c_loc)),
    sex_id = safe_int(get(c_sex)),
    age = safe_int(get(c_age)),
    cause_concept_id = safe_int(get(c_ccid)),
    cause_level = safe_int(get(c_clvl)),
    population = safe_num(get(c_pop)),
    deaths_smoothed_consistent = safe_num(get(c_dsc)),
    mortality_rate_smoothed_consistent = safe_num(get(c_mrsc))
  )]
  
  cm0 <- unique(cm[, .(
    cause_concept_id = safe_int(get(c_cm_id)),
    cause_name = as.character(get(c_cm_name)),
    cause_level = safe_int(get(c_cm_level))
  )], by = c("cause_concept_id"))
  
  mort0 <- merge(
    mort0,
    cm0,
    by = c("cause_concept_id", "cause_level"),
    all.x = TRUE,
    sort = FALSE
  )
  
  mort0[, sex_label := sex_label_fun(sex_id)]
  
  mort0 <- mort0[
    year_id %in% CFG$years &
      location_id == CFG$location_id_national &
      sex_id %in% CFG$sex_ids &
      age >= CFG$age_min & age <= CFG$age_max &
      cause_level %in% CFG$cause_levels
  ]
  
  # prudencia: si la tasa viniera faltante pero sí muertes+población, recalcular
  mort0[
    (is.na(mortality_rate_smoothed_consistent) | !is.finite(mortality_rate_smoothed_consistent)) &
      !is.na(deaths_smoothed_consistent) & !is.na(population) & population > 0,
    mortality_rate_smoothed_consistent := safe_rate(deaths_smoothed_consistent, population)
  ]
  
  # eliminar filas fuera de semántica mínima
  mort0 <- mort0[
    !is.na(cause_concept_id) & !is.na(cause_level) & !is.na(cause_name) &
      !is.na(year_id) & !is.na(age) & !is.na(sex_id)
  ]
  
  setorder(mort0, cause_level, cause_name, cause_concept_id, sex_id, year_id, age)
  
  dup_pk <- mort0[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]
  if (nrow(dup_pk) > 0L) {
    stop("PK duplicada en dataset de mortalidad preparado para QC. Revisar reconciliado.")
  }
  
  mort0[]
}

select_causes <- function(mort_dt) {
  cause_dt <- unique(mort_dt[, .(cause_concept_id, cause_name, cause_level)])
  setorder(cause_dt, cause_level, cause_name, cause_concept_id)
  
  if (isTRUE(CFG$test_mode)) {
    set.seed(CFG$test_seed)
    
    # prioriza causas con más masa para que el test sea más útil visualmente
    cause_mass <- mort_dt[, .(
      total_deaths = sum(deaths_smoothed_consistent, na.rm = TRUE)
    ), by = .(cause_concept_id, cause_name, cause_level)]
    setorder(cause_mass, cause_level, -total_deaths, cause_name)
    
    picked <- cause_mass[1:min(.N, CFG$test_n_causes)]
    cause_dt <- picked[, .(cause_concept_id, cause_name, cause_level)]
  }
  
  cause_dt[]
}

# ============================================================
# Métricas QC derivadas
# ============================================================
compute_heatmap_metrics <- function(dt_cause) {
  x <- copy(dt_cause)
  x[, log_rate := safe_log1p(mortality_rate_smoothed_consistent)]
  setorder(x, sex_id, age, year_id)
  
  x[, d_time_log := c(NA_real_, diff(log_rate)), by = .(sex_id, age)]
  x[, d_time_abs := abs(d_time_log)]
  
  setorder(x, sex_id, year_id, age)
  x[, d_age_log := c(NA_real_, diff(log_rate)), by = .(sex_id, year_id)]
  x[, d_age_abs := abs(d_age_log)]
  
  flags <- x[, .(
    max_temporal_jump_log = suppressWarnings(max(d_time_abs, na.rm = TRUE)),
    max_age_jump_log = suppressWarnings(max(d_age_abs, na.rm = TRUE)),
    share_high_temporal_jump = mean(d_time_abs > CFG$heatmap_temporal_jump_log_threshold, na.rm = TRUE),
    share_high_age_jump = mean(d_age_abs > CFG$heatmap_age_jump_log_threshold, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id, sex_label)]
  
  flags[!is.finite(max_temporal_jump_log), max_temporal_jump_log := NA_real_]
  flags[!is.finite(max_age_jump_log), max_age_jump_log := NA_real_]
  flags[!is.finite(share_high_temporal_jump), share_high_temporal_jump := NA_real_]
  flags[!is.finite(share_high_age_jump), share_high_age_jump := NA_real_]
  
  flags[, qc_heatmap_temporal_break :=
          !is.na(max_temporal_jump_log) &
          (max_temporal_jump_log >= CFG$heatmap_temporal_jump_log_threshold |
             share_high_temporal_jump >= CFG$heatmap_high_roughness_share_threshold)]
  
  flags[, qc_heatmap_age_break :=
          !is.na(max_age_jump_log) &
          (max_age_jump_log >= CFG$heatmap_age_jump_log_threshold |
             share_high_age_jump >= CFG$heatmap_high_roughness_share_threshold)]
  
  flags[, flag_type := fifelse(
    qc_heatmap_temporal_break & qc_heatmap_age_break, "qc_heatmap_temporal_break|qc_heatmap_age_break",
    fifelse(qc_heatmap_temporal_break, "qc_heatmap_temporal_break",
            fifelse(qc_heatmap_age_break, "qc_heatmap_age_break", NA_character_))
  )]
  
  list(cell = x, flags = flags)
}

compute_ratio_metrics <- function(dt_cause) {
  x <- dcast(
    dt_cause[, .(year_id, age, sex_id, mortality_rate_smoothed_consistent, deaths_smoothed_consistent, population)],
    year_id + age ~ sex_id,
    value.var = c("mortality_rate_smoothed_consistent", "deaths_smoothed_consistent", "population")
  )
  
  m_rate <- "mortality_rate_smoothed_consistent_8507"
  f_rate <- "mortality_rate_smoothed_consistent_8532"
  m_dth  <- "deaths_smoothed_consistent_8507"
  f_dth  <- "deaths_smoothed_consistent_8532"
  m_pop  <- "population_8507"
  f_pop  <- "population_8532"
  
  needed <- c(m_rate, f_rate, m_dth, f_dth, m_pop, f_pop)
  for (nm in needed) if (!nm %in% names(x)) x[, (nm) := NA_real_]
  
  x[, eligible_ratio :=
      !is.na(get(m_rate)) & !is.na(get(f_rate)) &
      get(m_rate) >= 0 & get(f_rate) >= 0 &
      get(f_rate) > 0 &
      get(m_dth) >= CFG$min_nonzero_deaths_for_ratio &
      get(f_dth) >= CFG$min_nonzero_deaths_for_ratio &
      get(m_pop) >= CFG$min_population_for_ratio &
      get(f_pop) >= CFG$min_population_for_ratio]
  
  x[, ratio_hm := fifelse(eligible_ratio, get(m_rate) / get(f_rate), NA_real_)]
  x[, ratio_hm_log := fifelse(!is.na(ratio_hm) & ratio_hm > 0, log(ratio_hm), NA_real_)]
  
  med <- x[, .(
    ratio_hm_summary = median(ratio_hm, na.rm = TRUE)
  ), by = age]
  med[!is.finite(ratio_hm_summary), ratio_hm_summary := NA_real_]
  
  setorder(x, year_id, age)
  x[, d_ratio_abs := c(NA_real_, abs(diff(ratio_hm_log))), by = year_id]
  
  annual <- x[, .(
    max_ratio = suppressWarnings(max(ratio_hm, na.rm = TRUE)),
    min_ratio = suppressWarnings(min(ratio_hm, na.rm = TRUE)),
    median_abs_step_log = median(d_ratio_abs, na.rm = TRUE),
    n_crossings_one = sum(diff(sign(ratio_hm - 1), lag = 1L) != 0, na.rm = TRUE)
  ), by = year_id]
  
  annual[!is.finite(max_ratio), max_ratio := NA_real_]
  annual[!is.finite(min_ratio), min_ratio := NA_real_]
  annual[!is.finite(median_abs_step_log), median_abs_step_log := NA_real_]
  
  flags <- annual[, .(
    qc_sex_ratio_extreme = any(max_ratio >= CFG$ratio_extreme_hi | min_ratio <= CFG$ratio_extreme_lo, na.rm = TRUE),
    qc_sex_ratio_serrucho = any(median_abs_step_log >= CFG$ratio_serrucho_threshold, na.rm = TRUE),
    qc_sex_ratio_many_crossings = any(n_crossings_one >= CFG$ratio_crossing_threshold, na.rm = TRUE)
  )]
  
  flags[, flag_type := fifelse(
    qc_sex_ratio_extreme & qc_sex_ratio_serrucho, "qc_sex_ratio_extreme|qc_sex_ratio_serrucho",
    fifelse(qc_sex_ratio_extreme, "qc_sex_ratio_extreme",
            fifelse(qc_sex_ratio_serrucho, "qc_sex_ratio_serrucho",
                    fifelse(qc_sex_ratio_many_crossings, "qc_sex_ratio_many_crossings", NA_character_)))
  )]
  
  list(curves = x, summary = med, flags = flags, annual = annual)
}

compute_median_age_metrics <- function(dt_cause) {
  x <- dt_cause[, .(
    median_age_death = weighted_median_age(age, deaths_smoothed_consistent),
    total_deaths = sum(deaths_smoothed_consistent, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id, sex_label, year_id)]
  
  setorder(x, sex_id, year_id)
  x[, jump_abs := c(NA_real_, abs(diff(median_age_death))), by = sex_id]
  
  flags <- x[, .(
    max_jump_abs = suppressWarnings(max(jump_abs, na.rm = TRUE)),
    n_large_jumps = sum(jump_abs >= CFG$age_jump_years_threshold, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, cause_level, sex_id, sex_label)]
  
  flags[!is.finite(max_jump_abs), max_jump_abs := NA_real_]
  flags[, qc_median_age_jump := !is.na(max_jump_abs) & max_jump_abs >= CFG$age_jump_years_threshold]
  flags[, flag_type := fifelse(qc_median_age_jump, "qc_median_age_jump", NA_character_)]
  
  list(series = x, flags = flags)
}

# ============================================================
# Gráficos
# ============================================================
plot_heatmap_cause <- function(dt_cause, cause_row) {
  p <- ggplot(
    dt_cause,
    aes(x = year_id, y = age, fill = mortality_rate_smoothed_consistent)
  ) +
    geom_tile() +
    facet_wrap(~ sex_label, ncol = 2) +
    scale_x_continuous(breaks = CFG$years) +
    scale_y_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 110)) +
    scale_fill_gradientn(
      colours = CFG$heatmap_palette,
      na.value = "grey90",
      labels = label_number(accuracy = 0.1, big.mark = ",")
    ) +
    labs(
      title = compress_cause_title(cause_row$cause_name, cause_row$cause_concept_id, cause_row$cause_level),
      subtitle = "Heatmap edad-año de tasa de mortalidad suavizada consistente",
      x = "Año",
      y = "Edad simple",
      fill = "Tasa por\n100 mil",
      caption = qc_note_heatmap
    ) +
    theme_minimal(base_size = CFG$base_size) +
    theme(
      panel.grid = element_blank(),
      strip.text = element_text(face = "bold"),
      plot.title = element_text(face = "bold")
    )
  p
}

plot_ratio_cause <- function(curves_dt, summary_dt, cause_row) {
  curves_plot <- curves_dt[!is.na(ratio_hm) & is.finite(ratio_hm)]
  summary_plot <- summary_dt[!is.na(ratio_hm_summary) & is.finite(ratio_hm_summary)]
  
  p <- ggplot() +
    geom_line(
      data = curves_plot,
      aes(x = age, y = ratio_hm, group = year_id),
      linewidth = 0.35,
      alpha = 0.35
    ) +
    geom_line(
      data = summary_plot,
      aes(x = age, y = ratio_hm_summary),
      linewidth = 1.0
    ) +
    geom_hline(yintercept = 1, linetype = 2) +
    scale_x_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 110)) +
    scale_y_continuous(labels = label_number(accuracy = 0.1)) +
    labs(
      title = compress_cause_title(cause_row$cause_name, cause_row$cause_concept_id, cause_row$cause_level),
      subtitle = "Ratio Hombre/Mujer por edad: líneas anuales + perfil resumen mediano",
      x = "Edad simple",
      y = "Ratio H/M",
      caption = qc_note_ratio
    ) +
    theme_minimal(base_size = CFG$base_size) +
    theme(
      plot.title = element_text(face = "bold")
    )
  p
}

plot_median_age_cause <- function(series_dt, cause_row) {
  series_plot <- series_dt[!is.na(median_age_death) & is.finite(median_age_death)]
  
  p <- ggplot(
    series_plot,
    aes(x = year_id, y = median_age_death, colour = sex_label, group = sex_label)
  ) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.7) +
    scale_x_continuous(breaks = CFG$years) +
    scale_y_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85, 95, 105, 110)) +
    labs(
      title = compress_cause_title(cause_row$cause_name, cause_row$cause_concept_id, cause_row$cause_level),
      subtitle = "Edad mediana de muerte ponderada por muertes suavizadas",
      x = "Año",
      y = "Edad mediana de muerte",
      colour = "Sexo",
      caption = qc_note_median_age
    ) +
    theme_minimal(base_size = CFG$base_size) +
    theme(
      plot.title = element_text(face = "bold")
    )
  p
}

# ============================================================
# Consolidación de flags
# ============================================================
summarize_qc_findings <- function(qc_heatmap_flags,
                                  qc_ratio_flags,
                                  qc_median_age_flags,
                                  cause_subset = NULL) {
  hm <- unique(qc_heatmap_flags[
    (qc_heatmap_temporal_break | qc_heatmap_age_break),
    .(cause_concept_id, cause_name, cause_level, flag_type)
  ])
  
  ra <- unique(qc_ratio_flags[
    (qc_sex_ratio_extreme | qc_sex_ratio_serrucho | qc_sex_ratio_many_crossings),
    .(cause_concept_id, cause_name, cause_level, flag_type)
  ])
  
  ma <- unique(qc_median_age_flags[
    qc_median_age_jump == TRUE,
    .(cause_concept_id, cause_name, cause_level, flag_type)
  ])
  
  all_flags <- rbindlist(list(hm, ra, ma), use.names = TRUE, fill = TRUE)
  
  if (is.null(cause_subset)) {
    cause_subset <- unique(all_flags[, .(cause_concept_id, cause_name, cause_level)])
  } else {
    cause_subset <- unique(cause_subset[, .(cause_concept_id, cause_name, cause_level)])
  }
  
  out <- merge(
    cause_subset,
    all_flags,
    by = c("cause_concept_id", "cause_name", "cause_level"),
    all.x = TRUE,
    sort = FALSE
  )
  
  out <- out[, .(
    n_flags = sum(!is.na(flag_type)),
    flag_types = collapse_flag_text(flag_type)
  ), by = .(cause_concept_id, cause_name, cause_level)]
  
  out[, review_priority := fifelse(
    n_flags >= CFG$review_high_n_flags, "Alta",
    fifelse(n_flags >= CFG$review_medium_n_flags, "Media", "Baja")
  )]
  
  setorder(out, -n_flags, cause_level, cause_name, cause_concept_id)
  out[]
}

# ============================================================
# Export helpers
# ============================================================
export_qc_table <- function(dt, stem, run_id = NA_character_) {
  csv_path <- file.path(CFG$output_qc_dir, paste0(stem, ".csv"))
  dict_path <- file.path(CFG$output_qc_dir, paste0(stem, "_dictionary_ext.csv"))
  
  fwrite(dt, csv_path)
  fwrite(build_dictionary_ext(dt), dict_path)
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = stem,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "qc",
    artifact_path = csv_path,
    n_rows = nrow(dt),
    n_cols = ncol(dt),
    notes = "QC bloque 1 mortalidad"
  )
  
  invisible(list(csv = csv_path, dict = dict_path))
}

pdf_name <- function(stem) {
  prefix <- if (isTRUE(CFG$test_mode)) "TEST_" else ""
  file.path(CFG$output_pdf_dir, paste0(prefix, stem, ".pdf"))
}

format_summary_value <- function(x) {
  if (is.numeric(x)) {
    y <- ifelse(is.finite(x), format(round(x, 3), trim = TRUE, scientific = FALSE, big.mark = ","), NA_character_)
    y[is.na(x)] <- ""
    return(y)
  }
  y <- as.character(x)
  y[is.na(y)] <- ""
  y
}

wrap_text_for_table <- function(x, width = 28L) {
  # Inserta saltos de línea para que el texto no exceda el ancho visual
  y <- as.character(x)
  y[is.na(y)] <- ""
  
  vapply(y, function(s) {
    if (nchar(s) <= width) return(s)
    paste(strwrap(s, width = width), collapse = "\n")
  }, character(1))
}

count_table_lines <- function(x) {
  y <- as.character(x)
  y[is.na(y) | !nzchar(y)] <- ""
  vapply(strsplit(y, "\n", fixed = TRUE), function(z) max(1L, length(z)), integer(1))
}

build_table_plot <- function(dt, title, subtitle = NULL, max_rows = CFG$summary_table_max_rows) {
  dt0 <- copy(as.data.table(dt))
  
  if (nrow(dt0) == 0L) {
    dt0 <- data.table(Estado = "Sin flags en este bloque")
  }
  
  dt0 <- dt0[1:min(.N, max_rows)]
  
  for (j in names(dt0)) {
    dt0[, (j) := format_summary_value(get(j))]
    if (j %in% c("cause_name", "flag_types")) {
      dt0[, (j) := wrap_text_for_table(get(j), width = 32L)]
    }
  }
  
  dt0[, row_lines := 1L]
  for (j in setdiff(names(dt0), "row_lines")) {
    dt0[, row_lines := pmax(row_lines, count_table_lines(get(j)))]
  }
  
  dt0[, row_height := pmax(1.15, row_lines * 0.78)]
  dt0[, y := rev(cumsum(rev(row_height))) - (row_height / 2)]
  dt0[, fila_id := .I]
  
  cols <- setdiff(names(dt0), c("fila_id", "row_lines", "row_height", "y"))
  
  long <- melt(
    dt0[, c("fila_id", "y", cols), with = FALSE],
    id.vars = c("fila_id", "y"),
    variable.name = "columna",
    value.name = "valor",
    variable.factor = FALSE
  )
  long[, col_id := match(columna, cols)]
  
  header_y <- max(dt0$y + dt0$row_height / 2) + 0.95
  header <- data.table(
    columna = cols,
    col_id = seq_along(cols),
    y = header_y,
    valor = cols
  )
  
  p <- ggplot() +
    geom_text(
      data = header,
      aes(x = col_id, y = y, label = valor),
      fontface = "bold",
      size = CFG$summary_table_text_size,
      vjust = 0.5,
      lineheight = 0.95
    ) +
    geom_text(
      data = long,
      aes(x = col_id, y = y, label = valor),
      size = CFG$summary_table_text_size,
      vjust = 0.5,
      lineheight = 0.95
    ) +
    geom_hline(yintercept = header_y - 0.48, linewidth = 0.3) +
    scale_x_continuous(
      breaks = seq_along(cols),
      labels = cols,
      expand = expansion(mult = c(0.03, 0.03))
    ) +
    scale_y_continuous(
      breaks = NULL,
      expand = expansion(mult = c(0.02, 0.05))
    ) +
    coord_cartesian(clip = "off") +
    labs(
      title = title,
      subtitle = subtitle,
      x = NULL,
      y = NULL,
      caption = paste0("Tabla resumen; máximo ", max_rows, " filas mostradas por página.")
    ) +
    theme_void(base_size = CFG$base_size) +
    theme(
      plot.title = element_text(face = "bold", hjust = 0),
      plot.subtitle = element_text(hjust = 0),
      plot.caption = element_text(hjust = 0),
      plot.margin = margin(15, 30, 15, 30)
    )
  
  p
}

append_summary_table_pages <- function(pdf_type = c("heatmap", "ratio", "median", "priority"),
                                       qc_dt,
                                       causes_n = NA_integer_) {
  pdf_type <- match.arg(pdf_type)
  
  if (pdf_type == "heatmap") {
    dt_show <- copy(qc_dt)[order(-as.integer(qc_heatmap_temporal_break) - as.integer(qc_heatmap_age_break), cause_level, cause_name)]
    dt_show <- dt_show[, .(
      cause_level,
      cause_name,
      sex_label,
      qc_heatmap_temporal_break,
      qc_heatmap_age_break,
      max_temporal_jump_log,
      max_age_jump_log,
      share_high_temporal_jump,
      share_high_age_jump,
      flag_type
    )]
    print(build_table_plot(
      dt_show,
      title = "Resumen tabular de flags QC - Heatmap edad-año",
      subtitle = paste0("Causas procesadas: ", causes_n)
    ))
  }
  
  if (pdf_type == "ratio") {
    dt_show <- copy(qc_dt)[order(-as.integer(qc_sex_ratio_extreme) - as.integer(qc_sex_ratio_serrucho) - as.integer(qc_sex_ratio_many_crossings), cause_level, cause_name)]
    dt_show <- dt_show[, .(
      cause_level,
      cause_name,
      qc_sex_ratio_extreme,
      qc_sex_ratio_serrucho,
      qc_sex_ratio_many_crossings,
      flag_type
    )]
    print(build_table_plot(
      dt_show,
      title = "Resumen tabular de flags QC - Ratio Hombre/Mujer",
      subtitle = paste0("Causas procesadas: ", causes_n)
    ))
  }
  
  if (pdf_type == "median") {
    dt_show <- copy(qc_dt)[order(-as.integer(qc_median_age_jump), cause_level, cause_name, sex_label)]
    dt_show <- dt_show[, .(
      cause_level,
      cause_name,
      sex_label,
      qc_median_age_jump,
      max_jump_abs,
      n_large_jumps,
      flag_type
    )]
    print(build_table_plot(
      dt_show,
      title = "Resumen tabular de flags QC - Edad mediana de muerte",
      subtitle = paste0("Causas procesadas: ", causes_n)
    ))
  }
  
  if (pdf_type == "priority") {
    dt_show <- copy(qc_dt)[order(-n_flags, cause_level, cause_name)]
    dt_show <- dt_show[, .(
      cause_level,
      cause_name,
      n_flags,
      flag_types,
      review_priority
    )]
    print(build_table_plot(
      dt_show,
      title = "Resumen tabular de priorización de revisión QC",
      subtitle = paste0("Causas procesadas: ", causes_n)
    ))
  }
  
  invisible(TRUE)
}

# ============================================================
# Main
# ============================================================
run_qc_extra_bloque1 <- function(test_mode = CFG$test_mode) {
  CFG$test_mode <<- isTRUE(test_mode)
  
  ensure_project_dirs()
  ensure_catalog_files()
  
  run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)
  
  tryCatch({
    ins <- read_inputs()
    mort <- prepare_data(ins$mort, ins$cm)
    causes <- select_causes(mort)
    
    msg("N causas a procesar: ", nrow(causes))
    
    heatmap_pdf <- pdf_name("qc_heatmap_age_year_mortality")
    ratio_pdf   <- pdf_name("qc_sex_ratio_age_mortality")
    median_pdf  <- pdf_name("qc_median_age_death_mortality")
    summary_pdf <- pdf_name("qc_priority_review_summary_tables")
    
    heatmap_flags_list <- vector("list", nrow(causes))
    ratio_flags_list   <- vector("list", nrow(causes))
    median_flags_list  <- vector("list", nrow(causes))
    
    pdf(heatmap_pdf, width = CFG$pdf_width, height = CFG$pdf_height, onefile = TRUE)
    for (i in seq_len(nrow(causes))) {
      cr <- causes[i]
      dt_cause <- mort[cause_concept_id == cr$cause_concept_id]
      hm <- compute_heatmap_metrics(dt_cause)
      heatmap_flags_list[[i]] <- hm$flags
      print(plot_heatmap_cause(dt_cause, cr))
    }
    qc_heatmap_flags <- rbindlist(heatmap_flags_list, use.names = TRUE, fill = TRUE)
    append_summary_table_pages("heatmap", qc_heatmap_flags, causes_n = nrow(causes))
    dev.off()
    
    pdf(ratio_pdf, width = CFG$pdf_width, height = CFG$pdf_height, onefile = TRUE)
    for (i in seq_len(nrow(causes))) {
      cr <- causes[i]
      dt_cause <- mort[cause_concept_id == cr$cause_concept_id]
      rr <- compute_ratio_metrics(dt_cause)
      rf <- copy(rr$flags)
      rf[, `:=`(
        cause_concept_id = cr$cause_concept_id,
        cause_name = cr$cause_name,
        cause_level = cr$cause_level
      )]
      setcolorder(rf, c("cause_concept_id", "cause_name", "cause_level", setdiff(names(rf), c("cause_concept_id", "cause_name", "cause_level"))))
      ratio_flags_list[[i]] <- rf
      print(plot_ratio_cause(rr$curves, rr$summary, cr))
    }
    qc_sex_ratio_flags <- rbindlist(ratio_flags_list, use.names = TRUE, fill = TRUE)
    append_summary_table_pages("ratio", qc_sex_ratio_flags, causes_n = nrow(causes))
    dev.off()
    
    pdf(median_pdf, width = CFG$pdf_width, height = CFG$pdf_height, onefile = TRUE)
    for (i in seq_len(nrow(causes))) {
      cr <- causes[i]
      dt_cause <- mort[cause_concept_id == cr$cause_concept_id]
      ma <- compute_median_age_metrics(dt_cause)
      median_flags_list[[i]] <- ma$flags
      print(plot_median_age_cause(ma$series, cr))
    }
    qc_median_age_flags <- rbindlist(median_flags_list, use.names = TRUE, fill = TRUE)
    append_summary_table_pages("median", qc_median_age_flags, causes_n = nrow(causes))
    dev.off()
    
    qc_priority_review_summary <- summarize_qc_findings(
      qc_heatmap_flags = qc_heatmap_flags,
      qc_ratio_flags = qc_sex_ratio_flags,
      qc_median_age_flags = qc_median_age_flags,
      cause_subset = causes
    )
    
    export_qc_table(qc_heatmap_flags, "qc_heatmap_flags", run_id)
    export_qc_table(qc_sex_ratio_flags, "qc_sex_ratio_flags", run_id)
    export_qc_table(qc_median_age_flags, "qc_median_age_flags", run_id)
    export_qc_table(qc_priority_review_summary, "qc_priority_review_summary", run_id)
    
    pdf(summary_pdf, width = CFG$pdf_width, height = CFG$pdf_height, onefile = TRUE)
    append_summary_table_pages("priority", qc_priority_review_summary, causes_n = nrow(causes))
    append_summary_table_pages("heatmap", qc_heatmap_flags, causes_n = nrow(causes))
    append_summary_table_pages("ratio", qc_sex_ratio_flags, causes_n = nrow(causes))
    append_summary_table_pages("median", qc_median_age_flags, causes_n = nrow(causes))
    dev.off()
    
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = "qc_heatmap_age_year_mortality_pdf",
      version = CFG$version,
      run_id = run_id,
      artifact_type = "report",
      artifact_path = heatmap_pdf,
      notes = "Bloque 1 QC mortalidad"
    )
    
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = "qc_sex_ratio_age_mortality_pdf",
      version = CFG$version,
      run_id = run_id,
      artifact_type = "report",
      artifact_path = ratio_pdf,
      notes = "Bloque 1 QC mortalidad"
    )
    
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = "qc_median_age_death_mortality_pdf",
      version = CFG$version,
      run_id = run_id,
      artifact_type = "report",
      artifact_path = median_pdf,
      notes = "Bloque 1 QC mortalidad"
    )
    
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = "qc_priority_review_summary_tables_pdf",
      version = CFG$version,
      run_id = run_id,
      artifact_type = "report",
      artifact_path = summary_pdf,
      notes = "Resumen tabular QC bloque 1 mortalidad"
    )
    
    register_run_finish(run_id, status = "success", message = "QC bloque 1 completado")
    
    msg("============================================")
    msg("QC BLOQUE 1 COMPLETADO")
    msg("============================================")
    msg("PDF heatmap : ", heatmap_pdf)
    msg("PDF ratio   : ", ratio_pdf)
    msg("PDF mediana : ", median_pdf)
    msg("PDF tablas  : ", summary_pdf)
    msg("QC dir      : ", CFG$output_qc_dir)
    
    invisible(list(
      causes = causes,
      qc_heatmap_flags = qc_heatmap_flags,
      qc_sex_ratio_flags = qc_sex_ratio_flags,
      qc_median_age_flags = qc_median_age_flags,
      qc_priority_review_summary = qc_priority_review_summary,
      pdfs = list(heatmap = heatmap_pdf, ratio = ratio_pdf, median = median_pdf, summary = summary_pdf)
    ))
    
  }, error = function(e) {
    register_run_finish(run_id, status = "failed", message = as.character(e$message))
    stop(e)
  })
}

# ============================================================
# Ejecución directa
# ============================================================
if (sys.nframe() == 0L) {
  run_qc_extra_bloque1(test_mode = CFG$test_mode)
}
