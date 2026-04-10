#!/usr/bin/env Rscript

# ============================================================
# 08c_qc_completeness_validation.R
# ------------------------------------------------------------
# Objetivo:
#   Generar QC adicional para validar el ajuste por completitud
#   producido por 08_build_death_cause_final.R, usando el
#   artefacto:
#     data/derived/qc/08_build_death_cause_final/
#       mortality_completeness_audit.csv|parquet
#
# Enfoque:
#   1) No modifica el pipeline productivo.
#   2) Evalúa si el ajuste realmente mueve las muertes/tasas
#      observadas hacia el valor esperado.
#   3) Produce tablas y figuras listas para anexar a informe.
#
# Salidas:
#   - QC tabular: data/derived/qc/08c_qc_completeness_validation/
#   - Figuras:    reports/qc_completeness_validation/
#
# Nota:
#   Según el contrato del proyecto, la geografía analítica base
#   para modelamiento es departamental 1:25; el nacional del
#   input puede venir como 0, pero para análisis downstream se
#   recomienda reconstruir el nacional agregando departamentos.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
  library(yaml)
  library(ggplot2)
  library(scales)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))

CFG <- list(
  version = "v0.1.0_qc_completeness_validation",
  dataset_id = "death_cause_final_completeness_validation",
  table_name = "qc_completeness_validation",
  
  years = 2018:2024,
  valid_sexes = c(8507L, 8532L),
  dept_ids = 1:25,
  national_output_id = 9000L,
  
  editorial_year = 2024L,
  age_min = 0L,
  age_max = 110L,
  rate_multiplier = 100000,
  
  verbose = TRUE,
  
  input_candidates = c(
    resolve_existing_qc_path("build_death_cause_final", "mortality_completeness_audit.csv"),
    resolve_existing_qc_path("build_death_cause_final", "mortality_completeness_audit.parquet")
  ),
  
  qc_dir = qc_dir_path("qc_completeness_validation"),
  report_dir = here("reports", "qc_completeness_validation")
)

for (d in c(CFG$qc_dir, CFG$report_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

ensure_project_dirs()
ensure_catalog_files()

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

detect_col <- function(dt, candidates, label) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    stop(
      "No se encontró columna para ", label,
      ". Candidatas: ", paste(candidates, collapse = ", ")
    )
  }
  hit[1]
}

safe_rate <- function(num, den, mult = 100000) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- mult * num[ok] / den[ok]
  out
}

safe_ratio <- function(num, den) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- num[ok] / den[ok]
  out
}

safe_pct_diff <- function(x, ref) {
  out <- rep(NA_real_, length(x))
  ok <- !is.na(x) & !is.na(ref) & ref != 0
  out[ok] <- 100 * (x[ok] - ref[ok]) / ref[ok]
  out
}

sex_label <- function(x) {
  fifelse(
    x == 8507L, "Hombre",
    fifelse(x == 8532L, "Mujer", "Otro")
  )
}

make_age_label <- function(age) {
  fifelse(
    is.na(age), NA_character_,
    fifelse(age == 0L, "<1",
            fifelse(age %in% 1:4, "1-4",
                    fifelse(age >= 110L, "110+",
                            as.character(age)
                    )
            )
    )
  )
}

save_plot <- function(p, path, width = 10, height = 6, dpi = 300) {
  ggsave(
    filename = path,
    plot = p,
    width = width,
    height = height,
    dpi = dpi,
    units = "in",
    bg = "white"
  )
  invisible(path)
}

build_dictionary_ext <- function(dt) {
  data.table(
    variable = names(dt),
    tipo = vapply(dt, function(x) class(x)[1], character(1)),
    n = nrow(dt),
    n_missing = vapply(dt, function(x) sum(is.na(x)), integer(1)),
    n_distinct = vapply(dt, function(x) uniqueN(x), integer(1))
  )
}

tryCatch({
  
  # ----------------------------------------------------------
  # Input
  # ----------------------------------------------------------
  msg("Resolviendo insumo de auditoría de completitud...")
  
  input_path <- first_existing(CFG$input_candidates)
  if (is.na(input_path)) {
    stop("No se encontró mortality_completeness_audit.csv|parquet del script 08.")
  }
  
  msg("Leyendo audit: ", input_path)
  audit <- as.data.table(read_auto(input_path))
  
  # ----------------------------------------------------------
  # Detectar columnas esperadas
  # ----------------------------------------------------------
  col_year <- detect_col(audit, c("year_id"), "año")
  col_loc  <- detect_col(audit, c("location_id"), "ubicación")
  col_sex  <- detect_col(audit, c("sex_id"), "sexo")
  col_age  <- detect_col(audit, c("age"), "edad")
  col_pop  <- detect_col(audit, c("population"), "población")
  
  col_obs  <- detect_col(
    audit,
    c("observed_allcause", "deaths_observed_allcause", "observed_deaths_allcause"),
    "muertes observadas all-cause"
  )
  col_exp  <- detect_col(
    audit,
    c("expected_allcause", "deaths_expected", "expected_deaths_allcause"),
    "muertes esperadas all-cause"
  )
  col_corr <- detect_col(
    audit,
    c("observed_corrected_allcause", "corrected_allcause", "deaths_corrected_allcause"),
    "muertes corregidas all-cause"
  )
  col_fac  <- detect_col(
    audit,
    c("correction_factor_completeness", "factor_completeness", "completeness_factor"),
    "factor de completitud"
  )
  
  keep <- c(col_year, col_loc, col_sex, col_age, col_pop, col_obs, col_exp, col_corr, col_fac)
  dt <- copy(audit)[, ..keep]
  
  setnames(
    dt,
    old = c(col_year, col_loc, col_sex, col_age, col_pop, col_obs, col_exp, col_corr, col_fac),
    new = c("year_id", "location_id", "sex_id", "age", "population",
            "observed_allcause", "expected_allcause", "observed_corrected_allcause",
            "correction_factor_completeness")
  )
  
  # ----------------------------------------------------------
  # Normalización / filtros mínimos
  # ----------------------------------------------------------
  msg("Normalizando y filtrando grano analítico base...")
  
  dt[, year_id := as.integer(year_id)]
  dt[, location_id := as.integer(location_id)]
  dt[, sex_id := as.integer(sex_id)]
  dt[, age := as.integer(age)]
  dt[, population := as.numeric(population)]
  dt[, observed_allcause := as.numeric(observed_allcause)]
  dt[, expected_allcause := as.numeric(expected_allcause)]
  dt[, observed_corrected_allcause := as.numeric(observed_corrected_allcause)]
  dt[, correction_factor_completeness := as.numeric(correction_factor_completeness)]
  
  dt <- dt[
    year_id %in% CFG$years &
      sex_id %in% CFG$valid_sexes &
      age >= CFG$age_min &
      age <= CFG$age_max
  ]
  
  # Preferencia: usar departamentos 1:25 y reconstruir nacional.
  # Si el audit ya viene a nivel 1:25, esto evita depender de un nacional input 0.
  dt_dept <- dt[location_id %in% CFG$dept_ids]
  
  if (nrow(dt_dept) == 0L) {
    stop("El audit no contiene location_id departamental 1:25. Revisar salida de 08.")
  }
  
  # QC básico denominadores
  qc_invalid_population <- dt_dept[is.na(population) | population <= 0]
  fwrite(qc_invalid_population, file.path(CFG$qc_dir, "qc_invalid_population_rows.csv"))
  
  # ----------------------------------------------------------
  # Nacional reconstruido desde suma deptos
  # ----------------------------------------------------------
  msg("Reconstruyendo nacional analítico desde suma de departamentos...")
  
  nat_age_sex_year <- dt_dept[
    ,
    .(
      population = sum(population, na.rm = TRUE),
      observed_allcause = sum(observed_allcause, na.rm = TRUE),
      expected_allcause = sum(expected_allcause, na.rm = TRUE),
      observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
    ),
    by = .(year_id, sex_id, age)
  ]
  
  nat_age_sex_year[, location_id := CFG$national_output_id]
  nat_age_sex_year[, correction_factor_implied := safe_ratio(observed_corrected_allcause, observed_allcause)]
  nat_age_sex_year[, expected_observed_ratio := safe_ratio(expected_allcause, observed_allcause)]
  nat_age_sex_year[, corrected_expected_ratio := safe_ratio(observed_corrected_allcause, expected_allcause)]
  nat_age_sex_year[, observed_rate_100k := safe_rate(observed_allcause, population, CFG$rate_multiplier)]
  nat_age_sex_year[, expected_rate_100k := safe_rate(expected_allcause, population, CFG$rate_multiplier)]
  nat_age_sex_year[, corrected_rate_100k := safe_rate(observed_corrected_allcause, population, CFG$rate_multiplier)]
  nat_age_sex_year[, sex_label := sex_label(sex_id)]
  nat_age_sex_year[, age_label := make_age_label(age)]
  
  # ----------------------------------------------------------
  # Tabla nacional por año
  # ----------------------------------------------------------
  msg("Construyendo resumen nacional anual...")
  
  tab_nat_year <- nat_age_sex_year[
    ,
    .(
      population = sum(population, na.rm = TRUE),
      observed_allcause = sum(observed_allcause, na.rm = TRUE),
      expected_allcause = sum(expected_allcause, na.rm = TRUE),
      observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
    ),
    by = .(year_id)
  ]
  
  tab_nat_year[, observed_rate_100k := safe_rate(observed_allcause, population, CFG$rate_multiplier)]
  tab_nat_year[, expected_rate_100k := safe_rate(expected_allcause, population, CFG$rate_multiplier)]
  tab_nat_year[, corrected_rate_100k := safe_rate(observed_corrected_allcause, population, CFG$rate_multiplier)]
  tab_nat_year[, expected_observed_ratio := safe_ratio(expected_allcause, observed_allcause)]
  tab_nat_year[, corrected_expected_ratio := safe_ratio(observed_corrected_allcause, expected_allcause)]
  tab_nat_year[, abs_gap_observed_vs_expected := expected_allcause - observed_allcause]
  tab_nat_year[, abs_gap_corrected_vs_expected := observed_corrected_allcause - expected_allcause]
  tab_nat_year[, pct_gap_observed_vs_expected := safe_pct_diff(observed_allcause, expected_allcause)]
  tab_nat_year[, pct_gap_corrected_vs_expected := safe_pct_diff(observed_corrected_allcause, expected_allcause)]
  
  fwrite(tab_nat_year, file.path(CFG$qc_dir, "tab_national_year_summary.csv"))
  
  # ----------------------------------------------------------
  # Tabla por año y sexo (nacional)
  # ----------------------------------------------------------
  tab_nat_year_sex <- nat_age_sex_year[
    ,
    .(
      population = sum(population, na.rm = TRUE),
      observed_allcause = sum(observed_allcause, na.rm = TRUE),
      expected_allcause = sum(expected_allcause, na.rm = TRUE),
      observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
    ),
    by = .(year_id, sex_id, sex_label)
  ]
  
  tab_nat_year_sex[, observed_rate_100k := safe_rate(observed_allcause, population, CFG$rate_multiplier)]
  tab_nat_year_sex[, expected_rate_100k := safe_rate(expected_allcause, population, CFG$rate_multiplier)]
  tab_nat_year_sex[, corrected_rate_100k := safe_rate(observed_corrected_allcause, population, CFG$rate_multiplier)]
  tab_nat_year_sex[, expected_observed_ratio := safe_ratio(expected_allcause, observed_allcause)]
  tab_nat_year_sex[, corrected_expected_ratio := safe_ratio(observed_corrected_allcause, expected_allcause)]
  
  fwrite(tab_nat_year_sex, file.path(CFG$qc_dir, "tab_national_year_sex_summary.csv"))
  
  # ----------------------------------------------------------
  # Tabla 2024 por edad y sexo (nacional)
  # ----------------------------------------------------------
  tab_nat_age_sex_2024 <- copy(nat_age_sex_year[year_id == CFG$editorial_year])
  
  setcolorder(
    tab_nat_age_sex_2024,
    c(
      "year_id", "sex_id", "sex_label", "age", "age_label", "population",
      "observed_allcause", "expected_allcause", "observed_corrected_allcause",
      "observed_rate_100k", "expected_rate_100k", "corrected_rate_100k",
      "expected_observed_ratio", "corrected_expected_ratio", "correction_factor_implied"
    )
  )
  
  fwrite(tab_nat_age_sex_2024, file.path(CFG$qc_dir, "tab_national_age_sex_2024.csv"))
  
  # ----------------------------------------------------------
  # Tabla departamental por año
  # ----------------------------------------------------------
  msg("Construyendo resumen departamental anual...")
  
  tab_dept_year <- dt_dept[
    ,
    .(
      population = sum(population, na.rm = TRUE),
      observed_allcause = sum(observed_allcause, na.rm = TRUE),
      expected_allcause = sum(expected_allcause, na.rm = TRUE),
      observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE),
      factor_median = median(correction_factor_completeness, na.rm = TRUE),
      factor_p25 = quantile(correction_factor_completeness, probs = 0.25, na.rm = TRUE, names = FALSE, type = 7),
      factor_p75 = quantile(correction_factor_completeness, probs = 0.75, na.rm = TRUE, names = FALSE, type = 7),
      factor_max = max(correction_factor_completeness, na.rm = TRUE)
    ),
    by = .(year_id, location_id)
  ]
  
  tab_dept_year[, observed_rate_100k := safe_rate(observed_allcause, population, CFG$rate_multiplier)]
  tab_dept_year[, expected_rate_100k := safe_rate(expected_allcause, population, CFG$rate_multiplier)]
  tab_dept_year[, corrected_rate_100k := safe_rate(observed_corrected_allcause, population, CFG$rate_multiplier)]
  tab_dept_year[, expected_observed_ratio := safe_ratio(expected_allcause, observed_allcause)]
  tab_dept_year[, corrected_expected_ratio := safe_ratio(observed_corrected_allcause, expected_allcause)]
  
  fwrite(tab_dept_year, file.path(CFG$qc_dir, "tab_department_year_summary.csv"))
  
  # ----------------------------------------------------------
  # Resumen sintético de hallazgos QC
  # ----------------------------------------------------------
  msg("Armando resumen sintético QC...")
  
  qc_summary <- rbindlist(list(
    data.table(
      metric = "n_rows_input",
      value_num = nrow(dt),
      value_chr = as.character(nrow(dt))
    ),
    data.table(
      metric = "n_rows_dept",
      value_num = nrow(dt_dept),
      value_chr = as.character(nrow(dt_dept))
    ),
    data.table(
      metric = "n_invalid_population_rows",
      value_num = nrow(qc_invalid_population),
      value_chr = as.character(nrow(qc_invalid_population))
    ),
    data.table(
      metric = "editorial_year_min_factor_2024",
      value_num = suppressWarnings(min(dt_dept[year_id == CFG$editorial_year]$correction_factor_completeness, na.rm = TRUE)),
      value_chr = as.character(suppressWarnings(min(dt_dept[year_id == CFG$editorial_year]$correction_factor_completeness, na.rm = TRUE)))
    ),
    data.table(
      metric = "editorial_year_max_factor_2024",
      value_num = suppressWarnings(max(dt_dept[year_id == CFG$editorial_year]$correction_factor_completeness, na.rm = TRUE)),
      value_chr = as.character(suppressWarnings(max(dt_dept[year_id == CFG$editorial_year]$correction_factor_completeness, na.rm = TRUE)))
    ),
    data.table(
      metric = "all_factors_equal_one_2024",
      value_num = as.integer(all(dt_dept[year_id == CFG$editorial_year]$correction_factor_completeness == 1, na.rm = TRUE)),
      value_chr = ifelse(all(dt_dept[year_id == CFG$editorial_year]$correction_factor_completeness == 1, na.rm = TRUE), "yes", "no")
    ),
    data.table(
      metric = "national_expected_observed_ratio_2024",
      value_num = tab_nat_year[year_id == CFG$editorial_year, expected_observed_ratio],
      value_chr = as.character(tab_nat_year[year_id == CFG$editorial_year, expected_observed_ratio])
    ),
    data.table(
      metric = "national_corrected_expected_ratio_2024",
      value_num = tab_nat_year[year_id == CFG$editorial_year, corrected_expected_ratio],
      value_chr = as.character(tab_nat_year[year_id == CFG$editorial_year, corrected_expected_ratio])
    )
  ), fill = TRUE)
  
  fwrite(qc_summary, file.path(CFG$qc_dir, "qc_summary.csv"))
  
  # ----------------------------------------------------------
  # Figuras
  # ----------------------------------------------------------
  msg("Construyendo figuras...")
  
  # Figura 1: tasas nacionales por año
  fig1_dt <- melt(
    copy(tab_nat_year)[, .(
      year_id,
      observed_rate_100k,
      expected_rate_100k,
      corrected_rate_100k
    )],
    id.vars = "year_id",
    variable.name = "series",
    value.name = "rate_100k"
  )
  
  fig1_dt[, series := factor(
    series,
    levels = c("observed_rate_100k", "expected_rate_100k", "corrected_rate_100k"),
    labels = c("Observada", "Esperada", "Corregida")
  )]
  
  p1 <- ggplot(fig1_dt, aes(x = year_id, y = rate_100k, color = series)) +
    geom_line(linewidth = 0.9) +
    geom_point(size = 2) +
    scale_x_continuous(breaks = CFG$years) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".", accuracy = 0.1)) +
    labs(
      title = "Validación de completitud: tasa nacional all-cause por año",
      subtitle = "Comparación entre tasa observada, esperada y corregida",
      x = "Año",
      y = "Tasa por 100 000",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  fig1_path <- file.path(CFG$report_dir, "fig_qc_b1_national_rates_by_year.png")
  save_plot(p1, fig1_path, width = 10, height = 6)
  
  # Figura 2: razón expected/observed por edad y sexo, 2024
  fig2_dt <- copy(tab_nat_age_sex_2024)
  
  p2 <- ggplot(fig2_dt, aes(x = age, y = expected_observed_ratio, color = sex_label)) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    scale_x_continuous(breaks = seq(0, 110, by = 5)) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".", accuracy = 0.01)) +
    labs(
      title = "Razón esperada/observada por edad y sexo",
      subtitle = paste0("Nacional reconstruido desde departamentos, ", CFG$editorial_year),
      x = "Edad (años)",
      y = "Razón esperada / observada",
      color = "Sexo"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  fig2_path <- file.path(CFG$report_dir, "fig_qc_b2_expected_observed_ratio_age_sex_2024.png")
  save_plot(p2, fig2_path, width = 10, height = 6)
  
  # Figura 3: factor implícito corregida/observada por edad y sexo, 2024
  p3 <- ggplot(fig2_dt, aes(x = age, y = correction_factor_implied, color = sex_label)) +
    geom_hline(yintercept = 1, linetype = 2) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.4) +
    scale_x_continuous(breaks = seq(0, 110, by = 5)) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".", accuracy = 0.01)) +
    labs(
      title = "Factor implícito corregida/observada por edad y sexo",
      subtitle = paste0("Nacional reconstruido desde departamentos, ", CFG$editorial_year),
      x = "Edad (años)",
      y = "Corregida / observada",
      color = "Sexo"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  fig3_path <- file.path(CFG$report_dir, "fig_qc_b3_implied_factor_age_sex_2024.png")
  save_plot(p3, fig3_path, width = 10, height = 6)
  
  # Figura 4: tasas por edad y sexo, observada vs esperada vs corregida, 2024
  fig4_dt <- melt(
    copy(tab_nat_age_sex_2024)[, .(
      sex_label, age,
      observed_rate_100k,
      expected_rate_100k,
      corrected_rate_100k
    )],
    id.vars = c("sex_label", "age"),
    variable.name = "series",
    value.name = "rate_100k"
  )
  
  fig4_dt[, series := factor(
    series,
    levels = c("observed_rate_100k", "expected_rate_100k", "corrected_rate_100k"),
    labels = c("Observada", "Esperada", "Corregida")
  )]
  
  p4 <- ggplot(fig4_dt, aes(x = age, y = rate_100k, color = series)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.1) +
    facet_wrap(~ sex_label, ncol = 1, scales = "free_y") +
    scale_x_continuous(breaks = seq(0, 110, by = 5)) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".", accuracy = 0.1)) +
    labs(
      title = "Perfil por edad de tasas all-cause: observada, esperada y corregida",
      subtitle = paste0("Nacional, ", CFG$editorial_year),
      x = "Edad (años)",
      y = "Tasa por 100 000",
      color = NULL
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      legend.position = "bottom",
      plot.title = element_text(face = "bold")
    )
  
  fig4_path <- file.path(CFG$report_dir, "fig_qc_b4_rates_age_sex_2024.png")
  save_plot(p4, fig4_path, width = 10, height = 8)
  
  # Figura 5: distribución departamental del factor mediano por año
  p5 <- ggplot(tab_dept_year, aes(x = factor(year_id), y = factor_median)) +
    geom_boxplot(outlier.alpha = 0.35) +
    scale_y_continuous(labels = label_number(big.mark = " ", decimal.mark = ".", accuracy = 0.01)) +
    labs(
      title = "Distribución departamental del factor mediano de completitud",
      subtitle = "Resumen anual por departamento",
      x = "Año",
      y = "Factor mediano"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold")
    )
  
  fig5_path <- file.path(CFG$report_dir, "fig_qc_b5_department_factor_distribution_by_year.png")
  save_plot(p5, fig5_path, width = 9, height = 6)
  
  # ----------------------------------------------------------
  # Diccionario simple del output QC
  # ----------------------------------------------------------
  dict_ext <- build_dictionary_ext(tab_nat_year)
  dict_path <- file.path(CFG$qc_dir, "qc_completeness_validation_dictionary_ext.csv")
  fwrite(dict_ext, dict_path)
  
  # ----------------------------------------------------------
  # Registrar artefactos
  # ----------------------------------------------------------
  qc_files <- c(
    file.path(CFG$qc_dir, "qc_invalid_population_rows.csv"),
    file.path(CFG$qc_dir, "tab_national_year_summary.csv"),
    file.path(CFG$qc_dir, "tab_national_year_sex_summary.csv"),
    file.path(CFG$qc_dir, "tab_national_age_sex_2024.csv"),
    file.path(CFG$qc_dir, "tab_department_year_summary.csv"),
    file.path(CFG$qc_dir, "qc_summary.csv"),
    dict_path
  )
  
  report_files <- c(
    fig1_path,
    fig2_path,
    fig3_path,
    fig4_path,
    fig5_path
  )
  
  for (p in qc_files) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "qc",
      artifact_path = p,
      n_rows = tryCatch(nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(ncol(fread(p)), error = function(e) NA_integer_),
      notes = "QC derivado para validar ajuste por completitud tras script 08"
    )
  }
  
  for (p in report_files) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "report",
      artifact_path = p,
      notes = "Figura de validación del ajuste por completitud"
    )
  }
  
  register_run_finish(
    run_id,
    status = "success",
    message = "08c_qc_completeness_validation completado"
  )
  
  msg("OK -> Input audit: ", input_path)
  msg("OK -> QC dir: ", CFG$qc_dir)
  msg("OK -> Reports dir: ", CFG$report_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
