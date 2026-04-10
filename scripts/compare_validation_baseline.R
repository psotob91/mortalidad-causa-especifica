#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here("R", "io_utils.R"))

qc_dir <- qc_dir_path("baseline_compare")
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

baseline_path <- here("data", "raw", "validation_baselines", "baseline_nat_year_sex_totals_current.csv")
if (!file.exists(baseline_path)) stop("No existe baseline de validacion: ", baseline_path)

baseline <- fread(baseline_path)
mort_tbl <- as.data.table(read_auto(here("data", "derived", "tables", "tbl_nat_year_sex_mort.parquet")))
avp_tbl <- as.data.table(read_auto(here("data", "derived", "tables", "tbl_nat_year_sex_avp.parquet")))

current <- rbindlist(list(
  mort_tbl[cause_level == 0L & cause_name == "Total", .(
    metric_type = "mortality",
    year_id = as.integer(year_id),
    sex_id = as.integer(sex_id),
    sex_label = as.character(sex_label),
    population = as.numeric(population),
    metric_abs = as.numeric(metric_abs),
    metric_rate = as.numeric(metric_rate)
  )],
  avp_tbl[cause_level == 0L & cause_name == "Total", .(
    metric_type = "avp",
    year_id = as.integer(year_id),
    sex_id = as.integer(sex_id),
    sex_label = as.character(sex_label),
    population = as.numeric(population),
    metric_abs = as.numeric(metric_abs),
    metric_rate = as.numeric(metric_rate)
  )]
), use.names = TRUE, fill = TRUE)

cmp <- merge(
  baseline[, .(metric_type, year_id, sex_id, sex_label, population_base = population, metric_abs_base = metric_abs, metric_rate_base = metric_rate)],
  current[, .(metric_type, year_id, sex_id, sex_label, population_current = population, metric_abs_current = metric_abs, metric_rate_current = metric_rate)],
  by = c("metric_type", "year_id", "sex_id", "sex_label"),
  all = TRUE,
  sort = TRUE
)

cmp[, abs_diff := metric_abs_current - metric_abs_base]
cmp[, rate_diff := metric_rate_current - metric_rate_base]
cmp[, population_diff := population_current - population_base]

summary_dt <- cmp[, .(
  n_rows = .N,
  n_abs_diff = sum(abs(abs_diff) > 1e-6, na.rm = TRUE),
  n_rate_diff = sum(abs(rate_diff) > 1e-6, na.rm = TRUE),
  max_abs_diff = suppressWarnings(max(abs(abs_diff), na.rm = TRUE)),
  max_rate_diff = suppressWarnings(max(abs(rate_diff), na.rm = TRUE))
), by = metric_type][order(metric_type)]

fwrite(cmp, file.path(qc_dir, "qc_baseline_vs_rerun_nat_year_sex_totals.csv"))
fwrite(summary_dt, file.path(qc_dir, "qc_baseline_vs_rerun_summary.csv"))

if (any(summary_dt$n_abs_diff > 0 | summary_dt$n_rate_diff > 0, na.rm = TRUE)) {
  stop("QC HARD FAIL: baseline vs rerun muestra diferencias. Revisar qc_baseline_vs_rerun_summary.csv")
}

message("Comparacion baseline APROBADA.")
