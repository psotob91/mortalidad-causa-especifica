#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here("R", "io_utils.R"))
source(here("R", "dictionary_utils.R"))

out_dir <- here("data", "raw", "validation_baselines")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

mort_path <- here("data", "derived", "tables", "tbl_nat_year_sex_mort.csv")
avp_path <- here("data", "derived", "tables", "tbl_nat_year_sex_avp.csv")

if (!file.exists(mort_path) || !file.exists(avp_path)) {
  stop("No encontré tbl_nat_year_sex_mort.csv y/o tbl_nat_year_sex_avp.csv para construir el baseline.")
}

mort <- as.data.table(read_auto(mort_path))
avp <- as.data.table(read_auto(avp_path))

baseline_keep <- function(dt, metric_label) {
  dt[
    location_id == 9000L &
      location_scope == "national" &
      age_group == "Todas las edades" &
      cause_level == 0L &
      cause_name == "Total",
    .(
      metric_type = metric_label,
      year_id = as.integer(year_id),
      sex_id = as.integer(sex_id),
      sex_label = as.character(sex_label),
      population = as.numeric(population),
      metric_abs = as.numeric(metric_abs),
      metric_rate = as.numeric(metric_rate),
      source_run_id = as.character(run_id)
    )
  ]
}

baseline <- rbindlist(
  list(
    baseline_keep(mort, "mortality"),
    baseline_keep(avp, "avp")
  ),
  use.names = TRUE,
  fill = TRUE
)
setorder(baseline, metric_type, year_id, sex_id)
baseline[, baseline_created_at := format(Sys.time(), "%Y-%m-%d %H:%M:%S")]
baseline[, baseline_label := "pre_clean_full_rerun_reference"]

csv_path <- file.path(out_dir, "baseline_nat_year_sex_totals_current.csv")
dict_path <- file.path(out_dir, "baseline_nat_year_sex_totals_current_dictionary_ext.csv")

fwrite(baseline, csv_path)
fwrite(build_dictionary_ext_basic(baseline), dict_path)

cat("Baseline guardado en:\n", csv_path, "\n", sep = "")
