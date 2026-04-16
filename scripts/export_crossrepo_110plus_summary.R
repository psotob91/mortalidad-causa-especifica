#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "dictionary_utils.R"))

CFG <- list(
  dataset_id = "death_110plus_summary",
  table_name = "death_110plus_summary",
  version = "v1.0.0",
  input_path = here("data", "final", "death_record_normalized", "death_record_normalized.parquet"),
  spec_path = here("config", "spec_death_110plus_summary.yml"),
  out_dir = here("data", "final", "crossrepo_snapshots"),
  qc_dir = here("data", "derived", "qc", "crossrepo_110plus_snapshot"),
  out_parquet = here("data", "final", "crossrepo_snapshots", "death_110plus_summary.parquet"),
  out_dict = here("data", "final", "crossrepo_snapshots", "death_110plus_summary_diccionario_ext.csv"),
  qc_csv = here("data", "derived", "qc", "crossrepo_110plus_snapshot", "qc_death_110plus_presence.csv"),
  qc_dict = here("data", "derived", "qc", "crossrepo_110plus_snapshot", "qc_death_110plus_presence_diccionario_ext.csv"),
  years = 2018:2024,
  dept_ids = 1:25,
  sex_ids = c(8507L, 8532L)
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)
ensure_project_dirs()
ensure_catalog_files()

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, CFG$dataset_id, CFG$version)

dept_from_location <- function(x) {
  as.integer(substr(sprintf("%06d", as.integer(x)), 1, 2))
}

spec <- read_spec(CFG$spec_path)
dt <- as.data.table(read_parquet(CFG$input_path))

source_run_id <- unique(na.omit(dt$run_id))
source_run_id <- if (length(source_run_id)) source_run_id[1] else NA_character_

base <- dt[
  year_id %in% CFG$years & sex_id %in% CFG$sex_ids,
  .(year_id, sex_id, dept_id = dept_from_location(location_id), age)
]
base <- base[dept_id %in% CFG$dept_ids]

dept_counts <- base[
  age >= 110L,
  .(death_count_110plus_observed = .N),
  by = .(year_id, sex_id, location_id = dept_id)
]

national_counts <- dept_counts[
  ,
  .(death_count_110plus_observed = sum(death_count_110plus_observed)),
  by = .(year_id, sex_id)
][, location_id := 0L]

counts <- rbindlist(list(dept_counts, national_counts), use.names = TRUE)
grid <- CJ(
  year_id = CFG$years,
  sex_id = CFG$sex_ids,
  location_id = c(0L, CFG$dept_ids),
  unique = TRUE
)

snapshot <- merge(grid, counts, by = c("year_id", "sex_id", "location_id"), all.x = TRUE)
snapshot[is.na(death_count_110plus_observed), death_count_110plus_observed := 0L]
snapshot[, has_death_110plus_observed := death_count_110plus_observed > 0L]
snapshot[, source_dataset_version := "death_record_normalized_current"]
snapshot[, source_run_id := fifelse(is.na(source_run_id), "unknown_run", source_run_id)]
setcolorder(snapshot, c(
  "year_id", "sex_id", "location_id", "death_count_110plus_observed",
  "has_death_110plus_observed", "source_dataset_version", "source_run_id"
))
setorderv(snapshot, c("year_id", "sex_id", "location_id"))

pk_dup_n <- snapshot[, .N, by = .(year_id, sex_id, location_id)][N > 1L, .N]
if (pk_dup_n > 0L) {
  register_run_finish(run_id, "failed", "Snapshot 110+ con PK duplicada.")
  stop("QC HARD FAIL: death_110plus_summary tiene PK duplicada.")
}

write_parquet(snapshot, CFG$out_parquet)

snapshot_dict <- dict_from_spec(spec, dataset_version = CFG$version, run_id = run_id, config_dir = here("config"))
snapshot_dict <- enrich_dict_with_stats(snapshot_dict, snapshot)
fwrite(snapshot_dict, CFG$out_dict)

qc <- data.table(
  run_id = run_id,
  snapshot_path = normalizePath(CFG$out_parquet, winslash = "/", mustWork = FALSE),
  n_rows = nrow(snapshot),
  n_years = uniqueN(snapshot$year_id),
  n_sexes = uniqueN(snapshot$sex_id),
  n_locations = uniqueN(snapshot$location_id),
  n_positive_110plus = snapshot[has_death_110plus_observed == TRUE, .N],
  total_deaths_110plus = snapshot[, sum(death_count_110plus_observed)],
  pk_duplicate_n = pk_dup_n,
  required_missing_n = sum(is.na(snapshot$year_id)) + sum(is.na(snapshot$sex_id)) +
    sum(is.na(snapshot$location_id)) + sum(is.na(snapshot$death_count_110plus_observed)) +
    sum(is.na(snapshot$has_death_110plus_observed)),
  status = fifelse(pk_dup_n == 0L, "OK", "FAIL")
)
fwrite(qc, CFG$qc_csv)
qc_dict <- build_dictionary_ext_basic(qc)
fwrite(qc_dict, CFG$qc_dict)

register_artifact(CFG$dataset_id, CFG$table_name, CFG$version, run_id, "final_dataset", CFG$out_parquet,
                  n_rows = nrow(snapshot), n_cols = ncol(snapshot),
                  notes = "Snapshot agregado y no sensible de muertes observadas 110+ para coherencia cruzada con demografia.")
register_artifact(CFG$dataset_id, paste0(CFG$table_name, "_dictionary_ext"), CFG$version, run_id, "dictionary_ext", CFG$out_dict,
                  n_rows = nrow(snapshot_dict), n_cols = ncol(snapshot_dict),
                  notes = "Diccionario extendido del snapshot cross-repo 110+.")
register_artifact(CFG$dataset_id, "qc_death_110plus_presence", CFG$version, run_id, "qc", CFG$qc_csv,
                  n_rows = nrow(qc), n_cols = ncol(qc),
                  notes = "QC estructural del snapshot de muertes observadas 110+.")
register_artifact(CFG$dataset_id, "qc_death_110plus_presence_dictionary_ext", CFG$version, run_id, "dictionary_ext", CFG$qc_dict,
                  n_rows = nrow(qc_dict), n_cols = ncol(qc_dict),
                  notes = "Diccionario del QC del snapshot de muertes observadas 110+.")
register_artifact(CFG$dataset_id, "spec_death_110plus_summary", CFG$version, run_id, "spec", CFG$spec_path,
                  notes = "Especificacion YAML del snapshot de muertes observadas 110+.")

register_run_finish(run_id, "success", "Snapshot cross-repo death_110plus_summary generado.")

cat("Snapshot 110+ exportado en:\n")
cat(" - ", CFG$out_parquet, "\n", sep = "")
cat("QC 110+ exportado en:\n")
cat(" - ", CFG$qc_csv, "\n", sep = "")
