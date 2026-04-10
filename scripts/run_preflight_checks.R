#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(yaml)
})

source(here("R", "io_utils.R"))

args <- commandArgs(trailingOnly = TRUE)
strict_mode <- !("--no-strict" %in% args)

out_dir <- here("data", "derived", "qc", "run_pipeline")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

is_readable <- function(path) file.exists(path) && file.access(path, 4) == 0
is_writable_dir <- function(path) dir.exists(path) && file.access(path, 2) == 0

runtime_cfg <- read_runtime_paths()
external_cfg <- read_external_sources()

checks <- list()
add_check <- function(check_id, severity, status, path = NA_character_, detail = NA_character_) {
  checks[[length(checks) + 1L]] <<- data.table(
    check_id = check_id,
    severity = severity,
    status = status,
    resolved_path = path,
    detail = detail
  )
}

add_check(
  "project_root",
  "blocking",
  if (dir.exists(here())) "ok" else "fail",
  here(),
  "Raiz del proyecto detectada por here()"
)

sinadef_path <- resolve_runtime_input_path(
  key = "sinadef_dir",
  default_repo_relative = file.path("data", "raw", "sinadef"),
  env_var = "MCE_SINADEF_DIR",
  must_work = FALSE
)
add_check(
  "runtime_input_sinadef_dir",
  "blocking",
  if (is_readable(sinadef_path)) "ok" else "fail",
  sinadef_path,
  "Input raw principal SINADEF; puede venir de MCE_SINADEF_DIR, MCE_RAW_ROOT o config/runtime_paths.yml"
)

required_external <- names(external_cfg$external_datasets)
if (length(required_external) > 0L) {
  req_meta <- rbindlist(lapply(unname(required_external), function(k) {
    meta <- external_cfg$external_datasets[[k]]
    data.table(
      input_id = k,
      required = isTRUE(meta$required)
    )
  }), use.names = TRUE, fill = TRUE)
  for (i in seq_len(nrow(req_meta))) {
    key <- req_meta$input_id[i]
    severity <- if (isTRUE(req_meta$required[i])) "blocking" else "warning"
    resolved <- resolve_external_dataset_path(
      key = key,
      external_yaml_path = here("config", "external_sources.yml"),
      must_work = FALSE
    )
    add_check(
      paste0("external_dataset_", key),
      severity,
      if (is_readable(resolved)) "ok" else if (severity == "blocking") "fail" else "warning",
      resolved,
      paste0("External dataset: ", key)
    )
  }
}

for (wd in c(
  here("data", "final"),
  here("data", "derived"),
  here("data", "_catalog"),
  here("reports"),
  here("outputs")
)) {
  dir.create(wd, recursive = TRUE, showWarnings = FALSE)
  add_check(
    paste0("writable_", basename(wd)),
    "blocking",
    if (is_writable_dir(wd)) "ok" else "fail",
    wd,
    "Directorio de salida escribible"
  )
}

baseline_path <- here("data", "raw", "validation_baselines", "baseline_nat_year_sex_totals_current.csv")
add_check(
  "baseline_validation_file",
  "warning",
  if (is_readable(baseline_path)) "ok" else "warning",
  baseline_path,
  "Baseline para comparacion pre/post rerun"
)

runtime_snapshot <- data.table(
  setting_name = c(
    "MCE_SINADEF_DIR",
    "MCE_RAW_ROOT",
    "MCE_EXTERNAL_ROOT",
    "runtime_paths.yml::raw_root",
    "runtime_paths.yml::external_root"
  ),
  value = c(
    Sys.getenv("MCE_SINADEF_DIR", unset = ""),
    Sys.getenv("MCE_RAW_ROOT", unset = ""),
    Sys.getenv("MCE_EXTERNAL_ROOT", unset = ""),
    if (!is.null(runtime_cfg$raw_root)) as.character(runtime_cfg$raw_root) else "",
    if (!is.null(runtime_cfg$external_root)) as.character(runtime_cfg$external_root) else ""
  )
)

checks_dt <- rbindlist(checks, fill = TRUE)
summary_dt <- checks_dt[, .N, by = .(severity, status)][order(severity, status)]

fwrite(checks_dt, file.path(out_dir, "preflight_checks.csv"))
fwrite(summary_dt, file.path(out_dir, "preflight_summary.csv"))
fwrite(runtime_snapshot, file.path(out_dir, "preflight_runtime_snapshot.csv"))

blocking_bad <- checks_dt[severity == "blocking" & status != "ok"]
if (strict_mode && nrow(blocking_bad) > 0L) {
  message("Preflight NO APROBADO. Revisar data/derived/qc/run_pipeline/preflight_checks.csv")
  quit(save = "no", status = 1)
}

message("Preflight APROBADO.")
