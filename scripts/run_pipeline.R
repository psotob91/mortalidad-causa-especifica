#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(yaml)
})

parse_args <- function(args) {
  out <- list(
    profile = "full",
    from_step = NA_character_,
    to_step = NA_character_,
    clean_first = FALSE,
    stop_on_qc_fail = TRUE,
    preflight_only = FALSE
  )
  i <- 1L
  while (i <= length(args)) {
    arg <- args[i]
    nxt <- if (i < length(args)) args[i + 1L] else NA_character_
    if (arg == "--profile" && !is.na(nxt)) {
      out$profile <- nxt; i <- i + 2L; next
    }
    if (arg == "--from-step" && !is.na(nxt)) {
      out$from_step <- nxt; i <- i + 2L; next
    }
    if (arg == "--to-step" && !is.na(nxt)) {
      out$to_step <- nxt; i <- i + 2L; next
    }
    if (arg == "--clean-first") {
      out$clean_first <- TRUE; i <- i + 1L; next
    }
    if (arg == "--no-stop-on-qc-fail") {
      out$stop_on_qc_fail <- FALSE; i <- i + 1L; next
    }
    if (arg == "--preflight-only") {
      out$preflight_only <- TRUE; i <- i + 1L; next
    }
    i <- i + 1L
  }
  out
}

cfg <- parse_args(commandArgs(trailingOnly = TRUE))
steps <- fread(here("config", "pipeline_steps.csv"))
profiles <- yaml::read_yaml(here("config", "pipeline_profiles.yml"))

profile_col <- unlist(profiles$profiles[[cfg$profile]]$include_columns, use.names = FALSE)[1]
if (is.na(profile_col) || !profile_col %in% names(steps)) {
  stop("Perfil invalido o no definido: ", cfg$profile)
}

steps <- steps[get(profile_col) == TRUE]
setorder(steps, step_order)

if (!is.na(cfg$from_step)) {
  start_order <- steps[step_id == cfg$from_step, step_order][1]
  if (is.na(start_order)) stop("from_step no encontrado: ", cfg$from_step)
  steps <- steps[step_order >= start_order]
}
if (!is.na(cfg$to_step)) {
  end_order <- steps[step_id == cfg$to_step, step_order][1]
  if (is.na(end_order)) stop("to_step no encontrado: ", cfg$to_step)
  steps <- steps[step_order <= end_order]
}

rscript_bin <- file.path(R.home("bin"), "Rscript")
qc_dir <- here("data", "derived", "qc", "run_pipeline")
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
history_path <- here("data", "_catalog", "pipeline_run_history.csv")

run_log <- data.table(
  step_id = character(),
  step_order = integer(),
  script_path = character(),
  status = character(),
  exit_code = integer(),
  started_at = character(),
  finished_at = character(),
  elapsed_sec = numeric()
)

run_one <- function(script_path) {
  start <- Sys.time()
  exit_code <- system2(rscript_bin, shQuote(script_path))
  finish <- Sys.time()
  list(exit_code = as.integer(exit_code), start = start, finish = finish)
}

preflight_path <- here("scripts", "run_preflight_checks.R")
preflight_res <- run_one(preflight_path)
if (preflight_res$exit_code != 0L) {
  stop("Preflight fallido. Revisar data/derived/qc/run_pipeline/preflight_checks.csv")
}

if (isTRUE(cfg$preflight_only)) {
  message("Preflight ejecutado correctamente.")
  quit(save = "no", status = 0)
}

if (isTRUE(cfg$clean_first)) {
  old_dry <- Sys.getenv("CLEAN_DRY_RUN", unset = "")
  old_confirm <- Sys.getenv("CLEAN_CONFIRM", unset = "")
  Sys.setenv(CLEAN_DRY_RUN = "false", CLEAN_CONFIRM = "YES")
  on.exit({
    if (nzchar(old_dry)) Sys.setenv(CLEAN_DRY_RUN = old_dry) else Sys.unsetenv("CLEAN_DRY_RUN")
    if (nzchar(old_confirm)) Sys.setenv(CLEAN_CONFIRM = old_confirm) else Sys.unsetenv("CLEAN_CONFIRM")
  }, add = TRUE)
  cleaner <- here("scripts", "clean_regenerable_outputs.R")
  clean_res <- run_one(cleaner)
  if (clean_res$exit_code != 0L) stop("Limpieza fallida antes del rerun.")
  dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)
}

for (i in seq_len(nrow(steps))) {
  script_rel <- steps$script_path_canonical[i]
  res <- run_one(here(script_rel))
  run_log <- rbind(run_log, data.table(
    step_id = steps$step_id[i],
    step_order = steps$step_order[i],
    script_path = script_rel,
    status = if (res$exit_code == 0L) "success" else "failed",
    exit_code = res$exit_code,
    started_at = format(res$start, "%Y-%m-%d %H:%M:%S"),
    finished_at = format(res$finish, "%Y-%m-%d %H:%M:%S"),
    elapsed_sec = as.numeric(difftime(res$finish, res$start, units = "secs"))
  ), fill = TRUE)
  fwrite(run_log, file.path(qc_dir, "pipeline_run_log.csv"))
  if (res$exit_code != 0L && isTRUE(cfg$stop_on_qc_fail) && isTRUE(steps$blocking[i])) {
    message("Pipeline detenido en step_id=", steps$step_id[i], ". Revisar pipeline_run_log.csv")
    if (file.exists(history_path)) {
      old <- fread(history_path)
      fwrite(rbind(old, run_log[nrow(run_log)]), history_path)
    } else {
      fwrite(run_log[nrow(run_log)], history_path)
    }
    quit(save = "no", status = 1)
  }
}

if (file.exists(history_path)) {
  old <- fread(history_path)
  fwrite(rbind(old, run_log, fill = TRUE), history_path)
} else {
  fwrite(run_log, history_path)
}

message("Pipeline finalizado. Revisar data/derived/qc/run_pipeline/pipeline_run_log.csv")
