#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

run_step_once <- function(label, script, args = character(), attempt = 1L) {
  start <- Sys.time()
  code <- system2(file.path(R.home("bin"), "Rscript"), c(shQuote(script), args))
  finish <- Sys.time()
  data.table(
    step = label,
    script = script,
    attempt = as.integer(attempt),
    status = if (identical(code, 0L)) "success" else "failed",
    exit_code = as.integer(code),
    started_at = format(start, "%Y-%m-%d %H:%M:%S"),
    finished_at = format(finish, "%Y-%m-%d %H:%M:%S"),
    elapsed_sec = as.numeric(difftime(finish, start, units = "secs"))
  )
}

run_step_with_retry <- function(label,
                                script,
                                args = character(),
                                max_attempts = 2L,
                                retry_wait_sec = 5L) {
  attempts <- vector("list", max_attempts)
  for (i in seq_len(max_attempts)) {
    res <- run_step_once(label, script, args, attempt = i)
    attempts[[i]] <- res
    if (identical(res$exit_code[1], 0L)) {
      return(rbindlist(attempts[seq_len(i)], fill = TRUE))
    }
    if (i < max_attempts) {
      message("Step falló y se reintentará: ", label, " (attempt ", i + 1L, " de ", max_attempts, ")")
      Sys.sleep(retry_wait_sec)
    }
  }
  rbindlist(attempts, fill = TRUE)
}

log_dir <- here("data", "derived", "qc", "run_full_rebuild")
dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
log_path <- file.path(log_dir, "run_full_rebuild_log.csv")

steps <- list(
  list(label = "core_pipeline_full_clean", script = here("scripts", "run_pipeline.R"), args = c("--profile", "full", "--clean-first")),
  list(label = "redistribution_sensitivity_no_redistribution_delete_gc", script = here("scripts", "sensitivity", "redistribution", "run_no_redistribution_delete_gc.R"), args = character()),
  list(label = "qc_portal", script = here("scripts", "build_qc_pipeline_report.R"), args = character()),
  list(label = "methodological_report", script = here("scripts", "build_methodological_adjustment_report.R"), args = character())
)

log_dt <- rbindlist(lapply(steps, function(st) {
  res <- run_step_with_retry(st$label, st$script, st$args)
  dir.create(dirname(log_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(log_path)) {
    fwrite(rbind(fread(log_path), res, fill = TRUE), log_path)
  } else {
    fwrite(res, log_path)
  }
  if (!identical(res$exit_code[1], 0L)) {
    message("Fallo en step: ", st$label, ". Revisar ", log_path)
    quit(save = "no", status = 1)
  }
  res
}), fill = TRUE)

message("Rebuild integral completado. Log: ", log_path)
