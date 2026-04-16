#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here("R", "review_portal_utils.R"))

portal_message("")
portal_message("Construyendo portal técnico multipágina...")
portal_message(sprintf("Commit base detectado: %s", git_state()$commit_sha))
portal_message(sprintf("Branch actual: %s", git_state()$branch))

build_started <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
status <- "success"
detail <- ""

tryCatch(
  {
    build_review_portal()
  },
  error = function(e) {
    status <<- "error"
    detail <<- conditionMessage(e)
  }
)

build_finished <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
log_path <- here("data", "derived", "qc", "review_portal", "build_log.csv")
fwrite(
  data.table(
    started_at = build_started,
    finished_at = build_finished,
    status = status,
    detail = detail,
    commit_sha = git_state()$commit_sha,
    branch = git_state()$branch
  ),
  log_path
)

if (!identical(status, "success")) {
  stop(sprintf("Falló la construcción del portal: %s", detail), call. = FALSE)
}

portal_message("Listo.")
portal_message(sprintf("Portal HTML: %s", here("reports", "qc_pipeline_encyclopedia", "index.html")))
portal_message(sprintf("PDFs: %s", here("reports", "qc_pipeline_encyclopedia", "pdf")))
portal_message(sprintf("Bitácora: %s", log_path))
