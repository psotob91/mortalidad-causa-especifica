library(here)
library(yaml)
library(data.table)
library(arrow)

project_paths <- function() {
  list(
    root        = here::here(),
    config_dir  = here::here("config"),
    raw_dir     = here::here("data", "raw"),
    staging_dir = here::here("data", "derived", "staging"),
    qc_dir      = here::here("data", "derived", "qc"),
    final_dir   = here::here("data", "final"),
    catalog_dir = here::here("data", "_catalog"),
    reports_dir = here::here("reports"),
    outputs_dir = here::here("outputs"),
    r_dir       = here::here("R"),
    scripts_dir = here::here("scripts")
  )
}

ensure_project_dirs <- function() {
  p <- project_paths()
  dirs <- c(
    p$raw_dir,
    p$staging_dir,
    p$qc_dir,
    p$final_dir,
    p$catalog_dir,
    p$reports_dir,
    p$outputs_dir
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
}

read_external_sources <- function(config_dir = here::here("config")) {
  yaml::read_yaml(file.path(config_dir, "external_sources.yml"))
}

resolve_external_path <- function(x) {
  normalizePath(here::here(x), winslash = "/", mustWork = FALSE)
}

assert_exists <- function(path, label = NULL) {
  if (!file.exists(path)) {
    stop(sprintf("No se encontró el archivo%s: %s",
                 ifelse(is.null(label), "", paste0(" [", label, "]")),
                 path))
  }
  invisible(path)
}

read_auto <- function(path, ...) {
  ext <- tolower(tools::file_ext(path))
  switch(
    ext,
    csv = data.table::fread(path, ...),
    parquet = arrow::read_parquet(path, as_data_frame = FALSE),
    rds = readRDS(path),
    xlsx = openxlsx::read.xlsx(path, ...),
    stop("Extensión no soportada: ", ext)
  )
}

write_csv_parquet <- function(dt, csv_path = NULL, parquet_path = NULL) {
  if (!is.null(csv_path)) data.table::fwrite(dt, csv_path)
  if (!is.null(parquet_path)) arrow::write_parquet(dt, parquet_path)
  invisible(TRUE)
}