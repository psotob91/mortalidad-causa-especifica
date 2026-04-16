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

subdir_maps <- function() {
  list(
    qc = c(
      "00a_audit_project_structure_governance_routes" = "audit_project_structure_governance_routes",
      "00c_reconcile_catalog_and_backfill_dictionaries" = "reconcile_catalog_and_backfill_dictionaries",
      "00d_baseline_compare" = "baseline_compare",
      "04_ingest_sinadef_raw" = "ingest_sinadef_raw",
      "05_normalize_death_record" = "normalize_death_record",
      "06_map_and_redistribute_deaths" = "map_and_redistribute_deaths",
      "07_qc_redistribution" = "qc_redistribution",
      "08_build_death_cause_final" = "build_death_cause_final",
      "08b_rollup_death_cause_final" = "rollup_death_cause_final",
      "08c_qc_completeness_validation" = "qc_completeness_validation",
      "09_build_mortality_rates" = "build_mortality_rates",
      "09b_reconcile_mortality_hierarchy" = "reconcile_mortality_hierarchy",
      "10_compute_avp_yll" = "compute_avp_yll",
      "11_build_report_tables" = "build_report_tables",
      "12_build_methods_catalogs" = "build_methods_catalogs",
      "12_diagnostic_pdfs" = "diagnostic_mortality_pdfs",
      "12_diagnostic_pdfs_avp" = "diagnostic_avp_rate_pdfs",
      "12_diagnostic_pdfs_avp_abs" = "diagnostic_avp_abs_pdfs",
      "12_diagnostic_pdfs_deaths" = "diagnostic_deaths_estimated_pdfs",
      "12_diagnostic_pdfs_extra_bloque1" = "diagnostic_mortality_qc_extra_block1_pdfs",
      "13_build_oms_reference_compare_partial" = "build_oms_reference_compare_partial"
    ),
    outputs = c(
      "07_qc_redistribution" = "qc_redistribution"
    )
  )
}

canonical_named_subdir <- function(kind, name) {
  maps <- subdir_maps()[[kind]]
  if (is.null(maps) || is.na(name) || !nzchar(name)) return(name)
  if (name %in% names(maps)) return(unname(maps[[name]]))
  vals <- unname(maps)
  if (name %in% vals) return(name)
  name
}

qc_dir_path <- function(name) {
  here::here("data", "derived", "qc", canonical_named_subdir("qc", name))
}

qc_path_candidates <- function(name, child = NULL) {
  paths <- qc_dir_path(name)
  if (!is.null(child)) paths <- file.path(paths, child)
  unique(paths)
}

resolve_existing_qc_path <- function(name, child = NULL, must_work = FALSE) {
  candidates <- qc_path_candidates(name, child)
  hit <- candidates[file.exists(candidates)][1]
  if (length(hit) == 0L || is.na(hit)) {
    if (isTRUE(must_work)) {
      stop("No se pudo resolver ruta QC para ", name, ". Intentados: ", paste(candidates, collapse = " | "))
    }
    return(candidates[1])
  }
  normalizePath(hit, winslash = "/", mustWork = FALSE)
}

output_aux_dir_path <- function(name) {
  here::here("outputs", canonical_named_subdir("outputs", name))
}

output_aux_path_candidates <- function(name, child = NULL) {
  paths <- output_aux_dir_path(name)
  if (!is.null(child)) paths <- file.path(paths, child)
  unique(paths)
}

resolve_existing_output_aux_path <- function(name, child = NULL, must_work = FALSE) {
  candidates <- output_aux_path_candidates(name, child)
  hit <- candidates[file.exists(candidates)][1]
  if (length(hit) == 0L || is.na(hit)) {
    if (isTRUE(must_work)) {
      stop("No se pudo resolver ruta output auxiliar para ", name, ". Intentados: ", paste(candidates, collapse = " | "))
    }
    return(candidates[1])
  }
  normalizePath(hit, winslash = "/", mustWork = FALSE)
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

read_runtime_paths <- function(config_dir = here::here("config")) {
  path <- file.path(config_dir, "runtime_paths.yml")
  if (!file.exists(path)) return(list())
  yaml::read_yaml(path)
}

is_absolute_path <- function(path) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) return(FALSE)
  grepl("^([A-Za-z]:[\\\\/]|/)", path)
}

normalize_runtime_path <- function(path, base_root = here::here()) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) return(NA_character_)
  candidate <- if (is_absolute_path(path)) path else file.path(base_root, path)
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

path_exists_with_retry <- function(path, retries = 3L, wait_sec = 2) {
  if (is.null(path) || length(path) == 0L || is.na(path) || !nzchar(path)) return(FALSE)
  for (i in seq_len(max(1L, as.integer(retries)))) {
    if (file.exists(path)) return(TRUE)
    Sys.sleep(wait_sec)
  }
  FALSE
}

get_runtime_override <- function(runtime_cfg, key) {
  if (is.null(runtime_cfg) || is.null(runtime_cfg$inputs) || is.null(runtime_cfg$inputs[[key]])) return(NA_character_)
  val <- runtime_cfg$inputs[[key]]
  if (is.list(val) && !is.null(val$path)) val <- val$path
  if (is.null(val) || length(val) == 0L || is.na(val) || !nzchar(val)) return(NA_character_)
  as.character(val)
}

resolve_runtime_input_path <- function(key,
                                       default_repo_relative,
                                       env_var = NULL,
                                       config_dir = here::here("config"),
                                       must_work = FALSE) {
  runtime_cfg <- read_runtime_paths(config_dir)
  env_path <- if (!is.null(env_var) && nzchar(env_var)) Sys.getenv(env_var, unset = "") else ""
  raw_root_env <- Sys.getenv("MCE_RAW_ROOT", unset = "")
  raw_root_cfg <- if (!is.null(runtime_cfg$raw_root)) as.character(runtime_cfg$raw_root) else ""
  cfg_override <- get_runtime_override(runtime_cfg, key)
  raw_relative <- sub("^data[/\\\\]raw[/\\\\]?", "", default_repo_relative)
  if (identical(raw_relative, default_repo_relative)) raw_relative <- basename(default_repo_relative)

  candidates <- c(
    if (nzchar(env_path)) normalize_runtime_path(env_path) else NA_character_,
    if (!is.na(cfg_override)) normalize_runtime_path(cfg_override) else NA_character_,
    if (nzchar(raw_root_env)) normalize_runtime_path(file.path(raw_root_env, raw_relative)) else NA_character_,
    if (nzchar(raw_root_cfg)) normalize_runtime_path(file.path(raw_root_cfg, raw_relative)) else NA_character_,
    normalize_runtime_path(default_repo_relative)
  )
  candidates <- unique(stats::na.omit(candidates))
  hit <- candidates[vapply(candidates, path_exists_with_retry, logical(1))][1]
  if (length(hit) == 0L || is.na(hit)) {
    if (isTRUE(must_work)) {
      stop("No se pudo resolver input runtime para ", key, ". Intentados: ", paste(candidates, collapse = " | "))
    }
    return(normalize_runtime_path(default_repo_relative))
  }
  normalizePath(hit, winslash = "/", mustWork = FALSE)
}

resolve_external_dataset_path <- function(key,
                                          external_yaml_path = here::here("config", "external_sources.yml"),
                                          config_dir = here::here("config"),
                                          must_work = FALSE) {
  ys <- yaml::read_yaml(external_yaml_path)
  if (is.null(ys$external_datasets[[key]]$path)) {
    stop("No se encontró path para external dataset: ", key)
  }
  rel <- as.character(ys$external_datasets[[key]]$path)
  runtime_cfg <- read_runtime_paths(config_dir)
  env_var <- paste0("MCE_EXT_", toupper(key), "_PATH")
  env_path <- Sys.getenv(env_var, unset = "")
  cfg_override <- NA_character_
  if (!is.null(runtime_cfg$external_dataset_overrides) &&
      !is.null(runtime_cfg$external_dataset_overrides[[key]]) &&
      nzchar(as.character(runtime_cfg$external_dataset_overrides[[key]]))) {
    cfg_override <- as.character(runtime_cfg$external_dataset_overrides[[key]])
  }
  external_root_env <- Sys.getenv("MCE_EXTERNAL_ROOT", unset = "")
  external_root_cfg <- if (!is.null(runtime_cfg$external_root)) as.character(runtime_cfg$external_root) else ""

  candidates <- c(
    if (nzchar(env_path)) normalize_runtime_path(env_path) else NA_character_,
    if (!is.na(cfg_override)) normalize_runtime_path(cfg_override) else NA_character_,
    if (nzchar(external_root_env)) normalize_runtime_path(rel, base_root = external_root_env) else NA_character_,
    if (nzchar(external_root_cfg)) normalize_runtime_path(rel, base_root = external_root_cfg) else NA_character_,
    normalize_runtime_path(rel)
  )
  candidates <- unique(stats::na.omit(candidates))
  hit <- candidates[vapply(candidates, path_exists_with_retry, logical(1))][1]
  if (length(hit) == 0L || is.na(hit)) {
    if (isTRUE(must_work)) {
      stop("No se pudo resolver external dataset ", key, ". Intentados: ", paste(candidates, collapse = " | "))
    }
    return(normalize_runtime_path(rel))
  }
  normalizePath(hit, winslash = "/", mustWork = FALSE)
}

resolve_external_path <- function(x) {
  normalize_runtime_path(x)
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
    parquet = {
      tryCatch(
        arrow::read_parquet(path, as_data_frame = FALSE),
        error = function(e) {
          csv_fallback <- sub("\\.parquet$", ".csv", path, ignore.case = TRUE)
          if (file.exists(csv_fallback)) {
            warning(
              "Fallo al leer parquet; usando CSV fallback: ",
              basename(path), " -> ", basename(csv_fallback),
              " | motivo: ", conditionMessage(e)
            )
            return(data.table::fread(csv_fallback, ...))
          }
          stop(e)
        }
      )
    },
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
