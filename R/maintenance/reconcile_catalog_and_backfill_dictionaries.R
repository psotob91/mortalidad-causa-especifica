#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "dictionary_utils.R"))

CFG <- list(
  version = "v0.1.0_catalog_reconcile_backfill",
  dataset_id = "catalog_reconcile_backfill",
  out_dir = here("data", "_catalog"),
  qc_dir = qc_dir_path("reconcile_catalog_and_backfill_dictionaries")
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)
ensure_project_dirs()
ensure_catalog_files()
run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

rel_path <- function(x) {
  gsub("\\\\", "/", sub(paste0("^", gsub("\\\\", "/", here()), "/?"), "", gsub("\\\\", "/", x)))
}

write_with_dict <- function(dt, stem, out_dir, artifact_type = "final_dataset", notes = NA_character_) {
  csv_path <- file.path(out_dir, paste0(stem, ".csv"))
  dict_path <- file.path(out_dir, paste0(stem, "_dictionary_ext.csv"))
  fwrite(dt, csv_path)
  dict_dt <- build_dictionary_ext_basic(dt)
  fwrite(dict_dt, dict_path)
  register_artifact(CFG$dataset_id, stem, CFG$version, run_id, artifact_type, csv_path, nrow(dt), ncol(dt), notes)
  register_artifact(CFG$dataset_id, stem, CFG$version, run_id, "dictionary_ext", dict_path, nrow(dict_dt), ncol(dict_dt), paste("Diccionario extendido:", notes))
}

scan_dirs <- c(
  here("data", "final"),
  here("data", "derived"),
  here("data", "raw", "validation_baselines")
)

tabular_files <- unlist(lapply(scan_dirs[file.exists(scan_dirs)], function(d) {
  list.files(d, pattern = "\\.(csv|parquet)$", recursive = TRUE, full.names = TRUE)
}), use.names = FALSE)
tabular_files <- tabular_files[!grepl("_dictionary_ext\\.csv$", tabular_files, perl = TRUE)]
tabular_files <- tabular_files[!grepl("catalogo_artefactos\\.csv$|provenance_runs\\.csv$", tabular_files, perl = TRUE)]
tabular_files <- unique(tabular_files[file.exists(tabular_files)])

backfill_rows <- rbindlist(lapply(tabular_files, function(path) {
  stem <- sub("\\.(csv|parquet)$", "", path, perl = TRUE)
  dict_path <- paste0(stem, "_dictionary_ext.csv")
  if (!file.exists(dict_path)) {
    dt <- as.data.table(read_auto(path))
    dict_dt <- build_dictionary_ext_basic(dt)
    fwrite(dict_dt, dict_path)
  }
  data.table(
    data_path = rel_path(path),
    dict_path = rel_path(dict_path),
    backfilled_dictionary = TRUE
  )
}), fill = TRUE)

artifacts_csv <- here("data", "_catalog", "catalogo_artefactos.csv")
artifacts <- if (file.exists(artifacts_csv)) fread(artifacts_csv) else data.table()
if (nrow(artifacts) > 0L) {
  artifacts[, artifact_rel_path := rel_path(artifact_path)]
}

file_inventory <- data.table(file_path = rel_path(tabular_files))
file_inventory[, ext := tools::file_ext(file_path)]
file_inventory[, dir_path := dirname(file_path)]
file_inventory[, file_name := basename(file_path)]
file_inventory[, logical_stem := sub("\\.(csv|parquet)$", "", file_name, perl = TRUE)]
file_inventory[, dictionary_path := file.path(dir_path, paste0(logical_stem, "_dictionary_ext.csv"))]
file_inventory[, has_dictionary_ext := file.exists(here(dictionary_path))]
file_inventory[, dir_scope := fcase(
  grepl("^data/final/", file_path), "final",
  grepl("^data/derived/qc/", file_path), "derived_qc",
  grepl("^data/derived/methods/", file_path), "derived_methods",
  grepl("^data/derived/tables/", file_path), "derived_tables",
  grepl("^data/derived/", file_path), "derived_other",
  grepl("^data/raw/validation_baselines/", file_path), "validation_baseline",
  default = "other"
)]

catalog_summary <- file_inventory[, .(
  has_csv = any(ext == "csv"),
  has_parquet = any(ext == "parquet"),
  has_dictionary_ext = first(has_dictionary_ext),
  canonical_path_pattern = paste0(dir_path[1], "/", logical_stem[1], fifelse(any(ext == "csv") & any(ext == "parquet"), ".{csv|parquet}", paste0(".", unique(ext)[1]))),
  path_resolution_policy = fifelse(any(ext == "csv") & any(ext == "parquet"), "canonical_with_format_fallback", "strict_canonical"),
  formats = paste(sort(unique(ext)), collapse = " | "),
  dir_scope = first(dir_scope)
), by = .(dir_path, logical_stem)]

if (nrow(artifacts) > 0L) {
  artifact_match <- unique(artifacts[, .(
    artifact_rel_path,
    dataset_id,
    table_name,
    artifact_type
  )])
  artifact_match[, file_name := basename(artifact_rel_path)]
  artifact_match[, logical_stem := sub("(_dictionary_ext)?\\.(csv|parquet|xlsx|docx|png)$", "", file_name, perl = TRUE)]
  artifact_match[, dir_path := dirname(artifact_rel_path)]
  artifact_match <- artifact_match[artifact_type != "dictionary_ext"]
  artifact_match <- artifact_match[, .(
    dataset_id = first(na.omit(dataset_id)),
    table_name = first(na.omit(table_name)),
    artifact_types = paste(sort(unique(artifact_type)), collapse = " | ")
  ), by = .(dir_path, logical_stem)]
  catalog_summary <- merge(catalog_summary, artifact_match, by = c("dir_path", "logical_stem"), all.x = TRUE, sort = FALSE)
} else {
  catalog_summary[, `:=`(dataset_id = NA_character_, table_name = NA_character_, artifact_types = NA_character_)]
}

catalog_summary[, governance_source := fifelse(
  grepl("^data/raw/validation_baselines/", file.path(dir_path, paste0(logical_stem, ".csv"))),
  "mixed",
  fifelse(grepl("^data/derived/methods/", file.path(dir_path, paste0(logical_stem, ".csv"))), "mixed", "code_only")
)]
catalog_summary[, legacy_or_fallback_paths := fifelse(path_resolution_policy == "canonical_with_format_fallback", "Mismo dataset canónico con fallback de formato csv/parquet", NA_character_)]
pipeline_steps_path <- here("config", "pipeline_steps.csv")
if (file.exists(pipeline_steps_path)) {
  step_map <- unique(fread(pipeline_steps_path)[, .(
    dataset_id = primary_dataset_id,
    legacy_compat_script = script_path_legacy,
    canonical_active_script = script_path_canonical,
    semantic_phase_name = phase_group
  )])
  catalog_summary <- merge(
    catalog_summary,
    step_map,
    by = "dataset_id",
    all.x = TRUE,
    sort = FALSE
  )
}

catalog_summary[, lifecycle_status := fcase(
  dir_scope == "validation_baseline", "validation_baseline_input",
  grepl("/deprecated/", dir_path), "historical_deprecated",
  default = "canonical_current"
)]
catalog_summary[, is_legacy_named_path := grepl("(^|/)[0-9]{2}[a-z]?_", canonical_path_pattern)]
catalog_summary[, semantic_phase_name := fifelse(
  !is.na(semantic_phase_name),
  semantic_phase_name,
  fcase(
    dir_scope == "final", "final_outputs",
    dir_scope == "derived_qc", "quality_control",
    dir_scope == "derived_methods", "methods_outputs",
    dir_scope == "derived_tables", "report_tables",
    dir_scope == "validation_baseline", "validation_baseline",
    default = dir_scope
  )
)]

setorder(catalog_summary, dir_scope, dir_path, logical_stem)

qc_missing_dict <- catalog_summary[has_dictionary_ext != TRUE]
format_policy_summary <- catalog_summary[, .N, by = .(dir_scope, path_resolution_policy)][order(dir_scope, path_resolution_policy)]

write_with_dict(catalog_summary, "catalogo_datasets_resumido", CFG$out_dir, notes = "Catálogo resumido de datasets y tablas generadas")
write_with_dict(qc_missing_dict, "qc_tablas_sin_dictionary_ext", CFG$qc_dir, artifact_type = "qc", notes = "Tablas tabulares sin dictionary_ext luego del backfill")
write_with_dict(format_policy_summary, "qc_format_policy_summary", CFG$qc_dir, artifact_type = "qc", notes = "Resumen de política de formatos y resolución")
write_with_dict(backfill_rows, "dictionary_backfill_summary", CFG$qc_dir, artifact_type = "qc", notes = "Resumen de diccionarios creados o verificados por backfill")

register_run_finish(run_id, status = "success", message = sprintf("files=%s", nrow(file_inventory)))
