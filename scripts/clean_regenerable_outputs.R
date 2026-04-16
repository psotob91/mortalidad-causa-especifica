#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
  library(data.table)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))

`%||%` <- function(x, y) if (is.null(x)) y else x

dry_run <- tolower(Sys.getenv("CLEAN_DRY_RUN", unset = "true")) != "false"
confirmed <- identical(Sys.getenv("CLEAN_CONFIRM", unset = ""), "YES")

clean_specs <- list(
  list(kind = "dir_contents", path = here("data", "final"), note = "datasets finales regenerables"),
  list(kind = "dir_contents", path = here("data", "derived"), note = "datasets derivados y QC regenerables"),
  list(kind = "dir_contents", path = here("data", "_catalog"), note = "catalogo/provenance regenerable"),
  list(kind = "dir_contents", path = here("outputs"), note = "salidas auxiliares regenerables"),
  list(kind = "dir_contents", path = here("reports", "all_causes_word_report"), note = "reportes Word y figuras regenerables"),
  list(kind = "dir_contents", path = here("reports", "all_causes_validation_report"), note = "reporte de validacion regenerable"),
  list(kind = "dir_contents", path = here("reports", "qc_completeness_validation"), note = "figuras QC regenerables"),
  list(
    kind = "dir_filtered",
    path = here("reports", "all_causes_validation_excels"),
    note = "salidas Excel/DOCX regenerables; preserva scripts .R",
    keep_pattern = "\\.R$"
  ),
  list(
    kind = "dir_filtered",
    path = here("reports", "methodological_adjustment_report"),
    note = "salidas del reporte metodologico; preserva plantillas .qmd/.R/.md",
    keep_pattern = "\\.(R|qmd|md)$"
  ),
  list(
    kind = "dir_filtered",
    path = here("reports", "qc_pipeline_encyclopedia"),
    note = "sitio QC regenerable; preserva solo templates fuente",
    keep_pattern = "^(templates)$"
  ),
  list(
    kind = "dir_regex",
    path = here(),
    note = "temporales locales del root",
    pattern = "^tmp_.*|.*\\.tmp$"
  )
)

recreate_dirs <- c(
  here("data", "final"),
  here("data", "derived"),
  here("data", "_catalog"),
  here("outputs"),
  here("reports", "all_causes_word_report"),
  here("reports", "all_causes_validation_report"),
  here("reports", "qc_completeness_validation"),
  here("reports", "all_causes_validation_excels"),
  here("reports", "methodological_adjustment_report"),
  here("reports", "qc_pipeline_encyclopedia")
)

collect_targets <- function(spec) {
  p <- spec$path
  if (!dir.exists(p)) return(character())
  kids <- list.files(p, full.names = TRUE, all.files = TRUE, no.. = TRUE)
  if (identical(spec$kind, "dir_contents")) return(kids)
  if (identical(spec$kind, "dir_filtered")) {
    keep_pattern <- spec$keep_pattern %||% "^$"
    return(kids[!grepl(keep_pattern, basename(kids), perl = TRUE)])
  }
  if (identical(spec$kind, "dir_regex")) {
    pattern <- spec$pattern %||% "^$"
    return(kids[grepl(pattern, basename(kids), perl = TRUE)])
  }
  character()
}

target_rows <- rbindlist(lapply(clean_specs, function(spec) {
  targets <- collect_targets(spec)
  data.table(
    rule_kind = spec$kind,
    root_path = normalizePath(spec$path, winslash = "/", mustWork = FALSE),
    target_path = normalizePath(targets, winslash = "/", mustWork = FALSE),
    note = spec$note
  )
}), fill = TRUE)

cat("Limpieza de artefactos regenerables\n")
cat("Modo dry-run:", dry_run, "\n")
cat("Confirmado:", confirmed, "\n\n")

for (spec in clean_specs) {
  p <- normalizePath(spec$path, winslash = "/", mustWork = FALSE)
  cat(if (dir.exists(spec$path)) "[REGLA]" else "[NO EXISTE]", p, " :: ", spec$note, "\n", sep = "")
}

cat("\nResumen de artefactos candidatos a limpieza:\n")
if (nrow(target_rows) == 0L) {
  cat("  (no se detectaron artefactos regenerables en las rutas objetivo)\n")
} else {
  summary_dt <- target_rows[, .N, by = .(root_path, note)][order(root_path)]
  for (i in seq_len(nrow(summary_dt))) {
    cat("  - ", summary_dt$root_path[i], " => ", summary_dt$N[i], " artefactos\n", sep = "")
  }
}

if (dry_run || !confirmed) {
  cat("\nNo se elimino nada. Para ejecutar la limpieza real usar:\n")
  cat("  CLEAN_DRY_RUN=false CLEAN_CONFIRM=YES Rscript scripts/clean_regenerable_outputs.R\n")
  quit(save = "no", status = 0)
}

for (tp in unique(target_rows$target_path)) {
  if (nzchar(tp) && file.exists(tp)) unlink(tp, recursive = TRUE, force = TRUE)
}

for (d in recreate_dirs) dir.create(d, recursive = TRUE, showWarnings = FALSE)
ensure_project_dirs()
ensure_catalog_files()

cat("\nLimpieza completada.\n")
