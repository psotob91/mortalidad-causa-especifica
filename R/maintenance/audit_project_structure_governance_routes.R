#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "dictionary_utils.R"))

CFG <- list(
  version = "v0.1.0_project_structure_governance_routes_audit",
  dataset_id = "project_structure_governance_routes_audit",
  out_dir = here("data", "derived", "project_audit"),
  qc_dir = qc_dir_path("audit_project_structure_governance_routes")
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

read_lines_safe <- function(path) {
  tryCatch(readLines(path, warn = FALSE, encoding = "UTF-8"), error = function(e) character())
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

script_paths <- list.files(
  here(),
  pattern = "\\.R$",
  recursive = TRUE,
  full.names = TRUE
)
script_paths <- unique(script_paths[file.exists(script_paths)])
script_paths <- script_paths[!grepl(paste0("^", gsub("\\\\", "/", here()), "/backups/"), gsub("\\\\", "/", script_paths))]

script_inventory <- rbindlist(lapply(script_paths, function(path) {
  lines <- read_lines_safe(path)
  txt <- paste(lines, collapse = "\n")
  script_name <- basename(path)
  path_rel <- rel_path(path)
  scope <- fcase(
    grepl("^deprecated/|^reports/deprecated/|^scripts/deprecated/|^R/maintenance/deprecated/|^R/diagnostics/deprecated/", path_rel), "deprecated_script",
    grepl("(^|/)(tmp_.*\\.R|arbol-estructura\\.R)$", path_rel), "scratch_script",
    grepl("^scripts/", path_rel), "core_or_utility",
    grepl("^reports/all_causes_validation_excels/", path_rel), "active_validation_report_script",
    grepl("^reports/final_word_report/", path_rel), "active_word_report_script",
    grepl("^R/diagnostics/", path_rel), "diagnostic_script",
    grepl("^R/maintenance/", path_rel), "maintenance_script",
    grepl("^R/", path_rel), "library_helper",
    grepl("^config/.*\\.R$", path_rel), "config_script",
    default = "other_r_script"
  )
  phase_prefix <- sub("^([0-9]{2}[a-z]?).*$", "\\1", script_name)
  if (!grepl("^[0-9]{2}", script_name)) phase_prefix <- NA_character_
  uses_find_first_existing <- grepl("find_first_existing", txt, fixed = TRUE)
  uses_here <- grepl("here\\(", txt, perl = TRUE)
  uses_external_relative <- grepl("\\.\\./", txt, perl = TRUE)
  uses_catalog_register <- grepl("register_artifact\\(", txt, perl = TRUE)
  uses_dict_builder <- grepl("dictionary_ext|dict_from_spec|build_dictionary_ext", txt, perl = TRUE)
  path_resolution_policy <- fifelse(
    uses_find_first_existing & grepl('here\\("data", "derived".*here\\("data", "final"', txt, perl = TRUE),
    "legacy_read_fallback_only",
    fifelse(
      uses_find_first_existing,
      "canonical_with_format_fallback",
      "strict_canonical"
    )
  )
  structural_recommendation <- fifelse(
    scope %in% c("deprecated_script", "scratch_script"),
    "documentar_solamente",
    fifelse(
      script_name == "07_qc_redistribution_debug.R",
      "documentar_solamente",
      fifelse(
        scope %in% c("core_or_utility", "active_validation_report_script", "active_word_report_script", "diagnostic_script", "library_helper", "config_script"),
        "sin_cambio",
        fifelse(
          scope %in% c("maintenance_script"),
        "sin_cambio",
        "documentar_solamente"
        )
      )
    )
  )
  data.table(
    script_path = path_rel,
    script_name = script_name,
    scope = scope,
    phase_prefix = phase_prefix,
    uses_here = uses_here,
    uses_find_first_existing = uses_find_first_existing,
    uses_external_relative_dependency = uses_external_relative,
    uses_catalog_register = uses_catalog_register,
    uses_dictionary_logic = uses_dict_builder,
    path_resolution_policy = path_resolution_policy,
    structural_recommendation = structural_recommendation,
    is_legacy_named_script = grepl("^[0-9]{2}[a-z]?_", script_name)
  )
}), fill = TRUE)
aliases_path <- here("config", "script_aliases.csv")
script_aliases <- if (file.exists(aliases_path)) fread(aliases_path) else data.table(
  legacy_script = character(),
  canonical_script = character(),
  phase_label = character(),
  scope = character(),
  status = character()
)
if (nrow(script_aliases) > 0L) {
  script_aliases_merge <- copy(script_aliases)[, .(
    legacy_script,
    canonical_script,
    phase_label,
    alias_scope = scope,
    alias_status = status
  )]
  script_inventory <- merge(
    script_inventory,
    script_aliases_merge,
    by.x = "script_path",
    by.y = "legacy_script",
    all.x = TRUE,
    sort = FALSE
  )
  script_aliases_canonical <- unique(copy(script_aliases)[, .(
    canonical_script,
    legacy_compat_script = legacy_script,
    canonical_phase_label = phase_label,
    canonical_alias_scope = scope,
    canonical_alias_status = status
  )])
  script_inventory <- merge(
    script_inventory,
    script_aliases_canonical,
    by.x = "script_path",
    by.y = "canonical_script",
    all.x = TRUE,
    sort = FALSE
  )
}

path_policy_overrides <- data.table(
  script_name = c(
    "12e_build_all_causes_validation_excels.R",
    "12f_build_all_causes_word_report.R"
  ),
  path_resolution_policy = c(
    "canonical_with_format_fallback",
    "canonical_with_format_fallback"
  )
)
script_inventory <- merge(
  script_inventory,
  path_policy_overrides,
  by = "script_name",
  all.x = TRUE,
  suffixes = c("", "_override"),
  sort = FALSE
)
script_inventory[, path_resolution_policy := fifelse(
  !is.na(path_resolution_policy_override),
  path_resolution_policy_override,
  path_resolution_policy
)]
script_inventory[, path_resolution_policy_override := NULL]

script_inventory[, lifecycle_status := fcase(
  grepl("^deprecated/|^reports/deprecated/|^scripts/deprecated/|^R/maintenance/deprecated/|^R/diagnostics/deprecated/", script_path), "historical_deprecated",
  grepl("(^|/)(tmp_.*\\.R|arbol-estructura\\.R)$", script_path), "scratch_unmanaged",
  !is.na(canonical_phase_label), "canonical_active",
  !is.na(alias_status), alias_status,
  scope %in% c("core_or_utility", "diagnostic_script", "maintenance_script", "library_helper", "config_script", "active_validation_report_script", "active_word_report_script"), "canonical_active",
  default = "historical_deprecated"
)]
script_inventory[, canonical_active_script := fifelse(
  !is.na(canonical_phase_label),
  script_path,
  fifelse(!is.na(canonical_script), canonical_script, fifelse(lifecycle_status == "canonical_active", script_path, NA_character_))
)]
script_inventory[, legacy_compat_script := fifelse(
  !is.na(alias_status),
  script_path,
  fifelse(!is.na(legacy_compat_script), legacy_compat_script, NA_character_)
)]
script_inventory[, semantic_phase_name := fifelse(
  !is.na(canonical_phase_label),
  canonical_phase_label,
  fifelse(!is.na(phase_label), phase_label, fifelse(!is.na(phase_prefix), phase_prefix, tools::file_path_sans_ext(script_name)))
)]

path_audit <- script_inventory[, .(
  script_path,
  script_name,
  scope,
  path_resolution_policy,
  uses_find_first_existing,
  uses_external_relative_dependency,
  lifecycle_status,
  assessment = fifelse(
    path_resolution_policy == "legacy_read_fallback_only",
    "requiere_fijar_ruta_canonica",
    fifelse(
      lifecycle_status %in% c("historical_deprecated", "scratch_unmanaged"),
      "documentado_fuera_de_operacion",
      fifelse(path_resolution_policy == "canonical_with_format_fallback", "aceptable_documentable", "aceptable")
    )
  )
)]

governance_decisions <- data.table(
  decision_id = c(
    "cause_target_sex_age_restrictions",
    "direct_demographic_incompatibility_handling",
    "direct_specific_icd_handling",
    "sensitive_methodological_positions",
    "covid_priority_override_u07",
    "read_auto_parquet_csv_fallback",
    "report_table_path_fallbacks",
    "oprm_pandemic_residual_logic"
  ),
  current_location = c(
    "scripts/build_cause_master.R + scripts/build_redistribution_rules.R + scripts/map_and_redistribute_deaths.R",
    "data/raw/redistribution_rules/patch_direct_demographic_incompatibility_handling.csv",
    "data/raw/redistribution_rules/patch_direct_specific_icd_handling.csv",
    "data/raw/oms_reference/patch_sensitive_methodological_positions.csv",
    "scripts/map_and_redistribute_deaths.R + scripts/build_methods_catalogs.R",
    "R/io_utils.R",
    "reports/all_causes_validation_excels/build_all_causes_validation_excels.R + reports/final_word_report/build_all_causes_word_report.R",
    "scripts/build_cause_master.R + scripts/build_death_cause_final.R"
  ),
  current_source = c(
    "mixed",
    "editable_master",
    "editable_master",
    "editable_master",
    "code_only",
    "code_only",
    "mixed",
    "code_only"
  ),
  recommended_governance = c(
    "migrar_parcialmente",
    "migrar_a_maestro_editable",
    "migrar_a_maestro_editable",
    "migrar_a_maestro_editable",
    "documentar_solamente",
    "dejar_en_codigo",
    "documentar_solamente",
    "documentar_solamente"
  ),
  downstream_scripts = c(
    "03, 06, 08, 09, 10",
    "06, 08, 09, 10, 12",
    "06, 12",
    "12, 13, 12f",
    "06, 12",
    "05, 06, 08, 09, 10, 11, 12, 13, 12e, 12f",
    "12e, 12f",
    "08, 10, 11, 12f"
  ),
  risk_if_moved = c(
    "medio",
    "bajo",
    "bajo",
    "bajo",
    "medio",
    "alto",
    "medio",
    "alto"
  ),
  rationale = c(
    "Las restricciones de sexo/edad tienen valor de gobernanza futura, pero hoy están atadas a la taxonomía causal y no conviene externalizarlas por completo sin rediseñar el flujo.",
    "Ya opera como maestro editable y conviene mantenerlo así para años futuros.",
    "Ya opera como maestro editable y conviene mantenerlo así para años futuros.",
    "Es claramente un maestro metodológico editable para decisiones sensibles OMS/Australia/proyecto.",
    "La prioridad COVID es una decisión algorítmica local del matching y no conviene generalizarla apresuradamente.",
    "El fallback parquet->csv es un comportamiento técnico transversal y debe quedarse en código.",
    "Los fallbacks de ruta en reportes deben documentarse y limitarse a formato, no a ubicaciones múltiples.",
    "La lógica OPRM es metodológicamente sensible y mejor mantenerla controlada en código con documentación fuerte."
  )
)

artifacts_csv <- here("data", "_catalog", "catalogo_artefactos.csv")
artifacts <- if (file.exists(artifacts_csv)) fread(artifacts_csv) else data.table()
if (nrow(artifacts) > 0L) {
  artifacts[, artifact_rel_path := rel_path(artifact_path)]
  artifacts[, logical_stem := sub("(_dictionary_ext)?\\.(csv|parquet|docx|png|xlsx)$", "", basename(artifact_rel_path), perl = TRUE)]
  artifact_summary <- artifacts[, .(
    versions = paste(unique(version), collapse = " | "),
    artifact_types = paste(sort(unique(artifact_type)), collapse = " | "),
    file_formats = paste(sort(unique(file_ext)), collapse = " | "),
    latest_artifact_path = artifact_rel_path[.N]
  ), by = .(dataset_id, table_name, logical_stem)]
} else {
  artifact_summary <- data.table()
}

write_with_dict(script_inventory, "script_inventory", CFG$out_dir, notes = "Inventario estructural de scripts activos")
write_with_dict(path_audit, "path_resolution_audit", CFG$out_dir, notes = "Auditoría de resolución de rutas")
write_with_dict(governance_decisions, "governance_decisions_audit", CFG$out_dir, notes = "Auditoría de decisiones en código vs maestros")
if (nrow(artifact_summary) > 0L) {
  write_with_dict(artifact_summary, "artifact_catalog_summary_pre_rerun", CFG$out_dir, notes = "Resumen del catálogo técnico antes del rerun limpio")
}

if (nrow(script_aliases) > 0L) {
  write_with_dict(script_aliases, "script_legacy_canonical_map", CFG$out_dir, notes = "Mapa de scripts legacy y canonicos")
}

qc_ambiguous <- path_audit[assessment == "requiere_fijar_ruta_canonica"]
write_with_dict(qc_ambiguous, "qc_ambiguous_paths_active_scripts", CFG$qc_dir, artifact_type = "qc", notes = "Scripts activos con política de rutas ambigua")

register_run_finish(run_id, status = "success", message = sprintf("scripts=%s", nrow(script_inventory)))
