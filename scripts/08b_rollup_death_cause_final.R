#!/usr/bin/env Rscript

# ============================================================
# 08b_rollup_death_cause_final.R
# ------------------------------------------------------------
# Objetivo:
#   Tomar death_cause_final TERMINAL y generar una
#   versión jerárquica con roll-up a todos los ancestros:
#   L4, L3, L2, L1 y TOTAL.
#
# Enfoque endurecido:
#   - usa cause_hierarchy_bridge
#   - fuerza explícitamente input leaf-only / is_terminal == TRUE
#   - agrega:
#       * deaths_observed
#       * deaths_post_redistribution
#       * deaths_final
#       * pandemic_excess_component
#   - reconstruye deaths_final_net_of_pandemic
#   - recalcula correction_factor_completeness como:
#       deaths_final_net_of_pandemic / deaths_post_redistribution
#     cuando el denominador > 0
#   - añade QC de parent == sum(children)
#
# Salidas:
#   data/final/death_cause_final_hierarchical/
#     - death_cause_final_hierarchical.csv
#     - death_cause_final_hierarchical.parquet
#     - death_cause_final_hierarchical_dictionary_ext.csv
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

CFG <- list(
  version = "v0.2.2_leaf_only_hardened_total_safe",
  dataset_id = "death_cause_final_hierarchical",
  table_name = "death_cause_final_hierarchical",
  
  input_final_candidates = c(
    here("data", "final", "death_cause_final", "death_cause_final.parquet"),
    here("data", "final", "death_cause_final", "death_cause_final.csv")
  ),
  
  input_bridge_candidates = c(
    here("data", "final", "cause_hierarchy_bridge", "cause_hierarchy_bridge.parquet"),
    here("data", "final", "cause_hierarchy_bridge", "cause_hierarchy_bridge.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  out_dir = here("data", "final", "death_cause_final_hierarchical"),
  qc_dir  = here("data", "derived", "qc", "08b_rollup_death_cause_final"),
  
  keep_only_levels = c(0L, 1L, 2L, 3L, 4L),
  
  qc_abs_tol = 1e-6,
  qc_rel_tol = 1e-11,
  
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

safe_nonneg <- function(x) pmax(0, as.numeric(x))

safe_ratio <- function(num, den) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- num[ok] / den[ok]
  out
}

build_dictionary_ext <- function(dt) {
  data.table(
    variable = names(dt),
    tipo = vapply(dt, function(x) class(x)[1], character(1)),
    n = nrow(dt),
    n_missing = vapply(dt, function(x) sum(is.na(x)), integer(1)),
    n_distinct = vapply(dt, function(x) uniqueN(x), integer(1)),
    example_values = vapply(dt, function(x) {
      vals <- unique(na.omit(as.character(x)))
      paste(head(vals, 5), collapse = " | ")
    }, character(1))
  )
}

ensure_project_dirs()
ensure_catalog_files()

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

qc_cause_hard_rollup <- function(dt, cm,
                                 abs_tol = CFG$qc_abs_tol,
                                 rel_tol = CFG$qc_rel_tol) {
  kids <- unique(
    cm[!is.na(parent_concept_id) & cause_concept_id != parent_concept_id,
       .(parent_concept_id, child_concept_id = cause_concept_id)]
  )
  
  if (nrow(kids) == 0L) return(invisible(TRUE))
  
  child_sum <- merge(
    dt[, .(
      year_id, location_id, sex_id, age,
      cause_concept_id, deaths_final
    )],
    kids,
    by.x = "cause_concept_id",
    by.y = "child_concept_id",
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(children_sum = sum(deaths_final, na.rm = TRUE)),
    by = .(year_id, location_id, sex_id, age, cause_concept_id = parent_concept_id)
  ]
  
  parent_val <- dt[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    parent_val = deaths_final
  )]
  
  chk <- merge(
    child_sum, parent_val,
    by = c("year_id", "location_id", "sex_id", "age", "cause_concept_id"),
    all = FALSE
  )
  
  chk[, diff := parent_val - children_sum]
  chk[, tol := pmax(abs_tol, rel_tol * pmax(abs(parent_val), abs(children_sum), 1))]
  chk[, pass := abs(diff) <= tol]
  
  fwrite(chk, file.path(CFG$qc_dir, "qc_parent_children_consistency.csv"))
  
  bad <- chk[pass == FALSE]
  if (nrow(bad) > 0L) {
    top <- bad[order(-abs(diff))][1:min(.N, 20)]
    stop(
      "QC HARD FAIL: parent != sum(children) en 08b. Revisar qc_parent_children_consistency.csv\n",
      paste(capture.output(print(top)), collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

tryCatch({
  
  msg("Resolviendo inputs...")
  
  final_path <- first_existing(CFG$input_final_candidates)
  bridge_path <- first_existing(CFG$input_bridge_candidates)
  cause_path <- first_existing(CFG$input_cause_candidates)
  
  if (is.na(final_path)) stop("No encontré death_cause_final.")
  if (is.na(bridge_path)) stop("No encontré cause_hierarchy_bridge.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  
  msg("Leyendo death_cause_final...")
  dt <- as.data.table(read_auto(final_path))
  
  msg("Leyendo cause_hierarchy_bridge...")
  br <- as.data.table(read_auto(bridge_path))
  
  msg("Leyendo cause_master...")
  cm <- as.data.table(read_auto(cause_path))
  
  req_dt <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "deaths_observed", "deaths_post_redistribution", "deaths_final",
    "correction_factor_completeness", "pandemic_excess_component", "run_id"
  )
  miss_dt <- setdiff(req_dt, names(dt))
  if (length(miss_dt) > 0L) {
    stop("Faltan columnas en death_cause_final: ", paste(miss_dt, collapse = ", "))
  }
  
  req_br <- c(
    "descendant_concept_id", "ancestor_concept_id",
    "ancestor_level", "descendant_is_terminal"
  )
  miss_br <- setdiff(req_br, names(br))
  if (length(miss_br) > 0L) {
    stop("Faltan columnas en cause_hierarchy_bridge: ", paste(miss_br, collapse = ", "))
  }
  
  req_cm <- c("cause_concept_id", "cause_level", "cause_name", "parent_concept_id", "is_terminal")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  dt <- dt[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    deaths_observed = as.numeric(deaths_observed),
    deaths_post_redistribution = as.numeric(deaths_post_redistribution),
    deaths_final = as.numeric(deaths_final),
    correction_factor_completeness = as.numeric(correction_factor_completeness),
    pandemic_excess_component = as.numeric(pandemic_excess_component),
    input_run_id = as.character(run_id)
  )]
  
  br <- br[, .(
    descendant_concept_id = as.integer(descendant_concept_id),
    ancestor_concept_id = as.integer(ancestor_concept_id),
    ancestor_level = as.integer(ancestor_level),
    descendant_is_terminal = as.logical(descendant_is_terminal)
  )]
  
  cm <- cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    parent_concept_id = as.integer(parent_concept_id),
    is_terminal = as.logical(is_terminal)
  )]
  
  # ----------------------------------------------------------
  # LEAF-ONLY EXPLÍCITO
  # ----------------------------------------------------------
  msg("Forzando input leaf-only / is_terminal == TRUE...")
  
  cm_meta <- unique(cm[, .(
    cause_concept_id,
    cause_level,
    cause_name,
    parent_concept_id,
    is_terminal
  )])
  
  dt_meta <- merge(
    dt,
    cm_meta,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  qc_input_terminality <- dt_meta[, .(
    n_rows_input = .N,
    n_rows_terminal = sum(is_terminal == TRUE, na.rm = TRUE),
    n_rows_non_terminal = sum(is_terminal == FALSE, na.rm = TRUE),
    n_rows_missing_terminality = sum(is.na(is_terminal))
  )]
  
  fwrite(qc_input_terminality, file.path(CFG$qc_dir, "qc_input_terminality.csv"))
  
  if (qc_input_terminality$n_rows_missing_terminality[1] > 0L) {
    stop("QC HARD FAIL: hay filas de death_cause_final sin terminalidad resoluble.")
  }
  
  dt_leaf <- dt_meta[is_terminal == TRUE]
  
  if (nrow(dt_leaf) == 0L) {
    stop("QC HARD FAIL: tras forzar leaf-only no quedaron filas en death_cause_final.")
  }
  
  dt_leaf <- dt_leaf[, .(
    deaths_observed = sum(deaths_observed, na.rm = TRUE),
    deaths_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE),
    pandemic_excess_component = sum(pandemic_excess_component, na.rm = TRUE),
    cause_level = unique(cause_level)[1],
    cause_name = unique(cause_name)[1],
    parent_concept_id = unique(parent_concept_id)[1],
    is_terminal = unique(is_terminal)[1],
    input_run_id = unique(input_run_id)[1]
  ), by = .(year_id, location_id, sex_id, age, cause_concept_id)]
  
  dt_leaf[, deaths_final_net_of_pandemic := safe_nonneg(deaths_final - pandemic_excess_component)]
  dt_leaf[, correction_factor_completeness := safe_ratio(deaths_final_net_of_pandemic, deaths_post_redistribution)]
  
  dup_leaf <- dt_leaf[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]
  fwrite(dup_leaf, file.path(CFG$qc_dir, "qc_leaf_duplicate_pk.csv"))
  if (nrow(dup_leaf) > 0L) {
    stop("QC HARD FAIL: leaf-only consolidado mantiene PK duplicada.")
  }
  
  # ----------------------------------------------------------
  # BRIDGE SOLO TERMINALES
  # ----------------------------------------------------------
  msg("Expandiendo terminales a ancestros vía bridge...")
  
  br_use <- br[
    descendant_is_terminal == TRUE &
      ancestor_level %in% CFG$keep_only_levels,
    .(
      terminal_cause_concept_id = descendant_concept_id,
      cause_concept_id = ancestor_concept_id,
      cause_level_bridge = ancestor_level
    )
  ]
  
  if (nrow(br_use) == 0L) {
    stop("La expansión bridge terminal->ancestro quedó vacía.")
  }
  
  x <- merge(
    dt_leaf[, .(
      year_id, location_id, sex_id, age,
      terminal_cause_concept_id = cause_concept_id,
      deaths_observed,
      deaths_post_redistribution,
      deaths_final,
      deaths_final_net_of_pandemic,
      pandemic_excess_component
    )],
    br_use,
    by = "terminal_cause_concept_id",
    all = FALSE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  
  if (nrow(x) == 0L) {
    stop("La expansión terminal -> ancestro quedó vacía.")
  }
  
  out_abs <- x[, .(
    deaths_observed = sum(deaths_observed, na.rm = TRUE),
    deaths_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE),
    deaths_final_net_of_pandemic = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    pandemic_excess_component = sum(pandemic_excess_component, na.rm = TRUE),
    cause_level_bridge = unique(cause_level_bridge)[1]
  ), by = .(year_id, location_id, sex_id, age, cause_concept_id)]
  
  # unir metadatos SOLO por cause_concept_id
  out_final <- merge(
    out_abs,
    unique(cm[, .(cause_concept_id, cause_level_cm = cause_level, cause_name, parent_concept_id, is_terminal)]),
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # resolver cause_level: preferir el bridge, luego cm
  out_final[, cause_level := fifelse(!is.na(cause_level_bridge), cause_level_bridge, cause_level_cm)]
  
  # fallback robusto para nodos no presentes en cause_master, especialmente TOTAL / nivel 0
  out_final[is.na(cause_name) & cause_level == 0L, cause_name := "TOTAL"]
  out_final[is.na(parent_concept_id) & cause_level == 0L, parent_concept_id := NA_integer_]
  out_final[is.na(is_terminal) & cause_level == 0L, is_terminal := FALSE]
  
  # para cualquier otro ancestro faltante, dejar marcadores explícitos
  out_final[is.na(cause_name), cause_name := paste0("CAUSE_", cause_concept_id)]
  out_final[is.na(is_terminal), is_terminal := FALSE]
  
  # parent_concept_id puede quedar NA en raíces; eso no debe romper
  qc_missing_cause_meta <- out_final[
    is.na(cause_level) | is.na(cause_name) | is.na(is_terminal),
    .N
  ]
  
  fwrite(
    out_final[is.na(cause_level) | is.na(cause_name) | is.na(is_terminal)],
    file.path(CFG$qc_dir, "qc_missing_cause_meta_rows.csv")
  )
  
  fwrite(
    data.table(n_missing_cause_meta = qc_missing_cause_meta),
    file.path(CFG$qc_dir, "qc_missing_cause_meta.csv")
  )
  
  if (qc_missing_cause_meta > 0L) {
    stop("QC HARD FAIL: faltan metadatos esenciales de causa tras el roll-up.")
  }
  
  out_final[, correction_factor_completeness := safe_ratio(
    deaths_final_net_of_pandemic,
    deaths_post_redistribution
  )]
  
  out_final[, `:=`(
    deaths_observed = safe_nonneg(deaths_observed),
    deaths_post_redistribution = safe_nonneg(deaths_post_redistribution),
    deaths_final = safe_nonneg(deaths_final),
    deaths_final_net_of_pandemic = safe_nonneg(deaths_final_net_of_pandemic),
    pandemic_excess_component = safe_nonneg(pandemic_excess_component)
  )]
  
  out_final[, run_id := run_id]
  
  out_final[, c("cause_level_bridge", "cause_level_cm") := NULL]
  
  setcolorder(out_final, c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_level", "cause_name", "parent_concept_id", "is_terminal",
    "deaths_observed", "deaths_post_redistribution", "deaths_final",
    "correction_factor_completeness", "pandemic_excess_component",
    "deaths_final_net_of_pandemic", "run_id"
  ))
  
  setorder(out_final, year_id, location_id, sex_id, age, cause_level, cause_concept_id)
  
  # ----------------------------------------------------------
  # QC
  # ----------------------------------------------------------
  msg("Corriendo QC...")
  
  qc_by_level <- out_final[, .(
    n_rows = .N,
    total_observed = sum(deaths_observed, na.rm = TRUE),
    total_post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
    total_final = sum(deaths_final, na.rm = TRUE),
    total_pandemic = sum(pandemic_excess_component, na.rm = TRUE)
  ), by = .(cause_level)][order(cause_level)]
  
  leaf_before <- dt_leaf[, .(
    obs_before = sum(deaths_observed, na.rm = TRUE),
    post_before = sum(deaths_post_redistribution, na.rm = TRUE),
    final_before = sum(deaths_final, na.rm = TRUE),
    pandemic_before = sum(pandemic_excess_component, na.rm = TRUE)
  ), by = .(year_id, sex_id)]
  
  leaf_after <- out_final[is_terminal == TRUE, .(
    obs_after = sum(deaths_observed, na.rm = TRUE),
    post_after = sum(deaths_post_redistribution, na.rm = TRUE),
    final_after = sum(deaths_final, na.rm = TRUE),
    pandemic_after = sum(pandemic_excess_component, na.rm = TRUE)
  ), by = .(year_id, sex_id)]
  
  qc_total_consistency <- merge(
    leaf_before, leaf_after,
    by = c("year_id", "sex_id"),
    all = TRUE,
    sort = TRUE
  )
  
  qc_total_consistency[, `:=`(
    diff_obs = obs_after - obs_before,
    diff_post = post_after - post_before,
    diff_final = final_after - final_before,
    diff_pandemic = pandemic_after - pandemic_before
  )]
  
  qc_dup_pk <- out_final[, .N, by = .(
    year_id, location_id, sex_id, age, cause_concept_id
  )][N > 1]
  
  fwrite(qc_by_level, file.path(CFG$qc_dir, "qc_by_level.csv"))
  fwrite(qc_total_consistency, file.path(CFG$qc_dir, "qc_total_consistency.csv"))
  fwrite(qc_dup_pk, file.path(CFG$qc_dir, "qc_duplicate_pk.csv"))
  
  if (nrow(qc_dup_pk) > 0L) {
    stop("QC HARD FAIL: PK duplicada en death_cause_final_hierarchical.")
  }
  
  qc_total_consistency[, `:=`(
    tol_obs = pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(obs_before), abs(obs_after), 1)),
    tol_post = pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(post_before), abs(post_after), 1)),
    tol_final = pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(final_before), abs(final_after), 1)),
    tol_pandemic = pmax(CFG$qc_abs_tol, CFG$qc_rel_tol * pmax(abs(pandemic_before), abs(pandemic_after), 1))
  )]
  
  bad_mass <- qc_total_consistency[
    is.na(diff_obs) | is.na(diff_post) | is.na(diff_final) | is.na(diff_pandemic) |
      abs(diff_obs) > tol_obs |
      abs(diff_post) > tol_post |
      abs(diff_final) > tol_final |
      abs(diff_pandemic) > tol_pandemic
  ]
  
  if (nrow(bad_mass) > 0L) {
    stop("QC HARD FAIL: el roll-up alteró la masa de las causas terminales. Revisar qc_total_consistency.csv")
  }
  
  if (any(out_final$deaths_final < 0, na.rm = TRUE) ||
      any(out_final$deaths_observed < 0, na.rm = TRUE) ||
      any(out_final$deaths_post_redistribution < 0, na.rm = TRUE) ||
      any(out_final$pandemic_excess_component < 0, na.rm = TRUE)) {
    stop("QC HARD FAIL: hay valores negativos en death_cause_final_hierarchical.")
  }
  
  # para el chequeo padre=hijos, usar un cm ampliado con los ancestros presentes en out_final
  cm_for_qc <- unique(rbindlist(list(
    cm[, .(cause_concept_id, parent_concept_id)],
    out_final[, .(cause_concept_id, parent_concept_id)]
  ), use.names = TRUE, fill = TRUE))
  
  qc_cause_hard_rollup(out_final, cm_for_qc)
  
  # ----------------------------------------------------------
  # EXPORT
  # ----------------------------------------------------------
  msg("Exportando...")
  out_csv <- file.path(CFG$out_dir, paste0(CFG$table_name, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$table_name, ".parquet"))
  out_dict <- file.path(CFG$out_dir, paste0(CFG$table_name, "_dictionary_ext.csv"))
  
  dict_ext <- build_dictionary_ext(out_final)
  
  write_csv_parquet(out_final, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(dict_ext, out_dict)
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(out_final),
    n_cols = ncol(out_final),
    notes = "CSV jerárquico con roll-up L0-L4 endurecido leaf-only"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_parquet,
    n_rows = nrow(out_final),
    n_cols = ncol(out_final),
    notes = "Parquet jerárquico con roll-up L0-L4 endurecido leaf-only"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "dictionary_ext",
    artifact_path = out_dict,
    n_rows = nrow(dict_ext),
    n_cols = ncol(dict_ext),
    notes = "Diccionario extendido jerárquico"
  )
  
  for (p in c(
    file.path(CFG$qc_dir, "qc_input_terminality.csv"),
    file.path(CFG$qc_dir, "qc_leaf_duplicate_pk.csv"),
    file.path(CFG$qc_dir, "qc_by_level.csv"),
    file.path(CFG$qc_dir, "qc_total_consistency.csv"),
    file.path(CFG$qc_dir, "qc_missing_cause_meta.csv"),
    file.path(CFG$qc_dir, "qc_missing_cause_meta_rows.csv"),
    file.path(CFG$qc_dir, "qc_duplicate_pk.csv"),
    file.path(CFG$qc_dir, "qc_parent_children_consistency.csv")
  )) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "qc",
      artifact_path = p,
      n_rows = tryCatch(nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(ncol(fread(p)), error = function(e) NA_integer_),
      notes = "QC 08b_rollup_death_cause_final"
    )
  }
  
  register_run_finish(
    run_id,
    status = "success",
    message = "08b_rollup_death_cause_final completado"
  )
  
  msg("OK -> CSV: ", out_csv)
  msg("OK -> Parquet: ", out_parquet)
  msg("OK -> Dictionary: ", out_dict)
  msg("OK -> QC dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})