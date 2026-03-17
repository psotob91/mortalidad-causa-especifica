#!/usr/bin/env Rscript

# ============================================================
# 09b_reconcile_mortality_hierarchy.R
# ------------------------------------------------------------
# Objetivo:
#   Reconciliar jerárquicamente la mortalidad suavizada para que:
#     1) La geografía sea aditiva:
#        suma deptos (1:25) = nacional aditivo (9000)
#     2) La jerarquía de causas sea exacta:
#        terminales -> ancestros (L4/L3/L2/L1/TOTAL)
#
# Enfoque deadline-mode, elegante y estable:
#   - truth geográfica primaria = departamentos 1:25
#   - truth de causas primaria   = causas terminales
#   - se reconstruyen los demás nodos por suma exacta
#   - las tasas NO se reconcilian directamente:
#       se recalculan desde deaths_smoothed_consistent / population
#
# Entradas:
#   - mortality_rate_cause_smoothed
#   - cause_hierarchy_bridge
#   - cause_master
#   - population_result
#
# Salidas:
#   data/final/mortality_rate_cause_smoothed_reconciled/
#     - mortality_rate_cause_smoothed_reconciled.csv
#     - mortality_rate_cause_smoothed_reconciled.parquet
#     - mortality_rate_cause_smoothed_reconciled_dictionary_ext.csv
#
# QC mínimo vital:
#   - geografía: 9000 == suma exacta deptos
#   - causas: padre == suma exacta hijos
#   - no negativos
#   - preservación de masa en la base depto-terminal
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
  library(yaml)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

CFG <- list(
  version = "v0.1.0_reconciled_deadline",
  dataset_id = "mortality_rate_cause_smoothed_reconciled",
  table_name = "mortality_rate_cause_smoothed_reconciled",
  
  input_smoothed_candidates = c(
    here("data", "final", "mortality_rate_cause_smoothed", "mortality_rate_cause_smoothed.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed", "mortality_rate_cause_smoothed.csv")
  ),
  
  input_bridge_candidates = c(
    here("data", "final", "cause_hierarchy_bridge", "cause_hierarchy_bridge.parquet"),
    here("data", "final", "cause_hierarchy_bridge", "cause_hierarchy_bridge.csv")
  ),
  
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  external_yaml_path = here("config", "external_sources.yml"),
  
  out_dir = here("data", "final", "mortality_rate_cause_smoothed_reconciled"),
  qc_dir  = here("data", "derived", "qc", "09b_reconcile_mortality_hierarchy"),
  
  years = 2018:2024,
  base_locations = 1:25,
  national_additive_id = 9000L,
  keep_cause_levels = c(0L, 1L, 2L, 3L, 4L),
  rate_multiplier = 100000,
  
  geo_abs_tol = 1e-8,
  geo_rel_tol = 1e-10,
  cause_abs_tol = 1e-8,
  cause_rel_tol = 1e-10,
  
  verbose = TRUE
)

for (d in c(CFG$out_dir, CFG$qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

ensure_project_dirs()
ensure_catalog_files()

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

resolve_external_path2 <- function(external_yaml, key) {
  ys <- yaml::read_yaml(external_yaml)
  if (is.null(ys$external_datasets[[key]]$path)) {
    stop("No se encontró path para external dataset: ", key)
  }
  rel <- ys$external_datasets[[key]]$path
  normalizePath(file.path(here(), rel), winslash = "/", mustWork = FALSE)
}

detect_col <- function(dt, candidates, label) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    stop("No se encontró columna para ", label, ". Candidatas: ",
         paste(candidates, collapse = ", "))
  }
  hit[1]
}

safe_rate <- function(num, den, mult = 100000) {
  out <- rep(NA_real_, length(num))
  ok <- !is.na(num) & !is.na(den) & den > 0
  out[ok] <- mult * num[ok] / den[ok]
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

# ------------------------------------------------------------
# QC helper: geografía aditiva hard
# ------------------------------------------------------------

qc_geo_hard <- function(dt, base_locations, national_additive_id,
                        abs_tol = CFG$geo_abs_tol,
                        rel_tol = CFG$geo_rel_tol) {
  # Prohibir nacional original si existe en vista reconciliada
  if (any(dt$location_id == 0L, na.rm = TRUE)) {
    stop("QC HARD FAIL: vista reconciliada contiene location_id=0. Debe excluirse.")
  }
  
  if (!any(dt$location_id == national_additive_id, na.rm = TRUE)) {
    stop("QC HARD FAIL: vista reconciliada no contiene nacional aditivo 9000.")
  }
  
  missing_base <- setdiff(base_locations, unique(dt$location_id))
  if (length(missing_base) > 0L) {
    stop("QC HARD FAIL: faltan deptos base en vista reconciliada: ",
         paste(missing_base, collapse = ", "))
  }
  
  chk_dept <- dt[
    location_id %in% base_locations,
    .(dept_sum = sum(deaths_smoothed_consistent, na.rm = TRUE)),
    by = .(year_id, sex_id, age, cause_concept_id)
  ]
  
  chk_nat <- dt[
    location_id == national_additive_id,
    .(nat_val = sum(deaths_smoothed_consistent, na.rm = TRUE)),
    by = .(year_id, sex_id, age, cause_concept_id)
  ]
  
  chk <- merge(
    chk_dept, chk_nat,
    by = c("year_id", "sex_id", "age", "cause_concept_id"),
    all = TRUE
  )
  
  if (anyNA(chk$dept_sum) || anyNA(chk$nat_val)) {
    fwrite(chk, file.path(CFG$qc_dir, "qc_geo_hard_compare.csv"))
    stop("QC HARD FAIL: geografía reconciliada con cobertura incompleta depto/nacional.")
  }
  
  chk[, diff := nat_val - dept_sum]
  chk[, tol := pmax(abs_tol, rel_tol * pmax(abs(nat_val), abs(dept_sum), 1))]
  chk[, pass := abs(diff) <= tol]
  
  fwrite(chk, file.path(CFG$qc_dir, "qc_geo_hard_compare.csv"))
  
  bad <- chk[pass == FALSE]
  
  if (nrow(bad) > 0L) {
    top <- bad[order(-abs(diff))][1:min(.N, 20)]
    stop(
      "QC HARD FAIL: nacional aditivo no es suma exacta de deptos dentro de tolerancia.\n",
      paste(capture.output(print(top)), collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

# ------------------------------------------------------------
# QC helper: jerarquía de causas exacta
# ------------------------------------------------------------
qc_cause_hard <- function(dt, cm,
                          abs_tol = CFG$cause_abs_tol,
                          rel_tol = CFG$cause_rel_tol) {
  # validar padre == suma hijos solo para nodos con hijos explícitos
  kids <- cm[
    !is.na(parent_concept_id) & cause_concept_id != parent_concept_id,
    .(parent_concept_id, child_concept_id = cause_concept_id)
  ]
  
  if (nrow(kids) == 0L) return(invisible(TRUE))
  
  child_sum <- merge(
    dt[, .(
      year_id, location_id, sex_id, age, cause_concept_id,
      deaths_smoothed_consistent
    )],
    kids,
    by.x = "cause_concept_id",
    by.y = "child_concept_id",
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(children_sum = sum(deaths_smoothed_consistent, na.rm = TRUE)),
    by = .(year_id, location_id, sex_id, age, cause_concept_id = parent_concept_id)
  ]
  
  parent_val <- dt[, .(
    year_id, location_id, sex_id, age, cause_concept_id,
    parent_val = deaths_smoothed_consistent
  )]
  
  chk <- merge(
    child_sum, parent_val,
    by = c("year_id", "location_id", "sex_id", "age", "cause_concept_id"),
    all = FALSE
  )
  
  chk[, diff := parent_val - children_sum]
  chk[, tol := pmax(abs_tol, rel_tol * pmax(abs(parent_val), abs(children_sum), 1))]
  chk[, pass := abs(diff) <= tol]
  
  fwrite(chk, file.path(CFG$qc_dir, "qc_cause_hard_compare.csv"))
  
  bad <- chk[pass == FALSE]
  
  if (nrow(bad) > 0L) {
    top <- bad[order(-abs(diff))][1:min(.N, 20)]
    stop(
      "QC HARD FAIL: padres no coinciden con suma de hijos dentro de tolerancia.\n",
      paste(capture.output(print(top)), collapse = "\n")
    )
  }
  
  invisible(TRUE)
}

tryCatch({
  
  msg("Resolviendo inputs.")
  smoothed_path <- first_existing(CFG$input_smoothed_candidates)
  bridge_path   <- first_existing(CFG$input_bridge_candidates)
  cause_path    <- first_existing(CFG$input_cause_candidates)
  
  if (is.na(smoothed_path)) stop("No encontré mortality_rate_cause_smoothed.")
  if (is.na(bridge_path)) stop("No encontré cause_hierarchy_bridge.")
  if (is.na(cause_path)) stop("No encontré cause_master.")
  if (!file.exists(CFG$external_yaml_path)) stop("No existe external_sources.yml")
  
  pop_path <- resolve_external_path2(CFG$external_yaml_path, "population_result")
  if (!file.exists(pop_path)) stop("No existe population_result: ", pop_path)
  
  msg("Leyendo mortality_rate_cause_smoothed.")
  dt <- as.data.table(read_auto(smoothed_path))
  
  msg("Leyendo cause_hierarchy_bridge.")
  br <- as.data.table(read_auto(bridge_path))
  
  msg("Leyendo cause_master.")
  cm <- as.data.table(read_auto(cause_path))
  
  msg("Leyendo population_result.")
  pop <- as.data.table(read_auto(pop_path))
  
  req_dt <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "population", "deaths_final", "deaths_smoothed",
    "cause_level", "cause_name", "parent_concept_id", "is_terminal"
  )
  miss_dt <- setdiff(req_dt, names(dt))
  if (length(miss_dt) > 0L) {
    stop("Faltan columnas en mortality_rate_cause_smoothed: ", paste(miss_dt, collapse = ", "))
  }
  
  req_br <- c("descendant_concept_id", "ancestor_concept_id", "ancestor_level", "descendant_is_terminal")
  miss_br <- setdiff(req_br, names(br))
  if (length(miss_br) > 0L) {
    stop("Faltan columnas en cause_hierarchy_bridge: ", paste(miss_br, collapse = ", "))
  }
  
  req_cm <- c("cause_concept_id", "cause_level", "cause_name", "parent_concept_id", "is_terminal")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  pop_col <- detect_col(pop, c("population"), "population")
  
  # Tipos
  dt[, `:=`(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    population = as.numeric(population),
    deaths_final = as.numeric(deaths_final),
    deaths_smoothed = as.numeric(deaths_smoothed),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    parent_concept_id = as.integer(parent_concept_id),
    is_terminal = as.logical(is_terminal)
  )]
  
  br[, `:=`(
    descendant_concept_id = as.integer(descendant_concept_id),
    ancestor_concept_id = as.integer(ancestor_concept_id),
    ancestor_level = as.integer(ancestor_level),
    descendant_is_terminal = as.logical(descendant_is_terminal)
  )]
  
  cm[, `:=`(
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    parent_concept_id = as.integer(parent_concept_id),
    is_terminal = as.logical(is_terminal)
  )]
  
  pop <- pop[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    population = as.numeric(get(pop_col))
  )][year_id %in% CFG$years]
  
  # limpiar
  dt <- dt[
    year_id %in% CFG$years &
      sex_id %in% c(8507L, 8532L) &
      age >= 0L & age <= 110L
  ]
  
  dt[is.na(deaths_final), deaths_final := 0]
  dt[is.na(deaths_smoothed), deaths_smoothed := 0]
  dt[deaths_final < 0, deaths_final := 0]
  dt[deaths_smoothed < 0, deaths_smoothed := 0]
  
  # ----------------------------------------------------------------
  # 1) Base canónica para reconciliar:
  #    departamentos 1:25 + causas terminales solamente
  # ----------------------------------------------------------------
  msg("Preparando base depto-terminal.")
  cm_term <- cm[is_terminal == TRUE & cause_concept_id > 0,
                .(cause_concept_id, cause_level, cause_name, parent_concept_id, is_terminal)]
  
  base_leaf <- dt[
    location_id %in% CFG$base_locations & is_terminal == TRUE,
    .(year_id, location_id, sex_id, age, cause_concept_id,
      population, deaths_final, deaths_smoothed)
  ]
  
  # si por alguna razón faltan metas de terminalidad en dt, reforzar con maestro
  base_leaf <- merge(
    base_leaf,
    cm_term,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # consolidar por PK base
  base_leaf <- base_leaf[, .(
    population = unique(population)[1],
    deaths_final = sum(deaths_final, na.rm = TRUE),
    deaths_smoothed = sum(deaths_smoothed, na.rm = TRUE),
    cause_level = unique(cause_level)[1],
    cause_name = unique(cause_name)[1],
    parent_concept_id = unique(parent_concept_id)[1],
    is_terminal = unique(is_terminal)[1]
  ), by = .(year_id, location_id, sex_id, age, cause_concept_id)]
  
  # QC: PK única base
  dups_base <- base_leaf[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]
  fwrite(dups_base, file.path(CFG$qc_dir, "qc_base_leaf_duplicate_pk.csv"))
  if (nrow(dups_base) > 0L) {
    stop("Base depto-terminal tiene PK duplicada. Revisar qc_base_leaf_duplicate_pk.csv")
  }
  
  # Masa base antes de reconciliar
  qc_mass_before <- base_leaf[, .(
    deaths_final_sum = sum(deaths_final, na.rm = TRUE),
    deaths_smoothed_sum = sum(deaths_smoothed, na.rm = TRUE)
  ), by = .(year_id, sex_id)][order(year_id, sex_id)]
  
  # ----------------------------------------------------------------
  # 2) Reconciliación geográfica:
  #    construir nacional aditivo 9000 = suma deptos
  # ----------------------------------------------------------------
  msg("Reconstruyendo nacional aditivo 9000.")
  nat_leaf <- base_leaf[, .(
    population = sum(population, na.rm = TRUE),
    deaths_final = sum(deaths_final, na.rm = TRUE),
    deaths_smoothed = sum(deaths_smoothed, na.rm = TRUE),
    cause_level = unique(cause_level)[1],
    cause_name = unique(cause_name)[1],
    parent_concept_id = unique(parent_concept_id)[1],
    is_terminal = unique(is_terminal)[1]
  ), by = .(year_id, sex_id, age, cause_concept_id)]
  
  nat_leaf[, location_id := CFG$national_additive_id]
  
  geo_leaf <- rbindlist(
    list(base_leaf, nat_leaf),
    use.names = TRUE,
    fill = TRUE
  )
  
  setcolorder(geo_leaf, c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "population", "deaths_final", "deaths_smoothed",
    "cause_level", "cause_name", "parent_concept_id", "is_terminal"
  ))
  
  # ----------------------------------------------------------------
  # 3) Reconciliación de causas:
  #    reconstruir ancestros desde terminales usando el bridge
  # ----------------------------------------------------------------
  msg("Reconstruyendo ancestros por suma exacta desde terminales.")
  term_bridge <- br[
    descendant_is_terminal == TRUE &
      ancestor_level %in% CFG$keep_cause_levels,
    .(terminal_cause_concept_id = descendant_concept_id,
      cause_concept_id = ancestor_concept_id,
      cause_level = ancestor_level)
  ]
  
  # expandir terminal -> ancestro
  x <- merge(
    geo_leaf[, .(
      year_id, location_id, sex_id, age,
      terminal_cause_concept_id = cause_concept_id,
      population, deaths_final, deaths_smoothed
    )],
    term_bridge,
    by = "terminal_cause_concept_id",
    all = FALSE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  
  if (nrow(x) == 0L) {
    stop("La expansión terminal -> ancestro quedó vacía.")
  }
  
  # Para causas, la población no se suma entre hijos.
  # Se conserva una sola vez por celda geográfica/edad/sexo/año/causa.
  # Por eso la población se toma luego desde la grilla poblacional.
  out_abs <- x[, .(
    deaths_final = sum(deaths_final, na.rm = TRUE),
    deaths_smoothed_consistent = sum(deaths_smoothed, na.rm = TRUE)
  ), by = .(year_id, location_id, sex_id, age, cause_concept_id, cause_level)]
  
  # añadir metadatos de causa
  out_abs <- merge(
    out_abs,
    cm[, .(cause_concept_id, cause_level, cause_name, parent_concept_id, is_terminal)],
    by = c("cause_concept_id", "cause_level"),
    all.x = TRUE,
    sort = FALSE
  )
  
  # ----------------------------------------------------------------
  # 4) Adjuntar población consistente:
  #    usar pop externa para deptos y reconstruir 9000 por suma exacta
  # ----------------------------------------------------------------
  msg("Adjuntando población consistente.")
  pop_base <- pop[
    location_id %in% CFG$base_locations,
    .(year_id, location_id, sex_id, age, population)
  ]
  
  pop_nat <- pop_base[, .(
    population = sum(population, na.rm = TRUE)
  ), by = .(year_id, sex_id, age)]
  pop_nat[, location_id := CFG$national_additive_id]
  
  pop_hier <- rbindlist(
    list(pop_base, pop_nat),
    use.names = TRUE,
    fill = TRUE
  )
  
  # merge de población a salida reconciliada
  out <- merge(
    out_abs,
    pop_hier,
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  
  out[, mortality_rate_smoothed_consistent := safe_rate(
    deaths_smoothed_consistent,
    population,
    CFG$rate_multiplier
  )]
  out[, mortality_rate_unit := paste0("per_", CFG$rate_multiplier)]
  
  # trazabilidad
  out[, reconciliation_method_geo := "sum_departments_to_9000"]
  out[, reconciliation_method_cause := "sum_terminals_to_ancestors_via_bridge"]
  out[, input_run_id := run_id]
  
  # ordenar
  setorder(out, year_id, location_id, sex_id, age, cause_level, cause_concept_id)
  
  # ----------------------------------------------------------------
  # 5) QC mínimo vital
  # ----------------------------------------------------------------
  msg("Corriendo QC mínimo vital.")
  
  # no negativos
  qc_nonnegative <- out[, .(
    n_rows = .N,
    n_neg_deaths_final = sum(deaths_final < 0, na.rm = TRUE),
    n_neg_deaths_smoothed_consistent = sum(deaths_smoothed_consistent < 0, na.rm = TRUE),
    n_neg_rate = sum(mortality_rate_smoothed_consistent < 0, na.rm = TRUE),
    n_missing_population = sum(is.na(population)),
    n_missing_rate = sum(is.na(mortality_rate_smoothed_consistent))
  )]
  
  # masa base depto-terminal preservada
  qc_mass_after <- out[
    location_id %in% CFG$base_locations & is_terminal == TRUE,
    .(
      deaths_final_sum = sum(deaths_final, na.rm = TRUE),
      deaths_smoothed_consistent_sum = sum(deaths_smoothed_consistent, na.rm = TRUE)
    ),
    by = .(year_id, sex_id)
  ][order(year_id, sex_id)]
  
  # masa base depto-terminal preservada
  qc_mass_after <- out[
    location_id %in% CFG$base_locations & is_terminal == TRUE,
    .(
      deaths_final_sum_after = sum(deaths_final, na.rm = TRUE),
      deaths_smoothed_sum_after = sum(deaths_smoothed_consistent, na.rm = TRUE)
    ),
    by = .(year_id, sex_id)
  ][order(year_id, sex_id)]
  
  qc_mass_before2 <- copy(qc_mass_before)[, .(
    year_id,
    sex_id,
    deaths_final_sum_before = deaths_final_sum,
    deaths_smoothed_sum_before = deaths_smoothed_sum
  )]
  
  qc_mass_compare <- merge(
    qc_mass_before2,
    qc_mass_after,
    by = c("year_id", "sex_id"),
    all = TRUE,
    sort = TRUE
  )
  
  qc_mass_compare[, diff_final := deaths_final_sum_after - deaths_final_sum_before]
  qc_mass_compare[, diff_smoothed := deaths_smoothed_sum_after - deaths_smoothed_sum_before]
  
  # geografía hard
  qc_geo_hard(out, CFG$base_locations, CFG$national_additive_id)
  
  # causas hard
  qc_cause_hard(out, cm[cause_level %in% CFG$keep_cause_levels])
  
  # resúmenes auxiliares
  qc_by_level <- out[, .(
    n_rows = .N,
    total_final = sum(deaths_final, na.rm = TRUE),
    total_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE)
  ), by = .(cause_level)][order(cause_level)]
  
  qc_geo_summary <- out[, .(
    total_final = sum(deaths_final, na.rm = TRUE),
    total_smoothed_consistent = sum(deaths_smoothed_consistent, na.rm = TRUE)
  ), by = .(year_id, location_id)][order(year_id, location_id)]
  
  # guardar qc
  qc_nonnegative_path <- file.path(CFG$qc_dir, "qc_nonnegative.csv")
  qc_mass_compare_path <- file.path(CFG$qc_dir, "qc_mass_compare.csv")
  qc_by_level_path <- file.path(CFG$qc_dir, "qc_by_level.csv")
  qc_geo_summary_path <- file.path(CFG$qc_dir, "qc_geo_summary.csv")
  
  fwrite(qc_nonnegative, qc_nonnegative_path)
  fwrite(qc_mass_compare, qc_mass_compare_path)
  fwrite(qc_by_level, qc_by_level_path)
  fwrite(qc_geo_summary, qc_geo_summary_path)
  
  # fallar si masa base cambió
  bad_mass <- qc_mass_compare[
    is.na(diff_final) | is.na(diff_smoothed) |
      abs(diff_final) > CFG$geo_abs_tol | abs(diff_smoothed) > CFG$geo_abs_tol
  ]
  if (nrow(bad_mass) > 0L) {
    stop("QC HARD FAIL: la reconciliación alteró la masa base depto-terminal. Revisar qc_mass_compare.csv")
  }
  
  if (qc_nonnegative$n_neg_deaths_final[1] > 0L ||
      qc_nonnegative$n_neg_deaths_smoothed_consistent[1] > 0L ||
      qc_nonnegative$n_neg_rate[1] > 0L) {
    stop("QC HARD FAIL: hay valores negativos tras reconciliación.")
  }
  
  # PK final
  dup_pk <- out[, .N, by = .(year_id, location_id, sex_id, age, cause_concept_id)][N > 1]
  dup_pk_path <- file.path(CFG$qc_dir, "qc_duplicate_pk.csv")
  fwrite(dup_pk, dup_pk_path)
  if (nrow(dup_pk) > 0L) {
    stop("QC HARD FAIL: PK duplicada en salida reconciliada.")
  }
  
  # ----------------------------------------------------------------
  # 6) Export
  # ----------------------------------------------------------------
  msg("Exportando salida reconciliada.")
  out_csv <- file.path(CFG$out_dir, paste0(CFG$table_name, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$table_name, ".parquet"))
  out_dict <- file.path(CFG$out_dir, paste0(CFG$table_name, "_dictionary_ext.csv"))
  
  dict_ext <- build_dictionary_ext(out)
  
  write_csv_parquet(out, csv_path = out_csv, parquet_path = out_parquet)
  fwrite(dict_ext, out_dict)
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_csv,
    n_rows = nrow(out),
    n_cols = ncol(out),
    notes = "CSV mortalidad suavizada reconciliada por geografía y causas"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_parquet,
    n_rows = nrow(out),
    n_cols = ncol(out),
    notes = "Parquet mortalidad suavizada reconciliada por geografía y causas"
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
    notes = "Diccionario extendido de la salida reconciliada"
  )
  
  for (p in c(
    qc_nonnegative_path,
    qc_mass_compare_path,
    qc_by_level_path,
    qc_geo_summary_path,
    dup_pk_path
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
      notes = "QC 09b_reconcile_mortality_hierarchy"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "09b_reconcile_mortality_hierarchy completado")
  
  msg("OK -> CSV: ", out_csv)
  msg("OK -> Parquet: ", out_parquet)
  msg("OK -> Dictionary: ", out_dict)
  msg("OK -> QC dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})