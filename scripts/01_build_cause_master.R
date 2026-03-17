#!/usr/bin/env Rscript

# ============================================================
# 01_build_cause_master.R
# ------------------------------------------------------------
# Construye maestro canónico de causas OMOP-like.
# Versión corregida:
# - preserva IDs fuente: cause_concept_id = 9000000 + codigo
# - mantiene unicidad por path_key
# - parent mapping robusto por parent_path_key
# - PROPAGA icd10_regex a niveles 1-3 combinando regex hijas
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(janitor)
  library(stringi)
  library(here)
  library(arrow)
})

CFG <- list(
  version = "v1.0.2",
  dataset_id = "cause_master",
  table_name = "cause_master",
  
  input_xlsx  = here("data", "raw", "cause_mapping", "codificacion-causas-enfermedad.xlsx"),
  input_sheet = "OMS-Enfermedades",
  input_patch_csv = here("data", "raw", "cause_mapping", "maestro_causas_mortalidad_morbilidad_patch_master.csv"),
  
  out_dir   = here("data", "final", "cause_master"),
  qc_dir    = here("data", "derived", "qc", "cause_master"),
  out_stem  = "cause_master",
  
  concept_prefix = 9000000L,
  add_total_node = TRUE,
  total_name = "Total",
  
  add_pandemic_node_if_missing = TRUE,
  pandemic_parent_name = "Otras causas",
  pandemic_name = "Otros / relacionado con pandemia",
  
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

clean_chr <- function(x) {
  x <- as.character(x)
  x <- stringi::stri_trim_both(x)
  x[x %in% c("", "NA", "NULL")] <- NA_character_
  x
}

pick_col <- function(nms, candidates) {
  hit <- intersect(candidates, nms)
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

make_path_key <- function(level1 = NA_character_,
                          level2 = NA_character_,
                          level3 = NA_character_,
                          level4 = NA_character_) {
  paste(
    fifelse(is.na(level1), "", level1),
    fifelse(is.na(level2), "", level2),
    fifelse(is.na(level3), "", level3),
    fifelse(is.na(level4), "", level4),
    sep = " || "
  )
}

safe_unique <- function(dt, by_cols) {
  dt <- unique(dt, by = by_cols)
  setorderv(dt, by_cols)
  dt
}

combine_regex <- function(x) {
  x <- clean_chr(x)
  x <- unique(na.omit(x))
  x <- x[x != ""]
  if (length(x) == 0L) return(NA_character_)
  if (length(x) == 1L) return(x)
  paste0("(", paste(x, collapse = "|"), ")")
}

if (!file.exists(CFG$input_xlsx)) {
  stop("No existe archivo fuente: ", CFG$input_xlsx)
}

raw <- as.data.table(readxl::read_excel(CFG$input_xlsx, sheet = CFG$input_sheet))
setnames(raw, janitor::make_clean_names(names(raw)))

msg("Columnas detectadas: ", paste(names(raw), collapse = ", "))

col_codigo <- pick_col(names(raw), c("codigo", "code", "bod_code"))
col_n1     <- pick_col(names(raw), c("nivel_01", "nivel_1", "level_1"))
col_n2     <- pick_col(names(raw), c("nivel_02", "nivel_2", "level_2"))
col_n3     <- pick_col(names(raw), c("nivel_03", "nivel_3", "level_3"))
col_n4     <- pick_col(names(raw), c("nivel_04", "nivel_4", "level_4"))
col_regex  <- pick_col(names(raw), c("regex_r", "regex", "icd10_regex"))

needed <- c(col_codigo, col_n1, col_n2, col_n3, col_n4)
if (any(is.na(needed))) {
  stop("No pude detectar codigo y/o columnas de niveles jerárquicos en el Excel.")
}

src <- raw[, .(
  codigo = get(col_codigo),
  nivel_01 = get(col_n1),
  nivel_02 = get(col_n2),
  nivel_03 = get(col_n3),
  nivel_04 = get(col_n4),
  icd10_regex = if (!is.na(col_regex)) get(col_regex) else NA_character_
)]

for (j in names(src)) {
  if (is.character(src[[j]]) || is.factor(src[[j]])) {
    src[, (j) := clean_chr(get(j))]
  }
}
src[, codigo := suppressWarnings(as.integer(codigo))]

src <- src[
  !(is.na(nivel_01) & is.na(nivel_02) & is.na(nivel_03) & is.na(nivel_04))
]

src[, path_key_source := make_path_key(nivel_01, nivel_02, nivel_03, nivel_04)]
qc_src_codigo_conflict <- src[
  !is.na(codigo),
  .(n_codigo = uniqueN(codigo), codigos = paste(sort(unique(codigo)), collapse = ",")),
  by = path_key_source
][n_codigo > 1]
fwrite(qc_src_codigo_conflict, file.path(CFG$qc_dir, "qc_source_path_multiple_codigos.csv"))
if (nrow(qc_src_codigo_conflict) > 0) {
  stop("Hay path_key fuente con múltiples codigo. Revisar qc_source_path_multiple_codigos.csv")
}

# ------------------------------------------------------------
# Construcción por nivel
# CAMBIO CLAVE:
# - todos los niveles arrastran icd10_regex_source
# - al colapsar por path_key se usa combine_regex()
# ------------------------------------------------------------
build_level_dt <- function(dt, level_num) {
  if (level_num == 1L) {
    out <- dt[!is.na(nivel_01), .(
      cause_level = 1L,
      cause_name = nivel_01,
      parent_name = NA_character_,
      level_1_name = nivel_01,
      level_2_name = NA_character_,
      level_3_name = NA_character_,
      level_4_name = NA_character_,
      path_key = make_path_key(nivel_01, NA, NA, NA),
      parent_path_key = NA_character_,
      codigo_fuente = codigo,
      icd10_regex_source = icd10_regex
    )]
  } else if (level_num == 2L) {
    out <- dt[!is.na(nivel_02), .(
      cause_level = 2L,
      cause_name = nivel_02,
      parent_name = nivel_01,
      level_1_name = nivel_01,
      level_2_name = nivel_02,
      level_3_name = NA_character_,
      level_4_name = NA_character_,
      path_key = make_path_key(nivel_01, nivel_02, NA, NA),
      parent_path_key = make_path_key(nivel_01, NA, NA, NA),
      codigo_fuente = codigo,
      icd10_regex_source = icd10_regex
    )]
  } else if (level_num == 3L) {
    out <- dt[!is.na(nivel_03), .(
      cause_level = 3L,
      cause_name = nivel_03,
      parent_name = nivel_02,
      level_1_name = nivel_01,
      level_2_name = nivel_02,
      level_3_name = nivel_03,
      level_4_name = NA_character_,
      path_key = make_path_key(nivel_01, nivel_02, nivel_03, NA),
      parent_path_key = make_path_key(nivel_01, nivel_02, NA, NA),
      codigo_fuente = codigo,
      icd10_regex_source = icd10_regex
    )]
  } else if (level_num == 4L) {
    out <- dt[!is.na(nivel_04), .(
      cause_level = 4L,
      cause_name = nivel_04,
      parent_name = nivel_03,
      level_1_name = nivel_01,
      level_2_name = nivel_02,
      level_3_name = nivel_03,
      level_4_name = nivel_04,
      path_key = make_path_key(nivel_01, nivel_02, nivel_03, nivel_04),
      parent_path_key = make_path_key(nivel_01, nivel_02, nivel_03, NA),
      codigo_fuente = codigo,
      icd10_regex_source = icd10_regex
    )]
  } else {
    stop("Nivel no soportado")
  }
  
  out <- out[, .(
    cause_level = first(cause_level),
    cause_name = first(cause_name),
    parent_name = first(parent_name),
    level_1_name = first(level_1_name),
    level_2_name = first(level_2_name),
    level_3_name = first(level_3_name),
    level_4_name = first(level_4_name),
    parent_path_key = first(parent_path_key),
    codigo_fuente = unique(na.omit(codigo_fuente))[1],
    icd10_regex = combine_regex(icd10_regex_source)
  ), by = path_key]
  
  out
}

lvl1 <- build_level_dt(src, 1L)
lvl2 <- build_level_dt(src, 2L)
lvl3 <- build_level_dt(src, 3L)
lvl4 <- build_level_dt(src, 4L)

nodes <- rbindlist(list(lvl1, lvl2, lvl3, lvl4), use.names = TRUE, fill = TRUE)
nodes <- safe_unique(nodes, by_cols = "path_key")

# Nodo total
if (isTRUE(CFG$add_total_node)) {
  total_row <- data.table(
    cause_level = 0L,
    cause_name = CFG$total_name,
    parent_name = NA_character_,
    level_1_name = NA_character_,
    level_2_name = NA_character_,
    level_3_name = NA_character_,
    level_4_name = NA_character_,
    path_key = "TOTAL",
    parent_path_key = NA_character_,
    codigo_fuente = NA_integer_,
    icd10_regex = NA_character_
  )
  nodes <- rbind(nodes, total_row, fill = TRUE)
}

# Nodo pandemia opcional
if (isTRUE(CFG$add_pandemic_node_if_missing)) {
  pandemic_parent_key <- make_path_key(CFG$pandemic_parent_name, NA, NA, NA)
  if (!nodes[path_key == pandemic_parent_key, .N]) {
    nodes <- rbind(
      nodes,
      data.table(
        cause_level = 1L,
        cause_name = CFG$pandemic_parent_name,
        parent_name = if (isTRUE(CFG$add_total_node)) CFG$total_name else NA_character_,
        level_1_name = CFG$pandemic_parent_name,
        level_2_name = NA_character_,
        level_3_name = NA_character_,
        level_4_name = NA_character_,
        path_key = pandemic_parent_key,
        parent_path_key = if (isTRUE(CFG$add_total_node)) "TOTAL" else NA_character_,
        codigo_fuente = NA_integer_,
        icd10_regex = NA_character_
      ),
      fill = TRUE
    )
  }
  
  pandemic_key <- make_path_key(CFG$pandemic_parent_name, CFG$pandemic_name, NA, NA)
  if (!nodes[path_key == pandemic_key, .N]) {
    nodes <- rbind(
      nodes,
      data.table(
        cause_level = 2L,
        cause_name = CFG$pandemic_name,
        parent_name = CFG$pandemic_parent_name,
        level_1_name = CFG$pandemic_parent_name,
        level_2_name = CFG$pandemic_name,
        level_3_name = NA_character_,
        level_4_name = NA_character_,
        path_key = pandemic_key,
        parent_path_key = pandemic_parent_key,
        codigo_fuente = NA_integer_,
        icd10_regex = NA_character_
      ),
      fill = TRUE
    )
  }
}

nodes <- safe_unique(nodes, by_cols = "path_key")

if (isTRUE(CFG$add_total_node)) {
  nodes[cause_level == 1L & is.na(parent_path_key), parent_path_key := "TOTAL"]
  nodes[cause_level == 1L & is.na(parent_name), parent_name := CFG$total_name]
}

setorder(nodes, cause_level, level_1_name, level_2_name, level_3_name, level_4_name, cause_name)

# IDs estables: 9000000 + codigo_fuente
nodes[, cause_concept_id := fifelse(
  !is.na(codigo_fuente),
  CFG$concept_prefix + as.integer(codigo_fuente),
  NA_integer_
)]

qc_dup_codigo_fuente <- nodes[!is.na(codigo_fuente), .N, by = codigo_fuente][N > 1]
fwrite(qc_dup_codigo_fuente, file.path(CFG$qc_dir, "qc_duplicate_codigo_fuente.csv"))
if (nrow(qc_dup_codigo_fuente) > 0) {
  nodes_check <- nodes[!is.na(codigo_fuente), .(n_path = uniqueN(path_key)), by = codigo_fuente][n_path > 1]
  fwrite(nodes_check, file.path(CFG$qc_dir, "qc_codigo_fuente_multiple_paths.csv"))
  if (nrow(nodes_check) > 0) {
    stop("Hay codigo_fuente asignado a múltiples path_key. Revisar qc_codigo_fuente_multiple_paths.csv")
  }
}

qc_dup_id_pre <- nodes[!is.na(cause_concept_id), .N, by = cause_concept_id][N > 1]
fwrite(qc_dup_id_pre, file.path(CFG$qc_dir, "qc_duplicate_source_concept_id.csv"))
if (nrow(qc_dup_id_pre) > 0) {
  nodes_check2 <- nodes[!is.na(cause_concept_id), .(n_path = uniqueN(path_key)), by = cause_concept_id][n_path > 1]
  fwrite(nodes_check2, file.path(CFG$qc_dir, "qc_source_concept_id_multiple_paths.csv"))
  if (nrow(nodes_check2) > 0) {
    stop("Hay cause_concept_id fuente asignado a múltiples path_key.")
  }
}

next_id <- max(nodes$cause_concept_id, na.rm = TRUE) + 1L
n_missing_ids <- nodes[is.na(cause_concept_id), .N]
if (n_missing_ids > 0) {
  nodes[is.na(cause_concept_id), cause_concept_id := seq.int(from = next_id, length.out = .N)]
}

nodes[, cause_code := fifelse(
  !is.na(codigo_fuente),
  as.character(codigo_fuente),
  fifelse(path_key == "TOTAL", "TOTAL",
          fifelse(cause_name == CFG$pandemic_name, "PANDEMIC_OTHER",
                  fifelse(cause_name == CFG$pandemic_parent_name, "PANDEMIC_PARENT",
                          paste0("ART_", cause_concept_id))))
)]

# Parent mapping
parent_lookup <- nodes[, .(parent_path_key = path_key, parent_concept_id = cause_concept_id)]
nodes <- merge(nodes, parent_lookup, by = "parent_path_key", all.x = TRUE, sort = FALSE)
nodes[path_key == "TOTAL", parent_concept_id := NA_integer_]

dup_id <- nodes[, .N, by = cause_concept_id][N > 1]
fwrite(dup_id, file.path(CFG$qc_dir, "qc_duplicate_concept_id.csv"))
if (nrow(dup_id) > 0) stop("Hay cause_concept_id duplicados.")

dup_path <- nodes[, .N, by = path_key][N > 1]
fwrite(dup_path, file.path(CFG$qc_dir, "qc_duplicate_path_key.csv"))
if (nrow(dup_path) > 0) stop("Hay path_key duplicados.")

bad_parent <- nodes[!is.na(parent_concept_id) & !parent_concept_id %in% nodes$cause_concept_id]
fwrite(bad_parent, file.path(CFG$qc_dir, "qc_bad_parent_links.csv"))
if (nrow(bad_parent) > 0) stop("Hay parent_concept_id inválidos.")

self_parent <- nodes[cause_concept_id == parent_concept_id]
fwrite(self_parent, file.path(CFG$qc_dir, "qc_self_parent.csv"))
if (nrow(self_parent) > 0) stop("Hay nodos que son su propio padre.")

child_counts <- nodes[!is.na(parent_concept_id), .N, by = parent_concept_id]
setnames(child_counts, c("parent_concept_id", "N"), c("cause_concept_id", "n_children"))
nodes <- merge(nodes, child_counts, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
nodes[is.na(n_children), n_children := 0L]
nodes[, is_terminal := n_children == 0L]

nodes[, is_garbage := FALSE]
nodes[, is_residual := grepl("\\botras?\\b|\\botros?\\b|residual", cause_name, ignore.case = TRUE)]
nodes[, is_covid_related := grepl("covid|pandemi", cause_name, ignore.case = TRUE)]

cause_master <- nodes[, .(
  cause_concept_id = as.integer(cause_concept_id),
  cause_code = as.character(cause_code),
  cause_name = as.character(cause_name),
  cause_level = as.integer(cause_level),
  parent_concept_id = as.integer(parent_concept_id),
  is_terminal = as.logical(is_terminal),
  is_garbage = as.logical(is_garbage),
  is_residual = as.logical(is_residual),
  is_covid_related = as.logical(is_covid_related),
  icd10_regex = as.character(icd10_regex),
  
  codigo_fuente = as.integer(codigo_fuente),
  parent_name = as.character(parent_name),
  path_key = as.character(path_key),
  parent_path_key = as.character(parent_path_key),
  level_1_name = as.character(level_1_name),
  level_2_name = as.character(level_2_name),
  level_3_name = as.character(level_3_name),
  level_4_name = as.character(level_4_name),
  n_children = as.integer(n_children)
)]

#
# ============================================================
# Patch opcional: clasificacion YLL / YLD / elegibilidad redistribucion
# ------------------------------------------------------------
# Regla:
# - SOLO amplía columnas del cause_master
# - NO debe cambiar nrow
# - clave de merge: cause_concept_id
# ============================================================

if (file.exists(CFG$input_patch_csv)) {
  msg("Aplicando patch de mortalidad/morbilidad: ", CFG$input_patch_csv)
  
  patch <- fread(CFG$input_patch_csv, encoding = "UTF-8")
  setDT(patch)
  setnames(patch, names(patch), trimws(names(patch)))
  
  # Normalizar nombres si vinieran levemente distintos
  if (!"cause_concept_id" %in% names(patch)) {
    alt_id <- intersect(
      c("source_concept_id", "target_concept_id", "concept_id"),
      names(patch)
    )
    if (length(alt_id) == 0L) {
      stop("El patch de cause master no trae cause_concept_id ni alias reconocible.")
    }
    setnames(patch, alt_id[1], "cause_concept_id")
  }
  
  patch[, cause_concept_id := as.integer(cause_concept_id)]
  
  # Mantener solo columnas realmente nuevas + llave
  cols_keep_patch <- intersect(
    c(
      "cause_concept_id",
      "yll_flag",
      "yld_flag",
      "burden_component_class",
      "mortality_plausibility",
      "redistribution_target_eligible_default",
      "redistribution_pool_default",
      "australia_check",
      "australia_note",
      "evidence_source"
    ),
    names(patch)
  )
  
  patch <- unique(patch[, ..cols_keep_patch], by = "cause_concept_id")
  
  # QC 1: duplicados en patch
  qc_patch_dup <- patch[, .N, by = cause_concept_id][N > 1]
  fwrite(qc_patch_dup, file.path(CFG$qc_dir, "qc_patch_duplicate_keys.csv"))
  if (nrow(qc_patch_dup) > 0L) {
    stop("Patch de cause master tiene cause_concept_id duplicados. Revisar qc_patch_duplicate_keys.csv")
  }
  
  # QC 2: resumen de match
  qc_patch_match_summary <- data.table(
    metric = c(
      "n_rows_cause_master_pre_patch",
      "n_rows_patch",
      "n_patch_ids_in_master",
      "n_patch_ids_not_in_master",
      "n_master_rows_without_patch"
    ),
    value = c(
      nrow(cause_master),
      nrow(patch),
      patch[cause_concept_id %in% cause_master$cause_concept_id, .N],
      patch[!cause_concept_id %in% cause_master$cause_concept_id, .N],
      cause_master[!cause_concept_id %in% patch$cause_concept_id, .N]
    )
  )
  fwrite(qc_patch_match_summary, file.path(CFG$qc_dir, "qc_patch_match_summary.csv"))
  
  qc_patch_not_in_master <- patch[!cause_concept_id %in% cause_master$cause_concept_id]
  fwrite(qc_patch_not_in_master, file.path(CFG$qc_dir, "qc_patch_ids_not_in_master.csv"))
  
  n_pre_patch <- nrow(cause_master)
  
  cause_master <- merge(
    cause_master,
    patch,
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  # Defaults seguros
  if (!"yll_flag" %in% names(cause_master)) cause_master[, yll_flag := NA_integer_]
  if (!"yld_flag" %in% names(cause_master)) cause_master[, yld_flag := NA_integer_]
  if (!"burden_component_class" %in% names(cause_master)) cause_master[, burden_component_class := NA_character_]
  if (!"mortality_plausibility" %in% names(cause_master)) cause_master[, mortality_plausibility := NA_character_]
  if (!"redistribution_target_eligible_default" %in% names(cause_master)) cause_master[, redistribution_target_eligible_default := NA_integer_]
  if (!"redistribution_pool_default" %in% names(cause_master)) cause_master[, redistribution_pool_default := NA_character_]
  if (!"australia_check" %in% names(cause_master)) cause_master[, australia_check := NA_character_]
  if (!"australia_note" %in% names(cause_master)) cause_master[, australia_note := NA_character_]
  if (!"evidence_source" %in% names(cause_master)) cause_master[, evidence_source := NA_character_]
  
  cause_master[, `:=`(
    yll_flag = as.integer(yll_flag),
    yld_flag = as.integer(yld_flag),
    burden_component_class = as.character(burden_component_class),
    mortality_plausibility = as.character(mortality_plausibility),
    redistribution_target_eligible_default = as.integer(redistribution_target_eligible_default),
    redistribution_pool_default = as.character(redistribution_pool_default),
    australia_check = as.character(australia_check),
    australia_note = as.character(australia_note),
    evidence_source = as.character(evidence_source)
  )]
  
  # ------------------------------------------------------------
  # Ajuste manual: categoria residual pandemia
  # ------------------------------------------------------------
  cause_master[cause_concept_id == 9001633, `:=`(
    yll_flag = 1L,
    yld_flag = 0L,
    burden_component_class = "yll_only"
  )]
  
  cause_master[cause_concept_id == 9000692, `:=`(
    yll_flag = 1L,
    yld_flag = 1L,
    burden_component_class = "both"
  )]
  
  # Defaults conservadores para nodos no presentes en patch:
  # - no bloquear mortalidad salvo que patch lo diga
  # - solo terminales yll_flag==0 quedan no elegibles por defecto
  cause_master[is.na(redistribution_target_eligible_default),
               redistribution_target_eligible_default := fifelse(
                 is_terminal == TRUE & !is.na(yll_flag) & yll_flag == 0L,
                 0L, 1L
               )]
  
  # QC 3: rowcount intacto
  qc_postmerge_rowcount <- data.table(
    metric = c("n_rows_pre_patch", "n_rows_post_patch", "delta"),
    value  = c(n_pre_patch, nrow(cause_master), nrow(cause_master) - n_pre_patch)
  )
  fwrite(qc_postmerge_rowcount, file.path(CFG$qc_dir, "qc_postmerge_rowcount.csv"))
  
  if (nrow(cause_master) != n_pre_patch) {
    stop("El patch alteró el número de filas de cause_master. Revisar qc_postmerge_rowcount.csv")
  }
  
  # QC 4: unicidad sigue intacta
  qc_dup_post_patch <- cause_master[, .N, by = cause_concept_id][N > 1]
  fwrite(qc_dup_post_patch, file.path(CFG$qc_dir, "qc_duplicate_concept_id_post_patch.csv"))
  if (nrow(qc_dup_post_patch) > 0L) {
    stop("El patch generó cause_concept_id duplicados. Revisar qc_duplicate_concept_id_post_patch.csv")
  }
  
  # QC 5: causas terminales yld-only / yll-only / both
  qc_patch_class_summary <- data.table(
    metric = c(
      "n_terminal_yll_only",
      "n_terminal_yld_only",
      "n_terminal_both",
      "n_terminal_unknown_class"
    ),
    value = c(
      cause_master[is_terminal == TRUE & burden_component_class == "yll_only", .N],
      cause_master[is_terminal == TRUE & burden_component_class == "yld_only", .N],
      cause_master[is_terminal == TRUE & burden_component_class == "both", .N],
      cause_master[is_terminal == TRUE & is.na(burden_component_class), .N]
    )
  )
  fwrite(qc_patch_class_summary, file.path(CFG$qc_dir, "qc_patch_class_summary.csv"))
  
} else {
  msg("No se encontró patch opcional de cause master. Se continúa sin patch.")
}

setorder(cause_master, cause_level, cause_concept_id)

qc_summary <- data.table(
  metric = c(
    "n_rows",
    "n_unique_concept_id",
    "n_unique_path_key",
    "n_with_codigo_fuente",
    "n_without_codigo_fuente",
    "n_level0",
    "n_level1",
    "n_level2",
    "n_level3",
    "n_level4",
    "n_terminal",
    "n_with_parent",
    "n_without_parent",
    "n_with_regex",
    "n_level1_with_regex",
    "n_level2_with_regex",
    "n_level3_with_regex",
    "n_level4_with_regex", 
    "n_yll_flag_1",
    "n_yll_flag_0",
    "n_yld_flag_1",
    "n_yld_flag_0",
    "n_terminal_target_eligible_1",
    "n_terminal_target_eligible_0"
  ),
  value = c(
    nrow(cause_master),
    uniqueN(cause_master$cause_concept_id),
    uniqueN(cause_master$path_key),
    cause_master[!is.na(codigo_fuente), .N],
    cause_master[is.na(codigo_fuente), .N],
    cause_master[cause_level == 0L, .N],
    cause_master[cause_level == 1L, .N],
    cause_master[cause_level == 2L, .N],
    cause_master[cause_level == 3L, .N],
    cause_master[cause_level == 4L, .N],
    cause_master[is_terminal == TRUE, .N],
    cause_master[!is.na(parent_concept_id), .N],
    cause_master[is.na(parent_concept_id), .N],
    cause_master[!is.na(icd10_regex) & icd10_regex != "", .N],
    cause_master[cause_level == 1L & !is.na(icd10_regex) & icd10_regex != "", .N],
    cause_master[cause_level == 2L & !is.na(icd10_regex) & icd10_regex != "", .N],
    cause_master[cause_level == 3L & !is.na(icd10_regex) & icd10_regex != "", .N],
    cause_master[cause_level == 4L & !is.na(icd10_regex) & icd10_regex != "", .N], 
    cause_master[yll_flag == 1L, .N],
    cause_master[yll_flag == 0L, .N],
    cause_master[yld_flag == 1L, .N],
    cause_master[yld_flag == 0L, .N],
    cause_master[is_terminal == TRUE & redistribution_target_eligible_default == 1L, .N],
    cause_master[is_terminal == TRUE & redistribution_target_eligible_default == 0L, .N]
  )
)
fwrite(qc_summary, file.path(CFG$qc_dir, "qc_summary.csv"))

out_csv     <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".csv"))
out_parquet <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".parquet"))
out_dict    <- file.path(CFG$out_dir, paste0(CFG$out_stem, "_dictionary_ext.csv"))

fwrite(cause_master, out_csv)
arrow::write_parquet(cause_master, out_parquet)

dict_dt <- data.table(
  column_name = names(cause_master),
  description = c(
    "ID único de la causa",
    "Código de la causa",
    "Nombre de la causa",
    "Nivel jerárquico",
    "ID del padre",
    "Indicador de nodo terminal",
    "Indicador de garbage",
    "Indicador de residual",
    "Indicador de relación con COVID/pandemia",
    "Regex ICD10 asociada",
    "Código fuente original del Excel",
    "Nombre del padre",
    "Clave jerárquica única",
    "Clave jerárquica del padre",
    "Nombre nivel 1",
    "Nombre nivel 2",
    "Nombre nivel 3",
    "Nombre nivel 4",
    "Número de hijos directos"
  ),
  data_type = sapply(cause_master, function(x) class(x)[1]),
  n_non_missing = sapply(cause_master, function(x) sum(!is.na(x))),
  n_missing = sapply(cause_master, function(x) sum(is.na(x))),
  n_unique = sapply(cause_master, uniqueN)
)
fwrite(dict_dt, out_dict)

msg("OK: cause_master generado")
msg(" - ", out_csv)
msg(" - ", out_parquet)
msg(" - ", out_dict)
msg("Chequeo concept_id únicos: ", uniqueN(cause_master$cause_concept_id), " / ", nrow(cause_master))
msg("Chequeo path_key únicos:   ", uniqueN(cause_master$path_key), " / ", nrow(cause_master))