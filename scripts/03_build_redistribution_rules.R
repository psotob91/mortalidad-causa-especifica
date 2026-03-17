#!/usr/bin/env Rscript

# ============================================================
# 03_build_redistribution_rules.R
# ------------------------------------------------------------
# Construye reglas canónicas de redistribución garbage en
# formato largo y expandidas a nodos terminales.
#
# Salidas:
#   data/final/redistribution_rules/redistribution_rules.csv
#   data/final/redistribution_rules/redistribution_rules.parquet
#   data/final/redistribution_rules/redistribution_rules_dictionary_ext.csv
#
# QC:
#   data/derived/qc/redistribution_rules/
#
# Soporta:
#   - sex_restriction opcional
#   - age_start / age_end opcionales
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(janitor)
  library(stringi)
  library(here)
  library(arrow)
})

# ------------------------------------------------------------
# Config
# ------------------------------------------------------------
CFG <- list(
  version = "v1.0.1",
  dataset_id = "death_garbage_redistribution_rules",
  table_name = "redistribution_rules",
  
  input_xlsx = here("data", "raw", "redistribution_rules", "codigos-redistribucion-muerte.xlsx"),
  input_sheet = 1,
  input_patch_targets_csv = here("data", "raw", "redistribution_rules", "patch_targets_redistribucion_excluir_yll.csv"),
  
  cause_master_path = here("data", "final", "cause_master", "cause_master.csv"),
  bridge_path = here("data", "final", "cause_hierarchy_bridge", "cause_hierarchy_bridge.csv"),
  
  out_dir = here("data", "final", "redistribution_rules"),
  qc_dir = here("data", "derived", "qc", "redistribution_rules"),
  out_stem = "redistribution_rules",
  
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------
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
  hit <- intersect(nms, candidates)
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

split_int_csv <- function(x) {
  x <- clean_chr(x)
  if (is.na(x)) return(integer())
  x <- gsub("\\s+", "", x)
  xs <- unlist(strsplit(x, ",", fixed = TRUE), use.names = FALSE)
  xs <- xs[nzchar(xs)]
  out <- suppressWarnings(as.integer(xs))
  out <- out[!is.na(out)]
  unique(out)
}

normalize_sex_restriction <- function(x) {
  x <- clean_chr(x)
  if (is.na(x)) return(NA_character_)
  
  z <- toupper(x)
  
  if (z %in% c("M", "MALE", "HOMBRE", "MASCULINO", "VARON", "VARÓN")) return("male")
  if (z %in% c("F", "FEMALE", "MUJER", "FEMENINO")) return("female")
  if (z %in% c("BOTH", "ALL", "TODOS", "TODAS", "AMBOS", "AMBAS")) return(NA_character_)
  
  x
}

safe_int <- function(x) suppressWarnings(as.integer(x))
safe_num <- function(x) suppressWarnings(as.numeric(x))

# ------------------------------------------------------------
# Validar insumos
# ------------------------------------------------------------
if (!file.exists(CFG$input_xlsx)) {
  stop("No existe archivo fuente de reglas: ", CFG$input_xlsx)
}
if (!file.exists(CFG$cause_master_path)) {
  stop("No existe cause_master: ", CFG$cause_master_path)
}
if (!file.exists(CFG$bridge_path)) {
  stop("No existe cause_hierarchy_bridge: ", CFG$bridge_path)
}

# ------------------------------------------------------------
# Leer fuentes
# ------------------------------------------------------------
raw <- as.data.table(readxl::read_excel(CFG$input_xlsx, sheet = CFG$input_sheet))
setnames(raw, janitor::make_clean_names(names(raw)))

cm <- fread(CFG$cause_master_path)
br <- fread(CFG$bridge_path)

msg("Columnas detectadas en reglas: ", paste(names(raw), collapse = ", "))

# ------------------------------------------------------------
# Detectar columnas
# ------------------------------------------------------------
col_rule_id    <- pick_col(names(raw), c("id", "rule_id"))
col_group_code <- pick_col(names(raw), c("rd_group_concept_id", "source_group_code", "rd_group_code"))
col_group_name <- pick_col(names(raw), c("grupo_de_redistribucion", "rd_group_name", "source_group_name"))
col_targets    <- pick_col(names(raw), c("target_concept_ids"))
col_sex        <- pick_col(names(raw), c("sex_restriction", "sexo_restriction", "sexo_restriccion"))
col_age_start  <- pick_col(names(raw), c("age_start", "edad_min", "edad_inicio"))
col_age_end    <- pick_col(names(raw), c("age_end", "edad_max", "edad_fin"))
col_notes      <- pick_col(names(raw), c("notes", "nota", "notas"))
col_regex      <- pick_col(names(raw), c("regex_r", "regex"))
col_method     <- pick_col(names(raw), c("metodo", "redistribution_method"))
col_scope      <- pick_col(names(raw), c("redistribution_scope", "alcance_de_enfermedades"))

needed <- c(col_rule_id, col_group_name, col_targets)
if (any(is.na(needed))) {
  stop(
    "No pude detectar columnas mínimas en el Excel de redistribución.\n",
    "Se requieren al menos: rule_id, source_group_name, target_concept_ids."
  )
}

# ------------------------------------------------------------
# Normalizar reglas fuente
# ------------------------------------------------------------
ru0 <- raw[, .(
  rule_id = get(col_rule_id),
  source_group_code = if (!is.na(col_group_code)) get(col_group_code) else NA,
  source_group_name = get(col_group_name),
  target_concept_ids = get(col_targets),
  sex_restriction = if (!is.na(col_sex)) get(col_sex) else NA,
  age_start = if (!is.na(col_age_start)) get(col_age_start) else NA,
  age_end = if (!is.na(col_age_end)) get(col_age_end) else NA,
  notes = if (!is.na(col_notes)) get(col_notes) else NA,
  regex_r = if (!is.na(col_regex)) get(col_regex) else NA,
  redistribution_method = if (!is.na(col_method)) get(col_method) else NA,
  redistribution_scope = if (!is.na(col_scope)) get(col_scope) else NA
)]

for (j in names(ru0)) {
  if (is.character(ru0[[j]]) || is.factor(ru0[[j]])) {
    ru0[, (j) := clean_chr(get(j))]
  }
}

ru0[, rule_id := as.character(rule_id)]
ru0[, source_group_code := fifelse(
  !is.na(source_group_code),
  as.character(source_group_code),
  paste0("GC_", rule_id)
)]
ru0[, source_group_name := as.character(source_group_name)]
ru0[, sex_restriction := vapply(sex_restriction, normalize_sex_restriction, character(1))]
ru0[, age_start := safe_int(age_start)]
ru0[, age_end := safe_int(age_end)]
ru0[, redistribution_method := fifelse(is.na(redistribution_method), "Asignación proporcional", redistribution_method)]

# Mantener solo reglas con targets declarados
ru0 <- ru0[!is.na(target_concept_ids)]

# ------------------------------------------------------------
# Validar cause_master / bridge
# ------------------------------------------------------------
req_cm <- c(
  "cause_concept_id", "cause_name", "cause_level", "is_terminal",
  "redistribution_target_eligible_default"
)
miss_cm <- setdiff(req_cm, names(cm))
if (length(miss_cm) > 0) {
  stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
}

req_br <- c("descendant_concept_id", "ancestor_concept_id")
miss_br <- setdiff(req_br, names(br))
if (length(miss_br) > 0) {
  stop("Faltan columnas en cause_hierarchy_bridge: ", paste(miss_br, collapse = ", "))
}

cm[, cause_concept_id := as.integer(cause_concept_id)]
cm[, cause_level := as.integer(cause_level)]
cm[, is_terminal := as.logical(is_terminal)]
cm[, redistribution_target_eligible_default := as.integer(redistribution_target_eligible_default)]
cm[is.na(redistribution_target_eligible_default), redistribution_target_eligible_default := 1L]

br[, descendant_concept_id := as.integer(descendant_concept_id)]
br[, ancestor_concept_id := as.integer(ancestor_concept_id)]

# terminales válidos
term <- cm[is_terminal == TRUE, .(
  terminal_cause_concept_id = cause_concept_id,
  terminal_cause_name = cause_name,
  terminal_cause_level = cause_level
)]

# bridge terminal -> ancestro
term_bridge <- merge(
  br,
  term,
  by.x = "descendant_concept_id",
  by.y = "terminal_cause_concept_id",
  all.y = TRUE,
  sort = FALSE
)

term_bridge <- unique(term_bridge[, .(
  terminal_cause_concept_id = descendant_concept_id,
  target_cause_concept_id = ancestor_concept_id
)])

# ------------------------------------------------------------
# Explode de target_concept_ids declarados por regla
# ------------------------------------------------------------
ru_targets_raw <- ru0[, {
  tids <- split_int_csv(target_concept_ids)
  if (length(tids) == 0L) {
    .(declared_target_concept_id = as.integer(NA))
  } else {
    .(declared_target_concept_id = tids)
  }
}, by = .(
  rule_id,
  source_group_code,
  source_group_name,
  sex_restriction,
  age_start,
  age_end,
  notes,
  regex_r,
  redistribution_method,
  redistribution_scope
)]

# QC: targets declarados faltantes en maestro
qc_bad_declared_targets <- ru_targets_raw[
  !is.na(declared_target_concept_id) &
    !declared_target_concept_id %in% cm$cause_concept_id
]
fwrite(qc_bad_declared_targets, file.path(CFG$qc_dir, "qc_bad_declared_target_ids.csv"))

if (nrow(qc_bad_declared_targets) > 0) {
  stop("Hay declared_target_concept_id que no existen en cause_master. Revisar qc_bad_declared_target_ids.csv")
}

# ------------------------------------------------------------
# Expandir targets declarados a terminales descendientes
# ------------------------------------------------------------
ru_targets_expanded <- merge(
  ru_targets_raw[!is.na(declared_target_concept_id)],
  term_bridge,
  by.x = "declared_target_concept_id",
  by.y = "target_cause_concept_id",
  all.x = TRUE,
  allow.cartesian = TRUE,
  sort = FALSE
)

# ------------------------------------------------------------
# Patch opcional: exclusión de targets no elegibles para YLL
# - Este patch es por rule_id + target_concept_id
# - Acepta varios alias de columna
# ------------------------------------------------------------
patch_targets <- data.table(
  rule_id = character(),
  target_cause_concept_id = integer(),
  patch_exclude_target = integer()
)

if (file.exists(CFG$input_patch_targets_csv)) {
  patch_targets <- fread(CFG$input_patch_targets_csv, encoding = "UTF-8")
  setDT(patch_targets)
  setnames(patch_targets, names(patch_targets), trimws(names(patch_targets)))
  
  # Detectar columna de rule_id
  if (!"rule_id" %in% names(patch_targets)) {
    alt_rule <- intersect(c("id", "rule", "ruleid"), names(patch_targets))
    if (length(alt_rule) == 0L) {
      stop("El patch de targets no trae rule_id ni alias reconocible.")
    }
    setnames(patch_targets, alt_rule[1], "rule_id")
  }
  
  # Detectar columna target
  if (!"target_cause_concept_id" %in% names(patch_targets)) {
    alt_target <- intersect(
      c(
        "target_concept_id",
        "cause_concept_id",
        "terminal_cause_concept_id",
        "concept_id"
      ),
      names(patch_targets)
    )
    if (length(alt_target) == 0L) {
      stop(
        "El patch de targets no trae target_cause_concept_id ",
        "ni target_concept_id ni alias reconocible."
      )
    }
    setnames(patch_targets, alt_target[1], "target_cause_concept_id")
  }
  
  # Detectar bandera de exclusión
  if (!"patch_exclude_target" %in% names(patch_targets)) {
    if ("redistribution_target_eligible_default" %in% names(patch_targets)) {
      patch_targets[, patch_exclude_target := fifelse(
        as.integer(redistribution_target_eligible_default) == 0L, 1L, 0L
      )]
    } else if ("recommended_action" %in% names(patch_targets)) {
      patch_targets[, patch_exclude_target := fifelse(
        as.character(recommended_action) == "exclude_from_yll_targets", 1L, 0L
      )]
    } else {
      patch_targets[, patch_exclude_target := 1L]
    }
  }
  
  patch_targets[, `:=`(
    rule_id = as.character(rule_id),
    target_cause_concept_id = as.integer(target_cause_concept_id),
    patch_exclude_target = as.integer(patch_exclude_target)
  )]
  
  patch_targets <- patch_targets[
    !is.na(rule_id) & !is.na(target_cause_concept_id)
  ][patch_exclude_target == 1L,
    .(rule_id, target_cause_concept_id, patch_exclude_target)
  ]
  
  # QC correcto: unicidad por rule_id + target
  qc_patch_targets_dup <- patch_targets[, .N, by = .(rule_id, target_cause_concept_id)][N > 1]
  fwrite(qc_patch_targets_dup, file.path(CFG$qc_dir, "qc_patch_targets_duplicate_keys.csv"))
  if (nrow(qc_patch_targets_dup) > 0L) {
    stop("Patch de targets tiene duplicados por rule_id + target_cause_concept_id.")
  }
  
  # dejar una sola fila por combinación
  patch_targets <- unique(patch_targets, by = c("rule_id", "target_cause_concept_id"))
}

# enriquecer expansión con elegibilidad desde cause_master
ru_targets_expanded <- merge(
  ru_targets_expanded,
  cm[, .(
    terminal_cause_concept_id = cause_concept_id,
    target_eligible_cm = redistribution_target_eligible_default
  )],
  by = "terminal_cause_concept_id",
  all.x = TRUE,
  sort = FALSE
)

ru_targets_expanded <- merge(
  ru_targets_expanded,
  patch_targets,
  by.x = c("rule_id", "terminal_cause_concept_id"),
  by.y = c("rule_id", "target_cause_concept_id"),
  all.x = TRUE,
  sort = FALSE
)

ru_targets_expanded[is.na(target_eligible_cm), target_eligible_cm := 1L]
ru_targets_expanded[is.na(patch_exclude_target), patch_exclude_target := 0L]

ru_targets_expanded[, target_removed_by_patch := fifelse(
  target_eligible_cm == 0L | patch_exclude_target == 1L,
  1L, 0L
)]

qc_no_terminal_descendants <- ru_targets_expanded[is.na(terminal_cause_concept_id)]
fwrite(qc_no_terminal_descendants, file.path(CFG$qc_dir, "qc_no_terminal_descendants.csv"))

if (nrow(qc_no_terminal_descendants) > 0) {
  stop("Hay targets declarados que no expanden a terminales. Revisar qc_no_terminal_descendants.csv")
}

# QC: targets removidos por patch
qc_patch_targets_removed <- ru_targets_expanded[target_removed_by_patch == 1L, .(
  rule_id,
  source_group_code,
  source_group_name,
  declared_target_concept_id,
  terminal_cause_concept_id,
  target_eligible_cm,
  patch_exclude_target
)]
fwrite(qc_patch_targets_removed, file.path(CFG$qc_dir, "qc_patch_targets_removed.csv"))

# Aplicar filtro
ru_targets_expanded_pre_filter_n <- nrow(ru_targets_expanded)

ru_targets_expanded <- ru_targets_expanded[target_removed_by_patch == 0L]

qc_patch_rowcount_summary <- data.table(
  metric = c(
    "n_rows_expanded_pre_filter",
    "n_rows_removed_by_patch",
    "n_rows_expanded_post_filter"
  ),
  value = c(
    ru_targets_expanded_pre_filter_n,
    nrow(qc_patch_targets_removed),
    nrow(ru_targets_expanded)
  )
)
fwrite(qc_patch_rowcount_summary, file.path(CFG$qc_dir, "qc_patch_rowcount_summary.csv"))

# QC: reglas que quedaron sin targets tras filtro
qc_rules_without_targets <- ru0[, .(rule_id)][
  !rule_id %in% unique(ru_targets_expanded$rule_id)
]
fwrite(qc_rules_without_targets, file.path(CFG$qc_dir, "qc_patch_rules_without_targets.csv"))

if (nrow(qc_rules_without_targets) > 0L) {
  stop("Hay reglas que quedaron sin targets luego del patch. Revisar qc_patch_rules_without_targets.csv")
}

# ------------------------------------------------------------
# Asignar pesos finales
# ------------------------------------------------------------
ru_targets_expanded[, n_declared_targets_postfilter := uniqueN(declared_target_concept_id), by = rule_id]
ru_targets_expanded[, declared_target_weight := 1 / n_declared_targets_postfilter]

ru_targets_expanded[, n_terminals_from_declared_postfilter :=
                      uniqueN(terminal_cause_concept_id),
                    by = .(rule_id, declared_target_concept_id)]

ru_targets_expanded[, terminal_weight_within_declared :=
                      1 / n_terminals_from_declared_postfilter]

ru_targets_expanded[, target_weight :=
                      declared_target_weight * terminal_weight_within_declared]

ru_targets_expanded[, target_weight := target_weight / sum(target_weight), by = rule_id]

# ------------------------------------------------------------
# Dataset final canónico
# ------------------------------------------------------------
rules_final <- unique(
  ru_targets_expanded[, .(
    rule_id,
    source_group_code,
    source_group_name,
    target_cause_concept_id = terminal_cause_concept_id,
    target_weight,
    sex_restriction,
    age_start,
    age_end,
    notes,
    declared_target_concept_id,
    regex_r,
    redistribution_method,
    redistribution_scope, 
    target_eligible_cm,
    patch_exclude_target,
    target_removed_by_patch
  )]
)

setorder(rules_final, rule_id, target_cause_concept_id)

# QC: pesos suman 1 por regla
qc_weights <- rules_final[, .(sum_weight = sum(target_weight)), by = rule_id]
fwrite(qc_weights, file.path(CFG$qc_dir, "qc_rule_weight_sums.csv"))

qc_patch_weight_sum_by_rule <- rules_final[, .(
  n_targets_final = .N,
  sum_weight = sum(target_weight)
), by = rule_id]
fwrite(qc_patch_weight_sum_by_rule, file.path(CFG$qc_dir, "qc_patch_weight_sum_by_rule.csv"))

bad_weights <- qc_weights[abs(sum_weight - 1) > 1e-10]
fwrite(bad_weights, file.path(CFG$qc_dir, "qc_bad_weight_sums.csv"))
if (nrow(bad_weights) > 0) {
  stop("Hay reglas cuyos target_weight no suman 1.")
}

# QC: targets finales son terminales
qc_non_terminal_final_targets <- merge(
  rules_final,
  cm[, .(target_cause_concept_id = cause_concept_id, is_terminal)],
  by = "target_cause_concept_id",
  all.x = TRUE,
  sort = FALSE
)[is_terminal != TRUE]

fwrite(qc_non_terminal_final_targets, file.path(CFG$qc_dir, "qc_non_terminal_final_targets.csv"))
if (nrow(qc_non_terminal_final_targets) > 0) {
  stop("Hay target_cause_concept_id finales que no son terminales. Revisar qc_non_terminal_final_targets.csv")
}

qc_summary <- data.table(
  metric = c(
    "n_rules",
    "n_rows_final",
    "n_unique_source_groups",
    "n_unique_final_targets",
    "n_rules_with_age_restriction",
    "n_rules_with_sex_restriction",
    "n_bad_declared_target_ids",
    "n_rules_without_terminal_descendants"
  ),
  value = c(
    uniqueN(rules_final$rule_id),
    nrow(rules_final),
    uniqueN(rules_final$source_group_code),
    uniqueN(rules_final$target_cause_concept_id),
    uniqueN(rules_final[!is.na(age_start) | !is.na(age_end), rule_id]),
    uniqueN(rules_final[!is.na(sex_restriction), rule_id]),
    nrow(qc_bad_declared_targets),
    uniqueN(qc_no_terminal_descendants$rule_id)
  )
)
fwrite(qc_summary, file.path(CFG$qc_dir, "qc_summary.csv"))

# ------------------------------------------------------------
# Diccionario
# ------------------------------------------------------------
dict_dt <- data.table(
  column_name = names(rules_final),
  description = c(
    "Identificador único de regla",
    "Código de grupo garbage origen",
    "Nombre de grupo garbage origen",
    "Causa terminal destino de redistribución",
    "Peso final de redistribución",
    "Restricción por sexo",
    "Edad mínima aplicable",
    "Edad máxima aplicable",
    "Notas metodológicas",
    "Target declarado originalmente en la fuente",
    "Regex de identificación del garbage code",
    "Método de redistribución",
    "Alcance de redistribución"
  ),
  data_type = sapply(rules_final, function(x) class(x)[1]),
  n_non_missing = sapply(rules_final, function(x) sum(!is.na(x))),
  n_missing = sapply(rules_final, function(x) sum(is.na(x))),
  n_unique = sapply(rules_final, uniqueN)
)

# ------------------------------------------------------------
# Export
# ------------------------------------------------------------
out_csv <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".csv"))
out_parquet <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".parquet"))
out_dict <- file.path(CFG$out_dir, paste0(CFG$out_stem, "_dictionary_ext.csv"))

fwrite(rules_final, out_csv)
arrow::write_parquet(rules_final, out_parquet)
fwrite(dict_dt, out_dict)

msg("OK: redistribution_rules generadas")
msg(" - ", out_csv)
msg(" - ", out_parquet)
msg(" - ", out_dict)
msg("Resumen:")
msg(" - n_rules: ", uniqueN(rules_final$rule_id))
msg(" - n_rows_final: ", nrow(rules_final))
msg(" - n_unique_final_targets: ", uniqueN(rules_final$target_cause_concept_id))
msg(" - max target_weight por regla (global): ", rules_final[, max(target_weight)])