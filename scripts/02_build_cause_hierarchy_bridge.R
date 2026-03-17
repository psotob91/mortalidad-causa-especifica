#!/usr/bin/env Rscript

# ============================================================
# 02_build_cause_hierarchy_bridge.R
# ------------------------------------------------------------
# Construye tabla de ancestros para roll-up jerárquico:
# una fila por descendant -> ancestor
#
# Salidas:
#   data/final/cause_hierarchy_bridge/cause_hierarchy_bridge.csv
#   data/final/cause_hierarchy_bridge/cause_hierarchy_bridge.parquet
#   data/final/cause_hierarchy_bridge/cause_hierarchy_bridge_dictionary_ext.csv
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

CFG <- list(
  version = "v1.0.0",
  dataset_id = "cause_hierarchy_bridge",
  table_name = "cause_hierarchy_bridge",
  
  input_cause_master = here("data", "final", "cause_master", "cause_master.csv"),
  
  out_dir = here("data", "final", "cause_hierarchy_bridge"),
  qc_dir  = here("data", "derived", "qc", "cause_hierarchy_bridge"),
  out_stem = "cause_hierarchy_bridge",
  
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

if (!file.exists(CFG$input_cause_master)) {
  stop("No existe cause_master: ", CFG$input_cause_master)
}

cm <- fread(CFG$input_cause_master)

req <- c("cause_concept_id", "parent_concept_id", "cause_level", "cause_name", "is_terminal")
miss <- setdiff(req, names(cm))
if (length(miss) > 0) {
  stop("Faltan columnas en cause_master: ", paste(miss, collapse = ", "))
}

cm[, cause_concept_id := as.integer(cause_concept_id)]
cm[, parent_concept_id := as.integer(parent_concept_id)]
cm[, cause_level := as.integer(cause_level)]

# ------------------------------------------------------------
# Validaciones previas
# ------------------------------------------------------------
dup_id <- cm[, .N, by = cause_concept_id][N > 1]
fwrite(dup_id, file.path(CFG$qc_dir, "qc_duplicate_concept_id.csv"))
if (nrow(dup_id) > 0) {
  stop("cause_master tiene cause_concept_id duplicados.")
}

bad_parent <- cm[
  !is.na(parent_concept_id) & !parent_concept_id %in% cm$cause_concept_id
]
fwrite(bad_parent, file.path(CFG$qc_dir, "qc_bad_parent_links.csv"))
if (nrow(bad_parent) > 0) {
  stop("cause_master tiene parent_concept_id inválidos.")
}

# mapa rápido
parent_map <- cm[, .(
  cause_concept_id,
  parent_concept_id,
  cause_level,
  cause_name
)]
setkey(parent_map, cause_concept_id)

# ------------------------------------------------------------
# Construcción del bridge
# ------------------------------------------------------------
# descendant_concept_id
# ancestor_concept_id
# ancestor_level
# distance
# is_self
# descendant_is_terminal

res <- vector("list", nrow(cm))

for (i in seq_len(nrow(cm))) {
  desc_id <- cm$cause_concept_id[i]
  desc_name <- cm$cause_name[i]
  desc_level <- cm$cause_level[i]
  desc_terminal <- cm$is_terminal[i]
  
  rows <- list(
    data.table(
      descendant_concept_id = desc_id,
      descendant_name = desc_name,
      descendant_level = desc_level,
      descendant_is_terminal = as.logical(desc_terminal),
      ancestor_concept_id = desc_id,
      ancestor_name = desc_name,
      ancestor_level = desc_level,
      distance = 0L,
      is_self = TRUE
    )
  )
  
  cur_id <- desc_id
  dist <- 0L
  seen <- integer()
  
  repeat {
    if (is.na(cur_id) || cur_id %in% seen) break
    seen <- c(seen, cur_id)
    
    cur_row <- parent_map[.(cur_id)]
    if (nrow(cur_row) == 0L) break
    
    par_id <- cur_row$parent_concept_id[1]
    if (is.na(par_id)) break
    
    par_row <- parent_map[.(par_id)]
    if (nrow(par_row) == 0L) {
      warning("No se encontró padre para concept_id = ", par_id)
      break
    }
    
    dist <- dist + 1L
    
    rows[[length(rows) + 1L]] <- data.table(
      descendant_concept_id = desc_id,
      descendant_name = desc_name,
      descendant_level = desc_level,
      descendant_is_terminal = as.logical(desc_terminal),
      ancestor_concept_id = par_id,
      ancestor_name = par_row$cause_name[1],
      ancestor_level = par_row$cause_level[1],
      distance = dist,
      is_self = FALSE
    )
    
    cur_id <- par_id
  }
  
  res[[i]] <- rbindlist(rows, use.names = TRUE)
}

bridge <- rbindlist(res, use.names = TRUE)

# ------------------------------------------------------------
# Validaciones del bridge
# ------------------------------------------------------------
bridge <- unique(
  bridge,
  by = c("descendant_concept_id", "ancestor_concept_id", "distance")
)

setorder(bridge, descendant_concept_id, distance, ancestor_concept_id)

# cada nodo debe tener exactamente una self-row
qc_self <- bridge[, .(
  n_self = sum(is_self),
  min_distance = min(distance),
  max_distance = max(distance)
), by = descendant_concept_id]

bad_self <- qc_self[n_self != 1L | min_distance != 0L]
fwrite(bad_self, file.path(CFG$qc_dir, "qc_bad_self_rows.csv"))
if (nrow(bad_self) > 0) {
  stop("Bridge inválido: problemas en self rows.")
}

# no debe haber ciclos
cycles <- bridge[descendant_concept_id == ancestor_concept_id & distance > 0L]
fwrite(cycles, file.path(CFG$qc_dir, "qc_cycles.csv"))
if (nrow(cycles) > 0) {
  stop("Bridge inválido: se detectaron ciclos.")
}

# cada descendiente debe tener ancestros en orden ascendente de distancia
qc_dist <- bridge[, .(
  n_rows = .N,
  max_distance = max(distance)
), by = descendant_concept_id]
fwrite(qc_dist, file.path(CFG$qc_dir, "qc_distance_summary.csv"))

# total de ancestros por nivel
qc_anc_levels <- bridge[, .N, by = .(descendant_level, ancestor_level)][order(descendant_level, ancestor_level)]
fwrite(qc_anc_levels, file.path(CFG$qc_dir, "qc_ancestor_levels.csv"))

# resumen general
qc_summary <- data.table(
  metric = c(
    "n_rows_bridge",
    "n_unique_descendants",
    "n_unique_ancestors",
    "n_terminal_descendants",
    "max_distance"
  ),
  value = c(
    nrow(bridge),
    uniqueN(bridge$descendant_concept_id),
    uniqueN(bridge$ancestor_concept_id),
    uniqueN(bridge[descendant_is_terminal == TRUE, descendant_concept_id]),
    max(bridge$distance)
  )
)
fwrite(qc_summary, file.path(CFG$qc_dir, "qc_summary.csv"))

# ------------------------------------------------------------
# Diccionario
# ------------------------------------------------------------
dict_dt <- data.table(
  column_name = names(bridge),
  description = c(
    "ID del nodo descendiente",
    "Nombre del nodo descendiente",
    "Nivel jerárquico del descendiente",
    "Indicador de si el descendiente es terminal",
    "ID del nodo ancestro",
    "Nombre del nodo ancestro",
    "Nivel jerárquico del ancestro",
    "Distancia jerárquica entre descendiente y ancestro",
    "Indicador de fila identidad (self-row)"
  ),
  data_type = sapply(bridge, function(x) class(x)[1]),
  n_non_missing = sapply(bridge, function(x) sum(!is.na(x))),
  n_missing = sapply(bridge, function(x) sum(is.na(x))),
  n_unique = sapply(bridge, uniqueN)
)

# ------------------------------------------------------------
# Export
# ------------------------------------------------------------
out_csv <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".csv"))
out_parquet <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".parquet"))
out_dict <- file.path(CFG$out_dir, paste0(CFG$out_stem, "_dictionary_ext.csv"))

fwrite(bridge, out_csv)
arrow::write_parquet(bridge, out_parquet)
fwrite(dict_dt, out_dict)

msg("OK: cause_hierarchy_bridge generado")
msg(" - ", out_csv)
msg(" - ", out_parquet)
msg(" - ", out_dict)

msg("Resumen:")
msg(" - descendientes únicos: ", uniqueN(bridge$descendant_concept_id))
msg(" - ancestros únicos: ", uniqueN(bridge$ancestor_concept_id))
msg(" - max_distance: ", max(bridge$distance))
msg(" - filas: ", nrow(bridge))