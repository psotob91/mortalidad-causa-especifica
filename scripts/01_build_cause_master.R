#!/usr/bin/env Rscript

# ============================================================
# 01_build_cause_master.R
# ------------------------------------------------------------
# Upgrade conservador sobre el maestro antiguo.
#
# Estrategia:
# 1) construir el cause_master base con la lógica antigua
# 2) preservar path_key, parent_path_key, IDs y columnas legacy
# 3) aplicar patch opcional legacy casi intacto
# 4) agregar solo el upgrade pandémico necesario para OMS GHE 2021
# 5) NO usar regex amplia para COVID específico
# 6) OPRM existe como nodo taxonómico; su magnitud se calcula en 08
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
  library(janitor)
  library(stringi)
  library(here)
  library(arrow)
  library(yaml)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "spec_utils.R"))

CFG <- list(
  version = "v1.3.0_upgrade_legacy_cause_master_patch_mode",
  dataset_id = "cause_master",
  table_name = "cause_master",
  input_xlsx  = here("data", "raw", "cause_mapping", "codificacion-causas-enfermedad.xlsx"),
  input_sheet = "OMS-Enfermedades",
  input_patch_csv = here("data", "raw", "cause_mapping", "maestro_causas_mortalidad_morbilidad_patch_master.csv"),
  spec_out_path = here("config", "spec_cause_master.yml"),
  out_dir   = here("data", "final", "cause_master"),
  qc_dir    = here("data", "derived", "qc", "cause_master"),
  out_stem  = "cause_master",
  concept_prefix = 9000000L,
  add_total_node = TRUE,
  total_name = "Total",
  pandemic_parent_name = "Otras causas",
  pandemic_root_name = "IV. Other /pandemic-related",
  add_covid_specific_node_if_missing = TRUE,
  covid_specific_name = "COVID-19",
  covid_specific_code = "COVID_19",
  covid_specific_icd10_regex = "^(U07|U09|U10)",
  add_oprm_node_if_missing = TRUE,
  oprm_name = "Other pandemic related mortality (OPRM)",
  oprm_code = "OPRM",
  indirect_pandemic_names = c(
    "measles",
    "lower respiratory infection",
    "lower respiratory infections",
    "lri",
    "pertussis"
  ),
  indirect_pandemic_codes = c("MEASLES", "LRI", "PERTUSSIS"),
  indirect_pandemic_icd10_regex = c(
    "^(B05)",
    "^(J09|J1[0-8]|J20|J21|J22|P23|U04)",
    "^(A37)"
  ),
  verbose = TRUE
)

dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "
")
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

make_path_key <- function(level1 = NA_character_, level2 = NA_character_, level3 = NA_character_, level4 = NA_character_) {
  paste(
    fifelse(is.na(level1), "", level1),
    fifelse(is.na(level2), "", level2),
    fifelse(is.na(level3), "", level3),
    fifelse(is.na(level4), "", level4),
    sep = " || "
  )
}

safe_logical <- function(x) {
  y <- as.logical(x)
  y[is.na(y)] <- FALSE
  y
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

any_grepl_vec <- function(patterns, x) {
  out <- rep(FALSE, length(x))
  pats <- unique(na.omit(as.character(patterns)))
  if (length(pats) == 0L) return(out)
  for (p in pats) {
    hit <- tryCatch(grepl(p, x, perl = TRUE), error = function(e) rep(FALSE, length(x)))
    out <- out | fifelse(is.na(hit), FALSE, hit)
  }
  out
}

ensure_project_dirs()
ensure_catalog_files()
run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

tryCatch({
  if (!file.exists(CFG$input_xlsx)) stop("No existe archivo fuente: ", CFG$input_xlsx)
  
  msg("Leyendo maestro de causas.")
  raw <- as.data.table(readxl::read_excel(CFG$input_xlsx, sheet = CFG$input_sheet))
  setnames(raw, janitor::make_clean_names(names(raw)))
  msg("Columnas detectadas: ", paste(names(raw), collapse = ", "))
  
  col_codigo <- pick_col(names(raw), c("codigo", "code", "bod_code"))
  col_n1     <- pick_col(names(raw), c("nivel_01", "nivel_1", "level_1"))
  col_n2     <- pick_col(names(raw), c("nivel_02", "nivel_2", "level_2"))
  col_n3     <- pick_col(names(raw), c("nivel_03", "nivel_3", "level_3"))
  col_n4     <- pick_col(names(raw), c("nivel_04", "nivel_4", "level_4"))
  col_regex  <- pick_col(names(raw), c("regex_r", "regex", "icd10_regex"))
  col_cie10  <- pick_col(names(raw), c("cie10", "cie_10", "icd10", "icd10_list"))
  
  needed <- c(col_codigo, col_n1, col_n2, col_n3, col_n4)
  if (any(is.na(needed))) stop("No pude detectar codigo y/o columnas de niveles jerárquicos en el Excel.")
  
  src <- raw[, .(
    codigo = get(col_codigo),
    nivel_01 = get(col_n1),
    nivel_02 = get(col_n2),
    nivel_03 = get(col_n3),
    nivel_04 = get(col_n4),
    cie10 = if (!is.na(col_cie10)) get(col_cie10) else NA_character_,
    icd10_regex = if (!is.na(col_regex)) get(col_regex) else NA_character_
  )]
  
  for (j in names(src)) {
    if (is.character(src[[j]]) || is.factor(src[[j]])) src[, (j) := clean_chr(get(j))]
  }
  src[, codigo := suppressWarnings(as.integer(codigo))]
  src <- src[!(is.na(nivel_01) & is.na(nivel_02) & is.na(nivel_03) & is.na(nivel_04))]
  
  # ----------------------------------------------------------
  # Construcción jerárquica base legacy
  # ----------------------------------------------------------
  l1 <- unique(src[!is.na(nivel_01), .(
    cause_name = nivel_01, cause_level = 1L, codigo_fuente = NA_integer_,
    level_1_name = nivel_01, level_2_name = NA_character_, level_3_name = NA_character_, level_4_name = NA_character_,
    cie10 = NA_character_, icd10_regex = NA_character_
  )])
  
  l2 <- unique(src[!is.na(nivel_02), .(
    cause_name = nivel_02, cause_level = 2L, codigo_fuente = NA_integer_,
    level_1_name = nivel_01, level_2_name = nivel_02, level_3_name = NA_character_, level_4_name = NA_character_,
    cie10 = NA_character_, icd10_regex = NA_character_
  )])
  
  l3 <- unique(src[!is.na(nivel_03), .(
    cause_name = nivel_03, cause_level = 3L, codigo_fuente = NA_integer_,
    level_1_name = nivel_01, level_2_name = nivel_02, level_3_name = nivel_03, level_4_name = NA_character_,
    cie10 = NA_character_, icd10_regex = NA_character_
  )])
  
  l4 <- unique(src[!is.na(nivel_04), .(
    cause_name = nivel_04, cause_level = 4L, codigo_fuente = codigo,
    level_1_name = nivel_01, level_2_name = nivel_02, level_3_name = nivel_03, level_4_name = nivel_04,
    cie10 = cie10, icd10_regex = icd10_regex
  )])
  
  nodes <- rbindlist(list(l1, l2, l3, l4), fill = TRUE, use.names = TRUE)
  nodes[, path_key := make_path_key(level_1_name, level_2_name, level_3_name, level_4_name)]
  nodes[, parent_path_key := fifelse(
    cause_level == 1L, NA_character_,
    fifelse(cause_level == 2L,
            make_path_key(level_1_name, NA_character_, NA_character_, NA_character_),
            fifelse(cause_level == 3L,
                    make_path_key(level_1_name, level_2_name, NA_character_, NA_character_),
                    make_path_key(level_1_name, level_2_name, level_3_name, NA_character_)
            )
    )
  )]
  nodes <- unique(nodes, by = c("cause_level", "path_key"))
  
  if (isTRUE(CFG$add_total_node)) {
    total_node <- data.table(
      cause_name = CFG$total_name,
      cause_level = 0L,
      codigo_fuente = NA_integer_,
      level_1_name = NA_character_, level_2_name = NA_character_, level_3_name = NA_character_, level_4_name = NA_character_,
      cie10 = NA_character_, icd10_regex = NA_character_,
      path_key = "TOTAL", parent_path_key = NA_character_
    )
    nodes <- rbind(nodes, total_node, fill = TRUE)
    nodes[cause_level == 1L, parent_path_key := "TOTAL"]
  }
  
  # ----------------------------------------------------------
  # Inserciones mínimas pandemia sobre árbol legacy
  # ----------------------------------------------------------
  have_parent <- nodes[tolower(cause_name) == tolower(CFG$pandemic_parent_name), .N] > 0L
  if (!have_parent) {
    if (nodes[tolower(cause_name) == tolower(CFG$pandemic_root_name), .N] == 0L) {
      nodes <- rbind(nodes, data.table(
        cause_name = CFG$pandemic_root_name,
        cause_level = 1L,
        codigo_fuente = NA_integer_,
        level_1_name = CFG$pandemic_root_name,
        level_2_name = NA_character_, level_3_name = NA_character_, level_4_name = NA_character_,
        cie10 = NA_character_, icd10_regex = NA_character_,
        path_key = make_path_key(CFG$pandemic_root_name, NA_character_, NA_character_, NA_character_),
        parent_path_key = "TOTAL"
      ), fill = TRUE)
    }
    
    nodes <- rbind(nodes, data.table(
      cause_name = CFG$pandemic_parent_name,
      cause_level = 2L,
      codigo_fuente = NA_integer_,
      level_1_name = CFG$pandemic_root_name,
      level_2_name = CFG$pandemic_parent_name,
      level_3_name = NA_character_, level_4_name = NA_character_,
      cie10 = NA_character_, icd10_regex = NA_character_,
      path_key = make_path_key(CFG$pandemic_root_name, CFG$pandemic_parent_name, NA_character_, NA_character_),
      parent_path_key = make_path_key(CFG$pandemic_root_name, NA_character_, NA_character_, NA_character_)
    ), fill = TRUE)
  }
  
  pandemic_parent_path <- nodes[tolower(cause_name) == tolower(CFG$pandemic_parent_name), path_key][1]
  if (is.na(pandemic_parent_path)) stop("No pude resolver pandemic_parent_path.")
  
  pandemic_l1 <- nodes[path_key == pandemic_parent_path, level_1_name][1]
  pandemic_l2 <- nodes[path_key == pandemic_parent_path, level_2_name][1]
  
  covid_parent_path <- make_path_key(pandemic_l1, pandemic_l2, "Pandemic explicit causes", NA_character_)
  if (nodes[path_key == covid_parent_path, .N] == 0L) {
    nodes <- rbind(nodes, data.table(
      cause_name = "Pandemic explicit causes",
      cause_level = 3L,
      codigo_fuente = NA_integer_,
      level_1_name = pandemic_l1, level_2_name = pandemic_l2,
      level_3_name = "Pandemic explicit causes", level_4_name = NA_character_,
      cie10 = NA_character_, icd10_regex = NA_character_,
      path_key = covid_parent_path,
      parent_path_key = pandemic_parent_path
    ), fill = TRUE)
  }
  
  if (isTRUE(CFG$add_covid_specific_node_if_missing) && nodes[tolower(cause_name) == tolower(CFG$covid_specific_name), .N] == 0L) {
    nodes <- rbind(nodes, data.table(
      cause_name = CFG$covid_specific_name,
      cause_level = 4L,
      codigo_fuente = NA_integer_,
      level_1_name = pandemic_l1, level_2_name = pandemic_l2,
      level_3_name = "Pandemic explicit causes", level_4_name = CFG$covid_specific_name,
      cie10 = NA_character_, icd10_regex = CFG$covid_specific_icd10_regex,
      path_key = make_path_key(pandemic_l1, pandemic_l2, "Pandemic explicit causes", CFG$covid_specific_name),
      parent_path_key = covid_parent_path
    ), fill = TRUE)
  }
  
  oprm_parent_path <- make_path_key(pandemic_l1, pandemic_l2, "Pandemic residual causes", NA_character_)
  if (nodes[path_key == oprm_parent_path, .N] == 0L) {
    nodes <- rbind(nodes, data.table(
      cause_name = "Pandemic residual causes",
      cause_level = 3L,
      codigo_fuente = NA_integer_,
      level_1_name = pandemic_l1, level_2_name = pandemic_l2,
      level_3_name = "Pandemic residual causes", level_4_name = NA_character_,
      cie10 = NA_character_, icd10_regex = NA_character_,
      path_key = oprm_parent_path,
      parent_path_key = pandemic_parent_path
    ), fill = TRUE)
  }
  
  if (isTRUE(CFG$add_oprm_node_if_missing) && nodes[tolower(cause_name) == tolower(CFG$oprm_name), .N] == 0L) {
    nodes <- rbind(nodes, data.table(
      cause_name = CFG$oprm_name,
      cause_level = 4L,
      codigo_fuente = NA_integer_,
      level_1_name = pandemic_l1, level_2_name = pandemic_l2,
      level_3_name = "Pandemic residual causes", level_4_name = CFG$oprm_name,
      cie10 = NA_character_, icd10_regex = NA_character_,
      path_key = make_path_key(pandemic_l1, pandemic_l2, "Pandemic residual causes", CFG$oprm_name),
      parent_path_key = oprm_parent_path
    ), fill = TRUE)
  }
  
  nodes <- unique(nodes, by = c("cause_level", "path_key"))
  
  # ----------------------------------------------------------
  # IDs / parent mapping legacy
  # ----------------------------------------------------------
  nodes[, cause_concept_id := fifelse(!is.na(codigo_fuente), as.numeric(CFG$concept_prefix) + as.numeric(codigo_fuente), NA_real_)]
  next_id <- suppressWarnings(max(nodes$cause_concept_id, na.rm = TRUE))
  if (!is.finite(next_id)) next_id <- as.numeric(CFG$concept_prefix)
  if (nodes[is.na(cause_concept_id), .N] > 0L) {
    nodes[is.na(cause_concept_id), cause_concept_id := seq(from = next_id + 1, length.out = .N, by = 1)]
  }
  if (any(nodes$cause_concept_id > .Machine$integer.max, na.rm = TRUE)) stop("cause_concept_id excede rango integer.")
  nodes[, cause_concept_id := as.integer(cause_concept_id)]
  
  nodes[, cause_code := fifelse(
    !is.na(codigo_fuente), as.character(codigo_fuente),
    fifelse(path_key == "TOTAL", "TOTAL",
            fifelse(tolower(cause_name) == tolower(CFG$covid_specific_name), CFG$covid_specific_code,
                    fifelse(tolower(cause_name) == tolower(CFG$oprm_name), CFG$oprm_code,
                            paste0("ART_", cause_concept_id)
                    )
            )
    )
  )]
  
  parent_lookup <- nodes[, .(parent_path_key = path_key, parent_concept_id = cause_concept_id)]
  nodes <- merge(nodes, parent_lookup, by = "parent_path_key", all.x = TRUE, sort = FALSE)
  nodes[path_key == "TOTAL", parent_concept_id := NA_integer_]
  
  child_counts <- nodes[!is.na(parent_concept_id), .N, by = parent_concept_id]
  setnames(child_counts, c("parent_concept_id", "N"), c("cause_concept_id", "n_children"))
  nodes <- merge(nodes, child_counts, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  nodes[is.na(n_children), n_children := 0L]
  nodes[, is_terminal := n_children == 0L]
  
  # ----------------------------------------------------------
  # Patch legacy casi intacto
  # ----------------------------------------------------------
  if (file.exists(CFG$input_patch_csv)) {
    msg("Aplicando patch de mortalidad/morbilidad: ", CFG$input_patch_csv)
    patch <- fread(CFG$input_patch_csv, encoding = "UTF-8")
    setDT(patch)
    setnames(patch, names(patch), trimws(names(patch)))
    
    if (!"cause_concept_id" %in% names(patch)) {
      alt_id <- intersect(c("source_concept_id", "target_concept_id", "concept_id"), names(patch))
      if (length(alt_id) == 0L) stop("El patch no trae cause_concept_id ni alias reconocible.")
      setnames(patch, alt_id[1], "cause_concept_id")
    }
    patch[, cause_concept_id := as.integer(cause_concept_id)]
    
    patch_drop_overlap <- setdiff(intersect(names(patch), names(nodes)), "cause_concept_id")
    if (length(patch_drop_overlap) > 0L) {
      msg("Columnas solapadas del patch removidas antes del merge: ", paste(patch_drop_overlap, collapse = ", "))
      patch[, (patch_drop_overlap) := NULL]
    }
    
    nodes <- merge(nodes, patch, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  }
  
  # ----------------------------------------------------------
  # Upgrade pandémico quirúrgico sobre resultado legacy
  # ----------------------------------------------------------
  for (nm in c(
    "is_covid_specific", "is_oprm", "is_indirect_pandemic_cause",
    "is_pandemic_related_any", "pandemic_bucket",
    "who_ghe_group", "who_ghe_level_override", "is_transversal_pandemic_adjustment"
  )) {
    if (!nm %in% names(nodes)) nodes[, (nm) := NA]
  }
  
  nodes[, cause_name_lc := tolower(clean_chr(cause_name))]
  nodes[, cause_code_lc := tolower(clean_chr(cause_code))]
  nodes[, cie10_lc := tolower(clean_chr(cie10))]
  nodes[, icd10_regex_lc := tolower(clean_chr(icd10_regex))]
  nodes[, path_key_lc := tolower(clean_chr(path_key))]
  
  if (!"is_garbage" %in% names(nodes)) nodes[, is_garbage := FALSE]
  if (!"is_residual" %in% names(nodes)) nodes[, is_residual := grepl("\botras?\b|\botros?\b|residual", cause_name_lc)]
  
  # COVID específico: SOLO nodo COVID explícito, nunca LRI amplia.
  nodes[, is_covid_specific := FALSE]
  nodes[
    is_terminal == TRUE & (
      cause_name_lc == tolower(CFG$covid_specific_name) |
        cause_code_lc == tolower(CFG$covid_specific_code)
    ),
    is_covid_specific := TRUE
  ]
  
  nodes[, is_oprm := FALSE]
  nodes[
    is_terminal == TRUE & (
      cause_name_lc == tolower(CFG$oprm_name) |
        cause_code_lc == tolower(CFG$oprm_code) |
        grepl("other pandemic related mortality|oprm", cause_name_lc, perl = TRUE)
    ),
    is_oprm := TRUE
  ]
  
  indirect_name_pattern <- paste(CFG$indirect_pandemic_names, collapse = "|")
  indirect_code_pattern <- paste(tolower(CFG$indirect_pandemic_codes), collapse = "|")
  
  nodes[, is_indirect_pandemic_cause := FALSE]
  nodes[
    is_terminal == TRUE & !is_covid_specific & !is_oprm & (
      grepl(indirect_name_pattern, cause_name_lc, perl = TRUE) |
        grepl(indirect_code_pattern, cause_code_lc, perl = TRUE) |
        any_grepl_vec(CFG$indirect_pandemic_icd10_regex, cie10_lc) |
        any_grepl_vec(CFG$indirect_pandemic_icd10_regex, icd10_regex_lc)
    ),
    is_indirect_pandemic_cause := TRUE
  ]
  
  nodes[, pandemic_bucket := fifelse(
    is_covid_specific, "covid_specific",
    fifelse(is_oprm, "oprm",
            fifelse(is_indirect_pandemic_cause, "indirect_pandemic", "non_pandemic"))
  )]
  nodes[, is_pandemic_related_any := pandemic_bucket != "non_pandemic"]
  nodes[, is_covid_related := is_pandemic_related_any]
  
  nodes[, who_ghe_group := fifelse(
    is_oprm, "IV. Other /pandemic-related",
    fifelse(is_covid_specific, "COVID-19",
            fifelse(is_indirect_pandemic_cause, "Indirect pandemic causes", NA_character_))
  )]
  nodes[, who_ghe_level_override := fifelse(is_oprm, 1L, NA_integer_)]
  nodes[, is_transversal_pandemic_adjustment := is_oprm]
  
  overlap_bad <- nodes[
    (as.integer(is_covid_specific) + as.integer(is_oprm) + as.integer(is_indirect_pandemic_cause)) > 1
  ]
  fwrite(overlap_bad, file.path(CFG$qc_dir, "qc_pandemic_flag_overlap_bad.csv"))
  if (nrow(overlap_bad) > 0L) stop("QC HARD FAIL: hay causas asignadas a más de un bucket pandémico.")
  
  oprm_term <- nodes[is_terminal == TRUE & is_oprm == TRUE]
  covid_term <- nodes[is_terminal == TRUE & is_covid_specific == TRUE]
  fwrite(oprm_term, file.path(CFG$qc_dir, "qc_oprm_terminal_nodes.csv"))
  fwrite(covid_term, file.path(CFG$qc_dir, "qc_covid_terminal_nodes.csv"))
  
  if (nrow(oprm_term) != 1L) stop("QC HARD FAIL: debe existir exactamente 1 nodo terminal OPRM. Encontrados: ", nrow(oprm_term))
  if (nrow(covid_term) != 1L) stop("QC HARD FAIL: debe existir exactamente 1 nodo terminal COVID específico. Encontrados: ", nrow(covid_term))
  
  bad_lri_as_covid <- nodes[is_covid_specific == TRUE & grepl("lower respiratory|v[ií]as respiratorias inferiores|lri", cause_name_lc, perl = TRUE)]
  fwrite(bad_lri_as_covid, file.path(CFG$qc_dir, "qc_bad_lri_as_covid.csv"))
  if (nrow(bad_lri_as_covid) > 0L) stop("QC HARD FAIL: LRI o equivalente quedó marcado como COVID específico.")
  
  for (nm in c(
    "yll_flag", "yld_flag", "burden_component_class", "mortality_plausibility",
    "redistribution_target_eligible_default", "redistribution_pool_default",
    "australia_check", "australia_note", "evidence_source"
  )) {
    if (!nm %in% names(nodes)) nodes[, (nm) := NA]
  }
  
  nodes[is_oprm == TRUE, `:=`(
    yll_flag = fifelse(is.na(yll_flag), 1L, as.integer(yll_flag)),
    yld_flag = fifelse(is.na(yld_flag), 0L, as.integer(yld_flag)),
    burden_component_class = fifelse(is.na(burden_component_class), "yll_only", as.character(burden_component_class))
  )]
  
  nodes[, parent_name := as.character(nodes[match(parent_concept_id, cause_concept_id), cause_name])]
  
  cause_master <- copy(nodes)
  cause_master[, run_id := run_id]
  
  preferred_order <- c(
    "cause_concept_id", "cause_code", "cause_name", "cause_level", "parent_concept_id",
    "is_terminal", "is_garbage", "is_residual", "is_covid_related",
    "is_covid_specific", "is_oprm", "is_indirect_pandemic_cause", "is_pandemic_related_any",
    "pandemic_bucket", "who_ghe_group", "who_ghe_level_override", "is_transversal_pandemic_adjustment",
    "cie10", "icd10_regex", "codigo_fuente", "parent_name",
    "path_key", "parent_path_key", "level_1_name", "level_2_name", "level_3_name", "level_4_name",
    "n_children", "yll_flag", "yld_flag", "burden_component_class", "mortality_plausibility",
    "redistribution_target_eligible_default", "redistribution_pool_default",
    "australia_check", "australia_note", "evidence_source", "run_id"
  )
  setcolorder(cause_master, intersect(preferred_order, names(cause_master)))
  setorder(cause_master, cause_level, cause_concept_id)
  
  spec <- list(
    dataset_id = "cause_master_bod_omop",
    table_name = "cause_master",
    primary_key = c("cause_concept_id"),
    required_columns = list(
      cause_concept_id = "integer",
      cause_code = "character",
      cause_name = "character",
      cause_level = "integer",
      parent_concept_id = "integer",
      is_terminal = "logical",
      is_garbage = "logical",
      is_residual = "logical",
      is_covid_related = "logical",
      is_covid_specific = "logical",
      is_oprm = "logical",
      is_indirect_pandemic_cause = "logical",
      is_pandemic_related_any = "logical",
      pandemic_bucket = "character",
      who_ghe_group = "character",
      who_ghe_level_override = "integer",
      is_transversal_pandemic_adjustment = "logical",
      icd10_regex = "character"
    )
  )
  
  validate_by_spec(cause_master[, intersect(names(spec$required_columns), names(cause_master)), with = FALSE], spec)
  writeLines(as.yaml(spec), CFG$spec_out_path)
  
  out_csv <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".csv"))
  out_parquet <- file.path(CFG$out_dir, paste0(CFG$out_stem, ".parquet"))
  out_dict <- file.path(CFG$out_dir, paste0(CFG$out_stem, "_dictionary_ext.csv"))
  
  write_csv_parquet(cause_master, csv_path = out_csv, parquet_path = out_parquet)
  dict_ext <- build_dictionary_ext(cause_master)
  fwrite(dict_ext, out_dict)
  
  qc_flag_summary <- cause_master[, .(
    n_rows = .N,
    n_terminal = sum(is_terminal, na.rm = TRUE),
    n_covid_specific = sum(is_covid_specific, na.rm = TRUE),
    n_oprm = sum(is_oprm, na.rm = TRUE),
    n_indirect_pandemic = sum(is_indirect_pandemic_cause, na.rm = TRUE),
    n_pandemic_any = sum(is_pandemic_related_any, na.rm = TRUE)
  )]
  fwrite(qc_flag_summary, file.path(CFG$qc_dir, "qc_pandemic_flag_summary.csv"))
  
  fwrite(cause_master[
    is_pandemic_related_any == TRUE,
    .(cause_concept_id, cause_name, cause_level, pandemic_bucket,
      who_ghe_group, who_ghe_level_override, is_transversal_pandemic_adjustment)
  ][order(pandemic_bucket, cause_level, cause_name)],
  file.path(CFG$qc_dir, "qc_pandemic_reporting_override.csv"))
  
  fwrite(cause_master[
    is_pandemic_related_any == TRUE,
    .(cause_concept_id, cause_code, cause_name, cause_level, pandemic_bucket,
      who_ghe_group, who_ghe_level_override, is_transversal_pandemic_adjustment,
      cie10, icd10_regex, path_key)
  ][order(pandemic_bucket, cause_level, cause_name)],
  file.path(CFG$qc_dir, "qc_pandemic_nodes_detail.csv"))
  
  fwrite(cause_master[
    is_indirect_pandemic_cause == TRUE,
    .(cause_concept_id, cause_code, cause_name, cause_level, cie10, icd10_regex, path_key)
  ][order(cause_name)],
  file.path(CFG$qc_dir, "qc_indirect_pandemic_nodes.csv"))
  
  register_artifact(CFG$dataset_id, CFG$table_name, CFG$version, run_id, "final_dataset", out_csv, nrow(cause_master), ncol(cause_master), "cause_master legacy upgrade con split pandémico OMS")
  register_artifact(CFG$dataset_id, CFG$table_name, CFG$version, run_id, "final_dataset", out_parquet, nrow(cause_master), ncol(cause_master), "cause_master legacy upgrade con split pandémico OMS")
  register_artifact(CFG$dataset_id, CFG$table_name, CFG$version, run_id, "dictionary_ext", out_dict, nrow(dict_ext), ncol(dict_ext), "diccionario extendido cause_master")
  register_artifact(CFG$dataset_id, CFG$table_name, CFG$version, run_id, "spec", CFG$spec_out_path, NA_integer_, NA_integer_, "spec_cause_master actualizado para split pandémico")
  
  register_run_finish(run_id, status = "success", message = "01_build_cause_master completado")
  msg("OK -> cause_master legacy upgrade exportado")
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
