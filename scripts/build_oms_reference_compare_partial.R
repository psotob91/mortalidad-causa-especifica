#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(here)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))

CFG <- list(
  version = "v0.1.0_oms_partial_compare",
  dataset_id = "oms_reference_compare_partial",
  oms_ref_path = here("data", "raw", "oms_reference", "oms_reference_partial_from_images.csv"),
  oms_notes_path = here("data", "raw", "oms_reference", "oms_reference_partial_notes_from_images.csv"),
  cause_methods_path = here("data", "derived", "methods", "cause_catalog_methods.csv"),
  redist_summary_path = here("data", "derived", "methods", "redistribution_methods_catalog_summary.csv"),
  coverage_path = here("data", "derived", "methods", "icd10_observed_coverage.csv"),
  out_dir = here("data", "derived", "methods"),
  qc_dir = qc_dir_path("build_oms_reference_compare_partial"),
  verbose = TRUE
)

for (d in c(CFG$out_dir, CFG$qc_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) if (isTRUE(CFG$verbose)) cat(..., "\n")

clean_chr <- function(x) {
  z <- as.character(x)
  z <- stringi::stri_trim_both(z)
  z[z %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  z
}

norm_code <- function(x) {
  z <- clean_chr(x)
  z <- toupper(z)
  z <- gsub("[[:space:]]", "", z)
  z[nchar(z) == 0L] <- NA_character_
  z
}

extract_codes_loose <- function(expr) {
  expr <- clean_chr(expr)
  if (is.na(expr)) return(character())
  hits <- gregexpr("[A-Z][0-9]{2}(?:\\.[0-9A-Z]{1,2})?", expr, perl = TRUE)
  vals <- regmatches(expr, hits)[[1]]
  vals <- norm_code(vals)
  unique(sort(na.omit(vals)))
}

collapse_vals <- function(x, max_n = 50L) {
  x <- unique(sort(na.omit(as.character(x))))
  if (length(x) == 0L) return(NA_character_)
  if (length(x) <= max_n) return(paste(x, collapse = ", "))
  paste0(paste(head(x, max_n), collapse = ", "), " (+", length(x) - max_n, " más)")
}

ensure_project_dirs()
ensure_catalog_files()
run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

tryCatch({
  msg("Leyendo referencia OMS parcial y tablas del proyecto.")
  oms <- as.data.table(read_auto(CFG$oms_ref_path))
  oms_notes <- as.data.table(read_auto(CFG$oms_notes_path))
  cause_methods <- as.data.table(read_auto(CFG$cause_methods_path))
  redist <- as.data.table(read_auto(CFG$redist_summary_path))
  coverage <- as.data.table(read_auto(CFG$coverage_path))

  oms[, `:=`(
    oms_row_code = as.integer(oms_row_code),
    oms_cause_name = clean_chr(oms_cause_name),
    icd10_expression = clean_chr(icd10_expression),
    footnote_refs = clean_chr(footnote_refs),
    project_cause_concept_id = suppressWarnings(as.integer(project_cause_concept_id)),
    project_cause_name = clean_chr(project_cause_name)
  )]

  oms_notes[, `:=`(
    footnote_id = clean_chr(footnote_id),
    footnote_text = clean_chr(footnote_text),
    redistribution_implication = clean_chr(redistribution_implication),
    affected_icd10_expression = clean_chr(affected_icd10_expression)
  )]

  coverage[, icd10_ucod_dot := norm_code(icd10_ucod_dot)]
  observed_codes <- unique(na.omit(coverage$icd10_ucod_dot))

  msg("Normalizando membresía OMS y notas.")
  oms_membership <- oms[, {
    codes <- extract_codes_loose(icd10_expression)
    .(
      n_codes_listed = length(codes),
      listed_codes = collapse_vals(codes, max_n = 120L),
      listed_codes_vec = list(codes),
      observed_codes_matching_oms = collapse_vals(intersect(codes, observed_codes), max_n = 120L),
      n_observed_codes_matching_oms = length(intersect(codes, observed_codes))
    )
  }, by = .(
    oms_row_code, oms_hierarchy_label, oms_cause_name, icd10_expression,
    footnote_refs, project_cause_concept_id, project_cause_name, source_note
  )]

  cause_methods[, `:=`(
    icd10_directos_observados_vec = strsplit(fifelse(is.na(icd10_directos_observados), "", icd10_directos_observados), ",\\s*"),
    icd10_excluidos_explicitos_regex_vec = strsplit(fifelse(is.na(icd10_excluidos_explicitos_regex), "", icd10_excluidos_explicitos_regex), ",\\s*")
  )]
  cause_methods[, icd10_directos_observados_vec := lapply(icd10_directos_observados_vec, function(x) norm_code(x[x != ""]))]
  cause_methods[, icd10_excluidos_explicitos_regex_vec := lapply(icd10_excluidos_explicitos_regex_vec, function(x) norm_code(x[x != ""]))]

  redist[, source_codes_vec := strsplit(fifelse(is.na(icd10_observados_fuente), "", icd10_observados_fuente), ",\\s*")]
  redist[, source_codes_vec := lapply(source_codes_vec, function(x) norm_code(x[x != ""]))]

  msg("Comparando OMS vs árbol causal del proyecto.")
  oms_vs_project_cause <- merge(
    oms_membership,
    cause_methods[, .(
      cause_concept_id = as.integer(cause_concept_id),
      cause_name,
      cause_level,
      receives_redistribution,
      used_for_mortality,
      used_for_avp,
      mismatch_class,
      recommended_action,
      icd10_directos_observados,
      icd10_excluidos_explicitos_regex,
      icd10_directos_observados_vec,
      icd10_excluidos_explicitos_regex_vec
    )],
    by.x = "project_cause_concept_id",
    by.y = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )

  oms_vs_project_cause[, `:=`(
    oms_listed_codes_vec = listed_codes_vec,
    project_direct_codes_vec = icd10_directos_observados_vec,
    project_excluded_codes_vec = icd10_excluidos_explicitos_regex_vec
  )]
  oms_vs_project_cause[, oms_vs_project_direct_missing := vapply(seq_len(.N), function(i) {
    collapse_vals(setdiff(oms_listed_codes_vec[[i]], project_direct_codes_vec[[i]]), max_n = 120L)
  }, character(1))]
  oms_vs_project_cause[, project_direct_not_in_oms := vapply(seq_len(.N), function(i) {
    collapse_vals(setdiff(project_direct_codes_vec[[i]], oms_listed_codes_vec[[i]]), max_n = 120L)
  }, character(1))]
  oms_vs_project_cause[, oms_vs_project_direct_status := fifelse(
    is.na(project_cause_concept_id),
    "oms_row_not_linked_to_project",
    fifelse(
      (is.na(oms_vs_project_direct_missing) | oms_vs_project_direct_missing == "") &
        (is.na(project_direct_not_in_oms) | project_direct_not_in_oms == ""),
      "exact_or_effective_match",
      fifelse(
        !is.na(footnote_refs) & nzchar(footnote_refs),
        "difference_with_oms_note_context",
        "difference_without_note_context"
      )
    )
  )]

  msg("Comparando notas/reglas OMS vs reglas de redistribución del proyecto.")
  note_map <- data.table(
    footnote_id = c("a", "c", "d", "e", "f", "g", "h", "star", "starstar"),
    compare_topic = c(
      "ill_defined_groupI_II",
      "unspecified_cancers",
      "uterus_unspecified",
      "drug_use_unspecified",
      "cardiovascular_garbage",
      "essential_hypertension",
      "injury_undetermined_intent",
      "direct_covid",
      "oprm_residual"
    )
  )
  oms_notes_cmp <- merge(oms_notes, note_map, by = "footnote_id", all.x = TRUE, sort = FALSE)

  redist_topics <- redist[, {
    topic <- fcase(
      grepl("I10", regex_r, perl = TRUE), "essential_hypertension",
      grepl("C55", regex_r, perl = TRUE), "uterus_unspecified",
      grepl("C76|C80|C97", regex_r, perl = TRUE), "unspecified_cancers",
      grepl("F19|X44", regex_r, perl = TRUE), "drug_use_unspecified",
      grepl("Y10|Y11|Y12|Y13|Y14|Y15|Y17|Y19|Y20|Y21|Y23|Y24|Y26|Y28|Y29|Y30|Y31|Y32|Y33|Y34", regex_r, perl = TRUE), "injury_undetermined_intent",
      grepl("I46|I472|I490|I500|I501|I509|I514|I515|I516|I519|I709", regex_r, perl = TRUE), "cardiovascular_garbage",
      grepl("R99|J69|J96", regex_r, perl = TRUE), "ill_defined_groupI_II",
      default = NA_character_
    )
    .(compare_topic = unique(na.omit(topic)))
  }, by = .(rule_id, source_group_code, source_group_name, regex_r, redistribution_method, redistribution_scope, notes, redistribution_reason, method_source)]
  redist_topics <- redist_topics[!is.na(compare_topic)]

  oms_vs_project_redist <- merge(
    oms_notes_cmp,
    redist_topics[, .(
      compare_topic, rule_id, source_group_code, source_group_name, regex_r,
      redistribution_method, redistribution_scope, redistribution_reason, method_source
    )],
    by = "compare_topic",
    all.x = TRUE,
    allow.cartesian = TRUE,
    sort = FALSE
  )
  oms_vs_project_redist[, project_alignment_status := fifelse(
    is.na(compare_topic),
    "oms_note_without_comparison_topic",
    fifelse(
      is.na(rule_id) & compare_topic %in% c("direct_covid", "oprm_residual"),
      "special_case_not_expected_in_redist_rules",
      fifelse(is.na(rule_id), "oms_note_without_project_rule_match", "oms_note_with_project_rule_match")
    )
  )]

  cause_qc <- oms_vs_project_cause[, .N, by = oms_vs_project_direct_status][order(oms_vs_project_direct_status)]
  redist_qc <- oms_vs_project_redist[, .N, by = project_alignment_status][order(project_alignment_status)]

  cause_export <- copy(oms_vs_project_cause)
  drop_cols <- intersect(c(
    "listed_codes_vec",
    "oms_listed_codes_vec",
    "project_direct_codes_vec",
    "project_excluded_codes_vec",
    "icd10_directos_observados_vec",
    "icd10_excluidos_explicitos_regex_vec"
  ), names(cause_export))
  if (length(drop_cols) > 0L) cause_export[, (drop_cols) := NULL]

  fwrite(cause_export,
         file.path(CFG$out_dir, "oms_vs_project_cause_compare_partial.csv"))
  fwrite(oms_vs_project_redist,
         file.path(CFG$out_dir, "oms_vs_project_redistribution_compare_partial.csv"))
  fwrite(cause_qc, file.path(CFG$qc_dir, "qc_oms_vs_project_cause_compare_summary.csv"))
  fwrite(redist_qc, file.path(CFG$qc_dir, "qc_oms_vs_project_redistribution_compare_summary.csv"))

  register_run_finish(
    run_id,
    status = "success",
    message = sprintf("rows=%s", nrow(oms_vs_project_cause) + nrow(oms_vs_project_redist))
  )
  msg("Comparación OMS parcial generada correctamente.")
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = conditionMessage(e))
  stop(e)
})
