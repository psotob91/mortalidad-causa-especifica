#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(stringi)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "dictionary_utils.R"))

CFG <- list(
  version = "v0.1.0_methods_base_catalogs",
  dataset_id = "methods_catalogs",
  table_name = "methods_catalogs",
  cause_master_path = here("data", "final", "cause_master", "cause_master.csv"),
  rules_path = here("data", "final", "redistribution_rules", "redistribution_rules.csv"),
  death_norm_path = here("data", "final", "death_record_normalized", "death_record_normalized.parquet"),
  mortality_report_long_path = here("data", "final", "report_tables", "mortality_report_long.csv"),
  avp_report_long_path = here("data", "final", "report_tables", "avp_report_long.csv"),
  hierarchy_qc_path = resolve_existing_qc_path("build_report_tables", "qc_cie10_hierarchy_compare_classified.csv"),
  direct_demog_detected_path = resolve_existing_qc_path("map_and_redistribute_deaths", "qc_direct_demographic_incompatibility_detected.csv"),
  direct_specific_detected_path = resolve_existing_qc_path("map_and_redistribute_deaths", "qc_direct_specific_icd_handling_detected.csv"),
  sensitive_positions_patch_path = here("data", "raw", "oms_reference", "patch_sensitive_methodological_positions.csv"),
  out_dir = here("data", "derived", "methods"),
  qc_dir = qc_dir_path("build_methods_catalogs"),
  verbose = TRUE
)

for (d in c(CFG$out_dir, CFG$qc_dir)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

write_tabular_with_dict <- function(dt, stem, out_dir, dataset_id, version, run_id, table_note) {
  csv_path <- file.path(out_dir, paste0(stem, ".csv"))
  parquet_path <- file.path(out_dir, paste0(stem, ".parquet"))
  dict_path <- file.path(out_dir, paste0(stem, "_dictionary_ext.csv"))
  write_csv_parquet(dt, csv_path = csv_path, parquet_path = parquet_path)
  dict_dt <- build_dictionary_ext_basic(dt)
  fwrite(dict_dt, dict_path)
  register_artifact(dataset_id, stem, version, run_id, "final_dataset", csv_path, nrow(dt), ncol(dt), table_note)
  register_artifact(dataset_id, stem, version, run_id, "final_dataset", parquet_path, nrow(dt), ncol(dt), table_note)
  register_artifact(dataset_id, stem, version, run_id, "dictionary_ext", dict_path, nrow(dict_dt), ncol(dict_dt), paste("Diccionario extendido:", table_note))
}

clean_chr <- function(x) {
  z <- as.character(x)
  z <- stringi::stri_trim_both(z)
  z[z %in% c("", "NA", "N/A", "NULL", "null")] <- NA_character_
  z
}

norm_icd10_nodot <- function(x) {
  z <- clean_chr(x)
  z <- toupper(z)
  z <- gsub("[\\.\\s]", "", z)
  z[nchar(z) == 0L] <- NA_character_
  z
}

icd10_add_dot <- function(x) {
  z <- clean_chr(x)
  z <- toupper(z)
  z[nchar(z) == 0L] <- NA_character_
  has_dot <- !is.na(z) & grepl("\\.", z)
  idx <- !has_dot & !is.na(z) & nchar(z) > 3L
  z[idx] <- paste0(substr(z[idx], 1, 3), ".", substr(z[idx], 4L, nchar(z[idx])))
  z
}

match_regex_idx <- function(code, patterns) {
  which(vapply(patterns, function(p) {
    if (is.na(p) || p == "") return(FALSE)
    grepl(p, code, perl = TRUE)
  }, logical(1)))
}

collapse_vals <- function(x, max_n = 50L) {
  x <- unique(sort(na.omit(as.character(x))))
  if (length(x) == 0L) return(NA_character_)
  if (length(x) <= max_n) return(paste(x, collapse = ", "))
  paste0(paste(head(x, max_n), collapse = ", "), " (+", length(x) - max_n, " más)")
}

extract_negative_lookahead_codes <- function(pattern) {
  if (is.na(pattern) || !nzchar(pattern)) return(character())
  m <- gregexpr("\\(\\?!([A-Z][0-9A-Z]{2,4})\\$\\)", pattern, perl = TRUE)
  hits <- regmatches(pattern, m)[[1]]
  if (length(hits) == 0L) return(character())
  out <- sub("^\\(\\?!([A-Z][0-9A-Z]{2,4})\\$\\)$", "\\1", hits, perl = TRUE)
  unique(sort(out))
}

ensure_project_dirs()
ensure_catalog_files()
run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

tryCatch({
  msg("Leyendo insumos metodológicos.")
  cm <- as.data.table(read_auto(CFG$cause_master_path))
  rr <- as.data.table(read_auto(CFG$rules_path))
  dx <- as.data.table(read_auto(CFG$death_norm_path))
  mort <- as.data.table(read_auto(CFG$mortality_report_long_path))
  avp <- as.data.table(read_auto(CFG$avp_report_long_path))
  hierarchy_qc <- as.data.table(read_auto(CFG$hierarchy_qc_path))
  direct_demog_detected <- if (file.exists(CFG$direct_demog_detected_path)) as.data.table(read_auto(CFG$direct_demog_detected_path)) else data.table()
  direct_specific_detected <- if (file.exists(CFG$direct_specific_detected_path)) as.data.table(read_auto(CFG$direct_specific_detected_path)) else data.table()
  sensitive_positions_patch <- if (file.exists(CFG$sensitive_positions_patch_path)) as.data.table(read_auto(CFG$sensitive_positions_patch_path)) else data.table()

  req_cm <- c("cause_concept_id", "cause_name", "cause_level", "parent_concept_id", "is_terminal", "icd10_regex", "cause_code")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))

  req_rr <- c("rule_id", "source_group_code", "source_group_name", "target_cause_concept_id", "target_weight", "regex_r")
  miss_rr <- setdiff(req_rr, names(rr))
  if (length(miss_rr) > 0L) stop("Faltan columnas en redistribution_rules: ", paste(miss_rr, collapse = ", "))

  req_dx <- c("death_id", "icd10_ucod", "year_id")
  miss_dx <- setdiff(req_dx, names(dx))
  if (length(miss_dx) > 0L) stop("Faltan columnas en death_record_normalized: ", paste(miss_dx, collapse = ", "))

  dx[, `:=`(
    icd10_ucod_nodot = norm_icd10_nodot(icd10_ucod),
    icd10_ucod_dot = icd10_add_dot(norm_icd10_nodot(icd10_ucod))
  )]
  obs <- dx[!is.na(icd10_ucod_nodot), .(
    n_deaths_raw = .N,
    year_min = min(year_id, na.rm = TRUE),
    year_max = max(year_id, na.rm = TRUE)
  ), by = .(icd10_ucod_nodot, icd10_ucod_dot)]
  setorder(obs, icd10_ucod_nodot)

  cm_regex_all <- unique(
    cm[!is.na(icd10_regex) & nzchar(icd10_regex),
       .(cause_concept_id, cause_code, cause_name, cause_level, parent_concept_id, is_terminal, is_garbage, is_residual, is_covid_related, icd10_regex)],
    by = "cause_concept_id"
  )
  setorderv(cm_regex_all, c("cause_level", "cause_concept_id"), c(-1L, 1L))

  covid_specific_target_id <- cm[
    is_terminal == TRUE & (cause_name == "COVID-19" | cause_code == "COVID_19"),
    cause_concept_id
  ][1]

  msg("Mapeando códigos observados con la misma prioridad del pipeline.")
  causemap_list <- vector("list", nrow(obs))
  for (i in seq_len(nrow(obs))) {
    cd_nd <- obs$icd10_ucod_nodot[i]
    cd_dot <- obs$icd10_ucod_dot[i]
    midx <- match_regex_idx(cd_nd, cm_regex_all$icd10_regex)
    used_rep <- "nodot"
    if (length(midx) == 0L) {
      midx <- match_regex_idx(cd_dot, cm_regex_all$icd10_regex)
      used_rep <- "dot"
    }
    if (length(midx) == 0L) {
      causemap_list[[i]] <- data.table(
        icd10_ucod_nodot = cd_nd,
        icd10_ucod_dot = cd_dot,
        mapped_cause_concept_id = NA_integer_,
        mapped_cause_code = NA_character_,
        mapped_cause_name = NA_character_,
        mapped_cause_level = NA_integer_,
        mapped_is_terminal = NA,
        mapped_is_garbage_master = NA,
        mapped_is_residual = NA,
        mapped_is_covid_related = NA,
        n_cause_hits = 0L,
        used_priority_override = FALSE,
        used_representation = NA_character_
      )
    } else {
      selected_idx <- midx[1]
      used_priority_override <- FALSE
      if (!is.na(covid_specific_target_id) &&
          grepl("^U(?:07|09|10)", cd_nd) &&
          covid_specific_target_id %in% cm_regex_all$cause_concept_id[midx]) {
        selected_idx <- midx[match(covid_specific_target_id, cm_regex_all$cause_concept_id[midx])]
        used_priority_override <- TRUE
      }
      causemap_list[[i]] <- data.table(
        icd10_ucod_nodot = cd_nd,
        icd10_ucod_dot = cd_dot,
        mapped_cause_concept_id = cm_regex_all$cause_concept_id[selected_idx],
        mapped_cause_code = cm_regex_all$cause_code[selected_idx],
        mapped_cause_name = cm_regex_all$cause_name[selected_idx],
        mapped_cause_level = cm_regex_all$cause_level[selected_idx],
        mapped_is_terminal = cm_regex_all$is_terminal[selected_idx],
        mapped_is_garbage_master = cm_regex_all$is_garbage[selected_idx],
        mapped_is_residual = cm_regex_all$is_residual[selected_idx],
        mapped_is_covid_related = cm_regex_all$is_covid_related[selected_idx],
        n_cause_hits = as.integer(length(midx)),
        used_priority_override = used_priority_override,
        used_representation = used_rep
      )
    }
  }
  causemap <- rbindlist(causemap_list, use.names = TRUE, fill = TRUE)

msg("Mapeando cobertura por reglas de redistribución.")
  rr_map <- unique(
    rr[!is.na(regex_r) & nzchar(regex_r),
       .(rule_id, source_group_code, source_group_name, regex_r)],
    by = c("rule_id", "source_group_code", "source_group_name", "regex_r")
  )
  rule_hits_list <- vector("list", nrow(obs))
  for (i in seq_len(nrow(obs))) {
    cd <- obs$icd10_ucod_nodot[i]
    ridx <- match_regex_idx(cd, rr_map$regex_r)
    if (length(ridx) == 0L) {
      rule_hits_list[[i]] <- data.table(
        icd10_ucod_nodot = cd,
        n_rule_hits = 0L,
        source_group_code = NA_character_,
        source_group_name = NA_character_,
        rule_id = NA_character_
      )
    } else {
      rule_hits_list[[i]] <- data.table(
        icd10_ucod_nodot = cd,
        n_rule_hits = as.integer(length(ridx)),
        source_group_code = paste(unique(rr_map$source_group_code[ridx]), collapse = " | "),
        source_group_name = paste(unique(rr_map$source_group_name[ridx]), collapse = " | "),
        rule_id = paste(unique(rr_map$rule_id[ridx]), collapse = " | ")
      )
    }
  }
  rule_hits <- rbindlist(rule_hits_list, use.names = TRUE, fill = TRUE)

  sex_rule_applies <- function(sex_id, sex_restriction) {
    if (is.na(sex_restriction) || sex_restriction == "") return(TRUE)
    if (sex_restriction == "male"   && sex_id == 8507L) return(TRUE)
    if (sex_restriction == "female" && sex_id == 8532L) return(TRUE)
    FALSE
  }

  age_rule_applies <- function(age, age_start, age_end) {
    ok_start <- is.na(age_start) || age >= age_start
    ok_end   <- is.na(age_end)   || age <= age_end
    ok_start && ok_end
  }

  msg("Reconstruyendo precedencia real de redistribución sobre combinaciones agregadas.")
  dx_combo <- dx[, .(
    n_deaths_combo = .N
  ), by = .(
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    icd10_ucod_nodot,
    icd10_ucod_dot
  )]

  ru_map_row <- unique(
    rr[!is.na(regex_r) & nzchar(regex_r),
       .(rule_id, source_group_code, source_group_name, regex_r,
         sex_restriction = clean_chr(sex_restriction),
         age_start = suppressWarnings(as.integer(age_start)),
         age_end = suppressWarnings(as.integer(age_end)))],
    by = c("rule_id", "source_group_code", "source_group_name", "regex_r",
           "sex_restriction", "age_start", "age_end")
  )

  code_rule_candidates_list <- vector("list", nrow(obs))
  for (i in seq_len(nrow(obs))) {
    cd <- obs$icd10_ucod_nodot[i]
    ridx <- match_regex_idx(cd, ru_map_row$regex_r)
    if (length(ridx) == 0L) {
      code_rule_candidates_list[[i]] <- NULL
    } else {
      cand <- copy(ru_map_row[ridx])
      cand[, `:=`(icd10_ucod_nodot = cd, has_rule_candidate = TRUE)]
      code_rule_candidates_list[[i]] <- cand
    }
  }
  code_rule_candidates <- rbindlist(code_rule_candidates_list, use.names = TRUE, fill = TRUE)

  combo_rules <- merge(
    dx_combo,
    code_rule_candidates,
    by = "icd10_ucod_nodot",
    all.x = TRUE,
    sort = FALSE,
    allow.cartesian = TRUE
  )
  combo_rules[, rule_applies := !is.na(rule_id) &
                mapply(sex_rule_applies, sex_id, sex_restriction) &
                mapply(age_rule_applies, age, age_start, age_end)]
  selected_combo_rules <- combo_rules[rule_applies == TRUE]
  if (nrow(selected_combo_rules) > 0L) {
    setorder(selected_combo_rules, icd10_ucod_nodot, sex_id, age, source_group_code, rule_id)
    selected_combo_rules <- selected_combo_rules[, .SD[1], by = .(sex_id, age, icd10_ucod_nodot)]
  } else {
    selected_combo_rules <- data.table(
      sex_id = integer(), age = integer(), icd10_ucod_nodot = character(),
      rule_id = character(), source_group_code = character(), source_group_name = character()
    )
  }

  row_rules <- merge(
    dx_combo,
    selected_combo_rules[, .(sex_id, age, icd10_ucod_nodot, rule_id, source_group_code, source_group_name)],
    by = c("sex_id", "age", "icd10_ucod_nodot"),
    all.x = TRUE,
    sort = FALSE
  )
  row_rules[, has_selected_rule := !is.na(rule_id)]

  causemap_row <- merge(
    dx_combo,
    causemap[, .(
      icd10_ucod_nodot, icd10_ucod_dot,
      mapped_cause_concept_id, mapped_cause_code, mapped_cause_name,
      mapped_cause_level, mapped_is_terminal, mapped_is_garbage_master,
      mapped_is_residual, mapped_is_covid_related
    )],
    by = c("icd10_ucod_nodot", "icd10_ucod_dot"),
    all.x = TRUE,
    sort = FALSE
  )
  row_resolution <- merge(
    causemap_row,
    row_rules,
    by = c("sex_id", "age", "icd10_ucod_nodot", "icd10_ucod_dot", "n_deaths_combo"),
    all.x = TRUE,
    sort = FALSE
  )
  row_resolution[is.na(has_selected_rule), has_selected_rule := FALSE]

  nonfatal_hard_exclude_ids <- c(
    9000580L, 9001280L, 9001360L, 9001380L, 9001370L, 9000880L, 9000831L,
    9000832L, 9000840L, 9000850L, 9000900L, 9000911L, 9000912L, 9000920L,
    9000990L, 9001000L, 9001020L, 9001070L, 9001090L, 9001490L, 9001502L,
    9000270L, 9000362L, 9000365L, 9000560L
  )

  row_resolution[, final_handling_status := fifelse(
    has_selected_rule == TRUE,
    "redistributed",
    fifelse(
      !is.na(mapped_cause_concept_id) & mapped_cause_concept_id %in% nonfatal_hard_exclude_ids,
      "excluded_nonfatal_direct",
      fifelse(
        !is.na(mapped_cause_concept_id) & !isTRUE(mapped_is_garbage_master),
        "direct_final",
        fifelse(
          !is.na(mapped_cause_concept_id) & isTRUE(mapped_is_garbage_master),
          "garbage_without_selected_rule",
          "uncovered"
        )
      )
    )
  )]

  code_handling_summary <- row_resolution[, .(
    n_deaths = sum(n_deaths_combo, na.rm = TRUE),
    final_source_group_name = collapse_vals(unique(na.omit(source_group_name)), max_n = 20L),
    final_rule_id = collapse_vals(unique(na.omit(rule_id)), max_n = 20L),
    final_cause_concept_id = collapse_vals(unique(na.omit(mapped_cause_concept_id)), max_n = 20L),
    final_cause_name = collapse_vals(unique(na.omit(mapped_cause_name)), max_n = 20L)
  ), by = .(icd10_ucod_nodot, icd10_ucod_dot, final_handling_status)]

  code_handling_wide <- dcast(
    code_handling_summary,
    icd10_ucod_nodot + icd10_ucod_dot ~ final_handling_status,
    value.var = "n_deaths",
    fun.aggregate = sum,
    fill = 0
  )
  code_handling_main <- code_handling_summary[
    order(icd10_ucod_nodot, -n_deaths)
  ][, .SD[1], by = icd10_ucod_nodot]
  setnames(code_handling_main,
           c("final_handling_status", "final_source_group_name", "final_rule_id", "final_cause_concept_id", "final_cause_name"),
           c("primary_final_handling_status", "primary_source_group_name", "primary_rule_id", "primary_final_cause_concept_id", "primary_final_cause_name"))

  icd10_observed_coverage <- merge(obs, causemap, by = c("icd10_ucod_nodot", "icd10_ucod_dot"), all.x = TRUE, sort = FALSE)
  icd10_observed_coverage <- merge(icd10_observed_coverage, rule_hits, by = "icd10_ucod_nodot", all.x = TRUE, sort = FALSE)
  icd10_observed_coverage <- merge(icd10_observed_coverage, code_handling_wide, by = c("icd10_ucod_nodot", "icd10_ucod_dot"), all.x = TRUE, sort = FALSE)
  icd10_observed_coverage <- merge(icd10_observed_coverage, code_handling_main, by = c("icd10_ucod_nodot", "icd10_ucod_dot"), all.x = TRUE, sort = FALSE)
  if (nrow(direct_demog_detected) > 0L) {
    demog_summary <- direct_demog_detected[, .(
      direct_demographic_handled_deaths = sum(n_deaths, na.rm = TRUE),
      direct_demographic_handling_label = collapse_vals(unique(handling_label), max_n = 10L),
      direct_demographic_replacement_group = collapse_vals(unique(replacement_source_group_name), max_n = 10L)
    ), by = .(icd10_ucod_nodot)]
    icd10_observed_coverage <- merge(
      icd10_observed_coverage,
      demog_summary,
      by = "icd10_ucod_nodot",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    icd10_observed_coverage[, `:=`(
      direct_demographic_handled_deaths = NA_real_,
      direct_demographic_handling_label = NA_character_,
      direct_demographic_replacement_group = NA_character_
    )]
  }
  if (nrow(direct_specific_detected) > 0L) {
    specific_summary <- direct_specific_detected[, .(
      direct_specific_handled_deaths = sum(n_deaths, na.rm = TRUE),
      direct_specific_handling_label = collapse_vals(unique(specific_handling_label), max_n = 10L),
      direct_specific_handling_type = collapse_vals(unique(handling_type), max_n = 10L)
    ), by = .(icd10_ucod_nodot)]
    icd10_observed_coverage <- merge(
      icd10_observed_coverage,
      specific_summary,
      by = "icd10_ucod_nodot",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    icd10_observed_coverage[, `:=`(
      direct_specific_handled_deaths = NA_real_,
      direct_specific_handling_label = NA_character_,
      direct_specific_handling_type = NA_character_
    )]
  }
  for (nm in c("direct_final", "redistributed", "excluded_nonfatal_direct", "garbage_without_selected_rule", "uncovered")) {
    if (!nm %in% names(icd10_observed_coverage)) icd10_observed_coverage[, (nm) := 0L]
    icd10_observed_coverage[is.na(get(nm)), (nm) := 0L]
  }
  for (nm in c("direct_demographic_handled_deaths", "direct_specific_handled_deaths")) {
    if (!nm %in% names(icd10_observed_coverage)) icd10_observed_coverage[, (nm) := 0]
    icd10_observed_coverage[is.na(get(nm)), (nm) := 0]
  }
  icd10_observed_coverage[, coverage_status := fifelse(
    direct_specific_handled_deaths > 0,
    "direct_specific_handled",
    fifelse(
      direct_demographic_handled_deaths > 0,
      "direct_demographic_incompatibility_handled",
      fifelse(
        !is.na(mapped_cause_concept_id) & !isTRUE(mapped_is_garbage_master),
        "direct_cause_map",
        fifelse(
          !is.na(mapped_cause_concept_id) & isTRUE(mapped_is_garbage_master) & n_rule_hits > 0L,
          "garbage_source_with_rule",
          fifelse(
            !is.na(mapped_cause_concept_id) & isTRUE(mapped_is_garbage_master) & (is.na(n_rule_hits) | n_rule_hits == 0L),
            "garbage_source_without_rule",
            fifelse(
              is.na(mapped_cause_concept_id) & n_rule_hits > 0L,
              "redistribution_only",
              "uncovered"
            )
          )
        )
      )
    )
  )]
  icd10_observed_coverage[, covered_anywhere := coverage_status != "uncovered"]
  icd10_observed_coverage[, final_chain_status := fifelse(
    direct_specific_handled_deaths > 0,
    "all_rows_direct_specific_handled",
    fifelse(
      direct_demographic_handled_deaths > 0,
      "all_rows_direct_demographic_incompatibility_handled",
      fifelse(
        redistributed > 0L & direct_final == 0L & excluded_nonfatal_direct == 0L,
        "all_rows_redistributed",
        fifelse(
          direct_final > 0L & redistributed == 0L & excluded_nonfatal_direct == 0L,
          "all_rows_direct_final",
          fifelse(
            excluded_nonfatal_direct > 0L & direct_final == 0L & redistributed == 0L,
            "all_rows_excluded_nonfatal_direct",
            fifelse(
              uncovered > 0L,
              "contains_uncovered_rows",
              "mixed_final_handling"
            )
          )
        )
      )
    )
  )]

  qc_uncovered <- icd10_observed_coverage[coverage_status == "uncovered"]
  qc_coverage_summary <- icd10_observed_coverage[, .(
    n_codes = .N,
    n_deaths_raw = sum(n_deaths_raw, na.rm = TRUE)
  ), by = coverage_status][order(coverage_status)]
  qc_final_chain_summary <- icd10_observed_coverage[, .(
    n_codes = .N,
    n_deaths_raw = sum(n_deaths_raw, na.rm = TRUE)
  ), by = final_chain_status][order(final_chain_status)]
  fwrite(qc_uncovered, file.path(CFG$qc_dir, "qc_uncovered_observed_icd10.csv"))
  fwrite(qc_coverage_summary, file.path(CFG$qc_dir, "qc_observed_coverage_summary.csv"))
  fwrite(qc_final_chain_summary, file.path(CFG$qc_dir, "qc_observed_final_chain_summary.csv"))

  msg("Construyendo catálogo metodológico de causas.")
  mort_used_ids <- unique(mort[cause_level > 0L, cause_concept_id])
  avp_used_ids <- unique(avp[cause_level > 0L, cause_concept_id])

  direct_hits_lookup <- cm_regex_all[, {
    hits <- obs[grepl(icd10_regex[1], icd10_ucod_nodot, perl = TRUE), icd10_ucod_nodot]
    .(
      n_icd10_directos_observados = length(unique(hits)),
      icd10_directos_observados = collapse_vals(hits, max_n = 80L)
    )
  }, by = .(cause_concept_id, icd10_regex)]
  direct_hits_lookup[, icd10_regex := NULL]

  explicit_exclusions <- cm_regex_all[, .(
    icd10_excluidos_explicitos_regex = collapse_vals(extract_negative_lookahead_codes(icd10_regex[1]), max_n = 80L)
  ), by = .(cause_concept_id, icd10_regex)]
  explicit_exclusions[, icd10_regex := NULL]

  rr_targets <- rr[, .(
    n_reglas_que_targetean = uniqueN(rule_id),
    receives_redistribution = TRUE
  ), by = .(cause_concept_id = target_cause_concept_id)]

  hierarchy_cols <- c("cause_concept_id", "mismatch_class", "impact_risk", "recommended_action")
  cause_catalog_methods <- merge(cm, direct_hits_lookup, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  cause_catalog_methods <- merge(cause_catalog_methods, explicit_exclusions, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  cause_catalog_methods <- merge(cause_catalog_methods, rr_targets, by = "cause_concept_id", all.x = TRUE, sort = FALSE)
  cause_catalog_methods <- merge(
    cause_catalog_methods,
    hierarchy_qc[, ..hierarchy_cols],
    by = "cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  cause_catalog_methods[, receives_redistribution := fifelse(is.na(receives_redistribution), FALSE, receives_redistribution)]
  cause_catalog_methods[, used_for_mortality := cause_concept_id %in% mort_used_ids]
  cause_catalog_methods[, used_for_avp := cause_concept_id %in% avp_used_ids]
  cause_catalog_methods[, receives_direct_mapping := !is.na(icd10_regex) & nzchar(icd10_regex)]

  msg("Construyendo catálogo metodológico de redistribución.")
  rr_targets_named <- merge(
    rr,
    cm[, .(target_cause_concept_id = cause_concept_id, target_cause_name = cause_name, target_cause_level = cause_level, target_is_terminal = is_terminal)],
    by = "target_cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )

  observed_by_rule <- rr_map[, {
    hits <- obs[grepl(regex_r[1], icd10_ucod_nodot, perl = TRUE), icd10_ucod_nodot]
    .(
      n_icd10_observados_fuente = length(unique(hits)),
      icd10_observados_fuente = collapse_vals(hits, max_n = 80L)
    )
  }, by = .(rule_id, source_group_code, source_group_name)]

  redistribution_methods_catalog_long <- merge(
    rr_targets_named,
    observed_by_rule,
    by = c("rule_id", "source_group_code", "source_group_name"),
    all.x = TRUE,
    sort = FALSE
  )
  redistribution_methods_catalog_long[, method_source := "project_pipeline"]
  redistribution_methods_catalog_long[, redistribution_reason := fifelse(
    grepl("nonfatal|yll", tolower(paste(source_group_name, notes)), perl = TRUE),
    "Código/categoría no letal o no elegible como causa final de mortalidad; se redirige a causas terminales plausibles.",
    "Grupo garbage / causa puente / código fuente no aceptado como causa final directa; se redistribuye según reglas del proyecto."
  )]

  redistribution_methods_catalog_summary <- redistribution_methods_catalog_long[, .(
    regex_r = first(regex_r),
    n_targets = uniqueN(target_cause_concept_id),
    targets = collapse_vals(paste0(target_cause_name, " [", target_cause_concept_id, "]"), max_n = 30L),
    n_icd10_observados_fuente = first(n_icd10_observados_fuente),
    icd10_observados_fuente = first(icd10_observados_fuente),
    redistribution_method = first(redistribution_method),
    redistribution_scope = first(redistribution_scope),
    notes = first(notes),
    redistribution_reason = first(redistribution_reason),
    method_source = first(method_source)
  ), by = .(rule_id, source_group_code, source_group_name)]

  qc_nonterminal_targets <- redistribution_methods_catalog_long[target_is_terminal != TRUE]
  fwrite(qc_nonterminal_targets, file.path(CFG$qc_dir, "qc_redistribution_nonterminal_targets.csv"))

  sensitive_methodological_positions <- data.table()
  if (nrow(sensitive_positions_patch) > 0L) {
    msg("Construyendo tabla metodológica de casos sensibles OMS/proyecto.")
    sensitive_positions_patch[, observed_regex := clean_chr(observed_regex)]
    sens_list <- vector("list", nrow(sensitive_positions_patch))
    for (i in seq_len(nrow(sensitive_positions_patch))) {
      rowi <- sensitive_positions_patch[i]
      hits <- icd10_observed_coverage[grepl(rowi$observed_regex, icd10_ucod_nodot, perl = TRUE)]
      sens_list[[i]] <- data.table(
        case_id = rowi$case_id,
        short_label = rowi$short_label,
        observed_regex = rowi$observed_regex,
        n_codes_observed = uniqueN(hits$icd10_ucod_nodot),
        n_deaths_observed = sum(hits$n_deaths_raw, na.rm = TRUE),
        observed_codes = collapse_vals(hits$icd10_ucod_nodot, max_n = 50L),
        coverage_status_summary = collapse_vals(unique(hits$coverage_status), max_n = 10L),
        final_chain_status_summary = collapse_vals(unique(hits$final_chain_status), max_n = 10L),
        mapped_causes_summary = collapse_vals(unique(hits$mapped_cause_name), max_n = 20L),
        primary_source_groups_summary = collapse_vals(unique(na.omit(hits$primary_source_group_name)), max_n = 20L),
        primary_final_causes_summary = collapse_vals(unique(na.omit(hits$primary_final_cause_name)), max_n = 20L),
        who_position = rowi$who_position,
        australia_position = rowi$australia_position,
        project_position_preferred = rowi$project_position_preferred,
        recommendation = rowi$recommendation,
        core_change_status = rowi$core_change_status,
        redistribution_change_status = rowi$redistribution_change_status,
        rationale = rowi$rationale,
        annex_note = rowi$annex_note
      )
    }
    sensitive_methodological_positions <- rbindlist(sens_list, use.names = TRUE, fill = TRUE)
    write_tabular_with_dict(
      sensitive_methodological_positions,
      stem = "sensitive_methodological_positions_review",
      out_dir = CFG$out_dir,
      dataset_id = CFG$dataset_id,
      version = CFG$version,
      run_id = run_id,
      table_note = "Tabla metodológica de casos sensibles OMS / Australia / proyecto"
    )
  }

  out_files <- list(
    cause_catalog_methods_csv = file.path(CFG$out_dir, "cause_catalog_methods.csv"),
    cause_catalog_methods_parquet = file.path(CFG$out_dir, "cause_catalog_methods.parquet"),
    redistribution_methods_catalog_long_csv = file.path(CFG$out_dir, "redistribution_methods_catalog_long.csv"),
    redistribution_methods_catalog_long_parquet = file.path(CFG$out_dir, "redistribution_methods_catalog_long.parquet"),
    redistribution_methods_catalog_summary_csv = file.path(CFG$out_dir, "redistribution_methods_catalog_summary.csv"),
    redistribution_methods_catalog_summary_parquet = file.path(CFG$out_dir, "redistribution_methods_catalog_summary.parquet"),
    icd10_observed_coverage_csv = file.path(CFG$out_dir, "icd10_observed_coverage.csv"),
    icd10_observed_coverage_parquet = file.path(CFG$out_dir, "icd10_observed_coverage.parquet")
  )

  fwrite(cause_catalog_methods, out_files$cause_catalog_methods_csv)
  write_parquet(cause_catalog_methods, out_files$cause_catalog_methods_parquet)
  fwrite(redistribution_methods_catalog_long, out_files$redistribution_methods_catalog_long_csv)
  write_parquet(redistribution_methods_catalog_long, out_files$redistribution_methods_catalog_long_parquet)
  fwrite(redistribution_methods_catalog_summary, out_files$redistribution_methods_catalog_summary_csv)
  write_parquet(redistribution_methods_catalog_summary, out_files$redistribution_methods_catalog_summary_parquet)
  fwrite(icd10_observed_coverage, out_files$icd10_observed_coverage_csv)
  write_parquet(icd10_observed_coverage, out_files$icd10_observed_coverage_parquet)

  for (fp in unlist(out_files)) {
    register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "staging",
      artifact_path = fp,
      n_rows = NA_integer_,
      n_cols = NA_integer_,
      notes = "Catálogo metodológico base"
    )
  }

  register_run_finish(run_id, status = "success", message = "Métodos base exportados")
  msg("OK -> métodos base exportados en: ", CFG$out_dir)
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = conditionMessage(e))
  stop(e)
})
