#!/usr/bin/env Rscript

# ============================================================
# 06_map_and_redistribute_deaths.R
# ------------------------------------------------------------
# Script fusionado:
#   - toma death_record_normalized
#   - mapea ICD-10 al nivel más específico disponible
#   - identifica garbage codes
#   - redistribuye garbage a terminales
#   - construye tabla canónica leaf operacional
#   - genera roll-ups L3/L2/L1
#   - conserva masa contable pre/post
#
# Soporta:
#   - sex_restriction opcional
#   - age_start / age_end opcionales
#
# Esta versión añade auditoría reforzada para detectar pérdidas
# contables y conflictos de reglas.
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
  library(stringi)
})

source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))

output_suffix <- Sys.getenv("REDIST_OUTPUT_SUFFIX", unset = "")
output_suffix_path <- if (nzchar(output_suffix)) paste0("_", output_suffix) else ""
redis_mode <- Sys.getenv("REDIST_MODE", unset = "canonical")

CFG <- list(
  version = "v1.1.3_covid_direct_priority_and_qc",
  dataset_id_leaf = "death_cause_leaf_post_redistribution",
  table_name_leaf = "death_cause_leaf_post_redistribution",
  
  dataset_id_l3 = "death_cause_rollup_l3",
  table_name_l3 = "death_cause_rollup_l3",
  
  dataset_id_l2 = "death_cause_rollup_l2",
  table_name_l2 = "death_cause_rollup_l2",
  
  dataset_id_l1 = "death_cause_rollup_l1",
  table_name_l1 = "death_cause_rollup_l1",
  
  input_death_path = here("data", "final", "death_record_normalized", "death_record_normalized.parquet"),
  input_cause_path = here("data", "final", "cause_master", "cause_master.csv"),
  input_rules_path = here("data", "final", "redistribution_rules", "redistribution_rules.csv"),
  input_demog_incompatibility_patch_path = here("data", "raw", "redistribution_rules", "patch_direct_demographic_incompatibility_handling.csv"),
  input_direct_specific_handling_patch_path = here("data", "raw", "redistribution_rules", "patch_direct_specific_icd_handling.csv"),
  
  out_dir_leaf = here("data", "final", paste0("death_cause_leaf_post_redistribution", output_suffix_path)),
  out_dir_l3   = here("data", "final", paste0("death_cause_rollup_l3", output_suffix_path)),
  out_dir_l2   = here("data", "final", paste0("death_cause_rollup_l2", output_suffix_path)),
  out_dir_l1   = here("data", "final", paste0("death_cause_rollup_l1", output_suffix_path)),
  
  qc_dir = qc_dir_path(paste0("map_and_redistribute_deaths", output_suffix_path)),
  methods_dir = here("data", "derived", "methods"),
  
  covid_year_min = 2020L,
  age_min = 0L,
  age_max = 110L,
  
  unmapped_non_gc_term_id = -1L,
  gc_no_target_term_id    = -2L,
  
  redistribution_version = if (nzchar(output_suffix)) paste0("v1_1_2_fused_with_audit_", output_suffix) else "v1_1_2_fused_with_audit",
  redis_mode = redis_mode,
  verbose = TRUE
)

for (d in c(CFG$out_dir_leaf, CFG$out_dir_l3, CFG$out_dir_l2, CFG$out_dir_l1, CFG$qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}
dir.create(CFG$methods_dir, recursive = TRUE, showWarnings = FALSE)

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
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
  z[nchar(z) == 0] <- NA_character_
  z
}

classify_covid_code_family <- function(x) {
  z <- norm_icd10_nodot(x)
  out <- rep(NA_character_, length(z))
  out[!is.na(z) & grepl("^U07", z)] <- "u07_acute"
  out[!is.na(z) & grepl("^U09", z)] <- "u09"
  out[!is.na(z) & grepl("^U10", z)] <- "u10"
  out
}

icd10_add_dot <- function(x) {
  z <- clean_chr(x)
  z <- toupper(z)
  z[nchar(z) == 0] <- NA_character_
  has_dot <- !is.na(z) & grepl("\\.", z)
  idx <- !has_dot & !is.na(z) & nchar(z) > 3
  z[idx] <- paste0(substr(z[idx], 1, 3), ".", substr(z[idx], 4, nchar(z[idx])))
  z
}

match_regex_idx <- function(code, patterns) {
  which(vapply(patterns, function(p) {
    if (is.na(p) || p == "") return(FALSE)
    grepl(p, code, perl = TRUE)
  }, logical(1)))
}

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

safe_pct <- function(num, den) {
  if (is.na(den) || den <= 0) return(0)
  round(100 * num / den, 6)
}

split_int_csv <- function(x) {
  z <- clean_chr(x)
  if (is.na(z)) return(integer())
  vals <- trimws(unlist(strsplit(z, ",", fixed = TRUE), use.names = FALSE))
  vals <- vals[nzchar(vals)]
  out <- suppressWarnings(as.integer(vals))
  unique(out[!is.na(out)])
}

split_num_csv <- function(x) {
  z <- clean_chr(x)
  if (is.na(z)) return(numeric())
  vals <- trimws(unlist(strsplit(z, ",", fixed = TRUE), use.names = FALSE))
  vals <- vals[nzchar(vals)]
  out <- suppressWarnings(as.numeric(vals))
  out[!is.na(out)]
}

ensure_project_dirs()
ensure_catalog_files()

for (fp in c(CFG$input_death_path, CFG$input_cause_path, CFG$input_rules_path)) {
  assert_exists(fp)
}

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id_leaf, version = CFG$version)

tryCatch({
  
  msg("Leyendo death_record_normalized...")
  dx <- as.data.table(read_auto(CFG$input_death_path))
  
  msg("Leyendo cause_master...")
  cm <- as.data.table(read_auto(CFG$input_cause_path))
  
  msg("Leyendo redistribution_rules...")
  ru <- as.data.table(read_auto(CFG$input_rules_path))
  
  demog_patch <- data.table(
    cause_concept_id = integer(),
    cause_name = character(),
    apply_on_sex_incompatibility = integer(),
    apply_on_age_incompatibility = integer(),
    replacement_rule_id = character(),
    handling_label = character(),
    handling_note = character(),
    method_source = character(),
    active = integer()
  )
  if (file.exists(CFG$input_demog_incompatibility_patch_path)) {
    msg("Leyendo patch de incompatibilidades demográficas directas...")
    demog_patch <- fread(CFG$input_demog_incompatibility_patch_path, encoding = "UTF-8")
  }
  
  specific_icd_patch <- data.table(
    patch_id = character(),
    icd10_regex = character(),
    sex_restriction = character(),
    age_start = integer(),
    age_end = integer(),
    handling_type = character(),
    replacement_rule_id = character(),
    split_target_concept_ids = character(),
    fallback_target_weights = character(),
    dynamic_weight_strategy = character(),
    handling_label = character(),
    handling_note = character(),
    method_source = character(),
    active = integer()
  )
  if (file.exists(CFG$input_direct_specific_handling_patch_path)) {
    msg("Leyendo patch de manejo directo especÃ­fico por ICD-10...")
    specific_icd_patch <- fread(CFG$input_direct_specific_handling_patch_path, encoding = "UTF-8")
  }
  
  req_dx <- c("death_id", "year_id", "sex_id", "age", "location_id", "icd10_ucod", "source_dataset", "run_id")
  miss_dx <- setdiff(req_dx, names(dx))
  if (length(miss_dx) > 0L) stop("Faltan columnas en death_record_normalized: ", paste(miss_dx, collapse = ", "))
  
  req_cm <- c(
    "cause_concept_id", "cause_code", "cause_name", "cause_level",
    "parent_concept_id", "is_terminal", "is_garbage", "is_residual",
    "is_covid_related", "icd10_regex"
  )
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  
  req_ru <- c(
    "rule_id", "source_group_code", "source_group_name",
    "target_cause_concept_id", "target_weight",
    "sex_restriction", "age_start", "age_end", "regex_r"
  )
  miss_ru <- setdiff(req_ru, names(ru))
  if (length(miss_ru) > 0L) stop("Faltan columnas en redistribution_rules: ", paste(miss_ru, collapse = ", "))
  
  dx[, `:=`(
    death_id = as.character(death_id),
    year_id = as.integer(year_id),
    sex_id = as.integer(sex_id),
    age = as.integer(pmax(CFG$age_min, pmin(CFG$age_max, age))),
    location_id = as.integer(location_id),
    icd10_ucod = as.character(icd10_ucod),
    source_dataset = as.character(source_dataset),
    input_run_id = as.character(run_id)
  )]
  
  cm[, `:=`(
    cause_concept_id = as.integer(cause_concept_id),
    cause_code = as.character(cause_code),
    cause_name = as.character(cause_name),
    cause_level = as.integer(cause_level),
    parent_concept_id = as.integer(parent_concept_id),
    is_terminal = as.logical(is_terminal),
    is_garbage = as.logical(is_garbage),
    is_residual = as.logical(is_residual),
    is_covid_related = as.logical(is_covid_related),
    icd10_regex = as.character(icd10_regex)
  )]
  if (!"sex_restriction_target_default" %in% names(cm)) cm[, sex_restriction_target_default := NA_character_]
  if (!"target_age_start_default" %in% names(cm)) cm[, target_age_start_default := NA_integer_]
  if (!"target_age_end_default" %in% names(cm)) cm[, target_age_end_default := NA_integer_]
  cm[, sex_restriction_target_default := clean_chr(as.character(sex_restriction_target_default))]
  cm[, target_age_start_default := suppressWarnings(as.integer(target_age_start_default))]
  cm[, target_age_end_default := suppressWarnings(as.integer(target_age_end_default))]
  
  if (!"yll_flag" %in% names(cm)) cm[, yll_flag := NA_integer_]
  cm[, yll_flag := as.integer(yll_flag)]
  
  ru[, `:=`(
    rule_id = as.character(rule_id),
    source_group_code = as.character(source_group_code),
    source_group_name = as.character(source_group_name),
    target_cause_concept_id = as.integer(target_cause_concept_id),
    target_weight = as.numeric(target_weight),
    sex_restriction = clean_chr(sex_restriction),
    age_start = suppressWarnings(as.integer(age_start)),
    age_end = suppressWarnings(as.integer(age_end)),
    regex_r = as.character(regex_r)
  )]
  if (!"target_sex_restriction_cm" %in% names(ru)) ru[, target_sex_restriction_cm := NA_character_]
  if (!"target_age_start_cm" %in% names(ru)) ru[, target_age_start_cm := NA_integer_]
  if (!"target_age_end_cm" %in% names(ru)) ru[, target_age_end_cm := NA_integer_]
  ru[, target_sex_restriction_cm := clean_chr(target_sex_restriction_cm)]
  ru[, target_age_start_cm := suppressWarnings(as.integer(target_age_start_cm))]
  ru[, target_age_end_cm := suppressWarnings(as.integer(target_age_end_cm))]
  
  if (nrow(demog_patch) > 0L) {
    if (!"cause_concept_id" %in% names(demog_patch)) stop("El patch de incompatibilidades demográficas no trae cause_concept_id.")
    if (!"replacement_rule_id" %in% names(demog_patch)) stop("El patch de incompatibilidades demográficas no trae replacement_rule_id.")
    demog_patch[, cause_concept_id := suppressWarnings(as.integer(cause_concept_id))]
    demog_patch[, cause_name := as.character(cause_name)]
    demog_patch[, apply_on_sex_incompatibility := suppressWarnings(as.integer(apply_on_sex_incompatibility))]
    demog_patch[, apply_on_age_incompatibility := suppressWarnings(as.integer(apply_on_age_incompatibility))]
    demog_patch[, replacement_rule_id := as.character(replacement_rule_id)]
    demog_patch[, handling_label := clean_chr(handling_label)]
    demog_patch[, handling_note := clean_chr(handling_note)]
    demog_patch[, method_source := clean_chr(method_source)]
    demog_patch[, active := fifelse(is.na(suppressWarnings(as.integer(active))), 1L, suppressWarnings(as.integer(active)))]
    bad_demog_rules <- demog_patch[active == 1L & !replacement_rule_id %in% unique(ru$rule_id)]
    fwrite(bad_demog_rules, file.path(CFG$qc_dir, "qc_bad_demographic_incompatibility_replacement_rules.csv"))
    if (nrow(bad_demog_rules) > 0L) {
      stop("El patch de incompatibilidades demográficas referencia replacement_rule_id inexistentes.")
    }
  }
  
  if (nrow(specific_icd_patch) > 0L) {
    req_specific <- c("patch_id", "icd10_regex", "handling_type")
    miss_specific <- setdiff(req_specific, names(specific_icd_patch))
    if (length(miss_specific) > 0L) {
      stop("El patch de manejo directo específico no trae columnas requeridas: ",
           paste(miss_specific, collapse = ", "))
    }
    for (j in names(specific_icd_patch)) {
      if (is.character(specific_icd_patch[[j]]) || is.factor(specific_icd_patch[[j]])) {
        specific_icd_patch[, (j) := clean_chr(get(j))]
      }
    }
    specific_icd_patch[, `:=`(
      patch_id = as.character(patch_id),
      icd10_regex = as.character(icd10_regex),
      sex_restriction = clean_chr(sex_restriction),
      age_start = suppressWarnings(as.integer(age_start)),
      age_end = suppressWarnings(as.integer(age_end)),
      handling_type = as.character(handling_type),
      replacement_rule_id = clean_chr(replacement_rule_id),
      split_target_concept_ids = clean_chr(split_target_concept_ids),
      fallback_target_weights = clean_chr(fallback_target_weights),
      dynamic_weight_strategy = clean_chr(dynamic_weight_strategy),
      handling_label = clean_chr(handling_label),
      handling_note = clean_chr(handling_note),
      method_source = clean_chr(method_source),
      active = fifelse(is.na(suppressWarnings(as.integer(active))), 1L, suppressWarnings(as.integer(active)))
    )]
    specific_icd_patch <- specific_icd_patch[active == 1L]
    bad_specific_types <- specific_icd_patch[!handling_type %in% c("replace_with_rule", "dynamic_year_split")]
    fwrite(bad_specific_types, file.path(CFG$qc_dir, "qc_bad_direct_specific_icd_handling_type.csv"))
    if (nrow(bad_specific_types) > 0L) {
      stop("El patch de manejo directo específico contiene handling_type no soportados.")
    }
    bad_specific_rules <- specific_icd_patch[
      handling_type == "replace_with_rule" &
        (is.na(replacement_rule_id) | !replacement_rule_id %in% unique(ru$rule_id))
    ]
    fwrite(bad_specific_rules, file.path(CFG$qc_dir, "qc_bad_direct_specific_icd_replacement_rules.csv"))
    if (nrow(bad_specific_rules) > 0L) {
      stop("El patch de manejo directo específico referencia replacement_rule_id inexistentes.")
    }
  }
  
  dx[, icd10_ucod_nodot := norm_icd10_nodot(icd10_ucod)]
  dx[, icd10_ucod_dot := icd10_add_dot(icd10_ucod_nodot)]
  
  if (dx[is.na(icd10_ucod_nodot), .N] > 0L) {
    stop("Hay registros con icd10_ucod_nodot NA.")
  }
  
  dx[, covid_code_family := classify_covid_code_family(icd10_ucod_nodot)]
  
  bad_covid_pre2020 <- dx[year_id < CFG$covid_year_min & !is.na(covid_code_family), .N]
  qc_pandemic_pre2020_examples <- dx[
    year_id < CFG$covid_year_min & !is.na(covid_code_family),
    .(death_id, year_id, location_id, sex_id, age, icd10_ucod, icd10_ucod_nodot, covid_code_family, source_dataset)
  ]
  
  qc_covid_direct_code_input_by_year <- dx[
    !is.na(covid_code_family),
    .(n_deaths = .N),
    by = .(year_id, covid_code_family, icd10_ucod_nodot)
  ][order(year_id, covid_code_family, icd10_ucod_nodot)]
  
  pre <- dx[, .(n_deaths = .N),
            by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, icd10_ucod_dot)]
  
  codes <- unique(pre[, .(icd10_ucod_nodot, icd10_ucod_dot)])
  setorder(codes, icd10_ucod_nodot)
  
  msg("ICD10 únicos a evaluar: ", nrow(codes))
  
  # ==========================================================
  # Maestro: mapeo non-garbage
  # ==========================================================
  cm_all <- copy(cm)
  
  cm_regex_all <- cm_all[!is.na(icd10_regex) & icd10_regex != "",
                         .(
                           cause_concept_id,
                           cause_code,
                           cause_name,
                           cause_level,
                           parent_concept_id,
                           is_terminal,
                           is_garbage,
                           is_residual,
                           is_covid_related,
                           icd10_regex
                         )
  ]
  
  setorderv(cm_regex_all, cols = c("cause_level", "cause_concept_id"), order = c(-1L, 1L))
  cm_regex_all <- unique(cm_regex_all, by = "cause_concept_id")
  covid_specific_target_id <- cm_all[
    is_terminal == TRUE &
      (cause_name == "COVID-19" | cause_code == "COVID_19"),
    cause_concept_id
  ][1]
  
  cm_term <- cm_all[is_terminal == TRUE,
                    .(
                      term_id = cause_concept_id,
                      cause_level,
                      parent_concept_id,
                      cause_name,
                      cause_code
                    )
  ]
  
  if (nrow(cm_term) == 0L) stop("No hay nodos terminales en cause_master.")
  
  cm_parent <- unique(cm_all[, .(cause_concept_id, parent_concept_id, cause_level)])
  setkey(cm_parent, cause_concept_id)
  
  get_ancestors_one <- function(term_id, parent_dt) {
    anc <- integer()
    cur <- term_id
    repeat {
      row <- parent_dt[J(cur)]
      if (nrow(row) == 0L || is.na(row$parent_concept_id[1])) break
      par <- row$parent_concept_id[1]
      anc <- c(anc, par)
      cur <- par
    }
    unique(anc)
  }
  
  term_anc_list <- lapply(cm_term$term_id, function(tid) {
    anc <- get_ancestors_one(tid, cm_parent)
    data.table(term_id = tid, ancestor_id = anc)
  })
  term_anc <- rbindlist(term_anc_list, use.names = TRUE, fill = TRUE)
  term_anc <- unique(rbindlist(list(
    term_anc,
    data.table(term_id = cm_term$term_id, ancestor_id = cm_term$term_id)
  )))
  
  causemap_list <- vector("list", nrow(codes))
  
  for (i in seq_len(nrow(codes))) {
    cd_nd  <- codes$icd10_ucod_nodot[i]
    cd_dot <- codes$icd10_ucod_dot[i]
    
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
        mapped_parent_concept_id = NA_integer_,
        mapped_is_terminal = NA,
        mapped_is_garbage_master = NA,
        mapped_is_residual = NA,
        mapped_is_covid_related = NA,
        n_cause_hits = 0L,
        used_representation = NA_character_,
        used_priority_override = FALSE
      )
    } else {
      selected_idx <- midx[1]
      used_priority_override <- FALSE
      if (!is.na(covid_specific_target_id) &&
          !is.na(classify_covid_code_family(cd_nd)) &&
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
        mapped_parent_concept_id = cm_regex_all$parent_concept_id[selected_idx],
        mapped_is_terminal = cm_regex_all$is_terminal[selected_idx],
        mapped_is_garbage_master = cm_regex_all$is_garbage[selected_idx],
        mapped_is_residual = cm_regex_all$is_residual[selected_idx],
        mapped_is_covid_related = cm_regex_all$is_covid_related[selected_idx],
        n_cause_hits = as.integer(length(midx)),
        used_representation = used_rep,
        used_priority_override = used_priority_override
      )
    }
  }
  
  causemap <- rbindlist(causemap_list, use.names = TRUE, fill = TRUE)
  causemap[, covid_code_family := classify_covid_code_family(icd10_ucod_nodot)]
  
  qc_multiple_cause_hits <- causemap[n_cause_hits > 1]
  fwrite(qc_multiple_cause_hits, file.path(CFG$qc_dir, "qc_multiple_cause_hits.csv"))
  
  qc_covid_direct_code_mapping_by_year <- merge(
    pre[, .(year_id, icd10_ucod_nodot, n_deaths)],
    causemap[!is.na(covid_code_family), .(
      icd10_ucod_nodot,
      covid_code_family,
      mapped_cause_concept_id,
      mapped_cause_name,
      mapped_is_covid_related,
      used_priority_override
    )],
    by = "icd10_ucod_nodot",
    all.x = FALSE,
    sort = FALSE
  )[
    ,
    .(
      n_deaths = sum(n_deaths),
      used_priority_override_any = any(used_priority_override, na.rm = TRUE)
    ),
    by = .(
      year_id,
      covid_code_family,
      mapped_cause_concept_id,
      mapped_cause_name,
      mapped_is_covid_related
    )
  ][order(year_id, covid_code_family, -n_deaths, mapped_cause_concept_id)]
  fwrite(
    qc_covid_direct_code_input_by_year,
    file.path(CFG$qc_dir, "qc_covid_direct_code_input_by_year.csv")
  )
  fwrite(
    qc_covid_direct_code_mapping_by_year,
    file.path(CFG$qc_dir, "qc_covid_direct_code_mapping_by_year.csv")
  )
  
  # ==========================================================
  # Garbage rules candidatas
  # ==========================================================
  ru_map <- unique(
    ru[!is.na(regex_r) & regex_r != "",
       .(rule_id, source_group_code, source_group_name, regex_r,
         sex_restriction, age_start, age_end)],
    by = c("rule_id", "source_group_code", "source_group_name", "regex_r",
           "sex_restriction", "age_start", "age_end")
  )
  
  ru_map[, rule_id_num := suppressWarnings(as.integer(gsub("[^0-9]", "", rule_id)))]
  setorderv(ru_map, cols = c("rule_id_num", "rule_id"), order = c(1L, 1L))
  
  rule_candidates_list <- vector("list", nrow(codes))
  for (i in seq_len(nrow(codes))) {
    cd <- codes$icd10_ucod_nodot[i]
    ridx <- match_regex_idx(cd, ru_map$regex_r)
    
    if (length(ridx) == 0L) {
      rule_candidates_list[[i]] <- data.table(
        icd10_ucod_nodot = cd,
        rule_id = NA_character_,
        source_group_code = NA_character_,
        source_group_name = NA_character_,
        sex_restriction = NA_character_,
        age_start = NA_integer_,
        age_end = NA_integer_,
        n_rule_hits = 0L
      )
    } else {
      rule_candidates_list[[i]] <- data.table(
        icd10_ucod_nodot = cd,
        rule_id = ru_map$rule_id[ridx],
        source_group_code = ru_map$source_group_code[ridx],
        source_group_name = ru_map$source_group_name[ridx],
        sex_restriction = ru_map$sex_restriction[ridx],
        age_start = ru_map$age_start[ridx],
        age_end = ru_map$age_end[ridx],
        n_rule_hits = as.integer(length(ridx))
      )
    }
  }
  rule_candidates <- rbindlist(rule_candidates_list, use.names = TRUE, fill = TRUE)
  
  qc_multiple_rule_hits <- rule_candidates[n_rule_hits > 1]
  fwrite(qc_multiple_rule_hits, file.path(CFG$qc_dir, "qc_multiple_rule_hits.csv"))
  
  # ==========================================================
  # Elegir regla válida por sexo/edad
  # ==========================================================
  setkey(pre, icd10_ucod_nodot)
  setkey(rule_candidates, icd10_ucod_nodot)
  pre_rule_candidates <- rule_candidates[pre, allow.cartesian = TRUE]
  
  pre_rule_candidates[, rule_applies :=
                        !is.na(rule_id) &
                        mapply(sex_rule_applies, sex_id, sex_restriction) &
                        mapply(age_rule_applies, age, age_start, age_end)]
  
  pre_rule_candidates[, sex_specific := !is.na(sex_restriction)]
  pre_rule_candidates[, age_span := fifelse(
    !is.na(age_start) & !is.na(age_end), age_end - age_start,
    fifelse(!is.na(age_start) | !is.na(age_end), 1000L, 9999L)
  )]
  pre_rule_candidates[, rule_id_num := suppressWarnings(as.integer(gsub("[^0-9]", "", rule_id)))]
  
  applicable <- pre_rule_candidates[rule_applies == TRUE]
  
  setorderv(
    applicable,
    cols = c("year_id","location_id","sex_id","age","icd10_ucod_nodot","sex_specific","age_span","rule_id_num","rule_id"),
    order = c(1L,1L,1L,1L,1L,-1L,1L,1L,1L)
  )
  
  selected_rules <- applicable[, .SD[1], by = .(
    year_id, location_id, sex_id, age, icd10_ucod_nodot, icd10_ucod_dot
  )]
  
  # QC: conflictos reales de selección
  qc_rule_conflicts_selected <- applicable[, .(
    n_candidate_rules = .N,
    candidate_rule_ids = paste(unique(rule_id), collapse = ","),
    candidate_sex_restriction = paste(unique(ifelse(is.na(sex_restriction), "NA", sex_restriction)), collapse = ","),
    candidate_age_start = paste(unique(ifelse(is.na(age_start), "NA", as.character(age_start))), collapse = ","),
    candidate_age_end = paste(unique(ifelse(is.na(age_end), "NA", as.character(age_end))), collapse = ","),
    n_deaths = first(n_deaths)
  ), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, icd10_ucod_dot)][n_candidate_rules > 1]
  fwrite(qc_rule_conflicts_selected, file.path(CFG$qc_dir, "qc_rule_conflicts_selected.csv"))
  
  selected_rule_keys <- unique(
    selected_rules[, .(
      year_id, location_id, sex_id, age, icd10_ucod_nodot, icd10_ucod_dot,
      has_selected_rule = TRUE
    )]
  )
  
  pre2 <- merge(
    pre,
    selected_rule_keys,
    by = c("year_id","location_id","sex_id","age","icd10_ucod_nodot","icd10_ucod_dot"),
    all.x = TRUE,
    sort = FALSE
  )
  pre2[is.na(has_selected_rule), has_selected_rule := FALSE]
  
  setkey(causemap, icd10_ucod_nodot, icd10_ucod_dot)
  setkey(pre2, icd10_ucod_nodot, icd10_ucod_dot)
  x <- causemap[pre2]
  
  selected_rules_small <- selected_rules[, .(
    year_id, location_id, sex_id, age, icd10_ucod_nodot, icd10_ucod_dot,
    rule_id, source_group_code, source_group_name,
    sex_restriction, age_start, age_end
  )]
  
  x <- merge(
    x,
    selected_rules_small,
    by = c("year_id","location_id","sex_id","age","icd10_ucod_nodot","icd10_ucod_dot"),
    all.x = TRUE,
    sort = FALSE
  )

  mapped_demog_meta <- unique(cm[, .(
    mapped_cause_concept_id = cause_concept_id,
    mapped_expected_sex = sex_restriction_target_default,
    mapped_expected_age_start = target_age_start_default,
    mapped_expected_age_end = target_age_end_default
  )])
  x <- merge(
    x,
    mapped_demog_meta,
    by = "mapped_cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  if (nrow(demog_patch) > 0L) {
    demog_patch_active <- unique(demog_patch[active == 1L, .(
      mapped_cause_concept_id = cause_concept_id,
      apply_on_sex_incompatibility,
      apply_on_age_incompatibility,
      replacement_rule_id,
      handling_label,
      handling_note,
      method_source
    )])
    replacement_rule_meta <- unique(ru[, .(
      replacement_rule_id = rule_id,
      replacement_source_group_code = source_group_code,
      replacement_source_group_name = source_group_name
    )])
    demog_patch_active <- merge(
      demog_patch_active,
      replacement_rule_meta,
      by = "replacement_rule_id",
      all.x = TRUE,
      sort = FALSE
    )
    x <- merge(
      x,
      demog_patch_active,
      by = "mapped_cause_concept_id",
      all.x = TRUE,
      sort = FALSE
    )
  } else {
    x[, `:=`(
      apply_on_sex_incompatibility = NA_integer_,
      apply_on_age_incompatibility = NA_integer_,
      replacement_rule_id = NA_character_,
      handling_label = NA_character_,
      handling_note = NA_character_,
      method_source = NA_character_,
      replacement_source_group_code = NA_character_,
      replacement_source_group_name = NA_character_
    )]
  }
  
  specific_patch_meta <- data.table(
    patch_id = character(),
    handling_type = character(),
    specific_replacement_rule_id = character(),
    split_target_concept_ids = character(),
    fallback_target_weights = character(),
    dynamic_weight_strategy = character(),
    specific_handling_label = character(),
    specific_handling_note = character(),
    specific_method_source = character()
  )
  if (nrow(specific_icd_patch) > 0L) {
    specific_match_list <- vector("list", nrow(specific_icd_patch))
    for (i in seq_len(nrow(specific_icd_patch))) {
      rowi <- specific_icd_patch[i]
      hit <- tryCatch(grepl(rowi$icd10_regex, x$icd10_ucod_nodot, perl = TRUE), error = function(e) rep(FALSE, nrow(x)))
      hit <- hit &
        mapply(sex_rule_applies, x$sex_id, rowi$sex_restriction) &
        mapply(age_rule_applies, x$age, rowi$age_start, rowi$age_end)
      specific_match_list[[i]] <- data.table(
        row_id = which(hit),
        patch_id = rowi$patch_id
      )
    }
    specific_matches <- rbindlist(specific_match_list, use.names = TRUE, fill = TRUE)
    qc_specific_patch_conflicts <- specific_matches[, .N, by = row_id][N > 1L]
    fwrite(qc_specific_patch_conflicts, file.path(CFG$qc_dir, "qc_direct_specific_icd_patch_conflicts.csv"))
    if (nrow(qc_specific_patch_conflicts) > 0L) {
      stop("Un mismo registro coincide con más de un patch de manejo directo específico.")
    }
    if (nrow(specific_matches) > 0L) {
      specific_matches <- merge(
        specific_matches,
        specific_icd_patch[, .(
          patch_id,
          handling_type,
          specific_replacement_rule_id = replacement_rule_id,
          split_target_concept_ids,
          fallback_target_weights,
          dynamic_weight_strategy,
          specific_handling_label = handling_label,
          specific_handling_note = handling_note,
          specific_method_source = method_source
        )],
        by = "patch_id",
        all.x = TRUE,
        sort = FALSE
      )
      replacement_rule_meta2 <- unique(
        ru[, .(
          specific_replacement_rule_id = rule_id,
          replacement_source_group_code2 = source_group_code,
          replacement_source_group_name2 = source_group_name
        )],
        by = "specific_replacement_rule_id"
      )
      specific_matches <- merge(
        specific_matches,
        replacement_rule_meta2,
        by = "specific_replacement_rule_id",
        all.x = TRUE,
        sort = FALSE
      )
      x[, row_id_specific_patch := .I]
      x <- merge(
        x,
        specific_matches,
        by.x = "row_id_specific_patch",
        by.y = "row_id",
        all.x = TRUE,
        sort = FALSE
      )
      x[, row_id_specific_patch := NULL]
      specific_patch_meta <- unique(specific_matches[, .(
        patch_id,
        handling_type,
        specific_replacement_rule_id,
        split_target_concept_ids,
        fallback_target_weights,
        dynamic_weight_strategy,
        specific_handling_label,
        specific_handling_note,
        specific_method_source
      )])
    } else {
      x[, `:=`(
        patch_id = NA_character_,
        handling_type = NA_character_,
        specific_replacement_rule_id = NA_character_,
        split_target_concept_ids = NA_character_,
        fallback_target_weights = NA_character_,
        dynamic_weight_strategy = NA_character_,
        specific_handling_label = NA_character_,
        specific_handling_note = NA_character_,
        specific_method_source = NA_character_,
        replacement_source_group_code2 = NA_character_,
        replacement_source_group_name2 = NA_character_
      )]
    }
  } else {
    x[, `:=`(
      patch_id = NA_character_,
      handling_type = NA_character_,
      specific_replacement_rule_id = NA_character_,
      split_target_concept_ids = NA_character_,
      fallback_target_weights = NA_character_,
      dynamic_weight_strategy = NA_character_,
      specific_handling_label = NA_character_,
      specific_handling_note = NA_character_,
      specific_method_source = NA_character_,
      replacement_source_group_code2 = NA_character_,
      replacement_source_group_name2 = NA_character_
    )]
  }
  x[, direct_specific_icd_handle := !is.na(patch_id) & is.na(rule_id)]
  qc_direct_specific_icd_handling_detected <- x[
    direct_specific_icd_handle == TRUE,
    .(
      year_id, location_id, sex_id, age, icd10_ucod_nodot, n_deaths,
      original_cause_concept_id = mapped_cause_concept_id,
      original_cause_name = mapped_cause_name,
      patch_id,
      handling_type,
      specific_replacement_rule_id,
      replacement_source_group_name = replacement_source_group_name2,
      split_target_concept_ids,
      dynamic_weight_strategy,
      specific_handling_label,
      specific_handling_note,
      specific_method_source
    )
  ][order(-n_deaths, icd10_ucod_nodot, year_id, location_id, sex_id, age)]
  fwrite(
    qc_direct_specific_icd_handling_detected,
    file.path(CFG$qc_dir, "qc_direct_specific_icd_handling_detected.csv")
  )
  qc_direct_specific_icd_handling_summary <- qc_direct_specific_icd_handling_detected[
    ,
    .(deaths = sum(n_deaths), rows = .N),
    by = .(
      patch_id,
      icd10_ucod_nodot,
      handling_type,
      original_cause_concept_id,
      original_cause_name,
      specific_replacement_rule_id,
      replacement_source_group_name,
      split_target_concept_ids,
      dynamic_weight_strategy,
      specific_handling_label
    )
  ][order(-deaths, patch_id)]
  fwrite(
    qc_direct_specific_icd_handling_summary,
    file.path(CFG$qc_dir, "qc_direct_specific_icd_handling_summary.csv")
  )
  fwrite(
    qc_direct_specific_icd_handling_summary,
    file.path(CFG$methods_dir, "direct_specific_icd_handling_summary.csv")
  )
  x[direct_specific_icd_handle == TRUE & handling_type == "replace_with_rule", `:=`(
    rule_id = specific_replacement_rule_id,
    source_group_code = replacement_source_group_code2,
    source_group_name = replacement_source_group_name2
  )]
  
  specific_split_lookup <- data.table(
    patch_id = character(),
    target_order = integer(),
    target_term_concept_id = integer(),
    fallback_weight = numeric()
  )
  if (nrow(specific_patch_meta[handling_type == "dynamic_year_split"]) > 0L) {
    specific_split_lookup <- specific_patch_meta[handling_type == "dynamic_year_split", {
      ids <- split_int_csv(split_target_concept_ids)
      wts <- split_num_csv(fallback_target_weights)
      if (length(ids) == 0L) stop("Patch dynamic_year_split sin split_target_concept_ids.")
      if (length(wts) > 0L && length(wts) != length(ids)) {
        stop("Patch dynamic_year_split con fallback_target_weights de longitud distinta a split_target_concept_ids.")
      }
      if (length(wts) == 0L) wts <- rep(1 / length(ids), length(ids))
      .(
        target_order = seq_along(ids),
        target_term_concept_id = ids,
        fallback_weight = as.numeric(wts)
      )
    }, by = .(patch_id, dynamic_weight_strategy)]
    bad_split_targets <- specific_split_lookup[!target_term_concept_id %in% cm[is_terminal == TRUE, cause_concept_id]]
    fwrite(bad_split_targets, file.path(CFG$qc_dir, "qc_bad_direct_specific_icd_split_targets.csv"))
    if (nrow(bad_split_targets) > 0L) {
      stop("Patch dynamic_year_split referencia targets no terminales o inexistentes.")
    }
  }
  dynamic_weight_lookup <- data.table(
    patch_id = character(),
    year_id = integer(),
    target_term_concept_id = integer(),
    target_weight_dynamic = numeric()
  )
  if (nrow(specific_split_lookup[dynamic_weight_strategy == "uterine_cancer_specified_female_by_year"]) > 0L) {
    female_id <- 8532L
    uterus_spec <- dx[
      sex_id == female_id & icd10_ucod_nodot %in% c("C53", "C530", "C531", "C538", "C539", "C54", "C540", "C541", "C542", "C543", "C548", "C549"),
      .(n = .N),
      by = .(year_id, stem = substr(icd10_ucod_nodot, 1L, 3L))
    ]
    uterus_spec_wide <- dcast(uterus_spec, year_id ~ stem, value.var = "n", fill = 0)
    if (!"C53" %in% names(uterus_spec_wide)) uterus_spec_wide[, C53 := 0]
    if (!"C54" %in% names(uterus_spec_wide)) uterus_spec_wide[, C54 := 0]
    uterus_spec_wide[, total := C53 + C54]
    uterus_spec_wide[, `:=`(
      share_c53 = fifelse(total > 0, C53 / total, NA_real_),
      share_c54 = fifelse(total > 0, C54 / total, NA_real_)
    )]
    overall_c53 <- sum(uterus_spec_wide$C53, na.rm = TRUE)
    overall_c54 <- sum(uterus_spec_wide$C54, na.rm = TRUE)
    overall_total <- overall_c53 + overall_c54
    fallback_c53 <- fifelse(overall_total > 0, overall_c53 / overall_total, 0.5)
    fallback_c54 <- fifelse(overall_total > 0, overall_c54 / overall_total, 0.5)
    uterus_lookup <- rbindlist(list(
      uterus_spec_wide[, .(year_id, target_term_concept_id = 9000710L, target_weight_dynamic = fifelse(is.na(share_c53), fallback_c53, share_c53))],
      uterus_spec_wide[, .(year_id, target_term_concept_id = 9000720L, target_weight_dynamic = fifelse(is.na(share_c54), fallback_c54, share_c54))]
    ), use.names = TRUE, fill = TRUE)
    uterine_patch_ids <- unique(specific_split_lookup[dynamic_weight_strategy == "uterine_cancer_specified_female_by_year", patch_id])
    dynamic_weight_lookup <- rbindlist(lapply(uterine_patch_ids, function(pid) {
      copy(uterus_lookup)[, patch_id := pid]
    }), use.names = TRUE, fill = TRUE)
    qc_c55_dynamic_split_weights_by_year <- dcast(
      uterus_lookup,
      year_id ~ target_term_concept_id,
      value.var = "target_weight_dynamic"
    )
    setnames(qc_c55_dynamic_split_weights_by_year, c("9000710", "9000720"), c("weight_c53_cervix", "weight_c54_corpus"), skip_absent = TRUE)
    fwrite(
      qc_c55_dynamic_split_weights_by_year,
      file.path(CFG$qc_dir, "qc_c55_dynamic_split_weights_by_year.csv")
    )
    fwrite(
      qc_c55_dynamic_split_weights_by_year,
      file.path(CFG$methods_dir, "c55_dynamic_split_weights_by_year.csv")
    )
  }
  x[, direct_sex_incompatibility := !is.na(mapped_expected_sex) &
      mapped_expected_sex != "" &
      !mapply(sex_rule_applies, sex_id, mapped_expected_sex)]
  x[, direct_age_incompatibility := (
    (!is.na(mapped_expected_age_start) & age < mapped_expected_age_start) |
      (!is.na(mapped_expected_age_end) & age > mapped_expected_age_end)
  )]
  x[, direct_demographic_incompatibility_type := fifelse(
    direct_sex_incompatibility & direct_age_incompatibility, "sex_age_incompatible",
    fifelse(direct_sex_incompatibility, "sex_incompatible",
      fifelse(direct_age_incompatibility, "age_incompatible", NA_character_))
  )]
  x[, direct_demographic_incompatibility_handle := (
    is.na(rule_id) &
      !is.na(mapped_cause_concept_id) &
      !is.na(replacement_rule_id) &
      ((apply_on_sex_incompatibility == 1L & direct_sex_incompatibility) |
         (apply_on_age_incompatibility == 1L & direct_age_incompatibility))
  )]
  qc_direct_demographic_incompatibility_detected <- x[
    direct_demographic_incompatibility_handle == TRUE,
    .(
      year_id, location_id, sex_id, age, icd10_ucod_nodot, n_deaths,
      original_cause_concept_id = mapped_cause_concept_id,
      original_cause_name = mapped_cause_name,
      mapped_expected_sex,
      mapped_expected_age_start,
      mapped_expected_age_end,
      direct_demographic_incompatibility_type,
      replacement_rule_id,
      replacement_source_group_code,
      replacement_source_group_name,
      handling_label,
      handling_note,
      method_source
    )
  ][order(-n_deaths, original_cause_name, year_id, location_id, sex_id, age)]
  fwrite(
    qc_direct_demographic_incompatibility_detected,
    file.path(CFG$qc_dir, "qc_direct_demographic_incompatibility_detected.csv")
  )
  qc_direct_demographic_incompatibility_summary <- qc_direct_demographic_incompatibility_detected[
    ,
    .(deaths = sum(n_deaths), rows = .N),
    by = .(
      direct_demographic_incompatibility_type,
      original_cause_concept_id,
      original_cause_name,
      replacement_rule_id,
      replacement_source_group_name,
      handling_label
    )
  ][order(-deaths, original_cause_name)]
  fwrite(
    qc_direct_demographic_incompatibility_summary,
    file.path(CFG$qc_dir, "qc_direct_demographic_incompatibility_summary.csv")
  )
  fwrite(
    qc_direct_demographic_incompatibility_summary,
    file.path(CFG$methods_dir, "direct_demographic_incompatibility_handling_summary.csv")
  )
  x[direct_demographic_incompatibility_handle == TRUE, `:=`(
    rule_id = replacement_rule_id,
    source_group_code = replacement_source_group_code,
    source_group_name = replacement_source_group_name
  )]
  
  qc_rule_candidates_not_applied <- pre_rule_candidates[
    !rule_applies & !is.na(rule_id),
    .(n = .N),
    by = .(icd10_ucod_nodot, rule_id, sex_restriction, age_start, age_end)
  ][order(-n)]
  fwrite(qc_rule_candidates_not_applied, file.path(CFG$qc_dir, "qc_rule_candidates_not_applied.csv"))
  
  # ==========================================================
  # Targets finales
  # ==========================================================
  targets <- unique(
    ru[, .(
      rule_id,
      target_term_concept_id = target_cause_concept_id,
      target_weight,
      sex_restriction,
      target_sex_restriction_cm,
      target_age_start_cm,
      target_age_end_cm,
      age_start,
      age_end
    )]
  )
  
  # ==========================================================
  # Construcción leaf
  # ==========================================================
  x[, is_garbage_code := !is.na(rule_id)]
  
  dynamic_specific_base <- x[direct_specific_icd_handle == TRUE & handling_type == "dynamic_year_split"]
  dynamic_specific_exp <- data.table(
    year_id = integer(), location_id = integer(), sex_id = integer(), age = integer(),
    cause_term_concept_id = integer(), deaths = numeric()
  )
  if (nrow(dynamic_specific_base) > 0L) {
    dynamic_specific_targets <- merge(
      dynamic_specific_base[, .(
        year_id, location_id, sex_id, age, icd10_ucod_nodot, n_deaths, patch_id,
        split_target_concept_ids, fallback_target_weights
      )],
      specific_split_lookup,
      by = "patch_id",
      all.x = TRUE,
      allow.cartesian = TRUE,
      sort = FALSE
    )
    dynamic_specific_targets <- merge(
      dynamic_specific_targets,
      dynamic_weight_lookup,
      by = c("patch_id", "year_id", "target_term_concept_id"),
      all.x = TRUE,
      sort = FALSE
    )
    dynamic_specific_targets[is.na(target_weight_dynamic), target_weight_dynamic := fallback_weight]
    dynamic_specific_targets[, target_weight_final := target_weight_dynamic / sum(target_weight_dynamic),
                             by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, patch_id)]
    qc_direct_specific_bad_weight <- dynamic_specific_targets[
      ,
      .(sum_target_weight_final = sum(target_weight_final)),
      by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, patch_id)
    ][abs(sum_target_weight_final - 1) > 1e-10]
    fwrite(
      qc_direct_specific_bad_weight,
      file.path(CFG$qc_dir, "qc_direct_specific_icd_bad_weight.csv")
    )
    if (nrow(qc_direct_specific_bad_weight) > 0L) {
      stop("El split dinámico de manejo directo específico no suma 1.")
    }
    dynamic_specific_targets[, deaths := as.numeric(n_deaths) * target_weight_final]
    dynamic_specific_exp <- dynamic_specific_targets[, .(
      year_id, location_id, sex_id, age,
      cause_term_concept_id = target_term_concept_id,
      deaths
    )]
  }
  
  lost_unmapped_non_gc <- x[is_garbage_code == FALSE & is.na(mapped_cause_concept_id) & !direct_specific_icd_handle, sum(n_deaths)]
  if (is.na(lost_unmapped_non_gc)) lost_unmapped_non_gc <- 0
  
  x[is_garbage_code == FALSE & is.na(mapped_cause_concept_id) & !direct_specific_icd_handle,
    mapped_cause_concept_id := CFG$unmapped_non_gc_term_id]
  
  non_gc <- x[is_garbage_code == FALSE & !direct_specific_icd_handle, .(
    year_id, location_id, sex_id, age,
    cause_term_concept_id = mapped_cause_concept_id,
    deaths = as.numeric(n_deaths)
  )]
  
  # ==========================================================
  # Reclasificar causas claramente no letales detectadas como UCOD
  # para que no entren al flujo de mortalidad específica
  # ==========================================================
  nonfatal_hard_exclude_ids <- c(
    9000580L, # Deficiencia de hierro en la dieta
    9001280L, # Hiperplasia prostática benigna
    9001360L, # Osteoartritis
    9001380L, # Dolor de espalda y cuello
    9001370L, # Gota
    9000880L, # Trastornos de ansiedad
    9000831L, # Trastorno depresivo mayor
    9000832L, # Distimia
    9000840L, # Trastorno bipolar
    9000850L, # Esquizofrenia
    9000900L, # Síndrome de autismo y Asperger
    9000911L, # Trastorno por déficit de atención/hiperactividad
    9000912L, # Trastorno de la conducta
    9000920L, # Discapacidad intelectual idiopática
    9000990L, # Migraña
    9001000L, # Cefalea no migrañosa
    9001020L, # Enfermedades de los órganos de los sentidos
    9001070L, # Otras pérdidas de visión
    9001090L, # Otros trastornos de los órganos sensoriales
    9001490L, # Enfermedad periodontal
    9001502L, # Otros trastornos orales
    9000270L, # Filariasis linfática
    9000362L, # Trematodos transmitidos por alimentos
    9000365L, # Lepra
    9000560L  # Deficiencia de yodo
  )
  
  qc_nonfatal_direct_ucod_reassigned <- non_gc[
    cause_term_concept_id %in% nonfatal_hard_exclude_ids
  ]
  
  fwrite(
    qc_nonfatal_direct_ucod_reassigned,
    file.path(CFG$qc_dir, "qc_nonfatal_direct_ucod_reassigned.csv")
  )

  qc_nonfatal_direct_ucod_summary <- qc_nonfatal_direct_ucod_reassigned[, .(
    deaths = sum(deaths),
    rows = .N
  ), by = .(year_id, cause_term_concept_id)][order(year_id, cause_term_concept_id)]
  fwrite(
    qc_nonfatal_direct_ucod_summary,
    file.path(CFG$qc_dir, "qc_nonfatal_direct_ucod_summary.csv")
  )

  excluded_nonfatal_direct_ucod <- non_gc[
    cause_term_concept_id %in% nonfatal_hard_exclude_ids,
    sum(deaths)
  ]
  if (is.na(excluded_nonfatal_direct_ucod)) excluded_nonfatal_direct_ucod <- 0

  # Estas causas yld-only no deben redistribuirse ni entrar al universo
  # de mortalidad específica final. Se excluyen con trazabilidad explícita.
  non_gc <- non_gc[!cause_term_concept_id %in% nonfatal_hard_exclude_ids]
  
  gc_base <- x[is_garbage_code == TRUE, .(
    year_id, location_id, sex_id, age,
    icd10_ucod_nodot, n_deaths, rule_id
  )]
  
  setkey(gc_base, rule_id)
  setkey(targets, rule_id)
  gc_with_targets <- targets[gc_base, allow.cartesian = TRUE]
  
  # gc_with_targets[, target_applies :=
  #                   mapply(sex_rule_applies, sex_id, sex_restriction) &
  #                   mapply(age_rule_applies, age, age_start, age_end)]
  # 
  # gc_with_targets <- gc_with_targets[target_applies == TRUE]
  # 
  # # Auditoría: garbage con targets válidos pero sin expansión final
  # qc_gc_with_targets_pre <- gc_with_targets[, .(
  #   n_rows = .N,
  #   deaths_input = sum(n_deaths)
  # ), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, rule_id)]
  # fwrite(qc_gc_with_targets_pre, file.path(CFG$qc_dir, "qc_gc_with_targets_pre.csv"))
  # 
  # if (nrow(gc_with_targets) > 0L) {
  #   gc_with_targets[, deaths := as.numeric(n_deaths) * target_weight]
  #   gc_exp <- gc_with_targets[, .(
  #     year_id, location_id, sex_id, age,
  #     cause_term_concept_id = target_term_concept_id,
  #     deaths
  #   )]
  # } else {
  #   gc_exp <- data.table(
  #     year_id = integer(), location_id = integer(), sex_id = integer(), age = integer(),
  #     cause_term_concept_id = integer(), deaths = numeric()
  #   )
  # }
  
  gc_with_targets[, target_applies :=
                    mapply(sex_rule_applies, sex_id, sex_restriction) &
                    mapply(sex_rule_applies, sex_id, target_sex_restriction_cm) &
                    mapply(age_rule_applies, age, age_start, age_end) &
                    mapply(age_rule_applies, age, target_age_start_cm, target_age_end_cm)]
  
  gc_with_targets <- gc_with_targets[target_applies == TRUE]
  
  # Renormalizar pesos DESPUÉS del filtro por sexo/edad
  if (nrow(gc_with_targets) > 0L) {
    gc_with_targets[, target_weight_filtered_sum := sum(target_weight),
                    by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, rule_id)]
    
    gc_with_targets[, target_weight_final := fifelse(
      target_weight_filtered_sum > 0,
      target_weight / target_weight_filtered_sum,
      NA_real_
    )]
    
    # QC: verificar que ahora sumen 1 dentro de cada grupo garbage
    qc_gc_target_weight_postfilter <- gc_with_targets[, .(
      sum_target_weight_original = sum(target_weight),
      sum_target_weight_final = sum(target_weight_final),
      n_targets_after_filter = .N
    ), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, rule_id)]
    
    fwrite(
      qc_gc_target_weight_postfilter,
      file.path(CFG$qc_dir, "qc_gc_target_weight_postfilter.csv")
    )
    
    qc_gc_bad_weight_postfilter <- qc_gc_target_weight_postfilter[
      abs(sum_target_weight_final - 1) > 1e-10
    ]
    
    fwrite(
      qc_gc_bad_weight_postfilter,
      file.path(CFG$qc_dir, "qc_gc_bad_weight_postfilter.csv")
    )
    
    # Auditoría: garbage con targets válidos ya renormalizados
    qc_gc_with_targets_pre <- gc_with_targets[, .(
      n_rows = .N,
      deaths_input = first(n_deaths),
      sum_target_weight_original = sum(target_weight),
      sum_target_weight_final = sum(target_weight_final)
    ), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, rule_id)]
    
    fwrite(
      qc_gc_with_targets_pre,
      file.path(CFG$qc_dir, "qc_gc_with_targets_pre.csv")
    )
    
    gc_with_targets[, deaths := as.numeric(n_deaths) * target_weight_final]
    
    gc_exp <- gc_with_targets[, .(
      year_id, location_id, sex_id, age,
      cause_term_concept_id = target_term_concept_id,
      deaths
    )]
  } else {
    gc_exp <- data.table(
      year_id = integer(), location_id = integer(), sex_id = integer(), age = integer(),
      cause_term_concept_id = integer(), deaths = numeric()
    )
  }
  
  gc_rules_with_targets <- unique(
    gc_with_targets[, .(year_id, location_id, sex_id, age, icd10_ucod_nodot, rule_id, has_targets = TRUE)]
  )
  
  gc_base2 <- merge(
    gc_base,
    gc_rules_with_targets,
    by = c("year_id","location_id","sex_id","age","icd10_ucod_nodot","rule_id"),
    all.x = TRUE,
    sort = FALSE
  )
  gc_base2[is.na(has_targets), has_targets := FALSE]
  
  lost_gc_no_targets <- gc_base2[has_targets == FALSE, sum(n_deaths)]
  
  gc_no_targets <- gc_base2[has_targets == FALSE, .(
    year_id, location_id, sex_id, age,
    cause_term_concept_id = CFG$gc_no_target_term_id,
    deaths = as.numeric(n_deaths)
  )]
  
  # Auditoría: garbage con targets válidos que no aparecen ni en gc_exp
  gc_exp_audit <- gc_exp[, .(deaths_output = sum(deaths)),
                         by = .(year_id, location_id, sex_id, age)]
  gc_base_targets_audit <- gc_base2[has_targets == TRUE, .(deaths_input = sum(n_deaths)),
                                    by = .(year_id, location_id, sex_id, age)]
  qc_gc_balance_by_group <- merge(
    gc_base_targets_audit,
    gc_exp_audit,
    by = c("year_id","location_id","sex_id","age"),
    all = TRUE,
    sort = FALSE
  )
  qc_gc_balance_by_group[is.na(deaths_input), deaths_input := 0]
  qc_gc_balance_by_group[is.na(deaths_output), deaths_output := 0]
  qc_gc_balance_by_group[, delta := deaths_output - deaths_input]
  fwrite(qc_gc_balance_by_group, file.path(CFG$qc_dir, "qc_gc_balance_by_group.csv"))
  
  qc_gc_base_without_output <- qc_gc_balance_by_group[abs(delta) > 1e-10]
  fwrite(qc_gc_base_without_output, file.path(CFG$qc_dir, "qc_gc_base_without_output.csv"))
  
  if (identical(CFG$redis_mode, "no_redistribution_delete_gc")) {
    msg("Modo sensibilidad activo: no_redistribution_delete_gc. Se elimina la masa garbage en lugar de redistribuirla.")
    leaf <- rbindlist(list(non_gc, dynamic_specific_exp), use.names = TRUE, fill = TRUE)
  } else {
    leaf <- rbindlist(list(non_gc, gc_exp, gc_no_targets, dynamic_specific_exp), use.names = TRUE, fill = TRUE)
  }
  leaf <- leaf[, .(deaths = sum(deaths)),
               by = .(year_id, location_id, sex_id, age, cause_term_concept_id)]
  
  if (leaf[deaths < 0, .N] > 0L) stop("ERROR: deaths negativas detectadas en leaf.")
  
  # ==========================================================
  # QC: no deben quedar muertes en causas yld-only / no-YLL
  # ==========================================================
  qc_yld_only_targets_after <- merge(
    leaf,
    cm[, .(cause_term_concept_id = cause_concept_id, cause_name, yll_flag)],
    by = "cause_term_concept_id",
    all.x = TRUE,
    sort = FALSE
  )[!is.na(yll_flag) & yll_flag == 0L & deaths > 0]
  
  fwrite(
    qc_yld_only_targets_after,
    file.path(CFG$qc_dir, "qc_yld_only_targets_after.csv")
  )
  
  qc_yld_only_targets_summary <- qc_yld_only_targets_after[, .(
    deaths = sum(deaths)
  ), by = .(cause_term_concept_id, cause_name, yll_flag)][order(-deaths)]
  
  fwrite(
    qc_yld_only_targets_summary,
    file.path(CFG$qc_dir, "qc_yld_only_targets_summary.csv")
  )
  
  # Permitir solo residuos muy puntuales si luego decides revisarlos manualmente
  allowed_remaining_nonfatal_ids <- integer()
  
  qc_yld_only_targets_after_hard <- qc_yld_only_targets_after[
    !cause_term_concept_id %in% allowed_remaining_nonfatal_ids
  ]
  
  fwrite(
    qc_yld_only_targets_after_hard,
    file.path(CFG$qc_dir, "qc_yld_only_targets_after_hard.csv")
  )
  
  if (nrow(qc_yld_only_targets_after_hard) > 0L) {
    stop("Persisten muertes en causas no letales tras la reclasificación. Revisar qc_yld_only_targets_after_hard.csv")
  }

  qc_sex_specific_cause_by_observed_sex <- merge(
    leaf,
    unique(cm[
      !is.na(sex_restriction_target_default) & sex_restriction_target_default != "",
      .(
        cause_term_concept_id = cause_concept_id,
        cause_name,
        expected_sex = sex_restriction_target_default
      )
    ]),
    by = "cause_term_concept_id",
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(deaths = sum(deaths, na.rm = TRUE)),
    by = .(cause_term_concept_id, cause_name, expected_sex, sex_id)
  ]
  qc_sex_specific_cause_by_observed_sex[
    ,
    observed_sex := fifelse(
      sex_id == 8507L, "male",
      fifelse(sex_id == 8532L, "female", "other")
    )
  ]
  qc_sex_specific_cause_by_observed_sex[, sex_mismatch := observed_sex != expected_sex]
  setorder(qc_sex_specific_cause_by_observed_sex, cause_name, sex_id)
  fwrite(
    qc_sex_specific_cause_by_observed_sex,
    file.path(CFG$qc_dir, "qc_sex_specific_cause_by_observed_sex.csv")
  )

  qc_sex_specific_mismatch_positive <- qc_sex_specific_cause_by_observed_sex[
    sex_mismatch == TRUE & deaths > 1e-10
  ][order(-deaths, cause_name, sex_id)]
  fwrite(
    qc_sex_specific_mismatch_positive,
    file.path(CFG$qc_dir, "qc_sex_specific_mismatch_positive.csv")
  )

  if (nrow(qc_sex_specific_mismatch_positive) > 0L) {
    msg(
      "ADVERTENCIA QC: persisten muertes en causas sexo-específicas incompatibles tras la redistribución. ",
      "Revisar qc_sex_specific_mismatch_positive.csv. ",
      "Estas filas pueden reflejar codificación fuente directa y no necesariamente ",
      "una fuga del motor de redistribución."
    )
  }

  qc_age_specific_cause_by_observed_age <- merge(
    leaf,
    unique(cm[
      !is.na(target_age_start_default) | !is.na(target_age_end_default),
      .(
        cause_term_concept_id = cause_concept_id,
        cause_name,
        expected_age_start = target_age_start_default,
        expected_age_end = target_age_end_default
      )
    ]),
    by = "cause_term_concept_id",
    all = FALSE,
    sort = FALSE
  )[
    ,
    .(deaths = sum(deaths, na.rm = TRUE)),
    by = .(cause_term_concept_id, cause_name, expected_age_start, expected_age_end, age)
  ]
  qc_age_specific_cause_by_observed_age[
    ,
    age_mismatch := (!is.na(expected_age_start) & age < expected_age_start) |
      (!is.na(expected_age_end) & age > expected_age_end)
  ]
  setorder(qc_age_specific_cause_by_observed_age, cause_name, age)
  fwrite(
    qc_age_specific_cause_by_observed_age,
    file.path(CFG$qc_dir, "qc_age_specific_cause_by_observed_age.csv")
  )

  qc_age_specific_mismatch_positive <- qc_age_specific_cause_by_observed_age[
    age_mismatch == TRUE & deaths > 1e-10
  ][order(-deaths, cause_name, age)]
  fwrite(
    qc_age_specific_mismatch_positive,
    file.path(CFG$qc_dir, "qc_age_specific_mismatch_positive.csv")
  )

  if (nrow(qc_age_specific_mismatch_positive) > 0L) {
    msg(
      "ADVERTENCIA QC: persisten muertes en causas con dominio etario incompatible tras la redistribución. ",
      "Revisar qc_age_specific_mismatch_positive.csv. ",
      "Estas filas pueden reflejar codificación fuente directa y no necesariamente ",
      "una fuga del motor de redistribución."
    )
  }
  
  leaf[, `:=`(
    run_id = run_id,
    redistribution_version = CFG$redistribution_version,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )]
  
  # ==========================================================
  # Conservación por bloque
  # ==========================================================
  qc_balance_blocks <- data.table(
    block = c("non_garbage", "garbage", "excluded_nonfatal_direct_ucod", "total_mortality_eligible"),
    deaths_pre = c(
      x[is_garbage_code == FALSE, sum(n_deaths)] - excluded_nonfatal_direct_ucod,
      x[is_garbage_code == TRUE, sum(n_deaths)],
      excluded_nonfatal_direct_ucod,
      x[, sum(n_deaths)] - excluded_nonfatal_direct_ucod
    ),
    deaths_post = c(
      non_gc[, sum(deaths)],
      if (identical(CFG$redis_mode, "no_redistribution_delete_gc")) 0 else gc_exp[, sum(deaths)] + gc_no_targets[, sum(deaths)],
      0,
      leaf[, sum(deaths)]
    )
  )
  qc_balance_blocks[, delta := deaths_post - deaths_pre]
  fwrite(qc_balance_blocks, file.path(CFG$qc_dir, "qc_balance_blocks.csv"))
  
  qc_sensitivity_gc_deleted <- gc_base2[, .(
    deaths_garbage_input = sum(n_deaths, na.rm = TRUE),
    deaths_with_targets = sum(fifelse(has_targets == TRUE, n_deaths, 0), na.rm = TRUE),
    deaths_without_targets = sum(fifelse(has_targets == FALSE, n_deaths, 0), na.rm = TRUE)
  ), by = .(year_id, sex_id, rule_id, icd10_ucod_nodot)]
  qc_sensitivity_gc_deleted[, redis_mode := CFG$redis_mode]
  fwrite(qc_sensitivity_gc_deleted, file.path(CFG$qc_dir, "qc_sensitivity_garbage_deleted.csv"))
  
  # ==========================================================
  # Roll-ups
  # ==========================================================
  get_path_levels_one <- function(term_id, cm_dt) {
    cur <- term_id
    ids <- integer()
    lvls <- integer()
    
    repeat {
      row <- cm_dt[cause_concept_id == cur]
      if (nrow(row) == 0L) break
      ids <- c(ids, cur)
      lvls <- c(lvls, row$cause_level[1])
      par <- row$parent_concept_id[1]
      if (is.na(par)) break
      cur <- par
    }
    
    list(ids = ids, lvls = lvls)
  }
  
  term_ids_pos <- unique(leaf[cause_term_concept_id > 0, cause_term_concept_id])
  
  path_list <- lapply(term_ids_pos, function(tid) {
    p <- get_path_levels_one(tid, cm_all)
    data.table(
      cause_term_concept_id = tid,
      ancestor_id = p$ids,
      ancestor_level = p$lvls
    )
  })
  paths <- rbindlist(path_list, use.names = TRUE, fill = TRUE)
  
  leaf_pos <- leaf[cause_term_concept_id > 0]
  
  setkey(leaf_pos, cause_term_concept_id)
  setkey(paths, cause_term_concept_id)
  leaf_j <- paths[leaf_pos, allow.cartesian = TRUE]
  
  l3 <- leaf_j[ancestor_level == 3,
               .(deaths = sum(deaths)),
               by = .(year_id, location_id, sex_id, age, cause_concept_id = ancestor_id)]
  
  l2 <- leaf_j[ancestor_level == 2,
               .(deaths = sum(deaths)),
               by = .(year_id, location_id, sex_id, age, cause_concept_id = ancestor_id)]
  
  l1 <- leaf_j[ancestor_level == 1,
               .(deaths = sum(deaths)),
               by = .(year_id, location_id, sex_id, age, cause_concept_id = ancestor_id)]
  
  # ==========================================================
  # QC resumen
  # ==========================================================
  qc_summary <- data.table(
    metric = c(
      "n_rows_pre",
      "n_rows_leaf",
      "total_deaths_pre_raw",
      "excluded_nonfatal_direct_ucod",
      "total_deaths_pre_mortality_eligible",
      "total_deaths_post",
      "delta_pre_post",
      "n_unique_icd10",
      "n_garbage_rows_pre",
      "n_non_garbage_mapped_any_level_rows_pre",
      "n_non_garbage_unmapped_rows_pre",
      "n_pandemic_explicit_rows_pre",
      "n_covid_pre2020_rows_pre",
      "lost_unmapped_non_gc",
      "lost_gc_no_targets",
      "n_direct_demographic_incompatibility_handled",
      "deaths_direct_demographic_incompatibility_handled",
      "n_direct_specific_icd_handled",
      "deaths_direct_specific_icd_handled",
      "n_sex_specific_mismatch_positive",
      "n_age_specific_mismatch_positive",
      "n_codes_multiple_rule_hits",
      "n_codes_multiple_cause_hits"
    ),
    value = c(
      nrow(pre),
      nrow(leaf),
      pre[, sum(n_deaths)],
      excluded_nonfatal_direct_ucod,
      pre[, sum(n_deaths)] - excluded_nonfatal_direct_ucod,
      leaf[, sum(deaths)],
      leaf[, sum(deaths)] - (pre[, sum(n_deaths)] - excluded_nonfatal_direct_ucod),
      uniqueN(codes$icd10_ucod_nodot),
      x[is_garbage_code == TRUE, sum(n_deaths)],
      x[is_garbage_code == FALSE & !is.na(mapped_cause_concept_id), sum(n_deaths)] - excluded_nonfatal_direct_ucod,
      lost_unmapped_non_gc,
      dx[!is.na(covid_code_family), .N],
      bad_covid_pre2020,
      lost_unmapped_non_gc,
      lost_gc_no_targets,
      nrow(qc_direct_demographic_incompatibility_detected),
      qc_direct_demographic_incompatibility_detected[, sum(n_deaths)],
      nrow(qc_direct_specific_icd_handling_detected),
      qc_direct_specific_icd_handling_detected[, sum(n_deaths)],
      nrow(qc_sex_specific_mismatch_positive),
      nrow(qc_age_specific_mismatch_positive),
      nrow(qc_multiple_rule_hits),
      nrow(qc_multiple_cause_hits)
    )
  )
  
  qc_mapping_status <- data.table(
    mapping_status_pre = c(
      "non_garbage_unmapped",
      "garbage_rule_matched",
      "non_garbage_mapped_any_level",
      "excluded_nonfatal_direct_ucod",
      "handled_direct_demographic_incompatibility",
      "handled_direct_specific_icd"
    ),
    deaths = c(
      lost_unmapped_non_gc,
      x[is_garbage_code == TRUE, sum(n_deaths)],
      x[is_garbage_code == FALSE, sum(n_deaths)] - lost_unmapped_non_gc - excluded_nonfatal_direct_ucod,
      excluded_nonfatal_direct_ucod,
      qc_direct_demographic_incompatibility_detected[, sum(n_deaths)],
      qc_direct_specific_icd_handling_detected[, sum(n_deaths)]
    )
  )
  
  qc_top_unmapped_non_garbage <- x[
    is_garbage_code == FALSE & is.na(mapped_cause_concept_id),
    .(deaths = sum(n_deaths)),
    by = .(icd10_ucod_nodot, icd10_ucod_dot)
  ][order(-deaths)][1:min(.N, 500)]
  
  if (nrow(qc_top_unmapped_non_garbage) == 0L) {
    qc_top_unmapped_non_garbage <- data.table(
      icd10_ucod_nodot = character(),
      icd10_ucod_dot = character(),
      deaths = numeric()
    )
  }
  
  qc_top_garbage_groups <- x[
    is_garbage_code == TRUE,
    .(deaths = sum(n_deaths)),
    by = .(rule_id, source_group_code, source_group_name)
  ][order(-deaths)][1:min(.N, 500)]
  
  qc_bad_targets_no_terminal_descendants <- data.table(
    n_bad_targets = 0L
  )
  
  # ==========================================================
  # Diccionario
  # ==========================================================
  dict_leaf <- data.table(
    variable = names(leaf),
    label = c(
      "Año calendario",
      "Location ID armonizado",
      "Sexo OMOP-like",
      "Edad simple",
      "Concept ID leaf post-redistribución",
      "Número de muertes",
      "Run ID",
      "Versión de redistribución",
      "Fecha-hora de creación"
    ),
    tipo = vapply(leaf, function(x) class(x)[1], character(1)),
    n = nrow(leaf),
    n_missing = vapply(leaf, function(x) sum(is.na(x)), integer(1)),
    n_distinct = vapply(leaf, function(x) uniqueN(x), integer(1)),
    example_values = vapply(leaf, function(x) {
      vals <- unique(na.omit(as.character(x)))
      paste(head(vals, 5), collapse = " | ")
    }, character(1))
  )
  
  # ==========================================================
  # Export principales
  # ==========================================================
  out_leaf_csv     <- file.path(CFG$out_dir_leaf, paste0(CFG$table_name_leaf, ".csv"))
  out_leaf_parquet <- file.path(CFG$out_dir_leaf, paste0(CFG$table_name_leaf, ".parquet"))
  out_leaf_dict    <- file.path(CFG$out_dir_leaf, paste0(CFG$table_name_leaf, "_dictionary_ext.csv"))
  
  out_l3_csv <- file.path(CFG$out_dir_l3, paste0(CFG$table_name_l3, ".csv"))
  out_l2_csv <- file.path(CFG$out_dir_l2, paste0(CFG$table_name_l2, ".csv"))
  out_l1_csv <- file.path(CFG$out_dir_l1, paste0(CFG$table_name_l1, ".csv"))
  
  write_csv_parquet(leaf, csv_path = out_leaf_csv, parquet_path = out_leaf_parquet)
  fwrite(dict_leaf, out_leaf_dict)
  fwrite(l3, out_l3_csv)
  fwrite(l2, out_l2_csv)
  fwrite(l1, out_l1_csv)
  
  # ==========================================================
  # Export QC
  # ==========================================================
  qc_summary_path                   <- file.path(CFG$qc_dir, "qc_summary.csv")
  qc_mapping_status_path            <- file.path(CFG$qc_dir, "qc_mapping_status.csv")
  qc_top_unmapped_non_garbage_path  <- file.path(CFG$qc_dir, "qc_top_unmapped_non_garbage.csv")
  qc_top_garbage_groups_path        <- file.path(CFG$qc_dir, "qc_top_garbage_groups.csv")
  qc_bad_targets_path               <- file.path(CFG$qc_dir, "qc_bad_targets_no_terminal_descendants.csv")
  qc_pandemic_pre2020_examples_path <- file.path(CFG$qc_dir, "qc_pandemic_pre2020_examples.csv")
  
  fwrite(qc_summary, qc_summary_path)
  fwrite(qc_mapping_status, qc_mapping_status_path)
  fwrite(qc_top_unmapped_non_garbage, qc_top_unmapped_non_garbage_path)
  fwrite(qc_top_garbage_groups, qc_top_garbage_groups_path)
  fwrite(qc_bad_targets_no_terminal_descendants, qc_bad_targets_path)
  fwrite(qc_pandemic_pre2020_examples, qc_pandemic_pre2020_examples_path)
  
  # ==========================================================
  # Provenance
  # ==========================================================
  register_artifact(
    dataset_id = CFG$dataset_id_leaf,
    table_name = CFG$table_name_leaf,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_leaf_csv,
    n_rows = nrow(leaf),
    n_cols = ncol(leaf),
    notes = "CSV final leaf post-redistribution"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id_leaf,
    table_name = CFG$table_name_leaf,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_leaf_parquet,
    n_rows = nrow(leaf),
    n_cols = ncol(leaf),
    notes = "Parquet final leaf post-redistribution"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id_leaf,
    table_name = CFG$table_name_leaf,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "dictionary_ext",
    artifact_path = out_leaf_dict,
    n_rows = nrow(dict_leaf),
    n_cols = ncol(dict_leaf),
    notes = "Diccionario extendido leaf post-redistribution"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id_l3,
    table_name = CFG$table_name_l3,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_l3_csv,
    n_rows = nrow(l3),
    n_cols = ncol(l3),
    notes = "Roll-up L3"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id_l2,
    table_name = CFG$table_name_l2,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_l2_csv,
    n_rows = nrow(l2),
    n_cols = ncol(l2),
    notes = "Roll-up L2"
  )
  
  register_artifact(
    dataset_id = CFG$dataset_id_l1,
    table_name = CFG$table_name_l1,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "final_dataset",
    artifact_path = out_l1_csv,
    n_rows = nrow(l1),
    n_cols = ncol(l1),
    notes = "Roll-up L1"
  )
  
  for (p in c(
    qc_summary_path,
    qc_mapping_status_path,
    qc_top_unmapped_non_garbage_path,
    qc_top_garbage_groups_path,
    qc_bad_targets_path,
    qc_pandemic_pre2020_examples_path,
    file.path(CFG$qc_dir, "qc_covid_direct_code_input_by_year.csv"),
    file.path(CFG$qc_dir, "qc_covid_direct_code_mapping_by_year.csv"),
    file.path(CFG$qc_dir, "qc_multiple_rule_hits.csv"),
    file.path(CFG$qc_dir, "qc_multiple_cause_hits.csv"),
    file.path(CFG$qc_dir, "qc_rule_candidates_not_applied.csv"),
    file.path(CFG$qc_dir, "qc_rule_conflicts_selected.csv"),
    file.path(CFG$qc_dir, "qc_gc_with_targets_pre.csv"),
    file.path(CFG$qc_dir, "qc_gc_balance_by_group.csv"),
    file.path(CFG$qc_dir, "qc_gc_base_without_output.csv"),
    file.path(CFG$qc_dir, "qc_balance_blocks.csv")
  )) {
    register_artifact(
      dataset_id = CFG$dataset_id_leaf,
      table_name = CFG$table_name_leaf,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "qc",
      artifact_path = p,
      n_rows = tryCatch(nrow(fread(p)), error = function(e) NA_integer_),
      n_cols = tryCatch(ncol(fread(p)), error = function(e) NA_integer_),
      notes = "QC map and redistribute deaths with audit"
    )
  }
  
  register_run_finish(run_id, status = "success", message = "06_map_and_redistribute_deaths completado")
  
  msg("OK -> leaf csv: ", out_leaf_csv)
  msg("OK -> leaf parquet: ", out_leaf_parquet)
  msg("OK -> leaf dict: ", out_leaf_dict)
  msg("OK -> l3: ", out_l3_csv)
  msg("OK -> l2: ", out_l2_csv)
  msg("OK -> l1: ", out_l1_csv)
  msg("OK -> qc dir: ", CFG$qc_dir)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
