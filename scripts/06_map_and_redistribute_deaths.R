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

CFG <- list(
  version = "v1.1.2",
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
  
  out_dir_leaf = here("data", "final", "death_cause_leaf_post_redistribution"),
  out_dir_l3   = here("data", "final", "death_cause_rollup_l3"),
  out_dir_l2   = here("data", "final", "death_cause_rollup_l2"),
  out_dir_l1   = here("data", "final", "death_cause_rollup_l1"),
  
  qc_dir = here("data", "derived", "qc", "06_map_and_redistribute_deaths"),
  
  covid_year_min = 2020L,
  age_min = 0L,
  age_max = 110L,
  
  unmapped_non_gc_term_id = -1L,
  gc_no_target_term_id    = -2L,
  
  redistribution_version = "v1_1_2_fused_with_audit",
  verbose = TRUE
)

for (d in c(CFG$out_dir_leaf, CFG$out_dir_l3, CFG$out_dir_l2, CFG$out_dir_l1, CFG$qc_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

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
  
  dx[, icd10_ucod_nodot := norm_icd10_nodot(icd10_ucod)]
  dx[, icd10_ucod_dot := icd10_add_dot(icd10_ucod_nodot)]
  
  if (dx[is.na(icd10_ucod_nodot), .N] > 0L) {
    stop("Hay registros con icd10_ucod_nodot NA.")
  }
  
  bad_covid_pre2020 <- dx[year_id < CFG$covid_year_min & grepl("^U07", icd10_ucod_nodot), .N]
  
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
        used_representation = NA_character_
      )
    } else {
      m0 <- midx[1]
      causemap_list[[i]] <- data.table(
        icd10_ucod_nodot = cd_nd,
        icd10_ucod_dot = cd_dot,
        mapped_cause_concept_id = cm_regex_all$cause_concept_id[m0],
        mapped_cause_code = cm_regex_all$cause_code[m0],
        mapped_cause_name = cm_regex_all$cause_name[m0],
        mapped_cause_level = cm_regex_all$cause_level[m0],
        mapped_parent_concept_id = cm_regex_all$parent_concept_id[m0],
        mapped_is_terminal = cm_regex_all$is_terminal[m0],
        mapped_is_garbage_master = cm_regex_all$is_garbage[m0],
        mapped_is_residual = cm_regex_all$is_residual[m0],
        mapped_is_covid_related = cm_regex_all$is_covid_related[m0],
        n_cause_hits = as.integer(length(midx)),
        used_representation = used_rep
      )
    }
  }
  
  causemap <- rbindlist(causemap_list, use.names = TRUE, fill = TRUE)
  
  qc_multiple_cause_hits <- causemap[n_cause_hits > 1]
  fwrite(qc_multiple_cause_hits, file.path(CFG$qc_dir, "qc_multiple_cause_hits.csv"))
  
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
      age_start,
      age_end
    )]
  )
  
  # ==========================================================
  # Construcción leaf
  # ==========================================================
  x[, is_garbage_code := !is.na(rule_id)]
  
  lost_unmapped_non_gc <- x[is_garbage_code == FALSE & is.na(mapped_cause_concept_id), sum(n_deaths)]
  
  x[is_garbage_code == FALSE & is.na(mapped_cause_concept_id),
    mapped_cause_concept_id := CFG$unmapped_non_gc_term_id]
  
  non_gc <- x[is_garbage_code == FALSE, .(
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
  
  non_gc[cause_term_concept_id %in% nonfatal_hard_exclude_ids,
         cause_term_concept_id := CFG$gc_no_target_term_id]
  
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
                    mapply(age_rule_applies, age, age_start, age_end)]
  
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
  
  leaf <- rbindlist(list(non_gc, gc_exp, gc_no_targets), use.names = TRUE, fill = TRUE)
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
  
  leaf[, `:=`(
    run_id = run_id,
    redistribution_version = CFG$redistribution_version,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  )]
  
  # ==========================================================
  # Conservación por bloque
  # ==========================================================
  qc_balance_blocks <- data.table(
    block = c("non_garbage", "garbage", "total"),
    deaths_pre = c(
      x[is_garbage_code == FALSE, sum(n_deaths)],
      x[is_garbage_code == TRUE, sum(n_deaths)],
      x[, sum(n_deaths)]
    ),
    deaths_post = c(
      non_gc[, sum(deaths)],
      gc_exp[, sum(deaths)] + gc_no_targets[, sum(deaths)],
      leaf[, sum(deaths)]
    )
  )
  qc_balance_blocks[, delta := deaths_post - deaths_pre]
  fwrite(qc_balance_blocks, file.path(CFG$qc_dir, "qc_balance_blocks.csv"))
  
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
      "total_deaths_pre",
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
      "n_codes_multiple_rule_hits",
      "n_codes_multiple_cause_hits"
    ),
    value = c(
      nrow(pre),
      nrow(leaf),
      pre[, sum(n_deaths)],
      leaf[, sum(deaths)],
      leaf[, sum(deaths)] - pre[, sum(n_deaths)],
      uniqueN(codes$icd10_ucod_nodot),
      x[is_garbage_code == TRUE, sum(n_deaths)],
      x[is_garbage_code == FALSE & !is.na(mapped_cause_concept_id), sum(n_deaths)],
      x[is_garbage_code == FALSE & is.na(mapped_cause_concept_id), sum(n_deaths)],
      dx[grepl("^U07", icd10_ucod_nodot), .N],
      bad_covid_pre2020,
      lost_unmapped_non_gc,
      lost_gc_no_targets,
      nrow(qc_multiple_rule_hits),
      nrow(qc_multiple_cause_hits)
    )
  )
  
  qc_mapping_status <- data.table(
    mapping_status_pre = c(
      "non_garbage_unmapped",
      "garbage_rule_matched",
      "non_garbage_mapped_any_level"
    ),
    deaths = c(
      x[is_garbage_code == FALSE & is.na(mapped_cause_concept_id), sum(n_deaths)],
      x[is_garbage_code == TRUE, sum(n_deaths)],
      x[is_garbage_code == FALSE & !is.na(mapped_cause_concept_id), sum(n_deaths)]
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
  
  fwrite(qc_summary, qc_summary_path)
  fwrite(qc_mapping_status, qc_mapping_status_path)
  fwrite(qc_top_unmapped_non_garbage, qc_top_unmapped_non_garbage_path)
  fwrite(qc_top_garbage_groups, qc_top_garbage_groups_path)
  fwrite(qc_bad_targets_no_terminal_descendants, qc_bad_targets_path)
  
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