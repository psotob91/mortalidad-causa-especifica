#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
  library(officer)
  library(flextable)
})

# ============================================================
# Utils del proyecto
# ============================================================
source(here("R", "io_utils.R"))
source(here("R", "catalog_utils.R"))
source(here("R", "age_utils.R"))
source(here("R", "dictionary_utils.R"))
source(here("R", "spec_utils.R"))
source(here("R", "maestro_utils.R"))

# ============================================================
# Config
# ============================================================
CFG <- list(
  version = "v1.0.3",
  dataset_id = "death_cause_redistribution_qc",
  table_name = "death_cause_redistribution_qc",
  
  input_dx   = here("data", "final", "death_record_normalized", "death_record_normalized.parquet"),
  input_cm   = here("data", "final", "cause_master", "cause_master.csv"),
  input_ru   = here("data", "final", "redistribution_rules", "redistribution_rules.csv"),
  input_leaf = here("data", "final", "death_cause_leaf_post_redistribution", "death_cause_leaf_post_redistribution.parquet"),
  input_l1   = here("data", "final", "death_cause_rollup_l1", "death_cause_rollup_l1.csv"),
  input_l2   = here("data", "final", "death_cause_rollup_l2", "death_cause_rollup_l2.csv"),
  input_l3   = here("data", "final", "death_cause_rollup_l3", "death_cause_rollup_l3.csv"),
  
  qc06_dir   = resolve_existing_qc_path("map_and_redistribute_deaths", must_work = TRUE),
  qc_dir     = qc_dir_path("qc_redistribution"),
  out_dir    = output_aux_dir_path("qc_redistribution"),
  
  expected_delta_abs = 2,
  min_deaths_for_pct_rank = 20,
  top_n = 20L,
  verbose = TRUE
)

dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(CFG$out_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================================
# Helpers
# ============================================================
msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(paste0(...), "\n")
}

fmt_num <- function(x, digits = 1) {
  format(
    round(as.numeric(x), digits),
    big.mark = ",",
    decimal.mark = ".",
    nsmall = digits,
    trim = TRUE
  )
}

read_dt_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") return(fread(path))
  if (ext == "parquet") return(as.data.table(arrow::read_parquet(path)))
  stop("Formato no soportado: ", path)
}

norm_icd10_nodot <- function(x) {
  z <- toupper(trimws(as.character(x)))
  z <- gsub("[\\.\\s]", "", z)
  z[z %in% c("", "NA", "NULL", "N/A", "null")] <- NA_character_
  z
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

top_n_dt <- function(dt, n) {
  if (nrow(dt) == 0L) return(copy(dt))
  dt[seq_len(min(nrow(dt), as.integer(n)))]
}

# ============================================================
# Mapeo ICD -> causa
# ============================================================
match_first_cause <- function(icd_vec, cm) {
  icd_vec <- norm_icd10_nodot(icd_vec)
  out <- rep(NA_integer_, length(icd_vec))
  
  cm2 <- copy(cm)
  cm2[, regex_len := nchar(icd10_regex)]
  setorderv(cm2, c("cause_level", "regex_len", "cause_concept_id"), c(-1L, -1L, 1L))
  
  for (i in seq_len(nrow(cm2))) {
    rx <- cm2$icd10_regex[i]
    if (is.na(rx) || rx == "") next
    hit <- is.na(out) & !is.na(icd_vec) & grepl(rx, icd_vec, perl = TRUE)
    out[hit] <- cm2$cause_concept_id[i]
  }
  
  out
}

# ============================================================
# Lookup jerárquico
# ============================================================
build_lookup <- function(cm) {
  x <- copy(cm[, .(
    cause_concept_id,
    parent_concept_id,
    cause_level,
    cause_code,
    cause_name,
    is_terminal,
    is_covid_related,
    yll_flag
  )])
  setkey(x, cause_concept_id)
  
  term_ids <- x[is_terminal == TRUE, cause_concept_id]
  
  out <- rbindlist(lapply(term_ids, function(tid) {
    row_out <- data.table(
      cause_concept_id = tid,
      cause_code_l1 = NA_character_,
      cause_name_l1 = NA_character_,
      cause_code_l2 = NA_character_,
      cause_name_l2 = NA_character_,
      cause_code_l3 = NA_character_,
      cause_name_l3 = NA_character_,
      cause_code_l4 = NA_character_,
      cause_name_l4 = NA_character_,
      is_covid_related = FALSE,
      yll_flag = NA_integer_
    )
    
    cur <- tid
    repeat {
      row <- x[J(cur)]
      if (nrow(row) == 0L) break
      
      lvl <- row$cause_level[1]
      if (lvl == 1L) {
        row_out[, `:=`(cause_code_l1 = row$cause_code[1], cause_name_l1 = row$cause_name[1])]
      } else if (lvl == 2L) {
        row_out[, `:=`(cause_code_l2 = row$cause_code[1], cause_name_l2 = row$cause_name[1])]
      } else if (lvl == 3L) {
        row_out[, `:=`(cause_code_l3 = row$cause_code[1], cause_name_l3 = row$cause_name[1])]
      } else if (lvl == 4L) {
        row_out[, `:=`(cause_code_l4 = row$cause_code[1], cause_name_l4 = row$cause_name[1])]
      }
      
      if (isTRUE(row$is_covid_related[1])) row_out[, is_covid_related := TRUE]
      if (is.na(row_out$yll_flag[1]) && !is.na(row$yll_flag[1])) row_out[, yll_flag := as.integer(row$yll_flag[1])]
      
      par <- row$parent_concept_id[1]
      if (is.na(par)) break
      cur <- par
    }
    
    row_out
  }), fill = TRUE)
  
  out[]
}

# ============================================================
# Escenario BEFORE
# ============================================================
derive_before <- function(dx, cm, ru, lk) {
  nonfatal_hard_exclude_ids <- c(
    9000580L, 9001280L, 9001360L, 9001380L, 9001370L,
    9000880L, 9000831L, 9000832L, 9000840L, 9000850L,
    9000900L, 9000911L, 9000912L, 9000920L, 9000990L,
    9001000L, 9001020L, 9001070L, 9001090L, 9001490L,
    9001502L, 9000270L, 9000362L, 9000365L, 9000560L
  )

  x <- copy(dx)
  x[, icd10_ucod_nodot := norm_icd10_nodot(icd10_ucod)]
  
  code_map <- unique(x[!is.na(icd10_ucod_nodot), .(icd10_ucod_nodot)])
  if (nrow(code_map) > 0L) {
    code_map[, mapped_cause_concept_id := match_first_cause(icd10_ucod_nodot, cm)]
    x <- merge(x, code_map, by = "icd10_ucod_nodot", all.x = TRUE, sort = FALSE)
  } else {
    x[, mapped_cause_concept_id := NA_integer_]
  }

  x <- merge(
    x,
    cm[, .(mapped_cause_concept_id = cause_concept_id, mapped_yll_flag = as.integer(yll_flag))],
    by = "mapped_cause_concept_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  ru_src <- unique(ru[, .(
    rule_id, source_group_code, source_group_name,
    sex_restriction, age_start, age_end, regex_r
  )])
  
  pre <- x[, .(deaths = .N), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot)]
  
  if (nrow(pre) > 0L) {
    code_rules <- unique(pre[!is.na(icd10_ucod_nodot), .(icd10_ucod_nodot)])
    rule_candidates_list <- vector("list", nrow(code_rules))
    for (i in seq_len(nrow(code_rules))) {
      cd <- code_rules$icd10_ucod_nodot[i]
      idx <- which(vapply(ru_src$regex_r, function(p) {
        if (is.na(p) || p == "") return(FALSE)
        grepl(p, cd, perl = TRUE)
      }, logical(1)))
      
      if (length(idx) == 0L) {
        rule_candidates_list[[i]] <- data.table(
          icd10_ucod_nodot = cd,
          rule_id = NA_character_,
          source_group_code = NA_character_,
          source_group_name = NA_character_,
          sex_restriction = NA_character_,
          age_start = NA_integer_,
          age_end = NA_integer_
        )
      } else {
        tmp <- copy(ru_src[idx])
        tmp[, icd10_ucod_nodot := cd]
        rule_candidates_list[[i]] <- tmp
      }
    }
    
    rule_candidates <- rbindlist(rule_candidates_list, fill = TRUE, use.names = TRUE)
    pre_rule_candidates <- merge(
      pre,
      rule_candidates,
      by = "icd10_ucod_nodot",
      all.x = TRUE,
      sort = FALSE,
      allow.cartesian = TRUE
    )
    
    applicable <- pre_rule_candidates[
      !is.na(rule_id) &
        vapply(seq_len(.N), function(i) sex_rule_applies(sex_id[i], sex_restriction[i]), logical(1)) &
        vapply(seq_len(.N), function(i) age_rule_applies(age[i], age_start[i], age_end[i]), logical(1))
    ]
    
    if (nrow(applicable) > 0L) {
      applicable[, rule_id_num := suppressWarnings(as.integer(gsub("[^0-9]", "", rule_id)))]
      applicable[, sex_specific := !is.na(sex_restriction)]
      applicable[, age_span := fifelse(
        !is.na(age_start) & !is.na(age_end), age_end - age_start,
        fifelse(!is.na(age_start) | !is.na(age_end), 1000L, 9999L)
      )]
      setorderv(
        applicable,
        cols = c("year_id","location_id","sex_id","age","icd10_ucod_nodot","sex_specific","age_span","rule_id_num","rule_id"),
        order = c(1L,1L,1L,1L,1L,-1L,1L,1L,1L)
      )
      selected_rules <- applicable[, .SD[1], by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot)]
      x <- merge(
        x,
        selected_rules[, .(year_id, location_id, sex_id, age, icd10_ucod_nodot, source_group_code, source_group_name)],
        by = c("year_id", "location_id", "sex_id", "age", "icd10_ucod_nodot"),
        all.x = TRUE,
        sort = FALSE
      )
    } else {
      x[, `:=`(source_group_code = NA_character_, source_group_name = NA_character_)]
    }
  } else {
    x[, `:=`(source_group_code = NA_character_, source_group_name = NA_character_)]
  }
  
  x[, is_garbage_code := !is.na(source_group_code) & source_group_code != ""]
  
  x_gc <- x[is_garbage_code == TRUE]
  x_ng <- x[is_garbage_code == FALSE]
  x_ng_nonfatal <- x_ng[!is.na(mapped_cause_concept_id) & mapped_cause_concept_id %in% nonfatal_hard_exclude_ids]
  x_ng <- x_ng[is.na(mapped_yll_flag) | mapped_yll_flag != 0L]
  x_ng <- x_ng[!mapped_cause_concept_id %in% nonfatal_hard_exclude_ids]
  
  before_non_gc <- x_ng[
    !is.na(mapped_cause_concept_id),
    .(deaths_before = .N),
    by = .(year_id, location_id, sex_id, age, cause_concept_id = mapped_cause_concept_id)
  ]
  
  before_non_gc <- merge(before_non_gc, lk, by = "cause_concept_id", all.x = TRUE)
  
  before_gc_group <- x_gc[
    !is.na(source_group_code),
    .(deaths_before = .N),
    by = .(year_id, location_id, sex_id, age, source_group_code, source_group_name)
  ]
  
  qc_non_gc_unmapped <- x_ng[
    is.na(mapped_cause_concept_id),
    .(deaths = .N),
    by = .(year_id, sex_id, age, icd10_ucod)
  ][order(-deaths)]
  
  qc_gc_without_rule <- x_gc[
    is.na(source_group_code),
    .(deaths = .N),
    by = .(year_id, sex_id, age, icd10_ucod)
  ][order(-deaths)]

  qc_nonfatal_direct_excluded <- x_ng_nonfatal[
    ,
    .(deaths = .N),
    by = .(year_id, sex_id, age, icd10_ucod)
  ][order(-deaths)]
  
  list(
    before_non_gc = before_non_gc,
    before_gc_group = before_gc_group,
    qc_non_gc_unmapped = qc_non_gc_unmapped,
    qc_gc_without_rule = qc_gc_without_rule,
    qc_nonfatal_direct_excluded = qc_nonfatal_direct_excluded
  )
}

# ============================================================
# Rollups BEFORE
# ============================================================
roll_before <- function(before_non_gc, level = c("l1", "l2", "l3")) {
  level <- match.arg(level)
  code_col <- paste0("cause_code_", level)
  name_col <- paste0("cause_name_", level)
  
  out <- before_non_gc[, .(
    deaths_before = sum(deaths_before)
  ), by = c("year_id", "location_id", "sex_id", "age", code_col, name_col)]
  
  setnames(out, c(code_col, name_col), c("cause_code", "cause_name"))
  out[]
}

add_gc_pool <- function(before_roll, before_gc_group, label = "Redistribution / Garbage") {
  gc <- before_gc_group[, .(
    deaths_before = sum(deaths_before)
  ), by = .(year_id, location_id, sex_id, age)]
  
  gc[, `:=`(
    cause_code = "GC_REDIST",
    cause_name = label
  )]
  
  rbind(before_roll, gc, fill = TRUE)
}

# ============================================================
# Rollups AFTER
# ============================================================
roll_after <- function(after_rollup, cm, level = c("l1", "l2", "l3")) {
  level <- match.arg(level)
  x <- copy(after_rollup)
  
  if ("deaths" %in% names(x) && !"deaths_after" %in% names(x)) {
    setnames(x, "deaths", "deaths_after")
  }
  
  if (!"cause_concept_id" %in% names(x)) {
    stop("El rollup after no tiene columna cause_concept_id.")
  }
  if (!"deaths_after" %in% names(x)) {
    stop("El rollup after no tiene columna deaths/deaths_after.")
  }
  
  cm_level <- switch(
    level,
    l1 = cm[cause_level == 1L, .(cause_concept_id, cause_code, cause_name)],
    l2 = cm[cause_level == 2L, .(cause_concept_id, cause_code, cause_name)],
    l3 = cm[cause_level == 3L, .(cause_concept_id, cause_code, cause_name)]
  )
  
  x <- merge(
    x,
    cm_level,
    by = "cause_concept_id",
    all.x = TRUE
  )
  
  x[is.na(cause_code), cause_code := paste0("UNK_", cause_concept_id)]
  x[is.na(cause_name), cause_name := paste0("Cause ", cause_concept_id)]
  
  x[, .(
    year_id,
    location_id,
    sex_id,
    age,
    cause_code,
    cause_name,
    deaths_after
  )]
}

after_special_bucket <- function(leaf, code = "NEG_AFTER", name = "Unmapped / no target after redistribution") {
  x <- copy(leaf)
  
  if ("cause_term_concept_id" %in% names(x) && !"cause_concept_id" %in% names(x)) {
    setnames(x, "cause_term_concept_id", "cause_concept_id")
  }
  if ("deaths" %in% names(x) && !"deaths_after" %in% names(x)) {
    setnames(x, "deaths", "deaths_after")
  }
  
  x <- x[cause_concept_id <= 0]
  if (nrow(x) == 0L) {
    return(data.table(
      year_id = integer(),
      location_id = integer(),
      sex_id = integer(),
      age = integer(),
      cause_code = character(),
      cause_name = character(),
      deaths_after = numeric()
    ))
  }
  
  x[, .(
    deaths_after = sum(deaths_after)
  ), by = .(year_id, location_id, sex_id, age)][, `:=`(
    cause_code = code,
    cause_name = name
  )][, .(year_id, location_id, sex_id, age, cause_code, cause_name, deaths_after)]
}

# ============================================================
# Merge BEFORE/AFTER
# ============================================================
merge_before_after <- function(before_dt, after_dt) {
  x <- merge(
    before_dt,
    after_dt,
    by = c("year_id", "location_id", "sex_id", "age", "cause_code", "cause_name"),
    all = TRUE
  )
  
  x[is.na(deaths_before), deaths_before := 0]
  x[is.na(deaths_after), deaths_after := 0]
  x[, abs_change := deaths_after - deaths_before]
  x[]
}

# ============================================================
# DOCX helpers
# ============================================================
doc_table <- function(doc, title, dt, note = NULL) {
  doc <- body_add_par(doc, title, style = "heading 1")
  if (!is.null(note)) doc <- body_add_par(doc, note, style = "Normal")
  ft <- flextable(dt)
  ft <- theme_booktabs(ft)
  ft <- autofit(ft)
  doc <- body_add_flextable(doc, ft)
  body_add_par(doc, "", style = "Normal")
}

fmt_main <- function(dt) {
  x <- copy(dt)
  for (nm in intersect(c("deaths_before", "deaths_after", "abs_change", "delta"), names(x))) {
    x[, (nm) := fmt_num(get(nm), 1)]
  }
  x
}

# ============================================================
# Main
# ============================================================
ensure_project_dirs()
ensure_catalog_files()

for (fp in c(
  CFG$input_dx, CFG$input_cm, CFG$input_ru,
  CFG$input_leaf, CFG$input_l1, CFG$input_l2, CFG$input_l3
)) {
  if (!file.exists(fp)) stop("No existe: ", fp)
}

run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version)

tryCatch({
  
  msg("Leyendo insumos... ")
  dx   <- read_dt_any(CFG$input_dx)
  cm   <- read_dt_any(CFG$input_cm)
  ru   <- read_dt_any(CFG$input_ru)
  leaf <- read_dt_any(CFG$input_leaf)
  l1   <- read_dt_any(CFG$input_l1)
  l2   <- read_dt_any(CFG$input_l2)
  l3   <- read_dt_any(CFG$input_l3)
  
  qc06_summary    <- fread(file.path(CFG$qc06_dir, "qc_summary.csv"))
  qc06_gc_balance <- fread(file.path(CFG$qc06_dir, "qc_gc_balance_by_group.csv"))
  qc06_mapping    <- fread(file.path(CFG$qc06_dir, "qc_mapping_status.csv"))
  qc06_pandemic   <- fread(file.path(CFG$qc06_dir, "qc_pandemic_pre2020_examples.csv"))
  
  msg("Construyendo lookup... ")
  lk <- build_lookup(cm)
  
  msg("Reconstruyendo BEFORE... ")
  bef <- derive_before(dx, cm, ru, lk)
  
  msg("Armando rollups BEFORE... ")
  before_l1 <- add_gc_pool(roll_before(bef$before_non_gc, "l1"), bef$before_gc_group)
  before_l2 <- add_gc_pool(roll_before(bef$before_non_gc, "l2"), bef$before_gc_group)
  before_l3 <- add_gc_pool(roll_before(bef$before_non_gc, "l3"), bef$before_gc_group)
  
  msg("Armando rollups AFTER... ")
  after_l1 <- roll_after(l1, cm, "l1")
  after_l2 <- roll_after(l2, cm, "l2")
  after_l3 <- roll_after(l3, cm, "l3")
  after_special <- after_special_bucket(leaf)
  after_l1 <- rbind(after_l1, after_special, fill = TRUE)
  after_l2 <- rbind(after_l2, after_special, fill = TRUE)
  after_l3 <- rbind(after_l3, after_special, fill = TRUE)
  
  msg("Combinando BEFORE/AFTER... ")
  ba_l1 <- merge_before_after(before_l1, after_l1)
  ba_l2 <- merge_before_after(before_l2, after_l2)
  ba_l3 <- merge_before_after(before_l3, after_l3)
  
  # ----------------------------------------------------------
  # QC balance
  # ----------------------------------------------------------
  qc_balance_total <- data.table(
    metric = c(
      "deaths_before_total",
      "deaths_after_total",
      "delta_total",
      "expected_delta_abs_threshold"
    ),
    value = c(
      ba_l1[, sum(deaths_before)],
      ba_l1[, sum(deaths_after)],
      ba_l1[, sum(deaths_after)] - ba_l1[, sum(deaths_before)],
      CFG$expected_delta_abs
    )
  )
  
  delta_total <- as.numeric(qc_balance_total[metric == "delta_total", value])
  
  balance_status <- if (abs(delta_total) <= CFG$expected_delta_abs) {
    "PASS_WITH_DOCUMENTED_RESIDUAL"
  } else {
    "REVIEW_REQUIRED"
  }
  
  qc_balance_year <- ba_l1[, .(
    deaths_before = sum(deaths_before),
    deaths_after  = sum(deaths_after),
    delta         = sum(deaths_after) - sum(deaths_before)
  ), by = year_id][order(year_id)]
  
  qc_balance_year_sex <- ba_l1[, .(
    deaths_before = sum(deaths_before),
    deaths_after  = sum(deaths_after),
    delta         = sum(deaths_after) - sum(deaths_before)
  ), by = .(year_id, sex_id)][order(year_id, sex_id)]
  
  # ----------------------------------------------------------
  # Tablas Word (solo absolutos: modo blindado)
  # ----------------------------------------------------------
  tab_l1_nat <- ba_l1[, .(
    deaths_before = sum(deaths_before),
    deaths_after  = sum(deaths_after)
  ), by = .(cause_code, cause_name)]
  tab_l1_nat[, abs_change := deaths_after - deaths_before]
  setorder(tab_l1_nat, -deaths_after)
  
  tab_l1_year <- ba_l1[, .(
    deaths_before = sum(deaths_before),
    deaths_after  = sum(deaths_after)
  ), by = .(year_id, cause_code, cause_name)]
  tab_l1_year[, abs_change := deaths_after - deaths_before]
  setorder(tab_l1_year, year_id, -deaths_after)
  
  tab_top_gain_abs_l3 <- ba_l3[, .(
    deaths_before = sum(deaths_before),
    deaths_after  = sum(deaths_after)
  ), by = .(cause_code, cause_name)]
  tab_top_gain_abs_l3[, abs_change := deaths_after - deaths_before]
  setorder(tab_top_gain_abs_l3, -abs_change)
  tab_top_gain_abs_l3 <- top_n_dt(tab_top_gain_abs_l3, CFG$top_n)
  
  tab_gc_groups <- bef$before_gc_group[, .(
    deaths_before = sum(deaths_before)
  ), by = .(source_group_code, source_group_name)]
  setorder(tab_gc_groups, -deaths_before)
  tab_gc_groups <- top_n_dt(tab_gc_groups, CFG$top_n)
  
  # ----------------------------------------------------------
  # Pandemia / COVID
  # ----------------------------------------------------------
  if ("cause_term_concept_id" %in% names(leaf) && !"cause_concept_id" %in% names(leaf)) {
    setnames(leaf, "cause_term_concept_id", "cause_concept_id")
  }
  if ("deaths" %in% names(leaf) && !"deaths_after" %in% names(leaf)) {
    setnames(leaf, "deaths", "deaths_after")
  }
  
  leaf_cov <- merge(
    leaf,
    lk[, .(cause_concept_id, is_covid_related)],
    by = "cause_concept_id",
    all.x = TRUE
  )
  
  before_cov <- merge(
    bef$before_non_gc[, .(year_id, location_id, sex_id, age, cause_concept_id, deaths_before)],
    lk[, .(cause_concept_id, is_covid_related)],
    by = "cause_concept_id",
    all.x = TRUE
  )
  
  qc_pandemic_summary <- merge(
    before_cov[is_covid_related == TRUE, .(deaths_before = sum(deaths_before)), by = year_id],
    leaf_cov[is_covid_related == TRUE, .(deaths_after = sum(deaths_after)), by = year_id],
    by = "year_id", all = TRUE
  )
  
  qc_pandemic_summary[is.na(deaths_before), deaths_before := 0]
  qc_pandemic_summary[is.na(deaths_after), deaths_after := 0]
  qc_pandemic_summary[, delta := deaths_after - deaths_before]
  setorder(qc_pandemic_summary, year_id)
  
  # ----------------------------------------------------------
  # QC resumen
  # ----------------------------------------------------------
  n_non_gc_unmapped_before <- if (nrow(bef$qc_non_gc_unmapped) == 0L) 0 else bef$qc_non_gc_unmapped[, sum(deaths)]
  n_gc_without_rule_before <- if (nrow(bef$qc_gc_without_rule) == 0L) 0 else bef$qc_gc_without_rule[, sum(deaths)]
  n_nonfatal_direct_excluded_before <- if (nrow(bef$qc_nonfatal_direct_excluded) == 0L) 0 else bef$qc_nonfatal_direct_excluded[, sum(deaths)]
  
  qc_summary <- data.table(
    metric = c(
      "balance_status",
      "delta_total",
      "expected_delta_abs_threshold",
      "n_non_gc_unmapped_before",
      "n_gc_without_rule_before",
      "n_nonfatal_direct_excluded_before",
      "n_qc06_pandemic_pre2020_rows"
    ),
    value = c(
      balance_status,
      as.character(delta_total),
      as.character(CFG$expected_delta_abs),
      as.character(n_non_gc_unmapped_before),
      as.character(n_gc_without_rule_before),
      as.character(n_nonfatal_direct_excluded_before),
      as.character(nrow(qc06_pandemic))
    )
  )
  
  # ----------------------------------------------------------
  # Export QC
  # ----------------------------------------------------------
  fwrite(qc_balance_total, file.path(CFG$qc_dir, "qc_balance_total.csv"))
  fwrite(qc_balance_year, file.path(CFG$qc_dir, "qc_balance_year.csv"))
  fwrite(qc_balance_year_sex, file.path(CFG$qc_dir, "qc_balance_year_sex.csv"))
  
  fwrite(ba_l1, file.path(CFG$qc_dir, "before_after_l1.csv"))
  fwrite(ba_l2, file.path(CFG$qc_dir, "before_after_l2.csv"))
  fwrite(ba_l3, file.path(CFG$qc_dir, "before_after_l3.csv"))
  
  fwrite(bef$qc_non_gc_unmapped, file.path(CFG$qc_dir, "qc_non_gc_unmapped_before.csv"))
  fwrite(bef$qc_gc_without_rule, file.path(CFG$qc_dir, "qc_gc_without_rule_before.csv"))
  fwrite(bef$qc_nonfatal_direct_excluded, file.path(CFG$qc_dir, "qc_nonfatal_direct_excluded_before.csv"))
  fwrite(qc_pandemic_summary, file.path(CFG$qc_dir, "qc_pandemic_summary.csv"))
  fwrite(qc_summary, file.path(CFG$qc_dir, "qc_summary.csv"))
  
  fwrite(qc06_summary, file.path(CFG$qc_dir, "qc06_summary_copy.csv"))
  fwrite(qc06_gc_balance, file.path(CFG$qc_dir, "qc06_gc_balance_by_group_copy.csv"))
  fwrite(qc06_mapping, file.path(CFG$qc_dir, "qc06_mapping_status_copy.csv"))
  fwrite(qc06_pandemic, file.path(CFG$qc_dir, "qc06_pandemic_pre2020_examples_copy.csv"))
  
  fwrite(tab_l1_nat, file.path(CFG$qc_dir, "tab_word_l1_national_before_after.csv"))
  fwrite(tab_l1_year, file.path(CFG$qc_dir, "tab_word_l1_year_before_after.csv"))
  fwrite(tab_top_gain_abs_l3, file.path(CFG$qc_dir, "tab_top_gain_abs_l3.csv"))
  fwrite(tab_gc_groups, file.path(CFG$qc_dir, "tab_top_gc_groups.csv"))
  
  # ----------------------------------------------------------
  # DOCX anexable
  # ----------------------------------------------------------
  doc <- read_docx()
  doc <- body_add_par(doc, "Anexo de redistribución de garbage codes", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste0(
      "Balance global: ", balance_status,
      ". Se acepta un delta absoluto esperado de hasta ",
      CFG$expected_delta_abs,
      " por un garbage residual ya documentado."
    ),
    style = "Normal"
  )
  
  doc <- doc_table(
    doc,
    "Tabla 1. Comparación nacional L1 antes y después",
    fmt_main(tab_l1_nat),
    note = "Versión blindada para entrega: solo absolutos."
  )
  
  doc <- doc_table(
    doc,
    "Tabla 2. Comparación por año L1",
    fmt_main(tab_l1_year)
  )
  
  doc <- doc_table(
    doc,
    "Tabla 3. Resumen pandemia/COVID",
    fmt_main(qc_pandemic_summary)
  )
  
  doc <- doc_table(
    doc,
    "Tabla 4. Top ganancias absolutas L3",
    fmt_main(tab_top_gain_abs_l3)
  )
  
  doc <- doc_table(
    doc,
    "Tabla 5. Top grupos garbage antes de redistribución",
    fmt_main(tab_gc_groups)
  )
  
  doc <- doc_table(
    doc,
    "Tabla 6. Balance contable global",
    qc_balance_total
  )
  
  out_docx <- file.path(CFG$out_dir, "anexo_redistribucion_before_after.docx")
  print(doc, target = out_docx)
  
  register_artifact(
    dataset_id = CFG$dataset_id,
    table_name = CFG$table_name,
    version = CFG$version,
    run_id = run_id,
    artifact_type = "report",
    artifact_path = out_docx,
    notes = "Anexo Word before/after redistribución - version blindada sin porcentajes"
  )
  
  register_run_finish(run_id, status = "success", message = "07_qc_redistribution completado")
  msg("OK: ", out_docx)
  
}, error = function(e) {
  register_run_finish(run_id, status = "failed", message = as.character(e$message))
  stop(e)
})
