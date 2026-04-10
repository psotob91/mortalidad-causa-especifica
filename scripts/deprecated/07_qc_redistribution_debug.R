#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
})

source(here("R", "io_utils.R"))

CFG <- list(
  input_dx = here("data", "final", "death_record_normalized", "death_record_normalized.parquet"),
  input_cm = here("data", "final", "cause_master", "cause_master.csv"),
  input_ru = here("data", "final", "redistribution_rules", "redistribution_rules.csv"),
  qc_dir = here("data", "derived", "qc", "07_qc_redistribution_debug"),
  debug_years = 2024L,
  debug_n_rows = 50000L
)

dir.create(CFG$qc_dir, recursive = TRUE, showWarnings = FALSE)

tick <- function(label) {
  cat(format(Sys.time(), "%Y-%m-%d %H:%M:%S"), " - ", label, "\n", sep = "")
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

match_first_cause <- function(icd_vec, cm) {
  icd_vec <- norm_icd10_nodot(icd_vec)
  out <- rep(NA_integer_, length(icd_vec))
  cm2 <- copy(cm[!is.na(icd10_regex) & icd10_regex != ""])
  cm2[, regex_len := nchar(icd10_regex)]
  setorderv(cm2, c("cause_level", "regex_len", "cause_concept_id"), c(-1L, -1L, 1L))
  for (i in seq_len(nrow(cm2))) {
    rx <- cm2$icd10_regex[i]
    hit <- is.na(out) & !is.na(icd_vec) & grepl(rx, icd_vec, perl = TRUE)
    out[hit] <- cm2$cause_concept_id[i]
  }
  out
}

tick("Leyendo insumos")
dx <- as.data.table(read_auto(CFG$input_dx))
cm <- fread(CFG$input_cm)
ru <- fread(CFG$input_ru)

tick("Filtrando subconjunto debug")
dx <- dx[year_id %in% CFG$debug_years]
if (nrow(dx) > CFG$debug_n_rows) {
  set.seed(123)
  dx <- dx[sample(.N, CFG$debug_n_rows)]
}
fwrite(dx, file.path(CFG$qc_dir, "debug_dx_sample.csv"))

tick("Normalizando ICD y armando code_map")
dx[, icd10_ucod_nodot := norm_icd10_nodot(icd10_ucod)]
code_map <- unique(dx[!is.na(icd10_ucod_nodot), .(icd10_ucod_nodot)])
code_map[, mapped_cause_concept_id := match_first_cause(icd10_ucod_nodot, cm)]
fwrite(code_map, file.path(CFG$qc_dir, "debug_code_map.csv"))

tick("Armando combinaciones únicas pre")
pre <- dx[, .(deaths = .N), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot)]
fwrite(pre, file.path(CFG$qc_dir, "debug_pre_unique.csv"))

tick("Evaluando reglas por combinación única")
ru_src <- unique(ru[, .(
  rule_id, source_group_code, source_group_name,
  sex_restriction, age_start, age_end, regex_r
)])

rule_candidates_list <- vector("list", nrow(pre))
for (i in seq_len(nrow(pre))) {
  cd <- pre$icd10_ucod_nodot[i]
  idx <- which(vapply(ru_src$regex_r, function(p) {
    if (is.na(p) || p == "") return(FALSE)
    grepl(p, cd, perl = TRUE)
  }, logical(1)))
  
  if (length(idx) == 0L) next
  
  tmp <- copy(ru_src[idx])
  tmp[, `:=`(
    year_id = pre$year_id[i],
    location_id = pre$location_id[i],
    sex_id = pre$sex_id[i],
    age = pre$age[i],
    icd10_ucod_nodot = cd
  )]
  rule_candidates_list[[i]] <- tmp
}

rule_candidates <- rbindlist(rule_candidates_list, fill = TRUE, use.names = TRUE)
fwrite(rule_candidates, file.path(CFG$qc_dir, "debug_rule_candidates.csv"))

tick("Filtrando reglas aplicables")
applicable <- rule_candidates[
  vapply(seq_len(.N), function(i) sex_rule_applies(sex_id[i], sex_restriction[i]), logical(1)) &
    vapply(seq_len(.N), function(i) age_rule_applies(age[i], age_start[i], age_end[i]), logical(1))
]
fwrite(applicable, file.path(CFG$qc_dir, "debug_rule_candidates_applicable.csv"))

tick("Resumen debug")
summary_dt <- data.table(
  metric = c("n_dx_rows", "n_unique_codes", "n_pre_groups", "n_rule_candidates", "n_rule_candidates_applicable"),
  value = c(nrow(dx), nrow(code_map), nrow(pre), nrow(rule_candidates), nrow(applicable))
)
fwrite(summary_dt, file.path(CFG$qc_dir, "debug_summary.csv"))

tick("OK debug")
