library(data.table)
library(stringi)

clean_icd10 <- function(x) {
  z <- toupper(stri_trim_both(as.character(x)))
  z[z == ""] <- NA_character_
  z
}

icd10_first_letter <- function(x) {
  z <- substr(clean_icd10(x), 1, 1)
  z[!stri_detect_regex(z, "^[A-Z]$")] <- "OTHER"
  z
}

build_regex_priority <- function(cause_master_dt) {
  x <- copy(cause_master_dt)
  x[, regex_len := nchar(icd10_regex)]
  setorderv(x, c("cause_level", "regex_len"), c(1, -1))
  x
}

map_icd_to_cause <- function(icd10_vec, cause_master_dt) {
  icd <- clean_icd10(icd10_vec)
  out <- rep(NA_integer_, length(icd))
  cm <- build_regex_priority(cause_master_dt)
  
  for (i in seq_len(nrow(cm))) {
    hit <- is.na(out) & !is.na(icd) & stringi::stri_detect_regex(icd, cm$icd10_regex[i])
    out[hit] <- cm$cause_concept_id[i]
  }
  out
}