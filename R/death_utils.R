library(data.table)
library(stringi)

build_death_id <- function(year_id, row_id, prefix = "SINADEF") {
  sprintf("%s_%s_%012d", prefix, year_id, row_id)
}

normalize_death_record <- function(dt,
                                   sex_maestro = NULL,
                                   location_lookup = NULL) {
  x <- as.data.table(copy(dt))
  
  if (!"death_id" %in% names(x)) {
    x[, death_id := build_death_id(year_id, .I)]
  }
  
  if ("icd10_ucod_raw" %in% names(x)) {
    x[, icd10_ucod := clean_icd10(icd10_ucod_raw)]
  }
  
  if ("sex_source_value" %in% names(x) && !is.null(sex_maestro)) {
    x[, sex_id := map_sex_to_omop(sex_source_value, sex_maestro)]
  }
  
  x[]
}