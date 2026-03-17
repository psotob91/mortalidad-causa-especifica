library(data.table)
library(here)

read_maestro_location <- function(config_dir = here::here("config"),
                                  hierarchical = FALSE) {
  fp <- if (hierarchical) {
    file.path(config_dir, "maestro_location_hierarchical.csv")
  } else {
    file.path(config_dir, "maestro_location_dept.csv")
  }
  fread(fp)
}

read_maestro_sex <- function(config_dir = here::here("config")) {
  fread(file.path(config_dir, "maestro_sex_omop.csv"))
}

read_maestro_age <- function(config_dir = here::here("config")) {
  fread(file.path(config_dir, "maestro_age_simple.csv"))
}

map_sex_to_omop <- function(x, sex_maestro) {
  z <- trimws(toupper(as.character(x)))
  out <- rep(NA_integer_, length(z))
  out[z %in% c("M", "MASCULINO", "HOMBRE", "MALE")] <- 8507L
  out[z %in% c("F", "FEMENINO", "MUJER", "FEMALE")] <- 8532L
  out
}