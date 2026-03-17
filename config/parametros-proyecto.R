# config/parametros-proyecto.R

# -----------------------------
# Horizonte temporal
# -----------------------------
YEARS_ANALISIS <- 2018:2024
YEARS_PREPANDEMIA <- 2018:2019
YEARS_PANDEMIA_O_TRANSICION <- 2020:2024

# -----------------------------
# Edad
# -----------------------------
AGE_MIN <- 0L
AGE_MAX <- 110L
OPEN_AGE <- 110L

# -----------------------------
# Sexo OMOP-like
# -----------------------------
SEX_IDS_VALID <- c(8507L, 8532L)
SEX_ID_MALE <- 8507L
SEX_ID_FEMALE <- 8532L
SEX_ID_TOTAL <- 0L

# -----------------------------
# Ubicación
# -----------------------------
LOCATION_ID_NATIONAL <- 0L
LOCATION_IDS_DEPT <- 1:25
LOCATION_ID_NATIONAL_HIER <- 9000L

# -----------------------------
# COVID / pandemia
# -----------------------------
COVID_YEAR_MIN_VALID <- 2020L
PANDEMIC_START_YEAR <- 2020L
PANDEMIC_END_YEAR <- 2024L

# -----------------------------
# Métodos
# -----------------------------
COMPLETENESS_METHOD_PRECOVID <- "demographic_baseline"
PANDEMIC_METHOD <- "baseline_plus_excess"
SMOOTHING_METHOD <- "gam"

# -----------------------------
# Reglas operativas
# -----------------------------
MIN_DEATHS_FOR_UNSMOOTHED_RATE <- 10L
RATE_MULTIPLIER <- 100000
ALLOW_ZERO_DEATH_ROWS <- TRUE

# -----------------------------
# Versionado
# -----------------------------
PROJECT_VERSION <- "v1.0.0"
CAUSE_MASTER_VERSION <- "v1.0.0"
REDISTRIBUTION_RULES_VERSION <- "v1.0.0"
OUTPUT_VERSION <- "v1.0.0"

# -----------------------------
# Dataset IDs
# -----------------------------
DATASET_ID_DEATH_RAW <- "death_record_sinadef_raw"
DATASET_ID_DEATH_NORMALIZED <- "death_record_sinadef_normalized"
DATASET_ID_CAUSE_MASTER <- "cause_master_bod_omop"
DATASET_ID_REDIST_RULES <- "death_garbage_redistribution_rules"
DATASET_ID_DEATH_POSTREDIST <- "death_cause_leaf_postredistribution"
DATASET_ID_DEATH_FINAL <- "death_cause_final"
DATASET_ID_MORTALITY_RATE <- "mortality_rate_cause_specific"
DATASET_ID_MORTALITY_RATE_SMOOTHED <- "mortality_rate_cause_specific_smoothed"
DATASET_ID_YLL <- "yll_cause_specific"