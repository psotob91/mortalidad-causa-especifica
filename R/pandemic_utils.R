library(data.table)

normalize_pandemic_label <- function(x) {
  x <- as.character(x %||% "")
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- tolower(trimws(x))
  x
}

pandemic_named_component_classes <- function() {
  c("covid_specific", "measles", "lri", "pertussis")
}

classify_pandemic_component <- function(cause_name,
                                        is_covid_specific = FALSE,
                                        is_oprm = FALSE) {
  nm <- normalize_pandemic_label(cause_name)
  out <- rep("non_pandemic", length(nm))

  out[isTRUE(is_oprm)] <- "oprm"
  out[grepl("^other pandemic related mortality \\(oprm\\)$", nm)] <- "oprm"
  out[grepl("^oprm$", nm)] <- "oprm"

  out[isTRUE(is_covid_specific)] <- "covid_specific"
  out[nm %in% c("covid-19", "covid 19")] <- "covid_specific"
  out[nm %in% c("sarampion", "measles")] <- "measles"
  out[nm %in% c("tos ferina", "pertussis")] <- "pertussis"
  out[nm %in% c(
    "infecciones de vias respiratorias inferiores",
    "infeccion de vias respiratorias inferiores",
    "lower respiratory infections",
    "lower respiratory infection",
    "lri"
  )] <- "lri"

  out
}

add_pandemic_component_flags <- function(cm_dt) {
  x <- copy(cm_dt)
  if (!"is_covid_specific" %in% names(x)) x[, is_covid_specific := FALSE]
  if (!"is_oprm" %in% names(x)) x[, is_oprm := FALSE]

  x[, pandemic_component_class := classify_pandemic_component(
    cause_name = cause_name,
    is_covid_specific = fcoalesce(as.logical(is_covid_specific), FALSE),
    is_oprm = fcoalesce(as.logical(is_oprm), FALSE)
  )]
  x[, is_pandemic_named_component := pandemic_component_class %in% pandemic_named_component_classes()]
  x[, is_pandemic_related_any := pandemic_component_class %in% c(pandemic_named_component_classes(), "oprm")]
  x
}

compute_allcause_baseline_ratio <- function(obs_dt, exp_dt) {
  x <- merge(
    obs_dt,
    exp_dt,
    by = c("year_id", "location_id", "sex_id", "age"),
    all = TRUE,
    suffixes = c("_obs", "_exp")
  )
  x[, completeness_ratio := deaths_expected / deaths_observed]
  x[]
}

compute_pandemic_excess <- function(obs_dt, baseline_dt) {
  x <- merge(
    obs_dt,
    baseline_dt,
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    suffixes = c("_obs", "_baseline")
  )
  x[, pandemic_excess := pmax(0, deaths_observed - deaths_baseline)]
  x[]
}
