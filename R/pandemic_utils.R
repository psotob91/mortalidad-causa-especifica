library(data.table)

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