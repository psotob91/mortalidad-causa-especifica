library(data.table)

compute_mortality_rate <- function(deaths_dt, pop_dt,
                                   deaths_col = "deaths_final",
                                   pop_col = "population",
                                   multiplier = 100000) {
  x <- merge(
    deaths_dt,
    pop_dt,
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE
  )
  x[, mortality_rate := multiplier * get(deaths_col) / get(pop_col)]
  x[]
}