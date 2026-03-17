library(data.table)

compute_yll <- function(deaths_dt, life_table_std_dt,
                        deaths_col = "deaths_final") {
  lt <- copy(life_table_std_dt)
  setnames(lt, old = c("exact_age"), new = c("age"), skip_absent = TRUE)
  
  x <- merge(
    deaths_dt,
    lt,
    by.x = c("sex_id", "age"),
    by.y = c("sex_id", "age"),
    all.x = TRUE
  )
  
  if (!"life_expectancy" %in% names(x)) {
    cand <- c("life_expectancy", "ex", "ex_standard")
    nm <- cand[cand %in% names(x)][1]
    if (!is.na(nm)) data.table::setnames(x, nm, "life_expectancy")
  }
  
  x[, yll := get(deaths_col) * life_expectancy]
  x[]
}