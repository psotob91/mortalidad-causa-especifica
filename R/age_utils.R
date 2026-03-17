library(data.table)

normalize_age_to_years <- function(age_value, age_unit = NULL,
                                   age_min = 0L, age_max = 110L) {
  a <- suppressWarnings(as.numeric(age_value))
  u <- toupper(trimws(as.character(age_unit)))
  
  age_years <- fifelse(
    is.na(a), NA_real_,
    fifelse(
      u %in% c("A", "AÑOS", "ANIOS", "YEAR", "YEARS", ""),
      a,
      fifelse(
        u %in% c("M", "MES", "MESES", "MONTH", "MONTHS"),
        0,
        fifelse(
          u %in% c("D", "DIA", "DIAS", "DAY", "DAYS"),
          0,
          a
        )
      )
    )
  )
  
  age_int <- as.integer(floor(age_years))
  age_int[!is.na(age_int) & age_int < age_min] <- age_min
  age_int[!is.na(age_int) & age_int > age_max] <- age_max
  age_int
}

make_age_group_5y <- function(age) {
  fifelse(
    is.na(age), NA_character_,
    fifelse(age == 0L, "<1",
            fifelse(age %in% 1:4, "1-4",
                    fifelse(age >= 110L, "110+",
                            paste0(floor(age / 5) * 5, "-", floor(age / 5) * 5 + 4)
                    )
            )
    )
  )
}