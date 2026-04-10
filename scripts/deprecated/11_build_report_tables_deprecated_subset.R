#!/usr/bin/env Rscript

# Wrapper deprecated:
# Reproduce la lógica histórica de tablas reportables limitadas a niveles 1:3.

Sys.setenv(
  MORTALITY_REPORT_CAUSE_LEVELS = "1,2,3",
  MORTALITY_TOP_CAUSE_LEVELS = "1,2,3"
)

source(here::here("scripts", "11_build_report_tables.R"))
