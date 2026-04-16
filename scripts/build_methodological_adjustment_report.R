#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(here)
})

source(here::here("R", "methodological_adjustment_report_utils.R"))

build_methodological_adjustment_report()
