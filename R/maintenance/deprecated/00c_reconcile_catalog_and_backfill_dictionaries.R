#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(here))
sys.source(here::here('R/maintenance/reconcile_catalog_and_backfill_dictionaries.R'), envir = globalenv())
