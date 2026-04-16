#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(data.table)
  library(here)
})

source(here("R", "io_utils.R"))

run_step <- function(script_path, env = character()) {
  old <- Sys.getenv(names(env), unset = NA_character_)
  on.exit({
    for (nm in names(old)) {
      if (is.na(old[[nm]])) {
        Sys.unsetenv(nm)
      } else {
        Sys.setenv(structure(old[[nm]], names = nm))
      }
    }
  }, add = TRUE)
  if (length(env)) do.call(Sys.setenv, as.list(env))
  status <- system2("Rscript", c(normalizePath(script_path, winslash = "/", mustWork = TRUE)), stdout = "", stderr = "")
  if (!identical(status, 0L)) stop("Falló step: ", script_path)
}

suffix <- "no_redistribution_delete_gc"

root <- here()
out_dir <- file.path(root, "data", "derived", "analysis_sensitivity", "redistribution", suffix)
qc_dir <- file.path(root, "data", "derived", "qc", "analysis_sensitivity", "redistribution", suffix)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(qc_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- data.table(
  stage = c(
    "leaf_post_mapping",
    "death_cause_final",
    "death_cause_final_hierarchical",
    "mortality_rate_cause_smoothed",
    "mortality_rate_cause_smoothed_reconciled",
    "avp_yll_cause_reconciled"
  ),
  dataset_path = c(
    file.path("data", "final", paste0("death_cause_leaf_post_redistribution_", suffix), "death_cause_leaf_post_redistribution.csv"),
    file.path("data", "final", paste0("death_cause_final_", suffix), "death_cause_final.csv"),
    file.path("data", "final", paste0("death_cause_final_hierarchical_", suffix), "death_cause_final_hierarchical.csv"),
    file.path("data", "final", paste0("mortality_rate_cause_smoothed_", suffix), "mortality_rate_cause_smoothed.csv"),
    file.path("data", "final", paste0("mortality_rate_cause_smoothed_reconciled_", suffix), "mortality_rate_cause_smoothed_reconciled.csv"),
    file.path("data", "final", paste0("avp_yll_cause_reconciled_", suffix), "avp_yll_cause_reconciled.csv")
  )
)

fwrite(manifest, file.path(out_dir, "sensitivity_manifest.csv"))

run_step(
  file.path(root, "scripts", "map_and_redistribute_deaths.R"),
  c(
    REDIST_MODE = suffix,
    REDIST_OUTPUT_SUFFIX = suffix
  )
)

run_step(
  file.path(root, "scripts", "build_death_cause_final.R"),
  c(
    DCF_LEAF_INPUT_PATH = file.path(root, "data", "final", paste0("death_cause_leaf_post_redistribution_", suffix), "death_cause_leaf_post_redistribution.csv"),
    DCF_OUTPUT_SUFFIX = suffix
  )
)

run_step(
  file.path(root, "scripts", "rollup_death_cause_final.R"),
  c(
    DCF_FINAL_INPUT_PATH = file.path(root, "data", "final", paste0("death_cause_final_", suffix), "death_cause_final.csv"),
    DCF_ROLLUP_OUTPUT_SUFFIX = suffix
  )
)

run_step(
  file.path(root, "scripts", "build_mortality_rates.R"),
  c(
    MORTALITY_INPUT_DEATH_PATH = file.path(root, "data", "final", paste0("death_cause_final_hierarchical_", suffix), "death_cause_final_hierarchical.csv"),
    MORTALITY_OUTPUT_SUFFIX = suffix
  )
)

run_step(
  file.path(root, "scripts", "reconcile_mortality_hierarchy.R"),
  c(
    RECON_INPUT_SMOOTHED_PATH = file.path(root, "data", "final", paste0("mortality_rate_cause_smoothed_", suffix), "mortality_rate_cause_smoothed.csv"),
    RECON_OUTPUT_SUFFIX = suffix
  )
)

run_step(
  file.path(root, "scripts", "compute_avp_yll.R"),
  c(
    AVP_INPUT_MORTALITY_PATH = file.path(root, "data", "final", paste0("mortality_rate_cause_smoothed_reconciled_", suffix), "mortality_rate_cause_smoothed_reconciled.csv"),
    AVP_OUTPUT_SUFFIX = suffix
  )
)

copy_if_exists <- function(src, dst) {
  if (file.exists(src)) {
    dir.create(dirname(dst), recursive = TRUE, showWarnings = FALSE)
    file.copy(src, dst, overwrite = TRUE)
  }
}

copy_if_exists(
  file.path(root, "data", "final", paste0("death_cause_final_", suffix), "death_cause_final.csv"),
  file.path(out_dir, "death_cause_final_no_redistribution.csv")
)
copy_if_exists(
  file.path(root, "data", "final", paste0("mortality_rate_cause_smoothed_reconciled_", suffix), "mortality_rate_cause_smoothed_reconciled.csv"),
  file.path(out_dir, "mortality_rate_cause_smoothed_reconciled_no_redistribution.csv")
)
copy_if_exists(
  file.path(root, "data", "final", paste0("avp_yll_cause_reconciled_", suffix), "avp_yll_cause_reconciled.csv"),
  file.path(out_dir, "avp_yll_cause_reconciled_no_redistribution.csv")
)
copy_if_exists(
  file.path(root, "data", "derived", "qc", paste0("map_and_redistribute_deaths_", suffix), "qc_balance_blocks.csv"),
  file.path(qc_dir, "qc_no_redistribution_mass_balance.csv")
)
copy_if_exists(
  file.path(root, "data", "derived", "qc", paste0("map_and_redistribute_deaths_", suffix), "qc_sensitivity_garbage_deleted.csv"),
  file.path(qc_dir, "qc_no_redistribution_deleted_garbage.csv")
)

year_sex_balance <- data.table()
canon_leaf_path <- file.path(root, "data", "final", "death_cause_leaf_post_redistribution", "death_cause_leaf_post_redistribution.csv")
sense_leaf_path <- file.path(root, "data", "final", paste0("death_cause_leaf_post_redistribution_", suffix), "death_cause_leaf_post_redistribution.csv")
if (file.exists(canon_leaf_path) && file.exists(sense_leaf_path)) {
  canon <- fread(canon_leaf_path, select = c("year_id", "sex_id", "deaths"))
  sens <- fread(sense_leaf_path, select = c("year_id", "sex_id", "deaths"))
  year_sex_balance <- merge(
    canon[, .(deaths_canonical = sum(deaths, na.rm = TRUE)), by = .(year_id, sex_id)],
    sens[, .(deaths_no_redistribution = sum(deaths, na.rm = TRUE)), by = .(year_id, sex_id)],
    by = c("year_id", "sex_id"),
    all = TRUE
  )
  year_sex_balance[is.na(year_sex_balance)] <- 0
  year_sex_balance[, deaths_removed_garbage := deaths_canonical - deaths_no_redistribution]
  fwrite(year_sex_balance, file.path(qc_dir, "qc_no_redistribution_year_sex_balance.csv"))
}

writeLines(
  c(
    paste("suffix:", suffix),
    paste("completed_at:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  ),
  file.path(out_dir, "run_log.txt")
)
