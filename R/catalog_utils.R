# R/catalog_utils.R
library(data.table)
library(digest)
library(here)

catalog_paths <- function() {
  list(
    catalog_dir = here("data", "_catalog"),
    artifacts_csv = here("data", "_catalog", "catalogo_artefactos.csv"),
    runs_csv = here("data", "_catalog", "provenance_runs.csv"),
    fallback_dir = here("data", "_catalog", "_locked_fallback")
  )
}

safe_read_catalog <- function(path, empty_dt) {
  if (!file.exists(path)) return(copy(empty_dt))
  tryCatch(
    fread(path),
    error = function(e) {
      warning(sprintf("No se pudo leer el catalogo '%s': %s", path, conditionMessage(e)))
      copy(empty_dt)
    }
  )
}

safe_write_catalog <- function(dt, path, label) {
  p <- catalog_paths()
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  dir.create(p$fallback_dir, recursive = TRUE, showWarnings = FALSE)
  
  ok <- tryCatch({
    fwrite(dt, path)
    TRUE
  }, error = function(e) {
    fallback_path <- file.path(
      p$fallback_dir,
      sprintf(
        "%s__%s.csv",
        label,
        format(Sys.time(), "%Y%m%d_%H%M%S")
      )
    )
    fwrite(dt, fallback_path)
    warning(
      sprintf(
        "No se pudo escribir '%s' por bloqueo/permisos. Se guardo fallback en '%s'. Error: %s",
        path,
        fallback_path,
        conditionMessage(e)
      )
    )
    FALSE
  })
  
  invisible(ok)
}

ensure_catalog_files <- function() {
  p <- catalog_paths()
  dir.create(p$catalog_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(p$fallback_dir, recursive = TRUE, showWarnings = FALSE)
  
  if (!file.exists(p$artifacts_csv)) {
    safe_write_catalog(
      data.table(
        dataset_id = character(),
        table_name = character(),
        version = character(),
        run_id = character(),
        artifact_type = character(),      # final_dataset | dictionary_ext | qc | report | staging | spec | master
        artifact_path = character(),
        file_ext = character(),
        n_rows = integer(),
        n_cols = integer(),
        file_hash = character(),
        created_at = character(),
        notes = character()
      ),
      p$artifacts_csv,
      "catalogo_artefactos_init"
    )
  }
  
  if (!file.exists(p$runs_csv)) {
    safe_write_catalog(
      data.table(
        run_id = character(),
        dataset_id = character(),
        version = character(),
        started_at = character(),
        finished_at = character(),
        status = character(),             # success | failed | running
        message = character()
      ),
      p$runs_csv,
      "provenance_runs_init"
    )
  }
  
  invisible(TRUE)
}

# Normaliza schema del runs catalog (por si existe viejo con tipos incorrectos)
normalize_runs_schema <- function(runs_dt) {
  wanted <- c("run_id","dataset_id","version","started_at","finished_at","status","message")
  
  # asegurar columnas
  for (nm in wanted) if (!nm %in% names(runs_dt)) runs_dt[, (nm) := NA_character_]
  
  # castear a character
  for (nm in wanted) runs_dt[, (nm) := as.character(get(nm))]
  
  runs_dt[, ..wanted]
}

file_hash_md5 <- function(path) {
  if (!file.exists(path)) return(NA_character_)
  digest::digest(file = path, algo = "md5")
}

register_artifact <- function(dataset_id, table_name, version, run_id,
                              artifact_type, artifact_path,
                              n_rows = NA_integer_, n_cols = NA_integer_,
                              notes = NA_character_) {
  ensure_catalog_files()
  p <- catalog_paths()
  
  ext <- tools::file_ext(artifact_path)
  h <- file_hash_md5(artifact_path)
  
  row <- data.table(
    dataset_id = dataset_id,
    table_name = table_name,
    version = version,
    run_id = run_id,
    artifact_type = artifact_type,
    artifact_path = normalizePath(artifact_path, winslash = "/", mustWork = FALSE),
    file_ext = ext,
    n_rows = as.integer(n_rows),
    n_cols = as.integer(n_cols),
    file_hash = h,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    notes = notes
  )
  
  cat_dt <- safe_read_catalog(
    p$artifacts_csv,
    data.table(
      dataset_id = character(),
      table_name = character(),
      version = character(),
      run_id = character(),
      artifact_type = character(),
      artifact_path = character(),
      file_ext = character(),
      n_rows = integer(),
      n_cols = integer(),
      file_hash = character(),
      created_at = character(),
      notes = character()
    )
  )
  cat_dt <- rbind(cat_dt, row, fill = TRUE)
  safe_write_catalog(cat_dt, p$artifacts_csv, "catalogo_artefactos")
  invisible(row)
}

register_run_start <- function(run_id, dataset_id, version) {
  ensure_catalog_files()
  p <- catalog_paths()
  
  runs <- safe_read_catalog(
    p$runs_csv,
    data.table(
      run_id = character(),
      dataset_id = character(),
      version = character(),
      started_at = character(),
      finished_at = character(),
      status = character(),
      message = character()
    )
  )
  runs <- normalize_runs_schema(runs)
  
  runs <- rbind(
    runs,
    data.table(
      run_id = as.character(run_id),
      dataset_id = as.character(dataset_id),
      version = as.character(version),
      started_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      finished_at = NA_character_,
      status = "running",
      message = NA_character_
    ),
    fill = TRUE
  )
  
  runs <- normalize_runs_schema(runs)
  safe_write_catalog(runs, p$runs_csv, "provenance_runs")
}

register_run_finish <- function(run_id, status = c("success","failed"), message = NA_character_) {
  status <- match.arg(status)
  p <- catalog_paths()
  
  runs <- safe_read_catalog(
    p$runs_csv,
    data.table(
      run_id = character(),
      dataset_id = character(),
      version = character(),
      started_at = character(),
      finished_at = character(),
      status = character(),
      message = character()
    )
  )
  runs <- normalize_runs_schema(runs)
  
  idx <- which(runs$run_id == as.character(run_id))
  if (length(idx) == 0) return(invisible(FALSE))
  
  runs[idx, `:=`(
    finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    status = status,
    message = as.character(message)
  )]
  
  runs <- normalize_runs_schema(runs)
  safe_write_catalog(runs, p$runs_csv, "provenance_runs")
  invisible(TRUE)
}
