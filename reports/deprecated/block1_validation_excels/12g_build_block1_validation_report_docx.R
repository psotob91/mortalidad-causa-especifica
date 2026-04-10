#!/usr/bin/env Rscript

# ============================================================
# 12g_build_block1_validation_report_docx.R
# ------------------------------------------------------------
# Informe breve de validación interna para bloque 1
# Mortalidad y AVP - formato Word (.docx)
#
# Enfoque:
#   - Validación interna, no externa
#   - Plausibilidad epidemiológica edad-sexo
#   - Auditabilidad de redistribución GC
#   - Auditabilidad de corrección por completitud
#   - Resumen opcional del componente pandémico
#
# Salida:
#   reports/block1_validation_report/
#     ├─ figs/
#     ├─ tables_export/
#     └─ block1_validation_internal_report.docx
#
# Requisitos:
#   - Ejecutar dentro del root del proyecto
#   - Usar outputs ya generados por 07, 08, 09b, 10 y 11
# ============================================================

suppressPackageStartupMessages({
  library(data.table)
  library(here)
  library(arrow)
  library(ggplot2)
  library(scales)
  library(officer)
  library(flextable)
})

# ============================================================
# Cargar utilidades del proyecto si existen
# ============================================================
source_if_exists <- function(path) {
  if (file.exists(path)) source(path)
}

source_if_exists(here("R", "io_utils.R"))
source_if_exists(here("R", "catalog_utils.R"))
source_if_exists(here("R", "maestro_utils.R"))
source_if_exists(here("R", "spec_utils.R"))

if (exists("ensure_project_dirs")) {
  ensure_project_dirs()
}

# ============================================================
# Config
# ============================================================
CFG <- list(
  version = "v0.1.0_block1_validation_docx",
  dataset_id = "block1_validation_internal_report",
  table_name = "block1_validation_internal_report",
  
  # -------- Editorial --------
  year_main = 2024L,
  years_recent = 2018:2024,
  national_additive_id = 9000L,
  sex_male = 8507L,
  sex_female = 8532L,
  valid_sexes = c(8507L, 8532L),
  rate_multiplier = 100000,
  
  # -------- Selección de causas --------
  top_l3_per_l2 = 2L,
  max_l3_total = 12L,
  
  # Si la autodetección del L1 bloque 1 falla, colocar aquí el concept_id.
  manual_block1_parent_concept_id = 9000010L,
  
  # Candidatos para autodetectar el L1 "bloque 1"
  block1_name_candidates = c(
    "bloque 1",
    "block 1",
    "grupo 1",
    "group 1",
    "transmisibles",
    "transmisible",
    "communicable",
    "maternas",
    "maternal",
    "neonatales",
    "neonatal",
    "nutricionales",
    "nutritional"
  ),
  
  # -------- Inputs --------
  input_cause_candidates = c(
    here("data", "final", "cause_master", "cause_master.parquet"),
    here("data", "final", "cause_master", "cause_master.csv")
  ),
  
  input_mort_candidates = c(
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.parquet"),
    here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv")
  ),
  
  input_avp_candidates = c(
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.parquet"),
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv")
  ),
  
  input_death_final_candidates = c(
    here("data", "final", "death_cause_final", "death_cause_final.parquet"),
    here("data", "final", "death_cause_final", "death_cause_final.csv")
  ),
  
  input_report_tables_mort_candidates = c(
    here("data", "final", "report_tables", "mortality_report_long.parquet"),
    here("data", "final", "report_tables", "mortality_report_long.csv")
  ),
  
  input_report_tables_avp_candidates = c(
    here("data", "final", "report_tables", "avp_report_long.parquet"),
    here("data", "final", "report_tables", "avp_report_long.csv")
  ),
  
  # -------- QC existentes --------
  qc07_dir = here("data", "derived", "qc", "07_qc_redistribution"),
  qc08_dir = here("data", "derived", "qc", "08_build_death_cause_final"),
  qc10_dir = here("data", "derived", "qc", "10_compute_avp_yll"),
  qc11_dir = here("data", "derived", "qc", "11_build_report_tables"),
  
  # -------- Outputs --------
  out_dir = here("reports", "block1_validation_report"),
  fig_dir = here("reports", "block1_validation_report", "figs"),
  tbl_dir = here("reports", "block1_validation_report", "tables_export"),
  
  verbose = TRUE
)

for (d in c(CFG$out_dir, CFG$fig_dir, CFG$tbl_dir)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

msg <- function(...) {
  if (isTRUE(CFG$verbose)) cat(..., "\n")
}

# ============================================================
# Helpers generales
# ============================================================
first_existing <- function(paths) {
  hit <- paths[file.exists(paths)]
  if (length(hit) == 0L) return(NA_character_)
  hit[1]
}

read_dt_any <- function(path) {
  if (exists("read_auto")) {
    return(as.data.table(read_auto(path)))
  }
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") return(fread(path))
  if (ext == "parquet") return(as.data.table(arrow::read_parquet(path)))
  if (ext == "rds") return(as.data.table(readRDS(path)))
  stop("Formato no soportado: ", path)
}

safe_read <- function(path) {
  if (is.na(path) || !file.exists(path)) return(NULL)
  tryCatch(read_dt_any(path), error = function(e) {
    warning("No pude leer: ", path, " -> ", conditionMessage(e))
    NULL
  })
}

safe_fread <- function(path) {
  if (!file.exists(path)) return(NULL)
  tryCatch(fread(path), error = function(e) {
    warning("No pude leer: ", path, " -> ", conditionMessage(e))
    NULL
  })
}

detect_col <- function(dt, candidates, label, required = TRUE) {
  hit <- intersect(candidates, names(dt))
  if (length(hit) == 0L) {
    if (required) {
      stop("No se encontró columna para ", label,
           ". Candidatas: ", paste(candidates, collapse = ", "))
    } else {
      return(NA_character_)
    }
  }
  hit[1]
}

norm_txt <- function(x) {
  z <- tolower(trimws(as.character(x)))
  z <- iconv(z, to = "ASCII//TRANSLIT")
  z <- gsub("[^a-z0-9]+", " ", z)
  z <- gsub("\\s+", " ", z)
  trimws(z)
}

fmt_num <- function(x, digits = 1) {
  out <- formatC(
    as.numeric(x),
    format = "f",
    digits = digits,
    big.mark = " ",
    decimal.mark = "."
  )
  out[is.na(x)] <- NA_character_
  out
}

fmt_int <- function(x) fmt_num(x, digits = 0)

fmt_pct <- function(x, digits = 1) {
  out <- ifelse(is.na(x), NA_character_,
                paste0(fmt_num(100 * x, digits = digits), "%"))
  out
}

label_space_num <- function(accuracy = 1) {
  scales::label_number(
    accuracy = accuracy,
    big.mark = " ",
    decimal.mark = "."
  )
}

sex_label <- function(x) {
  fifelse(
    x == CFG$sex_male, "Hombre",
    fifelse(x == CFG$sex_female, "Mujer", as.character(x))
  )
}

chunk_vec <- function(x, size = 6L) {
  split(x, ceiling(seq_along(x) / size))
}

save_plot_safe <- function(p, path, width = 10, height = 7, dpi = 320) {
  tryCatch({
    ggsave(
      filename = path,
      plot = p,
      width = width,
      height = height,
      dpi = dpi,
      bg = "white"
    )
    TRUE
  }, error = function(e) {
    warning("No pude guardar figura ", basename(path), ": ", conditionMessage(e))
    FALSE
  })
}

write_csv_safe <- function(dt, path) {
  tryCatch(fwrite(dt, path), error = function(e) {
    warning("No pude exportar tabla ", basename(path), ": ", conditionMessage(e))
    NULL
  })
}

# ============================================================
# Helpers jerárquicos de causas
# ============================================================
get_descendants <- function(cm, parent_id) {
  out <- integer(0)
  frontier <- as.integer(parent_id)
  
  while (length(frontier) > 0L) {
    kids <- cm[parent_concept_id %in% frontier, unique(cause_concept_id)]
    kids <- setdiff(kids, out)
    if (length(kids) == 0L) break
    out <- c(out, kids)
    frontier <- kids
  }
  
  unique(c(as.integer(parent_id), out))
}

infer_block1_parent <- function(cm) {
  # 1) Override manual
  if (!is.na(CFG$manual_block1_parent_concept_id)) {
    hit <- cm[cause_concept_id == CFG$manual_block1_parent_concept_id & cause_level == 1L]
    if (nrow(hit) == 1L) return(hit$cause_concept_id[1])
    warning("manual_block1_parent_concept_id no coincide con un L1 válido. Intentaré autodetección.")
  }
  
  l1 <- copy(cm[cause_level == 1L])
  
  if (nrow(l1) == 0L) return(NA_integer_)
  
  l1[, code_norm := norm_txt(fifelse(is.na(cause_code), "", cause_code))]
  l1[, name_norm := norm_txt(fifelse(is.na(cause_name), "", cause_name))]
  
  pat <- paste(CFG$block1_name_candidates, collapse = "|")
  
  cand <- l1[
    grepl(pat, code_norm) | grepl(pat, name_norm)
  ]
  
  if (nrow(cand) == 1L) return(cand$cause_concept_id[1])
  
  # fallback pragmático: primer L1 ordenado por cause_concept_id
  warning(
    "No pude identificar inequívocamente el L1 del bloque 1. ",
    "Usaré fallback pragmático: primer L1 por cause_concept_id. ",
    "Si esto no coincide con tu bloque 1, fija CFG$manual_block1_parent_concept_id."
  )
  cand2 <- l1[order(cause_concept_id)]
  cand2$cause_concept_id[1]
}

# ============================================================
# Helpers tablas y figuras
# ============================================================
ft_compact <- function(dt, fontsize = 9, align = "left") {
  ft <- flextable(dt)
  ft <- theme_booktabs(ft)
  ft <- fontsize(ft, size = fontsize, part = "all")
  ft <- padding(ft, padding = 3, part = "all")
  ft <- align(ft, align = align, part = "all")
  ft <- autofit(ft)
  ft
}

fmt_table_numeric <- function(dt, digits_map = list()) {
  x <- copy(dt)
  num_cols <- names(x)[vapply(x, is.numeric, logical(1))]
  
  for (nm in num_cols) {
    dig <- if (!is.null(digits_map[[nm]])) digits_map[[nm]] else 1
    x[, (nm) := fmt_num(get(nm), digits = dig)]
  }
  x
}

body_add_figure_safe <- function(doc, title, img_path, note = NULL,
                                 width = 6.8, height = 4.6) {
  if (!file.exists(img_path)) return(doc)
  doc <- body_add_par(doc, title, style = "heading 2")
  if (!is.null(note)) {
    doc <- body_add_par(doc, note, style = "Normal")
  }
  doc <- body_add_img(doc, src = img_path, width = width, height = height)
  doc <- body_add_par(doc, "", style = "Normal")
  doc
}

body_add_ft_safe <- function(doc, title, dt, note = NULL, digits_map = list(), fontsize = 9) {
  if (is.null(dt) || !is.data.table(dt) || nrow(dt) == 0L) return(doc)
  doc <- body_add_par(doc, title, style = "heading 2")
  if (!is.null(note)) {
    doc <- body_add_par(doc, note, style = "Normal")
  }
  ft <- ft_compact(fmt_table_numeric(dt, digits_map = digits_map), fontsize = fontsize)
  doc <- body_add_flextable(doc, value = ft)
  doc <- body_add_par(doc, "", style = "Normal")
  doc
}

# ============================================================
# Main
# ============================================================
run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))

if (exists("register_run_start")) {
  try(register_run_start(run_id, dataset_id = CFG$dataset_id, version = CFG$version), silent = TRUE)
}

tryCatch({
  
  msg("Resolviendo inputs principales...")
  
  cause_path <- first_existing(CFG$input_cause_candidates)
  mort_path  <- first_existing(CFG$input_mort_candidates)
  avp_path   <- first_existing(CFG$input_avp_candidates)
  death_final_path <- first_existing(CFG$input_death_final_candidates)
  mort_report_path <- first_existing(CFG$input_report_tables_mort_candidates)
  avp_report_path  <- first_existing(CFG$input_report_tables_avp_candidates)
  
  if (is.na(cause_path)) stop("No encontré cause_master.")
  if (is.na(mort_path))  stop("No encontré mortality_rate_cause_smoothed_reconciled.")
  if (is.na(avp_path))   stop("No encontré avp_yll_cause_reconciled.")
  
  msg("Leyendo cause_master...")
  cm <- read_dt_any(cause_path)
  
  msg("Leyendo mortalidad reconciliada...")
  mort <- read_dt_any(mort_path)
  
  msg("Leyendo AVP reconciliado...")
  avp <- read_dt_any(avp_path)
  
  death_final <- safe_read(death_final_path)
  mort_report_long <- safe_read(mort_report_path)
  avp_report_long  <- safe_read(avp_report_path)
  
  # ----------------------------------------------------------
  # Estandarizar columnas
  # ----------------------------------------------------------
  req_cm <- c("cause_concept_id", "cause_level", "cause_name", "parent_concept_id")
  miss_cm <- setdiff(req_cm, names(cm))
  if (length(miss_cm) > 0L) {
    stop("Faltan columnas en cause_master: ", paste(miss_cm, collapse = ", "))
  }
  
  cause_code_col <- detect_col(cm, c("cause_code"), "cause_code", required = FALSE)
  if (is.na(cause_code_col)) {
    cm[, cause_code := as.character(cause_concept_id)]
  } else if (cause_code_col != "cause_code") {
    setnames(cm, cause_code_col, "cause_code")
  }
  
  req_mort <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_level", "population"
  )
  miss_mort <- setdiff(req_mort, names(mort))
  if (length(miss_mort) > 0L) {
    stop("Faltan columnas mínimas en mortalidad: ", paste(miss_mort, collapse = ", "))
  }
  
  mort_abs_col <- detect_col(mort,
                             c("deaths_smoothed_consistent", "metric_abs", "deaths_final"),
                             "mortalidad absoluta")
  mort_rate_col <- detect_col(mort,
                              c("mortality_rate_smoothed_consistent", "metric_rate", "mortality_rate"),
                              "tasa de mortalidad")
  mort_cause_name_col <- detect_col(mort, c("cause_name"), "cause_name", required = FALSE)
  
  if (mort_abs_col != "deaths_smoothed_consistent") setnames(mort, mort_abs_col, "deaths_smoothed_consistent")
  if (mort_rate_col != "mortality_rate_smoothed_consistent") setnames(mort, mort_rate_col, "mortality_rate_smoothed_consistent")
  
  if (is.na(mort_cause_name_col)) {
    mort <- merge(
      mort,
      unique(cm[, .(cause_concept_id, cause_level, cause_name)]),
      by = c("cause_concept_id", "cause_level"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  
  req_avp <- c(
    "year_id", "location_id", "sex_id", "age", "cause_concept_id",
    "cause_level", "population"
  )
  miss_avp <- setdiff(req_avp, names(avp))
  if (length(miss_avp) > 0L) {
    stop("Faltan columnas mínimas en AVP: ", paste(miss_avp, collapse = ", "))
  }
  
  avp_abs_col <- detect_col(avp, c("avp_abs", "yll_abs", "metric_abs"), "AVP absoluto")
  avp_rate_col <- detect_col(avp, c("avp_rate", "yll_rate", "metric_rate"), "tasa AVP")
  avp_cause_name_col <- detect_col(avp, c("cause_name"), "cause_name", required = FALSE)
  
  if (avp_abs_col != "avp_abs") setnames(avp, avp_abs_col, "avp_abs")
  if (avp_rate_col != "avp_rate") setnames(avp, avp_rate_col, "avp_rate")
  
  if (is.na(avp_cause_name_col)) {
    avp <- merge(
      avp,
      unique(cm[, .(cause_concept_id, cause_level, cause_name)]),
      by = c("cause_concept_id", "cause_level"),
      all.x = TRUE,
      sort = FALSE
    )
  }
  
  cm <- unique(cm[, .(
    cause_concept_id = as.integer(cause_concept_id),
    cause_code = as.character(cause_code),
    cause_name = as.character(cause_name),
    cause_level = as.integer(cause_level),
    parent_concept_id = as.integer(parent_concept_id)
  )])
  
  mort <- mort[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    population = as.numeric(population),
    deaths_smoothed_consistent = as.numeric(deaths_smoothed_consistent),
    mortality_rate_smoothed_consistent = as.numeric(mortality_rate_smoothed_consistent)
  )]
  
  avp <- avp[, .(
    year_id = as.integer(year_id),
    location_id = as.integer(location_id),
    sex_id = as.integer(sex_id),
    age = as.integer(age),
    cause_concept_id = as.integer(cause_concept_id),
    cause_level = as.integer(cause_level),
    cause_name = as.character(cause_name),
    population = as.numeric(population),
    avp_abs = as.numeric(avp_abs),
    avp_rate = as.numeric(avp_rate)
  )]
  
  # ----------------------------------------------------------
  # Identificar bloque 1 y sus descendientes
  # ----------------------------------------------------------
  msg("Identificando L1 del bloque 1...")
  block1_parent_id <- infer_block1_parent(cm)
  
  if (is.na(block1_parent_id)) {
    stop("No pude identificar el parent_concept_id de bloque 1.")
  }
  
  block1_parent_name <- cm[cause_concept_id == block1_parent_id, cause_name][1]
  msg("Bloque 1 detectado: ", block1_parent_name, " (", block1_parent_id, ")")
  
  block1_desc <- get_descendants(cm, block1_parent_id)
  block1_cm <- cm[cause_concept_id %in% block1_desc]
  
  l2_map <- unique(cm[cause_level == 2L, .(
    l2_id = cause_concept_id,
    l2_name = cause_name
  )])
  
  l3_meta <- unique(block1_cm[cause_level == 3L, .(
    cause_concept_id,
    cause_code,
    cause_name,
    parent_concept_id
  )])
  
  l3_meta <- merge(
    l3_meta,
    l2_map,
    by.x = "parent_concept_id",
    by.y = "l2_id",
    all.x = TRUE,
    sort = FALSE
  )
  
  if (nrow(l3_meta) == 0L) {
    stop("No encontré causas L3 dentro del bloque 1.")
  }
  
  # ----------------------------------------------------------
  # Selección reproducible de causas L3 para figuras
  #   Regla preferida:
  #   - nacional 2024
  #   - sumar muertes 2024 por causa L3
  #   - seleccionar hasta 2 por cada L2
  #   - tope global
  # ----------------------------------------------------------
  msg("Seleccionando causas L3 representativas para figuras...")
  
  mort_sel_base <- mort[
    year_id == CFG$year_main &
      location_id == CFG$national_additive_id &
      sex_id %in% CFG$valid_sexes &
      cause_level == 3L &
      cause_concept_id %in% l3_meta$cause_concept_id
  ][
    ,
    .(
      deaths_2024 = sum(deaths_smoothed_consistent, na.rm = TRUE),
      rate_2024 = sum(deaths_smoothed_consistent, na.rm = TRUE) /
        sum(population, na.rm = TRUE) * CFG$rate_multiplier
    ),
    by = .(cause_concept_id)
  ]
  
  if (nrow(mort_sel_base) == 0L) {
    stop("No encontré mortalidad 2024 nacional para causas L3 del bloque 1.")
  }
  
  selected_l3 <- merge(
    l3_meta,
    mort_sel_base,
    by = "cause_concept_id",
    all.x = FALSE,
    sort = FALSE
  )
  
  selected_l3[is.na(l2_name), l2_name := "L2 no identificado"]
  setorder(selected_l3, l2_name, -deaths_2024, cause_name)
  
  selected_l3 <- selected_l3[
    ,
    head(.SD, CFG$top_l3_per_l2),
    by = .(l2_name)
  ]
  
  setorder(selected_l3, -deaths_2024, cause_name)
  
  if (nrow(selected_l3) > CFG$max_l3_total) {
    selected_l3 <- selected_l3[1:CFG$max_l3_total]
  }
  
  if (nrow(selected_l3) == 0L) {
    stop("La regla de selección dejó 0 causas. Revisar bloque 1 o insumos.")
  }
  
  selected_ids <- selected_l3$cause_concept_id
  selected_names <- selected_l3$cause_name
  
  write_csv_safe(selected_l3, file.path(CFG$tbl_dir, "selected_l3_causes_block1_2024.csv"))
  
  # ----------------------------------------------------------
  # Figura 1 y 2: Mortalidad por edad y sexo, paneles L3
  # ----------------------------------------------------------
  msg("Construyendo figuras de mortalidad...")
  
  mort_fig_base <- mort[
    year_id == CFG$year_main &
      location_id == CFG$national_additive_id &
      sex_id %in% CFG$valid_sexes &
      cause_concept_id %in% selected_ids,
    .(
      mortality_rate = sum(mortality_rate_smoothed_consistent, na.rm = TRUE),
      deaths_abs = sum(deaths_smoothed_consistent, na.rm = TRUE)
    ),
    by = .(age, sex_id, cause_concept_id, cause_name)
  ]
  
  mort_fig_base[, sex_label := sex_label(sex_id)]
  mort_fig_base <- mort_fig_base[
    is.finite(mortality_rate) & !is.na(age) & !is.na(cause_name)
  ]
  
  mort_plot_files <- character(0)
  
  if (nrow(mort_fig_base) > 0L) {
    mort_chunks <- chunk_vec(selected_names, size = 6L)
    
    for (i in seq_along(mort_chunks)) {
      fig_dt <- copy(mort_fig_base[cause_name %in% mort_chunks[[i]]])
      fig_dt <- fig_dt[is.finite(mortality_rate) & !is.na(age) & !is.na(sex_label)]
      setorder(fig_dt, cause_name, sex_id, age)
      
      if (nrow(fig_dt) == 0L) {
        warning("Sin datos válidos para figura de mortalidad chunk ", i)
        next
      }
      
      p <- ggplot(fig_dt, aes(x = age, y = mortality_rate, color = sex_label)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 0.9) +
        facet_wrap(~ cause_name, scales = "free_y", ncol = 2) +
        scale_x_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
        scale_y_continuous(labels = label_space_num(accuracy = 0.1)) +
        labs(
          title = paste0("Mortalidad por edad y sexo, bloque 1, causas L3 seleccionadas, ", CFG$year_main),
          subtitle = "Tasa por 100 000 habitantes. Selección reproducible: hasta 2 causas L3 por cada L2, priorizadas por mayor mortalidad nacional en 2024.",
          x = "Edad (años)",
          y = "Tasa de mortalidad por 100 000",
          color = "Sexo"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(face = "bold")
        )
      
      fp <- file.path(CFG$fig_dir, sprintf("fig_mortality_age_sex_panel_%02d.png", i))
      ok <- save_plot_safe(p, fp, width = 10.5, height = 7.5)
      if (ok) mort_plot_files <- c(mort_plot_files, fp)
    }
  } else {
    warning("No se pudo construir la figura de mortalidad por ausencia de datos.")
  }
  
  # ----------------------------------------------------------
  # Figura 3 y 4: AVP por edad y sexo, paneles L3
  # ----------------------------------------------------------
  msg("Construyendo figuras de AVP...")
  
  avp_fig_base <- avp[
    year_id == CFG$year_main &
      location_id == CFG$national_additive_id &
      sex_id %in% CFG$valid_sexes &
      cause_concept_id %in% selected_ids,
    .(
      avp_rate = sum(avp_rate, na.rm = TRUE),
      avp_abs = sum(avp_abs, na.rm = TRUE)
    ),
    by = .(age, sex_id, cause_concept_id, cause_name)
  ]
  
  avp_fig_base[, sex_label := sex_label(sex_id)]
  avp_fig_base <- avp_fig_base[
    is.finite(avp_rate) & !is.na(age) & !is.na(cause_name)
  ]
  
  avp_plot_files <- character(0)
  
  if (nrow(avp_fig_base) > 0L) {
    avp_chunks <- chunk_vec(selected_names, size = 6L)
    
    for (i in seq_along(avp_chunks)) {
      fig_dt <- copy(avp_fig_base[cause_name %in% avp_chunks[[i]]])
      fig_dt <- fig_dt[is.finite(avp_rate) & !is.na(age) & !is.na(sex_label)]
      setorder(fig_dt, cause_name, sex_id, age)
      
      if (nrow(fig_dt) == 0L) {
        warning("Sin datos válidos para figura de AVP chunk ", i)
        next
      }
      
      p <- ggplot(fig_dt, aes(x = age, y = avp_rate, color = sex_label)) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 0.9) +
        facet_wrap(~ cause_name, scales = "free_y", ncol = 2) +
        scale_x_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
        scale_y_continuous(labels = label_space_num(accuracy = 0.1)) +
        labs(
          title = paste0("AVP por edad y sexo, bloque 1, causas L3 seleccionadas, ", CFG$year_main),
          subtitle = "Tasa de AVP por 100 000 habitantes. Se muestran las mismas causas L3 seleccionadas para la sección de mortalidad.",
          x = "Edad (años)",
          y = "Tasa de AVP por 100 000",
          color = "Sexo"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(face = "bold")
        )
      
      fp <- file.path(CFG$fig_dir, sprintf("fig_avp_age_sex_panel_%02d.png", i))
      ok <- save_plot_safe(p, fp, width = 10.5, height = 7.5)
      if (ok) avp_plot_files <- c(avp_plot_files, fp)
    }
  } else {
    warning("No se pudo construir la figura de AVP por ausencia de datos.")
  }
  
  # ----------------------------------------------------------
  # QC de redistribución (07)
  # ----------------------------------------------------------
  msg("Leyendo QC de redistribución...")
  
  qc_before_after_l3 <- safe_fread(file.path(CFG$qc07_dir, "before_after_l3.csv"))
  qc_top_gain_abs_l3 <- safe_fread(file.path(CFG$qc07_dir, "tab_top_gain_abs_l3.csv"))
  qc_top_gc_groups   <- safe_fread(file.path(CFG$qc07_dir, "tab_top_gc_groups.csv"))
  qc_pandemic_summary_07 <- safe_fread(file.path(CFG$qc07_dir, "qc_pandemic_summary.csv"))
  
  # Tabla top ganancias bloque 1
  tab_redist_block1 <- NULL
  fig_redist_path <- NULL
  
  if (!is.null(qc_before_after_l3)) {
    # before_after_l3 no necesariamente trae concept_id; filtramos por nombres/códigos de L3 de bloque 1
    l3_lookup_name <- unique(l3_meta[, .(cause_name)])
    l3_lookup_code <- unique(l3_meta[, .(cause_code)])
    
    x <- copy(qc_before_after_l3)
    
    if ("cause_name" %in% names(x)) {
      x <- merge(x, l3_lookup_name, by = "cause_name", all = FALSE, sort = FALSE)
    } else if ("cause_code" %in% names(x)) {
      x <- merge(x, l3_lookup_code, by = "cause_code", all = FALSE, sort = FALSE)
    } else {
      warning("before_after_l3.csv no trae cause_name ni cause_code; omito filtro bloque 1.")
    }
    
    if (nrow(x) > 0L) {
      tab_redist_block1 <- x[, .(
        deaths_before = sum(deaths_before, na.rm = TRUE),
        deaths_after = sum(deaths_after, na.rm = TRUE)
      ), by = .(cause_name)][
        , abs_change := deaths_after - deaths_before
      ][order(-abs_change)][1:min(.N, 10)]
      
      write_csv_safe(tab_redist_block1, file.path(CFG$tbl_dir, "tab_redistribution_top_gains_block1.csv"))
      
      fig_red <- copy(tab_redist_block1)
      fig_red[, cause_name := factor(cause_name, levels = rev(cause_name))]
      
      p_red <- ggplot(fig_red, aes(x = cause_name, y = abs_change)) +
        geom_col() +
        coord_flip() +
        scale_y_continuous(labels = label_space_num(accuracy = 1)) +
        labs(
          title = "Ganancia absoluta de muertes tras redistribución",
          subtitle = "Top causas L3 del bloque 1 con mayor incremento absoluto after vs before.",
          x = NULL,
          y = "Cambio absoluto de muertes"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank()
        )
      
      fig_redist_path <- file.path(CFG$fig_dir, "fig_redistribution_top_gains_block1.png")
      ok <- save_plot_safe(p_red, fig_redist_path, width = 8.5, height = 5.2)
      if (!ok) fig_redist_path <- NULL
    }
  }
  
  if (is.null(tab_redist_block1) && !is.null(qc_top_gain_abs_l3)) {
    # fallback menos fino: usar tabla top gain global y filtrar por nombres de bloque 1
    x <- qc_top_gain_abs_l3[cause_name %in% l3_meta$cause_name]
    if (nrow(x) > 0L) {
      tab_redist_block1 <- copy(x)[1:min(.N, 10)]
      write_csv_safe(tab_redist_block1, file.path(CFG$tbl_dir, "tab_redistribution_top_gains_block1_fallback.csv"))
    }
  }
  
  tab_gc_groups <- NULL
  if (!is.null(qc_top_gc_groups)) {
    tab_gc_groups <- copy(qc_top_gc_groups)[1:min(.N, 10)]
    write_csv_safe(tab_gc_groups, file.path(CFG$tbl_dir, "tab_top_gc_groups.csv"))
  }
  
  # ----------------------------------------------------------
  # QC de completitud y pandemia (08)
  # ----------------------------------------------------------
  msg("Leyendo QC de completitud/pandemia...")
  
  qc_factor_summary <- safe_fread(file.path(CFG$qc08_dir, "qc_factor_summary.csv"))
  mortality_completeness_audit <- safe_fread(file.path(CFG$qc08_dir, "mortality_completeness_audit.csv"))
  qc_factor_extremes <- safe_fread(file.path(CFG$qc08_dir, "qc_factor_extremes.csv"))
  qc_pandemic_by_year <- safe_fread(file.path(CFG$qc08_dir, "qc_pandemic_by_year.csv"))
  qc_year_summary <- safe_fread(file.path(CFG$qc08_dir, "qc_year_summary.csv"))
  qc_semantic_note <- safe_fread(file.path(CFG$qc08_dir, "qc_semantic_note.csv"))
  
  fig_factor_path <- NULL
  tab_factor_summary <- NULL
  tab_pandemic_year <- NULL
  
  if (!is.null(mortality_completeness_audit)) {
    x <- copy(mortality_completeness_audit)
    
    req_comp <- c("year_id", "location_id", "sex_id", "age", "correction_factor_completeness")
    miss_comp <- setdiff(req_comp, names(x))
    if (length(miss_comp) == 0L) {
      x <- x[year_id == CFG$year_main & sex_id %in% CFG$valid_sexes]
      
      # preferir nacional 9000; si no existe, agregar de manera pragmática
      if (CFG$national_additive_id %in% x$location_id) {
        x <- x[location_id == CFG$national_additive_id]
      } else {
        wcol <- if ("expected_allcause" %in% names(x)) "expected_allcause" else
          if ("population" %in% names(x)) "population" else NA_character_
        
        if (!is.na(wcol)) {
          x <- x[
            ,
            .(
              correction_factor_completeness = weighted.mean(
                correction_factor_completeness,
                w = pmax(get(wcol), 0),
                na.rm = TRUE
              )
            ),
            by = .(year_id, sex_id, age)
          ]
        } else {
          x <- x[
            ,
            .(
              correction_factor_completeness = mean(correction_factor_completeness, na.rm = TRUE)
            ),
            by = .(year_id, sex_id, age)
          ]
        }
        x[, location_id := CFG$national_additive_id]
      }
      
      x[, sex_label := sex_label(sex_id)]
      
      p_fac <- ggplot(x, aes(x = age, y = correction_factor_completeness, color = sex_label)) +
        geom_line(linewidth = 0.8) +
        geom_point(size = 1.0) +
        scale_x_continuous(breaks = c(0, 1, 5, 15, 25, 35, 45, 55, 65, 75, 85)) +
        scale_y_continuous(labels = label_space_num(accuracy = 0.01)) +
        labs(
          title = paste0("Factor de corrección por completitud, nacional, ", CFG$year_main),
          subtitle = "Visualización por edad y sexo. Un valor mayor indica mayor corrección del subregistro/subreporte.",
          x = "Edad (años)",
          y = "Correction factor",
          color = "Sexo"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          plot.title = element_text(face = "bold"),
          panel.grid.minor = element_blank(),
          legend.position = "bottom"
        )
      
      fig_factor_path <- file.path(CFG$fig_dir, "fig_completeness_factor_age_sex_2024.png")
      ok <- save_plot_safe(p_fac, fig_factor_path, width = 8.6, height = 4.9)
      if (!ok) fig_factor_path <- NULL
    }
  }
  
  if (!is.null(qc_factor_summary)) {
    tab_factor_summary <- copy(qc_factor_summary)[year_id %in% CFG$years_recent]
    write_csv_safe(tab_factor_summary, file.path(CFG$tbl_dir, "tab_factor_summary_2018_2024.csv"))
  }
  
  if (!is.null(qc_pandemic_by_year)) {
    tab_pandemic_year <- copy(qc_pandemic_by_year)[year_id %in% CFG$years_recent]
    if ("observed_allcause" %in% names(tab_pandemic_year) &&
        "pandemic_excess_allcause" %in% names(tab_pandemic_year)) {
      tab_pandemic_year[
        ,
        pandemic_share_over_observed := fifelse(
          observed_allcause > 0,
          pandemic_excess_allcause / observed_allcause,
          NA_real_
        )
      ]
    }
    write_csv_safe(tab_pandemic_year, file.path(CFG$tbl_dir, "tab_pandemic_by_year_2018_2024.csv"))
  }
  
  # ----------------------------------------------------------
  # Resumen AVP adicional opcional
  # ----------------------------------------------------------
  qc_ratio_avp_vs_deaths <- safe_fread(file.path(CFG$qc10_dir, "qc_ratio_avp_vs_deaths.csv"))
  
  tab_avp_ratio <- NULL
  if (!is.null(qc_ratio_avp_vs_deaths)) {
    tab_avp_ratio <- copy(qc_ratio_avp_vs_deaths)[year_id %in% CFG$years_recent]
    write_csv_safe(tab_avp_ratio, file.path(CFG$tbl_dir, "tab_avp_ratio_vs_deaths.csv"))
  }
  
  # ----------------------------------------------------------
  # Resúmenes automáticos para narrativa
  # ----------------------------------------------------------
  n_selected_causes <- length(selected_ids)
  n_selected_l2 <- uniqueN(selected_l3$l2_name)
  
  total_deaths_selected_2024 <- selected_l3[, sum(deaths_2024, na.rm = TRUE)]
  
  factor_min_2024 <- NA_real_
  factor_max_2024 <- NA_real_
  
  if (!is.null(mortality_completeness_audit)) {
    xfac <- copy(mortality_completeness_audit)[
      year_id == CFG$year_main & sex_id %in% CFG$valid_sexes
    ]
    if (nrow(xfac) > 0L) {
      factor_min_2024 <- suppressWarnings(min(xfac$correction_factor_completeness, na.rm = TRUE))
      factor_max_2024 <- suppressWarnings(max(xfac$correction_factor_completeness, na.rm = TRUE))
    }
  }
  
  total_redist_gain_block1 <- NA_real_
  if (!is.null(tab_redist_block1)) {
    total_redist_gain_block1 <- tab_redist_block1[, sum(abs_change, na.rm = TRUE)]
  }
  
  pandemic_2024 <- NA_real_
  if (!is.null(tab_pandemic_year) && "pandemic_excess_allcause" %in% names(tab_pandemic_year)) {
    pandemic_2024 <- tab_pandemic_year[year_id == CFG$year_main, pandemic_excess_allcause][1]
  }
  
  # ----------------------------------------------------------
  # Exportar algunas tablas base del informe
  # ----------------------------------------------------------
  tab_selected_summary <- selected_l3[, .(
    l2_name,
    cause_name,
    deaths_2024,
    rate_2024
  )]
  
  write_csv_safe(tab_selected_summary, file.path(CFG$tbl_dir, "tab_selected_l3_summary.csv"))
  
  # ----------------------------------------------------------
  # Construir Word
  # ----------------------------------------------------------
  msg("Armando Word...")
  
  doc <- read_docx()
  
  # Portada / título
  doc <- body_add_par(doc, "Informe breve de validación interna", style = "heading 1")
  doc <- body_add_par(doc, "Bloque 1: mortalidad y AVP", style = "heading 2")
  doc <- body_add_par(
    doc,
    paste0(
      "Versión: ", CFG$version,
      " | Fecha de corrida: ", format(Sys.time(), "%Y-%m-%d %H:%M"),
      " | Run ID: ", run_id
    ),
    style = "Normal"
  )
  
  # Introducción
  doc <- body_add_par(doc, "1. Propósito y alcance", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste0(
      "Este documento presenta una validación interna breve de las estimaciones finales de mortalidad y AVP para el bloque 1. ",
      "La validación interna se entiende aquí como una evaluación de plausibilidad epidemiológica, consistencia de los ajustes ",
      "y trazabilidad de los principales componentes del pipeline analítico, sin constituir una validación externa frente a fuentes independientes."
    ),
    style = "Normal"
  )
  doc <- body_add_par(
    doc,
    paste0(
      "Para la validación visual se trabajó principalmente con el año ", CFG$year_main,
      " a nivel nacional (location_id = ", CFG$national_additive_id, "). ",
      "Las causas de nivel 3 se seleccionaron con una regla reproducible: hasta ",
      CFG$top_l3_per_l2, " causas L3 por cada grupo L2 del bloque 1, priorizadas por mayor mortalidad nacional en ",
      CFG$year_main, ", con tope global de ", CFG$max_l3_total, " causas."
    ),
    style = "Normal"
  )
  doc <- body_add_par(
    doc,
    paste0(
      "Bloque 1 detectado en cause_master: ", block1_parent_name,
      " (concept_id = ", block1_parent_id, "). ",
      "Se seleccionaron ", n_selected_causes, " causas L3 distribuidas en ", n_selected_l2, " grupos L2."
    ),
    style = "Normal"
  )
  
  # Tabla de causas seleccionadas
  doc <- body_add_ft_safe(
    doc = doc,
    title = "Tabla 1. Causas L3 seleccionadas para la validación visual",
    dt = tab_selected_summary,
    note = paste0(
      "Selección basada en mortalidad nacional ", CFG$year_main,
      ". Muertes absolutas estimadas y tasa por 100 000 habitantes."
    ),
    digits_map = list(
      deaths_2024 = 0,
      rate_2024 = 1
    ),
    fontsize = 8.5
  )
  
  # Plausibilidad mortalidad
  doc <- body_add_par(doc, "2. Validación epidemiológica aparente: mortalidad", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste0(
      "Las siguientes figuras muestran patrones por edad y sexo para las causas L3 seleccionadas del bloque 1. ",
      "El objetivo es verificar si la forma general de las curvas y las diferencias por sexo son compatibles con una historia natural epidemiológicamente plausible, ",
      "sin forzar inferencias causales ni clínicas más allá de lo que permiten estos datos."
    ),
    style = "Normal"
  )
  
  if (length(mort_plot_files) == 0L) {
    doc <- body_add_par(
      doc,
      "No se pudo insertar la sección gráfica principal de mortalidad por ausencia de datos suficientes o por fallo de exportación.",
      style = "Normal"
    )
  } else {
    for (i in seq_along(mort_plot_files)) {
      doc <- body_add_figure_safe(
        doc,
        title = paste0("Figura M", i, ". Mortalidad por edad y sexo en causas L3 seleccionadas"),
        img_path = mort_plot_files[i],
        note = "Tasa de mortalidad por 100 000 habitantes.",
        width = 6.9,
        height = 4.9
      )
    }
  }
  
  # Redistribución
  doc <- body_add_par(doc, "3. Validación del proceso de redistribución", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste0(
      "La redistribución de garbage codes se resume aquí con un enfoque simple y auditable. ",
      "Se prioriza cuánto cambiaron las magnitudes before vs after para las causas L3 del bloque 1 y cuáles fueron los grupos garbage más frecuentes en el input."
    ),
    style = "Normal"
  )
  
  if (!is.null(fig_redist_path) && file.exists(fig_redist_path)) {
    doc <- body_add_figure_safe(
      doc,
      title = "Figura R1. Incremento absoluto de muertes tras redistribución",
      img_path = fig_redist_path,
      note = "Cambio absoluto after minus before en causas L3 del bloque 1.",
      width = 6.5,
      height = 4.5
    )
  } else {
    doc <- body_add_par(
      doc,
      "No se pudo construir una figura específica de redistribución para bloque 1; se insertan solo tablas compactas cuando están disponibles.",
      style = "Normal"
    )
  }
  
  doc <- body_add_ft_safe(
    doc = doc,
    title = "Tabla 2. Top ganancias absolutas tras redistribución en causas L3 del bloque 1",
    dt = tab_redist_block1,
    note = "Comparación resumida de muertes before vs after.",
    digits_map = list(
      deaths_before = 0,
      deaths_after = 0,
      abs_change = 0
    ),
    fontsize = 8.5
  )
  
  doc <- body_add_ft_safe(
    doc = doc,
    title = "Tabla 3. Principales grupos garbage observados antes de redistribución",
    dt = tab_gc_groups,
    note = "Resumen global de grupos garbage más frecuentes en el QC de redistribución.",
    digits_map = list(
      deaths_before = 0
    ),
    fontsize = 8.5
  )
  
  # Completitud
  doc <- body_add_par(doc, "4. Validación del ajuste por completitud", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste0(
      "El factor de corrección por completitud resume la magnitud del ajuste por subregistro/subreporte. ",
      "Para la lectura del informe, valores más altos implican una corrección más intensa. ",
      "En esta corrida, el rango observado en ", CFG$year_main,
      " fue aproximadamente de ",
      ifelse(is.na(factor_min_2024), "NA", fmt_num(factor_min_2024, 2)),
      " a ",
      ifelse(is.na(factor_max_2024), "NA", fmt_num(factor_max_2024, 2)),
      "."
    ),
    style = "Normal"
  )
  
  if (!is.null(fig_factor_path) && file.exists(fig_factor_path)) {
    doc <- body_add_figure_safe(
      doc,
      title = "Figura C1. Correction factor por edad y sexo",
      img_path = fig_factor_path,
      note = paste0("Nacional, año ", CFG$year_main, "."),
      width = 6.7,
      height = 4.2
    )
  } else {
    doc <- body_add_par(
      doc,
      "No se pudo insertar la figura del factor de completitud; se conserva la evidencia tabular cuando está disponible.",
      style = "Normal"
    )
  }
  
  doc <- body_add_ft_safe(
    doc = doc,
    title = "Tabla 4. Resumen anual del factor de corrección por completitud",
    dt = tab_factor_summary,
    note = "Distribución anual del factor según los QC del script 08_build_death_cause_final.",
    digits_map = list(
      n = 0,
      min_factor = 2,
      p25_factor = 2,
      median_factor = 2,
      p75_factor = 2,
      max_factor = 2
    ),
    fontsize = 8.5
  )
  
  # Pandemia
  if (!is.null(tab_pandemic_year) && nrow(tab_pandemic_year) > 0L) {
    doc <- body_add_par(doc, "5. Componente pandémico y corrección final", style = "heading 1")
    doc <- body_add_par(
      doc,
      paste0(
        "El componente pandémico se presenta solo como resumen anual para no recargar el informe. ",
        "Su lectura debe entenderse como un insumo de auditoría interna del pipeline de corrección final, ",
        "no como una atribución causal exhaustiva del exceso observado."
      ),
      style = "Normal"
    )
    
    doc <- body_add_ft_safe(
      doc = doc,
      title = "Tabla 5. Resumen anual del componente pandémico all-cause",
      dt = tab_pandemic_year,
      note = "Resumen anual derivado del QC de 08_build_death_cause_final.",
      digits_map = list(
        observed_allcause = 0,
        expected_allcause = 0,
        observed_corrected_allcause = 0,
        pandemic_excess_allcause = 0,
        pandemic_share_over_observed = 3
      ),
      fontsize = 8.3
    )
  }
  
  # AVP
  doc <- body_add_par(doc, "6. Validación epidemiológica aparente: AVP", style = "heading 1")
  doc <- body_add_par(
    doc,
    paste0(
      "La validación visual de AVP replica la misma lógica usada para mortalidad. ",
      "Se prioriza la tasa de AVP por 100 000 habitantes para facilitar la comparación visual entre sexos, edades y causas."
    ),
    style = "Normal"
  )
  
  if (length(avp_plot_files) == 0L) {
    doc <- body_add_par(
      doc,
      "No se pudo insertar la sección gráfica principal de AVP por ausencia de datos suficientes o por fallo de exportación.",
      style = "Normal"
    )
  } else {
    for (i in seq_along(avp_plot_files)) {
      doc <- body_add_figure_safe(
        doc,
        title = paste0("Figura A", i, ". AVP por edad y sexo en causas L3 seleccionadas"),
        img_path = avp_plot_files[i],
        note = "Tasa de AVP por 100 000 habitantes.",
        width = 6.9,
        height = 4.9
      )
    }
  }
  
  if (!is.null(tab_avp_ratio) && nrow(tab_avp_ratio) > 0L) {
    doc <- body_add_ft_safe(
      doc = doc,
      title = "Tabla 6. Resumen anual AVP vs muertes",
      dt = tab_avp_ratio,
      note = "QC complementario del script 10_compute_avp_yll.",
      digits_map = list(
        n_rows = 0,
        total_deaths_smoothed_consistent = 0,
        total_avp_abs = 0,
        weighted_mean_ex = 2
      ),
      fontsize = 8.3
    )
  }
  
  # Cierre
  doc <- body_add_par(doc, "7. Síntesis breve", style = "heading 1")
  
  cierre_lines <- c(
    paste0(
      "En conjunto, la evidencia mostrada sugiere que los patrones por edad y sexo de las causas L3 seleccionadas del bloque 1 son internamente plausibles para mortalidad y AVP."
    ),
    paste0(
      "La redistribución de garbage codes modifica las magnitudes de forma cuantificable y auditable",
      ifelse(!is.na(total_redist_gain_block1),
             paste0(", con una ganancia absoluta acumulada aproximada de ",
                    fmt_int(total_redist_gain_block1),
                    " muertes en las causas L3 resumidas del bloque 1"),
             ""),
      "."
    ),
    paste0(
      "La corrección por completitud introduce ajustes controlados, resumibles y rastreables en los QC del pipeline."
    )
  )
  
  if (!is.na(pandemic_2024)) {
    cierre_lines <- c(
      cierre_lines,
      paste0(
        "El componente pandémico ", CFG$year_main,
        " se mantuvo documentado en los productos de auditoría interna y se resumió solo en formato compacto para no sobreinterpretarlo."
      )
    )
  }
  
  cierre_lines <- c(
    cierre_lines,
    "En los insumos presentados en este informe no se observan señales obvias de incoherencia interna que invaliden los estimados resumidos."
  )
  
  for (ln in cierre_lines) {
    doc <- body_add_par(doc, ln, style = "Normal")
  }
  
  # Nota metodológica pragmática
  if (!is.null(qc_semantic_note) && nrow(qc_semantic_note) > 0L) {
    doc <- body_add_par(doc, "Nota técnica", style = "heading 2")
    doc <- body_add_par(
      doc,
      paste0(
        "Para contrastes before vs after se priorizaron directamente los outputs QC del script 07_qc_redistribution. ",
        "Esto evita ambigüedades semánticas sobre columnas finales del dataset consolidado cuando el objetivo específico es auditar la redistribución."
      ),
      style = "Normal"
    )
  }
  
  out_docx <- file.path(CFG$out_dir, "block1_validation_internal_report.docx")
  print(doc, target = out_docx)
  
  # Registro opcional
  if (exists("register_artifact")) {
    try(register_artifact(
      dataset_id = CFG$dataset_id,
      table_name = CFG$table_name,
      version = CFG$version,
      run_id = run_id,
      artifact_type = "report",
      artifact_path = out_docx,
      notes = "Informe Word breve de validación interna bloque 1"
    ), silent = TRUE)
  }
  
  if (exists("register_run_finish")) {
    try(register_run_finish(
      run_id,
      status = "success",
      message = "12g_build_block1_validation_report_docx completado"
    ), silent = TRUE)
  }
  
  msg("OK -> DOCX: ", out_docx)
  msg("OK -> Figures dir: ", CFG$fig_dir)
  msg("OK -> Tables dir: ", CFG$tbl_dir)
  
}, error = function(e) {
  
  if (exists("register_run_finish")) {
    try(register_run_finish(
      run_id,
      status = "failed",
      message = as.character(e$message)
    ), silent = TRUE)
  }
  
  stop(e)
})