suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(knitr)
  library(quarto)
})

source(here("R", "review_portal_utils.R"))

build_methodological_adjustment_report <- function() {
  report_root <- here("reports", "methodological_adjustment_report")
  table_dir <- file.path(report_root, "tables")
  figure_dir <- file.path(report_root, "figures")
  pdf_dir <- file.path(report_root, "pdf")
  for (d in c(report_root, table_dir, figure_dir, pdf_dir)) ensure_dir(d)

  red_abds <- build_redistribution_abds_data_v3()

  rules <- fread(here("data", "final", "redistribution_rules", "redistribution_rules.csv"), showProgress = FALSE)
  red_balance_total <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_total.csv"), showProgress = FALSE)
  red_balance_year <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_year.csv"), showProgress = FALSE)
  garbage_groups <- fread(here("data", "derived", "qc", "qc_redistribution", "tab_top_gc_groups.csv"), showProgress = FALSE)
  hier <- fread(
    here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv"),
    select = c(
      "year_id", "location_id", "sex_id", "age", "cause_level", "cause_concept_id", "cause_name",
      "deaths_observed", "deaths_post_redistribution", "deaths_final_net_of_pandemic", "deaths_final",
      "pandemic_excess_component", "pandemic_reassigned_out_component"
    ),
    showProgress = FALSE
  )
  leaf <- fread(
    here("data", "final", "death_cause_final", "death_cause_final.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "observed_allcause", "expected_allcause", "observed_corrected_allcause", "correction_factor_completeness"),
    showProgress = FALSE
  )

  rules_compact <- rules[, .(
    origen = source_group_name,
    destino_id = target_cause_concept_id,
    peso = target_weight,
    restriccion_sexo = sex_restriction,
    edad_inicio = age_start,
    edad_fin = age_end,
    metodo = redistribution_method,
    criterio = notes
  )]
  red_mass_global <- red_balance_total
  red_mass_year <- red_balance_year
  garbage_saved <- garbage_groups[, .(
    source_group_code,
    source_group_name,
    deaths_garbage_before = deaths_before,
    deaths_if_garbage_deleted = 0,
    deaths_saved_by_redistribution = deaths_before
  )][order(-deaths_saved_by_redistribution)]
  red_gain_level <- hier[, .(
    observed = sum(deaths_observed, na.rm = TRUE),
    post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE)
  ), by = .(year_id, cause_level, cause_concept_id, cause_name)]
  red_gain_level[, net_gain := post_redistribution - observed]
  red_gain_level[, abs_net_gain := abs(net_gain)]

  pandemic_stage_year <- hier[cause_level == 0, .(
    observed = sum(deaths_observed, na.rm = TRUE),
    post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
    net_of_pandemic = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    final = sum(deaths_final, na.rm = TRUE),
    pandemic_excess = sum(pandemic_excess_component, na.rm = TRUE),
    oprm_reassigned = sum(pandemic_reassigned_out_component, na.rm = TRUE)
  ), by = year_id][order(year_id)]
  pandemic_stage_year[, completeness_gain := final - net_of_pandemic]

  subregistro_year <- unique(leaf[, .(year_id, location_id, sex_id, age, observed_allcause, expected_allcause, observed_corrected_allcause)])
  subregistro_year <- subregistro_year[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
  ), by = year_id][order(year_id)]
  subregistro_year[, gap_before := expected_allcause - observed_allcause]
  subregistro_year[, gap_after := expected_allcause - observed_corrected_allcause]
  subregistro_year[, corrected_expected_ratio := observed_corrected_allcause / expected_allcause]
  subregistro_region_year <- unique(leaf[, .(year_id, location_id, sex_id, age, observed_allcause, expected_allcause, observed_corrected_allcause)])
  subregistro_region_year <- subregistro_region_year[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
  ), by = .(year_id, location_id)][order(year_id, location_id)]
  subregistro_region_year[, gap_before := expected_allcause - observed_allcause]
  subregistro_region_year[, gap_after := expected_allcause - observed_corrected_allcause]
  subregistro_region_year[, corrected_expected_ratio := observed_corrected_allcause / expected_allcause]

  fwrite(rules_compact, file.path(table_dir, "redistribution_rules_compact.csv"))
  fwrite(red_abds$table_3_1, file.path(table_dir, "table_3_1_redistribution_groups.csv"))
  fwrite(red_abds$table_3_2, file.path(table_dir, "table_3_2_impact_total_by_year.csv"))
  fwrite(red_abds$table_3_3, file.path(table_dir, "table_3_3_impact_by_age_sex_year.csv"))
  fwrite(red_abds$table_3_4, file.path(table_dir, "table_3_4_before_after_by_disease_group.csv"))
  fwrite(red_abds$box_3_2, file.path(table_dir, "box_3_2_case_trace.csv"))
  fwrite(red_mass_global, file.path(table_dir, "redistribution_mass_global.csv"))
  fwrite(red_mass_year, file.path(table_dir, "redistribution_mass_by_year.csv"))
  fwrite(garbage_saved, file.path(table_dir, "redistribution_garbage_saved.csv"))
  fwrite(red_gain_level, file.path(table_dir, "redistribution_gain_loss_by_year_level_cause.csv"))
  fwrite(pandemic_stage_year, file.path(table_dir, "pandemic_stage_by_year.csv"))
  fwrite(subregistro_year, file.path(table_dir, "subregistro_expected_vs_corrected_by_year.csv"))
  fwrite(subregistro_region_year, file.path(table_dir, "subregistro_expected_vs_corrected_by_region_year.csv"))

  p_garbage <- ggplot(head(garbage_saved, 25), aes(x = reorder(source_group_name, deaths_saved_by_redistribution), y = deaths_saved_by_redistribution)) +
    geom_col(fill = "#0f4c81") +
    coord_flip() +
    labs(title = "Muertes garbage salvadas por redistribucion", x = NULL, y = "Muertes") +
    theme_minimal(base_size = 11)
  p_gain <- ggplot(red_gain_level[abs_net_gain > 0][order(-abs_net_gain)][1:40], aes(x = reorder(cause_name, net_gain), y = net_gain, fill = factor(cause_level))) +
    geom_col() +
    coord_flip() +
    facet_wrap(~ year_id, scales = "free_y") +
    labs(title = "Ganancias y perdidas top por redistribucion", x = NULL, y = "Muertes", fill = "Nivel") +
    theme_minimal(base_size = 9)
  p_pandemic <- ggplot(melt(pandemic_stage_year, id.vars = "year_id", measure.vars = c("observed", "post_redistribution", "net_of_pandemic", "final"), variable.name = "stage", value.name = "deaths"),
                       aes(x = year_id, y = deaths, color = stage)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    labs(title = "Etapas de ajuste pandemico y subregistro", x = "Ano", y = "Muertes", color = NULL) +
    theme_minimal(base_size = 11)
  p_gap <- ggplot(melt(subregistro_year, id.vars = "year_id", measure.vars = c("gap_before", "gap_after"), variable.name = "gap_type", value.name = "gap"),
                  aes(x = year_id, y = gap, fill = gap_type)) +
    geom_col(position = "dodge") +
    labs(title = "Brecha INEI antes y despues de correccion", x = "Ano", y = "Brecha de muertes", fill = NULL) +
    theme_minimal(base_size = 11)

  save_plot_png(p_garbage, file.path(figure_dir, "redistribution_garbage_saved.png"), width = 9, height = 7)
  save_plot_png(p_gain, file.path(figure_dir, "redistribution_gain_loss_top.png"), width = 12, height = 8)
  save_plot_png(p_pandemic, file.path(figure_dir, "pandemic_stage_by_year.png"), width = 10, height = 6)
  save_plot_png(p_gap, file.path(figure_dir, "subregistro_gap_before_after_by_year.png"), width = 10, height = 6)

  qmd <- file.path(report_root, "methodological_adjustment_report.qmd")
  qmd_lines <- c(
    "---",
    "title: \"Informe metodologico de ajustes\"",
    "subtitle: \"Redistribucion, pandemia y subregistro\"",
    "lang: es",
    "format:",
    "  pdf:",
    "    toc: true",
    "    toc-title: \"Indice\"",
    "    number-sections: true",
    "    geometry: margin=1.8cm",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "```{r setup}",
    "library(data.table); library(knitr)",
    sprintf("table_dir <- %s", dQuote(normalizePath(table_dir, winslash = "/", mustWork = FALSE))),
    sprintf("figure_dir <- %s", dQuote(normalizePath(figure_dir, winslash = "/", mustWork = FALSE))),
    "```",
    "",
    "# Redistribucion",
    "",
    "Los codigos identificados para redistribucion se agrupan primero en grupos de redistribucion. Cada grupo se redistribuye como un bloque completo al mismo alcance de causas destino y con un solo metodo dominante, de manera an�loga a la logica editorial del ABDS.",
    "",
    "La magnitud del proceso debe mostrarse primero por grupo, metodo y alcance. Luego debe cuantificarse su impacto total en muertes y AVP, y por ultimo debe mostrarse como cambia la carga antes y despues de la redistribucion.",
    "",
    "```{r}",
    "t31 <- fread(file.path(table_dir, 'table_3_1_redistribution_groups.csv'))",
    "t31[, Number := format(round(Number), big.mark = ',')]",
    "t31[, Proportion_pct := sprintf('%.1f', round(Proportion_pct, 1))]",
    "kable(t31[, .(Redistribution_group, ICD10_codes, Method, Scope_of_target_diseases, Number, Proportion_pct)], caption = sprintf('Tabla 3.1: Numero y proporcion de muertes por grupo de redistribucion, metodo y causas destino, %s', max(fread(file.path(table_dir, 'table_3_2_impact_total_by_year.csv'))$Reference_year)))",
    "```",
    "",
    "```{r}",
    "t32 <- fread(file.path(table_dir, 'table_3_2_impact_total_by_year.csv'))",
    "t32_all <- t32[, .(Reference_year = 'All years', Total_deaths = sum(Total_deaths), Deaths_for_redistribution = sum(Deaths_for_redistribution), Per_cent_of_total_deaths = 100 * sum(Deaths_for_redistribution) / sum(Total_deaths), Total_YLL = sum(Total_YLL), YLL_for_redistributed_deaths = sum(YLL_for_redistributed_deaths), Per_cent_of_YLL_redistributed = 100 * sum(YLL_for_redistributed_deaths) / sum(Total_YLL))]",
    "t32 <- rbind(t32[, Reference_year := as.character(Reference_year)], t32_all, fill = TRUE)",
    "num_cols <- setdiff(names(t32), 'Reference_year')",
    "for (cc in num_cols) t32[[cc]] <- if (grepl('Per_cent', cc)) sprintf('%.1f', round(t32[[cc]], 1)) else format(round(t32[[cc]]), big.mark = ',')",
    "kable(t32, caption = 'Tabla 3.2: Numero y porcentaje de muertes y AVP, totales y redistribuidos, por ano de referencia')",
    "```",
    "",
    "```{r}",
    "t33 <- fread(file.path(table_dir, 'table_3_3_impact_by_age_sex_year.csv'))",
    "for (cc in setdiff(names(t33), 'Age_group')) t33[[cc]] <- format(round(t33[[cc]]), big.mark = ',')",
    "kable(t33, caption = sprintf('Tabla 3.3: Numero de muertes identificadas para redistribucion y AVP asociados, por edad y sexo, %s', max(fread(file.path(table_dir, 'table_3_2_impact_total_by_year.csv'))$Reference_year)))",
    "```",
    "",
    "La Tabla 3.4 muestra el numero de muertes clasificadas en grupos de enfermedad antes y despues de la redistribucion. Esta tabla es el puente principal entre el metodo de redistribucion y el perfil de carga resultante.",
    "",
    "```{r}",
    "t34 <- fread(file.path(table_dir, 'table_3_4_before_after_by_disease_group.csv'))",
    "t34_disp <- copy(t34)",
    "t34_disp[, Disease_group_display := Disease_group]",
    "t34_disp[duplicated(Disease_group_display), Disease_group_display := '']",
    "for (cc in c('Deaths','YLLs')) t34_disp[[cc]] <- format(round(t34_disp[[cc]]), big.mark = ',')",
    "for (cc in c('Percent_deaths','Percent_YLLs')) t34_disp[[cc]] <- sprintf('%.1f', round(t34_disp[[cc]], 1))",
    "t34_disp[Disease_group %in% c('Redistribution','All deaths') & Stage == 'Increase (before to after)', c('Deaths','Percent_deaths','YLLs','Percent_YLLs') := list('..','..','..','..')]",
    "kable(t34_disp[, .(Disease_group = Disease_group_display, Stage, Deaths, Percent_deaths, YLLs, Percent_YLLs)], caption = sprintf('Tabla 3.4: Numero y proporcion de muertes antes y despues de la redistribucion y cambio asociado, por grupo de enfermedad: Nacional, %s', max(fread(file.path(table_dir, 'table_3_2_impact_total_by_year.csv'))$Reference_year)))",
    "```",
    "",
    "## Box 3.2. Como funciona la redistribucion",
    "",
    "```{r}",
    "bx <- fread(file.path(table_dir, 'box_3_2_case_trace.csv'))",
    "cat(sprintf('Esta caja explica el proceso de redistribucion usando %s como ejemplo. Antes de la redistribucion habia %s muertes en este grupo. Despues de la redistribucion hubo %s muertes, lo que refleja una ganancia de %s muertes. Los grupos de redistribucion con alcance directo aportaron %s muertes. El componente proporcional amplio aporto unas %s muertes estimadas. Las %s muertes restantes provinieron de otros grupos de redistribucion en los que el grupo focal permanecio dentro del universo de causas destino.', bx$focal_group[1], format(round(bx$deaths_before[1]), big.mark = ','), format(round(bx$deaths_after[1]), big.mark = ','), format(round(bx$deaths_gain[1]), big.mark = ','), format(round(bx$direct_specific_group_deaths[1]), big.mark = ','), format(round(bx$proportional_general_group_deaths[1]), big.mark = ','), format(round(bx$remaining_gain_from_other_groups[1]), big.mark = ',')))",
    "```",
    "",
    "![Muertes garbage salvadas](figures/redistribution_garbage_saved.png)",
    "",
    "# Correccion por pandemia",
    "",
    "El componente pandemico separa mortalidad observada, post-redistribucion, neta de pandemia y final. La tabla cuantifica exceso pandemico y reasignacion OPRM.",
    "",
    "![Etapas pandemicas](figures/pandemic_stage_by_year.png)",
    "",
    "```{r}",
    "kable(fread(file.path(table_dir, 'pandemic_stage_by_year.csv')), caption = 'Etapas de ajuste por ano')",
    "```",
    "",
    "# Correccion por subregistro",
    "",
    "La correccion por completitud compara mortalidad observada SINADEF con mortalidad esperada INEI. La brecha posterior se reporta empiricamente; no se asume igualdad exacta si existen truncamientos o reglas metodologicas.",
    "",
    "![Brecha INEI antes y despues](figures/subregistro_gap_before_after_by_year.png)",
    "",
    "```{r}",
    "kable(fread(file.path(table_dir, 'subregistro_expected_vs_corrected_by_year.csv')), caption = 'Esperado INEI vs observado y corregido por ano')",
    "```",
    "",
    "```{r}",
    "kable(head(fread(file.path(table_dir, 'subregistro_expected_vs_corrected_by_region_year.csv')), 40), caption = 'Esperado INEI vs observado y corregido por region y ano - muestra')",
    "```"
  )
  write_text_file(qmd, qmd_lines)
  quarto::quarto_render(qmd, output_file = "methodological_adjustment_report.pdf", quiet = TRUE)
  pdf_src <- file.path(report_root, "methodological_adjustment_report.pdf")
  pdf_dst <- file.path(pdf_dir, "methodological_adjustment_report.pdf")
  if (file.exists(pdf_src)) file.copy(pdf_src, pdf_dst, overwrite = TRUE)
  invisible(list(report_root = report_root, pdf = file.path(pdf_dir, "methodological_adjustment_report.pdf")))
}

build_methodological_adjustment_report <- function() {
  report_root <- here("reports", "methodological_adjustment_report")
  table_dir <- file.path(report_root, "tables")
  pdf_dir <- file.path(report_root, "pdf")
  for (d in c(report_root, table_dir, pdf_dir)) ensure_dir(d)

  red_abds <- build_redistribution_abds_data_v3()
  pan <- build_pandemic_tables_v2()

  fwrite(red_abds$table_3_1, file.path(table_dir, "table_3_1_redistribution_groups.csv"))
  fwrite(red_abds$table_3_2, file.path(table_dir, "table_3_2_impact_total_by_year.csv"))
  fwrite(red_abds$table_3_3, file.path(table_dir, "table_3_3_impact_by_age_sex_year.csv"))
  fwrite(red_abds$table_3_4, file.path(table_dir, "table_3_4_before_after_by_disease_group.csv"))
  fwrite(red_abds$box_3_2, file.path(table_dir, "box_3_2_case_trace.csv"))
  fwrite(pan$p1, file.path(table_dir, "tabla_p1_subregistro_total_por_anio.csv"))
  fwrite(pan$p2, file.path(table_dir, "tabla_p2_pandemia_total_por_anio.csv"))
  fwrite(pan$p3, file.path(table_dir, "tabla_p3_before_after_por_grupo_causal_subregistro.csv"))
  fwrite(pan$p4, file.path(table_dir, "tabla_p4_before_after_por_grupo_causal_pandemia.csv"))
  fwrite(pan$p5, file.path(table_dir, "tabla_p5_balance_pandemico_qc.csv"))
  fwrite(pan$p6, file.path(table_dir, "tabla_p6_factor_completitud_resumen.csv"))
  fwrite(pan$x1, file.path(table_dir, "tabla_x1_conciliacion_redistribucion_pandemia_subregistro.csv"))

  qmd <- file.path(report_root, "methodological_adjustment_report.qmd")
  qmd_lines <- c(
    "---",
    "title: \"Informe metodológico de ajustes\"",
    "subtitle: \"Redistribución, pandemia y subregistro\"",
    "lang: es",
    "format:",
    "  pdf:",
    "    toc: true",
    "    toc-title: \"Índice\"",
    "    number-sections: true",
    "    geometry: margin=1.8cm",
    "execute:",
    "  echo: false",
    "  warning: false",
    "  message: false",
    "---",
    "",
    "```{r setup}",
    "library(data.table); library(knitr)",
    sprintf("table_dir <- %s", dQuote(normalizePath(table_dir, winslash = "/", mustWork = FALSE))),
    "```",
    "",
    "# Redistribución",
    "",
    "La redistribución se presenta con lógica ABDS: grupos, magnitud total, perfil por edad y sexo, cambio before/after y caso trazado.",
    "",
    "```{r}",
    "t31 <- fread(file.path(table_dir, 'table_3_1_redistribution_groups.csv'))",
    "kable(t31, caption = 'Tabla 3.1. Grupos de redistribución, método y alcance')",
    "```",
    "",
    "```{r}",
    "t32 <- fread(file.path(table_dir, 'table_3_2_impact_total_by_year.csv'))",
    "kable(t32, caption = 'Tabla 3.2. Muertes y AVP redistribuidos por año')",
    "```",
    "",
    "```{r}",
    "t33 <- fread(file.path(table_dir, 'table_3_3_impact_by_age_sex_year.csv'))",
    "kable(t33, caption = 'Tabla 3.3. Muertes y AVP redistribuidos por edad y sexo')",
    "```",
    "",
    "```{r}",
    "t34 <- fread(file.path(table_dir, 'table_3_4_before_after_by_disease_group.csv'))",
    "kable(t34, caption = 'Tabla 3.4. Before/after por grupo de enfermedad')",
    "```",
    "",
    "# Pandemia y subregistro",
    "",
    "En este proyecto, subregistro y reasignación pandémica son mecanismos distintos. El subregistro aumenta la masa total corregida. La pandemia preserva esa masa y solo reasigna causa. Además, `deaths_observed` y `deaths_post_redistribution` son equivalentes en la versión actual del pipeline.",
    "",
    "```{r}",
    "p1 <- fread(file.path(table_dir, 'tabla_p1_subregistro_total_por_anio.csv'))",
    "kable(p1, caption = 'Tabla P1. Impacto total de subregistro por año')",
    "```",
    "",
    "```{r}",
    "p2 <- fread(file.path(table_dir, 'tabla_p2_pandemia_total_por_anio.csv'))",
    "kable(p2, caption = 'Tabla P2. Impacto pandémico total por año')",
    "```",
    "",
    "```{r}",
    "p3 <- fread(file.path(table_dir, 'tabla_p3_before_after_por_grupo_causal_subregistro.csv'))",
    "kable(p3[p3$year_id == max(p3$year_id), ], caption = 'Tabla P3. Before/after de completitud por grupo causal, último año')",
    "```",
    "",
    "```{r}",
    "p4 <- fread(file.path(table_dir, 'tabla_p4_before_after_por_grupo_causal_pandemia.csv'))",
    "kable(p4[p4$year_id == max(p4$year_id), ], caption = 'Tabla P4. Before/after de reasignación pandémica por grupo causal, último año')",
    "```",
    "",
    "```{r}",
    "p5 <- fread(file.path(table_dir, 'tabla_p5_balance_pandemico_qc.csv'))",
    "kable(head(p5, 40), caption = 'Tabla P5. Balance pandémico QC - muestra')",
    "```",
    "",
    "```{r}",
    "p6 <- fread(file.path(table_dir, 'tabla_p6_factor_completitud_resumen.csv'))",
    "kable(p6, caption = 'Tabla P6. Resumen del factor de completitud')",
    "```",
    "",
    "```{r}",
    "x1 <- fread(file.path(table_dir, 'tabla_x1_conciliacion_redistribucion_pandemia_subregistro.csv'))",
    "kable(x1, caption = 'Tabla X1. Conciliacion entre redistribucion y pandemia/subregistro')",
    "```"
  )
  write_text_file(qmd, qmd_lines)
  quarto::quarto_render(qmd, output_file = "methodological_adjustment_report.pdf", quiet = TRUE)
  pdf_src <- file.path(report_root, "methodological_adjustment_report.pdf")
  pdf_dst <- file.path(pdf_dir, "methodological_adjustment_report.pdf")
  if (file.exists(pdf_src)) file.copy(pdf_src, pdf_dst, overwrite = TRUE)
  invisible(list(report_root = report_root, pdf = file.path(pdf_dir, "methodological_adjustment_report.pdf")))
}
