library(data.table)
library(jsonlite)

source("R/review_portal_utils.R", encoding = "UTF-8")

to_utf8_clean <- function(x) {
  if (is.null(x)) return(x)
  x <- fix_mojibake_text(x)
  replacements <- c(
    "Asignaci.*proporcional" = "Asignación proporcional",
    "gastrointestinaleseses" = "Gastrointestinales",
    "gastrointestinaleses" = "Gastrointestinales",
    "gastrointestinales" = "Gastrointestinales",
    "cardiovasculareses, infantiles/congenitas" = "Cardiovasculares e infantiles/congénitas",
    "cardiovasculareses, infantiles/cong" = "Cardiovasculares e infantiles/congénitas",
    "Cardiovasculares e infantiles/congenitasenitas" = "Cardiovasculares e infantiles/congénitas",
    "Cardiovasculares e infantiles/congénitasénitas" = "Cardiovasculares e infantiles/congénitas",
    "Todas las otras casas intermedias e inmediatas no espec.*ficas" = "Todas las otras causas intermedias e inmediatas no específicas",
    "Amiloidosis no especificada, signos y s.*ntomas respiratorios no especificados, y caquexia" = "Amiloidosis no especificada, signos y síntomas respiratorios no especificados, y caquexia",
    "Signos y s.*ntomas cardiacos, enfermedades digestivas no especificadas y anomalias congenitas" = "Signos y síntomas cardiacos, enfermedades digestivas no especificadas y anomalías congénitas",
    "Falla card.*aca" = "Falla cardíaca",
    "Hipertensi.*n" = "Hipertensión",
    "Cancer \\(c.*nceres digestivos\\)" = "Cáncer (cánceres digestivos)",
    "100 y m.*s" = "100 y más",
    "Redistribucion / garbage" = "Redistribución / garbage"
  )
  for (pat in names(replacements)) {
    x <- gsub(pat, replacements[[pat]], x)
  }
  enc2utf8(x)
}

fmt_num1 <- function(x) ifelse(is.na(x), "..", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))
fmt_pct1 <- function(x) ifelse(is.na(x), "..", sprintf("%.1f", round(x, 1)))

distribution_table_html_local <- function(dt, caption = NULL, note = NULL) {
  header <- paste(sprintf("<th>%s</th>", esc_html(names(dt))), collapse = "")
  rows <- apply(dt, 1, function(row) {
    paste0("<tr>", paste(sprintf("<td>%s</td>", esc_html(as.character(row))), collapse = ""), "</tr>")
  })
  html <- c(
    if (!is.null(caption)) sprintf("<h3>%s</h3>", esc_html(caption)),
    "<div class=\"table-wrap\"><table class=\"portal-table\"><thead><tr>",
    header,
    "</tr></thead><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table></div>",
    if (!is.null(note)) sprintf("<p class=\"muted\"><strong>Nota.</strong> %s</p>", esc_html(note))
  )
  paste(html, collapse = "")
}

distribution_table_34_compact_html <- function(dt, caption = NULL, note = NULL) {
  groups <- unique(dt[[1]])
  rows <- c()
  for (grp in groups) {
    sub <- dt[dt[[1]] == grp, , drop = FALSE]
    if (!nrow(sub)) next
    for (i in seq_len(nrow(sub))) {
      row <- as.list(sub[i, ])
      cells <- c()
      if (i == 1L) {
        cells <- c(cells, sprintf("<td rowspan=\"%s\"><strong>%s</strong></td>", nrow(sub), esc_html(grp)))
      }
      cells <- c(
        cells,
        sprintf("<td>%s</td>", esc_html(row[[2]])),
        sprintf("<td>%s</td>", esc_html(row[[3]])),
        sprintf("<td>%s</td>", esc_html(row[[4]])),
        sprintf("<td>%s</td>", esc_html(row[[5]])),
        sprintf("<td>%s</td>", esc_html(row[[6]]))
      )
      row_class <- if (grepl("^Cambio", row[[2]])) " class=\"stage-change\"" else ""
      rows <- c(rows, paste0("<tr", row_class, ">", paste(cells, collapse = ""), "</tr>"))
    }
  }
  header <- paste(sprintf("<th>%s</th>", esc_html(names(dt))), collapse = "")
  html <- c(
    if (!is.null(caption)) sprintf("<h3>%s</h3>", esc_html(caption)),
    "<div class=\"table-wrap\"><table class=\"portal-table portal-table-compact\"><thead><tr>",
    header,
    "</tr></thead><tbody>",
    paste(rows, collapse = ""),
    "</tbody></table></div>",
    if (!is.null(note)) sprintf("<p class=\"muted\"><strong>Nota.</strong> %s</p>", esc_html(note))
  )
  paste(html, collapse = "")
}

details_block_local <- function(title, body_html) {
  sprintf("<details class=\"glossary-term\"><summary>%s</summary>%s</details>", esc_html(title), body_html)
}

mk_lines <- function(dt, value_col, pct_col, unit_label) {
  if (nrow(dt) == 0) return(character())
  vapply(seq_len(nrow(dt)), function(i) {
    sprintf(
      "<li>%s (%s %s adicionales, un aumento de %s%%)</li>",
      esc_html(dt$Disease_group[i]),
      fmt_num1(dt[[value_col]][i]),
      unit_label,
      fmt_pct1(dt[[pct_col]][i])
    )
  }, character(1))
}

root_dir <- file.path(getwd(), "reports", "qc_pipeline_encyclopedia")
out_dir <- file.path(getwd(), "data", "derived", "qc", "review_portal", "redistribution")
module_dir <- file.path(root_dir, "modules", "redistribucion")
downloads_dir <- file.path(module_dir, "downloads")
dir.create(module_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(downloads_dir, recursive = TRUE, showWarnings = FALSE)

t31 <- fread(file.path(out_dir, "table_3_1_redistribution_groups.csv"), encoding = "UTF-8")
t32 <- fread(file.path(out_dir, "table_3_2_impact_total_by_year.csv"), encoding = "UTF-8")
t33 <- fread(file.path(out_dir, "table_3_3_impact_by_age_sex_year.csv"), encoding = "UTF-8")
t34 <- fread(file.path(out_dir, "table_3_4_before_after_by_disease_group.csv"), encoding = "UTF-8")
bx <- fread(file.path(out_dir, "box_3_2_case_trace_cancer.csv"), encoding = "UTF-8")
met <- fromJSON(file.path(out_dir, "redistribution_text_metrics.json"), simplifyVector = TRUE)

for (dt in list(t31, t33, t34, bx)) {
  char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
  for (col in char_cols) dt[, (col) := to_utf8_clean(get(col))]
}

char_cols_31 <- names(t31)[vapply(t31, is.character, logical(1))]
for (col in char_cols_31) t31[, (col) := to_utf8_clean(get(col))]

t31[, ICD10_codes := fifelse(nzchar(ICD10_codes), ICD10_codes, "Ver descarga completa")]
t31_disp <- copy(t31)
t31_disp[, `:=`(Number = fmt_num1(Number), Proportion_pct = fmt_pct1(Proportion_pct))]
t31_disp <- t31_disp[, .(
  `Grupo de redistribución` = Redistribution_group,
  `Códigos CIE-10` = ICD10_codes,
  `Método` = Method,
  `Alcance de causas destino` = Scope_of_target_diseases,
  `Número` = Number,
  `Proporción (%)` = Proportion_pct
)]

t32_all <- data.table(
  Reference_year = "Todos los anos",
  Total_deaths = sum(t32$Total_deaths, na.rm = TRUE),
  Deaths_for_redistribution = sum(t32$Deaths_for_redistribution, na.rm = TRUE),
  Per_cent_of_total_deaths = 100 * sum(t32$Deaths_for_redistribution, na.rm = TRUE) / sum(t32$Total_deaths, na.rm = TRUE),
  Total_YLL = sum(t32$Total_YLL, na.rm = TRUE),
  YLL_for_redistributed_deaths = sum(t32$YLL_for_redistributed_deaths, na.rm = TRUE),
  Per_cent_of_YLL_redistributed = 100 * sum(t32$YLL_for_redistributed_deaths, na.rm = TRUE) / sum(t32$Total_YLL, na.rm = TRUE)
)
t32_disp <- rbind(copy(t32), t32_all, fill = TRUE)
t32_disp[, Reference_year := as.character(Reference_year)]
t32_disp[, `:=`(
  Total_deaths = fmt_num1(Total_deaths),
  Deaths_for_redistribution = fmt_num1(Deaths_for_redistribution),
  Per_cent_of_total_deaths = fmt_pct1(Per_cent_of_total_deaths),
  Total_YLL = fmt_num1(Total_YLL),
  YLL_for_redistributed_deaths = fmt_num1(YLL_for_redistributed_deaths),
  Per_cent_of_YLL_redistributed = fmt_pct1(Per_cent_of_YLL_redistributed)
)]
t32_disp <- t32_disp[, .(
  `Año de referencia` = Reference_year,
  `Muertes totales` = Total_deaths,
  `Muertes para redistribución` = Deaths_for_redistribution,
  `% del total de muertes` = Per_cent_of_total_deaths,
  `AVP totales` = Total_YLL,
  `AVP de muertes redistribuidas` = YLL_for_redistributed_deaths,
  `% de AVP redistribuidos` = Per_cent_of_YLL_redistributed
)]

t33_disp <- copy(t33)
num33 <- setdiff(names(t33_disp), "Age_group")
for (nm in num33) t33_disp[, (nm) := fmt_num1(get(nm))]
t33_disp <- t33_disp[, .(
  `Grupo de edad` = Age_group,
  `Muertes femeninas` = Female_deaths,
  `Muertes masculinas` = Male_deaths,
  `Muertes en personas` = Person_deaths,
  `AVP femeninos` = Female_YLL,
  `AVP masculinos` = Male_YLL,
  `AVP en personas` = Person_YLL
)]

t34_disp <- copy(t34)
t34_disp[, Stage := fifelse(
  Stage == "Antes de la redistribucion", "Antes de la redistribución",
  fifelse(Stage == "Despues de la redistribucion", "Después de la redistribución",
          fifelse(Stage == "Cambio (antes a despues)", "Cambio (antes a después)", Stage))
)]
t34_disp[, Disease_group := fifelse(Disease_group == "Redistribucion / garbage", "Redistribución / garbage", Disease_group)]
for (nm in c("Deaths", "YLLs")) t34_disp[, (nm) := fmt_num1(get(nm))]
for (nm in c("Percent_deaths", "Percent_YLLs")) t34_disp[, (nm) := fmt_pct1(get(nm))]
t34_disp[Disease_group %in% c("Redistribución / garbage", "Todas las muertes") & Stage == "Cambio (antes a después)", c("Deaths", "Percent_deaths", "YLLs", "Percent_YLLs") := list("..", "..", "..", "..")]
t34_disp <- t34_disp[, .(
  `Grupo de enfermedad` = Disease_group,
  `Etapa` = Stage,
  `Muertes` = Deaths,
  `% muertes` = Percent_deaths,
  `AVP` = YLLs,
  `% AVP` = Percent_YLLs
)]

ref_year <- as.character(met$reference_year)
total_red_deaths <- met$total_deaths_redistributed
total_red_pct <- met$total_pct_deaths_redistributed
total_red_yll_pct <- met$total_pct_yll_redistributed

t34w <- copy(t34)
char_cols_34w <- names(t34w)[vapply(t34w, is.character, logical(1))]
for (col in char_cols_34w) t34w[, (col) := to_utf8_clean(get(col))]
abs_deaths <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-Deaths)][1:3]
pct_deaths <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & is.finite(Percent_deaths)][order(-Percent_deaths)][1:3]
abs_yll <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-YLLs)][1:3]
pct_yll <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & is.finite(Percent_YLLs)][order(-Percent_YLLs)][1:3]

box_title <- to_utf8_clean(bx$focal_group[1])
box_gain_pct <- if (is.finite(bx$deaths_gain[1]) && bx$deaths_before[1] > 0) 100 * bx$deaths_gain[1] / bx$deaths_before[1] else NA_real_
box_accounted_pct <- if (is.finite(bx$deaths_gain[1]) && bx$deaths_gain[1] > 0) 100 * bx$accounted_by_direct_and_proportional[1] / bx$deaths_gain[1] else NA_real_

sections_html <- c(
  '<section class="card">',
  '<div class="eyebrow">Grupos de redistribución</div><h2 class="section-title">Grupos de redistribución</h2>',
  '<p>Los códigos CIE-10 identificados para redistribución se asignaron primero a grupos de redistribución. Cada grupo se redistribuyó como un bloque completo al mismo universo de causas destino. Todas las muertes asignadas a un grupo se redistribuyeron con el mismo algoritmo.</p>',
  '<p>La tabla de abajo muestra los grupos de redistribución, las causas destino y el método de redistribución. El método por el cual se redistribuyó cada grupo dependió del nivel de evidencia disponible. Los grupos canónicos se muestran aunque en el año visible tengan cero muertes redistribuidas.</p>',
  details_block_local('Cómo leer la Tabla 3.1', '<p><strong>Grupo de redistribución</strong> es el grupo operativo de causas garbage o mal definidas. <strong>Códigos CIE-10</strong> se muestran resumidos en la tabla y completos en la descarga. <strong>Método</strong> indica si el grupo utilizó evidencia directa, MCOD indirecto, ambos o asignación proporcional. <strong>Alcance de causas destino</strong> indica el universo de reasignación. <strong>Número</strong> es el volumen de muertes identificadas para redistribución en el año de referencia. Si el grupo existe en las reglas pero no tuvo casos ese año, se muestra con <strong>0</strong>.</p>'),
  distribution_table_html_local(t31_disp, sprintf('Tabla 3.1: Número y proporción de muertes por grupo de redistribución, método y causas destino, %s', ref_year), 'Las listas detalladas de CIE-10 están disponibles en las descargas. La tabla principal conserva una lista acortada y legible.'),
  '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_1_redistribution_groups.csv">Descargar Tabla 3.1 CSV</a><a class="btn ghost" href="downloads/redistribution_rules_full.csv">Descargar reglas completas de redistribución</a></div>',
  '</section>',
  '<section class="card">',
  '<div class="eyebrow">Impacto de la redistribución</div><h2 class="section-title">Impacto de la redistribución</h2>',
  sprintf('<p>Los AVP específicos por causa se ven afectados por las causas de muerte identificadas para redistribución y por los métodos usados para reasignarlas. En este estudio, %s muertes fueron identificadas para redistribución en %s, equivalentes a %s%% de las muertes y %s%% de los AVP.</p>', fmt_num1(total_red_deaths), ref_year, fmt_pct1(total_red_pct), fmt_pct1(total_red_yll_pct)),
  '<div class="lead-note"><strong>Nota metodológica.</strong> Las tablas 3.2 a 3.4 miden el efecto puro de redistribución antes de pandemia y subregistro, dentro del mismo universo de muertes. La sensibilidad extendida sin redistribución y con borrado de garbage se reporta aparte. Puede generar más muertes corregidas y más AVP por cambios en completitud y exceso pandémico.</div>',
  details_block_local('Cómo leer la Tabla 3.2', '<p><strong>Muertes totales</strong> y <strong>AVP totales</strong> corresponden al mismo universo base, antes de pandemia y subregistro. <strong>Muertes para redistribución</strong> son las muertes inicialmente asignadas a grupos garbage o mal definidos. <strong>AVP de muertes redistribuidas</strong> son los AVP asociados a esas mismas muertes en el escenario base pre-redistribución. Esta tabla mide el efecto puro de redistribución, ceteris paribus.</p>'),
  distribution_table_html_local(t32_disp, 'Tabla 3.2: Número y porcentaje de muertes y AVP, totales y redistribuidos, por año de referencia'),
  '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_2_impact_total_by_year.csv">Descargar Tabla 3.2 CSV</a></div>',
  '</section>',
  '<section class="card">',
  sprintf('<p>El número de muertes identificadas para redistribución varió con la edad. La tabla siguiente muestra el patrón por edad y sexo para %s.</p>', ref_year),
  details_block_local('Cómo leer la Tabla 3.3', '<p><strong>Muertes femeninas</strong>, <strong>muertes masculinas</strong> y <strong>muertes en personas</strong> son las muertes identificadas para redistribución en cada grupo de edad. Las columnas de AVP muestran la carga de esas mismas muertes en el escenario base pre-redistribución. La última fila debe cerrar con el total de muertes y AVP identificados para redistribución en el año de referencia.</p>'),
  distribution_table_html_local(t33_disp, sprintf('Tabla 3.3: Número de muertes identificadas para redistribución y AVP asociados, por edad y sexo, %s', ref_year)),
  '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_3_impact_by_age_sex_year.csv">Descargar Tabla 3.3 CSV</a></div>',
  '</section>',
  '<section class="card">',
  '<p>La Tabla 3.4 muestra el número de muertes clasificadas a grupos de enfermedad antes y después de la redistribución. Las mayores ganancias absolutas de muertes por redistribución fueron para:</p>',
  '<ul>', paste(mk_lines(abs_deaths, "Deaths", "Percent_deaths", "muertes"), collapse = ''), '</ul>',
  '<p>Las mayores ganancias proporcionales de muertes, aparte de las descritas arriba, fueron para:</p>',
  '<ul>', paste(mk_lines(pct_deaths, "Deaths", "Percent_deaths", "muertes"), collapse = ''), '</ul>',
  '<p>El impacto de la redistribución sobre los AVP también se muestra en la Tabla 3.4. Las mayores ganancias absolutas de AVP fueron para:</p>',
  '<ul>', paste(mk_lines(abs_yll, "YLLs", "Percent_YLLs", "AVP"), collapse = ''), '</ul>',
  '<p>Otras grandes ganancias porcentuales en AVP fueron para:</p>',
  '<ul>', paste(mk_lines(pct_yll, "YLLs", "Percent_YLLs", "AVP"), collapse = ''), '</ul>',
  details_block_local('Cómo leer la Tabla 3.4', '<p>Cada grupo de enfermedad se muestra en tres filas. <strong>Antes de la redistribución</strong> es el escenario base con una categoría residual explícita <strong>Redistribución / garbage</strong>. <strong>Después de la redistribución</strong> es el escenario post-redistribución. <strong>Cambio (antes a después)</strong> expresa la ganancia absoluta y proporcional. Las filas de <strong>Redistribución / garbage</strong> y <strong>Todas las muertes</strong> se incluyen para mostrar el cierre del ejercicio ceteris paribus, por eso su fila de cambio queda sin valor.</p>'),
  distribution_table_34_compact_html(t34_disp, sprintf('Tabla 3.4: Número y proporción de muertes antes y después de la redistribución y cambio asociado, por grupo de enfermedad: Nacional, %s', ref_year)),
  '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_4_before_after_by_disease_group.csv">Descargar Tabla 3.4 CSV</a></div>',
  '</section>',
  '<section class="card box-highlight">',
  '<h3>Box 3.2: Cómo funciona la redistribución</h3>',
  sprintf('<p>Esta caja explica el proceso de redistribución y muestra, como ejemplo, de dónde provienen las muertes adicionales en %s como resultado de la redistribución.</p>', esc_html(tolower(box_title))),
  sprintf('<p>La Tabla 3.4 muestra que antes de la redistribución había %s muertes clasificadas en %s. Después de la redistribución hubo %s muertes, lo que refleja una ganancia de %s muertes%s.</p>', fmt_num1(bx$deaths_before[1]), esc_html(box_title), fmt_num1(bx$deaths_after[1]), fmt_num1(bx$deaths_gain[1]), if (is.finite(box_gain_pct)) paste0(', o un aumento adicional de ', fmt_pct1(box_gain_pct), '%') else ''),
  sprintf('<p>La Tabla 3.1 muestra que %s muertes fueron identificadas en grupos de redistribución ya orientados hacia %s. Un componente proporcional amplio aportó además unas %s muertes estimadas, según la participación pre-redistribución de %s.</p>', fmt_num1(bx$direct_specific_group_deaths[1]), esc_html(box_title), fmt_num1(bx$proportional_general_group_deaths[1]), paste0(fmt_pct1(100 * bx$pre_redistribution_share[1]), '%')),
  if (is.finite(box_accounted_pct)) sprintf('<p>Hasta aquí, alrededor de %s de la ganancia total en muertes de %s (%s de %s) puede explicarse por grupos dirigidos de forma específica más el componente proporcional amplio.</p>', paste0(fmt_pct1(box_accounted_pct), '%'), esc_html(tolower(box_title)), fmt_num1(bx$accounted_by_direct_and_proportional[1]), fmt_num1(bx$deaths_gain[1])) else '<p>En este caso, la ganancia neta final fue nula. La caja se conserva como trazabilidad metodológica.</p>',
  sprintf('<p>Las %s muertes restantes provinieron de otros grupos de redistribución en los que %s estaba dentro del alcance como causa destino. Esto preserva la lógica ABDS de rastrear la ganancia hasta trayectorias específicas de redistribución.</p>', fmt_num1(bx$remaining_gain_from_other_groups[1]), esc_html(tolower(box_title))),
  '</section>',
  '<section class="card">',
  '<h3>Descargas y notas metodológicas</h3>',
  '<p class="muted">El HTML mantiene resúmenes legibles en pantalla y deja los artefactos técnicos completos en descargas para que la tabla siga siendo clara.</p>',
  glossary_html(c('masa', 'peso', 'garbage'), 'Glosario y notas metodológicas'),
  '<div class="nav-actions"><a class="btn ghost" href="downloads/box_3_2_case_trace_cancer.csv">Descargar traza de Box 3.2</a><a class="btn ghost" href="downloads/redistribution_text_metrics.json">Descargar metricas narrativas JSON</a><a class="btn ghost" href="downloads/audit_group_catalog_vs_observed.csv">Descargar auditoria catalogo vs observado</a><a class="btn ghost" href="downloads/audit_group_raw_sinadef_presence.csv">Descargar auditoria cruda SINADEF</a><a class="btn" href="../../index.html">Volver al portal</a></div>',
  '</section>'
)

html <- c(
  '<!doctype html>',
  '<html lang="es">',
  '<head>',
  '  <meta charset="utf-8">',
  '  <meta name="viewport" content="width=device-width, initial-scale=1">',
  '  <title>Redistribución</title>',
  '  <link rel="stylesheet" href="../../assets/portal.css">',
  '  <script defer src="../../assets/portal.js"></script>',
  '</head>',
  '<body>',
  '<div class="portal-shell">',
  '<section class="hero"><div class="eyebrow">Portal técnico reproducible</div><h1>Redistribución</h1><p>Reconstrucción estilo ABDS de grupos de redistribución, impacto y caso trazado con insumos del estudio del Perú.</p><div class="hero-meta"><span class="meta-chip">Sitio estático multipágina</span><span class="meta-chip">HTML interactivo + PDF</span><span class="meta-chip">Listo para hosting protegido</span></div></section>',
  '<div class="layout-grid"><div class="sidebar"><h3>Navegación</h3><ul><li><a href="../../index.html">Volver al portal</a></li></ul></div><main class="content-stack">',
  sections_html,
  sprintf('<div class="footer-note">Generado automaticamente desde %s</div>', format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
  '</main></div></div>',
  '</body>',
  '</html>'
)
writeLines(html, file.path(module_dir, "index.html"), useBytes = TRUE)
html_path <- file.path(module_dir, "index.html")
html_txt <- paste(readLines(html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
html_txt <- fix_mojibake_text(html_txt)
html_txt <- gsub("Cardiovasculares e infantiles/congenitasenitas", "Cardiovasculares e infantiles/congénitas", html_txt, fixed = TRUE)
html_txt <- gsub("Cardiovasculares e infantiles/congenitas", "Cardiovasculares e infantiles/congénitas", html_txt, fixed = TRUE)
html_txt <- gsub("congénitasénitas", "congénitas", html_txt, fixed = TRUE)
html_txt <- gsub("Gastrointestinaleses", "Gastrointestinales", html_txt, fixed = TRUE)
html_txt <- gsub(">Ano de referencia<", ">Año de referencia<", html_txt, fixed = TRUE)
html_txt <- gsub(">Muertes para redistribucion<", ">Muertes para redistribución<", html_txt, fixed = TRUE)
html_txt <- gsub(">Antes de la redistribucion<", ">Antes de la redistribución<", html_txt, fixed = TRUE)
html_txt <- gsub(">Despues de la redistribucion<", ">Después de la redistribución<", html_txt, fixed = TRUE)
html_txt <- gsub(">Cambio \\(antes a despues\\)<", ">Cambio (antes a después)<", html_txt)
writeLines(enc2utf8(strsplit(html_txt, "\n", fixed = TRUE)[[1]]), html_path, useBytes = TRUE)

md <- c(
  '# Grupos de redistribución',
  '',
  'La tabla siguiente muestra los grupos de redistribución, las causas destino y el método de redistribución. El método por el cual se redistribuyó cada grupo dependió del nivel de evidencia disponible. Los grupos canónicos se muestran aunque en el año visible tengan cero muertes redistribuidas.',
  '',
  sprintf('## Tabla 3.1: Número y proporción de muertes por grupo de redistribución, método y causas destino, %s', ref_year),
  '',
  pipe_table_text(t31_disp, max_rows = 200L),
  '',
  '# Impacto de la redistribución',
  '',
  sprintf('En este estudio, %s muertes fueron identificadas para redistribución en %s, equivalentes a %s%% de las muertes y %s%% de los AVP.', fmt_num1(total_red_deaths), ref_year, fmt_pct1(total_red_pct), fmt_pct1(total_red_yll_pct)),
  '',
  'Nota metodológica. Las tablas 3.2 a 3.4 miden el efecto puro de redistribución antes de pandemia y subregistro, dentro del mismo universo de muertes. La sensibilidad extendida sin redistribución y con borrado de garbage se reporta aparte. Puede generar más muertes corregidas y más AVP por cambios en completitud y exceso pandémico.',
  '',
  '## Tabla 3.2: Número y porcentaje de muertes y AVP, totales y redistribuidos, por año de referencia',
  '',
  pipe_table_text(t32_disp, max_rows = 200L),
  '',
  sprintf('## Tabla 3.3: Número de muertes identificadas para redistribución y AVP asociados, por edad y sexo, %s', ref_year),
  '',
  pipe_table_text(t33_disp, max_rows = 200L),
  '',
  sprintf('## Tabla 3.4: Número y proporción de muertes antes y después de la redistribución y cambio asociado, por grupo de enfermedad: Nacional, %s', ref_year),
  '',
  pipe_table_text(t34_disp, max_rows = 400L),
  '',
  '## Box 3.2: Cómo funciona la redistribución',
  '',
  sprintf('Esta caja explica el proceso de redistribución y muestra, como ejemplo, de dónde provienen las muertes adicionales en %s como resultado de la redistribución.', tolower(box_title))
)
md <- gsub("congénitasénitas", "congénitas", md, fixed = TRUE)
md <- gsub("Ano de referencia", "Año de referencia", md, fixed = TRUE)
md <- gsub("Muertes para redistribucion", "Muertes para redistribución", md, fixed = TRUE)
md <- gsub("Antes de la redistribucion", "Antes de la redistribución", md, fixed = TRUE)
md <- gsub("Despues de la redistribucion", "Después de la redistribución", md, fixed = TRUE)
md <- gsub("Cambio \\(antes a despues\\)", "Cambio (antes a después)", md)
writeLines(enc2utf8(md), file.path(out_dir, "redistribution_pdf_body.md"), useBytes = TRUE)

for (nm in c("table_3_1_redistribution_groups.csv","table_3_2_impact_total_by_year.csv","table_3_3_impact_by_age_sex_year.csv","table_3_4_before_after_by_disease_group.csv","box_3_2_case_trace_cancer.csv","audit_group_catalog_vs_observed.csv","audit_group_raw_sinadef_presence.csv","audit_group_table31_inclusion.csv","redistribution_text_metrics.json","redistribution_rules_full.csv")) {
  src <- file.path(out_dir, nm)
  if (file.exists(src)) file.copy(src, file.path(downloads_dir, nm), overwrite = TRUE)
}

cat("REWRITE_REDIS_OK\n")
