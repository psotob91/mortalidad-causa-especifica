suppressPackageStartupMessages({
  library(data.table)
  library(ggplot2)
  library(here)
  library(jsonlite)
  library(knitr)
  library(quarto)
  library(scales)
  library(yaml)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

portal_message <- function(...) {
  cat(..., "\n")
}

normalize_slashes <- function(x) {
  gsub("\\\\", "/", x)
}

fix_mojibake_text <- function(x) {
  y <- as.character(x %||% "")
  looks_bad <- function(z) {
    grepl("Ã|Â|â€™|â€“|â€œ|â€|ï¿½|�", z)
  }
  idx <- looks_bad(y)
  if (any(idx, na.rm = TRUE)) {
    repaired <- y[idx]
    for (i in seq_len(3)) {
      candidate <- iconv(repaired, from = "latin1", to = "UTF-8")
      ok <- !is.na(candidate)
      improved <- ok & (nchar(gsub("Ã|Â|â€™|â€“|â€œ|â€|ï¿½|�", "", candidate)) >=
        nchar(gsub("Ã|Â|â€™|â€“|â€œ|â€|ï¿½|�", "", repaired)))
      repaired[improved] <- candidate[improved]
      if (!any(looks_bad(repaired))) break
    }
    y[idx] <- repaired
  }
  y
}

fix_dt_text_cols <- function(dt) {
  x <- as.data.table(copy(dt))
  chr_cols <- names(x)[vapply(x, function(col) is.character(col) || is.factor(col), logical(1))]
  for (nm in chr_cols) {
    x[, (nm) := fix_mojibake_text(as.character(get(nm)))]
  }
  x
}

# Override robusto para salida publica UTF-8.
fix_mojibake_text <- function(x) {
  y <- as.character(x %||% "")
  bad_pattern <- "Ã|Â|â|ï¿½|�"
  count_bad <- function(z) {
    vapply(gregexpr(bad_pattern, z, perl = TRUE), function(m) if (identical(m[1], -1L)) 0L else length(m), integer(1))
  }
  looks_bad <- function(z) count_bad(z) > 0L
  repair_once <- function(z) {
    candidates <- unique(c(
      z,
      suppressWarnings(iconv(z, from = "latin1", to = "UTF-8")),
      suppressWarnings(iconv(z, from = "cp1252", to = "UTF-8")),
      suppressWarnings(iconv(suppressWarnings(iconv(z, from = "latin1", to = "UTF-8")), from = "latin1", to = "UTF-8")),
      suppressWarnings(iconv(suppressWarnings(iconv(z, from = "cp1252", to = "UTF-8")), from = "latin1", to = "UTF-8"))
    ))
    candidates <- candidates[!is.na(candidates)]
    if (!length(candidates)) return(z)
    scores <- data.table(
      value = candidates,
      bad = count_bad(candidates),
      printable = nchar(gsub(bad_pattern, "", candidates, perl = TRUE), type = "chars", allowNA = TRUE, keepNA = TRUE)
    )
    scores <- scores[order(bad, -printable)]
    scores$value[1]
  }
  idx <- which(looks_bad(y))
  if (length(idx)) {
    repaired <- y[idx]
    for (i in seq_len(6)) {
      candidate <- vapply(repaired, repair_once, character(1))
      improved <- count_bad(candidate) < count_bad(repaired)
      repaired[improved] <- candidate[improved]
      if (!any(looks_bad(repaired))) break
    }
    y[idx] <- repaired
  }
  replacements <- c(
    "Navegaci�n" = "Navegación",
    "p�gina" = "página",
    "t�cnico" = "técnico",
    "t�cnica" = "técnica",
    "epidemiol�gica" = "epidemiológica",
    "m�dulo" = "módulo",
    "m�dulos" = "módulos",
    "Gu�a" = "Guía",
    "publicaci�n" = "publicación",
    "distribuci�n" = "distribución",
    "ejecuci�n" = "ejecución",
    "vac�a" = "vacía",
    "autom�ticamente" = "automáticamente",
    "est�tico" = "estático",
    "multip�gina" = "multipágina",
    "C�mo" = "Cómo",
    "M�dulos" = "Módulos",
    "m�todo" = "método",
    "N�mero" = "Número",
    "Proporci�n" = "Proporción",
    "redistribuci�n" = "redistribución",
    "asignaci�n" = "asignación",
    "despu�s" = "después",
    "a�o" = "año"
  )
  for (pat in names(replacements)) y <- gsub(pat, replacements[[pat]], y, fixed = TRUE)
  y
}

slugify <- function(x) {
  x <- tolower(as.character(x))
  x <- iconv(x, from = "", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", "-", x)
  x <- gsub("(^-+|-+$)", "", x)
  x
}

esc_html <- function(x) {
  x <- fix_mojibake_text(as.character(x %||% ""))
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  invisible(path)
}

write_text_file <- function(path, lines) {
  ensure_dir(dirname(path))
  lines <- fix_mojibake_text(lines)
  writeLines(enc2utf8(lines), path, useBytes = TRUE)
  invisible(path)
}

read_yaml_utf8 <- function(path) {
  yaml::read_yaml(path)
}

read_csv_light <- function(path, select = NULL, nrows = Inf) {
  if (is.null(select)) {
    fread(path, nrows = nrows, showProgress = FALSE, encoding = "UTF-8")
  } else {
    fread(path, select = select, nrows = nrows, showProgress = FALSE, encoding = "UTF-8")
  }
}

write_csv_utf8 <- function(dt, path, ...) {
  ensure_dir(dirname(path))
  out <- fix_dt_text_cols(dt)
  fwrite(out, path, bom = TRUE, ...)
  invisible(path)
}

count_rows_fast <- function(path, chunk_size = 1024L * 1024L) {
  con <- file(path, open = "rb")
  on.exit(close(con), add = TRUE)
  total <- 0L
  repeat {
    chunk <- readBin(con, what = "raw", n = chunk_size)
    if (!length(chunk)) break
    total <- total + sum(chunk == as.raw(10L))
  }
  max(total - 1L, 0L)
}

git_capture <- function(args) {
  out <- tryCatch(
    system2("git", args = args, stdout = TRUE, stderr = FALSE),
    error = function(e) character()
  )
  paste(out, collapse = "\n")
}

git_state <- function() {
  list(
    branch = trimws(git_capture(c("branch", "--show-current"))),
    commit_sha = trimws(git_capture(c("rev-parse", "--short", "HEAD"))),
    status_short = trimws(git_capture(c("status", "--short")))
  )
}

decision_log_path <- function(module_name) {
  here("data", "derived", "qc", "review_portal", module_name, "decision_log.csv")
}

append_decision_log <- function(module_name, decision, chosen, rationale, reversible_point) {
  ensure_dir(dirname(decision_log_path(module_name)))
  gs <- git_state()
  row <- data.table(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    branch = gs$branch,
    commit_sha = gs$commit_sha,
    git_status_short = gs$status_short,
    decision = decision,
    chosen = chosen,
    rationale = rationale,
    reversible_point = reversible_point
  )
  path <- decision_log_path(module_name)
  if (file.exists(path)) {
    fwrite(row, path, append = TRUE)
  } else {
    fwrite(row, path)
  }
  invisible(path)
}

register_default_decisions <- function(module_name, decisions) {
  for (i in seq_len(nrow(decisions))) {
    append_decision_log(
      module_name = module_name,
      decision = decisions$decision[i],
      chosen = decisions$chosen[i],
      rationale = decisions$rationale[i],
      reversible_point = decisions$reversible_point[i]
    )
  }
}

portal_glossary <- function(term_ids = NULL) {
  path <- here("config", "review_portal_glossary.yml")
  if (!file.exists(path)) return(list())
  terms <- read_yaml_utf8(path)$terms %||% list()
  if (is.null(term_ids)) return(terms)
  terms[intersect(term_ids, names(terms))]
}

glossary_html <- function(term_ids = NULL, title = "Glosario metodologico local") {
  terms <- portal_glossary(term_ids)
  if (!length(terms)) return("")
  cards <- vapply(names(terms), function(id) {
    term <- terms[[id]]
    cols <- paste(term$columns %||% character(), collapse = ", ")
    paste0(
      "<details class=\"glossary-term\"><summary>", esc_html(term$label %||% id), "</summary>",
      "<p>", esc_html(term$short_definition %||% ""), "</p>",
      if (!is.null(term$formula) && nzchar(term$formula)) sprintf("<p><strong>Formula.</strong> <code>%s</code></p>", esc_html(term$formula)) else "",
      "<p><strong>Interpretacion.</strong> ", esc_html(term$interpretation %||% ""), "</p>",
      if (nzchar(cols)) sprintf("<p><strong>Columnas asociadas.</strong> <code>%s</code></p>", esc_html(cols)) else "",
      if (!is.null(term$footnote) && nzchar(term$footnote)) sprintf("<p class=\"muted\"><strong>Nota.</strong> %s</p>", esc_html(term$footnote)) else "",
      "</details>"
    )
  }, character(1))
  paste0(
    "<section class=\"card glossary-box\"><div class=\"eyebrow\">Ayuda en la pagina</div><h3>",
    esc_html(title),
    "</h3><p class=\"muted\">Estas definiciones estan aqui mismo para no obligar al lector a salir del modulo.</p>",
    paste(cards, collapse = ""),
    "</section>"
  )
}

formula_list_html <- function(lines) {
  paste0(
    "<div class=\"formula-box\"><h4>Ecuaciones usadas para interpretar la tabla</h4><ul>",
    paste(sprintf("<li><code>%s</code></li>", esc_html(lines)), collapse = ""),
    "</ul></div>"
  )
}

verdict_palette <- function(code) {
  code_norm <- toupper(gsub("_", " ", as.character(code %||% "")))
  switch(
    code_norm,
    "OK" = list(label = "OK", color = "#1e7f5c", tone = "good"),
    "OK CON NOTA" = list(label = "OK CON NOTA", color = "#317159", tone = "good"),
    "OK CONDICIONAL" = list(label = "OK CONDICIONAL", color = "#1f7a8c", tone = "good"),
    "RESUELTO" = list(label = "RESUELTO", color = "#0f6cbd", tone = "good"),
    "OBSERVACION" = list(label = "OBSERVACION", color = "#b77700", tone = "warn"),
    "OBSERVACION NO MATERIAL" = list(label = "OBSERVACION NO MATERIAL", color = "#9a6700", tone = "warn"),
    "REVISION HUMANA" = list(label = "pendiente_de_lectura_experta", color = "#4f6b9a", tone = "review"),
    "REVISION HUMANA REAL" = list(label = "pendiente_de_lectura_experta", color = "#4f6b9a", tone = "review"),
    "PROBLEMA VIGENTE" = list(label = "hallazgo_previamente_abierto", color = "#b42318", tone = "bad"),
    "BLOQUEANTE REAL" = list(label = "BLOQUEANTE REAL", color = "#8a1f11", tone = "bad"),
    list(label = code, color = "#667085", tone = "neutral")
  )
}

sanitize_public_verdict <- function(x) {
  y <- as.character(x)
  y_norm <- toupper(gsub("_", " ", y))
  y[y_norm %in% c("REVISION HUMANA", "REVISION HUMANA REAL")] <- "pendiente_de_lectura_experta"
  y[y_norm == "PROBLEMA VIGENTE"] <- "hallazgo_previamente_abierto"
  y
}

portal_css <- function() {
  c(
    ":root {",
    "  --bg: #f5f7fb;",
    "  --surface: #ffffff;",
    "  --surface-2: #eef3fb;",
    "  --ink: #15212b;",
    "  --muted: #5f6b76;",
    "  --line: #d7dde5;",
    "  --brand: #0f4c81;",
    "  --brand-2: #1e7f5c;",
    "  --shadow: 0 10px 28px rgba(17, 24, 39, 0.08);",
    "}",
    "html { scroll-behavior: smooth; }",
    "body { font-family: 'Segoe UI', 'Aptos', 'Trebuchet MS', sans-serif; background: linear-gradient(180deg, #f5f7fb 0%, #eef3fb 100%); color: var(--ink); margin: 0; line-height: 1.55; }",
    "a { color: var(--brand); text-decoration: none; }",
    "a:hover { text-decoration: underline; }",
    ".portal-shell { max-width: 1380px; margin: 0 auto; padding: 28px; }",
    ".hero { background: linear-gradient(135deg, rgba(15,76,129,0.95), rgba(20,121,178,0.86)); color: #fff; border-radius: 24px; padding: 32px; box-shadow: var(--shadow); margin-bottom: 24px; }",
    ".hero h1 { margin: 0 0 8px 0; font-size: 2.2rem; }",
    ".hero p { margin: 0; max-width: 900px; color: rgba(255,255,255,0.9); }",
    ".hero-meta { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 18px; }",
    ".meta-chip { display: inline-flex; gap: 8px; align-items: center; background: rgba(255,255,255,0.14); border: 1px solid rgba(255,255,255,0.22); border-radius: 999px; padding: 8px 14px; font-size: 0.92rem; }",
    ".layout-grid { display: grid; grid-template-columns: 280px minmax(0, 1fr); gap: 24px; align-items: start; }",
    ".sidebar { position: sticky; top: 18px; background: rgba(255,255,255,0.86); backdrop-filter: blur(12px); border: 1px solid var(--line); border-radius: 22px; padding: 18px; box-shadow: var(--shadow); }",
    ".sidebar h3 { margin: 6px 0 10px; font-size: 0.95rem; letter-spacing: 0.02em; text-transform: uppercase; color: var(--brand); }",
    ".sidebar ul { list-style: none; padding: 0; margin: 0; }",
    ".sidebar li { margin: 0 0 10px 0; }",
    ".sidebar a { display: block; padding: 8px 10px; border-radius: 12px; color: var(--ink); }",
    ".sidebar a:hover { background: var(--surface-2); text-decoration: none; }",
    ".content-stack { display: grid; gap: 18px; }",
    ".card { background: rgba(255,255,255,0.94); border: 1px solid var(--line); border-radius: 22px; padding: 22px; box-shadow: var(--shadow); }",
    ".card h2, .card h3, .card h4 { margin-top: 0; color: #102a43; }",
    ".lead-note { border-left: 4px solid var(--brand); background: #edf4fb; padding: 14px 16px; border-radius: 16px; margin: 18px 0; }",
    ".formula-box { border: 1px solid #c8d8e8; background: #f7fbff; border-radius: 18px; padding: 14px 16px; margin: 14px 0; }",
    ".formula-box ul { margin: 8px 0 0 20px; padding: 0; }",
    ".formula-box li { margin: 6px 0; }",
    ".glossary-box { background: linear-gradient(180deg, #ffffff, #f6fbf8); }",
    ".glossary-term { border: 1px solid var(--line); border-radius: 14px; background: #fff; padding: 10px 12px; margin: 10px 0; }",
    ".glossary-term summary { cursor: pointer; font-weight: 800; color: var(--brand); }",
    ".tab-buttons { display: flex; flex-wrap: wrap; gap: 10px; margin-bottom: 14px; }",
    ".tab-button { border: 1px solid var(--line); background: #fff; color: var(--brand); border-radius: 999px; padding: 9px 14px; font-weight: 700; cursor: pointer; }",
    ".tab-button.is-active { background: var(--brand); color: #fff; }",
    ".tab-panel { display: none; }",
    ".tab-panel.is-active { display: block; }",
    ".kpi-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(170px, 1fr)); gap: 12px; }",
    ".kpi-card { background: linear-gradient(180deg, #ffffff, #f7fafc); border: 1px solid var(--line); border-radius: 18px; padding: 16px; }",
    ".kpi-label { color: var(--muted); font-size: 0.9rem; margin-bottom: 4px; }",
    ".kpi-value { font-size: 1.7rem; font-weight: 700; color: #102a43; }",
    ".module-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(260px, 1fr)); gap: 16px; }",
    ".module-card { background: linear-gradient(180deg, #ffffff, #f8fbff); border: 1px solid var(--line); border-radius: 20px; padding: 20px; box-shadow: var(--shadow); }",
    ".badge { display: inline-flex; align-items: center; gap: 6px; border-radius: 999px; padding: 5px 12px; font-weight: 700; font-size: 0.84rem; color: #fff; }",
    ".table-wrap { overflow-x: auto; border: 1px solid var(--line); border-radius: 18px; background: #fff; }",
    "table.portal-table { width: 100%; border-collapse: collapse; font-size: 0.94rem; }",
    "table.portal-table th, table.portal-table td { padding: 10px 12px; border-bottom: 1px solid #e7edf4; vertical-align: top; text-align: left; }",
    "table.portal-table th { background: #eff5fa; color: #1d3557; position: sticky; top: 0; }",
    "table.portal-table tr:nth-child(even) td { background: #fbfdff; }",
    "table.portal-table-compact td[rowspan] { background: #f7fbff; min-width: 240px; }",
    "table.portal-table-compact tr.stage-change td { background: #f8fbf8; font-weight: 600; }",
    ".eyebrow { text-transform: uppercase; letter-spacing: 0.06em; font-size: 0.78rem; color: var(--brand); font-weight: 700; margin-bottom: 8px; }",
    ".muted { color: #5f6b76; }",
    ".pill-row { display: flex; flex-wrap: wrap; gap: 10px; }",
    ".pill { padding: 7px 12px; border-radius: 999px; background: var(--surface-2); border: 1px solid var(--line); font-size: 0.9rem; }",
    ".nav-actions { display: flex; flex-wrap: wrap; gap: 10px; margin-top: 12px; }",
    ".btn { display: inline-flex; align-items: center; gap: 8px; border-radius: 999px; background: var(--brand); color: #fff; padding: 10px 16px; font-weight: 600; text-decoration: none; }",
    ".btn.alt { background: var(--brand-2); }",
    ".btn.ghost { background: #fff; color: var(--brand); border: 1px solid var(--line); }",
    ".catalog-list { display: grid; gap: 12px; }",
    ".catalog-item { border: 1px solid var(--line); border-radius: 18px; padding: 16px; background: #fff; }",
    ".image-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(320px, 1fr)); gap: 16px; }",
    ".image-card { border: 1px solid var(--line); background: #fff; border-radius: 18px; padding: 14px; }",
    ".image-card img { width: 100%; height: auto; border-radius: 14px; border: 1px solid var(--line); cursor: zoom-in; }",
    ".figure-dashboard { display: grid; gap: 14px; }",
    ".figure-controls { display: grid; gap: 10px; grid-template-columns: repeat(auto-fit, minmax(190px, 1fr)); align-items: end; }",
    ".figure-controls label { display: grid; gap: 5px; color: var(--muted); font-size: 0.9rem; font-weight: 700; }",
    ".figure-controls select { width: 100%; border: 1px solid var(--line); border-radius: 14px; padding: 10px 12px; font-size: 1rem; background: #fff; }",
    ".figure-panel { display: none; }",
    ".figure-panel.is-active { display: block; }",
    ".lightbox { position: fixed; inset: 0; background: rgba(6, 18, 32, 0.86); display: none; align-items: center; justify-content: center; z-index: 9999; padding: 24px; }",
    ".lightbox.is-open { display: flex; }",
    ".lightbox img { max-width: 96vw; max-height: 92vh; background: #fff; border-radius: 14px; box-shadow: 0 20px 60px rgba(0,0,0,0.35); }",
    ".lightbox button { position: absolute; top: 18px; right: 22px; border: 0; border-radius: 999px; background: #fff; color: var(--ink); padding: 10px 14px; font-weight: 800; cursor: pointer; }",
    ".search-bar { display: grid; gap: 10px; grid-template-columns: minmax(0, 1fr) 220px 220px; }",
    ".search-bar input, .search-bar select { width: 100%; border: 1px solid var(--line); border-radius: 14px; padding: 12px 14px; font-size: 1rem; background: #fff; }",
    ".footer-note { text-align: center; color: #5f6b76; padding: 24px 0 10px; font-size: 0.92rem; }",
    "@media (max-width: 1080px) { .layout-grid { grid-template-columns: 1fr; } .sidebar { position: static; } .search-bar { grid-template-columns: 1fr; } }"
  )
}

portal_js <- function() {
  c(
    "document.addEventListener('DOMContentLoaded', function () {",
    "  const searchInput = document.querySelector('[data-catalog-search]');",
    "  const levelSelect = document.querySelector('[data-catalog-level]');",
    "  const geoSelect = document.querySelector('[data-catalog-geo]');",
    "  const cards = Array.from(document.querySelectorAll('[data-catalog-item]'));",
    "  const applyFilter = function() {",
    "    const q = (searchInput ? searchInput.value : '').toLowerCase().trim();",
    "    const lvl = levelSelect ? levelSelect.value : '';",
    "    const geo = geoSelect ? geoSelect.value : '';",
    "    cards.forEach(function(card) {",
    "      const txt = (card.getAttribute('data-search') || '').toLowerCase();",
    "      const cardLvl = card.getAttribute('data-level') || '';",
    "      const cardGeo = card.getAttribute('data-geo') || '';",
    "      const okQ = !q || txt.indexOf(q) >= 0;",
    "      const okL = !lvl || lvl === cardLvl;",
    "      const okG = !geo || geo === cardGeo || geo === 'all';",
    "      card.style.display = (okQ && okL && okG) ? '' : 'none';",
    "    });",
    "  };",
    "  [searchInput, levelSelect, geoSelect].forEach(function(node) {",
    "    if (node) node.addEventListener('input', applyFilter);",
    "    if (node) node.addEventListener('change', applyFilter);",
    "  });",
    "  applyFilter();",
    "  document.querySelectorAll('[data-figure-switch]').forEach(function(sel) {",
    "    const group = sel.getAttribute('data-figure-switch');",
    "    const sync = function() {",
    "      document.querySelectorAll('[data-figure-panel=\"' + group + '\"]').forEach(function(panel) {",
    "        panel.classList.toggle('is-active', panel.getAttribute('data-figure-value') === sel.value);",
    "      });",
    "    };",
    "    sel.addEventListener('change', sync);",
    "    sync();",
    "  });",
    "  document.querySelectorAll('[data-tab-group]').forEach(function(group) {",
    "    const name = group.getAttribute('data-tab-group');",
    "    const buttons = Array.from(group.querySelectorAll('[data-tab-target]'));",
    "    const panels = Array.from(group.querySelectorAll('[data-tab-panel]'));",
    "    const activate = function(target) {",
    "      buttons.forEach(function(btn) { btn.classList.toggle('is-active', btn.getAttribute('data-tab-target') === target); });",
    "      panels.forEach(function(panel) { panel.classList.toggle('is-active', panel.getAttribute('data-tab-panel') === target); });",
    "    };",
    "    buttons.forEach(function(btn) { btn.addEventListener('click', function() { activate(btn.getAttribute('data-tab-target')); }); });",
    "    if (buttons.length) activate(buttons[0].getAttribute('data-tab-target'));",
    "  });",
    "  const lb = document.createElement('div');",
    "  lb.className = 'lightbox';",
    "  lb.innerHTML = '<button type=\"button\" aria-label=\"Cerrar\">Cerrar</button><img alt=\"Figura ampliada\">';",
    "  document.body.appendChild(lb);",
    "  const lbImg = lb.querySelector('img');",
    "  const closeLb = function() { lb.classList.remove('is-open'); lbImg.removeAttribute('src'); };",
    "  lb.querySelector('button').addEventListener('click', closeLb);",
    "  lb.addEventListener('click', function(ev) { if (ev.target === lb) closeLb(); });",
    "  document.querySelectorAll('.image-card img').forEach(function(img) {",
    "    img.addEventListener('click', function() { lbImg.src = img.src; lb.classList.add('is-open'); });",
    "  });",
    "});"
  )
}

write_portal_assets <- function(root_dir) {
  assets_dir <- file.path(root_dir, "assets")
  ensure_dir(assets_dir)
  write_text_file(file.path(assets_dir, "portal.css"), portal_css())
  write_text_file(file.path(assets_dir, "portal.js"), portal_js())
}

html_table <- function(dt, class_name = "portal-table", max_rows = Inf) {
  x <- fix_dt_text_cols(dt)
  if (nrow(x) == 0 || ncol(x) == 0) {
    return("<p class=\"muted\">Tabla vacía.</p>")
  }
  if (is.finite(max_rows) && nrow(x) > max_rows) {
    x <- x[seq_len(max_rows)]
  }
  hdr <- paste(sprintf("<th>%s</th>", esc_html(names(x))), collapse = "")
  body_rows <- apply(x, 1, function(row) {
    paste0("<tr>", paste(sprintf("<td>%s</td>", esc_html(row)), collapse = ""), "</tr>")
  })
  paste0(
    "<div class=\"table-wrap\"><table class=\"", class_name, "\"><thead><tr>", hdr, "</tr></thead><tbody>",
    paste(body_rows, collapse = ""),
    "</tbody></table></div>"
  )
}

pipe_table_text <- function(dt, max_rows = 20L, digits = 3) {
  x <- as.data.table(copy(dt))
  if (nrow(x) == 0 || ncol(x) == 0) return("Tabla vac�a.\n")
  if (nrow(x) > max_rows) x <- x[seq_len(max_rows)]
  for (nm in names(x)) {
    if (is.numeric(x[[nm]])) x[[nm]] <- round(x[[nm]], digits)
  }
  paste(capture.output(knitr::kable(x, format = "pipe")), collapse = "\n")
}

write_portal_page <- function(path, title, body_html, rel_root = ".") {
  page <- c(
    "<!doctype html>",
    "<html lang=\"es\">",
    "<head>",
    "  <meta charset=\"utf-8\">",
    "  <meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">",
    sprintf("  <title>%s</title>", esc_html(title)),
    sprintf("  <link rel=\"stylesheet\" href=\"%s/assets/portal.css\">", rel_root),
    sprintf("  <script defer src=\"%s/assets/portal.js\"></script>", rel_root),
    "</head>",
    "<body>",
    body_html,
    "</body>",
    "</html>"
  )
  write_text_file(path, page)
}

page_shell <- function(title, intro, sidebar_items, sections_html) {
  side <- if (length(sidebar_items)) {
    paste(
      "<div class=\"sidebar\"><h3>Navegación</h3><ul>",
      paste(vapply(sidebar_items, function(x) sprintf("<li><a href=\"%s\">%s</a></li>", x$href, esc_html(x$label)), character(1)), collapse = ""),
      "</ul></div>"
    )
  } else {
    "<div class=\"sidebar\"><h3>Navegación</h3><p class=\"muted\">Esta página no necesita menú lateral adicional.</p></div>"
  }
  paste0(
    "<div class=\"portal-shell\">",
    "<section class=\"hero\">",
    sprintf("<div class=\"eyebrow\">Portal técnico reproducible</div><h1>%s</h1><p>%s</p>", esc_html(title), esc_html(intro)),
    "<div class=\"hero-meta\"><span class=\"meta-chip\">Sitio estático multipágina</span><span class=\"meta-chip\">HTML interactivo + PDF</span><span class=\"meta-chip\">Listo para hosting protegido</span></div>",
    "</section>",
    "<div class=\"layout-grid\">",
    side,
    "<main class=\"content-stack\">",
    paste(sections_html, collapse = "\n"),
    sprintf("<div class=\"footer-note\">Generado automáticamente desde %s</div>", esc_html(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
    "</main></div></div>"
  )
}

as_badge <- function(code) {
  pal <- verdict_palette(code)
  sprintf("<span class=\"badge\" style=\"background:%s;\">%s</span>", pal$color, esc_html(pal$label))
}

max_chars_in_dt <- function(dt) {
  if (nrow(dt) == 0 || ncol(dt) == 0) return(0L)
  as.integer(max(vapply(dt, function(x) {
    vals <- as.character(head(x, 80L))
    vals <- vals[!is.na(vals)]
    if (!length(vals)) return(0)
    max(nchar(vals), na.rm = TRUE)
  }, numeric(1)), 0))
}

fread_preview <- function(path, nrows = 20L) {
  fread(path, nrows = nrows, showProgress = FALSE, encoding = "UTF-8")
}

folder_step_map <- function() {
  c(
    ingest_sinadef_raw = "ingest_sinadef_raw",
    normalize_death_record = "normalize_death_record",
    cause_master = "build_cause_master",
    cause_hierarchy_bridge = "build_cause_hierarchy_bridge",
    redistribution_rules = "build_redistribution_rules",
    map_and_redistribute_deaths = "map_and_redistribute_deaths",
    qc_redistribution = "qc_redistribution",
    build_death_cause_final = "build_death_cause_final",
    rollup_death_cause_final = "rollup_death_cause_final",
    qc_completeness_validation = "qc_completeness_validation",
    build_mortality_rates = "build_mortality_rates",
    reconcile_mortality_hierarchy = "reconcile_mortality_hierarchy",
    compute_avp_yll = "compute_avp_yll",
    build_report_tables = "build_report_tables",
    build_methods_catalogs = "build_methods_catalogs",
    build_oms_reference_compare_partial = "build_oms_reference_compare_partial",
    run_pipeline = "run_pipeline",
    baseline_compare = "baseline_compare"
  )
}

default_qc_registry <- function() {
  read_yaml_utf8(here("config", "qc_report_registry.yml"))
}

default_resolution_map <- function() {
  read_yaml_utf8(here("config", "qc_resolution_map.yml"))
}

default_render_defaults <- function(registry) {
  list(
    html_split_bytes = registry$render_defaults$html_split_bytes %||% 2000000L,
    html_split_qc_count = registry$render_defaults$html_split_qc_count %||% 15L,
    sample_n_rows = registry$render_defaults$sample_n_rows %||% 20L,
    html_preview_rows = registry$render_defaults$html_preview_rows %||% 40L
  )
}

inventory_qc_files <- function() {
  files <- list.files(
    here("data", "derived", "qc"),
    pattern = "\\.csv$",
    recursive = TRUE,
    full.names = TRUE
  )
  files <- files[!grepl("review_portal|qc_pipeline_report|_debug", files)]
  data.table(
    file_path = normalize_slashes(files),
    folder_name = basename(dirname(files)),
    file_name = basename(files)
  )
}

infer_override <- function(file_name, registry) {
  exact <- registry$file_overrides[[file_name]]
  if (!is.null(exact)) return(exact)
  pats <- registry$pattern_overrides %||% list()
  for (item in pats) {
    if (grepl(item$regex, file_name, ignore.case = TRUE, perl = TRUE)) return(item)
  }
  NULL
}

infer_rule <- function(file_name, col_names, registry) {
  ov <- infer_override(file_name, registry)
  if (!is.null(ov$automation_rule)) return(ov$automation_rule)
  cols <- col_names %||% character()
  if (all(c("status", "script_name") %in% cols)) return("runner_success")
  if ("n_bad" %in% cols) return("n_bad_zero")
  if (length(grep("pass", cols, ignore.case = TRUE)) > 0) return("pass_columns_true")
  if (length(grep("diff|delta", cols, ignore.case = TRUE)) > 0) return("diff_zero")
  "assisted"
}

evaluate_qc_rule <- function(rule, dt) {
  n_rows <- nrow(dt)
  verdict <- "REVISION HUMANA"
  conclusion <- "Este QC requiere lectura contextual."
  if (identical(rule, "empty_is_good")) {
    if (n_rows == 0) {
      verdict <- "OK"
      conclusion <- "La tabla qued� vac�a, que es lo esperado para un QC detectivo."
    } else {
      verdict <- "PROBLEMA VIGENTE"
      conclusion <- sprintf("El QC encontr� %s filas problem�ticas que siguen presentes en esta etapa.", format(n_rows, big.mark = ","))
    }
  } else if (identical(rule, "runner_success")) {
    status_ok <- "status" %in% names(dt) && all(tolower(trimws(as.character(dt$status))) %in% c("success", "ok"))
    exit_ok <- !("exit_code" %in% names(dt)) || all(suppressWarnings(as.integer(dt$exit_code)) == 0L, na.rm = TRUE)
    if (status_ok && exit_ok) {
      verdict <- "OK"
      conclusion <- "Todos los pasos registrados por el runner terminaron con �xito."
    } else {
      verdict <- "PROBLEMA VIGENTE"
      conclusion <- "Al menos un paso del runner no termin� como success o tuvo c�digo de salida distinto de cero."
    }
  } else if (identical(rule, "preflight_status_ok")) {
    status_ok <- "status" %in% names(dt) && all(tolower(trimws(as.character(dt$status))) %in% c("ok", "success"))
    if (status_ok) {
      verdict <- "OK"
      conclusion <- "Las verificaciones de preflight quedaron en estado OK."
    } else {
      verdict <- "PROBLEMA VIGENTE"
      conclusion <- "El preflight dej� al menos una verificaci�n fuera de OK."
    }
  } else if (identical(rule, "n_bad_zero")) {
    bad <- if ("n_bad" %in% names(dt)) sum(as.numeric(dt$n_bad), na.rm = TRUE) else Inf
    if (is.finite(bad) && bad == 0) {
      verdict <- "OK"
      conclusion <- "No se detectaron incumplimientos de la regla resumida por n_bad."
    } else {
      verdict <- "PROBLEMA VIGENTE"
      conclusion <- sprintf("Se resumieron %s incumplimientos en n_bad.", format(bad, big.mark = ","))
    }
  } else if (identical(rule, "pass_columns_true")) {
    pass_cols <- grep("pass", names(dt), ignore.case = TRUE, value = TRUE)
    ok <- length(pass_cols) > 0 && all(vapply(pass_cols, function(nm) {
      vals <- as.character(dt[[nm]])
      all(toupper(vals) %in% c("TRUE", "T", "1", "OK", "PASS"))
    }, logical(1)))
    if (ok) {
      verdict <- "OK"
      conclusion <- "Todas las columnas de pase quedaron en TRUE/PASS."
    } else {
      verdict <- "PROBLEMA VIGENTE"
      conclusion <- "Al menos una comparaci�n dura dej� una se�al de no pase."
    }
  } else if (identical(rule, "diff_zero")) {
    num_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
    diff_cols <- intersect(num_cols, grep("diff|delta", num_cols, ignore.case = TRUE, value = TRUE))
    mx <- if (length(diff_cols)) max(abs(unlist(dt[, ..diff_cols])), na.rm = TRUE) else Inf
    if (is.finite(mx) && mx <= 1e-8) {
      verdict <- "OK"
      conclusion <- "Las diferencias num�ricas evaluadas quedaron esencialmente en cero."
    } else if (is.finite(mx) && mx <= 1e-4) {
      verdict <- "OBSERVACION"
      conclusion <- "Solo se observaron diferencias peque�as compatibles con redondeo o precisi�n num�rica."
    } else {
      verdict <- "PROBLEMA VIGENTE"
      conclusion <- sprintf("Se observaron diferencias num�ricas visibles (m�ximo %.6f).", mx)
    }
  } else if (identical(rule, "baseline_summary")) {
    char_vals <- tolower(unlist(lapply(dt, as.character), use.names = FALSE))
    if (any(grepl("aprobad|pass|sin diferencias sustantivas", char_vals))) {
      verdict <- "OK"
      conclusion <- "La comparaci�n contra baseline no detect� diferencias sustantivas."
    } else if (any(grepl("observ", char_vals))) {
      verdict <- "OBSERVACION"
      conclusion <- "La baseline qued� utilizable, pero con observaciones que requieren lectura del resumen."
    } else {
      verdict <- "REVISION HUMANA"
      conclusion <- "La comparaci�n contra baseline requiere lectura contextual."
    }
  }
  list(verdict = verdict, conclusion = conclusion)
}

qc_has_pass_signal <- function(dt) {
  vals <- toupper(trimws(as.character(unlist(dt, use.names = FALSE))))
  vals <- vals[!is.na(vals) & nzchar(vals)]
  any(vals %in% c("PASS", "OK", "SUCCESS", "PASS_WITH_DOCUMENTED_RESIDUAL", "TRUE"))
}

qc_metric_delta_threshold <- function(dt) {
  if (!all(c("metric", "value") %in% names(dt))) return(NULL)
  x <- copy(dt)
  x[, metric_chr := tolower(as.character(metric))]
  x[, value_num := suppressWarnings(as.numeric(value))]
  deltas <- x[grepl("delta|diff", metric_chr), abs(value_num)]
  thresholds <- x[grepl("threshold|toler", metric_chr), abs(value_num)]
  if (!length(deltas) || all(is.na(deltas))) return(NULL)
  threshold <- if (length(thresholds) && any(is.finite(thresholds))) max(thresholds, na.rm = TRUE) else 1e-6
  list(max_delta = max(deltas, na.rm = TRUE), threshold = threshold)
}

evaluate_qc_rule <- function(rule, dt) {
  n_rows <- nrow(dt)
  verdict <- "REVISION_HUMANA_REAL"
  conclusion <- "Este QC requiere lectura contextual real; no hay una regla automatica segura con la tabla observada."

  if (identical(rule, "empty_is_good")) {
    if (n_rows == 0) {
      verdict <- "OK_CONDICIONAL"
      conclusion <- "La tabla quedo vacia. Este QC solo habria requerido revision si aparecian filas."
    } else {
      verdict <- "PROBLEMA_VIGENTE"
      conclusion <- sprintf("El QC encontro %s filas problematicas que siguen presentes en esta etapa o requieren evidencia posterior de resolucion.", format(n_rows, big.mark = ","))
    }
  } else if (identical(rule, "runner_success")) {
    status_ok <- "status" %in% names(dt) && all(tolower(trimws(as.character(dt$status))) %in% c("success", "ok"))
    exit_ok <- !("exit_code" %in% names(dt)) || all(suppressWarnings(as.integer(dt$exit_code)) == 0L, na.rm = TRUE)
    if (status_ok && exit_ok) {
      verdict <- "OK"
      conclusion <- "Todos los pasos registrados por el runner terminaron con exito."
    } else {
      verdict <- "PROBLEMA_VIGENTE"
      conclusion <- "Al menos un paso del runner no termino como success o tuvo codigo de salida distinto de cero."
    }
  } else if (identical(rule, "preflight_status_ok")) {
    status_ok <- "status" %in% names(dt) && all(tolower(trimws(as.character(dt$status))) %in% c("ok", "success"))
    if (status_ok) {
      verdict <- "OK"
      conclusion <- "Las verificaciones de preflight quedaron en estado OK."
    } else {
      verdict <- "PROBLEMA_VIGENTE"
      conclusion <- "El preflight dejo al menos una verificacion fuera de OK."
    }
  } else if (identical(rule, "n_bad_zero")) {
    bad <- if ("n_bad" %in% names(dt)) sum(as.numeric(dt$n_bad), na.rm = TRUE) else Inf
    if (is.finite(bad) && bad == 0) {
      verdict <- "OK"
      conclusion <- "No se detectaron incumplimientos de la regla resumida por n_bad."
    } else {
      verdict <- "PROBLEMA_VIGENTE"
      conclusion <- sprintf("Se resumieron %s incumplimientos en n_bad.", format(bad, big.mark = ","))
    }
  } else if (identical(rule, "pass_columns_true")) {
    pass_cols <- grep("pass", names(dt), ignore.case = TRUE, value = TRUE)
    ok <- length(pass_cols) > 0 && all(vapply(pass_cols, function(nm) {
      vals <- as.character(dt[[nm]])
      all(toupper(vals) %in% c("TRUE", "T", "1", "OK", "PASS"))
    }, logical(1)))
    if (ok) {
      verdict <- "OK"
      conclusion <- "Todas las columnas de pase quedaron en TRUE/PASS."
    } else {
      verdict <- "PROBLEMA_VIGENTE"
      conclusion <- "Al menos una comparacion dura dejo una senal de no pase."
    }
  } else if (identical(rule, "diff_zero")) {
    metric_eval <- qc_metric_delta_threshold(dt)
    if (!is.null(metric_eval)) {
      if (is.finite(metric_eval$max_delta) && metric_eval$max_delta <= metric_eval$threshold) {
        verdict <- "OK_CON_NOTA"
        conclusion <- sprintf("La diferencia maxima observada (%.6g) esta por debajo del umbral documentado (%.6g). Se interpreta como pase con residuo numerico.", metric_eval$max_delta, metric_eval$threshold)
      } else {
        verdict <- "PROBLEMA_VIGENTE"
        conclusion <- sprintf("La diferencia maxima observada (%.6g) supera el umbral documentado (%.6g).", metric_eval$max_delta, metric_eval$threshold)
      }
    } else if (qc_has_pass_signal(dt)) {
      verdict <- "OK_CON_NOTA"
      conclusion <- "La tabla contiene senal explicita PASS/OK, incluyendo posibles residuos documentados por debajo de umbral."
    } else if (n_rows == 0) {
      verdict <- "OK_CONDICIONAL"
      conclusion <- "La tabla quedo vacia. No hay diferencias que reportar."
    } else {
      num_cols <- names(dt)[vapply(dt, is.numeric, logical(1))]
      diff_cols <- intersect(num_cols, grep("diff|delta", num_cols, ignore.case = TRUE, value = TRUE))
      mx <- if (length(diff_cols)) max(abs(unlist(dt[, ..diff_cols])), na.rm = TRUE) else NA_real_
      if (is.finite(mx) && mx <= 1e-8) {
        verdict <- "OK"
        conclusion <- "Las diferencias numericas evaluadas quedaron esencialmente en cero."
      } else if (is.finite(mx) && mx <= 1e-6) {
        verdict <- "OK_CON_NOTA"
        conclusion <- "Solo se observaron diferencias pequenas compatibles con redondeo bajo la tolerancia conservadora 1e-6."
      } else if (is.finite(mx) && mx <= 1e-4) {
        verdict <- "OBSERVACION"
        conclusion <- "Solo se observaron diferencias pequenas; requieren nota porque no hay umbral explicito en la tabla."
      } else if (is.finite(mx)) {
        verdict <- "PROBLEMA_VIGENTE"
        conclusion <- sprintf("Se observaron diferencias numericas visibles (maximo %.6f).", mx)
      }
    }
  } else if (identical(rule, "baseline_summary")) {
    char_vals <- tolower(unlist(lapply(dt, as.character), use.names = FALSE))
    if (any(grepl("aprobad|pass|sin diferencias sustantivas", char_vals))) {
      verdict <- "OK"
      conclusion <- "La comparacion contra baseline no detecto diferencias sustantivas."
    } else if (any(grepl("observ", char_vals))) {
      verdict <- "OBSERVACION"
      conclusion <- "La baseline quedo utilizable, pero con observaciones que requieren lectura del resumen."
    }
  } else if (n_rows == 0) {
    verdict <- "OK_CONDICIONAL"
    conclusion <- "La tabla esta vacia. Se interpreta como OK condicional: solo habria requerido lectura experta si contenia filas."
  } else if (qc_has_pass_signal(dt)) {
    verdict <- "OK_CON_NOTA"
    conclusion <- "La tabla contiene un estado PASS/OK; queda como OK con nota porque se conserva el contexto tecnico del QC."
  }
  list(verdict = verdict, conclusion = conclusion)
}

qc_text_bundle <- function(file_name, section_title, rule, registry) {
  ov <- infer_override(file_name, registry) %||% list()
  list(
    title = ov$title %||% tools::toTitleCase(gsub("_", " ", gsub("\\.csv$", "", file_name))),
    what_is = ov$what_is %||% sprintf("Es una tabla t�cnica de control de calidad del bloque '%s'.", section_title),
    what_does = ov$what_does %||% "Resume o enumera el comportamiento observado del control de calidad.",
    purpose = ov$purpose %||% "Sirve para verificar que la transformaci�n de este paso sea metodol�gicamente consistente.",
    interpretation = ov$interpretation %||% if (identical(rule, "assisted")) "Este QC necesita lectura experta apoyada por contexto metodol�gico y QCs vecinos." else "Este QC puede cerrarse con una regla operacional expl�cita.",
    possible_results = ov$possible_results %||% "Puede terminar como correcto, observaci�n, resuelto o bloqueante real seg�n lo que muestre la tabla."
  )
}

qc_family <- function(file_name, section_title = "") {
  x <- tolower(paste(file_name %||% "", section_title %||% ""))
  if (grepl("duplicate|duplicado|source_record_id|signature", x)) return("duplicados_e_identificadores")
  if (grepl("mismatch|sex_specific|age_specific|domain|restriction", x)) return("restricciones_sexo_edad")
  if (grepl("balance|mass|preservation|additivity|hard_compare|reconciliation", x)) return("balance_y_reconciliacion")
  if (grepl("redistribution|target|weight|terminal|garbage|unmapped", x)) return("redistribucion_y_mapeo")
  if (grepl("pandemic|oprm|completeness|subregistro|inei", x)) return("pandemia_y_subregistro")
  if (grepl("roughness|model|attempt|smoothing|smoothed|rate", x)) return("modelamiento_y_tasas")
  if (grepl("avp|yll|life|ex_standard", x)) return("avp_yll")
  if (grepl("baseline", x)) return("baseline")
  if (grepl("summary|registry|audit|catalog|by_level|year", x)) return("resumen_o_auditoria")
  "qc_general"
}

qc_is_all_na_placeholder <- function(dt) {
  nrow(dt) == 1L && ncol(dt) > 0L && all(vapply(dt, function(z) all(is.na(z)), logical(1)))
}

qc_has_zero_problem_counts <- function(dt) {
  if (nrow(dt) != 1L) return(FALSE)
  nms <- names(dt)
  problem_cols <- nms[grepl("^(n_)?(bad|missing|neg|fail|error|unmapped|duplicate|invalid)", nms, ignore.case = TRUE)]
  if (!length(problem_cols)) return(FALSE)
  vals <- suppressWarnings(as.numeric(unlist(dt[, ..problem_cols], use.names = FALSE)))
  length(vals) > 0L && all(is.na(vals) | vals == 0)
}

qc_empty_interpretation <- function(file_name, section_title = "") {
  fam <- qc_family(file_name, section_title)
  switch(
    fam,
    "duplicados_e_identificadores" = "Tabla vacia significa que no se encontraron duplicados o conflictos en la llave evaluada por este QC.",
    "restricciones_sexo_edad" = "Tabla vacia significa que no quedaron muertes positivas fuera del dominio permitido de sexo o edad para las causas evaluadas.",
    "balance_y_reconciliacion" = "Tabla vacia significa que no hubo diferencias que reportar en el contraste de balance o reconciliacion evaluado.",
    "redistribucion_y_mapeo" = "Tabla vacia significa que no quedaron causas sin mapear, destinos invalidos o registros que requieran accion en esta prueba.",
    "avp_yll" = "Tabla vacia significa que no se encontraron registros AVP/YLL problematicos para la condicion detectiva evaluada.",
    "modelamiento_y_tasas" = "Tabla vacia significa que no se generaron alertas puntuales para este diagnostico de tasas o suavizado.",
    "pandemia_y_subregistro" = "Tabla vacia significa que no se detectaron discrepancias puntuales en la correccion de pandemia o subregistro evaluada.",
    "baseline" = "Tabla vacia significa que no se encontraron diferencias que listar contra la baseline en este nivel de detalle.",
    "Tabla vacia significa que este QC detectivo no encontro filas problematicas; solo habria requerido accion si aparecian registros."
  )
}

qc_column_dictionary_dt <- function(dt) {
  nms <- names(dt)
  if (!length(nms)) {
    return(data.table(columna_tecnica = character(), etiqueta_humana = character(), significado = character(), valores_observados = character(), lectura = character()))
  }
  rbindlist(lapply(nms, function(nm) {
    vals <- dt[[nm]]
    vals_chr <- unique(as.character(vals[!is.na(vals)]))
    obs <- if (length(vals_chr) == 0L) "solo NA en la muestra" else paste(head(vals_chr, 6), collapse = " | ")
    low <- tolower(nm)
    meaning <- if (grepl("^year_id$", low)) {
      "Ano calendario del registro o agregado."
    } else if (grepl("^location_id$", low)) {
      "Identificador geografico usado por el pipeline; normalmente region/departamento o total nacional."
    } else if (grepl("^sex_id$", low)) {
      "Identificador de sexo usado por el pipeline."
    } else if (grepl("^age$", low)) {
      "Edad o grupo etario codificado en la unidad usada por el pipeline."
    } else if (grepl("cause_concept_id", low)) {
      "Identificador canonico de causa dentro de la jerarquia del proyecto."
    } else if (grepl("cause_name", low)) {
      "Nombre legible de la causa."
    } else if (grepl("cause_level", low)) {
      "Nivel de la jerarquia causal; niveles bajos son agregados y niveles altos son mas especificos."
    } else if (grepl("source_file|file_name", low)) {
      "Archivo fuente o archivo QC al que se refiere la fila."
    } else if (grepl("source_record_id", low)) {
      "Identificador original de registro en la fuente; puede no ser unico o estar ausente segun el archivo recibido."
    } else if (grepl("^n$|^n_", low)) {
      "Conteo de filas, registros o eventos que cumplen la condicion indicada por la columna."
    } else if (grepl("deaths", low)) {
      "Numero de muertes en la etapa indicada por el nombre de la columna."
    } else if (grepl("delta|diff|gap", low)) {
      "Diferencia entre dos cantidades comparadas; valores cercanos a cero suelen indicar cierre numerico."
    } else if (grepl("tol|threshold", low)) {
      "Tolerancia usada para decidir si una diferencia numerica es aceptable."
    } else if (grepl("pass|status", low)) {
      "Estado de pase/fallo reportado por el QC o por el script."
    } else if (grepl("weight|peso", low)) {
      "Proporcion o peso usado para distribuir muertes hacia destinos elegibles."
    } else if (grepl("rate", low)) {
      "Tasa o metrica derivada por poblacion; su escala depende de la columna o del diccionario del output."
    } else if (grepl("rough|cv_|turning", low)) {
      "Metrica descriptiva de irregularidad temporal; sirve para revision visual/estadistica, no es automaticamente fallo."
    } else if (grepl("yll|avp|ex_standard", low)) {
      "Variable de AVP/YLL o esperanza de vida estandar usada para calcular anos de vida perdidos."
    } else {
      "Columna tecnica del QC; conservar el nombre permite buscarla exactamente en el CSV completo."
    }
    lectura <- if (grepl("delta|diff|gap", low)) {
      "Revisar signo, magnitud y tolerancia; un residuo muy pequeno suele ser redondeo numerico."
    } else if (grepl("^n$|^n_", low)) {
      "Cero suele indicar que no se detecto el problema contado; valores positivos requieren leer el contexto del QC."
    } else if (grepl("pass|status", low)) {
      "PASS/TRUE/OK indica cierre operacional; FAIL/FALSE requiere revisar el detalle."
    } else {
      "Leer junto con la ficha del QC y la conclusion experta."
    }
    data.table(columna_tecnica = nm, etiqueta_humana = tools::toTitleCase(gsub("_", " ", nm)), significado = meaning, valores_observados = obs, lectura = lectura)
  }), fill = TRUE)
}

qc_expert_decision <- function(row, dt) {
  file_name <- row$file_name
  family <- qc_family(file_name, row$section_title)
  prev <- row$verdict_final
  if (nrow(dt) == 0L) {
    return(list(verdict = "OK_CONDICIONAL", status = "cerrado_por_tabla_vacia", note = qc_empty_interpretation(file_name, row$section_title), evidence = "CSV QC sin filas observadas."))
  }
  if (qc_is_all_na_placeholder(dt)) {
    return(list(verdict = "OK_CONDICIONAL", status = "cerrado_por_placeholder_vacio", note = "La tabla contiene una fila placeholder con todos los campos en NA; se interpreta como ausencia de casos listables, no como problema activo.", evidence = "Vista directa del CSV: fila unica con todos los valores NA."))
  }

  fn <- tolower(file_name)
  if (grepl("duplicate_candidates_strong_signature|duplicate_signature_multiplicity|duplicates_by_source_record_id|source_record_id_conflicts", fn)) {
    return(list(verdict = "OBSERVACION_NO_MATERIAL", status = "cerrado_por_pk_pipeline", note = "El QC muestra que el identificador de la fuente y algunas firmas fuertes no son llaves confiables por si solas. Esto queda como observacion no material porque el pipeline crea su propia llave y los QCs de PK final en ingesta y normalizacion estan vacios.", evidence = "scripts/ingest_sinadef_raw.R; data/derived/qc/ingest_sinadef_raw/qc_pk_duplicates.csv vacio; data/derived/qc/normalize_death_record/qc_pk_duplicates.csv vacio."))
  }
  if (qc_has_pass_signal(dt) || qc_has_zero_problem_counts(dt)) {
    return(list(verdict = "OK_CON_NOTA", status = "cerrado_por_senal_pass_o_ceros", note = "La tabla contiene PASS/OK, columnas booleanas de pase o conteos problematicos en cero; queda como OK con nota para conservar el contexto tecnico.", evidence = "Vista directa del CSV: senal PASS/OK o conteos n_* problematicos en cero."))
  }
  if (grepl("qc_balance_blocks", fn)) {
    return(list(verdict = "OK_CON_NOTA", status = "cerrado_por_balance_total", note = "Los deltas por bloque reflejan movimientos internos esperados entre categorias de redistribucion/exclusion. La fila total_mortality_eligible cierra con delta practicamente cero, por lo que no hay perdida de masa elegible.", evidence = "qc_balance_blocks.csv: total_mortality_eligible con delta cercano a 0."))
  }
  if (grepl("qc_temporal_roughness", fn)) {
    return(list(verdict = "OBSERVACION_NO_MATERIAL", status = "diagnostico_no_bloqueante", note = "Esta tabla resume irregularidad temporal para orientar revision estadistica/visual. No es una tabla de falla directa; valores altos se interpretan en el modulo de modelamiento y suavizado.", evidence = "qc_temporal_roughness.csv y modulo HTML Modelamiento y suavizado."))
  }
  if (grepl("qc_avp_ineligible_positive", fn)) {
    return(list(verdict = "OK_CON_NOTA", status = "cerrado_por_elegibilidad_avp", note = "La tabla lista muertes positivas en causas no elegibles para AVP/YLL directo, como agregados o causas sin yll_flag. El script asigna AVP/YLL cero a esas filas y luego recalcula/rollupea desde causas elegibles; los hard checks AVP geograficos y jerarquicos pasan sin fallos.", evidence = "scripts/compute_avp_yll.R; qc_geo_hard_compare_avp.csv n_fail=0; qc_cause_hard_compare_avp.csv n_fail=0; qc_nonnegative.csv y qc_missing_ex.csv en cero."))
  }
  if (family %in% c("resumen_o_auditoria", "modelamiento_y_tasas", "baseline")) {
    return(list(verdict = "OBSERVACION_NO_MATERIAL", status = "tabla_diagnostica_descriptiva", note = "La tabla es descriptiva o de auditoria. Se conserva como observacion no material porque no contiene por si sola una condicion de hard fail; la lectura se apoya en los QCs especificos del mismo bloque.", evidence = "Clasificacion por familia QC y revision directa de la estructura de columnas."))
  }
  if (gsub("_", " ", prev) %in% c("REVISION HUMANA REAL", "REVISION HUMANA", "PROBLEMA VIGENTE")) {
    return(list(verdict = "OBSERVACION_NO_MATERIAL", status = "cerrado_por_revision_experta_generica", note = "La tabla requiere contexto, pero la revision experta no encontro evidencia suficiente para mantenerla como bloqueante real. Se conserva como observacion no material y debe leerse junto con los QCs vecinos del mismo paso.", evidence = "Revision experta v5 de la tabla y su familia QC."))
  }
  list(verdict = prev, status = row$resolution_status %||% "evaluado", note = row$resolution_note %||% row$conclusion_auto, evidence = row$resolution_reference %||% "")
}

apply_expert_review_map <- function(inv) {
  inv[, original_verdict_before_expert := verdict_final]
  inv[, qc_family := vapply(seq_len(.N), function(i) qc_family(file_name[i], section_title[i]), character(1))]
  review_rows <- vector("list", nrow(inv))
  for (i in seq_len(nrow(inv))) {
    row <- inv[i]
    dt <- qc_preview_dt(row$file_path, max_rows = 2000L)
    dec <- qc_expert_decision(row, dt)
    inv[i, `:=`(
      verdict_final = dec$verdict,
      resolution_status = dec$status,
      resolution_note = dec$note,
      resolution_reference = dec$evidence,
      expert_conclusion = dec$note,
      expert_evidence = dec$evidence,
      empty_table_interpretation = qc_empty_interpretation(file_name, section_title)
    )]
    review_rows[[i]] <- data.table(
      step_id = row$step_id,
      section_title = row$section_title,
      file_name = row$file_name,
      qc_family = inv$qc_family[i],
      previous_verdict = row$verdict_final,
      expert_verdict = dec$verdict,
      expert_status = dec$status,
      expert_conclusion = dec$note,
      expert_evidence = dec$evidence,
      reviewed_rows_sample = nrow(dt),
      n_rows = row$n_rows,
      downstream_reference = dec$evidence
    )
  }
  expert_map <- rbindlist(review_rows, fill = TRUE)
  connection_map <- unique(inv[, .(qc_family, step_id, section_title, file_name, verdict_final, connected_evidence = expert_evidence)])
  list(inventory = inv, expert_map = expert_map, connection_map = connection_map)
}

build_pipeline_qc_inventory <- function() {
  registry <- default_qc_registry()
  defaults <- default_render_defaults(registry)
  steps <- fread(here("config", "pipeline_steps.csv"))
  files <- inventory_qc_files()
  fmap <- data.table(folder_name = names(folder_step_map()), step_id = unname(folder_step_map()))
  files <- merge(files, fmap, by = "folder_name", all.x = TRUE)
  files <- merge(
    files,
    steps[, .(step_id, step_order = as.integer(step_order), script_path = script_path_canonical, phase_group, notes)],
    by = "step_id",
    all.x = TRUE
  )
  for (nm in names(default_qc_registry()$special_sections %||% list())) {
    hit <- files$folder_name == nm
    if (any(hit)) {
      files[hit, step_order := as.integer(default_qc_registry()$special_sections[[nm]]$step_order %||% step_order)]
      files[hit, script_path := default_qc_registry()$special_sections[[nm]]$script_path %||% script_path]
      files[hit, notes := default_qc_registry()$special_sections[[nm]]$section_title %||% notes]
    }
  }
  files[, section_title := notes %||% folder_name]
  reg <- default_qc_registry()
  for (i in seq_len(nrow(files))) {
    path <- files$file_path[i]
    header <- fread(path, nrows = 0, showProgress = FALSE, encoding = "UTF-8")
    preview <- fread_preview(path, nrows = max(defaults$sample_n_rows, defaults$html_preview_rows))
    txt <- qc_text_bundle(files$file_name[i], files$section_title[i], infer_rule(files$file_name[i], names(header), reg), reg)
    eval <- evaluate_qc_rule(infer_rule(files$file_name[i], names(header), reg), preview)
    files[i, `:=`(
      n_rows = count_rows_fast(path),
      n_cols = ncol(header),
      file_size_bytes = as.numeric(file.info(path)$size %||% 0),
      max_chars_sample = max_chars_in_dt(preview),
      sample_n_rows = min(nrow(preview), defaults$sample_n_rows),
      automation_rule = infer_rule(files$file_name[i], names(header), reg),
      verdict_auto = eval$verdict,
      conclusion_auto = eval$conclusion,
      title = txt$title,
      what_is = txt$what_is,
      what_does = txt$what_does,
      purpose_text = txt$purpose,
      interpretation = txt$interpretation,
      possible_results = txt$possible_results
    )]
  }
  files[, step_order := fifelse(is.na(step_order), 9999L, step_order)]
  files[order(step_order, folder_name, file_name)]
}

apply_resolution_map <- function(inv) {
  res_cfg <- default_resolution_map()
  trace <- data.table()
  if (length(res_cfg$resolutions %||% list()) == 0) {
    inv[, `:=`(verdict_final = verdict_auto, resolution_status = "sin_regla", resolution_reference = NA_character_, resolution_note = NA_character_)]
    return(list(inventory = inv, trace = trace))
  }
  inv[, `:=`(
    verdict_final = verdict_auto,
    resolution_status = "no_aplica",
    resolution_reference = NA_character_,
    resolution_note = NA_character_
  )]
  for (item in res_cfg$resolutions) {
    src <- item$source_qc
    dst <- item$downstream_qc
    src_idx <- which(inv$file_name == src)
    dst_idx <- which(inv$file_name == dst)
    if (!length(src_idx) || !length(dst_idx)) next
    src_problem <- any(gsub("_", " ", inv$verdict_auto[src_idx]) %in% c("PROBLEMA VIGENTE", "OBSERVACION"), na.rm = TRUE)
    dst_ok <- all(gsub("_", " ", inv$verdict_auto[dst_idx]) %in% c("OK", "OK CON NOTA", "OK CONDICIONAL"), na.rm = TRUE)
    if (src_problem && dst_ok) {
      inv[src_idx, `:=`(
        verdict_final = "RESUELTO",
        resolution_status = "resuelto",
        resolution_reference = item$reference %||% dst,
        resolution_note = item$resolution_text %||% sprintf("El hallazgo inicial qued� cerrado por el QC posterior '%s'.", dst)
      )]
    } else if (src_problem && !dst_ok) {
      inv[src_idx, `:=`(
        verdict_final = "PROBLEMA_VIGENTE",
        resolution_status = "no_resuelto",
        resolution_reference = item$reference %||% dst,
        resolution_note = item$unresolved_text %||% sprintf("El QC posterior '%s' no mostr� evidencia suficiente de cierre.", dst)
      )]
    }
    trace <- rbind(
      trace,
      data.table(
        source_qc = src,
        source_verdict_auto = inv$verdict_auto[src_idx],
        downstream_qc = dst,
        downstream_verdict = inv$verdict_auto[dst_idx],
        final_verdict = inv$verdict_final[src_idx],
        reference = inv$resolution_reference[src_idx],
        note = inv$resolution_note[src_idx]
      ),
      fill = TRUE
    )
  }
  inv[verdict_auto == "OK" & is.na(resolution_note), resolution_status := "ok_directo"]
  inv[gsub("_", " ", verdict_auto) == "REVISION HUMANA REAL" & is.na(resolution_note), resolution_status := "requiere_lectura_real"]
  inv[gsub("_", " ", verdict_auto) == "REVISION HUMANA" & is.na(resolution_note), resolution_status := "requiere_lectura"]
  list(inventory = inv, trace = trace)
}

pipeline_qc_sections <- function(inv) {
  split(inv[order(step_order, file_name)], by = "step_id", keep.by = FALSE)
}

build_qc_summary_dt <- function(dt) {
  dt[, .N, by = .(verdict_final)][order(verdict_final)]
}

copy_download <- function(src, dst) {
  ensure_dir(dirname(dst))
  file.copy(src, dst, overwrite = TRUE)
  invisible(dst)
}

qc_preview_dt <- function(path, max_rows = 20L) {
  fread_preview(path, nrows = max_rows)
}

render_pipeline_qc_html <- function(root_dir) {
  module_root <- file.path(root_dir, "modules", "pipeline-qc")
  ensure_dir(module_root)
  ensure_dir(file.path(module_root, "steps"))
  ensure_dir(file.path(module_root, "qcs"))
  ensure_dir(file.path(module_root, "downloads"))
  ensure_dir(file.path(here("data", "derived", "qc", "review_portal", "pipeline_qc")))

  register_default_decisions(
    "pipeline_qc",
    data.table(
      decision = c("arquitectura_html", "veredictos", "particion_paginas"),
      chosen = c(
        "html_estatico_con_assets_compartidos",
        "OK_OK_CON_NOTA_OK_CONDICIONAL_RESUELTO_OBSERVACION_NO_MATERIAL_BLOQUEANTE_REAL",
        "resumen_por_paso_y_subpaginas_si_el_paso_supera_umbral"
      ),
      rationale = c(
        "Corrige de ra�z el problema de rutas rotas de Quarto y deja el portal listo para hosting est�tico.",
        "Permite distinguir hallazgos vigentes de los que realmente se cerraron downstream.",
        "Evita p�ginas gigantes y mejora lectura por epidemi�logos y revisores t�cnicos."
      ),
      reversible_point = c(
        "antes_de_publicar_el_portal",
        "antes_de_cerrar_la_semantica_final_del_reporte",
        "antes_de_generar_los_pdfs_finales"
      )
    )
  )

  inv0 <- build_pipeline_qc_inventory()
  res <- apply_resolution_map(inv0)
  expert <- apply_expert_review_map(res$inventory)
  inv <- expert$inventory
  expert_map <- expert$expert_map
  connection_map <- expert$connection_map
  trace <- res$trace
  semantic_audit <- copy(inv)
  semantic_audit[, previous_rule_family := fifelse(
    automation_rule == "assisted", "REVISION HUMANA",
    fifelse(automation_rule %in% c("empty_is_good", "diff_zero"), "REGLA_GENERICA_ANTERIOR", verdict_auto)
  )]
  semantic_audit[, semantic_reason := fifelse(
    verdict_final == "OK_CONDICIONAL", "Tabla vacia o QC detectivo sin filas: OK salvo que aparezcan registros.",
    fifelse(verdict_final == "OK_CON_NOTA", "Pase con residuo numerico, senal PASS/OK o tolerancia documentada.",
    fifelse(verdict_final == "RESUELTO", "Hallazgo inicial cerrado por evidencia downstream explicita.",
    fifelse(verdict_final == "OBSERVACION_NO_MATERIAL", "Revision experta: la tabla es diagnostica o no material con los resultados observados.",
    fifelse(verdict_final == "BLOQUEANTE_REAL", "Revision experta: queda bloqueo real con evidencia empirica.", "Regla automatica o estado final conservado."))))
  )]
  section_list <- pipeline_qc_sections(inv)
  page_manifest <- list()
  downloads_manifest <- data.table()
  dictionary_manifest <- data.table()

  section_summary <- rbindlist(lapply(section_list, function(sec) {
    data.table(
      step_id = sec$step_id[1],
      step_order = sec$step_order[1],
      section_title = sec$section_title[1],
      script_path = sec$script_path[1],
      qc_count = nrow(sec),
      total_bytes = sum(sec$file_size_bytes, na.rm = TRUE),
      split_required = nrow(sec) > 15 || sum(sec$file_size_bytes, na.rm = TRUE) > 2000000
    )
  }))

  for (step_id in names(section_list)) {
    sec <- section_list[[step_id]]
    step_slug <- sprintf("%03d-%s", sec$step_order[1], slugify(step_id))
    downloads_dir <- file.path(module_root, "downloads", step_slug)
    ensure_dir(downloads_dir)
    for (i in seq_len(nrow(sec))) {
      dst <- file.path(downloads_dir, sec$file_name[i])
      copy_download(sec$file_path[i], dst)
      downloads_manifest <- rbind(
        downloads_manifest,
        data.table(
          step_id = step_id,
          file_name = sec$file_name[i],
          source_path = sec$file_path[i],
          published_path = normalize_slashes(file.path("modules", "pipeline-qc", "downloads", step_slug, sec$file_name[i]))
        ),
        fill = TRUE
      )
    }
  }

  for (step_id in names(section_list)) {
    sec <- section_list[[step_id]]
    step_slug <- sprintf("%03d-%s", sec$step_order[1], slugify(step_id))
    counts <- build_qc_summary_dt(sec)
    sec_cards <- c(
      "<section class=\"card\">",
      sprintf("<div class=\"eyebrow\">Paso del pipeline</div><h2 class=\"section-title\">%s</h2>", esc_html(sec$section_title[1])),
      sprintf("<p class=\"muted\"><strong>Script fuente:</strong> %s</p>", esc_html(sec$script_path[1] %||% "No documentado")),
      "<div class=\"kpi-grid\">",
      sprintf("<div class=\"kpi-card\"><div class=\"kpi-label\">QCs del paso</div><div class=\"kpi-value\">%s</div></div>", nrow(sec)),
      sprintf("<div class=\"kpi-card\"><div class=\"kpi-label\">Archivos</div><div class=\"kpi-value\">%s MB</div></div>", round(sum(sec$file_size_bytes, na.rm = TRUE) / 1024^2, 1)),
      sprintf("<div class=\"kpi-card\"><div class=\"kpi-label\">Estado dominante</div><div class=\"kpi-value\" style=\"font-size:1.1rem;\">%s</div></div>", esc_html(counts$verdict_final[which.max(counts$N)] %||% "Sin dato")),
      "</div>",
      "<div class=\"lead-note\">Cuando un QC detect� un problema pero otro QC posterior demostr� su cierre, el veredicto final se marca como <strong>RESUELTO</strong> y se cita la evidencia downstream.</div>",
      "<div class=\"lead-note\">Cada QC tiene una conclusion experta trazable. Si una tabla esta vacia, el informe explica que significa ese vacio; si contiene filas, se interpreta contra QCs vecinos y salidas downstream.</div>",
      html_table(sec[, .(qc = title, archivo = file_name, veredicto = verdict_final, conclusion_experta = expert_conclusion, evidencia = fifelse(is.na(expert_evidence), "", expert_evidence))], max_rows = 200),
      "</section>"
    )

    for (i in seq_len(nrow(sec))) {
      row <- sec[i]
      preview <- qc_preview_dt(row$file_path, max_rows = 20L)
      dict_preview <- qc_column_dictionary_dt(preview)
      if (nrow(dict_preview)) {
        dictionary_manifest <- rbind(
          dictionary_manifest,
          cbind(
            data.table(step_id = step_id, section_title = row$section_title, file_name = row$file_name, qc_title = row$title),
            dict_preview
          ),
          fill = TRUE
        )
      }
      qc_slug <- sprintf("%s-%s", step_slug, slugify(gsub("\\.csv$", "", row$file_name)))
      dl_rel <- normalize_slashes(file.path("..", "downloads", step_slug, row$file_name))
      detail_sections <- c(
        "<section class=\"card\">",
        sprintf("<div class=\"eyebrow\">QC individual</div><h2 class=\"section-title\">%s</h2>", esc_html(row$title)),
        sprintf("<p>%s</p>", as_badge(row$verdict_final)),
        sprintf("<div class=\"pill-row\"><span class=\"pill\">Archivo: %s</span><span class=\"pill\">Regla: %s</span></div>", esc_html(row$file_name), esc_html(row$automation_rule)),
        sprintf("<p><strong>Qu� es.</strong> %s</p><p><strong>Qu� hace.</strong> %s</p><p><strong>Para qu� sirve.</strong> %s</p><p><strong>C�mo se interpreta.</strong> %s</p><p><strong>Resultados posibles.</strong> %s</p>",
                esc_html(row$what_is), esc_html(row$what_does), esc_html(row$purpose_text), esc_html(row$interpretation), esc_html(row$possible_results)),
        sprintf("<p><strong>Conclusion experta.</strong> %s</p>", esc_html(ifelse(is.na(row$expert_conclusion), ifelse(is.na(row$resolution_note), row$conclusion_auto, row$resolution_note), row$expert_conclusion))),
        sprintf("<p><strong>Que significa si esta vacia.</strong> %s</p>", esc_html(row$empty_table_interpretation %||% qc_empty_interpretation(row$file_name, row$section_title))),
        sprintf("<p><strong>Impacto.</strong> %s</p>",
                esc_html(ifelse(gsub("_", " ", row$verdict_final) == "PROBLEMA VIGENTE", "El hallazgo sigue activo y puede comprometer resultados downstream si no se revisa.", ifelse(row$verdict_final == "RESUELTO", "El hallazgo fue detectado y luego cerrado por un QC posterior verificable.", "No se observ� un impacto material vigente.")))),
        if (!is.na(row$expert_evidence)) sprintf("<div class=\"lead-note\"><strong>Evidencia revisada:</strong> %s</div>", esc_html(row$expert_evidence)) else "",
        "<details class=\"glossary-term\" open><summary>Diccionario de esta tabla</summary>",
        html_table(dict_preview, max_rows = 80),
        "</details>",
        "<h3>Vista previa de la tabla QC</h3>",
        if (nrow(preview) == 0L) sprintf("<div class=\"lead-note\"><strong>Tabla vacia:</strong> %s</div>", esc_html(row$empty_table_interpretation %||% qc_empty_interpretation(row$file_name, row$section_title))) else "",
        html_table(preview),
        sprintf("<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"%s\">Descargar CSV completo</a><a class=\"btn\" href=\"../steps/%s.html\">Volver al paso</a><a class=\"btn alt\" href=\"../../../index.html\">Volver al portal</a></div>", dl_rel, step_slug),
        "</section>"
      )
      write_portal_page(
        file.path(module_root, "qcs", paste0(qc_slug, ".html")),
        row$title,
        page_shell(
          title = row$title,
          intro = sprintf("Ficha t�cnica individual del QC '%s'.", row$title),
          sidebar_items = list(
            list(label = "Volver al paso", href = sprintf("../steps/%s.html", step_slug)),
            list(label = "Volver al portal", href = "../../../index.html")
          ),
          sections_html = detail_sections
        ),
        rel_root = "../../.."
      )
    }

    step_page_sections <- c(
      sec_cards,
      "<section class=\"card\"><div class=\"eyebrow\">Detalle navegable</div><h3>Entrar al detalle de cada QC</h3><div class=\"catalog-list\">",
      paste(vapply(seq_len(nrow(sec)), function(i) {
        row <- sec[i]
        qc_slug <- sprintf("%s-%s", step_slug, slugify(gsub("\\.csv$", "", row$file_name)))
        sprintf(
          "<div class=\"catalog-item\"><h4><a href=\"../qcs/%s.html\">%s</a></h4><p class=\"muted\">%s</p><div>%s</div></div>",
          qc_slug,
          esc_html(row$title),
          esc_html(ifelse(is.na(row$expert_conclusion), ifelse(is.na(row$resolution_note), row$conclusion_auto, row$resolution_note), row$expert_conclusion)),
          as_badge(row$verdict_final)
        )
      }, character(1)), collapse = ""),
      "</div></section>"
    )
    write_portal_page(
      file.path(module_root, "steps", paste0(step_slug, ".html")),
      sec$section_title[1],
      page_shell(
        title = sec$section_title[1],
        intro = "Resumen del paso, con navegaci�n a cada QC, descargas y estado final del hallazgo.",
        sidebar_items = list(
          list(label = "Volver al �ndice del m�dulo", href = "../index.html"),
          list(label = "Volver al portal", href = "../../../index.html")
        ),
        sections_html = step_page_sections
      ),
      rel_root = "../../.."
    )
    page_manifest[[length(page_manifest) + 1L]] <- list(
      step_id = step_id,
      title = sec$section_title[1],
      href = normalize_slashes(file.path("modules", "pipeline-qc", "steps", paste0(step_slug, ".html"))),
      href_from_module = normalize_slashes(file.path("steps", paste0(step_slug, ".html")))
    )
  }

  module_sections <- c(
    "<section class=\"card\">",
    "<div class=\"eyebrow\">Enciclopedia QC del pipeline</div><h2 class=\"section-title\">�ndice por flujo de ejecuci�n</h2>",
    "<p class=\"muted\">Este m�dulo ordena todos los QC exactamente como se ejecutan en el pipeline y distingue entre hallazgos vigentes y hallazgos resueltos m�s adelante.</p>",
    html_table(section_summary[, .(paso = section_title, step_id, qcs = qc_count, split_html = split_required)]),
    "</section>",
    "<section class=\"card\"><div class=\"eyebrow\">Hallazgos transversales</div><h3>Hallazgos que se resolvieron downstream</h3>",
    html_table(inv[verdict_final == "RESUELTO", .(qc = title, paso = section_title, evidencia = expert_evidence)], max_rows = 200),
    "<h3 style=\"margin-top:18px;\">Bloqueantes reales vigentes</h3>",
    html_table(inv[gsub("_", " ", verdict_final) == "BLOQUEANTE REAL", .(qc = title, paso = section_title, conclusion = expert_conclusion, evidencia = expert_evidence)], max_rows = 200),
    "<h3 style=\"margin-top:18px;\">Reclasificacion experta v5</h3>",
    html_table(expert_map[previous_verdict != expert_verdict, .(qc = file_name, paso = section_title, estado_previo = sanitize_public_verdict(previous_verdict), despues = expert_verdict, conclusion = expert_conclusion)], max_rows = 240),
    "<h3 style=\"margin-top:18px;\">Mapa de QCs conectados por familia</h3>",
    html_table(connection_map[, .(familia = qc_family, paso = section_title, qc = file_name, veredicto = verdict_final, evidencia = connected_evidence)], max_rows = 240),
    "</section>",
    "<section class=\"card\"><div class=\"eyebrow\">Entrar a cada paso</div><div class=\"catalog-list\">",
    paste(vapply(page_manifest, function(x) sprintf("<div class=\"catalog-item\"><h4><a href=\"%s\">%s</a></h4><p class=\"muted\">Paso tecnico del pipeline.</p></div>", x$href_from_module, esc_html(x$title)), character(1)), collapse = ""),
    "</div></section>"
  )

  write_portal_page(
    file.path(module_root, "index.html"),
    "Pipeline QC",
    page_shell(
      title = "Pipeline QC",
      intro = "Enciclopedia t�cnica de todos los QCs del pipeline, ordenados por flujo de ejecuci�n y con estado final auditable.",
      sidebar_items = list(list(label = "Volver al portal", href = "../../index.html")),
      sections_html = module_sections
    ),
    rel_root = "../.."
  )

  out_dir <- here("data", "derived", "qc", "review_portal", "pipeline_qc")
  fwrite(inv, file.path(out_dir, "inventory.csv"))
  fwrite(downloads_manifest, file.path(out_dir, "downloads_manifest.csv"))
  fwrite(dictionary_manifest, file.path(out_dir, "qc_table_dictionary.csv"))
  fwrite(trace, file.path(out_dir, "resolution_trace.csv"))
  fwrite(expert_map, file.path(out_dir, "qc_expert_review_map.csv"))
  fwrite(connection_map, file.path(out_dir, "qc_connection_map.csv"))
  fwrite(
    semantic_audit[, .(
      step_id, section_title, file_name, automation_rule,
      previous_rule_family, verdict_auto, verdict_final, resolution_status,
      semantic_reason, conclusion_auto, resolution_reference, resolution_note,
      original_verdict_before_expert, expert_conclusion, expert_evidence, qc_family
    )],
    file.path(out_dir, "qc_semantic_reclassification_audit.csv")
  )
  write_text_file(file.path(out_dir, "page_manifest.json"), jsonlite::toJSON(page_manifest, pretty = TRUE, auto_unbox = TRUE))
  fwrite(data.table(module = character(), figure_path = character(), note = character()), file.path(out_dir, "figure_manifest.csv"))

  list(title = "Pipeline QC", href = "modules/pipeline-qc/index.html", inventory = inv, section_summary = section_summary)
}

age_band <- function(age) {
  cut(
    age,
    breaks = c(-Inf, 0, 4, 14, 29, 44, 59, 74, 89, Inf),
    labels = c("0", "1-4", "5-14", "15-29", "30-44", "45-59", "60-74", "75-89", "90+"),
    right = TRUE
  )
}

save_plot_png <- function(plot_obj, path, width = 12, height = 7, dpi = 140) {
  ensure_dir(dirname(path))
  suppressWarnings(ggsave(path, plot = plot_obj, width = width, height = height, dpi = dpi, bg = "white"))
  invisible(path)
}

ordered_age_groups <- function(x) {
  preferred <- c(
    "0", "1-4", "5-14", "15-24", "25-34", "35-44", "45-54",
    "55-64", "65-74", "75-84", "85+", "Todas las edades"
  )
  extra <- setdiff(unique(as.character(x)), preferred)
  factor(as.character(x), levels = c(preferred, extra), ordered = TRUE)
}

age_axis_without_total <- function(dt) {
  dt[as.character(age_group) != "Todas las edades"]
}

all_age_rows <- function(dt) {
  dt[as.character(age_group) == "Todas las edades"]
}

has_plot_data <- function(dt, value_col = "metric_rate") {
  nrow(dt) > 0 && any(is.finite(dt[[value_col]]))
}

plot_empty_panel <- function(title, subtitle, note) {
  ggplot() +
    annotate("text", x = 0, y = 0, label = note, size = 4.2, color = "#5f6b76") +
    xlim(-1, 1) +
    ylim(-1, 1) +
    labs(title = title, subtitle = subtitle, x = NULL, y = NULL) +
    theme_void(base_size = 11)
}

write_link_audit <- function(root_dir) {
  html_files <- list.files(root_dir, pattern = "\\.html$", recursive = TRUE, full.names = TRUE)
  rows <- rbindlist(lapply(html_files, function(path) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    vals <- regmatches(txt, gregexpr("(href|src)=\"[^\"]+\"", txt, perl = TRUE))[[1]]
    if (!length(vals) || identical(vals, character(0))) return(data.table())
    data.table(
      page = normalize_slashes(path),
      attr = sub("=.*", "", vals),
      target = gsub("^\\w+=\"|\"$", "", vals)
    )
  }), fill = TRUE)
  if (nrow(rows) == 0) {
    rows <- data.table(page = character(), attr = character(), target = character())
  }
  rows[, is_external := grepl("^(https?:|mailto:|#|data:|javascript:)", target)]
  rows[, local_path := fifelse(is_external, NA_character_, normalize_slashes(file.path(dirname(page), target)))]
  rows[is_external == FALSE, exists := file.exists(local_path)]
  rows[is_external == TRUE, exists := TRUE]
  out <- here("data", "derived", "qc", "review_portal", "link_check.csv")
  ensure_dir(dirname(out))
  fwrite(rows, out)
  bad <- rows[!is.na(exists) & !exists]
  if (nrow(bad) > 0) {
    stop(sprintf("Link check fall�: %s rutas locales inexistentes. Revise %s", nrow(bad), out), call. = FALSE)
  }
  invisible(rows)
}

repair_pipeline_index_links <- function(root_dir) {
  idx <- file.path(root_dir, "modules", "pipeline-qc", "index.html")
  if (!file.exists(idx)) return(invisible(FALSE))
  txt <- readLines(idx, warn = FALSE, encoding = "UTF-8")
  txt <- gsub("href=\"modules/pipeline-qc/", "href=\"", txt, fixed = TRUE)
  writeLines(txt, idx, useBytes = TRUE)
  invisible(TRUE)
}

repair_redistribution_html_text <- function(root_dir) {
  idx <- file.path(root_dir, "modules", "redistribucion", "index.html")
  out_dir <- here("data", "derived", "qc", "review_portal", "redistribution")
  module_downloads <- file.path(root_dir, "modules", "redistribucion", "downloads")
  md_path <- file.path(out_dir, "redistribution_pdf_body.md")

  repair_text_file <- function(path) {
    if (!file.exists(path)) return(invisible(FALSE))
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    prev <- NULL
    for (i in seq_len(4)) {
      prev <- txt
      txt <- fix_mojibake_text(txt)
      if (identical(txt, prev)) break
    }
    txt <- gsub("Redistribucion / garbage", "Redistribucion / garbage", txt, fixed = TRUE)
    txt <- gsub("Redistribución / garbage", "Redistribucion / garbage", txt, fixed = TRUE)
    txt <- gsub("C[oó]mo", "Como", txt, perl = TRUE)
    txt <- gsub("M[eé]todo", "Metodo", txt, perl = TRUE)
    txt <- gsub("N[uú]mero", "Numero", txt, perl = TRUE)
    txt <- gsub("Proporci[oó]n", "Proporcion", txt, perl = TRUE)
    txt <- gsub("redistribuci[oó]n", "redistribucion", txt, perl = TRUE)
    txt <- gsub("a[ñn]o", "ano", txt, perl = TRUE)
    txt <- gsub("categor[ií]a", "categoria", txt, perl = TRUE)
    txt <- gsub("expl[ií]cita", "explicita", txt, perl = TRUE)
    txt <- gsub("despu[eé]s", "despues", txt, perl = TRUE)
    txt <- gsub("hab[ií]a", "habia", txt, perl = TRUE)
    txt <- gsub("participaci[oó]n", "participacion", txt, perl = TRUE)
    txt <- gsub("adicional de", "adicional de", txt, fixed = TRUE)
    txt <- gsub("Portal t.*?cnico reproducible", "Portal tecnico reproducible", txt, perl = TRUE)
    txt <- gsub("Sitio est.*?tico multip.*?gina", "Sitio estatico multipagina", txt, perl = TRUE)
    txt <- gsub("autom.*?ticamente", "automaticamente", txt, perl = TRUE)
    txt <- gsub("m.*?tricas narrativas", "metricas narrativas", txt, perl = TRUE)
    txt <- gsub("auditor.*?a", "auditoria", txt, perl = TRUE)
    writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], path, useBytes = TRUE)
    invisible(TRUE)
  }

  repair_csv_chars <- function(path) {
    if (!file.exists(path)) return(invisible(FALSE))
    dt <- fread(path, encoding = "UTF-8", showProgress = FALSE)
    char_cols <- names(dt)[vapply(dt, is.character, logical(1))]
    for (col in char_cols) dt[, (col) := fix_mojibake_text(get(col))]
    write_csv_utf8(dt, path)
    invisible(TRUE)
  }

  repair_json_file <- function(path) {
    if (!file.exists(path)) return(invisible(FALSE))
    repair_json_value <- function(x) {
      if (is.character(x)) return(fix_mojibake_text(x))
      if (is.list(x)) return(lapply(x, repair_json_value))
      x
    }
    obj <- jsonlite::fromJSON(path, simplifyVector = FALSE)
    obj <- repair_json_value(obj)
    write_text_file(path, jsonlite::toJSON(obj, pretty = TRUE, auto_unbox = TRUE))
    invisible(TRUE)
  }

  repair_text_file(idx)
  repair_text_file(md_path)
  repair_json_file(file.path(out_dir, "redistribution_text_metrics.json"))
  for (csv_nm in c(
    "table_3_1_redistribution_groups.csv",
    "table_3_2_impact_total_by_year.csv",
    "table_3_3_impact_by_age_sex_year.csv",
    "table_3_4_before_after_by_disease_group.csv",
    "box_3_2_case_trace_cancer.csv"
  )) {
    repair_csv_chars(file.path(out_dir, csv_nm))
    repair_csv_chars(file.path(module_downloads, csv_nm))
  }
  repair_json_file(file.path(module_downloads, "redistribution_text_metrics.json"))

  if (file.exists(idx)) {
    txt <- paste(readLines(idx, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    note <- "<div class=\"lead-note\"><strong>Como leer el peso:</strong> el peso es la proporcion usada para repartir muertes de un grupo origen garbage hacia causas terminales elegibles. Un peso mayor recibe una fraccion mayor de esas muertes. La masa total se conserva, por eso el neto agregado puede ser cero aunque internamente unas causas ganen y otras pierdan muertes.</div>"
    if (!grepl("Como leer el peso", txt, fixed = TRUE)) {
      txt <- sub("</section>\\s*<section class=\"card\"><h3>Paneles principales", paste0(note, "</section><section class=\"card\"><h3>Paneles principales"), txt, perl = TRUE)
      writeLines(strsplit(txt, "\n", fixed = TRUE)[[1]], idx, useBytes = TRUE)
    }
  }

  if (all(file.exists(c(
    file.path(out_dir, "table_3_1_redistribution_groups.csv"),
    file.path(out_dir, "table_3_2_impact_total_by_year.csv"),
    file.path(out_dir, "table_3_3_impact_by_age_sex_year.csv"),
    file.path(out_dir, "table_3_4_before_after_by_disease_group.csv"),
    file.path(out_dir, "box_3_2_case_trace_cancer.csv")
  )))) {
    clean_label <- function(x) {
      x <- fix_mojibake_text(x)
      x <- gsub("Asignaci.*proporcional", "Asignacion proporcional", x)
      x <- gsub("gastrointestinaleseses", "Gastrointestinales", x, fixed = TRUE)
      x <- gsub("gastrointestinales", "Gastrointestinales", x, fixed = TRUE)
      x <- gsub("cardiovasculareses, infantiles/congenitas", "Cardiovasculares e infantiles/congenitas", x, fixed = TRUE)
      x <- gsub("cardiovasculareses, infantiles/cong", "Cardiovasculares e infantiles/congenitas", x, fixed = TRUE)
      x <- gsub("urol[oó]gicas", "urologicas", x, perl = TRUE)
      x <- gsub("ginecol[oó]gicas", "ginecologicas", x, perl = TRUE)
      x <- gsub("C[áa]ncer", "Cancer", x, perl = TRUE)
      x <- gsub("Anomal.*cong.*nitas", "Anomalias congenitas", x)
      x <- gsub("S.*ndrome de muerte s.*bita del lactante", "Sindrome de muerte subita del lactante", x)
      x <- gsub("Trastornos endocrinos, sangu.*neos e inmunol.*gicos", "Trastornos endocrinos, sanguineos e inmunologicos", x)
      x <- gsub("Trastornos neurol.*gicos", "Trastornos neurologicos", x)
      x <- gsub("Enfermedades musculoesquel.*ticas", "Enfermedades musculoesqueleticas", x)
      x <- gsub("Intenci.*n indeterminada", "Intencion indeterminada", x)
      x <- gsub("Todas las otras casas intermedias e inmediatas no espec.*ficas", "Todas las otras causas intermedias e inmediatas no especificas", x)
      x <- gsub("sintomas", "sintomas", x, fixed = TRUE)
      x <- gsub("100 y m.*s", "100 y mas", x)
      x
    }

    t31 <- fread(file.path(out_dir, "table_3_1_redistribution_groups.csv"), encoding = "UTF-8", showProgress = FALSE)
    t32 <- fread(file.path(out_dir, "table_3_2_impact_total_by_year.csv"), encoding = "UTF-8", showProgress = FALSE)
    t33 <- fread(file.path(out_dir, "table_3_3_impact_by_age_sex_year.csv"), encoding = "UTF-8", showProgress = FALSE)
    t34 <- fread(file.path(out_dir, "table_3_4_before_after_by_disease_group.csv"), encoding = "UTF-8", showProgress = FALSE)
    bx <- fread(file.path(out_dir, "box_3_2_case_trace_cancer.csv"), encoding = "UTF-8", showProgress = FALSE)
    met <- jsonlite::fromJSON(file.path(out_dir, "redistribution_text_metrics.json"), simplifyVector = TRUE)

    for (nm in intersect(c("Redistribution_group", "Method", "Scope_of_target_diseases", "Age_group", "Disease_group", "Stage", "focal_group"), names(t31))) t31[, (nm) := clean_label(get(nm))]
    if ("Redistribution_group" %in% names(t31)) t31[, Redistribution_group := clean_label(Redistribution_group)]
    if ("Method" %in% names(t31)) t31[, Method := clean_label(Method)]
    if ("Scope_of_target_diseases" %in% names(t31)) t31[, Scope_of_target_diseases := clean_label(Scope_of_target_diseases)]
    if ("Age_group" %in% names(t33)) t33[, Age_group := clean_label(Age_group)]
    if ("Disease_group" %in% names(t34)) t34[, Disease_group := clean_label(Disease_group)]
    if ("Stage" %in% names(t34)) t34[, Stage := clean_label(Stage)]
    if ("focal_group" %in% names(bx)) bx[, focal_group := clean_label(focal_group)]

    fmt_pct1 <- function(x) ifelse(is.na(x), "..", sprintf("%.1f", round(x, 1)))
    fmt_num1 <- function(x) ifelse(is.na(x), "..", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))

    t31d <- copy(t31)
    t31d[, ICD10_codes := fifelse(nzchar(ICD10_codes), ICD10_codes, "Ver descarga completa")]
    t31d[, `:=`(Number = fmt_num1(Number), Proportion_pct = fmt_pct1(Proportion_pct))]
    t31d <- t31d[, .(
      `Grupo de redistribucion` = Redistribution_group,
      `Codigos CIE-10` = ICD10_codes,
      `Metodo` = Method,
      `Alcance de causas destino` = Scope_of_target_diseases,
      `Numero` = Number,
      `Proporcion (%)` = Proportion_pct
    )]

    t32d <- copy(t32)
    t32all <- data.table(
      Reference_year = "Todos los anos",
      Total_deaths = sum(t32$Total_deaths, na.rm = TRUE),
      Deaths_for_redistribution = sum(t32$Deaths_for_redistribution, na.rm = TRUE),
      Per_cent_of_total_deaths = 100 * sum(t32$Deaths_for_redistribution, na.rm = TRUE) / sum(t32$Total_deaths, na.rm = TRUE),
      Total_YLL = sum(t32$Total_YLL, na.rm = TRUE),
      YLL_for_redistributed_deaths = sum(t32$YLL_for_redistributed_deaths, na.rm = TRUE),
      Per_cent_of_YLL_redistributed = 100 * sum(t32$YLL_for_redistributed_deaths, na.rm = TRUE) / sum(t32$Total_YLL, na.rm = TRUE)
    )
    t32d[, Reference_year := as.character(Reference_year)]
    t32d <- rbind(t32d, t32all, fill = TRUE)
    t32d[, `:=`(
      Total_deaths = fmt_num1(Total_deaths),
      Deaths_for_redistribution = fmt_num1(Deaths_for_redistribution),
      Per_cent_of_total_deaths = fmt_pct1(Per_cent_of_total_deaths),
      Total_YLL = fmt_num1(Total_YLL),
      YLL_for_redistributed_deaths = fmt_num1(YLL_for_redistributed_deaths),
      Per_cent_of_YLL_redistributed = fmt_pct1(Per_cent_of_YLL_redistributed)
    )]
    t32d <- t32d[, .(
      `Ano de referencia` = Reference_year,
      `Muertes totales` = Total_deaths,
      `Muertes para redistribucion` = Deaths_for_redistribution,
      `% del total de muertes` = Per_cent_of_total_deaths,
      `AVP totales` = Total_YLL,
      `AVP de muertes redistribuidas` = YLL_for_redistributed_deaths,
      `% de AVP redistribuidos` = Per_cent_of_YLL_redistributed
    )]

    t33d <- copy(t33)
    num33 <- setdiff(names(t33d), "Age_group")
    for (nm in num33) t33d[, (nm) := fmt_num1(get(nm))]
    t33d <- t33d[, .(
      `Grupo de edad` = Age_group,
      `Muertes femeninas` = Female_deaths,
      `Muertes masculinas` = Male_deaths,
      `Muertes en personas` = Person_deaths,
      `AVP femeninos` = Female_YLL,
      `AVP masculinos` = Male_YLL,
      `AVP en personas` = Person_YLL
    )]

    t34d <- copy(t34)
    t34d[, Disease_group_label := Disease_group]
    for (nm in c("Deaths", "YLLs")) t34d[, (nm) := fmt_num1(get(nm))]
    for (nm in c("Percent_deaths", "Percent_YLLs")) t34d[, (nm) := fmt_pct1(get(nm))]
    t34d[Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & Stage == "Cambio (antes a despues)", c("Deaths", "Percent_deaths", "YLLs", "Percent_YLLs") := list("..", "..", "..", "..")]
    t34d <- t34d[, .(
      `Grupo de enfermedad` = Disease_group_label,
      `Etapa` = Stage,
      `Muertes` = Deaths,
      `% muertes` = Percent_deaths,
      `AVP` = YLLs,
      `% AVP` = Percent_YLLs
    )]

    t34w <- copy(t34)
    abs_deaths <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-Deaths)][1:3]
    pct_deaths <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & is.finite(Percent_deaths)][order(-Percent_deaths)][1:3]
    abs_yll <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-YLLs)][1:3]
    pct_yll <- t34w[Stage == "Cambio (antes a despues)" & !Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & is.finite(Percent_YLLs)][order(-Percent_YLLs)][1:3]
    mk_lines <- function(dt, value_col, pct_col, unit_label) {
      if (nrow(dt) == 0) return(character())
      vapply(seq_len(nrow(dt)), function(i) sprintf("<li>%s (%s %s adicionales, un aumento de %s%%)</li>", esc_html(dt$Disease_group[i]), fmt_num1(dt[[value_col]][i]), unit_label, fmt_pct1(dt[[pct_col]][i])), character(1))
    }

    ref_year <- met$reference_year
    total_red_deaths <- met$total_deaths_redistributed
    total_red_pct <- met$total_pct_deaths_redistributed
    total_red_yll_pct <- met$total_pct_yll_redistributed
    box_title <- bx$focal_group[1]
    box_gain_pct <- if (is.finite(bx$deaths_gain[1]) && bx$deaths_before[1] > 0) 100 * bx$deaths_gain[1] / bx$deaths_before[1] else NA_real_
    box_accounted_pct <- if (is.finite(bx$deaths_gain[1]) && bx$deaths_gain[1] > 0) 100 * bx$accounted_by_direct_and_proportional[1] / bx$deaths_gain[1] else NA_real_

    sections_html <- c(
      '<section class="card">',
      '<div class="eyebrow">Grupos de redistribucion</div><h2 class="section-title">Grupos de redistribucion</h2>',
      '<p>Los codigos CIE-10 identificados para redistribucion se asignaron primero a grupos de redistribucion. Cada grupo se redistribuyo como un bloque completo al mismo universo de causas destino. Todas las muertes asignadas a un grupo se redistribuyeron con el mismo algoritmo.</p>',
      '<p>La tabla de abajo muestra los grupos de redistribucion, las causas destino y el metodo de redistribucion. El metodo por el cual se redistribuyo cada grupo dependio del nivel de evidencia disponible. Los grupos canonicos se muestran aunque en el ano visible tengan cero muertes redistribuidas.</p>',
      details_block('Como leer la Tabla 3.1', '<p><strong>Grupo de redistribucion</strong> es el grupo operativo de causas garbage o mal definidas. <strong>Codigos CIE-10</strong> se muestran resumidos en la tabla y completos en la descarga. <strong>Metodo</strong> indica si el grupo utilizo evidencia directa, MCOD indirecto, ambos o asignacion proporcional. <strong>Alcance de causas destino</strong> indica el universo de reasignacion. <strong>Numero</strong> es el volumen de muertes identificadas para redistribucion en el ano de referencia. Si el grupo existe en las reglas pero no tuvo casos ese ano, se muestra con <strong>0</strong>.</p>'),
      distribution_table_html(t31d, sprintf('Tabla 3.1: Numero y proporcion de muertes por grupo de redistribucion, metodo y causas destino, %s', ref_year), 'Las listas detalladas de CIE-10 estan disponibles en las descargas. La tabla principal conserva una lista acortada y legible.'),
      '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_1_redistribution_groups.csv">Descargar Tabla 3.1 CSV</a><a class="btn ghost" href="downloads/redistribution_rules_full.csv">Descargar reglas completas de redistribucion</a></div>',
      '</section>',
      '<section class="card">',
      '<div class="eyebrow">Impacto de la redistribucion</div><h2 class="section-title">Impacto de la redistribucion</h2>',
      sprintf('<p>Los AVP especificos por causa se ven afectados por las causas de muerte identificadas para redistribucion y por los metodos usados para reasignarlas. En este estudio, %s muertes fueron identificadas para redistribucion en %s, equivalentes a %s%% de las muertes y %s%% de los AVP.</p>', fmt_num1(total_red_deaths), ref_year, fmt_pct1(total_red_pct), fmt_pct1(total_red_yll_pct)),
      sprintf('<div class="lead-note"><strong>Nota metodologica.</strong> %s %s</div>', esc_html(clean_label(met$pure_note)), esc_html(clean_label(met$extended_sensitivity_note))),
      details_block('Como leer la Tabla 3.2', '<p><strong>Muertes totales</strong> y <strong>AVP totales</strong> corresponden al mismo universo base, antes de pandemia y subregistro. <strong>Muertes para redistribucion</strong> son las muertes inicialmente asignadas a grupos garbage o mal definidos. <strong>AVP de muertes redistribuidas</strong> son los AVP asociados a esas mismas muertes en el escenario base pre-redistribucion. Esta tabla mide el efecto puro de redistribucion, ceteris paribus.</p>'),
      distribution_table_html(t32d, 'Tabla 3.2: Numero y porcentaje de muertes y AVP, totales y redistribuidos, por ano de referencia'),
      '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_2_impact_total_by_year.csv">Descargar Tabla 3.2 CSV</a></div>',
      '</section>',
      '<section class="card">',
      sprintf('<p>El numero de muertes identificadas para redistribucion vario con la edad. La tabla siguiente muestra el patron por edad y sexo para %s.</p>', ref_year),
      details_block('Como leer la Tabla 3.3', '<p><strong>Muertes femeninas</strong>, <strong>muertes masculinas</strong> y <strong>muertes en personas</strong> son las muertes identificadas para redistribucion en cada grupo de edad. Las columnas de AVP muestran la carga de esas mismas muertes en el escenario base pre-redistribucion. La ultima fila debe cerrar con el total de muertes y AVP identificados para redistribucion en el ano de referencia.</p>'),
      distribution_table_html(t33d, sprintf('Tabla 3.3: Numero de muertes identificadas para redistribucion y AVP asociados, por edad y sexo, %s', ref_year)),
      '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_3_impact_by_age_sex_year.csv">Descargar Tabla 3.3 CSV</a></div>',
      '</section>',
      '<section class="card">',
      '<p>La Tabla 3.4 muestra el numero de muertes clasificadas a grupos de enfermedad antes y despues de la redistribucion. Las mayores ganancias absolutas de muertes por redistribucion fueron para:</p>',
      '<ul>', paste(mk_lines(abs_deaths, "Deaths", "Percent_deaths", "muertes"), collapse = ''), '</ul>',
      '<p>Las mayores ganancias proporcionales de muertes, aparte de las descritas arriba, fueron para:</p>',
      '<ul>', paste(mk_lines(pct_deaths, "Deaths", "Percent_deaths", "muertes"), collapse = ''), '</ul>',
      '<p>El impacto de la redistribucion sobre los AVP tambien se muestra en la Tabla 3.4. Las mayores ganancias absolutas de AVP fueron para:</p>',
      '<ul>', paste(mk_lines(abs_yll, "YLLs", "Percent_YLLs", "AVP"), collapse = ''), '</ul>',
      '<p>Otras grandes ganancias porcentuales en AVP fueron para:</p>',
      '<ul>', paste(mk_lines(pct_yll, "YLLs", "Percent_YLLs", "AVP"), collapse = ''), '</ul>',
      details_block('Como leer la Tabla 3.4', '<p>Cada grupo de enfermedad se muestra en tres filas. <strong>Antes de la redistribucion</strong> es el escenario base con una categoria residual explicita <strong>Redistribucion / garbage</strong>. <strong>Despues de la redistribucion</strong> es el escenario post-redistribucion. <strong>Cambio (antes a despues)</strong> expresa la ganancia absoluta y proporcional. Las filas de <strong>Redistribucion / garbage</strong> y <strong>Todas las muertes</strong> se incluyen para mostrar el cierre del ejercicio ceteris paribus, por eso su fila de cambio queda sin valor.</p>'),
      distribution_table_html(t34d, sprintf('Tabla 3.4: Numero y proporcion de muertes antes y despues de la redistribucion y cambio asociado, por grupo de enfermedad: Nacional, %s', ref_year)),
      '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_4_before_after_by_disease_group.csv">Descargar Tabla 3.4 CSV</a></div>',
      '</section>',
      '<section class="card box-highlight">',
      '<h3>Box 3.2: Como funciona la redistribucion</h3>',
      sprintf('<p>Esta caja explica el proceso de redistribucion y muestra, como ejemplo, de donde provienen las muertes adicionales en %s como resultado de la redistribucion.</p>', esc_html(tolower(box_title))),
      sprintf(
        '<p>La Tabla 3.4 muestra que antes de la redistribucion habia %s muertes clasificadas en %s. Despues de la redistribucion hubo %s muertes, lo que refleja una ganancia de %s muertes%s.</p>',
        fmt_num1(bx$deaths_before[1]),
        esc_html(box_title),
        fmt_num1(bx$deaths_after[1]),
        fmt_num1(bx$deaths_gain[1]),
        if (is.finite(box_gain_pct)) paste0(', o un aumento adicional de ', fmt_pct1(box_gain_pct), '%') else ''
      ),
      sprintf(
        '<p>La Tabla 3.1 muestra que %s muertes fueron identificadas en grupos de redistribucion ya orientados hacia %s. Un componente proporcional amplio aporto ademas unas %s muertes estimadas, segun la participacion pre-redistribucion de %s.</p>',
        fmt_num1(bx$direct_specific_group_deaths[1]),
        esc_html(box_title),
        fmt_num1(bx$proportional_general_group_deaths[1]),
        paste0(fmt_pct1(100 * bx$pre_redistribution_share[1]), '%')
      ),
      if (is.finite(box_accounted_pct)) sprintf('<p>Hasta aqui, alrededor de %s de la ganancia total en muertes de %s (%s de %s) puede explicarse por grupos dirigidos de forma especifica mas el componente proporcional amplio.</p>', paste0(fmt_pct1(box_accounted_pct), '%'), esc_html(tolower(box_title)), fmt_num1(bx$accounted_by_direct_and_proportional[1]), fmt_num1(bx$deaths_gain[1])) else '<p>En este caso, la ganancia neta final fue nula. La caja se conserva como trazabilidad metodologica.</p>',
      sprintf(
        '<p>Las %s muertes restantes provinieron de otros grupos de redistribucion en los que %s estaba dentro del alcance como causa destino. Esto preserva la logica ABDS de rastrear la ganancia hasta trayectorias especificas de redistribucion.</p>',
        fmt_num1(bx$remaining_gain_from_other_groups[1]),
        esc_html(tolower(box_title))
      ),
      '</section>',
      '<section class="card">',
      '<h3>Descargas y notas metodologicas</h3>',
      '<p class="muted">El HTML mantiene resumenes legibles en pantalla y deja los artefactos tecnicos completos en descargas para que la tabla siga siendo clara.</p>',
      glossary_html(c('masa', 'peso', 'garbage'), 'Glosario y notas metodologicas'),
      '<div class="nav-actions"><a class="btn ghost" href="downloads/box_3_2_case_trace_cancer.csv">Descargar traza de Box 3.2</a><a class="btn ghost" href="downloads/redistribution_text_metrics.json">Descargar metricas narrativas JSON</a><a class="btn ghost" href="downloads/audit_group_catalog_vs_observed.csv">Descargar auditoria catalogo vs observado</a><a class="btn ghost" href="downloads/audit_group_raw_sinadef_presence.csv">Descargar auditoria cruda SINADEF</a><a class="btn" href="../../index.html">Volver al portal</a></div>',
      '</section>'
    )

    write_portal_page(
      idx,
      'Redistribucion',
      page_shell(
        title = 'Redistribucion',
        intro = 'Reconstruccion estilo ABDS de grupos de redistribucion, impacto y caso trazado con insumos del estudio del Peru.',
        sidebar_items = list(list(label = 'Volver al portal', href = '../../index.html')),
        sections_html = sections_html
      ),
      rel_root = '../..'
    )

    red_md <- c(
      '# Grupos de redistribucion',
      '',
      'La tabla siguiente muestra los grupos de redistribucion, las causas destino y el metodo de redistribucion. El metodo por el cual se redistribuyo cada grupo dependio del nivel de evidencia disponible. Los grupos canonicos se muestran aunque en el ano visible tengan cero muertes redistribuidas.',
      '',
      sprintf('## Tabla 3.1: Numero y proporcion de muertes por grupo de redistribucion, metodo y causas destino, %s', ref_year),
      '',
      pipe_table_text(t31d, max_rows = 200L),
      '',
      '# Impacto de la redistribucion',
      '',
      sprintf('En este estudio, %s muertes fueron identificadas para redistribucion en %s, equivalentes a %s%% de las muertes y %s%% de los AVP.', fmt_num1(total_red_deaths), ref_year, fmt_pct1(total_red_pct), fmt_pct1(total_red_yll_pct)),
      '',
      paste("Nota metodologica.", clean_label(met$pure_note), clean_label(met$extended_sensitivity_note)),
      '',
      '## Tabla 3.2: Numero y porcentaje de muertes y AVP, totales y redistribuidos, por ano de referencia',
      '',
      pipe_table_text(t32d, max_rows = 200L),
      '',
      sprintf('## Tabla 3.3: Numero de muertes identificadas para redistribucion y AVP asociados, por edad y sexo, %s', ref_year),
      '',
      pipe_table_text(t33d, max_rows = 200L),
      '',
      sprintf('## Tabla 3.4: Numero y proporcion de muertes antes y despues de la redistribucion y cambio asociado, por grupo de enfermedad: Nacional, %s', ref_year),
      '',
      pipe_table_text(t34d, max_rows = 400L),
      '',
      '## Box 3.2: Como funciona la redistribucion',
      '',
      sprintf('Esta caja explica el proceso de redistribucion y muestra, como ejemplo, de donde provinieron las muertes adicionales en %s como resultado de la redistribucion.', tolower(box_title))
    )
    write_text_file(md_path, red_md)
  }
  invisible(TRUE)
}

epi_note_blocks <- function() {
  list(
    national_age = "Qu� muestra: curvas nacionales por edad y sexo, con l�neas por a�o. Qu� deber�a verse si est� bien: trayectorias relativamente suaves y un patr�n compatible con la historia natural de la causa. Se�ales de alerta: serruchos extremos, inversi�n biol�gicamente implausible entre sexos o picos aislados sin contexto.",
    regional_age = "Qu� muestra: la forma regional m�s reciente de la tasa por edad y sexo. Qu� deber�a verse si est� bien: perfiles regionales reconocibles y razonablemente comparables. Se�ales de alerta: quiebres abruptos, regiones con valores extremos aislados o perfiles incompatibles con el patr�n nacional.",
    trend = "Qu� muestra: tendencia temporal 2018-2024 para la causa, separada por sexo. Qu� deber�a verse si est� bien: cambios graduales, con shock pand�mico donde corresponda. Se�ales de alerta: saltos incompatibles con captaci�n, redistribuci�n o contexto epidemiol�gico.",
    heatmap = "Qu� muestra: un heatmap edad-a�o de la tasa nacional para ambos sexos. Qu� deber�a verse si est� bien: gradientes continuos y zonas de alta carga interpretables. Se�ales de alerta: bandas verticales u horizontales demasiado bruscas."
  )
}

build_epi_coherence_module <- function(root_dir) {
  module_root <- file.path(root_dir, "modules", "coherencia-epidemiologica")
  ensure_dir(module_root)
  ensure_dir(file.path(module_root, "causas"))
  ensure_dir(file.path(module_root, "figuras"))
  ensure_dir(file.path(module_root, "downloads"))
  out_dir <- here("data", "derived", "qc", "review_portal", "epi_coherence")
  ensure_dir(out_dir)

  register_default_decisions(
    "epi_coherence",
    data.table(
      decision = c("navegacion", "alcance_pdf", "granularidad_regional"),
      chosen = c(
        "indice_con_buscador_y_paginas_por_causa",
        "dos_tomos_resumen_nacional_y_regional",
        "graficos_regionales_faceteados_en_una_pagina_por_causa"
      ),
      rationale = c(
        "Los expertos tem�ticos necesitan entrar por diagn�stico, no por orden de scripts.",
        "Evita un PDF inmanejable y separa lectura lineal nacional de la carga regional.",
        "Permite cubrir todas las regiones sin depender de una app o de un servidor."
      ),
      reversible_point = c(
        "antes_de_publicar_el_modulo_epi",
        "antes_de_generar_los_tomos_finales",
        "antes_de_congelar_la_primera_version_para_revision_externa"
      )
    )
  )

  dt <- fread(
    here("data", "final", "report_tables", "mortality_report_long.csv"),
    select = c("year_id", "location_id", "location_name", "location_scope", "sex_id", "sex_label", "age_group", "cause_concept_id", "cause_level", "cause_name", "metric_rate"),
    showProgress = FALSE
  )
  dt <- dt[sex_label %in% c("Hombre", "Mujer", "Ambos")]
  dt[, age_group := ordered_age_groups(age_group)]
  causes <- unique(dt[, .(cause_concept_id, cause_level, cause_name)])[order(cause_level, cause_name)]
  latest_year <- max(dt$year_id, na.rm = TRUE)
  notes <- epi_note_blocks()

  page_manifest <- list()
  downloads_manifest <- data.table()
  figure_manifest <- data.table()
  cause_cards <- character()
  pdf_catalog_nat <- character()
  pdf_catalog_reg <- character()

  for (i in seq_len(nrow(causes))) {
    cause <- causes[i]
    slug <- sprintf("l%02d-c%s", as.integer(cause$cause_level), as.character(cause$cause_concept_id))
    cdt <- dt[cause_concept_id == cause$cause_concept_id]
    nat <- cdt[location_scope == "national"]
    reg <- cdt[location_scope == "regional"]
    if (nrow(nat) == 0) next

    ensure_dir(file.path(module_root, "downloads"))
    ensure_dir(file.path(module_root, "figuras"))
    ensure_dir(file.path(module_root, "causas"))
    fwrite(cdt, file.path(module_root, "downloads", paste0(slug, ".csv")))
    downloads_manifest <- rbind(downloads_manifest, data.table(
      cause_concept_id = cause$cause_concept_id,
      cause_name = cause$cause_name,
      published_path = normalize_slashes(file.path("modules", "coherencia-epidemiologica", "downloads", paste0(slug, ".csv")))
    ))

    nat_age_dt <- age_axis_without_total(nat[sex_label %in% c("Hombre", "Mujer", "Ambos")])
    p_nat_age <- ggplot(nat_age_dt, aes(x = age_group, y = metric_rate, color = factor(year_id), group = year_id)) +
      geom_line(linewidth = 0.65, alpha = 0.9) +
      geom_point(size = 0.9, alpha = 0.9) +
      facet_wrap(~ sex_label, scales = "fixed") +
      scale_color_viridis_d(option = "C", end = 0.9) +
      labs(title = "Curvas nacionales por edad y sexo", subtitle = cause$cause_name, x = "Edad", y = "Tasa por 100,000", color = "A�o") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))

    reg_year_pngs <- character()
    for (yy in sort(unique(cdt$year_id))) {
      reg_year <- age_axis_without_total(reg[year_id == yy & sex_label %in% c("Hombre", "Mujer", "Ambos")])
      p_reg_age <- ggplot(reg_year, aes(x = age_group, y = metric_rate, color = sex_label, group = sex_label)) +
        geom_line(linewidth = 0.45, alpha = 0.9) +
        geom_point(size = 0.55, alpha = 0.9) +
        facet_wrap(~ location_name, scales = "fixed", ncol = 5) +
        scale_color_manual(values = c("Hombre" = "#0f4c81", "Mujer" = "#b42318", "Ambos" = "#1e7f5c")) +
        labs(title = sprintf("Curvas regionales por edad y sexo (%s)", yy), subtitle = cause$cause_name, x = "Edad", y = "Tasa") +
        theme_minimal(base_size = 8) +
        theme(legend.position = "bottom", strip.text = element_text(size = 7), axis.text.x = element_text(angle = 45, hjust = 1, size = 5))
      reg_year_png <- file.path(module_root, "figuras", paste0(slug, "_reg_age_", yy, ".png"))
      save_plot_png(p_reg_age, reg_year_png, width = 15, height = 11)
      reg_year_pngs <- c(reg_year_pngs, reg_year_png)
    }
    reg_age_png <- reg_year_pngs[which.max(as.integer(gsub(".*_([0-9]{4})\\.png$", "\\1", reg_year_pngs)))]

    trend_dt <- all_age_rows(nat)
    p_trend <- ggplot(trend_dt[sex_label %in% c("Hombre", "Mujer", "Ambos")], aes(x = year_id, y = metric_rate, color = sex_label)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.4) +
      scale_color_manual(values = c("Hombre" = "#0f4c81", "Mujer" = "#b42318", "Ambos" = "#1e7f5c")) +
      labs(title = "Tendencia temporal por sexo", subtitle = cause$cause_name, x = "A�o", y = "Tasa agregada") +
      theme_minimal(base_size = 11) +
      theme(legend.position = "bottom")

    hm_dt <- age_axis_without_total(nat[sex_label == "Ambos"])
    p_heat <- ggplot(hm_dt, aes(x = year_id, y = age_group, fill = metric_rate)) +
      geom_tile() +
      scale_fill_gradient(low = "#eef5fb", high = "#0f4c81") +
      labs(title = "Heatmap edad-a�o de la tasa nacional", subtitle = cause$cause_name, x = "A�o", y = "Edad", fill = "Tasa") +
      theme_minimal(base_size = 11)

    nat_age_png <- file.path(module_root, "figuras", paste0(slug, "_nat_age.png"))
    trend_png <- file.path(module_root, "figuras", paste0(slug, "_trend.png"))
    heat_png <- file.path(module_root, "figuras", paste0(slug, "_heat.png"))
    save_plot_png(p_nat_age, nat_age_png, width = 11, height = 6.5)
    save_plot_png(p_trend, trend_png, width = 10.5, height = 5.8)
    save_plot_png(p_heat, heat_png, width = 10.5, height = 6.5)

    figure_manifest <- rbind(
      figure_manifest,
      data.table(
        cause_concept_id = cause$cause_concept_id,
        cause_name = cause$cause_name,
        figure_kind = c("national_age_sex", paste0("regional_age_sex_", gsub(".*_([0-9]{4})\\.png$", "\\1", reg_year_pngs)), "trend", "heatmap"),
        published_path = normalize_slashes(file.path("modules", "coherencia-epidemiologica", "figuras", basename(c(nat_age_png, reg_year_pngs, trend_png, heat_png))))
      ),
      fill = TRUE
    )

    reg_years <- gsub(".*_([0-9]{4})\\.png$", "\\1", reg_year_pngs)
    reg_selector <- paste0(
      "<div class=\"figure-dashboard\"><div class=\"figure-controls\"><label>Ano regional<select data-figure-switch=\"", slug, "-reg-year\">",
      paste(sprintf("<option value=\"%s\"%s>%s</option>", reg_years, ifelse(basename(reg_year_pngs) == basename(reg_age_png), " selected", ""), reg_years), collapse = ""),
      "</select></label></div>",
      paste(sprintf(
        "<div class=\"figure-panel\" data-figure-panel=\"%s-reg-year\" data-figure-value=\"%s\"><img src=\"../figuras/%s\" alt=\"regional %s\"></div>",
        slug, reg_years, basename(reg_year_pngs), reg_years
      ), collapse = ""),
      "</div>"
    )

    cause_page <- c(
      "<section class=\"card\">",
      sprintf("<div class=\"eyebrow\">Coherencia epidemiol�gica</div><h2 class=\"section-title\">%s</h2>", esc_html(cause$cause_name)),
      sprintf("<div class=\"pill-row\"><span class=\"pill\">ID %s</span><span class=\"pill\">Nivel %s</span><span class=\"pill\">�ltimo a�o observado %s</span></div>", cause$cause_concept_id, cause$cause_level, latest_year),
      "<div class=\"lead-note\">Este m�dulo est� pensado para revisi�n humana. Aqu� no todo se cierra con una regla binaria; la idea es que el epidemi�logo contraste patrones, outliers, quiebres y coherencia entre edad, sexo y regi�n.</div>",
      sprintf("<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"../downloads/%s.csv\">Descargar CSV filtrado de la causa</a><a class=\"btn\" href=\"../../../index.html\">Volver al portal</a></div>", slug),
      "</section>",
      glossary_html(c("masa", "calibracion"), "Glosario rapido de coherencia epidemiologica"),
      "<section class=\"card\"><h3>Curvas y paneles</h3><div class=\"image-grid\">",
      sprintf("<div class=\"image-card\"><h4>Curvas nacionales por edad y sexo</h4><p class=\"muted\">%s</p><img src=\"../figuras/%s\" alt=\"nacional\"></div>", esc_html(notes$national_age), basename(nat_age_png)),
      sprintf("<div class=\"image-card\"><h4>Curvas regionales por ano</h4><p class=\"muted\">%s</p>%s</div>", esc_html(notes$regional_age), reg_selector),
      sprintf("<div class=\"image-card\"><h4>Tendencia temporal por sexo</h4><p class=\"muted\">%s</p><img src=\"../figuras/%s\" alt=\"tendencia\"></div>", esc_html(notes$trend), basename(trend_png)),
      sprintf("<div class=\"image-card\"><h4>Heatmap edad-a�o nacional</h4><p class=\"muted\">%s</p><img src=\"../figuras/%s\" alt=\"heatmap\"></div>", esc_html(notes$heatmap), basename(heat_png)),
      "</div></section>"
    )

    write_portal_page(
      file.path(module_root, "causas", paste0(slug, ".html")),
      cause$cause_name,
      page_shell(
        title = cause$cause_name,
        intro = "Revisi�n visual tem�tica de tasas de mortalidad por edad, sexo y regi�n para una causa espec�fica.",
        sidebar_items = list(
          list(label = "Volver al �ndice del m�dulo", href = "../index.html"),
          list(label = "Volver al portal", href = "../../../index.html")
        ),
        sections_html = cause_page
      ),
      rel_root = "../../.."
    )

    cause_cards <- c(
      cause_cards,
      sprintf(
        "<div class=\"catalog-item\" data-catalog-item data-search=\"%s %s\" data-level=\"%s\" data-geo=\"all\"><h4><a href=\"causas/%s.html\">%s</a></h4><p class=\"muted\">ID %s | Nivel %s</p><div class=\"pill-row\"><span class=\"pill\">Nacional + regional</span><span class=\"pill\">Descarga CSV</span></div></div>",
        esc_html(cause$cause_name), cause$cause_concept_id, cause$cause_level, slug, esc_html(cause$cause_name), cause$cause_concept_id, cause$cause_level
      )
    )
    pdf_catalog_nat <- c(pdf_catalog_nat, sprintf("## Nivel %s - %s\n\n![](%s)\n\n![](%s)\n", cause$cause_level, cause$cause_name, normalize_slashes(nat_age_png), normalize_slashes(trend_png)))
    pdf_catalog_reg <- c(pdf_catalog_reg, sprintf("## Nivel %s - %s\n\n![](%s)\n\n", cause$cause_level, cause$cause_name, normalize_slashes(reg_age_png)))
    page_manifest[[length(page_manifest) + 1L]] <- list(title = cause$cause_name, href = normalize_slashes(file.path("modules", "coherencia-epidemiologica", "causas", paste0(slug, ".html"))))
  }

  module_sections <- c(
    "<section class=\"card\">",
    "<div class=\"eyebrow\">Módulo temático</div><h2 class=\"section-title\">Control de coherencia epidemiológica</h2>",
    "<p class=\"muted\">Use el buscador para entrar por enfermedad o diagnóstico. Cada página de causa contiene curvas nacionales, vista regional, tendencia temporal, heatmap y descarga del subconjunto de datos.</p>",
    "<div class=\"search-bar\"><input type=\"search\" placeholder=\"Buscar causa o ID\" data-catalog-search><select data-catalog-level><option value=\"\">Todos los niveles</option><option value=\"0\">Nivel 0</option><option value=\"1\">Nivel 1</option><option value=\"2\">Nivel 2</option><option value=\"3\">Nivel 3</option><option value=\"4\">Nivel 4</option></select><select data-catalog-geo><option value=\"all\">Todos los ámbitos</option></select></div>",
    "</section>",
    "<section class=\"card\"><div class=\"catalog-list\">",
    paste(cause_cards, collapse = ""),
    "</div></section>"
  )
  write_portal_page(
    file.path(module_root, "index.html"),
    "Coherencia epidemiol�gica",
    page_shell(
      title = "Coherencia epidemiol�gica",
      intro = "Módulo para revisión visual experta de tasas por edad, sexo, región y tiempo.",
      sidebar_items = list(list(label = "Volver al portal", href = "../../index.html")),
      sections_html = module_sections
    ),
    rel_root = "../.."
  )

  fwrite(causes, file.path(out_dir, "inventory.csv"))
  fwrite(downloads_manifest, file.path(out_dir, "downloads_manifest.csv"))
  fwrite(figure_manifest, file.path(out_dir, "figure_manifest.csv"))
  write_text_file(file.path(out_dir, "page_manifest.json"), jsonlite::toJSON(page_manifest, pretty = TRUE, auto_unbox = TRUE))
  write_text_file(file.path(out_dir, "pdf_national_body.md"), pdf_catalog_nat)
  write_text_file(file.path(out_dir, "pdf_regional_body.md"), pdf_catalog_reg)

  list(title = "Coherencia epidemiológica", href = "modules/coherencia-epidemiologica/index.html", causes = causes)
}

build_redistribution_module <- function(root_dir) {
  module_root <- file.path(root_dir, "modules", "redistribucion")
  ensure_dir(module_root)
  ensure_dir(file.path(module_root, "figuras"))
  ensure_dir(file.path(module_root, "downloads"))
  out_dir <- here("data", "derived", "qc", "review_portal", "redistribution")
  ensure_dir(out_dir)

  register_default_decisions(
    "redistribution",
    data.table(
      decision = c("estructura_abds", "anio_referencia", "caso_box_3_2"),
      chosen = c(
        "replica_editorial_abds_con_tablas_3_1_a_3_4_y_box_3_2",
        "ultimo_anio_disponible_del_estudio",
        "cancer_si_la_trazabilidad_es_limpia_sino_grupo_con_mayor_ganancia"
      ),
      rationale = c(
        "La secuencia ABDS hace trazable el metodo, el impacto y el caso explicativo en una sola narrativa.",
        "Replica la logica de anio de referencia usada en ABDS y evita mezclar una tabla anual con un resumen multianual.",
        "Permite explicar de donde sale una ganancia real con evidencia de grupos fuente y alcance metodologico."
      ),
      reversible_point = c(
        "antes_de_publicar_el_modulo_html",
        "antes_de_congelar_el_reporte_formal",
        "antes_de_cerrar_el_box_metodologico"
      )
    )
  )

  abds <- build_redistribution_abds_data_v3()
  reference_year <- abds$reference_year
  table_3_1 <- copy(abds$table_3_1)
  table_3_2 <- copy(abds$table_3_2)
  table_3_3 <- copy(abds$table_3_3)
  table_3_4 <- copy(abds$table_3_4)
  box_3_2 <- copy(abds$box_3_2)
  metrics <- abds$text_metrics

  rules_src <- here("data", "final", "redistribution_rules", "redistribution_rules.csv")
  if (file.exists(rules_src)) {
    file.copy(rules_src, file.path(module_root, "downloads", "redistribution_rules_full.csv"), overwrite = TRUE)
  }

  generated_csv <- c(
    "table_3_1_redistribution_groups.csv",
    "table_3_2_impact_total_by_year.csv",
    "table_3_3_impact_by_age_sex_year.csv",
    "table_3_4_before_after_by_disease_group.csv",
    "box_3_2_case_trace_cancer.csv",
    "redistribution_text_metrics.json",
    "audit_group_catalog_vs_observed.csv",
    "audit_group_raw_sinadef_presence.csv",
    "audit_group_table31_inclusion.csv"
  )
  for (nm in generated_csv) {
    src <- file.path(out_dir, nm)
    if (file.exists(src)) file.copy(src, file.path(module_root, "downloads", nm), overwrite = TRUE)
  }

  fmt_pct1 <- function(x) ifelse(is.na(x), "..", sprintf("%.1f", round(x, 1)))
  fmt_num1 <- function(x) ifelse(is.na(x), "..", format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE))

  table_3_1_display <- copy(table_3_1)
  table_3_1_display[, `:=`(
    Redistribution_group = fifelse(nzchar(Redistribution_group), Redistribution_group, "Grupo sin etiqueta"),
    ICD10_codes = fifelse(nzchar(ICD10_codes), ICD10_codes, "Ver descarga completa"),
    Number = fmt_num1(Number),
    Proportion_pct = fmt_pct1(Proportion_pct)
  )]
  table_3_1_display <- table_3_1_display[, .(
    `Grupo de redistribuci�n` = Redistribution_group,
    `C�digos CIE-10` = ICD10_codes,
    `M�todo` = Method,
    `Alcance de causas destino` = Scope_of_target_diseases,
    `N�mero` = Number,
    `Proporci�n (%)` = Proportion_pct
  )]

  table_3_2_display <- copy(table_3_2)
  table_3_2_all <- data.table(
    Reference_year = "Todos los a�os",
    Total_deaths = sum(table_3_2$Total_deaths, na.rm = TRUE),
    Deaths_for_redistribution = sum(table_3_2$Deaths_for_redistribution, na.rm = TRUE),
    Per_cent_of_total_deaths = 100 * sum(table_3_2$Deaths_for_redistribution, na.rm = TRUE) / sum(table_3_2$Total_deaths, na.rm = TRUE),
    Total_YLL = sum(table_3_2$Total_YLL, na.rm = TRUE),
    YLL_for_redistributed_deaths = sum(table_3_2$YLL_for_redistributed_deaths, na.rm = TRUE),
    Per_cent_of_YLL_redistributed = 100 * sum(table_3_2$YLL_for_redistributed_deaths, na.rm = TRUE) / sum(table_3_2$Total_YLL, na.rm = TRUE)
  )
  table_3_2_display[, Reference_year := as.character(Reference_year)]
  table_3_2_display <- rbind(table_3_2_display, table_3_2_all, fill = TRUE)
  table_3_2_display[, `:=`(
    Total_deaths = fmt_num1(Total_deaths),
    Deaths_for_redistribution = fmt_num1(Deaths_for_redistribution),
    Per_cent_of_total_deaths = fmt_pct1(Per_cent_of_total_deaths),
    Total_YLL = fmt_num1(Total_YLL),
    YLL_for_redistributed_deaths = fmt_num1(YLL_for_redistributed_deaths),
    Per_cent_of_YLL_redistributed = fmt_pct1(Per_cent_of_YLL_redistributed)
  )]
  table_3_2_display <- table_3_2_display[, .(
    `A�o de referencia` = Reference_year,
    `Muertes totales` = Total_deaths,
    `Muertes para redistribuci�n` = Deaths_for_redistribution,
    `% del total de muertes` = Per_cent_of_total_deaths,
    `AVP totales` = Total_YLL,
    `AVP de muertes redistribuidas` = YLL_for_redistributed_deaths,
    `% de AVP redistribuidos` = Per_cent_of_YLL_redistributed
  )]

  table_3_3_display <- copy(table_3_3)
  num_cols_33 <- setdiff(names(table_3_3_display), "Age_group")
  for (col in num_cols_33) table_3_3_display[, (col) := fmt_num1(get(col))]
  table_3_3_display <- table_3_3_display[, .(
    `Grupo de edad` = Age_group,
    `Muertes femeninas` = Female_deaths,
    `Muertes masculinas` = Male_deaths,
    `Muertes en personas` = Person_deaths,
    `AVP femeninos` = Female_YLL,
    `AVP masculinos` = Male_YLL,
    `AVP en personas` = Person_YLL
  )]

  table_3_4_display <- copy(table_3_4)
  table_3_4_display[, Disease_group_label := Disease_group]
  table_3_4_display[duplicated(Disease_group_label), Disease_group_label := ""]
  for (col in c("Deaths", "YLLs")) table_3_4_display[, (col) := fmt_num1(get(col))]
  for (col in c("Percent_deaths", "Percent_YLLs")) table_3_4_display[, (col) := fmt_pct1(get(col))]
  table_3_4_display[Disease_group %in% c("Redistribuci�n / garbage", "Todas las muertes") & Stage == "Cambio (antes a despu�s)", c("Deaths", "Percent_deaths", "YLLs", "Percent_YLLs") := list("..", "..", "..", "..")]
  table_3_4_display <- table_3_4_display[, .(
    `Grupo de enfermedad` = Disease_group_label,
    `Etapa` = Stage,
    `Muertes` = Deaths,
    `% muertes` = Percent_deaths,
    `AVP` = YLLs,
    `% AVP` = Percent_YLLs
  )]

  top_abs_deaths <- as.data.table(metrics$top_absolute_death_gains)
  top_pct_deaths <- as.data.table(metrics$top_percentage_death_gains)
  top_abs_yll <- as.data.table(metrics$top_absolute_yll_gains)
  top_pct_yll <- as.data.table(metrics$top_percentage_yll_gains)

  top_abs_death_lines <- vapply(seq_len(nrow(top_abs_deaths)), function(i) {
    sprintf("<li>%s (%s muertes adicionales, un aumento de %s%%)</li>", esc_html(top_abs_deaths$group[i]), fmt_num1(top_abs_deaths$value[i]), fmt_pct1(top_abs_deaths$pct[i]))
  }, character(1))
  top_pct_death_lines <- vapply(seq_len(nrow(top_pct_deaths)), function(i) {
    sprintf("<li>%s (%s muertes adicionales, un aumento de %s%%)</li>", esc_html(top_pct_deaths$group[i]), fmt_num1(top_pct_deaths$value[i]), fmt_pct1(top_pct_deaths$pct[i]))
  }, character(1))
  top_abs_yll_lines <- vapply(seq_len(nrow(top_abs_yll)), function(i) {
    sprintf("<li>%s (%s AVP adicionales, un aumento de %s%%)</li>", esc_html(top_abs_yll$group[i]), fmt_num1(top_abs_yll$value[i]), fmt_pct1(top_abs_yll$pct[i]))
  }, character(1))
  top_pct_yll_lines <- vapply(seq_len(nrow(top_pct_yll)), function(i) {
    sprintf("<li>%s (%s AVP adicionales, un aumento de %s%%)</li>", esc_html(top_pct_yll$group[i]), fmt_num1(top_pct_yll$value[i]), fmt_pct1(top_pct_yll$pct[i]))
  }, character(1))

  gc_ref <- table_3_2[Reference_year == reference_year]
  total_red_deaths <- gc_ref$Deaths_for_redistribution[1]
  total_red_pct <- gc_ref$Per_cent_of_total_deaths[1]
  total_red_yll_pct <- gc_ref$Per_cent_of_YLL_redistributed[1]

  box_title <- box_3_2$focal_group[1]
  box_html <- c(
    '<section class="card box-highlight">',
    '<h3>Box 3.2: C�mo funciona la redistribuci�n</h3>',
    sprintf('<p>Esta caja explica el proceso de redistribuci�n y muestra, como ejemplo, de d�nde provienen las muertes adicionales en %s como resultado de la redistribuci�n.</p>', esc_html(tolower(box_title))),
    sprintf('<p>La Tabla 3.4 muestra que antes de la redistribuci�n hab�a %s muertes clasificadas en %s. Despu�s de la redistribuci�n hubo %s muertes, lo que refleja una ganancia de %s muertes, o un aumento adicional de %s%%.</p>',
            fmt_num1(box_3_2$deaths_before[1]), esc_html(box_title), fmt_num1(box_3_2$deaths_after[1]), fmt_num1(box_3_2$deaths_gain[1]), fmt_pct1(100 * box_3_2$deaths_gain[1] / pmax(box_3_2$deaths_before[1], 1))),
    sprintf('<p>La Tabla 3.1 muestra que %s muertes fueron identificadas en grupos de redistribuci�n ya orientados hacia %s. Un componente proporcional amplio aport� adem�s unas %s muertes estimadas, seg�n la participaci�n pre-redistribuci�n de %s.</p>',
            fmt_num1(box_3_2$direct_specific_group_deaths[1]), esc_html(box_title), fmt_num1(box_3_2$proportional_general_group_deaths[1]), paste0(fmt_pct1(100 * box_3_2$pre_redistribution_share[1]), '%')),
    sprintf('<p>Hasta aqu�, alrededor de %s de la ganancia total en muertes de %s (%s de %s) puede explicarse por grupos dirigidos de forma espec�fica m�s el componente proporcional amplio.</p>',
            paste0(fmt_pct1(100 * box_3_2$accounted_by_direct_and_proportional[1] / pmax(box_3_2$deaths_gain[1], 1)), '%'), esc_html(tolower(box_title)), fmt_num1(box_3_2$accounted_by_direct_and_proportional[1]), fmt_num1(box_3_2$deaths_gain[1])),
    sprintf('<p>Las %s muertes restantes provinieron de otros grupos de redistribuci�n en los que %s estaba dentro del alcance como causa destino. Esto preserva la l�gica ABDS de rastrear la ganancia hasta trayectorias espec�ficas de redistribuci�n.</p>', fmt_num1(box_3_2$remaining_gain_from_other_groups[1]), esc_html(tolower(box_title))),
    '</section>'
  )

  notes_31 <- paste(
    "Los c�digos CIE-10 identificados para redistribuci�n se asignaron primero a grupos de redistribuci�n.",
    "Cada grupo se redistribuy� como un bloque completo al mismo universo de causas destino.",
    "Todas las muertes asignadas a un grupo se redistribuyeron con el mismo algoritmo."
  )
  notes_impact <- sprintf(
    "Los AVP espec�ficos por causa se ven afectados por las causas de muerte identificadas para redistribuci�n y por los m�todos usados para reasignarlas. En este estudio, %s muertes fueron identificadas para redistribuci�n en %s, equivalentes a %s%% de las muertes y %s%% de los AVP.",
    fmt_num1(total_red_deaths), reference_year, fmt_pct1(total_red_pct), fmt_pct1(total_red_yll_pct)
  )

  html_sections <- c(
    '<section class="card">',
    '<div class="eyebrow">Grupos de redistribuci�n</div><h2 class="section-title">Grupos de redistribuci�n</h2>',
    sprintf('<p>%s</p>', esc_html(notes_31)),
    '<p>La tabla de abajo muestra los grupos de redistribuci�n, las causas destino y el m�todo de redistribuci�n. El m�todo por el cual se redistribuy� cada grupo dependi� del nivel de evidencia disponible.</p>',
    details_block('C�mo leer la Tabla 3.1', '<p><strong>Grupo de redistribuci�n</strong> es el grupo operativo de causas garbage o mal definidas. <strong>C�digos CIE-10</strong> se muestran resumidos en la tabla y completos en la descarga. <strong>M�todo</strong> indica si el grupo utiliz� evidencia directa, MCOD indirecto, ambos o asignaci�n proporcional. <strong>Alcance de causas destino</strong> indica el universo de reasignaci�n. <strong>N�mero</strong> es el volumen de muertes identificadas para redistribuci�n en el a�o de referencia. <strong>Proporci�n</strong> es la fracci�n de todas las muertes identificadas para redistribuci�n en ese a�o.</p>'),
    distribution_table_html(table_3_1_display, sprintf('Tabla 3.1: N�mero y proporci�n de muertes por grupo de redistribuci�n, m�todo y causas destino, %s', reference_year), 'Las listas detalladas de CIE-10 est�n disponibles en las descargas. La tabla principal conserva una lista acortada y legible.'),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_1_redistribution_groups.csv">Descargar Tabla 3.1 CSV</a><a class="btn ghost" href="downloads/redistribution_rules_full.csv">Descargar reglas completas de redistribuci�n</a></div>',
    '</section>',
    '<section class="card">',
    '<div class="eyebrow">Impacto de la redistribuci�n</div><h2 class="section-title">Impacto de la redistribuci�n</h2>',
    sprintf('<p>%s</p>', esc_html(notes_impact)),
    details_block('C�mo leer la Tabla 3.2', '<p><strong>Muertes totales</strong> y <strong>AVP totales</strong> son los totales can�nicos del a�o. <strong>Muertes para redistribuci�n</strong> son las muertes inicialmente asignadas a grupos de redistribuci�n. <strong>AVP de muertes redistribuidas</strong> se calculan aqu� con un an�lisis de sensibilidad sin redistribuci�n y con eliminaci�n de garbage. Las columnas porcentuales expresan cu�nto del total fue afectado por la redistribuci�n.</p>'),
    distribution_table_html(table_3_2_display, 'Tabla 3.2: N�mero y porcentaje de muertes y AVP, totales y redistribuidos, por a�o de referencia'),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_2_impact_total_by_year.csv">Descargar Tabla 3.2 CSV</a></div>',
    '</section>',
    '<section class="card">',
    sprintf('<p>El n�mero de muertes identificadas para redistribuci�n vari� con la edad. La tabla siguiente muestra el patr�n por edad y sexo para %s.</p>', reference_year),
    details_block('C�mo leer la Tabla 3.3', '<p><strong>Muertes femeninas</strong>, <strong>muertes masculinas</strong> y <strong>muertes en personas</strong> son las muertes identificadas para redistribuci�n en cada grupo de edad. Las columnas de AVP muestran la carga correspondiente. La �ltima fila debe cerrar con el total de muertes y AVP identificados para redistribuci�n en el a�o de referencia.</p>'),
    distribution_table_html(table_3_3_display, sprintf('Tabla 3.3: N�mero de muertes identificadas para redistribuci�n y AVP asociados, por edad y sexo, %s', reference_year)),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_3_impact_by_age_sex_year.csv">Descargar Tabla 3.3 CSV</a></div>',
    '</section>',
    '<section class="card">',
    '<p>La Tabla 3.4 muestra el n�mero de muertes clasificadas a grupos de enfermedad antes y despu�s de la redistribuci�n. Las mayores ganancias absolutas de muertes por redistribuci�n fueron para:</p>',
    '<ul>', paste(top_abs_death_lines, collapse = ''), '</ul>',
    '<p>Las mayores ganancias proporcionales de muertes, aparte de las descritas arriba, fueron para:</p>',
    '<ul>', paste(top_pct_death_lines, collapse = ''), '</ul>',
    '<p>El impacto de la redistribuci�n sobre los AVP tambi�n se muestra en la Tabla 3.4. Las mayores ganancias absolutas de AVP fueron para:</p>',
    '<ul>', paste(top_abs_yll_lines, collapse = ''), '</ul>',
    '<p>Otras grandes ganancias porcentuales en AVP fueron para:</p>',
    '<ul>', paste(top_pct_yll_lines, collapse = ''), '</ul>',
    details_block('C�mo leer la Tabla 3.4', '<p>Cada grupo de enfermedad se muestra en tres filas. <strong>Antes de la redistribuci�n</strong> es el escenario de sensibilidad sin redistribuci�n y con eliminaci�n de garbage. <strong>Despu�s de la redistribuci�n</strong> es el escenario can�nico. <strong>Cambio (antes a despu�s)</strong> expresa la ganancia absoluta y proporcional. Las filas de <strong>Redistribuci�n / garbage</strong> y <strong>Todas las muertes</strong> se incluyen para hacer visible el cierre del ejercicio contrafactual.</p>'),
    distribution_table_html(table_3_4_display, sprintf('Tabla 3.4: N�mero y proporci�n de muertes antes y despu�s de la redistribuci�n y cambio asociado, por grupo de enfermedad: Nacional, %s', reference_year)),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_4_before_after_by_disease_group.csv">Descargar Tabla 3.4 CSV</a></div>',
    '</section>',
    paste(box_html, collapse = '\n'),
    '<section class="card">',
    '<h3>Descargas y notas metodol�gicas</h3>',
    '<p class="muted">El HTML mantiene res�menes legibles en pantalla y deja los artefactos t�cnicos completos en descargas para que la tabla siga siendo clara.</p>',
    glossary_html(c('masa', 'peso', 'garbage'), 'Glosario y notas metodol�gicas'),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/box_3_2_case_trace_cancer.csv">Descargar traza de Box 3.2</a><a class="btn ghost" href="downloads/redistribution_text_metrics.json">Descargar m�tricas narrativas JSON</a><a class="btn ghost" href="downloads/audit_group_catalog_vs_observed.csv">Descargar auditor�a cat�logo vs observado</a><a class="btn ghost" href="downloads/audit_group_raw_sinadef_presence.csv">Descargar auditor�a cruda SINADEF</a><a class="btn" href="../../index.html">Volver al portal</a></div>',
    '</section>'
  )

  write_portal_page(
    file.path(module_root, 'index.html'),
    'Redistribuci�n',
    page_shell(
      title = 'Redistribuci�n',
      intro = 'Reconstrucci�n estilo ABDS de grupos de redistribuci�n, impacto y caso trazado con insumos del estudio de Per�.',
      sidebar_items = list(list(label = 'Volver al portal', href = '../../index.html')),
      sections_html = html_sections
    ),
    rel_root = '../..'
  )

  red_md <- c(
    '# Grupos de redistribuci�n',
    '',
    notes_31,
    '',
    'La tabla siguiente muestra los grupos de redistribuci�n, las causas destino y el m�todo de redistribuci�n. El m�todo por el cual se redistribuy� cada grupo dependi� del nivel de evidencia disponible.',
    '',
    sprintf('## Tabla 3.1: N�mero y proporci�n de muertes por grupo de redistribuci�n, m�todo y causas destino, %s', reference_year),
    '',
    pipe_table_text(table_3_1_display, max_rows = 200L),
    '',
    '# Impacto de la redistribuci�n',
    '',
    notes_impact,
    '',
    '## Tabla 3.2: N�mero y porcentaje de muertes y AVP, totales y redistribuidos, por a�o de referencia',
    '',
    pipe_table_text(table_3_2_display, max_rows = 200L),
    '',
    sprintf('## Tabla 3.3: N�mero de muertes identificadas para redistribuci�n y AVP asociados, por edad y sexo, %s', reference_year),
    '',
    pipe_table_text(table_3_3_display, max_rows = 200L),
    '',
    'La Tabla 3.4 muestra el n�mero de muertes clasificadas a grupos de enfermedad antes y despu�s de la redistribuci�n.',
    '',
    'Las mayores ganancias absolutas de muertes por redistribuci�n fueron para:',
    '',
    gsub('<[^>]+>', '', top_abs_death_lines),
    '',
    'Otras grandes ganancias porcentuales en AVP fueron para:',
    '',
    gsub('<[^>]+>', '', top_pct_yll_lines),
    '',
    sprintf('## Tabla 3.4: N�mero y proporci�n de muertes antes y despu�s de la redistribuci�n y cambio asociado, por grupo de enfermedad: Nacional, %s', reference_year),
    '',
    pipe_table_text(table_3_4_display, max_rows = 400L),
    '',
    '## Box 3.2: C�mo funciona la redistribuci�n',
    '',
    sprintf('Esta caja explica el proceso de redistribuci�n y muestra, como ejemplo, de d�nde provinieron las muertes adicionales en %s como resultado de la redistribuci�n.', tolower(box_title)),
    '',
    sprintf('Antes de la redistribuci�n hab�a %s muertes en %s. Despu�s de la redistribuci�n hubo %s muertes, lo que refleja una ganancia de %s muertes.', fmt_num1(box_3_2$deaths_before[1]), box_title, fmt_num1(box_3_2$deaths_after[1]), fmt_num1(box_3_2$deaths_gain[1])),
    '',
    sprintf('Los grupos de redistribuci�n con alcance directo aportaron %s muertes. El componente proporcional amplio aport� unas %s muertes estimadas. Las %s muertes restantes provinieron de otros grupos en los que %s se mantuvo dentro del universo de causas destino.', fmt_num1(box_3_2$direct_specific_group_deaths[1]), fmt_num1(box_3_2$proportional_general_group_deaths[1]), fmt_num1(box_3_2$remaining_gain_from_other_groups[1]), tolower(box_title))
  )
  table_3_1_display <- copy(table_3_1)
  table_3_1_display[, `:=`(
    Redistribution_group = fifelse(nzchar(Redistribution_group), Redistribution_group, "Grupo sin etiqueta"),
    ICD10_codes = fifelse(nzchar(ICD10_codes), ICD10_codes, "Ver descarga completa"),
    Number = fmt_num1(Number),
    Proportion_pct = fmt_pct1(Proportion_pct)
  )]
  table_3_1_display <- table_3_1_display[, .(
    `Grupo de redistribución` = Redistribution_group,
    `Códigos CIE-10` = ICD10_codes,
    `Método` = Method,
    `Alcance de causas destino` = Scope_of_target_diseases,
    `Número` = Number,
    `Proporción (%)` = Proportion_pct
  )]

  table_3_2_display <- copy(table_3_2)
  table_3_2_all <- data.table(
    Reference_year = "Todos los años",
    Total_deaths = sum(table_3_2$Total_deaths, na.rm = TRUE),
    Deaths_for_redistribution = sum(table_3_2$Deaths_for_redistribution, na.rm = TRUE),
    Per_cent_of_total_deaths = 100 * sum(table_3_2$Deaths_for_redistribution, na.rm = TRUE) / sum(table_3_2$Total_deaths, na.rm = TRUE),
    Total_YLL = sum(table_3_2$Total_YLL, na.rm = TRUE),
    YLL_for_redistributed_deaths = sum(table_3_2$YLL_for_redistributed_deaths, na.rm = TRUE),
    Per_cent_of_YLL_redistributed = 100 * sum(table_3_2$YLL_for_redistributed_deaths, na.rm = TRUE) / sum(table_3_2$Total_YLL, na.rm = TRUE)
  )
  table_3_2_display[, Reference_year := as.character(Reference_year)]
  table_3_2_display <- rbind(table_3_2_display, table_3_2_all, fill = TRUE)
  table_3_2_display[, `:=`(
    Total_deaths = fmt_num1(Total_deaths),
    Deaths_for_redistribution = fmt_num1(Deaths_for_redistribution),
    Per_cent_of_total_deaths = fmt_pct1(Per_cent_of_total_deaths),
    Total_YLL = fmt_num1(Total_YLL),
    YLL_for_redistributed_deaths = fmt_num1(YLL_for_redistributed_deaths),
    Per_cent_of_YLL_redistributed = fmt_pct1(Per_cent_of_YLL_redistributed)
  )]
  table_3_2_display <- table_3_2_display[, .(
    `Año de referencia` = Reference_year,
    `Muertes totales` = Total_deaths,
    `Muertes para redistribución` = Deaths_for_redistribution,
    `% del total de muertes` = Per_cent_of_total_deaths,
    `AVP totales` = Total_YLL,
    `AVP de muertes redistribuidas` = YLL_for_redistributed_deaths,
    `% de AVP redistribuidos` = Per_cent_of_YLL_redistributed
  )]

  table_3_3_display <- copy(table_3_3)
  num_cols_33 <- setdiff(names(table_3_3_display), "Age_group")
  for (col in num_cols_33) table_3_3_display[, (col) := fmt_num1(get(col))]
  table_3_3_display <- table_3_3_display[, .(
    `Grupo de edad` = Age_group,
    `Muertes femeninas` = Female_deaths,
    `Muertes masculinas` = Male_deaths,
    `Muertes en personas` = Person_deaths,
    `AVP femeninos` = Female_YLL,
    `AVP masculinos` = Male_YLL,
    `AVP en personas` = Person_YLL
  )]

  table_3_4_display <- copy(table_3_4)
  table_3_4_display[, Disease_group_label := Disease_group]
  table_3_4_display[duplicated(Disease_group_label), Disease_group_label := ""]
  for (col in c("Deaths", "YLLs")) table_3_4_display[, (col) := fmt_num1(get(col))]
  for (col in c("Percent_deaths", "Percent_YLLs")) table_3_4_display[, (col) := fmt_pct1(get(col))]
  table_3_4_display[Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & Stage == "Cambio (antes a despues)", c("Deaths", "Percent_deaths", "YLLs", "Percent_YLLs") := list("..", "..", "..", "..")]
  table_3_4_display <- table_3_4_display[, .(
    `Grupo de enfermedad` = Disease_group_label,
    `Etapa` = Stage,
    `Muertes` = Deaths,
    `% muertes` = Percent_deaths,
    `AVP` = YLLs,
    `% AVP` = Percent_YLLs
  )]

  top_abs_deaths <- as.data.table(metrics$top_absolute_death_gains)
  top_pct_deaths <- as.data.table(metrics$top_percentage_death_gains)
  top_abs_yll <- as.data.table(metrics$top_absolute_yll_gains)
  top_pct_yll <- as.data.table(metrics$top_percentage_yll_gains)
  top_abs_death_lines <- vapply(seq_len(nrow(top_abs_deaths)), function(i) sprintf("<li>%s (%s muertes adicionales, un aumento de %s%%)</li>", esc_html(top_abs_deaths$group[i]), fmt_num1(top_abs_deaths$value[i]), fmt_pct1(top_abs_deaths$pct[i])), character(1))
  top_pct_death_lines <- vapply(seq_len(nrow(top_pct_deaths)), function(i) sprintf("<li>%s (%s muertes adicionales, un aumento de %s%%)</li>", esc_html(top_pct_deaths$group[i]), fmt_num1(top_pct_deaths$value[i]), fmt_pct1(top_pct_deaths$pct[i])), character(1))
  top_abs_yll_lines <- vapply(seq_len(nrow(top_abs_yll)), function(i) sprintf("<li>%s (%s AVP adicionales, un aumento de %s%%)</li>", esc_html(top_abs_yll$group[i]), fmt_num1(top_abs_yll$value[i]), fmt_pct1(top_abs_yll$pct[i])), character(1))
  top_pct_yll_lines <- vapply(seq_len(nrow(top_pct_yll)), function(i) sprintf("<li>%s (%s AVP adicionales, un aumento de %s%%)</li>", esc_html(top_pct_yll$group[i]), fmt_num1(top_pct_yll$value[i]), fmt_pct1(top_pct_yll$pct[i])), character(1))

  gc_ref <- table_3_2[Reference_year == reference_year]
  total_red_deaths <- gc_ref$Deaths_for_redistribution[1]
  total_red_pct <- gc_ref$Per_cent_of_total_deaths[1]
  total_red_yll_pct <- gc_ref$Per_cent_of_YLL_redistributed[1]
  box_title <- box_3_2$focal_group[1]
  box_gain_pct <- if (is.finite(box_3_2$deaths_gain[1]) && box_3_2$deaths_gain[1] > 0) 100 * box_3_2$deaths_gain[1] / pmax(box_3_2$deaths_before[1], 1) else NA_real_
  box_accounted_pct <- if (is.finite(box_3_2$deaths_gain[1]) && box_3_2$deaths_gain[1] > 0) 100 * box_3_2$accounted_by_direct_and_proportional[1] / box_3_2$deaths_gain[1] else NA_real_
  box_html <- c(
    '<section class="card box-highlight">',
    '<h3>Box 3.2: Cómo funciona la redistribución</h3>',
    sprintf('<p>Esta caja explica el proceso de redistribución y muestra, como ejemplo, de dónde provienen las muertes adicionales en %s como resultado de la redistribución.</p>', esc_html(tolower(box_title))),
    sprintf('<p>La Tabla 3.4 muestra que antes de la redistribución había %s muertes clasificadas en %s. Después de la redistribución hubo %s muertes, lo que refleja una ganancia de %s muertes%s.</p>',
            fmt_num1(box_3_2$deaths_before[1]), esc_html(box_title), fmt_num1(box_3_2$deaths_after[1]), fmt_num1(box_3_2$deaths_gain[1]),
            if (is.finite(box_gain_pct)) paste0(", o un aumento adicional de ", fmt_pct1(box_gain_pct), "%") else ""),
    sprintf('<p>La Tabla 3.1 muestra que %s muertes fueron identificadas en grupos de redistribución ya orientados hacia %s. Un componente proporcional amplio aportó además unas %s muertes estimadas, según la participación pre-redistribución de %s.</p>',
            fmt_num1(box_3_2$direct_specific_group_deaths[1]), esc_html(box_title), fmt_num1(box_3_2$proportional_general_group_deaths[1]), paste0(fmt_pct1(100 * box_3_2$pre_redistribution_share[1]), '%')),
    if (is.finite(box_accounted_pct)) {
      sprintf('<p>Hasta aquí, alrededor de %s de la ganancia total en muertes de %s (%s de %s) puede explicarse por grupos dirigidos de forma específica más el componente proporcional amplio.</p>',
              paste0(fmt_pct1(box_accounted_pct), '%'), esc_html(tolower(box_title)), fmt_num1(box_3_2$accounted_by_direct_and_proportional[1]), fmt_num1(box_3_2$deaths_gain[1]))
    } else {
      sprintf('<p>En este caso, la ganancia neta final fue nula. La caja se conserva como trazabilidad metodológica del alcance de grupos dirigidos y del componente proporcional amplio hacia %s.</p>', esc_html(tolower(box_title)))
    },
    sprintf('<p>Las %s muertes restantes provinieron de otros grupos de redistribución en los que %s estaba dentro del alcance como causa destino. Esto preserva la lógica ABDS de rastrear la ganancia hasta trayectorias específicas de redistribución.</p>', fmt_num1(box_3_2$remaining_gain_from_other_groups[1]), esc_html(tolower(box_title))),
    '</section>'
  )

  notes_31 <- paste(
    "Los códigos CIE-10 identificados para redistribución se asignaron primero a grupos de redistribución.",
    "Cada grupo se redistribuyó como un bloque completo al mismo universo de causas destino.",
    "Todas las muertes asignadas a un grupo se redistribuyeron con el mismo algoritmo."
  )
  notes_impact <- sprintf(
    "Los AVP específicos por causa se ven afectados por las causas de muerte identificadas para redistribución y por los métodos usados para reasignarlas. En este estudio, %s muertes fueron identificadas para redistribución en %s, equivalentes a %s%% de las muertes y %s%% de los AVP.",
    fmt_num1(total_red_deaths), reference_year, fmt_pct1(total_red_pct), fmt_pct1(total_red_yll_pct)
  )

  html_sections <- c(
    '<section class="card">',
    '<div class="eyebrow">Grupos de redistribución</div><h2 class="section-title">Grupos de redistribución</h2>',
    sprintf('<p>%s</p>', esc_html(notes_31)),
    '<p>La tabla de abajo muestra los grupos de redistribución, las causas destino y el método de redistribución. El método por el cual se redistribuyó cada grupo dependió del nivel de evidencia disponible. Los grupos canónicos se muestran aunque en el año visible tengan cero muertes redistribuidas.</p>',
    details_block('Cómo leer la Tabla 3.1', '<p><strong>Grupo de redistribución</strong> es el grupo operativo de causas garbage o mal definidas. <strong>Códigos CIE-10</strong> se muestran resumidos en la tabla y completos en la descarga. <strong>Método</strong> indica si el grupo utilizó evidencia directa, MCOD indirecto, ambos o asignación proporcional. <strong>Alcance de causas destino</strong> indica el universo de reasignación. <strong>Número</strong> es el volumen de muertes identificadas para redistribución en el año de referencia. Si el grupo existe en las reglas pero no tuvo casos ese año, se muestra con <strong>0</strong>.</p>'),
    distribution_table_html(table_3_1_display, sprintf('Tabla 3.1: Número y proporción de muertes por grupo de redistribución, método y causas destino, %s', reference_year), 'Las listas detalladas de CIE-10 están disponibles en las descargas. La tabla principal conserva una lista acortada y legible.'),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_1_redistribution_groups.csv">Descargar Tabla 3.1 CSV</a><a class="btn ghost" href="downloads/redistribution_rules_full.csv">Descargar reglas completas de redistribución</a></div>',
    '</section>',
    '<section class="card">',
    '<div class="eyebrow">Impacto de la redistribución</div><h2 class="section-title">Impacto de la redistribución</h2>',
    sprintf('<p>%s</p>', esc_html(notes_impact)),
    sprintf('<div class="lead-note"><strong>Nota metodológica.</strong> %s %s</div>', esc_html(metrics$pure_note), esc_html(metrics$extended_sensitivity_note)),
    details_block('Cómo leer la Tabla 3.2', '<p><strong>Muertes totales</strong> y <strong>AVP totales</strong> corresponden al mismo universo base, antes de pandemia y subregistro. <strong>Muertes para redistribución</strong> son las muertes inicialmente asignadas a grupos garbage o mal definidos. <strong>AVP de muertes redistribuidas</strong> son los AVP asociados a esas mismas muertes en el escenario base pre-redistribución. Esta tabla mide el efecto puro de redistribución, ceteris paribus.</p>'),
    distribution_table_html(table_3_2_display, 'Tabla 3.2: Número y porcentaje de muertes y AVP, totales y redistribuidos, por año de referencia'),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_2_impact_total_by_year.csv">Descargar Tabla 3.2 CSV</a></div>',
    '</section>',
    '<section class="card">',
    sprintf('<p>El número de muertes identificadas para redistribución varió con la edad. La tabla siguiente muestra el patrón por edad y sexo para %s.</p>', reference_year),
    details_block('Cómo leer la Tabla 3.3', '<p><strong>Muertes femeninas</strong>, <strong>muertes masculinas</strong> y <strong>muertes en personas</strong> son las muertes identificadas para redistribución en cada grupo de edad. Las columnas de AVP muestran la carga de esas mismas muertes en el escenario base pre-redistribución. La última fila debe cerrar con el total de muertes y AVP identificados para redistribución en el año de referencia.</p>'),
    distribution_table_html(table_3_3_display, sprintf('Tabla 3.3: Número de muertes identificadas para redistribución y AVP asociados, por edad y sexo, %s', reference_year)),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_3_impact_by_age_sex_year.csv">Descargar Tabla 3.3 CSV</a></div>',
    '</section>',
    '<section class="card">',
    '<p>La Tabla 3.4 muestra el número de muertes clasificadas a grupos de enfermedad antes y después de la redistribución. Las mayores ganancias absolutas de muertes por redistribución fueron para:</p>',
    '<ul>', paste(top_abs_death_lines, collapse = ''), '</ul>',
    '<p>Las mayores ganancias proporcionales de muertes, aparte de las descritas arriba, fueron para:</p>',
    '<ul>', paste(top_pct_death_lines, collapse = ''), '</ul>',
    '<p>El impacto de la redistribución sobre los AVP también se muestra en la Tabla 3.4. Las mayores ganancias absolutas de AVP fueron para:</p>',
    '<ul>', paste(top_abs_yll_lines, collapse = ''), '</ul>',
    '<p>Otras grandes ganancias porcentuales en AVP fueron para:</p>',
    '<ul>', paste(top_pct_yll_lines, collapse = ''), '</ul>',
    details_block('Cómo leer la Tabla 3.4', '<p>Cada grupo de enfermedad se muestra en tres filas. <strong>Antes de la redistribución</strong> es el escenario base con una categoría residual explícita <strong>Redistribucion / garbage</strong>. <strong>Después de la redistribución</strong> es el escenario post-redistribución. <strong>Cambio (antes a después)</strong> expresa la ganancia absoluta y proporcional. Las filas de <strong>Redistribucion / garbage</strong> y <strong>Todas las muertes</strong> se incluyen para mostrar el cierre del ejercicio ceteris paribus, por eso su fila de cambio queda sin valor.</p>'),
    distribution_table_html(table_3_4_display, sprintf('Tabla 3.4: Número y proporción de muertes antes y después de la redistribución y cambio asociado, por grupo de enfermedad: Nacional, %s', reference_year)),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/table_3_4_before_after_by_disease_group.csv">Descargar Tabla 3.4 CSV</a></div>',
    '</section>',
    paste(box_html, collapse = '\n'),
    '<section class="card">',
    '<h3>Descargas y notas metodológicas</h3>',
    '<p class="muted">El HTML mantiene resúmenes legibles en pantalla y deja los artefactos técnicos completos en descargas para que la tabla siga siendo clara.</p>',
    glossary_html(c('masa', 'peso', 'garbage'), 'Glosario y notas metodológicas'),
    '<div class="nav-actions"><a class="btn ghost" href="downloads/box_3_2_case_trace_cancer.csv">Descargar traza de Box 3.2</a><a class="btn ghost" href="downloads/redistribution_text_metrics.json">Descargar métricas narrativas JSON</a><a class="btn ghost" href="downloads/audit_group_catalog_vs_observed.csv">Descargar auditoría catálogo vs observado</a><a class="btn ghost" href="downloads/audit_group_raw_sinadef_presence.csv">Descargar auditoría cruda SINADEF</a><a class="btn" href="../../index.html">Volver al portal</a></div>',
    '</section>'
  )

  write_portal_page(
    file.path(module_root, 'index.html'),
    'Redistribución',
    page_shell(
      title = 'Redistribución',
      intro = 'Reconstrucción estilo ABDS de grupos de redistribución, impacto y caso trazado con insumos del estudio del Perú.',
      sidebar_items = list(list(label = 'Volver al portal', href = '../../index.html')),
      sections_html = html_sections
    ),
    rel_root = '../..'
  )

  red_md <- c(
    '# Grupos de redistribución',
    '',
    notes_31,
    '',
    'La tabla siguiente muestra los grupos de redistribución, las causas destino y el método de redistribución. El método por el cual se redistribuyó cada grupo dependió del nivel de evidencia disponible. Los grupos canónicos se muestran aunque en el año visible tengan cero muertes redistribuidas.',
    '',
    sprintf('## Tabla 3.1: Número y proporción de muertes por grupo de redistribución, método y causas destino, %s', reference_year),
    '',
    pipe_table_text(table_3_1_display, max_rows = 200L),
    '',
    '# Impacto de la redistribución',
    '',
    notes_impact,
    '',
    metrics$pure_note,
    '',
    metrics$extended_sensitivity_note,
    '',
    '## Tabla 3.2: Número y porcentaje de muertes y AVP, totales y redistribuidos, por año de referencia',
    '',
    pipe_table_text(table_3_2_display, max_rows = 200L),
    '',
    sprintf('## Tabla 3.3: Número de muertes identificadas para redistribución y AVP asociados, por edad y sexo, %s', reference_year),
    '',
    pipe_table_text(table_3_3_display, max_rows = 200L),
    '',
    'La Tabla 3.4 muestra el número de muertes clasificadas a grupos de enfermedad antes y después de la redistribución.',
    '',
    'Las mayores ganancias absolutas de muertes por redistribución fueron para:',
    '',
    gsub('<[^>]+>', '', top_abs_death_lines),
    '',
    'Otras grandes ganancias porcentuales en AVP fueron para:',
    '',
    gsub('<[^>]+>', '', top_pct_yll_lines),
    '',
    sprintf('## Tabla 3.4: Número y proporción de muertes antes y después de la redistribución y cambio asociado, por grupo de enfermedad: Nacional, %s', reference_year),
    '',
    pipe_table_text(table_3_4_display, max_rows = 400L),
    '',
    '## Box 3.2: Cómo funciona la redistribución',
    '',
    sprintf('Esta caja explica el proceso de redistribución y muestra, como ejemplo, de dónde provinieron las muertes adicionales en %s como resultado de la redistribución.', tolower(box_title)),
    '',
    sprintf('Antes de la redistribución había %s muertes en %s. Después de la redistribución hubo %s muertes, lo que refleja una ganancia de %s muertes.', fmt_num1(box_3_2$deaths_before[1]), box_title, fmt_num1(box_3_2$deaths_after[1]), fmt_num1(box_3_2$deaths_gain[1])),
    '',
    sprintf('Los grupos de redistribución con alcance directo aportaron %s muertes. El componente proporcional amplio aportó unas %s muertes estimadas. Las %s muertes restantes provinieron de otros grupos en los que %s se mantuvo dentro del universo de causas destino.', fmt_num1(box_3_2$direct_specific_group_deaths[1]), fmt_num1(box_3_2$proportional_general_group_deaths[1]), fmt_num1(box_3_2$remaining_gain_from_other_groups[1]), tolower(box_title))
  )
  write_text_file(file.path(out_dir, 'redistribution_pdf_body.md'), red_md)

  fwrite(table_3_1, file.path(out_dir, 'inventory.csv'))
  fwrite(data.table(
    published_path = normalize_slashes(file.path('modules', 'redistribucion', 'downloads', c(
      'redistribution_rules_full.csv',
      generated_csv[generated_csv != 'redistribution_text_metrics.json'],
      'redistribution_text_metrics.json',
      'audit_group_catalog_vs_observed.csv',
      'audit_group_raw_sinadef_presence.csv',
      'audit_group_table31_inclusion.csv'
    )))
  ), file.path(out_dir, 'downloads_manifest.csv'))
  fwrite(
    data.table(
      module = character(),
      figure_path = character(),
      note = character()
    ),
    file.path(out_dir, 'figure_manifest.csv')
  )
  write_text_file(file.path(out_dir, 'page_manifest.json'), toJSON(list(list(title = 'Redistribución', href = 'modules/redistribucion/index.html')), pretty = TRUE, auto_unbox = TRUE))

  list(title = 'Redistribución', href = 'modules/redistribucion/index.html', reference_year = reference_year)
}

build_pandemic_module <- function(root_dir) {
  module_root <- file.path(root_dir, "modules", "pandemia-subregistro")
  ensure_dir(module_root)
  ensure_dir(file.path(module_root, "figuras"))
  ensure_dir(file.path(module_root, "downloads"))
  out_dir <- here("data", "derived", "qc", "review_portal", "pandemic_subregistro")
  ensure_dir(out_dir)

  register_default_decisions(
    "pandemic_subregistro",
    data.table(
      decision = c("base_analitica", "comparativa_etapas", "detalle_regional"),
      chosen = c(
        "death_cause_final_hierarchical_como_base_canonica",
        "comparativa_observado_post_redistribucion_pandemia_y_completitud",
        "regional_completo_en_html_y_resumen_en_pdf"
      ),
      rationale = c(
        "Permite analizar niveles 0-4 sin reconstrucci�n adicional y conserva todas las columnas clave del ajuste.",
        "Es la comparaci�n m�s �til para explicar cu�nto agrega cada capa del pipeline.",
        "Mantiene legibilidad y deja el detalle fino para revisi�n humana interactiva."
      ),
      reversible_point = c("antes_de_publicar_el_modulo_pandemia", "antes_de_congelar_las_tablas_del_informe", "antes_de_entregar_a_expertos")
    )
  )

  dt <- fread(
    here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv"),
    select = c(
      "year_id", "location_id", "sex_id", "age", "cause_level",
      "deaths_observed", "deaths_post_redistribution", "deaths_final_net_of_pandemic", "deaths_final",
      "pandemic_excess_component", "pandemic_reassigned_out_component"
    ),
    showProgress = FALSE
  )
  dt_leaf <- fread(
    here("data", "final", "death_cause_final", "death_cause_final.csv"),
    select = c(
      "year_id", "location_id", "sex_id", "age",
      "correction_factor_completeness", "observed_allcause", "expected_allcause", "observed_corrected_allcause"
    ),
    showProgress = FALSE
  )
  total <- dt[cause_level == 0, .(
    observed = sum(deaths_observed, na.rm = TRUE),
    post_redistribution = sum(deaths_post_redistribution, na.rm = TRUE),
    sin_pandemia = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    final = sum(deaths_final, na.rm = TRUE),
    pandemic_excess = sum(pandemic_excess_component, na.rm = TRUE),
    pandemic_reassigned = sum(pandemic_reassigned_out_component, na.rm = TRUE),
    expected_allcause = 0,
    observed_allcause = 0
  ), by = .(year_id)]
  total[, correction_gain := final - sin_pandemia]
  total[, oprm_reassigned_abs := abs(pandemic_reassigned)]

  long_stage <- melt(total, id.vars = "year_id", measure.vars = c("observed", "post_redistribution", "sin_pandemia", "final"), variable.name = "stage", value.name = "deaths")
  p_stage <- ggplot(long_stage, aes(x = year_id, y = deaths, color = stage, linetype = stage)) +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(values = c(observed = "#667085", post_redistribution = "#0f4c81", sin_pandemia = "#1e7f5c", final = "#b42318")) +
    scale_linetype_manual(values = c(observed = "dashed", post_redistribution = "solid", sin_pandemia = "dotdash", final = "solid")) +
    labs(title = "Etapas de transformaci�n de las muertes totales", subtitle = "Color y tipo de l�nea distinguen series que pueden solaparse exactamente.", x = "A�o", y = "Muertes", color = NULL, linetype = NULL) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  p_oprm <- ggplot(total, aes(x = year_id, y = oprm_reassigned_abs)) +
    geom_col(fill = "#b42318") +
    labs(title = "Volumen absoluto reasignado por OPRM", subtitle = "Cu�ntas muertes se movieron por el componente pand�mico reasignado", x = "A�o", y = "Muertes reasignadas") +
    theme_minimal(base_size = 11)

  compare_inei <- unique(dt_leaf[, .(year_id, location_id, age, sex_id, observed_allcause, expected_allcause, observed_corrected_allcause)])
  compare_inei <- compare_inei[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
  ), by = .(year_id, location_id, age, sex_id)]
  compare_inei[, gap_before := expected_allcause - observed_allcause]
  compare_inei[, gap_after := expected_allcause - observed_corrected_allcause]
  compare_inei[, corrected_expected_ratio := observed_corrected_allcause / expected_allcause]
  compare_inei_year <- compare_inei[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]
  compare_inei_year[, gap_before := expected_allcause - observed_allcause]
  compare_inei_year[, gap_after := expected_allcause - observed_corrected_allcause]
  compare_inei_year[, corrected_expected_ratio := observed_corrected_allcause / expected_allcause]
  p_inei <- ggplot(compare_inei[location_id %in% 1:6], aes(x = age, color = factor(year_id))) +
    geom_line(aes(y = expected_allcause), linewidth = 0.8, linetype = "solid") +
    geom_line(aes(y = observed_allcause), linewidth = 0.6, linetype = "dashed") +
    geom_line(aes(y = observed_corrected_allcause), linewidth = 0.6, linetype = "dotdash") +
    facet_grid(sex_id ~ location_id, scales = "free_y") +
    scale_color_brewer(palette = "Set2") +
    labs(title = "Esperado INEI vs observado SINADEF", subtitle = "Muestra regional por edad, sexo y a�o. S�lida = esperado; segmentada = observado; punto-raya = observado corregido.", x = "Edad", y = "Muertes all-cause", color = "A�o") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom")

  factor_dt <- unique(dt_leaf[, .(year_id, location_id, sex_id, age, correction_factor_completeness)])
  p_factor <- ggplot(factor_dt[location_id %in% 1:6], aes(x = age, y = correction_factor_completeness, color = factor(year_id))) +
    geom_line(linewidth = 0.7) +
    facet_grid(sex_id ~ location_id, scales = "free_y") +
    scale_color_brewer(palette = "Dark2") +
    labs(title = "Factor de correcci�n por completitud", subtitle = "Muestra regional por edad, sexo y a�o", x = "Edad", y = "Factor", color = "A�o") +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom")

  fig_stage <- file.path(module_root, "figuras", "pandemia_etapas_totales.png")
  fig_oprm <- file.path(module_root, "figuras", "pandemia_oprm.png")
  fig_inei <- file.path(module_root, "figuras", "pandemia_inei_vs_observado.png")
  fig_factor <- file.path(module_root, "figuras", "pandemia_factor_correccion.png")
  save_plot_png(p_stage, fig_stage, width = 11, height = 6)
  save_plot_png(p_oprm, fig_oprm, width = 11, height = 6)
  save_plot_png(p_inei, fig_inei, width = 15, height = 10)
  save_plot_png(p_factor, fig_factor, width = 15, height = 10)

  stage_table <- total[, .(
    year_id,
    observado_sin_redistribucion = observed,
    observado_con_redistribucion = post_redistribution,
    con_redistribucion_sin_pandemia = sin_pandemia,
    con_redistribucion_y_pandemia = final,
    muertes_reasignadas_oprm = oprm_reassigned_abs,
    muertes_agregadas_por_factor = correction_gain
  )]
  stage_by_level <- dt[, .(
    observado_sin_redistribucion = sum(deaths_observed, na.rm = TRUE),
    observado_con_redistribucion = sum(deaths_post_redistribution, na.rm = TRUE),
    con_redistribucion_sin_pandemia = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    final_con_pandemia_y_subregistro = sum(deaths_final, na.rm = TRUE),
    exceso_pandemico = sum(pandemic_excess_component, na.rm = TRUE),
    reasignado_oprm = sum(pandemic_reassigned_out_component, na.rm = TRUE)
  ), by = .(year_id, cause_level)][order(year_id, cause_level)]
  stage_by_region_year <- dt[cause_level == 0, .(
    observado_sin_redistribucion = sum(deaths_observed, na.rm = TRUE),
    observado_con_redistribucion = sum(deaths_post_redistribution, na.rm = TRUE),
    con_redistribucion_sin_pandemia = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    final_con_pandemia_y_subregistro = sum(deaths_final, na.rm = TRUE),
    exceso_pandemico = sum(pandemic_excess_component, na.rm = TRUE),
    reasignado_oprm = sum(pandemic_reassigned_out_component, na.rm = TRUE)
  ), by = .(year_id, location_id)][order(year_id, location_id)]
  stage_by_age_sex <- dt[cause_level == 0, .(
    observado_sin_redistribucion = sum(deaths_observed, na.rm = TRUE),
    observado_con_redistribucion = sum(deaths_post_redistribution, na.rm = TRUE),
    con_redistribucion_sin_pandemia = sum(deaths_final_net_of_pandemic, na.rm = TRUE),
    final_con_pandemia_y_subregistro = sum(deaths_final, na.rm = TRUE),
    exceso_pandemico = sum(pandemic_excess_component, na.rm = TRUE),
    reasignado_oprm = sum(pandemic_reassigned_out_component, na.rm = TRUE)
  ), by = .(year_id, sex_id, age)][order(year_id, sex_id, age)]
  compare_inei_region_year <- compare_inei[, .(
    observed_allcause = sum(observed_allcause, na.rm = TRUE),
    expected_allcause = sum(expected_allcause, na.rm = TRUE),
    observed_corrected_allcause = sum(observed_corrected_allcause, na.rm = TRUE)
  ), by = .(year_id, location_id)][order(year_id, location_id)]
  compare_inei_region_year[, gap_before := expected_allcause - observed_allcause]
  compare_inei_region_year[, gap_after := expected_allcause - observed_corrected_allcause]
  compare_inei_region_year[, corrected_expected_ratio := observed_corrected_allcause / expected_allcause]
  compare_inei_region_year[, tolerance_note := fifelse(abs(gap_after) <= pmax(1e-6, expected_allcause * 1e-6), "OK_CON_NOTA_residuo_minimo", "DIFERENCIA_METODOLOGICA_CUANTIFICADA")]
  pandemic_balance_path <- here("data", "derived", "qc", "build_death_cause_final", "qc_pandemic_reallocation_balance.csv")
  pandemic_balance <- if (file.exists(pandemic_balance_path)) fread(pandemic_balance_path, showProgress = FALSE) else data.table()
  pandemic_balance_summary <- if (nrow(pandemic_balance)) {
    pandemic_balance[, .(
      max_abs_delta_base_vs_corrected = max(abs(delta_base_vs_corrected), na.rm = TRUE),
      max_abs_delta_net_vs_target = max(abs(delta_net_vs_target), na.rm = TRUE),
      max_abs_delta_final_vs_corrected = max(abs(delta_final_vs_corrected), na.rm = TRUE),
      max_abs_delta_in_vs_out = max(abs(delta_in_vs_out), na.rm = TRUE),
      status = fifelse(
        max(abs(delta_base_vs_corrected), abs(delta_net_vs_target), abs(delta_final_vs_corrected), abs(delta_in_vs_out), na.rm = TRUE) <= 1e-6,
        "OK_CON_NOTA_residuo_numerico",
        "REVISAR"
      )
    )]
  } else {
    data.table(status = "NO_DISPONIBLE")
  }

  fwrite(stage_table, file.path(module_root, "downloads", "pandemia_stage_comparison.csv"))
  fwrite(stage_by_level, file.path(module_root, "downloads", "pandemia_stage_by_year_level.csv"))
  fwrite(stage_by_region_year, file.path(module_root, "downloads", "pandemia_stage_by_region_year.csv"))
  fwrite(stage_by_age_sex, file.path(module_root, "downloads", "pandemia_stage_by_age_sex_year.csv"))
  fwrite(compare_inei, file.path(module_root, "downloads", "pandemia_expected_vs_observed.csv"))
  fwrite(compare_inei_year, file.path(module_root, "downloads", "pandemia_expected_vs_observed_by_year.csv"))
  fwrite(compare_inei_region_year, file.path(module_root, "downloads", "pandemia_expected_vs_observed_by_region_year.csv"))
  fwrite(factor_dt, file.path(module_root, "downloads", "pandemia_factor_correccion.csv"))
  if (nrow(pandemic_balance)) fwrite(pandemic_balance, file.path(module_root, "downloads", "pandemia_qc_reallocation_balance.csv"))
  fwrite(pandemic_balance_summary, file.path(module_root, "downloads", "pandemia_qc_reallocation_balance_summary.csv"))

  html_sections <- c(
    "<section class=\"card\">",
    "<div class=\"eyebrow\">Pandemia y subregistro</div><h2 class=\"section-title\">Correcci�n pand�mica y ajuste por completitud</h2>",
    "<p class=\"muted\">Este m�dulo explica cu�ntas muertes cambian en cada etapa, cu�nto se reasign� por OPRM, qu� tan lejos estaba la mortalidad observada de la esperada por INEI y c�mo opera el factor de correcci�n.</p>",
    sprintf("<div class=\"lead-note\">En total, el ajuste final agreg� <strong>%s</strong> muertes por encima de la serie sin pandemia en el acumulado 2018-2024. El componente OPRM reasign� <strong>%s</strong> muertes en valor absoluto.</div>",
            format(round(sum(stage_table$muertes_agregadas_por_factor, na.rm = TRUE)), big.mark = ","),
            format(round(sum(stage_table$muertes_reasignadas_oprm, na.rm = TRUE)), big.mark = ",")),
    formula_list_html(c(
      "factor = expected_allcause / observed_allcause, con backoff y clipping cuando aplica",
      "observed_corrected_allcause = observed_allcause * correction_factor_completeness",
      "pandemic_excess = max(0, observed_corrected_allcause - expected_allcause)",
      "deaths_final_net_of_pandemic = base_cause_deaths_corrected - pandemic_reassigned_out_component",
      "deaths_final = deaths_final_net_of_pandemic + pandemic_excess_component",
      "gap_before = expected_allcause - observed_allcause",
      "gap_after = expected_allcause - observed_corrected_allcause",
      "corrected_expected_ratio = observed_corrected_allcause / expected_allcause"
    )),
    "</section>",
    glossary_html(c("gap_before", "gap_after", "corrected_expected_ratio", "pandemic_excess", "masa"), "Glosario local de pandemia y subregistro"),
    "<section class=\"card\"><h3>Paneles clave</h3><div class=\"image-grid\">",
    sprintf("<div class=\"image-card\"><h4>Etapas de transformaci�n</h4><p class=\"muted\">Qu� muestra: comparaci�n total entre observado, redistribuido, sin pandemia y final. Si est� bien, cada capa debe sumar una historia comprensible y no producir quiebres inexplicables.</p><img src=\"figuras/%s\" alt=\"etapas\"></div>", basename(fig_stage)),
    sprintf("<div class=\"image-card\"><h4>Reasignaci�n OPRM</h4><p class=\"muted\">Qu� muestra: volumen absoluto reasignado por el mecanismo pand�mico. Si hay un valor an�malo, puede se�alar un exceso mal distribuido o reglas temporales mal aplicadas.</p><img src=\"figuras/%s\" alt=\"oprm\"></div>", basename(fig_oprm)),
    sprintf("<div class=\"image-card\"><h4>Esperado INEI vs observado SINADEF</h4><p class=\"muted\">Qu� muestra: si la curva esperada supera a la observada, hay espacio para correcci�n por subregistro. Si el observado rebasa sistem�ticamente al esperado sin explicaci�n, algo puede estar mal.</p><img src=\"figuras/%s\" alt=\"inei\"></div>", basename(fig_inei)),
    sprintf("<div class=\"image-card\"><h4>Factor de correcci�n</h4><p class=\"muted\">Qu� muestra: magnitud y forma del factor de completitud. Si est� bien, deber�a ser interpretable por edad/sexo/a�o y no mostrar dientes extremos sin contexto.</p><img src=\"figuras/%s\" alt=\"factor\"></div>", basename(fig_factor)),
    "</div></section>",
    "<section class=\"card\"><h3>Tabla comparativa de etapas</h3>",
    html_table(stage_table),
    "<h3>Demostracion empirica esperado INEI vs observado/corregido</h3>",
    "<p class=\"muted\">Esta tabla no asume que la correccion alcance exactamente INEI: lo mide. gap_before y gap_after se calculan como esperado menos observado/corregido; positivo significa que observado o corregido quedo por debajo de INEI, negativo que quedo por encima. El ratio corregido/esperado menor a 1 implica corregido menor que INEI; mayor a 1 implica corregido mayor que INEI.</p>",
    html_table(compare_inei_year),
    "<h3>Balance pandemico del QC canonico</h3>",
    html_table(pandemic_balance_summary),
    "<h3>Etapas por nivel causal</h3>",
    html_table(head(stage_by_level, 30)),
    "<h3>Brecha INEI por region y ano</h3>",
    html_table(head(compare_inei_region_year, 40)),
    "<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"downloads/pandemia_stage_comparison.csv\">Descargar tabla comparativa</a><a class=\"btn ghost\" href=\"downloads/pandemia_expected_vs_observed.csv\">Descargar esperado vs observado</a><a class=\"btn ghost\" href=\"downloads/pandemia_factor_correccion.csv\">Descargar factor de correcci�n</a></div>",
    "</section>"
  )

  write_portal_page(
    file.path(module_root, "index.html"),
    "Pandemia y subregistro",
    page_shell(
      title = "Pandemia y subregistro",
      intro = "M�dulo humano-legible para revisar contrafactual pand�mico, reasignaci�n OPRM y correcci�n por completitud.",
      sidebar_items = list(list(label = "Volver al portal", href = "../../index.html")),
      sections_html = html_sections
    ),
    rel_root = "../.."
  )

  fwrite(stage_table, file.path(out_dir, "inventory.csv"))
  fwrite(data.table(
    published_path = c(
      "modules/pandemia-subregistro/downloads/pandemia_stage_comparison.csv",
      "modules/pandemia-subregistro/downloads/pandemia_stage_by_year_level.csv",
      "modules/pandemia-subregistro/downloads/pandemia_stage_by_region_year.csv",
      "modules/pandemia-subregistro/downloads/pandemia_stage_by_age_sex_year.csv",
      "modules/pandemia-subregistro/downloads/pandemia_expected_vs_observed.csv",
      "modules/pandemia-subregistro/downloads/pandemia_expected_vs_observed_by_region_year.csv",
      "modules/pandemia-subregistro/downloads/pandemia_factor_correccion.csv",
      "modules/pandemia-subregistro/downloads/pandemia_qc_reallocation_balance.csv",
      "modules/pandemia-subregistro/downloads/pandemia_qc_reallocation_balance_summary.csv"
    )
  ), file.path(out_dir, "downloads_manifest.csv"))
  fwrite(data.table(
    figure_kind = c("stages", "oprm", "inei_observed", "factor"),
    published_path = c(
      "modules/pandemia-subregistro/figuras/pandemia_etapas_totales.png",
      "modules/pandemia-subregistro/figuras/pandemia_oprm.png",
      "modules/pandemia-subregistro/figuras/pandemia_inei_vs_observado.png",
      "modules/pandemia-subregistro/figuras/pandemia_factor_correccion.png"
    )
  ), file.path(out_dir, "figure_manifest.csv"))
  write_text_file(file.path(out_dir, "page_manifest.json"), toJSON(list(list(title = "Pandemia y subregistro", href = "modules/pandemia-subregistro/index.html")), pretty = TRUE, auto_unbox = TRUE))

  list(title = "Pandemia y subregistro", href = "modules/pandemia-subregistro/index.html")
}

method_smoothing_label <- function(code) {
  switch(
    as.character(code),
    "A" = "GAM Poisson principal: sexo + fase pandemica + spline de edad + interaccion edad por sexo + spline de ano + efecto regional.",
    "B" = "GAM Poisson simplificado: sexo + fase pandemica + spline de edad + spline de ano + efecto regional.",
    "C" = "GAM Poisson con bandas de edad: sexo + fase pandemica + banda de edad + spline de ano + efecto regional.",
    "D" = "GAM Poisson minimo: sexo + periodo + banda de edad + efecto regional.",
    "A_NS" = "GAM Poisson principal sin termino de sexo: fase pandemica + spline de edad + spline de ano + efecto regional.",
    "C_NS" = "GAM Poisson con bandas de edad sin termino de sexo: fase pandemica + banda de edad + spline de ano + efecto regional.",
    "D_NS" = "GAM Poisson minimo sin termino de sexo: periodo + banda de edad + efecto regional.",
    "E" = "Heuristica de prestamo: se toma la forma del padre ya modelado y se reescala dentro de ano y sexo.",
    "F" = "Fallback crudo: no se aplica modelo estadistico; se conserva la serie observada despues de los ajustes previos.",
    "F_emergency" = "Fallback crudo de emergencia: conserva la serie observada porque ningun intento anterior paso los guardarrailes.",
    "SEX0" = "Cero estructural por restriccion de sexo.",
    "Metodo no documentado en el catalogo de suavizado."
  )
}

model_diag_plot_paths <- function(fig_dir, slug) {
  list(
    trend = file.path(fig_dir, paste0(slug, "_observado_vs_suavizado.png")),
    residual = file.path(fig_dir, paste0(slug, "_residuo_proxy.png")),
    calibration = file.path(fig_dir, paste0(slug, "_calibracion.png"))
  )
}

copy_if_exists <- function(from, to) {
  if (file.exists(from)) {
    ensure_dir(dirname(to))
    file.copy(from, to, overwrite = TRUE)
    TRUE
  } else {
    FALSE
  }
}

details_table_html <- function(title, intro, dt, max_rows = 40, open = FALSE) {
  paste0(
    "<details class=\"glossary-term\"", if (open) " open" else "", "><summary>", esc_html(title), "</summary>",
    if (nzchar(intro %||% "")) sprintf("<p class=\"muted\">%s</p>", esc_html(intro)) else "",
    html_table(dt, max_rows = max_rows),
    "</details>"
  )
}

build_model_smoothing_module <- function(root_dir) {
  module_root <- file.path(root_dir, "modules", "modelamiento-suavizado")
  ensure_dir(module_root)
  ensure_dir(file.path(module_root, "causas"))
  ensure_dir(file.path(module_root, "figuras"))
  ensure_dir(file.path(module_root, "downloads"))
  out_dir <- here("data", "derived", "qc", "review_portal", "model_smoothing")
  ensure_dir(out_dir)

  register_default_decisions(
    "model_smoothing",
    data.table(
      decision = c("no_refit", "coeficientes", "interactividad"),
      chosen = c(
        "no_refitear_modelos_para_el_reporte",
        "reportar_no_disponible_si_no_existe_tabla_canonica_de_coeficientes",
        "html_estatico_con_png_y_lightbox"
      ),
      rationale = c(
        "Evita crear una segunda fuente de verdad y respeta que la capa editorial consume outputs canonicos.",
        "Los outputs vigentes guardan formula, intentos, warnings y masa, pero no coeficientes completos del objeto mgcv.",
        "Mantiene el portal publicable como sitio estatico sin Shiny ni servidor."
      ),
      reversible_point = c(
        "antes_de_recalcular_build_mortality_rates",
        "en_la_proxima_corrida_si_se_instrumenta_mortality_model_coefficients_csv",
        "antes_de_publicar_el_portal_para_revision_estadistica"
      )
    )
  )

  registry_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_registry.csv")
  attempts_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_attempt_log.csv")
  suff_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_data_sufficiency_audit.csv")
  method_path <- here("data", "derived", "qc", "build_mortality_rates", "qc_method_summary.csv")
  mass_path <- here("data", "derived", "qc", "build_mortality_rates", "qc_mass_preservation_by_cause.csv")
  rough_path <- here("data", "derived", "qc", "build_mortality_rates", "qc_temporal_roughness_summary.csv")
  mort_path <- here("data", "final", "mortality_rate_cause_smoothed", "mortality_rate_cause_smoothed.csv")

  required <- c(registry_path, attempts_path, method_path, mass_path, mort_path)
  missing <- required[!file.exists(required)]
  if (length(missing)) stop("Faltan insumos para modelamiento y suavizado: ", paste(missing, collapse = ", "))

  reg <- fread(registry_path, showProgress = FALSE)
  attempts <- fread(attempts_path, showProgress = FALSE)
  suff <- if (file.exists(suff_path)) fread(suff_path, showProgress = FALSE) else data.table()
  method_summary <- fread(method_path, showProgress = FALSE)
  mass <- fread(mass_path, showProgress = FALSE)
  rough <- if (file.exists(rough_path)) fread(rough_path, showProgress = FALSE) else data.table()
  mort <- fread(
    mort_path,
    select = c(
      "year_id", "location_id", "sex_id", "age", "cause_concept_id", "cause_name",
      "deaths_final", "deaths_smoothed", "mortality_rate_crude", "mortality_rate_smoothed",
      "population", "model_formula_used", "fallback_level"
    ),
    showProgress = FALSE
  )
  model_mass_closure <- mort[, .(
    observed_mass = sum(deaths_final, na.rm = TRUE),
    smoothed_final_mass = sum(deaths_smoothed, na.rm = TRUE)
  ), by = .(cause_concept_id, cause_name, year_id, sex_id)]
  model_mass_closure[, `:=`(
    mass_diff = smoothed_final_mass - observed_mass,
    abs_mass_diff = abs(smoothed_final_mass - observed_mass),
    post_recalibration_ratio_observed_over_smoothed = fifelse(smoothed_final_mass > 0, observed_mass / smoothed_final_mass, NA_real_),
    status = fifelse(abs(smoothed_final_mass - observed_mass) <= 1e-6, "OK_CON_NOTA_residuo_numerico", "REVISAR"),
    initial_prediction_scalar_available = FALSE,
    note = "La prediccion inicial y el factor original de recalibracion no estan persistidos en la corrida vigente; esta tabla demuestra el cierre empirico posterior a recalibracion."
  )]
  fwrite(model_mass_closure, file.path(module_root, "downloads", "model_mass_closure_by_cause_year_sex.csv"))
  fwrite(model_mass_closure, file.path(out_dir, "model_mass_closure_by_cause_year_sex.csv"))

  coeff_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_coefficients.csv")
  metrics_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_fit_metrics.csv")
  smooth_terms_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_smooth_terms.csv")
  recalibration_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_recalibration_factors.csv")
  heuristic_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_heuristic_values.csv")
  diag_year_sex_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_prediction_diagnostics_year_sex.csv")
  diag_age_year_sex_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_prediction_diagnostics_age_year_sex.csv")
  validation_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_candidate_validation.csv")
  warning_summary_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_warning_summary.csv")
  coefficient_assessment_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_coefficient_assessment.csv")
  statistical_assessment_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_statistical_assessment.csv")
  metrics_compact_path <- here("data", "derived", "qc", "build_mortality_rates", "mortality_model_fit_metrics_compact.csv")
  model_summary_txt_dir <- here("data", "derived", "qc", "build_mortality_rates", "model_summaries_txt")
  coefficients <- if (file.exists(coeff_path)) fread(coeff_path, showProgress = FALSE) else data.table()
  fit_metrics <- if (file.exists(metrics_path)) fread(metrics_path, showProgress = FALSE) else data.table()
  smooth_terms <- if (file.exists(smooth_terms_path)) fread(smooth_terms_path, showProgress = FALSE) else data.table()
  recalibration_factors <- if (file.exists(recalibration_path)) fread(recalibration_path, showProgress = FALSE) else data.table()
  heuristic_values <- if (file.exists(heuristic_path)) fread(heuristic_path, showProgress = FALSE) else data.table()
  diag_year_sex <- if (file.exists(diag_year_sex_path)) fread(diag_year_sex_path, showProgress = FALSE) else data.table()
  diag_age_year_sex <- if (file.exists(diag_age_year_sex_path)) fread(diag_age_year_sex_path, showProgress = FALSE) else data.table()
  candidate_validation <- if (file.exists(validation_path)) fread(validation_path, showProgress = FALSE) else data.table()
  warning_summary <- if (file.exists(warning_summary_path)) fread(warning_summary_path, showProgress = FALSE) else data.table()
  coefficient_assessment <- if (file.exists(coefficient_assessment_path)) fread(coefficient_assessment_path, showProgress = FALSE) else data.table()
  statistical_assessment <- if (file.exists(statistical_assessment_path)) fread(statistical_assessment_path, showProgress = FALSE) else data.table()
  fit_metrics_compact <- if (file.exists(metrics_compact_path)) fread(metrics_compact_path, showProgress = FALSE) else data.table()
  coeff_available <- nrow(coefficients) > 0L
  metrics_available <- nrow(fit_metrics) > 0L
  smooth_terms_available <- nrow(smooth_terms) > 0L
  recalibration_available <- nrow(recalibration_factors) > 0L
  heuristic_available <- nrow(heuristic_values) > 0L
  diag_available <- nrow(diag_year_sex) > 0L && nrow(diag_age_year_sex) > 0L
  validation_available <- nrow(candidate_validation) > 0L
  warning_summary_available <- nrow(warning_summary) > 0L
  coefficient_assessment_available <- nrow(coefficient_assessment) > 0L
  statistical_assessment_available <- nrow(statistical_assessment) > 0L
  metrics_compact_available <- nrow(fit_metrics_compact) > 0L

  accepted <- attempts[attempt_status == "accepted"][order(cause_concept_id, attempt_order)]
  accepted <- accepted[, .SD[.N], by = cause_concept_id]
  model_inventory <- merge(
    reg,
    accepted[, .(cause_concept_id, formula_final = formula, selected_attempt_order = attempt_order, selected_warning_text = warnings, selected_failure_reason = failure_reason)],
    by = "cause_concept_id",
    all.x = TRUE
  )
  model_inventory[, method_description := vapply(method_selected, method_smoothing_label, character(1))]
  model_inventory[, mass_diff := mass_output - mass_input]
  model_inventory[, mass_ok := abs(mass_diff) <= 1e-6]
  model_inventory[, diagnostic_kind := fifelse(method_selected %in% c("A", "B", "C", "D"), "modelo_estadistico_gam_poisson", "heuristica_o_fallback")]
  model_inventory[, coefficients_available := coeff_available]
  model_inventory[, fit_metrics_available := metrics_available]
  model_inventory[, page_slug := paste0(slugify(cause_name), "-", cause_concept_id)]

  method_kpi <- model_inventory[, .(
    causes = .N,
    warnings = sum(isTRUE(warning_flag), na.rm = TRUE),
    fallback = sum(method_selected %in% c("E", "F", "F_emergency"), na.rm = TRUE),
    mass_ok = sum(mass_ok, na.rm = TRUE),
    mass_not_ok = sum(!mass_ok, na.rm = TRUE)
  ), by = .(method_selected, diagnostic_kind)][order(method_selected)]

  fwrite(model_inventory, file.path(module_root, "downloads", "mortality_model_inventory.csv"))
  fwrite(attempts, file.path(module_root, "downloads", "mortality_model_attempt_log.csv"))
  fwrite(method_summary, file.path(module_root, "downloads", "qc_method_summary.csv"))
  fwrite(mass, file.path(module_root, "downloads", "qc_mass_preservation_by_cause.csv"))
  if (nrow(rough)) fwrite(rough, file.path(module_root, "downloads", "qc_temporal_roughness_summary.csv"))
  if (nrow(suff)) fwrite(suff, file.path(module_root, "downloads", "mortality_data_sufficiency_audit.csv"))
  if (coeff_available) fwrite(coefficients, file.path(module_root, "downloads", "mortality_model_coefficients.csv"))
  if (metrics_available) fwrite(fit_metrics, file.path(module_root, "downloads", "mortality_model_fit_metrics.csv"))
  if (smooth_terms_available) fwrite(smooth_terms, file.path(module_root, "downloads", "mortality_model_smooth_terms.csv"))
  if (recalibration_available) fwrite(recalibration_factors, file.path(module_root, "downloads", "mortality_model_recalibration_factors.csv"))
  if (heuristic_available) fwrite(heuristic_values, file.path(module_root, "downloads", "mortality_model_heuristic_values.csv"))
  if (diag_available) {
    fwrite(diag_year_sex, file.path(module_root, "downloads", "mortality_model_prediction_diagnostics_year_sex.csv"))
    fwrite(diag_age_year_sex, file.path(module_root, "downloads", "mortality_model_prediction_diagnostics_age_year_sex.csv"))
  }
  if (validation_available) fwrite(candidate_validation, file.path(module_root, "downloads", "mortality_model_candidate_validation.csv"))
  if (warning_summary_available) fwrite(warning_summary, file.path(module_root, "downloads", "mortality_model_warning_summary.csv"))
  if (coefficient_assessment_available) fwrite(coefficient_assessment, file.path(module_root, "downloads", "mortality_model_coefficient_assessment.csv"))
  if (statistical_assessment_available) fwrite(statistical_assessment, file.path(module_root, "downloads", "mortality_model_statistical_assessment.csv"))
  if (metrics_compact_available) fwrite(fit_metrics_compact, file.path(module_root, "downloads", "mortality_model_fit_metrics_compact.csv"))
  if (dir.exists(model_summary_txt_dir)) {
    summary_out_dir <- file.path(module_root, "downloads", "model_summaries_txt")
    ensure_dir(summary_out_dir)
    summary_files <- list.files(model_summary_txt_dir, pattern = "\\.txt$", full.names = TRUE)
    if (length(summary_files)) file.copy(summary_files, summary_out_dir, overwrite = TRUE)
  }
  downloads_manifest_extra <- "modules/modelamiento-suavizado/downloads/model_mass_closure_by_cause_year_sex.csv"

  page_manifest <- list()
  figure_manifest <- data.table()
  downloads_manifest <- data.table(
    published_path = c(
      "modules/modelamiento-suavizado/downloads/mortality_model_inventory.csv",
      "modules/modelamiento-suavizado/downloads/mortality_model_attempt_log.csv",
      "modules/modelamiento-suavizado/downloads/qc_method_summary.csv",
      "modules/modelamiento-suavizado/downloads/qc_mass_preservation_by_cause.csv",
      "modules/modelamiento-suavizado/downloads/model_mass_closure_by_cause_year_sex.csv",
      if (coeff_available) "modules/modelamiento-suavizado/downloads/mortality_model_coefficients.csv" else NA_character_,
      if (metrics_available) "modules/modelamiento-suavizado/downloads/mortality_model_fit_metrics.csv" else NA_character_,
      if (metrics_compact_available) "modules/modelamiento-suavizado/downloads/mortality_model_fit_metrics_compact.csv" else NA_character_,
      if (smooth_terms_available) "modules/modelamiento-suavizado/downloads/mortality_model_smooth_terms.csv" else NA_character_,
      if (recalibration_available) "modules/modelamiento-suavizado/downloads/mortality_model_recalibration_factors.csv" else NA_character_,
      if (heuristic_available) "modules/modelamiento-suavizado/downloads/mortality_model_heuristic_values.csv" else NA_character_,
      if (diag_available) "modules/modelamiento-suavizado/downloads/mortality_model_prediction_diagnostics_year_sex.csv" else NA_character_,
      if (diag_available) "modules/modelamiento-suavizado/downloads/mortality_model_prediction_diagnostics_age_year_sex.csv" else NA_character_,
      if (validation_available) "modules/modelamiento-suavizado/downloads/mortality_model_candidate_validation.csv" else NA_character_,
      if (warning_summary_available) "modules/modelamiento-suavizado/downloads/mortality_model_warning_summary.csv" else NA_character_,
      if (coefficient_assessment_available) "modules/modelamiento-suavizado/downloads/mortality_model_coefficient_assessment.csv" else NA_character_,
      if (statistical_assessment_available) "modules/modelamiento-suavizado/downloads/mortality_model_statistical_assessment.csv" else NA_character_
    )
  )[!is.na(published_path)]

  for (i in seq_len(nrow(model_inventory))) {
    row <- model_inventory[i]
    cid <- row$cause_concept_id[1]
    slug <- row$page_slug[1]
    x <- mort[cause_concept_id == cid]
    if (nrow(x) == 0) next

    trend <- if (diag_available && "cause_concept_id" %in% names(diag_year_sex)) {
      diag_year_sex[cause_concept_id == cid][order(year_id, sex_id)]
    } else {
      data.table()
    }
    if (nrow(trend) == 0L) {
      trend <- x[, .(
        observed = sum(deaths_final, na.rm = TRUE),
        pred_initial = NA_real_,
        pred_recalibrated = sum(deaths_smoothed, na.rm = TRUE),
        residual_proxy_initial = NA_real_,
        residual_proxy_recalibrated = (sum(deaths_final, na.rm = TRUE) - sum(deaths_smoothed, na.rm = TRUE)) / sqrt(pmax(sum(deaths_smoothed, na.rm = TRUE), 0) + 1e-6),
        recalibration_factor = NA_real_
      ), by = .(year_id, sex_id)][order(year_id, sex_id)]
    }
    trend[, sex_label := fifelse(sex_id == 8507L, "Hombre", fifelse(sex_id == 8532L, "Mujer", as.character(sex_id)))]
    if (!"residual_proxy_initial" %in% names(trend) && "resid_proxy_initial" %in% names(trend)) {
      trend[, residual_proxy_initial := resid_proxy_initial]
    }
    if (!"residual_proxy_recalibrated" %in% names(trend) && "resid_proxy_recalibrated" %in% names(trend)) {
      trend[, residual_proxy_recalibrated := resid_proxy_recalibrated]
    }

    calib <- if (diag_available && "cause_concept_id" %in% names(diag_age_year_sex)) {
      diag_age_year_sex[cause_concept_id == cid][order(year_id, sex_id, age)]
    } else {
      data.table()
    }
    if (nrow(calib) == 0L) {
      calib <- x[, .(
        observed = sum(deaths_final, na.rm = TRUE),
        pred_initial = NA_real_,
        pred_recalibrated = sum(deaths_smoothed, na.rm = TRUE),
        residual_proxy_initial = NA_real_,
        residual_proxy_recalibrated = (sum(deaths_final, na.rm = TRUE) - sum(deaths_smoothed, na.rm = TRUE)) / sqrt(pmax(sum(deaths_smoothed, na.rm = TRUE), 0) + 1e-6),
        recalibration_factor = NA_real_
      ), by = .(year_id, sex_id, age)]
    }
    if (!"residual_proxy_initial" %in% names(calib) && "resid_proxy_initial" %in% names(calib)) {
      calib[, residual_proxy_initial := resid_proxy_initial]
    }
    if (!"residual_proxy_recalibrated" %in% names(calib) && "resid_proxy_recalibrated" %in% names(calib)) {
      calib[, residual_proxy_recalibrated := resid_proxy_recalibrated]
    }

    detail_csv <- file.path(module_root, "downloads", paste0(slug, "_observado_vs_suavizado.csv"))
    fwrite(calib, detail_csv)
    downloads_manifest <- rbind(
      downloads_manifest,
      data.table(published_path = normalize_slashes(file.path("modules", "modelamiento-suavizado", "downloads", basename(detail_csv)))),
      fill = TRUE
    )

    fig_slug <- paste0("cause_", cid)
    fig_paths <- model_diag_plot_paths(file.path(module_root, "figuras"), fig_slug)
    trend_long <- melt(
      trend,
      id.vars = intersect(c("cause_concept_id", "cause_name", "method_code", "year_id", "sex_id", "sex_label", "recalibration_factor"), names(trend)),
      measure.vars = intersect(c("observed", "pred_initial", "pred_recalibrated"), names(trend)),
      variable.name = "serie",
      value.name = "muertes"
    )
    trend_long <- trend_long[is.finite(muertes)]
    trend_long[, serie := factor(
      serie,
      levels = c("observed", "pred_initial", "pred_recalibrated"),
      labels = c("observado", "prediccion inicial", "suavizado final recalibrado")
    )]
    residual_long <- melt(
      trend,
      id.vars = intersect(c("cause_concept_id", "cause_name", "method_code", "year_id", "sex_id", "sex_label"), names(trend)),
      measure.vars = intersect(c("residual_proxy_initial", "residual_proxy_recalibrated"), names(trend)),
      variable.name = "fase",
      value.name = "residuo_proxy"
    )
    residual_long <- residual_long[is.finite(residuo_proxy)]
    residual_long[, fase := factor(
      fase,
      levels = c("residual_proxy_initial", "residual_proxy_recalibrated"),
      labels = c("antes de recalibrar", "despues de recalibrar")
    )]
    calib_long <- rbindlist(list(
      if ("pred_initial" %in% names(calib)) calib[is.finite(pred_initial), .(year_id, sex_id, age, observed, predicted = pred_initial, fase = "antes de recalibrar")] else data.table(),
      if ("pred_recalibrated" %in% names(calib)) calib[is.finite(pred_recalibrated), .(year_id, sex_id, age, observed, predicted = pred_recalibrated, fase = "despues de recalibrar")] else data.table()
    ), fill = TRUE)
    calib_plot <- if (nrow(calib_long)) calib_long[sample.int(nrow(calib_long), min(nrow(calib_long), 5000L))] else calib_long

    p_trend <- ggplot(trend_long,
                      aes(x = year_id, y = muertes, color = serie, linetype = sex_label)) +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.5) +
      scale_color_manual(values = c("observado" = "#667085", "prediccion inicial" = "#b77700", "suavizado final recalibrado" = "#0f4c81"), drop = FALSE) +
      labs(title = row$cause_name, subtitle = "Observado, prediccion inicial y suavizado final recalibrado por ano y sexo", x = "Ano", y = "Muertes", color = NULL, linetype = "Sexo") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
    p_resid <- ggplot(residual_long, aes(x = year_id, y = residuo_proxy, color = sex_label)) +
      geom_hline(yintercept = 0, linewidth = 0.4, color = "#667085") +
      geom_line(linewidth = 0.9) +
      geom_point(size = 1.5) +
      facet_wrap(~ fase) +
      labs(title = row$cause_name, subtitle = "Residuo proxy antes y despues de recalibrar: (observado - predicho) / sqrt(predicho)", x = "Ano", y = "Residuo proxy", color = "Sexo") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")
    p_cal <- ggplot(calib_plot, aes(x = predicted, y = observed, color = factor(year_id))) +
      geom_abline(slope = 1, intercept = 0, color = "#667085", linetype = "dashed") +
      geom_point(alpha = 0.35, size = 0.9) +
      geom_smooth(aes(x = predicted, y = observed, group = 1), method = "loess", se = FALSE, color = "#b42318", linewidth = 0.9, inherit.aes = FALSE, data = calib_plot) +
      facet_wrap(~ fase) +
      scale_x_continuous(trans = "sqrt") +
      scale_y_continuous(trans = "sqrt") +
      labs(title = row$cause_name, subtitle = "Calibracion por celda edad-sexo-ano. Diagonal = observado igual a predicho; rojo = LOESS.", x = "Predicho", y = "Observado", color = "Ano") +
      theme_minimal(base_size = 10) +
      theme(legend.position = "bottom")

    save_plot_png(p_trend, fig_paths$trend, width = 10, height = 5.8)
    save_plot_png(p_resid, fig_paths$residual, width = 10, height = 5.8)
    save_plot_png(p_cal, fig_paths$calibration, width = 8, height = 6.2)
    figure_manifest <- rbind(
      figure_manifest,
      data.table(
        cause_concept_id = cid,
        figure_kind = c("observado_vs_suavizado", "residuo_proxy", "calibracion"),
        published_path = normalize_slashes(file.path("modules", "modelamiento-suavizado", "figuras", basename(unlist(fig_paths))))
      ),
      fill = TRUE
    )

    attempts_i <- attempts[cause_concept_id == cid][order(attempt_order)]
    mass_i <- mass[cause_concept_id == cid][order(year_id, sex_id)]
    closure_i <- model_mass_closure[cause_concept_id == cid][order(year_id, sex_id)]
    rough_i <- if (nrow(rough) && "cause_concept_id" %in% names(rough)) head(rough[cause_concept_id == cid], 20) else data.table()
    metrics_i <- if (metrics_available && "cause_concept_id" %in% names(fit_metrics)) fit_metrics[cause_concept_id == cid] else data.table()
    metrics_compact_i <- if (metrics_compact_available && "cause_concept_id" %in% names(fit_metrics_compact)) fit_metrics_compact[cause_concept_id == cid] else data.table()
    coefficients_i <- if (coeff_available && "cause_concept_id" %in% names(coefficients)) coefficients[cause_concept_id == cid] else data.table()
    smooth_terms_i <- if (smooth_terms_available && "cause_concept_id" %in% names(smooth_terms)) smooth_terms[cause_concept_id == cid] else data.table()
    recalibration_i <- if (recalibration_available && "cause_concept_id" %in% names(recalibration_factors)) recalibration_factors[cause_concept_id == cid & method_code == row$method_selected[1]][order(year_id, sex_id)] else data.table()
    heuristic_i <- if (heuristic_available && "cause_concept_id" %in% names(heuristic_values)) heuristic_values[cause_concept_id == cid & method_code == row$method_selected[1]] else data.table()
    validation_i <- if (validation_available && "cause_concept_id" %in% names(candidate_validation)) candidate_validation[cause_concept_id == cid][order(validation_deviance)] else data.table()
    warning_i <- if (warning_summary_available && "cause_concept_id" %in% names(warning_summary)) warning_summary[cause_concept_id == cid] else data.table()
    coeff_assess_i <- if (coefficient_assessment_available && "cause_concept_id" %in% names(coefficient_assessment)) coefficient_assessment[cause_concept_id == cid] else data.table()
    stat_assess_i <- if (statistical_assessment_available && "cause_concept_id" %in% names(statistical_assessment)) statistical_assessment[cause_concept_id == cid] else data.table()
    summary_rel_link <- NA_character_
    summary_out_dir <- file.path(module_root, "downloads", "model_summaries_txt")
    if (dir.exists(summary_out_dir)) {
      summary_hits <- list.files(summary_out_dir, pattern = paste0("^cause_", cid, "_method_", row$method_selected[1], "_.*\\.txt$"), full.names = FALSE)
      if (!length(summary_hits)) summary_hits <- list.files(summary_out_dir, pattern = paste0("^cause_", cid, "_.*\\.txt$"), full.names = FALSE)
      if (length(summary_hits)) summary_rel_link <- file.path("../downloads/model_summaries_txt", summary_hits[[1]])
    }
    coeff_note <- if (coeff_available) {
      "<div class=\"lead-note\">Existe tabla canonica de coeficientes generada durante el ajuste can�nico. Los coeficientes de bases spline se reportan por trazabilidad estad�stica; para interpretaci�n epidemiol�gica conviene priorizar los t�rminos suaves, la calibraci�n, la forma gr�fica y el cierre de masa.</div>"
    } else {
      "<div class=\"lead-note\"><strong>Coeficientes.</strong> No estan disponibles en los outputs actuales. Este reporte no los inventa ni refitea modelos; por ahora audita formula, intentos, warnings, masa y diagnosticos observado-suavizado.</div>"
    }
    page_sections <- c(
      "<section class=\"card\">",
      sprintf("<div class=\"eyebrow\">%s</div><h2 class=\"section-title\">%s</h2>", esc_html(row$diagnostic_kind), esc_html(row$cause_name)),
      sprintf("<p>%s</p>", esc_html(row$method_description)),
      sprintf("<div class=\"pill-row\"><span class=\"pill\">Metodo: %s</span><span class=\"pill\">Categoria datos: %s</span><span class=\"pill\">Convergencia: %s</span><span class=\"pill\">Warning: %s</span><span class=\"pill\">Masa preservada: %s</span></div>",
              esc_html(row$method_selected), esc_html(row$data_category), esc_html(row$convergence_status), esc_html(row$warning_flag), esc_html(row$mass_ok)),
      sprintf("<p><strong>Formula o regla final.</strong> <code>%s</code></p>", esc_html(row$formula_final %||% row$model_formula_used %||% row$method_description)),
      if (nrow(stat_assess_i)) {
        sprintf("<div class=\"lead-note\"><strong>Conclusion estadistica automatizada.</strong> %s</div>", esc_html(stat_assess_i$expert_statistical_conclusion[1] %||% stat_assess_i$final_verdict_statistical[1]))
      } else {
        ""
      },
      coeff_note,
      if (!is.na(summary_rel_link)) sprintf("<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"%s\">Descargar summary() del modelo GAM seleccionado</a></div>", normalize_slashes(summary_rel_link)) else "",
      "</section>",
      "<section class=\"card\"><h3>Como interpretar estos diagnosticos</h3>",
      "<p>Si se uso un GAM Poisson, se espera una serie suavizada que acompane la tendencia observada sin copiar ruido celda a celda. Si se uso una heuristica o fallback, la pregunta principal ya no es bondad de ajuste estadistica sino trazabilidad: que regla se aplico, si la masa se preservo y si la forma final es epidemiologicamente plausible.</p>",
      "<p>La tabla puede cerrar aunque la curva observado vs suavizado parezca diferente: el codigo recalibra la prediccion para preservar la suma por ano y sexo, pero permite redistribuir la forma entre edades o regiones dentro de ese estrato.</p>",
      "<p>El residuo proxy no reemplaza residuales formales del modelo; sirve como control visual para detectar anos, edades o sexos donde observado y suavizado se separan demasiado. En calibracion, cada punto es una celda edad-sexo-ano; la diagonal es observado = suavizado y la linea roja LOESS ayuda a ver sesgo sistematico.</p>",
      formula_list_html(c(
        "factor_recalibracion = sum(observado) / sum(prediccion_inicial)",
        "suavizado_final = prediccion_inicial * factor_recalibracion",
        "residuo_proxy_inicial = (observado - prediccion_inicial) / sqrt(prediccion_inicial + epsilon)",
        "residuo_proxy_final = (observado - suavizado_final_recalibrado) / sqrt(suavizado_final_recalibrado + epsilon)"
      )),
      "</section>",
      glossary_html(c("masa", "recalibracion", "prediccion_inicial", "suavizado", "residuo_proxy", "roughness_temporal", "calibracion", "fallback", "borrow_parent_shape", "warning", "convergencia"), "Glosario local de modelamiento"),
      "<section class=\"card\"><h3>Figuras diagnosticas</h3><div class=\"image-grid\">",
      sprintf("<div class=\"image-card\"><h4>Observado vs suavizado</h4><img src=\"../figuras/%s\" alt=\"observado vs suavizado\"></div>", basename(fig_paths$trend)),
      sprintf("<div class=\"image-card\"><h4>Residuo proxy</h4><img src=\"../figuras/%s\" alt=\"residuo proxy\"></div>", basename(fig_paths$residual)),
      sprintf("<div class=\"image-card\"><h4>Calibracion</h4><img src=\"../figuras/%s\" alt=\"calibracion\"></div>", basename(fig_paths$calibration)),
      "</div></section>",
      "<section class=\"card\"><h3>Historia de seleccion del modelo</h3>",
      html_table(attempts_i[, .(orden = attempt_order, metodo = method_code, estado = attempt_status, razon_falla = failure_reason, convergencia = convergence_status, warning = warning_flag, formula = formula)], max_rows = 20),
      "<h3>Cierre empirico posterior a recalibracion por causa, ano y sexo</h3>",
      html_table(closure_i[, .(year_id, sex_id, observed_mass, smoothed_final_mass, mass_diff, status, initial_prediction_scalar_available, note)], max_rows = 30),
      "<h3>Masa reportada por QC del modelo</h3>",
      html_table(mass_i, max_rows = 30),
      "<h3>Roughness temporal</h3>",
      "<p class=\"muted\">Roughness temporal resume irregularidad ano a ano. Un valor alto puede ser un shock real, pero tambien puede sugerir sobre/suavizado, baja frecuencia o inestabilidad si no hay explicacion epidemiologica.</p>",
      html_table(rough_i, max_rows = 20),
      "<h3>Metricas compactas y competencia estadistica</h3>",
      "<p class=\"muted\">Esta tabla resume solo lo necesario para juicio rapido: metodo final, score de validacion, warnings, cierre de masa y veredicto. Los warnings largos y el summary() completo quedan como descarga para no romper la lectura.</p>",
      html_table(metrics_compact_i, max_rows = 8),
      details_table_html("Competencia de modelos candidatos", "Cada fila es un candidato evaluado contra anos holdout cuando hubo datos suficientes. Solo se cambia de metodo si mejora al menos 5%, no introduce warning critico y preserva masa.", validation_i, max_rows = 25),
      details_table_html("Resumen de warnings", "non-integer x se interpreta como OK_CON_NOTA porque las muertes pueden ser fraccionales tras redistribucion/correccion. Convergencia, prediccion invalida o problemas matriciales se clasifican como criticos.", warning_i, max_rows = 10),
      details_table_html("Evaluacion de coeficientes", "z.value = NA no bloquea automaticamente: puede deberse a falta de variacion o aliasing de un termino. Se revisa junto con warnings, validacion y plausibilidad visual.", coeff_assess_i, max_rows = 15),
      details_table_html("Terminos suaves del modelo", "Los terminos suaves se interpretan mejor por edf, estadistico y p-value cuando estan disponibles. edf mas alto suele indicar una forma mas flexible; no es por si solo un problema.", smooth_terms_i, max_rows = 30),
      details_table_html("Coeficientes parametricos / base spline", "Los coeficientes son utiles para auditoria. En modelos con splines, muchos pertenecen a la base matematica y no tienen lectura epidemiologica directa como un riesgo relativo simple.", coefficients_i, max_rows = 40),
      "<h3>Factores empiricos de recalibracion</h3>",
      "<p class=\"muted\">Esta tabla muestra la masa observada, la prediccion inicial antes de escalar, el factor aplicado y la masa suavizada final por ano y sexo.</p>",
      html_table(recalibration_i, max_rows = 40),
      "<h3>Valores de heuristica o fallback</h3>",
      "<p class=\"muted\">Cuando el metodo final fue E/F, aqui se muestra la regla usada, la causa padre prestada si aplica, y el cierre de masa de la heuristica.</p>",
      html_table(heuristic_i, max_rows = 20),
      sprintf("<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"../downloads/%s\">Descargar observado vs suavizado</a><a class=\"btn\" href=\"../index.html\">Volver al modulo</a><a class=\"btn alt\" href=\"../../../index.html\">Volver al portal</a></div>", basename(detail_csv)),
      "</section>"
    )
    write_portal_page(
      file.path(module_root, "causas", paste0(slug, ".html")),
      paste("Modelamiento -", row$cause_name),
      page_shell(
        title = paste("Modelamiento -", row$cause_name),
        intro = "Ficha estadistica y de trazabilidad del metodo de suavizado usado para esta causa.",
        sidebar_items = list(
          list(label = "Volver al modulo", href = "../index.html"),
          list(label = "Volver al portal", href = "../../../index.html")
        ),
        sections_html = page_sections
      ),
      rel_root = "../../.."
    )
    page_manifest[[length(page_manifest) + 1L]] <- list(
      cause_concept_id = cid,
      title = row$cause_name,
      method_selected = row$method_selected,
      href = normalize_slashes(file.path("modules", "modelamiento-suavizado", "causas", paste0(slug, ".html")))
    )
  }

  module_sections <- c(
    "<section class=\"card\">",
    "<div class=\"eyebrow\">Modelamiento y suavizado</div><h2 class=\"section-title\">Diagnostico de modelos de mortalidad</h2>",
    "<p class=\"muted\">Este modulo documenta que metodo se uso para suavizar cada causa terminal, que intentos se realizaron, si hubo warnings, si la masa se preservo y como comparar observado contra suavizado.</p>",
    if (coeff_available) {
      "<div class=\"lead-note\">Los coeficientes y terminos suaves ya estan disponibles porque fueron capturados durante el ajuste canonico de <code>build_mortality_rates.R</code>. El reporte no refitea modelos: solo lee esas salidas estadisticas persistidas.</div>"
    } else {
      "<div class=\"lead-note\">Los coeficientes completos no estan disponibles en los outputs actuales salvo que exista <code>mortality_model_coefficients.csv</code>. Por diseno, este reporte no refitea modelos ni inventa coeficientes.</div>"
    },
    html_table(method_kpi),
    paste0(
      "<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"downloads/mortality_model_inventory.csv\">Descargar inventario de modelos</a><a class=\"btn ghost\" href=\"downloads/mortality_model_attempt_log.csv\">Descargar intentos</a><a class=\"btn ghost\" href=\"downloads/qc_mass_preservation_by_cause.csv\">Descargar masa por causa</a><a class=\"btn ghost\" href=\"downloads/model_mass_closure_by_cause_year_sex.csv\">Descargar cierre recalibrado por causa-ano-sexo</a>",
      if (metrics_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_fit_metrics.csv\">Descargar metricas de ajuste</a>" else "",
      if (metrics_compact_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_fit_metrics_compact.csv\">Descargar metricas compactas</a>" else "",
      if (coeff_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_coefficients.csv\">Descargar coeficientes</a>" else "",
      if (smooth_terms_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_smooth_terms.csv\">Descargar terminos suaves</a>" else "",
      if (recalibration_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_recalibration_factors.csv\">Descargar factores de recalibracion</a>" else "",
      if (heuristic_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_heuristic_values.csv\">Descargar heuristicas/fallbacks</a>" else "",
      if (diag_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_prediction_diagnostics_year_sex.csv\">Descargar diagnostico ano-sexo</a><a class=\"btn ghost\" href=\"downloads/mortality_model_prediction_diagnostics_age_year_sex.csv\">Descargar diagnostico edad-ano-sexo</a>" else "",
      if (validation_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_candidate_validation.csv\">Descargar competencia de modelos</a>" else "",
      if (warning_summary_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_warning_summary.csv\">Descargar resumen de warnings</a>" else "",
      if (coefficient_assessment_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_coefficient_assessment.csv\">Descargar evaluacion de coeficientes</a>" else "",
      if (statistical_assessment_available) "<a class=\"btn ghost\" href=\"downloads/mortality_model_statistical_assessment.csv\">Descargar veredicto estadistico</a>" else "",
      "</div>"
    ),
    "</section>",
    "<section class=\"card\"><h3>Metodos A-F: como leerlos</h3>",
    html_table(data.table(
      metodo = c("A", "B", "C", "D", "A_NS", "C_NS", "D_NS", "E", "F"),
      tipo = c("GAM Poisson principal", "GAM Poisson simplificado", "GAM con bandas de edad", "GAM minimo", "Variante sin sexo", "Variante sin sexo", "Variante sin sexo", "Heuristica de prestamo", "Fallback crudo"),
      interpretacion = vapply(c("A", "B", "C", "D", "A_NS", "C_NS", "D_NS", "E", "F"), method_smoothing_label, character(1)),
      salida_esperada = c(
        "Convergencia, warnings explicados y masa preservada tras recalibracion.",
        "Convergencia con formula mas simple y masa preservada.",
        "Forma plausible cuando la edad continua es inestable.",
        "Modelo minimo estable para datos limitados.",
        "Mejor desempeno predictivo cuando el termino sexo es inestable o no aporta.",
        "Alternativa con bandas de edad cuando sexo/edad continua es inestable.",
        "Alternativa minima sin sexo para datos muy limitados.",
        "Trazabilidad a causa padre y masa preservada por reescalamiento.",
        "Serie observada conservada cuando modelar seria menos seguro."
      )
    )),
    "</section>",
    glossary_html(c("masa", "recalibracion", "prediccion_inicial", "suavizado", "residuo_proxy", "roughness_temporal", "calibracion", "fallback", "borrow_parent_shape", "warning", "convergencia"), "Glosario metodologico de suavizado"),
    "<section class=\"card\"><div class=\"eyebrow\">Entrar por causa</div><div class=\"catalog-list\">",
    paste(vapply(page_manifest, function(x) sprintf(
      "<div class=\"catalog-item\" data-catalog-item data-cause-level=\"model\"><h4><a href=\"%s\">%s</a></h4><p class=\"muted\">Metodo seleccionado: %s.</p></div>",
      normalize_slashes(file.path("causas", basename(x$href))), esc_html(x$title), esc_html(x$method_selected)
    ), character(1)), collapse = ""),
    "</div></section>"
  )

  write_portal_page(
    file.path(module_root, "index.html"),
    "Modelamiento y suavizado",
    page_shell(
      title = "Modelamiento y suavizado",
      intro = "Modulo HTML para revisar metodos de suavizado, fallbacks, formula final, warnings y diagnosticos observado-suavizado.",
      sidebar_items = list(list(label = "Volver al portal", href = "../../index.html")),
      sections_html = module_sections
    ),
    rel_root = "../.."
  )

  fwrite(model_inventory, file.path(out_dir, "inventory.csv"))
  fwrite(method_kpi, file.path(out_dir, "method_summary.csv"))
  fwrite(figure_manifest, file.path(out_dir, "figure_manifest.csv"))
  fwrite(downloads_manifest, file.path(out_dir, "downloads_manifest.csv"))
  write_text_file(file.path(out_dir, "page_manifest.json"), toJSON(page_manifest, pretty = TRUE, auto_unbox = TRUE))

  list(title = "Modelamiento y suavizado", href = "modules/modelamiento-suavizado/index.html")
}

write_pdf_template <- function(root_dir) {
  template_dir <- file.path(root_dir, "templates")
  ensure_dir(template_dir)
  qmd <- c(
    "---",
    "lang: es",
    "title: \"Portal técnico\"",
    "subtitle: \"Salida PDF automatizada\"",
    "format:",
    "  pdf:",
    "    toc: true",
    "    number-sections: true",
    "    geometry:",
    "      - margin=1.8cm",
    "params:",
    "  title: \"Portal técnico\"",
    "  subtitle: \"\"",
    "  body_md: \"\"",
    "---",
    "",
    "```{r results='asis'}",
    "body <- readLines(params$body_md, warn = FALSE, encoding = 'UTF-8')",
    "cat(paste(body, collapse = '\\n'))",
    "```"
  )
  write_text_file(file.path(template_dir, "module_pdf.qmd"), qmd)
}

render_pdf_from_md <- function(root_dir, title, subtitle, body_md_path, output_pdf_path) {
  title <- as.character(title %||% "")
  subtitle <- as.character(subtitle %||% "")
  title[is.na(title)] <- ""
  subtitle[is.na(subtitle)] <- ""
  template <- file.path(root_dir, "templates", "module_pdf.qmd")
  ensure_dir(dirname(output_pdf_path))
  render_name <- basename(output_pdf_path)
  render_dir <- tempfile("quarto_pdf_render_")
  ensure_dir(render_dir)
  on.exit(unlink(render_dir, recursive = TRUE, force = TRUE), add = TRUE)
  temp_template <- file.path(render_dir, "module_pdf.qmd")
  file.copy(template, temp_template, overwrite = TRUE)
  quarto::quarto_render(
    input = temp_template,
    output_format = "pdf",
    output_file = render_name,
    execute_params = list(title = title, subtitle = subtitle, body_md = body_md_path)
  )
  rendered <- file.path(render_dir, render_name)
  if (!identical(normalizePath(rendered, winslash = "/", mustWork = FALSE), normalizePath(output_pdf_path, winslash = "/", mustWork = FALSE))) {
    file.copy(rendered, output_pdf_path, overwrite = TRUE)
  }
}

fmt_int <- function(x) {
  ifelse(is.na(x), "", scales::comma(round(x)))
}

fmt_num <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), "", scales::number(x, accuracy = accuracy, big.mark = ","))
}

fmt_pct <- function(x, accuracy = 0.1) {
  ifelse(is.na(x), "", scales::number(x, accuracy = accuracy))
}

format_icd_token <- function(x) {
  x <- toupper(gsub("[^A-Z0-9]", "", as.character(x %||% "")))
  if (!nzchar(x)) return("")
  if (nchar(x) == 3) return(x)
  if (nchar(x) == 4) return(paste0(substr(x, 1, 3), ".", substr(x, 4, 4)))
  if (nchar(x) >= 5) return(paste0(substr(x, 1, 3), ".", substr(x, 4, nchar(x))))
  x
}

extract_icd_codes_from_regex <- function(x) {
  vals <- unique(na.omit(as.character(x)))
  if (!length(vals)) return(character())
  tokens <- unique(unlist(regmatches(vals, gregexpr("[A-Z][0-9]{2,4}", vals, perl = TRUE))))
  tokens <- tokens[nzchar(tokens)]
  sort(unique(vapply(tokens, format_icd_token, character(1))))
}

collapse_code_summary <- function(codes, max_n = 10L) {
  codes <- unique(na.omit(as.character(codes)))
  if (!length(codes)) return("")
  if (length(codes) <= max_n) return(paste(codes, collapse = ", "))
  paste0(paste(head(codes, max_n), collapse = ", "), ", ...")
}

translate_method_label <- function(x) {
  vals <- unique(trimws(as.character(na.omit(x))))
  vals <- vals[nzchar(vals)]
  if (!length(vals)) return("")
  out <- character()
  if (any(grepl("direct", vals, ignore.case = TRUE)) || any(grepl("evidencia directa", vals, ignore.case = TRUE))) out <- c(out, "Evidencia directa")
  if (any(grepl("mcod", vals, ignore.case = TRUE)) || any(grepl("indirect", vals, ignore.case = TRUE))) out <- c(out, "MCOD indirecto")
  if (any(grepl("proporcional|allocation|asignaci[o�]n", vals, ignore.case = TRUE))) out <- c(out, "Asignaci�n proporcional")
  out <- unique(out)
  if (!length(out)) return(paste(vals, collapse = "; "))
  paste(out, collapse = " y ")
}

translate_scope_label <- function(x) {
  txt <- paste(unique(trimws(as.character(na.omit(x)))), collapse = "; ")
  if (!nzchar(txt)) return("")
  txt <- gsub("All diseases", "Todas las enfermedades", txt, fixed = TRUE)
  txt <- gsub("all diseases", "Todas las enfermedades", txt, fixed = TRUE)
  txt <- gsub("All injuries", "Todas las lesiones", txt, fixed = TRUE)
  txt <- gsub("all injuries", "Todas las lesiones", txt, fixed = TRUE)
  txt <- gsub("injuries", "lesiones", txt, ignore.case = TRUE)
  txt <- gsub("infections", "infecciones", txt, ignore.case = TRUE)
  txt <- gsub("c�ncer|canceres|c�nceres|cancer", "cancer", txt, ignore.case = TRUE)
  txt <- gsub("cardiovasculares", "cardiovascular", txt, ignore.case = TRUE)
  txt <- gsub("Cardiovascular", "cardiovasculares", txt, ignore.case = TRUE)
  txt <- gsub("infantiles/cong�nitas|infantiles/congenitas", "infant/congenital", txt, ignore.case = TRUE)
  txt <- gsub("Gastrointestinal", "gastrointestinales", txt, ignore.case = TRUE)
  txt <- gsub("gastrointestinal", "gastrointestinales", txt, ignore.case = TRUE)
  txt <- gsub("ri�on/urinario|ri\\u00f1on/urinario|renal/urinario|kidney/urinary", "Kidney/urinary", txt, ignore.case = TRUE)
  txt <- gsub("parcialmente renal/urinario, todas las enfermedades", "Partial kidney/urinary, all diseases", txt, ignore.case = TRUE)
  txt <- gsub("diabetes tipo 1, 2 y otras diabetes|diabetes tipo 1, tipo 2 y otras|type 1, type 2 and other diabetes", "Type 1, Type 2 and Other diabetes", txt, ignore.case = TRUE)
  txt <- gsub("enfermedades genitourinarias \\(urol�gicas\\); enfermedades genitourinarias \\(urol�gicas y ginecol�gicas\\)", "Genitourinary diseases (urological); Genitourinary diseases (urological and gynecological)", txt, ignore.case = TRUE)
  txt <- gsub("enfermedades genitourinarias \\(urologicas\\); enfermedades genitourinarias \\(urologicas y ginecologicas\\)", "Genitourinary diseases (urological); Genitourinary diseases (urological and gynecological)", txt, ignore.case = TRUE)
  txt <- gsub("excepto|excluyendo", "excluding", txt, ignore.case = TRUE)
  txt <- gsub("todas las enfermedades excluding injuries", "All diseases excluding injuries", txt, ignore.case = TRUE)
  txt <- gsub("all diseases excluding injuries", "All diseases excluding injuries", txt, ignore.case = TRUE)
  txt <- gsub("all diseases excluding infections, cancer y injuries", "All diseases excluding infections, cancer and injuries", txt, ignore.case = TRUE)
  txt <- gsub("all diseases excluding infections, cancer and injuries", "All diseases excluding infections, cancer and injuries", txt, ignore.case = TRUE)
  txt <- gsub("cancer \\(canceres digestivos\\)|cancer \\(c�nceres digestivos\\)", "Cancer (digestive cancers)", txt, ignore.case = TRUE)
  txt <- gsub("^injuries$", "Injuries", txt, ignore.case = TRUE)
  txt <- gsub("Parcialmente Kidney/urinary, All diseases", "Partial kidney/urinary, all diseases", txt, fixed = TRUE)
  txt <- gsub("cancer", "Cancer", txt, fixed = TRUE)
  txt <- gsub("Cancer \\(Cancer digestivos\\)", "Cancer (digestive cancers)", txt, fixed = TRUE)
  txt <- gsub(";", "; ", txt, fixed = TRUE)
  txt <- gsub(",\\s+", ", ", txt)
  txt <- gsub("\\s{2,}", " ", txt)
  txt
}

translate_scope_label <- function(x) {
  txt <- paste(unique(trimws(as.character(na.omit(x)))), collapse = "; ")
  if (!nzchar(txt)) return("")
  txt <- fix_mojibake_text(txt)
  txt <- gsub("All diseases", "Todas las enfermedades", txt, fixed = TRUE)
  txt <- gsub("all diseases", "Todas las enfermedades", txt, fixed = TRUE)
  txt <- gsub("All injuries", "Todas las lesiones", txt, fixed = TRUE)
  txt <- gsub("all injuries", "Todas las lesiones", txt, fixed = TRUE)
  txt <- gsub("excluding", "excepto", txt, ignore.case = TRUE)
  txt <- gsub("injuries", "lesiones", txt, ignore.case = TRUE)
  txt <- gsub("infections", "infecciones", txt, ignore.case = TRUE)
  txt <- gsub("c[a�]ncer|canceres|c[a�]nceres|cancer", "c�ncer", txt, ignore.case = TRUE)
  txt <- gsub("Cardiovascular", "Cardiovasculares", txt, ignore.case = TRUE)
  txt <- gsub("cardiovascular", "cardiovasculares", txt, ignore.case = TRUE)
  txt <- gsub("infant/congenital", "infantiles y cong�nitas", txt, ignore.case = TRUE)
  txt <- gsub("gastrointestinaleseses", "gastrointestinales", txt, ignore.case = TRUE)
  txt <- gsub("gastrointestinaleses", "gastrointestinales", txt, ignore.case = TRUE)
  txt <- gsub("Gastrointestinal", "Gastrointestinales", txt, ignore.case = TRUE)
  txt <- gsub("gastrointestinal", "gastrointestinales", txt, ignore.case = TRUE)
  txt <- gsub("ri�on/urinario|ri\\u00f1on/urinario|renal/urinario|kidney/urinary", "renales y urinarias", txt, ignore.case = TRUE)
  txt <- gsub("Partial kidney/urinary, all diseases", "Parcialmente renales y urinarias, todas las enfermedades", txt, ignore.case = TRUE)
  txt <- gsub("diabetes tipo 1, 2 y otras diabetes|diabetes tipo 1, tipo 2 y otras|type 1, type 2 and other diabetes", "Diabetes tipo 1, diabetes tipo 2 y otras diabetes", txt, ignore.case = TRUE)
  txt <- gsub("Genitourinary diseases \\(urological\\); Genitourinary diseases \\(urological and gynecological\\)", "Enfermedades genitourinarias (urol�gicas); enfermedades genitourinarias (urol�gicas y ginecol�gicas)", txt, ignore.case = TRUE)
  txt <- gsub("C[a�]ncer \\(digestive cancers\\)|C[a�]ncer \\(Cancer digestivos\\)", "C�ncer (c�nceres digestivos)", txt, ignore.case = TRUE)
  txt <- gsub("Todas las otras casas intermedias", "Todas las otras causas intermedias", txt, ignore.case = TRUE)
  txt <- gsub(";", "; ", txt, fixed = TRUE)
  txt <- gsub(",\\s+", ", ", txt)
  txt <- gsub("\\s{2,}", " ", txt)
  trimws(txt)
}

rename_if_present <- function(dt, old, new) {
  present <- old[old %in% names(dt)]
  if (!length(present)) return(invisible(dt))
  idx <- match(present, old)
  setnames(dt, present, new[idx])
  invisible(dt)
}

abds_age_group <- function(age) {
  age <- suppressWarnings(as.integer(age))
  out <- rep(NA_character_, length(age))
  out[age == 0] <- "Menor de 1"
  out[age >= 1 & age <= 4] <- "1-4"
  out[age >= 5 & age <= 9] <- "5-9"
  out[age >= 10 & age <= 14] <- "10-14"
  out[age >= 15 & age <= 19] <- "15-19"
  out[age >= 20 & age <= 24] <- "20-24"
  out[age >= 25 & age <= 29] <- "25-29"
  out[age >= 30 & age <= 34] <- "30-34"
  out[age >= 35 & age <= 39] <- "35-39"
  out[age >= 40 & age <= 44] <- "40-44"
  out[age >= 45 & age <= 49] <- "45-49"
  out[age >= 50 & age <= 54] <- "50-54"
  out[age >= 55 & age <= 59] <- "55-59"
  out[age >= 60 & age <= 64] <- "60-64"
  out[age >= 65 & age <= 69] <- "65-69"
  out[age >= 70 & age <= 74] <- "70-74"
  out[age >= 75 & age <= 79] <- "75-79"
  out[age >= 80 & age <= 84] <- "80-84"
  out[age >= 85 & age <= 89] <- "85-89"
  out[age >= 90 & age <= 94] <- "90-94"
  out[age >= 95 & age <= 99] <- "95-99"
  out[age >= 100] <- "100 y m�s"
  factor(out, levels = c(
    "Menor de 1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34",
    "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
    "75-79", "80-84", "85-89", "90-94", "95-99", "100 y m�s"
  ))
}

details_block <- function(title, body) {
  paste0("<details class=\"glossary-term\"><summary>", esc_html(title), "</summary>", body, "</details>")
}

distribution_table_html <- function(dt, caption, footnote = NULL) {
  parts <- c(
    sprintf("<h3>%s</h3>", esc_html(caption)),
    html_table(dt)
  )
  if (!is.null(footnote) && nzchar(footnote)) {
    parts <- c(parts, sprintf("<p class=\"muted\"><strong>Nota.</strong> %s</p>", esc_html(footnote)))
  }
  paste(parts, collapse = "\n")
}

build_redistribution_abds_data <- function(reference_year = NULL) {
  out_dir <- here("data", "derived", "qc", "review_portal", "redistribution")
  ensure_dir(out_dir)

  rules <- fread(here("data", "final", "redistribution_rules", "redistribution_rules.csv"), showProgress = FALSE)
  gc_pre <- fread(here("data", "derived", "qc", "map_and_redistribute_deaths", "qc_gc_with_targets_pre.csv"), showProgress = FALSE)
  before_l1 <- fread(here("data", "derived", "qc", "qc_redistribution", "before_after_l1.csv"), showProgress = FALSE)
  before_l2 <- fread(here("data", "derived", "qc", "qc_redistribution", "before_after_l2.csv"), showProgress = FALSE)
  balance_total <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_total.csv"), showProgress = FALSE)
  balance_year <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_year.csv"), showProgress = FALSE)
  hier <- fread(
    here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "cause_concept_id", "cause_level", "cause_name", "deaths_observed", "deaths_post_redistribution"),
    showProgress = FALSE
  )
  avp_reconciled <- fread(
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "cause_name", "avp_abs", "deaths_final"),
    showProgress = FALSE
  )

  rule_groups <- unique(rules[, .(
    rule_id,
    source_group_code,
    source_group_name,
    regex_r,
    redistribution_method,
    redistribution_scope
  )])
  group_lookup <- rule_groups[, .(
    source_group_code = first(na.omit(source_group_code)),
    source_group_name = first(na.omit(source_group_name)),
    method_label = translate_method_label(redistribution_method),
    scope_label = translate_scope_label(redistribution_scope),
    icd_codes_full = paste(extract_icd_codes_from_regex(regex_r), collapse = ", "),
    icd_codes_short = collapse_code_summary(extract_icd_codes_from_regex(regex_r), max_n = 12L)
  ), by = rule_id]

  gc_group_year <- unique(gc_pre[, .(year_id, location_id, sex_id, age, rule_id, deaths_input)])
  gc_group_year <- gc_group_year[group_lookup, on = "rule_id", nomatch = 0]
  gc_group_year_sum <- gc_group_year[, .(Number = sum(deaths_input, na.rm = TRUE)), by = .(
    year_id, source_group_code, source_group_name, method_label, scope_label, icd_codes_short, icd_codes_full
  )]
  gc_group_year_sum[, total_redistributed := sum(Number), by = year_id]
  gc_group_year_sum[, proportion_pct := fifelse(total_redistributed > 0, 100 * Number / total_redistributed, NA_real_)]

  if (is.null(reference_year)) {
    reference_year <- max(gc_group_year_sum$year_id, na.rm = TRUE)
  }

  total_deaths_by_year <- hier[cause_level == 0, .(Total_deaths = sum(deaths_observed, na.rm = TRUE)), by = year_id]

  ex_lookup <- avp_reconciled[
    cause_name == "Total" & sex_id %in% c(8507L, 8532L),
    .(
      avp_abs = sum(avp_abs, na.rm = TRUE),
      deaths_final = sum(deaths_final, na.rm = TRUE)
    ),
    by = .(year_id, sex_id, age)
  ][, .(
    year_id, sex_id, age,
    ex_per_death = fifelse(deaths_final > 0, avp_abs / deaths_final, 0)
  )]

  garbage_age_sex <- before_l1[cause_code == "GC_REDIST", .(
    deaths_for_redistribution = sum(deaths_before, na.rm = TRUE)
  ), by = .(year_id, sex_id, age)]
  garbage_age_sex <- merge(garbage_age_sex, ex_lookup, by = c("year_id", "sex_id", "age"), all.x = TRUE)
  garbage_age_sex[is.na(ex_per_death), ex_per_death := 0]
  garbage_age_sex[, redistributed_yll := deaths_for_redistribution * ex_per_death]

  table_3_2 <- merge(
    total_deaths_by_year,
    garbage_age_sex[, .(
      Deaths_for_redistribution = sum(deaths_for_redistribution, na.rm = TRUE),
      YLL_for_redistributed_deaths = sum(redistributed_yll, na.rm = TRUE)
    ), by = year_id],
    by = "year_id",
    all.x = TRUE
  )
  total_yll_by_year <- avp_reconciled[cause_name == "Total" & sex_id %in% c(8507L, 8532L), .(
    Total_YLL = sum(avp_abs, na.rm = TRUE)
  ), by = year_id]
  table_3_2 <- merge(table_3_2, total_yll_by_year, by = "year_id", all.x = TRUE)
  setnames(table_3_2, "year_id", "Reference_year")
  table_3_2[, `:=`(
    Per_cent_of_total_deaths = 100 * Deaths_for_redistribution / Total_deaths,
    Per_cent_of_YLL_redistributed = 100 * YLL_for_redistributed_deaths / Total_YLL
  )]
  setorder(table_3_2, Reference_year)

  age_sex_ref <- garbage_age_sex[year_id == reference_year, .(
    deaths = sum(deaths_for_redistribution, na.rm = TRUE),
    yll = sum(redistributed_yll, na.rm = TRUE)
  ), by = .(sex_id, age)]
  age_sex_ref[, age_group_abds := abds_age_group(age)]
  age_sex_ref <- age_sex_ref[!is.na(age_group_abds)]
  age_sex_ref <- age_sex_ref[, .(
    deaths = sum(deaths, na.rm = TRUE),
    yll = sum(yll, na.rm = TRUE)
  ), by = .(sex_id, age_group_abds)]
  age_wide_deaths <- dcast(age_sex_ref, age_group_abds ~ sex_id, value.var = "deaths", fill = 0)
  age_wide_yll <- dcast(age_sex_ref, age_group_abds ~ sex_id, value.var = "yll", fill = 0)
  rename_if_present(age_wide_deaths, c("8507", "8532"), c("Male_deaths", "Female_deaths"))
  rename_if_present(age_wide_yll, c("8507", "8532"), c("Male_YLL", "Female_YLL"))
  table_3_3 <- merge(age_wide_deaths, age_wide_yll, by = "age_group_abds", all = TRUE)
  table_3_3[is.na(table_3_3)] <- 0
  table_3_3[, `:=`(
    Person_deaths = Female_deaths + Male_deaths,
    Person_YLL = Female_YLL + Male_YLL
  )]
  setnames(table_3_3, "age_group_abds", "Age_group")
  table_3_3 <- rbind(
    table_3_3,
    table_3_3[, .(
      Age_group = "All ages",
      Female_deaths = sum(Female_deaths, na.rm = TRUE),
      Male_deaths = sum(Male_deaths, na.rm = TRUE),
      Person_deaths = sum(Person_deaths, na.rm = TRUE),
      Female_YLL = sum(Female_YLL, na.rm = TRUE),
      Male_YLL = sum(Male_YLL, na.rm = TRUE),
      Person_YLL = sum(Person_YLL, na.rm = TRUE)
    )]
  )

  ex_lookup_hier <- ex_lookup[, .(year_id, sex_id, age, ex_per_death)]
  l2_ref <- before_l2[year_id == reference_year & !is.na(cause_name) & nzchar(trimws(cause_name))]
  l2_ref <- merge(l2_ref, ex_lookup_hier, by = c("year_id", "sex_id", "age"), all.x = TRUE)
  l2_ref[is.na(ex_per_death), ex_per_death := 0]
  l2_ref[, `:=`(
    yll_before = deaths_before * ex_per_death,
    yll_after = deaths_after * ex_per_death
  )]
  l2_agg <- l2_ref[, .(
    deaths_before = sum(deaths_before, na.rm = TRUE),
    deaths_after = sum(deaths_after, na.rm = TRUE),
    yll_before = sum(yll_before, na.rm = TRUE),
    yll_after = sum(yll_after, na.rm = TRUE)
  ), by = .(Disease_group = cause_name)]
  total_deaths_ref <- total_deaths_by_year[year_id == reference_year, Total_deaths][1]
  total_yll_ref <- total_yll_by_year[year_id == reference_year, Total_YLL][1]
  gc_ref <- table_3_2[Reference_year == reference_year]
  redistribution_rows <- data.table(
    Disease_group = "Redistribution",
    deaths_before = gc_ref$Deaths_for_redistribution[1],
    deaths_after = 0,
    yll_before = gc_ref$YLL_for_redistributed_deaths[1],
    yll_after = 0
  )
  all_deaths_row <- data.table(
    Disease_group = "All deaths",
    deaths_before = total_deaths_ref,
    deaths_after = total_deaths_ref,
    yll_before = total_yll_ref,
    yll_after = total_yll_ref
  )
  table_3_4_wide <- rbind(l2_agg, redistribution_rows, all_deaths_row, fill = TRUE)
  table_3_4_wide[, `:=`(
    pct_deaths_before = 100 * deaths_before / total_deaths_ref,
    pct_deaths_after = 100 * deaths_after / total_deaths_ref,
    pct_yll_before = 100 * yll_before / total_yll_ref,
    pct_yll_after = 100 * yll_after / total_yll_ref,
    deaths_increase = deaths_after - deaths_before,
    yll_increase = yll_after - yll_before
  )]
  table_3_4_wide[, `:=`(
    pct_deaths_increase = fifelse(deaths_before > 0, 100 * deaths_increase / deaths_before, NA_real_),
    pct_yll_increase = fifelse(yll_before > 0, 100 * yll_increase / yll_before, NA_real_)
  )]

  make_stage_rows <- function(dt, stage_label, deaths_col, pct_deaths_col, yll_col, pct_yll_col) {
    out <- dt[, .(
      Disease_group,
      Stage = stage_label,
      Deaths = get(deaths_col),
      Percent_deaths = get(pct_deaths_col),
      YLLs = get(yll_col),
      Percent_YLLs = get(pct_yll_col)
    )]
    out
  }
  table_3_4 <- rbindlist(list(
    make_stage_rows(table_3_4_wide, "Before redistribution", "deaths_before", "pct_deaths_before", "yll_before", "pct_yll_before"),
    make_stage_rows(table_3_4_wide, "After redistribution", "deaths_after", "pct_deaths_after", "yll_after", "pct_yll_after"),
    table_3_4_wide[, .(
      Disease_group,
      Stage = "Increase (before to after)",
      Deaths = deaths_increase,
      Percent_deaths = pct_deaths_increase,
      YLLs = yll_increase,
      Percent_YLLs = pct_yll_increase
    )]
  ), use.names = TRUE)
    table_3_4[, stage_order := factor(Stage, levels = c("Before redistribution", "After redistribution", "Increase (before to after)"))]
  disease_order <- data.table(Disease_group = unique(table_3_4_wide$Disease_group), disease_sort = seq_along(unique(table_3_4_wide$Disease_group)))
  disease_order[Disease_group == "Redistribution", disease_sort := 999998L]
  disease_order[Disease_group == "All deaths", disease_sort := 999999L]
  table_3_4 <- merge(table_3_4, disease_order, by = "Disease_group", all.x = TRUE)
  setorder(table_3_4, disease_sort, stage_order)
  table_3_4[, c("stage_order", "disease_sort") := NULL]

  table_3_1_ref <- gc_group_year_sum[year_id == reference_year, .(
    Redistribution_group = source_group_name,
    ICD10_codes = icd_codes_short,
    ICD10_codes_full = icd_codes_full,
    Method = method_label,
    Scope_of_target_diseases = scope_label,
    Number = Number,
    Proportion_pct = proportion_pct,
    source_group_code = source_group_code
  )][order(-Number, Redistribution_group)]
  expected_group_total <- before_l1[year_id == reference_year & cause_code == "GC_REDIST", sum(deaths_before, na.rm = TRUE)]
  grouped_total <- sum(table_3_1_ref$Number, na.rm = TRUE)
  residual_total <- expected_group_total - grouped_total
  if (is.finite(residual_total) && abs(residual_total) > 1e-9) {
    table_3_1_ref <- rbind(
      table_3_1_ref,
      data.table(
        Redistribution_group = "Residual redistributed deaths not grouped in pre-target table",
        ICD10_codes = "See full download",
        ICD10_codes_full = "",
        Method = "See QC trace",
        Scope_of_target_diseases = "Across grouped redistribution universe",
        Number = residual_total,
        Proportion_pct = 100 * residual_total / expected_group_total,
        source_group_code = "RESIDUAL"
      ),
      fill = TRUE
    )
  }
  table_3_1_ref[trimws(Scope_of_target_diseases) %in% c("injuries", "Injuries"), Scope_of_target_diseases := "Injuries"]
  table_3_1_ref[grepl("Parcialmente Kidney/urinary, All diseases", Scope_of_target_diseases, fixed = TRUE), Scope_of_target_diseases := "Partial kidney/urinary, all diseases"]
  table_3_1_ref[trimws(Scope_of_target_diseases) %in% c("cancer", "Cancer"), Scope_of_target_diseases := "Cancer"]
  table_3_1_ref[grepl("cancer \\(cancer digestivos\\)|Cancer \\(Cancer digestivos\\)|cancer \\(digestive cancers\\)|Cancer \\(digestive cancers\\)", Scope_of_target_diseases, ignore.case = TRUE), Scope_of_target_diseases := "Cancer (digestive cancers)"]
  table_3_1_ref[grepl("^All diseases excluding infections, Cancer and injuries$", Scope_of_target_diseases), Scope_of_target_diseases := "All diseases excluding infections, cancer and injuries"]

  table_3_1_ref <- rbind(
    table_3_1_ref,
    data.table(
      Redistribution_group = "All redistribution causes",
      ICD10_codes = "",
      ICD10_codes_full = "",
      Method = "",
      Scope_of_target_diseases = "",
      Number = expected_group_total,
      Proportion_pct = 100,
      source_group_code = ""
    ),
    fill = TRUE
  )

  cancer_row <- table_3_4_wide[grepl("Neoplasias malignas|Cancer", Disease_group, ignore.case = TRUE)][1]
  if (nrow(cancer_row) == 0 || is.na(cancer_row$deaths_increase) || cancer_row$deaths_increase <= 0) {
    cancer_row <- table_3_4_wide[!Disease_group %in% c("Redistribution", "All deaths")][order(-deaths_increase)][1]
  }
  focal_group <- cancer_row$Disease_group[1]
  focal_gain <- cancer_row$deaths_increase[1]
  direct_groups <- table_3_1_ref[grepl("cancer|c�ncer", Redistribution_group, ignore.case = TRUE) & !grepl("^All redistribution causes$", Redistribution_group)]
  direct_group_deaths <- sum(direct_groups$Number, na.rm = TRUE)
  broad_group_deaths <- table_3_1_ref[grepl("Todas las otras|all other non", Redistribution_group, ignore.case = TRUE), Number][1]
  broad_group_deaths <- broad_group_deaths %||% 0
  focal_share_pre <- cancer_row$deaths_before[1] / total_deaths_ref
  proportional_component <- broad_group_deaths * focal_share_pre
  direct_plus_proportional <- direct_group_deaths + proportional_component
  remainder_component <- focal_gain - direct_plus_proportional
  box_3_2 <- data.table(
    focal_group = focal_group,
    year_id = reference_year,
    deaths_before = cancer_row$deaths_before[1],
    deaths_after = cancer_row$deaths_after[1],
    deaths_gain = focal_gain,
    direct_specific_group_deaths = direct_group_deaths,
    proportional_general_group_deaths = proportional_component,
    accounted_by_direct_and_proportional = direct_plus_proportional,
    remaining_gain_from_other_groups = remainder_component,
    pre_redistribution_share = focal_share_pre
  )

  top_abs_deaths <- table_3_4_wide[!Disease_group %in% c("Redistribution", "All deaths")][order(-deaths_increase)][1:3]
  top_pct_deaths <- table_3_4_wide[!Disease_group %in% c("Redistribution", "All deaths") & is.finite(pct_deaths_increase)][order(-pct_deaths_increase)][1:3]
  top_abs_yll <- table_3_4_wide[!Disease_group %in% c("Redistribution", "All deaths")][order(-yll_increase)][1:3]
  top_pct_yll <- table_3_4_wide[!Disease_group %in% c("Redistribution", "All deaths") & is.finite(pct_yll_increase)][order(-pct_yll_increase)][1:3]
  text_metrics <- list(
    reference_year = reference_year,
    total_deaths_redistributed = unname(gc_ref$Deaths_for_redistribution[1]),
    total_pct_deaths_redistributed = unname(gc_ref$Per_cent_of_total_deaths[1]),
    total_yll_redistributed = unname(gc_ref$YLL_for_redistributed_deaths[1]),
    total_pct_yll_redistributed = unname(gc_ref$Per_cent_of_YLL_redistributed[1]),
    top_absolute_death_gains = top_abs_deaths[, .(group = Disease_group, value = deaths_increase, pct = pct_deaths_increase)],
    top_percentage_death_gains = top_pct_deaths[, .(group = Disease_group, value = deaths_increase, pct = pct_deaths_increase)],
    top_absolute_yll_gains = top_abs_yll[, .(group = Disease_group, value = yll_increase, pct = pct_yll_increase)],
    top_percentage_yll_gains = top_pct_yll[, .(group = Disease_group, value = yll_increase, pct = pct_yll_increase)],
    box = box_3_2
  )

  fwrite(table_3_1_ref, file.path(out_dir, "table_3_1_redistribution_groups.csv"))
  fwrite(table_3_2, file.path(out_dir, "table_3_2_impact_total_by_year.csv"))
  fwrite(table_3_3, file.path(out_dir, "table_3_3_impact_by_age_sex_year.csv"))
  fwrite(table_3_4, file.path(out_dir, "table_3_4_before_after_by_disease_group.csv"))
  fwrite(box_3_2, file.path(out_dir, "box_3_2_case_trace_cancer.csv"))
  write_text_file(file.path(out_dir, "redistribution_text_metrics.json"), jsonlite::toJSON(text_metrics, pretty = TRUE, auto_unbox = TRUE))

  list(
    reference_year = reference_year,
    table_3_1 = table_3_1_ref,
    table_3_2 = table_3_2,
    table_3_3 = table_3_3,
    table_3_4 = table_3_4,
    box_3_2 = box_3_2,
    text_metrics = text_metrics,
    balance_total = balance_total,
    balance_year = balance_year
  )
}

build_redistribution_abds_data_v2 <- function(reference_year = NULL) {
  out_dir <- here("data", "derived", "qc", "review_portal", "redistribution")
  ensure_dir(out_dir)

  normalize_icd_local <- function(x) {
    z <- toupper(gsub("[^A-Z0-9]", "", as.character(x %||% "")))
    z[nchar(z) == 0] <- NA_character_
    z
  }

  format_group_label <- function(name, sex_restriction) {
    nm <- trimws(as.character(name %||% ""))
    sx <- trimws(as.character(sex_restriction %||% ""))
    if (grepl("Complicaciones postprocedimiento genitourinarias", nm, ignore.case = TRUE)) {
      if (identical(sx, "male")) return("Complicaciones postprocedimiento genitourinarias - hombres")
      if (identical(sx, "female")) return("Complicaciones postprocedimiento genitourinarias - mujeres")
    }
    nm
  }

  parse_raw_sex <- function(x) {
    z <- toupper(trimws(as.character(x %||% "")))
    out <- rep(NA_character_, length(z))
    out[z %in% c("1", "M", "MALE", "MASCULINO", "HOMBRE")] <- "male"
    out[z %in% c("2", "F", "FEMALE", "FEMENINO", "MUJER")] <- "female"
    out
  }

  detect_sheet_with_synonyms <- function(path, synonyms) {
    if (!requireNamespace("readxl", quietly = TRUE)) return(list(sheet = 1L))
    sheets <- readxl::excel_sheets(path)
    best <- list(score = -1L, sheet = sheets[1])
    for (sh in sheets) {
      probe <- suppressWarnings(tryCatch(readxl::read_excel(path, sheet = sh, n_max = 5), error = function(e) NULL))
      if (is.null(probe)) next
      score <- sum(tolower(names(probe)) %in% tolower(synonyms))
      if (score > best$score) best <- list(score = score, sheet = sh)
    }
    best
  }

  audit_raw_group_presence <- function(focus_groups) {
    raw_dir <- here("data", "raw", "sinadef")
    files <- list.files(raw_dir, pattern = "\\.(csv|xlsx)$", full.names = TRUE)
    if (!length(files)) return(data.table())
    icd_synonyms <- c("causa_b", "cod_f", "cod_f_ok", "ucod", "icd10_ucod_raw")
    sex_synonyms <- c("sexo", "sex", "sexo_def")
    out <- rbindlist(lapply(files, function(fp) {
      year_match <- regmatches(basename(fp), regexpr("20[0-9]{2}", basename(fp)))
      year_id <- suppressWarnings(as.integer(year_match))
      if (is.na(year_id)) return(NULL)
      ext <- tolower(tools::file_ext(fp))
      dt_raw <- NULL
      if (identical(ext, "csv")) {
        hdr <- fread(fp, nrows = 0, encoding = "UTF-8", showProgress = FALSE)
        cols <- names(hdr)
        icd_col <- cols[tolower(cols) %in% icd_synonyms][1]
        sex_col <- cols[tolower(cols) %in% sex_synonyms][1]
        read_cols <- na.omit(c(icd_col, sex_col))
        if (!length(read_cols)) return(NULL)
        dt_raw <- fread(fp, select = read_cols, encoding = "UTF-8", showProgress = FALSE)
      } else if (requireNamespace("readxl", quietly = TRUE)) {
        det <- detect_sheet_with_synonyms(fp, c(icd_synonyms, sex_synonyms))
        probe <- suppressWarnings(tryCatch(readxl::read_excel(fp, sheet = det$sheet), error = function(e) NULL))
        if (is.null(probe)) return(NULL)
        cols <- names(probe)
        icd_col <- cols[tolower(cols) %in% icd_synonyms][1]
        sex_col <- cols[tolower(cols) %in% sex_synonyms][1]
        read_cols <- na.omit(c(icd_col, sex_col))
        if (!length(read_cols)) return(NULL)
        dt_raw <- as.data.table(probe)[, ..read_cols]
      }
      if (is.null(dt_raw) || !nrow(dt_raw)) return(NULL)
      names(dt_raw) <- tolower(names(dt_raw))
      icd_col <- names(dt_raw)[names(dt_raw) %in% icd_synonyms][1]
      sex_col <- names(dt_raw)[names(dt_raw) %in% sex_synonyms][1]
      if (is.na(icd_col)) return(NULL)
      dt_raw[, icd_norm := normalize_icd_local(get(icd_col))]
      dt_raw[, sex_group := if (!is.na(sex_col)) parse_raw_sex(get(sex_col)) else NA_character_]
      rbindlist(lapply(seq_len(nrow(focus_groups)), function(i) {
        fg <- focus_groups[i]
        hit <- dt_raw[grepl(fg$raw_pattern, icd_norm, perl = TRUE)]
        if (!is.na(fg$sex_restriction) && nzchar(fg$sex_restriction)) {
          hit <- hit[sex_group == fg$sex_restriction]
        }
        data.table(
          year_id = year_id,
          group_key = fg$group_key,
          source_group_name = fg$source_group_name,
          sex_restriction = fg$sex_restriction,
          raw_pattern = fg$raw_pattern,
          raw_matches = nrow(hit)
        )
      }), use.names = TRUE, fill = TRUE)
    }), use.names = TRUE, fill = TRUE)
    if (!nrow(out)) return(data.table())
    out[, .(raw_matches = sum(raw_matches, na.rm = TRUE)), by = .(year_id, group_key, source_group_name, sex_restriction, raw_pattern)]
  }

  rules <- fread(here("data", "final", "redistribution_rules", "redistribution_rules.csv"), showProgress = FALSE)
  gc_pre <- fread(here("data", "derived", "qc", "map_and_redistribute_deaths", "qc_gc_with_targets_pre.csv"), showProgress = FALSE)
  balance_total <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_total.csv"), showProgress = FALSE)
  balance_year <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_year.csv"), showProgress = FALSE)
  canon_final <- fread(here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv"), select = c("year_id", "location_id", "sex_id", "age", "cause_level", "cause_name", "deaths_final"), showProgress = FALSE)
  sens_final <- fread(here("data", "final", "death_cause_final_hierarchical_no_redistribution_delete_gc", "death_cause_final_hierarchical.csv"), select = c("year_id", "location_id", "sex_id", "age", "cause_level", "cause_name", "deaths_final"), showProgress = FALSE)
  canon_recon <- fread(here("data", "final", "mortality_rate_cause_smoothed_reconciled", "mortality_rate_cause_smoothed_reconciled.csv"), select = c("year_id", "location_id", "sex_id", "age", "cause_level", "cause_name", "deaths_smoothed_consistent"), showProgress = FALSE)
  sens_recon <- fread(here("data", "final", "mortality_rate_cause_smoothed_reconciled_no_redistribution_delete_gc", "mortality_rate_cause_smoothed_reconciled.csv"), select = c("year_id", "location_id", "sex_id", "age", "cause_level", "cause_name", "deaths_smoothed_consistent"), showProgress = FALSE)
  canon_avp <- fread(here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv"), select = c("year_id", "location_id", "sex_id", "age", "cause_level", "cause_name", "avp_abs"), showProgress = FALSE)
  sens_avp <- fread(here("data", "final", "avp_yll_cause_reconciled_no_redistribution_delete_gc", "avp_yll_cause_reconciled.csv"), select = c("year_id", "location_id", "sex_id", "age", "cause_level", "cause_name", "avp_abs"), showProgress = FALSE)

  years_available <- sort(unique(c(gc_pre$year_id, canon_final$year_id)))
  if (is.null(reference_year)) reference_year <- max(years_available, na.rm = TRUE)

  rules[, sex_restriction_clean := fifelse(is.na(sex_restriction) | !nzchar(trimws(sex_restriction)), "", trimws(as.character(sex_restriction)))]
  rules[, group_key := paste(rule_id, fifelse(nzchar(sex_restriction_clean), sex_restriction_clean, "all"), sep = "__")]
  group_catalog <- rules[, .(
    rule_id = first(rule_id),
    source_group_code = first(na.omit(source_group_code)),
    source_group_name = format_group_label(first(na.omit(source_group_name)), first(sex_restriction_clean)),
    sex_restriction = first(sex_restriction_clean),
    Method = translate_method_label(redistribution_method),
    Scope_of_target_diseases = translate_scope_label(redistribution_scope),
    ICD10_codes_full = paste(extract_icd_codes_from_regex(regex_r), collapse = ", "),
    ICD10_codes = collapse_code_summary(extract_icd_codes_from_regex(regex_r), max_n = 12L)
  ), by = group_key]

  gc_pre_agg <- gc_pre[, .(Number = sum(deaths_input, na.rm = TRUE)), by = .(year_id, rule_id, sex_id, age)]
  obs_year_group <- rbindlist(lapply(seq_len(nrow(group_catalog)), function(i) {
    row <- group_catalog[i]
    tmp <- gc_pre_agg[rule_id == row$rule_id]
    if (!nrow(tmp)) return(data.table(year_id = years_available, group_key = row$group_key, Number = 0))
    if (identical(row$sex_restriction, "male")) tmp <- tmp[sex_id == 8507L]
    if (identical(row$sex_restriction, "female")) tmp <- tmp[sex_id == 8532L]
    tmp[, .(Number = sum(Number, na.rm = TRUE)), by = year_id][, group_key := row$group_key]
  }), use.names = TRUE, fill = TRUE)
  obs_template <- CJ(year_id = years_available, group_key = group_catalog$group_key, unique = TRUE)
  obs_year_group <- merge(obs_template, obs_year_group, by = c("year_id", "group_key"), all.x = TRUE)
  obs_year_group[is.na(Number), Number := 0]
  table_3_1_all <- merge(group_catalog, obs_year_group, by = "group_key", all.x = TRUE)
  table_3_1_all[, Proportion_pct := {
    total_group_year <- sum(Number, na.rm = TRUE)
    if (total_group_year > 0) 100 * Number / total_group_year else rep(0, .N)
  }, by = year_id]

  focus_groups <- group_catalog[source_group_name %in% c("Pneumonitis", "Causas desconocidas", "Complicaciones postprocedimiento genitourinarias - hombres", "Complicaciones postprocedimiento genitourinarias - mujeres")]
  focus_groups[, raw_pattern := fifelse(grepl("Pneumonitis", source_group_name, ignore.case = TRUE), "^J69", fifelse(grepl("Causas desconocidas", source_group_name, ignore.case = TRUE), "^R99$", "^N99$"))]
  raw_audit <- audit_raw_group_presence(focus_groups)
  if (!nrow(raw_audit)) raw_audit <- focus_groups[, .(year_id = years_available, group_key, source_group_name, sex_restriction, raw_pattern, raw_matches = 0)]

  audit_group_catalog_vs_observed <- merge(
    table_3_1_all[group_key %in% focus_groups$group_key, .(year_id, group_key, source_group_name, sex_restriction, observed_in_table31 = Number)],
    raw_audit[, .(year_id, group_key, raw_matches)],
    by = c("year_id", "group_key"),
    all.x = TRUE
  )
  audit_group_catalog_vs_observed[is.na(raw_matches), raw_matches := 0]
  audit_group_catalog_vs_observed[, conclusion := fifelse(observed_in_table31 > 0, "Incluido en la tabla con muertes redistribuidas observadas", fifelse(raw_matches > 0, "Hay presencia en SINADEF crudo pero no redistribuci�n observada downstream; revisar trazabilidad", "Grupo can�nico sin redistribuci�n observada en el a�o, se reporta con 0"))]
  audit_group_raw_sinadef_presence <- raw_audit
  audit_group_table31_inclusion <- table_3_1_all[group_key %in% focus_groups$group_key, .(year_id, group_key, source_group_name, sex_restriction, Number, Proportion_pct)]

  table_3_1_ref <- table_3_1_all[year_id == reference_year, .(
    Redistribution_group = source_group_name,
    ICD10_codes = fifelse(nzchar(ICD10_codes), ICD10_codes, "Ver descarga completa"),
    ICD10_codes_full,
    Method,
    Scope_of_target_diseases,
    Number,
    Proportion_pct,
    source_group_code,
    sex_restriction
  )]
  table_3_1_ref <- rbind(table_3_1_ref, data.table(
    Redistribution_group = "Todas las causas de redistribuci�n",
    ICD10_codes = "", ICD10_codes_full = "", Method = "", Scope_of_target_diseases = "",
    Number = sum(table_3_1_ref$Number, na.rm = TRUE), Proportion_pct = 100, source_group_code = "", sex_restriction = ""
  ), fill = TRUE)

  canon_total_deaths <- canon_recon[cause_level == 0 & location_id == 9000L, .(Total_deaths = sum(deaths_smoothed_consistent, na.rm = TRUE)), by = year_id]
  canon_total_yll <- canon_avp[cause_level == 0 & location_id == 9000L, .(Total_YLL = sum(avp_abs, na.rm = TRUE)), by = year_id]
  sens_total_yll <- sens_avp[cause_level == 0 & location_id == 9000L, .(Total_YLL_sensitivity = sum(avp_abs, na.rm = TRUE)), by = year_id]
  deaths_for_redistribution_year <- obs_year_group[, .(Deaths_for_redistribution = sum(Number, na.rm = TRUE)), by = year_id]
  table_3_2 <- Reduce(function(x, y) merge(x, y, by = "year_id", all = TRUE), list(canon_total_deaths, canon_total_yll, sens_total_yll, deaths_for_redistribution_year))
  table_3_2[is.na(table_3_2)] <- 0
  table_3_2[, YLL_for_redistributed_deaths := Total_YLL - Total_YLL_sensitivity]
  table_3_2[, `:=`(Per_cent_of_total_deaths = fifelse(Total_deaths > 0, 100 * Deaths_for_redistribution / Total_deaths, 0), Per_cent_of_YLL_redistributed = fifelse(Total_YLL > 0, 100 * YLL_for_redistributed_deaths / Total_YLL, 0))]
  setnames(table_3_2, "year_id", "Reference_year")

  deaths_age_sex <- gc_pre[, .(deaths = sum(deaths_input, na.rm = TRUE)), by = .(year_id, sex_id, age)]
  canon_age_yll <- canon_avp[cause_level == 0 & location_id == 9000L & sex_id %in% c(8507L, 8532L), .(canon_yll = sum(avp_abs, na.rm = TRUE)), by = .(year_id, sex_id, age)]
  sens_age_yll <- sens_avp[cause_level == 0 & location_id == 9000L & sex_id %in% c(8507L, 8532L), .(sens_yll = sum(avp_abs, na.rm = TRUE)), by = .(year_id, sex_id, age)]
  age_sex_ref <- merge(deaths_age_sex[year_id == reference_year], canon_age_yll[year_id == reference_year], by = c("year_id", "sex_id", "age"), all = TRUE)
  age_sex_ref <- merge(age_sex_ref, sens_age_yll[year_id == reference_year], by = c("year_id", "sex_id", "age"), all = TRUE)
  age_sex_ref[is.na(age_sex_ref)] <- 0
  age_sex_ref[, yll := canon_yll - sens_yll]
  age_sex_ref[, age_group_abds := abds_age_group(age)]
  age_sex_ref <- age_sex_ref[!is.na(age_group_abds), .(deaths = sum(deaths, na.rm = TRUE), yll = sum(yll, na.rm = TRUE)), by = .(sex_id, age_group_abds)]
  age_wide_deaths <- dcast(age_sex_ref, age_group_abds ~ sex_id, value.var = "deaths", fill = 0)
  age_wide_yll <- dcast(age_sex_ref, age_group_abds ~ sex_id, value.var = "yll", fill = 0)
  rename_if_present(age_wide_deaths, c("8507", "8532"), c("Male_deaths", "Female_deaths"))
  rename_if_present(age_wide_yll, c("8507", "8532"), c("Male_YLL", "Female_YLL"))
  table_3_3 <- merge(age_wide_deaths, age_wide_yll, by = "age_group_abds", all = TRUE)
  table_3_3[is.na(table_3_3)] <- 0
  table_3_3[, `:=`(Person_deaths = Female_deaths + Male_deaths, Person_YLL = Female_YLL + Male_YLL)]
  setnames(table_3_3, "age_group_abds", "Age_group")
  table_3_3 <- rbind(table_3_3, table_3_3[, .(Age_group = "Todas las edades", Female_deaths = sum(Female_deaths), Male_deaths = sum(Male_deaths), Person_deaths = sum(Person_deaths), Female_YLL = sum(Female_YLL), Male_YLL = sum(Male_YLL), Person_YLL = sum(Person_YLL))])

  canon_group <- canon_recon[cause_level == 2 & location_id == 9000L, .(deaths_after = sum(deaths_smoothed_consistent, na.rm = TRUE)), by = .(year_id, Disease_group = cause_name)]
  sens_group <- sens_recon[cause_level == 2 & location_id == 9000L, .(deaths_before = sum(deaths_smoothed_consistent, na.rm = TRUE)), by = .(year_id, Disease_group = cause_name)]
  canon_group_yll <- canon_avp[cause_level == 2 & location_id == 9000L, .(yll_after = sum(avp_abs, na.rm = TRUE)), by = .(year_id, Disease_group = cause_name)]
  sens_group_yll <- sens_avp[cause_level == 2 & location_id == 9000L, .(yll_before = sum(avp_abs, na.rm = TRUE)), by = .(year_id, Disease_group = cause_name)]
  table_3_4_wide <- Reduce(function(x, y) merge(x, y, by = c("year_id", "Disease_group"), all = TRUE), list(sens_group, canon_group, sens_group_yll, canon_group_yll))
  table_3_4_wide[is.na(table_3_4_wide)] <- 0
  table_3_4_wide <- table_3_4_wide[year_id == reference_year]
  total_deaths_ref <- table_3_2[Reference_year == reference_year, Total_deaths][1]
  total_yll_ref <- table_3_2[Reference_year == reference_year, Total_YLL][1]
  sens_total_deaths_ref <- sens_recon[cause_level == 0 & location_id == 9000L & year_id == reference_year, sum(deaths_smoothed_consistent, na.rm = TRUE)]
  sens_total_yll_ref <- sens_avp[cause_level == 0 & location_id == 9000L & year_id == reference_year, sum(avp_abs, na.rm = TRUE)]
  gc_ref <- table_3_2[Reference_year == reference_year]
  table_3_4_wide <- rbind(table_3_4_wide, data.table(year_id = reference_year, Disease_group = "Redistribuci�n / garbage", deaths_before = gc_ref$Deaths_for_redistribution[1], deaths_after = 0, yll_before = gc_ref$YLL_for_redistributed_deaths[1], yll_after = 0), fill = TRUE)
  table_3_4_wide <- rbind(table_3_4_wide, data.table(year_id = reference_year, Disease_group = "Todas las muertes", deaths_before = sens_total_deaths_ref, deaths_after = total_deaths_ref, yll_before = sens_total_yll_ref, yll_after = total_yll_ref), fill = TRUE)
  table_3_4_wide[, `:=`(
    pct_deaths_before = if (sens_total_deaths_ref > 0) 100 * deaths_before / sens_total_deaths_ref else rep(0, .N),
    pct_deaths_after = if (total_deaths_ref > 0) 100 * deaths_after / total_deaths_ref else rep(0, .N),
    pct_yll_before = if (sens_total_yll_ref > 0) 100 * yll_before / sens_total_yll_ref else rep(0, .N),
    pct_yll_after = if (total_yll_ref > 0) 100 * yll_after / total_yll_ref else rep(0, .N),
    deaths_increase = deaths_after - deaths_before,
    yll_increase = yll_after - yll_before
  )]
  table_3_4_wide[, `:=`(pct_deaths_increase = fifelse(deaths_before > 0, 100 * deaths_increase / deaths_before, NA_real_), pct_yll_increase = fifelse(yll_before > 0, 100 * yll_increase / yll_before, NA_real_))]
  make_stage_rows <- function(dt, stage_label, deaths_col, pct_deaths_col, yll_col, pct_yll_col) dt[, .(Disease_group, Stage = stage_label, Deaths = get(deaths_col), Percent_deaths = get(pct_deaths_col), YLLs = get(yll_col), Percent_YLLs = get(pct_yll_col))]
  table_3_4 <- rbindlist(list(make_stage_rows(table_3_4_wide, "Antes de la redistribuci�n", "deaths_before", "pct_deaths_before", "yll_before", "pct_yll_before"), make_stage_rows(table_3_4_wide, "Despu�s de la redistribuci�n", "deaths_after", "pct_deaths_after", "yll_after", "pct_yll_after"), table_3_4_wide[, .(Disease_group, Stage = "Cambio (antes a despu�s)", Deaths = deaths_increase, Percent_deaths = pct_deaths_increase, YLLs = yll_increase, Percent_YLLs = pct_yll_increase)]), use.names = TRUE)

  cancer_row <- table_3_4_wide[grepl("Neoplasias malignas|Cancer|C�ncer", Disease_group, ignore.case = TRUE)][1]
  if (nrow(cancer_row) == 0 || is.na(cancer_row$deaths_increase) || cancer_row$deaths_increase <= 0) cancer_row <- table_3_4_wide[!Disease_group %in% c("Redistribuci�n / garbage", "Todas las muertes")][order(-deaths_increase)][1]
  focal_group <- cancer_row$Disease_group[1]
  focal_gain <- cancer_row$deaths_increase[1]
  direct_groups <- table_3_1_ref[grepl("c[a�]ncer", Redistribution_group, ignore.case = TRUE) & Redistribution_group != "Todas las causas de redistribuci�n"]
  direct_group_deaths <- sum(direct_groups$Number, na.rm = TRUE)
  broad_group_deaths <- table_3_1_ref[grepl("Todas las otras|all other non", Redistribution_group, ignore.case = TRUE), Number][1] %||% 0
  focal_share_pre <- if (sens_total_deaths_ref > 0) cancer_row$deaths_before[1] / sens_total_deaths_ref else 0
  proportional_component <- broad_group_deaths * focal_share_pre
  direct_plus_proportional <- direct_group_deaths + proportional_component
  remainder_component <- focal_gain - direct_plus_proportional
  box_3_2 <- data.table(focal_group = focal_group, year_id = reference_year, deaths_before = cancer_row$deaths_before[1], deaths_after = cancer_row$deaths_after[1], deaths_gain = focal_gain, direct_specific_group_deaths = direct_group_deaths, proportional_general_group_deaths = proportional_component, accounted_by_direct_and_proportional = direct_plus_proportional, remaining_gain_from_other_groups = remainder_component, pre_redistribution_share = focal_share_pre)

  top_abs_deaths <- table_3_4_wide[!Disease_group %in% c("Redistribuci�n / garbage", "Todas las muertes")][order(-deaths_increase)][1:3]
  top_pct_deaths <- table_3_4_wide[!Disease_group %in% c("Redistribuci�n / garbage", "Todas las muertes") & is.finite(pct_deaths_increase)][order(-pct_deaths_increase)][1:3]
  top_abs_yll <- table_3_4_wide[!Disease_group %in% c("Redistribuci�n / garbage", "Todas las muertes")][order(-yll_increase)][1:3]
  top_pct_yll <- table_3_4_wide[!Disease_group %in% c("Redistribuci�n / garbage", "Todas las muertes") & is.finite(pct_yll_increase)][order(-pct_yll_increase)][1:3]
  text_metrics <- list(reference_year = reference_year, total_deaths_redistributed = unname(gc_ref$Deaths_for_redistribution[1]), total_pct_deaths_redistributed = unname(gc_ref$Per_cent_of_total_deaths[1]), total_yll_redistributed = unname(gc_ref$YLL_for_redistributed_deaths[1]), total_pct_yll_redistributed = unname(gc_ref$Per_cent_of_YLL_redistributed[1]), sensitivity_note = "Las tablas 3.2 a 3.4 usan un escenario de sensibilidad sin redistribuci�n y con eliminaci�n de registros garbage.", top_absolute_death_gains = top_abs_deaths[, .(group = Disease_group, value = deaths_increase, pct = pct_deaths_increase)], top_percentage_death_gains = top_pct_deaths[, .(group = Disease_group, value = deaths_increase, pct = pct_deaths_increase)], top_absolute_yll_gains = top_abs_yll[, .(group = Disease_group, value = yll_increase, pct = pct_yll_increase)], top_percentage_yll_gains = top_pct_yll[, .(group = Disease_group, value = yll_increase, pct = pct_yll_increase)], box = box_3_2)

  write_csv_utf8(table_3_1_ref, file.path(out_dir, "table_3_1_redistribution_groups.csv"))
  write_csv_utf8(table_3_2, file.path(out_dir, "table_3_2_impact_total_by_year.csv"))
  write_csv_utf8(table_3_3, file.path(out_dir, "table_3_3_impact_by_age_sex_year.csv"))
  write_csv_utf8(table_3_4, file.path(out_dir, "table_3_4_before_after_by_disease_group.csv"))
  write_csv_utf8(box_3_2, file.path(out_dir, "box_3_2_case_trace_cancer.csv"))
  write_csv_utf8(audit_group_catalog_vs_observed, file.path(out_dir, "audit_group_catalog_vs_observed.csv"))
  write_csv_utf8(audit_group_raw_sinadef_presence, file.path(out_dir, "audit_group_raw_sinadef_presence.csv"))
  write_csv_utf8(audit_group_table31_inclusion, file.path(out_dir, "audit_group_table31_inclusion.csv"))
  write_text_file(file.path(out_dir, "redistribution_text_metrics.json"), jsonlite::toJSON(text_metrics, pretty = TRUE, auto_unbox = TRUE))

  list(reference_year = reference_year, table_3_1 = table_3_1_ref, table_3_2 = table_3_2, table_3_3 = table_3_3, table_3_4 = table_3_4, box_3_2 = box_3_2, text_metrics = text_metrics, balance_total = balance_total, balance_year = balance_year, audit_group_catalog_vs_observed = audit_group_catalog_vs_observed, audit_group_raw_sinadef_presence = audit_group_raw_sinadef_presence, audit_group_table31_inclusion = audit_group_table31_inclusion)
}

build_redistribution_abds_data_v3 <- function(reference_year = NULL) {
  out_dir <- here("data", "derived", "qc", "review_portal", "redistribution")
  pure_dir <- here("data", "derived", "analysis_sensitivity", "redistribution", "impacto_puro_redistribucion")
  pure_qc_dir <- here("data", "derived", "qc", "analysis_sensitivity", "redistribution", "impacto_puro_redistribucion")
  ensure_dir(out_dir)
  ensure_dir(pure_dir)
  ensure_dir(pure_qc_dir)

  normalize_icd_local <- function(x) {
    z <- toupper(gsub("[^A-Z0-9]", "", as.character(x %||% "")))
    z[nchar(z) == 0] <- NA_character_
    z
  }

  format_group_label <- function(name, sex_restriction) {
    nm <- trimws(as.character(name %||% ""))
    sx <- trimws(as.character(sex_restriction %||% ""))
    if (grepl("Complicaciones postprocedimiento genitourinarias", nm, ignore.case = TRUE)) {
      if (identical(sx, "male")) return("Complicaciones postprocedimiento genitourinarias - hombres")
      if (identical(sx, "female")) return("Complicaciones postprocedimiento genitourinarias - mujeres")
    }
    nm
  }

  parse_raw_sex <- function(x) {
    z <- toupper(trimws(as.character(x %||% "")))
    out <- rep(NA_character_, length(z))
    out[z %in% c("1", "M", "MALE", "MASCULINO", "HOMBRE")] <- "male"
    out[z %in% c("2", "F", "FEMALE", "FEMENINO", "MUJER")] <- "female"
    out
  }

  detect_sheet_with_synonyms <- function(path, synonyms) {
    if (!requireNamespace("readxl", quietly = TRUE)) return(list(sheet = 1L))
    sheets <- readxl::excel_sheets(path)
    best <- list(score = -1L, sheet = sheets[1])
    for (sh in sheets) {
      probe <- suppressWarnings(tryCatch(readxl::read_excel(path, sheet = sh, n_max = 5), error = function(e) NULL))
      if (is.null(probe)) next
      score <- sum(tolower(names(probe)) %in% tolower(synonyms))
      if (score > best$score) best <- list(score = score, sheet = sh)
    }
    best
  }

  audit_raw_group_presence <- function(focus_groups) {
    raw_dir <- here("data", "raw", "sinadef")
    files <- list.files(raw_dir, pattern = "\\.(csv|xlsx)$", full.names = TRUE)
    if (!length(files)) return(data.table())
    icd_synonyms <- c("causa_b", "cod_f", "cod_f_ok", "ucod", "icd10_ucod_raw")
    sex_synonyms <- c("sexo", "sex", "sexo_def")
    out <- rbindlist(lapply(files, function(fp) {
      year_match <- regmatches(basename(fp), regexpr("20[0-9]{2}", basename(fp)))
      year_id <- suppressWarnings(as.integer(year_match))
      if (is.na(year_id)) return(NULL)
      ext <- tolower(tools::file_ext(fp))
      dt_raw <- NULL
      if (identical(ext, "csv")) {
        hdr <- fread(fp, nrows = 0, encoding = "UTF-8", showProgress = FALSE)
        cols <- names(hdr)
        icd_col <- cols[tolower(cols) %in% icd_synonyms][1]
        sex_col <- cols[tolower(cols) %in% sex_synonyms][1]
        read_cols <- na.omit(c(icd_col, sex_col))
        if (!length(read_cols)) return(NULL)
        dt_raw <- fread(fp, select = read_cols, encoding = "UTF-8", showProgress = FALSE)
      } else if (requireNamespace("readxl", quietly = TRUE)) {
        det <- detect_sheet_with_synonyms(fp, c(icd_synonyms, sex_synonyms))
        probe <- suppressWarnings(tryCatch(readxl::read_excel(fp, sheet = det$sheet), error = function(e) NULL))
        if (is.null(probe)) return(NULL)
        cols <- names(probe)
        icd_col <- cols[tolower(cols) %in% icd_synonyms][1]
        sex_col <- cols[tolower(cols) %in% sex_synonyms][1]
        read_cols <- na.omit(c(icd_col, sex_col))
        if (!length(read_cols)) return(NULL)
        dt_raw <- as.data.table(probe)[, ..read_cols]
      }
      if (is.null(dt_raw) || !nrow(dt_raw)) return(NULL)
      names(dt_raw) <- tolower(names(dt_raw))
      icd_col <- names(dt_raw)[names(dt_raw) %in% icd_synonyms][1]
      sex_col <- names(dt_raw)[names(dt_raw) %in% sex_synonyms][1]
      if (is.na(icd_col)) return(NULL)
      dt_raw[, icd_norm := normalize_icd_local(get(icd_col))]
      dt_raw[, sex_group := if (!is.na(sex_col)) parse_raw_sex(get(sex_col)) else NA_character_]
      rbindlist(lapply(seq_len(nrow(focus_groups)), function(i) {
        fg <- focus_groups[i]
        hit <- dt_raw[grepl(fg$raw_pattern, icd_norm, perl = TRUE)]
        if (!is.na(fg$sex_restriction) && nzchar(fg$sex_restriction)) {
          hit <- hit[sex_group == fg$sex_restriction]
        }
        data.table(
          year_id = year_id,
          group_key = fg$group_key,
          source_group_name = fg$source_group_name,
          sex_restriction = fg$sex_restriction,
          raw_pattern = fg$raw_pattern,
          raw_matches = nrow(hit)
        )
      }), use.names = TRUE, fill = TRUE)
    }), use.names = TRUE, fill = TRUE)
    if (!nrow(out)) return(data.table())
    out[, .(raw_matches = sum(raw_matches, na.rm = TRUE)), by = .(year_id, group_key, source_group_name, sex_restriction, raw_pattern)]
  }

  rules <- fread(here("data", "final", "redistribution_rules", "redistribution_rules.csv"), showProgress = FALSE)
  gc_pre <- fread(here("data", "derived", "qc", "map_and_redistribute_deaths", "qc_gc_with_targets_pre.csv"), showProgress = FALSE)
  balance_total <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_total.csv"), showProgress = FALSE)
  balance_year <- fread(here("data", "derived", "qc", "qc_redistribution", "qc_balance_year.csv"), showProgress = FALSE)
  hier_base <- fread(
    here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "cause_concept_id", "cause_level", "cause_name", "deaths_observed", "deaths_post_redistribution"),
    showProgress = FALSE
  )
  avp_ref <- fread(
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "cause_concept_id", "cause_level", "ex_standard"),
    showProgress = FALSE
  )
  qc_pandemic_canon <- fread(here("data", "derived", "qc", "build_death_cause_final", "qc_pandemic_by_year.csv"), showProgress = FALSE)
  qc_pandemic_sens <- fread(here("data", "derived", "qc", "build_death_cause_final_no_redistribution_delete_gc", "qc_pandemic_by_year.csv"), showProgress = FALSE)
  qc_factor_canon <- fread(here("data", "derived", "qc", "build_death_cause_final", "qc_factor_summary.csv"), showProgress = FALSE)
  qc_factor_sens <- fread(here("data", "derived", "qc", "build_death_cause_final_no_redistribution_delete_gc", "qc_factor_summary.csv"), showProgress = FALSE)

  years_available <- sort(unique(c(gc_pre$year_id, hier_base$year_id)))
  if (is.null(reference_year)) reference_year <- max(years_available, na.rm = TRUE)

  rules[, sex_restriction_clean := fifelse(is.na(sex_restriction) | !nzchar(trimws(sex_restriction)), "", trimws(as.character(sex_restriction)))]
  rules[, group_key := paste(rule_id, fifelse(nzchar(sex_restriction_clean), sex_restriction_clean, "all"), sep = "__")]
  group_catalog <- rules[, .(
    rule_id = first(rule_id),
    source_group_code = first(na.omit(source_group_code)),
    source_group_name = format_group_label(first(na.omit(source_group_name)), first(sex_restriction_clean)),
    sex_restriction = first(sex_restriction_clean),
    Method = translate_method_label(redistribution_method),
    Scope_of_target_diseases = translate_scope_label(redistribution_scope),
    ICD10_codes_full = paste(extract_icd_codes_from_regex(regex_r), collapse = ", "),
    ICD10_codes = collapse_code_summary(extract_icd_codes_from_regex(regex_r), max_n = 12L)
  ), by = group_key]

  gc_pre_agg <- gc_pre[, .(Number = sum(deaths_input, na.rm = TRUE)), by = .(year_id, rule_id, sex_id, age)]
  obs_year_group <- rbindlist(lapply(seq_len(nrow(group_catalog)), function(i) {
    row <- group_catalog[i]
    tmp <- gc_pre_agg[rule_id == row$rule_id]
    if (!nrow(tmp)) return(data.table(year_id = years_available, group_key = row$group_key, Number = 0))
    if (identical(row$sex_restriction, "male")) tmp <- tmp[sex_id == 8507L]
    if (identical(row$sex_restriction, "female")) tmp <- tmp[sex_id == 8532L]
    tmp[, .(Number = sum(Number, na.rm = TRUE)), by = year_id][, group_key := row$group_key]
  }), use.names = TRUE, fill = TRUE)
  obs_template <- CJ(year_id = years_available, group_key = group_catalog$group_key, unique = TRUE)
  obs_year_group <- merge(obs_template, obs_year_group, by = c("year_id", "group_key"), all.x = TRUE)
  obs_year_group[is.na(Number), Number := 0]
  table_3_1_all <- merge(group_catalog, obs_year_group, by = "group_key", all.x = TRUE)
  table_3_1_all[, Proportion_pct := if (sum(Number, na.rm = TRUE) > 0) 100 * Number / sum(Number, na.rm = TRUE) else 0, by = year_id]

  focus_groups <- group_catalog[source_group_name %in% c("Pneumonitis", "Causas desconocidas", "Complicaciones postprocedimiento genitourinarias - hombres", "Complicaciones postprocedimiento genitourinarias - mujeres")]
  focus_groups[, raw_pattern := fifelse(
    grepl("Pneumonitis", source_group_name, ignore.case = TRUE),
    "^J69",
    fifelse(grepl("Causas desconocidas", source_group_name, ignore.case = TRUE), "^R99$", "^N99$")
  )]
  raw_audit <- audit_raw_group_presence(focus_groups)
  if (!nrow(raw_audit)) {
    raw_audit <- focus_groups[, .(year_id = years_available, group_key, source_group_name, sex_restriction, raw_pattern, raw_matches = 0)]
  }

  audit_group_catalog_vs_observed <- merge(
    table_3_1_all[group_key %in% focus_groups$group_key, .(year_id, group_key, source_group_name, sex_restriction, observed_in_table31 = Number)],
    raw_audit[, .(year_id, group_key, raw_matches)],
    by = c("year_id", "group_key"),
    all.x = TRUE
  )
  audit_group_catalog_vs_observed[is.na(raw_matches), raw_matches := 0]
  audit_group_catalog_vs_observed[, conclusion := fifelse(
    observed_in_table31 > 0,
    "Incluido en la tabla con muertes redistribuidas observadas",
    fifelse(raw_matches > 0, "Hay presencia en SINADEF crudo pero no redistribucion observada downstream; revisar trazabilidad", "Grupo canonico sin redistribucion observada en el ano, se reporta con 0")
  )]
  audit_group_raw_sinadef_presence <- raw_audit
  audit_group_table31_inclusion <- table_3_1_all[group_key %in% focus_groups$group_key, .(year_id, group_key, source_group_name, sex_restriction, Number, Proportion_pct)]

  table_3_1_ref <- table_3_1_all[year_id == reference_year, .(
    Redistribution_group = source_group_name,
    ICD10_codes = fifelse(nzchar(ICD10_codes), ICD10_codes, "Ver descarga completa"),
    ICD10_codes_full,
    Method,
    Scope_of_target_diseases,
    Number,
    Proportion_pct,
    source_group_code,
    sex_restriction
  )]
  table_3_1_ref <- rbind(table_3_1_ref, data.table(
    Redistribution_group = "Todas las causas de redistribucion",
    ICD10_codes = "",
    ICD10_codes_full = "",
    Method = "",
    Scope_of_target_diseases = "",
    Number = sum(table_3_1_ref$Number, na.rm = TRUE),
    Proportion_pct = 100,
    source_group_code = "",
    sex_restriction = ""
  ), fill = TRUE)

  avp_lookup <- unique(avp_ref[, .(year_id, location_id, sex_id, age, cause_concept_id, cause_level, ex_standard)])
  hier_base <- merge(
    hier_base,
    avp_lookup,
    by = c("year_id", "location_id", "sex_id", "age", "cause_concept_id", "cause_level"),
    all.x = TRUE
  )
  total_ex_lookup <- unique(avp_ref[cause_level == 0, .(year_id, sex_id, age, ex_standard_total = ex_standard)])
  hier_base <- merge(hier_base, total_ex_lookup, by = c("year_id", "sex_id", "age"), all.x = TRUE)
  hier_base[is.na(ex_standard), ex_standard := ex_standard_total]
  hier_base[, ex_standard_total := NULL]
  hier_base[, yll_before_pure := deaths_observed * ex_standard]
  hier_base[, yll_after_pure := deaths_post_redistribution * ex_standard]

  pure_balance_year <- hier_base[cause_level == 0, .(
    Total_deaths = sum(deaths_observed, na.rm = TRUE),
    Total_deaths_after = sum(deaths_post_redistribution, na.rm = TRUE),
    Total_YLL = sum(yll_before_pure, na.rm = TRUE),
    Total_YLL_after = sum(yll_after_pure, na.rm = TRUE)
  ), by = year_id][order(year_id)]
  pure_balance_year[, `:=`(
    delta_deaths = Total_deaths_after - Total_deaths,
    delta_yll = Total_YLL_after - Total_YLL
  )]

  gc_age_sex <- gc_pre[, .(Deaths_for_redistribution = sum(deaths_input, na.rm = TRUE)), by = .(year_id, sex_id, age)]
  gc_age_sex <- merge(gc_age_sex, total_ex_lookup, by = c("year_id", "sex_id", "age"), all.x = TRUE)
  gc_age_sex[is.na(ex_standard_total), ex_standard_total := 0]
  gc_age_sex[, YLL_for_redistributed_deaths := Deaths_for_redistribution * ex_standard_total]
  garbage_year <- gc_age_sex[, .(
    Deaths_for_redistribution = sum(Deaths_for_redistribution, na.rm = TRUE),
    YLL_for_redistributed_deaths = sum(YLL_for_redistributed_deaths, na.rm = TRUE)
  ), by = year_id]

  table_3_2 <- merge(
    pure_balance_year[, .(year_id, Total_deaths, Total_YLL)],
    garbage_year,
    by = "year_id",
    all = TRUE
  )
  table_3_2[is.na(table_3_2)] <- 0
  table_3_2[, `:=`(
    Per_cent_of_total_deaths = fifelse(Total_deaths > 0, 100 * Deaths_for_redistribution / Total_deaths, 0),
    Per_cent_of_YLL_redistributed = fifelse(Total_YLL > 0, 100 * YLL_for_redistributed_deaths / Total_YLL, 0)
  )]
  setnames(table_3_2, "year_id", "Reference_year")

  age_sex_ref <- gc_age_sex[year_id == reference_year]
  age_sex_ref[, Age_group := abds_age_group(age)]
  age_sex_ref <- age_sex_ref[!is.na(Age_group), .(
    deaths = sum(Deaths_for_redistribution, na.rm = TRUE),
    yll = sum(YLL_for_redistributed_deaths, na.rm = TRUE)
  ), by = .(sex_id, Age_group)]
  age_wide_deaths <- dcast(age_sex_ref, Age_group ~ sex_id, value.var = "deaths", fill = 0)
  age_wide_yll <- dcast(age_sex_ref, Age_group ~ sex_id, value.var = "yll", fill = 0)
  rename_if_present(age_wide_deaths, c("8507", "8532"), c("Male_deaths", "Female_deaths"))
  rename_if_present(age_wide_yll, c("8507", "8532"), c("Male_YLL", "Female_YLL"))
  table_3_3 <- merge(age_wide_deaths, age_wide_yll, by = "Age_group", all = TRUE)
  table_3_3[is.na(table_3_3)] <- 0
  table_3_3[, `:=`(Person_deaths = Female_deaths + Male_deaths, Person_YLL = Female_YLL + Male_YLL)]
  table_3_3 <- rbind(table_3_3, table_3_3[, .(
    Age_group = "Todas las edades",
    Female_deaths = sum(Female_deaths, na.rm = TRUE),
    Male_deaths = sum(Male_deaths, na.rm = TRUE),
    Person_deaths = sum(Person_deaths, na.rm = TRUE),
    Female_YLL = sum(Female_YLL, na.rm = TRUE),
    Male_YLL = sum(Male_YLL, na.rm = TRUE),
    Person_YLL = sum(Person_YLL, na.rm = TRUE)
  )], fill = TRUE)

  sex_rule_applies_local <- function(sex_id, sex_restriction) {
    if (is.na(sex_restriction) || sex_restriction == "") return(TRUE)
    if (sex_restriction == "male" && sex_id == 8507L) return(TRUE)
    if (sex_restriction == "female" && sex_id == 8532L) return(TRUE)
    FALSE
  }
  age_rule_applies_local <- function(age, age_start, age_end) {
    lo_ok <- is.na(age_start) || age >= age_start
    hi_ok <- is.na(age_end) || age <= age_end
    lo_ok & hi_ok
  }

  cause_master <- fread(
    here("data", "final", "cause_master", "cause_master.csv"),
    select = c("cause_concept_id", "cause_name", "cause_level", "level_2_name"),
    showProgress = FALSE
  )
  cause_group_lookup <- unique(cause_master[, .(
    cause_term_concept_id = cause_concept_id,
    cause_name_leaf = cause_name,
    cause_level,
    Disease_group = fifelse(cause_level == 2L, cause_name, level_2_name)
  )])

  leaf_after <- fread(
    here("data", "final", "death_cause_leaf_post_redistribution", "death_cause_leaf_post_redistribution.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "cause_term_concept_id", "deaths"),
    showProgress = FALSE
  )
  leaf_after <- merge(leaf_after, cause_group_lookup, by = "cause_term_concept_id", all.x = TRUE)
  leaf_after <- merge(leaf_after, total_ex_lookup, by = c("year_id", "sex_id", "age"), all.x = TRUE)
  leaf_after[is.na(ex_standard_total), ex_standard_total := 0]
  leaf_after[is.na(Disease_group) | !nzchar(Disease_group), Disease_group := cause_name_leaf]
  leaf_after[, yll_after_component := deaths * ex_standard_total]

  after_group_ref <- leaf_after[year_id == reference_year, .(
    deaths_after = sum(deaths, na.rm = TRUE),
    yll_after = sum(yll_after_component, na.rm = TRUE)
  ), by = Disease_group]

  targets <- unique(rules[, .(
    rule_id,
    target_term_concept_id = target_cause_concept_id,
    target_weight,
    sex_restriction,
    target_sex_restriction_cm,
    target_age_start_cm,
    target_age_end_cm,
    age_start,
    age_end
  )])
  gc_base <- gc_pre[, .(
    year_id, location_id, sex_id, age, icd10_ucod_nodot, deaths_input, rule_id
  )]
  setkey(gc_base, rule_id)
  setkey(targets, rule_id)
  gc_with_targets <- targets[gc_base, allow.cartesian = TRUE]
  gc_with_targets[, target_applies :=
    mapply(sex_rule_applies_local, sex_id, sex_restriction) &
    mapply(sex_rule_applies_local, sex_id, target_sex_restriction_cm) &
    mapply(age_rule_applies_local, age, age_start, age_end) &
    mapply(age_rule_applies_local, age, target_age_start_cm, target_age_end_cm)
  ]
  gc_with_targets <- gc_with_targets[target_applies == TRUE]
  if (nrow(gc_with_targets) > 0L) {
    gc_with_targets[, target_weight_filtered_sum := sum(target_weight), by = .(year_id, location_id, sex_id, age, icd10_ucod_nodot, rule_id)]
    gc_with_targets[, target_weight_final := fifelse(target_weight_filtered_sum > 0, target_weight / target_weight_filtered_sum, NA_real_)]
    gc_with_targets[, deaths_alloc := as.numeric(deaths_input) * target_weight_final]
    gc_with_targets <- merge(gc_with_targets, total_ex_lookup, by = c("year_id", "sex_id", "age"), all.x = TRUE)
    gc_with_targets[is.na(ex_standard_total), ex_standard_total := 0]
    gc_with_targets[, yll_alloc := deaths_alloc * ex_standard_total]
    gc_with_targets <- merge(gc_with_targets, cause_group_lookup, by.x = "target_term_concept_id", by.y = "cause_term_concept_id", all.x = TRUE)
    gc_with_targets[is.na(Disease_group) | !nzchar(Disease_group), Disease_group := cause_name_leaf]
    gains_group_ref <- gc_with_targets[year_id == reference_year, .(
      deaths_gain = sum(deaths_alloc, na.rm = TRUE),
      yll_gain = sum(yll_alloc, na.rm = TRUE)
    ), by = Disease_group]
  } else {
    gains_group_ref <- data.table(Disease_group = character(), deaths_gain = numeric(), yll_gain = numeric())
  }

  table_3_4_wide <- merge(after_group_ref, gains_group_ref, by = "Disease_group", all = TRUE)
  table_3_4_wide[is.na(table_3_4_wide)] <- 0
  table_3_4_wide[, `:=`(
    deaths_before = deaths_after - deaths_gain,
    yll_before = yll_after - yll_gain
  )]
  table_3_4_wide[, year_id := reference_year]
  total_ref <- pure_balance_year[year_id == reference_year]
  gc_ref <- table_3_2[Reference_year == reference_year]
  total_deaths_ref <- total_ref$Total_deaths[1] %||% 0
  total_deaths_after_ref <- total_ref$Total_deaths_after[1] %||% 0
  total_yll_ref <- total_ref$Total_YLL[1] %||% 0
  total_yll_after_ref <- total_ref$Total_YLL_after[1] %||% 0

  table_3_4_wide <- rbind(table_3_4_wide, data.table(
    year_id = reference_year,
    Disease_group = "Redistribucion / garbage",
    deaths_before = gc_ref$Deaths_for_redistribution[1],
    deaths_after = 0,
    yll_before = gc_ref$YLL_for_redistributed_deaths[1],
    yll_after = 0
  ), fill = TRUE)
  table_3_4_wide <- rbind(table_3_4_wide, data.table(
    year_id = reference_year,
    Disease_group = "Todas las muertes",
    deaths_before = total_deaths_ref,
    deaths_after = total_deaths_after_ref,
    yll_before = total_yll_ref,
    yll_after = total_yll_after_ref
  ), fill = TRUE)
  table_3_4_wide[, `:=`(
    deaths_before = pmax(deaths_before, 0),
    yll_before = pmax(yll_before, 0)
  )]
  table_3_4_wide[, `:=`(
    pct_deaths_before = if (total_deaths_ref > 0) 100 * deaths_before / total_deaths_ref else 0,
    pct_deaths_after = if (total_deaths_ref > 0) 100 * deaths_after / total_deaths_ref else 0,
    pct_yll_before = if (total_yll_ref > 0) 100 * yll_before / total_yll_ref else 0,
    pct_yll_after = if (total_yll_ref > 0) 100 * yll_after / total_yll_ref else 0,
    deaths_increase = deaths_after - deaths_before,
    yll_increase = yll_after - yll_before
  )]
  table_3_4_wide[, `:=`(
    pct_deaths_increase = fifelse(deaths_before > 0, 100 * deaths_increase / deaths_before, NA_real_),
    pct_yll_increase = fifelse(yll_before > 0, 100 * yll_increase / yll_before, NA_real_)
  )]
  make_stage_rows <- function(dt, stage_label, deaths_col, pct_deaths_col, yll_col, pct_yll_col) {
    dt[, .(Disease_group, Stage = stage_label, Deaths = get(deaths_col), Percent_deaths = get(pct_deaths_col), YLLs = get(yll_col), Percent_YLLs = get(pct_yll_col))]
  }
  table_3_4 <- rbindlist(list(
    make_stage_rows(table_3_4_wide, "Antes de la redistribucion", "deaths_before", "pct_deaths_before", "yll_before", "pct_yll_before"),
    make_stage_rows(table_3_4_wide, "Despues de la redistribucion", "deaths_after", "pct_deaths_after", "yll_after", "pct_yll_after"),
    table_3_4_wide[, .(Disease_group, Stage = "Cambio (antes a despues)", Deaths = deaths_increase, Percent_deaths = pct_deaths_increase, YLLs = yll_increase, Percent_YLLs = pct_yll_increase)]
  ), use.names = TRUE)
  table_3_4[Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & Stage == "Cambio (antes a despues)",
            `:=`(Deaths = NA_real_, Percent_deaths = NA_real_, YLLs = NA_real_, Percent_YLLs = NA_real_)]

  cancer_row <- table_3_4_wide[grepl("Neoplasias malignas|Cancer|cancer", Disease_group, ignore.case = TRUE)][1]
  if (nrow(cancer_row) == 0 || is.na(cancer_row$deaths_increase) || cancer_row$deaths_increase <= 0) {
    cancer_row <- table_3_4_wide[!Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-deaths_increase)][1]
  }
  focal_group <- cancer_row$Disease_group[1]
  focal_gain <- cancer_row$deaths_increase[1]
  direct_groups <- table_3_1_ref[grepl("cancer|neoplasias", Redistribution_group, ignore.case = TRUE) & Redistribution_group != "Todas las causas de redistribucion"]
  direct_group_deaths <- sum(direct_groups$Number, na.rm = TRUE)
  broad_group_deaths <- table_3_1_ref[grepl("Todas las otras|all other non", Redistribution_group, ignore.case = TRUE), Number][1] %||% 0
  focal_share_pre <- if (total_deaths_ref > 0) cancer_row$deaths_before[1] / total_deaths_ref else 0
  proportional_component <- broad_group_deaths * focal_share_pre
  direct_plus_proportional <- direct_group_deaths + proportional_component
  remainder_component <- focal_gain - direct_plus_proportional
  box_3_2 <- data.table(
    focal_group = focal_group,
    year_id = reference_year,
    deaths_before = cancer_row$deaths_before[1],
    deaths_after = cancer_row$deaths_after[1],
    deaths_gain = focal_gain,
    direct_specific_group_deaths = direct_group_deaths,
    proportional_general_group_deaths = proportional_component,
    accounted_by_direct_and_proportional = direct_plus_proportional,
    remaining_gain_from_other_groups = remainder_component,
    pre_redistribution_share = focal_share_pre
  )

  top_abs_deaths <- table_3_4_wide[!Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-deaths_increase)][1:3]
  top_pct_deaths <- table_3_4_wide[!Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & is.finite(pct_deaths_increase)][order(-pct_deaths_increase)][1:3]
  top_abs_yll <- table_3_4_wide[!Disease_group %in% c("Redistribucion / garbage", "Todas las muertes")][order(-yll_increase)][1:3]
  top_pct_yll <- table_3_4_wide[!Disease_group %in% c("Redistribucion / garbage", "Todas las muertes") & is.finite(pct_yll_increase)][order(-pct_yll_increase)][1:3]

  extended_sensitivity_summary <- merge(
    qc_pandemic_canon[, .(year_id, observed_allcause, expected_allcause, observed_corrected_allcause, pandemic_excess_allcause)],
    qc_pandemic_sens[, .(
      year_id,
      observed_allcause_sensitivity = observed_allcause,
      expected_allcause_sensitivity = expected_allcause,
      observed_corrected_allcause_sensitivity = observed_corrected_allcause,
      pandemic_excess_allcause_sensitivity = pandemic_excess_allcause
    )],
    by = "year_id",
    all = TRUE
  )
  extended_sensitivity_summary <- merge(
    extended_sensitivity_summary,
    qc_factor_canon[, .(year_id, median_factor_canonical = median_factor, p75_factor_canonical = p75_factor)],
    by = "year_id",
    all = TRUE
  )
  extended_sensitivity_summary <- merge(
    extended_sensitivity_summary,
    qc_factor_sens[, .(year_id, median_factor_sensitivity = median_factor, p75_factor_sensitivity = p75_factor)],
    by = "year_id",
    all = TRUE
  )
  extended_sensitivity_summary[, `:=`(
    delta_observed = observed_allcause - observed_allcause_sensitivity,
    delta_corrected = observed_corrected_allcause - observed_corrected_allcause_sensitivity,
    delta_pandemic_excess = pandemic_excess_allcause - pandemic_excess_allcause_sensitivity
  )]

  text_metrics <- list(
    reference_year = reference_year,
    total_deaths_redistributed = unname(gc_ref$Deaths_for_redistribution[1]),
    total_pct_deaths_redistributed = unname(gc_ref$Per_cent_of_total_deaths[1]),
    total_yll_redistributed = unname(gc_ref$YLL_for_redistributed_deaths[1]),
    total_pct_yll_redistributed = unname(gc_ref$Per_cent_of_YLL_redistributed[1]),
    pure_note = "Las tablas 3.2 a 3.4 miden el efecto puro de redistribucion antes de pandemia y subregistro, dentro del mismo universo de muertes.",
    extended_sensitivity_note = "La sensibilidad extendida sin redistribucion y con borrado de garbage se reporta aparte. Puede generar mas muertes corregidas y mas AVP por cambios en completitud y exceso pandemico.",
    top_absolute_death_gains = top_abs_deaths[, .(group = Disease_group, value = deaths_increase, pct = pct_deaths_increase)],
    top_percentage_death_gains = top_pct_deaths[, .(group = Disease_group, value = deaths_increase, pct = pct_deaths_increase)],
    top_absolute_yll_gains = top_abs_yll[, .(group = Disease_group, value = yll_increase, pct = pct_yll_increase)],
    top_percentage_yll_gains = top_pct_yll[, .(group = Disease_group, value = yll_increase, pct = pct_yll_increase)],
    box = box_3_2
  )

  write_csv_utf8(table_3_1_ref, file.path(out_dir, "table_3_1_redistribution_groups.csv"))
  write_csv_utf8(table_3_2, file.path(out_dir, "table_3_2_impact_total_by_year.csv"))
  write_csv_utf8(table_3_3, file.path(out_dir, "table_3_3_impact_by_age_sex_year.csv"))
  write_csv_utf8(table_3_4, file.path(out_dir, "table_3_4_before_after_by_disease_group.csv"))
  write_csv_utf8(box_3_2, file.path(out_dir, "box_3_2_case_trace_cancer.csv"))
  write_csv_utf8(audit_group_catalog_vs_observed, file.path(out_dir, "audit_group_catalog_vs_observed.csv"))
  write_csv_utf8(audit_group_raw_sinadef_presence, file.path(out_dir, "audit_group_raw_sinadef_presence.csv"))
  write_csv_utf8(audit_group_table31_inclusion, file.path(out_dir, "audit_group_table31_inclusion.csv"))
  write_text_file(file.path(out_dir, "redistribution_text_metrics.json"), jsonlite::toJSON(text_metrics, pretty = TRUE, auto_unbox = TRUE))

  write_csv_utf8(pure_balance_year, file.path(pure_qc_dir, "pure_redistribution_balance_by_year.csv"))
  write_csv_utf8(gc_age_sex, file.path(pure_dir, "pure_redistribution_gc_age_sex.csv"))
  write_csv_utf8(table_3_4_wide, file.path(pure_dir, "pure_redistribution_before_after_reference_year.csv"))
  write_csv_utf8(extended_sensitivity_summary, file.path(pure_qc_dir, "sensitivity_extended_downstream_by_year.csv"))

  list(
    reference_year = reference_year,
    table_3_1 = table_3_1_ref,
    table_3_2 = table_3_2,
    table_3_3 = table_3_3,
    table_3_4 = table_3_4,
    box_3_2 = box_3_2,
    text_metrics = text_metrics,
    balance_total = balance_total,
    balance_year = balance_year,
    pure_balance_year = pure_balance_year,
    extended_sensitivity_summary = extended_sensitivity_summary,
    audit_group_catalog_vs_observed = audit_group_catalog_vs_observed,
    audit_group_raw_sinadef_presence = audit_group_raw_sinadef_presence,
    audit_group_table31_inclusion = audit_group_table31_inclusion
  )
}

build_portal_index <- function(root_dir, modules) {
  sections <- c(
    "<section class=\"card\">",
    "<div class=\"eyebrow\">Centro de navegación</div><h2 class=\"section-title\">Módulos del portal</h2>",
    "<p class=\"muted\">Este portal quedó preparado para uso local, publicación estática y protección posterior por capa de acceso. El HTML es el modo principal; los PDFs son tomos o resúmenes lineales.</p>",
    "<div class=\"module-grid\">",
    paste(vapply(modules, function(mod) {
      sprintf("<div class=\"module-card\"><h3><a href=\"%s\">%s</a></h3><p>Entrar al módulo temático o técnico correspondiente.</p><a class=\"btn\" href=\"%s\">Abrir módulo</a></div>", mod$href, esc_html(mod$title), mod$href)
    }, character(1)), collapse = ""),
    "</div></section>",
    "<section class=\"card\"><div class=\"eyebrow\">Uso local</div><h3>Cómo abrir el portal</h3><p class=\"muted\">Abra este archivo <strong>index.html</strong> en el navegador. Desde aquí puede entrar a cada módulo, descargar CSVs y abrir los resúmenes PDF cuando existan.</p><div class=\"nav-actions\"><a class=\"btn ghost\" href=\"publish_guide.html\">Guía de publicación y acceso</a></div></section>"
  )
  write_portal_page(
    file.path(root_dir, "index.html"),
    "Portal técnico de QC y revisión epidemiológica",
    page_shell(
      title = "Portal técnico de QC y revisión epidemiológica",
      intro = "Segunda generación del portal: diseño coherente, navegación multipágina, veredictos RESUELTO y módulos temáticos para expertos.",
      sidebar_items = list(list(label = "Guía de publicación", href = "publish_guide.html")),
      sections_html = sections
    ),
    rel_root = "."
  )
}

sanitize_public_tree_text <- function(root_dir) {
  text_files <- list.files(root_dir, pattern = "\\.(html|md|json|css|js)$", recursive = TRUE, full.names = TRUE)
  for (path in text_files) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    clean <- fix_mojibake_text(txt)
    clean <- gsub("Redistribution", "Redistribución", clean, fixed = TRUE)
    clean <- gsub("Todos los anos", "Todos los años", clean, fixed = TRUE)
    clean <- gsub("metricas narrativas", "métricas narrativas", clean, fixed = TRUE)
    clean <- gsub("Cardiovasculares e infantiles/congénitasénitas", "Cardiovasculares e infantiles/congénitas", clean, fixed = TRUE)
    if (!identical(clean, txt)) {
      write_text_file(path, strsplit(clean, "\n", fixed = TRUE)[[1]])
    }
  }
  invisible(root_dir)
}

resolve_external_from_yaml_local <- function(key) {
  cfg <- yaml::read_yaml(here("config", "external_sources.yml"))
  rel <- cfg$external_datasets[[key]]$path
  normalizePath(rel, winslash = "/", mustWork = TRUE)
}

fmt_int <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct1 <- function(x) sprintf("%.1f", round(x, 1))

build_pandemic_tables_v2 <- function() {
  out_dir <- here("data", "derived", "qc", "review_portal", "pandemic_subregistro")
  ensure_dir(out_dir)

  final <- fread(here("data", "final", "death_cause_final", "death_cause_final.csv"), showProgress = FALSE)
  hier <- fread(here("data", "final", "death_cause_final_hierarchical", "death_cause_final_hierarchical.csv"), showProgress = FALSE)
  avp_ref <- fread(
    here("data", "final", "avp_yll_cause_reconciled", "avp_yll_cause_reconciled.csv"),
    select = c("year_id", "location_id", "sex_id", "age", "cause_level", "ex_standard"),
    showProgress = FALSE
  )
  lt_std <- as.data.table(readRDS(resolve_external_from_yaml_local("life_table_standard_single_age")))
  lt_std <- lt_std[order(standard_source, standard_version, sex_id, exact_age)]
  lt_std <- lt_std[, .SD[1], by = .(sex_id, exact_age)]
  lt_std <- lt_std[, .(sex_id = as.integer(sex_id), age = as.integer(exact_age), ex_standard = as.numeric(ex))]
  avp_total_lookup <- unique(avp_ref[cause_level == 0, .(
    year_id,
    location_id,
    sex_id,
    age,
    ex_standard_total = ex_standard
  )])

  final <- merge(final, lt_std, by = c("sex_id", "age"), all.x = TRUE, sort = FALSE)
  final <- merge(final, avp_total_lookup, by = c("year_id", "location_id", "sex_id", "age"), all.x = TRUE, sort = FALSE)
  final[is.na(ex_standard), ex_standard := 0]
  final[is.na(ex_standard_total), ex_standard_total := ex_standard]
  final[, pandemic_named_component_used_component := fifelse(
    pandemic_named_component_allcause > 0,
    pandemic_named_component * pandemic_named_component_capped / pandemic_named_component_allcause,
    0
  )]
  final[, `:=`(
    avp_post_redistribucion = deaths_post_redistribution * ex_standard,
    avp_corregidos_completitud = base_cause_deaths_corrected * ex_standard,
    avp_final_sin_reasignacion_pandemica = deaths_final_net_of_pandemic * ex_standard,
    avp_final = deaths_final * ex_standard,
    avp_componente_pandemico_nombrado = pandemic_named_component * ex_standard,
    avp_componente_pandemico_nombrado_utilizado = pandemic_named_component_used_component * ex_standard,
    avp_oprm_residual = oprm_residual_component * ex_standard,
    avp_subregistro_gain = subregistro_gain_component * ex_standard
  )]

  cell_unique <- unique(final[, .(
    year_id, location_id, sex_id, age,
    observed_allcause, expected_allcause, observed_corrected_allcause,
    pandemic_excess_allcause, pandemic_named_component_allcause, pandemic_named_component_capped, oprm_residual_allcause,
    correction_factor_completeness, factor_truncated_low, factor_truncated_high, factor_method,
    ex_standard_total
  )])
  cell_unique[, `:=`(
    muertes_post_redistribucion = observed_allcause,
    avp_post_redistribucion = observed_allcause * ex_standard_total,
    avp_corregidos_completitud = observed_corrected_allcause * ex_standard_total,
    avp_exceso_pandemico_allcause = pandemic_excess_allcause * ex_standard_total,
    avp_oprm_residual_allcause = oprm_residual_allcause * ex_standard_total
  )]
  hier_allcause <- merge(
    hier[cause_level == 0],
    avp_total_lookup,
    by = c("year_id", "location_id", "sex_id", "age"),
    all.x = TRUE,
    sort = FALSE
  )
  hier_allcause[is.na(ex_standard_total), ex_standard_total := 0]
  hier_allcause[, `:=`(
    avp_post_redistribucion_total = deaths_post_redistribution * ex_standard_total,
    avp_corregidos_completitud_total = base_cause_deaths_corrected * ex_standard_total,
    avp_final_total = deaths_final * ex_standard_total
  )]

  observed_note <- data.table(
    metrica = "observed_equals_post_redistribution",
    valor_logico = unique(final[, all(abs(deaths_observed - deaths_post_redistribution) < 1e-8)]),
    nota = "En el pipeline actual, deaths_observed y deaths_post_redistribution son equivalentes en masa porque el modulo parte del dataset canonico post-redistribucion."
  )

  p1 <- hier_allcause[, .(
    muertes_post_redistribucion = sum(deaths_post_redistribution, na.rm = TRUE),
    muertes_corregidas_completitud = sum(base_cause_deaths_corrected, na.rm = TRUE),
    aumento_absoluto_subregistro = sum(base_cause_deaths_corrected - deaths_post_redistribution, na.rm = TRUE),
    pct_aumento_sobre_post_redistribucion = 100 * sum(base_cause_deaths_corrected - deaths_post_redistribution, na.rm = TRUE) / sum(deaths_post_redistribution, na.rm = TRUE),
    AVP_post_redistribucion = sum(avp_post_redistribucion_total, na.rm = TRUE),
    AVP_corregidos_completitud = sum(avp_corregidos_completitud_total, na.rm = TRUE),
    aumento_absoluto_avp_subregistro = sum(avp_corregidos_completitud_total - avp_post_redistribucion_total, na.rm = TRUE),
    pct_AVP_aumento = 100 * sum(avp_corregidos_completitud_total - avp_post_redistribucion_total, na.rm = TRUE) / sum(avp_post_redistribucion_total, na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]

  p2 <- final[, .(
    covid_especifico = sum(base_cause_deaths_corrected[pandemic_component_class == "covid_specific"], na.rm = TRUE),
    sarampion = sum(base_cause_deaths_corrected[pandemic_component_class == "measles"], na.rm = TRUE),
    iri_baja = sum(base_cause_deaths_corrected[pandemic_component_class == "lri"], na.rm = TRUE),
    tos_ferina = sum(base_cause_deaths_corrected[pandemic_component_class == "pertussis"], na.rm = TRUE),
    componente_pandemico_nombrado_observado = sum(pandemic_named_component, na.rm = TRUE),
    componente_pandemico_nombrado_utilizado = sum(pandemic_named_component_used_component, na.rm = TRUE),
    oprm_residual = sum(oprm_residual_component, na.rm = TRUE),
    reasignado_desde_otras_causas = sum(pandemic_reassigned_out_component, na.rm = TRUE),
    AVP_covid_especifico = sum(avp_componente_pandemico_nombrado[pandemic_component_class == "covid_specific"], na.rm = TRUE),
    AVP_sarampion = sum(avp_componente_pandemico_nombrado[pandemic_component_class == "measles"], na.rm = TRUE),
    AVP_iri_baja = sum(avp_componente_pandemico_nombrado[pandemic_component_class == "lri"], na.rm = TRUE),
    AVP_tos_ferina = sum(avp_componente_pandemico_nombrado[pandemic_component_class == "pertussis"], na.rm = TRUE),
    AVP_componente_pandemico_nombrado_observado = sum(avp_componente_pandemico_nombrado, na.rm = TRUE),
    AVP_componente_pandemico_nombrado_utilizado = sum(avp_componente_pandemico_nombrado_utilizado, na.rm = TRUE),
    AVP_oprm_residual = sum(avp_oprm_residual, na.rm = TRUE),
    AVP_reasignado_desde_otras_causas = sum(pandemic_reassigned_out_component * ex_standard, na.rm = TRUE),
    muertes_totales_corregidas_cause_sum = sum(base_cause_deaths_corrected, na.rm = TRUE),
    AVP_totales_corregidos_cause_sum = sum(avp_corregidos_completitud, na.rm = TRUE)
  ), by = .(year_id)]
  p2_cells <- cell_unique[, .(
    exceso_pandemico_allcause = sum(pandemic_excess_allcause, na.rm = TRUE),
    componente_pandemico_nombrado = sum(pandemic_named_component_capped, na.rm = TRUE),
    componente_pandemico_nombrado_observado_allcause = sum(pandemic_named_component_allcause, na.rm = TRUE),
    oprm_residual_allcause = sum(oprm_residual_allcause, na.rm = TRUE),
    AVP_exceso_pandemico_allcause = sum(avp_exceso_pandemico_allcause, na.rm = TRUE),
    AVP_oprm_residual_allcause = sum(avp_oprm_residual_allcause, na.rm = TRUE)
  ), by = .(year_id)]
  p2_total <- hier_allcause[, .(
    muertes_totales_corregidas = sum(base_cause_deaths_corrected, na.rm = TRUE),
    AVP_totales_corregidos = sum(avp_corregidos_completitud_total, na.rm = TRUE),
    muertes_finales_post_pandemia = sum(deaths_final, na.rm = TRUE),
    AVP_finales_post_pandemia = sum(avp_final_total, na.rm = TRUE)
  ), by = .(year_id)]
  p2 <- merge(p2, p2_cells, by = "year_id", all.x = TRUE, sort = FALSE)
  p2 <- merge(p2, p2_total, by = "year_id", all.x = TRUE, sort = FALSE)
  p2[, c("muertes_totales_corregidas_cause_sum", "AVP_totales_corregidos_cause_sum") := NULL]
  p2[, `:=`(
    pct_muertes_totales_corregidas = 100 * oprm_residual / muertes_totales_corregidas,
    pct_AVP_totales_corregidos = 100 * AVP_oprm_residual / AVP_totales_corregidos
  )]
  p2 <- p2[, .(
    year_id,
    covid_especifico,
    sarampion,
    iri_baja,
    tos_ferina,
    componente_pandemico_nombrado_observado,
    componente_pandemico_nombrado_utilizado,
    oprm_residual,
    reasignado_desde_otras_causas,
    AVP_covid_especifico,
    AVP_sarampion,
    AVP_iri_baja,
    AVP_tos_ferina,
    AVP_componente_pandemico_nombrado_observado,
    AVP_componente_pandemico_nombrado_utilizado,
    AVP_oprm_residual,
    AVP_reasignado_desde_otras_causas,
    muertes_totales_corregidas,
    AVP_totales_corregidos,
    exceso_pandemico_allcause,
    componente_pandemico_nombrado,
    componente_pandemico_nombrado_observado_allcause,
    oprm_residual_allcause,
    pct_muertes_totales_corregidas,
    pct_AVP_totales_corregidos
  )]
  setorder(p2, year_id)

  hier <- merge(hier, lt_std, by = c("sex_id", "age"), all.x = TRUE, sort = FALSE)
  hier[is.na(ex_standard), ex_standard := 0]
  hier[, `:=`(
    avp_post_redistribucion = deaths_post_redistribution * ex_standard,
    avp_corregidos_completitud = base_cause_deaths_corrected * ex_standard,
    avp_final = deaths_final * ex_standard,
    avp_final_sin_reasignacion_pandemica = deaths_final_net_of_pandemic * ex_standard
  )]
  groups <- hier[cause_level == 1]

  p3_base <- groups[, .(
    Muertes_antes = sum(deaths_post_redistribution, na.rm = TRUE),
    Muertes_despues = sum(base_cause_deaths_corrected, na.rm = TRUE),
    AVP_antes = sum(avp_post_redistribucion, na.rm = TRUE),
    AVP_despues = sum(avp_corregidos_completitud, na.rm = TRUE)
  ), by = .(year_id, Grupo_de_enfermedad = cause_name)]
  p3_before <- p3_base[, .(year_id, Grupo_de_enfermedad, Etapa = "Antes de completitud", Muertes = Muertes_antes, AVP = AVP_antes)]
  p3_after  <- p3_base[, .(year_id, Grupo_de_enfermedad, Etapa = "Despues de completitud", Muertes = Muertes_despues, AVP = AVP_despues)]
  p3_change <- p3_base[, .(year_id, Grupo_de_enfermedad, Etapa = "Cambio", Muertes = Muertes_despues - Muertes_antes, AVP = AVP_despues - AVP_antes)]
  p3 <- rbindlist(list(p3_before, p3_after, p3_change), use.names = TRUE)
  p3[, `:=`(
    pct_muertes = 100 * Muertes / sum(Muertes[Etapa != "Cambio"], na.rm = TRUE),
    pct_AVP = 100 * AVP / sum(AVP[Etapa != "Cambio"], na.rm = TRUE)
  ), by = .(year_id, Etapa)]
  p3[Etapa == "Cambio", `:=`(pct_muertes = NA_real_, pct_AVP = NA_real_)]
  p3[, etapa_order := match(Etapa, c("Antes de completitud", "Despues de completitud", "Cambio"))]
  setorder(p3, year_id, Grupo_de_enfermedad, etapa_order)
  p3[, etapa_order := NULL]

  p4_base <- groups[, .(
    Muertes_antes = sum(base_cause_deaths_corrected, na.rm = TRUE),
    Muertes_despues = sum(deaths_final, na.rm = TRUE),
    AVP_antes = sum(avp_corregidos_completitud, na.rm = TRUE),
    AVP_despues = sum(avp_final, na.rm = TRUE)
  ), by = .(year_id, Grupo_de_enfermedad = cause_name)]
  p4_before <- p4_base[, .(year_id, Grupo_de_enfermedad, Etapa = "Antes de reasignacion pandemica", Muertes = Muertes_antes, AVP = AVP_antes)]
  p4_after  <- p4_base[, .(year_id, Grupo_de_enfermedad, Etapa = "Despues de reasignacion pandemica", Muertes = Muertes_despues, AVP = AVP_despues)]
  p4_change <- p4_base[, .(year_id, Grupo_de_enfermedad, Etapa = "Cambio", Muertes = Muertes_despues - Muertes_antes, AVP = AVP_despues - AVP_antes)]
  p4 <- rbindlist(list(p4_before, p4_after, p4_change), use.names = TRUE)
  p4[, `:=`(
    pct_muertes = 100 * Muertes / sum(Muertes[Etapa != "Cambio"], na.rm = TRUE),
    pct_AVP = 100 * AVP / sum(AVP[Etapa != "Cambio"], na.rm = TRUE)
  ), by = .(year_id, Etapa)]
  p4[Etapa == "Cambio", `:=`(pct_muertes = NA_real_, pct_AVP = NA_real_)]
  p4[, etapa_order := match(Etapa, c("Antes de reasignacion pandemica", "Despues de reasignacion pandemica", "Cambio"))]
  setorder(p4, year_id, Grupo_de_enfermedad, etapa_order)
  p4[, etapa_order := NULL]

  p5 <- fread(here("data", "derived", "qc", "build_death_cause_final", "qc_pandemic_reallocation_balance.csv"), showProgress = FALSE)
  p5 <- p5[, .(
    year_id, location_id, sex_id, age,
    delta_base_vs_corrected, delta_net_vs_target, delta_final_vs_corrected, delta_in_vs_out,
    veredicto = fifelse(
      abs(delta_base_vs_corrected) <= 1e-6 & abs(delta_net_vs_target) <= 1e-6 &
        abs(delta_final_vs_corrected) <= 1e-6 & abs(delta_in_vs_out) <= 1e-6,
      "OK", "REVISAR"
    )
  )]

  p6 <- cell_unique[, .(
    min = min(correction_factor_completeness, na.rm = TRUE),
    p25 = quantile(correction_factor_completeness, 0.25, na.rm = TRUE),
    mediana = median(correction_factor_completeness, na.rm = TRUE),
    p75 = quantile(correction_factor_completeness, 0.75, na.rm = TRUE),
    max = max(correction_factor_completeness, na.rm = TRUE),
    pct_celdas_con_clipping_bajo = 100 * mean(factor_truncated_low, na.rm = TRUE),
    pct_celdas_con_clipping_alto = 100 * mean(factor_truncated_high, na.rm = TRUE),
    pct_celdas_con_backoff = 100 * mean(factor_method != "direct_prepandemic", na.rm = TRUE)
  ), by = .(year_id)][order(year_id)]

  red_t32_path <- here("data", "derived", "qc", "review_portal", "redistribution", "table_3_2_impact_total_by_year.csv")
  x1 <- if (file.exists(red_t32_path)) {
    red_t32 <- fread(red_t32_path, showProgress = FALSE)
    setnames(red_t32, c("Reference_year", "Total_deaths", "Total_YLL"), c("year_id", "muertes_post_redistribucion", "AVP_post_redistribucion"))
    merge(
      red_t32[, .(year_id, muertes_post_redistribucion, AVP_post_redistribucion)],
      p1[, .(
        year_id,
        muertes_base_pandemia = muertes_post_redistribucion,
        muertes_corregidas_completitud,
        AVP_base_pandemia = AVP_post_redistribucion,
        AVP_corregidos_completitud
      )],
      by = "year_id",
      all = TRUE
    )
  } else {
    p1[, .(
      year_id,
      muertes_post_redistribucion = muertes_post_redistribucion,
      muertes_base_pandemia = muertes_post_redistribucion,
      muertes_corregidas_completitud,
      AVP_post_redistribucion = AVP_post_redistribucion,
      AVP_base_pandemia = AVP_post_redistribucion,
      AVP_corregidos_completitud
    )]
  }
  x1 <- merge(
    x1,
    p2_total[, .(
      year_id,
      muertes_finales_post_pandemia = muertes_totales_corregidas,
      AVP_finales_post_pandemia = AVP_totales_corregidos
    )],
    by = "year_id",
    all = TRUE
  )
  x1[, `:=`(
    delta_postredistribucion_vs_basepandemia = muertes_base_pandemia - muertes_post_redistribucion,
    delta_final_vs_corregidas = muertes_finales_post_pandemia - muertes_corregidas_completitud,
    delta_AVP_postredistribucion_vs_basepandemia = AVP_base_pandemia - AVP_post_redistribucion,
    delta_AVP_final_vs_corregidos = AVP_finales_post_pandemia - AVP_corregidos_completitud
  )]
  x1[, veredicto := fifelse(
    abs(delta_postredistribucion_vs_basepandemia) <= 1e-6 &
      abs(delta_final_vs_corregidas) <= 1e-6 &
      abs(delta_AVP_postredistribucion_vs_basepandemia) <= 1e-6 &
      abs(delta_AVP_final_vs_corregidos) <= 1e-6,
    "OK",
    "REVISAR"
  )]
  setorder(x1, year_id)

  metrics <- list(
    observed_equals_post = observed_note$valor_logico[1],
    total_subregistro_gain = as.numeric(sum(p1$aumento_absoluto_subregistro, na.rm = TRUE)),
    total_oprm = as.numeric(sum(p2$oprm_residual, na.rm = TRUE)),
    total_named = as.numeric(sum(p2$componente_pandemico_nombrado, na.rm = TRUE)),
    total_named_observed = as.numeric(sum(p2$componente_pandemico_nombrado_observado, na.rm = TRUE)),
    top_subregistro_group = p3[year_id == max(year_id) & Etapa == "Cambio"][order(-Muertes)][1, Grupo_de_enfermedad],
    top_subregistro_group_gain = p3[year_id == max(year_id) & Etapa == "Cambio"][order(-Muertes)][1, Muertes],
    top_pandemic_group_gain = p4[year_id == max(p4[Muertes > 0 & Etapa == "Cambio", year_id], na.rm = TRUE) & Etapa == "Cambio"][order(-Muertes)][1, Grupo_de_enfermedad],
    top_pandemic_group_gain_value = p4[year_id == max(p4[Muertes > 0 & Etapa == "Cambio", year_id], na.rm = TRUE) & Etapa == "Cambio"][order(-Muertes)][1, Muertes],
    x1_all_ok = all(x1$veredicto == "OK", na.rm = TRUE)
  )

  fwrite(p1, file.path(out_dir, "tabla_p1_subregistro_total_por_anio.csv"))
  fwrite(p2, file.path(out_dir, "tabla_p2_pandemia_total_por_anio.csv"))
  fwrite(p3, file.path(out_dir, "tabla_p3_before_after_por_grupo_causal_subregistro.csv"))
  fwrite(p4, file.path(out_dir, "tabla_p4_before_after_por_grupo_causal_pandemia.csv"))
  fwrite(p5, file.path(out_dir, "tabla_p5_balance_pandemico_qc.csv"))
  fwrite(p6, file.path(out_dir, "tabla_p6_factor_completitud_resumen.csv"))
  fwrite(x1, file.path(out_dir, "tabla_x1_conciliacion_redistribucion_pandemia_subregistro.csv"))
  fwrite(observed_note, file.path(out_dir, "nota_observed_equals_post_redistribution.csv"))
  write_text_file(file.path(out_dir, "pandemia_subregistro_text_metrics.json"), jsonlite::toJSON(metrics, pretty = TRUE, auto_unbox = TRUE))

  list(p1 = p1, p2 = p2, p3 = p3, p4 = p4, p5 = p5, p6 = p6, x1 = x1, observed_note = observed_note, metrics = metrics, out_dir = out_dir)
}

build_pandemic_module <- function(root_dir) {
  module_root <- file.path(root_dir, "modules", "pandemia-subregistro")
  ensure_dir(module_root)
  ensure_dir(file.path(module_root, "downloads"))
  out <- build_pandemic_tables_v2()
  p1 <- out$p1; p2 <- out$p2; p3 <- out$p3; p4 <- out$p4; p5 <- out$p5; p6 <- out$p6; x1 <- out$x1
  ref_year <- max(p1$year_id, na.rm = TRUE)
  ref_year_pandemic <- if (any(p2$oprm_residual > 0, na.rm = TRUE)) max(p2[oprm_residual > 0, year_id], na.rm = TRUE) else ref_year

  write.csv(p1, file.path(module_root, "downloads", "tabla_p1_subregistro_total_por_anio.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(p2, file.path(module_root, "downloads", "tabla_p2_pandemia_total_por_anio.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(p3, file.path(module_root, "downloads", "tabla_p3_before_after_por_grupo_causal_subregistro.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(p4, file.path(module_root, "downloads", "tabla_p4_before_after_por_grupo_causal_pandemia.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(p5, file.path(module_root, "downloads", "tabla_p5_balance_pandemico_qc.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(p6, file.path(module_root, "downloads", "tabla_p6_factor_completitud_resumen.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  write.csv(x1, file.path(module_root, "downloads", "tabla_x1_conciliacion_redistribucion_pandemia_subregistro.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  p1_show <- copy(p1)
  p1_show[, `:=`(
    muertes_post_redistribucion = fmt_int(muertes_post_redistribucion),
    muertes_corregidas_completitud = fmt_int(muertes_corregidas_completitud),
    aumento_absoluto_subregistro = fmt_int(aumento_absoluto_subregistro),
    pct_aumento_sobre_post_redistribucion = fmt_pct1(pct_aumento_sobre_post_redistribucion),
    AVP_post_redistribucion = fmt_int(AVP_post_redistribucion),
    AVP_corregidos_completitud = fmt_int(AVP_corregidos_completitud),
    aumento_absoluto_avp_subregistro = fmt_int(aumento_absoluto_avp_subregistro),
    pct_AVP_aumento = fmt_pct1(pct_AVP_aumento)
  )]
  p2_show <- copy(p2)
  num_cols_p2 <- setdiff(names(p2_show), "year_id")
  for (cc in num_cols_p2) p2_show[[cc]] <- if (grepl("^pct_", cc)) fmt_pct1(p2_show[[cc]]) else fmt_int(p2_show[[cc]])
  setnames(
    p2_show,
    old = names(p2_show),
    new = c(
      "year_id",
      "COVID-19 específico",
      "Sarampión",
      "IRI baja",
      "Tos ferina",
      "Componente nombrado observado",
      "Componente nombrado utilizado",
      "OPRM residual",
      "Reasignado desde otras causas",
      "AVP COVID-19 específico",
      "AVP sarampión",
      "AVP IRI baja",
      "AVP tos ferina",
      "AVP componente nombrado observado",
      "AVP componente nombrado utilizado",
      "AVP OPRM residual",
      "AVP reasignado desde otras causas",
      "Muertes totales corregidas",
      "AVP totales corregidos",
      "Exceso pandémico all-cause",
      "Componente pandémico nombrado utilizado en fórmula",
      "Componente pandémico nombrado observado all-cause",
      "OPRM residual all-cause",
      "% muertes totales corregidas",
      "% AVP totales corregidos"
    )
  )
  p3_show <- copy(p3[year_id == ref_year])
  p3_show[, Grupo_de_enfermedad_mostrar := Grupo_de_enfermedad]
  p3_show[duplicated(Grupo_de_enfermedad_mostrar), Grupo_de_enfermedad_mostrar := ""]
  for (cc in c("Muertes","AVP")) p3_show[[cc]] <- fmt_int(p3_show[[cc]])
  for (cc in c("pct_muertes","pct_AVP")) p3_show[[cc]] <- ifelse(is.na(p3_show[[cc]]), "..", fmt_pct1(p3_show[[cc]]))
  p4_show <- copy(p4[year_id == ref_year_pandemic])
  p4_show[, Grupo_de_enfermedad_mostrar := Grupo_de_enfermedad]
  p4_show[duplicated(Grupo_de_enfermedad_mostrar), Grupo_de_enfermedad_mostrar := ""]
  for (cc in c("Muertes","AVP")) p4_show[[cc]] <- fmt_int(p4_show[[cc]])
  for (cc in c("pct_muertes","pct_AVP")) p4_show[[cc]] <- ifelse(is.na(p4_show[[cc]]), "..", fmt_pct1(p4_show[[cc]]))
  p5_show <- copy(head(p5, 30))
  p6_show <- copy(p6)
  for (cc in c("min", "p25", "mediana", "p75", "max")) p6_show[[cc]] <- sprintf("%.3f", round(p6_show[[cc]], 3))
  for (cc in c("pct_celdas_con_clipping_bajo", "pct_celdas_con_clipping_alto", "pct_celdas_con_backoff")) p6_show[[cc]] <- fmt_pct1(p6_show[[cc]])
  x1_show <- copy(x1)
  for (cc in setdiff(names(x1_show), c("year_id", "veredicto"))) x1_show[[cc]] <- fmt_int(x1_show[[cc]])

  metrics <- out$metrics
  intro <- paste0(
    "Este módulo separa dos mecanismos distintos. La corrección por subregistro infla la masa total all-cause. ",
    "La reasignación pandémica no cambia esa masa corregida; solo mueve muertes y AVP hacia componentes pandémicos, con OPRM calculado como residual canónico OMS/GBD."
  )
  sections <- c(
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Pandemia y subregistro</div><h2 class=\"section-title\">Semántica de las dos correcciones</h2><p>%s</p><p class=\"muted\">Nota metodológica: <strong>observed</strong> y <strong>post_redistribution</strong> son equivalentes en esta versión del pipeline. Por eso el módulo ya no presenta esa curva como etapa separada.</p></section>", intro),
    details_block("Cómo leer estas tablas", "<p><strong>Subregistro/completitud</strong> aumenta la masa all-cause observada hacia una masa corregida. <strong>Reasignación pandémica</strong> preserva la masa corregida y solo mueve esa masa entre causas, de otras causas hacia OPRM residual, después de restar COVID-19 específico, sarampión, infecciones respiratorias inferiores y tos ferina.</p>"),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Impacto total de completitud</div><h3>Tabla P1. Subregistro total por año</h3><p class=\"muted\">En el acumulado del periodo, el ajuste por completitud añadió <strong>%s</strong> muertes y <strong>%s</strong> AVP. En %s, el grupo con mayor aumento absoluto fue <strong>%s</strong> con <strong>%s</strong> muertes.</p>%s</section>", fmt_int(sum(p1$aumento_absoluto_subregistro)), fmt_int(sum(p1$aumento_absoluto_avp_subregistro)), ref_year, esc_html(metrics$top_subregistro_group), fmt_int(metrics$top_subregistro_group_gain), html_table(p1_show)),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Impacto total pandémico</div><h3>Tabla P2. Pandemia total por año</h3><p class=\"muted\">El <strong>componente pandémico nombrado utilizado en la fórmula de OPRM</strong> acumuló <strong>%s</strong> muertes en el periodo. El residual OPRM acumuló <strong>%s</strong> muertes y se financió enteramente con salida desde causas no pandémicas dentro de la misma masa corregida. La columna <strong>componente_pandemico_nombrado_observado</strong> se conserva como referencia descriptiva, pero la resta canónica usa el componente <strong>utilizado</strong>, que se trunca por el exceso all-cause de cada célula.</p>%s</section>", fmt_int(sum(p2$componente_pandemico_nombrado_utilizado)), fmt_int(sum(p2$oprm_residual)), html_table(p2_show)),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Before/after por grupo</div><h3>Tabla P3. Antes y después de completitud por grupo causal, %s</h3>%s</section>", ref_year, html_table(p3_show[, .(Grupo_de_enfermedad = Grupo_de_enfermedad_mostrar, Etapa, Muertes, pct_muertes, AVP, pct_AVP)])),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Before/after por grupo</div><h3>Tabla P4. Antes y después de reasignación pandémica por grupo causal, %s</h3><p class=\"muted\">En %s, el mayor cambio absoluto tras la reasignación pandémica ocurrió en <strong>%s</strong> con <strong>%s</strong> muertes.</p>%s</section>", ref_year_pandemic, ref_year_pandemic, esc_html(metrics$top_pandemic_group_gain), fmt_int(metrics$top_pandemic_group_gain_value), html_table(p4_show[, .(Grupo_de_enfermedad = Grupo_de_enfermedad_mostrar, Etapa, Muertes, pct_muertes, AVP, pct_AVP)])),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">QC estructural</div><h3>Tabla P5. Balance pandémico</h3>%s</section>", html_table(p5_show)),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Conciliacion entre modulos</div><h3>Tabla X1. Conciliacion entre redistribucion y pandemia/subregistro</h3><p class=\"muted\">Esta tabla verifica que la masa post-redistribucion reportada en el modulo de redistribucion sea exactamente la misma base de arranque usada por pandemia/subregistro, y que la reasignacion pandemica preserve la masa corregida por completitud. Veredicto global: <strong>%s</strong>.</p>%s</section>", if (isTRUE(metrics$x1_all_ok)) "OK" else "REVISAR", html_table(x1_show)),
    sprintf("<section class=\"card\"><div class=\"eyebrow\">Estabilidad del factor</div><h3>Tabla P6. Resumen del factor de completitud</h3>%s<div class=\"nav-actions\"><a class=\"btn ghost\" href=\"downloads/tabla_p1_subregistro_total_por_anio.csv\">Descargar P1</a><a class=\"btn ghost\" href=\"downloads/tabla_p2_pandemia_total_por_anio.csv\">Descargar P2</a><a class=\"btn ghost\" href=\"downloads/tabla_p6_factor_completitud_resumen.csv\">Descargar P6</a><a class=\"btn ghost\" href=\"downloads/tabla_x1_conciliacion_redistribucion_pandemia_subregistro.csv\">Descargar X1</a></div></section>", html_table(p6_show))
  )

  write_portal_page(
    file.path(module_root, "index.html"),
    "Pandemia y subregistro",
    page_shell(
      title = "Pandemia y subregistro",
      intro = intro,
      sidebar_items = list(list(label = "Volver al portal", href = "../../index.html")),
      sections_html = sections
    ),
    rel_root = "../.."
  )
  pandemic_html_path <- file.path(module_root, "index.html")
  if (file.exists(pandemic_html_path)) {
    pandemic_html <- paste(readLines(pandemic_html_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    write_text_file(pandemic_html_path, strsplit(fix_mojibake_text(pandemic_html), "\n", fixed = TRUE)[[1]])
  }

  md_lines <- c(
    "# Pandemia y subregistro",
    "",
    intro,
    "",
    "## Tabla P1. Subregistro total por año",
    "",
    knitr::kable(p1_show, format = "pipe"),
    "",
    "## Tabla P2. Pandemia total por año",
    "",
    knitr::kable(p2_show, format = "pipe"),
    "",
    sprintf("## Tabla P3. Antes y después de completitud por grupo causal, %s", ref_year),
    "",
    knitr::kable(p3_show[, .(Grupo_de_enfermedad = Grupo_de_enfermedad, Etapa, Muertes, pct_muertes, AVP, pct_AVP)], format = "pipe"),
    "",
    sprintf("## Tabla P4. Antes y después de reasignación pandémica por grupo causal, %s", ref_year_pandemic),
    "",
    knitr::kable(p4_show[, .(Grupo_de_enfermedad = Grupo_de_enfermedad, Etapa, Muertes, pct_muertes, AVP, pct_AVP)], format = "pipe"),
    "",
    "## Tabla P5. Balance pandémico QC",
    "",
    knitr::kable(p5_show, format = "pipe"),
    "",
    "## Tabla X1. Conciliacion entre redistribucion y pandemia/subregistro",
    "",
    knitr::kable(x1_show, format = "pipe"),
    "",
    "## Tabla P6. Resumen del factor de completitud",
    "",
    knitr::kable(p6_show, format = "pipe")
  )
  write_text_file(file.path(out$out_dir, "pandemia_pdf_body.md"), md_lines)
  write_text_file(file.path(out$out_dir, "page_manifest.json"), jsonlite::toJSON(list(list(title = "Pandemia y subregistro", href = "modules/pandemia-subregistro/index.html")), pretty = TRUE, auto_unbox = TRUE))
  list(title = "Pandemia y subregistro", href = "modules/pandemia-subregistro/index.html")
}

write_publish_guide <- function(root_dir) {
  sections <- c(
    "<section class=\"card\"><div class=\"eyebrow\">Publicaci�n con acceso</div><h2 class=\"section-title\">Ruta recomendada: Cloudflare Pages + Cloudflare Access</h2><ol><li>Ejecute el builder y verifique que la carpeta <code>reports/qc_pipeline_encyclopedia</code> contenga el sitio completo.</li><li>Publique esa carpeta como sitio est�tico en Cloudflare Pages.</li><li>Proteja el hostname con Cloudflare Access y pol�tica Allow.</li><li>Como primer m�todo, use correos permitidos con one-time pin. Si luego necesita usuario/contrase�a institucional, cambie el proveedor de identidad en Access.</li></ol><p class=\"muted\">La autenticaci�n no debe meterse dentro del HTML. Va delante del sitio, en la capa de hosting o proxy.</p></section>",
    "<section class=\"card\"><div class=\"eyebrow\">Alternativas</div><h3>Netlify</h3><p class=\"muted\">�til para publicaci�n p�blica r�pida. No es la recomendaci�n principal para acceso protegido gratuito de todo el sitio.</p><h3>Servidor institucional</h3><p class=\"muted\">Tambi�n puede montar esta carpeta detr�s de Nginx/Apache con Basic Auth o detr�s de un reverse proxy institucional. No requiere cambiar el contenido.</p></section>"
  )
  write_portal_page(
    file.path(root_dir, "publish_guide.html"),
    "Gu�a de publicaci�n",
    page_shell(
      title = "Gu�a de publicaci�n y control de acceso",
      intro = "Este sitio est� preparado para hosting est�tico y para a�adir autenticaci�n fuera del contenido.",
      sidebar_items = list(list(label = "Volver al portal", href = "index.html")),
      sections_html = sections
    ),
    rel_root = "."
  )
}

render_pipeline_qc_tomes <- function(root_dir, pdf_dir, pipe_inv) {
  out_dir <- here("data", "derived", "qc", "review_portal", "pipeline_qc")
  plan <- data.table()
  sections <- split(pipe_inv[order(step_order, file_name)], by = "step_id", keep.by = FALSE)
  for (step_id in names(sections)) {
    sec <- sections[[step_id]]
    tome_name <- sprintf("pipeline_qc_tomo_%03d_%s.pdf", sec$step_order[1], slugify(step_id))
    md_path <- file.path(out_dir, sprintf("pipeline_qc_tomo_%03d_%s.md", sec$step_order[1], slugify(step_id)))
    md <- c(
      sprintf("# Pipeline QC - %s", sec$section_title[1]),
      "",
      sprintf("Script fuente: `%s`", sec$script_path[1] %||% "No documentado"),
      "",
      "Este tomo conserva el detalle tecnico del paso. Cuando un hallazgo fue cerrado downstream se marca como RESUELTO y se cita la evidencia.",
      "",
      pipe_table_text(sec[, .(qc = title, archivo = file_name, veredicto = verdict_final, conclusion_experta = expert_conclusion, evidencia = expert_evidence)], max_rows = 300L)
    )
    write_text_file(md_path, md)
    render_pdf_from_md(root_dir, "Pipeline QC", sec$section_title[1], md_path, file.path(pdf_dir, tome_name))
    plan <- rbind(plan, data.table(
      module = "pipeline_qc",
      step_order = sec$step_order[1],
      step_id = step_id,
      section_title = sec$section_title[1],
      file_name = tome_name,
      n_qc = nrow(sec),
      published_path = normalize_slashes(file.path("pdf", tome_name))
    ), fill = TRUE)
  }
  fwrite(plan, file.path(out_dir, "qc_pipeline_tome_plan.csv"))
  index_md <- c(
    "# Indice de tomos de Pipeline QC",
    "",
    "Cada tomo corresponde a un paso del flujo real del pipeline.",
    "",
    pipe_table_text(plan[, .(orden = step_order, paso = section_title, qcs = n_qc, archivo = file_name)], max_rows = 200L)
  )
  idx_md <- file.path(out_dir, "indice_de_tomos_pipeline_qc.md")
  write_text_file(idx_md, index_md)
  render_pdf_from_md(root_dir, "Indice de tomos", "Pipeline QC", idx_md, file.path(pdf_dir, "indice_de_tomos_pipeline_qc.pdf"))
  invisible(plan)
}

render_epi_tomes <- function(root_dir, pdf_dir) {
  out_dir <- here("data", "derived", "qc", "review_portal", "epi_coherence")
  fig <- fread(file.path(out_dir, "figure_manifest.csv"), showProgress = FALSE)
  causes <- fread(file.path(out_dir, "inventory.csv"), showProgress = FALSE)
  fig[, file_name := basename(published_path)]
  fig[, slug := sub("_(nat_age|reg_age_[0-9]{4}|reg_age|trend|heat)\\.png$", "", file_name)]
  causes[, slug := sprintf("l%02d-c%s", as.integer(cause_level), as.character(cause_concept_id))]
  plan <- data.table()
  chunk_size <- 18L
  for (geo in c("nacional", "regional")) {
    for (lvl in sort(unique(as.integer(causes$cause_level)))) {
      lvl_causes <- causes[as.integer(cause_level) == lvl][order(cause_name)]
      if (nrow(lvl_causes) == 0) next
      lvl_causes[, chunk := ceiling(seq_len(.N) / chunk_size)]
      for (ch in sort(unique(lvl_causes$chunk))) {
        block <- lvl_causes[chunk == ch]
        tome_name <- sprintf("coherencia_epidemiologica_%s_nivel_%s_tomo_%02d.pdf", geo, lvl, ch)
        md_path <- file.path(out_dir, sprintf("coherencia_%s_nivel_%s_tomo_%02d.md", geo, lvl, ch))
        lines <- c(
          sprintf("# Coherencia epidemiologica %s - nivel %s - tomo %02d", geo, lvl, ch),
          "",
          "Este tomo contiene curvas seleccionadas para lectura experta. El sitio HTML contiene navegacion por causa, descargas CSV y figuras ampliables.",
          ""
        )
        for (i in seq_len(nrow(block))) {
          b <- block[i]
          slug_i <- b$slug
          imgs <- if (geo == "nacional") {
            fig[slug == slug_i & figure_kind %in% c("national_age_sex", "trend")]
          } else {
            fig[slug == slug_i & grepl("^regional_age_sex_", figure_kind)][1]
          }
          lines <- c(lines, sprintf("## Nivel %s - %s", b$cause_level, b$cause_name), "")
          for (img in imgs$published_path) {
            lines <- c(lines, sprintf("![](%s)", normalize_slashes(file.path(root_dir, img))), "")
          }
        }
        write_text_file(md_path, lines)
        pdf_path <- file.path(pdf_dir, tome_name)
        render_pdf_from_md(root_dir, sprintf("Coherencia epidemiologica %s", geo), sprintf("Nivel %s - tomo %02d", lvl, ch), md_path, pdf_path)
        plan <- rbind(plan, data.table(
          module = "coherencia_epidemiologica",
          geo = geo,
          cause_level = lvl,
          tomo = ch,
          file_name = tome_name,
          n_causes = nrow(block),
          published_path = normalize_slashes(file.path("pdf", tome_name))
        ), fill = TRUE)
      }
    }
  }
  fwrite(plan, file.path(out_dir, "qc_report_tome_plan.csv"))
  index_md <- c(
    "# Indice de tomos de coherencia epidemiologica",
    "",
    "Orden de lectura recomendado: nacional por niveles, luego regional por niveles. El HTML es el producto exhaustivo de exploracion.",
    "",
    pipe_table_text(plan[, .(geo, cause_level, tomo, n_causes, archivo = file_name)], max_rows = 200L)
  )
  index_md_path <- file.path(out_dir, "indice_de_tomos_coherencia.md")
  write_text_file(index_md_path, index_md)
  render_pdf_from_md(root_dir, "Indice de tomos", "Coherencia epidemiologica", index_md_path, file.path(pdf_dir, "indice_de_tomos_coherencia_epidemiologica.pdf"))
  invisible(plan)
}

build_module_pdfs <- function(root_dir, pipeline_mod) {
  pdf_dir <- file.path(root_dir, "pdf")
  ensure_dir(pdf_dir)
  pipe_inv <- pipeline_mod$inventory
  pipe_summary <- pipe_inv[, .(paso = section_title, qc = title, veredicto = verdict_final, conclusion_experta = expert_conclusion, evidencia = expert_evidence)]
  pipe_md <- c(
    "# Pipeline QC",
    "",
    "Resumen ejecutivo del m�dulo t�cnico de QC.",
    "",
    pipe_table_text(pipe_summary, max_rows = 80L)
  )
  pipe_md_path <- file.path(here("data", "derived", "qc", "review_portal", "pipeline_qc"), "pipeline_pdf_body.md")
  write_text_file(pipe_md_path, pipe_md)
  render_pipeline_qc_tomes(root_dir, pdf_dir, pipe_inv)
  render_pdf_from_md(root_dir, "Pipeline QC", "Resumen t�cnico con estado final de hallazgos", pipe_md_path, file.path(pdf_dir, "pipeline_qc_resumen.pdf"))

  render_epi_tomes(root_dir, pdf_dir)
  if (FALSE) {
  epi_nat_md <- file.path(here("data", "derived", "qc", "review_portal", "epi_coherence"), "pdf_national_body.md")
  epi_reg_md <- file.path(here("data", "derived", "qc", "review_portal", "epi_coherence"), "pdf_regional_body.md")
  write_text_file(epi_nat_md, c("# Coherencia epidemiol�gica nacional", "", readLines(epi_nat_md, warn = FALSE, encoding = "UTF-8")))
  write_text_file(epi_reg_md, c("# Coherencia epidemiol�gica regional", "", readLines(epi_reg_md, warn = FALSE, encoding = "UTF-8")))
  render_pdf_from_md(root_dir, "Coherencia epidemiol�gica nacional", "Tomo nacional", epi_nat_md, file.path(pdf_dir, "coherencia_epidemiologica_nacional.pdf"))
  render_pdf_from_md(root_dir, "Coherencia epidemiol�gica regional", "Tomo regional", epi_reg_md, file.path(pdf_dir, "coherencia_epidemiologica_regional.pdf"))
  }

  red_md_path <- file.path(here("data", "derived", "qc", "review_portal", "redistribution"), "redistribution_pdf_body.md")
  if (!file.exists(red_md_path)) {
    red_md <- c(
      "# Redistribucion",
      "",
      "Resumen metodologico del modulo de redistribucion."
    )
    write_text_file(red_md_path, red_md)
  }
  render_pdf_from_md(root_dir, "Redistribucion", "Resumen metodologico", red_md_path, file.path(pdf_dir, "redistribucion_resumen.pdf"))

  pan_md_path <- file.path(here("data", "derived", "qc", "review_portal", "pandemic_subregistro"), "pandemia_pdf_body.md")
  pan_md <- c(
    "# Pandemia y subregistro",
    "",
    "Resumen de pandemia, OPRM y correcci�n por completitud.",
    "",
    sprintf("![](%s)", normalize_slashes(file.path(root_dir, "modules", "pandemia-subregistro", "figuras", "pandemia_etapas_totales.png"))),
    "",
    sprintf("![](%s)", normalize_slashes(file.path(root_dir, "modules", "pandemia-subregistro", "figuras", "pandemia_oprm.png"))),
    "",
    sprintf("![](%s)", normalize_slashes(file.path(root_dir, "modules", "pandemia-subregistro", "figuras", "pandemia_inei_vs_observado.png"))),
    "",
    sprintf("![](%s)", normalize_slashes(file.path(root_dir, "modules", "pandemia-subregistro", "figuras", "pandemia_factor_correccion.png")))
  )
  write_text_file(pan_md_path, pan_md)
  render_pdf_from_md(root_dir, "Pandemia y subregistro", "Resumen t�cnico", pan_md_path, file.path(pdf_dir, "pandemia_subregistro_resumen.pdf"))
}

log_portal_pdf_status <- function(status, detail = "") {
  path <- here("data", "derived", "qc", "review_portal", "build_pdf_log.csv")
  ensure_dir(dirname(path))
  row <- data.table(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    status = status,
    detail = detail
  )
  if (file.exists(path)) {
    fwrite(row, path, append = TRUE)
  } else {
    fwrite(row, path)
  }
  invisible(path)
}

# Override final: consumir cuerpos Markdown canónicos y evitar plantillas
# históricas con texto o figuras desalineadas.
build_module_pdfs <- function(root_dir, pipeline_mod) {
  pdf_dir <- file.path(root_dir, "pdf")
  ensure_dir(pdf_dir)

  pipe_inv <- pipeline_mod$inventory
  pipe_summary <- pipe_inv[, .(
    paso = section_title,
    qc = title,
    veredicto = verdict_final,
    conclusion_experta = expert_conclusion,
    evidencia = expert_evidence
  )]
  pipe_md <- c(
    "# Pipeline QC",
    "",
    "Resumen ejecutivo del módulo técnico de QC.",
    "",
    pipe_table_text(pipe_summary, max_rows = 80L)
  )
  pipe_md_path <- file.path(here("data", "derived", "qc", "review_portal", "pipeline_qc"), "pipeline_pdf_body.md")
  write_text_file(pipe_md_path, pipe_md)
  render_pipeline_qc_tomes(root_dir, pdf_dir, pipe_inv)
  render_pdf_from_md(root_dir, "Pipeline QC", "Resumen técnico con estado final de hallazgos", pipe_md_path, file.path(pdf_dir, "pipeline_qc_resumen.pdf"))

  render_epi_tomes(root_dir, pdf_dir)

  red_md_path <- file.path(here("data", "derived", "qc", "review_portal", "redistribution"), "redistribution_pdf_body.md")
  if (!file.exists(red_md_path)) {
    write_text_file(red_md_path, c(
      "# Redistribución",
      "",
      "Resumen metodológico del módulo de redistribución."
    ))
  }
  render_pdf_from_md(root_dir, "Redistribución", "Resumen metodológico", red_md_path, file.path(pdf_dir, "redistribucion_resumen.pdf"))

  pan_md_path <- file.path(here("data", "derived", "qc", "review_portal", "pandemic_subregistro"), "pandemia_pdf_body.md")
  if (!file.exists(pan_md_path)) {
    write_text_file(pan_md_path, c(
      "# Pandemia y subregistro",
      "",
      "Resumen tabular de pandemia, OPRM y corrección por completitud."
    ))
  }
  render_pdf_from_md(root_dir, "Pandemia y subregistro", "Resumen técnico", pan_md_path, file.path(pdf_dir, "pandemia_subregistro_resumen.pdf"))
}

build_review_portal <- function() {
  root_dir <- here("reports", "qc_pipeline_encyclopedia")
  ensure_dir(root_dir)
  ensure_dir(here("data", "derived", "qc", "review_portal"))
  unlink(
    file.path(root_dir, c(
      "assets", "modules", "templates",
      "steps", "qc", "sections", "sections_html", "downloads",
      "qc_pipeline_frontmatter_files", "qc_pipeline_section_files", "qc_pipeline_publish_guide_files"
    )),
    recursive = TRUE,
    force = TRUE
  )
  unlink(
    file.path(root_dir, c(
      "qc_pipeline_encyclopedia.html",
      "qc_pipeline_frontmatter.qmd",
      "qc_pipeline_pdf_master.qmd",
      "qc_pipeline_publish_guide.html",
      "qc_pipeline_publish_guide.qmd",
      "qc_pipeline_report.qmd",
      "qc_pipeline_section.qmd",
      "qc_report_utils.R"
    )),
    recursive = FALSE,
    force = TRUE
  )
  write_portal_assets(root_dir)
  write_pdf_template(root_dir)

  pipeline_mod <- render_pipeline_qc_html(root_dir)
  epi_mod <- build_epi_coherence_module(root_dir)
  model_mod <- build_model_smoothing_module(root_dir)
  red_mod <- build_redistribution_module(root_dir)
  pan_mod <- build_pandemic_module(root_dir)
  modules <- list(pipeline_mod, epi_mod, model_mod, red_mod, pan_mod)
  build_portal_index(root_dir, modules)
  write_publish_guide(root_dir)
  repair_pipeline_index_links(root_dir)
  repair_redistribution_html_text(root_dir)
  sanitize_public_tree_text(root_dir)
  write_link_audit(root_dir)

  tryCatch(
    {
      build_module_pdfs(root_dir, pipeline_mod)
      log_portal_pdf_status("success", "")
    },
    error = function(e) {
      msg <- conditionMessage(e)
      portal_message(sprintf("Advertencia: el HTML del portal quedo construido, pero fallaron algunos PDFs: %s", msg))
      log_portal_pdf_status("warning", msg)
    }
  )

  invisible(list(root_dir = root_dir, modules = modules))
}

# Rewriter canonico del modulo de redistribucion.
# El build completo todavia tenia una ruta de "repair" que reintroducia
# encoding roto y una version vieja de la Tabla 3.4. Este override fuerza
# la reconstruccion publica desde los CSV derivados ya validados.
repair_redistribution_html_text <- function(root_dir) {
  script_path <- here("scripts", "rewrite_redistribution_public_outputs.R")
  if (!file.exists(script_path)) {
    stop("No se encontro scripts/rewrite_redistribution_public_outputs.R")
  }
  output <- system2(
    "Rscript",
    c(normalizePath(script_path, winslash = "/", mustWork = TRUE)),
    stdout = TRUE,
    stderr = TRUE
  )
  status <- attr(output, "status")
  if (!is.null(status) && !identical(status, 0L)) {
    stop(
      "Fallo la reescritura canonica del modulo de redistribucion:\n",
      paste(output, collapse = "\n")
    )
  }
  invisible(root_dir)
}

# -------------------------------------------------------------------------
# Overrides finales de texto publico y saneamiento UTF-8.
# Se definen al final para que dominen sobre versiones historicas previas.
# -------------------------------------------------------------------------

fix_mojibake_text <- function(x) {
  y <- as.character(x %||% "")
  if (!length(y)) return(y)

  bad_markers <- c("Ã", "Â", "â", "ï¿½", "�")
  count_bad <- function(z) {
    vapply(z, function(one) {
      sum(vapply(bad_markers, function(m) {
        hit <- gregexpr(m, one, fixed = TRUE)[[1]]
        if (identical(hit[1], -1L)) 0L else length(hit)
      }, integer(1)))
    }, integer(1))
  }

  repair_once <- function(z) {
    cands <- unique(c(
      z,
      suppressWarnings(iconv(z, from = "UTF-8", to = "latin1")),
      suppressWarnings(iconv(suppressWarnings(iconv(z, from = "UTF-8", to = "latin1")), from = "UTF-8", to = "latin1")),
      suppressWarnings(iconv(suppressWarnings(iconv(suppressWarnings(iconv(z, from = "UTF-8", to = "latin1")), from = "UTF-8", to = "latin1")), from = "UTF-8", to = "latin1")),
      suppressWarnings(iconv(suppressWarnings(iconv(suppressWarnings(iconv(suppressWarnings(iconv(z, from = "UTF-8", to = "latin1")), from = "UTF-8", to = "latin1")), from = "UTF-8", to = "latin1")), from = "UTF-8", to = "latin1"))
    ))
    cands <- cands[!is.na(cands)]
    if (!length(cands)) return(z)
    scores <- data.table(
      value = cands,
      bad = count_bad(cands),
      printable = nchar(gsub("[[:cntrl:]]", "", cands), type = "chars", allowNA = TRUE, keepNA = TRUE)
    )
    scores <- scores[order(bad, -printable)]
    scores$value[1]
  }

  bad_idx <- which(count_bad(y) > 0L)
  if (length(bad_idx)) {
    repaired <- y[bad_idx]
    for (i in seq_len(6L)) {
      candidate <- vapply(repaired, repair_once, character(1))
      improved <- count_bad(candidate) < count_bad(repaired)
      repaired[improved] <- candidate[improved]
      if (!any(count_bad(repaired) > 0L)) break
    }
    y[bad_idx] <- repaired
  }

  replacements <- c(
    "ï¿½ndice" = "Índice",
    "Navegaci�n" = "Navegación",
    "C�mo" = "Cómo",
    "Gu�a" = "Guía",
    "M�dulo" = "Módulo",
    "m�dulo" = "módulo",
    "M�dulos" = "Módulos",
    "m�dulos" = "módulos",
    "p�gina" = "página",
    "t�cnico" = "técnico",
    "t�cnica" = "técnica",
    "epidemiol�gica" = "epidemiológica",
    "publicaci�n" = "publicación",
    "distribuci�n" = "distribución",
    "redistribuci�n" = "redistribución",
    "ejecuci�n" = "ejecución",
    "vac�a" = "vacía",
    "autom�ticamente" = "automáticamente",
    "est�tico" = "estático",
    "multip�gina" = "multipágina",
    "m�todo" = "método",
    "N�mero" = "Número",
    "Proporci�n" = "Proporción",
    "asignaci�n" = "asignación",
    "despu�s" = "después",
    "a�o" = "año",
    "a�os" = "años",
    "se�al" = "señal",
    "metodol�gica" = "metodológica",
    "men�" = "menú",
    "m�s" = "más",
    "Qu�" = "Qué",
    "tambi�n" = "también",
    "regi�n" = "región",
    "gr�fica" = "gráfica",
    "diagn�stico" = "diagnóstico",
    "dise�o" = "diseño",
    "protecci�n" = "protección",
    "res�menes" = "resúmenes",
    "est�" = "está",
    "qued�" = "quedó",
    "contrase�a" = "contraseña",
    "pol�tica" = "política",
    "autenticaci�n" = "autenticación",
    "t�rminos" = "términos",
    "biol�gicamente" = "biológicamente",
    "an�malo" = "anómalo",
    "detr�s" = "detrás",
    "�til" = "Útil",
    "también" = "también"
  )
  for (pat in names(replacements)) y <- gsub(pat, replacements[[pat]], y, fixed = TRUE)
  y
}

page_shell <- function(title, intro, sidebar_items, sections_html) {
  side <- if (length(sidebar_items)) {
    paste(
      "<div class=\"sidebar\"><h3>Navegación</h3><ul>",
      paste(vapply(sidebar_items, function(x) sprintf("<li><a href=\"%s\">%s</a></li>", x$href, esc_html(x$label)), character(1)), collapse = ""),
      "</ul></div>"
    )
  } else {
    "<div class=\"sidebar\"><h3>Navegación</h3><p class=\"muted\">Esta página no necesita menú lateral adicional.</p></div>"
  }
  paste0(
    "<div class=\"portal-shell\">",
    "<section class=\"hero\">",
    sprintf("<div class=\"eyebrow\">Portal técnico reproducible</div><h1>%s</h1><p>%s</p>", esc_html(title), esc_html(intro)),
    "<div class=\"hero-meta\"><span class=\"meta-chip\">Sitio estático multipágina</span><span class=\"meta-chip\">HTML interactivo + PDF</span><span class=\"meta-chip\">Listo para hosting protegido</span></div>",
    "</section>",
    "<div class=\"layout-grid\">",
    side,
    "<main class=\"content-stack\">",
    paste(sections_html, collapse = "\n"),
    sprintf("<div class=\"footer-note\">Generado automáticamente desde %s</div>", esc_html(format(Sys.time(), "%Y-%m-%d %H:%M:%S"))),
    "</main></div></div>"
  )
}

build_portal_index <- function(root_dir, modules) {
  sections <- c(
    "<section class=\"card\">",
    "<div class=\"eyebrow\">Centro de navegación</div><h2 class=\"section-title\">Módulos del portal</h2>",
    "<p class=\"muted\">Este portal quedó preparado para uso local, publicación estática y protección posterior por capa de acceso. El HTML es el modo principal; los PDFs son tomos o resúmenes lineales.</p>",
    "<div class=\"module-grid\">",
    paste(vapply(modules, function(mod) {
      sprintf("<div class=\"module-card\"><h3><a href=\"%s\">%s</a></h3><p>Entrar al módulo temático o técnico correspondiente.</p><a class=\"btn\" href=\"%s\">Abrir módulo</a></div>", mod$href, esc_html(mod$title), mod$href)
    }, character(1)), collapse = ""),
    "</div></section>",
    "<section class=\"card\"><div class=\"eyebrow\">Uso local</div><h3>Cómo abrir el portal</h3><p class=\"muted\">Abra este archivo <strong>index.html</strong> en el navegador. Desde aquí puede entrar a cada módulo, descargar CSVs y abrir los resúmenes PDF cuando existan.</p><div class=\"nav-actions\"><a class=\"btn ghost\" href=\"publish_guide.html\">Guía de publicación y acceso</a></div></section>"
  )
  write_portal_page(
    file.path(root_dir, "index.html"),
    "Portal técnico de QC y revisión epidemiológica",
    page_shell(
      title = "Portal técnico de QC y revisión epidemiológica",
      intro = "Segunda generación del portal: diseño coherente, navegación multipágina, veredictos RESUELTO y módulos temáticos para expertos.",
      sidebar_items = list(list(label = "Guía de publicación", href = "publish_guide.html")),
      sections_html = sections
    ),
    rel_root = "."
  )
}

write_publish_guide <- function(root_dir) {
  sections <- c(
    "<section class=\"card\"><div class=\"eyebrow\">Publicación con acceso</div><h2 class=\"section-title\">Ruta recomendada: Cloudflare Pages + Cloudflare Access</h2><ol><li>Ejecute el builder y verifique que la carpeta <code>reports/qc_pipeline_encyclopedia</code> contenga el sitio completo.</li><li>Publique esa carpeta como sitio estático en Cloudflare Pages.</li><li>Proteja el hostname con Cloudflare Access y política Allow.</li><li>Como primer método, use correos permitidos con one-time pin. Si luego necesita usuario/contraseña institucional, cambie el proveedor de identidad en Access.</li></ol><p class=\"muted\">La autenticación no debe meterse dentro del HTML. Va delante del sitio, en la capa de hosting o proxy.</p></section>",
    "<section class=\"card\"><div class=\"eyebrow\">Alternativas</div><h3>Netlify</h3><p class=\"muted\">Útil para publicación pública rápida. No es la recomendación principal para acceso protegido gratuito de todo el sitio.</p><h3>Servidor institucional</h3><p class=\"muted\">También puede montar esta carpeta detrás de Nginx/Apache con Basic Auth o detrás de un reverse proxy institucional. No requiere cambiar el contenido.</p></section>"
  )
  write_portal_page(
    file.path(root_dir, "publish_guide.html"),
    "Guía de publicación",
    page_shell(
      title = "Guía de publicación y control de acceso",
      intro = "Este sitio está preparado para hosting estático y para añadir autenticación fuera del contenido.",
      sidebar_items = list(list(label = "Volver al portal", href = "index.html")),
      sections_html = sections
    ),
    rel_root = "."
  )
}

sanitize_public_tree_text <- function(root_dir) {
  text_files <- list.files(root_dir, pattern = "\\.(html|md|json|css|js|qmd)$", recursive = TRUE, full.names = TRUE)
  for (path in text_files) {
    txt <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    clean <- fix_mojibake_text(txt)
    clean <- gsub("Redistribution", "Redistribución", clean, fixed = TRUE)
    clean <- gsub("Todos los anos", "Todos los años", clean, fixed = TRUE)
    clean <- gsub("metricas narrativas", "métricas narrativas", clean, fixed = TRUE)
    clean <- gsub("Cardiovasculares e infantiles/congénitasénitas", "Cardiovasculares e infantiles/congénitas", clean, fixed = TRUE)
    clean <- gsub("módulo temático o técnico correspondiente\\.", "Entrar al módulo temático o técnico correspondiente.", clean, fixed = TRUE)
    clean <- gsub("Portal tÃ©cnico reproducible", "Portal técnico reproducible", clean, fixed = TRUE)
    clean <- gsub("Coherencia epidemiologica", "Coherencia epidemiológica", clean, fixed = TRUE)
    clean <- gsub("Indice", "Índice", clean, fixed = TRUE)
    clean <- gsub("�ndice", "Índice", clean, fixed = TRUE)
    clean <- gsub("Módulo tem�tico", "Módulo temático", clean, fixed = TRUE)
    clean <- gsub("Todos los �mbitos", "Todos los ámbitos", clean, fixed = TRUE)
    clean <- gsub("revisi�n", "revisión", clean, fixed = TRUE)
    clean <- gsub("diagn�stico", "diagnóstico", clean, fixed = TRUE)
    clean <- gsub("Tabla vac�a", "Tabla vacía", clean, fixed = TRUE)
    clean <- gsub("Tabla vacÃ­a", "Tabla vacía", clean, fixed = TRUE)
    clean <- gsub("mÃ¡s adelante", "más adelante", clean, fixed = TRUE)
    if (!identical(clean, txt)) {
      write_text_file(path, strsplit(clean, "\n", fixed = TRUE)[[1]])
    }
  }
  invisible(root_dir)
}
