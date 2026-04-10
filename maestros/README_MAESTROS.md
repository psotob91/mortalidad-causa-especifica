# Indice de Maestros del Proyecto

Este indice sirve para orientarse rapido dentro de la documentacion metodologica y operativa del proyecto. La idea es evitar que un analista o un agente abra todo siempre.

## Regla simple
Lee primero lo minimo necesario para tu tarea.

## Orden de lectura base
Para cualquier tarea tecnica o analitica:

1. `Agents.md`
2. `maestros/README_MAESTROS.md`
3. `maestros/metodologia/00_MASTER_INSTRUCCIONES_PROYECTO.txt`
4. `maestros/metodologia/01_MASTER_FLUJO_PIPELINE.txt`
5. `maestros/metodologia/02_MASTER_REGLAS_REDISTRIBUCION.txt`
6. `maestros/metodologia/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt`
7. `maestros/metodologia/04_MASTER_QC.txt`

## Que documento sirve para que

### Metodologia
- `metodologia/00_MASTER_INSTRUCCIONES_PROYECTO.txt`
  Regla general del proyecto, prioridades de fuente y principios de trabajo.
- `metodologia/01_MASTER_FLUJO_PIPELINE.txt`
  Flujo conceptual del pipeline de punta a punta.
- `metodologia/02_MASTER_REGLAS_REDISTRIBUCION.txt`
  Restricciones duras de redistribucion y terminalidad.
- `metodologia/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt`
  Criterios para pandemia y ajuste por subregistro.
- `metodologia/04_MASTER_QC.txt`
  Criterios de control de calidad estructural, logico y epidemiologico.

### Agente / auditoria profunda
- `agente/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`
  Protocolo completo de auditoria fase por fase. No hace falta abrirlo para una corrida normal del pipeline.

### Continuidad
- `continuidad/06_MASTER_ONBOARDING_PROYECTO.txt`
  Vista de entrada para un nuevo analista o nuevo agente.
- `continuidad/07_MASTER_RUTAS_TRABAJO_HUMANO_Y_AGENTE.txt`
  Explica dos formas de trabajo: humana clasica y asistida por agente.
- `continuidad/08_MASTER_REPORTING_Y_PUBLICACION.txt`
  Orienta trabajos futuros de tablas elegantes, figuras e informes reproducibles.
- `continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt`
  Resume la migracion grande del repo, sus parches, validaciones y estado actual.
- `continuidad/10_MASTER_REGLA_TRABAJO_EN_MAIN.txt`
  Fija la regla de capturar el commit de arranque al empezar cualquier edicion sobre `main`.

## Que leer segun tu tarea

### Solo quieres correr el pipeline
Lee:
1. `docs/quickstart_first_use.md`
2. `docs/operations_manual.md`
3. `docs/server_deployment_manual.pdf` solo si necesitas mas detalle

### Quieres auditar, corregir o migrar
Lee:
1. base metodologica `00-04`
2. `agente/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`
3. `continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt` si necesitas contexto de cambios recientes
4. luego scripts y helpers reales del repo

### Quieres producir tablas, figuras o un manuscrito
Lee:
1. base metodologica `00-04`
2. `continuidad/06_MASTER_ONBOARDING_PROYECTO.txt`
3. `continuidad/07_MASTER_RUTAS_TRABAJO_HUMANO_Y_AGENTE.txt`
4. `continuidad/08_MASTER_REPORTING_Y_PUBLICACION.txt`
5. `continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt`
6. luego `reports/`, `R/diagnostics/`, `data/final/` y `data/derived/qc/`

## Regla para evitar contradicciones
- `metodologia/` define reglas sustantivas y QC.
- `agente/` define como auditar profundo.
- `continuidad/` define como continuar el proyecto sin perderse.
- `docs/` explica como operar y configurar el proyecto.

No mezclar setup operativo con reglas metodologicas cuando no haga falta.
