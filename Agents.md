# GUIA DE ENTRADA PARA CODEX Y OTROS AGENTES

## PROPOSITO
Este archivo es la puerta de entrada del proyecto para cualquier agente que vaya a operar, auditar, extender o mantener el repositorio.

La idea no es obligar a leer todo siempre, sino leer lo correcto segun la tarea.

## ORDEN MINIMO DE LECTURA PARA CUALQUIER TAREA
Antes de hacer cualquier analisis o cambio, leer siempre:

1. `./maestros/README_MAESTROS.md`
2. `./maestros/metodologia/00_MASTER_INSTRUCCIONES_PROYECTO.txt`
3. `./maestros/metodologia/01_MASTER_FLUJO_PIPELINE.txt`
4. `./maestros/metodologia/02_MASTER_REGLAS_REDISTRIBUCION.txt`
5. `./maestros/metodologia/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt`
6. `./maestros/metodologia/04_MASTER_QC.txt`

Si la tarea requiere continuidad reciente o trabajo sobre `main`, leer tambien:
7. `./maestros/continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt`
8. `./maestros/continuidad/10_MASTER_REGLA_TRABAJO_EN_MAIN.txt`

Estos documentos constituyen la base metodologica y de QC del proyecto.

## LECTURA ADICIONAL SEGUN TIPO DE TAREA
### Ruta A. Operar el pipeline
Si la tarea es configurar rutas, correr el pipeline o validar un rerun:

- `./docs/operations_manual.md`
- `./docs/server_deployment_manual.pdf`
- `./docs/quickstart_first_use.md`

### Ruta B. Auditar, corregir o migrar
Si la tarea es auditar, refactorizar, corregir bugs o revisar consistencia global:

- lectura minima obligatoria anterior
- `./maestros/agente/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`

### Ruta C. Extender reportes, tablas, visualizaciones o manuscrito
Si la tarea es preparar nuevas tablas, figuras, visualizaciones elegantes o un informe reproducible:

- lectura minima obligatoria anterior
- `./maestros/continuidad/06_MASTER_ONBOARDING_PROYECTO.txt`
- `./maestros/continuidad/07_MASTER_RUTAS_TRABAJO_HUMANO_Y_AGENTE.txt`
- `./maestros/continuidad/08_MASTER_REPORTING_Y_PUBLICACION.txt`
- `./maestros/continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt` si necesitas contexto de cambios recientes
- luego revisar `reports/`, `R/diagnostics/` y los outputs finales vigentes

## PRIORIDAD DE FUENTES
1. Archivos adjuntos en el chat
2. Codigo y configuracion del repositorio actual
3. Documentos maestros del proyecto

## REGLA DE CODIGO
Para archivos tecnicos (`.R`, `.yml`, `.yaml`, `.css`):
- usar primero archivos adjuntos en el chat si existen
- si no existen, usar la version del repositorio actual
- no inventar codigo ni contratos de datos

## REGLAS METODOLOGICAS DURAS
Verificar siempre:
- redistribucion solo en causas terminales
- reconciliacion jerarquica completa
- ajuste por subregistro correcto
- inclusion metodologicamente consistente de pandemia
- uso consistente de IDs de causa
- calculo correcto de AVP/YLL

## COMPORTAMIENTO ESPERADO DEL AGENTE
Siempre debes:
1. identificar la tarea real antes de editar
2. leer la ruta documental correcta segun la tarea
3. si vas a editar `main`, registrar primero el commit de arranque con `git rev-parse --short HEAD`
4. no asumir estructura de datos sin verificar
5. explicar riesgos downstream cuando propongas cambios
6. proponer QC posterior cuando toques logica o contratos

## RESTRICCIONES
- No inventar diccionarios
- No romper jerarquias
- No redistribuir fuera de causas terminales
- No tratar anexos editoriales como si fueran outputs productivos
- No mezclar setup operativo con decisiones metodologicas

## GUIA RAPIDA SEGUN EL PEDIDO DEL USUARIO
Si el usuario pide ayuda para correr el proyecto:
- leer `README.md`
- leer `docs/quickstart_first_use.md`
- leer `docs/operations_manual.md`

Si el usuario pide auditoria o correccion:
- leer la base metodologica
- leer `05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`
- revisar luego scripts y helpers reales del repo

Si el usuario pide nuevas figuras, tablas o manuscrito:
- leer la base metodologica
- leer la capa de continuidad
- revisar `reports/`, `data/final/`, `data/derived/qc/` y los scripts diagnosticos relevantes

## EJEMPLOS DE PROMPTS DE ARRANQUE
- "Lee `Agents.md`, el indice de maestros y la ruta de reporting; luego propon una bateria de figuras elegantes para publicacion."
- "Lee metodologia `00-04` y revisa si una tabla regional nueva respeta jerarquia, AVP y QC."
- "Lee la ruta operativa y ayudame a configurar `runtime_paths.yml` para correr en Windows."

## NOTA FINAL
Si existe conflicto entre fuentes:
- priorizar archivos adjuntos del chat
- luego codigo real del repositorio
- luego documentos maestros

Si algo no esta definido:
- no asumir
- explicitar la laguna
- proponer solucion concreta

