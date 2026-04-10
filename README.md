# Mortalidad y AVP por causa especifica en Peru, 2018-2024

Pipeline reproducible para construir mortalidad causa-especifica y AVP/YLL a partir de registros de defuncion, con redistribucion de *garbage codes*, ajuste por completitud, reconciliacion jerarquica y una capa final de tablas, QC y reportes.

## Que hace este proyecto

El flujo analitico cubre cinco bloques:

- ingesta y normalizacion de registros de defuncion;
- construccion del maestro de causas y reglas de redistribucion;
- estimacion de mortalidad final, tasas y reconciliacion;
- calculo de AVP/YLL;
- generacion de tablas, QC y productos editoriales.

## Si quieres usarlo rapido

La forma recomendada es:

```powershell
Rscript .\scripts\run_preflight_checks.R
Rscript .\scripts\run_pipeline.R --profile full --clean-first
Rscript .\scripts\compare_validation_baseline.R
```

Si es tu primera vez:
- mira `docs/quickstart_first_use.md`
- luego `docs/operations_manual.md`
- abre el PDF largo solo si tu caso de rutas es especial

## Dos rutas de trabajo

### Ruta clasica del analista
Si quieres entender el proyecto y correrlo manualmente:

1. `README.md`
2. `docs/quickstart_first_use.md`
3. `docs/operations_manual.md`
4. `docs/server_deployment_manual.pdf` si necesitas mas detalle
5. `maestros/README_MAESTROS.md` si quieres entrar a la metodologia completa

### Ruta asistida por agente Codex
Si quieres seguir trabajando con ayuda de un agente:

1. `Agents.md`
2. `maestros/README_MAESTROS.md`
3. los maestros metodologicos `00-04`
4. luego la ruta documental que corresponda: operacion, auditoria o reporting

## Estructura operativa

- `scripts/`: entrypoints operativos del pipeline.
- `R/`: helpers compartidos.
- `R/diagnostics/`: diagnosticos reproducibles fuera del rerun normal.
- `R/maintenance/`: mantenimiento, auditoria y gobernanza.
- `reports/`: constructores de productos editoriales.
- `config/`: contratos, specs y configuracion de rutas.
- `data/derived/qc/`: QC tabular canonico.
- `outputs/`: auxiliares no tabulares.

## Inputs externos y portabilidad

El proyecto no depende de una unica ubicacion fija de `data/raw/`. Los insumos sensibles o externos pueden montarse fuera del repo y resolverse por:

1. variables de entorno;
2. `config/runtime_paths.yml`;
3. fallback local del repo.

Variables principales:

- `MCE_SINADEF_DIR`
- `MCE_RAW_ROOT`
- `MCE_EXTERNAL_ROOT`

## Donde vive la logica metodologica

La capa metodologica y de continuidad vive en:

- `Agents.md`
- `maestros/README_MAESTROS.md`
- `maestros/metodologia/00_MASTER_INSTRUCCIONES_PROYECTO.txt`
- `maestros/metodologia/01_MASTER_FLUJO_PIPELINE.txt`
- `maestros/metodologia/02_MASTER_REGLAS_REDISTRIBUCION.txt`
- `maestros/metodologia/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt`
- `maestros/metodologia/04_MASTER_QC.txt`
- `maestros/agente/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`
- `maestros/continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt`
- `maestros/continuidad/10_MASTER_REGLA_TRABAJO_EN_MAIN.txt`

## Documentacion de operacion

- `docs/quickstart_first_use.md`
- `docs/operations_manual.md`
- `docs/server_deployment_manual.pdf`

## Nota de repositorio

Existe una rama historica de respaldo para trazabilidad tecnica, pero `main` es la unica rama recomendada para operacion.

