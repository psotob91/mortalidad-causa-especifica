# Mortalidad y AVP por causa especifica en Peru, 2018-2024

Pipeline reproducible para construir mortalidad causa-especifica y AVP/YLL a partir de registros de defuncion, con redistribucion de *garbage codes*, ajuste por completitud, reconciliacion jerarquica y capa final de reportes.

## Que hace este proyecto

El flujo analitico cubre cinco bloques:

- ingesta y normalizacion de registros de defuncion;
- construccion del maestro de causas y reglas de redistribucion;
- estimacion de mortalidad final, tasas y reconciliacion;
- calculo de AVP/YLL;
- generacion de tablas, QC y reportes.

La logica metodologica vigente no vive en este README sino en:

- `AGENTS.md`
- `maestros/00_MASTER_INSTRUCCIONES_PROYECTO.txt`
- `maestros/01_MASTER_FLUJO_PIPELINE.txt`
- `maestros/02_MASTER_REGLAS_REDISTRIBUCION.txt`
- `maestros/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt`
- `maestros/04_MASTER_QC.txt`
- `maestros/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`

## Estructura operativa

- `scripts/`: entrypoints operativos del pipeline.
- `R/`: helpers compartidos.
- `R/diagnostics/`: diagnosticos reproducibles fuera del rerun normal.
- `R/maintenance/`: mantenimiento, auditoria y gobernanza.
- `reports/`: constructores de productos editoriales.
- `config/`: contratos, specs y configuracion de rutas.
- `data/derived/qc/`: QC tabular canonico.
- `outputs/`: auxiliares no tabulares.

## Como correrlo

La forma recomendada es:

```powershell
Rscript .\scripts\run_preflight_checks.R
Rscript .\scripts\run_pipeline.R --profile full --clean-first
Rscript .\scripts\compare_validation_baseline.R
```

Tambien puedes correr perfiles parciales:

```powershell
Rscript .\scripts\run_pipeline.R --profile core
Rscript .\scripts\run_pipeline.R --profile methods
Rscript .\scripts\run_pipeline.R --profile reports
```

## Inputs externos y portabilidad

El proyecto ya no depende de una unica ubicacion fija de `data/raw/`. Los insumos sensibles o externos pueden montarse fuera del repo y resolverse por:

1. variables de entorno;
2. `config/runtime_paths.yml`;
3. fallback local del repo.

Variables principales:

- `MCE_SINADEF_DIR`
- `MCE_RAW_ROOT`
- `MCE_EXTERNAL_ROOT`

## Documentacion de operacion

Para montaje en servidor, Docker, permisos de lectura/escritura y ejemplos de ejecucion:

- `docs/operations_manual.md`
- `docs/server_deployment_manual.pdf`

## Estado del repositorio

Esta version usa nombres canonicos semanticos en scripts y carpetas activas. Existe una rama historica de respaldo para trazabilidad tecnica, pero `main` es la unica rama recomendada para operacion.
