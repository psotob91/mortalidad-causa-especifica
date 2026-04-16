# Rerun reproducible desde cero

Este repositorio se publica con estructura reproducible.

Los outputs regenerables no deben subirse a GitHub. Se reconstruyen localmente con el pipeline.

## Dependencias esperadas

- R con las librerías del proyecto ya instaladas.
- `config/runtime_paths.yml` y `config/external_sources.yml` apuntando a insumos válidos.
- Acceso local a las fuentes externas del proyecto.

## Limpieza de artefactos regenerables

Para previsualizar la limpieza:

```powershell
Rscript scripts/clean_regenerable_outputs.R
```

Para ejecutar la limpieza real:

```powershell
$env:CLEAN_DRY_RUN='false'
$env:CLEAN_CONFIRM='YES'
Rscript scripts/clean_regenerable_outputs.R
```

## Rerun integral recomendado

La ruta canónica para reconstruir core, portal QC y reporte metodológico es:

```powershell
Rscript scripts/run_full_rebuild_from_clean.R
```

Ese wrapper ejecuta:

1. `scripts/run_pipeline.R --profile full --clean-first`
2. `scripts/sensitivity/redistribution/run_no_redistribution_delete_gc.R`
3. `scripts/build_qc_pipeline_report.R`
4. `scripts/build_methodological_adjustment_report.R`

## Criterio de éxito

El rerun se considera exitoso si:

- `data/derived/qc/run_pipeline/pipeline_run_log.csv` no contiene fallos bloqueantes.
- `data/derived/qc/run_full_rebuild/run_full_rebuild_log.csv` termina con todos los steps en `success`.
- `data/derived/qc/review_portal/build_log.csv` termina en `success`.
- `data/derived/qc/review_portal/link_check.csv` no reporta rutas rotas.
- Las tablas canónicas de redistribución y pandemia/subregistro se regeneran nuevamente.

## Outputs canónicos a revisar después del rerun

- `data/derived/qc/review_portal/redistribution/`
- `data/derived/qc/review_portal/pandemic_subregistro/`
- `reports/qc_pipeline_encyclopedia/`
- `reports/methodological_adjustment_report/`
