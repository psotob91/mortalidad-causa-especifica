# Quickstart de Primer Uso

Esta chuleta es para una primera corrida. Si quieres mas detalle, abre `docs/operations_manual.md`.

## Checklist rapido

- [ ] ya se donde esta `sinadef`
- [ ] ya se donde estan los externos
- [ ] ya decidi si usare variables o `runtime_paths.yml`
- [ ] ya corri preflight

## Caso 1. Windows

### Si `sinadef` y externos estan fuera del repo
```powershell
$env:MCE_SINADEF_DIR="D:/raw_protegido/sinadef"
$env:MCE_EXTERNAL_ROOT="D:/repos_datos"
Rscript .\scripts\run_preflight_checks.R
Rscript .\scripts\run_pipeline.R --profile full --clean-first
Rscript .\scripts\compare_validation_baseline.R
```

### Si `sinadef` esta dentro del repo
Deja `data/raw/sinadef` dentro del proyecto y define solo externos si hace falta:

```powershell
$env:MCE_EXTERNAL_ROOT="D:/repos_datos"
Rscript .\scripts\run_preflight_checks.R
Rscript .\scripts\run_pipeline.R --profile full --clean-first
Rscript .\scripts\compare_validation_baseline.R
```

## Caso 2. Servidor o Linux

```bash
export MCE_SINADEF_DIR=/mnt/protected/raw/sinadef
export MCE_EXTERNAL_ROOT=/mnt/projects
Rscript ./scripts/run_preflight_checks.R
Rscript ./scripts/run_pipeline.R --profile full --clean-first
Rscript ./scripts/compare_validation_baseline.R
```

## Que llenar y que dejar en blanco

### Si usas variables de entorno
En `config/runtime_paths.yml` deja esto vacio:

```yaml
raw_root: ""
external_root: ""
inputs:
  sinadef_dir: ""
external_dataset_overrides:
  population_result: ""
  life_table_mortality_single_age: ""
  life_table_standard_single_age: ""
```

### Si no usas variables
Llena `raw_root`, `external_root` o los overrides puntuales segun tu caso.

## Dos layouts tipicos

### Raw fuera del repo
```text
D:/raw_protegido/sinadef
D:/repos_datos/demografia-poblacion-inei
D:/repos_datos/tabla-mortalidad-peru
D:/repos_datos/tabla-vida-estandar
```

### Raw dentro del repo
```text
mortalidad-causa-especifica/
  data/raw/sinadef/
  externos/
```

## Si algo falla
Revisa primero:
- `data/derived/qc/run_pipeline/preflight_checks.csv`
- `data/derived/qc/run_pipeline/pipeline_run_log.csv`
- `data/derived/qc/baseline_compare/qc_baseline_vs_rerun_summary.csv`

## Si trabajas con agente Codex
Haz que lea en este orden:
1. `Agents.md`
2. `maestros/README_MAESTROS.md`
3. `docs/operations_manual.md`
4. `maestros/continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt` si el hilo es nuevo y necesitas contexto

