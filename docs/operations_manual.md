# Manual de Operaciones del Pipeline

## Objetivo
Este manual deja un punto unico de configuracion para ejecutar el proyecto:
- en laptop local
- en servidor
- dentro de Docker

La regla operacional es:
- los `raw` y datasets externos pueden vivir fuera del repo
- los outputs se escriben dentro del proyecto
- el cambio de ubicacion se controla sin editar scripts metodologicos

## Punto unico de configuracion
La resolucion de inputs externos sigue esta precedencia:
1. Variables de entorno
2. `config/runtime_paths.yml`
3. Fallback al layout local del repo

Archivo principal:
- `config/runtime_paths.yml`

Helper central:
- `R/io_utils.R`

## Variables de entorno soportadas
### Raw principal
- `MCE_SINADEF_DIR`
  Usa una ruta directa al raw SINADEF.
- `MCE_RAW_ROOT`
  Usa una raiz general de raw y busca ahi la subcarpeta relativa del proyecto.

### Datasets externos
- `MCE_EXTERNAL_ROOT`
  Usa una raiz general para los paths relativos definidos en `config/external_sources.yml`

Overrides puntuales disponibles hoy:
- `MCE_EXT_POPULATION_RESULT_PATH`
- `MCE_EXT_LIFE_TABLE_MORTALITY_SINGLE_AGE_PATH`
- `MCE_EXT_LIFE_TABLE_STANDARD_SINGLE_AGE_PATH`

## Configuracion por archivo
Puedes editar `config/runtime_paths.yml` y dejar:

```yaml
raw_root: "/mnt/protected/raw"
external_root: "/mnt/projects"

inputs:
  sinadef_dir: ""

external_dataset_overrides:
  population_result: ""
  life_table_mortality_single_age: ""
  life_table_standard_single_age: ""
```

Notas:
- `raw_root` debe apuntar a la raiz donde vive el raw del proyecto.
- `external_root` debe apuntar a la raiz desde la que se resuelven los paths relativos declarados en `external_sources.yml`.
- Los overrides puntuales sirven cuando un dataset externo no sigue el layout normal.

## Ejecucion recomendada
### 1. Preflight
Verifica montaje, lectura y escritura antes de correr:

```powershell
Rscript .\scripts\run_preflight_checks.R
```

### 2. Rerun limpio completo
```powershell
Rscript .\scripts\run_pipeline.R --profile full --clean-first
```

### 3. Solo core
```powershell
Rscript .\scripts\run_pipeline.R --profile core
```

### 4. Solo metodos
```powershell
Rscript .\scripts\run_pipeline.R --profile methods
```

### 5. Solo reportes
```powershell
Rscript .\scripts\run_pipeline.R --profile reports
```

## Ejemplos de servidor/Docker
### Linux / Docker
```bash
export MCE_SINADEF_DIR=/mnt/protected/raw/sinadef
export MCE_EXTERNAL_ROOT=/mnt/projects
Rscript ./scripts/run_pipeline.R --profile full --clean-first
```

### Windows PowerShell
```powershell
$env:MCE_SINADEF_DIR="D:\\raw_protegido\\sinadef"
$env:MCE_EXTERNAL_ROOT="D:\\repos_datos"
Rscript .\scripts\run_pipeline.R --profile full --clean-first
```

## Directorios que deben ser solo lectura
- raw externo montado fuera del repo
- datasets externos apuntados desde `external_sources.yml`

## Directorios que deben ser escribibles
- `data/final`
- `data/derived`
- `data/_catalog`
- `reports`
- `outputs`

## Scripts canonicos
La capa semantica oficial y el mapa historico estan en:
- `config/script_aliases.csv`
- `config/pipeline_steps.csv`

Los scripts canonicos son la unica interfaz oficial para operacion y documentacion futura.

## Semantica de carpetas de codigo
- `scripts/`: entrypoints operativos que un analista corre normalmente.
- `R/`: helpers y modulos compartidos.
- `R/diagnostics/`: diagnosticos reproducibles fuera del rerun normal.
- `R/maintenance/`: auditoria, gobernanza y mantenimiento no operativo diario.
- `reports/`: constructores de salidas editoriales/reportables.
- `config/`: configuracion y soporte, no entrypoints operativos.

## Semantica de carpetas de salida
- `data/derived/qc/`: QC tabular canonico del pipeline.
- `reports/`: productos editoriales y reportables.
- `outputs/`: auxiliares no tabulares y no editoriales.

Las rutas QC y auxiliares activas usan solo nombres semanticos canonicos.

## Politica de nombres y estado operativo
### Oficial
- scripts canonicos sin prefijo numerico
- `run_preflight_checks.R`
- `run_pipeline.R`
- entradas `script_path_canonical` de `pipeline_steps.csv`

### Rama historica
- la referencia historica completa vive en la rama `deprecated/full-history`
- `main` es la unica rama recomendada para operacion y mantenimiento corriente

## Validacion posterior
Tras una corrida completa revisar:
- `data/derived/qc/run_pipeline/preflight_checks.csv`
- `data/derived/qc/run_pipeline/pipeline_run_log.csv`
- `data/derived/qc/baseline_compare/qc_baseline_vs_rerun_summary.csv`

Para recalcular la comparacion baseline:

```powershell
Rscript .\scripts\compare_validation_baseline.R
```

Condiciones esperadas:
- preflight sin fallos bloqueantes
- pipeline sin steps fallidos
- baseline sin diferencias inesperadas

## Politica operativa de warnings
- Warnings bloqueantes: cualquier fallo de preflight, step fallido del runner, o diferencias en baseline.
- Warnings tolerados pero documentados:
  - outliers de `yll_rate` asociados a denominadores muy pequenos en edades extremas
  - fallback `parquet -> csv` del mismo dataset canónico cuando el reporte puede seguir sin cambiar ubicacion de lectura
- Warnings a corregir si reaparecen:
  - reciclado de `data.table` en scripts metodologicos
  - warnings de maquetacion del Word final

Registro vigente:
- `docs/minor_observations_status.csv`
