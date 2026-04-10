# Manual de Operaciones del Pipeline

## Para que sirve este manual
Este manual esta pensado para una persona con perfil informatico basico que necesita configurar el proyecto por primera vez y correrlo sin tocar scripts metodologicos.

La idea central es simple:
- el proyecto puede leer el `raw` y los datasets externos desde cualquier ubicacion;
- los resultados siempre se escriben dentro del repo;
- tu solo debes decidir donde estan tus insumos y completar un solo punto de configuracion.

## Dos formas de entrar al proyecto
### Ruta 1. Trabajo clasico del analista
Si solo quieres correr el pipeline por primera vez:

1. `README.md`
2. `docs/quickstart_first_use.md`
3. `docs/operations_manual.md`
4. `docs/server_deployment_manual.pdf` solo si tu caso de rutas es especial

Que encontraras ahi:
- en `README.md`: una vista breve del proyecto
- en la quickstart: la secuencia minima de primer uso
- en este manual: configuracion paso a paso y ejemplos concretos
- en el PDF largo: una version mas extensa y pedagogica

### Ruta 2. Trabajo asistido por agente Codex
Si quieres trabajar con un agente para seguir extendiendo el proyecto:

1. `Agents.md`
2. `maestros/README_MAESTROS.md`
3. `maestros/metodologia/00_MASTER_INSTRUCCIONES_PROYECTO.txt`
4. `maestros/metodologia/01_MASTER_FLUJO_PIPELINE.txt`
5. `maestros/metodologia/02_MASTER_REGLAS_REDISTRIBUCION.txt`
6. `maestros/metodologia/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt`
7. `maestros/metodologia/04_MASTER_QC.txt`

Luego, segun la tarea:
- si quieres auditar o corregir: `maestros/agente/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt`
- si quieres crear nuevas tablas, figuras o un informe: `maestros/continuidad/08_MASTER_REPORTING_Y_PUBLICACION.txt`
- si vas a continuar trabajo reciente o abrir un hilo nuevo sobre `main`: `maestros/continuidad/09_MASTER_CONTEXTO_MIGRACION_Y_ESTADO_ACTUAL.txt` y `maestros/continuidad/10_MASTER_REGLA_TRABAJO_EN_MAIN.txt`

Ejemplo de arranque con agente:
- "Lee `Agents.md`, el indice de maestros y la ruta de reporting; luego propon una bateria de figuras elegantes para publicacion."

## Regla de resolucion de rutas
El proyecto busca las rutas en este orden:

1. variables de entorno
2. `config/runtime_paths.yml`
3. fallback al layout local del repo

Eso significa que:
- si defines una variable de entorno, esa variable manda;
- si no defines variable, se usa `config/runtime_paths.yml`;
- si ambos estan vacios, el proyecto intenta usar el layout interno del repo.

## Antes de empezar
Necesitas confirmar estas cuatro cosas:

- [ ] ya se donde esta la carpeta de `sinadef`
- [ ] ya se donde estan los externos principales
- [ ] ya decidi si usare variables de entorno o `runtime_paths.yml`
- [ ] tengo permiso de lectura para insumos y de escritura para outputs

## Paso 1. Identifica tu caso
Usa esta tabla para decidir rapidamente que escenario te corresponde.

| Caso | Donde esta `sinadef` | Donde estan los externos | Recomendacion principal |
|---|---|---|---|
| **Caso A** | Fuera del repo | Fuera del repo | Usar variables de entorno o `runtime_paths.yml` |
| **Caso B** | Dentro del repo | Fuera del repo | Dejar `sinadef` interno y configurar solo externos |
| **Caso C** | Dentro del repo | Dentro del repo | Dejar `runtime_paths.yml` vacio o con rutas relativas internas |

## Paso 2. Entiende que ruta necesita cada cosa
### Que es `MCE_SINADEF_DIR`
Debe apuntar a la **carpeta** donde esta el raw principal de SINADEF.

Debe apuntar a algo parecido a esto:
- `D:/datos/sinadef`
- `/mnt/protected/raw/sinadef`
- `C:/proyectos/mortalidad-causa-especifica/data/raw/sinadef`

No debe apuntar a:
- un archivo individual `.csv`, `.xlsx` o `.parquet`
- la carpeta `data/raw/` general si dentro no existe `sinadef`
- una carpeta demasiado arriba que aun no contiene `sinadef`

### Que es `MCE_EXTERNAL_ROOT`
Debe apuntar a la raiz desde la cual se vuelven validos los paths de `config/external_sources.yml`.

Hoy los datasets externos principales son:
- `population_result`
- `life_table_mortality_single_age`
- `life_table_standard_single_age`

En `config/external_sources.yml` veras rutas como estas:
- `../demografia-poblacion-inei/data/final/population_inei/population_result.parquet`
- `../tabla-mortalidad-peru/data/final/life_table_mortality/single_age/ref_life_table_mortality_single_age.csv`
- `../tabla-vida-estandar/data/final/standard_life_table/life_table_standard_reference_single_age.rds`

Eso significa que `MCE_EXTERNAL_ROOT` debe ayudarte a llegar a esos proyectos hermanos o, si no tienes ese layout, debes usar un override puntual.

## Paso 3. Como encontrar las rutas en disco
### En Windows
1. Abre el Explorador.
2. Entra a la carpeta que quieres usar.
3. Haz clic en la barra de direccion.
4. Copia la ruta completa.
5. Pegala en PowerShell o en `runtime_paths.yml`.

Ejemplo:
- si estas dentro de `D:\repos_datos\demografia-poblacion-inei\data\final\population_inei`, la ruta de la carpeta es esa carpeta completa.

### En Linux o servidor
1. Abre la terminal.
2. Entra a la carpeta con `cd`.
3. Corre `pwd` para ver la ruta completa.
4. Copia esa ruta.

Ejemplo:
```bash
cd /mnt/projects/demografia-poblacion-inei/data/final/population_inei
pwd
```

## Paso 4. Entiende `config/runtime_paths.yml`
El archivo actual es este:

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

### Que significa cada campo
#### `raw_root`
**LLENAR** si quieres apuntar a una carpeta madre que contiene `sinadef`.

**DEJAR EN BLANCO** si:
- usaras `MCE_SINADEF_DIR`, o
- `data/raw/sinadef/` vive dentro del repo.

#### `external_root`
**LLENAR** si quieres usar una raiz comun para datasets externos.

**DEJAR EN BLANCO** si:
- usaras `MCE_EXTERNAL_ROOT`, o
- tus externos ya viven dentro del repo, o
- usaras overrides puntuales por dataset.

#### `inputs.sinadef_dir`
**LLENAR** solo si quieres apuntar directamente a la carpeta final de SINADEF.

**DEJAR EN BLANCO** si `raw_root` ya resuelve bien `sinadef` o si usaras `MCE_SINADEF_DIR`.

#### `external_dataset_overrides.*`
**LLENAR** solo si un dataset externo esta en una ubicacion especial y no sigue el layout comun.

**DEJAR EN BLANCO** si `external_root` o `MCE_EXTERNAL_ROOT` ya resuelven bien la ruta.

## Paso 5. Que llenar y que dejar en blanco segun tu caso
### Caso A. Raw fuera del repo y externos fuera del repo
Este es el caso mas comun en servidor o Docker.

#### Opcion recomendada: variables de entorno
```powershell
$env:MCE_SINADEF_DIR="D:/raw_protegido/sinadef"   # **LLENAR** con la carpeta final de sinadef
$env:MCE_EXTERNAL_ROOT="D:/repos_datos"           # **LLENAR** con la raiz comun de externos
```

En este caso, en `config/runtime_paths.yml` puedes dejar todo vacio:

```yaml
raw_root: ""                                      # **DEJAR EN BLANCO**
external_root: ""                                 # **DEJAR EN BLANCO**

inputs:
  sinadef_dir: ""                                 # **DEJAR EN BLANCO**

external_dataset_overrides:
  population_result: ""                           # **DEJAR EN BLANCO**
  life_table_mortality_single_age: ""             # **DEJAR EN BLANCO**
  life_table_standard_single_age: ""              # **DEJAR EN BLANCO**
```

#### Opcion alternativa: solo YAML
Si no quieres definir variables, puedes llenar el YAML asi:

```yaml
raw_root: "D:/raw_protegido"                      # **LLENAR** si `sinadef` cuelga de esta raiz
external_root: "D:/repos_datos"                   # **LLENAR** con la raiz comun de externos

inputs:
  sinadef_dir: ""                                 # **DEJAR EN BLANCO** si raw_root ya resuelve sinadef

external_dataset_overrides:
  population_result: ""                           # **DEJAR EN BLANCO**
  life_table_mortality_single_age: ""             # **DEJAR EN BLANCO**
  life_table_standard_single_age: ""              # **DEJAR EN BLANCO**
```

### Caso B. Raw dentro del repo y externos fuera del repo
Este caso sirve mucho para laptops de trabajo.

Supongamos que `sinadef` vive aqui:
- `C:/proyectos/mortalidad-causa-especifica/data/raw/sinadef`

Y los externos viven fuera del repo.

#### Opcion recomendada
Define solo el root externo:

```powershell
$env:MCE_EXTERNAL_ROOT="D:/repos_datos"           # **LLENAR**
```

Y deja `runtime_paths.yml` asi:

```yaml
raw_root: ""                                      # **DEJAR EN BLANCO** porque sinadef esta dentro del repo
external_root: ""                                 # **DEJAR EN BLANCO** si ya usas MCE_EXTERNAL_ROOT

inputs:
  sinadef_dir: ""                                 # **DEJAR EN BLANCO**

external_dataset_overrides:
  population_result: ""                           # **DEJAR EN BLANCO**
  life_table_mortality_single_age: ""             # **DEJAR EN BLANCO**
  life_table_standard_single_age: ""              # **DEJAR EN BLANCO**
```

### Caso C. Raw y externos dentro del mismo proyecto
Este caso es valido para pruebas controladas, laptops o una instalacion simple.

Supongamos que tienes este layout:

```text
mortalidad-causa-especifica/
  data/
    raw/
      sinadef/
  externos/
    demografia-poblacion-inei/
    tabla-mortalidad-peru/
    tabla-vida-estandar/
```

En este escenario puedes:
- ajustar `config/external_sources.yml` para usar rutas relativas internas, o
- definir overrides puntuales en `runtime_paths.yml`.

Ejemplo usando overrides puntuales:

```yaml
raw_root: ""                                      # **DEJAR EN BLANCO**
external_root: ""                                 # **DEJAR EN BLANCO**

inputs:
  sinadef_dir: ""                                 # **DEJAR EN BLANCO** porque se usara data/raw/sinadef del repo

external_dataset_overrides:
  population_result: "externos/demografia-poblacion-inei/data/final/population_inei/population_result.parquet"            # **LLENAR**
  life_table_mortality_single_age: "externos/tabla-mortalidad-peru/data/final/life_table_mortality/single_age/ref_life_table_mortality_single_age.csv"  # **LLENAR**
  life_table_standard_single_age: "externos/tabla-vida-estandar/data/final/standard_life_table/life_table_standard_reference_single_age.rds"              # **LLENAR**
```

En este caso normalmente no necesitas variables de entorno.

## Paso 6. Ejemplos completos paso a paso
### Escenario 1. Servidor / Linux / Docker
#### Situacion
- raw SINADEF montado en `/mnt/protected/raw/sinadef`
- proyectos externos montados bajo `/mnt/projects`

#### Que debes hacer
1. Confirmar que `/mnt/protected/raw/sinadef` existe.
2. Confirmar que `/mnt/projects/demografia-poblacion-inei` existe.
3. Confirmar que `/mnt/projects/tabla-mortalidad-peru` existe.
4. Confirmar que `/mnt/projects/tabla-vida-estandar` existe.
5. Definir variables de entorno.
6. Correr preflight.

#### Comandos
```bash
export MCE_SINADEF_DIR=/mnt/protected/raw/sinadef
export MCE_EXTERNAL_ROOT=/mnt/projects
Rscript ./scripts/run_preflight_checks.R
```

#### Que debes dejar en blanco en `runtime_paths.yml`
Deja todo en blanco si ya usaste esas variables.

### Escenario 2. PC Windows
#### Situacion
- raw SINADEF en `D:\raw_protegido\sinadef`
- externos en `D:\repos_datos`

#### Que debes hacer
1. Abre `D:\raw_protegido\sinadef` y confirma que esa es la carpeta correcta.
2. Abre `D:\repos_datos` y confirma que ahi estan los proyectos externos.
3. Copia las rutas exactas desde el Explorador.
4. Abre PowerShell en la raiz del repo.
5. Define las variables.
6. Corre preflight.

#### Comandos
```powershell
$env:MCE_SINADEF_DIR="D:/raw_protegido/sinadef"
$env:MCE_EXTERNAL_ROOT="D:/repos_datos"
Rscript .\scripts\run_preflight_checks.R
```

#### Si usas OneDrive o una ruta larga
Tambien es valido algo como esto:

```powershell
$env:MCE_SINADEF_DIR="C:/Users/TuUsuario/OneDrive/Datos/sinadef"
$env:MCE_EXTERNAL_ROOT="C:/Users/TuUsuario/OneDrive/ReposExternos"
```

### Escenario 3. `sinadef` dentro del mismo proyecto
#### Situacion
- `sinadef` esta en `data/raw/sinadef`
- quieres trabajar todo desde una sola carpeta del proyecto

#### Que debes hacer
1. Confirmar que existe `data/raw/sinadef` dentro del repo.
2. Decidir si tus externos tambien estaran dentro del repo.
3. Si los externos estaran dentro, llenar overrides puntuales o usar rutas relativas internas compatibles.
4. Correr preflight.

#### Ejemplo minimo
```powershell
Rscript .\scripts\run_preflight_checks.R
```

#### Que debe quedar vacio
```yaml
raw_root: ""                                      # **DEJAR EN BLANCO**
external_root: ""                                 # **DEJAR EN BLANCO** si no usas raiz externa comun

inputs:
  sinadef_dir: ""                                 # **DEJAR EN BLANCO**
```

## Paso 7. Corre el preflight
El preflight debe correrse siempre antes del pipeline completo.

```powershell
Rscript .\scripts\run_preflight_checks.R
```

### Que archivos revisar
- `data/derived/qc/run_pipeline/preflight_checks.csv`
- `data/derived/qc/run_pipeline/preflight_summary.csv`

### Que significa que paso bien
Debes ver que:
- `runtime_input_sinadef_dir` esta en `ok`
- los datasets externos requeridos estan en `ok`
- los directorios de salida escribibles estan en `ok`

## Paso 8. Corre el pipeline completo
Cuando el preflight este aprobado, corre:

```powershell
Rscript .\scripts\run_pipeline.R --profile full --clean-first
```

Tambien puedes correr por perfil:

```powershell
Rscript .\scripts\run_pipeline.R --profile core
Rscript .\scripts\run_pipeline.R --profile methods
Rscript .\scripts\run_pipeline.R --profile reports
```

## Paso 9. Valida el resultado final
Despues del pipeline completo, corre:

```powershell
Rscript .\scripts\compare_validation_baseline.R
```

Revisa:
- `data/derived/qc/run_pipeline/pipeline_run_log.csv`
- `data/derived/qc/baseline_compare/qc_baseline_vs_rerun_summary.csv`

Debes esperar:
- todos los steps con `status = success`
- baseline sin diferencias sustantivas

## Errores comunes y como corregirlos
### Error 1. `sinadef` no existe
**Sintoma:** el preflight falla en `runtime_input_sinadef_dir`.

**Causa probable:** apuntaste a una carpeta equivocada o a un archivo en lugar de una carpeta.

**Como corregirlo:** vuelve a ubicar la carpeta final de `sinadef` y corrige `MCE_SINADEF_DIR`, `raw_root` o `inputs.sinadef_dir`.

### Error 2. Falta un externo
**Sintoma:** falla `external_dataset_population_result` o algun dataset externo requerido.

**Causa probable:** `MCE_EXTERNAL_ROOT` apunta demasiado arriba o demasiado abajo, o el layout no coincide con `config/external_sources.yml`.

**Como corregirlo:** revisa `config/external_sources.yml` y confirma la ruta relativa real. Si tu layout es especial, usa `external_dataset_overrides`.

### Error 3. Llenaste `raw_root` y `sinadef_dir` con rutas distintas
**Sintoma:** confusion sobre cual ruta se esta usando.

**Causa probable:** doble configuracion innecesaria.

**Como corregirlo:** deja una sola estrategia. Si ya usas `MCE_SINADEF_DIR`, deja vacios `raw_root` y `sinadef_dir`.

### Error 4. Usaste rutas Linux en Windows o viceversa
**Sintoma:** el preflight no encuentra carpetas que sabes que existen.

**Causa probable:** copiaste un ejemplo de otro sistema operativo.

**Como corregirlo:** usa rutas del sistema real donde estas trabajando.

### Error 5. Copiaste la ruta de un archivo cuando se esperaba una carpeta
**Sintoma:** el preflight sigue fallando aunque la ruta existe.

**Causa probable:** apuntaste a `algo.csv` o `algo.parquet` cuando se esperaba la carpeta contenedora.

**Como corregirlo:** usa la carpeta correcta en `MCE_SINADEF_DIR`, `raw_root` o `external_root`. Usa archivo individual solo en `external_dataset_overrides`.

## Checklist de primer uso
Sigue este orden exacto:

1. localiza `sinadef`
2. localiza los externos principales
3. decide si tu caso es A, B o C
4. completa variables o `runtime_paths.yml`
5. corre `run_preflight_checks.R`
6. revisa `preflight_checks.csv`
7. corre `run_pipeline.R --profile full --clean-first`
8. corre `compare_validation_baseline.R`
9. revisa `pipeline_run_log.csv` y `qc_baseline_vs_rerun_summary.csv`

## Comandos oficiales
```powershell
Rscript .\scripts\run_preflight_checks.R
Rscript .\scripts\run_pipeline.R --profile full --clean-first
Rscript .\scripts\compare_validation_baseline.R
```

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

## Politica operativa de warnings
Warnings bloqueantes:
- fallos de preflight
- steps del runner con `status != success`
- diferencias sustantivas en baseline

Warnings tolerados pero documentados:
- outliers de `yll_rate` en celdas extremas con denominadores muy pequenos
- fallback `parquet -> csv` del mismo dataset canonico cuando el reporte puede continuar sin cambiar de fuente metodologica

Registro vigente:
- `docs/minor_observations_status.csv`

