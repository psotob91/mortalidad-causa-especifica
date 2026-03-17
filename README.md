# mortalidad-causa-especifica


- [Mortalidad por causa específica en Perú,
  2018-2024](#mortalidad-por-causa-específica-en-perú-2018-2024)
  - [Resumen](#resumen)
  - [Alcance del pipeline](#alcance-del-pipeline)
  - [Principios de reproducibilidad](#principios-de-reproducibilidad)
  - [Qué sí se versiona en GitHub](#qué-sí-se-versiona-en-github)
    - [1) Código fuente](#1-código-fuente)
    - [2) Configuración y contratos](#2-configuración-y-contratos)
    - [3) Insumos públicos necesarios para
      reconstrucción](#3-insumos-públicos-necesarios-para-reconstrucción)
    - [4) Documentación estructural](#4-documentación-estructural)
    - [5) Infraestructura reproducible](#5-infraestructura-reproducible)
  - [Qué no se debe versionar](#qué-no-se-debe-versionar)
    - [Datos confidenciales](#datos-confidenciales)
    - [Outputs derivados](#outputs-derivados)
    - [Archivos temporales o locales](#archivos-temporales-o-locales)
  - [Dependencias externas del
    proyecto](#dependencias-externas-del-proyecto)
  - [Estructura general sugerida](#estructura-general-sugerida)
  - [Orden lógico de ejecución](#orden-lógico-de-ejecución)
  - [Seguridad y gobernanza de datos](#seguridad-y-gobernanza-de-datos)
  - [Cómo reconstruir resultados](#cómo-reconstruir-resultados)
  - [Estado recomendado del repositorio
    público](#estado-recomendado-del-repositorio-público)
  - [Licencia](#licencia)
  - [Contacto / autoría](#contacto--autoría)

# Mortalidad por causa específica en Perú, 2018-2024

Pipeline reproducible para ingestión, normalización, mapeo,
redistribución de *garbage codes*, corrección por completitud,
modelamiento de tasas de mortalidad por causa, reconciliación jerárquica
y cálculo de AVP/YLL para el estudio nacional de carga de enfermedad del
Perú.

## Resumen

Este repositorio implementa un flujo analítico orientado a estimar
mortalidad causa-específica y AVP/YLL con una arquitectura reproducible
basada en:

- especificaciones YAML para contratos de datos;
- utilidades modulares en `R/`;
- artefactos tabulares versionables y auditables;
- separación explícita entre insumos públicos, datos confidenciales y
  salidas derivadas;
- compatibilidad con un enfoque jerárquico tipo OMOP-like para causas y
  metadatos.

## Alcance del pipeline

El flujo principal cubre:

1.  **Construcción del maestro de causas** y de la jerarquía de
    ancestros.
2.  **Ingesta de registros SINADEF crudos**.
3.  **Normalización del registro individual de defunción**.
4.  **Mapeo CIE-10 y redistribución de códigos basura**.
5.  **Corrección por completitud y manejo del componente pandémico**.
6.  **Roll-up jerárquico de causas**.
7.  **Modelamiento y suavizamiento de tasas de mortalidad**.
8.  **Reconciliación geográfica y jerárquica**.
9.  **Cálculo de AVP/YLL**.
10. **Construcción de tablas finales reportables**.

## Principios de reproducibilidad

Este repositorio **sí puede ser público**, pero **no debe incluir datos
individuales confidenciales de SINADEF** ni salidas derivadas
construidas a partir de dichos registros. Por tanto, la estrategia
recomendada es:

- **versionar** código, especificaciones, diccionarios, metadatos,
  configuraciones, insumos públicos y documentación estructural;
- **excluir** los registros individuales de defunción y cualquier
  derivado que permita reconstrucción directa o indirecta de datos
  sensibles;
- **regenerar localmente** las salidas finales a partir de los insumos
  autorizados.

## Qué sí se versiona en GitHub

### 1) Código fuente

- `*.R`
- carpeta `R/`
- scripts auxiliares del proyecto

### 2) Configuración y contratos

- `config/*.yml`
- `config/*.yaml`
- `config/*.R`
- `config/maestro_age_simple.csv`
- `config/maestro_external_inputs.csv`

### 3) Insumos públicos necesarios para reconstrucción

- `data/raw/cause_mapping/`
- `data/raw/redistribution_rules/`
- cualquier otro subdirectorio de `data/raw/` que contenga solo
  catálogos, maestros o fuentes públicas no sensibles

### 4) Documentación estructural

- `README.qmd`
- `README.md`
- `*.md`
- `*.txt`
- `tree_project.txt`
- `Estructura-del-proyecto-actualizada.txt`
- documentación metodológica no sensible

### 5) Infraestructura reproducible

Si existen en el proyecto, conviene versionar también:

- `renv.lock`
- `.Rprofile`
- `renv/activate.R`
- archivos CSS, SCSS, plantillas Quarto o YAML de render

## Qué no se debe versionar

### Datos confidenciales

- `data/raw/sinadef/`
- cualquier archivo con registros individuales identificables o
  potencialmente reidentificables
- exportaciones intermedias derivadas directamente del nivel individual

### Outputs derivados

- `data/final/`
- `data/derived/`
- `outputs/`
- reportes renderizados, tablas QC, parquet, csv finales, catálogos de
  ejecución y logs

### Archivos temporales o locales

- `.Rhistory`, `.RData`, `.Ruserdata`, `.Rproj.user/`
- `*.log`, `*.tmp`, `*.bak`, `*.out`
- carpetas de render de Quarto

## Dependencias externas del proyecto

Este repositorio depende de insumos externos definidos en
`config/external_sources.yml`, por ejemplo:

- población analítica de `demografia-poblacion-inei`;
- tablas de mortalidad de referencia;
- tabla estándar de esperanza de vida para AVP/YLL.

Para reproducibilidad completa, estos repositorios o snapshots de datos
deben estar disponibles localmente con las rutas relativas esperadas, o
bien adaptarse el archivo `config/external_sources.yml` al entorno de
ejecución.

## Estructura general sugerida

``` text
mortalidad-causa-especifica/
├─ R/
├─ config/
├─ data/
│  ├─ raw/
│  │  ├─ cause_mapping/
│  │  ├─ redistribution_rules/
│  │  └─ sinadef/        # NO versionar
│  ├─ derived/           # NO versionar
│  └─ final/             # NO versionar
├─ outputs/              # NO versionar
├─ README.qmd
├─ README.md
└─ .gitignore
```

## Orden lógico de ejecución

El pipeline sigue aproximadamente esta secuencia:

``` text
01_build_cause_master.R
02_build_cause_hierarchy_bridge.R
03_build_redistribution_rules.R
04_ingest_sinadef_raw.R
05_normalize_death_record.R
06_map_and_redistribute_deaths.R
07_qc_redistribution.R
08_build_death_cause_final.R
08b_rollup_death_cause_final.R
09_build_mortality_rates.R
09b_reconcile_mortality_hierarchy.R
10_compute_avp_yll.R
11_build_report_tables.R
```

## Seguridad y gobernanza de datos

Este repositorio está diseñado para separar claramente tres capas:

- **capa pública reproducible**: código, especificaciones, catálogos
  públicos y documentación;
- **capa sensible local**: registros individuales SINADEF y cualquier
  insumo restringido;
- **capa derivada regenerable**: salidas analíticas y diagnósticos QC.

En un repositorio público, la regla debe ser: **solo publicar lo
necesario para reproducir la lógica, no los datos sensibles ni sus
derivados directos**.

## Cómo reconstruir resultados

1.  Clonar este repositorio.
2.  Ubicar localmente los insumos públicos versionados.
3.  Colocar los datos restringidos en las rutas locales excluidas por
    `.gitignore`.
4.  Configurar las rutas de `external_sources.yml`.
5.  Ejecutar los scripts en el orden del pipeline.
6.  Verificar artefactos QC y catálogos de trazabilidad.

## Estado recomendado del repositorio público

Este repositorio debería contener principalmente:

- código fuente ejecutable;
- contratos y diccionarios;
- catálogos maestros públicos;
- documentación metodológica;
- instrucciones claras para reconstrucción local.

No debería contener:

- SINADEF individual;
- tablas finales derivadas de SINADEF;
- outputs masivos de QC;
- reportes generados automáticamente.

## Licencia

Mantener la licencia del repositorio remoto y agregar aquí cualquier
precisión metodológica o de uso si fuese necesario.

## Contacto / autoría

**Percy Soto Becerra**  
Consultoría y desarrollo metodológico para estudio nacional de carga de
enfermedad del Perú.
