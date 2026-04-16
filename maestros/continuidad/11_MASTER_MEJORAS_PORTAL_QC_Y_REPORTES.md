# MASTER 11 - Mejoras portal QC y reportes metodologicos

## Proposito
Este documento deja continuidad para el portal tecnico de QC, coherencia epidemiologica, redistribucion, pandemia/subregistro y modelamiento/suavizado.

La regla de trabajo aplicada fue: no cerrar hallazgos por inferencia desde codigo. Cada correccion se valido con outputs reales, QCs, tablas, figuras o revision visual.

## Checkpoint de trabajo
- Rama: `main`
- Commit de arranque registrado: `1e1d916`
- Estado inicial: worktree con cambios/no trackeados previos del portal y reportes; no se revirtieron.
- Builder principal: `scripts/build_qc_pipeline_report.R`
- Portal principal: `reports/qc_pipeline_encyclopedia/index.html`
- Reporte formal independiente: `reports/methodological_adjustment_report/pdf/methodological_adjustment_report.pdf`

## Correcciones aplicadas
- Se agrego `config/review_portal_glossary.yml` como glosario vivo con definiciones, formulas, interpretacion, columnas asociadas y notas.
- Se agregaron bloques de glosario local dentro de paginas de coherencia, modelamiento, redistribucion y pandemia/subregistro.
- En modelamiento/suavizado se agrego una tabla de cierre empirico posterior a recalibracion por `cause_concept_id + year_id + sex_id`.
- En modelamiento/suavizado se agrego explicacion de prediccion inicial, recalibracion, masa final, residuo proxy, calibracion y roughness temporal.
- En modelamiento/suavizado se agrego curva LOESS al grafico de calibracion observado vs suavizado.
- En coherencia epidemiologica nacional se cambio el eje Y a escala compartida entre `Ambos`, `Hombre` y `Mujer` dentro de cada causa.
- En coherencia epidemiologica regional se agregaron paneles por ano 2018-2024 y selector HTML por ano.
- En redistribucion se corrigio la narrativa de `garbage borrado vs salvado`: el contrafactual aplica solo a muertes inicialmente garbage, no a todas las muertes del estudio.
- En redistribucion se agrego tabla contextual con total de muertes, total garbage y proporcion garbage.
- En pandemia/subregistro se agregaron ecuaciones visibles para factor, observado corregido, exceso pandemico, neto sin pandemia, final, gaps y ratio corregido/esperado.
- En pandemia/subregistro se agrego balance canonico desde `qc_pandemic_reallocation_balance.csv`.
- En pandemia/subregistro se corrigio el grafico de etapas para distinguir series solapadas por color y tipo de linea.
- En pandemia/subregistro se corrigio el subtitulo del grafico INEI vs SINADEF para explicitar que la linea punto-raya es observado corregido.

## Evidencia empirica revisada
- Build del portal: `data/derived/qc/review_portal/build_log.csv` con `status = success`.
- Link-check: `data/derived/qc/review_portal/link_check.csv` con `6252` rutas locales evaluadas y `0` rotas.
- Coherencia epidemiologica: `168` causas cubiertas, niveles `0-4`, y `1176` figuras regionales por ano (`168 causas * 7 anos`).
- Suavizado: `data/derived/qc/review_portal/model_smoothing/model_mass_closure_by_cause_year_sex.csv` con maximo `abs_mass_diff = 3.783498e-10` y `0` estratos fallidos.
- Redistribucion: `data/derived/qc/qc_redistribution/qc_balance_total.csv` muestra `delta_total = 2.328306e-10` frente a umbral `2`, por lo que corresponde `OK_CON_NOTA`.
- Garbage: `reports/qc_pipeline_encyclopedia/modules/redistribucion/downloads/redistribution_garbage_context_summary.csv` estima `202959` muertes inicialmente garbage, equivalente a `0.132854` del total previo a redistribucion.
- Pandemia/subregistro: `reports/qc_pipeline_encyclopedia/modules/pandemia-subregistro/downloads/pandemia_qc_reallocation_balance_summary.csv` muestra maximos absolutos de delta alrededor de `1e-12` y estado `OK_CON_NOTA_residuo_numerico`.
- Tomos Pipeline QC: restaurados en `reports/qc_pipeline_encyclopedia/pdf/` con indice `indice_de_tomos_pipeline_qc.pdf` y tomos numerados por paso `000` a `999`.
- Tomos de coherencia: generados por ambito nacional/regional, nivel causal y chunk en `reports/qc_pipeline_encyclopedia/pdf/`.

## Revision visual realizada
- Se reviso visualmente el panel regional de `Total` 2024. Las curvas ahora son visibles y usan eje compartido; la similitud de forma en `Total` parece epidemiologicamente esperable para mortalidad all-cause por edad, no perdida de datos.
- Se reviso visualmente la curva nacional de `Total`; el eje Y compartido permite comparar hombres y mujeres.
- Se reviso visualmente el grafico de calibracion de `COVID-19`; la diagonal y LOESS quedan visibles y sirven para evaluar sesgo sistematico.
- Se reviso visualmente el grafico de redistribucion de ganancia/perdida; muestra redistribucion interna, no perdida de masa total.
- Se reviso visualmente el grafico de etapas de pandemia/subregistro y se corrigio para distinguir series solapadas.
- Se reviso visualmente INEI vs SINADEF; queda pendiente mejorar legibilidad de etiquetas de sexo/region, aunque el grafico ya aclara esperado, observado y observado corregido.

## Decisiones asumidas sin consulta
- No se refitearon modelos dentro del reporte; se respeta que el portal consume outputs canonicos.
- No se inventaron coeficientes ni predicciones iniciales de modelos si no existen como outputs.
- El factor original de recalibracion se reporta como no disponible en la corrida vigente; se demuestra el cierre posterior a recalibracion con datos observados.
- La vista regional especifica por region queda para una mejora posterior; en esta iteracion se implemento selector por ano y panel de todas las regiones, que es exhaustivo y estatico.
- La autenticacion del sitio sigue fuera del HTML; se mantiene como capa de hosting/proxy.

## Pendientes recomendados
- Instrumentar una proxima corrida controlada de `scripts/build_mortality_rates.R` para persistir prediccion inicial y factor de recalibracion real por causa, ano y sexo, sin cambiar estimaciones.
- Evaluar si conviene agregar paginas HTML por region especifica para coherencia epidemiologica; esto puede generar muchas imagenes si se hace para 168 causas * 25 regiones.
- Mejorar el grafico INEI vs SINADEF con etiquetas humanas de region y sexo en vez de IDs numericos.
- Considerar un dashboard estatico mas interactivo para coherencia regional si el peso del sitio sigue siendo aceptable.
- Revisar warnings de `geom_smooth()` y silenciarlos si son puramente informativos, para que el log sea mas limpio.
- Si se requiere publicacion externa, probar una copia de `reports/qc_pipeline_encyclopedia/` en Cloudflare Pages + Access.

## Recomendacion para el siguiente chat
Entrar con este orden:
1. Leer este master `maestros/continuidad/11_MASTER_MEJORAS_PORTAL_QC_Y_REPORTES.md`.
2. Verificar `data/derived/qc/review_portal/build_log.csv`.
3. Verificar `data/derived/qc/review_portal/link_check.csv`.
4. Abrir `reports/qc_pipeline_encyclopedia/index.html`.
5. Revisar visualmente tres causas centinela: `Total`, `COVID-19` y una causa rara.
6. Si se va a tocar logica de modelamiento, registrar commit base y definir QC posterior antes de editar.
