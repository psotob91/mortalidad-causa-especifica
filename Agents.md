# INSTRUCCIONES PARA CODEX (OBLIGATORIO)

## REGLA PRINCIPAL
Antes de ejecutar cualquier tarea, debes leer SIEMPRE los siguientes archivos del proyecto:

- ./maestros/00_MASTER_INSTRUCCIONES_PROYECTO.txt
- ./maestros/01_MASTER_FLUJO_PIPELINE.txt
- ./maestros/02_MASTER_REGLAS_REDISTRIBUCION.txt
- ./maestros/03_MASTER_PANDEMIA_Y_SUBREGISTRO.txt
- ./maestros/04_MASTER_QC.txt
- ./maestros/05_MASTER_PROMPT_AUDITORIA_FULL_REPO.txt

Estos documentos constituyen la fuente metodológica principal del proyecto.

---

## PRIORIDAD DE FUENTES

1. Archivos adjuntos en el chat (máxima prioridad)
2. Código del repositorio GitHub:
   https://github.com/psotob91/mortalidad-causa-especifica-v2
3. Documentos fuente del proyecto

---

## REGLA DE CÓDIGO

Para archivos técnicos (.R, .yml, .yaml, .css):
- usar primero archivos adjuntos en el chat
- si no existen, usar el repositorio GitHub
- no inventar código

---

## COMPORTAMIENTO ESPERADO

Siempre debes:

1. Revisar los MASTER_*.txt antes de cualquier análisis
2. Identificar errores metodológicos o de código
3. Explicar el problema técnico
4. Proponer corrección concreta
5. Indicar:
   - script afectado
   - función específica
   - impacto downstream
6. Proponer QC posterior

---

## RESTRICCIONES

- No asumir estructura de datos sin verificar
- No inventar diccionarios
- No romper jerarquías
- No redistribuir fuera de causas terminales

---

## ENFOQUE METODOLÓGICO

Debes verificar siempre:

- Redistribución solo en causas terminales
- Reconciliación jerárquica completa
- Ajuste por subregistro correcto
- Inclusión de pandemia (OMS 2021)
- Uso consistente de IDs de causa
- Cálculo correcto de AVP

---

## Modo de trabajo por defecto

Para tareas grandes de auditoría del repo, trabajar en dos etapas:

1. **Plan**
   - mapear scripts
   - identificar fases
   - listar inputs/outputs
   - detectar riesgos
   - proponer orden de auditoría
   - no editar código todavía salvo solicitud explícita

2. **Ejecución**
   - auditar una fase por vez
   - proponer cambios mínimos
   - no avanzar a la siguiente fase si la actual queda NO APROBADA

---

## NOTA FINAL

Si existe conflicto entre fuentes:
- priorizar archivos adjuntos en chat
- luego repo GitHub
- luego documentos fuente

Si algo no está definido:
- no asumir
- proponer solución
- solicitar definición
