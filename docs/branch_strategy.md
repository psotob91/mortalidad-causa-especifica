# Estrategia de ramas

## Rama recomendada para trabajo y operacion

La rama oficial para clonar, montar y ejecutar el proyecto es:

- `main`

Esa rama contiene solo la capa vigente del pipeline:

- scripts canónicos operativos
- helpers vigentes
- diagnósticos útiles
- mantenimiento recurrente útil
- configuración runtime
- manuales de operación
- insumos públicos versionables

## Rama histórica de respaldo

La rama:

- `deprecated/full-history`

preserva historia técnica, rutas legacy, wrappers, scripts retirados y material de referencia que puede servir para trazabilidad o rescate puntual.

## Cuándo usar cada una

Usa `main` si:

- vas a correr el pipeline
- vas a montar el proyecto en una nueva PC o servidor
- vas a mantener el flujo vigente
- vas a revisar la documentación operativa

Consulta `deprecated/full-history` solo si:

- necesitas rastrear una decisión histórica
- necesitas rescatar una referencia antigua
- quieres comparar la estructura final con versiones previas

## Advertencia

`deprecated/full-history` no es la rama recomendada para operación regular. La ejecución, validación y documentación vigentes están definidas para `main`.
