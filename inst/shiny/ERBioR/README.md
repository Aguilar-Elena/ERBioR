# ERBioR Shiny v0.7

Interfaz profesional para ERBioR 0.9.0.

## Cambios v0.7

### Dashboard en Resultados
Se añaden cuatro tarjetas:
- agentes evaluados;
- cuestionarios evaluados por agente;
- mayor riesgo observado;
- número de resultados con prioridad inmediata.

### Dashboard en Planificación
Se añaden cuatro tarjetas:
- medidas preventivas activas;
- medidas marcadas para revisión de redacción;
- ítems en los que debe revisarse la aplicabilidad;
- medidas procedentes del cuestionario de trabajadores.

### Qué significa «revisión de redacción»
Es una heurística editorial. Marca medidas activas que contienen fórmulas
predefinidas demasiado genéricas. No crea una nueva deficiencia, no modifica
la clase de riesgo y no significa que el cálculo científico sea incorrecto.

### Qué significa «revisar aplicabilidad»
Identifica preguntas condicionadas, históricas o dependientes de una tarea,
instalación o situación concreta que han sido respondidas `No`. El técnico
debe confirmar que la condición existía; si no existía, puede corresponder
`No procede`.

La v0.7 amplía esta detección a supuestos históricos y dependientes de tareas,
no únicamente a preguntas que comienzan literalmente por `Si`.

## Ejecución

```r
source("00_INSTALL_DEPENDENCIES.R")
source("02_SMOKE_TEST_APP.R")
shiny::runApp(".")
```

El test debe terminar con:

`ALL APP v0.7 SMOKE TESTS PASSED`
