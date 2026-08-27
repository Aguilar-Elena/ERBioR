source(file.path("R","excel_io.R"))

cat("ERBioR Shiny v0.7 smoke tests\n")
cat("=============================\n\n")
erbio_require_app_packages()

# 1. Header normalization
stopifnot(
  identical(
    .erbio_norm_header(c("Pregunta","RESPUESTA","Sección","Nº")),
    c("pregunta","respuesta","seccion","numero")
  )
)
cat("1) Header normalization — PASS\n")

# 2. Applicability heuristic: simple, historical and task-dependent cases.
tests <- c(
  "Si existe riesgo de exposición a agentes biológicos para los que haya vacunas eficaces...",
  "La empresa que utilizaba en el momento de la entrada en vigor del R.D. 664/1997...",
  "En los servicios de aislamiento en que se encuentren pacientes...",
  "¿Utiliza mascarillas cuando se prevea la formación de aerosoles?"
)
stopifnot(all(.erbio_is_conditional_item(tests)))
stopifnot(
  !.erbio_is_conditional_item("¿Están limpias las instalaciones?")
)
cat("2) Expanded applicability heuristic — PASS\n")

# 3. Editorial wording flag
e <- .erbio_editorial_flag(
  "Definir e implantar la medida técnica u organizativa necesaria para asegurar el cumplimiento efectivo del control descrito en el ítem."
)
stopifnot(e$flag == "REVISAR_REDACCION_GENERICA")
cat("3) Editorial wording flag — PASS\n")

# 4. Multiple workers template
tmp <- tempfile(fileext=".xlsx")
sid <- ERBioR::erbio_sector_questionnaires()$questionnaire_id[[1]]
erbio_make_excel_template(sid, tmp, n_workers=3)
w <- .erbio_read_sheet(tmp, "05_TRABAJADORES")
stopifnot(nrow(w) == 102L)
stopifnot(length(unique(w$worker_id)) == 3L)
stopifnot("ayuda_cumplimentacion" %in% names(w))
cat("4) Multiple workers template — PASS\n")

# 5. Agent lookup and hand-off
prep <- erbio_prepare_agent_resolution(
  c("Brucella Abortus", "Mycobacterium tuberculosus")
)
selected <- stats::setNames(
  c("Brucella abortus", "Mycobacterium tuberculosis"),
  c("Brucella Abortus", "Mycobacterium tuberculosus")
)
refs <- .erbio_resolve_excel_agents(
  c("Brucella Abortus", "Mycobacterium tuberculosus"),
  selected_agents=selected,
  prepared_resolution=prep
)
stopifnot(refs[[1]]$agent_name == "Brucella abortus")
stopifnot(refs[[2]]$agent_name == "Mycobacterium tuberculosis")
cat("5) Agent resolution hand-off — PASS\n")

# 6. Highest-observed descriptor
rr <- data.frame(
  input_agent=c("A","A","B","B"),
  agent_name=c("Agent A","Agent A","Agent B","Agent B"),
  reference_level=c(3,3,3,3),
  exposure=c("Frecuente","Frecuente","Continua","Continua"),
  questionnaire_name=c("General","Sector","General","Sector"),
  risk_score=c(3,9,6,12),
  risk_class=c("Tolerable","Importante","Moderado","Intolerable"),
  priority=c("Media","Alta","Media-alta","Inmediata"),
  stringsAsFactors=FALSE
)
hs <- .erbio_highest_observed_summary(rr)
stopifnot(nrow(hs) == 2L)
stopifnot(hs$highest_observed_risk_class[hs$input_agent=="B"] == "Intolerable")
cat("6) Highest observed risk descriptor — PASS\n")

cat("\nALL APP v0.7 SMOKE TESTS PASSED\n")
