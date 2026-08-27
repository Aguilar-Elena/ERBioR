###############################################################################
# ERBioR core v0.4
# Computational reconstruction of the ERBio biological-risk assessment engine
# Source basis: Aguilar Elena doctoral thesis (2015), Chapter 4 and Annex 3.
#
# STATUS
# ------
# This is an auditable CORE PROTOTYPE, not yet a frozen/validated package release.
# It implements the deterministic parts that are unambiguous and makes source
# ambiguities explicit instead of silently resolving them.
#
# IMPORTANT SOURCE ISSUES (see audit document)
# --------------------------------------------
# 1) Probability matrix: corrected versus the legacy Shiny script at
#    Mejorable + Irregular => Baja (1), not Media (2).
# 2) Compliance boundary at exactly 25% is internally inconsistent in the thesis
#    (Table 45 interval notation vs prose). The boundary policy is recorded.
# 3) Exposure percentage ranges overlap at 1, 5, 10 and 50%; strict mode refuses
#    to assign those exact boundaries unless the caller chooses a policy.
# 4) The thesis operational example uses binary SI/NO/NO PROCEDE scoring even for
#    the audit questionnaire, while Chapter 4 also describes a 0-4 audit score.
#    Both are implemented as separate functions.
# 5) The thesis does not define a general aggregation rule when the four
#    questionnaire-level risk classes disagree. This prototype therefore does
#    not silently compute a global risk class.
###############################################################################

ERBIOR_SOURCE_VERSION <- "ERBio doctoral thesis 2015 | core reconstruction v0.4"

.erbio_stop <- function(...) stop(..., call. = FALSE)

.erbio_norm <- function(x) {
  x <- trimws(tolower(as.character(x)))
  # Conservative accent normalization for expected Spanish labels.
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "aeiouun", x)
  x <- gsub("[[:space:]_\\-]+", " ", x)
  x
}

# -----------------------------------------------------------------------------
# 1. Authoritative deterministic lookup tables
# -----------------------------------------------------------------------------

erbio_probability_matrix <- function() {
  data.frame(
    compliance = c("Muy deficiente", "Deficiente", "Mejorable", "Aceptable"),
    Continuo = c(4L, 4L, 3L, 2L),
    Muy_frecuente = c(4L, 3L, 2L, 2L),
    Frecuente = c(3L, 3L, 2L, 1L),
    Irregular = c(3L, 2L, 1L, 1L),
    Ocasional = c(2L, 1L, 1L, 1L),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

erbio_risk_matrix <- function() {
  # Rows = reference level; columns = probability.
  # Risk score is the product. Class is derived deterministically below.
  out <- expand.grid(
    reference_level = 1:4,
    probability = 1:4,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )
  out$risk_score <- out$reference_level * out$probability
  out$risk_class <- vapply(out$risk_score, erbio_risk_class, character(1))
  out[order(out$reference_level, out$probability), ]
}

erbio_risk_actions <- function() {
  data.frame(
    risk_class = c("Trivial", "Tolerable", "Moderado", "Importante", "Intolerable"),
    priority = c("Baja", "Media", "Media-alta", "Alta", "Inmediata"),
    action = c(
      "No se requiere accion especifica; revisar periodicamente las condiciones para evitar aumento del riesgo.",
      "No es necesario introducir mejoras salvo que un analisis mas preciso lo justifique; considerar soluciones rentables y realizar comprobaciones periodicas de la eficacia de las medidas de control.",
      "Realizar esfuerzos para reducir el riesgo y determinar las inversiones precisas; implantar las medidas en un periodo determinado y, cuando proceda, precisar mejor la probabilidad de dano.",
      "Corregir la situacion de forma inmediata; pueden ser necesarios recursos considerables y, si el trabajo esta en curso, remediar en un plazo inferior al de los riesgos moderados.",
      "Situacion critica: accion inmediata; detener el trabajo hasta reducir el riesgo y, si no puede reducirse incluso con recursos ilimitados, prohibir el trabajo."
    ),
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# 2. Exposure classification
# -----------------------------------------------------------------------------

erbio_exposure_from_cadence <- function(cadence) {
  x <- .erbio_norm(cadence)
  map <- c(
    "mensual" = "Ocasional",
    "una vez al mes" = "Ocasional",
    "monthly" = "Ocasional",
    "semanal" = "Irregular",
    "una vez a la semana" = "Irregular",
    "weekly" = "Irregular",
    "diaria" = "Frecuente",
    "diario" = "Frecuente",
    "una vez al dia" = "Frecuente",
    "daily" = "Frecuente",
    "horaria" = "Muy frecuente",
    "cada hora" = "Muy frecuente",
    "una vez en una hora" = "Muy frecuente",
    "hourly" = "Muy frecuente",
    "continua" = "Continuo",
    "continuo" = "Continuo",
    "toda la jornada" = "Continuo",
    "continuous" = "Continuo"
  )
  if (!x %in% names(map)) {
    .erbio_stop(
      "Cadencia no reconocida. Use mensual/semanal/diaria/horaria/continua ",
      "o proporcione directamente una categoria ERBio."
    )
  }
  unname(map[[x]])
}

erbio_exposure_level <- function(
  exposure = NULL,
  time_percent = NULL,
  boundary_policy = c("strict", "upper_category", "lower_category")
) {
  boundary_policy <- match.arg(boundary_policy)

  if (!is.null(exposure) && !is.null(time_percent)) {
    .erbio_stop("Proporcione solo 'exposure' o 'time_percent', no ambos.")
  }

  if (!is.null(exposure)) {
    x <- .erbio_norm(exposure)
    map <- c(
      "ocasional" = "Ocasional",
      "irregular" = "Irregular",
      "frecuente" = "Frecuente",
      "muy frecuente" = "Muy frecuente",
      "continuo" = "Continuo",
      "continua" = "Continuo"
    )
    if (!x %in% names(map)) .erbio_stop("Categoria de exposicion ERBio no reconocida.")
    return(unname(map[[x]]))
  }

  if (is.null(time_percent) || length(time_percent) != 1 || !is.finite(time_percent)) {
    .erbio_stop("Debe proporcionar una categoria de exposicion o un porcentaje temporal valido.")
  }
  p <- as.numeric(time_percent)
  if (p < 0 || p > 100) .erbio_stop("time_percent debe estar entre 0 y 100.")

  # Thesis: Ocasional 0.1-1%; Irregular 1-5%; Frecuente 5-10%;
  # Muy frecuente 10-50%; Continua >50% / toda la jornada.
  # Exact boundaries 1,5,10,50 overlap in the prose source.
  ambiguous <- p %in% c(1, 5, 10, 50)
  if (ambiguous && boundary_policy == "strict") {
    .erbio_stop(
      "El porcentaje coincide con un limite solapado en la tesis (1, 5, 10 o 50%). ",
      "Seleccione boundary_policy='upper_category' o 'lower_category' y registre la decision."
    )
  }

  if (p == 0) {
    return("Sin exposicion temporal declarada")
  }
  if (p > 0 && p < 0.1) {
    .erbio_stop(
      "La tesis define Ocasional desde 0,1% del tiempo. Valores >0 y <0,1% no tienen categoria porcentual explicita; use una categoria/cadencia o documente una extension."
    )
  }

  if (!ambiguous) {
    if (p < 1) return("Ocasional")
    if (p < 5) return("Irregular")
    if (p < 10) return("Frecuente")
    if (p < 50) return("Muy frecuente")
    return("Continuo")
  }

  if (boundary_policy == "upper_category") {
    if (p == 1) return("Irregular")
    if (p == 5) return("Frecuente")
    if (p == 10) return("Muy frecuente")
    if (p == 50) return("Continuo")
  }
  if (boundary_policy == "lower_category") {
    if (p == 1) return("Ocasional")
    if (p == 5) return("Irregular")
    if (p == 10) return("Frecuente")
    if (p == 50) return("Muy frecuente")
  }

  .erbio_stop("No se pudo clasificar el nivel de exposicion.")
}

# -----------------------------------------------------------------------------
# 3. Compliance scoring
# -----------------------------------------------------------------------------

.erbio_binary_value <- function(x) {
  y <- .erbio_norm(x)
  yes <- y %in% c("si", "s", "yes", "true", "1", "cumple", "compliant")
  no <- y %in% c("no", "n", "false", "0", "no cumple", "noncompliant")
  nax <- is.na(x) | y %in% c("na", "n/a", "no procede", "no aplica", "not applicable", "np", "")
  out <- rep(NA_integer_, length(y))
  out[yes] <- 1L
  out[no] <- 0L
  out[nax] <- NA_integer_
  unknown <- !(yes | no | nax)
  if (any(unknown)) {
    .erbio_stop("Respuesta binaria no reconocida: ", paste(unique(as.character(x[unknown])), collapse = ", "))
  }
  out
}

erbio_compliance_binary <- function(responses) {
  if (length(responses) == 0) .erbio_stop("No hay respuestas.")
  x <- .erbio_binary_value(responses)
  n_applicable <- sum(!is.na(x))
  if (n_applicable == 0) .erbio_stop("No hay items aplicables; no puede calcularse el cumplimiento.")
  n_yes <- sum(x == 1L, na.rm = TRUE)
  n_no <- sum(x == 0L, na.rm = TRUE)
  pct <- 100 * n_yes / n_applicable
  list(
    percent = pct,
    n_total = length(x),
    n_applicable = n_applicable,
    n_compliant = n_yes,
    n_noncompliant = n_no,
    n_not_applicable = sum(is.na(x)),
    scoring = "binary_yes_no_na"
  )
}

erbio_compliance_audit_0_4 <- function(scores) {
  # Chapter 4 describes each audit item as 0..4. The thesis does not state the
  # arithmetic formula explicitly; sum(score)/(4*n_applicable)*100 is the direct
  # computational operationalization of 'percentage of total possible points'.
  if (length(scores) == 0) .erbio_stop("No hay puntuaciones.")
  x <- suppressWarnings(as.numeric(scores))
  invalid <- !is.na(x) & (!is.finite(x) | x < 0 | x > 4 | x != floor(x))
  if (any(invalid)) .erbio_stop("Las puntuaciones de auditoria deben ser enteros 0,1,2,3,4 o NA.")
  n_applicable <- sum(!is.na(x))
  if (n_applicable == 0) .erbio_stop("No hay items aplicables; no puede calcularse el cumplimiento.")
  pct <- 100 * sum(x, na.rm = TRUE) / (4 * n_applicable)
  list(
    percent = pct,
    n_total = length(x),
    n_applicable = n_applicable,
    points_observed = sum(x, na.rm = TRUE),
    points_possible = 4 * n_applicable,
    n_not_applicable = sum(is.na(x)),
    n_below_full = sum(x < 4, na.rm = TRUE),
    scoring = "audit_0_4_chapter4_operationalization"
  )
}

erbio_compliance_class <- function(
  percent,
  boundary_policy = c("prose_2015", "table45_intervals", "strict")
) {
  boundary_policy <- match.arg(boundary_policy)
  if (length(percent) != 1 || !is.finite(percent) || percent < 0 || percent > 100) {
    .erbio_stop("percent debe ser un unico valor entre 0 y 100.")
  }
  p <- as.numeric(percent)

  # Table 45 interval notation and the prose conflict at exactly 25%.
  if (boundary_policy == "strict" && p == 25) {
    .erbio_stop(
      "25% es un limite internamente discordante en la fuente. ",
      "Use boundary_policy='prose_2015' o 'table45_intervals' y registre la decision."
    )
  }

  if (boundary_policy == "prose_2015") {
    # Prose explicitly says 'igual o menor del 25%' for Muy deficiente.
    if (p <= 25) return("Muy deficiente")
    if (p <= 50) return("Deficiente")
    if (p <= 75) return("Mejorable")
    return("Aceptable")
  }

  if (boundary_policy == "table45_intervals") {
    # Printed intervals: [0,25), (25,50], (50,75], (75,100].
    # Exactly 25 is left uncovered in the printed interval notation.
    if (p < 25) return("Muy deficiente")
    if (p == 25) return(NA_character_)
    if (p <= 50) return("Deficiente")
    if (p <= 75) return("Mejorable")
    return("Aceptable")
  }

  # strict is equivalent to prose away from the disputed 25% boundary.
  if (p < 25) return("Muy deficiente")
  if (p <= 50) return("Deficiente")
  if (p <= 75) return("Mejorable")
  "Aceptable"
}

# -----------------------------------------------------------------------------
# 4. Probability and risk
# -----------------------------------------------------------------------------

erbio_probability <- function(compliance_class, exposure) {
  c0 <- .erbio_norm(compliance_class)
  e0 <- .erbio_norm(exposure)
  c_map <- c(
    "muy deficiente" = "Muy deficiente",
    "deficiente" = "Deficiente",
    "mejorable" = "Mejorable",
    "aceptable" = "Aceptable"
  )
  e_map <- c(
    "continuo" = "Continuo",
    "continua" = "Continuo",
    "muy frecuente" = "Muy_frecuente",
    "frecuente" = "Frecuente",
    "irregular" = "Irregular",
    "ocasional" = "Ocasional"
  )
  if (!c0 %in% names(c_map)) .erbio_stop("Clase de cumplimiento no reconocida.")
  if (!e0 %in% names(e_map)) .erbio_stop("Nivel de exposicion no reconocido.")
  tab <- erbio_probability_matrix()
  row <- match(unname(c_map[[c0]]), tab$compliance)
  col <- unname(e_map[[e0]])
  value <- as.integer(tab[row, col])
  text <- c("1" = "Baja", "2" = "Media", "3" = "Alta", "4" = "Muy Alta")[[as.character(value)]]
  list(value = value, label = text)
}

erbio_risk_score <- function(probability, reference_level) {
  p <- as.integer(probability)
  r <- as.integer(reference_level)
  if (length(p) != 1 || is.na(p) || !p %in% 1:4) .erbio_stop("probability debe ser 1,2,3 o 4.")
  if (length(r) != 1 || is.na(r) || !r %in% 1:4) .erbio_stop("reference_level debe ser 1,2,3 o 4.")
  as.integer(p * r)
}

erbio_risk_class <- function(score) {
  s <- as.integer(score)
  if (length(s) != 1 || is.na(s) || !s %in% c(1,2,3,4,6,8,9,12,16)) {
    .erbio_stop("Puntuacion ERBio no valida: ", score)
  }
  if (s == 1) return("Trivial")
  if (s %in% c(2,3)) return("Tolerable")
  if (s %in% c(4,6)) return("Moderado")
  if (s %in% c(8,9)) return("Importante")
  "Intolerable"
}

erbio_action <- function(risk_class) {
  tab <- erbio_risk_actions()
  idx <- match(.erbio_norm(risk_class), .erbio_norm(tab$risk_class))
  if (is.na(idx)) .erbio_stop("Clase de riesgo no reconocida.")
  list(priority = tab$priority[idx], action = tab$action[idx])
}

# -----------------------------------------------------------------------------
# 5. Failed controls => preventive planning candidates
# -----------------------------------------------------------------------------

erbio_failed_controls <- function(responses, item_text = NULL, scoring = c("binary", "audit_0_4")) {
  scoring <- match.arg(scoring)
  if (is.null(item_text)) item_text <- paste0("Item ", seq_along(responses))
  if (length(item_text) != length(responses)) .erbio_stop("item_text debe tener la misma longitud que responses.")

  if (scoring == "binary") {
    x <- .erbio_binary_value(responses)
    idx <- which(!is.na(x) & x == 0L)
    return(data.frame(
      item = idx,
      text = item_text[idx],
      observed = rep("No", length(idx)),
      planning_candidate = item_text[idx],
      stringsAsFactors = FALSE
    ))
  }

  x <- suppressWarnings(as.numeric(responses))
  invalid <- !is.na(x) & (!is.finite(x) | x < 0 | x > 4 | x != floor(x))
  if (any(invalid)) .erbio_stop("Puntuaciones de auditoria invalidas.")
  idx <- which(!is.na(x) & x < 4)
  data.frame(
    item = idx,
    text = item_text[idx],
    observed = x[idx],
    planning_candidate = item_text[idx],
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# 6. Questionnaire-level assessment
# -----------------------------------------------------------------------------

erbio_assess <- function(
  reference_level,
  exposure,
  compliance_percent = NULL,
  responses = NULL,
  scoring = c("binary", "audit_0_4"),
  item_text = NULL,
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override")
) {
  scoring <- match.arg(scoring)
  compliance_boundary_policy <- match.arg(compliance_boundary_policy)
  full_compliance_rule <- match.arg(full_compliance_rule)

  expo <- erbio_exposure_level(exposure = exposure)

  if (!is.null(compliance_percent) && !is.null(responses)) {
    .erbio_stop("Proporcione compliance_percent o responses, no ambos.")
  }
  if (is.null(compliance_percent) && is.null(responses)) {
    .erbio_stop("Debe proporcionar compliance_percent o responses.")
  }

  scoring_detail <- NULL
  failed <- NULL
  if (!is.null(responses)) {
    scoring_detail <- if (scoring == "binary") {
      erbio_compliance_binary(responses)
    } else {
      erbio_compliance_audit_0_4(responses)
    }
    compliance_percent <- scoring_detail$percent
    failed <- erbio_failed_controls(responses, item_text = item_text, scoring = scoring)
  }

  compliance_class <- erbio_compliance_class(
    compliance_percent,
    boundary_policy = compliance_boundary_policy
  )

  if (is.na(compliance_class)) {
    .erbio_stop("El porcentaje de cumplimiento no puede clasificarse bajo la politica seleccionada.")
  }

  prob <- erbio_probability(compliance_class, expo)
  score <- erbio_risk_score(prob$value, reference_level)
  cls <- erbio_risk_class(score)
  act <- erbio_action(cls)

  full_compliance <- isTRUE(all.equal(as.numeric(compliance_percent), 100, tolerance = 1e-12))
  status <- "matrix_assessment"
  source_warning <- character(0)

  if (full_compliance) {
    source_warning <- c(
      source_warning,
      "La tesis indica que si se cumplen todos los items aplicables puede concluirse que, mientras se mantengan las condiciones, no hay exposicion. Esta regla entra en tension con la matriz de riesgo cuando se declara exposicion."
    )
    if (full_compliance_rule == "thesis_override") {
      status <- "no_exposure_while_conditions_remain_thesis_rule"
    }
  }

  if (as.numeric(compliance_percent) == 25) {
    source_warning <- c(
      source_warning,
      "25% es un limite discordante entre la prosa y la notacion de intervalos de la Tabla 45; la politica usada queda registrada."
    )
  }

  out <- list(
    source_version = ERBIOR_SOURCE_VERSION,
    assessment_status = status,
    reference_level = as.integer(reference_level),
    exposure = expo,
    compliance_percent = as.numeric(compliance_percent),
    compliance_class = compliance_class,
    probability_value = prob$value,
    probability_label = prob$label,
    risk_score = score,
    risk_class = cls,
    priority = act$priority,
    action = act$action,
    scoring = scoring,
    scoring_detail = scoring_detail,
    failed_controls = failed,
    compliance_boundary_policy = compliance_boundary_policy,
    full_compliance_rule = full_compliance_rule,
    warnings = source_warning
  )
  class(out) <- c("erbio_assessment", "list")
  out
}

print.erbio_assessment <- function(x, ...) {
  cat("ERBioR assessment\n")
  cat("------------------\n")
  cat("Reference level:      ", x$reference_level, "\n", sep = "")
  cat("Exposure:             ", x$exposure, "\n", sep = "")
  cat("Compliance:           ", sprintf("%.2f%%", x$compliance_percent), " (", x$compliance_class, ")\n", sep = "")
  cat("Probability:          ", x$probability_value, " (", x$probability_label, ")\n", sep = "")
  cat("Risk score:           ", x$risk_score, "\n", sep = "")
  cat("Risk class:           ", x$risk_class, "\n", sep = "")
  cat("Priority:             ", x$priority, "\n", sep = "")
  cat("Assessment status:    ", x$assessment_status, "\n", sep = "")
  if (length(x$warnings)) {
    cat("Warnings:\n")
    for (w in x$warnings) cat(" - ", w, "\n", sep = "")
  }
  invisible(x)
}

# -----------------------------------------------------------------------------
# 7. Multiple-questionnaire set (NO silent global aggregation)
# -----------------------------------------------------------------------------

erbio_assess_set <- function(assessments, aggregation_rule = c("unresolved", "max_risk_extension")) {
  aggregation_rule <- match.arg(aggregation_rule)
  if (!is.list(assessments) || length(assessments) == 0) .erbio_stop("assessments debe ser una lista no vacia.")
  valid <- vapply(assessments, inherits, logical(1), what = "erbio_assessment")
  if (!all(valid)) .erbio_stop("Todos los elementos deben ser objetos erbio_assessment.")

  scores <- vapply(assessments, `[[`, integer(1), "risk_score")
  classes <- vapply(assessments, `[[`, character(1), "risk_class")

  if (length(unique(classes)) == 1) {
    return(list(
      questionnaire_results = assessments,
      global_risk_class = unique(classes),
      global_risk_score = max(scores),
      aggregation_status = "unanimous_questionnaire_classes",
      note = "El ejemplo de la tesis considera global el mismo grado cuando todos los cuestionarios coinciden."
    ))
  }

  if (aggregation_rule == "unresolved") {
    return(list(
      questionnaire_results = assessments,
      global_risk_class = NA_character_,
      global_risk_score = NA_integer_,
      aggregation_status = "source_rule_unresolved_for_discordant_questionnaires",
      note = "La tesis no define una regla general de agregacion cuando los cuestionarios producen clases distintas. No se ha inferido una."
    ))
  }

  # Explicit extension, NOT a thesis rule.
  idx <- which.max(scores)
  list(
    questionnaire_results = assessments,
    global_risk_class = classes[idx],
    global_risk_score = scores[idx],
    aggregation_status = "max_risk_operational_extension_not_source_rule",
    note = "Regla conservadora de maximo riesgo solicitada por el usuario; no esta definida como regla general en la fuente original."
  )
}



# =============================================================================
# 8. ERBioR v0.2 questionnaire layer
# =============================================================================
# v0.2 adds source-versioned questionnaires extracted from the final Annex 1
# instruments and their validation lineage. The deterministic v0.1 engine above
# is intentionally preserved.

ERBIOR_QUESTION_BANK_VERSION <- "ERBioR_question_bank_v0_2.csv"
ERBIOR_QUESTIONNAIRE_REGISTRY_VERSION <- "ERBioR_questionnaire_registry_v0_2.csv"

.erbio_find_data_file <- function(filename, path = NULL) {
  candidates <- character(0)
  if (!is.null(path)) {
    if (dir.exists(path)) candidates <- c(candidates, file.path(path, filename)) else candidates <- c(candidates, path)
  }
  opt <- getOption("erbio.data_dir", NULL)
  if (!is.null(opt)) candidates <- c(candidates, file.path(opt, filename))
  env <- Sys.getenv("ERBIOR_DATA_DIR", unset = "")
  if (nzchar(env)) candidates <- c(candidates, file.path(env, filename))
  pkg_file <- system.file("extdata", filename, package = "ERBioR")
  if (nzchar(pkg_file)) candidates <- c(candidates, pkg_file)
  candidates <- c(
    candidates,
    file.path(getwd(), filename),
    file.path(getwd(), "data", filename)
  )
  candidates <- unique(candidates)
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    .erbio_stop(
      "No se encuentra ", filename, ". Coloque el CSV en el directorio de trabajo, ",
      "en ./data, use options(erbio.data_dir='...') o proporcione path=."
    )
  }
  normalizePath(found[[1]], winslash = "/", mustWork = TRUE)
}

erbio_load_question_bank <- function(path = NULL) {
  f <- .erbio_find_data_file(ERBIOR_QUESTION_BANK_VERSION, path)
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
  required <- c(
    "questionnaire_id", "questionnaire_name", "version", "instrument_type",
    "sector", "subtype", "section", "final_item", "original_item", "item_id",
    "item_text", "default_scoring", "allowed_scoring", "validation_status"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) .erbio_stop("Question bank incompleto. Faltan columnas: ", paste(missing, collapse = ", "))
  x$final_item <- as.integer(x$final_item)
  x$original_item <- as.integer(x$original_item)
  class(x) <- c("erbio_question_bank", class(x))
  attr(x, "source_file") <- f
  x
}

erbio_load_questionnaire_registry <- function(path = NULL) {
  f <- .erbio_find_data_file(ERBIOR_QUESTIONNAIRE_REGISTRY_VERSION, path)
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
  class(x) <- c("erbio_questionnaire_registry", class(x))
  attr(x, "source_file") <- f
  x
}

erbio_questionnaire_catalog <- function(path = NULL) {
  b <- erbio_load_question_bank(path)
  ids <- unique(b$questionnaire_id)
  out <- do.call(rbind, lapply(ids, function(id) {
    z <- b[b$questionnaire_id == id, , drop = FALSE]
    data.frame(
      questionnaire_id = id,
      questionnaire_name = z$questionnaire_name[[1]],
      version = z$version[[1]],
      instrument_type = z$instrument_type[[1]],
      sector = z$sector[[1]],
      subtype = z$subtype[[1]],
      n_items = nrow(z),
      validation_status = z$validation_status[[1]],
      stringsAsFactors = FALSE
    )
  }))
  rownames(out) <- NULL
  out
}

erbio_get_questionnaire <- function(questionnaire_id, version = NULL, path = NULL) {
  b <- erbio_load_question_bank(path)
  id <- as.character(questionnaire_id)[1]
  z <- b[b$questionnaire_id == id, , drop = FALSE]
  if (!nrow(z)) .erbio_stop("questionnaire_id no reconocido: ", id)
  if (!is.null(version)) {
    z <- z[z$version == version, , drop = FALSE]
    if (!nrow(z)) .erbio_stop("No existe la version solicitada para ", id, ": ", version)
  }
  # v0.2 bank contains one active version per questionnaire_id. Refuse ambiguity if this changes.
  versions <- unique(z$version)
  if (length(versions) != 1L) {
    .erbio_stop("Hay varias versiones activas de ", id, "; especifique version= explicitamente.")
  }
  z <- z[order(z$final_item), , drop = FALSE]
  expected <- seq_len(nrow(z))
  if (!identical(z$final_item, as.integer(expected))) {
    .erbio_stop("La numeracion final del cuestionario no es consecutiva: ", id)
  }
  class(z) <- c("erbio_questionnaire", class(z))
  z
}

erbio_validate_question_bank <- function(path = NULL) {
  b <- erbio_load_question_bank(path)
  issues <- character(0)
  if (anyDuplicated(b$item_id)) issues <- c(issues, "item_id duplicados")
  key <- paste(b$questionnaire_id, b$version, b$final_item, sep = "|")
  if (anyDuplicated(key)) issues <- c(issues, "questionnaire/version/final_item duplicados")
  ids <- unique(b$questionnaire_id)
  for (id in ids) {
    z <- b[b$questionnaire_id == id, , drop = FALSE]
    if (!identical(sort(as.integer(z$final_item)), seq_len(nrow(z)))) {
      issues <- c(issues, paste0("secuencia de items no consecutiva: ", id))
    }
    if (any(!nzchar(trimws(z$item_text)))) issues <- c(issues, paste0("item_text vacio: ", id))
  }
  list(
    valid = !length(issues),
    n_questionnaires = length(ids),
    n_items = nrow(b),
    issues = unique(issues)
  )
}

.erbio_align_questionnaire_responses <- function(responses, q) {
  n <- nrow(q)
  if (is.null(responses)) .erbio_stop("responses no puede ser NULL.")
  if (!length(responses)) .erbio_stop("responses esta vacio.")

  nm <- names(responses)
  if (is.null(nm) || !any(nzchar(nm))) {
    if (length(responses) != n) {
      .erbio_stop("El cuestionario ", q$questionnaire_id[[1]], " requiere ", n,
                  " respuestas; se recibieron ", length(responses), ".")
    }
    names(responses) <- q$item_id
    return(responses)
  }

  if (any(!nzchar(nm))) .erbio_stop("Si se nombran las respuestas, todas deben tener nombre.")
  if (anyDuplicated(nm)) .erbio_stop("Hay nombres de respuesta duplicados.")

  # Accept stable item_id names or final item numbers as character names.
  if (all(nm %in% q$item_id)) {
    missing <- setdiff(q$item_id, nm)
    extra <- setdiff(nm, q$item_id)
    if (length(missing) || length(extra)) {
      .erbio_stop("Las respuestas nombradas no cubren exactamente todos los item_id.")
    }
    return(responses[q$item_id])
  }

  final_names <- as.character(q$final_item)
  if (all(nm %in% final_names)) {
    if (!setequal(nm, final_names)) .erbio_stop("Las respuestas por numero de item deben cubrir todos los items finales.")
    responses <- responses[final_names]
    names(responses) <- q$item_id
    return(responses)
  }

  .erbio_stop("Los nombres de responses deben ser item_id estables o numeros finales del cuestionario.")
}

erbio_score_questionnaire <- function(
  questionnaire_id,
  responses,
  version = NULL,
  scoring = c("default", "binary", "audit_0_4"),
  path = NULL
) {
  scoring <- match.arg(scoring)
  q <- erbio_get_questionnaire(questionnaire_id, version = version, path = path)
  x <- .erbio_align_questionnaire_responses(responses, q)

  if (scoring == "default") {
    scoring <- if (q$default_scoring[[1]] == "binary_yes_no_na") "binary" else q$default_scoring[[1]]
  }
  allowed <- unique(unlist(strsplit(q$allowed_scoring[[1]], "\\|", fixed = FALSE)))
  allowed2 <- c(
    if ("binary_yes_no_na" %in% allowed) "binary" else character(0),
    if ("audit_0_4" %in% allowed) "audit_0_4" else character(0)
  )
  if (!scoring %in% allowed2) {
    .erbio_stop("Scoring '", scoring, "' no permitido para ", questionnaire_id, ". Permitidos: ", paste(allowed2, collapse = ", "))
  }

  detail <- if (scoring == "binary") erbio_compliance_binary(x) else erbio_compliance_audit_0_4(x)
  failed0 <- erbio_failed_controls(x, item_text = q$item_text, scoring = scoring)
  failed <- if (nrow(failed0)) {
    idx <- failed0$item
    data.frame(
      item_id = q$item_id[idx],
      final_item = q$final_item[idx],
      original_item = q$original_item[idx],
      section = q$section[idx],
      item_text = q$item_text[idx],
      observed = failed0$observed,
      validation_status = q$validation_status[idx],
      planning_candidate = q$item_text[idx],
      stringsAsFactors = FALSE
    )
  } else {
    data.frame(
      item_id = character(0), final_item = integer(0), original_item = integer(0),
      section = character(0), item_text = character(0), observed = character(0),
      validation_status = character(0), planning_candidate = character(0),
      stringsAsFactors = FALSE
    )
  }

  out <- list(
    questionnaire_id = q$questionnaire_id[[1]],
    questionnaire_name = q$questionnaire_name[[1]],
    version = q$version[[1]],
    instrument_type = q$instrument_type[[1]],
    sector = q$sector[[1]],
    subtype = q$subtype[[1]],
    validation_status = q$validation_status[[1]],
    n_items = nrow(q),
    scoring = scoring,
    scoring_detail = detail,
    compliance_percent = detail$percent,
    failed_controls = failed,
    provenance = list(
      source = q$source[[1]],
      question_bank = ERBIOR_QUESTION_BANK_VERSION,
      source_lines = range(c(q$source_line_start, q$source_line_end), na.rm = TRUE)
    )
  )
  class(out) <- c("erbio_questionnaire_score", "list")
  out
}

print.erbio_questionnaire_score <- function(x, ...) {
  cat("ERBioR questionnaire score\n")
  cat("---------------------------\n")
  cat("Questionnaire: ", x$questionnaire_name, "\n", sep = "")
  cat("ID/version:    ", x$questionnaire_id, " / ", x$version, "\n", sep = "")
  cat("Items:         ", x$n_items, "\n", sep = "")
  cat("Validation:    ", x$validation_status, "\n", sep = "")
  cat("Scoring:       ", x$scoring, "\n", sep = "")
  cat("Compliance:    ", sprintf("%.2f%%", x$compliance_percent), "\n", sep = "")
  cat("Failed items:  ", nrow(x$failed_controls), "\n", sep = "")
  invisible(x)
}

erbio_assess_questionnaire <- function(
  questionnaire_id,
  responses,
  reference_level,
  exposure,
  version = NULL,
  scoring = c("default", "binary", "audit_0_4"),
  path = NULL,
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override")
) {
  scoring <- match.arg(scoring)
  compliance_boundary_policy <- match.arg(compliance_boundary_policy)
  full_compliance_rule <- match.arg(full_compliance_rule)

  qs <- erbio_score_questionnaire(
    questionnaire_id = questionnaire_id,
    responses = responses,
    version = version,
    scoring = scoring,
    path = path
  )

  # Use the already-source-versioned percentage; attach the enriched failed-control table below.
  a <- erbio_assess(
    reference_level = reference_level,
    exposure = exposure,
    compliance_percent = qs$compliance_percent,
    scoring = if (qs$scoring == "audit_0_4") "audit_0_4" else "binary",
    compliance_boundary_policy = compliance_boundary_policy,
    full_compliance_rule = full_compliance_rule
  )
  a$questionnaire_id <- qs$questionnaire_id
  a$questionnaire_name <- qs$questionnaire_name
  a$questionnaire_version <- qs$version
  a$instrument_type <- qs$instrument_type
  a$sector <- qs$sector
  a$subtype <- qs$subtype
  a$validation_status <- qs$validation_status
  a$questionnaire_score <- qs$scoring_detail
  a$failed_controls <- qs$failed_controls
  a$provenance <- qs$provenance
  if (identical(qs$validation_status, "not_validated_insufficient_sector_sample")) {
    a$warnings <- c(
      a$warnings,
      "El cuestionario sectorial se conserva desde el Anexo 1, pero la tesis indica que no fue validado por insuficiencia de muestra sectorial. No debe etiquetarse como instrumento validado."
    )
  }
  class(a) <- c("erbio_questionnaire_assessment", class(a))
  a
}

print.erbio_questionnaire_assessment <- function(x, ...) {
  cat("ERBioR v0.3 questionnaire assessment\n")
  cat("------------------------------------\n")
  cat("Questionnaire:        ", x$questionnaire_name, "\n", sep = "")
  cat("ID/version:           ", x$questionnaire_id, " / ", x$questionnaire_version, "\n", sep = "")
  cat("Validation status:    ", x$validation_status, "\n", sep = "")
  cat("Reference level:      ", x$reference_level, "\n", sep = "")
  cat("Exposure:             ", x$exposure, "\n", sep = "")
  cat("Compliance:           ", sprintf("%.2f%%", x$compliance_percent), " (", x$compliance_class, ")\n", sep = "")
  cat("Probability:          ", x$probability_value, " (", x$probability_label, ")\n", sep = "")
  cat("Risk score/class:     ", x$risk_score, " / ", x$risk_class, "\n", sep = "")
  cat("Failed controls:      ", nrow(x$failed_controls), "\n", sep = "")
  cat("Priority:             ", x$priority, "\n", sep = "")
  if (length(x$warnings)) {
    cat("Warnings:\n")
    for (w in x$warnings) cat(" - ", w, "\n", sep = "")
  }
  invisible(x)
}

# Source-preserved questionnaire-level branching notes relevant to execution.
erbio_questionnaire_rules <- function(questionnaire_id = NULL) {
  rules <- data.frame(
    questionnaire_id = c("audit", "audit", "all"),
    rule_id = c("AUD-G1-STOP", "AUD-NONDELIBERATE-SCOPE", "NA-DENOM"),
    rule = c(
      "Si la evaluaci\u00f3n muestra \u00fanicamente exposici\u00f3n posible a un agente del grupo 1 sin riesgo conocido, el Anexo 1 indica no aplicar el resto de preguntas de auditor\u00eda, manteniendo evaluaci\u00f3n y principios de correcta seguridad e higiene profesional.",
      "En actividades sin intenci\u00f3n deliberada de manipular agentes biol\u00f3gicos pero con posible exposici\u00f3n, el Anexo 1 indica aplicar los requisitos salvo los espec\u00edficamente exceptuados seg\u00fan el contexto de establecimientos sanitarios/veterinarios, laboratorios, locales para animales o procedimientos industriales.",
      "Los \u00edtems 'No procede' se excluyen del denominador del porcentaje de cumplimiento."
    ),
    source = c(
      "ERBio thesis 2015 Annex 1 audit instructions",
      "ERBio thesis 2015 Annex 1 audit instructions",
      "ERBio thesis 2015 Chapter 4 and validation/results"
    ),
    stringsAsFactors = FALSE
  )
  if (is.null(questionnaire_id)) return(rules)
  rules[rules$questionnaire_id %in% c(as.character(questionnaire_id), "all"), , drop = FALSE]
}


###############################################################################
# 12. v0.3 biological-agent registry and reference-level layer
###############################################################################

.erbio_agent_norm <- function(x) {
  x <- trimws(tolower(as.character(x)))
  x <- chartr("\u00e1\u00e9\u00ed\u00f3\u00fa\u00fc\u00f1", "aeiouun", x)
  x <- gsub("[[:punct:]]+", " ", x)
  x <- gsub("[[:space:]]+", " ", x)
  trimws(x)
}

.erbio_agent_file <- function(filename, path = NULL) {
  if (!is.null(path)) return(file.path(path, filename))
  data_dir <- getOption("erbio.data_dir", NULL)
  if (!is.null(data_dir)) return(file.path(data_dir, filename))
  env <- Sys.getenv("ERBIOR_DATA_DIR", unset = "")
  if (nzchar(env)) return(file.path(env, filename))
  pkg_file <- system.file("extdata", filename, package = "ERBioR")
  if (nzchar(pkg_file)) return(pkg_file)
  filename
}

erbio_load_agent_registry <- function(path = NULL) {
  f <- .erbio_agent_file("ERBioR_agent_registry_v0_8.csv", path)
  if (!file.exists(f)) .erbio_stop("No se encuentra el registro de agentes v0.8: ", f)
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
  logical_cols <- c(
    "group3_limited_airborne", "flag_A", "flag_D", "flag_T", "flag_V",
    "basebio_available",
    "eu_rowwise_verified",
    "eu_name_concordance",
    "eu_current_intended_name_concordance",
    "eu_spanish_original_text_name_concordance",
    "eu_group_concordance",
    "eu_double_asterisk_concordance",
    "eu_flag_A_concordance",
    "eu_flag_D_concordance",
    "eu_flag_T_concordance",
    "eu_flag_V_concordance",
    "eu_exact_raw_text_claimed",
    "eu_corrigendum_relevant",
    "audit_group_consistency", "audit_double_asterisk_consistency",
    "audit_flag_A_consistency", "audit_flag_D_consistency",
    "audit_flag_T_consistency", "audit_flag_V_consistency"
  )
  for (nm in intersect(logical_cols, names(x))) {
    if (!is.logical(x[[nm]])) x[[nm]] <- tolower(as.character(x[[nm]])) %in% c("true","t","1","yes","si","s\u00ed")
  }
  x$risk_group <- as.integer(x$risk_group)
  x$boe_line <- as.integer(x$boe_line)
  x
}

erbio_load_basebio_index <- function(path = NULL) {
  f <- .erbio_agent_file("ERBioR_basebio_index_v0_7.csv", path)
  if (!file.exists(f)) .erbio_stop("No se encuentra el indice BASEBiO v0.7: ", f)
  tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
}

erbio_agent_rules <- function(rule_id = NULL, path = NULL) {
  f <- .erbio_agent_file("ERBioR_agent_source_rules_v0_3.csv", path)
  if (!file.exists(f)) .erbio_stop("No se encuentran las reglas de fuente v0.3: ", f)
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
  if (is.null(rule_id)) return(x)
  hit <- x[x$rule_id %in% as.character(rule_id), , drop = FALSE]
  if (!nrow(hit)) .erbio_stop("Regla de agente no encontrada: ", paste(rule_id, collapse = ", "))
  hit
}

erbio_validate_agent_registry <- function(path = NULL) {
  x <- erbio_load_agent_registry(path)
  issues <- character(0)
  required <- c("agent_id","agent_type","agent_name","risk_group","classification_raw","source_url")
  missing_cols <- setdiff(required, names(x))
  if (length(missing_cols)) issues <- c(issues, paste("Columnas ausentes:", paste(missing_cols, collapse=", ")))
  if (anyDuplicated(x$agent_id)) issues <- c(issues, "agent_id duplicado")
  if (anyDuplicated(x$boe_line)) issues <- c(issues, "boe_line duplicada")
  if (any(!x$risk_group %in% 2:4)) issues <- c(issues, "Grupo fuera de 2-4")
  if (any(x$group3_limited_airborne & x$risk_group != 3, na.rm=TRUE)) issues <- c(issues, "Marca ** fuera de grupo 3")
  if (any(!nzchar(trimws(x$agent_name)))) issues <- c(issues, "Nombre de agente vacio")
  list(
    valid = length(issues) == 0L,
    n_agents = nrow(x),
    n_by_type = table(x$agent_type),
    n_by_group = table(x$risk_group),
    n_basebio_linked = sum(x$basebio_available, na.rm = TRUE),
    n_eu_verified = if ("eu_rowwise_verified" %in% names(x))
      sum(x$eu_rowwise_verified, na.rm = TRUE) else 0L,
    n_eu_name_concordant = if ("eu_name_concordance" %in% names(x))
      sum(x$eu_name_concordance, na.rm = TRUE) else 0L,
    n_eu_group_concordant = if ("eu_group_concordance" %in% names(x))
      sum(x$eu_group_concordance, na.rm = TRUE) else 0L,
    n_eu_name_discrepancies = if ("eu_name_concordance" %in% names(x))
      sum(!x$eu_name_concordance, na.rm = TRUE) else NA_integer_,
    issues = issues
  )
}

erbio_agent_lookup <- function(query, type = NULL, exact = TRUE, path = NULL) {
  x <- erbio_load_agent_registry(path)
  if (!is.null(type)) {
    type_norm <- .erbio_norm(type)
    map <- c("bacteria"="bacteria","bacterias"="bacteria","virus"="virus","prion"="prion","priones"="prion","parasito"="parasite","parasitos"="parasite","parasite"="parasite","hongo"="fungus","hongos"="fungus","fungus"="fungus")
    if (!type_norm %in% names(map)) .erbio_stop("Tipo de agente no reconocido: ", type)
    x <- x[x$agent_type == unname(map[[type_norm]]), , drop = FALSE]
  }
  q <- .erbio_agent_norm(query)
  vals <- .erbio_agent_norm(x$agent_name)
  rawvals <- .erbio_agent_norm(x$legal_name_es_raw)
  bbvals <- if ("basebio_name" %in% names(x)) .erbio_agent_norm(x$basebio_name) else rep("", nrow(x))
  if (isTRUE(exact)) {
    hit <- x[vals == q | rawvals == q | bbvals == q | x$agent_id == as.character(query), , drop = FALSE]
  } else {
    hit <- x[grepl(q, vals, fixed=TRUE) | grepl(q, rawvals, fixed=TRUE) | grepl(q, bbvals, fixed=TRUE), , drop = FALSE]
  }
  hit
}

erbio_reference_level_from_agent <- function(agent, path = NULL, allow_partial = FALSE) {
  hit <- erbio_agent_lookup(agent, exact = !allow_partial, path = path)
  if (!nrow(hit)) {
    return(structure(list(
      agent_query = as.character(agent),
      classification_status = "unclassified_not_implicitly_group1",
      reference_level = NA_integer_,
      regulatory_minimum_group = NA_integer_,
      requires_professional_assessment = TRUE,
      warnings = c("El agente no figura en el registro legal consultado. El RD 664/1997 establece que los agentes no incluidos en grupos 2-4 no quedan implicitamente clasificados en grupo 1."),
      source_rule = "UNLISTED-NOT-G1"
    ), class="erbio_agent_reference"))
  }
  if (nrow(hit) > 1L) .erbio_stop("La consulta devuelve varios agentes. Use un nombre mas especifico o agent_id.")
  structure(list(
    agent_query = as.character(agent),
    agent_id = hit$agent_id[[1]],
    agent_name = hit$agent_name[[1]],
    agent_type = hit$agent_type[[1]],
    classification_status = hit$regulatory_status[[1]],
    reference_level = as.integer(hit$risk_group[[1]]),
    regulatory_minimum_group = as.integer(hit$risk_group[[1]]),
    group3_limited_airborne = isTRUE(hit$group3_limited_airborne[[1]]),
    flags = c(A=isTRUE(hit$flag_A[[1]]),D=isTRUE(hit$flag_D[[1]]),T=isTRUE(hit$flag_T[[1]]),V=isTRUE(hit$flag_V[[1]])),
    basebio_available = isTRUE(hit$basebio_available[[1]]),
    basebio_name = hit$basebio_name[[1]],
    requires_professional_assessment = FALSE,
    warnings = if (isTRUE(hit$group3_limited_airborne[[1]])) "Grupo 3 con marca **: riesgo de infeccion potencialmente limitado por no ser normalmente infeccioso por via aerea; no implica descenso del grupo legal." else character(0),
    source_rule = "CLASSIFIED-ANNEX-II"
  ), class="erbio_agent_reference")
}

erbio_unclassified_human_virus <- function(agent_name) {
  structure(list(
    agent_query = as.character(agent_name),
    classification_status = "unclassified_human_isolated_virus",
    reference_level = NA_integer_,
    regulatory_minimum_group = 2L,
    requires_professional_assessment = TRUE,
    warnings = c("Virus aislado en humanos no evaluado/clasificado: el RD 664/1997 establece grupo 2 como minimo salvo demostracion de que es improbable que cause enfermedad. La clasificacion final requiere evaluacion profesional."),
    source_rule = "HUMAN-VIRUS-MIN-G2"
  ), class="erbio_agent_reference")
}

erbio_reference_level_from_agents <- function(agents, policy = c("report_all", "explicit_max_group"), path = NULL) {
  policy <- match.arg(policy)
  agents <- as.character(agents)
  refs <- lapply(agents, erbio_reference_level_from_agent, path = path)
  groups <- vapply(refs, function(z) if (length(z$reference_level) && !is.na(z$reference_level)) z$reference_level else NA_integer_, integer(1))
  unresolved <- anyNA(groups)
  unique_groups <- sort(unique(groups[!is.na(groups)]))
  result <- list(
    agents = agents,
    references = refs,
    risk_groups = groups,
    policy = policy,
    reference_level = NA_integer_,
    selection_status = "reported_without_collapse",
    warnings = character(0)
  )
  if (unresolved) {
    result$warnings <- c(result$warnings, "Al menos un agente carece de clasificacion legal resuelta; no se calcula un nivel de referencia unico.")
  } else if (length(unique_groups) == 1L) {
    result$reference_level <- unique_groups[[1]]
    result$selection_status <- "all_agents_same_group"
  } else if (policy == "explicit_max_group") {
    result$reference_level <- max(groups)
    result$selection_status <- "explicit_conservative_max_group_policy"
    result$warnings <- c(result$warnings, "Se ha aplicado una politica computacional conservadora explicita (grupo maximo). Esta regla no debe presentarse como una regla original de ERBio para seleccionar el agente mas probable.")
  } else {
    result$warnings <- c(result$warnings, "Los agentes clasificados pertenecen a grupos distintos. El RD 664/1997 exige considerar el peligro de todos los agentes presentes y ERBio selecciona el/los agentes mas probables; v0.3 no colapsa automaticamente grupos discordantes.")
  }
  class(result) <- "erbio_multi_agent_reference"
  result
}

erbio_assess_agent_questionnaire <- function(
  agent,
  questionnaire_id,
  responses,
  exposure,
  version = NULL,
  scoring = NULL,
  path = NULL,
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override")
) {
  ref <- erbio_reference_level_from_agent(agent, path = path)
  if (is.na(ref$reference_level)) .erbio_stop("No se puede evaluar automaticamente: el agente no tiene nivel de referencia legal resuelto.")
  a <- erbio_assess_questionnaire(
    questionnaire_id = questionnaire_id,
    responses = responses,
    reference_level = ref$reference_level,
    exposure = exposure,
    version = version,
    scoring = scoring,
    path = path,
    compliance_boundary_policy = match.arg(compliance_boundary_policy),
    full_compliance_rule = match.arg(full_compliance_rule)
  )
  a$agent_reference <- ref
  a$agent_id <- ref$agent_id
  a$agent_name <- ref$agent_name
  a$agent_type <- ref$agent_type
  a$basebio_available <- ref$basebio_available
  a$warnings <- unique(c(a$warnings, ref$warnings))
  class(a) <- c("erbio_agent_questionnaire_assessment", class(a))
  a
}

print.erbio_agent_reference <- function(x, ...) {
  cat("ERBioR v0.3 agent reference\n")
  cat("-----------------------------\n")
  if (!is.null(x$agent_name)) cat("Agent:                ", x$agent_name, "\n", sep="") else cat("Agent query:          ", x$agent_query, "\n", sep="")
  cat("Classification:       ", x$classification_status, "\n", sep="")
  cat("Reference level:      ", ifelse(is.na(x$reference_level), "NA", x$reference_level), "\n", sep="")
  if (!is.null(x$regulatory_minimum_group) && !is.na(x$regulatory_minimum_group)) cat("Regulatory minimum:   ", x$regulatory_minimum_group, "\n", sep="")
  cat("Professional review:  ", x$requires_professional_assessment, "\n", sep="")
  if (length(x$warnings)) for (w in x$warnings) cat("Warning: ", w, "\n", sep="")
  invisible(x)
}

###############################################################################
# END ERBioR core v0.3 regression-compatible layer
###############################################################################

###############################################################################
# 13. v0.4 workplace-level assessment, preventive planning and reporting
###############################################################################
# v0.4 is an orchestration layer. It does NOT change the mathematical engine,
# the v0.2 questionnaire bank, or the v0.3 legal agent classification.
#
# Source-preserving design rules:
# - Audit + General + Workers + >=1 sector-specific questionnaire are required
#   by default for a complete workplace assessment.
# - Each classified biological agent is assessed separately against every
#   selected questionnaire. Different agent groups are not silently collapsed.
# - Questionnaire-level discordance is preserved; no global class is invented.
# - Failed questionnaire items become preventive-planning candidates. Fields
#   such as responsible person, deadline and resources are intentionally left
#   for professional completion rather than inferred by the software.
###############################################################################

ERBIOR_WORKPLACE_VERSION <- "ERBioR workplace orchestration v0.4"
ERBIOR_WORKPLACE_RULES_VERSION <- "ERBioR_workplace_rules_v0_4.csv"

.erbio_match_named_option <- function(x, id, default = NULL) {
  if (is.null(x)) return(default)
  if (is.list(x)) x <- unlist(x, use.names = TRUE)
  if (!length(x)) return(default)
  if (is.null(names(x)) || !any(nzchar(names(x)))) {
    if (length(x) == 1L) return(as.character(x[[1]]))
    .erbio_stop("La opcion debe ser escalar o estar nombrada por questionnaire_id.")
  }
  if (id %in% names(x)) return(as.character(x[[id]]))
  default
}

erbio_sector_questionnaires <- function(path = NULL) {
  x <- erbio_questionnaire_catalog(path)
  x[x$instrument_type == "sector_specific_questionnaire", , drop = FALSE]
}

erbio_validate_workplace_questionnaires <- function(
  questionnaire_responses,
  path = NULL,
  require_core = TRUE,
  require_sector = TRUE
) {
  if (!is.list(questionnaire_responses) || length(questionnaire_responses) == 0L) {
    .erbio_stop("questionnaire_responses debe ser una lista nombrada no vacia.")
  }
  ids <- names(questionnaire_responses)
  if (is.null(ids) || any(!nzchar(ids))) {
    .erbio_stop("questionnaire_responses debe estar nombrada con questionnaire_id.")
  }
  if (anyDuplicated(ids)) .erbio_stop("Hay questionnaire_id duplicados en questionnaire_responses.")

  catalog <- erbio_questionnaire_catalog(path)
  unknown <- setdiff(ids, catalog$questionnaire_id)
  if (length(unknown)) {
    .erbio_stop("Questionnaire_id no reconocido: ", paste(unknown, collapse = ", "))
  }

  core <- c("audit", "general", "workers")
  missing_core <- setdiff(core, ids)
  if (isTRUE(require_core) && length(missing_core)) {
    .erbio_stop("Faltan cuestionarios principales requeridos: ", paste(missing_core, collapse = ", "))
  }

  selected <- catalog[match(ids, catalog$questionnaire_id), , drop = FALSE]
  sector_ids <- selected$questionnaire_id[selected$instrument_type == "sector_specific_questionnaire"]
  if (isTRUE(require_sector) && !length(sector_ids)) {
    .erbio_stop("Se requiere al menos un cuestionario sectorial para la evaluacion integral ERBio.")
  }

  list(
    valid = TRUE,
    questionnaire_ids = ids,
    core_present = intersect(core, ids),
    missing_core = missing_core,
    sector_questionnaire_ids = sector_ids,
    selected_catalog = selected
  )
}

.erbio_align_agent_exposure <- function(agents, exposure) {
  agents <- as.character(agents)
  if (!length(agents)) .erbio_stop("agents no puede estar vacio.")
  if (length(exposure) == 1L && (is.null(names(exposure)) || !nzchar(names(exposure)[1]))) {
    out <- rep(as.character(exposure[[1]]), length(agents))
    names(out) <- agents
    return(out)
  }
  nm <- names(exposure)
  if (is.null(nm) || any(!nzchar(nm))) {
    .erbio_stop("exposure debe ser escalar o un vector nombrado por cada agente de entrada.")
  }
  if (anyDuplicated(nm)) .erbio_stop("Hay nombres duplicados en exposure.")
  if (!setequal(nm, agents)) {
    .erbio_stop("El exposure nombrado debe cubrir exactamente todos los agentes de entrada.")
  }
  out <- as.character(exposure[agents])
  names(out) <- agents
  out
}

.erbio_agent_summary_row <- function(ref, input_agent, exposure) {
  data.frame(
    input_agent = as.character(input_agent),
    agent_id = if (!is.null(ref$agent_id)) ref$agent_id else NA_character_,
    agent_name = if (!is.null(ref$agent_name)) ref$agent_name else as.character(input_agent),
    agent_type = if (!is.null(ref$agent_type)) ref$agent_type else NA_character_,
    classification_status = ref$classification_status,
    reference_level = if (length(ref$reference_level)) as.integer(ref$reference_level) else NA_integer_,
    regulatory_minimum_group = if (!is.null(ref$regulatory_minimum_group) && length(ref$regulatory_minimum_group)) as.integer(ref$regulatory_minimum_group) else NA_integer_,
    exposure = as.character(exposure),
    basebio_available = if (!is.null(ref$basebio_available)) isTRUE(ref$basebio_available) else FALSE,
    requires_professional_assessment = isTRUE(ref$requires_professional_assessment),
    source_rule = if (!is.null(ref$source_rule)) ref$source_rule else NA_character_,
    stringsAsFactors = FALSE
  )
}

.erbio_risk_results_row <- function(input_agent, assessment) {
  data.frame(
    input_agent = as.character(input_agent),
    agent_id = if (!is.null(assessment$agent_id)) assessment$agent_id else NA_character_,
    agent_name = if (!is.null(assessment$agent_name)) assessment$agent_name else as.character(input_agent),
    reference_level = as.integer(assessment$reference_level),
    exposure = assessment$exposure,
    questionnaire_id = assessment$questionnaire_id,
    questionnaire_name = assessment$questionnaire_name,
    questionnaire_version = assessment$questionnaire_version,
    validation_status = assessment$validation_status,
    compliance_percent = as.numeric(assessment$compliance_percent),
    compliance_class = assessment$compliance_class,
    probability_value = as.integer(assessment$probability_value),
    probability_label = assessment$probability_label,
    risk_score = as.integer(assessment$risk_score),
    risk_class = assessment$risk_class,
    priority = assessment$priority,
    assessment_status = assessment$assessment_status,
    stringsAsFactors = FALSE
  )
}

.erbio_risk_rank <- function(x) {
  map <- c("Trivial"=1L, "Tolerable"=2L, "Moderado"=3L, "Importante"=4L, "Intolerable"=5L)
  unname(map[as.character(x)])
}

erbio_build_preventive_plan <- function(
  questionnaire_scores,
  risk_results,
  priority_policy = c("context_only", "max_observed_extension")
) {
  priority_policy <- match.arg(priority_policy)
  if (!is.list(questionnaire_scores)) .erbio_stop("questionnaire_scores debe ser una lista.")
  if (!is.data.frame(risk_results)) .erbio_stop("risk_results debe ser un data.frame.")

  rows <- list()
  k <- 0L
  for (qid in names(questionnaire_scores)) {
    qs <- questionnaire_scores[[qid]]
    fc <- qs$failed_controls
    if (is.null(fc) || !nrow(fc)) next
    ctx <- risk_results[risk_results$questionnaire_id == qid, , drop = FALSE]
    agents_ctx <- if (nrow(ctx)) paste(unique(ctx$agent_name), collapse = "; ") else ""
    classes_ctx <- if (nrow(ctx)) paste(unique(ctx$risk_class), collapse = "; ") else ""
    priorities_ctx <- if (nrow(ctx)) unique(ctx$priority) else character(0)

    if (!length(priorities_ctx)) {
      plan_priority <- NA_character_
      priority_status <- "no_resolved_agent_risk_context"
    } else if (length(priorities_ctx) == 1L) {
      plan_priority <- priorities_ctx[[1]]
      priority_status <- "single_unambiguous_risk_priority"
    } else if (priority_policy == "max_observed_extension") {
      max_score <- max(ctx$risk_score, na.rm = TRUE)
      idx <- which(ctx$risk_score == max_score)[1]
      plan_priority <- ctx$priority[[idx]]
      priority_status <- "explicit_max_observed_risk_extension"
    } else {
      plan_priority <- NA_character_
      priority_status <- "requires_professional_prioritization_due_to_multiple_risk_contexts"
    }

    for (i in seq_len(nrow(fc))) {
      k <- k + 1L
      rows[[k]] <- data.frame(
        plan_id = sprintf("PLAN-%04d", k),
        questionnaire_id = qid,
        questionnaire_name = qs$questionnaire_name,
        questionnaire_version = qs$version,
        instrument_type = qs$instrument_type,
        sector = qs$sector,
        item_id = fc$item_id[[i]],
        final_item = as.integer(fc$final_item[[i]]),
        section = fc$section[[i]],
        deficiency = fc$item_text[[i]],
        observed = as.character(fc$observed[[i]]),
        preventive_measure_candidate = if ("expert_preventive_action" %in% names(fc) &&
          length(fc$expert_preventive_action) >= i &&
          !is.na(fc$expert_preventive_action[[i]]) &&
          nzchar(trimws(as.character(fc$expert_preventive_action[[i]])))) {
          fc$expert_preventive_action[[i]]
        } else {
          fc$planning_candidate[[i]]
        },
        validation_status = fc$validation_status[[i]],
        agent_context = agents_ctx,
        risk_class_context = classes_ctx,
        planning_priority = plan_priority,
        priority_status = priority_status,
        responsible = NA_character_,
        target_date = NA_character_,
        resources = NA_character_,
        implementation_status = "pending_professional_planning",
        source_rule = "FAILED-CONTROL-PLANNING-CANDIDATE",
        stringsAsFactors = FALSE
      )
    }
  }

  if (!length(rows)) {
    return(data.frame(
      plan_id=character(0), questionnaire_id=character(0), questionnaire_name=character(0),
      questionnaire_version=character(0), instrument_type=character(0), sector=character(0),
      item_id=character(0), final_item=integer(0), section=character(0), deficiency=character(0),
      observed=character(0), preventive_measure_candidate=character(0), validation_status=character(0),
      agent_context=character(0), risk_class_context=character(0), planning_priority=character(0),
      priority_status=character(0), responsible=character(0), target_date=character(0), resources=character(0),
      implementation_status=character(0), source_rule=character(0), stringsAsFactors = FALSE
    ))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

erbio_workplace_rules <- function(rule_id = NULL, path = NULL) {
  f <- .erbio_find_data_file(ERBIOR_WORKPLACE_RULES_VERSION, path)
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
  if (is.null(rule_id)) return(x)
  hit <- x[x$rule_id %in% as.character(rule_id), , drop = FALSE]
  if (!nrow(hit)) .erbio_stop("Regla de workplace no encontrada: ", paste(rule_id, collapse = ", "))
  hit
}

erbio_assess_workplace <- function(
  activity,
  agents,
  questionnaire_responses,
  exposure,
  versions = NULL,
  scoring = NULL,
  path = NULL,
  require_core = TRUE,
  require_sector = TRUE,
  questionnaire_aggregation_rule = c("unresolved", "max_risk_extension"),
  workplace_aggregation_rule = c("unresolved", "max_risk_extension"),
  preventive_priority_policy = c("context_only", "max_observed_extension"),
  compliance_boundary_policy = c("prose_2015", "table45_intervals", "strict"),
  full_compliance_rule = c("flag_only", "thesis_override")
) {
  questionnaire_aggregation_rule <- match.arg(questionnaire_aggregation_rule)
  workplace_aggregation_rule <- match.arg(workplace_aggregation_rule)
  preventive_priority_policy <- match.arg(preventive_priority_policy)
  compliance_boundary_policy <- match.arg(compliance_boundary_policy)
  full_compliance_rule <- match.arg(full_compliance_rule)

  if (length(activity) != 1L || is.na(activity) || !nzchar(trimws(as.character(activity)))) {
    .erbio_stop("activity debe ser un texto no vacio.")
  }
  agents <- as.character(agents)
  if (!length(agents) || any(is.na(agents)) || any(!nzchar(trimws(agents)))) {
    .erbio_stop("agents debe contener al menos un agente no vacio.")
  }
  if (anyDuplicated(agents)) .erbio_stop("agents contiene entradas duplicadas; use una sola entrada por agente.")

  qcheck <- erbio_validate_workplace_questionnaires(
    questionnaire_responses, path = path,
    require_core = require_core, require_sector = require_sector
  )
  exposures <- .erbio_align_agent_exposure(agents, exposure)

  # Score each questionnaire once. This layer is independent of agent identity.
  qscores <- list()
  for (qid in qcheck$questionnaire_ids) {
    qscores[[qid]] <- erbio_score_questionnaire(
      questionnaire_id = qid,
      responses = questionnaire_responses[[qid]],
      version = .erbio_match_named_option(versions, qid, default = NULL),
      scoring = .erbio_match_named_option(scoring, qid, default = "default"),
      path = path
    )
  }

  refs <- lapply(agents, erbio_reference_level_from_agent, path = path)
  names(refs) <- agents
  agent_rows <- Map(.erbio_agent_summary_row, refs, agents, unname(exposures))
  agent_summary <- do.call(rbind, agent_rows)
  rownames(agent_summary) <- NULL

  risk_rows <- list()
  agent_results <- list()
  rrk <- 0L
  warnings <- character(0)

  for (j in seq_along(agents)) {
    ag <- agents[[j]]
    ref <- refs[[j]]
    ex <- exposures[[ag]]

    if (is.na(ref$reference_level)) {
      agent_results[[ag]] <- list(
        input_agent = ag,
        reference = ref,
        exposure = ex,
        questionnaire_assessments = list(),
        questionnaire_set = NULL,
        status = "unresolved_agent_reference_level"
      )
      warnings <- c(warnings, paste0("Agente sin nivel de referencia resuelto: ", ag, ". Requiere evaluacion profesional."), ref$warnings)
      next
    }

    qalist <- list()
    for (qid in qcheck$questionnaire_ids) {
      a <- erbio_assess_questionnaire(
        questionnaire_id = qid,
        responses = questionnaire_responses[[qid]],
        reference_level = ref$reference_level,
        exposure = ex,
        version = .erbio_match_named_option(versions, qid, default = NULL),
        scoring = .erbio_match_named_option(scoring, qid, default = "default"),
        path = path,
        compliance_boundary_policy = compliance_boundary_policy,
        full_compliance_rule = full_compliance_rule
      )
      a$agent_reference <- ref
      a$agent_id <- ref$agent_id
      a$agent_name <- ref$agent_name
      a$agent_type <- ref$agent_type
      a$basebio_available <- ref$basebio_available
      a$warnings <- unique(c(a$warnings, ref$warnings))
      qalist[[qid]] <- a
      rrk <- rrk + 1L
      risk_rows[[rrk]] <- .erbio_risk_results_row(ag, a)
      warnings <- c(warnings, a$warnings)
    }

    qset <- erbio_assess_set(unname(qalist), aggregation_rule = questionnaire_aggregation_rule)
    if (is.na(qset$global_risk_class)) {
      warnings <- c(warnings, paste0("Resultados de cuestionarios discordantes para ", ref$agent_name, "; no se calcula una clase global del agente con la politica por defecto."))
    }
    agent_results[[ag]] <- list(
      input_agent = ag,
      reference = ref,
      exposure = ex,
      questionnaire_assessments = qalist,
      questionnaire_set = qset,
      status = if (is.na(qset$global_risk_class)) "questionnaire_discordance_preserved" else "questionnaire_classes_unanimous"
    )
  }

  risk_results <- if (length(risk_rows)) {
    z <- do.call(rbind, risk_rows); rownames(z) <- NULL; z
  } else {
    data.frame(
      input_agent=character(0), agent_id=character(0), agent_name=character(0), reference_level=integer(0),
      exposure=character(0), questionnaire_id=character(0), questionnaire_name=character(0),
      questionnaire_version=character(0), validation_status=character(0), compliance_percent=numeric(0),
      compliance_class=character(0), probability_value=integer(0), probability_label=character(0),
      risk_score=integer(0), risk_class=character(0), priority=character(0), assessment_status=character(0),
      stringsAsFactors = FALSE
    )
  }

  qsummary <- do.call(rbind, lapply(qscores, function(qs) data.frame(
    questionnaire_id = qs$questionnaire_id,
    questionnaire_name = qs$questionnaire_name,
    version = qs$version,
    instrument_type = qs$instrument_type,
    sector = qs$sector,
    validation_status = qs$validation_status,
    n_items = as.integer(qs$n_items),
    compliance_percent = as.numeric(qs$compliance_percent),
    n_failed_controls = as.integer(nrow(qs$failed_controls)),
    stringsAsFactors = FALSE
  )))
  rownames(qsummary) <- NULL

  plan <- erbio_build_preventive_plan(qscores, risk_results, priority_policy = preventive_priority_policy)

  unresolved_agents <- agent_summary$input_agent[is.na(agent_summary$reference_level)]
  resolved_agents <- agent_summary$input_agent[!is.na(agent_summary$reference_level)]
  highest_score <- if (nrow(risk_results)) max(risk_results$risk_score, na.rm = TRUE) else NA_integer_
  highest_class <- if (nrow(risk_results)) {
    ranks <- .erbio_risk_rank(risk_results$risk_class)
    risk_results$risk_class[[which.max(ranks)]]
  } else NA_character_

  workplace_global_class <- NA_character_
  workplace_global_score <- NA_integer_
  workplace_aggregation_status <- "source_rule_unresolved_at_workplace_level"

  per_agent_global <- vapply(agent_results, function(z) {
    if (is.null(z$questionnaire_set)) return(NA_character_)
    z$questionnaire_set$global_risk_class
  }, character(1))
  per_agent_score <- vapply(agent_results, function(z) {
    if (is.null(z$questionnaire_set)) return(NA_integer_)
    z$questionnaire_set$global_risk_score
  }, integer(1))

  # A single resolved agent with unanimous questionnaire classes needs no extra
  # cross-agent aggregation; its questionnaire-set result can be surfaced.
  if (length(agents) == 1L && !is.na(per_agent_global[[1]])) {
    workplace_global_class <- per_agent_global[[1]]
    workplace_global_score <- per_agent_score[[1]]
    workplace_aggregation_status <- "single_agent_unanimous_questionnaire_result"
  } else if (workplace_aggregation_rule == "max_risk_extension" && nrow(risk_results)) {
    idx <- which.max(risk_results$risk_score)
    workplace_global_class <- risk_results$risk_class[[idx]]
    workplace_global_score <- risk_results$risk_score[[idx]]
    workplace_aggregation_status <- "explicit_max_observed_risk_extension_not_source_rule"
    warnings <- c(warnings, "Se ha aplicado una extension explicita de maximo riesgo observado a nivel workplace; no debe presentarse como regla original de ERBio.")
  } else if (length(agents) > 1L) {
    warnings <- c(warnings, "ERBioR v0.4 no colapsa automaticamente varios agentes en una unica clase global de workplace.")
  }

  if (length(unresolved_agents)) {
    status <- "incomplete_unresolved_agent_reference_level"
  } else if (any(is.na(per_agent_global))) {
    status <- "complete_inputs_questionnaire_discordance_preserved"
  } else {
    status <- "complete_inputs"
  }

  provenance <- data.frame(
    component = c("software", "method", "question_bank", "agent_registry", "technical_agent_layer", "workplace_rules"),
    version = c(
      ERBIOR_WORKPLACE_VERSION,
      ERBIOR_SOURCE_VERSION,
      ERBIOR_QUESTION_BANK_VERSION,
      "ERBioR_agent_registry_v0_8.csv",
      "ERBioR_basebio_index_v0_7.csv",
      ERBIOR_WORKPLACE_RULES_VERSION
    ),
    role = c(
      "workplace orchestration/reporting",
      "ERBio deterministic method",
      "source-versioned questionnaire items",
      "legal biological-agent classification layer",
      "technical enrichment; non-overriding",
      "v0.4 orchestration rules"
    ),
    stringsAsFactors = FALSE
  )

  out <- list(
    software_version = ERBIOR_WORKPLACE_VERSION,
    activity = as.character(activity),
    assessment_status = status,
    questionnaire_validation = qcheck,
    questionnaire_scores = qscores,
    questionnaire_summary = qsummary,
    agent_references = refs,
    agent_summary = agent_summary,
    agent_results = agent_results,
    risk_results = risk_results,
    preventive_plan = plan,
    resolved_agents = resolved_agents,
    unresolved_agents = unresolved_agents,
    highest_observed_risk_score = highest_score,
    highest_observed_risk_class = highest_class,
    workplace_global_risk_score = workplace_global_score,
    workplace_global_risk_class = workplace_global_class,
    workplace_aggregation_status = workplace_aggregation_status,
    questionnaire_aggregation_rule = questionnaire_aggregation_rule,
    workplace_aggregation_rule = workplace_aggregation_rule,
    preventive_priority_policy = preventive_priority_policy,
    warnings = unique(warnings[nzchar(warnings)]),
    provenance = provenance
  )
  class(out) <- c("erbio_workplace_assessment", "list")
  out
}

print.erbio_workplace_assessment <- function(x, ...) {
  cat("ERBioR v0.4 workplace assessment\n")
  cat("--------------------------------\n")
  cat("Activity:                     ", x$activity, "\n", sep="")
  cat("Status:                       ", x$assessment_status, "\n", sep="")
  cat("Agents:                       ", nrow(x$agent_summary), "\n", sep="")
  cat("Resolved agents:              ", length(x$resolved_agents), "\n", sep="")
  cat("Questionnaires:               ", nrow(x$questionnaire_summary), "\n", sep="")
  cat("Preventive-planning items:    ", nrow(x$preventive_plan), "\n", sep="")
  cat("Highest observed risk:        ", ifelse(is.na(x$highest_observed_risk_class), "NA", x$highest_observed_risk_class), "\n", sep="")
  cat("Workplace global risk class:  ", ifelse(is.na(x$workplace_global_risk_class), "NA (not aggregated)", x$workplace_global_risk_class), "\n", sep="")
  cat("Aggregation status:           ", x$workplace_aggregation_status, "\n", sep="")
  if (length(x$warnings)) {
    cat("Warnings:\n")
    for (w in x$warnings) cat(" - ", w, "\n", sep="")
  }
  invisible(x)
}

.erbio_md_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("\\|", "\\\\|", x)
  x <- gsub("[\\r\\n]+", " ", x)
  x
}

.erbio_md_table <- function(df, cols = names(df)) {
  if (!nrow(df)) return("_Sin registros._")
  z <- df[, cols, drop = FALSE]
  z[] <- lapply(z, .erbio_md_escape)
  header <- paste0("| ", paste(names(z), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(z)), collapse = " | "), " |")
  body <- apply(z, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

ERBIOR_VERSION <- "0.9.0"
erbio_version <- function() ERBIOR_VERSION

erbio_report_patch_version <- function() "v0.4-frozen-report-layer"

erbio_render_workplace_report <- function(x, max_plan_rows = 100L) {
  if (!inherits(x, "erbio_workplace_assessment")) .erbio_stop("x debe ser un objeto erbio_workplace_assessment.")
  max_plan_rows <- as.integer(max_plan_rows)
  if (is.na(max_plan_rows) || max_plan_rows < 0L) .erbio_stop("max_plan_rows debe ser >=0.")

  # Preserve both the user-entered agent label and the canonical legal name in the report.
  # This improves provenance and ensures aliases/normalised legal names remain auditable.
  agents <- x$agent_summary[, c("input_agent","agent_name","agent_type","classification_status","reference_level","exposure","basebio_available"), drop=FALSE]
  qsum <- x$questionnaire_summary[, c("questionnaire_id","questionnaire_name","validation_status","n_items","compliance_percent","n_failed_controls"), drop=FALSE]
  risks <- x$risk_results[, c("agent_name","questionnaire_id","compliance_class","probability_value","risk_score","risk_class","priority"), drop=FALSE]
  plan <- x$preventive_plan
  plan_note <- character(0)
  if (nrow(plan) > max_plan_rows) {
    plan_note <- paste0("\n_Se muestran ", max_plan_rows, " de ", nrow(plan), " medidas candidatas; el CSV exportado conserva todas._")
    plan <- plan[seq_len(max_plan_rows), , drop=FALSE]
  }
  if (nrow(plan)) {
    plan <- plan[, c("plan_id","questionnaire_id","item_id","preventive_measure_candidate","risk_class_context","planning_priority","priority_status"), drop=FALSE]
  }

  warnings <- if (length(x$warnings)) paste0("- ", x$warnings, collapse="\n") else "_Sin advertencias adicionales._"
  provenance <- .erbio_md_table(x$provenance, c("component","version","role"))

  # Build an independent provenance label from the named agent-results list.
  # This is intentionally redundant with agent_summary: it guarantees that the
  # exact user-entered agent labels remain present in the rendered report even
  # if a canonical-name field is NA or normalised differently.
  report_input_agents <- names(x$agent_results)
  if (is.null(report_input_agents) || !length(report_input_agents) || any(!nzchar(report_input_agents))) {
    report_input_agents <- as.character(x$agent_summary$input_agent)
  }
  report_canonical_agents <- as.character(x$agent_summary$agent_name)
  report_canonical_agents <- report_canonical_agents[!is.na(report_canonical_agents) & nzchar(report_canonical_agents)]

  lines <- c(
    "# ERBioR \u2014 Informe reproducible de evaluaci\u00f3n de riesgo biol\u00f3gico",
    "",
    paste0("**Actividad/puesto evaluado:** ", x$activity),
    paste0("**Versi\u00f3n de software:** ", x$software_version),
    paste0("**Estado de evaluaci\u00f3n:** ", x$assessment_status),
    paste0("**Mayor clase de riesgo observada (descriptiva, no agregada):** ", ifelse(is.na(x$highest_observed_risk_class), "NA", x$highest_observed_risk_class)),
    paste0("**Clase global workplace:** ", ifelse(is.na(x$workplace_global_risk_class), "No calculada", x$workplace_global_risk_class)),
    paste0("**Estado de agregaci\u00f3n:** ", x$workplace_aggregation_status),
    "",
    "## 1. Agentes biol\u00f3gicos considerados",
    # Keep a literal, unescaped provenance line so the exact user-entered agent
    # is always present as plain text in the reproducible report. Agent names
    # from the legal registry cannot contain Markdown table delimiters here.
    paste0("**Agentes introducidos (literal):** ", paste(as.character(x$agent_summary$input_agent), collapse = "; ")),
    paste0("**Agentes introducidos:** ", paste(.erbio_md_escape(report_input_agents), collapse = "; ")),
    paste0("**Nombres can\u00f3nicos resueltos:** ", if (length(report_canonical_agents)) paste(.erbio_md_escape(report_canonical_agents), collapse = "; ") else "No resueltos"),
    "",
    .erbio_md_table(agents),
    "",
    "## 2. Cuestionarios ERBio",
    .erbio_md_table(qsum),
    "",
    "## 3. Resultados de riesgo por agente y cuestionario",
    .erbio_md_table(risks),
    "",
    "## 4. Planificaci\u00f3n preventiva candidata",
    if (nrow(plan)) .erbio_md_table(plan) else "_No se han detectado \u00edtems incumplidos en los cuestionarios seleccionados._",
    plan_note,
    "",
    "Los campos responsable, plazo y recursos se conservan para cumplimentaci\u00f3n profesional; ERBioR no los infiere autom\u00e1ticamente.",
    "",
    "## 5. Advertencias y l\u00edmites de interpretaci\u00f3n",
    warnings,
    "",
    "## 6. Trazabilidad",
    "**Registro legal de agentes:** ERBioR_agent_registry_v0_8.csv (audited data layer; supersedes bundled ERBioR_agent_registry_v0_3.csv)",
    "**Capa t\u00e9cnica de agentes:** ERBioR_basebio_index_v0_7.csv",
    "",
    provenance,
    "",
    "### Nota metodol\u00f3gica",
    "ERBioR conserva los resultados por agente y por cuestionario. Cuando los cuestionarios discrepan o existen varios agentes con diferente contexto de riesgo, la pol\u00edtica por defecto no inventa una \u00fanica clase global. Las extensiones conservadoras solo se aplican cuando se solicitan expl\u00edcitamente y quedan etiquetadas como tales."
  )
  paste(lines, collapse = "\n")
}

erbio_write_workplace_report <- function(x, file = "ERBioR_workplace_report.md", max_plan_rows = 100L) {
  txt <- erbio_render_workplace_report(x, max_plan_rows = max_plan_rows)
  writeLines(enc2utf8(txt), con = file, useBytes = TRUE)
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

erbio_export_workplace_bundle <- function(x, dir, overwrite = FALSE, max_plan_rows = 100L) {
  if (!inherits(x, "erbio_workplace_assessment")) .erbio_stop("x debe ser un objeto erbio_workplace_assessment.")
  dir <- as.character(dir)[1]
  if (!nzchar(dir)) .erbio_stop("dir no puede estar vacio.")
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(dir)) .erbio_stop("No se pudo crear el directorio de exportacion: ", dir)

  files <- c(
    report = file.path(dir, "ERBioR_workplace_report.md"),
    agents = file.path(dir, "ERBioR_agent_summary.csv"),
    questionnaires = file.path(dir, "ERBioR_questionnaire_summary.csv"),
    risks = file.path(dir, "ERBioR_risk_results.csv"),
    plan = file.path(dir, "ERBioR_preventive_plan.csv"),
    provenance = file.path(dir, "ERBioR_provenance.csv"),
    warnings = file.path(dir, "ERBioR_warnings.txt")
  )
  existing <- files[file.exists(files)]
  if (length(existing) && !isTRUE(overwrite)) {
    .erbio_stop("Ya existen archivos de exportacion. Use overwrite=TRUE o un directorio vacio.")
  }

  erbio_write_workplace_report(x, files[["report"]], max_plan_rows = max_plan_rows)
  write.csv(x$agent_summary, files[["agents"]], row.names = FALSE, na = "")
  write.csv(x$questionnaire_summary, files[["questionnaires"]], row.names = FALSE, na = "")
  write.csv(x$risk_results, files[["risks"]], row.names = FALSE, na = "")
  write.csv(x$preventive_plan, files[["plan"]], row.names = FALSE, na = "")
  write.csv(x$provenance, files[["provenance"]], row.names = FALSE, na = "")
  writeLines(if (length(x$warnings)) enc2utf8(x$warnings) else "Sin advertencias adicionales.", files[["warnings"]], useBytes = TRUE)
  out_paths <- normalizePath(unname(files), winslash = "/", mustWork = TRUE)
  names(out_paths) <- names(files)
  out_paths
}

###############################################################################
# END ERBioR core v0.4
###############################################################################
