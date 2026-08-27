
# =============================================================================
# ERBioR Shiny v0.3 — Excel I/O + multi-worker aggregation
# =============================================================================

erbio_require_app_packages <- function() {
  pkgs <- c("ERBioR", "shiny", "DT", "readxl", "openxlsx")
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Faltan paquetes: ", paste(missing, collapse = ", "), call. = FALSE)
  }
  invisible(TRUE)
}

.erbio_norm_header <- function(x) {
  y <- trimws(as.character(x))

  # Normalize common Spanish/European header characters explicitly before
  # ASCII conversion. macOS iconv may transliterate accents with punctuation
  # (e.g. "Sección" -> "Secci'on"), which would otherwise produce
  # platform-dependent headers such as "secci_on".
  replacements <- c(
    "á" = "a", "é" = "e", "í" = "i", "ó" = "o", "ú" = "u",
    "ü" = "u", "ñ" = "n",
    "Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O", "Ú" = "U",
    "Ü" = "U", "Ñ" = "N",
    "º" = "o", "ª" = "a"
  )
  for (from in names(replacements)) {
    y <- gsub(from, replacements[[from]], y, fixed = TRUE)
  }

  y <- iconv(y, from = "UTF-8", to = "ASCII", sub = "")
  y <- tolower(y)
  y <- gsub("[^a-z0-9]+", "_", y)
  y <- gsub("^_+|_+$", "", y)
  aliases <- c(
    "n" = "numero",
    "n_o" = "numero",
    "n0" = "numero",
    "no" = "numero",
    "num" = "numero",
    "numero" = "numero",
    "number" = "numero",
    "seccion" = "seccion",
    "section" = "seccion",
    "pregunta" = "pregunta",
    "question" = "pregunta",
    "respuesta" = "respuesta",
    "response" = "respuesta",
    "field" = "campo",
    "value" = "valor",
    "questionnaire_id" = "questionnaire_id",
    "item_id" = "item_id",
    "worker_id" = "worker_id",
    "id_trabajador" = "worker_id",
    "trabajador_id" = "worker_id"
  )
  hit <- y %in% names(aliases)
  y[hit] <- unname(aliases[y[hit]])
  y
}

.erbio_normalize_sheet_names <- function(df) {
  names(df) <- .erbio_norm_header(names(df))
  df
}

.erbio_read_sheet <- function(path, sheet) {
  x <- readxl::read_excel(path, sheet = sheet, .name_repair = "minimal")
  .erbio_normalize_sheet_names(x)
}

.erbio_clean_response <- function(x) {
  y <- ifelse(is.na(x), "", trimws(as.character(x)))
  y[y %in% c("SI", "Sí", "sí", "si", "SÍ", "YES", "Yes", "yes")] <- "Si"
  y[y %in% c("NO", "no", "No")] <- "No"
  y[y %in% c(
    "N/A", "NA", "NP", "N.P.", "No Procede", "no procede",
    "NO PROCEDE", "No procede", "NOT APPLICABLE", "Not applicable", "not applicable"
  )] <- "No procede"
  y
}

.erbio_clean_character <- function(x) {
  ifelse(is.na(x), "", trimws(as.character(x)))
}


.erbio_is_conditional_item <- function(text) {
  z <- trimws(as.character(text))

  # This is an interpretive/help heuristic only. It never modifies scoring.
  # It tries to identify questions whose applicability depends on a prior
  # circumstance, historical condition, type of installation or task.
  patterns <- c(
    "^[¿?\\s]*(si\\b|cuando\\b|en caso\\b|en el caso\\b|para el caso\\b)",
    "en el momento de la entrada en vigor",
    "^[¿?\\s]*en los servicios de aislamiento\\b",
    "^[¿?\\s]*en laboratorios y procedimientos industriales\\b",
    "\\bcuando se prevea\\b",
    "\\bcuando se puedan producir\\b",
    "\\bsi no es posible\\b"
  )

  vapply(
    z,
    function(one) {
      any(vapply(
        patterns,
        function(p) grepl(p, one, ignore.case = TRUE, perl = TRUE),
        logical(1)
      ))
    },
    logical(1)
  )
}

.erbio_applicability_help <- function(text) {
  conditional <- .erbio_is_conditional_item(text)
  ifelse(
    conditional,
    paste(
      "Ítem condicionado o dependiente de una situación previa:",
      "confirme que el supuesto descrito existe y es aplicable a la actividad.",
      "Si no existe o no resulta aplicable, valore «No procede».",
      "Esta ayuda es interpretativa y no forma parte del texto fuente."
    ),
    ""
  )
}

.erbio_conditional_item_flag <- function(text) {
  ifelse(
    .erbio_is_conditional_item(text),
    "REVISAR_APLICABILIDAD",
    "OK"
  )
}

.erbio_editorial_flag <- function(...) {
  txt <- tolower(paste(..., collapse = " "))
  patterns <- tolower(c(
    "asegurar el cumplimiento efectivo del control descrito en el ítem",
    "analizar el punto concreto del proceso sectorial",
    "definir e implantar la medida técnica u organizativa necesaria",
    "la especificación final debe ajustarse a las condiciones reales del puesto"
  ))
  hit <- patterns[vapply(
    patterns,
    function(p) grepl(p, txt, fixed = TRUE),
    logical(1)
  )]
  list(
    flag = if (length(hit)) "REVISAR_REDACCION_GENERICA" else "OK",
    reason = if (length(hit)) paste(hit, collapse = " | ") else ""
  )
}

.erbio_highest_observed_summary <- function(risk_results) {
  if (is.null(risk_results) || !nrow(risk_results)) return(data.frame())

  keys <- unique(risk_results$input_agent)
  out <- lapply(keys, function(key) {
    z <- risk_results[risk_results$input_agent == key, , drop = FALSE]
    max_score <- max(z$risk_score, na.rm = TRUE)
    top <- z[z$risk_score == max_score, , drop = FALSE]

    data.frame(
      input_agent = key,
      agent_name = top$agent_name[[1]],
      reference_level = top$reference_level[[1]],
      exposure = top$exposure[[1]],
      highest_observed_risk_score = max_score,
      highest_observed_risk_class = paste(unique(top$risk_class), collapse = " / "),
      highest_observed_priority = paste(unique(top$priority), collapse = " / "),
      questionnaires_at_highest_observed = paste(
        unique(top$questionnaire_name),
        collapse = "; "
      ),
      interpretation = paste(
        "Mayor riesgo observado entre los cuestionarios;",
        "descriptor para interpretación profesional, no agregación global."
      ),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, out)
}

.erbio_worker_class_distribution <- function(individual_summary) {
  if (is.null(individual_summary) || !nrow(individual_summary)) return(data.frame())

  lev <- c("Muy deficiente", "Deficiente", "Mejorable", "Aceptable")
  tab <- table(factor(individual_summary$compliance_class, levels = lev))
  n <- sum(tab)

  data.frame(
    compliance_class = names(tab),
    n_workers = as.integer(tab),
    pct_workers = if (n) 100 * as.integer(tab) / n else 0,
    stringsAsFactors = FALSE
  )
}

.erbio_worker_top_deficiencies <- function(item_summary, n = 10L) {
  if (is.null(item_summary) || !nrow(item_summary)) return(data.frame())

  z <- item_summary[
    !is.na(item_summary$n_no) & item_summary$n_no > 0,
    ,
    drop = FALSE
  ]
  if (!nrow(z)) return(z)

  z <- z[
    order(-z$pct_no_applicable, -z$n_no, z$numero),
    ,
    drop = FALSE
  ]
  head(z, n)
}

.erbio_round_display <- function(df, digits = 2L) {
  if (is.null(df) || !nrow(df)) return(df)
  out <- df
  num <- vapply(out, is.numeric, logical(1))
  out[num] <- lapply(out[num], function(v) round(v, digits))
  out
}

# -----------------------------------------------------------------------------
# Dynamic Excel template
# -----------------------------------------------------------------------------

erbio_make_excel_template <- function(sector_id, file, n_workers = 1L, language = ERBioR::erbio_get_language()) {
  erbio_require_app_packages()

  language <- match.arg(language, c("es", "en"))
  sector_id <- as.character(sector_id)[1]
  n_workers <- suppressWarnings(as.integer(n_workers))
  if (is.na(n_workers) || n_workers < 1L || n_workers > 1000L) {
    stop("n_workers debe ser un entero entre 1 y 1000.", call. = FALSE)
  }

  sectors <- ERBioR::erbio_sector_questionnaires()
  hit <- sectors[sectors$questionnaire_id == sector_id, , drop = FALSE]
  if (!nrow(hit)) {
    stop("Cuestionario sectorial no reconocido: ", sector_id, call. = FALSE)
  }
  if (identical(language, "en")) {
    tr <- ERBioR::erbio_load_translation_registry()
    en_name <- unique(tr$questionnaire_name_en[tr$questionnaire_id == sector_id])
    en_name <- en_name[!is.na(en_name) & nzchar(en_name)]
    if (length(en_name)) hit$questionnaire_name[[1]] <- en_name[[1]]
  }

  wb <- openxlsx::createWorkbook()

  hdr <- openxlsx::createStyle(
    fgFill = "#17365D",
    fontColour = "#FFFFFF",
    textDecoration = "bold",
    halign = "center",
    valign = "center"
  )
  subhdr <- openxlsx::createStyle(
    fgFill = "#D9EAF7",
    textDecoration = "bold",
    valign = "top"
  )
  input_style <- openxlsx::createStyle(
    fgFill = "#FFF8E1",
    wrapText = TRUE,
    valign = "top"
  )
  locked_style <- openxlsx::createStyle(
    fgFill = "#F4F6F7",
    wrapText = TRUE,
    valign = "top"
  )
  worker_id_style <- openxlsx::createStyle(
    fgFill = "#E2F0D9",
    textDecoration = "bold",
    halign = "center"
  )

  # Instructions
  openxlsx::addWorksheet(wb, "00_INSTRUCCIONES")
  inst <- data.frame(
    Campo = c(
      "Uso", "Flujo", "Respuestas", "Exposición", "Agentes",
      "Cuestionario de trabajadores", "worker_id", "Trazabilidad",
      "Agregación de trabajadores", "Planificación"
    ),
    Detalle = c(
      paste0("Plantilla ERBioR para: ", hit$questionnaire_name[[1]], "."),
      "Complete datos y agentes; responda Auditoría, General, todos los cuestionarios personales de Trabajadores y el cuestionario Sectorial; guarde y suba el .xlsx.",
      "Si / No / No procede.",
      "Ocasional / Irregular / Frecuente / Muy frecuente / Continua.",
      "Introduzca uno o más agentes. ERBioR comprobará su clasificación.",
      paste0(
        "Esta plantilla contiene ", n_workers,
        " cuestionario(s) personal(es) de 34 ítems. Cada trabajador debe responder su bloque completo."
      ),
      "Es un identificador numérico interno y anónimo. No introduzca nombre, DNI ni otros identificadores personales. Si necesita añadir trabajadores, copie un bloque completo de 34 filas y asigne un worker_id nuevo.",
      "No cambie questionnaire_id, item_id ni el texto fuente. La aplicación tolera mayúsculas/minúsculas en los encabezados, pero valida los identificadores y las preguntas.",
      "ERBioR conserva el resultado individual de cada trabajador y calcula además un indicador grupal documentado: total Sí / (Sí + No) de todas las respuestas aplicables. Esta agregación es una extensión computacional explícita de ERBioR, no una regla matemática definida literalmente por el método ERBio original.",
      "Cada ítem con al menos una respuesta No queda identificado para análisis preventivo grupal; se informa cuántos trabajadores responden No y su porcentaje. Los ítems condicionados incluyen una ayuda de cumplimentación separada del texto fuente para recordar el uso de «No procede» cuando corresponda."
    ),
    stringsAsFactors = FALSE
  )
  if (identical(language, "en")) names(inst) <- c("Field", "Detail")
  openxlsx::writeData(wb, "00_INSTRUCCIONES", inst)
  openxlsx::addStyle(
    wb, "00_INSTRUCCIONES", hdr,
    rows = 1, cols = 1:2, gridExpand = TRUE
  )
  openxlsx::setColWidths(wb, "00_INSTRUCCIONES", 1:2, c(28, 105))
  openxlsx::freezePane(wb, "00_INSTRUCCIONES", firstRow = TRUE)

  # Metadata
  openxlsx::addWorksheet(wb, "01_DATOS")
  dat <- data.frame(
    Campo = c(
      "evaluation_id", "empresa", "centro", "actividad_puesto",
      "sector_id", "sector_nombre", "numero_trabajadores", "language",
      "fecha_evaluacion", "evaluador", "observaciones"
    ),
    Valor = c(
      "", "", "", "",
      sector_id, hit$questionnaire_name[[1]], n_workers, language,
      "", "", ""
    ),
    stringsAsFactors = FALSE
  )
  if (identical(language, "en")) names(dat) <- c("Field", "Value")
  openxlsx::writeData(wb, "01_DATOS", dat)
  openxlsx::addStyle(wb, "01_DATOS", hdr, rows = 1, cols = 1:2, gridExpand = TRUE)
  openxlsx::addStyle(
    wb, "01_DATOS", subhdr,
    rows = 2:(nrow(dat) + 1), cols = 1, gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, "01_DATOS", input_style,
    rows = 2:(nrow(dat) + 1), cols = 2, gridExpand = TRUE
  )
  openxlsx::setColWidths(wb, "01_DATOS", 1:2, c(28, 72))

  # Agents
  openxlsx::addWorksheet(wb, "02_AGENTES")
  ag <- data.frame(
    agent_name = rep("", 20),
    exposure = rep("", 20),
    observaciones = rep("", 20),
    stringsAsFactors = FALSE
  )
  openxlsx::writeData(wb, "02_AGENTES", ag)
  openxlsx::addStyle(wb, "02_AGENTES", hdr, rows = 1, cols = 1:3, gridExpand = TRUE)
  openxlsx::addStyle(
    wb, "02_AGENTES", input_style,
    rows = 2:21, cols = 1:3, gridExpand = TRUE
  )
  openxlsx::dataValidation(
    wb, "02_AGENTES",
    cols = 2, rows = 2:21,
    type = "list",
    value = if (identical(language, "en")) '"Occasional,Irregular,Frequent,Very frequent,Continuous"' else '"Ocasional,Irregular,Frecuente,Muy frecuente,Continua"'
  )
  openxlsx::setColWidths(wb, "02_AGENTES", 1:3, c(55, 22, 55))
  openxlsx::freezePane(wb, "02_AGENTES", firstRow = TRUE)

  write_questionnaire <- function(sheet, qid) {
    q <- ERBioR::erbio_get_questionnaire_i18n(qid, language = language)
    out <- data.frame(
      questionnaire_id = q$questionnaire_id,
      item_id = q$item_id,
      numero = q$final_item,
      seccion = q$section,
      pregunta = q$item_text,
      ayuda_cumplimentacion = .erbio_applicability_help(q$item_text),
      respuesta = rep("", nrow(q)),
      stringsAsFactors = FALSE
    )
    if (identical(language, "en")) {
      names(out) <- c("questionnaire_id", "item_id", "number", "section", "question", "completion_help", "response")
    }
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet, out)
    openxlsx::addStyle(wb, sheet, hdr, rows = 1, cols = 1:8, gridExpand = TRUE)
    openxlsx::addStyle(
      wb, sheet, locked_style,
      rows = 2:(nrow(out) + 1), cols = 1:6, gridExpand = TRUE
    )
    openxlsx::addStyle(
      wb, sheet, input_style,
      rows = 2:(nrow(out) + 1), cols = 7, gridExpand = TRUE
    )
    openxlsx::dataValidation(
      wb, sheet,
      cols = 7, rows = 2:(nrow(out) + 1),
      type = "list",
      value = if (identical(language, "en")) '"Yes,No,Not applicable"' else '"Si,No,No procede"'
    )
    openxlsx::setColWidths(wb, sheet, 1:7, c(28, 31, 9, 35, 82, 52, 18))
    openxlsx::freezePane(wb, sheet, firstRow = TRUE)
  }

  write_questionnaire("03_AUDITORIA", "audit")
  write_questionnaire("04_GENERAL", "general")

  # Workers in long format, one 34-row block per worker.
  q_workers <- ERBioR::erbio_get_questionnaire_i18n("workers", language = language)
  workers <- do.call(
    rbind,
    lapply(seq_len(n_workers), function(wid) {
      data.frame(
        worker_id = wid,
        questionnaire_id = q_workers$questionnaire_id,
        item_id = q_workers$item_id,
        numero = q_workers$final_item,
        seccion = q_workers$section,
        pregunta = q_workers$item_text,
        ayuda_cumplimentacion = .erbio_applicability_help(q_workers$item_text),
        respuesta = rep("", nrow(q_workers)),
        stringsAsFactors = FALSE
      )
    })
  )

  if (identical(language, "en")) {
    names(workers) <- c("worker_id", "questionnaire_id", "item_id", "number", "section", "question", "completion_help", "response")
  }
  openxlsx::addWorksheet(wb, "05_TRABAJADORES")
  openxlsx::writeData(wb, "05_TRABAJADORES", workers)
  openxlsx::addStyle(
    wb, "05_TRABAJADORES", hdr,
    rows = 1, cols = 1:7, gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, "05_TRABAJADORES", locked_style,
    rows = 2:(nrow(workers) + 1), cols = 2:7, gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, "05_TRABAJADORES", worker_id_style,
    rows = 2:(nrow(workers) + 1), cols = 1, gridExpand = TRUE
  )
  openxlsx::addStyle(
    wb, "05_TRABAJADORES", input_style,
    rows = 2:(nrow(workers) + 1), cols = 8, gridExpand = TRUE
  )
  openxlsx::dataValidation(
    wb, "05_TRABAJADORES",
    cols = 8, rows = 2:(nrow(workers) + 1),
    type = "list",
    value = if (identical(language, "en")) '"Yes,No,Not applicable"' else '"Si,No,No procede"'
  )
  openxlsx::setColWidths(
    wb, "05_TRABAJADORES",
    1:8, c(14, 24, 31, 9, 28, 78, 48, 18)
  )
  openxlsx::freezePane(wb, "05_TRABAJADORES", firstRow = TRUE)

  write_questionnaire("06_SECTOR", sector_id)

  # Sector catalogue
  openxlsx::addWorksheet(wb, "07_CATALOGO_SECTORES")
  openxlsx::writeData(wb, "07_CATALOGO_SECTORES", sectors)
  openxlsx::addStyle(
    wb, "07_CATALOGO_SECTORES", hdr,
    rows = 1, cols = 1:ncol(sectors), gridExpand = TRUE
  )
  openxlsx::setColWidths(
    wb, "07_CATALOGO_SECTORES",
    1:ncol(sectors), widths = "auto"
  )

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
  normalizePath(file, winslash = "/", mustWork = TRUE)
}

# -----------------------------------------------------------------------------
# Generic single questionnaire reader
# -----------------------------------------------------------------------------

.erbio_read_single_questionnaire <- function(path, sheet, expected_id = NULL) {
  q <- .erbio_read_sheet(path, sheet)

  needed <- c("questionnaire_id", "item_id", "pregunta", "respuesta")
  missing <- setdiff(needed, names(q))
  if (length(missing)) {
    stop(
      sheet, " debe contener las columnas: ",
      paste(needed, collapse = ", "),
      ". Faltan: ", paste(missing, collapse = ", "),
      ". Los encabezados pueden escribirse con mayúsculas o minúsculas.",
      call. = FALSE
    )
  }

  q$questionnaire_id <- .erbio_clean_character(q$questionnaire_id)
  q$item_id <- .erbio_clean_character(q$item_id)
  q$pregunta <- .erbio_clean_character(q$pregunta)

  # Drop only fully empty trailing rows.
  keep <- nzchar(q$questionnaire_id) | nzchar(q$item_id) |
    nzchar(q$pregunta) | !is.na(q$respuesta)
  q <- q[keep, , drop = FALSE]

  ids <- unique(q$questionnaire_id[nzchar(q$questionnaire_id)])
  if (length(ids) != 1L) {
    stop(sheet, ": questionnaire_id debe ser único.", call. = FALSE)
  }
  qid <- ids[[1]]

  if (!is.null(expected_id) && !identical(qid, expected_id)) {
    stop(
      sheet, ": questionnaire_id inesperado: ", qid,
      ". Se esperaba: ", expected_id, ".",
      call. = FALSE
    )
  }

  source_q <- ERBioR::erbio_get_questionnaire(qid)
  source_q_en <- ERBioR::erbio_get_questionnaire_i18n(qid, language = "en")

  if (nrow(q) != nrow(source_q)) {
    stop(
      sheet, ": contiene ", nrow(q), " filas de cuestionario; ERBioR espera ",
      nrow(source_q), " para ", qid, ".",
      call. = FALSE
    )
  }

  # Reorder by stable item_id, allowing users to sort rows without breaking import.
  if (anyDuplicated(q$item_id)) {
    stop(sheet, ": existen item_id duplicados.", call. = FALSE)
  }
  if (!setequal(q$item_id, source_q$item_id)) {
    missing_ids <- setdiff(source_q$item_id, q$item_id)
    extra_ids <- setdiff(q$item_id, source_q$item_id)
    stop(
      sheet, ": los item_id no coinciden con ERBioR. ",
      if (length(missing_ids)) paste0("Faltan: ", paste(head(missing_ids, 10), collapse = ", "), ". ") else "",
      if (length(extra_ids)) paste0("Sobran/no reconocidos: ", paste(head(extra_ids, 10), collapse = ", "), ".") else "",
      call. = FALSE
    )
  }

  q <- q[match(source_q$item_id, q$item_id), , drop = FALSE]

  text_ok <- q$pregunta == as.character(source_q$item_text) | q$pregunta == as.character(source_q_en$item_text)
  if (!all(text_ok)) {
    bad <- which(!text_ok)
    stop(
      sheet, ": el texto de ", length(bad),
      " pregunta(s) ha sido modificado. Primer item afectado: ",
      source_q$item_id[bad[[1]]], ".",
      call. = FALSE
    )
  }

  resp <- .erbio_clean_response(q$respuesta)
  if (any(!nzchar(resp))) {
    bad <- which(!nzchar(resp))
    stop(
      sheet, ": hay ", length(bad),
      " respuesta(s) vacía(s). Primer item sin respuesta: ",
      source_q$item_id[bad[[1]]], ".",
      call. = FALSE
    )
  }

  allowed <- c("Si", "No", "No procede")
  bad <- setdiff(unique(resp), allowed)
  if (length(bad)) {
    stop(
      sheet, ": respuesta(s) no válida(s): ",
      paste(bad, collapse = ", "),
      ". Valores permitidos: Si, No, No procede.",
      call. = FALSE
    )
  }

  list(
    id = qid,
    responses = stats::setNames(resp, source_q$item_id),
    source = q
  )
}

# -----------------------------------------------------------------------------
# Multiple workers
# -----------------------------------------------------------------------------

.erbio_read_workers <- function(path) {
  sheet <- "05_TRABAJADORES"
  q <- .erbio_read_sheet(path, sheet)

  # Backward compatibility: old one-worker template without worker_id.
  if (!"worker_id" %in% names(q)) {
    q$worker_id <- 1L
  }

  needed <- c(
    "worker_id", "questionnaire_id", "item_id", "pregunta", "respuesta"
  )
  missing <- setdiff(needed, names(q))
  if (length(missing)) {
    stop(
      sheet, " debe contener: ", paste(needed, collapse = ", "),
      ". Faltan: ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  q$worker_id <- .erbio_clean_character(q$worker_id)
  q$questionnaire_id <- .erbio_clean_character(q$questionnaire_id)
  q$item_id <- .erbio_clean_character(q$item_id)
  q$pregunta <- .erbio_clean_character(q$pregunta)

  # Remove completely empty trailing/copy rows.
  keep <- nzchar(q$worker_id) | nzchar(q$questionnaire_id) |
    nzchar(q$item_id) | nzchar(q$pregunta) | !is.na(q$respuesta)
  q <- q[keep, , drop = FALSE]

  if (!nrow(q)) {
    stop("05_TRABAJADORES no contiene respuestas.", call. = FALSE)
  }
  if (any(!nzchar(q$worker_id))) {
    stop(
      "05_TRABAJADORES: todas las filas de respuesta deben tener worker_id.",
      call. = FALSE
    )
  }

  # Numeric anonymous IDs only.
  wid_num <- suppressWarnings(as.integer(q$worker_id))
  if (any(is.na(wid_num)) || any(wid_num < 1L)) {
    stop(
      "05_TRABAJADORES: worker_id debe ser un identificador numérico positivo ",
      "(1, 2, 3, ...). No introduzca nombres ni otros datos personales.",
      call. = FALSE
    )
  }
  q$worker_id <- wid_num

  if (any(q$questionnaire_id != "workers")) {
    stop(
      "05_TRABAJADORES: todas las filas deben tener questionnaire_id = workers.",
      call. = FALSE
    )
  }

  source_q <- ERBioR::erbio_get_questionnaire("workers")
  source_q_en <- ERBioR::erbio_get_questionnaire_i18n("workers", language = "en")
  worker_ids <- sort(unique(q$worker_id))

  individual <- vector("list", length(worker_ids))
  names(individual) <- as.character(worker_ids)

  long_clean <- list()

  for (i in seq_along(worker_ids)) {
    wid <- worker_ids[[i]]
    w <- q[q$worker_id == wid, , drop = FALSE]

    if (nrow(w) != nrow(source_q)) {
      stop(
        "Trabajador ", wid, ": tiene ", nrow(w),
        " filas; debe contener exactamente ", nrow(source_q),
        " respuestas (una por cada item del cuestionario).",
        call. = FALSE
      )
    }
    if (anyDuplicated(w$item_id)) {
      stop(
        "Trabajador ", wid, ": existen item_id duplicados.",
        call. = FALSE
      )
    }
    if (!setequal(w$item_id, source_q$item_id)) {
      stop(
        "Trabajador ", wid,
        ": los 34 item_id no coinciden con el cuestionario ERBio de trabajadores.",
        call. = FALSE
      )
    }

    w <- w[match(source_q$item_id, w$item_id), , drop = FALSE]

    text_ok <- w$pregunta == as.character(source_q$item_text) | w$pregunta == as.character(source_q_en$item_text)
    if (!all(text_ok)) {
      bad <- which(!text_ok)
      stop(
        "Trabajador ", wid,
        ": el texto de la pregunta ha sido modificado en ",
        source_q$item_id[bad[[1]]], ".",
        call. = FALSE
      )
    }

    resp <- .erbio_clean_response(w$respuesta)
    if (any(!nzchar(resp))) {
      bad <- which(!nzchar(resp))
      stop(
        "Trabajador ", wid, ": faltan ", length(bad),
        " respuesta(s). Primera: ", source_q$item_id[bad[[1]]], ".",
        call. = FALSE
      )
    }
    invalid <- setdiff(unique(resp), c("Si", "No", "No procede"))
    if (length(invalid)) {
      stop(
        "Trabajador ", wid, ": respuesta(s) no válida(s): ",
        paste(invalid, collapse = ", "), ".",
        call. = FALSE
      )
    }

    responses_named <- stats::setNames(resp, source_q$item_id)
    score <- ERBioR::erbio_score_questionnaire(
      "workers",
      responses_named
    )

    individual[[i]] <- list(
      worker_id = wid,
      responses = responses_named,
      score = score
    )

    w$respuesta <- resp
    long_clean[[i]] <- w
  }

  long <- do.call(rbind, long_clean)

  # Source-preserving individual summary.
  individual_summary <- do.call(
    rbind,
    lapply(individual, function(z) {
      data.frame(
        worker_id = z$worker_id,
        n_total = z$score$n_items,
        n_applicable = z$score$scoring_detail$n_applicable,
        n_si = z$score$scoring_detail$n_compliant,
        n_no = z$score$scoring_detail$n_noncompliant,
        n_no_procede = z$score$scoring_detail$n_not_applicable,
        compliance_percent = z$score$compliance_percent,
        compliance_class = ERBioR::erbio_compliance_class(
          z$score$compliance_percent,
          boundary_policy = "prose_2015"
        ),
        failed_controls = nrow(z$score$failed_controls),
        stringsAsFactors = FALSE
      )
    })
  )

  # Documented group extension:
  # every applicable worker-item answer is one binary observation.
  all_resp <- unlist(lapply(individual, `[[`, "responses"), use.names = FALSE)
  detail <- ERBioR::erbio_compliance_binary(all_resp)
  group_class <- ERBioR::erbio_compliance_class(
    detail$percent,
    boundary_policy = "prose_2015"
  )

  group_summary <- data.frame(
    aggregation_rule = "pooled_applicable_worker_responses_v0.1",
    n_workers = length(worker_ids),
    n_total_responses = detail$n_total,
    n_applicable_responses = detail$n_applicable,
    n_si = detail$n_compliant,
    n_no = detail$n_noncompliant,
    n_no_procede = detail$n_not_applicable,
    compliance_percent = detail$percent,
    compliance_class = group_class,
    mean_individual_compliance = mean(individual_summary$compliance_percent),
    median_individual_compliance = stats::median(individual_summary$compliance_percent),
    min_individual_compliance = min(individual_summary$compliance_percent),
    max_individual_compliance = max(individual_summary$compliance_percent),
    stringsAsFactors = FALSE
  )

  # Per-item group distribution.
  item_summary <- do.call(
    rbind,
    lapply(source_q$item_id, function(iid) {
      rr <- long$respuesta[long$item_id == iid]
      applicable <- rr %in% c("Si", "No")
      n_app <- sum(applicable)
      n_yes <- sum(rr == "Si")
      n_no <- sum(rr == "No")
      n_np <- sum(rr == "No procede")
      data.frame(
        item_id = iid,
        numero = source_q$final_item[source_q$item_id == iid],
        pregunta = source_q$item_text[source_q$item_id == iid],
        n_workers = length(worker_ids),
        n_applicable = n_app,
        n_si = n_yes,
        n_no = n_no,
        n_no_procede = n_np,
        pct_si_applicable = if (n_app) 100 * n_yes / n_app else NA_real_,
        pct_no_applicable = if (n_app) 100 * n_no / n_app else NA_real_,
        workers_with_no = n_no,
        stringsAsFactors = FALSE
      )
    })
  )

  list(
    worker_ids = worker_ids,
    long = long,
    individual = individual,
    individual_summary = individual_summary,
    group_summary = group_summary,
    item_summary = item_summary,
    aggregation_rule = "pooled_applicable_worker_responses_v0.1",
    aggregation_note = paste(
      "El cuestionario se puntúa individualmente para cada trabajador.",
      "Para la vista grupal ERBioR v0.3 Shiny utiliza una extensión computacional explícita:",
      "total de respuestas Si dividido por (Si + No) entre todas las respuestas aplicables.",
      "No procede se excluye. El método ERBio original exige la participación de los trabajadores y una visión global,",
      "pero esta regla matemática concreta de agregación se etiqueta como extensión computacional ERBioR."
    )
  )
}

.erbio_detect_workbook_language <- function(path) {
  # Prefer explicit machine metadata when present.
  out <- tryCatch({
    dat <- .erbio_read_sheet(path, "01_DATOS")
    if (all(c("campo", "valor") %in% names(dat))) {
      nn <- tolower(.erbio_clean_character(dat$campo))
      vv <- tolower(.erbio_clean_character(dat$valor))
      idx <- which(nn %in% c("language", "idioma"))
      if (length(idx)) {
        z <- vv[idx[[1]]]
        if (z %in% c("es", "spanish", "espanol", "español")) return("es")
        if (z %in% c("en", "english", "ingles", "inglés")) return("en")
      }
    }
    NA_character_
  }, error = function(e) NA_character_)
  if (!is.na(out)) return(out)

  # Fallback: compare stable item_id + visible question text against both
  # approved language registries. This does not affect scientific scoring.
  q <- tryCatch(.erbio_read_sheet(path, "04_GENERAL"), error = function(e) NULL)
  if (is.null(q) || !all(c("item_id", "pregunta") %in% names(q))) return("unknown")
  q$item_id <- .erbio_clean_character(q$item_id)
  q$pregunta <- .erbio_clean_character(q$pregunta)
  src_es <- ERBioR::erbio_get_questionnaire("general")
  src_en <- ERBioR::erbio_get_questionnaire_i18n("general", language = "en")
  m <- match(q$item_id, src_es$item_id)
  ok <- !is.na(m) & nzchar(q$pregunta)
  if (!any(ok)) return("unknown")
  es_hits <- sum(q$pregunta[ok] == as.character(src_es$item_text[m[ok]]))
  en_hits <- sum(q$pregunta[ok] == as.character(src_en$item_text[m[ok]]))
  if (es_hits > en_hits && es_hits > 0L) "es" else if (en_hits > es_hits && en_hits > 0L) "en" else "unknown"
}

# -----------------------------------------------------------------------------
# Full workbook import
# -----------------------------------------------------------------------------

erbio_read_excel_evaluation <- function(path) {
  erbio_require_app_packages()
  input_language <- .erbio_detect_workbook_language(path)

  required <- c(
    "01_DATOS", "02_AGENTES", "03_AUDITORIA",
    "04_GENERAL", "05_TRABAJADORES", "06_SECTOR"
  )
  actual <- readxl::excel_sheets(path)
  missing <- setdiff(required, actual)
  if (length(missing)) {
    stop(
      "Faltan hojas requeridas: ", paste(missing, collapse = ", "), ".",
      call. = FALSE
    )
  }

  dat <- .erbio_read_sheet(path, "01_DATOS")
  if (!all(c("campo", "valor") %in% names(dat))) {
    stop(
      "01_DATOS debe contener las columnas Campo y Valor.",
      call. = FALSE
    )
  }
  meta_values <- .erbio_clean_character(dat$valor)
  meta_names <- .erbio_clean_character(dat$campo)
  meta <- setNames(meta_values, meta_names)

  activity <- meta[["actividad_puesto"]]
  sector_id <- meta[["sector_id"]]

  if (is.null(activity) || !nzchar(activity)) {
    stop(
      "Falta actividad_puesto en 01_DATOS. Indique el puesto o actividad evaluada.",
      call. = FALSE
    )
  }
  if (is.null(sector_id) || !nzchar(sector_id)) {
    stop(
      "Falta sector_id en 01_DATOS.",
      call. = FALSE
    )
  }

  # Agents
  ag <- .erbio_read_sheet(path, "02_AGENTES")
  if (!all(c("agent_name", "exposure") %in% names(ag))) {
    stop(
      "02_AGENTES debe contener agent_name y exposure.",
      call. = FALSE
    )
  }

  ag$agent_name <- .erbio_clean_character(ag$agent_name)
  ag$exposure <- .erbio_clean_character(ag$exposure)
  exposure_alias <- c(
    "Occasional" = "Ocasional", "Irregular" = "Irregular",
    "Frequent" = "Frecuente", "Very frequent" = "Muy frecuente",
    "Continuous" = "Continua"
  )
  hit_exp <- ag$exposure %in% names(exposure_alias)
  ag$exposure[hit_exp] <- unname(exposure_alias[ag$exposure[hit_exp]])
  if ("observaciones" %in% names(ag)) {
    ag$observaciones <- .erbio_clean_character(ag$observaciones)
  }

  ag <- ag[nzchar(ag$agent_name), , drop = FALSE]
  if (!nrow(ag)) {
    stop(
      "Debe indicar al menos un agente biológico en 02_AGENTES.",
      call. = FALSE
    )
  }

  if (any(!nzchar(ag$exposure))) {
    bad_agents <- ag$agent_name[!nzchar(ag$exposure)]
    stop(
      "Falta indicar la exposición para: ",
      paste(bad_agents, collapse = ", "), ".",
      call. = FALSE
    )
  }

  allowed_exp <- c(
    "Ocasional", "Irregular", "Frecuente",
    "Muy frecuente", "Continua", "Continuo"
  )
  bad_exp <- setdiff(unique(ag$exposure), allowed_exp)
  if (length(bad_exp)) {
    stop(
      "Exposición no válida: ", paste(bad_exp, collapse = ", "),
      ". Valores admitidos: Ocasional, Irregular, Frecuente, Muy frecuente o Continua.",
      call. = FALSE
    )
  }

  qa <- .erbio_read_single_questionnaire(path, "03_AUDITORIA", "audit")
  qg <- .erbio_read_single_questionnaire(path, "04_GENERAL", "general")
  workers <- .erbio_read_workers(path)
  qs <- .erbio_read_single_questionnaire(path, "06_SECTOR", sector_id)

  qresponses <- list(
    audit = qa$responses,
    general = qg$responses
  )
  qresponses[[qs$id]] <- qs$responses

  list(
    metadata = meta,
    activity = activity,
    sector_id = sector_id,
    agents_table = ag,
    agents = ag$agent_name,
    exposure = stats::setNames(ag$exposure, ag$agent_name),
    questionnaire_responses_nonworkers = qresponses,
    workers = workers,
    input_language = input_language
  )
}

# -----------------------------------------------------------------------------
# Agent resolution used by Excel evaluation
# -----------------------------------------------------------------------------


.erbio_rank_agent_candidates <- function(query, candidates, max_n = 20L) {
  if (is.null(candidates) || !nrow(candidates)) return(candidates)

  q <- tolower(trimws(as.character(query)[1]))
  nm <- tolower(as.character(candidates$agent_name))

  # Base-R edit distance only ranks the candidates; it never changes
  # the legal classification and never auto-selects an inexact agent.
  candidates$.distance <- as.numeric(utils::adist(q, nm))
  candidates <- candidates[
    order(candidates$.distance, candidates$agent_name),
    ,
    drop = FALSE
  ]
  candidates <- head(candidates, max_n)
  candidates$.distance <- NULL
  rownames(candidates) <- NULL
  candidates
}


.erbio_normalize_agent_query <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("[\u00A0\u2007\u202F]", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  x <- trimws(x)
  x <- sub("[.;,:]+$", "", x, perl = TRUE)
  x
}

erbio_prepare_agent_resolution <- function(agents) {
  agents <- as.character(agents)
  out <- vector("list", length(agents))
  names(out) <- agents

  for (i in seq_along(agents)) {
    query <- .erbio_normalize_agent_query(agents[[i]])

    exact <- ERBioR::erbio_agent_lookup(query, exact = TRUE)

    # Defensive normalized exact fallback: avoid presenting a fuzzy species
    # when the legal registry contains the same binomial with source punctuation.
    if (nrow(exact) != 1L) {
      reg <- ERBioR::erbio_load_agent_registry()
      if (is.data.frame(reg) && nrow(reg) && "agent_name" %in% names(reg)) {
        nq <- tolower(.erbio_normalize_agent_query(query))
        nr <- tolower(.erbio_normalize_agent_query(reg$agent_name))
        hit <- which(nr == nq)
        if (length(hit) == 1L) exact <- reg[hit, , drop = FALSE]
      }
    }

    if (nrow(exact) == 1L) {
      out[[i]] <- list(
        input = query,
        status = "exact",
        resolved_agent = exact$agent_name[[1]],
        candidates = exact
      )
      next
    }

    # First search the whole user string as a partial expression.
    partial <- ERBioR::erbio_agent_lookup(query, exact = FALSE)

    # If a misspelling prevents any match, broaden only to the first
    # taxonomic token (typically the genus), then rank candidates by
    # edit distance. Nothing is silently auto-selected.
    if (!nrow(partial)) {
      first_token <- strsplit(query, "\\s+")[[1]][1]
      partial <- ERBioR::erbio_agent_lookup(first_token, exact = FALSE)
    }

    partial <- .erbio_rank_agent_candidates(query, partial)

    out[[i]] <- list(
      input = query,
      status = if (nrow(partial)) "requires_selection" else "not_found",
      resolved_agent = NA_character_,
      candidates = partial
    )
  }

  class(out) <- c("erbio_app_agent_resolution", "list")
  out
}

erbio_agent_resolution_summary <- function(resolution) {
  if (!length(resolution)) return(data.frame())

  do.call(
    rbind,
    lapply(seq_along(resolution), function(i) {
      z <- resolution[[i]]
      data.frame(
        row = i,
        input_agent = z$input,
        status = z$status,
        resolved_agent = if (
          is.null(z$resolved_agent) || is.na(z$resolved_agent)
        ) "" else z$resolved_agent,
        n_candidates = if (is.null(z$candidates)) 0L else nrow(z$candidates),
        stringsAsFactors = FALSE
      )
    })
  )
}

.erbio_resolve_excel_agents <- function(
  agents,
  selected_agents = NULL,
  prepared_resolution = NULL
) {
  agents <- as.character(agents)

  if (is.null(prepared_resolution)) {
    prepared_resolution <- erbio_prepare_agent_resolution(agents)
  }

  if (!is.null(selected_agents)) {
    # Preserve the input-agent names while coercing values to character.
    # as.character() can drop vector attributes, including names.
    selected_names <- names(selected_agents)

    if (is.null(selected_names)) {
      stop(
        "No se ha podido conservar la correspondencia entre el agente ",
        "introducido y el agente ERBioR seleccionado. Vuelva a validar el Excel.",
        call. = FALSE
      )
    }

    selected_agents <- as.character(selected_agents)
    names(selected_agents) <- selected_names
  }

  refs <- vector("list", length(agents))
  names(refs) <- agents
  unresolved <- character(0)

  for (i in seq_along(agents)) {
    input_agent <- agents[[i]]
    prep <- prepared_resolution[[i]]

    chosen <- ""

    if (identical(prep$status, "exact")) {
      chosen <- prep$resolved_agent
    } else if (
      !is.null(selected_agents) &&
      input_agent %in% names(selected_agents)
    ) {
      chosen <- trimws(selected_agents[[input_agent]])
    }

    if (!nzchar(chosen)) {
      if (identical(prep$status, "not_found")) {
        unresolved <- c(
          unresolved,
          paste0(
            "«", input_agent,
            "»: no se encontraron candidatos en el registro ERBioR."
          )
        )
      } else {
        unresolved <- c(
          unresolved,
          paste0(
            "«", input_agent,
            "»: requiere seleccionar el agente concreto."
          )
        )
      }
      next
    }

    exact <- ERBioR::erbio_agent_lookup(chosen, exact = TRUE)
    if (nrow(exact) != 1L) {
      unresolved <- c(
        unresolved,
        paste0(
          "«", input_agent,
          "»: la selección «", chosen,
          "» no identifica un único agente."
        )
      )
      next
    }

    refs[[i]] <- ERBioR::erbio_reference_level_from_agent(
      exact$agent_name[[1]]
    )
  }

  if (length(unresolved)) {
    stop(
      "No puede iniciarse el cálculo hasta resolver todos los agentes:\n- ",
      paste(unresolved, collapse = "\n- "),
      call. = FALSE
    )
  }

  refs
}

# -----------------------------------------------------------------------------
# Preventive plan helpers
# -----------------------------------------------------------------------------

.erbio_plan_row <- function(
  questionnaire_id,
  item_id,
  deficiency,
  n_workers_affected = NA_integer_,
  n_applicable_workers = NA_integer_,
  pct_workers_no = NA_real_,
  context = ""
) {
  language <- ERBioR::erbio_get_language()
  pa <- ERBioR::erbio_preventive_action(item_id, language = language)
  if (identical(language, "en")) {
    tr <- ERBioR::erbio_load_translation_registry()
    it <- match(item_id, tr$item_id)
    if (!is.na(it) && nzchar(as.character(tr$question_text_en[[it]]))) deficiency <- as.character(tr$question_text_en[[it]])
  }

  editorial <- .erbio_editorial_flag(
    pa$cause_assessment_es[[1]],
    pa$corrective_action_es[[1]],
    pa$implementation_requirements_es[[1]],
    pa$verification_criteria_es[[1]]
  )

  data.frame(
    questionnaire_id = questionnaire_id,
    item_id = item_id,
    deficiency = deficiency,
    conditional_item_flag = .erbio_conditional_item_flag(deficiency),
    n_workers_affected = n_workers_affected,
    n_applicable_workers = n_applicable_workers,
    pct_workers_no = pct_workers_no,
    cause_assessment_es = pa$cause_assessment_es[[1]],
    cause_assessment_en = if ("cause_assessment_en" %in% names(pa)) pa$cause_assessment_en[[1]] else "",
    corrective_action_es = pa$corrective_action_es[[1]],
    corrective_action_en = if ("corrective_action_en" %in% names(pa)) pa$corrective_action_en[[1]] else "",
    implementation_requirements_es = pa$implementation_requirements_es[[1]],
    implementation_requirements_en = if ("implementation_requirements_en" %in% names(pa)) pa$implementation_requirements_en[[1]] else "",
    verification_criteria_es = pa$verification_criteria_es[[1]],
    verification_criteria_en = if ("verification_criteria_en" %in% names(pa)) pa$verification_criteria_en[[1]] else "",
    cause_assessment = if (identical(language, "en") && "cause_assessment_en" %in% names(pa)) pa$cause_assessment_en[[1]] else pa$cause_assessment_es[[1]],
    corrective_action = if (identical(language, "en") && "corrective_action_en" %in% names(pa)) pa$corrective_action_en[[1]] else pa$corrective_action_es[[1]],
    implementation_requirements = if (identical(language, "en") && "implementation_requirements_en" %in% names(pa)) pa$implementation_requirements_en[[1]] else pa$implementation_requirements_es[[1]],
    verification_criteria = if (identical(language, "en") && "verification_criteria_en" %in% names(pa)) pa$verification_criteria_en[[1]] else pa$verification_criteria_es[[1]],
    preventive_measure_candidate = if (identical(language, "en") && "preventive_action_en" %in% names(pa)) pa$preventive_action_en[[1]] else pa$preventive_action_es[[1]],
    preventive_taxonomy = pa$taxonomy_primary[[1]],
    preventive_prescription_level = pa$prescription_level[[1]],
    preventive_review_status = if ("review_status" %in% names(pa)) pa$review_status[[1]] else "",
    preventive_provenance = if ("provenance_layer" %in% names(pa)) pa$provenance_layer[[1]] else "",
    editorial_review_flag = editorial$flag,
    editorial_review_reason = editorial$reason,
    context = context,
    responsible = NA_character_,
    target_date = NA_character_,
    resources = NA_character_,
    stringsAsFactors = FALSE
  )
}

.erbio_plan_from_score <- function(score, questionnaire_id, context = "") {
  fc <- score$failed_controls
  if (is.null(fc) || !nrow(fc)) return(data.frame())

  do.call(
    rbind,
    lapply(seq_len(nrow(fc)), function(i) {
      .erbio_plan_row(
        questionnaire_id = questionnaire_id,
        item_id = fc$item_id[[i]],
        deficiency = fc$item_text[[i]],
        context = context
      )
    })
  )
}

.erbio_plan_from_workers <- function(workers) {
  fail <- workers$item_summary[
    !is.na(workers$item_summary$n_no) & workers$item_summary$n_no > 0,
    ,
    drop = FALSE
  ]
  if (!nrow(fail)) return(data.frame())

  do.call(
    rbind,
    lapply(seq_len(nrow(fail)), function(i) {
      .erbio_plan_row(
        questionnaire_id = "workers",
        item_id = fail$item_id[[i]],
        deficiency = fail$pregunta[[i]],
        n_workers_affected = fail$n_no[[i]],
        n_applicable_workers = fail$n_applicable[[i]],
        pct_workers_no = fail$pct_no_applicable[[i]],
        context = paste0(
          "Respuesta No en ", fail$n_no[[i]], " de ",
          fail$n_applicable[[i]],
          " trabajadores con respuesta aplicable (",
          sprintf("%.1f%%", fail$pct_no_applicable[[i]]), ")."
        )
      )
    })
  )
}

.erbio_localize_result_label <- function(x, language = ERBioR::erbio_get_language()) {
  language <- match.arg(language, c("es", "en"))
  x <- as.character(x)
  if (!identical(language, "en")) return(x)
  map <- c(
    "Ocasional" = "Occasional", "Irregular" = "Irregular",
    "Frecuente" = "Frequent", "Muy frecuente" = "Very frequent",
    "Continua" = "Continuous", "Continuo" = "Continuous",
    "Muy deficiente" = "Very poor", "Deficiente" = "Poor",
    "Mejorable" = "Needs improvement", "Aceptable" = "Acceptable",
    "Trivial" = "Trivial", "Tolerable" = "Tolerable",
    "Moderado" = "Moderate", "Importante" = "Important",
    "Intolerable" = "Intolerable", "Baja" = "Low", "Media" = "Medium",
    "Alta" = "High", "Inmediata" = "Immediate",
    "SÍ — REQUERIDA" = "YES — REQUIRED", "NO" = "NO"
  )
  hit <- x %in% names(map)
  x[hit] <- unname(map[x[hit]])
  x
}

# -----------------------------------------------------------------------------
# Full evaluation with explicit workers-group extension
# -----------------------------------------------------------------------------


erbio_evaluate_payload <- function(
  payload,
  selected_agents = NULL,
  prepared_resolution = NULL,
  language = ERBioR::erbio_get_language()
) {
  language <- match.arg(language, c("es", "en"))
  refs <- .erbio_resolve_excel_agents(
    payload$agents,
    selected_agents = selected_agents,
    prepared_resolution = prepared_resolution
  )

  # Questionnaire scores independent of agent.
  q_scores <- lapply(
    names(payload$questionnaire_responses_nonworkers),
    function(qid) {
      ERBioR::erbio_score_questionnaire(
        qid,
        payload$questionnaire_responses_nonworkers[[qid]],
        language = language
      )
    }
  )
  names(q_scores) <- names(payload$questionnaire_responses_nonworkers)

  q_summary <- do.call(
    rbind,
    lapply(names(q_scores), function(qid) {
      s <- q_scores[[qid]]
      data.frame(
        questionnaire_id = qid,
        questionnaire_name = s$questionnaire_name,
        aggregation = "single_questionnaire",
        n_respondents = 1L,
        n_applicable = s$scoring_detail$n_applicable,
        n_si = s$scoring_detail$n_compliant,
        n_no = s$scoring_detail$n_noncompliant,
        n_no_procede = s$scoring_detail$n_not_applicable,
        compliance_percent = s$compliance_percent,
        compliance_class = .erbio_localize_result_label(ERBioR::erbio_compliance_class(
          s$compliance_percent,
          boundary_policy = "prose_2015"
        ), language),
        stringsAsFactors = FALSE
      )
    })
  )

  wg <- payload$workers$group_summary
  q_workers_summary <- data.frame(
    questionnaire_id = "workers",
    questionnaire_name = if (identical(language, "en")) "Biological risk assessment in workers" else "Valoración del riesgo biológico en trabajadores",
    aggregation = wg$aggregation_rule,
    n_respondents = wg$n_workers,
    n_applicable = wg$n_applicable_responses,
    n_si = wg$n_si,
    n_no = wg$n_no,
    n_no_procede = wg$n_no_procede,
    compliance_percent = wg$compliance_percent,
    compliance_class = .erbio_localize_result_label(wg$compliance_class, language),
    stringsAsFactors = FALSE
  )
  q_summary <- rbind(q_summary, q_workers_summary)

  risk_rows <- list()
  k <- 0L

  for (i in seq_along(payload$agents)) {
    input_agent <- payload$agents[[i]]
    ref <- refs[[i]]
    exp <- payload$exposure[[input_agent]]

    for (qid in names(payload$questionnaire_responses_nonworkers)) {
      k <- k + 1L
      rr <- ERBioR::erbio_assess_agent_questionnaire(
        agent = ref$agent_name,
        questionnaire_id = qid,
        responses = payload$questionnaire_responses_nonworkers[[qid]],
        exposure = exp
      )
      risk_rows[[k]] <- data.frame(
        input_agent = input_agent,
        agent_name = ref$agent_name,
        reference_level = ref$reference_level,
        exposure = .erbio_localize_result_label(rr$exposure, language),
        questionnaire_id = qid,
        questionnaire_name = rr$questionnaire_name,
        questionnaire_aggregation = "source_questionnaire",
        compliance_percent = rr$compliance_percent,
        compliance_class = .erbio_localize_result_label(rr$compliance_class, language),
        probability_value = rr$probability_value,
        probability_label = .erbio_localize_result_label(rr$probability_label, language),
        risk_score = rr$risk_score,
        risk_class = .erbio_localize_result_label(rr$risk_class, language),
        priority = .erbio_localize_result_label(rr$priority, language),
        stringsAsFactors = FALSE
      )
    }

    wr <- ERBioR::erbio_assess(
      reference_level = ref$reference_level,
      exposure = exp,
      compliance_percent = wg$compliance_percent
    )
    k <- k + 1L
    risk_rows[[k]] <- data.frame(
      input_agent = input_agent,
      agent_name = ref$agent_name,
      reference_level = ref$reference_level,
      exposure = .erbio_localize_result_label(wr$exposure, language),
      questionnaire_id = "workers",
      questionnaire_name = if (identical(language, "en")) "Biological risk assessment in workers" else "Valoración del riesgo biológico en trabajadores",
      questionnaire_aggregation = wg$aggregation_rule,
      compliance_percent = wr$compliance_percent,
      compliance_class = .erbio_localize_result_label(wr$compliance_class, language),
      probability_value = wr$probability_value,
      probability_label = .erbio_localize_result_label(wr$probability_label, language),
      risk_score = wr$risk_score,
      risk_class = .erbio_localize_result_label(wr$risk_class, language),
      priority = .erbio_localize_result_label(wr$priority, language),
      stringsAsFactors = FALSE
    )
  }

  risk_results <- do.call(rbind, risk_rows)
  agent_risk_summary <- .erbio_highest_observed_summary(risk_results)
  worker_class_distribution <- .erbio_worker_class_distribution(
    payload$workers$individual_summary
  )
  worker_top_deficiencies <- .erbio_worker_top_deficiencies(
    payload$workers$item_summary,
    n = 10L
  )

  agent_summary <- do.call(
    rbind,
    lapply(seq_along(payload$agents), function(i) {
      input_agent <- payload$agents[[i]]
      z <- refs[[i]]
      data.frame(
        input_agent = input_agent,
        resolved_agent = z$agent_name,
        agent_id = z$agent_id,
        agent_type = z$agent_type,
        reference_level = z$reference_level,
        classification_status = z$classification_status,
        exposure = .erbio_localize_result_label(payload$exposure[[input_agent]], language),
        professional_classification_review = if (
          isTRUE(z$requires_professional_assessment)
        ) if (identical(language, "en")) "YES — REQUIRED" else "SÍ — REQUERIDA" else "NO",
        stringsAsFactors = FALSE
      )
    })
  )

  plan_parts <- c(
    lapply(names(q_scores), function(qid) {
      .erbio_plan_from_score(
        q_scores[[qid]],
        qid,
        context = "Deficiencia detectada en cuestionario de evaluación."
      )
    }),
    list(.erbio_plan_from_workers(payload$workers))
  )
  plan_parts <- plan_parts[vapply(plan_parts, nrow, integer(1)) > 0L]

  preventive_plan <- if (length(plan_parts)) {
    do.call(rbind, plan_parts)
  } else {
    data.frame(
      questionnaire_id = character(0),
      item_id = character(0),
      deficiency = character(0),
      conditional_item_flag = character(0),
      n_workers_affected = integer(0),
      n_applicable_workers = integer(0),
      pct_workers_no = numeric(0),
      cause_assessment_es = character(0), cause_assessment_en = character(0), cause_assessment = character(0),
      corrective_action_es = character(0), corrective_action_en = character(0), corrective_action = character(0),
      implementation_requirements_es = character(0), implementation_requirements_en = character(0), implementation_requirements = character(0),
      verification_criteria_es = character(0), verification_criteria_en = character(0), verification_criteria = character(0),
      preventive_measure_candidate = character(0),
      preventive_taxonomy = character(0),
      preventive_prescription_level = character(0),
      preventive_review_status = character(0),
      preventive_provenance = character(0),
      editorial_review_flag = character(0),
      editorial_review_reason = character(0),
      context = character(0),
      responsible = character(0),
      target_date = character(0),
      resources = character(0),
      stringsAsFactors = FALSE
    )
  }

  n_preventive_measures <- nrow(preventive_plan)

  n_editorial_flags <- if (n_preventive_measures) {
    sum(preventive_plan$editorial_review_flag != "OK")
  } else 0L

  n_conditional_flags <- if (n_preventive_measures) {
    sum(preventive_plan$conditional_item_flag != "OK")
  } else 0L

  n_worker_measures <- if (n_preventive_measures) {
    sum(preventive_plan$questionnaire_id == "workers")
  } else 0L

  n_immediate_results <- if (nrow(risk_results)) {
    sum(risk_results$priority %in% c("Inmediata", "Immediate"), na.rm = TRUE)
  } else 0L

  warnings <- c(
    if (identical(language, "en")) "Worker questionnaire group aggregation is a documented ERBioR extension; individual results are retained." else payload$workers$aggregation_note,
    paste0(
      if (identical(language, "en")) "Agents entered and resolved: " else "Agentes introducidos y resueltos: ",
      paste(
        paste0(
          payload$agents,
          " → ",
          vapply(refs, `[[`, character(1), "agent_name")
        ),
        collapse = "; "
      ),
      "."
    ),
    if (identical(language, "en")) "The worker-questionnaire group aggregation is explicitly identified as an ERBioR extension and does not replace retention of individual results." else "La agregación grupal del cuestionario de trabajadores se identifica expresamente como extensión ERBioR y no sustituye la conservación de los resultados individuales.",
    if (n_editorial_flags) paste0(
      n_editorial_flags, " de ", n_preventive_measures,
      " medidas preventivas activas han sido marcadas por una heurística editorial ",
      "porque contienen una o más fórmulas de redacción genérica. ",
      "La marca no modifica el cálculo del riesgo ni significa que la medida sea científicamente incorrecta; ",
      "indica que conviene revisar su formulación antes de emitir la planificación definitiva."
    ) else NULL,
    if (n_conditional_flags) paste0(
      n_conditional_flags,
      " ítem(s) respondidos «No» dependen de una condición, situación histórica, ",
      "tipo de instalación o tarea previa. Confirme que el supuesto era aplicable; ",
      "si no lo era, la respuesta puede corresponder a «No procede». ",
      "Este aviso no añade riesgos ni deficiencias nuevas."
    ) else NULL,
    if (identical(language, "en")) "Responsible person, target date and resources are not inferred automatically." else "Responsable, plazo y recursos de la planificación no se infieren automáticamente."
  )

  worker_individual_out <- payload$workers$individual_summary
  worker_group_out <- payload$workers$group_summary
  worker_item_out <- payload$workers$item_summary
  if (identical(language, "en")) {
    if ("compliance_class" %in% names(worker_individual_out)) worker_individual_out$compliance_class <- .erbio_localize_result_label(worker_individual_out$compliance_class, language)
    if ("compliance_class" %in% names(worker_group_out)) worker_group_out$compliance_class <- .erbio_localize_result_label(worker_group_out$compliance_class, language)
    if ("pregunta" %in% names(worker_item_out)) {
      trw <- ERBioR::erbio_get_questionnaire_i18n("workers", language = "en")
      mw <- match(worker_item_out$item_id, trw$item_id)
      ok <- !is.na(mw)
      worker_item_out$pregunta[ok] <- trw$item_text[mw[ok]]
    }
  }

  list(
    software_version = as.character(utils::packageVersion("ERBioR")),
    activity = payload$activity,
    sector_id = payload$sector_id,
    assessment_status = "completed_with_documented_workers_group_extension",
    payload = payload,
    references = refs,
    agent_summary = agent_summary,
    agent_risk_summary = agent_risk_summary,
    questionnaire_summary = q_summary,
    worker_individual_summary = worker_individual_out,
    worker_group_summary = worker_group_out,
    worker_class_distribution = worker_class_distribution,
    worker_item_summary = worker_item_out,
    worker_top_deficiencies = worker_top_deficiencies,
    risk_results = risk_results,
    preventive_plan = preventive_plan,
    presentation_language = language,
    input_language = if (!is.null(payload$input_language)) payload$input_language else "unknown",
    n_preventive_measures = n_preventive_measures,
    n_editorial_flags = n_editorial_flags,
    n_conditional_flags = n_conditional_flags,
    n_worker_measures = n_worker_measures,
    n_immediate_results = n_immediate_results,
    warnings = unique(warnings)
  )
}

erbio_evaluate_excel <- function(
  path,
  selected_agents = NULL,
  prepared_resolution = NULL
) {
  payload <- erbio_read_excel_evaluation(path)
  erbio_evaluate_payload(
    payload,
    selected_agents = selected_agents,
    prepared_resolution = prepared_resolution
  )
}

# -----------------------------------------------------------------------------
# Report / bundle for Shiny v0.3 object
# -----------------------------------------------------------------------------

.erbio_md_escape_app <- function(x) {
  y <- as.character(x)
  y <- gsub("\\|", "\\\\|", y)
  y <- gsub("\n", " ", y, fixed = TRUE)
  y
}

.erbio_md_table_app <- function(df) {
  if (is.null(df) || !nrow(df)) return("_Sin datos._")
  df[] <- lapply(df, .erbio_md_escape_app)
  header <- paste0("| ", paste(names(df), collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", ncol(df)), collapse = " | "), " |")
  body <- apply(df, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

.erbio_report_plan_actions <- function(plan) {
  if (is.null(plan) || !nrow(plan)) return(data.frame())
  keep <- c(
    "questionnaire_id", "item_id", "deficiency",
    "n_workers_affected", "pct_workers_no",
    "corrective_action_es",
    "implementation_requirements_es",
    "verification_criteria_es",
    "preventive_prescription_level",
    "conditional_item_flag"
  )
  .erbio_round_display(plan[, keep, drop = FALSE], 2L)
}

.erbio_report_plan_causes <- function(plan) {
  if (is.null(plan) || !nrow(plan)) return(data.frame())
  keep <- c(
    "questionnaire_id", "item_id", "deficiency",
    "cause_assessment_es", "editorial_review_flag",
    "editorial_review_reason"
  )
  plan[, keep, drop = FALSE]
}

erbio_render_app_report <- function(x) {
  risk_summary <- .erbio_round_display(x$agent_risk_summary, 2L)
  q_summary <- .erbio_round_display(x$questionnaire_summary, 2L)
  risk_detail <- .erbio_round_display(x$risk_results, 2L)
  worker_group <- .erbio_round_display(x$worker_group_summary, 2L)
  worker_classes <- .erbio_round_display(x$worker_class_distribution, 2L)
  worker_top <- .erbio_round_display(x$worker_top_deficiencies, 2L)
  worker_ind <- .erbio_round_display(x$worker_individual_summary, 2L)
  plan_actions <- .erbio_report_plan_actions(x$preventive_plan)
  plan_causes <- .erbio_report_plan_causes(x$preventive_plan)

  review_rows <- if (nrow(x$preventive_plan)) {
    x$preventive_plan[
      x$preventive_plan$editorial_review_flag != "OK" |
        x$preventive_plan$conditional_item_flag != "OK",
      c(
        "questionnaire_id", "item_id", "deficiency",
        "conditional_item_flag", "editorial_review_flag",
        "editorial_review_reason"
      ),
      drop = FALSE
    ]
  } else data.frame()

  lines <- c(
    "# ERBioR — Informe profesional de evaluación de riesgo biológico",
    "",
    paste0("**Actividad/puesto:** ", x$activity),
    paste0("**Sector:** ", x$sector_id),
    paste0("**Versión ERBioR:** ", x$software_version),
    "",
    "## 1. Resumen ejecutivo",
    "",
    "La tabla siguiente muestra, para cada agente, la **mayor clase de riesgo observada** entre los cuestionarios evaluados. Es un descriptor para facilitar la interpretación profesional y **no constituye una agregación matemática global de los cuestionarios**.",
    "",
    .erbio_md_table_app(risk_summary),
    "",
    "## 2. Agentes biológicos",
    .erbio_md_table_app(x$agent_summary),
    "",
    "## 3. Resumen de cuestionarios",
    .erbio_md_table_app(q_summary),
    "",
    "## 4. Riesgo por agente y cuestionario",
    .erbio_md_table_app(risk_detail),
    "",
    "## 5. Cuestionario de trabajadores",
    "",
    "### 5.1 Resumen grupal",
    .erbio_md_table_app(worker_group),
    "",
    "### 5.2 Distribución de trabajadores por clase de cumplimiento",
    .erbio_md_table_app(worker_classes),
    "",
    "### 5.3 Principales deficiencias percibidas",
    .erbio_md_table_app(worker_top),
    "",
    "### 5.4 Resultados individuales",
    .erbio_md_table_app(worker_ind),
    "",
    "### 5.5 Nota de agregación",
    x$payload$workers$aggregation_note,
    "",
    "## 6. Planificación preventiva experta",
    "",
    "### 6.1 Acciones, implantación y verificación",
    .erbio_md_table_app(plan_actions),
    "",
    "### 6.2 Análisis causal y revisión editorial",
    .erbio_md_table_app(plan_causes),
    "",
    "## 7. Avisos para revisión profesional",
    "",
    if (nrow(review_rows)) .erbio_md_table_app(review_rows) else "_No se han generado avisos adicionales de aplicabilidad o redacción genérica._",
    "",
    "## 8. Trazabilidad y advertencias",
    "",
    paste0("- ", x$warnings, collapse = "\n"),
    "",
    "- Los campos responsable, plazo y recursos requieren cumplimentación profesional; ERBioR no los infiere automáticamente.",
    "- La revisión editorial identifica formulaciones genéricas de la medida preventiva; no es una revisión científica del cálculo ni una nueva deficiencia.",
    "- La revisión de aplicabilidad identifica preguntas condicionadas o históricas respondidas «No»; confirme si realmente eran aplicables antes de cerrar la evaluación."
  )

  paste(lines, collapse = "\n")
}

erbio_export_app_bundle <- function(x, dir) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  utils::write.csv(x$agent_summary, file.path(dir, "01_agents.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$agent_risk_summary, file.path(dir, "02_agent_highest_observed_risk.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$questionnaire_summary, file.path(dir, "03_questionnaires.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$risk_results, file.path(dir, "04_risk_results.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$worker_individual_summary, file.path(dir, "05_workers_individual.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$worker_class_distribution, file.path(dir, "06_workers_class_distribution.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$worker_top_deficiencies, file.path(dir, "07_workers_top_deficiencies.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$worker_item_summary, file.path(dir, "08_workers_items_group.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  utils::write.csv(x$preventive_plan, file.path(dir, "09_preventive_plan_structured.csv"), row.names = FALSE, fileEncoding = "UTF-8")

  review_rows <- if (nrow(x$preventive_plan)) {
    x$preventive_plan[
      x$preventive_plan$editorial_review_flag != "OK" |
        x$preventive_plan$conditional_item_flag != "OK",
      ,
      drop = FALSE
    ]
  } else x$preventive_plan

  utils::write.csv(review_rows, file.path(dir, "10_professional_review_flags.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  writeLines(erbio_render_app_report(x), file.path(dir, "11_professional_report.md"), useBytes = TRUE)
  invisible(dir)
}

