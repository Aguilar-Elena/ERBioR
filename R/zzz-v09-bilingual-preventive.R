###############################################################################
# ERBioR v0.9 development layer
# Recovered bilingual scientific registry + approved preventive-action registry.
# The deterministic ERBio calculation core remains unchanged.
###############################################################################

ERBIOR_TRANSLATION_REGISTRY_VERSION <- "ERBioR_scientific_translation_registry_v1_1_ALL_1008_APPROVED_EN.csv"
ERBIOR_PREVENTIVE_REGISTRY_VERSION <- "ERBioR_preventive_actions_APPROVED_ES_v0_5.csv"
ERBIOR_UI_DICTIONARY_VERSION <- "ERBioR_ui_dictionary_es_en_v0_1.csv"
ERBIOR_EU_AUDIT_SUMMARY_VERSION <- "ERBioR_source_audit_summary_v0_8.csv"

.erbio_language_env <- new.env(parent = emptyenv())
.erbio_language_env$language <- "es"

erbio_set_language <- function(language = c("es", "en")) {
  language <- match.arg(language)
  .erbio_language_env$language <- language
  options(erbio.language = language)
  invisible(language)
}

erbio_get_language <- function() {
  x <- getOption("erbio.language", .erbio_language_env$language)
  if (!x %in% c("es", "en")) x <- "es"
  x
}

.erbio_read_utf8_csv <- function(filename, path = NULL) {
  f <- .erbio_find_data_file(filename, path)
  x <- tryCatch(
    read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM"),
    error = function(e) read.csv(f, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8")
  )
  attr(x, "source_file") <- f
  x
}

erbio_load_translation_registry <- function(path = NULL) {
  x <- .erbio_read_utf8_csv(ERBIOR_TRANSLATION_REGISTRY_VERSION, path)
  required <- c(
    "item_id", "questionnaire_id", "questionnaire_name_es", "questionnaire_name_en",
    "section_es", "section_en", "question_text_es", "question_text_en",
    "question_translation_status", "source_validation_status", "english_validation_scope",
    "taxonomy_code", "taxonomy_name_es", "taxonomy_name_en",
    "cause_assessment_es", "cause_assessment_en", "corrective_action_es", "corrective_action_en",
    "implementation_requirements_es", "implementation_requirements_en",
    "verification_criteria_es", "verification_criteria_en",
    "preventive_action_es", "preventive_action_en", "preventive_translation_status"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) .erbio_stop("Translation registry incompleto. Faltan columnas: ", paste(missing, collapse = ", "))
  x
}

erbio_validate_translation_registry <- function(path = NULL) {
  x <- erbio_load_translation_registry(path)
  required_en <- c(
    "questionnaire_name_en", "section_en", "question_text_en", "taxonomy_name_en",
    "cause_assessment_en", "corrective_action_en", "implementation_requirements_en",
    "verification_criteria_en", "preventive_action_en"
  )
  nonempty <- function(z) !is.na(z) & nzchar(trimws(as.character(z)))
  missing_en <- sum(!Reduce(`&`, lapply(x[required_en], nonempty)))
  list(
    valid = nrow(x) == 1008L && !anyDuplicated(x$item_id) && missing_en == 0L &&
      all(x$question_translation_status == "APPROVED_EN_TRANSLATION") &&
      all(x$preventive_translation_status == "APPROVED_EN_TRANSLATION"),
    n_items = nrow(x),
    n_unique_item_id = length(unique(x$item_id)),
    n_question_approved_en = sum(x$question_translation_status == "APPROVED_EN_TRANSLATION", na.rm = TRUE),
    n_preventive_approved_en = sum(x$preventive_translation_status == "APPROVED_EN_TRANSLATION", na.rm = TRUE),
    n_missing_required_english_rows = missing_en,
    psychometric_validation_implied = FALSE
  )
}

erbio_load_preventive_action_registry <- function(path = NULL) {
  x <- .erbio_read_utf8_csv(ERBIOR_PREVENTIVE_REGISTRY_VERSION, path)
  required <- c(
    "item_id", "questionnaire_id", "item_text_es_source", "validation_status_source",
    "taxonomy_primary", "taxonomy_name", "prescription_level",
    "cause_assessment_es", "corrective_action_es", "implementation_requirements_es",
    "verification_criteria_es", "preventive_action_es", "review_status"
  )
  missing <- setdiff(required, names(x))
  if (length(missing)) .erbio_stop("Preventive action registry incompleto. Faltan columnas: ", paste(missing, collapse = ", "))
  x
}

erbio_validate_preventive_action_registry <- function(path = NULL) {
  x <- erbio_load_preventive_action_registry(path)
  q <- erbio_load_question_bank(path)
  idx <- match(x$item_id, q$item_id)
  missing_in_bank <- is.na(idx)
  source_mismatch <- rep(FALSE, nrow(x))
  comparable <- !missing_in_bank
  source_mismatch[comparable] <- as.character(x$item_text_es_source[comparable]) != as.character(q$item_text[idx[comparable]])
  required_text <- c("cause_assessment_es", "corrective_action_es", "implementation_requirements_es", "verification_criteria_es", "preventive_action_es")
  nonempty <- function(z) !is.na(z) & nzchar(trimws(as.character(z)))
  missing_required <- sum(!Reduce(`&`, lapply(x[required_text], nonempty)))
  valid <- nrow(x) == 1008L && !anyDuplicated(x$item_id) &&
    sum(x$review_status == "APPROVED_ES", na.rm = TRUE) == 1008L &&
    !any(missing_in_bank) && !any(source_mismatch) && missing_required == 0L
  list(
    valid = valid,
    n_actions = nrow(x),
    n_approved_es = sum(x$review_status == "APPROVED_ES", na.rm = TRUE),
    n_source_text_mismatches = sum(source_mismatch),
    n_missing_item_ids_in_question_bank = sum(missing_in_bank),
    n_missing_required_action_rows = missing_required
  )
}

erbio_load_ui_dictionary <- function(path = NULL) {
  x <- .erbio_read_utf8_csv(ERBIOR_UI_DICTIONARY_VERSION, path)
  if (!all(c("key", "es", "en") %in% names(x))) .erbio_stop("UI dictionary incompleto.")
  x
}

erbio_translate_ui <- function(key, language = erbio_get_language(), path = NULL) {
  language <- match.arg(language, c("es", "en"))
  d <- erbio_load_ui_dictionary(path)
  idx <- match(as.character(key), d$key)
  out <- as.character(key)
  ok <- !is.na(idx)
  out[ok] <- d[[language]][idx[ok]]
  out
}

erbio_get_questionnaire_i18n <- function(questionnaire_id, language = erbio_get_language(), version = NULL, path = NULL) {
  language <- match.arg(language, c("es", "en"))
  q <- erbio_get_questionnaire(questionnaire_id, version = version, path = path)
  tr <- erbio_load_translation_registry(path)
  z <- tr[match(q$item_id, tr$item_id), , drop = FALSE]
  if (any(is.na(z$item_id))) .erbio_stop("Faltan item_id en el registro bilingue para ", questionnaire_id, ".")
  q$questionnaire_name <- z[[paste0("questionnaire_name_", language)]]
  q$sector <- z[[paste0("sector_", language)]]
  q$subtype <- z[[paste0("subtype_", language)]]
  q$section <- z[[paste0("section_", language)]]
  q$item_text <- z[[paste0("question_text_", language)]]
  q$presentation_language <- language
  attr(q, "translation_status") <- if (language == "en") z$question_translation_status else rep("SOURCE_ES", nrow(z))
  q
}

.erbio_enrich_failed_controls <- function(failed, language = erbio_get_language(), path = NULL) {
  if (!nrow(failed)) {
    failed$preventive_taxonomy <- character(0)
    failed$preventive_prescription_level <- character(0)
    failed$expert_preventive_action_es <- character(0)
    failed$expert_preventive_action_en <- character(0)
    failed$expert_preventive_action <- character(0)
    return(failed)
  }
  p <- erbio_load_preventive_action_registry(path)
  tr <- erbio_load_translation_registry(path)
  ip <- match(failed$item_id, p$item_id)
  it <- match(failed$item_id, tr$item_id)
  if (any(is.na(ip)) || any(is.na(it))) .erbio_stop("No se pudo enlazar la capa preventiva con todos los controles fallidos.")
  failed$preventive_taxonomy <- p$taxonomy_primary[ip]
  failed$preventive_prescription_level <- p$prescription_level[ip]
  failed$expert_preventive_action_es <- p$preventive_action_es[ip]
  failed$expert_preventive_action_en <- tr$preventive_action_en[it]
  failed$expert_preventive_action <- if (identical(language, "en")) failed$expert_preventive_action_en else failed$expert_preventive_action_es
  # Preserve the historical/frozen meaning of planning_candidate: the failed
  # questionnaire item text.  The approved expert prescription is carried in
  # expert_preventive_action and is consumed explicitly by the planning layer.
  failed
}

# v0.9 override: same deterministic scoring, enriched presentation/preventive layer.
erbio_score_questionnaire <- function(
  questionnaire_id,
  responses,
  version = NULL,
  scoring = c("default", "binary", "audit_0_4"),
  path = NULL,
  language = erbio_get_language()
) {
  scoring <- match.arg(scoring)
  language <- match.arg(language, c("es", "en"))
  q_source <- erbio_get_questionnaire(questionnaire_id, version = version, path = path)
  q <- erbio_get_questionnaire_i18n(questionnaire_id, language = language, version = version, path = path)
  x <- .erbio_align_questionnaire_responses(responses, q_source)

  if (scoring == "default") {
    scoring <- if (q_source$default_scoring[[1]] == "binary_yes_no_na") "binary" else q_source$default_scoring[[1]]
  }
  allowed <- unique(unlist(strsplit(q_source$allowed_scoring[[1]], "\\|", fixed = FALSE)))
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
  failed <- .erbio_enrich_failed_controls(failed, language = language, path = path)

  out <- list(
    questionnaire_id = q_source$questionnaire_id[[1]],
    questionnaire_name = q$questionnaire_name[[1]],
    version = q_source$version[[1]],
    instrument_type = q_source$instrument_type[[1]],
    sector = q$sector[[1]], subtype = q$subtype[[1]],
    validation_status = q_source$validation_status[[1]],
    presentation_language = language,
    n_items = nrow(q_source), scoring = scoring,
    scoring_detail = detail, compliance_percent = detail$percent,
    failed_controls = failed,
    provenance = list(
      source = q_source$source[[1]], question_bank = ERBIOR_QUESTION_BANK_VERSION,
      translation_registry = ERBIOR_TRANSLATION_REGISTRY_VERSION,
      preventive_registry = ERBIOR_PREVENTIVE_REGISTRY_VERSION,
      presentation_language = language,
      source_lines = range(c(q_source$source_line_start, q_source$source_line_end), na.rm = TRUE)
    )
  )
  class(out) <- c("erbio_questionnaire_score", "list")
  out
}

erbio_bilingual_invariance_check <- function(questionnaire_id, responses, version = NULL, scoring = "default", path = NULL) {
  es <- erbio_score_questionnaire(questionnaire_id, responses, version, scoring, path, language = "es")
  en <- erbio_score_questionnaire(questionnaire_id, responses, version, scoring, path, language = "en")
  list(
    invariant = identical(es$compliance_percent, en$compliance_percent) &&
      identical(es$scoring_detail, en$scoring_detail) &&
      identical(es$failed_controls$item_id, en$failed_controls$item_id),
    questionnaire_id = questionnaire_id,
    compliance_percent_es = es$compliance_percent,
    compliance_percent_en = en$compliance_percent,
    failed_item_ids_identical = identical(es$failed_controls$item_id, en$failed_controls$item_id)
  )
}

#' Retrieve the approved preventive action for one questionnaire item
#'
#' Returns the approved Spanish preventive-action registry row for a stable
#' ERBioR item_id. When language = "en", approved English presentation fields
#' from the bilingual translation registry are added without altering the
#' scientific identifiers or preventive taxonomy.
#'
#' @param item_id Stable ERBioR questionnaire item identifier.
#' @param language Presentation language, "es" or "en".
#' @param path Optional data path used by ERBioR registry loaders.
#' @return A one-row data.frame with the preventive-action record.
erbio_preventive_action <- function(item_id, language = erbio_get_language(), path = NULL) {
  if (length(item_id) != 1L || is.na(item_id) || !nzchar(trimws(as.character(item_id)))) {
    .erbio_stop("item_id debe identificar un \u00fanico \u00edtem ERBioR.")
  }
  language <- match.arg(language, c("es", "en"))
  item_id <- as.character(item_id)

  p <- erbio_load_preventive_action_registry(path)
  idx <- which(as.character(p$item_id) == item_id)
  if (length(idx) != 1L) {
    if (!length(idx)) .erbio_stop("No existe acci\u00f3n preventiva aprobada para item_id: ", item_id)
    .erbio_stop("item_id duplicado en el registro preventivo: ", item_id)
  }
  out <- p[idx, , drop = FALSE]

  tr <- erbio_load_translation_registry(path)
  it <- match(item_id, tr$item_id)
  if (!is.na(it)) {
    en_fields <- c(
      "cause_assessment_en", "corrective_action_en",
      "implementation_requirements_en", "verification_criteria_en",
      "preventive_action_en", "preventive_translation_status"
    )
    for (nm in en_fields) {
      if (nm %in% names(tr)) out[[nm]] <- tr[[nm]][it]
    }
  }

  out$presentation_language <- language
  if (identical(language, "en") && "preventive_action_en" %in% names(out)) {
    out$preventive_action <- out$preventive_action_en
  } else {
    out$preventive_action <- out$preventive_action_es
  }
  out
}
