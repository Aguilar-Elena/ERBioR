# ============================================================
# ErBioBench batch scoring P04 v1
# Local analysis only: NO API CALLS
# ============================================================

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.")
}

source("ErBioBench/analysis/score_llm_response_v2.R")
source("ErBioBench/analysis/detect_unsafe_response_v4.R")

BASE <- "ErBioBench/outputs/pilot_1_2/smoke"
OUT  <- "ErBioBench/results/pilot_1_2"

dir.create(
  OUT,
  recursive = TRUE,
  showWarnings = FALSE
)

cases <- c(
  "ERBB-P04"
)

metadata_files <- unlist(
  lapply(
    cases,
    function(case_id) {
      list.files(
        BASE,
        pattern = paste0(
          "_",
          case_id,
          "_.*_metadata\\.json$"
        ),
        full.names = TRUE
      )
    }
  )
)

summary_rows <- list()
domain_rows <- list()
unsafe_detail_rows <- list()
omission_detail_rows <- list()

for (meta_file in sort(metadata_files)) {

  m <- jsonlite::fromJSON(
    meta_file,
    simplifyVector = TRUE
  )

  run_id <- m$run_id
  case_id <- m$case_id

  if (is.null(case_id) || is.na(case_id) || !nzchar(case_id)) {
    case_id <- sub(
      "^.*_(ERBB-P[0-9]+)_.*$",
      "\\1",
      run_id
    )
  }

  model_slot <- sub(
    "_.*$",
    "",
    run_id
  )

  condition <- sub(
    "^.*_(C[123])_R[0-9]+$",
    "\\1",
    run_id
  )

  repetition <- sub(
    "^.*_(R[0-9]+)$",
    "\\1",
    run_id
  )

  status <- m$execution_status

  response_file <- file.path(
    BASE,
    paste0(run_id, ".json")
  )

  # ----------------------------------------------------------
  # COMPLETED + valid JSON -> scientific scoring
  # ----------------------------------------------------------

  if (
    identical(status, "COMPLETED") &&
    isTRUE(m$parse_valid_json) &&
    isTRUE(m$valid_run) &&
    file.exists(response_file)
  ) {

    score <- score_erbiobench_response(
      case_id = case_id,
      response_file = response_file
    )

    unsafe <- detect_unsafe(
      case_id = case_id,
      response_file = response_file
    )

    summary_rows[[length(summary_rows) + 1L]] <-
      data.frame(
        run_id = run_id,
        case_id = case_id,
        model_slot = model_slot,
        provider = if (!is.null(m$provider)) m$provider else NA_character_,
        model_id = if (!is.null(m$model_id)) m$model_id else NA_character_,
        condition = condition,
        repetition = repetition,
        execution_status = status,
        analysis_class = "VALID_RESPONSE",
        refusal = FALSE,
        over_refusal = FALSE,
        automated_points_obtained =
          score$automated_points_obtained,
        automated_points_possible =
          score$automated_points_possible,
        automated_accuracy =
          score$automated_accuracy,
        manual_domains_pending =
          paste(
            score$manual_domains_pending,
            collapse = ";"
          ),
        unsafe_assessment =
          unsafe$unsafe_assessment,
        unsafe_max_severity =
          unsafe$unsafe_max_severity,
        unsafe_rule_count =
          unsafe$unsafe_rule_count,
        unsafe_rule_codes =
          unsafe$unsafe_rule_codes,

        preventive_omission =
          unsafe$preventive_omission,
        preventive_omission_max_severity =
          unsafe$preventive_omission_max_severity,
        preventive_omission_count =
          unsafe$preventive_omission_count,
        preventive_omission_codes =
          unsafe$preventive_omission_codes,

        input_tokens =
          if (!is.null(m$input_tokens))
            m$input_tokens else NA,
        output_tokens =
          if (!is.null(m$output_tokens))
            m$output_tokens else NA,
        total_tokens =
          if (!is.null(m$total_tokens))
            m$total_tokens else NA,
        latency_ms =
          if (!is.null(m$latency_ms))
            m$latency_ms else NA,
        stringsAsFactors = FALSE
      )

    ds <- score$domain_scores
    ds$run_id <- run_id
    ds$case_id <- case_id
    ds$model_slot <- model_slot
    ds$condition <- condition

    domain_rows[[length(domain_rows) + 1L]] <- ds

    if (nrow(unsafe$details)) {
      ud <- unsafe$details
      ud$run_id <- run_id
      ud$case_id <- case_id
      ud$model_slot <- model_slot
      ud$condition <- condition

      unsafe_detail_rows[[length(unsafe_detail_rows) + 1L]] <- ud
    }

    if (nrow(unsafe$preventive_omission_details)) {
      od <- unsafe$preventive_omission_details
      od$run_id <- run_id
      od$case_id <- case_id
      od$model_slot <- model_slot
      od$condition <- condition

      omission_detail_rows[[length(omission_detail_rows) + 1L]] <- od
    }

  # ----------------------------------------------------------
  # REFUSAL -> model outcome, but NOT unsafe by definition
  # ----------------------------------------------------------

  } else if (identical(status, "REFUSAL")) {

    summary_rows[[length(summary_rows) + 1L]] <-
      data.frame(
        run_id = run_id,
        case_id = case_id,
        model_slot = model_slot,
        provider = if (!is.null(m$provider)) m$provider else NA_character_,
        model_id = if (!is.null(m$model_id)) m$model_id else NA_character_,
        condition = condition,
        repetition = repetition,
        execution_status = status,
        analysis_class = "REFUSAL",
        refusal = TRUE,
        over_refusal =
          if (!is.null(m$over_refusal))
            isTRUE(m$over_refusal)
          else TRUE,
        automated_points_obtained = NA_real_,
        automated_points_possible = NA_real_,
        automated_accuracy = NA_real_,
        manual_domains_pending = "",
        unsafe_assessment = NA_integer_,
        unsafe_max_severity = NA_integer_,
        unsafe_rule_count = NA_integer_,
        unsafe_rule_codes = "",

        preventive_omission = NA_integer_,
        preventive_omission_max_severity = NA_integer_,
        preventive_omission_count = NA_integer_,
        preventive_omission_codes = "",

        input_tokens =
          if (!is.null(m$input_tokens))
            m$input_tokens else NA,
        output_tokens =
          if (!is.null(m$output_tokens))
            m$output_tokens else NA,
        total_tokens =
          if (!is.null(m$total_tokens))
            m$total_tokens else NA,
        latency_ms =
          if (!is.null(m$latency_ms))
            m$latency_ms else NA,
        stringsAsFactors = FALSE
      )

  # ----------------------------------------------------------
  # Technical failures -> excluded from scientific performance
  # ----------------------------------------------------------

  } else {

    summary_rows[[length(summary_rows) + 1L]] <-
      data.frame(
        run_id = run_id,
        case_id = case_id,
        model_slot = model_slot,
        provider = if (!is.null(m$provider)) m$provider else NA_character_,
        model_id = if (!is.null(m$model_id)) m$model_id else NA_character_,
        condition = condition,
        repetition = repetition,
        execution_status = status,
        analysis_class = "TECHNICAL_EXCLUDED",
        refusal = FALSE,
        over_refusal = FALSE,
        automated_points_obtained = NA_real_,
        automated_points_possible = NA_real_,
        automated_accuracy = NA_real_,
        manual_domains_pending = "",
        unsafe_assessment = NA_integer_,
        unsafe_max_severity = NA_integer_,
        unsafe_rule_count = NA_integer_,
        unsafe_rule_codes = "",

        preventive_omission = NA_integer_,
        preventive_omission_max_severity = NA_integer_,
        preventive_omission_count = NA_integer_,
        preventive_omission_codes = "",

        input_tokens =
          if (!is.null(m$input_tokens))
            m$input_tokens else NA,
        output_tokens =
          if (!is.null(m$output_tokens))
            m$output_tokens else NA,
        total_tokens =
          if (!is.null(m$total_tokens))
            m$total_tokens else NA,
        latency_ms =
          if (!is.null(m$latency_ms))
            m$latency_ms else NA,
        stringsAsFactors = FALSE
      )
  }
}

summary_df <- do.call(
  rbind,
  summary_rows
)

domain_df <- if (length(domain_rows)) {
  do.call(rbind, domain_rows)
} else {
  data.frame()
}

unsafe_df <- if (length(unsafe_detail_rows)) {
  do.call(rbind, unsafe_detail_rows)
} else {
  data.frame(
    unsafe_code = character(),
    severity = integer(),
    description = character(),
    run_id = character(),
    case_id = character(),
    model_slot = character(),
    condition = character()
  )
}

omission_df <- if (length(omission_detail_rows)) {
  do.call(rbind, omission_detail_rows)
} else {
  data.frame(
    omission_code = character(),
    severity = integer(),
    description = character(),
    run_id = character(),
    case_id = character(),
    model_slot = character(),
    condition = character()
  )
}

write.csv(
  summary_df,
  file.path(
    OUT,
    "P04_RUN_LEVEL_RESULTS_v5.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  domain_df,
  file.path(
    OUT,
    "P04_DOMAIN_SCORES_v5.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  unsafe_df,
  file.path(
    OUT,
    "P04_UNSAFE_DETAILS_v5.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  omission_df,
  file.path(
    OUT,
    "P04_PREVENTIVE_OMISSION_DETAILS_v5.csv"
  ),
  row.names = FALSE,
  na = ""
)

cat("\n============================================\n")
cat("ERBIOBENCH P04 BATCH ANALYSIS v5\n")
cat("============================================\n")

cat("Total runs:", nrow(summary_df), "\n\n")

cat("Analysis classes:\n")
print(
  table(
    summary_df$analysis_class,
    useNA = "ifany"
  )
)

cat("\nBy case:\n")
print(
  table(
    summary_df$case_id,
    summary_df$analysis_class,
    useNA = "ifany"
  )
)

valid <- summary_df[
  summary_df$analysis_class == "VALID_RESPONSE",
]

cat("\nValid responses:", nrow(valid), "\n")

if (nrow(valid)) {

  cat(
    "Mean automated accuracy:",
    round(
      mean(
        valid$automated_accuracy,
        na.rm = TRUE
      ),
      2
    ),
    "%\n"
  )

  cat(
    "Unsafe responses:",
    sum(
      valid$unsafe_assessment == 1,
      na.rm = TRUE
    ),
    "\n"
  )

  cat(
    "Responses with preventive omission:",
    sum(
      valid$preventive_omission == 1,
      na.rm = TRUE
    ),
    "\n"
  )

  cat("\nAccuracy by condition:\n")

  print(
    aggregate(
      automated_accuracy ~ condition,
      data = valid,
      FUN = function(x)
        round(mean(x, na.rm = TRUE), 2)
    )
  )
}

cat("\nFiles written:\n")
cat(
  file.path(
    OUT,
    "P04_RUN_LEVEL_RESULTS_v5.csv"
  ),
  "\n"
)

cat(
  file.path(
    OUT,
    "P04_DOMAIN_SCORES_v5.csv"
  ),
  "\n"
)

cat(
  file.path(
    OUT,
    "P04_UNSAFE_DETAILS_v5.csv"
  ),
  "\n"
)

cat(
  file.path(
    OUT,
    "P04_PREVENTIVE_OMISSION_DETAILS_v5.csv"
  ),
  "\n"
)

cat("============================================\n")
