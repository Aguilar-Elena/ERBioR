suppressPackageStartupMessages(
  library(jsonlite)
)

source("ErBioBench/analysis/score_llm_response_v3.R")
source("ErBioBench/analysis/detect_unsafe_response_v4.R")

MANIFEST <- "ErBioBench/protocol/pilot_run_manifest_smoke_v2.csv"
OUT <- "ErBioBench/results/pilot_1_2"

dir.create(
  OUT,
  recursive = TRUE,
  showWarnings = FALSE
)

manifest <- read.csv(
  MANIFEST,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

run_rows <- list()
domain_rows <- list()
unsafe_rows <- list()
omission_rows <- list()
scoring_error_rows <- list()

safe_chr <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_character_)
  }
  as.character(x[[1]])
}

safe_num <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(NA_real_)
  }
  suppressWarnings(as.numeric(x[[1]]))
}

safe_bool <- function(x) {
  if (is.null(x) || length(x) == 0L) {
    return(FALSE)
  }
  isTRUE(x[[1]])
}

technical_subtype <- function(status, finish, error) {

  status <- ifelse(is.na(status), "", status)
  finish <- ifelse(is.na(finish), "", finish)
  error  <- ifelse(is.na(error), "", error)

  if (identical(status, "QUOTA_EXHAUSTED")) {
    return("QUOTA_EXHAUSTED")
  }

  if (
    identical(status, "API_FAILURE") &&
    (
      grepl("429", error, fixed = TRUE) ||
      grepl("rate limit", error, ignore.case = TRUE)
    )
  ) {
    return("RATE_LIMIT_429")
  }

  if (identical(status, "INVALID_JSON")) {

    if (
      finish %in%
        c("length", "max_tokens", "incomplete")
    ) {
      return("OUTPUT_TRUNCATION")
    }

    return("INVALID_JSON_OTHER")
  }

  if (identical(status, "API_FAILURE")) {
    return("API_FAILURE_OTHER")
  }

  paste0(
    status,
    if (nzchar(finish))
      paste0("_", finish)
    else
      ""
  )
}


for (i in seq_len(nrow(manifest))) {

  mrow <- manifest[i, ]

  run_id <- mrow$run_id
  case_id <- mrow$case_id
  cond <- mrow$condition_id
  model_slot <- mrow$model_slot
  provider <- mrow$provider
  model_id <- mrow$model_id

  meta_file <- mrow$metadata_file
  response_file <- mrow$structured_output_file

  category <- "NEVER_ATTEMPTED"

  status <- NA_character_
  valid_json <- FALSE
  finish_reason <- NA_character_
  error_message <- NA_character_

  input_tokens <- NA_real_
  output_tokens <- NA_real_
  total_tokens <- NA_real_
  latency_ms <- NA_real_

  refusal <- 0L
  over_refusal <- 0L
  technical_type <- NA_character_

  automated_points_obtained <- NA_real_
  automated_points_possible <- NA_real_
  automated_accuracy <- NA_real_
  unsafe_assessment <- NA_integer_
  preventive_omission <- NA_integer_

  scoring_status <- "NOT_SCORED"
  scoring_error <- NA_character_

  if (file.exists(meta_file)) {

    meta <- fromJSON(
      meta_file,
      simplifyVector = TRUE
    )

    status <- safe_chr(
      meta$execution_status
    )

    valid_json <- safe_bool(
      meta$parse_valid_json
    )

    finish_reason <- safe_chr(
      meta$finish_reason
    )

    error_message <- safe_chr(
      meta$error
    )

    input_tokens <- safe_num(
      meta$input_tokens
    )

    output_tokens <- safe_num(
      meta$output_tokens
    )

    total_tokens <- safe_num(
      meta$total_tokens
    )

    latency_ms <- safe_num(
      meta$latency_ms
    )

    if (
      identical(status, "COMPLETED") &&
      valid_json &&
      file.exists(response_file)
    ) {

      category <- "SCIENTIFIC_VALID"

      score_result <- tryCatch(
        {

          s <- score_erbiobench_response(
            case_id = case_id,
            response_file = response_file
          )

          u <- detect_unsafe(
            case_id = case_id,
            response_file = response_file
          )

          list(
            ok = TRUE,
            score = s,
            unsafe = u
          )
        },
        error = function(e) {

          list(
            ok = FALSE,
            error = conditionMessage(e)
          )
        }
      )

      if (isTRUE(score_result$ok)) {

        s <- score_result$score
        u <- score_result$unsafe

        scoring_status <- "SCORED"

        automated_points_obtained <-
          s$automated_points_obtained

        automated_points_possible <-
          s$automated_points_possible

        automated_accuracy <-
          s$automated_accuracy

        unsafe_assessment <-
          u$unsafe_assessment

        preventive_omission <-
          u$preventive_omission

        d <- s$domain_scores

        d$run_id <- run_id
        d$model_slot <- model_slot
        d$provider <- provider
        d$model_id <- model_id
        d$case_id <- case_id
        d$condition <- cond

        domain_rows[[length(domain_rows) + 1L]] <- d

        if (
          !is.null(u$details) &&
          is.data.frame(u$details) &&
          nrow(u$details) > 0L
        ) {

          z <- u$details

          z$run_id <- run_id
          z$model_slot <- model_slot
          z$provider <- provider
          z$model_id <- model_id
          z$case_id <- case_id
          z$condition <- cond

          unsafe_rows[[length(unsafe_rows) + 1L]] <- z
        }

        if (
          !is.null(
            u$preventive_omission_details
          ) &&
          is.data.frame(
            u$preventive_omission_details
          ) &&
          nrow(
            u$preventive_omission_details
          ) > 0L
        ) {

          z <-
            u$preventive_omission_details

          z$run_id <- run_id
          z$model_slot <- model_slot
          z$provider <- provider
          z$model_id <- model_id
          z$case_id <- case_id
          z$condition <- cond

          omission_rows[[length(omission_rows) + 1L]] <- z
        }

      } else {

        scoring_status <- "SCORING_ERROR"
        scoring_error <- score_result$error

        scoring_error_rows[[length(scoring_error_rows) + 1L]] <- data.frame(
          run_id = run_id,
          model_slot = model_slot,
          provider = provider,
          model_id = model_id,
          case_id = case_id,
          condition = cond,
          error = scoring_error,
          stringsAsFactors = FALSE
        )
      }

    } else if (
      identical(status, "REFUSAL")
    ) {

      category <- "REFUSAL"
      refusal <- 1L

      if (
        !is.null(meta$over_refusal)
      ) {
        over_refusal <-
          as.integer(
            isTRUE(meta$over_refusal)
          )
      }

    } else {

      category <- "TECHNICAL_FAILURE"

      technical_type <-
        technical_subtype(
          status,
          finish_reason,
          error_message
        )
    }
  }

  row <- data.frame(
    run_id = run_id,
    model_slot = model_slot,
    provider = provider,
    model_id = model_id,
    case_id = case_id,
    condition = cond,
    repetition = mrow$repetition,
    analysis_category = category,
    execution_status = status,
    parse_valid_json = valid_json,
    finish_reason = finish_reason,
    technical_subtype = technical_type,
    input_tokens = input_tokens,
    output_tokens = output_tokens,
    total_tokens = total_tokens,
    latency_ms = latency_ms,
    refusal = refusal,
    over_refusal = over_refusal,
    scoring_status = scoring_status,
    scoring_error = scoring_error,
    automated_points_obtained =
      automated_points_obtained,
    automated_points_possible =
      automated_points_possible,
    automated_accuracy =
      automated_accuracy,
    unsafe_assessment =
      unsafe_assessment,
    preventive_omission =
      preventive_omission,
    stringsAsFactors = FALSE
  )

  run_rows[[length(run_rows) + 1L]] <- row
}


runs <- do.call(
  rbind,
  run_rows
)

stopifnot(
  nrow(runs) == 540L
)


write.csv(
  runs,
  file.path(
    OUT,
    "SMOKE_ALL_RUN_LEVEL_RESULTS_v1.csv"
  ),
  row.names = FALSE,
  na = ""
)


if (length(domain_rows)) {

  domains <- do.call(
    rbind,
    domain_rows
  )

  write.csv(
    domains,
    file.path(
      OUT,
      "SMOKE_ALL_DOMAIN_SCORES_v1.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}


if (length(unsafe_rows)) {

  unsafe <- do.call(
    rbind,
    unsafe_rows
  )

  write.csv(
    unsafe,
    file.path(
      OUT,
      "SMOKE_ALL_UNSAFE_DETAILS_v1.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}


if (length(omission_rows)) {

  omissions <- do.call(
    rbind,
    omission_rows
  )

  write.csv(
    omissions,
    file.path(
      OUT,
      "SMOKE_ALL_PREVENTIVE_OMISSION_DETAILS_v1.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}


if (length(scoring_error_rows)) {

  scoring_errors <- do.call(
    rbind,
    scoring_error_rows
  )

  write.csv(
    scoring_errors,
    file.path(
      OUT,
      "SMOKE_ALL_SCORING_ERRORS_v1.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}


valid <- runs[
  runs$analysis_category ==
    "SCIENTIFIC_VALID" &
  runs$scoring_status ==
    "SCORED",
]


summarise_group <- function(df, vars) {

  key <- interaction(
    df[, vars, drop = FALSE],
    drop = TRUE,
    lex.order = TRUE
  )

  spl <- split(
    df,
    key
  )

  out <- lapply(
    spl,
    function(z) {

      first <- z[1, vars, drop = FALSE]

      cbind(
        first,
        data.frame(
          n_valid = nrow(z),
          mean_accuracy =
            mean(
              z$automated_accuracy,
              na.rm = TRUE
            ),
          sd_accuracy =
            if (nrow(z) > 1L)
              sd(
                z$automated_accuracy,
                na.rm = TRUE
              )
            else
              NA_real_,
          median_accuracy =
            median(
              z$automated_accuracy,
              na.rm = TRUE
            ),
          unsafe_n =
            sum(
              z$unsafe_assessment,
              na.rm = TRUE
            ),
          unsafe_pct =
            100 *
            mean(
              z$unsafe_assessment,
              na.rm = TRUE
            ),
          preventive_omission_n =
            sum(
              z$preventive_omission,
              na.rm = TRUE
            ),
          preventive_omission_pct =
            100 *
            mean(
              z$preventive_omission,
              na.rm = TRUE
            ),
          stringsAsFactors = FALSE
        )
      )
    }
  )

  do.call(
    rbind,
    out
  )
}


summary_condition <-
  summarise_group(
    valid,
    c("condition")
  )

summary_model_condition <-
  summarise_group(
    valid,
    c(
      "model_slot",
      "condition"
    )
  )

summary_provider_condition <-
  summarise_group(
    valid,
    c(
      "provider",
      "condition"
    )
  )

summary_case_condition <-
  summarise_group(
    valid,
    c(
      "case_id",
      "condition"
    )
  )


write.csv(
  summary_condition,
  file.path(
    OUT,
    "SMOKE_SUMMARY_BY_CONDITION_v1.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  summary_model_condition,
  file.path(
    OUT,
    "SMOKE_SUMMARY_BY_MODEL_CONDITION_v1.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  summary_provider_condition,
  file.path(
    OUT,
    "SMOKE_SUMMARY_BY_PROVIDER_CONDITION_v1.csv"
  ),
  row.names = FALSE,
  na = ""
)

write.csv(
  summary_case_condition,
  file.path(
    OUT,
    "SMOKE_SUMMARY_BY_CASE_CONDITION_v1.csv"
  ),
  row.names = FALSE,
  na = ""
)


# ------------------------------------------------------------
# COMPLETE-CASE PAIRED SUBSET
# Same model + same case must have valid C1, C2 and C3.
# ------------------------------------------------------------

pair_key <- paste(
  valid$model_slot,
  valid$case_id,
  sep = "|"
)

pair_split <- split(
  valid,
  pair_key
)

complete_pairs <- pair_split[
  vapply(
    pair_split,
    function(z) {
      all(
        c("C1", "C2", "C3") %in%
          z$condition
      )
    },
    logical(1)
  )
]

paired <- if (
  length(complete_pairs)
) {
  do.call(
    rbind,
    complete_pairs
  )
} else {
  valid[0, ]
}

write.csv(
  paired,
  file.path(
    OUT,
    "SMOKE_COMPLETE_CASE_PAIRED_RUNS_v1.csv"
  ),
  row.names = FALSE,
  na = ""
)

if (nrow(paired) > 0L) {

  paired_summary <-
    summarise_group(
      paired,
      c("condition")
    )

  write.csv(
    paired_summary,
    file.path(
      OUT,
      "SMOKE_COMPLETE_CASE_PAIRED_SUMMARY_v1.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}


# ------------------------------------------------------------
# CONSOLE REPORT
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("ERBIOBENCH GLOBAL SMOKE SCORING\n")
cat("========================================\n")

cat(
  "Nominal runs:",
  nrow(runs),
  "\n"
)

cat(
  "Scientific valid:",
  sum(
    runs$analysis_category ==
      "SCIENTIFIC_VALID"
  ),
  "\n"
)

cat(
  "Successfully scored:",
  nrow(valid),
  "\n"
)

cat(
  "Refusals:",
  sum(
    runs$analysis_category ==
      "REFUSAL"
  ),
  "\n"
)

cat(
  "Technical failures:",
  sum(
    runs$analysis_category ==
      "TECHNICAL_FAILURE"
  ),
  "\n"
)

cat(
  "Never attempted:",
  sum(
    runs$analysis_category ==
      "NEVER_ATTEMPTED"
  ),
  "\n"
)

cat(
  "Scoring errors:",
  sum(
    runs$scoring_status ==
      "SCORING_ERROR"
  ),
  "\n"
)


cat("\n")
cat("========================================\n")
cat("VALID RESPONSES BY CONDITION\n")
cat("========================================\n")

print(
  summary_condition,
  row.names = FALSE
)


cat("\n")
cat("========================================\n")
cat("COMPLETE-CASE PAIRED SUBSET\n")
cat("========================================\n")

cat(
  "Model-case pairs with C1+C2+C3:",
  length(complete_pairs),
  "\n"
)

cat(
  "Paired runs:",
  nrow(paired),
  "\n"
)

if (nrow(paired) > 0L) {
  print(
    paired_summary,
    row.names = FALSE
  )
}


cat("\n")
cat("========================================\n")
cat("RUN STATUS BY MODEL\n")
cat("========================================\n")

for (
  model in sort(
    unique(runs$model_slot)
  )
) {

  z <- runs[
    runs$model_slot == model,
  ]

  cat(
    model,
    "| valid =",
    sum(
      z$analysis_category ==
        "SCIENTIFIC_VALID"
    ),
    "| refusal =",
    sum(
      z$analysis_category ==
        "REFUSAL"
    ),
    "| technical =",
    sum(
      z$analysis_category ==
        "TECHNICAL_FAILURE"
    ),
    "| never =",
    sum(
      z$analysis_category ==
        "NEVER_ATTEMPTED"
    ),
    "\n"
  )
}


cat("\n")
cat("========================================\n")
cat("TECHNICAL SUBTYPES\n")
cat("========================================\n")

print(
  table(
    runs$technical_subtype,
    useNA = "no"
  )
)


cat("\nDONE\n")
