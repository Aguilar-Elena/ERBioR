suppressPackageStartupMessages(
  library(jsonlite)
)

source("ErBioBench/analysis/score_llm_response_v2.R")
source("ErBioBench/analysis/detect_unsafe_response_v4.R")

BASE <- "ErBioBench/outputs/pilot_1_2/smoke"
OUT  <- "ErBioBench/results/pilot_1_2"

dir.create(
  OUT,
  recursive = TRUE,
  showWarnings = FALSE
)

cases <- sprintf("ERBB-P%02d", 7:15)
conds <- c("C1", "C2", "C3")

run_rows <- list()
domain_rows <- list()
unsafe_rows <- list()
omission_rows <- list()

for (case_id in cases) {

  for (cond in conds) {

    run_id <- paste(
      "M3",
      case_id,
      cond,
      "R01",
      sep = "_"
    )

    meta_file <- file.path(
      BASE,
      paste0(run_id, "_metadata.json")
    )

    response_file <- file.path(
      BASE,
      paste0(run_id, ".json")
    )

    if (!file.exists(meta_file)) {
      next
    }

    meta <- fromJSON(
      meta_file,
      simplifyVector = TRUE
    )

    status <- as.character(
      meta$execution_status
    )

    valid_json <- isTRUE(
      meta$parse_valid_json
    )

    row <- data.frame(
      run_id = run_id,
      case_id = case_id,
      condition = cond,
      execution_status = status,
      parse_valid_json = valid_json,
      finish_reason =
        if (!is.null(meta$finish_reason))
          as.character(meta$finish_reason)
        else NA_character_,
      input_tokens =
        if (!is.null(meta$input_tokens))
          as.numeric(meta$input_tokens)
        else NA_real_,
      output_tokens =
        if (!is.null(meta$output_tokens))
          as.numeric(meta$output_tokens)
        else NA_real_,
      total_tokens =
        if (!is.null(meta$total_tokens))
          as.numeric(meta$total_tokens)
        else NA_real_,
      automated_points_obtained = NA_real_,
      automated_points_possible = NA_real_,
      automated_accuracy = NA_real_,
      unsafe_assessment = NA_integer_,
      preventive_omission = NA_integer_,
      stringsAsFactors = FALSE
    )

    if (
      identical(status, "COMPLETED") &&
      valid_json &&
      file.exists(response_file)
    ) {

      s <- score_erbiobench_response(
        case_id = case_id,
        response_file = response_file
      )

      u <- detect_unsafe(
        case_id = case_id,
        response_file = response_file
      )

      row$automated_points_obtained <-
        s$automated_points_obtained

      row$automated_points_possible <-
        s$automated_points_possible

      row$automated_accuracy <-
        s$automated_accuracy

      row$unsafe_assessment <-
        u$unsafe_assessment

      row$preventive_omission <-
        u$preventive_omission

      d <- s$domain_scores
      d$run_id <- run_id
      d$case_id <- case_id
      d$condition <- cond

      domain_rows[[length(domain_rows) + 1L]] <- d

      if (
        !is.null(u$details) &&
        nrow(u$details) > 0L
      ) {
        z <- u$details
        z$run_id <- run_id
        z$case_id <- case_id
        z$condition <- cond

        unsafe_rows[[length(unsafe_rows) + 1L]] <- z
      }

      if (
        !is.null(u$preventive_omission_details) &&
        nrow(u$preventive_omission_details) > 0L
      ) {
        z <- u$preventive_omission_details
        z$run_id <- run_id
        z$case_id <- case_id
        z$condition <- cond

        omission_rows[[length(omission_rows) + 1L]] <- z
      }
    }

    run_rows[[length(run_rows) + 1L]] <- row
  }
}

runs <- do.call(
  rbind,
  run_rows
)

write.csv(
  runs,
  file.path(
    OUT,
    "M3_P07_P15_RUN_LEVEL_RESULTS_v1.csv"
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
      "M3_P07_P15_DOMAIN_SCORES_v1.csv"
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
      "M3_P07_P15_UNSAFE_DETAILS_v1.csv"
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
      "M3_P07_P15_PREVENTIVE_OMISSION_DETAILS_v1.csv"
    ),
    row.names = FALSE,
    na = ""
  )
}

cat("\n========================================\n")
cat("M3 P07-P15 AUTOMATED SCORING\n")
cat("========================================\n")

cat(
  "Runs:",
  nrow(runs),
  "\n"
)

cat(
  "Valid:",
  sum(
    runs$execution_status == "COMPLETED" &
    runs$parse_valid_json
  ),
  "\n"
)

cat(
  "Technical excluded:",
  sum(
    !(
      runs$execution_status == "COMPLETED" &
      runs$parse_valid_json
    )
  ),
  "\n\n"
)

valid <- runs[
  runs$execution_status == "COMPLETED" &
  runs$parse_valid_json,
]

print(
  valid[
    ,
    c(
      "case_id",
      "condition",
      "automated_accuracy",
      "unsafe_assessment",
      "preventive_omission"
    )
  ],
  row.names = FALSE
)

cat("\nOVERALL VALID RESPONSES\n")

cat(
  "Mean accuracy:",
  round(
    mean(
      valid$automated_accuracy,
      na.rm = TRUE
    ),
    3
  ),
  "\n"
)

cat(
  "Unsafe:",
  sum(
    valid$unsafe_assessment,
    na.rm = TRUE
  ),
  "/",
  nrow(valid),
  "\n"
)

cat(
  "Preventive omissions:",
  sum(
    valid$preventive_omission,
    na.rm = TRUE
  ),
  "/",
  nrow(valid),
  "\n"
)

cat("\nBY CONDITION\n")

for (cc in conds) {

  z <- valid[
    valid$condition == cc,
  ]

  cat(
    cc,
    "| n =",
    nrow(z),
    "| mean =",
    if (nrow(z))
      round(
        mean(
          z$automated_accuracy,
          na.rm = TRUE
        ),
        3
      )
    else NA,
    "| unsafe =",
    sum(
      z$unsafe_assessment,
      na.rm = TRUE
    ),
    "| omissions =",
    sum(
      z$preventive_omission,
      na.rm = TRUE
    ),
    "\n"
  )
}

cat("\nDONE\n")
