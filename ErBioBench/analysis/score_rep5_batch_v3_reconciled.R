options(stringsAsFactors = FALSE)

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.")
}

SCORER <- "ErBioBench/analysis/score_llm_response_v3.R"
PROTOCOL_DIR <- "ErBioBench/protocol"
SMOKE_DIR <- "ErBioBench/outputs/pilot_1_2/smoke"
REP5_DIR <- "ErBioBench/outputs/pilot_1_2/rep5"
OUT_DIR <- "ErBioBench/results/pilot_1_2/rep5_scoring_v3"

dir.create(
  OUT_DIR,
  recursive = TRUE,
  showWarnings = FALSE
)

source(SCORER)

# ------------------------------------------------------------
# 1. FIND THE FROZEN 2700-RUN MANIFEST
# ------------------------------------------------------------

manifest_candidates <- list.files(
  PROTOCOL_DIR,
  pattern = "\\.csv$",
  recursive = TRUE,
  full.names = TRUE
)

valid_manifest <- list()

for (f in manifest_candidates) {

  x <- tryCatch(
    read.csv(
      f,
      check.names = FALSE,
      stringsAsFactors = FALSE
    ),
    error = function(e) NULL
  )

  if (is.null(x)) next
  if (!"run_id" %in% names(x)) next

  ids <- as.character(x$run_id)

  expected_pattern <-
    "^(O[123]|A[123]|G[123]|M[123])_ERBB-P(0[1-9]|1[0-5])_C[123]_R0[1-5]$"

  ok_pattern <- grepl(expected_pattern, ids)

  if (
    nrow(x) == 2700L &&
    length(unique(ids)) == 2700L &&
    all(ok_pattern)
  ) {
    valid_manifest[[f]] <- x
  }
}

if (length(valid_manifest) != 1L) {

  cat("\nCandidate 2700-run manifests found:\n")

  if (length(valid_manifest)) {
    cat(
      paste0(" - ", names(valid_manifest)),
      sep = "\n"
    )
  } else {
    cat(" NONE\n")
  }

  stop(
    "Expected exactly one unambiguous 2700-run REP5 manifest."
  )
}

manifest_file <- names(valid_manifest)[1]
manifest <- valid_manifest[[1]]

cat("\nREP5 manifest detected:\n")
cat(manifest_file, "\n")
cat("Rows:", nrow(manifest), "\n")
cat("Unique run_ids:", length(unique(manifest$run_id)), "\n")

# ------------------------------------------------------------
# 2. HELPERS
# ------------------------------------------------------------

safe_value <- function(x, name, default = NA) {

  if (!name %in% names(x)) {
    return(default)
  }

  z <- x[[name]]

  if (is.null(z) || length(z) == 0L) {
    return(default)
  }

  z[[1]]
}

as_bool <- function(x) {

  if (length(x) == 0L || is.na(x)) {
    return(NA)
  }

  if (is.logical(x)) return(x)

  z <- tolower(trimws(as.character(x)))

  if (z %in% c("true", "1")) return(TRUE)
  if (z %in% c("false", "0")) return(FALSE)

  NA
}

parse_run_id <- function(run_id) {

  m <- regexec(
    "^([A-Z][0-9])_(ERBB-P[0-9]{2})_(C[123])_R([0-9]{2})$",
    run_id
  )

  z <- regmatches(run_id, m)[[1]]

  if (length(z) != 5L) {
    stop("Cannot parse run_id: ", run_id)
  }

  list(
    model_slot = z[2],
    case_id = z[3],
    condition_id = z[4],
    repetition = as.integer(sub("^0", "", z[5]))
  )
}

metadata_path_for <- function(run_id, repetition) {

  if (repetition == 1L) {

    p_recovery <- file.path(
      "ErBioBench/outputs/pilot_1_2/rep5_recovery_attempts/technical_r01_v1",
      paste0(run_id, "_metadata.json")
    )

    p_rep5 <- file.path(
      REP5_DIR,
      paste0(run_id, "_metadata.json")
    )

    p_smoke <- file.path(
      SMOKE_DIR,
      paste0(run_id, "_metadata.json")
    )

    if (file.exists(p_recovery)) return(p_recovery)
    if (file.exists(p_rep5)) return(p_rep5)
    if (file.exists(p_smoke)) return(p_smoke)

    return(p_smoke)
  }

  file.path(
    REP5_DIR,
    paste0(run_id, "_metadata.json")
  )
}

response_path_for <- function(run_id, repetition) {

  if (repetition == 1L) {

    p_recovery <- file.path(
      "ErBioBench/outputs/pilot_1_2/rep5_recovery_attempts/technical_r01_v1",
      paste0(run_id, ".json")
    )

    p_rep5 <- file.path(
      REP5_DIR,
      paste0(run_id, ".json")
    )

    p_smoke <- file.path(
      SMOKE_DIR,
      paste0(run_id, ".json")
    )

    if (file.exists(p_recovery)) return(p_recovery)
    if (file.exists(p_rep5)) return(p_rep5)
    if (file.exists(p_smoke)) return(p_smoke)

    return(p_smoke)
  }

  file.path(
    REP5_DIR,
    paste0(run_id, ".json")
  )
}

# ------------------------------------------------------------
# 3. SCORE ALL 2700 PLANNED RUNS
# ------------------------------------------------------------

master_rows <- vector(
  "list",
  nrow(manifest)
)

domain_rows <- list()
audit_rows <- list()

domain_index <- 0L
audit_index <- 0L

for (i in seq_len(nrow(manifest))) {

  run_id <- as.character(manifest$run_id[i])
  parsed <- parse_run_id(run_id)

  metadata_file <- metadata_path_for(
    run_id,
    parsed$repetition
  )

  response_file <- response_path_for(
    run_id,
    parsed$repetition
  )

  issues <- character(0)

  # Defaults
  execution_status <- NA_character_
  parse_valid_json <- NA
  valid_run <- NA
  refusal <- NA
  over_refusal <- NA
  provider <- NA_character_
  model_family <- NA_character_
  model_id <- NA_character_
  model_tier <- NA_character_
  finish_reason <- NA_character_
  latency_ms <- NA_real_
  input_tokens <- NA_real_
  output_tokens <- NA_real_
  total_tokens <- NA_real_

  if (!file.exists(metadata_file)) {

    issues <- c(
      issues,
      "MISSING_METADATA"
    )

  } else {

    meta <- tryCatch(
      jsonlite::fromJSON(
        metadata_file,
        simplifyVector = TRUE
      ),
      error = function(e) e
    )

    if (inherits(meta, "error")) {

      issues <- c(
        issues,
        paste0(
          "METADATA_PARSE_ERROR:",
          conditionMessage(meta)
        )
      )

    } else {

      meta_run_id <- as.character(
        safe_value(meta, "run_id", "")
      )

      if (!identical(meta_run_id, run_id)) {
        issues <- c(
          issues,
          "METADATA_RUN_ID_MISMATCH"
        )
      }

      meta_case <- as.character(
        safe_value(meta, "case_id", "")
      )

      if (!identical(
        meta_case,
        parsed$case_id
      )) {
        issues <- c(
          issues,
          "METADATA_CASE_MISMATCH"
        )
      }

      meta_condition <- as.character(
        safe_value(meta, "condition_id", "")
      )

      if (!identical(
        meta_condition,
        parsed$condition_id
      )) {
        issues <- c(
          issues,
          "METADATA_CONDITION_MISMATCH"
        )
      }

      meta_rep <- suppressWarnings(
        as.integer(
          safe_value(
            meta,
            "repetition",
            NA_integer_
          )
        )
      )

      if (
        is.na(meta_rep) ||
        meta_rep != parsed$repetition
      ) {
        issues <- c(
          issues,
          "METADATA_REPETITION_MISMATCH"
        )
      }

      execution_status <- as.character(
        safe_value(
          meta,
          "execution_status",
          NA_character_
        )
      )

      parse_valid_json <- as_bool(
        safe_value(
          meta,
          "parse_valid_json",
          NA
        )
      )

      valid_run <- as_bool(
        safe_value(
          meta,
          "valid_run",
          NA
        )
      )

      refusal <- as_bool(
        safe_value(
          meta,
          "refusal",
          NA
        )
      )

      over_refusal <- as_bool(
        safe_value(
          meta,
          "over_refusal",
          NA
        )
      )

      provider <- as.character(
        safe_value(
          meta,
          "provider",
          NA_character_
        )
      )

      model_family <- as.character(
        safe_value(
          meta,
          "model_family",
          NA_character_
        )
      )

      model_id <- as.character(
        safe_value(
          meta,
          "model_id",
          NA_character_
        )
      )

      model_tier <- as.character(
        safe_value(
          meta,
          "model_tier",
          NA_character_
        )
      )

      finish_reason <- as.character(
        safe_value(
          meta,
          "finish_reason",
          NA_character_
        )
      )

      latency_ms <- suppressWarnings(
        as.numeric(
          safe_value(
            meta,
            "latency_ms",
            NA_real_
          )
        )
      )

      input_tokens <- suppressWarnings(
        as.numeric(
          safe_value(
            meta,
            "input_tokens",
            NA_real_
          )
        )
      )

      output_tokens <- suppressWarnings(
        as.numeric(
          safe_value(
            meta,
            "output_tokens",
            NA_real_
          )
        )
      )

      total_tokens <- suppressWarnings(
        as.numeric(
          safe_value(
            meta,
            "total_tokens",
            NA_real_
          )
        )
      )
    }
  }

  scoring_eligible <-
    identical(
      execution_status,
      "COMPLETED"
    ) &&
    isTRUE(parse_valid_json) &&
    isTRUE(valid_run)

  automated_points_obtained <- NA_real_
  automated_points_possible <- NA_real_
  automated_accuracy <- NA_real_
  manual_domains_pending <- NA_character_
  scoring_error <- NA_character_

  if (scoring_eligible) {

    if (!file.exists(response_file)) {

      issues <- c(
        issues,
        "ELIGIBLE_BUT_JSON_MISSING"
      )

    } else {

      result <- tryCatch(
        score_erbiobench_response(
          case_id = parsed$case_id,
          response_file = response_file
        ),
        error = function(e) e
      )

      if (inherits(result, "error")) {

        scoring_error <-
          conditionMessage(result)

        issues <- c(
          issues,
          paste0(
            "SCORER_ERROR:",
            scoring_error
          )
        )

      } else {

        automated_points_obtained <-
          result$automated_points_obtained

        automated_points_possible <-
          result$automated_points_possible

        automated_accuracy <-
          result$automated_accuracy

        if (
          length(result$manual_domains_pending)
        ) {
          manual_domains_pending <-
            paste(
              result$manual_domains_pending,
              collapse = "|"
            )
        }

        d <- result$domain_scores

        d$run_id <- run_id
        d$model_slot <- parsed$model_slot
        d$case_id <- parsed$case_id
        d$condition_id <- parsed$condition_id
        d$repetition <- parsed$repetition

        d <- d[
          ,
          c(
            "run_id",
            "model_slot",
            "case_id",
            "condition_id",
            "repetition",
            "domain",
            "max_points",
            "applicable",
            "auto_scored",
            "correct",
            "obtained_points"
          )
        ]

        domain_index <- domain_index + 1L
        domain_rows[[domain_index]] <- d
      }
    }
  }

  # This is deliberately NOT an accuracy score.
  protocol_valid_response <-
    identical(
      execution_status,
      "COMPLETED"
    ) &&
    isTRUE(parse_valid_json) &&
    isTRUE(valid_run)

  row <- data.frame(
    run_id = run_id,
    model_slot = parsed$model_slot,
    case_id = parsed$case_id,
    condition_id = parsed$condition_id,
    repetition = parsed$repetition,
    execution_status = execution_status,
    parse_valid_json = parse_valid_json,
    valid_run = valid_run,
    refusal = refusal,
    over_refusal = over_refusal,
    protocol_valid_response =
      protocol_valid_response,
    provider = provider,
    model_family = model_family,
    model_id = model_id,
    model_tier = model_tier,
    finish_reason = finish_reason,
    latency_ms = latency_ms,
    input_tokens = input_tokens,
    output_tokens = output_tokens,
    total_tokens = total_tokens,
    metadata_file = metadata_file,
    response_file =
      if (file.exists(response_file))
        response_file
      else
        NA_character_,
    scoring_eligible = scoring_eligible,
    automated_points_obtained =
      automated_points_obtained,
    automated_points_possible =
      automated_points_possible,
    automated_accuracy =
      automated_accuracy,
    manual_domains_pending =
      manual_domains_pending,
    scoring_error = scoring_error,
    data_issue =
      if (length(issues))
        paste(
          unique(issues),
          collapse = "|"
        )
      else
        NA_character_,
    stringsAsFactors = FALSE
  )

  master_rows[[i]] <- row

  if (length(issues)) {

    audit_index <- audit_index + 1L

    audit_rows[[audit_index]] <-
      data.frame(
        run_id = run_id,
        issue = paste(
          unique(issues),
          collapse = "|"
        ),
        stringsAsFactors = FALSE
      )
  }

  if (
    i %% 250L == 0L ||
    i == nrow(manifest)
  ) {
    cat(
      sprintf(
        "Processed %d / %d\n",
        i,
        nrow(manifest)
      )
    )
  }
}

master <- do.call(
  rbind,
  master_rows
)

if (length(domain_rows)) {
  domains <- do.call(
    rbind,
    domain_rows
  )
} else {
  domains <- data.frame()
}

if (length(audit_rows)) {
  audit <- do.call(
    rbind,
    audit_rows
  )
} else {
  audit <- data.frame(
    run_id = character(0),
    issue = character(0)
  )
}

# ------------------------------------------------------------
# 4. HARD INTEGRITY CHECKS
# ------------------------------------------------------------

cat("\n============================================\n")
cat("REP5 V3 INTEGRITY CHECKS\n")
cat("============================================\n")

cat(
  "Master rows:",
  nrow(master),
  "\n"
)

cat(
  "Unique run_ids:",
  length(unique(master$run_id)),
  "\n"
)

cat(
  "Duplicated run_ids:",
  sum(duplicated(master$run_id)),
  "\n"
)

cat(
  "Missing metadata:",
  sum(
    grepl(
      "MISSING_METADATA",
      master$data_issue
    ),
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Scorer errors:",
  sum(
    !is.na(master$scoring_error)
  ),
  "\n"
)

cat(
  "Scoring eligible:",
  sum(
    master$scoring_eligible,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Scored with accuracy:",
  sum(
    !is.na(master$automated_accuracy)
  ),
  "\n"
)

cat(
  "Domain rows:",
  nrow(domains),
  "\n"
)

cat("\nExecution status counts:\n")
print(
  table(
    master$execution_status,
    useNA = "ifany"
  )
)

cat("\nBy model slot:\n")
print(
  table(
    master$model_slot,
    master$execution_status,
    useNA = "ifany"
  )
)

cat("\nBy condition:\n")
print(
  table(
    master$condition_id,
    master$execution_status,
    useNA = "ifany"
  )
)

# Expected design dimensions
stopifnot(
  nrow(master) == 2700L
)

stopifnot(
  length(unique(master$run_id)) == 2700L
)

stopifnot(
  sum(duplicated(master$run_id)) == 0L
)

stopifnot(
  all(
    table(master$model_slot) == 225L
  )
)

stopifnot(
  all(
    table(master$condition_id) == 900L
  )
)

stopifnot(
  all(
    table(master$repetition) == 540L
  )
)

stopifnot(
  all(
    table(master$case_id) == 180L
  )
)

# ------------------------------------------------------------
# 5. WRITE DEFINITIVE TABLES
# ------------------------------------------------------------

MASTER_OUT <- file.path(
  OUT_DIR,
  "REP5_MASTER_V3.csv"
)

DOMAIN_OUT <- file.path(
  OUT_DIR,
  "REP5_DOMAIN_SCORES_V3.csv"
)

AUDIT_OUT <- file.path(
  OUT_DIR,
  "REP5_SCORING_AUDIT_V3.csv"
)

MANIFEST_INFO_OUT <- file.path(
  OUT_DIR,
  "REP5_SCORING_MANIFEST_INFO_V3.txt"
)

write.csv(
  master,
  MASTER_OUT,
  row.names = FALSE,
  na = ""
)

write.csv(
  domains,
  DOMAIN_OUT,
  row.names = FALSE,
  na = ""
)

write.csv(
  audit,
  AUDIT_OUT,
  row.names = FALSE,
  na = ""
)

writeLines(
  c(
    paste0(
      "manifest_file=",
      manifest_file
    ),
    paste0(
      "scorer_file=",
      SCORER
    ),
    paste0(
      "master_rows=",
      nrow(master)
    ),
    paste0(
      "unique_run_ids=",
      length(unique(master$run_id))
    ),
    paste0(
      "scoring_eligible=",
      sum(
        master$scoring_eligible,
        na.rm = TRUE
      )
    ),
    paste0(
      "scored_accuracy_rows=",
      sum(
        !is.na(
          master$automated_accuracy
        )
      )
    ),
    paste0(
      "domain_rows=",
      nrow(domains)
    ),
    paste0(
      "audit_rows=",
      nrow(audit)
    )
  ),
  MANIFEST_INFO_OUT
)

cat("\n============================================\n")
cat("FILES WRITTEN\n")
cat("============================================\n")
cat(MASTER_OUT, "\n")
cat(DOMAIN_OUT, "\n")
cat(AUDIT_OUT, "\n")
cat(MANIFEST_INFO_OUT, "\n")

if (nrow(audit) > 0L) {

  cat(
    "\nWARNING: scoring audit contains ",
    nrow(audit),
    " row(s).\n",
    sep = ""
  )

  cat(
    "Do NOT freeze analysis until reviewed.\n"
  )

} else {

  cat("\nAUDIT CLEAN: 0 issues detected.\n")
}

cat("\nREP5 batch scoring v3 finished.\n")
