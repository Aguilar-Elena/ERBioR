# ============================================================
# ErBioBench automated scorer v1.0
# ============================================================

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.")
}

PROTOCOL_DIR <- "ErBioBench/protocol"
GOLD_DIR <- "ErBioBench/pilot/gold"

# ------------------------------------------------------------
# BASIC HELPERS
# ------------------------------------------------------------

read_json_nested <- function(path) {
  jsonlite::fromJSON(
    path,
    simplifyVector = TRUE,
    simplifyDataFrame = FALSE,
    simplifyMatrix = FALSE
  )
}

json_missing <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0L) return(TRUE)

  if (is.list(x) &&
      length(x) == 0L) {
    return(TRUE)
  }

  if (length(x) == 1L &&
      is.atomic(x) &&
      is.na(x)) {
    return(TRUE)
  }

  FALSE
}

same_num <- function(x, y, tolerance = 0.01) {

  if (json_missing(x) && json_missing(y)) {
    return(TRUE)
  }

  if (json_missing(x) || json_missing(y)) {
    return(FALSE)
  }

  if (length(x) != 1L || length(y) != 1L) {
    return(FALSE)
  }

  nx <- suppressWarnings(
    as.numeric(as.character(x))
  )

  ny <- suppressWarnings(
    as.numeric(as.character(y))
  )

  if (
    length(nx) != 1L ||
    length(ny) != 1L ||
    is.na(nx) ||
    is.na(ny) ||
    !is.finite(nx) ||
    !is.finite(ny)
  ) {
    return(FALSE)
  }

  isTRUE(
    abs(nx - ny) <= tolerance
  )
}

same_chr <- function(x, y) {

  if (json_missing(x) && json_missing(y)) {
    return(TRUE)
  }

  if (json_missing(x) || json_missing(y)) {
    return(FALSE)
  }

  identical(
    trimws(tolower(as.character(x))),
    trimws(tolower(as.character(y)))
  )
}

norm_ids <- function(x) {

  if (json_missing(x)) {
    return(character(0))
  }

  # Plain atomic vector of identifiers
  if (is.atomic(x) && !is.list(x)) {
    out <- as.character(x)
    out <- out[!is.na(out) & nzchar(trimws(out))]
    return(sort(unique(out)))
  }

  # Named object containing item_id
  if (is.list(x) &&
      !is.null(names(x)) &&
      "item_id" %in% names(x)) {

    out <- as.character(x$item_id)
    out <- out[!is.na(out) & nzchar(trimws(out))]
    return(sort(unique(out)))
  }

  # List of objects, each potentially containing item_id
  if (is.list(x)) {

    out <- unlist(
      lapply(
        x,
        function(z) {

          if (is.list(z) &&
              !is.null(z$item_id)) {
            return(as.character(z$item_id))
          }

          if (is.atomic(z) &&
              length(z) == 1L) {
            return(as.character(z))
          }

          character(0)
        }
      ),
      use.names = FALSE
    )

    out <- out[!is.na(out) & nzchar(trimws(out))]
    return(sort(unique(out)))
  }

  character(0)
}

same_id_set <- function(x, y) {
  identical(norm_ids(x), norm_ids(y))
}

points <- function(correct, max_points) {
  if (isTRUE(correct)) max_points else 0
}

# ------------------------------------------------------------
# RUBRIC
# ------------------------------------------------------------

rubric <- read.csv(
  file.path(
    PROTOCOL_DIR,
    "scoring_rubric_v1.csv"
  ),
  stringsAsFactors = FALSE
)

weights <- setNames(
  rubric$max_points,
  rubric$domain
)

# ------------------------------------------------------------
# GOLD HELPERS
# ------------------------------------------------------------

read_gold <- function(case_id) {

  path <- file.path(
    GOLD_DIR,
    paste0(case_id, "_gold.json")
  )

  if (!file.exists(path)) {
    stop("Gold file not found: ", path)
  }

  read_json_nested(path)
}

# Convert a single ERBioR assessment object
# into the canonical scoring representation.

assessment_unit <- function(x,
                            questionnaire_id = NULL) {

  failed <- character(0)

  if (!json_missing(x$failed_controls)) {

    fc <- x$failed_controls

    if (is.list(fc)) {

      if (!is.null(names(fc)) &&
          "item_id" %in% names(fc)) {

        failed <- as.character(fc$item_id)

      } else {

        failed <- vapply(
          fc,
          function(z) {
            if (is.list(z) &&
                !is.null(z$item_id)) {
              as.character(z$item_id)
            } else {
              NA_character_
            }
          },
          character(1)
        )

        failed <- failed[!is.na(failed)]
      }
    }
  }

  list(
    agent_id =
      if (!json_missing(x$agent_id))
        as.character(x$agent_id)
      else NULL,

    agent_name =
      if (!json_missing(x$agent_name))
        as.character(x$agent_name)
      else NULL,

    questionnaire_id =
      if (!json_missing(questionnaire_id))
        questionnaire_id
      else if (!json_missing(x$questionnaire_id))
        x$questionnaire_id
      else NULL,

    reference_level = x$reference_level,

    exposure = x$exposure,

    compliance_percent = x$compliance_percent,

    compliance_class = x$compliance_class,

    probability_value = x$probability_value,

    grb = x$risk_score,

    risk_class = x$risk_class,

    failed_controls = failed
  )
}

# ------------------------------------------------------------
# BUILD CANONICAL GOLD
# ------------------------------------------------------------

canonical_gold <- function(case_id) {

  g <- read_gold(case_id)
  x <- g$engine_output

  out <- list(
    case_id = case_id,
    agents = list(),
    units = list(),
    professional_review_required =
      isTRUE(g$professional_review_required),
    questionnaire_aggregation_status = NULL,
    agent_aggregation_status = NULL,
    workplace_global_class = NULL
  )

  # ----------------------------------------------------------
  # P01-P04 / P10
  # Single assessment
  # ----------------------------------------------------------

  if (case_id %in%
      c(
        "ERBB-P01",
        "ERBB-P02",
        "ERBB-P03",
        "ERBB-P04",
        "ERBB-P10"
      )) {

    out$agents <- list(
      list(
        agent_id = x$agent_id,
        agent_name = x$agent_name,
        reference_level = x$reference_level,
        professional_review_required = FALSE
      )
    )

    out$units <- list(
      assessment_unit(x)
    )
  }

  # ----------------------------------------------------------
  # P05
  # ----------------------------------------------------------

  if (case_id == "ERBB-P05") {

    vals <- x$agent_assessments

    out$units <- lapply(
      vals,
      assessment_unit
    )

    out$agents <- lapply(
      vals,
      function(z) {
        list(
          agent_id = z$agent_id,
          agent_name = z$agent_name,
          reference_level = z$reference_level,
          professional_review_required = FALSE
        )
      }
    )

    out$agent_aggregation_status <- "report_all"
  }

  # ----------------------------------------------------------
  # P06
  # ----------------------------------------------------------

  if (case_id == "ERBB-P06") {

    out$units <- lapply(
      x$assessments,
      assessment_unit
    )

    out$agents <- lapply(
      x$assessments,
      function(z) {
        list(
          agent_id = z$agent_id,
          agent_name = z$agent_name,
          reference_level = z$reference_level,
          professional_review_required = FALSE
        )
      }
    )

    out$agent_aggregation_status <- "unresolved"
  }

  # ----------------------------------------------------------
  # P07
  # ----------------------------------------------------------

  if (case_id == "ERBB-P07") {

    out$agents <- list(
      list(
        agent_id = NULL,
        agent_name = x$agent_query,
        classification_status =
          x$classification_status,
        regulatory_minimum_group =
          x$regulatory_minimum_group,
        reference_level =
          x$reference_level,
        professional_review_required = TRUE
      )
    )

    out$professional_review_required <- TRUE
  }

  # ----------------------------------------------------------
  # P08
  # ----------------------------------------------------------

  if (case_id == "ERBB-P08") {

    out$agents <- list(
      list(
        agent_id = NULL,
        agent_name = x$agent_query,
        classification_status =
          x$classification_status,
        regulatory_minimum_group =
          x$regulatory_minimum_group,
        reference_level =
          x$reference_level,
        professional_review_required = TRUE
      )
    )

    out$professional_review_required <- TRUE
  }

  # ----------------------------------------------------------
  # P09
  # ----------------------------------------------------------

  if (case_id == "ERBB-P09") {

    ar <- x$agent_reference

    out$agents <- list(
      list(
        agent_id = ar$agent_id,
        agent_name = ar$agent_name,
        reference_level = ar$reference_level,
        professional_review_required = TRUE
      )
    )

    out$professional_review_required <- TRUE

    out$units <- list(
      list(
        agent_id = ar$agent_id,
        agent_name = ar$agent_name,
        questionnaire_id = "general",
        reference_level = ar$reference_level,
        exposure = NULL,
        compliance_percent =
          x$questionnaire_score$compliance_percent,
        compliance_class = NULL,
        probability_value = NULL,
        grb = NULL,
        risk_class = NULL,
        failed_controls =
          vapply(
            x$questionnaire_score$failed_controls,
            function(z) z$item_id,
            character(1)
          )
      )
    )
  }

  # ----------------------------------------------------------
  # P11
  # ----------------------------------------------------------

  if (case_id == "ERBB-P11") {

    z <- x$assessment

    out$agents <- list(
      list(
        agent_id = z$agent_id,
        agent_name = z$agent_name,
        reference_level = z$reference_level,
        professional_review_required = FALSE
      )
    )

    out$units <- list(
      assessment_unit(z)
    )
  }

  # ----------------------------------------------------------
  # P12
  # ----------------------------------------------------------

  if (case_id == "ERBB-P12") {

    qids <- c(
      "audit",
      "general",
      "workers"
    )

    vals <- list(
      x$audit,
      x$general,
      x$workers
    )

    out$units <- Map(
      function(z, qid) {
        assessment_unit(z, qid)
      },
      vals,
      qids
    )

    out$agents <- list(
      list(
        agent_id = x$audit$agent_id,
        agent_name = x$audit$agent_name,
        reference_level =
          x$audit$reference_level,
        professional_review_required = FALSE
      )
    )

    out$questionnaire_aggregation_status <-
      "unresolved"
  }

  # ----------------------------------------------------------
  # P13/P14
  # ----------------------------------------------------------

  if (case_id %in%
      c("ERBB-P13", "ERBB-P14")) {

    z <- x$assessment

    out$agents <- list(
      list(
        agent_id = z$agent_id,
        agent_name = z$agent_name,
        reference_level = z$reference_level,
        professional_review_required = FALSE
      )
    )

    out$units <- list(
      assessment_unit(z)
    )
  }

  # ----------------------------------------------------------
  # P15
  # ----------------------------------------------------------

  if (case_id == "ERBB-P15") {

    resolved <- x$resolved_agents

    qids <- c(
      "audit",
      "general",
      "workers"
    )

    eb <- resolved$ebolavirus
    br <- resolved$brucella_melitensis

    out$units <- c(
      Map(
        function(qid) {
          assessment_unit(
            eb[[qid]],
            qid
          )
        },
        qids
      ),
      Map(
        function(qid) {
          assessment_unit(
            br[[qid]],
            qid
          )
        },
        qids
      )
    )

    uv <- x$unresolved_human_virus

    out$agents <- list(
      list(
        agent_id =
          eb$audit$agent_id,
        agent_name =
          eb$audit$agent_name,
        reference_level = 4,
        professional_review_required = FALSE
      ),
      list(
        agent_id =
          br$audit$agent_id,
        agent_name =
          br$audit$agent_name,
        reference_level = 3,
        professional_review_required = FALSE
      ),
      list(
        agent_id = NULL,
        agent_name = uv$agent_query,
        classification_status =
          uv$classification_status,
        regulatory_minimum_group =
          uv$regulatory_minimum_group,
        reference_level = NULL,
        professional_review_required = TRUE
      )
    )

    out$questionnaire_aggregation_status <-
      "unresolved"

    out$agent_aggregation_status <-
      "unresolved"

    out$workplace_global_class <- NULL

    out$professional_review_required <- TRUE
  }

  out
}

# ------------------------------------------------------------
# RESPONSE HELPERS
# ------------------------------------------------------------

response_agent_ids <- function(r) {

  if (json_missing(r$identified_agents)) {
    return(character(0))
  }

  ids <- vapply(
    r$identified_agents,
    function(z) {
      if (!json_missing(z$agent_id))
        as.character(z$agent_id)
      else
        NA_character_
    },
    character(1)
  )

  ids[!is.na(ids)]
}

find_response_unit <- function(response,
                               gold_unit) {

  if (json_missing(response$evaluation_units)) {
    return(NULL)
  }

  candidate <- response$evaluation_units

  matches <- vapply(
    candidate,
    function(z) {

      agent_ok <-
        if (!json_missing(gold_unit$agent_id)) {
          same_chr(
            z$agent_id,
            gold_unit$agent_id
          )
        } else {
          TRUE
        }

      q_ok <-
        if (!json_missing(
          gold_unit$questionnaire_id
        )) {
          same_chr(
            z$questionnaire_id,
            gold_unit$questionnaire_id
          )
        } else {
          TRUE
        }

      agent_ok && q_ok
    },
    logical(1)
  )

  idx <- which(matches)

  if (length(idx) != 1L) {
    return(NULL)
  }

  candidate[[idx]]
}

# ------------------------------------------------------------
# SCORE ONE UNIT
# ------------------------------------------------------------

score_unit <- function(g, r) {

  out <- list()

  out$exposure <-
    same_chr(
      r$exposure$category,
      g$exposure
    )

  out$compliance <-
    same_num(
      r$compliance$percent,
      g$compliance_percent
    )

  out$probability <-
    same_num(
      r$probability$value,
      g$probability_value
    )

  out$grb <-
    same_num(
      r$grb,
      g$grb
    )

  out$risk_class <-
    same_chr(
      r$risk_class,
      g$risk_class
    )

  out$failed_controls <-
    same_id_set(
      r$failed_controls,
      g$failed_controls
    )

  out
}

# ------------------------------------------------------------
# MAIN SCORER
# ------------------------------------------------------------

score_erbiobench_response <- function(case_id,
                                      response_file) {

  gold <- canonical_gold(case_id)
  response <- read_json_nested(response_file)

  if (!identical(
    as.character(response$case_id),
    case_id
  )) {
    stop("case_id mismatch.")
  }

  domain <- list()

  # ----------------------------------------------------------
  # AGENT IDENTIFICATION
  # ----------------------------------------------------------

  gold_ids <- vapply(
    gold$agents,
    function(z) {
      if (!json_missing(z$agent_id))
        as.character(z$agent_id)
      else
        NA_character_
    },
    character(1)
  )

  gold_ids <- gold_ids[!is.na(gold_ids)]

  response_ids <- response_agent_ids(response)

  domain$agent_identification <-
    if (length(gold_ids) > 0L) {
      same_id_set(
        response_ids,
        gold_ids
      )
    } else {
      length(response$identified_agents) >= 1L
    }

  # ----------------------------------------------------------
  # REGULATORY / REFERENCE LEVEL
  # ----------------------------------------------------------

  regulatory_correct <- TRUE

  for (ga in gold$agents) {

    candidates <- response$identified_agents

    if (json_missing(candidates)) {
      regulatory_correct <- FALSE
      next
    }

    idx <- which(vapply(
      candidates,
      function(ra) {

        if (!json_missing(ga$agent_id)) {
          same_chr(
            ra$agent_id,
            ga$agent_id
          )
        } else {
          same_chr(
            ra$agent_name,
            ga$agent_name
          )
        }
      },
      logical(1)
    ))

    if (length(idx) != 1L) {
      regulatory_correct <- FALSE
      next
    }

    ra <- candidates[[idx]]

    if (!same_num(
      ra$reference_level,
      ga$reference_level
    )) {
      regulatory_correct <- FALSE
    }

    if (!json_missing(
      ga$regulatory_minimum_group
    )) {

      if (!same_num(
        ra$regulatory_minimum_group,
        ga$regulatory_minimum_group
      )) {
        regulatory_correct <- FALSE
      }
    }
  }

  domain$regulatory_reference_level <-
    regulatory_correct

  # ----------------------------------------------------------
  # UNIT-LEVEL DOMAINS
  # ----------------------------------------------------------

  unit_scores <- lapply(
    gold$units,
    function(gu) {

      ru <- find_response_unit(
        response,
        gu
      )

      if (is.null(ru)) {
        return(list(
          exposure = FALSE,
          compliance = FALSE,
          probability = FALSE,
          grb = FALSE,
          risk_class = FALSE,
          failed_controls = FALSE
        ))
      }

      score_unit(gu, ru)
    }
  )

  if (length(unit_scores) > 0L) {

    domain$exposure <-
      all(vapply(
        unit_scores,
        `[[`,
        logical(1),
        "exposure"
      ))

    domain$compliance <-
      all(vapply(
        unit_scores,
        `[[`,
        logical(1),
        "compliance"
      ))

    domain$probability <-
      all(vapply(
        unit_scores,
        `[[`,
        logical(1),
        "probability"
      ))

    domain$grb <-
      all(vapply(
        unit_scores,
        `[[`,
        logical(1),
        "grb"
      ))

    domain$risk_class <-
      all(vapply(
        unit_scores,
        `[[`,
        logical(1),
        "risk_class"
      ))

    domain$failed_controls <-
      all(vapply(
        unit_scores,
        `[[`,
        logical(1),
        "failed_controls"
      ))

  } else {

    domain$exposure <- NA
    domain$compliance <- NA
    domain$probability <- NA
    domain$grb <- NA
    domain$risk_class <- NA
    domain$failed_controls <- NA
  }

  # ----------------------------------------------------------
  # PROFESSIONAL REVIEW
  # ----------------------------------------------------------

  domain$professional_review <-
    identical(
      isTRUE(
        response$professional_review_required
      ),
      isTRUE(
        gold$professional_review_required
      )
    )

  # ----------------------------------------------------------
  # UNCERTAINTY MANAGEMENT
  # ----------------------------------------------------------

  if (gold$professional_review_required) {

    domain$uncertainty_management <-
      isTRUE(
        response$uncertainty$present
      ) &&
      isTRUE(
        response$professional_review_required
      )

  } else {

    domain$uncertainty_management <- TRUE
  }

  # ----------------------------------------------------------
  # TRACEABILITY / COHERENCE
  # Partial automation only
  # ----------------------------------------------------------

  domain$traceability_coherence <- NA

  # Preventive semantic quality requires adjudication
  domain$preventive_actions <- NA

  # ----------------------------------------------------------
  # APPLICABILITY
  # ----------------------------------------------------------

  applicability <- read.csv(
    file.path(
      PROTOCOL_DIR,
      "case_scoring_applicability_v1.csv"
    ),
    stringsAsFactors = FALSE
  )

  app <- applicability[
    applicability$case_id == case_id,
    ,
    drop = FALSE
  ]

  if (nrow(app) != 1L) {
    stop("Applicability row missing.")
  }

  # ----------------------------------------------------------
  # POINT CALCULATION
  # ----------------------------------------------------------

  scoring_rows <- list()

  for (d in names(weights)) {

    applicable <-
      if (d %in% names(app)) {
        app[[d]] == 1
      } else {
        TRUE
      }

    result <-
      if (d %in% names(domain))
        domain[[d]]
      else
        NA

    auto_scored <-
      !is.na(result)

    obtained <-
      if (!applicable) {
        NA_real_
      } else if (!auto_scored) {
        NA_real_
      } else {
        points(result, weights[[d]])
      }

    scoring_rows[[length(scoring_rows)+1]] <-
      data.frame(
        domain = d,
        max_points = weights[[d]],
        applicable = applicable,
        auto_scored = auto_scored,
        correct =
          if (is.na(result)) NA else result,
        obtained_points = obtained,
        stringsAsFactors = FALSE
      )
  }

  score_table <- do.call(
    rbind,
    scoring_rows
  )

  auto_applicable <-
    score_table$applicable &
    score_table$auto_scored

  auto_max <-
    sum(
      score_table$max_points[
        auto_applicable
      ]
    )

  auto_obtained <-
    sum(
      score_table$obtained_points[
        auto_applicable
      ],
      na.rm = TRUE
    )

  automated_accuracy <-
    if (auto_max > 0)
      100 * auto_obtained / auto_max
    else
      NA_real_

  list(
    case_id = case_id,
    response_file = response_file,
    domain_scores = score_table,
    automated_points_obtained =
      auto_obtained,
    automated_points_possible =
      auto_max,
    automated_accuracy =
      automated_accuracy,
    manual_domains_pending =
      score_table$domain[
        score_table$applicable &
        !score_table$auto_scored
      ]
  )
}

# ------------------------------------------------------------
# CLI
# ------------------------------------------------------------

args <- commandArgs(
  trailingOnly = TRUE
)

if (length(args) == 2L) {

  result <- score_erbiobench_response(
    case_id = args[1],
    response_file = args[2]
  )

  cat("\n===========================================\n")
  cat("ERBIOBENCH AUTOMATED SCORER\n")
  cat("===========================================\n")
  cat("Case:", result$case_id, "\n")
  cat(
    "Automated score:",
    result$automated_points_obtained,
    "/",
    result$automated_points_possible,
    "\n"
  )
  cat(
    "Automated accuracy:",
    round(result$automated_accuracy, 2),
    "%\n"
  )

  if (length(result$manual_domains_pending)) {
    cat(
      "Manual domains pending:",
      paste(
        result$manual_domains_pending,
        collapse = ", "
      ),
      "\n"
    )
  }

  cat("\n")
  print(
    result$domain_scores,
    row.names = FALSE
  )

  cat("===========================================\n")
}
