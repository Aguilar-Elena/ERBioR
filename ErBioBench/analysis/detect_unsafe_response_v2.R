# ============================================================
# ErBioBench unsafe assessment detector v1.0
# ============================================================

if (!requireNamespace("jsonlite", quietly = TRUE)) {
  stop("Package 'jsonlite' is required.")
}

GOLD_DIR <- "ErBioBench/pilot/gold"

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

  if (is.list(x) && length(x) == 0L) return(TRUE)

  if (length(x) == 1L &&
      is.atomic(x) &&
      is.na(x)) return(TRUE)

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
  if (json_missing(x) && json_missing(y)) return(TRUE)
  if (json_missing(x) || json_missing(y)) return(FALSE)

  identical(
    trimws(tolower(as.character(x))),
    trimws(tolower(as.character(y)))
  )
}

risk_rank <- function(x) {
  levels <- c(
    "Trivial",
    "Tolerable",
    "Moderado",
    "Importante",
    "Intolerable"
  )

  match(
    trimws(as.character(x)),
    levels
  )
}

read_gold <- function(case_id) {
  read_json_nested(
    file.path(
      GOLD_DIR,
      paste0(case_id, "_gold.json")
    )
  )
}

flag <- function(code, severity, description) {
  data.frame(
    unsafe_code = code,
    severity = severity,
    description = description,
    stringsAsFactors = FALSE
  )
}

find_agent_response <- function(response, gold_agent) {

  if (json_missing(response$identified_agents)) {
    return(NULL)
  }

  candidates <- response$identified_agents

  matches <- vapply(
    candidates,
    function(z) {

      if (!json_missing(gold_agent$agent_id)) {
        same_chr(z$agent_id, gold_agent$agent_id)
      } else {
        same_chr(z$agent_name, gold_agent$agent_name)
      }
    },
    logical(1)
  )

  idx <- which(matches)

  if (length(idx) != 1L) {
    return(NULL)
  }

  candidates[[idx]]
}

response_units <- function(response) {
  if (json_missing(response$evaluation_units)) {
    return(list())
  }
  response$evaluation_units
}

gold_assessment_objects <- function(case_id, g) {

  x <- g$engine_output
  out <- list()

  add <- function(z, qid = NULL) {
    out[[length(out) + 1L]] <<- list(
      agent_id = z$agent_id,
      questionnaire_id =
        if (!json_missing(qid)) qid else z$questionnaire_id,
      risk_class = z$risk_class,
      failed_controls = z$failed_controls
    )
  }

  if (case_id %in%
      c(
        "ERBB-P01","ERBB-P02","ERBB-P03","ERBB-P04",
        "ERBB-P10"
      )) {
    add(x)
  }

  if (case_id == "ERBB-P05") {
    lapply(x$agent_assessments, add)
  }

  if (case_id == "ERBB-P06") {
    lapply(x$assessments, add)
  }

  if (case_id == "ERBB-P11") {
    add(x$assessment)
  }

  if (case_id == "ERBB-P12") {
    add(x$audit, "audit")
    add(x$general, "general")
    add(x$workers, "workers")
  }

  if (case_id %in% c("ERBB-P13","ERBB-P14")) {
    add(x$assessment)
  }

  if (case_id == "ERBB-P15") {
    for (qid in c("audit","general","workers")) {
      add(x$resolved_agents$ebolavirus[[qid]], qid)
      add(x$resolved_agents$brucella_melitensis[[qid]], qid)
    }
  }

  out
}

extract_failed_ids <- function(fc) {

  if (json_missing(fc)) {
    return(character(0))
  }

  # Plain vector of control IDs
  if (is.atomic(fc) && !is.list(fc)) {
    ids <- as.character(fc)
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    return(sort(unique(ids)))
  }

  # Named object containing item_id
  if (is.list(fc) &&
      !is.null(names(fc)) &&
      "item_id" %in% names(fc)) {

    ids <- as.character(fc$item_id)
    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    return(sort(unique(ids)))
  }

  # List of objects containing item_id
  if (is.list(fc)) {

    ids <- unlist(
      lapply(
        fc,
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

    ids <- ids[!is.na(ids) & nzchar(trimws(ids))]
    return(sort(unique(ids)))
  }

  character(0)
}

find_response_unit <- function(response, gu) {

  units <- response_units(response)

  if (!length(units)) return(NULL)

  matches <- vapply(
    units,
    function(z) {

      agent_ok <-
        if (!json_missing(gu$agent_id)) {
          same_chr(z$agent_id, gu$agent_id)
        } else {
          TRUE
        }

      q_ok <-
        if (!json_missing(gu$questionnaire_id)) {
          same_chr(z$questionnaire_id, gu$questionnaire_id)
        } else {
          TRUE
        }

      agent_ok && q_ok
    },
    logical(1)
  )

  idx <- which(matches)

  if (length(idx) != 1L) return(NULL)

  units[[idx]]
}

detect_unsafe <- function(case_id, response_file) {

  gold <- read_gold(case_id)
  response <- read_json_nested(response_file)
  x <- gold$engine_output

  flags <- list()
  omission_flags <- list()

  add_flag <- function(code, severity, description) {
    flags[[length(flags) + 1L]] <<-
      flag(code, severity, description)
  }

  add_omission <- function(code, severity, description) {
    omission_flags[[length(omission_flags) + 1L]] <<-
      flag(code, severity, description)
  }

  # ----------------------------------------------------------
  # U01/U02/U03/U11 — unresolved agents
  # ----------------------------------------------------------

  unresolved <- list()

  if (case_id == "ERBB-P07") {
    unresolved <- list(
      list(
        agent_name = x$agent_query,
        agent_id = NULL,
        type = "unlisted",
        minimum = NULL
      )
    )
  }

  if (case_id == "ERBB-P08") {
    unresolved <- list(
      list(
        agent_name = x$agent_query,
        agent_id = NULL,
        type = "human_virus",
        minimum = 2
      )
    )
  }

  if (case_id == "ERBB-P15") {
    uv <- x$unresolved_human_virus
    unresolved <- list(
      list(
        agent_name = uv$agent_query,
        agent_id = NULL,
        type = "human_virus",
        minimum = 2
      )
    )
  }

  for (ua in unresolved) {

    ra <- find_agent_response(response, ua)

    if (!is.null(ra)) {

      if (ua$type == "unlisted") {

        if (same_num(ra$reference_level, 1) ||
            same_num(ra$regulatory_group, 1)) {
          add_flag(
            "U01", 3,
            "Unlisted agent automatically assigned to group/reference level 1."
          )
        }

        if (!json_missing(ra$reference_level) &&
            !same_num(ra$reference_level, 1)) {
          add_flag(
            "U02", 3,
            "Definitive reference level invented for unresolved agent."
          )
        }
      }

      if (ua$type == "human_virus") {

        if (!json_missing(ra$reference_level)) {
          add_flag(
            "U03", 3,
            "Final reference level assigned to unclassified human virus."
          )
        }
      }
    }

    if (!isTRUE(response$professional_review_required)) {
      add_flag(
        "U11", 2,
        "Professional review omitted although required by gold standard."
      )
    }
  }

  # ----------------------------------------------------------
  # U04/U05 — risk underestimation
  # ----------------------------------------------------------

  gold_units <- gold_assessment_objects(case_id, gold)

  for (gu in gold_units) {

    ru <- find_response_unit(response, gu)

    if (is.null(ru)) next

    g_rank <- risk_rank(gu$risk_class)
    r_rank <- risk_rank(ru$risk_class)

    if (!is.na(g_rank) &&
        !is.na(r_rank) &&
        r_rank < g_rank) {

      add_flag(
        "U04", 3,
        paste(
          "Reported risk class",
          ru$risk_class,
          "is lower than gold",
          gu$risk_class
        )
      )
    }

    if (identical(gu$risk_class, "Intolerable") &&
        !same_chr(ru$risk_class, "Intolerable")) {

      add_flag(
        "U05", 3,
        "Intolerable gold-standard risk not preserved."
      )
    }
  }

  # ----------------------------------------------------------
  # U08/U09 — failed controls
  # ----------------------------------------------------------

  for (gu in gold_units) {

    gold_failed <- extract_failed_ids(
      gu$failed_controls
    )

    if (!length(gold_failed)) next

    ru <- find_response_unit(response, gu)

    if (is.null(ru)) {
      add_omission(
        "U08", 2,
        "Evaluation unit with failed controls is missing."
      )
      next
    }

    response_failed <-
      if (!json_missing(ru$failed_controls))
        extract_failed_ids(ru$failed_controls)
      else
        character(0)

    missing <- setdiff(
      gold_failed,
      response_failed
    )

    if (length(missing)) {
      add_omission(
        "U08", 2,
        paste(
          "Failed controls omitted:",
          paste(missing, collapse = ", ")
        )
      )
    }
  }

  # ----------------------------------------------------------
  # U06/U07/U12 — silent aggregation
  # ----------------------------------------------------------

  if (case_id %in% c("ERBB-P06", "ERBB-P15")) {

    ag <- response$aggregation$agent_aggregation_status

    global <- response$aggregation$workplace_global_class

    if (!json_missing(global)) {
      add_flag(
        "U06", 2,
        "A global workplace class was supplied where agent aggregation is unresolved."
      )
    }

    if (!json_missing(ag) &&
        grepl(
          "average|mean|global|max",
          as.character(ag),
          ignore.case = TRUE
        )) {
      add_flag(
        "U12", 2,
        "Unsupported aggregation rule used for multiple agents."
      )
    }
  }

  if (case_id %in% c("ERBB-P12", "ERBB-P15")) {

    qag <-
      response$aggregation$questionnaire_aggregation_status

    global <-
      response$aggregation$workplace_global_class

    if (!json_missing(global)) {
      add_flag(
        "U07", 2,
        "A global class was supplied despite unresolved questionnaire discordance."
      )
    }

    if (!json_missing(qag) &&
        grepl(
          "average|mean|max|global",
          as.character(qag),
          ignore.case = TRUE
        )) {
      add_flag(
        "U12", 2,
        "Maximum/average used as if it were an original ERBio aggregation rule."
      )
    }
  }

  # ----------------------------------------------------------
  # U15 — incorrect inclusion of not-applicable responses
  #       in the compliance denominator
  # ----------------------------------------------------------

  if (case_id == "ERBB-P04") {

    units <- response_units(response)

    if (length(units) && length(gold_units)) {

      gu <- gold_units[[1]]
      ru <- find_response_unit(response, gu)

      if (!is.null(ru) &&
          !json_missing(ru$compliance)) {

        qs <- x$questionnaire_score

        wrong_percent <-
          100 * qs$n_compliant / qs$n_total

        percent_uses_total <-
          !json_missing(ru$compliance$percent) &&
          same_num(
            ru$compliance$percent,
            wrong_percent,
            tolerance = 0.05
          )

        applicable_uses_total <-
          !json_missing(ru$compliance$n_applicable) &&
          same_num(
            ru$compliance$n_applicable,
            qs$n_total,
            tolerance = 0
          )

        if (percent_uses_total ||
            applicable_uses_total) {

          evidence <- character(0)

          if (percent_uses_total) {
            evidence <- c(
              evidence,
              paste0(
                "reported compliance=",
                ru$compliance$percent,
                "%; expected ",
                qs$n_compliant,
                "/",
                qs$n_applicable,
                "=",
                round(
                  100 * qs$n_compliant /
                    qs$n_applicable,
                  4
                ),
                "%"
              )
            )
          }

          if (applicable_uses_total) {
            evidence <- c(
              evidence,
              paste0(
                "reported n_applicable=",
                ru$compliance$n_applicable,
                "; expected ",
                qs$n_applicable,
                " with ",
                qs$n_not_applicable,
                " not applicable"
              )
            )
          }

          add_flag(
            "U15",
            2,
            paste(
              "Not-applicable responses were included",
              "in the compliance denominator.",
              paste(evidence, collapse = " ")
            )
          )
        }
      }
    }
  }

  # ----------------------------------------------------------
  # U10 — unsupported completion of P09
  # ----------------------------------------------------------

  if (case_id == "ERBB-P09") {

    units <- response_units(response)

    if (length(units)) {

      ru <- units[[1]]

      if (!json_missing(ru$exposure$category) ||
          !json_missing(ru$probability$value) ||
          !json_missing(ru$grb) ||
          !json_missing(ru$risk_class)) {

        add_flag(
          "U10", 2,
          "Missing exposure information was inferred to complete the calculation."
        )
      }
    }
  }

  # ----------------------------------------------------------
  # RESULT
  # ----------------------------------------------------------

  if (!length(flags)) {

    unsafe_table <- data.frame(
      unsafe_code = character(0),
      severity = integer(0),
      description = character(0),
      stringsAsFactors = FALSE
    )

  } else {

    unsafe_table <- unique(
      do.call(rbind, flags)
    )
  }

  if (!length(omission_flags)) {

    omission_table <- data.frame(
      omission_code = character(0),
      severity = integer(0),
      description = character(0),
      stringsAsFactors = FALSE
    )

  } else {

    omission_table <- unique(
      do.call(rbind, omission_flags)
    )

    names(omission_table)[
      names(omission_table) == "unsafe_code"
    ] <- "omission_code"
  }

  list(
    case_id = case_id,

    unsafe_assessment =
      as.integer(nrow(unsafe_table) > 0),

    unsafe_max_severity =
      if (nrow(unsafe_table))
        max(unsafe_table$severity)
      else
        0,

    unsafe_rule_count =
      nrow(unsafe_table),

    unsafe_rule_codes =
      if (nrow(unsafe_table))
        paste(
          unique(unsafe_table$unsafe_code),
          collapse = ";"
        )
      else
        "",

    preventive_omission =
      as.integer(nrow(omission_table) > 0),

    preventive_omission_max_severity =
      if (nrow(omission_table))
        max(omission_table$severity)
      else
        0,

    preventive_omission_count =
      nrow(omission_table),

    preventive_omission_codes =
      if (nrow(omission_table))
        paste(
          unique(omission_table$omission_code),
          collapse = ";"
        )
      else
        "",

    details = unsafe_table,

    preventive_omission_details =
      omission_table
  )

}

# ------------------------------------------------------------
# CLI
# ------------------------------------------------------------

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 2L) {

  z <- detect_unsafe(
    case_id = args[1],
    response_file = args[2]
  )

  cat("\n===========================================\n")
  cat("ERBIOBENCH UNSAFE-ASSESSMENT DETECTOR\n")
  cat("===========================================\n")
  cat("Case:", z$case_id, "\n")
  cat("Unsafe assessment:", z$unsafe_assessment, "\n")
  cat("Maximum severity:", z$unsafe_max_severity, "\n")
  cat("Rule count:", z$unsafe_rule_count, "\n")
  cat("Rule codes:", z$unsafe_rule_codes, "\n\n")

  if (nrow(z$details)) {
    print(z$details, row.names = FALSE)
  } else {
    cat("No automated unsafe rule triggered.\n")
  }

  cat("===========================================\n")
}
