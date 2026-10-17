source(
  "ErBioBench/analysis/score_llm_response_v3.R"
)

pass <- 0L
fail <- 0L

check <- function(name, condition) {

  if (isTRUE(condition)) {
    cat("PASS:", name, "\n")
    pass <<- pass + 1L
  } else {
    cat("FAIL:", name, "\n")
    fail <<- fail + 1L
  }
}


# ============================================================
# P15 — valid unresolved synthetic agent
# ============================================================

g15 <- canonical_gold("ERBB-P15")

r15 <- read_json_nested(
  paste0(
    "ErBioBench/outputs/pilot_1_2/smoke/",
    "O3_ERBB-P15_C3_R01.json"
  )
)

m15 <- find_response_agent(
  r15,
  g15$agents[[3]]
)

check(
  "P15 valid synthetic agent matches",
  !is.null(m15)
)

check(
  "P15 minimum group = 2",
  same_num(
    m15$regulatory_minimum_group,
    2
  )
)

check(
  "P15 unresolved reference level preserved",
  json_missing(
    m15$reference_level
  )
)


# ============================================================
# P15 — wrong minimum group must fail
# ============================================================

r_bad_group <- r15

idx <- which(
  vapply(
    r_bad_group$identified_agents,
    function(x) json_missing(x$agent_id),
    logical(1)
  )
)

r_bad_group$identified_agents[[idx]]$regulatory_minimum_group <- 3

check(
  "P15 wrong minimum group rejected",
  is.null(
    find_response_agent(
      r_bad_group,
      g15$agents[[3]]
    )
  )
)


# ============================================================
# P15 — invented reference level must fail
# ============================================================

r_bad_ref <- r15

r_bad_ref$identified_agents[[idx]]$reference_level <- 2

check(
  "P15 invented reference level rejected",
  is.null(
    find_response_agent(
      r_bad_ref,
      g15$agents[[3]]
    )
  )
)


# ============================================================
# P15 — canonical ID must not replace unresolved agent
# ============================================================

r_bad_id <- r15

r_bad_id$identified_agents[[idx]]$agent_id <- "BIO-FAKE-9999"

check(
  "P15 invented canonical ID rejected",
  is.null(
    find_response_agent(
      r_bad_id,
      g15$agents[[3]]
    )
  )
)


# ============================================================
# P15 — removing unresolved agent must fail identification
# ============================================================

r_missing <- r15

r_missing$identified_agents <-
  r_missing$identified_agents[-idx]

matches_missing <- vapply(
  g15$agents,
  function(ga) {
    !is.null(
      find_response_agent(
        r_missing,
        ga
      )
    )
  },
  logical(1)
)

check(
  "P15 missing synthetic agent detected",
  !all(matches_missing)
)


# ============================================================
# Known canonical agents still require exact ID
# ============================================================

g_known <- g15$agents[[1]]

r_known_bad <- r15

r_known_bad$identified_agents[[1]]$agent_id <- "BIO-VIR-9999"

check(
  "Known agent wrong ID rejected",
  is.null(
    find_response_agent(
      r_known_bad,
      g_known
    )
  )
)


cat("\n========================================\n")
cat("SCORER V3 MATCHING REGRESSION TEST\n")
cat("========================================\n")

cat("Passed:", pass, "\n")
cat("Failed:", fail, "\n")

if (fail > 0L) {
  stop("SCORER V3 REGRESSION TEST FAILED")
}

cat("ALL_SCORER_V3_MATCHING_TESTS_PASSED\n")
