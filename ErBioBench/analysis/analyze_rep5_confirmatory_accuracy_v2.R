suppressPackageStartupMessages(
  library(lme4)
)

INFILE <- paste0(
  "ErBioBench/results/pilot_1_2/rep5_scoring_v3/",
  "REP5_MASTER_V3.csv"
)

OUTDIR <- paste0(
  "ErBioBench/results/pilot_1_2/",
  "rep5_confirmatory_v2"
)

dir.create(
  OUTDIR,
  recursive = TRUE,
  showWarnings = FALSE
)

d0 <- read.csv(
  INFILE,
  stringsAsFactors = FALSE,
  check.names = FALSE
)

# ============================================================
# CORPUS VALIDATION
# ============================================================

stopifnot(nrow(d0) == 2700L)
stopifnot(length(unique(d0$run_id)) == 2700L)

cat("============================================\n")
cat("ERBIOBENCH REP5 CONFIRMATORY ACCURACY v2 — REDUCED NON-SINGULAR MODEL\n")
cat("============================================\n")

cat("Nominal runs:", nrow(d0), "\n")

print(
  table(
    d0$execution_status,
    useNA = "ifany"
  )
)

# ============================================================
# PRIMARY SCIENTIFIC POPULATION
#
# Frozen plan:
#   execution_status = COMPLETED
#   parse_valid_json = TRUE
#
# REFUSAL / INVALID_JSON are NOT assigned zero accuracy.
# ============================================================

truthy <- function(x) {
  toupper(trimws(as.character(x))) %in%
    c("TRUE", "T", "1")
}

d <- d0[
  d0$execution_status == "COMPLETED" &
    truthy(d0$parse_valid_json),
  ,
  drop = FALSE
]

d$automated_accuracy <- as.numeric(
  d$automated_accuracy
)

stopifnot(!anyNA(d$automated_accuracy))

cat(
  "Primary scientific population:",
  nrow(d),
  "\n"
)

# Expected current frozen corpus = 2263
stopifnot(nrow(d) == 2263L)

# ============================================================
# VARIABLES
# ============================================================

condition_col <- if (
  "condition_id" %in% names(d)
) {
  "condition_id"
} else {
  "condition"
}

d$condition <- factor(
  d[[condition_col]],
  levels = c("C1", "C2", "C3")
)

d$model_slot <- factor(d$model_slot)
d$case_id <- factor(d$case_id)
d$repetition <- factor(d$repetition)

# Repeated condition block:
# model × case × repetition
d$pair_id <- interaction(
  d$model_slot,
  d$case_id,
  d$repetition,
  drop = TRUE
)

stopifnot(!anyNA(d$condition))

# ============================================================
# DESCRIPTIVE ACCURACY
# ============================================================

desc <- do.call(
  rbind,
  lapply(
    split(d, d$condition),
    function(z) {
      data.frame(
        condition = as.character(z$condition[1]),
        n = nrow(z),
        mean = mean(z$automated_accuracy),
        sd = sd(z$automated_accuracy),
        median = median(z$automated_accuracy),
        q1 = unname(
          quantile(
            z$automated_accuracy,
            0.25
          )
        ),
        q3 = unname(
          quantile(
            z$automated_accuracy,
            0.75
          )
        ),
        min = min(z$automated_accuracy),
        max = max(z$automated_accuracy)
      )
    }
  )
)

write.csv(
  desc,
  file.path(
    OUTDIR,
    "REP5_ACCURACY_DESCRIPTIVES_v1.csv"
  ),
  row.names = FALSE
)

cat("\nACCURACY DESCRIPTIVES\n")
print(desc, row.names = FALSE)

# ============================================================
# PRIMARY MIXED MODEL
#
# Prespecified factors:
# - model slot
# - case
# - repetition
#
# Repeated C1/C2/C3 block:
# - model × case × repetition
# ============================================================

m_full <- lmer(
  automated_accuracy ~
    condition +
    (1 | model_slot) +
    (1 | case_id),
  data = d,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(
      maxfun = 300000
    )
  )
)

m_null <- lmer(
  automated_accuracy ~
    1 +
    (1 | model_slot) +
    (1 | case_id),
  data = d,
  REML = FALSE,
  control = lmerControl(
    optimizer = "bobyqa",
    optCtrl = list(
      maxfun = 300000
    )
  )
)

cat("\n============================================\n")
cat("FULL PRIMARY MIXED MODEL\n")
cat("============================================\n")

print(summary(m_full))

singular_full <- isSingular(
  m_full,
  tol = 1e-5
)

cat(
  "\nSingular full model:",
  singular_full,
  "\n"
)

cat("\nRANDOM-EFFECT VARIANCES\n")

vc <- as.data.frame(
  VarCorr(m_full)
)

print(
  vc[
    ,
    c("grp", "vcov", "sdcor")
  ],
  row.names = FALSE
)

write.csv(
  vc,
  file.path(
    OUTDIR,
    "REP5_PRIMARY_RANDOM_EFFECTS_REDUCED_v2.csv"
  ),
  row.names = FALSE
)

# ============================================================
# GLOBAL CONDITION EFFECT
# ============================================================

lrt <- anova(
  m_null,
  m_full
)

cat("\n============================================\n")
cat("GLOBAL CONDITION EFFECT — LRT\n")
cat("============================================\n")

print(lrt)

capture.output(
  lrt,
  file = file.path(
    OUTDIR,
    "REP5_PRIMARY_GLOBAL_LRT_REDUCED_v2.txt"
  )
)

# ============================================================
# PRESPECIFIED WALD CONTRASTS
# ============================================================

b <- fixef(m_full)
V <- as.matrix(vcov(m_full))
nm <- names(b)

wald_contrast <- function(name, L) {

  est <- sum(L * b)

  se <- sqrt(
    as.numeric(
      t(L) %*% V %*% L
    )
  )

  z <- est / se

  p <- 2 * pnorm(
    abs(z),
    lower.tail = FALSE
  )

  data.frame(
    contrast = name,
    estimate = est,
    SE = se,
    z = z,
    p_raw = p,
    CI95_low = est - 1.96 * se,
    CI95_high = est + 1.96 * se,
    stringsAsFactors = FALSE
  )
}

L_C2_C1 <- setNames(
  rep(0, length(b)),
  nm
)
L_C2_C1["conditionC2"] <- 1

L_C3_C1 <- setNames(
  rep(0, length(b)),
  nm
)
L_C3_C1["conditionC3"] <- 1

L_C3_C2 <- setNames(
  rep(0, length(b)),
  nm
)
L_C3_C2["conditionC3"] <- 1
L_C3_C2["conditionC2"] <- -1

contrasts <- rbind(
  wald_contrast(
    "C2 - C1",
    L_C2_C1
  ),
  wald_contrast(
    "C3 - C1",
    L_C3_C1
  ),
  wald_contrast(
    "C3 - C2",
    L_C3_C2
  )
)

contrasts$p_holm <- p.adjust(
  contrasts$p_raw,
  method = "holm"
)

write.csv(
  contrasts,
  file.path(
    OUTDIR,
    "REP5_PRIMARY_CONTRASTS_REDUCED_v2.csv"
  ),
  row.names = FALSE
)

cat("\n============================================\n")
cat("PRESPECIFIED MIXED-MODEL CONTRASTS\n")
cat("============================================\n")

print(
  contrasts,
  row.names = FALSE
)

# ============================================================
# COMPLETE-CASE PAIRED SENSITIVITY
#
# This is sensitivity only, NOT primary population replacement.
# ============================================================

tab <- table(
  d$pair_id,
  d$condition
)

complete_ids <- rownames(tab)[
  apply(
    tab,
    1,
    function(x) all(x == 1L)
  )
]

dcc <- d[
  as.character(d$pair_id) %in% complete_ids,
  ,
  drop = FALSE
]

cat("\n============================================\n")
cat("COMPLETE-CASE PAIRED SENSITIVITY\n")
cat("============================================\n")

cat(
  "Complete model-case-repetition blocks:",
  length(complete_ids),
  "\n"
)

cat(
  "Observations:",
  nrow(dcc),
  "\n"
)

w <- reshape(
  dcc[
    ,
    c(
      "pair_id",
      "condition",
      "automated_accuracy"
    )
  ],
  idvar = "pair_id",
  timevar = "condition",
  direction = "wide"
)

names(w) <- sub(
  "^automated_accuracy\\.",
  "",
  names(w)
)

stopifnot(
  all(
    c("C1", "C2", "C3") %in%
      names(w)
  )
)

paired_effect <- function(name, x, y) {

  delta <- x - y

  data.frame(
    contrast = name,
    n_pairs = length(delta),
    mean_difference = mean(delta),
    median_difference = median(delta),
    q1_difference = unname(
      quantile(delta, 0.25)
    ),
    q3_difference = unname(
      quantile(delta, 0.75)
    ),
    stringsAsFactors = FALSE
  )
}

paired <- rbind(
  paired_effect(
    "C2 - C1",
    w$C2,
    w$C1
  ),
  paired_effect(
    "C3 - C1",
    w$C3,
    w$C1
  ),
  paired_effect(
    "C3 - C2",
    w$C3,
    w$C2
  )
)

wilcox_safe <- function(x, y) {

  suppressWarnings(
    wilcox.test(
      x,
      y,
      paired = TRUE,
      exact = FALSE
    )$p.value
  )
}

paired$p_wilcoxon_raw <- c(
  wilcox_safe(w$C2, w$C1),
  wilcox_safe(w$C3, w$C1),
  wilcox_safe(w$C3, w$C2)
)

paired$p_wilcoxon_holm <- p.adjust(
  paired$p_wilcoxon_raw,
  method = "holm"
)

write.csv(
  paired,
  file.path(
    OUTDIR,
    "REP5_PRIMARY_PAIRED_SENSITIVITY_v2.csv"
  ),
  row.names = FALSE
)

cat("\nPAIRED EFFECTS / WILCOXON\n")

print(
  paired,
  row.names = FALSE
)

# ============================================================
# SAVE MODEL OBJECTS + SUMMARY
# ============================================================

saveRDS(
  m_full,
  file.path(
    OUTDIR,
    "REP5_PRIMARY_MODEL_REDUCED_v2.rds"
  )
)

saveRDS(
  m_null,
  file.path(
    OUTDIR,
    "REP5_PRIMARY_MODEL_NULL_REDUCED_v2.rds"
  )
)

capture.output(
  summary(m_full),
  file = file.path(
    OUTDIR,
    "REP5_PRIMARY_MODEL_REDUCED_SUMMARY_v2.txt"
  )
)

writeLines(
  c(
    paste0(
      "Nominal runs: ",
      nrow(d0)
    ),
    paste0(
      "Primary scientific population: ",
      nrow(d)
    ),
    paste0(
      "Complete paired blocks: ",
      length(complete_ids)
    ),
    paste0(
      "Full model singular: ",
      singular_full
    )
  ),
  file.path(
    OUTDIR,
    "REP5_PRIMARY_ANALYSIS_STATUS_v2.txt"
  )
)

cat("\n============================================\n")
cat("PRIMARY ANALYSIS COMPLETE\n")
cat("============================================\n")
cat("Output:", OUTDIR, "\n")
