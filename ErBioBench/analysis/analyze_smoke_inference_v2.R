suppressPackageStartupMessages(
  library(lme4)
)

INFILE <- paste0(
  "ErBioBench/results/pilot_1_2/",
  "SMOKE_COMPLETE_CASE_PAIRED_RUNS_v1.csv"
)

OUTDIR <- "ErBioBench/results/pilot_1_2"

d <- read.csv(
  INFILE,
  stringsAsFactors = FALSE
)

# ============================================================
# VALIDATION
# ============================================================

stopifnot(nrow(d) == 288L)

d$condition <- factor(
  d$condition,
  levels = c("C1", "C2", "C3")
)

d$model_slot <- factor(d$model_slot)
d$case_id <- factor(d$case_id)

d$pair_id <- interaction(
  d$model_slot,
  d$case_id,
  drop = TRUE
)

tab <- table(
  d$pair_id,
  d$condition
)

stopifnot(nrow(tab) == 96L)
stopifnot(all(tab == 1L))

cat("========================================\n")
cat("ERBIOBENCH SMOKE INFERENTIAL ANALYSIS\n")
cat("========================================\n")
cat("Rows:", nrow(d), "\n")
cat("Complete model-case pairs:", nrow(tab), "\n")


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
        )
      )
    }
  )
)

cat("\n========================================\n")
cat("ACCURACY DESCRIPTIVES\n")
cat("========================================\n")

print(
  desc,
  row.names = FALSE
)


# ============================================================
# MIXED MODEL
#
# Crossed:
# model
# case
# model × case repeated block
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
      maxfun = 200000
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
      maxfun = 200000
    )
  )
)

cat("\n========================================\n")
cat("MIXED MODEL\n")
cat("========================================\n")

print(summary(m_full))

cat("\nSingular full model:")
cat(
  isSingular(
    m_full,
    tol = 1e-5
  ),
  "\n"
)


# ============================================================
# GLOBAL CONDITION TEST
# ============================================================

lrt <- anova(
  m_null,
  m_full
)

cat("\n========================================\n")
cat("GLOBAL CONDITION EFFECT — LRT\n")
cat("========================================\n")

print(lrt)


# ============================================================
# FIXED-EFFECT CONTRASTS
# Wald approximation
# ============================================================

b <- fixef(m_full)
V <- as.matrix(vcov(m_full))

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
    p = p,
    CI95_low = est - 1.96 * se,
    CI95_high = est + 1.96 * se,
    stringsAsFactors = FALSE
  )
}

nm <- names(b)

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
  contrasts$p,
  method = "holm"
)

cat("\n========================================\n")
cat("MIXED-MODEL PAIRWISE CONTRASTS\n")
cat("========================================\n")

print(
  contrasts,
  row.names = FALSE
)


# ============================================================
# WIDE PAIRED DATA
# ============================================================

w_acc <- reshape(
  d[
    ,
    c(
      "pair_id",
      "automated_accuracy",
      "condition"
    )
  ],
  idvar = "pair_id",
  timevar = "condition",
  direction = "wide"
)

names(w_acc) <- sub(
  "automated_accuracy\\.",
  "",
  names(w_acc)
)


# ============================================================
# PAIRED DIFFERENCES
# ============================================================

paired_effect <- function(a, b, name) {

  dif <- b - a

  data.frame(
    contrast = name,
    n = length(dif),
    mean_difference = mean(dif),
    sd_difference = sd(dif),
    median_difference = median(dif),
    q1_difference =
      unname(quantile(dif, 0.25)),
    q3_difference =
      unname(quantile(dif, 0.75)),
    stringsAsFactors = FALSE
  )
}

pair_effects <- rbind(
  paired_effect(
    w_acc$C1,
    w_acc$C2,
    "C2 - C1"
  ),
  paired_effect(
    w_acc$C1,
    w_acc$C3,
    "C3 - C1"
  ),
  paired_effect(
    w_acc$C2,
    w_acc$C3,
    "C3 - C2"
  )
)

cat("\n========================================\n")
cat("RAW PAIRED EFFECTS\n")
cat("========================================\n")

print(
  pair_effects,
  row.names = FALSE
)


# ============================================================
# WILCOXON PAIRED SENSITIVITY
#
# Used because accuracy is strongly bounded and C3 is
# concentrated at the ceiling.
# ============================================================

wilcox_safe <- function(x, y, label) {

  z <- suppressWarnings(
    wilcox.test(
      x,
      y,
      paired = TRUE,
      exact = FALSE,
      correct = TRUE
    )
  )

  data.frame(
    contrast = label,
    statistic =
      unname(z$statistic),
    p = z$p.value,
    stringsAsFactors = FALSE
  )
}

wilcox_results <- rbind(
  wilcox_safe(
    w_acc$C1,
    w_acc$C2,
    "C1 vs C2"
  ),
  wilcox_safe(
    w_acc$C1,
    w_acc$C3,
    "C1 vs C3"
  ),
  wilcox_safe(
    w_acc$C2,
    w_acc$C3,
    "C2 vs C3"
  )
)

wilcox_results$p_holm <- p.adjust(
  wilcox_results$p,
  method = "holm"
)

cat("\n========================================\n")
cat("PAIRED WILCOXON SENSITIVITY\n")
cat("========================================\n")

print(
  wilcox_results,
  row.names = FALSE
)


# ============================================================
# BINARY OUTCOMES
# Exact paired discordance test
# ============================================================

binary_wide <- function(variable) {

  tmp <- d[
    ,
    c(
      "pair_id",
      "condition",
      variable
    )
  ]

  reshape(
    tmp,
    idvar = "pair_id",
    timevar = "condition",
    direction = "wide"
  )
}

exact_discordance <- function(x,
                              y,
                              outcome,
                              contrast) {

  ok <- !is.na(x) & !is.na(y)

  x <- as.integer(x[ok])
  y <- as.integer(y[ok])

  n10 <- sum(
    x == 1L &
    y == 0L
  )

  n01 <- sum(
    x == 0L &
    y == 1L
  )

  nd <- n10 + n01

  p <- if (nd == 0L) {
    1
  } else {
    binom.test(
      n10,
      nd,
      p = 0.5,
      alternative = "two.sided"
    )$p.value
  }

  data.frame(
    outcome = outcome,
    contrast = contrast,
    n_pairs = length(x),
    first_only = n10,
    second_only = n01,
    discordant_total = nd,
    exact_p = p,
    stringsAsFactors = FALSE
  )
}


# UNSAFE
wu <- binary_wide(
  "unsafe_assessment"
)

unsafe_exact <- rbind(
  exact_discordance(
    wu$unsafe_assessment.C1,
    wu$unsafe_assessment.C2,
    "unsafe_assessment",
    "C1 vs C2"
  ),
  exact_discordance(
    wu$unsafe_assessment.C1,
    wu$unsafe_assessment.C3,
    "unsafe_assessment",
    "C1 vs C3"
  ),
  exact_discordance(
    wu$unsafe_assessment.C2,
    wu$unsafe_assessment.C3,
    "unsafe_assessment",
    "C2 vs C3"
  )
)


# PREVENTIVE OMISSION
wo <- binary_wide(
  "preventive_omission"
)

omission_exact <- rbind(
  exact_discordance(
    wo$preventive_omission.C1,
    wo$preventive_omission.C2,
    "preventive_omission",
    "C1 vs C2"
  ),
  exact_discordance(
    wo$preventive_omission.C1,
    wo$preventive_omission.C3,
    "preventive_omission",
    "C1 vs C3"
  ),
  exact_discordance(
    wo$preventive_omission.C2,
    wo$preventive_omission.C3,
    "preventive_omission",
    "C2 vs C3"
  )
)


unsafe_exact$p_holm <- p.adjust(
  unsafe_exact$exact_p,
  method = "holm"
)

omission_exact$p_holm <- p.adjust(
  omission_exact$exact_p,
  method = "holm"
)


cat("\n========================================\n")
cat("UNSAFE — EXACT PAIRED TESTS\n")
cat("========================================\n")

print(
  unsafe_exact,
  row.names = FALSE
)

cat("\n========================================\n")
cat("PREVENTIVE OMISSION — EXACT PAIRED TESTS\n")
cat("========================================\n")

print(
  omission_exact,
  row.names = FALSE
)


# ============================================================
# OUTPUT FILES
# ============================================================

write.csv(
  desc,
  file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_ACCURACY_DESCRIPTIVES_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  contrasts,
  file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_LMM_CONTRASTS_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  pair_effects,
  file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_PAIRED_EFFECTS_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  wilcox_results,
  file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_WILCOXON_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  unsafe_exact,
  file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_UNSAFE_EXACT_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  omission_exact,
  file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_OMISSION_EXACT_v1.csv"
  ),
  row.names = FALSE
)

capture.output(
  summary(m_full),
  file = file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_LMM_SUMMARY_v1.txt"
  )
)

capture.output(
  lrt,
  file = file.path(
    OUTDIR,
    "SMOKE_INFERENCE_V2_LMM_LRT_v1.txt"
  )
)


cat("\n========================================\n")
cat("ANALYSIS COMPLETE\n")
cat("========================================\n")

cat(
  "Mixed model singular:",
  isSingular(
    m_full,
    tol = 1e-5
  ),
  "\n"
)

cat("SMOKE_INFERENCE_V2_DONE\n")
