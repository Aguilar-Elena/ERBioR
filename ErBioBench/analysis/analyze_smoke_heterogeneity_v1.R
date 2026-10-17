INFILE <- paste0(
  "ErBioBench/results/pilot_1_2/",
  "SMOKE_COMPLETE_CASE_PAIRED_RUNS_v1.csv"
)

OUTDIR <- "ErBioBench/results/pilot_1_2"

d <- read.csv(
  INFILE,
  stringsAsFactors = FALSE
)

stopifnot(nrow(d) == 288L)

# ------------------------------------------------------------
# One row per model-case pair
# ------------------------------------------------------------

wide <- reshape(
  d[
    ,
    c(
      "model_slot",
      "provider",
      "case_id",
      "condition",
      "automated_accuracy",
      "unsafe_assessment",
      "preventive_omission"
    )
  ],
  idvar = c(
    "model_slot",
    "provider",
    "case_id"
  ),
  timevar = "condition",
  direction = "wide"
)

stopifnot(nrow(wide) == 96L)

wide$delta_C2_C1 <-
  wide$automated_accuracy.C2 -
  wide$automated_accuracy.C1

wide$delta_C3_C1 <-
  wide$automated_accuracy.C3 -
  wide$automated_accuracy.C1

wide$delta_C3_C2 <-
  wide$automated_accuracy.C3 -
  wide$automated_accuracy.C2

wide$C3_perfect <-
  as.integer(
    abs(
      wide$automated_accuracy.C3 - 100
    ) < 1e-9
  )


# ------------------------------------------------------------
# Generic summary function
# ------------------------------------------------------------

summarise_effect <- function(z) {

  data.frame(
    n_pairs = nrow(z),

    mean_C1 =
      mean(z$automated_accuracy.C1),

    mean_C2 =
      mean(z$automated_accuracy.C2),

    mean_C3 =
      mean(z$automated_accuracy.C3),

    mean_delta_C2_C1 =
      mean(z$delta_C2_C1),

    median_delta_C2_C1 =
      median(z$delta_C2_C1),

    mean_delta_C3_C1 =
      mean(z$delta_C3_C1),

    median_delta_C3_C1 =
      median(z$delta_C3_C1),

    min_delta_C3_C1 =
      min(z$delta_C3_C1),

    max_delta_C3_C1 =
      max(z$delta_C3_C1),

    mean_delta_C3_C2 =
      mean(z$delta_C3_C2),

    median_delta_C3_C2 =
      median(z$delta_C3_C2),

    min_delta_C3_C2 =
      min(z$delta_C3_C2),

    max_delta_C3_C2 =
      max(z$delta_C3_C2),

    C3_perfect_n =
      sum(z$C3_perfect),

    C3_perfect_pct =
      100 * mean(z$C3_perfect),

    stringsAsFactors = FALSE
  )
}


# ------------------------------------------------------------
# BY MODEL
# ------------------------------------------------------------

model_split <- split(
  wide,
  wide$model_slot
)

by_model <- do.call(
  rbind,
  lapply(
    names(model_split),
    function(m) {

      z <- model_split[[m]]

      cbind(
        data.frame(
          model_slot = m,
          provider = z$provider[1],
          stringsAsFactors = FALSE
        ),
        summarise_effect(z)
      )
    }
  )
)

rownames(by_model) <- NULL

by_model <- by_model[
  order(
    -by_model$mean_delta_C3_C2
  ),
]


# ------------------------------------------------------------
# BY CASE
# ------------------------------------------------------------

case_split <- split(
  wide,
  wide$case_id
)

by_case <- do.call(
  rbind,
  lapply(
    names(case_split),
    function(k) {

      z <- case_split[[k]]

      cbind(
        data.frame(
          case_id = k,
          stringsAsFactors = FALSE
        ),
        summarise_effect(z)
      )
    }
  )
)

rownames(by_case) <- NULL

by_case <- by_case[
  order(
    -by_case$mean_delta_C3_C2
  ),
]


# ------------------------------------------------------------
# LEAVE-ONE-MODEL-OUT SENSITIVITY
# ------------------------------------------------------------

loo_model <- do.call(
  rbind,
  lapply(
    sort(unique(wide$model_slot)),
    function(m) {

      z <- wide[
        wide$model_slot != m,
      ]

      data.frame(
        excluded_model = m,
        n_pairs = nrow(z),
        mean_delta_C2_C1 =
          mean(z$delta_C2_C1),
        mean_delta_C3_C1 =
          mean(z$delta_C3_C1),
        mean_delta_C3_C2 =
          mean(z$delta_C3_C2),
        stringsAsFactors = FALSE
      )
    }
  )
)


# ------------------------------------------------------------
# LEAVE-ONE-CASE-OUT SENSITIVITY
# ------------------------------------------------------------

loo_case <- do.call(
  rbind,
  lapply(
    sort(unique(wide$case_id)),
    function(k) {

      z <- wide[
        wide$case_id != k,
      ]

      data.frame(
        excluded_case = k,
        n_pairs = nrow(z),
        mean_delta_C2_C1 =
          mean(z$delta_C2_C1),
        mean_delta_C3_C1 =
          mean(z$delta_C3_C1),
        mean_delta_C3_C2 =
          mean(z$delta_C3_C2),
        stringsAsFactors = FALSE
      )
    }
  )
)


# ------------------------------------------------------------
# C3 NON-PERFECT PAIRS
# ------------------------------------------------------------

c3_nonperfect <- wide[
  wide$automated_accuracy.C3 < 100,
  c(
    "model_slot",
    "provider",
    "case_id",
    "automated_accuracy.C1",
    "automated_accuracy.C2",
    "automated_accuracy.C3",
    "delta_C3_C1",
    "delta_C3_C2"
  )
]

c3_nonperfect <- c3_nonperfect[
  order(
    c3_nonperfect$automated_accuracy.C3,
    c3_nonperfect$model_slot,
    c3_nonperfect$case_id
  ),
]


# ------------------------------------------------------------
# GLOBAL SUMMARY
# ------------------------------------------------------------

global <- summarise_effect(wide)

model_delta_sd <- data.frame(
  metric = c(
    "SD model means C2-C1",
    "SD model means C3-C1",
    "SD model means C3-C2"
  ),
  value = c(
    sd(by_model$mean_delta_C2_C1),
    sd(by_model$mean_delta_C3_C1),
    sd(by_model$mean_delta_C3_C2)
  )
)

case_delta_sd <- data.frame(
  metric = c(
    "SD case means C2-C1",
    "SD case means C3-C1",
    "SD case means C3-C2"
  ),
  value = c(
    sd(by_case$mean_delta_C2_C1),
    sd(by_case$mean_delta_C3_C1),
    sd(by_case$mean_delta_C3_C2)
  )
)


# ------------------------------------------------------------
# SAVE
# ------------------------------------------------------------

write.csv(
  wide,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_PAIR_LEVEL_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  by_model,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_BY_MODEL_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  by_case,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_BY_CASE_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  loo_model,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_LOO_MODEL_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  loo_case,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_LOO_CASE_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  c3_nonperfect,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_C3_NONPERFECT_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  model_delta_sd,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_MODEL_SD_v1.csv"
  ),
  row.names = FALSE
)

write.csv(
  case_delta_sd,
  file.path(
    OUTDIR,
    "SMOKE_HETEROGENEITY_CASE_SD_v1.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# CONSOLE
# ------------------------------------------------------------

cat("\n")
cat("========================================\n")
cat("ERBIOBENCH SMOKE HETEROGENEITY\n")
cat("========================================\n")

cat(
  "Complete model-case pairs:",
  nrow(wide),
  "\n"
)

cat("\n")
cat("========================================\n")
cat("GLOBAL EFFECTS\n")
cat("========================================\n")

print(
  global,
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("BY MODEL\n")
cat("========================================\n")

print(
  by_model[
    ,
    c(
      "model_slot",
      "provider",
      "n_pairs",
      "mean_C1",
      "mean_C2",
      "mean_C3",
      "mean_delta_C2_C1",
      "mean_delta_C3_C1",
      "mean_delta_C3_C2",
      "C3_perfect_pct"
    )
  ],
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("BY CASE\n")
cat("========================================\n")

print(
  by_case[
    ,
    c(
      "case_id",
      "n_pairs",
      "mean_C1",
      "mean_C2",
      "mean_C3",
      "mean_delta_C2_C1",
      "mean_delta_C3_C1",
      "mean_delta_C3_C2",
      "C3_perfect_pct"
    )
  ],
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("C3 NON-PERFECT COMPLETE PAIRS\n")
cat("========================================\n")

print(
  c3_nonperfect,
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("LEAVE-ONE-MODEL-OUT RANGE\n")
cat("========================================\n")

cat(
  "C2-C1:",
  range(
    loo_model$mean_delta_C2_C1
  ),
  "\n"
)

cat(
  "C3-C1:",
  range(
    loo_model$mean_delta_C3_C1
  ),
  "\n"
)

cat(
  "C3-C2:",
  range(
    loo_model$mean_delta_C3_C2
  ),
  "\n"
)

cat("\n")
cat("========================================\n")
cat("LEAVE-ONE-CASE-OUT RANGE\n")
cat("========================================\n")

cat(
  "C2-C1:",
  range(
    loo_case$mean_delta_C2_C1
  ),
  "\n"
)

cat(
  "C3-C1:",
  range(
    loo_case$mean_delta_C3_C1
  ),
  "\n"
)

cat(
  "C3-C2:",
  range(
    loo_case$mean_delta_C3_C2
  ),
  "\n"
)

cat("\n")
cat("========================================\n")
cat("SD OF MEAN EFFECTS ACROSS MODELS\n")
cat("========================================\n")

print(
  model_delta_sd,
  row.names = FALSE
)

cat("\n")
cat("========================================\n")
cat("SD OF MEAN EFFECTS ACROSS CASES\n")
cat("========================================\n")

print(
  case_delta_sd,
  row.names = FALSE
)

cat("\nSMOKE_HETEROGENEITY_DONE\n")
