# ERBioR v0.9.0.9000 pre-benchmark verification
# Run from RStudio/R on the machine where the package will be frozen.

PKG <- normalizePath(
  file.path(getwd(), "ERBioR"),
  winslash = "/",
  mustWork = TRUE
)

cat("\nPACKAGE:\n", PKG, "\n", sep = "")

TEMP_LIB <- file.path(tempdir(), "ERBioR_v09_prebenchmark_lib")
dir.create(TEMP_LIB, recursive = TRUE, showWarnings = FALSE)

R_EXE <- if (.Platform$OS.type == "windows") {
  file.path(R.home("bin"), "R.exe")
} else {
  file.path(R.home("bin"), "R")
}

cat("\n1) CLEAN INSTALL\n")
install_status <- system2(
  R_EXE,
  c("CMD", "INSTALL", "--no-multiarch", "-l", shQuote(TEMP_LIB), shQuote(PKG))
)
if (!identical(install_status, 0L)) stop("R CMD INSTALL failed.")

.libPaths(c(TEMP_LIB, .libPaths()))
suppressPackageStartupMessages(library(ERBioR, lib.loc = TEMP_LIB))

cat("Installed version: ", as.character(packageVersion("ERBioR")), "\n", sep = "")
stopifnot(as.character(packageVersion("ERBioR")) == "0.9.0.9000")

cat("\n2) PREVENTIVE REGISTRY VALIDATION\n")
v <- erbio_validate_preventive_action_registry()
print(v)
stopifnot(
  isTRUE(v$valid),
  v$n_actions == 1008L,
  v$n_approved_es == 1008L,
  v$n_source_text_mismatches == 0L
)

cat("\n3) FULL TESTTHAT SUITE\n")
if (!requireNamespace("testthat", quietly = TRUE)) {
  stop("Install testthat first: install.packages('testthat')")
}
res <- testthat::test_dir(file.path(PKG, "tests", "testthat"), reporter = "summary")
if (any(vapply(res, function(x) inherits(x, "testthat_results") && FALSE, logical(1)))) {
  stop("Unexpected testthat result structure.")
}

cat("\n4) FOCUSED MANUAL SMOKE TEST\n")
r <- rep("Si", 48)
r[28] <- "No"
s <- erbio_score_questionnaire("general", r)
print(s$failed_controls[, c(
  "item_id", "item_text", "preventive_taxonomy",
  "preventive_prescription_level", "expert_preventive_action_es"
)])

stopifnot(
  nrow(s$failed_controls) == 1L,
  s$failed_controls$item_id[[1]] == "GENERAL-F2015-028",
  nzchar(s$failed_controls$expert_preventive_action_es[[1]]),
  s$failed_controls$expert_preventive_action_es[[1]] != s$failed_controls$item_text[[1]]
)

cat("\nPRE-BENCHMARK PACKAGE TEST SCRIPT COMPLETED.\n")
cat("Next: run the real audit kit and R CMD check --as-cran on this exact build.\n")
