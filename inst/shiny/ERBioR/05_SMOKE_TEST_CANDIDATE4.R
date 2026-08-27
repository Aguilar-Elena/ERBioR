suppressPackageStartupMessages(library(ERBioR))
app_dir <- system.file("shiny", "ERBioR", package = "ERBioR")
source(file.path(app_dir, "R", "excel_io.R"), local = .GlobalEnv)

ERBioR::erbio_set_language("en")
r <- erbio_prepare_agent_resolution(c("Brucella Abortus", "Mycobacterium tuberculosis"))
stopifnot(r[[1]]$status == "exact")
stopifnot(r[[2]]$status == "exact")
stopifnot(identical(r[[2]]$resolved_agent, "Mycobacterium tuberculosis"))

tr <- ERBioR::erbio_load_translation_registry()
stopifnot(nrow(tr) == 1008L)
stopifnot(all(nzchar(tr$question_text_en)))
cat("CANDIDATE4 STATIC/AGENT I18N SMOKE PASSED\n")
