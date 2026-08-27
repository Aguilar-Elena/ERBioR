# ERBioR cross-language Excel smoke test
# Verifies that a Spanish workbook can be parsed while output language is English.

ERBioR::erbio_set_language("en")
app_dir <- system.file("shiny", "ERBioR", package = "ERBioR")
source(file.path(app_dir, "R", "excel_io.R"), local = .GlobalEnv)

f <- system.file("templates", "ERBioR_Plantilla_Depuradoras_CORREGIDA_v0_3.xlsx", package = "ERBioR")
stopifnot(nzchar(f), file.exists(f))

p <- erbio_read_excel_evaluation(f)
stopifnot(p$input_language %in% c("es", "unknown"))
stopifnot(all(unlist(p$questionnaire_responses_nonworkers, use.names = FALSE) %in% c("Si", "No", "No procede")))
stopifnot(ERBioR::erbio_get_language() == "en")

cat("CROSS-LANGUAGE EXCEL IMPORT SMOKE TEST PASSED\n")
