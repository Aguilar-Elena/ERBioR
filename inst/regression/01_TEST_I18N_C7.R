# ERBioR C7 i18n source audit (does not replace runtime Shiny tests)
app <- system.file("shiny", "ERBioR", "app.R", package = "ERBioR")
if (!nzchar(app)) stop("Bundled Shiny app.R not found")
txt <- paste(readLines(app, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
required_en <- c(
  "Highest observed risk by agent",
  "Questionnaire status",
  "Professional review notices",
  "No worker results yet.",
  "Expert preventive planning",
  "The output separates the cause, corrective action, implementation requirements and effectiveness verification.",
  "No exact match was found. Select one of the suggested matches:",
  "Agents entered:",
  "Workers:",
  "Processing…"
)
missing <- required_en[!vapply(required_en, grepl, logical(1), x = txt, fixed = TRUE)]
if (length(missing)) stop("Missing EN UI strings: ", paste(missing, collapse = " | "))
cat("C7 I18N SOURCE AUDIT PASSED\n")
