# ERBioR Shiny launcher -------------------------------------------------------
erbio_run_app <- function(launch.browser = TRUE, ...) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop("Package 'shiny' is required to run the ERBioR app.", call. = FALSE)
  }
  app_dir <- system.file("shiny", "ERBioR", package = "ERBioR")
  if (!nzchar(app_dir) || !dir.exists(app_dir)) {
    stop("ERBioR Shiny application files were not found in the installed package.", call. = FALSE)
  }
  shiny::runApp(app_dir, launch.browser = launch.browser, ...)
}

#' Return the installed ERBioR Shiny build identifier
#' @export
erbio_shiny_build_id <- function() {
  f <- system.file("shiny", "ERBioR", "BUILD_ID.txt", package = "ERBioR")
  if (!nzchar(f) || !file.exists(f)) return(NA_character_)
  trimws(readLines(f, warn = FALSE, n = 1L))
}
