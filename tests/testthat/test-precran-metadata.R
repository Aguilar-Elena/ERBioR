test_that("pre-CRAN provenance assets are installed", {
  manifest <- system.file("extdata", "ERBioR_source_manifest_v0_7.csv", package = "ERBioR")
  audit <- system.file("extdata", "ERBioR_source_audit_summary_v0_7.csv", package = "ERBioR")
  expect_true(nzchar(manifest))
  expect_true(file.exists(manifest))
  expect_true(nzchar(audit))
  expect_true(file.exists(audit))

  src <- utils::read.csv(manifest, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  expect_true(any(src$doi == "10.5281/zenodo.22107465"))
  expect_true(any(src$source_id == "DIR2019-1833-ANNEX-III" & grepl("pending", src$rowwise_verification, fixed = TRUE)))
})
