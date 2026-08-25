test_that("pre-CRAN provenance assets are installed", {
  manifest <- system.file("extdata", "ERBioR_source_manifest_v0_8.csv", package = "ERBioR")
  audit <- system.file("extdata", "ERBioR_source_audit_summary_v0_8.csv", package = "ERBioR")
  eu_audit <- system.file("extdata", "ERBioR_agent_eu_audit_v0_8.csv", package = "ERBioR")
  expect_true(nzchar(manifest) && file.exists(manifest))
  expect_true(nzchar(audit) && file.exists(audit))
  expect_true(nzchar(eu_audit) && file.exists(eu_audit))

  src <- utils::read.csv(manifest, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8-BOM")
  expect_true(any(src$doi == "10.5281/zenodo.22069658"))
  expect_true(any(src$source_id == "DOUE-L-2019-81658" & grepl("508/508", src$rowwise_verification, fixed = TRUE)))
  expect_true(any(src$source_id == "DOUE-L-2020-80871" & grepl("1/1", src$rowwise_verification, fixed = TRUE)))
})
