test_that("installed package finds its internal extdata", {
  old <- getOption("erbio.data_dir")
  on.exit(options(erbio.data_dir = old), add = TRUE)
  options(erbio.data_dir = NULL)
  expect_equal(nrow(erbio_get_questionnaire("general")), 48L)
  expect_equal(erbio_reference_level_from_agent("Mycobacterium tuberculosis")$reference_level, 3L)
  expect_equal(erbio_version(), "0.9.0")
})
