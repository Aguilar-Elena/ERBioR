test_that("v0.9 recovered preventive registry is complete", {
  v <- erbio_validate_preventive_action_registry()
  expect_true(v$valid)
  expect_equal(v$n_actions, 1008L)
  expect_equal(v$n_approved_es, 1008L)
  expect_equal(v$n_source_text_mismatches, 0L)
})

test_that("v0.9 recovered English registry is complete", {
  v <- erbio_validate_translation_registry()
  expect_true(v$valid)
  expect_equal(v$n_items, 1008L)
  expect_equal(v$n_question_approved_en, 1008L)
  expect_equal(v$n_preventive_approved_en, 1008L)
  expect_equal(v$n_missing_required_english_rows, 0L)
  expect_false(v$psychometric_validation_implied)
})

test_that("ES and EN presentation preserve scientific scoring", {
  q <- erbio_get_questionnaire("general")
  r <- rep("Si", nrow(q))
  r[28] <- "No"
  z <- erbio_bilingual_invariance_check("general", r)
  expect_true(z$invariant)
})


test_that("v0.9 preserves source planning candidates while using approved expert actions", {
  r <- rep("Si", 48)
  r[28] <- "No"
  qs <- erbio_score_questionnaire("general", r, language = "es")
  expect_equal(nrow(qs$failed_controls), 1L)
  expect_identical(qs$failed_controls$planning_candidate, qs$failed_controls$item_text)
  expect_true(nzchar(qs$failed_controls$expert_preventive_action[[1]]))
  expect_false(identical(
    qs$failed_controls$expert_preventive_action[[1]],
    qs$failed_controls$item_text[[1]]
  ))

  risk_results <- data.frame(
    questionnaire_id = "general",
    agent_name = "Mycobacterium tuberculosis",
    risk_class = "Moderado",
    priority = "Media",
    risk_score = 6L,
    stringsAsFactors = FALSE
  )
  plan <- erbio_build_preventive_plan(
    questionnaire_scores = list(general = qs),
    risk_results = risk_results
  )
  expect_equal(nrow(plan), 1L)
  expect_identical(
    plan$preventive_measure_candidate[[1]],
    qs$failed_controls$expert_preventive_action[[1]]
  )
})
