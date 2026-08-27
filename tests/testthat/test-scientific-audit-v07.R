test_that("v0.7 Spanish legal-source audit metadata and corrections are preserved", {
  reg <- erbio_load_agent_registry()
  expect_equal(nrow(reg), 509L)
  expect_true(all(reg$legal_primary_source_id == "BOE-A-2021-19371"))
  expect_true(all(reg$spanish_row_audit_status == "verified_semantic_against_official_order_2021"))
  expect_true(all(reg$audit_group_consistency))
  expect_true(all(reg$audit_double_asterisk_consistency))
  expect_true(all(reg$audit_flag_A_consistency))
  expect_true(all(reg$audit_flag_D_consistency))
  expect_true(all(reg$audit_flag_T_consistency))
  expect_true(all(reg$audit_flag_V_consistency))
  expect_equal(sum(reg$legal_name_correction_status != "none"), 3L)

  expect_equal(
    reg$legal_name_es_raw[reg$agent_id == "BIO-VIR-0063"],
    "Coronavirus del s\u00edndrome respiratorio agudo grave (SARS-CoV)."
  )
  expect_equal(
    reg$legal_name_es_raw[reg$agent_id == "BIO-VIR-0064"],
    "Coronavirus del s\u00edndrome respiratorio agudo grave 2 (SARS-CoV-2) (1)."
  )
  expect_equal(
    reg$legal_name_es_raw[reg$agent_id == "BIO-VIR-0065"],
    "Coronavirus del s\u00edndrome respiratorio de Oriente Medio (MERS-CoV)."
  )
})

test_that("v0.7 BASEBiO false-positive rejection and reviewed aliases are explicit", {
  reg <- erbio_load_agent_registry()
  bad <- reg[reg$agent_id == "BIO-VIR-0080", , drop = FALSE]
  expect_false(bad$basebio_available[[1]])
  expect_equal(bad$basebio_match_status[[1]], "rejected_false_positive")
  expect_equal(bad$basebio_review_status[[1]], "rejected_false_positive_taxonomic_family_mismatch")
  expect_equal(bad$basebio_rejected_candidate_name[[1]], "Otros Coronaviridae de patogenicidad conocida")
  expect_equal(sum(reg$basebio_available), 147L)

  bb <- erbio_load_basebio_index()
  cor <- bb[bb$basebio_name == "Otros Coronaviridae de patogenicidad conocida", , drop = FALSE]
  expect_equal(cor$legal_agent_ids[[1]], "BIO-VIR-0066")
  expect_equal(as.integer(cor$legal_link_count[[1]]), 1L)

  expect_equal(reg$basebio_review_status[reg$agent_id == "BIO-VIR-0086"], "manual_alias_verified_nomenclature_discrepancy")
  expect_equal(reg$basebio_review_status[reg$agent_id == "BIO-PAR-0075"], "manual_alias_verified_lifecycle_expansion")
  expect_equal(reg$basebio_review_status[reg$agent_id == "BIO-FUNG-0011"], "manual_alias_verified_abbreviation_expansion")
})

test_that("v0.7 does not overclaim independent EU rowwise verification", {

  reg_path <- system.file(
    "extdata",
    "ERBioR_agent_registry_v0_7.csv",
    package = "ERBioR"
  )

  expect_true(nzchar(reg_path))
  expect_true(file.exists(reg_path))

  reg <- utils::read.csv(
    reg_path,
    stringsAsFactors = FALSE,
    check.names = FALSE,
    fileEncoding = "UTF-8-BOM"
  )

  eu_verified <- tolower(as.character(reg$eu_rowwise_verified)) %in%
    c("true", "t", "1", "yes", "si", "sí")

  expect_false(any(eu_verified))

  expect_true(all(
    reg$eu_rowwise_audit_status ==
      "pending_independent_rowwise_eurlex_verification"
  ))

  audit_path <- system.file(
    "extdata",
    "ERBioR_agent_scientific_audit_v0_7.csv",
    package = "ERBioR"
  )

  expect_true(nzchar(audit_path))
  expect_true(file.exists(audit_path))

})
