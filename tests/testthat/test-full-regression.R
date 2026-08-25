test_that("frozen v0.4 full regression suite", {
  options(erbio.data_dir = system.file("extdata", package = "ERBioR"))

  ###############################################################################
  # ERBioR tests v0.4
  # Full regression through v0.3 + workplace-layer validation for ERBioR_core_v0_4.R
  #
  # Run from a folder containing:
  #   ERBioR_core_v0_4.R
  #   ERBioR_question_bank_v0_2.csv
  #   ERBioR_questionnaire_registry_v0_2.csv
  ###############################################################################


  assert_equal <- function(actual, expected, label = "") {
    ok <- isTRUE(all.equal(actual, expected, check.attributes = FALSE))
    if (!ok) {
      stop(
        "FAIL: ", label,
        " | expected=", paste(expected, collapse = ","),
        " actual=", paste(actual, collapse = ","),
        call. = FALSE
      )
    }
    invisible(TRUE)
  }

  assert_true <- function(x, label = "") {
    if (!isTRUE(x)) stop("FAIL: ", label, call. = FALSE)
    invisible(TRUE)
  }

  assert_error <- function(expr, pattern = NULL, label = "") {
    got <- NULL
    tryCatch(
      force(expr),
      error = function(e) got <<- conditionMessage(e)
    )
    if (is.null(got)) stop("FAIL: expected error: ", label, call. = FALSE)
    if (!is.null(pattern) && !grepl(pattern, got, ignore.case = TRUE)) {
      stop("FAIL: wrong error for ", label, " | got: ", got, call. = FALSE)
    }
    invisible(TRUE)
  }

  cat("[1/15] v0.1 regression: probability matrix, all 20 cells...\n")
  expected_prob <- list(
    "Muy deficiente" = c("Continuo"=4L,"Muy frecuente"=4L,"Frecuente"=3L,"Irregular"=3L,"Ocasional"=2L),
    "Deficiente"     = c("Continuo"=4L,"Muy frecuente"=3L,"Frecuente"=3L,"Irregular"=2L,"Ocasional"=1L),
    "Mejorable"      = c("Continuo"=3L,"Muy frecuente"=2L,"Frecuente"=2L,"Irregular"=1L,"Ocasional"=1L),
    "Aceptable"      = c("Continuo"=2L,"Muy frecuente"=2L,"Frecuente"=1L,"Irregular"=1L,"Ocasional"=1L)
  )
  for (cc in names(expected_prob)) {
    for (ex in names(expected_prob[[cc]])) {
      assert_equal(
        erbio_probability(cc, ex)$value,
        unname(expected_prob[[cc]][[ex]]),
        paste(cc, ex)
      )
    }
  }

  cat("[2/15] v0.1 regression: risk matrix, all 16 combinations...\n")
  expected_class_by_score <- c(
    "1"="Trivial", "2"="Tolerable", "3"="Tolerable", "4"="Moderado",
    "6"="Moderado", "8"="Importante", "9"="Importante",
    "12"="Intolerable", "16"="Intolerable"
  )
  for (r in 1:4) {
    for (p in 1:4) {
      s <- r * p
      assert_equal(erbio_risk_score(p, r), as.integer(s), paste("score", r, p))
      assert_equal(
        erbio_risk_class(s),
        unname(expected_class_by_score[[as.character(s)]]),
        paste("class", r, p)
      )
    }
  }
  assert_equal(erbio_probability("Mejorable", "Irregular")$value, 1L, "legacy correction")

  cat("[3/15] Question-bank structural validation...\n")
  v <- erbio_validate_question_bank()
  assert_true(v$valid, "question bank valid")
  assert_equal(v$n_questionnaires, 27L, "27 active questionnaires")
  assert_equal(v$n_items, 1008L, "1008 source-preserved final items")
  assert_equal(length(v$issues), 0L, "no bank issues")

  cat("[4/15] Core and sector questionnaire counts...\n")
  cat0 <- erbio_questionnaire_catalog()
  assert_equal(nrow(cat0), 27L, "catalog rows")
  assert_equal(cat0$n_items[cat0$questionnaire_id == "audit"], 32L, "audit final n")
  assert_equal(cat0$n_items[cat0$questionnaire_id == "general"], 48L, "general final n")
  assert_equal(cat0$n_items[cat0$questionnaire_id == "workers"], 34L, "workers final n")
  assert_equal(
    sum(cat0$n_items[cat0$instrument_type == "sector_specific_questionnaire"]),
    894L,
    "sector items total"
  )
  assert_equal(
    sum(cat0$instrument_type == "sector_specific_questionnaire"),
    24L,
    "24 sector questionnaires"
  )

  cat("[5/15] Stable IDs and final numbering...\n")
  qa <- erbio_get_questionnaire("audit")
  qg <- erbio_get_questionnaire("general")
  qw <- erbio_get_questionnaire("workers")
  assert_equal(qa$item_id[[1]], "AUDIT-F2015-001", "audit first stable ID")
  assert_equal(qg$item_id[[1]], "GENERAL-F2015-001", "general first stable ID")
  assert_equal(qw$item_id[[1]], "WORKERS-F2015-001", "workers first stable ID")
  assert_equal(qa$final_item, 1:32, "audit numbering")
  assert_equal(qg$final_item, 1:48, "general numbering")
  assert_equal(qw$final_item, 1:34, "workers numbering")

  cat("[6/15] Annex-3 general example: 33 yes, 7 no, 8 N/A => 82.5%...\n")
  general_responses <- c(rep("Si", 33), rep("No", 7), rep("No procede", 8))
  gs <- erbio_score_questionnaire("general", general_responses)
  assert_equal(gs$compliance_percent, 82.5, "33/(33+7)")
  assert_equal(gs$scoring_detail$n_applicable, 40L, "40 applicable")
  assert_equal(gs$scoring_detail$n_not_applicable, 8L, "8 N/A")
  assert_equal(nrow(gs$failed_controls), 7L, "7 failed controls")
  assert_true(all(gs$failed_controls$planning_candidate == gs$failed_controls$item_text), "planning candidates preserve source item text")

  cat("[7/15] Complete questionnaire assessment...\n")
  ga <- erbio_assess_questionnaire(
    "general",
    general_responses,
    reference_level = 3,
    exposure = "Muy frecuente"
  )
  assert_equal(ga$compliance_class, "Aceptable", "general compliance class")
  assert_equal(ga$probability_value, 2L, "Aceptable + Muy frecuente = Media/2")
  assert_equal(ga$risk_score, 6L, "NR3 x P2 = 6")
  assert_equal(ga$risk_class, "Moderado", "GRB6 = Moderado")
  assert_equal(nrow(ga$failed_controls), 7L, "assessment retains failed controls")
  assert_equal(ga$validation_status, "validated_final", "core questionnaire validation status")

  cat("[8/15] Named responses by stable item ID are order-independent...\n")
  set.seed(2015)
  idx <- sample(seq_along(general_responses))
  named_responses <- general_responses
  names(named_responses) <- qg$item_id
  named_responses <- named_responses[idx]
  gs_named <- erbio_score_questionnaire("general", named_responses)
  assert_equal(gs_named$compliance_percent, 82.5, "stable-ID alignment")
  assert_equal(nrow(gs_named$failed_controls), 7L, "stable-ID failed controls")

  cat("[9/15] Named responses by final item number are accepted...\n")
  num_responses <- general_responses
  names(num_responses) <- as.character(qg$final_item)
  num_responses <- num_responses[rev(seq_along(num_responses))]
  gs_num <- erbio_score_questionnaire("general", num_responses)
  assert_equal(gs_num$compliance_percent, 82.5, "final-number alignment")

  cat("[10/15] Error handling: N/A-only, wrong length, wrong version, wrong scoring...\n")
  assert_error(
    erbio_score_questionnaire("workers", rep("No procede", 34)),
    "No hay items aplicables",
    "workers all N/A"
  )
  assert_error(
    erbio_score_questionnaire("general", rep("Si", 47)),
    "requiere 48 respuestas",
    "wrong response length"
  )
  assert_error(
    erbio_get_questionnaire("general", version = "version_inexistente"),
    "No existe la version",
    "unknown version"
  )
  assert_error(
    erbio_score_questionnaire("general", rep(4, 48), scoring = "audit_0_4"),
    "no permitido",
    "general must not use audit 0-4"
  )

  cat("[11/15] Audit supports both binary and source-preserved 0-4 semantics...\n")
  as_bin <- erbio_score_questionnaire("audit", rep("Si", 32), scoring = "binary")
  assert_equal(as_bin$compliance_percent, 100, "audit binary 100%")
  as_04 <- erbio_score_questionnaire("audit", rep(4, 32), scoring = "audit_0_4")
  assert_equal(as_04$compliance_percent, 100, "audit 0-4 100%")
  as_04b <- erbio_score_questionnaire("audit", c(3, rep(4,31)), scoring = "audit_0_4")
  assert_equal(nrow(as_04b$failed_controls), 1L, "audit score below 4 becomes failed control")
  assert_equal(as_04b$failed_controls$final_item[[1]], 1L, "audit failed first item")

  cat("[12/15] Sector-specific instruments remain explicitly unvalidated...\n")
  qs <- erbio_get_questionnaire("sec_wastewater")
  assert_equal(nrow(qs), 12L, "wastewater sector n")
  sa <- erbio_assess_questionnaire(
    "sec_wastewater",
    rep("Si", 12),
    reference_level = 2,
    exposure = "Ocasional"
  )
  assert_equal(sa$validation_status, "not_validated_insufficient_sector_sample", "sector validation status")
  assert_true(any(grepl("no fue validado", sa$warnings, ignore.case = TRUE)), "sector warning present")

  cat("[13/15] Questionnaire-level source rules are exposed...\n")
  rules <- erbio_questionnaire_rules("audit")
  assert_true(all(c("AUD-G1-STOP", "AUD-NONDELIBERATE-SCOPE", "NA-DENOM") %in% rules$rule_id), "audit rules")

  cat("[14/15] Set aggregation regression: unanimous vs discordant...\n")
  a1 <- erbio_assess(reference_level = 3, exposure = "Muy frecuente", compliance_percent = 82.5)
  a2 <- erbio_assess(reference_level = 3, exposure = "Frecuente", compliance_percent = 60)
  # Both are Moderado: 3x2=6
  u <- erbio_assess_set(list(a1, a2))
  assert_equal(u$global_risk_class, "Moderado", "unanimous class")
  assert_equal(u$aggregation_status, "unanimous_questionnaire_classes", "unanimous status")
  a3 <- erbio_assess(reference_level = 4, exposure = "Continuo", compliance_percent = 10)
  d <- erbio_assess_set(list(a1, a3))
  assert_true(is.na(d$global_risk_class), "discordant global class unresolved")
  assert_equal(d$aggregation_status, "source_rule_unresolved_for_discordant_questionnaires", "discordant status")

  cat("[15/15] Registry preserves validation lineage...\n")
  reg <- erbio_load_questionnaire_registry()
  assert_true(any(reg$questionnaire_id == "audit" & reg$n_items == 38), "audit pre-elimination n38")
  assert_true(any(reg$questionnaire_id == "audit" & reg$n_items == 32), "audit final n32")
  assert_true(any(reg$questionnaire_id == "general" & reg$n_items == 55), "general pre-elimination n55")
  assert_true(any(reg$questionnaire_id == "general" & reg$n_items == 48), "general final n48")
  assert_true(any(reg$questionnaire_id == "workers" & reg$n_items == 34), "workers n34")
  assert_true(any(reg$questionnaire_id == "audit" & abs(reg$cronbach_alpha - 0.814) < 1e-12, na.rm = TRUE), "audit final alpha")
  assert_true(any(reg$questionnaire_id == "general" & abs(reg$cronbach_alpha - 0.779) < 1e-12, na.rm = TRUE), "general final alpha")

  cat("\nERBioR v0.2 regression layer passed within v0.3 suite\n")


  cat("[16/25] Agent-registry structural validation...\n")
  av <- erbio_validate_agent_registry()
  assert_true(av$valid, "agent registry valid")
  assert_true(av$n_agents > 300L, "substantial Annex II registry")
  assert_equal(length(av$issues), 0L, "no agent-registry issues")

  cat("[17/25] Legal sentinel agents and groups...\n")
  assert_equal(erbio_reference_level_from_agent("Bacillus anthracis")$reference_level, 3L, "B anthracis group")
  assert_equal(erbio_reference_level_from_agent("Mycobacterium tuberculosis")$reference_level, 3L, "M tuberculosis group")
  assert_equal(erbio_reference_level_from_agent("Nairovirus de la fiebre hemorrágica de Crimea-Congo")$reference_level, 4L, "CCHF group")
  assert_equal(erbio_reference_level_from_agent("Virus de la viruela (mayor & menor)")$reference_level, 4L, "variola group")
  assert_equal(erbio_reference_level_from_agent("Aspergillus fumigatus")$reference_level, 2L, "A fumigatus group")
  assert_equal(erbio_reference_level_from_agent("Coronavirus del síndrome respiratorio agudo grave 2 (SARS-CoV-2)")$reference_level, 3L, "SARS-CoV-2 BASEBiO alias lookup")

  cat("[18/25] Legal A/D/T/V flags and ** semantics...\n")
  ba <- erbio_reference_level_from_agent("Bacillus anthracis")
  assert_true(unname(ba$flags[["T"]]), "B anthracis toxin flag")
  mtb <- erbio_reference_level_from_agent("Mycobacterium tuberculosis")
  assert_true(unname(mtb$flags[["V"]]), "M tuberculosis vaccine flag")
  ts <- erbio_reference_level_from_agent("Taenia solium")
  assert_equal(ts$reference_level, 3L, "T solium group")
  assert_true(ts$group3_limited_airborne, "T solium double-asterisk flag")
  af <- erbio_reference_level_from_agent("Aspergillus fumigatus")
  assert_true(unname(af$flags[["A"]]), "A fumigatus allergy flag")

  cat("[19/25] Unlisted agent is NOT silently group 1...\n")
  un <- erbio_reference_level_from_agent("Agente completamente inexistente XYZ")
  assert_true(is.na(un$reference_level), "unlisted reference NA")
  assert_equal(un$classification_status, "unclassified_not_implicitly_group1", "unlisted status")
  assert_true(un$requires_professional_assessment, "unlisted professional review")

  cat("[20/25] Unclassified human virus exposes regulatory minimum group 2 without final classification...\n")
  uv <- erbio_unclassified_human_virus("Novel human virus X")
  assert_true(is.na(uv$reference_level), "novel virus final reference unresolved")
  assert_equal(uv$regulatory_minimum_group, 2L, "novel human virus minimum")
  assert_true(uv$requires_professional_assessment, "novel virus professional assessment")

  cat("[21/25] Exact vs partial lookup behavior...\n")
  assert_equal(nrow(erbio_agent_lookup("Bacillus anthracis")), 1L, "exact lookup")
  assert_true(nrow(erbio_agent_lookup("Borrelia", exact=FALSE)) >= 4L, "partial Borrelia lookup")
  assert_equal(nrow(erbio_agent_lookup("Bacillus anthracis", type="bacteria")), 1L, "typed lookup")

  cat("[22/25] Multiple-agent logic does not silently collapse discordant groups...\n")
  ma_same <- erbio_reference_level_from_agents(c("Bacillus anthracis", "Coxiella burnetii"))
  assert_equal(ma_same$reference_level, 3L, "same-group multiagent")
  ma_diff <- erbio_reference_level_from_agents(c("Bacillus anthracis", "Bordetella pertussis"))
  assert_true(is.na(ma_diff$reference_level), "discordant multiagent unresolved")
  assert_equal(ma_diff$selection_status, "reported_without_collapse", "discordant policy")
  ma_max <- erbio_reference_level_from_agents(c("Bacillus anthracis", "Bordetella pertussis"), policy="explicit_max_group")
  assert_equal(ma_max$reference_level, 3L, "explicit max group")
  assert_equal(ma_max$selection_status, "explicit_conservative_max_group_policy", "explicit policy trace")

  cat("[23/25] BASEBiO technical index and separation from legal group...\n")
  bb <- erbio_load_basebio_index()
  assert_equal(nrow(bb), 175L, "BASEBiO public index 175")
  assert_true(erbio_reference_level_from_agent("Bacillus anthracis")$basebio_available, "B anthracis BASEBiO link")
  assert_equal(erbio_reference_level_from_agent("Bacillus anthracis")$reference_level, 3L, "legal group unaffected by BASEBiO")

  cat("[24/25] Source rules are machine-readable...\n")
  arules <- erbio_agent_rules()
  assert_true(all(c("UNLISTED-NOT-G1","HUMAN-VIRUS-MIN-G2","GROUP3-LIMITED-AIRBORNE","HEALTHY-WORKER-BASELINE","MULTI-AGENT-ALL-HAZARDS","BASEBIO-TECHNICAL-NONOVERRIDE") %in% arules$rule_id), "core regulatory rules")

  cat("[25/25] End-to-end agent -> reference level -> questionnaire -> GRB...\n")
  resp <- c(rep("Si",33), rep("No",7), rep("No procede",8))
  e2e <- erbio_assess_agent_questionnaire(
    agent="Mycobacterium tuberculosis",
    questionnaire_id="general",
    responses=resp,
    exposure="Muy frecuente"
  )
  assert_equal(e2e$reference_level, 3L, "agent-derived NR")
  assert_equal(e2e$compliance_percent, 82.5, "questionnaire score")
  assert_equal(e2e$probability_value, 2L, "probability")
  assert_equal(e2e$risk_score, 6L, "GRB")
  assert_equal(e2e$risk_class, "Moderado", "risk class")
  assert_equal(e2e$agent_name, "Mycobacterium tuberculosis", "agent provenance")

  cat("\nERBioR v0.3 regression layer passed within v0.4 suite\n")

  # -----------------------------------------------------------------------------
  # v0.4 workplace-level layer
  # -----------------------------------------------------------------------------

  cat("[26/38] Sector questionnaire catalog and workplace rule registry...\n")
  scat <- erbio_sector_questionnaires()
  assert_equal(nrow(scat), 24L, "24 sector questionnaires exposed")
  wrules <- erbio_workplace_rules()
  assert_true(all(c("WP-CORE-PLUS-SECTOR","WP-NO-SILENT-Q-AGG","FAILED-CONTROL-PLANNING-CANDIDATE") %in% wrules$rule_id), "workplace rules")

  cat("[27/38] Integral questionnaire-set validation requires core + sector...\n")
  wp_responses <- list(
    audit = rep("Si", 32),
    general = c(rep("Si",33), rep("No",7), rep("No procede",8)),
    workers = c(rep("Si",26), rep("No",8)),
    sec_wastewater = c(rep("Si",10), rep("No",2))
  )
  qv <- erbio_validate_workplace_questionnaires(wp_responses)
  assert_true(qv$valid, "integral questionnaire selection valid")
  assert_equal(length(qv$core_present), 3L, "three core questionnaires")
  assert_equal(qv$sector_questionnaire_ids, "sec_wastewater", "sector questionnaire")

  cat("[28/38] Missing sector questionnaire is rejected by default...\n")
  assert_error(
    erbio_validate_workplace_questionnaires(wp_responses[c("audit","general","workers")]),
    "al menos un cuestionario sectorial",
    "missing sector"
  )

  cat("[29/38] Unknown questionnaire ID is rejected...\n")
  bad_q <- wp_responses
  bad_q[["sec_nonexistent"]] <- rep("Si", 3)
  assert_error(erbio_validate_workplace_questionnaires(bad_q), "no reconocido", "unknown questionnaire")

  cat("[30/38] Single-agent workplace assessment runs end-to-end...\n")
  wp1 <- erbio_assess_workplace(
    activity = "Tratamiento de aguas residuales",
    agents = "Mycobacterium tuberculosis",
    questionnaire_responses = wp_responses,
    exposure = "Frecuente"
  )
  assert_true(inherits(wp1, "erbio_workplace_assessment"), "workplace class")
  assert_equal(nrow(wp1$agent_summary), 1L, "single agent row")
  assert_equal(nrow(wp1$questionnaire_summary), 4L, "four questionnaire rows")
  assert_equal(nrow(wp1$risk_results), 4L, "four agent-questionnaire risk rows")
  assert_equal(wp1$agent_summary$reference_level[[1]], 3L, "agent-derived NR")

  cat("[31/38] Unanimous questionnaire classes can surface for one agent only...\n")
  wp_unanimous_responses <- list(
    audit = rep("Si", 32),
    general = rep("Si", 48),
    workers = rep("Si", 34),
    sec_wastewater = rep("Si", 12)
  )
  wp_u <- erbio_assess_workplace(
    activity = "Tratamiento de aguas residuales",
    agents = "Mycobacterium tuberculosis",
    questionnaire_responses = wp_unanimous_responses,
    exposure = "Frecuente"
  )
  assert_equal(wp_u$workplace_global_risk_class, "Tolerable", "single-agent unanimous global class")
  assert_equal(wp_u$workplace_global_risk_score, 3L, "single-agent unanimous global score")
  assert_equal(wp_u$workplace_aggregation_status, "single_agent_unanimous_questionnaire_result", "single-agent status")

  cat("[32/38] Questionnaire discordance remains unresolved in workplace default...\n")
  wp_discordant_responses <- wp_unanimous_responses
  wp_discordant_responses$workers <- c(rep("Si",17), rep("No",17))
  wp_d <- erbio_assess_workplace(
    activity = "Tratamiento de aguas residuales",
    agents = "Mycobacterium tuberculosis",
    questionnaire_responses = wp_discordant_responses,
    exposure = "Frecuente"
  )
  assert_true(is.na(wp_d$workplace_global_risk_class), "discordant workplace global unresolved")
  assert_equal(wp_d$assessment_status, "complete_inputs_questionnaire_discordance_preserved", "discordance status")
  assert_true(any(grepl("discordantes", wp_d$warnings, ignore.case=TRUE)), "discordance warning")

  cat("[33/38] Multiple agents are assessed separately without silent workplace collapse...\n")
  wp_multi <- erbio_assess_workplace(
    activity = "Operación con exposición biológica mixta",
    agents = c("Nairovirus de la fiebre hemorrágica de Crimea-Congo", "Bordetella pertussis"),
    questionnaire_responses = wp_unanimous_responses,
    exposure = "Frecuente"
  )
  assert_equal(nrow(wp_multi$agent_summary), 2L, "two agents")
  assert_equal(nrow(wp_multi$risk_results), 8L, "2x4 risk rows")
  assert_true(is.na(wp_multi$workplace_global_risk_class), "multi-agent global unresolved")
  assert_equal(wp_multi$workplace_aggregation_status, "source_rule_unresolved_at_workplace_level", "multi-agent no collapse")
  assert_equal(wp_multi$highest_observed_risk_class, "Moderado", "descriptive highest class")

  cat("[34/38] Named per-agent exposures are aligned and preserved...\n")
  exp_named <- c(
    "Mycobacterium tuberculosis" = "Muy frecuente",
    "Bordetella pertussis" = "Ocasional"
  )
  wp_exp <- erbio_assess_workplace(
    activity = "Actividad mixta",
    agents = names(exp_named),
    questionnaire_responses = wp_unanimous_responses,
    exposure = exp_named
  )
  assert_equal(wp_exp$agent_summary$exposure[wp_exp$agent_summary$input_agent == "Mycobacterium tuberculosis"], "Muy frecuente", "named exposure MTB")
  assert_equal(wp_exp$agent_summary$exposure[wp_exp$agent_summary$input_agent == "Bordetella pertussis"], "Ocasional", "named exposure Bordetella")

  cat("[35/38] Unresolved agent is reported without corrupting resolved-agent results...\n")
  wp_unres <- erbio_assess_workplace(
    activity = "Actividad con agente parcialmente identificado",
    agents = c("Mycobacterium tuberculosis", "Agente completamente inexistente XYZ"),
    questionnaire_responses = wp_unanimous_responses,
    exposure = "Frecuente"
  )
  assert_equal(wp_unres$assessment_status, "incomplete_unresolved_agent_reference_level", "unresolved agent status")
  assert_equal(length(wp_unres$unresolved_agents), 1L, "one unresolved agent")
  assert_equal(nrow(wp_unres$risk_results), 4L, "known agent still assessed")
  assert_true(is.na(wp_unres$workplace_global_risk_class), "incomplete workplace has no global class")

  cat("[36/38] Preventive plan deduplicates questionnaire failures from agent loops...\n")
  # wp1 failures: audit 0 + general 7 + workers 8 + sector 2 = 17 unique questionnaire failures.
  assert_equal(nrow(wp1$preventive_plan), 17L, "17 unique failed controls")
  assert_equal(length(unique(wp1$preventive_plan$item_id)), 17L, "failed items are not duplicated per agent")
  assert_true(all(wp1$preventive_plan$source_rule == "FAILED-CONTROL-PLANNING-CANDIDATE"), "planning source rule")
  assert_true(all(is.na(wp1$preventive_plan$responsible)), "responsible not invented")
  assert_true(all(is.na(wp1$preventive_plan$target_date)), "deadline not invented")
  assert_true(all(is.na(wp1$preventive_plan$resources)), "resources not invented")

  cat("[37/38] Reproducible Markdown report contains core traceability sections...\n")
  report_txt <- erbio_render_workplace_report(wp1)
  assert_true(grepl("Tratamiento de aguas residuales", report_txt, fixed=TRUE), "report activity")
  assert_true(grepl("Mycobacterium tuberculosis", report_txt, fixed=TRUE), "report agent")
  assert_true(grepl("## 4. Planificación preventiva candidata", report_txt, fixed=TRUE), "report plan section")
  assert_true(grepl("ERBioR_agent_registry_v0_3.csv", report_txt, fixed=TRUE), "report legal registry provenance")
  assert_true(grepl("no los infiere automáticamente", report_txt, fixed=TRUE), "report professional completion caveat")

  cat("[38/38] Reproducibility bundle export writes all seven artifacts...\n")
  outdir_test <- file.path(tempdir(), paste0("erbio_v04_export_", as.integer(Sys.time())))
  paths <- erbio_export_workplace_bundle(wp1, outdir_test, overwrite=TRUE)
  assert_equal(length(paths), 7L, "seven exported artifacts")
  assert_true(all(file.exists(paths)), "all exported artifacts exist")
  assert_true(file.info(paths[["report"]])$size > 0, "report is non-empty")
  assert_true(file.info(paths[["plan"]])$size > 0, "plan CSV is non-empty")

  cat("\nALL ERBioR v0.4 TESTS PASSED\n")
  expect_true(TRUE)
})
