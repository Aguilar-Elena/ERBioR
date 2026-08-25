# ERBioR 0.8.0.9000

- Completes the independent 509-row semantic concordance audit against the official EU Annex III basis.
- Audits 508 rows against Commission Directive (EU) 2019/1833 and SARS-CoV-2 against Commission Directive (EU) 2020/739.
- Records 509/509 risk-group concordance and 509/509 concordance for parsed A/D/T/V and group-3 (**) semantics.
- Documents one historical Spanish-language 2019 source-text duplication involving `Fusobacterium necrophorum`; current/intended EU identity is treated as concordant after cross-checking consolidated multilingual EU evidence and a 2023 corrigendum in affected language versions. Both entries remain group 2.
- Explicitly distinguishes semantic rowwise verification from exact raw-text identity.
- Adds `ERBioR_agent_eu_audit_v0_8.csv`, `ERBioR_agent_scientific_audit_v0_8.csv`, v0.8 manifest/summary/QA, and a scientific audit report.
- Carries the frozen v0.4 regression suite forward unchanged.

# ERBioR 0.7.0.9000

- Adds a 509-row scientific source audit of the Spanish biological-agent registry against the official Annex II replacement in Order TES/1287/2021 (BOE-A-2021-19371).
- Makes the 2021 official amending act the primary Spanish legal row source and retains consolidated RD 664/1997 as a secondary informative reference.
- Corrects three SARS-CoV/SARS-CoV-2/MERS-CoV spacing artifacts inherited from consolidated HTML rendering using the official BOE PDF.
- Rejects one false-positive BASEBiO linkage from Picornaviridae to Coronaviridae and reviews six other non-exact technical links with explicit scope/nomenclature states.
- Keeps all independent EUR-Lex rowwise-verification flags false until the dedicated 509-row EU audit is completed.
- Adds rowwise audit, source manifest and audit-summary assets.
- Does not modify the frozen v0.4 deterministic ERBio calculation logic.

# ERBioR 0.6.0.9000

- Adds formal package and method citations, including the ERBio Zenodo DOI `10.5281/zenodo.22069658`.
- Adds a source manifest separating ERBio method provenance, Spanish legal classification, EU concordance and BASEBiO technical enrichment.
- Adds a source-audit summary that explicitly records pending row-by-row EUR-Lex verification rather than overstating concordance.
- Expands user documentation and executable examples by API layer.
- Adds an explicit `testthat` expectation to the frozen v0.4 regression test so the suite is no longer reported as an empty/skipped test.
- Updates the package development version to 0.6.0.9000.
- Does not change the frozen v0.4 ERBio calculation logic.

# ERBioR 0.5.0.9000

- Converts frozen v0.4 into a formal R package layout.
- Adds installed-package `extdata` discovery so users do not need to set `erbio.data_dir`.
- Preserves the complete v0.4 deterministic/questionnaire/agent/workplace engine.
- Adds `erbio_version()`.
- Adapts the 38-block v0.4 regression suite to `testthat`.
- Passes `R CMD check` with 0 errors, 0 warnings and 0 notes on R 4.6.1/macOS aarch64 under `--as-cran` (2026-08-25).

# ERBioR 0.4 frozen

- Workplace-level assessment.
- Core + sector questionnaire validation.
- Multi-agent handling without silent aggregation.
- Preventive-plan generation and reproducibility bundle export.
- 38/38 regression blocks passed on 2026-08-25.

### Pre-CRAN candidate patch 2

- Fixed a syntax-only parse error in `tests/testthat/test-precran-metadata.R` caused by an accidental standalone backslash.
- No computational or methodological ERBioR code was changed.
