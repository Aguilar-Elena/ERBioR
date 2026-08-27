# ERBioR 0.9.0

- Added formal software authorship metadata for Raúl Aguilar-Elena and Ana Delgado-García, including verified ORCID identifiers.
- Corrected the ERBio method reference to DOI 10.5281/zenodo.22107465.
- Distinguished the ERBio method reference from the open core-questionnaire and validation dataset DOI 10.5281/zenodo.22069658.
- Consolidated bilingual Shiny, cross-language Excel import, and macOS/Windows header-normalization fixes.


- Recovered and integrated the v0.8 independent EU regulatory audit layer.
- Added the approved 1,008-item Spanish expert preventive-action registry.
- Added the approved 1,008-item Spanish-English scientific translation registry.
- Added language API (`erbio_set_language()`, `erbio_get_language()`) and bilingual questionnaire rendering.
- Enriched failed controls with expert preventive actions while preserving deterministic scoring.
- Added bilingual scientific invariance checks.
- Technical/linguistic English approval is explicitly separated from psychometric validation.

# ERBioR 0.7.0.9000

- Adds a 509-row scientific source audit of the Spanish biological-agent registry against the official Annex II replacement in Order TES/1287/2021 (BOE-A-2021-19371).
- Makes the 2021 official amending act the primary Spanish legal row source and retains consolidated RD 664/1997 as a secondary informative reference.
- Corrects three SARS-CoV/SARS-CoV-2/MERS-CoV spacing artifacts inherited from consolidated HTML rendering using the official BOE PDF.
- Rejects one false-positive BASEBiO linkage from Picornaviridae to Coronaviridae and reviews six other non-exact technical links with explicit scope/nomenclature states.
- Keeps all independent EUR-Lex rowwise-verification flags false until the dedicated 509-row EU audit is completed.
- Adds rowwise audit, source manifest and audit-summary assets.
- Does not modify the frozen v0.4 deterministic ERBio calculation logic.

# ERBioR 0.6.0.9000

- Adds formal package and method citations, including the ERBio Zenodo DOI `10.5281/zenodo.22107465`.
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

## Cross-language Shiny import candidate 3

- Spanish and English Excel workbooks are accepted independently of the current UI language.
- Workbook language is detected from metadata or stable item-id/question-text matching.
- Responses are normalized to the same internal Spanish-core response codes before scientific scoring.
- The selected UI language controls questionnaire presentation and approved preventive-action output.
- Cross-language imports display an explicit input/output language notice.
