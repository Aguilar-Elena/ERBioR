## ERBioR 0.9.0.9000 pre-CRAN status

ERBioR 0.9.0.9000 carries forward the frozen deterministic ERBio regression layer and the independently audited Spanish and European biological-agent regulatory source layers.

The current development version adds:

- an approved 1,008-item Spanish expert preventive-action registry;
- an approved 1,008-item Spanish-English scientific translation registry;
- bilingual Spanish-English questionnaire presentation;
- bilingual Shiny functionality;
- cross-language Excel import independent of the selected interface language;
- preservation of one language-neutral deterministic ERBio calculation core;
- explicit separation between technical/linguistic translation approval and psychometric validation;
- formal software authorship metadata and ORCID identifiers.

### Scientific and regression verification

The ERBioR v0.9 pre-benchmark suite completed successfully on the current build.

The preventive-action registry validation returned:

- 1,008 actions;
- 1,008 approved Spanish actions;
- 0 source-text mismatches;
- 0 missing item IDs;
- 0 missing required action rows.

The complete regression suite passed, including the frozen v0.4 ERBio regression layer, scientific source-audit tests and v0.9 bilingual/preventive tests.

The Shiny smoke tests also passed:

- legacy v0.7 application smoke tests;
- bilingual core smoke test;
- cross-language Excel import smoke test.

Cross-platform header normalization was verified on macOS after correcting platform-specific ASCII transliteration behaviour.

### Regulatory source audit

The package includes the independent row-by-row European regulatory audit developed in the v0.8 layer.

The audit covers 509 classified biological-agent rows:

- 508 rows based on Commission Directive (EU) 2019/1833;
- SARS-CoV-2 based on Commission Directive (EU) 2020/739.

The regulatory audit layer remains separate from the deterministic ERBio calculation logic.

### Remaining pre-CRAN step

R CMD check --as-cran will be rerun on the exact GitHub/release source before submission.
