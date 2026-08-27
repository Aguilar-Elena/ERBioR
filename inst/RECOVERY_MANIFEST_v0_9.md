# ERBioR recovery manifest — 0.9.0.9000

This development build was reconstructed from the recovered ERBioR v0.7 complete source bundle plus the recovered v0.8 regulatory-audit and v0.9 bilingual/preventive artifacts.

## Preserved scientific core

The deterministic ERBio v0.4 calculation logic from the v0.7 complete package is retained. The recovery does not intentionally change risk matrices, compliance calculations, exposure rules, questionnaire IDs, agent IDs, or workplace orchestration logic.

## Integrated v0.8 layer

- EU regulatory audit document.
- v0.8 source manifest.
- v0.8 source audit summary.
- Spanish regulatory layer remains legally primary for Spain; EU audit is an independent verification layer.

## Integrated v0.9 development layer

- 1,008-row Spanish expert preventive-action registry, all `APPROVED_ES`.
- 1,008-row ES/EN scientific translation registry, all question and preventive translations `APPROVED_EN_TRANSLATION`.
- UI ES/EN dictionary.
- English translation approval record.
- Bilingual language API and invariance checker.
- Recovered v0.9 pre-benchmark script.
- Wastewater Excel template retained as a recovered reference/template artifact.

## Validation boundary

English approval is technical/linguistic. It does not imply independent psychometric validation of the English questionnaire instruments.

This reconstruction was assembled in an environment without R. Therefore structural/data QA can be performed here, but `R CMD INSTALL`, the full `testthat` suite, and `R CMD check --as-cran` must be run in an R environment before freezing a release candidate.

## Bilingual Shiny candidate 3 — cross-language import

The Shiny import layer now separates workbook language from presentation language. Spanish Excel can be assessed in an English UI and vice versa. Stable item IDs and normalized response codes feed the unchanged deterministic scientific core; approved bilingual registry fields control presentation.
