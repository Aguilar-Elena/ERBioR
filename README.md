# ERBioR 0.8.0.9000 — independent EU source-audit candidate

`ERBioR` is a source-traceable R implementation of the ERBio method for occupational biological-risk assessment. It keeps the original deterministic method logic separate from regulatory data, technical enrichment and explicit computational extensions.

## Method reference

The underlying ERBio method is available through the persistent Zenodo DOI:

**Aguilar-Elena R. (2015). _Riesgos biológicos laborales: ERBio, un nuevo método de evaluación teórica_. Universidad Pública de Navarra. https://doi.org/10.5281/zenodo.22069658**

After installation, use:

```r
citation("ERBioR")
```

to obtain both the method and software citations.

## What the package contains

- deterministic ERBio exposure, compliance, probability and GRB calculations;
- versioned questionnaire bank with 27 active instruments and 1,008 source-preserved items;
- Spanish regulatory biological-agent registry with 509 classified entries, audited against the official 2021 Annex II replacement;
- BASEBiO technical index kept separate from legal classification;
- agent-to-reference-level assessment;
- workplace-level assessment using the three core questionnaires plus at least one sector-specific instrument;
- preventive-planning candidates derived from failed questionnaire controls;
- reproducible Markdown/CSV reporting and export;
- frozen regression coverage through ERBioR v0.4.

## Quick start

```r
library(ERBioR)

erbio_version()

erbio_probability("Aceptable", "Frecuente")
erbio_risk_class(erbio_risk_score(probability = 2, reference_level = 3))

erbio_reference_level_from_agent("Mycobacterium tuberculosis")

erbio_questionnaire_catalog()
```

A complete workplace example:

```r
responses <- list(
  audit = rep("Si", 32),
  general = rep("Si", 48),
  workers = rep("Si", 34),
  sec_wastewater = rep("Si", 12)
)

assessment <- erbio_assess_workplace(
  activity = "Tratamiento de aguas residuales",
  agents = "Mycobacterium tuberculosis",
  questionnaire_responses = responses,
  exposure = "Frecuente"
)

assessment
erbio_render_workplace_report(assessment)
```

## Deliberate safeguards

ERBioR does not silently classify an unlisted biological agent as risk group 1. It does not silently collapse discordant questionnaire-level results into one global class, and it keeps multi-agent results separate unless an explicit computational extension is requested. Sector-specific questionnaires remain labelled according to their documented validation status.

## Source layers

1. **ERBio method:** Aguilar-Elena (2015), DOI `10.5281/zenodo.22069658`.
2. **Spanish legal classification:** Annex II of Real Decreto 664/1997, consolidated through 25 November 2021.
3. **European concordance:** Annex III of Directive 2000/54/EC.
4. **Technical enrichment:** INSST BASEBiO.

The Spanish BOE registry is the legal classification layer used by the software. BASEBiO is a technical enrichment layer and does not override the legal group. The independent EU Annex III rowwise semantic audit is completed in v0.8: 509/509 rows audited, 509/509 current/intended biological-agent identity concordance and 509/509 risk-group concordance. One historical Spanish-language 2019 source-text duplication is preserved as provenance; no unresolved substantive name or group discrepancy remains.

## Validation status

The frozen v0.4 regression suite contains 38 blocks covering the mathematical engine, questionnaire layer, biological-agent layer, workplace assessment, preventive planning and reproducible reporting. The 0.5 package wrapper passed `R CMD check` with **0 errors, 0 warnings and 0 notes** on R 4.6.1/macOS aarch64 under `--as-cran` on 25 August 2026. Version 0.6 adds documentation, citation and provenance assets without changing the frozen v0.4 calculation logic.

## Development status

This is a pre-CRAN development candidate. Before public submission, ERBioR should additionally pass checks on at least one Windows environment and complete the dedicated source-data concordance audit documented in `inst/extdata/ERBioR_source_audit_summary_v0_6.csv`.


## Scientific source audit (v0.7)

Version 0.8 retains the v0.7 Spanish/BASEBiO audit and adds an independent rowwise semantic audit against the official EU Annex III basis: Directive (EU) 2019/1833 for 508 rows and Directive (EU) 2020/739 for SARS-CoV-2. Current/intended biological-agent identity, risk-group and parsed A/D/T/V/(**) semantics are concordant in 509/509 rows. A historical Spanish-language 2019 original-text duplication involving `Fusobacterium necrophorum` is preserved as provenance and cross-checked against consolidated multilingual EU evidence and a later corrigendum in affected language versions. Exact raw-text identity is not claimed. See `inst/SCIENTIFIC_AUDIT_v0_8.md`.
