# ERBioR <img src="inst/shiny/ERBioR/www/ERBioR.png" align="right" height="120" alt="ERBioR logo"/>

**Reproducible Occupational Biological Risk Assessment with the ERBio Method**

<!-- badges: start -->
![Version](https://img.shields.io/badge/version-0.9.0-blue)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20%3E%3D%203-green.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![ERBio method DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.22107465-blue)](https://doi.org/10.5281/zenodo.22107465)
<!-- badges: end -->

`ERBioR` is an R package for **occupational biological risk assessment** based on the ERBio method.

It is designed for occupational hygienists, occupational safety and health professionals, prevention practitioners, occupational health researchers and other specialists who need to assess biological risk in a structured, reproducible and source-traceable way.

Unlike a simple risk calculator, `ERBioR` separates the original deterministic ERBio method from regulatory biological-agent data, questionnaire instruments, scientific source audits, preventive-action enrichment and explicit computational extensions.

This architecture allows the original method to be reproduced while preserving the provenance, validation status and scientific role of each information layer.

The package also includes a fully bilingual Spanish-English presentation layer, Excel-based workflows and a Shiny application for users who prefer a guided graphical interface.

<hr>

## Why ERBioR?

Occupational biological-risk assessment requires more than identifying a biological agent.

In practice, the evaluator must integrate information on:

- the biological agent and its regulatory risk group;
- the conditions and frequency of exposure;
- compliance with preventive and organisational controls;
- the characteristics of the workplace;
- the applicable ERBio questionnaires;
- the resulting probability and biological-risk level;
- and the preventive actions arising from identified deficiencies.

The ERBio method provides a structured framework for integrating these components.

`ERBioR` translates that framework into reproducible R functions while retaining the methodological distinctions, documented source rules and explicit ambiguities of the original method.

It helps answer questions such as:

- Which regulatory risk group applies to a biological agent?
- What ERBio reference level corresponds to that agent?
- How should exposure frequency be classified?
- What level of preventive-control compliance results from the ERBio questionnaires?
- What probability and GRB risk score result from the assessment?
- Which questionnaire controls have failed?
- Which approved preventive actions correspond to those failed controls?
- How should several biological agents be reported when their classifications differ?
- Can the same assessment be performed in Spanish or English without changing the scientific result?
- Can a complete workplace assessment be exported as a reproducible report and data bundle?

<hr>

## What does ERBioR do?

`ERBioR` implements the ERBio workflow through distinct but connected layers:

1. **Deterministic ERBio calculation** — exposure, preventive-control compliance, probability, reference level, GRB risk score, risk class and associated action.

2. **Questionnaire assessment** — versioned ERBio questionnaire bank with stable item identifiers, preserved source semantics and explicit validation status.

3. **Biological-agent assessment** — source-traceable regulatory registry, exact and partial lookup, legal-group resolution and ERBio reference-level derivation without silently assigning unlisted agents to risk group 1.

4. **Workplace-level assessment** — integrated assessment using the required core questionnaires plus at least one applicable sector-specific questionnaire.

5. **Preventive planning** — extraction of failed controls and association with approved expert preventive actions.

6. **Scientific provenance** — machine-readable manifests, Spanish source audits and an independent rowwise European regulatory concordance audit.

7. **Bilingual presentation** — Spanish-English questionnaire presentation, preventive actions, interface text and Excel workflows while preserving a single language-neutral scientific calculation core.

8. **Interactive workflow** — bundled Shiny application for guided biological-risk assessment without requiring direct R programming.

9. **Reproducible reporting** — Markdown reports and exportable data bundles preserving inputs, decisions, results and source traceability.

<hr>

## Main functions

| Function | Layer | What it does |
|---|---|---|
| `erbio_exposure_from_cadence()` | Core ERBio | Converts ERBio exposure cadence into the corresponding exposure category |
| `erbio_exposure_level()` | Core ERBio | Derives or validates the ERBio exposure level |
| `erbio_compliance_binary()` | Core ERBio | Calculates preventive-control compliance from binary questionnaire responses |
| `erbio_compliance_audit_0_4()` | Core ERBio | Calculates compliance using the source-preserved 0–4 audit semantics |
| `erbio_compliance_class()` | Core ERBio | Converts compliance percentage into the ERBio compliance class |
| `erbio_probability()` | Core ERBio | Obtains probability from compliance class and exposure |
| `erbio_risk_score()` | Core ERBio | Calculates the ERBio GRB risk score |
| `erbio_risk_class()` | Core ERBio | Classifies the GRB score into the ERBio risk class |
| `erbio_action()` | Core ERBio | Returns the action associated with the ERBio risk class |
| `erbio_assess()` | Core ERBio | Runs an integrated deterministic ERBio assessment |
| `erbio_assess_set()` | Core ERBio | Evaluates a set of assessments while preserving unresolved discordance |
| `erbio_questionnaire_catalog()` | Questionnaires | Lists available ERBio questionnaires and metadata |
| `erbio_get_questionnaire()` | Questionnaires | Retrieves a versioned ERBio questionnaire |
| `erbio_get_questionnaire_i18n()` | Questionnaires | Retrieves questionnaire content in Spanish or English |
| `erbio_score_questionnaire()` | Questionnaires | Scores a questionnaire and identifies failed controls |
| `erbio_assess_questionnaire()` | Questionnaires | Integrates questionnaire score, exposure and reference level |
| `erbio_agent_lookup()` | Biological agents | Searches the regulatory biological-agent registry |
| `erbio_reference_level_from_agent()` | Biological agents | Derives the ERBio reference level for one resolved agent |
| `erbio_reference_level_from_agents()` | Biological agents | Reports multiple agents without silently collapsing discordant groups |
| `erbio_assess_agent_questionnaire()` | Biological agents | Integrates one biological agent with one questionnaire assessment |
| `erbio_validate_agent_registry()` | Biological agents | Validates the regulatory and scientific-audit structure of the agent registry |
| `erbio_sector_questionnaires()` | Workplace | Lists sector-specific ERBio questionnaires |
| `erbio_validate_workplace_questionnaires()` | Workplace | Checks that the required questionnaire set is complete |
| `erbio_assess_workplace()` | Workplace | Runs an integrated workplace-level ERBio assessment |
| `erbio_build_preventive_plan()` | Prevention | Generates deduplicated preventive-planning candidates |
| `erbio_preventive_action()` | Prevention | Returns the approved preventive action associated with an ERBioR item |
| `erbio_load_preventive_action_registry()` | Prevention | Loads the approved preventive-action registry |
| `erbio_validate_preventive_action_registry()` | Prevention | Validates the preventive-action registry |
| `erbio_set_language()` | Language | Sets the active presentation language |
| `erbio_get_language()` | Language | Returns the active presentation language |
| `erbio_translate_ui()` | Language | Returns translated interface text from a stable key |
| `erbio_load_translation_registry()` | Language | Loads the approved scientific translation registry |
| `erbio_validate_translation_registry()` | Language | Validates the translation registry |
| `erbio_bilingual_invariance_check()` | Language | Checks scientific invariance across Spanish and English presentation |
| `erbio_render_workplace_report()` | Reporting | Renders a reproducible workplace assessment in Markdown |
| `erbio_write_workplace_report()` | Reporting | Writes the Markdown assessment report to disk |
| `erbio_export_workplace_bundle()` | Reporting | Exports the complete reproducibility bundle |
| `erbio_run_app()` | Shiny | Launches the bilingual ERBioR interactive application |
| `erbio_shiny_build_id()` | Shiny | Returns the bundled Shiny build identifier |
| `erbio_version()` | Utilities | Returns the ERBioR package version |

<hr>

## Installation

```r
# From CRAN, once published:
install.packages("ERBioR")

# Development version from GitHub:
# install.packages("remotes")
remotes::install_github("Aguilar-Elena/ERBioR")
```

Then load the package:

```r
library(ERBioR)
```

<hr>

## Language

`ERBioR` is fully bilingual in **Spanish and English**.

```r
erbio_get_language()

erbio_set_language("es")
erbio_set_language("en")
```

Questionnaire presentation, preventive-action outputs and Shiny interface text can be rendered in either language.

The bilingual layer does **not** modify the scientific calculation.

Stable questionnaire identifiers, item identifiers, scoring rules, exposure classification, probability calculations, reference levels and GRB risk results remain language-neutral.

Spanish and English Excel workbooks can also be imported independently of the selected interface language.

Cross-language inputs are normalised before scoring so that equivalent assessments produce the same scientific result.

Technical and linguistic approval of translated content is explicitly kept separate from psychometric validation.

<hr>

## Minimal usage

### ERBio deterministic calculation

```r
library(ERBioR)

erbio_probability(
  "Aceptable",
  "Frecuente"
)

erbio_risk_score(
  probability = 2,
  reference_level = 3
)

erbio_risk_class(6)
```

A complete deterministic assessment can be performed with:

```r
assessment <- erbio_assess(
  reference_level = 3,
  exposure = "Muy frecuente",
  compliance_percent = 82.5
)

assessment
```

### Biological-agent lookup

```r
erbio_agent_lookup(
  "Bacillus anthracis"
)

erbio_reference_level_from_agent(
  "Mycobacterium tuberculosis"
)
```

For several agents:

```r
erbio_reference_level_from_agents(
  c(
    "Bacillus anthracis",
    "Coxiella burnetii"
  )
)
```

By default, discordant regulatory groups are preserved rather than silently reduced to one workplace value.

### Questionnaire assessment

```r
erbio_questionnaire_catalog()

general <- erbio_get_questionnaire(
  "general"
)

nrow(general)

score <- erbio_score_questionnaire(
  "general",
  rep("Si", 48)
)

score
```

An integrated questionnaire assessment:

```r
assessment <- erbio_assess_questionnaire(
  "general",
  rep("Si", 48),
  reference_level = 3,
  exposure = "Frecuente"
)

assessment
```

### Bilingual questionnaire presentation

```r
erbio_set_language("en")

general_en <- erbio_get_questionnaire_i18n(
  "general"
)

head(general_en)
```

Switch back to Spanish:

```r
erbio_set_language("es")
```

### Complete workplace assessment

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
```

### Preventive planning

Failed questionnaire controls can be transformed into structured preventive-planning candidates:

```r
plan <- erbio_build_preventive_plan(
  assessment
)

plan
```

Approved preventive-action information for an individual ERBioR item can also be retrieved directly:

```r
erbio_preventive_action(
  "GENERAL-F2015-028",
  language = "es"
)
```

### Interactive application

```r
library(ERBioR)

erbio_run_app()
```

The ERBioR Shiny application provides a guided bilingual interface for questionnaire completion, biological-agent assessment, preventive planning, reporting and Excel-based workflows.

### Reproducible report

```r
erbio_render_workplace_report(
  assessment
)

erbio_write_workplace_report(
  assessment,
  file = "ERBioR_workplace_report.md"
)
```

To export the complete reproducibility bundle:

```r
erbio_export_workplace_bundle(
  assessment,
  dir = "ERBioR_results"
)
```

<hr>

## Questionnaire system

The package contains a versioned ERBio questionnaire bank with **27 active instruments and 1,008 source-preserved items**.

The workplace workflow requires the three core questionnaires:

- audit;
- general assessment;
- worker perception;

plus at least one applicable sector-specific instrument.

Not-applicable responses are excluded from the compliance denominator.

Stable item identifiers and final item numbers are preserved so that named responses remain reproducible even if presentation order changes.

Sector-specific questionnaires retain their documented validation status and are not automatically presented as psychometrically validated instruments where such validation has not been established.

<hr>

## Biological-agent registry

`ERBioR` includes a source-traceable biological-agent registry containing **509 classified entries**.

The legal classification layer is derived from the Spanish regulatory framework, with the replacement of Annex II introduced by Orden TES/1287/2021 used as the primary Spanish row source.

The package also contains an independent rowwise semantic audit against the corresponding European Annex III regulatory basis.

Important safeguards are built into the registry logic:

- an unlisted biological agent is **not** silently classified as risk group 1;
- exact and partial lookup behaviour are explicitly separated;
- multiple agents are reported separately when their risk groups differ;
- unclassified human viruses are handled through an explicit regulatory minimum rather than converted into a false final classification;
- BASEBiO technical information is kept separate from the legal regulatory risk group;
- regulatory and technical enrichment layers are source-traceable.

<hr>

## European regulatory audit

The v0.8 scientific-audit layer incorporated into ERBioR 0.9.0 contains an independent rowwise European regulatory concordance audit.

The audited registry records:

- **509/509** biological-agent rows verified;
- **508** rows linked to Commission Directive (EU) 2019/1833;
- **1** SARS-CoV-2 row linked to Commission Directive (EU) 2020/739;
- **509/509** current/intended agent-name concordance;
- **509/509** risk-group concordance;
- concordance of parsed A/D/T/V annotations;
- concordance of group-3 `**` semantics;
- preservation of the documented historical Spanish-language source-text anomaly involving `Fusobacterium necrophorum`.

Exact raw-text identity across every historical regulatory-language version is not claimed.

The audit distinguishes current regulatory concordance from historical source-text provenance.

<hr>

## Preventive-action registry

ERBioR 0.9.0 includes an approved Spanish expert preventive-action registry covering the complete active questionnaire bank.

The registry contains:

- **1,008 ERBioR item identifiers**;
- **1,008 approved Spanish preventive actions**;
- explicit linkage between preventive actions and stable questionnaire item IDs;
- source-text consistency checks;
- prevention-planning support without altering questionnaire scoring.

Preventive actions are treated as a professional preventive-planning layer and remain separate from the deterministic scientific calculation.

<hr>

## Scientific translation registry

The bilingual version includes an approved Spanish-English scientific translation registry covering the complete active questionnaire bank.

It contains:

- **1,008 source questionnaire items**;
- Spanish source wording;
- approved English scientific translation;
- stable item identifiers;
- translation-approval metadata;
- explicit separation of linguistic approval from psychometric validation.

The same underlying scientific identifiers and calculations are used in both languages.

<hr>

## Excel workflow

ERBioR includes Excel-based workflows intended to support assessment without requiring direct manipulation of R objects.

Spanish and English workbooks can be imported independently of the selected Shiny interface language.

The import layer normalises recognised headers and response values before scientific scoring.

A bundled wastewater-treatment template is available from the installed package:

```r
system.file(
  "templates",
  "ERBioR_Plantilla_Depuradoras_CORREGIDA_v0_3.xlsx",
  package = "ERBioR"
)
```

The Excel layer is designed so that presentation language does not modify scientific results.

<hr>

## Outputs

Depending on the function used, `ERBioR` returns structured R objects and reproducible files containing:

| Output | Description |
|---|---|
| Exposure classification | ERBio exposure category derived from the selected exposure information |
| Compliance result | Percentage and compliance class derived from questionnaire responses |
| Probability | ERBio probability level resulting from exposure and preventive-control compliance |
| Reference level | ERBio reference level derived from the resolved biological agent |
| GRB score | Deterministic biological-risk score |
| Risk class | ERBio risk classification associated with the GRB score |
| Action | Action associated with the resulting risk class |
| Failed controls | Questionnaire items requiring preventive consideration |
| Preventive actions | Approved preventive-planning information linked to failed controls |
| Agent summary | Separate regulatory resolution and reference level for each biological agent |
| Questionnaire summary | Score, compliance, failed controls and source warnings for each instrument |
| Preventive plan | Deduplicated preventive-planning candidates |
| Markdown report | Human-readable reproducible workplace assessment |
| Reproducibility bundle | Report and structured data preserving assessment inputs, results and traceability |

<hr>

## Scientific provenance

ERBioR separates the principal scientific and regulatory source layers.

### 1. ERBio method

The underlying ERBio method was developed by Raúl Aguilar-Elena:

**Aguilar-Elena, R. (2015). _Riesgos biológicos laborales: ERBio, un nuevo método de evaluación teórica_. Universidad Pública de Navarra.**

DOI: [10.5281/zenodo.22107465](https://doi.org/10.5281/zenodo.22107465)

### 2. Core questionnaires and validation dataset

The open dataset containing the ERBio core questionnaires, scoring algorithm and validation evidence is available separately:

**Aguilar-Elena, R. ERBio — Core questionnaires for occupational biological risk assessment: the general assessment and worker perception instruments, with scoring algorithm and validation evidence.**

DOI: [10.5281/zenodo.22069658](https://doi.org/10.5281/zenodo.22069658)

### 3. Spanish regulatory classification

The Spanish legal classification layer is based on Annex II of Real Decreto 664/1997, including the replacement introduced by Orden TES/1287/2021.

### 4. European regulatory concordance

The European concordance layer uses Annex III of Directive 2000/54/EC as updated by:

- Commission Directive (EU) 2019/1833;
- Commission Directive (EU) 2020/739 for SARS-CoV-2.

### 5. Technical enrichment

INSST BASEBiO information is treated as a technical enrichment source and is kept separate from the legal biological-agent classification.

<hr>

## Methodological notes

`ERBioR` preserves several distinctions that are important for scientific reproducibility.

**Unlisted biological agents:** an agent absent from the regulatory registry is not automatically classified as risk group 1.

**Multiple biological agents:** when several agents have different legal risk groups, ERBioR does not silently collapse them into one result unless an explicit computational policy is selected.

**Questionnaire discordance:** if questionnaire-level risk results differ and the original ERBio source does not define a unique aggregation rule, ERBioR preserves the discordance instead of silently manufacturing a global class.

**BASEBiO:** technical information from BASEBiO does not overwrite the applicable regulatory classification.

**Questionnaire scoring:** binary and source-preserved 0–4 audit semantics are implemented separately where required by the source material.

**Language:** Spanish-English presentation affects text only. It does not change scientific identifiers, scoring rules or calculated risk results.

**Preventive actions:** expert preventive-action text is an enrichment layer for preventive planning and does not alter ERBio scoring.

<hr>

## Deliberate safeguards

`ERBioR` is intentionally conservative where the original method or regulatory sources do not resolve an issue unambiguously.

The package does **not**:

- silently classify an unlisted agent as risk group 1;
- silently collapse discordant questionnaire-level risk classes into one global class;
- silently collapse several biological agents into one workplace classification;
- allow BASEBiO technical enrichment to overwrite the legal biological-agent group;
- allow presentation language to alter the scientific result;
- present expert preventive-action text as part of the original deterministic ERBio calculation;
- present linguistic translation approval as psychometric validation;
- present an explicit computational extension as though it were an original ERBio source rule.

Where alternative interpretations are computationally useful, they are exposed through explicit policy arguments.

<hr>

## Limitations

`ERBioR` is a professional decision-support and reproducibility tool. It does not replace the judgement of a qualified occupational safety, occupational hygiene or occupational health professional.

The quality of an assessment depends on the quality of the underlying workplace information, identification of biological agents, exposure characterisation and selection of applicable questionnaires.

A regulatory registry cannot substitute for case-specific identification of biological hazards or professional interpretation of unusual biological agents.

Sector-specific questionnaire instruments retain their documented validation status. Their inclusion in the package does not imply that every sector instrument has undergone independent psychometric validation.

The English scientific translation registry has undergone technical and linguistic approval. This does not constitute independent psychometric validation of an English-language instrument.

Preventive actions are intended to support planning and professional review. Their presence does not remove the need to determine whether a measure is applicable, proportionate and legally sufficient in the specific workplace.

<hr>

## Validation and testing

ERBioR includes a frozen regression suite covering:

- deterministic ERBio probability and risk matrices;
- questionnaire structure and scoring;
- exposure classification;
- biological-agent registry logic;
- legal A/D/T/V and `**` semantics;
- handling of unlisted agents;
- handling of unclassified human viruses;
- multiple-agent logic;
- BASEBiO separation;
- workplace-level assessment;
- preventive planning;
- reporting;
- reproducibility-bundle export.

The frozen v0.4 regression layer contains **38 regression blocks**.

ERBioR 0.9.0 additionally includes dedicated validation for:

- the 509-row biological-agent regulatory registry;
- the Spanish legal-source audit;
- the independent European regulatory audit;
- the 1,008-item questionnaire bank;
- the 1,008-item approved Spanish preventive-action registry;
- the 1,008-item Spanish-English scientific translation registry;
- bilingual scientific invariance;
- cross-language Excel import;
- package installation;
- package provenance and metadata.

Technical or linguistic approval of translated content is explicitly kept separate from psychometric validation.

<hr>

## Citation

If you use `ERBioR` in research, professional practice or academic work, please cite the software and, where appropriate, the original ERBio method.

### ERBioR software

```text
Aguilar-Elena, R. & Delgado-García, A. (2026).
ERBioR: Reproducible Occupational Biological Risk Assessment with the ERBio Method.
R package version 0.9.0.
```

### ERBio method

```text
Aguilar-Elena, R. (2015).
Riesgos biológicos laborales: ERBio, un nuevo método de evaluación teórica.
Doctoral thesis, Universidad Pública de Navarra.
https://doi.org/10.5281/zenodo.22107465
```

### Core questionnaires and validation dataset

```text
Aguilar-Elena, R. (2026).
ERBio — Core questionnaires for occupational biological risk assessment:
the general assessment and worker perception instruments,
with scoring algorithm and validation evidence.
https://doi.org/10.5281/zenodo.22069658
```

After installation:

```r
citation("ERBioR")
```

returns the citation information distributed with the package.

<hr>

## Authors

**PhD. Raúl Aguilar-Elena** · raguilar@universidadviu.com
Occupational Risk Prevention and Occupational Health Research Group
Universidad Internacional de Valencia (VIU), Valencia, Spain
ORCID: [0000-0001-8656-8155](https://orcid.org/0000-0001-8656-8155)

**Ana Delgado-García** · a.delgado@usal.es
BISITE Research Group
Universidad de Salamanca (USAL), Salamanca, Spain
ORCID: [0009-0003-7079-5507](https://orcid.org/0009-0003-7079-5507)

Raúl Aguilar-Elena is the author of the original ERBio occupational biological-risk assessment method.

Raúl Aguilar-Elena and Ana Delgado-García are authors and developers of the ERBioR software.

The authorship of the ERBioR software is therefore distinct from the authorship of the original ERBio method.

<hr>

## Maintainer

**PhD. Raúl Aguilar-Elena**  
Universidad Internacional de Valencia (VIU), Valencia, Spain  
raguilar@universidadviu.com

<hr>

## License

GPL (>= 3) © 2026 Raúl Aguilar-Elena & Ana Delgado-García
