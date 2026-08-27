# ERBioR <img src="man/figures/ERBioR.png" align="right" height="120" alt="ERBioR logo"/>

**Qualitative Assessment of Occupational Biological Exposure Risk**

<!-- badges: start -->
![Development version](https://img.shields.io/badge/devel-0.9.0-blue)
![R CMD check](https://img.shields.io/badge/R--CMD--check-0%20errors%20%7C%200%20warnings%20%7C%200%20notes-brightgreen)
[![License: GPL v3+](https://img.shields.io/badge/License-GPL%20v3%2B-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.html)
[![ERBio method DOI](https://img.shields.io/badge/DOI-10.5281%2Fzenodo.22107465-blue)](https://doi.org/10.5281/zenodo.22107465)
<!-- badges: end -->

`ERBioR` is an R package for **occupational biological risk assessment** based on the ERBio method.

It is designed for occupational risk prevention professionals, occupational hygienists, occupational health researchers and other specialists who need to assess biological risk in a structured, reproducible and source-traceable way.

Unlike a simple risk calculator, `ERBioR` keeps the original deterministic ERBio method logic separate from regulatory biological-agent data, questionnaire instruments, technical enrichment and explicit computational extensions. The package can therefore reproduce the method while preserving the provenance and validation status of each layer.

<hr>


## Authors

- **Raúl Aguilar-Elena** — Author of the ERBio method; developer and maintainer of ERBioR. ORCID: [0000-0001-8656-8155](https://orcid.org/0000-0001-8656-8155).
- **Ana Delgado-García** — Developer and author of the ERBioR software. ORCID: [0009-0003-7079-5507](https://orcid.org/0009-0003-7079-5507).

The ERBio method was originally developed by Raúl Aguilar-Elena. Software authorship of ERBioR is distinct from authorship of the original ERBio method.


### Core questionnaires and validation dataset

The open dataset containing the ERBio core questionnaires, scoring algorithm and validation evidence is available at:

**Aguilar-Elena, R. ERBio — Core questionnaires for occupational biological risk assessment: the general assessment and worker perception instruments, with scoring algorithm and validation evidence.**
https://doi.org/10.5281/zenodo.22069658

## Why ERBioR?

Occupational biological-risk assessment requires more than identifying a biological agent. In practice, the evaluator must combine information on the agent, its regulatory risk group, the characteristics and frequency of exposure, preventive-control compliance and the specific conditions of the workplace.

The ERBio method provides a structured framework for integrating these elements. `ERBioR` translates that framework into reproducible R functions while retaining the methodological distinctions and documented ambiguities of the original source.

`ERBioR` was developed to make this process auditable and computationally reproducible.

It helps answer questions such as:

- Which regulatory risk group applies to a biological agent?
- What ERBio reference level corresponds to that agent?
- How is exposure classified from its frequency or duration?
- What level of preventive-control compliance is obtained from the ERBio questionnaires?
- What probability and GRB risk score result from the combined assessment?
- Which questionnaire controls have failed and should be considered in preventive planning?
- How should several biological agents be reported when their classifications differ?
- Can a complete workplace assessment be exported as a reproducible report and data bundle?

<hr>

## What does ERBioR do?

`ERBioR` implements the ERBio assessment workflow through distinct but connected layers:

1. **Deterministic ERBio calculation** — exposure, control compliance, probability, reference level, GRB risk score, risk class and associated action.

2. **Questionnaire assessment** — versioned ERBio questionnaire bank with stable item identifiers, preserved source semantics and explicit validation status.

3. **Biological-agent assessment** — source-traceable regulatory registry, exact/partial lookup behaviour, legal-group resolution and ERBio reference-level derivation without silently assigning unlisted agents to group 1.

4. **Workplace-level assessment** — integral evaluation using the three core questionnaires plus at least one sector-specific questionnaire, with one or more biological agents assessed without silent aggregation.

5. **Preventive planning and reporting** — extraction of failed controls, deduplicated preventive-planning candidates and reproducible Markdown/CSV export.

6. **Scientific provenance layer** — machine-readable manifests, source audits and an independent rowwise EU concordance audit of the biological-agent registry.

<hr>


### Bilingual ERBioR

ERBioR 0.9 provides Spanish-English functionality while preserving a single language-neutral scientific calculation core. Questionnaire presentation, preventive-action output and Shiny interface text can be rendered in Spanish or English without changing the underlying ERBio calculations.

Spanish and English Excel workbooks can be imported independently of the selected interface language. Cross-language imports are normalized before scoring so that scientific results remain invariant across languages.

## Main functions

| Function | Layer | What it does |
|---|---|---|
| `erbio_exposure_from_cadence()` | Core ERBio | Converts an ERBio exposure cadence into the corresponding exposure category |
| `erbio_exposure_level()` | Core ERBio | Derives or validates the ERBio exposure level |
| `erbio_compliance_binary()` | Core ERBio | Calculates preventive-control compliance from binary questionnaire responses |
| `erbio_compliance_audit_0_4()` | Core ERBio | Calculates compliance using the source-preserved 0–4 audit semantics |
| `erbio_compliance_class()` | Core ERBio | Converts compliance percentage into the ERBio compliance class |
| `erbio_probability()` | Core ERBio | Obtains probability from compliance class and exposure |
| `erbio_risk_score()` | Core ERBio | Calculates the ERBio GRB risk score |
| `erbio_risk_class()` | Core ERBio | Classifies the GRB score into the ERBio risk class |
| `erbio_action()` | Core ERBio | Returns the action associated with the ERBio risk class |
| `erbio_assess()` | Core ERBio | Runs an integrated deterministic ERBio assessment |
| `erbio_assess_set()` | Core ERBio | Evaluates a set of assessments while preserving unresolved discordance by default |
| `erbio_questionnaire_catalog()` | Questionnaires | Lists the available ERBio questionnaires and their metadata |
| `erbio_get_questionnaire()` | Questionnaires | Retrieves a versioned ERBio questionnaire |
| `erbio_score_questionnaire()` | Questionnaires | Scores one questionnaire and identifies failed controls |
| `erbio_assess_questionnaire()` | Questionnaires | Integrates questionnaire score, exposure and reference level |
| `erbio_agent_lookup()` | Biological agents | Searches the regulatory biological-agent registry |
| `erbio_reference_level_from_agent()` | Biological agents | Derives the ERBio reference level for one resolved agent |
| `erbio_reference_level_from_agents()` | Biological agents | Reports reference levels for multiple agents without silent collapse |
| `erbio_assess_agent_questionnaire()` | Biological agents | Integrates one biological agent with one questionnaire assessment |
| `erbio_sector_questionnaires()` | Workplace | Lists sector-specific questionnaire instruments |
| `erbio_validate_workplace_questionnaires()` | Workplace | Checks that the required core and sector questionnaire set is complete |
| `erbio_assess_workplace()` | Workplace | Runs the integral workplace-level ERBio assessment |
| `erbio_build_preventive_plan()` | Prevention | Generates deduplicated preventive-planning candidates from failed controls |
| `erbio_render_workplace_report()` | Reporting | Renders a reproducible workplace assessment as Markdown |
| `erbio_write_workplace_report()` | Reporting | Writes the Markdown report to disk |
| `erbio_export_workplace_bundle()` | Reporting | Exports the complete reproducibility bundle |
| `erbio_version()` | Utilities | Returns the ERBioR package development version |

<hr>

## Installation

```r
# From CRAN (once published):
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

## Minimal usage

### ERBio deterministic calculation

```r
library(ERBioR)

erbio_probability("Aceptable", "Frecuente")
erbio_risk_score(probability = 2, reference_level = 3)
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
erbio_agent_lookup("Bacillus anthracis")

erbio_reference_level_from_agent(
  "Mycobacterium tuberculosis"
)
```

For several agents:

```r
erbio_reference_level_from_agents(
  c("Bacillus anthracis", "Coxiella burnetii")
)
```

By default, discordant groups are preserved rather than silently reduced to one workplace value.

### Questionnaire assessment

```r
erbio_questionnaire_catalog()

general <- erbio_get_questionnaire("general")
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

### Reproducible report

```r
erbio_render_workplace_report(assessment)

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

The workplace workflow requires the three core questionnaires — audit, general and workers — plus at least one applicable sector-specific instrument. Not-applicable responses are excluded from the compliance denominator.

Stable item identifiers and final item numbers are supported so that named responses remain reproducible even if presentation order changes.

Sector-specific questionnaires retain their documented validation status and are not automatically presented as validated instruments where such validation has not been established.

<hr>

## Biological-agent registry

`ERBioR` includes a source-traceable biological-agent registry containing **509 classified entries**.

The legal classification layer is derived from the Spanish regulatory framework, with the 2021 replacement of Annex II used as the primary Spanish row source. The package also contains an independent rowwise semantic audit against the corresponding EU Annex III basis.

Important safeguards are built into the registry logic:

- an unlisted biological agent is **not** silently classified as risk group 1;
- exact and partial lookup behaviour are explicitly separated;
- multiple agents are reported separately when their groups differ;
- the special regulatory treatment of unclassified human viruses is exposed explicitly rather than converted into a false final classification;
- BASEBiO technical information is kept separate from the legal risk group.

<hr>

## Source traceability

The package separates four principal source layers:

1. **ERBio method** — Aguilar-Elena (2015), DOI `10.5281/zenodo.22107465`.
2. **Spanish legal classification** — Annex II of Real Decreto 664/1997, with the replacement introduced by Orden TES/1287/2021.
3. **European concordance** — Annex III of Directive 2000/54/EC as updated by Commission Directive (EU) 2019/1833 and Commission Directive (EU) 2020/739 for SARS-CoV-2.
4. **Technical enrichment** — INSST BASEBiO, kept separate from the legal classification layer.

The v0.8 scientific-audit layer records:

- **509/509** rows subjected to EU semantic rowwise audit;
- **509/509** current/intended biological-agent identity concordance;
- **509/509** risk-group concordance;
- concordance of parsed A/D/T/V and group-3 `**` semantics;
- preservation of one historical Spanish-language 2019 source-text duplication involving `Fusobacterium necrophorum` as provenance rather than silently rewriting the historical source.

Exact raw-text identity across all source versions is not claimed.

The installed provenance assets are available under `inst/extdata/`, while the scientific audit report is distributed as `inst/SCIENTIFIC_AUDIT_v0_8.md`.

<hr>

## Outputs

Depending on the function used, `ERBioR` returns structured R objects and reproducible files containing:

| Output | Description |
|---|---|
| Exposure classification | ERBio exposure category derived from the selected exposure information |
| Compliance result | Percentage and compliance class from questionnaire/control responses |
| Probability | ERBio probability level resulting from exposure and control compliance |
| Reference level | ERBio reference level derived from the resolved biological agent |
| GRB score | Deterministic biological-risk score |
| Risk class | ERBio risk classification associated with the GRB score |
| Action | Action associated with the resulting risk class |
| Failed controls | Questionnaire items requiring preventive consideration |
| Agent summary | Separate resolution and reference-level result for each biological agent |
| Questionnaire summary | Score, compliance, failed controls and source warnings for each instrument |
| Preventive plan | Deduplicated preventive-planning candidates derived from failed controls |
| Markdown report | Human-readable reproducible workplace assessment report |
| Reproducibility bundle | Report and CSV artifacts preserving assessment inputs, results and traceability |

<hr>

## Deliberate safeguards

`ERBioR` is intentionally conservative about information that the original method or regulatory sources do not resolve unambiguously.

In particular, the package does **not**:

- silently classify an unlisted agent as risk group 1;
- silently collapse discordant questionnaire-level risk classes into one global class;
- silently collapse several biological agents into one workplace classification;
- allow BASEBiO technical enrichment to overwrite the legal biological-agent group;
- present an explicit computational extension as though it were an original ERBio source rule.

Where alternative interpretations are computationally useful, they are exposed through explicit policy arguments such as `max_risk_extension` or `explicit_max_group`.

<hr>

## Validation and testing

The frozen ERBioR v0.4 regression layer contains **38 regression blocks** covering the mathematical engine, questionnaire layer, biological-agent layer, workplace assessment, preventive planning and reproducible reporting.

The v0.8 package candidate additionally includes pre-CRAN metadata checks and dedicated scientific-audit tests.

The validated candidate passed `R CMD check --as-cran` on R 4.6.1/macOS aarch64 on 25 August 2026 with:

```text
0 errors | 0 warnings | 0 notes
```

The deterministic v0.4 calculation layer is preserved while later development versions add packaging, documentation, source provenance and scientific-audit assets.

<hr>

## Method reference

The underlying ERBio method is available through the persistent Zenodo DOI:

**Aguilar-Elena, R. (2015). _Riesgos biológicos laborales: ERBio, un nuevo método de evaluación teórica_. Universidad Pública de Navarra.**  
https://doi.org/10.5281/zenodo.22069658

After installation, run:

```r
citation("ERBioR")
```

to obtain the method and software citation information distributed with the package.

<hr>

## Development status

`ERBioR` 0.8.0.9000 is currently a pre-CRAN development candidate.

The package has passed the local `R CMD check --as-cran` validation described above. Multiplatform checks and CRAN submission remain separate release steps.

<hr>

## Author

## Authors

**PhD. Raúl Aguilar-Elena**  
Author of the ERBio method; developer and maintainer of ERBioR.  
Occupational Risk Prevention and Occupational Health Research Group  
Universidad Internacional de Valencia (VIU), Valencia, Spain  
ORCID: 0000-0001-8656-8155  
raguilar@universidadviu.com

**Ana Delgado-García**  
Developer and author of the ERBioR software.  
ORCID: 0009-0003-7079-5507  
a.delgado@usal.es

<hr>

## License

GPL (>= 3) © 2026 Raúl Aguilar-Elena
