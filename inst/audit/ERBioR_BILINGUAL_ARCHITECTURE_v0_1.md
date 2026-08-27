# ERBioR bilingual architecture — i18n plan v0.1

## Decision

ERBioR will support Spanish (`es`) and English (`en`) as first-class languages.
Language changes presentation only. Scientific calculations, item IDs, agent IDs,
risk matrices and deterministic rules remain single-source and language-neutral.

The Spanish Shiny v0.7 is retained as the functional baseline. The bilingual
work starts in a new v0.8-i18n branch.

## Scope: what “fully bilingual” means

The language selector must control:

1. Shiny navigation, buttons, help text, validation messages and warnings.
2. R console print methods and user-facing errors.
3. Exposure, compliance, probability, risk and priority display labels.
4. Questionnaire names, sector names, section names and all 1,008 question texts.
5. Preventive-action content:
   - causal assessment;
   - corrective action;
   - implementation requirements;
   - effectiveness verification.
6. Excel template:
   - localized visible sheet/header labels;
   - localized questions and response options;
   - language metadata.
7. Imported Excel:
   - Spanish and English workbooks accepted regardless of the current UI language;
   - stable `item_id` and `questionnaire_id` remain the machine identifiers.
8. Results tables, workers dashboard and preventive-planning dashboard.
9. Professional report and reproducibility bundle.
10. Traceability metadata identifying the language used for presentation.

## Architecture

### Canonical scientific layer

Do not duplicate calculations by language.

Internal objects keep:
- stable IDs;
- numeric values;
- deterministic scientific outputs;
- language-neutral/internal status codes where possible.

A display layer maps those outputs to Spanish or English labels.

### R API

Add:

```r
erbio_set_language("es")
erbio_set_language("en")
erbio_get_language()

# or per-call:
erbio_render_workplace_report(x, language = "en")
erbio_excel_template(..., language = "en")
```

The default remains Spanish during development for backward compatibility.

### Shiny

One selector:

`ES | EN`

Switching language must not:
- rerun the scientific assessment;
- alter stored responses;
- alter selected agents;
- alter risk scores.

It only rerenders labels/content in the selected language.

### Excel

A workbook contains a machine-readable metadata field:

`language = es` or `language = en`

Spanish response values:
- Sí
- No
- No procede

English:
- Yes
- No
- Not applicable

Both are normalized to the same internal response codes.

Visible worksheet names may be localized, but import uses aliases and stable IDs.

## Translation governance

### Critical distinction: translation vs validation

The validated Spanish core questionnaires do **not** automatically become
psychometrically validated English questionnaires after translation.

Track separately:

- `source_validation_status`
- `question_translation_status`
- `english_validation_scope`

Recommended English translation statuses:

- `NOT_STARTED`
- `DRAFT_EN`
- `REVIEWED_EN`
- `APPROVED_EN_TRANSLATION`

`APPROVED_EN_TRANSLATION` means linguistic/technical approval of the translation.
It does not mean new psychometric validation unless a separate validation study
has been performed.

Sector-specific questionnaires remain explicitly unvalidated where the Spanish
source is already unvalidated.

### Preventive actions

Each of the 1,008 actions requires separate English fields for:

- cause assessment;
- corrective action;
- implementation requirements;
- verification criteria.

The concatenated English preventive action must be generated from these approved
components, not independently translated as a fifth unrelated text.

## Freeze rule

ERBioR must not be advertised as “fully bilingual” until:

- 1,008/1,008 question translations are present;
- 1,008/1,008 x 4 preventive components are translated;
- all English translations have passed technical review;
- bilingual import/export smoke tests pass;
- the same numerical test cases return identical scientific results in ES and EN.

## Bilingual invariance test

For every locked test case:

`scientific_result(ES input) == scientific_result(EN input)`

The only permitted differences are display-language strings.

This invariance test should become part of the pre-ErBioBench audit.
