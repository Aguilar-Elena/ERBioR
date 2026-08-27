# ERBioR v0.7 scientific source audit

## Scope

This audit separates software verification from source-data verification. The frozen v0.4 calculation logic is not changed. Version 0.7 audits the biological-agent data layer and its provenance.

## Spanish legal classification

The primary row source is the official Order TES/1287/2021 (BOE-A-2021-19371), whose article replaces Annex II of RD 664/1997. The consolidated RD 664/1997 remains a secondary convenience reference only.

All 509 operational registry rows are marked `verified_semantic_against_official_order_2021`. This status means that the agent/classification/note semantics were reviewed against the official 2021 Annex II source. It does **not** claim byte-for-byte transcription: `classification_raw` is a normalized semantic field.

Three coronavirus names contained spacing artifacts inherited from the consolidated HTML rendering and were restored from the official PDF:

- SARS-CoV
- SARS-CoV-2
- MERS-CoV

Unusual spellings that are present in the official legal source are preserved in the legal raw-name field rather than silently modernized.

## Internal registry consistency

- 509/509 group classifications are internally consistent with `classification_raw`.
- 509/509 double-asterisk semantics are consistent.
- 509/509 A, D, T and V note flags are consistent with the parsed fields.
- No duplicate agent IDs or BOE line identifiers are introduced.

## BASEBiO technical-link audit

BASEBiO remains a technical enrichment layer and never overrides the legal group. One false-positive candidate was rejected: `BIO-VIR-0080` (Otros Picornaviridae de patogenicidad conocida) had been incorrectly linked to the BASEBiO entry `Otros Coronaviridae de patogenicidad conocida`.

Six other non-exact links were manually reviewed and retained with explicit scope/nomenclature labels. The reviewed retained links include spelling/orthographic normalization, a Guaranito/Guanarito nomenclature difference, lifecycle expansion for *Taenia solium*, synonym abbreviation for *Cladophialophora bantiana*, and a representative species-level Ebola page flagged as a scope mismatch rather than treated as exact equivalence.

After the false-positive rejection, 147 legal registry rows remain linked to BASEBiO entries.

## European concordance

Directive (EU) 2019/1833 is the EU technical update underlying the Spanish 2021 Annex II replacement, and Directive (EU) 2020/739 added SARS-CoV-2. The Spanish transposition basis is verified. However, an **independent row-by-row EUR-Lex comparison of all 509 rows remains pending**. ERBioR v0.7 therefore does not claim full independent EU rowwise concordance.

## Method provenance

The underlying ERBio method is referenced separately from the software using DOI `10.5281/zenodo.22107465`.
