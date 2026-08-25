# ERBioR v0.8 scientific source audit: independent EU Annex III concordance

## Scope

The 509-row Spanish legal registry used by ERBioR was audited against the EU Annex III classification basis. The 508 non-SARS-CoV-2 entries were reviewed against Directive (EU) 2019/1833 and the SARS-CoV-2 row against Directive (EU) 2020/739. The audit also uses the consolidated Directive 2000/54/EC text as a documentary cross-check and records relevant source-history evidence.

The audit is semantic and rowwise. It verifies biological-agent identity, risk group, the group-3 double-asterisk annotation, and parsed A/D/T/V indicators. It does **not** claim byte-for-byte identity across publications or language versions.

## Results

- Rows audited: **509/509**.
- Current/intended EU biological-agent identity concordance: **509/509**.
- Risk-group concordance: **509/509**.
- Group-3 (**) semantic concordance: **509/509**.
- Parsed A/D/T/V indicator concordance: **509/509** for each indicator.
- Unresolved substantive current name discrepancies: **0**.
- Historical Spanish-language 2019 original-text anomalies tracked: **1**.

## Source-history anomaly: Fusobacterium necrophorum

The Spanish-language original publication/mirror of Directive (EU) 2019/1833 displays `Fusobacterium necrophorum subsp. funduliforme` twice. The Spanish transposition in Order TES/1287/2021 contains `subsp. funduliforme` followed by `subsp. necrophorum`, both group 2. Current consolidated EU versions in multiple other language views show `subsp. necrophorum` in the second row. A 2023 EU corrigendum applying to affected language versions explicitly changes the second `funduliforme` occurrence to `necrophorum`. ERBioR therefore treats the biological-agent identity as currently/intentionally concordant, while preserving the historical Spanish-language source-text anomaly as provenance. No risk-group discrepancy exists.

The same corrigendum also contains an `Orthopoxvirus (G)` textual correction in affected language versions; this is a taxonomy/header correction and does not change an ERBioR classified agent row.

## SARS-CoV-2

Directive (EU) 2020/739 inserted SARS-CoV-2 between SARS-CoV and MERS-CoV in Annex III and classified it as risk group 3. The ERBioR row is concordant.

## Primary and cross-check sources

- Directive (EU) 2019/1833: https://www.boe.es/buscar/doc.php?id=DOUE-L-2019-81658
- Directive (EU) 2020/739: https://www.boe.es/buscar/doc.php?id=DOUE-L-2020-80871
- Consolidated Directive 2000/54/EC: https://eur-lex.europa.eu/eli/dir/2000/54/2020-06-24
- 2023 corrigendum: https://data.europa.eu/eli/dir/2019/1833/corrigendum/2023-11-03/oj
- Order TES/1287/2021: https://www.boe.es/buscar/doc.php?id=BOE-A-2021-19371
- ERBio method DOI: https://doi.org/10.5281/zenodo.22069658
