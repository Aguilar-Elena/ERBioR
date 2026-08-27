# ERBioR v0.8 independent EU regulatory audit

## Scope

Version 0.8 completes the independent EU-source audit of the 509 classified biological-agent registry entries. It does not alter the frozen ERBio v0.4 deterministic calculation logic.

## EU legal sources

- Commission Directive (EU) 2019/1833 replaced Annex III of Directive 2000/54/EC and provides the updated Community classification.
- Commission Directive (EU) 2020/739 inserted SARS-CoV-2 into Annex III as group 3.

The audit compares the operational Spanish legal registry against those EU sources as a separate source-verification layer rather than inferring EU concordance merely from Spanish transposition.

## Result

- 508 classified entries are attributed to Directive (EU) 2019/1833.
- 1 classified entry (SARS-CoV-2) is attributed to Directive (EU) 2020/739.
- 509/509 registry rows are marked `eu_rowwise_verified = TRUE`.
- 509/509 risk-group semantics are concordant.
- 509/509 note semantics (including A/D/T/V where applicable) are concordant.

Unclassified virus order/family/genus headings in the EU table are not represented as agent-registry rows. Classified family/genus-level entries are retained where the EU table explicitly assigns a risk group.

## Documented source-language discrepancy: Fusobacterium

The Spanish-language 2019 EU table prints `Fusobacterium necrophorum subsp. funduliforme` twice. The authentic English EU text identifies the second row as `Fusobacterium necrophorum subsp. necrophorum`, which also matches Order TES/1287/2021. ERBioR therefore retains `Fusobacterium necrophorum subsp. necrophorum` for `BIO-BACT-0076` and records the multilingual resolution explicitly. The classification is group 2 in all relevant sources.

EUR-Lex metadata shows corrigenda to Directive 2019/1833 for several language versions, but the listed corrigenda do not concern Spanish. Therefore ERBioR does not describe the Spanish duplicate as an officially corrected Spanish-language text; it is recorded as a multilingual source discrepancy resolved using another authentic EU language version plus the Spanish transposition.

## SARS-CoV-2

`BIO-VIR-0064` is sourced specifically to Directive (EU) 2020/739 rather than retrospectively attributed to Directive 2019/1833. The EU amendment inserts SARS-CoV-2 between SARS-CoV and MERS-CoV with group 3 classification.

## Non-substantive normalization cases

- The Poliovirus type 2 footnote number differs between the EU table and the Spanish transposition; agent identity, group 3 and V note are unchanged.
- Six source-rendering classification punctuation cases are stored semantically (`2` or `3 (**)`) without source punctuation.

## Source hierarchy after v0.8

1. ERBio method: DOI 10.5281/zenodo.22107465.
2. Spanish legal classification: Order TES/1287/2021 (plus the 2020 SARS-CoV-2 transposition history), with consolidated RD 664/1997 as secondary reference.
3. Independent EU control: Directive (EU) 2019/1833 + Directive (EU) 2020/739.
4. INSST BASEBiO: technical enrichment only; it never overrides legal classification.

## Remaining scientific audit before v1.0

The biological-agent regulatory layer is now audited in both Spanish and EU sources. The remaining major source-verification task is the dedicated item-by-item audit of the 1,008 questionnaire-bank items against the ERBio thesis/Zenodo source and their validation lineage.


## Spanish-transposition spacing differences

Two rows preserve the exact Spanish transposition rendering while documenting that the EU Annex III inserts a space after `subsp.`:

- `BIO-BACT-0071`: `Francisella tularensis subsp.holarctica` vs EU `subsp. holarctica`.
- `BIO-BACT-0101`: `Mycobacterium avium subsp.avium` vs EU `subsp. avium`.

These are semantic-equivalence/source-layout cases only. Risk group and note semantics are unchanged.
