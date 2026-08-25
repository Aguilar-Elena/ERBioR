## ERBioR 0.8.0.9000 scientific-audit status

This development candidate carries forward the frozen v0.4 computational regression suite and the v0.7 Spanish/BASEBiO source audit. Version 0.8 adds an independent semantic row-by-row audit of the 509-entry biological-agent registry against the official EU Annex III basis (508 rows from Directive (EU) 2019/1833 and the SARS-CoV-2 insertion from Directive (EU) 2020/739). One historical Spanish-language 2019 source-text duplication is documented and resolved at the semantic/current-intended identity level through source-history cross-checking; no unresolved name or risk-group discrepancy was identified. Exact raw-text identity is not claimed.

The user should rerun `devtools::test()` and `devtools::check()` on this candidate before release.
