# R Package Landscape for artbin

## Overview

artbin provides sample size and power calculations for binary outcome clinical trials,
supporting superiority, non-inferiority, and substantial-superiority designs.

## Existing R Packages

### Most Relevant

| Package | CRAN | Description | Overlap |
|---------|------|-------------|---------|
| `pwr` | Yes | Basic power analysis (Cohen's effect sizes) | Low — does not support NI/SS trials |
| `TrialSize` | Yes | Sample size for clinical trials, including binary outcomes | Moderate — supports NI but fewer methods |
| `clinfun` | Yes | Clinical trial functions including binary outcome SS | Low |
| `PASSED` | Yes | Power and sample size for various study designs | Moderate |
| `samplesize` | Yes | Sample size calculations including NI | Moderate |
| `PowerTOST` | Yes | Power/SS for (bio)equivalence and NI studies | Low — focused on PK/bioequivalence |
| `epiR` | Yes | Epidemiology tools including sample size | Low |
| `Hmisc` | Yes | Miscellaneous, includes some SS functions | Low |

### Most Similar

| Package | CRAN | Notes |
|---------|------|-------|
| `TrialSize` | Yes | Has `TwoSampleProportion.NI.Diff()`, but lacks k-arm, local/distant, and continuity correction as integrated |
| `samplesize` | Yes | Has NI for proportions but limited test options |

## Assessment

No existing R package provides the full feature set of artbin:
- Superiority + non-inferiority + substantial-superiority in one function
- Three methods for null variance estimation (sample, fixed marginal totals, constrained ML)
- Score, Wald, conditional, and trend tests
- K-arm (multi-group) trials
- Local vs distant alternatives
- Continuity correction
- Loss-to-follow-up adjustment
- Integrated allocation ratio handling with per-arm rounding

## Conclusion

A **new R package** is the appropriate route. While contributions to `TrialSize` or `samplesize`
are possible in principle, artbin's scope, internal architecture (dual art2bin/k-group paths),
and validated test suite make a standalone package the better choice. This also preserves the
MRC CTU provenance and allows the package to be cited as a direct translation of the
published Stata software.
