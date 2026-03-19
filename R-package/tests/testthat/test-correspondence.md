# Test Correspondence: Stata → R

This document describes how the R tests correspond to the Stata test suite.

## Stata test files → R test files

| Stata file | R file | Notes |
|-----------|--------|-------|
| `artbin_testing_1.do` | `test-artbin-ni.R` | NI SS and power, allocation ratios |
| `artbin_testing_2.do` | `test-artbin-sup.R` | 2-arm superiority SS and power |
| `artbin_testing_3.do` | `test-artbin-ccorrect.R` | Continuity correction |
| `artbin_testing_4.do` | `test-artbin-ni.R` | Margin option (merged into NI tests) |
| `artbin_testing_5.do` | `test-artbin-sup.R` | EAST comparison (reference values in sup tests) |
| `artbin_testing_6.do` | `test-artbin-ni.R`, `test-artbin-sup.R` | onesided and ccorrect options |
| `artbin_testing_7.do` | `test-artbin-kgroup.R` | Algorithm routing, option permutations |
| `artbin_errortest_8.do` | `test-artbin-errors.R` | Error messages |
| `artbin_test_ltfu.do` | `test-artbin-ltfu.R` | LTFU option (round-trip, STREAM trial) |
| `artbin_test_rounding.do` | `test-artbin-rounding.R` | Per-arm rounding |

## Key reference values

All reference values are taken from Stata v2.1.1 test outputs.

| Call | Expected n | Source |
|------|-----------|--------|
| `artbin(pr=c(0.1,0.1), margin=0.2, alpha=0.1, power=0.9, wald=TRUE)` | 78 (39+39) | Blackwelder 1982 |
| `artbin(pr=c(0.3,0.3), margin=0.05, alpha=0.05, power=0.9, wald=TRUE)` | 3532 | Julious 2011 Table 4 |
| `artbin(pr=c(0.15,0.15), margin=0.15, alpha=0.05, power=0.9, wald=TRUE)` | 240 | Pocock 2003 |
| `artbin(pr=c(0.2,0.2), margin=0.1, alpha=0.2, power=0.8, wald=TRUE)` | 290 | Sealed Envelope |
| `artbin(pr=c(0.05,0.1), alpha=0.05, power=0.9, wald=TRUE)` | 1156 (578+578) | Pocock 1983 |
| `artbin(pr=c(0.9,0.9), margin=-0.05, onesided=TRUE)` | 914 (457+457) | Stata artbin example 3 |
| `artbin(pr=c(0.1,0.2,0.3,0.4), alpha=0.1, power=0.9)` | 176 (44+44+44+44) | Stata artbin example 4 |
| `artbin(pr=c(0.7,0.75), margin=-0.1, wald=TRUE, aratios=c(1,2), ltfu=0.2)` | 398 (133+265) | STREAM trial |

## Notes on differences from Stata

- **Dropped option:** `nchi` (undocumented in Stata) is not implemented in R.
  This option forces 2-arm superiority to use the k-group calculation;
  the R package always uses art2bin for 2-arm trials without `condit`.
- **Score test (nvm=3) vs Wald:** These give different results.
  `artbin(pr=c(0.1,0.05), wald=TRUE)` → 1156; without wald → 1164.
  The Stata `power` command matches artbin's score test (1164).
- **Rounding:** Per-arm ceiling rounding is always done in `artbin()`,
  never inside `.art2bin()` or `.artbin_kgroup()`. This matches Stata v2.1.0+.
