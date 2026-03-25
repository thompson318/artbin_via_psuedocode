# artbin Project Memory

## Repository structure
- `/home/thompson/software/artbin/` - git repo root
- `package/` - Stata package (artbin.ado, art2bin.ado)
- `testing/` - Stata test files and logs
- `examples/` - Stata examples
- `R-package/` - R tidyverse package (rebuilt 2026-03-18)

## R Package (R-package/)
- Translates Stata artbin/art2bin to R
- 0 errors, 0 warnings, 3 NOTEs (expected) on R CMD check (built 2026-03-19)
- 138 tests all passing
- Entry point: `artbin(pr, ...)` in `R/artbin.R`
- Internal: `.art2bin()` in `R/art2bin.R`, `.artbin_kgroup()` in `R/kgroup.R`, utils in `R/utils.R`
- Test files: test-artbin-ni.R, test-artbin-sup.R, test-artbin-ccorrect.R, test-artbin-kgroup.R, test-artbin-ltfu.R, test-artbin-rounding.R, test-artbin-errors.R
- Also includes: Shiny GUI (inst/shiny/artbin_app/app.R), GitHub Actions (.github/workflows/R-CMD-check.yaml), CONTRIBUTING.md
- Analysis in Stata2R/, pseudocode in pseudocode/

## Key design decisions
- Two-arm path uses `.art2bin()` (translated from art2bin.ado)
- K-group / conditional path uses `.artbin_kgroup()` (translated from artbin.ado k-groups section)
- Rounding always done in `artbin()` after unrounded calculation (matches v2.1.x Stata behavior)
- `nvmethod` defaults to NULL in signature; set to 1 if `wald=TRUE`, else 3

## Reference test values (current Stata v2.1.1)
- `artbin(pr=c(0.1,0.05), alpha=0.05, power=0.9, wald=TRUE)` → n=1156
- `artbin(pr=c(0.9,0.9), margin=-0.05, onesided=TRUE)` → n=914 (457 per arm)
- `artbin(pr=c(0.1,0.2,0.3,0.4), alpha=0.1, power=0.9)` → n=176 (44 per arm)
- STREAM: `artbin(pr=c(0.7,0.75), margin=-0.1, wald=TRUE, aratios=c(1,2), ltfu=0.2)` → n=398 (133+265)
  - Note: artbin_examples.do comment says 399/266 (was v2.0.2 behavior with double rounding)
- NI Blackwelder: `artbin(pr=c(0.1,0.1), margin=0.2, alpha=0.1, power=0.9, wald=TRUE)` → n=78 (39+39)

## Stata notation vs R
- Stata's `invnormal(p)` = R's `qnorm(p)`
- Stata's `normprob(x)` = R's `pnorm(x)`
- Stata's `invchi2(df, p)` = R's `qchisq(p, df)` [note arg order]
- Stata's `nchi2(df, ncp, x)` = R's `pchisq(x, df, ncp)` [note arg order]
- Stata's `npnchi2(df, x, p)` = custom `.npnchi2(df, x, p)` via uniroot
