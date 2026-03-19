# Contributing to artbin

Thank you for your interest in contributing to artbin! This R package is a
translation of the Stata artbin package maintained by the MRC Clinical Trials
Unit at UCL.

## Reporting issues

Please open a GitHub issue for:
- Bugs or incorrect sample size / power calculations
- Discrepancies between R and Stata artbin results
- Missing features or documentation gaps

When reporting a calculation discrepancy, please include:
1. The R function call and output
2. The equivalent Stata command and output (if available)
3. The expected result (with reference, if any)

## Development workflow

1. Fork the repository and create a feature branch
2. Make your changes
3. Add or update tests in `tests/testthat/`
4. Run `devtools::check()` — there must be 0 errors, 0 warnings, 0 notes
5. Open a pull request with a clear description of the change

## Testing

Run the test suite with:

```r
devtools::test()
```

All tests are in `tests/testthat/` and are organized by topic:

| File | Coverage |
|------|----------|
| `test-artbin-ni.R` | Non-inferiority and substantial-superiority |
| `test-artbin-sup.R` | 2-arm superiority |
| `test-artbin-kgroup.R` | Multi-arm (k-group) trials |
| `test-artbin-ltfu.R` | Loss-to-follow-up option |
| `test-artbin-rounding.R` | Per-arm rounding behaviour |
| `test-artbin-ccorrect.R` | Continuity correction |
| `test-artbin-errors.R` | Error handling |

Reference values are taken from the Stata artbin test suite
(`testing/artbin_testing_*.do`) and published literature.

## Code style

- Follow base R style (no tidyverse in the package itself)
- Internal functions are prefixed with `.`
- Each exported function should have a roxygen2 doc block
- Keep translation close to the Stata source where possible,
  to make future sync easier

## Versioning

The R package version tracks the Stata package version. Do not bump the
version in `DESCRIPTION` unless releasing a new version.

## License

By contributing, you agree that your contributions will be licensed under
the GPL-3 license. See `LICENSE` for details.
