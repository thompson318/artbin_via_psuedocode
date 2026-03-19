# Translation Recommendation: artbin Stata → R

## Recommendation: Create a New R Package

**Package name:** `artbin`

### Rationale

1. **No equivalent package exists** — No CRAN package covers the full design space of artbin
   (superiority + NI + substantial-superiority, k-arm, multiple test types, multiple NVM methods).

2. **Preservation of validated behavior** — The Stata artbin package has been validated against
   published literature (Blackwelder 1982, Julious 2011, Pocock 1983, 2003), commercial software
   (Cytel EAST), and Stata's built-in `power` command. A new R package can be tested against
   the same reference values and Stata test outputs.

3. **Clean API** — The Stata syntax maps naturally to a single R function `artbin(pr, ...)`.

4. **Maintainability** — Standalone package is easier to maintain, version, and cite than a
   contribution to an existing package with different design philosophy.

### Proposed Package Structure

```
artbin (R package)
├── artbin()          — main user-facing function
├── .art2bin()        — internal: 2-arm calculation (from art2bin.ado)
├── .artbin_kgroup()  — internal: k-group calculation (from artbin.ado k-groups section)
└── .utils            — shared utilities (npnchi2, continuity correction, _pe2)
```

### Testing Strategy

- Port all Stata test suites (artbin_testing_1 through artbin_testing_7) to testthat
- Use reference values from Stata test logs as expected values
- Include error condition tests from artbin_errortest_8.do

### Shiny GUI

Recreate artbin.dlg as a Shiny application with equivalent options.
