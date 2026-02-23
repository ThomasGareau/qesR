## Test environments

- Local macOS (aarch64), R 4.4.0
- GitHub Actions workflow added for R-devel checks (`.github/workflows/r-devel-check.yml`)

## R CMD check results

### 1) Standard as-CRAN run

Command:

`R CMD check qesR_0.4.4.tar.gz --as-cran`

Result: 0 ERROR, 0 WARNING, 3 NOTE

Notes:

1. CRAN incoming/URL checks could not resolve hosts in this local environment (offline DNS).
2. `unable to verify current time`.
3. HTML manual validation note from the local HTML validator (`<main>` not recognized).

### 2) As-CRAN run with HTML validator disabled (diagnostic)

Command:

`_R_CHECK_RD_VALIDATE_RD2HTML_=FALSE R CMD check qesR_0.4.4.tar.gz --as-cran`

Result: 0 ERROR, 0 WARNING, 2 NOTE

This confirms the additional NOTE above is specific to local HTML validation, not package code issues.

## Changes made before submission

- Removed direct global-environment assignments flagged by `R CMD check`.
- Assignment behavior now targets the calling environment while preserving user-facing workflow.
- Added a dedicated GitHub Actions `r-devel` check workflow.
