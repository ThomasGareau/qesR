## Test environments

- Local macOS (aarch64), R 4.4.0, testthat 3.2.1.1.

## R CMD check results

Command, on the tarball from `R CMD build`:

`R CMD check --as-cran --run-donttest qesR_0.4.4.tar.gz`

Local result (macOS, R 4.4.0): 0 ERRORs, 0 WARNINGs, NOTEs as listed below.
The tests pass (933 pass, 0 fail, 10 skip); every skip gives its reason.

* checking CRAN incoming feasibility ... NOTE

  New submission.

  Found the following (possibly) invalid URLs:
  `https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PAQBDR`,
  Status: 202, Message: Accepted.

  The URL is valid: it is the Harvard Dataverse landing page of the 2022
  Quebec Election Study and opens in a browser. Dataverse answers the
  automated request with 202 Accepted instead of 200.

* checking for future file timestamps ... NOTE

  unable to verify current time. This depends on the check machine reaching
  a time server and is not related to the package.

* checking HTML version of manual ... NOTE (local only)

  The local HTML validator is the macOS system `/usr/bin/tidy` ("HTML Tidy for
  Mac OS X released on 31 October 2006"). It predates HTML5 and reports
  `<main> is not recognized!` for every Rd file, because R's own HTML help
  uses `<main>`. The NOTE does not occur with tidy-html5, and the Rd files
  have no problems of their own. With the validator switched off
  (`_R_CHECK_RD_VALIDATE_RD2HTML_=FALSE`) the NOTE goes away.

* checking examples ... NOTE (network-dependent)

  Seven `\donttest{}` examples each download the 2022 study from Dataverse
  and took 9 to 13 s elapsed (about 4.5 s CPU) locally. The time is the
  download and parse. This NOTE depends on the connection and did not appear
  in every local run.

## Changes made before submission

- Removed direct global-environment assignments flagged by `R CMD check`.
- Assignment behavior now targets the calling environment while preserving user-facing workflow.
- Every function returns its result visibly; `assign_global` defaults to `FALSE` and, when `TRUE`, assigns into the caller's frame through one internal helper. A test scans the installed namespace for any other `assign()` call and for `.GlobalEnv`.
- Removed the download fallback that disabled TLS certificate verification (`download.file.extra = "--insecure"` and a `system2("curl", "--insecure")` shell-out), and `readRDS()` on downloaded files. A test scans the installed namespace for these calls.
- Configured a GitHub Actions `R CMD check --as-cran` matrix (macOS, Windows, Ubuntu devel/release/oldrel-1/4.1) in `.github/workflows/R-CMD-check.yml`. It has not run yet; its results will be added here before submission.
- Restored an offline testthat suite (edition 3); network tests are skipped on CRAN.
