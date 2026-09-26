# qesR (development version)

- Help pages are now generated with roxygen2 from comments in `R/`. Their content is unchanged: same topics, arguments, defaults and examples. The package help page is also available as `?qesR` and now lists the authors and project links.
- The test suite is back (testthat 3rd edition). It restores the tests removed before 0.4.4, except one that required the insecure TLS retry, and adds contract tests for the 14 exported functions: their arguments, visible return values, no writes outside `tempdir()` or into your workspace by default, opt-in assignment, and the column names that `get_qes()`, `get_qes_master()`, `get_decon()` and the codebook helpers return. Tests run offline against a simulated Dataverse; the live check against Dataverse runs only when `QESR_LIVE=true`.
- `R CMD check --as-cran` now runs on GitHub Actions for every push and pull request on macOS, Windows and Ubuntu (R devel, release, oldrel-1 and 4.1).

## Workspace assignment

- `get_qes()`, `get_qes_master()` and `get_decon()` no longer write into your workspace by default (`assign_global` now defaults to `FALSE`; it was `TRUE` in 0.4.4). They always return the data, visibly: write `qes2018 <- get_qes("qes2018")`. The first call in a session that leaves `assign_global` unset prints a one-time note (class `qesR_message_assign_default`); passing `assign_global` explicitly avoids it.
- With `assign_global = TRUE`, every function now assigns into the environment it was called from: the global environment at top level, the function's own frame when called inside a function. `get_qes_codebook()` and `qes_codebook()` previously assigned into a frame of their own, so the object never reached you.
- `get_qes(assign_global = TRUE)` still assigns both `<code>` and, with `with_codebook = TRUE`, `<code>_codebook`.
- Study codes are trimmed and case-insensitive: `get_qes(" QES2018 ")` reads `qes2018`, assigns `qes2018` and stores `"qes2018"` in `attr(, "qes_survey_code")` (0.4.4 stored the code as typed). `get_qes_master(surveys = "all")` means every study. Codes are never matched fuzzily: `"2018"` is an error that suggests `qes2018`.
- `get_codebook(assign_global = TRUE)` and its aliases now assign exactly the object they return, in the requested `layout` (0.4.4 assigned the compact codebook whatever the layout).
- `get_qes_master(save_path =, assign_global = TRUE)` now sets `saved_to` before assigning, so the assigned and returned objects are identical.

## Messages and errors

- Every error, warning and message that qesR itself raises now has a class, so scripts can handle them with `tryCatch()` without matching text. Errors inherit from `qesR_error` (`qesR_error_input`, `qesR_error_unknown_study`, `qesR_error_unknown_variable`, `qesR_error_ambiguous_file`, `qesR_error_network`, `qesR_error_source`), warnings from `qesR_warning` and messages from `qesR_message`. Conditions carry data fields such as `study`, `suggestions`, `url` and the root cause in `parent`. See `?qesR`.
- Messages, warnings and errors are available in English and French. qesR follows `options(qesR.lang =)`, then the `QESR_LANG` and `LANGUAGE` environment variables, then the messages locale. The language never changes what functions return: qesR's own part of the reasons recorded in `attr(get_qes_master(), "failed_surveys")` is always in English (a root cause reported by R itself, such as a download error, keeps R's text).
- Unknown study codes suggest near matches.

## Downloads

- Removed the insecure download fallback. qesR 0.4.4 retried a failed download with TLS certificate verification turned off (for `qes2022` always, and for any study whose first error mentioned SSL), first through `download.file()` and then by running the `curl --insecure` command. A failed download is now an error of class `qesR_error_network` that keeps the original error; qesR never disables certificate checks and never runs external download programs.
- Requests now identify themselves only as `qesR/<version> R/<version>` and are spaced at least one second apart per server.
- qesR no longer reads `.rds` files from Dataverse: `readRDS()` is never applied to downloaded content.

## Soft-deprecated names (kept indefinitely)

- Eleven 0.4.4 helpers become legacy wrappers. They keep their arguments and output, keep working and will not be removed. Each prints a one-time notice (class `qesR_message_deprecated`) naming its replacement, but only once that replacement exists. `quiet = TRUE` does not hide the notice; `options(qesR.quiet_deprecated = TRUE)` does. The table is in `?qesR-deprecated`.
- Notices shown now: `get_codebook()` and `get_qes_codebook()` (use `qes_codebook()`, same arguments) and `get_preview()` (use `head(get_qes(srvy), obs)`).
- Silent until their replacements ship: `format_codebook()`, `get_value_labels()`, `get_question()`, `get_codebook_files()`, `get_qes_codebook_files()`, `download_codebook()` and `get_qescodes()`. `get_decon()` stays stable until 0.7.0.

# qesR 0.4.4

- Added `get_qes_master()` for harmonized merged datasets across QES studies.
- Added merge quality controls: within-year respondent de-duplication and all-empty-row removal.
- Added broader harmonization for age, age group, education, turnout, vote choice, and weights.
- Renamed opaque legacy merged variables (e.g., `q10`, `q16`, `voteprec`) to readable names and added transparent old-to-new mapping via `variable_name_map`.
- Added variable-name mapping sidecar output (`*_variable_name_map.csv`) when saving/building merged datasets.
- Improved codebook workflows with compact/wide/long layouts and value-label extraction helpers.
- Updated researcher website use-case figures with clearer year-axis labeling, dynamic y-axis bounds, and expandable full code blocks for reproducibility.
- Added researcher website content with merged-dataset documentation, study citations, and analysis examples.

# qesR 0.4.3

- Added `get_question(..., full = TRUE)` support for richer question-text recovery.
- Improved codebook metadata parsing and handling of support files.

# qesR 0.4.2

- Expanded QES catalog entries and Dataverse file selection behavior.

# qesR 0.4.1

- Added `get_decon()` and export updates.

# qesR 0.4.0

- Added codebook retrieval and formatting helpers.
