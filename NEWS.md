# qesR (development version)

- Help pages are now generated with roxygen2 from comments in `R/`. Their content is unchanged: same topics, arguments, defaults and examples. The package help page is also available as `?qesR` and now lists the authors and project links.
- The test suite is back (testthat 3rd edition). It restores the tests removed before 0.4.4, except one that required the insecure TLS retry, and adds contract tests for the 14 exported functions: their arguments, visible return values, no writes outside `tempdir()` or into your workspace by default, opt-in assignment, and the column names that `get_qes()`, `get_qes_master()`, `get_decon()` and the codebook helpers return. Tests run offline against a simulated Dataverse; the live check against Dataverse runs only when `QESR_LIVE=true`.
- `R CMD check --as-cran` now runs on GitHub Actions for every push and pull request on macOS, Windows and Ubuntu (R devel, release, oldrel-1 and 4.1).

## Study catalog, documents and citations

- New `qes_studies()` lists every study from a catalog shipped with the package, with no network request: code, family, deposit title and authors, English and French display names, year, design, target population, server, DOI, the pinned dataset version and data file, source language, licence and the dataset UNF. `qes_studies(check_updates = TRUE)` asks Dataverse, one metadata request per deposit, whether a newer version exists or the pinned data file changed; a study that cannot be checked is reported as `"unreachable"` instead of failing.
- New `qes_docs()` lists the codebooks, questionnaires and technical or methodological reports of each study, with their role, language, size, md5 and download URL, offline.
- New `qes_cite()` returns the citation of qesR and of each dataset (authors, year, verbatim deposit title, DOI, repository, pinned version and UNF) as text, BibTeX or `bibentry`. `citation("qesR")` now works (static `inst/CITATION`). The citation vignettes are generated from the catalog; the hand-copied tables, which carried a stray `[fileUNF]` token and non-DOI links, are gone from the README and vignettes.
- The 1998 deposit is split into its three surveys: `qes1998` still reads the combined CROP-CREATEC panel file (1,483 rows, as in 0.4.4), and the new codes `qes1998_crop` (450 rows) and `qes1998_createc` (1,057 rows) read the two firms' files. All three cover francophones only, each with the definition its codebook gives.
- The Durand panels, the CROP polls and the 1998 polls are no longer presented as "Quebec Election Study" panels: `get_qescodes(detailed = TRUE)` and the `get_qes()` banner use their own titles (for example "Panel Survey on the 2018 Quebec Election"), and the same names now appear in the `qes_name_en` column of `get_qes_master()` for those studies. French names carry their accents. `documentation` is the doi.org link.
- `get_qes_master()` keeps building the 11 studies of 0.4.4 by default, and `surveys = "all"` keeps meaning those 11; the new 1998 codes are built only when named.
- The catalog is versioned (`inst/extdata/VERSIONS`), and a synthetic demonstration study, `qes_demo`, ships in its own tree for offline examples and tests (loading it with `get_qes()` arrives with the new reader).

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
- Notices shown from this version: `get_qescodes()` (use `qes_studies()`), and `get_codebook_files()` and `get_qes_codebook_files()` (use `qes_docs()`). These two now return every document of the study from the offline catalog, with the 0.4.4 columns, and no longer download metadata; their `file` and `refresh` arguments are ignored, with a one-time note (class `qesR_message_arg_ignored`).
- Silent until their replacements ship: `format_codebook()`, `get_value_labels()`, `get_question()` and `download_codebook()`. `get_decon()` stays stable until 0.7.0.

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
