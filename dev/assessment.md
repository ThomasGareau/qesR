# qesR assessment (branch `redesign`, HEAD c154985, version 0.4.4)

Date: 2026-09-26. Scope: all of `R/`, `man/`, `vignettes/`, `_pkgdown.yml`, DESCRIPTION/LICENSE, CI, repository root. Evidence comes from:

- reading the code;
- a clean `R CMD build` and `R CMD check --as-cran` in a scratch copy;
- exercising every export against live Borealis and Harvard Dataverse (one to a few requests per study);
- rebuilding `get_qes_master()`;
- an independent verification pass on 179 candidate findings.

Findings the verifiers only partly confirmed are marked **(partial)** and carry the corrected detail. Refuted claims are listed in Appendix A. The only files written in the repository are this one and `dev/downstream-impact.md`. All line numbers refer to the current working tree, which has no content diff against HEAD.

---

## 1. Executive summary

- **The download layer works for the happy path, and not much else.** `get_qes("qes1998")` returns a labelled 1483 x 42 data frame in about 7 s. But the package downloads Dataverse's ingested `.tab` exports instead of the original `.sav`/`.dta`. It then rebuilds the labels from DDI XML, scraped PDFs and hand-written overrides, and that route is wrong in several verified ways. For example, qes1998 data get value labels from a *different* file (the CREATEC poll).
- **Harmonization, the package's main selling point, currently gives wrong results.**
  - `get_qes_master()` stacks any raw column name that appears in two or more studies, then renames it with a label taken from the 2007 questionnaire. `vote_federal_2006` holds 2012 satisfaction with democracy; `feeling_david_0_100` holds 2018 abortion attitudes.
  - Core variables have validity errors:
    - `party_best` holds importance ratings.
    - 2018 interest is inverted.
    - 2018 people born in other provinces are coded as not Canadian-born.
    - 2018 ideology is entirely NA.
    - 380 real qes2007_panel respondents are silently deduplicated away.
    - Intention and reported vote are pooled into one variable.
- **Downstream users exist, so every fix changes someone's numbers.** The owner's paper (OJ_WelfareQuebec) calls `get_qes()` 56 times and `get_qes_master()` 3 times. Every export must keep working with the same call signature. Its article pipeline is not affected by any data bug, but three exploratory and chapter files that use the master are (§10). Fixes must ship as a new version, and the paper should pin the qesR version it used.
- **Four documented features do nothing.** One attribute-stripping bug means `get_codebook_files()`, `get_qes_codebook_files()` and `download_codebook()` return 0 rows for every study, and the PDF expansion in `get_question(full = TRUE)` never runs.
- **Security: downloads silently fall back to `curl --insecure`.** This happens on any SSL error, and on any error at all for qes2022. A CRAN reviewer or security-minded user will object.
- **CRAN status: mechanically clean, substantively open.** The check gives 0 ERROR, 0 WARNING and 3 NOTEs, all environmental or expected. But three of the six reviewer requests are only partly met, and the copyright/licence issue (LICENSE names the data owners as the copyright holders of the MIT code, and CC BY-NC 2022 microdata ship inside an MIT package) is unresolved and unexplained. cesR, the package qesR copies, was archived from CRAN in 2025-10.
- **There are no tests at all.** A 700-line testthat suite was deleted in 397aa0c. Nearly every bug listed here would have been caught by a small offline fixture test.
- **Documentation looks finished but misleads.** On CRAN the analysis vignettes run on an undocumented 300-rows-per-study sample (for example N = 10 for 2010, with confidence intervals). The website runs on the tracked 40,606-row root file, which was built by older code (today's rebuild gives 39,132 rows), so the two report different numbers.
- **Silent data loss and corruption paths:** the text reader silently drops rows whenever R's messages are not in English (D10). This is live: in a French session `get_qes_master()` fails to read 1,824 panel rows, which (net of deduplication) explains the whole 1,474-row gap between the 40,606-row and 39,132-row masters. `get_question()` silently answers for a different variable (A6), and CROP text in the master is CP850 mojibake (H4).
- **Worth keeping:** a single catalog with correct DOIs, three light Imports, clean temp-file hygiene, provenance attributes on the master, and a bilingual vignette set. The data-access core is worth keeping. The harmonization layer should be rebuilt around a declarative crosswalk, behind the existing `get_qes_master()` signature.

---

## 2. Scorecard

The dimension readers' scores were inputs. The scores below are recalibrated after verification.

| Dimension | Score /10 | Rationale |
|---|---|---|
| Download / Dataverse | **3** | DOI-based lookup and temp hygiene are sound. But it takes ingested `.tab` rather than originals, applies the wrong file's DDI in multi-file studies, silently disables TLS verification, has no working retry, backoff, timeout increase or User-Agent, and pins no version. |
| Catalog / metadata | **4** | All 11 DOIs resolve and every UNF matches. The catalog has 8 columns: no N, design, dates, weights, licence, version or file id. Three Durand panels are branded "QES". Citations are copied by hand in three places and are already stale (V1 vs V1.1). |
| Codebooks / labels | **3** | A real `qes_codebook` class with three layouts. But metadata attributes are dropped, 369/718 of the 2022 questions are cut at 80 characters, 246/254 of the 2018 labels are just the variable name, question = label in 718/718 rows, output depends on which OS tools are installed, and there is no EN/FR support. |
| Harmonization | **2** | Every harmonized column records its source variable, and a few overrides are verified correct. But automatic name-based stacking merges unrelated questions, at least 10 core variables have verified validity errors, 380 respondents are dropped, and there is no crosswalk spec, no tests and no documentation per variable. |
| API / naming | **4** | Survey-code access is learnable and the errors are clear. But there are 14 exports for 11 functions, three names collide with cesR/labelled, `assign_global` survives (and is broken through the aliases), Rd and roxygen are two sources of truth, argument names are inconsistent, and nothing is bilingual. |
| Docs / vignettes / pkgdown | **4** | 12 EN/FR vignettes and a site that deploys. But CRAN and the site show different numbers, the analyses pool incompatible designs without weights, some statements are factually wrong, the "show code" blocks have drifted from the code that runs, and FR is thinner. |
| CRAN / CI | **5** | 0/0/3 check. Copyright is unresolved, `cran-comments.md` is unchanged, 2 vignettes run no code, network examples `stop()` when offline, and CI is one ubuntu r-devel job on `main` only. |
| Performance / caching | **3** | Only an in-session codebook cache. Every call re-fetches metadata, DDI, PDF and data. Metadata helpers download the full data file. The master build takes about 73 s and 46 requests, about half of them wasted. |
| Tests | **0** | `tests/` is empty. |
| Observed runtime behaviour | **4** | Nothing is written to home, getwd or cache dirs, and bad input gives clear errors. Silent wrong values appear in `get_decon()` (-99 codes leak; ideology mean 2.6), in master variables, and in 0-row helpers. |

---

## 3. R CMD check baseline and CRAN reviewer requests

### 3.1 Baseline

Commands, all in the scratchpad: rsync the repo excluding `._*` and `.git`, run `R CMD build`, then run `_R_CHECK_CRAN_INCOMING_REMOTE_=true R CMD check --as-cran qesR_0.4.4.tar.gz`. Environment: R 4.4.0 aarch64, macOS 26.6.2, `/usr/bin/tidy` from 2006, no qpdf.

**Result: 0 ERROR, 0 WARNING, 3 NOTEs.** The run took 3 min 14 s, including `--run-donttest`, which hit Dataverse live (examples 35 s CPU / 82 s elapsed).

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Thomas Gareau-Paquette <tg454@cornell.edu>'
New submission
Found the following (possibly) invalid URLs:
  URL: https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PAQBDR
    From: inst/doc/fr-citations-etudes.html inst/doc/study-citations.html README.md
    Status: 202
    Message: Accepted
* checking for future file timestamps ... NOTE
unable to verify current time
* checking HTML version of manual ... NOTE
download_codebook.html:15:44 (download_codebook.Rd:3): Error: <main> is not recognized!
[... same pattern for all 15 Rd files ...]
Status: 3 NOTEs
Build log: Removed empty directory 'qesR/tests' / Omitted 'LazyData' from DESCRIPTION
```

What each NOTE means:

- **New submission:** unavoidable on a first submission. Say so in `cran-comments.md`, which currently does not.
- **202 URL:** an AWS WAF bot challenge on Harvard's `dataset.xhtml` (`x-amzn-waf-action: challenge`). The link works. Linking only `https://doi.org/...` avoids the NOTE; the doi.org form was not flagged.
- **Time and HTML NOTEs:** caused by the local environment. The HTML one comes from Apple's 2006 tidy, and CRAN's tidy-html5 accepts `<main>`.
- **Tarball:** 687,907 bytes, 84 entries, no leaked artifacts. `man/figures/logo.png` (309 KB, 512x601) is 45% of the tarball.

### 3.2 CRAN reviewer (B. Altmann, 2026-02-27) and status on c154985

| # | Request | Status | Evidence |
|---|---|---|---|
| 1 | Method references in Description, `authors (year) <doi:...>` | **Partial (acceptable)** | DESCRIPTION:18-21 adds only `<https://csdc-cecd.ca/portail-quebecois-sur-lopinion-publique/>`. The reviewer allowed the `<https:>` fallback. No dataset DOI is cited, even though R/qes_catalog.R:56-67 holds 11. |
| 2 | Small executable examples | **Resolved, weakly** | All 15 Rd files have `\examples`. Only `get_qescodes()` (man/get_qescodes.Rd, man/qesR-package.Rd) runs offline. Even the in-memory helpers are network-only `\donttest` (man/format_codebook.Rd:13-18, man/get_value_labels.Rd:14-19, man/get_question.Rd:14-19). |
| 3 | Vignettes should execute code | **Partial** | vignettes/study-citations.Rmd and fr-citations-etudes.Rmd have 0 chunks. get-started.Rmd:18,47,54,62,70,88 and fr-demarrage.Rmd:19,45 are `eval = FALSE`. Only `get_qescodes()` runs. |
| 4 | No writes to home/getwd; no default paths (R/qes_download.R) | **Resolved in code** | The getwd roots are gone (HEAD has 0 `getwd` in qes_download.R, and qes_download.R:1197-1204 now uses `system.file`). `download_codebook(dest_dir = tempdir())` (codebook.R:314). Leftovers: README.md:49-50 and pkgdown/index.md:65-66 still show `save_path = "qes_master.csv"`. The local-codebook path is now dead code (see D5). |
| 5 | Do not modify .GlobalEnv (R/get_qes.R) | **Mostly** | Defaults flipped to `assign_global = FALSE` (get_qes.R:18, decon.R:89, master.R:1736). Remaining problems: `.assign_into_caller(..., envir = parent.frame(2L))` (assign_utils.R:1-4) still writes to .GlobalEnv at top level when opted in. The argument is still called `assign_global`. man/get_codebook.Rd:18, get_qes_codebook.Rd:18, qes_codebook.Rd:18 and get_decon.Rd:10 still say "global environment". README.md:78 says "assign object qes2018 into .GlobalEnv". cran-comments.md:34-35 says the workflow is preserved. |
| 6 | All copyright holders in Authors@R; explain in submission comments | **Not resolved** | DESCRIPTION:13-16 adds `person(given = "Quebec Election Study", role = "cph")`. LICENSE:2 still reads `COPYRIGHT HOLDER: Quebec Election Study/Étude électorale québécoise rightful owners`, the exact line the reviewer quoted. LICENSE.md:3 says `qesR contributors`. There is no inst/COPYRIGHTS. cran-comments.md has no resubmission section. No decision is recorded on the `cph` entry: "Quebec Election Study" is not a legal person, and the only QES-origin content in the tarball is ASCII-folded questionnaire wording (qes_download.R:488-683) plus the derived 2022 rows in inst/extdata. Either drop the entry (once those rows go) or name the actual rights holder, and explain the choice in cran-comments. Minor, unverified against the reviewer: DESCRIPTION:18 leaves "Dataverse" unquoted; CRAN usually wants software/API names in single quotes. |

qesR is not on CRAN: the package page, the Archive and the incoming queues all return 404 or nothing.

---

## 4. What happened when the functions were exercised

Setup: scratch install, a fresh working directory per script, and snapshots of globalenv, getwd, tempdir, `~` and `~/Library/Caches` before and after. French locale.

| Call | Result |
|---|---|
| `get_qescodes()` | 0 s, 11x3. The column `get_qes_call_char` holds quoted strings such as `"\"qes2022\""`. `name_fr` has no accents. |
| `get_qes("qes2022")` | 14.7 s. Base data.frame 1521x718: 372 labelled, 294 integer, 41 character (dates stay character). Prints "Codebook/support files available: 0" although it had just downloaded the 648 KB codebook PDF. 234 numeric and 28 character columns contain -99; the original `.dta` has the same -99 values, so qesR does not introduce them. `cps_yob` is coded 1..92 with the years as labels. A second call took 10.7 s: full re-download. |
| `get_qes("qes1998")` | 6.7 s, 1483x42. 6 requests (metadata, 3 DDI, PDF, data). Silently loads 1 of 3 surveys. |
| `get_qes("qes1998", file = "CROP")` | 8.3 s, then `Unsupported file extension 'pdf'`: the regex matched `LivredeCodes_CROP_1998.pdf`. |
| `get_codebook("qes2022")` | 10.7 s (downloads the full data again). Prints `<qes_codebook>` with no survey or DOI. The second call is 0 s (cache). `print(cb, n = 3)` errors "choix de 'na.print' incorrect". |
| `get_codebook_files()`, `download_codebook()` | 0 rows / "No codebook/support files found" for every study. Creates an empty `dest_dir`. |
| `get_question(d22, "cps_interest_1")` | Returns "...Set the slider to a number from 0". The text is truncated and `full = TRUE` does not fix it. |
| `get_preview("qes2007", 3)` | 5.2 s; downloads the whole study to show 3 rows. `obs = 2.7` silently becomes 2. |
| `get_decon()` | 10.6 s. Factors carry '-99' levels. Ideology minimum is -99 and its mean is 2.64 (about 4.96 once the codes are removed). |
| `get_decon("qes1998")` | 17 of 18 target columns are all-NA, with no warning. |
| `get_qes_master(save_path = "m.rds", assign_global = TRUE)` | 78 s, 39,132 x 100, 11/11 studies, 46 requests. The assigned global copy has `saved_to = NULL` while the returned copy has the path. CROP is 61% of rows. The tracked root `qes_master.csv` (Feb 22) has 40,606 rows (CROP 24,026 = 59%), so it was produced by older code. The 1,474-row gap is D10: the fresh build ran in a French-message session and lost 858 qes2007_panel and 616 qes2018_panel rows. |
| Error paths | `"QES2022"`, `"2022"` and `"qes 2022"` give "Unknown survey code" with no suggestion. `lang = "fr"` gives "argument inutilisé". Offline, the call fails in 0 s; the root cause appears only as a separate warning. `get_qes_master` offline says "No studies could be loaded" and drops the per-study reasons it collected. |
| Side effects | None under home, caches or `R_user_dir`. `tempdir()` is clean after every call. Files in getwd appear only from explicit `save_path` or `dest_dir`. |

---

## 5. Verified problems

These findings are deduplicated across the nine review dimensions. Each one is filed under the dimension where it is best fixed. Severities are post-verification.

### 5.1 Download / Dataverse

**D1. Silent TLS-verification bypass (high).** R/qes_download.R:67-68, 85-88, 105-122; R/qes_catalog.R:73.
```r
should_retry_insecure <- isTRUE(allow_insecure_retry) || grepl("ssl|certificate", initial_error %||% "", ignore.case = TRUE)
options(download.file.method = "libcurl", download.file.extra = "--insecure --location --retry 3 --retry-delay 1")
system2("curl", args = c("-fsSL", "--retry", "3", "--retry-delay", "1", "--insecure", "--location", url, "-o", destfile), ...)
allow_insecure_retry = c(TRUE, rep(FALSE, 10))
```
- **The libcurl step does nothing insecure.** `?download.file` says `extra` applies only to the "wget" and "curl" methods, so it is just a repeat of the same request.
- **The `system2` curl step really does disable verification.** It prints no message on success. It serves data and `.rds` files that are later passed to `readRDS()` (qes_download.R:2166-2168). Deserialising downloaded RDS is itself a risk on R < 4.4.0 (CVE-2024-27322, code execution), and `Depends: R (>= 4.1)` (DESCRIPTION:25) includes those versions. Drop `.rds` from the remote reader or raise the floor. The external binary is not declared in SystemRequirements.
- **For qes2022 it triggers on any failure.** For other studies it needs the error text to mention SSL. **(partial)** With libcurl the SSL detail usually ends up in a warning rather than in the error text, so for Borealis studies the regex path rarely fires. "Hammering" the server is overstated: at most about 3 to 6 requests.
- **Recommendation:** delete the insecure path and the curl shell-out. If an escape hatch is really needed, make it `options(qesR.insecure = TRUE)` with a loud warning.

**D2. Labels come from the wrong file in multi-file studies (high, partial).** R/qes_download.R:856-891 (scoring), 894-928, 2184.
```r
selected_bonus <- if (... identical(selected_file_id, candidate_file_id)) 5L else 0L
score <- as.numeric((1000L * nrow(data)) + ... + selected_bonus - ...)
```
- **Live run, qes1998:** the selected data file is 329987 (panel). The DDI actually used is 316121 (CREATEC), with scores 49760 vs 42673.
- **Corrected detail:** the real differences affect `vote94` (code 4 "nsp refus autres" loses its label and a code-9 label is attached), `age`, `scol`, `occup`, and `meilpm` (code 3 changes meaning). Several variable labels also become "...recodée". The reviewer also named `intvote`, `q4m1/2` and `raison1/2`, but their label sets are the same apart from order.
- **Why it matters:** the wrong labels feed factor codings into harmonization.
- **Recommendation:** always use the selected file's own DDI. Better, use the original file's native labels (D3).

**D3. Downloads the ingested `.tab`, not the original; SPSS vs Stata twins chosen by byte size (high).** R/qes_download.R:253, 187-202, 2162-2164, 2049-2095.
```r
access_url <- sprintf("%s/api/access/datafile/%s", study$server, file_id)
preferred_extensions <- c("sav", "zsav", "dta", ...)   # never matches: every file is served as *.tab
return(candidates[which.max(candidates$size), , drop = FALSE])
```
- **Live metadata:** all 11 studies' data files are `.tab` with an `originalFileFormat` of SPSS or Stata.
- **Size picks the twin inconsistently:** qes2007, qes2012 and qes2014 get the STATA twin; qes2008 gets the SPSS twin.
- **Consequences:**
  - Data is parsed with `read.delim` with no encoding set. That produces mojibake in non-UTF-8 locales (R 4.1 on Windows falls within `Depends: R (>= 4.1)`), drops leading zeros, and keeps `""` rather than NA.
  - The native labels and SPSS user-missing definitions are lost, which is the reason the whole DDI/PDF/override apparatus exists.
- **Verified that the fix works:** `GET /api/access/datafile/286331?format=original` returned 303 to a `.sav`, and `haven::read_sav` read 450x37 with 34 labelled variables.
- **(partial)** "More bytes" is not always true: the 2018 `.dta` is 6.3 MB vs a 2.9 MB `.tab`. Switching to originals does not remove the codebook's need for the DDI.
- **Recommendation:** use `?format=original`, choose the reader from `originalFileName`, pin the file id per study in the catalog, and use the DDI only to fill gaps.
- **Measured effect on get_qes() output (§10):** the raw codes equal the originals value for value in 2012, 2014, 2018 and 2022. The defects are in the text: U+FFFD in 2018 (1,805 cells) and 2022 (2,517 cells), because the damage is already in the server's `.tab`; C1 characters in 2012 labels; 2012 labels lowercased and cut at 80 characters, because the Stata twin is used; 2022 dates returned as character; and some column types changed.
- **Caveat: switching changes values, not just labels.** `haven::read_sav()` defaults to `user_na = FALSE`, which turns SPSS user-missing codes into NA and so changes harmonization inputs. (None of 2012, 2014, 2018 or 2022 declares user-missing values, but other studies may.) Read with `user_na = TRUE` and document the handling. Sources also differ in encoding (CROP is CP850, see H4), so the catalog needs a per-study `encoding` field for `read_sav(encoding =)`.

**D4. Label and question content depends on which OS tools are installed and on load_all vs install (high).**
R/qes_download.R:930-947, 1117-1167, 1191-1259, 1756-1848, 2026-2034; R/get_qes.R:111-250.
- **Several extraction paths, used opportunistically:**
  - PDF text is extracted with `pdftotext`, then `gs`, then a generated Python/PyPDF2 script.
  - `.doc` files are read with macOS-only `textutil`.
- **All three PDF extractors always run.** R/get_qes.R:236-240 builds `candidates <- list(.read_pdf_lines_with_pdftotext(...), .read_pdf_lines_with_gs(...), .read_pdf_lines_with_python(...))`, which runs every extractor even though only the first result is used. On the 2022 PDF this takes 3.1 s when pdftotext alone takes 0.23 s.
- **The local enrichment only works under devtools::load_all.** `.find_local_codebook_files` looks only in `system.file(package = "qesR")/codebooks`. `^codebooks$` is in .Rbuildignore:15 and there is no `inst/codebooks`, so an installed package never finds it. Under `devtools::load_all` it does, so the developer sees richer codebooks than users do.
- **Dead code:** about 420 lines. **(partial)** The remote PDF path is live, and so is its dependence on external tools.
- **No `SystemRequirements` field in DESCRIPTION.**
- **Recommendation:** stop scraping at runtime. Extract EN/FR question text offline in `data-raw/` into a small versioned table shipped in `inst/extdata`.

**D5. No persistent cache and no version pinning; metadata helpers download the full data file (medium).**
Locations: R/get_qes.R:27; R/codebook.R:1, 196, 280, 326; R/get_qes.R:71; R/qes_download.R:142-146, 170.
- **Why "latest" drifts:** `.fetch_qes_metadata` uses `"%s/api/datasets/:persistentId/?persistentId=%s"` and then reads `latestVersion`. Neither a version nor a file id is recorded.
- **Stale version already:** qes2022 is V1.1 on Dataverse (2024-04-10) while the docs say V1. The data UNF is unchanged, so no data has drifted yet.
- **`get_codebook()` downloads the data file.** It uses `read_data = TRUE`. `get_codebook_files()` and `download_codebook()` call `get_codebook()`, but they only need the metadata JSON.
- **`get_preview()` downloads the whole study** and then calls `head()`.
- **Measured:** 5 requests per `get_qes("qes2008")`, repeated identically on the second call.
- **(partial)** The session codebook cache softens repeat calls to the codebook helpers.

**D6. `file =` regex matches PDFs; qes1998 holds 3 surveys under one code (medium).**
R/qes_download.R:204-221; R/qes_catalog.R:15, 41, 54.
```r
matches <- grepl(file, files_df$filename, ignore.case = TRUE)
return(files_df[which(matches)[1], , drop = FALSE])
```
- `file = "CROP"` and `file = "createc"` both select PDFs.
- An exact name containing parentheses, e.g. `"... (SPSS).tab"`, fails with "No files matched".
- **Recommendation:** match only among data files, try an exact match first, and error when more than one data file matches. Split 1998 into `qes1998_panel`, `crop1998` and `createc1998`.

**D7. No real retry, backoff, timeout increase or User-Agent; errors lose the root cause (medium, partial).**
R/qes_download.R:52-61, 91-100, 121-134; R/master.R:1799-1801.
- **HTTP 429/5xx and timeouts `stop()` immediately** on the 10 Borealis studies.
- **R's default 60 s timeout caps the whole transfer.** A 3 s cap was verified to abort a slow download.
- **The libcurl root cause, e.g. "Couldn't connect to server", appears only as a separate warning.** The error message itself does not contain it.
- **`suppressMessages()` cannot silence the progress output.**
- **`get_qes_master` discards its collected `failed` reasons** when every study fails.
- **(partial)** R's default User-Agent is sent, so the traffic is not anonymous, just not tagged as qesR.

**D8. DDI and PDF failures give no diagnostic, and the label source is not recorded (medium, partial).**
R/qes_download.R:274-297, 1144-1159, 2184-2185; R/get_qes.R:415-418.
- `tryCatch(..., error = function(e) FALSE)` swallows the failure.
- The `ddi_file_id` that was used is already computed, then thrown away.
- **Recommendation:** record it as `attr(data, "qes_label_source")`.

**D10. `.read_text_table` silently drops rows on a stray quote (critical, live under non-English messages).** R/qes_download.R:2049-2095.
```r
has_eof_warning <- !is.null(first$warning) &&
  grepl("EOF within quoted string", first$warning, ignore.case = TRUE)   # :2075-2076
```
- Reproduced: input `a\tb / 1\t"x / 2\tok / 3\tz` returns 1 row, not 3. The warning `read.delim` raises is "incomplete final line", so the quote-free fallback never runs.
- The check parses English warning text, so it also fails under a French locale, where the warning is translated ("ligne finale incomplète...").
- **Live in `get_qes_master()`.** The owner's `~/.Renviron` sets fr_CA, so French messages are the default. The master then reads qes2007_panel as 1,234 of 2,442 rows and qes2018_panel as 634 of 1,250, with no warning. The last row kept swallows the rest of the file into a single text field of 882,838 characters. In an English session all rows are read, but every string cell keeps its literal quotes, because the fallback uses `quote = ""`. `get_qes()` for 2012, 2014, 2018 and 2022 keeps every row.
- The deleted `test-readers.R` covered this case and now fails.
- qes2022 is not truncated (1521 rows = DDI `caseQnty`). **Fix:** assert the row count against DDI `caseQnty` after every read, and never branch on warning text.

**D9. Lower-severity items.** All low.

| Finding | Location | Detail |
|---|---|---|
| Duplicate value-label text is mutated | qes_download.R:1982-1984 | `make.unique()` turns duplicate label text into "Other.1". Haven allows duplicate label names. |
| DDI fallback endpoint does not exist | qes_download.R:264-268 | `/api/files/{id}/metadata/ddi` returns 404 "API endpoint does not exist" (verified). |
| Documentation URL hard-codes both servers | qes_catalog.R:79-83 | Could be derived from the server column. |
| Return class changes by file format | qes_download.R:2135-2178 | tibble vs data.frame vs anything from `readRDS`. |
| Truncated downloads look complete | codebook.R:346-356 | An interrupted download leaves a truncated file, which later calls then skip. Verified with a server that stalls at 5 KB. |
| Duplicated helper logic | qes_download.R:1712-1754 vs 1870-1910 | Question-map loop copied; the extension list appears 3 times; the non-data pattern twice and already drifted. |
| Hard-coded, accent-stripped overrides | qes_download.R:488-683 | About 200 lines of study overrides in R code with French accents stripped ("Montreal", "Francais"). |

### 5.2 Catalog / metadata

**C1. Derived CC BY-NC 2022 respondent rows ship in an MIT package, unattributed and with no licence stated (high).**
Location: inst/extdata/qes_master.csv (3300 rows x 7 columns: `qes_year, qes_code, qes_name_en, age_group, turnout, vote_choice, sovereignty_support`; 300 per study including qes2022); DESCRIPTION:22 `License: MIT + file LICENSE`.
- **What it is:** derived respondent-level data (no IDs, no weights), not raw microdata.
- **Mismatch:** the live licence for 10.7910/DVN/PAQBDR is CC BY-NC 4.0, which requires attribution; nothing attributes the 2022 rows. The Borealis QES studies are CC0 (2018 checked: CC0 1.0, empty `termsOfUse`). The tracked `codebooks/codebooks_2022/...Codebook v1-1.pdf` is likewise redistributed in the public repo without attribution.
- **Nothing documents it:** there is no `inst/COPYRIGHTS`, no licence column in the catalog, and no mention in the docs.
- **Why it matters:** it is tied to reviewer request 6. CC BY-NC is not an OSI/FSF free licence, but whether CRAN would reject it outright was **not verified** against policy text; treat it as a probable objection, not a confirmed blocker.
- **Recommendation:** replace the file with a synthetic example, or drop the qes2022 rows. Add a licence column to the catalog and show it in `get_qescodes(detailed = TRUE)` and in `get_qes()` messages.

**C2. Durand panels presented as "Quebec Election Study ... Panel" (medium, partial).**
Location: R/qes_catalog.R:33, 36, 40 (EN) and 46, 49, 53 (FR); pkgdown/index.md:81, 84, 88. It also flows into data via R/master.R:1580 `qes_name_en = meta$name_en`.
- **Actual authors:** Dataverse titles and authors are "Sondage panel sur l'élection québécoise de 2018" (Durand, Blais) and the 2012 and 2007 panels (Durand, Goyder).
- **(partial)** The CROP and 1998 entries are not QES-branded, only loosely titled (1998 is "Quebec Elections 1998", qes_catalog.R:41). The citation vignettes already give the correct authors.
- **Package branding:** 5 of 11 studies are not QES (3 Durand panels, CROP, the 1998 polls), yet Title/Description say "Quebec Election Study". Reword to "Quebec election surveys" or similar.

**C3. The catalog is thin (medium).**
R/qes_catalog.R:2-75, 104-107.
- **What it contains:** code, year (character, e.g. `"2007-2010"`), names, DOI and server only.
- **What it lacks:** N, design (cross-section, panel or pooled polls), election date, mode, weights, licence, dataset version, file id and authors.
- **Weight choice is invisible.** Only 5 studies set a weight explicitly (master.R:56-177). The rest fall back to the first hit in a 22-name list, so qes2012_panel and qes2007_panel get generic `pond` rather than `pond_post`/`pondvote` (qes2018_panel takes `weight`, per `qes_master_source_map.csv`). qes2022 offers `cps_`/`pes_weight_general` and their `_trimmed` versions (2022 codebook), but the master uses `cps_weight_general` for everything, including post-election items. Weight choice (CPS/PES, trimmed or not) belongs in the catalog/crosswalk. **(partial)** The chosen column is recorded in `source_map`.

**C4. Citations are hand-copied 3 times, already stale, and there is no inst/CITATION (medium).**
Location: README.md:132-146; vignettes/study-citations.Rmd:12-24; vignettes/fr-citations-etudes.Rmd:12-24.
- **Stale:** every row says "V1", and the literal token `[fileUNF]` is pasted in.
- **No citation from R:** `citation("qesR")` gives no dataset citations.
- **Recommendation:** store the citations in the catalog, generate the tables in a chunk (which also addresses reviewer request 3), link doi.org only (which removes the 202 NOTE), and add `inst/CITATION`.

**C5. Code grammar and naming (low).**
- **Code grammar is inconsistent:** `qes2018_panel` vs `qes_crop_2007_2010`.
- **Codes are matched exactly and case-sensitively** (qes_download.R:32-47), with no near-match suggestion.
- `name_fr` has no accents (qes_catalog.R:43-55). Use `é` escapes.
- `get_qes_call_char` is a cesR leftover (qes_catalog.R:101-102).

### 5.3 Codebooks / labels

**K1. Codebook metadata attributes are dropped during alignment (high).**
Where it happens: R/qes_download.R:1520 (`codebook[keep, c(...), drop = FALSE]` in `.finalize_qes_codebook`), reached from `.align_codebook_to_data` at 1607 and 1674-1676. That function is called unconditionally at 2205 whenever `read_data = TRUE`, which is always.

What is lost and what survives:
- The attributes set at 2038-2043 are lost: `survey_code`, `doi`, `doi_url`, `selected_data_file`, `files`, `codebook_files`.
- Only `value_labels_map` survives. This was reproduced offline.

Consequences:
- `get_codebook_files()` / `get_qes_codebook_files()` return 0 rows (codebook.R:283).
- `download_codebook()` always reports "none found" (codebook.R:327-336).
- `get_qes()` prints "0 support files" (get_qes.R:43).
- `print.qes_codebook` shows no DOI.
- `.expand_question_from_pdf` returns NA (get_qes.R:286-303).
- README.md:104-107 advertises exactly these calls.

Fix: restore the attributes with the existing `.copy_codebook_attrs` (codebook.R:18) after alignment, and add a test for it.

**K2. Documentation-file detection misses the flagship studies' codebooks (high).**
R/qes_download.R:228-231:
```r
codebook_pattern <- "(codebook|questionnaire|instrument|readme|documentation|syntax|dictionary|metadata|ddi|\\.pdf$)"
```
- **What it misses (checked live):**
  - qes2007, qes2012, qes2014 and qes2018 each return 0 files. Their EN/FR `.doc` and `.docx` files are missed, along with the extensionless 2018 "Rapport méthodologique", whose contentType is application/pdf.
  - qes2008 misses its FR `.doc`.
- **Effect:** even after K1 is fixed, 4 of 11 studies would still list no documentation.
- **Fix:** treat every non-data file as documentation, using contentType. Better, curate a per-study documentation manifest in the catalog with language and role.

**K3. 80-character truncation is not detected (high).**
Location: R/get_qes.R:92-109.
```r
dangling <- c("a","an","the","to","of","for","in","on","at","with","from","and","or")
```
- **Scale in qes2022:** 369 of 718 questions are exactly 80 characters long, and about 335 of those end mid-word. The detector flags 35.
- **Example:** `cps_qc_referendum` = "...asked whether Quebec shoul".
- **Effect:** `full = TRUE` is effectively a no-op. It is also blocked by K1.

**K4. Missing label and question metadata is filled with the variable name (medium).**
R/qes_download.R:1559-1562, 1651-1654:
```r
codebook$label[both_missing] <- codebook$variable[both_missing]
```
- **Scale:** 246 of 254 qes2018 labels equal the variable name.
- **Effect:** `get_question(qes2018, "q1")` returns `"q1"` instead of the documented `NA` and warning (get_qes.R:471-472, 558). This hides the fact that 2018 has effectively no codebook, although EN and FR questionnaires exist.

**K5. `label` and `question` are cross-copied, and raw newlines are kept (medium).**
R/qes_download.R:300-308, 801, 1553-1557.
- **Duplication:** `label == question` in 718 of 718 qes2022 rows.
- **Newlines:** 21 labels contain `\n`. `.squish_ws` already exists (line 9) but is not applied to DDI labels.

**K6. No cross-study search; `get_value_labels()` takes one variable and returns empty silently (medium).**
Location: R/codebook.R:130-133.
```r
.assert_single_string(variable, "variable")
map <- map[intersect(variable, names(map))]
```
An unknown name returns `named list()` with no warning.

**K7. Lower-severity items (low).**
- **No `lang` argument anywhere**, and the overrides are ASCII-folded.
- **`format_codebook` on a long-layout input** produces duplicates and drops unlabelled variables (codebook.R:33-89), and print counts rows as variables (codebook.R:395).
- **The two question-cleaning heuristics disagree:** the `(If|Si)` rule at qes_download.R:986 vs `If` only at get_qes.R:267.

### 5.4 Harmonization (`get_qes_master`, `get_decon`)

**H1. Automatic cross-study stacking by raw column name (critical).**
Mechanism, all in R/master.R:
- `.discover_crossstudy_raw_variables` (1186-1217) keeps any name that appears in at least 2 studies.
- `.master_opaque_short_names()` (1230-1296) renames them from the 2007 questionnaire, e.g. `q39 = "feeling_charest_0_100"`, `q74 = "vote_federal_2006"`, `q42 = "feeling_david_0_100"`, `q70 = "provincial_pid_item"`.

Verified mismatches in the built artifact:

| Harmonized name | What it actually holds |
|---|---|
| `vote_federal_2006` (qes2012) | "fairly satisfied = 772; not very satisfied = 472..." (satisfaction with democracy) |
| `feeling_david_0_100` (qes2018) | codes 1/2/98 (abortion item), not a 0-100 thermometer |
| `provincial_pid_item` (qes2018) | home language (1 = English, 2 = French) |
| `issue_qc_status` (qes2018) | party first choice |

Scale: 63 renamed columns, 27 of them populated for 2018 and 45 for 2012.

Fix: delete the name-based stacking. Harmonize only what an explicit crosswalk lists. Keep raw extras with a study prefix (e.g. `qes2018__q42`) or in a separate long table.

**H2. `party_best` is filled from unrelated q8 items, and numeric party codes are turned into "Don't know" (high).**
- **Wrong source:** master.R:23 `party_best = c("cps_partybest", "partybest", "qpartybest", "Q8")`, combined with the case-insensitive fallback in decon.R:18-25.
  - qes2007 gets "assez/très important" and qes2012 gets "very/quite important": these are importance ratings.
  - qes2014 gets Q8, "best campaign". qes2018 q8 is "Quel parti était votre premier choix?" (asked if Q7 = NON), also a different item. qes2022 `cps_partybest` asks which party is best on an issue, which is a different construct again.
- **Bare numbers treated as don't-know:** master.R:882-894 has the DK regex `^[-+]?[0-9]+$`, applied before the party patterns. The only numeric map covers `vote_choice`/`party_lean` for 2018.
  - Result: all 367 qes2018 `party_best` values become "Don't know / Refused".
  - **(partial)** 348 of them are substantive: 306 are codes 1-4 and 42 are code 96 ("other party").

**H3. `political_interest`: the 1-4 remap never runs (high).**
Location: master.R:838-847.
```r
valid_num <- is.finite(num) & num >= 0 & num <= 10; out[valid_num] <- num[valid_num]
# ...then...
out[is.na(out) & num %in% c(1)] <- 10
```
- **Effect:** qes2018 keeps raw codes 1-4 on a scale where 1 means "very interested", so 2018 respondents look like the least interested in the series and the direction is inverted. `.coerce_scale_0_10(c("1","2","3","4"), kind = "interest")` returns `1 2 3 4`.
- **Other scale problems:**
  - qes2012_panel stays on 0-3.
  - qes2014 ideology loses its endpoints. The scale is 0-10, but the French endpoint labels ("0: Le plus à gauche", "10: Le plus à droite") are not recognised, so all 142 answers at 0 or 10 become NA (1,195 → 1,053). Values 1-9 are correct.

**H4. Other verified coding errors in core variables (high).**

| Variable | Location | Evidence | Effect |
|---|---|---|---|
| `born_canada` | master.R:682 | `no_hit <- grepl("^(0|2|no|non)$...")`. qes2018 q69 is coded 1 Québec / 2 elsewhere in Canada / 3 outside Canada. | 138 people born in other provinces coded "No". 304 foreign-born left as raw "3". 98/99 left raw. French DK labels in 2014 also pass through. |
| `ideology` 2018 | master.R:69, 352-370 | Override `ideology = "q36"`; the file has `q36_1`. The resolver falls back silently. | All 3072 values NA, despite 2,490 valid 0-10 answers. **(partial:** the count is 2,490, not 2,586). |
| `education` | master.R:375, 740-790 | 1998 `scol` code 2 is "secondary completed OR technical/CEGEP", and the college regex runs first. `iconv(..., "ASCII//TRANSLIT")` on macOS turns "maîtrise" into "ma^itrise". | 1998 shows College = 789 with no Secondary category. "maîtrise" left raw (268 rows). Results depend on the platform. |
| `language` | master.R:7, 64, 338-342 | Candidate order picks `LANG` (2014 interview language) ahead of `QLANG` (mother tongue). 2022 uses `cps_UserLanguage`. 1998 is imputed with `"French"`. | One column mixes interview language, mother tongue and (via the auto-stacked `home_language` column in the same master) home language. 2014 and 2022 show 0 "Other". |
| `income`, `religion` | master.R:15-16, 70-71 | No standardizer exists. | Five bracket schemes and raw codes (-99, 99). README.md:53 claims income is harmonized. The 2018 religion filter (q66) is ignored, so "none" is lost. |
| `turnout` / `vote_choice` | master.R:20-21, 58-62, 167-174, 399-408 | 2022 uses `cps_turnout` / `cps_votechoice1` (campaign intention), although `pes_turnout` / `pes_votechoice` exist in the same file (fileid f7449513). 1998 uses `vote_choice = "intvote"` although `q3post` ("Vote déclaré") exists. | Intention and recall pooled. 2022 "turnout" (98%) means "certain to vote". In 1998, 101 respondents coded as non-voters still have a vote choice. |
| `party_lean` | master.R:24, 118, 151 | Holds a different construct in each study. | 2012: q13 "best stands up for Quebec". 2007: second choice. 2014: first choice of respondents who didn't vote for it. CROP: identical to `vote_choice`. |
| `sovereignty` | master.R:25-26, 1055-1060 | 2007/2008 q19 uses the 1995 "souveraineté assortie d'une offre de partenariat" wording; 2012-2022 ask about an "independent country". | Question effect mixed with trend. `sovereignty` and `sovereignty_support` are identical duplicate columns. |
| `age_group` | master.R:536, 587-598 | Mixes 6-band and 3-band schemes. `"<18"` normalizes to `"18"` and then to NA. | Panels use 3 bands. 229 respondents aged 16-17 in qes2018 get NA. |
| `province_territory` | master.R:344-347, 815-828 | Every Quebec region is collapsed to "Quebec", and NAs are filled with "Quebec". | Raw leftovers: `"RESTE DU QU\u0090BEC"` (7,203 CROP rows), "Region - ROQ". Regional information is destroyed. |
| encoding | master.R (no per-source encoding) | Byte 0x90 is `É` in DOS code page 850, so CROP text is CP850 read as UTF-8; any CROP string or label may be damaged. qes2022 `vote_choice_text` already has U+FFFD in 4 rows ("Bloc Montr\ufffd\ufffdal", "Lib\ufffd\ufffdral"). `utils::write.csv` (master.R:1871, 1877) sets no `fileEncoding` (low: native encoding on Windows R 4.1). | Garbled accents. Add a test scanning output for 0x80-0x9F and U+FFFD. |
| `survey_weight` | master.R:29-52, 1016-1022 | Raw weights, not normalized, source not recorded. | Mean weight (tracked master): CROP 6.04, qes2007_panel 6.00, qes2012_panel 6.30, qes1998 2.19, all others incl. qes2018_panel 1.00. Pooled weighted estimates are dominated by CROP (59-61% of rows). |
| missing values | master.R:452-465 | Only NA and "na"-like strings count as missing. | 98/99/-99 leak into `born_canada`, `income` and `religion`. |

**H5. Deduplication drops 380 real respondents (high, partial: the reviewer did not identify the study).**
Location: master.R:3, 1127-1145.
```r
key <- paste0(codes, "||", tolower(ids)); keep <- !valid | !duplicated(key)
```
- **Where they come from:** qes2007_panel has 2442 rows but 2062 in the master. `quest` is unique only within the subsample `nompn` (674A/674B).
- **They are different people:** paired rows agree on sex no more often than chance (0.51 vs 0.52).
- **Visibility:** only a total count is reported: `attr(, "duplicates_removed")` is 381 in the tracked build and 31 in the fresh French build, which D10 had already truncated to 1,234 rows.
- **Replaced IDs:** qes2014 and qes2018_panel source IDs are replaced by synthetic IDs, so users cannot merge back to the source files.

**H6. `get_decon()` is a second, weaker harmonizer (high).**
Location: R/decon.R:41-60.
```r
turnout = c("cps_turnout", "q1", "turnout")
votechoice = c("cps_votechoice1", "qes_votechoice", "qvote", "q2")
```
- **Wrong questions for qes2018:** `turnout` gets q1 (satisfaction with democracy), `votechoice` gets q2 (most important issue). The master's own overrides say q5 and q6.
- **Coverage:** no per-study overrides. 1998 fills 1 of 18 targets. 2014 would take Q1 and Q2 (issue, turnout).
- **Missing codes pass through:** -99 is never recoded (decon.R:31-36), so the 2022 ideology mean is 2.64.
- **Naming disagrees with the master:** `votechoice` vs `vote_choice`, `yob` vs `year_of_birth`.

**H7. Harmonization mechanism (high).**
- **Everything is hard-coded R literals:** `.master_harmonization_lookup()` (master.R:1-54), `.master_study_overrides()` (56-177), and about 90 `grepl` calls.
- **The source map records only the column name:** `qes_code, qes_year, qes_name_en, harmonized_variable, source_variable` (master.R:1639-1646). It has no wording, value map, timing, file version or counts of dropped rows.
- **No tests and no per-variable documentation.**
- **Master-level post-processing runs on the pooled column** (master.R:391-398, 1016-1022). **(partial)** The turnout gate rarely changes results; the weight rule depends on which studies are loaded.
- **Dead branches:**
  - The 1998 allervot fallback (238-242) never runs.
  - `map_qes2018` never fires, because labels are converted to text first (942-974).
  - `vote_choice_text` for 2018 comes from `q2_96_other` (line 22), which is the open "other" text for Q2, the most-important-issue question. It is NA in every row only because `get_qes()` drops that column's labels. Fixing the labels alone would put issue text into 88 rows, so fix both together.

**H8. No panel structure (medium).** The master header has no `wave`, `timing` or `panel_wave` column (`election_timing_eval` is the auto-stacked `q33` rename from H1, master.R:1245, not a wave marker). The three Durand panels are wide files flattened to one row with one source variable per concept: no long format, no wave linkage, no panel weights, no attrition handling. The crosswalk (P1) must model wave as a dimension, not just a `timing` flag.

### 5.5 API / naming

**A1. `assign_global` still exists, and fails through the aliases (high).**
- **Where it lives:** R/assign_utils.R:1-4. It is called from get_qes.R:39,51, codebook.R:204, decon.R:100 and master.R:1858.
- **What it does:** writes to .GlobalEnv from the console when opted in.
- **Broken through the aliases:** through `get_qes_codebook()` and `qes_codebook()` (codebook.R:217-258) it assigns into the alias's own frame and the object is lost. Verified: TRUE for a direct call, FALSE through the alias.
- **Inconsistent result in the master:** `get_qes_master` assigns before setting `saved_to` (master.R:1856-1881), so the assigned copy differs from the returned one.
- **Stale docs:** the Rd files and README still say "global" (§3.2 row 5).
- **Downstream dependency:** 53 of the paper's calls are bare `get_qes("qes20xx")` followed by use of the global object, plus 2 conditional calls. They rely on the v0.4.4 default `TRUE`. HEAD's `FALSE` default makes them fail with "object not found", and in `tableA1_modelsummary.R` a `tryCatch` hides that failure, so the QES table silently disappears.
- **Fix:** keep the argument, because removing it changes the signature. Assign only into the caller's frame, and fix the alias path. Whether the default stays `FALSE`, as CRAN request 5 asks, or goes back to `TRUE` is an **open decision for the owner**. If it stays `FALSE`, record it in NEWS as a breaking change and have the paper pin v0.4.4 (d1faad6) or assign explicitly (`qes2012 <- get_qes("qes2012")`).

**A2. Name collisions (medium).**
- `get_question`, `get_preview` and `get_decon` collide with CRAN cesR, whose signatures differ, e.g. `cesR::get_preview(srvy, obs = 6, pos = 1)`.
- `get_value_labels` collides with `labelled::get_value_labels(x, prefixed)`.

**A3. Alias exports and Rd drift (medium).**
- **Three pure aliases** (`get_qes_codebook`, `qes_codebook`, `get_qes_codebook_files`), each with its own Rd file. (Sharing the name `qes_codebook` between a function and its S3 class is normal in R and is kept in P1.)
- **man/ and NAMESPACE are hand-written.** There is no roxygen header and no RoxygenNote, while R/ carries unused roxygen blocks with no `@examples`. The WIP commit edited the Rd files directly.
- **Drift example:** R/codebook.R:172 says "calling environment" while man/get_codebook.Rd:18 says "global environment".
- `qesR-package.Rd` lacks `\alias{qesR}`, and no Rd file has `\seealso`.

**A4. Inconsistent vocabulary and return types (medium).**
- **Study argument:** `srvy` in most functions, `surveys` in master.R:1735. `get_decon` is the only function with a default study.
- **`file`:** is a regex, even in `download_codebook`, where it selects the *data* file.
- **cesR-style names:** `do`, `q`, `obs`.
- **No `quiet` argument** in `get_preview` (forced TRUE) or `get_question`, although both can use the network.
- **Return types vary:** `get_value_labels` returns a list or a data.frame depending on `long`. The class of `get_qes` output varies by file format.

**A6. `get_question()` silently answers for a different variable (medium).** R/get_qes.R:431-439.
```r
starts_with <- names(data)[startsWith(lower_names, tolower(q))]
contains <- names(data)[grepl(tolower(q), lower_names, fixed = TRUE)]
```
Reproduced: `get_question(data.frame(q10 = 1), "q1")` returns q10's label, and `"q36"` returns `q36_1`'s, with no warning. The deleted test expecting a "Close matches" error now fails. Fix: exact (case-insensitive) match only; suggest near matches in the error.

**A5. Lower-severity items (low).**
- **Metadata attributes:** named inconsistently (`qes_survey_code` on data vs `survey_code` on the codebook), with no accessors.
- **Errors:** conditions are unclassed.
- **`with_codebook = FALSE`:** still does the PDF work. **(partial)** The DDI must still be fetched for labels.
- **`save_path`:** also writes a second file, and any extension other than `.rds` is written as CSV.
- **Validation gaps:**
  - `format_codebook(data.frame(a = 1))` is accepted.
  - `print(cb, n = 3)` errors.
  - `download_codebook` creates `dest_dir` before it knows there is anything to download.

### 5.6 Docs / vignettes / pkgdown

**V1. Vignettes show different data on CRAN and on the website (high).**
Code: `master_paths <- c("qes_master.csv", "../qes_master.csv", pkg_master)`. Same pattern in vignettes/analysis-vote-choice.Rmd:28-35, 69-70; analysis-descriptive.Rmd:28-33; analysis-sovereignty.Rmd:37-38, 93-98; and the FR mirrors.

- **Website:** pkgdown resolves the tracked 40,606-row root file, which was built by older code (a rebuild today gives 39,132 rows, §4). So the site shows stale output, not just a bigger sample.
- **CRAN and installed builds:** use `inst/extdata`, which has 300 rows per study. The subsample is undocumented, and nothing in the repo generates it.
- **Resulting differences:**

| Statistic | CRAN / installed | Website |
|---|---|---|
| 1998 N | 206 | 1,048 |
| 2010 N | 10 (shown with CIs) | 730 |
| 2022 PLQ share | 7.5% | 10.9% |

- **Mislabelled table:** analysis-descriptive's "Sample size" table prints 300 as the survey N.

**V2. Substantive errors in the analysis vignettes (medium).**
- **Designs pooled without weights.**
  - analysis-vote-choice.Rmd:141-171 groups only by year. On the website, 2008 is CROP 10,011 + QES 1,151 rows.
  - CIs use a simple-random-sample SE, and nothing in the repo applies a weight.
  - **(partial)** In the CRAN build the file has no weight column.
- **Wrong denominator note.** Line 238 says "Shares use respondents with a non-missing `vote_choice`". In fact they are shares among major-party voters (lines 41-48). The FR mirror has the same error at line 271.
- **False statement about CROP.** analysis-descriptive.Rmd:79-82 and 277-278 say CROP "does not contain a row-level year field". But master.R:1675-1679 derives it, and every CROP row has a year. The sovereignty vignette splits CROP by year (analysis-sovereignty.Rmd:218).
- **Sovereignty wording.** The sovereignty vignettes (158, 217) call the item "the dedicated sovereignty question harmonized across study periods". See H4 for why that is not true.
- **Trend plots.**
  - Loess is fit to 9 points on an evenly spaced index, not on actual years.
  - QS 1998 and PCQ before 2022 are shown as 0.0%.
  - `warning = FALSE` is set globally.

**V3. Displayed code has drifted from executed code; EN/FR parity (medium).**
- **"Show code" blocks are static text, not the executed chunks.**
  - Blocks: analysis-sovereignty.Rmd:32-88, analysis-vote-choice.Rmd:64-134. They have drifted from the chunks that actually run.
  - EN: the displayed blocks omit the plotting code.
  - FR: the displayed block contains a different plot and produces an extra table and figure (fr-analyse-choix-vote.Rmd:124-148).
  - analysis-descriptive has no code block at all, although NEWS.md:9 promises "expandable full code blocks".
- **FR vignettes are thinner.**
  - FR getting-started (`fr-demarrage`) lacks 5 of the EN sections, uses `devtools` where EN uses `remotes`, and its "Voir aussi" entries are not links.
  - FR merged-data vignette (`fr-donnees-fusionnees`) lacks `source_map` and the quality-controls section.
- **FR vote-choice palette:** PLQ is drawn in blue (`"#2c7fb8"`) and PQ in brown.

**V4. Lower-severity items (low).**
- **pkgdown navigation:** has no French entries and no descriptive use case. FR pages are reachable only through a hand-maintained JS map (_pkgdown.yml:7-56).
- **README examples:**
  - They save output to getwd.
  - `get_question(qes2018, "some_variable", full = TRUE)` (README.md:112-117) uses a placeholder variable, and `full = TRUE` is already the default.
- **NEWS.md:**
  - Line 4 says "within-year" de-duplication, but the code keys on survey code.
  - The breaking `assign_global` default change is not recorded.
- **Suggests loaded unconditionally:** dplyr and ggplot2 are attached without a guard in 6 vignettes. **(partial)** This is a noSuggests robustness issue, not a CRAN policy violation.

### 5.7 CRAN / CI

**R1. Copyright and licence (high).** Details in §3.2 row 6 and C1. Also: `cran-comments.md` is unchanged since the first submission. It has no resubmission section, and line 34 claims the global assignments were removed.

**R2. Network examples error when offline (medium, partial).**
- Examples and CI depend on the network.
- `.download_file_with_fallback` ends in `stop()` (qes_download.R:71-74), and `get_qes_master` also `stop()`s (master.R:1797-1799).
- CRAN policy asks that code using internet resources fail gracefully. `--run-donttest` would then report an ERROR during an outage.
- **(partial)** The vignettes are not affected: every network chunk is `eval = FALSE`.
- **All 14 `\donttest` examples use `"qes2022"`** (no other study code appears in man/). That is the only study with `allow_insecure_retry = TRUE`, the only CC BY-NC one, and the WAF-fronted host behind the 202 NOTE, so `--run-donttest` exercises exactly those paths. Use a CC0 Borealis study.

**R3. CI coverage (low).**
- .github/workflows/r-devel-check.yml:3-8, 15, 27, 39 runs one ubuntu r-devel job, on `main` and on PRs into `main`.
- There is no release, oldrel, Windows or macOS job, and the declared floor `R (>= 4.1)` is never tested.
- The `redesign` branch is checked only once a PR is opened.

**R4. DESCRIPTION tidy-ups (low).**
- `LazyData: false` (DESCRIPTION:24) is dropped at build because there is no data/ directory.
- `pkgdown` is in Suggests but unused (CI installs it through `extra-packages`).
- There is no `SystemRequirements` field.
- Version 0.4.4 has not been bumped.

### 5.8 Performance

Items D3, D4 and D5 already cover the main costs. The remaining CPU hotspots (all low):

| Hotspot | Location | Measurement / fix |
|---|---|---|
| DDI parsing recomputes XML namespaces on every node | qes_download.R:300-301, 796-827 | 1.24 s for the 2022 DDI; passing `ns = character()` brings it to 0.69 s with identical output. |
| `.is_trivial_value_label` runs per category | qes_download.R:411-460 | About 70x faster if vectorised, provided the call sites are batched too. |
| Reference year parsed per row | master.R:997 | Per-row `vapply` takes 0.6 s; doing it over the 9 unique years is instant. |
| Codebook alignment builds one data.frame per column | qes_download.R:1597-1677 | 0.26 s for 1000 columns. |
| Master build holds all raw studies in memory | master.R:1750-1813 | Peak about 150 MB. |
| Codebook cache keyed on the user's regex | codebook.R:3-5 | `"sav"` and `"SAV"` are cached separately. |

Measured master build: 46 requests (11 metadata, 16 DDI, 9 PDF, 11 data), 72.7 s wall, 15.2 s CPU. Profile: `.fetch_best_qes_ddi` 46%, `.enrich_codebook_questions` 15%, post-processing 14%.

### 5.9 Tests

- `tests/` is empty.
- Commit 397aa0c deleted `tests/testthat.R` and 12 test files (701 lines), including `test-master.R` (245 lines). The 0.4.3 tarball still ships 9 of them.
- They can be recovered with `git show 397aa0c^:tests/...`. They are offline (8 uses of `local_mocked_bindings`), so restoring is cheap but needs triage:
  - The critic's run against current code: 36 tests, 32 pass. Failures are `.read_text_table` (D10), the close-match test (A6), the global-object lookup (a deliberate change), and `test-validation.R`, which passes alone but failed in the full run (cause not verified).
  - `test-codes.R:22-26` asserts "qes2022 has insecure retry fallback enabled", locking in D1. Delete it.
  - `local_mocked_bindings` needs `testthat (>= 3.1.7)`; the old DESCRIPTION had `>= 3.0.0` plus `Config/testthat/edition: 3`.

---

## 6. Harmonization today

### 6.1 Inventory

The master has 100 columns. About 30 are core harmonized variables; 70 are auto-stacked "cross-study" columns (H1). The table below gives, per study, where each core variable comes from, following `qes_master_source_map.csv`.

Study order: 2022 / 2018 / 2018p / 2014 / 2012 / 2012p / CROP / 2008 / 2007 / 2007p / 1998.

| Variable | Sources, in study order | Mechanism | Verified state |
|---|---|---|---|
| turnout | cps_turnout / q5 / vote / Q2 / q21 / participation / - / q11 / q11 / avote / q1post | Generic regex (382-415) plus per-study code maps (179-253) | 2014 Q2, 2012 q21, 2007 q11, 2018 q5 and 1998 q1post are correct. 2022 is intention (H4). |
| vote_choice | cps_votechoice1 / q6 / vote / Q3 / q25 / voteprov / intvoteprov / q12a / q12 / vote / intvote | Party regex (871-919) | Intention in 2022, CROP and 1998. |
| sovereignty(_support) | cps_qc_referendum / q26 / independance / Q19 / q52 / souv_rec / intvoteref / q19 / q19 / intref / voteref | Regex (417-450) plus per-study maps (255-295) | Direction correct. Wording mixed (H4). Duplicate column. |
| party_best, party_lean | various | Candidate lists plus regex | Invalid (H2, H4). |
| age, age_group | birth year → age → 6 bands, else a 3-band source | `.derive_age_numeric` | Age missing for CROP, 1998 and the 3 panels. Two band schemes. |
| gender | cps_genderid / qsexe / sexfix / QSEXE / sexe ... | Regex | OK. CROP has 1,003 NA. |
| education | cps_edu / qscol / d3 / QSCOL / scol / - / scol / q77 / q77 / scol / scol | Code map plus regex | 1998 wrong, "maîtrise" raw, 2012p all NA. |
| language | mixed constructs (interview, mother tongue; `home_language` stacked separately) | Regex plus constants | Invalid (H4). |
| political_interest, ideology | 0-10 items and 4-point items | `.coerce_scale_0_10` | 2018 interest inverted, 2018 ideology NA, ranges differ. |
| born_canada, income, religion | - | Almost none | Invalid or unharmonized. |
| province_territory | region variables | Collapsed | No information left. |
| survey_weight | cps_weight_general / pond / weight / POND / pond / pond / XPOND / pond / pond / pond / ponderc | First match | Raw. Means 1.0-6.3 by study (H4). 2022 PES and trimmed weights unused. |
| respondent_id | source IDs or synthetic | Uniqueness ratio < 0.5 → synthetic | 380 respondents dropped. 2014 and 2018p IDs replaced. |

What exists and works:
- a per-study override list, which is the right idea;
- a `source_map` attribute;
- `haven::as_factor` before recoding, so recodes act on label text;
- CROP wave year derived from `projet`.

The overrides spot-checked correct: qes2014 turnout/vote/sovereignty, 2018 q5/q26, 2022 `cps_qc_referendum`, and 1998 `voteref`.

### 6.2 Where it falls short

1. **Validity.** More than 10 core variables have verified errors (H2-H5). Every one of them happened silently.
2. **Mechanism.**
   - **Silent candidate lookup.** Candidates are tried in order with a case-insensitive fallback, and nothing is reported when nothing matches. That is how both `Q8` → `q8` and `q36` → NA happened.
   - **Regex recoding runs on the pooled column,** not per study.
   - **Missing values** are handled by regex, one variable at a time.
   - **Timing** (pre- vs post-election) has no column of its own.
   - **Weights** are not normalized, and there is no strata/cluster information or `survey`/`srvyr` interop; the vignettes use SRS CIs (V2).
   - **Panels** have no wave dimension (H8).
3. **Provenance.**
   - No question wording, value maps, file version or counts of dropped rows are recorded.
   - The source is recorded per column only, not per value.
4. **Verification.** Nothing is checked against external benchmarks.
   - **Election results.** Reported vote shares can be compared with official results: the DGEQ Atlas, and Daoust and Guévremont (doi:10.7910/DVN/PNANLW).
   - **Census margins.** Weighted margins can be compared with census figures.
   - Neither check exists.

---

## 7. Landscape

### 7.1 QES and related Quebec surveys

All entries below were checked against the live Borealis and Harvard APIs on 2026-09-26. Access for ICPSR studies was not verified, because their site returned 403.

| Study | Year | Host / ID | Access | In qesR |
|---|---|---|---|---|
| QES 2022 (C-Dem) | 2022 | Harvard 10.7910/DVN/PAQBDR (V1.1); Borealis **10.5683/SP3/CJQF0U** (V1.2, "2022 Quebec Election Survey") | Open, CC BY-NC (both) | Yes (Harvard). The Borealis deposit is a **separate deposit, not a mirror**: its `.tab` UNF is `JfvVsZ1gyHR4VZMg7eGX7g==` vs Harvard's `I/DFDdqJv7wNEoyyRdxaIw==`. Useful for its `.sps`/`_dta.zip` originals, not for silent failover. |
| QES 2018 / 2014 / 2012 / 2008 / 2007 | | Borealis cecd-csdc | Open, CC0 | Yes |
| Durand panels 2007 / 2012 / 2018; CROP 2007-10; 1998 polls | | Borealis cecd-csdc | Open, CC0 | Yes. 1998 holds 3 files under one code. |
| **MEDW 2012 Quebec provincial election study** (pre and post) | 2012 | Harvard 10.7910/DVN/06OOVN | Open, CC0 | **No** |
| **Durand federal-election panels, Quebec only** | 2008, 2011 | 10.5683/SP3/0UYMWX, 10.5683/SP3/NCGSZU | Open, CC0 | **No** |
| **CROP polls: Charte des valeurs + vote intention** (6 waves) | 2013-14 | 10.5683/SP3/AQYTPS | Open, CC0 | **No** |
| CORA **CROP Political Survey** series (148 datasets, incl. the 1995 referendum period) | 1977-2005 | Borealis `cora` | Open (no licence field set) | No |
| 1974-80 CNES + 1980 referendum panel | 1980 | CORA 10.5683/SP3/RBKRTJ | Open, CC BY-NC | No |
| Quebec provincial election studies 1960, 1973; Separatism 1963; 1995 referendum study | | ICPSR 9002 / 9004 / 9007 / 3726 | Login required | No (link only) |
| CPES Merged 2020-23 (includes QES 2022) | | Harvard 10.7910/DVN/2PREYQ | Open, CC BY-NC | No. A useful bridge to other provinces. |
| Party-system indicators 1900-2022; DGEQ Atlas | | Harvard 10.7910/DVN/PNANLW; Données Québec | Open | No. These are validation benchmarks. |
| QES 2026 | 2026 (election 2026-10-05) | Not deposited | — | Plan a slot |

**Coverage gaps:**
- nothing before 1998 in the package;
- the 2009-2011 and 2013 intention series;
- a second 2012 study (MEDW);
- a second 2022 host. qes2022 is the only Harvard-hosted study and the only one with the insecure flag; the Borealis deposit differs in data (UNF), so any fallback must be explicit and version-checked.

### 7.2 Lessons from comparable packages

| Package | Pattern to adopt | URL |
|---|---|---|
| **ces** (CRAN 1.1.0, 4 Imports) | `list_ces_datasets()`; `get_ces_subset(year, variables)`; `download_ces_dataset(year, path)` with no default path; codebook export; session cache in `tempdir()`. This is now the Canadian benchmark. It does **no** cross-year harmonization and is not bilingual, which is qesR's opening. | https://cran.r-project.org/package=ces |
| **cesR** | The pattern qesR copied: `assign()` into `pos = 1`, no cache. **Archived 2025-10-03.** A cautionary example. | https://cran.r-project.org/package=cesR |
| **retroharmonize** | A crosswalk table with `id, var_name_orig, var_name_target, val_numeric_orig/target, val_label_orig/target, na_label_target, class_target`. Keeps the original values and labels as attributes. Reuse the column names so qesR's crosswalk also works with it (list it in Suggests only). | https://retroharmonize.dataobservatory.eu/articles/harmonize_labels.html |
| **CCES cumulative / ccesMRPprep** | A harmonized file deposited on Dataverse with its own DOI, plus variable-name tables per year. | https://github.com/kuriwaki/cces_cumulative |
| **dataverse** (IQSS) | `original = TRUE`; `use_cache = "none"/"session"/"disk"`, with disk caching only for versioned requests; `cache_info()`, `cache_reset()`. | https://iqss.github.io/dataverse-client-r/reference/cache.html |
| **manifestoR** | Version pinning (`mp_use_corpus_version`, `mp_check_for_corpus_update`) and `mp_cite()`. | https://cran.r-project.org/package=manifestoR |
| **ipumsr** | Consistent family prefixes (`ipums_var_info`, `ipums_val_labels`); label algebra `lbl_na_if`, `lbl_relabel`. Recommend it rather than rebuilding it. | https://tech.popdata.org/ipumsr/reference/index.html |
| **labelled** | The column shape of `look_for()` (`pos, variable, label, col_type, missing, values`) for `qes_search()`. | https://larmarange.github.io/labelled/reference/look_for.html |
| **gssr / gssrdoc**, **vdemdata** | `gss_which_years(var)`; a help page per variable; `find_var()`. | https://kjhealy.github.io/gssr/ |
| **essurvey** | Verb families (`import_*`, `show_*`, `download_*`); `recode_missings()`. | https://docs.ropensci.org/essurvey/ |
| **httr2 / rOpenSci devguide** | Retry with backoff on 429/503, honouring Retry-After. Offline tests with webfakes, httptest2 or vcr. | https://httr2.r-lib.org/reference/req_retry.html ; https://devguide.ropensci.org/pkg_building.html |

---

## 8. Repository hygiene

Checked with `git ls-files`, `.Rbuildignore`, `.gitignore`, `du` and a grep over vignettes, R/, README, pkgdown, `.github` and scripts.

| Item | Size | Tracked? | Build-ignored? | Who depends on it | Recommendation |
|---|---|---|---|---|---|
| `qes_master.csv` (root) | 16 MB, 11 commits of history | **Yes** | Yes | Every analysis vignette (6), through `"../qes_master.csv"` (e.g. vignettes/analysis-vote-choice.Rmd:29). This is what makes the website numbers differ from CRAN. | Fix the vignettes first (single data source, V1). Then `git rm --cached` and publish as a release asset or Dataverse deposit. Removing it before the vignettes change will silently switch the site to the 300-row sample. |
| `qes_master.rds`, `qes_master_source_map.csv`, `qes_master_variable_name_map.csv` | 0.9 MB / 78 KB / 11 KB | Yes | Yes | No code or vignette reads them. | Untrack them; regenerate from `scripts/`. |
| `qes_master_test*.{csv,rds}` | ~1 MB | No (gitignored) | Yes | none | Delete locally. |
| `inst/extdata/qes_master.csv` | 224 KB | Yes | **Shipped** | Vignettes, via `system.file` | Replace with a documented synthetic or aggregate example (licence, C1). |
| `qesR_0.1.0 ... 0.4.3.tar.gz` | ~130 KB total | No (`*.tar.gz` ignored) | Yes | none | Delete. The 0.4.3 tarball is the only local copy of the old tests, but they can also be recovered from git history. |
| `..Rcheck/` | 23 MB | No | Yes | none | Delete. |
| `docs/` | 67 MB, stale (e.g. `docs/tutorials`) | No (ignored) | Yes | CI rebuilds and deploys it to gh-pages (`.github/workflows/pkgdown.yml`) | Delete locally. Run `git fetch` to refresh the stale `origin/gh-pages` ref from 2026-02-22. |
| `codebooks/` | 16 MB, 21 third-party PDF/DOC files | **Yes** | Yes | Only the dead code in `.find_local_codebook_files` / `.local_codebook_subdir` (qes_download.R:1169-1224) | Treat as source material for `data-raw/` extraction. Keep it out of git; the files are on Dataverse. Rights: Borealis QES material is CC0; the 2022 codebook is CC BY-NC and needs attribution if redistributed. |
| `scripts/` | build_qes_master.R, build_pkgdown_site.R, commit_and_push_*.sh | Yes | Yes | Vignette error messages name `scripts/build_qes_master.R`. CI does **not** use `build_pkgdown_site.R`. The two `commit_and_push_*.sh` scripts are personal git helpers. | Move `build_qes_master.R` to `data-raw/`. Drop the two commit scripts and the pkgdown wrapper. |
| `Images/` | 5 logo PNGs | No (ignored) | Yes | none (the logo ships from `man/figures/logo.png`) | Fine. Shrink `man/figures/logo.png` (309 KB) to about 40 KB. |
| `._*` AppleDouble files | 332 outside `.git`, including `.git/objects/pack/._pack-*.idx`, which triggers git "non-monotonic index" errors | No | R 4.4 build drops them automatically | A direct `R CMD INSTALL <dir>` installs `._extdata` | Run `dot_clean` on the drive, or work on APFS. Add `._*` to `.gitignore`. |
| `.claude/`, `.Rproj.user`, `.DS_Store` | — | No | Yes | — | Fine. |

---

## 9. Prioritized roadmap

Size estimates: S is under a day, M is 1-3 days, L is about a week, XL is more than that. **These are human-effort estimates for a developer working by hand. They are not estimates of how long a Claude-driven implementation would take.** The day-and-week figures here and in the "Order" paragraph measure scope. They are not a schedule.

**Backward-compatibility constraint (owner's brief).** Downstream users exist, starting with the owner's paper (§10). Every current export must keep working:
- `get_qes()` and `get_qes_master()` keep their call signatures and are fixed underneath.
- A function that gets a new name keeps its old name as an alias that is at most soft-deprecated.

Every data fix changes output, so each fix must:
- ship in a new version, with a NEWS entry listing the changed outputs;
- be released only after the paper has pinned the version it used.

### P0: CRAN blockers and correctness (do before resubmitting)

| # | Item | Why | Size |
|---|---|---|---|
| 0.1 | Remove the insecure-TLS fallback and the curl shell-out; drop `.rds` from the remote reader (D1) | Security; likely reviewer objection | S |
| 0.2 | Licence: set LICENSE COPYRIGHT HOLDER to the author, make LICENSE.md match, replace or remove the bundled qes2022 microdata, decide the `cph` entry (drop it, or name the real rights holder), add `inst/COPYRIGHTS` with CC BY-NC attribution if any 2022-derived data stays, and write the resubmission section of `cran-comments.md` answering all 6 points (R1, C1) | Reviewer request 6 is still open | S |
| 0.3 | Keep the `assign_global` / `object_name` arguments (the signature must not change) but make the opt-in path assign only into the caller's frame; fix the alias path and the Rd and README wording. The default (`FALSE` for CRAN request 5, or `TRUE` as in v0.4.4, which the paper's 55 bare calls rely on) is an **open decision for the owner** (A1) | Reviewer request 5; the feature is also broken | S |
| 0.4 | Restore codebook attributes after alignment (K1); treat non-data files as documentation (K2) | Four exports currently return nothing | S |
| 0.5 | Use the selected file's own DDI (D2); download `?format=original` and read it with haven (D3) | Wrong labels today | M |
| 0.6 | Fail gracefully when offline: return `invisible(NULL)` with a message, add offline examples for `format_codebook`, `get_value_labels` and `get_question` on toy objects, point network examples at a CC0 Borealis study instead of qes2022, and build the citation vignettes from the catalog (R2, reviewer requests 2 and 3) | CRAN policy; reviewer request | M |
| 0.7 | Stop shipping wrong harmonized output while keeping `get_qes_master()` and `get_decon()` exported with the same signatures:<br>• Fix the verified value errors underneath: H1 stacking, H2, H3 (including the 2014 ideology endpoints), H4 (ideology q36_1, born_canada, language), H5 dedup, and the H7 `vote_choice_text` source.<br>• Or route both functions through the P1 crosswalk as soon as it exists.<br>• Warn in the documentation and in a one-time message that the output changed in this version. | Silent wrong results reach users, including the owner's paper | M |
| 0.7b | Fix silent data paths: locale-independent text reader with a row-count check against DDI `caseQnty` (D10, live under French messages); exact-match `get_question` (A6) | Silent row loss / wrong answers | S |
| 0.8 | Restore the deleted tests (397aa0c) and add fixture tests for K1, D2 and the master coding errors; delete the test that locks in insecure retry; add `testthat (>= 3.1.7)` and `Config/testthat/edition: 3`; add a CI matrix (release, oldrel, Windows for encoding, macOS) that also runs on non-main branches | No regression net today | M |
| 0.9 | Vignettes: move the analysis vignettes to pkgdown-only `vignettes/articles/` (build-ignored) and delete `inst/extdata/qes_master.csv`. That removes the two-data-sources problem (V1) and the shipped BY-NC rows (C1) in one step; request 3 is met by catalog-generated citation vignettes plus an offline get-started chunk. Then fix the denominator note and the false CROP statement, and separate designs or label estimates as unweighted (V2) | Published numbers disagree | S-M |
| 0.10 | Add the P1 `qes_*` families alongside the current names. Every current export stays as a working alias. Whether any alias is ever dropped is an open owner decision (see P1) | No CRAN users yet, but there are downstream GitHub users | M |

### P1: Harmonization and API redesign

On the owner's ideas:

- **Data-driven crosswalk in `inst/extdata` with generated docs: yes, and this is the core of P1.**
  - Use retroharmonize's column names, adding `question_en`, `question_fr`, `timing` (pre/post), `wave` (for the three panels, with a long panel output option), `weight_var`, `na_label_target`, `source_ref` and `confidence`.
  - Generate the per-variable reference vignette and `?qes_crosswalk` from the crosswalk file.
  - Pushback: do not auto-generate the mappings from name matches. Draft them from the DDI, then review every row by hand. A crosswalk full of heuristics just moves the bug from code into data.
- **Testable harmonization, including checks against known election results and weighted margins: yes.**
  - Unit tests: every source variable exists in the pinned file; every source code maps to an allowed target; no raw sentinel values survive.
  - Benchmark tests (CI only): weighted reported vote per election compared with official results (DGEQ Atlas, doi:10.7910/DVN/PNANLW), within a stated tolerance, and turnout over-report documented rather than hidden.
  - Pushback: tests alone won't catch validity errors like H1-H4. Each crosswalk row also needs a human reading of the question wording.
- **`qes_*` verbs with aliases: yes to prefixes, and keep the aliases.**
  - Add the new names before the resubmission (P0 0.10). `get_qes()` and `get_qes_master()` stay as supported entry points, not deprecated ones, fixed underneath with unchanged signatures.
  - **Open decision for the owner:** whether to keep, soft-deprecate or eventually drop these aliases:
    - `get_codebook`, `get_qes_codebook`, `qes_codebook`
    - `get_codebook_files`, `get_qes_codebook_files`, `download_codebook`
    - `format_codebook`, `get_question`, `get_value_labels`, `get_preview`, `get_decon`, `get_qescodes`
  - Arguments on each side:
    - **For trimming:** the cesR/labelled name collisions (A2) and a smaller Rd surface.
    - **Against:** existing scripts break.
  - The default proposed here is that every alias keeps working and at most gets a soft `.Deprecated()` notice. Nothing is removed without an explicit owner decision and a NEWS entry.
  - Proposed families:
    - discovery: `qes_studies()`, `qes_search()`, `qes_which_studies()`
    - data: `qes_data(study, variables = NULL, version = "pinned")`, `qes_download(study, path)`
    - metadata: `qes_codebook(study, layout, lang)`, `qes_value_labels()`, `qes_question()`, `qes_docs()`, `qes_download_docs(study, path, pattern)`
    - harmonization: `qes_crosswalk()`, `qes_harmonize()`, `qes_master()`
    - infrastructure: `qes_cache_info()`, `qes_cache_clear()`, `qes_cite()`
  - Standardize on `study` / `studies`.
  - Size: L.
- **Rich catalog data frame: yes.**
  - Ship `inst/extdata/qes_studies.csv` with code, family (QES / Durand panel / CROP polls / MEDW), design, mode, election date, fieldwork dates, N, languages, recommended weight, other weights, DOI, host plus mirror, pinned version, file id, file MD5, original format, licence, authors and citation.
  - Split qes1998 into its 3 surveys. Use accented names with `\u` escapes.
  - Size: M.
- **Metadata-only codebooks: yes.** Build the codebook from the DDI plus the original file headers, without downloading the full data. Replace runtime PDF and Word scraping with pre-extracted EN/FR question text produced in `data-raw/`. This fixes K3-K5 and D4. Size: L.
- **Version pinning and provenance: yes.**
  - Request the `versions/{v}` endpoint and verify the file MD5 after download.
  - Attach `qes_version`, `qes_file_id` and `qes_label_source` to results.
  - Size: M.

### P2: Capabilities

| Item | Notes | Size |
|---|---|---|
| **Bilingual cross-study search**: `qes_search("souverain|sovereign", lang = "both")` over a shipped variable index (< 1 MB), accent-insensitive, returning labelled-style columns. | The owner's idea, endorsed. It depends on the P1 index, so it cannot come first. | M |
| **Opt-in disk cache** under `tools::R_user_dir("qesR", "cache")`, keyed by DOI, version, file id and MD5, with atomic `.part` writes and cache management functions. Enabled only by an option or interactive confirmation; the default stays session-only. | The owner's idea, endorsed. CRAN policy allows it only if opt-in and actively managed. | M |
| Polite HTTP layer: 3 attempts with backoff on 429/5xx, a longer timeout, a qesR User-Agent, and the root cause kept in errors (D7). Base R or `curl` only. | Keeps dependencies light. | S |
| New studies: MEDW 2012, Durand 2008/2011 federal panels, CROP 2013-14; the Borealis 2022 deposit as an explicit, version-pinned alternative (its data UNF differs from Harvard's); a `qes2026` slot. | Needs catalog rows plus crosswalk rows, but only after P1. | M |
| **Conditional:** a harmonized cumulative file deposited on Borealis with its own DOI (the CCES model). `qes_master()` would download the pinned deposit by default; `qes_harmonize()` remains for flexibility. | **Only if the data owners give permission** (QES/CECD for the Borealis studies, the C-Dem PIs for 2022) **and a licence check passes.** The 2022 microdata are CC BY-NC, so any deposit containing 2022 rows inherits BY-NC and cannot be CC0. Without both, do not deposit, and keep building locally. If allowed, it is faster, citable, and identical for every user. | L |
| Weights and design: normalized per study and wave, with `weight_source` and `weight_type` columns; CPS/PES and trimmed/untrimmed chosen per variable from the crosswalk; `srvyr`/`survey` interop documented (Suggests only). | | S-M |
| CORA CROP 1977-2005 series as a separate `qes_polls` family. | Large value, but reuse terms must be confirmed with CORA first. | XL |

### P3: Polish

- **Bilingual messages:** `getOption("qesR.lang")` for messages and catalog names. Pushback: this is worth less than bilingual *metadata* (question text and value labels). Do the metadata first, then use a small internal message table, avoiding a gettext po/ setup until it pays off.
- **Repository root cleanup (§8):** yes, but only after V1. Removing the root CSV first silently changes the published website.
- **Documentation tooling:**
  - Move fully to roxygen2 with `@examples`, `@family` and `\alias{qesR}`.
  - Do not promise full FR parity: the 12 mirrors have already drifted (V3). Keep a smaller FR set (get-started, codebook, crosswalk reference) generated from one source.
  - Add pkgdown FR navigation derived from a naming convention.
  - Use doi.org links only.
- **Smaller fixes:**
  - Vectorized DDI parsing with `ns = character()`.
  - Lazy PDF extractors, if any are kept.
  - Classed conditions.
  - Fuzzy survey-code suggestions.
  - A smaller logo.
  - Remove `LazyData`.
  - A NEWS entry for every breaking change.

**Order:**
1. P0, including the new names (about 2-4 weeks of human effort; see the note above).
2. The owner's paper pins its qesR version (§10).
3. Resubmit as 0.5.0, with every current export still working and the changed outputs listed in NEWS.
4. The P1 crosswalk and catalog (about 4-6 weeks of human effort) then replace the harmonization internals behind the same `get_qes_master()` signature.

---

## 10. Downstream impact (v0.4.4 outputs)

This section measures what the verified bugs do to the outputs the owner's paper actually uses. Full variable lists, counts and check commands are in [`dev/downstream-impact.md`](downstream-impact.md).

**Method.**
- **Version:** HEAD c154985, whose data output equals v0.4.4 (d1faad6). The WIP diff touches only the `assign_global` defaults, `get_question()` and the codebook roots.
- **What was compared:** default `get_qes()` calls for qes2012, qes2014, qes2018 and qes2022. Each was compared with the original `.sav`/`.dta` files downloaded with `?format=original`.
- **Masters:** the tracked master (built in an English session) and a fresh v0.4.4 build (French session).
- **Paper project:** read-only.

**Fixing these bugs will change results when the paper's scripts are re-run.** It will certainly change every `get_qes_master()`-based output and the label, factor-text and type outputs of `get_qes()`. So:
- Release the fixes as a new version, with NEWS listing the changed outputs.
- The paper should pin the qesR version it used and record it in the replication output: an renv lockfile, or `remotes::install_github(..., ref = "d1faad6")`, plus `packageDescription("qesR")$RemoteSha`.

### 10.1 Per bug

For the four studies, raw codes returned by `get_qes()` equal the originals value for value. The only differences are weights and timers rounded in the `.tab` (at most about 5e-8 relative). Row and column counts match: 1505/1517/3072/1521 rows and 177/140/254/718 columns.

| Bug | Changes `get_qes()` 2012 / 2014 / 2018 / 2022? | Changes master? | Affected variables and magnitude |
|---|---|---|---|
| D10 locale-dependent reader | no / no / no / no | **yes** | qes2007_panel reads 1,234 of 2,442 rows (1,204 after dedup) and qes2018_panel 634 of 1,250, all columns, under French messages. This accounts for the whole 1,474-row gap in §4. Under English messages all rows are read, but string cells keep literal quotes (53,724 and 3,750 cells). |
| D3 encoding (U+FFFD from the server's `.tab`) | no / no / **yes** / **yes** | yes | 2018: `district`, `NM_MUNCP` (1,805 cells) and the Q2_960/961 label text. 2022: 26 text columns, 2,517 cells (`pid_fr` 843, `pes_mostimpissue` 576, `fedname` 517, `fr_pid_pr` 432...). Master: 2022 `vote_choice_text`, 4 rows. |
| D3/DDI C1 characters in labels | **yes** / no / no / no | yes | 2012 q58 (1,464 cells of `as_factor()` text) and q65 (28). These carry into the H1 columns `party_best_environment` and `feeling_business_0_100`. |
| D3 twin and type choice | **yes** / no / no / **yes** | no | 2012 (Stata twin): 174 of 177 variable labels lowercased, 107 of them also cut at 80 characters, and value labels lowercased. 2022: 6 date columns returned as character, with `""` in place of NA for 903 cells. `cps_votechoice3_5_TEXT` becomes integer and `pes_maildifficult_11_TEXT` logical (`""` becomes NA in 3,041 cells). 653 columns change from double to integer (`cps_income` factor text `1e+05` in 87 cells). |
| Label rebuild drops labels | **yes** / labels only / **yes** / labels only | via H7 | Mostly trivial `"1"="1"` labels. Real changes in `as_factor()` text: 2012 q64 and q62aa-ai (code 96 `''` becomes `96`, 145 cells). 2018 `q2_96_other` loses 84 open-text labels (88 respondents), and Q2_960-Q2_96I (19 columns) show `1` instead of a blank label. |
| D9 overrides and rebuilt variable labels | no / no / **yes** / **yes** | no | 2018: 8 variables gain correct but accent-stripped value labels the original lacks (q0qc, qsexe, qlangue, qscol, q5, q5a, q6, q26). 2022: 17 variable labels differ (4 longer, 1 shorter, and the 12 `pes_maildifficult_*` labels are NA). |
| D2 wrong-file DDI | no / labels only / no / no | no | 2014 LANG gains correct English/Français labels. The substantive D2 error, in 1998, is outside these studies. |
| K1 codebook attributes | attribute only (all four) | no | `attr(, "qes_codebook")` has no survey, DOI or file list. |
| -99 / DK codes | **no** (faithful to the source) | text | 2022: -99 in 262 columns and 128,927 cells, identical to the `.dta`. No SPSS user-missing values in these studies. Master: 2022 `income` -99 in 39 rows and `religion` in 2. |
| H1 name stacking | — | **yes** | 63 renamed columns, 45 of them populated for 2012 and 27 for 2018 (for example `vote_federal_2006` = 2012 satisfaction with democracy). |
| H2 party_best | — | **yes** | Wrong item in 2012 (1,505 rows), 2014 (1,517) and 2018 (367 values, all collapsed to DK). 2022 is a different construct. |
| H3 interest / ideology endpoints | — | **yes** | 2018 `political_interest` is inverted on a 1-4 scale (3,012 values, mean 2.09). 2014 `ideology` loses its 142 answers at 0 or 10 (1,195 becomes 1,053). |
| H4 core coding | — | **yes** | 2018 ideology is all NA (2,490 valid answers in `q36_1`). 2018 `born_canada`: 138 wrongly coded "No", 304 raw `3`, 25 raw 98/99. `language` in 2014 and 2022 is the interview or UI language. 2012 income is empty. 2022 turnout and vote are intention. 2018 `age_group` is NA for 229 respondents aged 16-17. |
| H5 dedup | — | **yes** | 380 real qes2007_panel respondents dropped (tracked build). The four cross-sections lose none. |
| H7 `vote_choice_text` 2018 | — | masked | It is sourced from the issue "other" text. It is NA only because of the label drop above, so the two bugs must be fixed together. |
| Weights / C2 / H8 | — | minor | Target studies have mean weight 1.0. 2022 PES items are weighted with the CPS weight. Panel names are cosmetic. |
| A1 default flip (HEAD) | no data change | no | 55 of the paper's calls are bare `get_qes()` calls relying on `assign_global = TRUE`. Under HEAD they fail, and `tableA1` fails **silently**. |
| Others (D1, D4-D8, H6, K2-K7, A2-A6, C1, C3-C5, V, R) | no | no | No data drift: the Dataverse versions are unchanged. The paper never calls `get_decon()` or passes `file =`. |

### 10.2 OJ_WelfareQuebec exposure

18 files call qesR directly (56 `get_qes()` calls and 3 `get_qes_master()` calls). `replication_article.Rmd` runs three of them through `readLines`. "Live" means the file is part of the current article revision. `AR/` = `Revisions_QC_juillet2026/07_Article_rework/`.

| Script | Live? | Exposure | Affected variables | Suggested check |
|---|---|---|---|---|
| AR/build_fig6_rework_v4.R | yes | none (data); A1 breaks it under HEAD | none: raw codes through `as.numeric()` and valid-code windows | Raw-vs-original identity check; N = 4477 (918/950/1748/861) |
| AR/modelsummary_tables/tableA1_modelsummary.R | yes | none (data); **A1 drops the QES table silently** under HEAD | none | `qes_status` in the JSON; `formals(qesR::get_qes)$assign_global` is `TRUE` |
| AR/modelsummary_tables/tableA4_qes_modelsummary.R | yes | none (data); A1 | none | Model N = 4477 |
| AR/replication/replication_article.Rmd | yes | inherits the three above | none | Knit a copy outside the project; N = 4477; Table A1 QES row present |
| AR/comments_aout/audit_modeles/audit3_prep_*.R (2 files) | yes | none (data); A1 | none | `table(d$year)` vs the same prep on the originals |
| Backups, v1-v3, build_english_fig6.R, fig_3_pooled_qes_ces.R, fig_3_qes_only.R (8 files) | no | none (data); A1 | none | fig_3_qes_only.R writes the **same figure file** as the affected multinomial script. chapitre_v2.Rmd uses that figure, so confirm which script wrote it |
| build_chapter_figures.R, figure_map_core_vs_appendix.Rmd | no | none (data); A1 (conditional call) | none; raw qes2022 columns equal the source | The `cps_yob`-as-age error is in the script, not in qesR |
| build_fig_qes_ces_multinomial.R | no | **affected** (master) + A1 at line 78 | 2014 `ideology` (H3 endpoints); 2014 and 2022 `language` (H4); 2012 `income` empty, imputed as 0; 2022 `vote_choice` = intention. D10 does not reach it: the panel rows drop on missing age, and the master N is 2,780 in both builds | `table(m$ideology[m$qes_code=="qes2014"])` vs raw Q32 (0 = 55, 10 = 87) |
| memo_figures_chapitre.Rmd | no | **affected** (master) | Pooled 2012/2014/2022 `ideology` by party (H3): PLQ mean 5.89 vs 6.08 with the 2014 endpoints, so the headline "PLQ ≈ CAQ" gap is 0.03 instead of 0.22. 2018 is excluded because of the H4 q36 bug. 2022 vote is intention | Recompute `party_summ` with Q32 endpoints restored |
| analyses_exploratoires.Rmd | no | **affected** (master); **D10 was live in its knitted PDF** | All qes2007_panel and qes2018_panel columns (D10, H5): the PDF shows 2018 n = 194, which matches the French build (720 in an English build). 2014 N is 1,053 instead of 1,195 (H3). The 2018 sections show only Durand panel rows (H4). §1.7 pools all 11 studies, mixing CROP, sovereignty wording and intention/recall. §9.2 has the same H3 distortion as the memo | `table(m$qes_code)` under `LANGUAGE=en` and `fr`: qes2007_panel 2062 vs 1204 |

**Bottom line.** The article pipeline (Fig. 6, Tables A1 and A4, the replication Rmd) uses no affected variable, and its published N reproduces. The package risk to it is A1: reinstalling HEAD would break it rather than change its numbers. The three master-based exploratory and chapter files are affected and should be re-run after the fixes, with the new version recorded.

---

## Appendix A. Refuted or overstated claims

Downstream-impact corrections (§10): D10 is live, not latent; qes2014 ideology is a 0-10 endpoint loss, not a 1-9 scale; the 2022 -99 values come from the source; the 2018 `vote_choice_text` source is issue text. Critic corrections applied in the previous revision: fr-demarrage chunk 19; weight means per study; panel weight sources; inst/extdata contents and unverified CRAN licence stance; Borealis 2022 is a distinct deposit (UNF differs); A3/P1 naming contradiction resolved; stale root master (40,606 vs 39,132 rows); language construct mixing. One critic citation was off: the `write.csv` calls are master.R:1871/1877, not 1872/1880.

| Claim | Why refuted |
|---|---|
| "`get_question(full = TRUE)` re-downloads the PDF codebook on every call and may trigger a full study download" | The PDF path is effectively unreachable: `codebook_files` is stripped (K1) and the truncation detector rarely fires (K3). A fallback full download happens only when attributes are missing, and it goes through the session-cached `get_codebook`. The cited lines were also wrong. |
| "dplyr verbs drop the `qes_codebook` attribute and trigger a hidden download" | Tested: `filter`, `select` and `mutate` keep both attributes. Only `d[, cols]` and `merge()` drop them, and those drop `qes_survey_code` as well, so no fetch happens. |
| "The insecure retry hammers Harvard Dataverse" | 3 to about 6 requests with 1 s delays. `curl --retry` does not retry a 404. |
| "Every SSL error on Borealis triggers `--insecure`" | With libcurl the SSL detail is in a warning, not the error text, so the regex rarely matches. The flagged qes2022 path is the real exposure. |
| "Without DDI users get unlabelled data" | True only for `.tab`/`.csv` files. Native `.sav`/`.dta` labels are kept (moot today because only `.tab` is downloaded). |
| "`get_decon('qes2014')` returns NA columns" | It returns data from the *wrong* questions (Q1/Q2), which is worse. |
| "Vignettes fail offline" | Every network chunk is `eval = FALSE`. Only examples fail offline. |
| "The CRAN rejection objects to unconditional Suggests / a missing SystemRequirements / the 202 URL" | The reviewer email mentions none of these. They are hardening items, not blockers. |
| "The FR citation table fails to translate study names" | The names are official deposit titles and must stay verbatim. |
| "`get_qes()` is 10x slower because of eager PDF extraction" | The slowdown applies only to the extraction step. Wall time is dominated by downloads. |
| "`maîtrise` escapes the education regex because of Unicode normalization" | The cause is macOS `iconv` TRANSLIT producing `ma^itrise`. |
| Duplicate helpers: "delete the get_qes.R PDF path" | Those helpers are shared and still called. Deleting them would break enrichment. |
| Count claims: "a Dataverse outage fails vignette builds", "income -99 present in the master's numeric columns", "qes2018 ideology has 2,586 valid answers" | Checks found: vignette builds do not use the network; -99 is removed in numeric scales but remains in income/religion as text; ideology has 2,490 valid answers. |
