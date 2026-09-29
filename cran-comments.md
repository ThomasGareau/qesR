## Resubmission

This is a resubmission of qesR (first submitted as 0.4.4, reviewed by
B. Altmann on 2026-02-27). This version answers each point of the
review:

1. **Method references in the Description.** The Description now cites the
   dataset of every study the package reads, in the form
   `authors (year) <doi:...>`: the Quebec Election Studies of 2007, 2008,
   2012, 2014, 2018 and 2022, the Durand panel surveys of 2007, 2012 and
   2018, the CROP polls of 2007-2010 and the 1998 election polls (11 DOIs).
   'Dataverse', 'Borealis', 'SPSS', 'Stata' and 'MD5' are in single quotes.

2. **Small executable examples.** Every Rd file has examples, and all of
   them run without a network connection: they use `qes_demo`, a small
   synthetic study shipped in `inst/extdata/demo/` (60 random respondents,
   5 KB), and the catalog and variable metadata that ship with the package.
   The helpers that work on objects in memory (`format_codebook()`,
   `get_value_labels()`, `get_question()`, ...) have runnable examples on
   the demo study or the shipped metadata. The only example that uses the network is
   `qes_studies(check_updates = TRUE)` (one metadata request per deposit),
   in `\donttest{}`, run only `if (curl::has_internet())` and inside
   `tryCatch()`. No example has commented-out code. Tests run every
   example with the network code replaced by an error, to keep them
   offline, and check that no example line is commented-out code.

3. **Vignettes should execute code.** The eight vignettes (four in English,
   four in French: getting started, study citations, moving from 0.4.4, and
   the harmonization reference generated from the package's specification)
   execute their code, offline, on the catalog, the shipped metadata and
   `qes_demo`. Only the chunks that would download a full study are shown
   without being run. The analysis examples that need full data files are
   no longer vignettes: they are articles on the package website only.

4. **No writes to the user's home or working directory.** Nothing is
   written outside `tempdir()` by default. Downloads are cached in a folder
   of `tempdir()`, removed when R exits; a persistent cache in
   `tools::R_user_dir("qesR", "cache")` is used only when the user sets
   `options(qesR.cache = "disk")`. `qes_download(path =)` has no default
   and requires an existing directory chosen by the user;
   `download_codebook(dest_dir = tempdir())` keeps its temporary default;
   `get_qes_master(save_path =)` writes only when given a path. The README and
   the vignettes no longer show code that saves into the working
   directory: their `qes_download()` example writes to a folder of
   `tempdir()`. A test
   calls every exported function with its default arguments and checks
   that the global environment, the working directory, the home directory
   and `R_user_dir()` are unchanged.

5. **Do not modify the global environment.** No function writes into
   `.GlobalEnv`. Every function returns its result visibly. The
   `assign_global` argument of `get_qes()`, `get_qes_master()`,
   `get_decon()` and the codebook functions now defaults to `FALSE`; when a
   user sets it to `TRUE`, the object is assigned into the calling
   environment (`parent.frame()` of the exported function), which is the
   global environment only when the user calls the function at top level
   themselves. The argument keeps its name only so that code written for
   0.4.4 keeps working (the function signatures are unchanged); the help
   pages say where the object goes. A test scans the installed namespace:
   `.GlobalEnv` and `globalenv()` appear nowhere, and `assign()` appears only
   in the one internal helper that performs the opt-in assignment.

6. **Copyright holders.** The `cph` entry "Quebec Election Study" is
   removed from `Authors@R`: it is not a legal person, and the package ships
   none of the studies' data. The author, Thomas Gareau-Paquette, is the
   copyright holder of the code (role `cph`), and `LICENSE` now names him
   (it named "Quebec Election Study/Étude électorale québécoise rightful
   owners"). The file `inst/COPYRIGHTS` lists the third-party content that
   does ship: variable and value labels, question wording and counts of the
   studies released by their authors under CC0 1.0 (each with its DOI), and
   the same metadata of the 2022 study, which is licensed CC BY-NC 4.0
   (see the section "Question for the CRAN team" below: we ask you whether
   this is acceptable). The file
   `inst/extdata/qes_master.csv`, which held respondent rows derived from
   the 2022 study, is removed, and the package contains no respondent data.
   qesR downloads the data files from their Dataverse deposits (Borealis
   and the Harvard Dataverse) at the user's request.

   **Third-party benchmark data.** `inst/extdata/validation/` ships one
   third-party table, `census_margins.csv`: counts of the Quebec population
   summed from Statistics Canada census tables 98-10-0020-01, 98-10-0218-01
   and 98-10-0384-01, the 2011 Census Profile (98-316-XWE2011001) and the
   2006 table 97-551-XCB2006009, used under the Statistics Canada Open
   Licence (<https://www.statcan.gc.ca/en/terms-conditions/open-licence>),
   with the attribution and non-endorsement notice it requires
   (`inst/COPYRIGHTS`, section 3). The official election results of
   Élections Québec that the development repository uses as validation
   benchmarks (`official_results.csv`, `official_turnout.csv`) and the
   validation report computed from them (`validation_report.csv`) are
   excluded from the package build (`.Rbuildignore`). Élections Québec's
   open-data licence (<https://www.dgeq.org/licence.html>) covers only the
   files listed on its open-data site, which for these tables means the
   elections of 2014, 2018 and 2022; it allows commercial use and
   sub-licensing, but it requires an indemnity and a fixed notice, and
   Élections Québec may cancel it without giving a reason, which would
   cancel every sub-licence, so it probably does not meet the terms CRAN
   expects of a data licence, and the package does not rely on it. The
   elections of 1998, 2007, 2008 and 2012 fall under the terms of use of its site, which need its written
   permission for uses other than non-profit reproduction and for
   adaptations; that permission has not been asked for or obtained. The
   internal checks that read these files are skipped when they are absent,
   and the tests that need them run only on the source tree. The election
   dates in `inst/extdata/catalog/elections.csv` are facts taken from the
   official results pages, each row with its source URL.

## Question for the CRAN team: data files under CC BY-NC 4.0

From version 0.7.1, qesR ships metadata of the 2022 Quebec Election Study
(Mahéo, Bélanger, Stephenson and Harell 2023, <doi:10.7910/DVN/PAQBDR>),
which its authors license under CC BY-NC 4.0
(<https://creativecommons.org/licenses/by-nc/4.0/>). These data files carry
that licence, not the package's MIT licence: the rows of the 2022 study in
`inst/extdata/dict/variables.csv.gz` and `values.csv.gz` (variable and value
labels, question wording quoted from the study's codebook in English and
French, and the unweighted count of each answer code), in
`inst/extdata/harmonize/crosswalk.csv`, `valuemaps.csv`, `gates.csv`,
`waves.csv`, `weights.csv`, `expected/marginals.csv` and
`expected/hashes.csv` (wording, labels, counts and hashes used to check the
harmonization), in the notes of `inst/extdata/catalog/studies.csv` (the
size of its two waves, from its codebook), and in the test fixtures
`tests/testthat/fixtures/v044-get-qes-names.csv` and
`tests/testthat/fixtures/spec_legacy/valuemaps.csv`, and in the two
harmonization-reference vignettes (`harmonization-reference`,
`fr-reference-harmonisation`), which print the wording and labels of the
specification (a few labels and counts are also quoted in the other
vignettes, in the tests and in NEWS.md). No respondent rows ship. Section 2 of `inst/COPYRIGHTS`
lists these files exactly (a test checks the list against the package),
gives the attribution the licence requires and says that this content is not covered by the MIT licence; the
`Copyright` field of `DESCRIPTION` says that the MIT licence covers the
package code only and points to it. `License` stays `MIT + file LICENSE`.

This is a change in the licensing of included third-party material: until
0.7.0 the package shipped only CC0 study metadata. R's licence database
(`share/licenses/license.db`) classes CC BY-NC 4.0 as not FOSS and as
restricting use, so `DESCRIPTION` now declares `License_restricts_use: yes`
and `License_is_FOSS: no` for the package as distributed, while its code
stays under MIT.

CC BY-NC 4.0 restricts commercial use, so these files are not free in the
sense of the CRAN policy. Is it acceptable for the package to include them
as documented third-party data under their own licence? If it is not, we
will remove them from the package and have qesR fetch them at runtime
instead (from the study's deposit, into the user's cache, as versions 0.5.0
to 0.7.0 did, which built them from the user's copy of the data file), so
that no CC BY-NC 4.0 content remains in the tarball. We will follow your
answer.

Other changes since the first submission are in NEWS.md. Every exported
function of 0.4.4 keeps its name and arguments.

## Test environments

- Local: macOS 26 (aarch64), R 4.4.0.
- GitHub Actions (`.github/workflows/R-CMD-check.yml`): macOS and Windows
  (R release), Ubuntu (R devel, release, oldrel-1 and 4.1), with
  `--as-cran`. RESULTS TO ADD BEFORE SUBMISSION (maintainer): the workflow
  runs only after the branch is pushed, which has not been done yet.
- win-builder (R devel): TO RUN BEFORE SUBMISSION (maintainer): upload the
  tarball with `devtools::check_win_devel()`; the results go to the
  maintainer's e-mail.

## R CMD check results

`_R_CHECK_CRAN_INCOMING_REMOTE_=false R CMD check --as-cran qesR_0.7.1.tar.gz`
on the local machine (macOS, R 4.4.0), run on 2026-09-29 on the release
tarball of 0.7.1, built by `data-raw/build_tarball.sh` (a copy of the tree
with git's file modes), without network access: 0 errors | 0 warnings |
2 NOTEs, both from the local machine (future file timestamps and the HTML
version of the manual, below). Without network access the incoming check
could not read the CRAN package index, so its "New submission" NOTE and its
remote URL and DOI checks did not run here. TO RERUN BEFORE SUBMISSION
(maintainer): `sh data-raw/build_tarball.sh <dir> --check`, with network
access, and update this section with its result.

The tests pass (7,572 expectations, 0 failures, 0 warnings, in about 110
seconds; 23 skipped with their reason: network tests and the tests that
need the build-ignored official results, skipped on CRAN). The examples
and the vignettes run without errors.

* checking CRAN incoming feasibility ... NOTE (expected on CRAN's machines;
  not reached locally without network access, see above)

  New submission.

  Possibly misspelled words in DESCRIPTION: the local machine has no
  spell checker (neither aspell nor hunspell), so this part of the
  incoming check did not run here. If it lists Bélanger, Blais, Durand,
  Goyder, Mahéo, Nadeau or "al", these are the surnames of the authors of
  the datasets cited in the Description with their DOIs, and the "al" of
  "et al."; "catalogued" (British spelling) and "codebooks" are correct
  words. (MAINTAINER: replace this paragraph
  with the words the win-builder check actually lists.)

  The earlier submission also had a NOTE on the URL
  `https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PAQBDR`
  (Status: 202, Message: Accepted). The package no longer links to Harvard
  Dataverse pages: the README, vignettes and citations link only to
  `https://doi.org/...`, and the local check with
  `_R_CHECK_CRAN_INCOMING_REMOTE_=true` reported no URL or DOI problem. If
  the NOTE appears on CRAN's machines for `doi:10.7910/DVN/PAQBDR` (the
  2022 study, cited in the Description), it is the same case: the DOI
  resolves to the Harvard Dataverse landing page of the study, which opens
  in a browser, but Harvard Dataverse answers some automated requests with
  202 Accepted (a bot challenge) instead of 200. The DOI is valid.

* checking for future file timestamps ... NOTE

  unable to verify current time. This depends on the check machine reaching
  a time server and is not related to the package.

* checking HTML version of manual ... NOTE (local only)

  The local HTML validator is the macOS system `/usr/bin/tidy` ("HTML Tidy
  for Mac OS X released on 31 October 2006"). It predates HTML5 and reports
  `<main> is not recognized!` for every Rd file, because R's own HTML help
  uses `<main>`. The NOTE does not occur with tidy-html5, and the Rd files
  have no problems of their own.

## Dependencies

- `curl` (Imports) is the HTTP client. It returns the status, headers and
  body of a request in one call, which qesR needs to honour `Retry-After`,
  to recognise a server's refusal of automated requests without retrying
  it, and to verify each file before it is kept. It has per-transfer stall
  timeouts, no R dependencies and never shells out. It replaces
  `utils::download.file()`. On Linux, installing `curl` from source needs
  the system libcurl (libcurl4-openssl-dev), which is standard.
- `survey` and `srvyr` (Suggests) are used only by `qes_design()`, which
  turns harmonized data into a survey design when the user asks for one;
  it checks that they are installed and says which one to install when
  they are not. The examples, tests and vignettes that use them run only
  when they are installed.
- `jsonlite` (Imports) reads Dataverse's JSON answers, used only for
  `qes_studies(check_updates = TRUE)` and `qes_download(version =
  "latest")`.
- `haven` (Imports) reads the original SPSS and Stata data files.
- `xml2` is no longer imported.
- Network use: requests go only to the Dataverse servers listed in the
  shipped catalog, one at a time and at least one second apart, with the
  User-Agent `qesR/<version> R/<version>` and no other identifying
  information. qesR never disables TLS certificate checks, never runs
  external programs and never calls `readRDS()` or `load()` on downloaded
  content.
