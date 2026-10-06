# data-raw: how qesR's shipped tables are built

Everything under `inst/extdata/` that is not edited by hand is written by a
script in this folder. Run every script from the package root. `data-raw/` is
build-ignored, so none of this is in the package tarball.

- **Clean clone** means the script runs from a fresh `git clone` with nothing
  else, or with only plain public downloads (`fetch_inputs.R`,
  `live_fetch.R`).
- **CI --check** says which workflow runs the script's `--check` mode.
  `--check` writes nothing and fails when a committed file differs from what
  the script would write.

## Getting the inputs

```sh
Rscript data-raw/fetch_inputs.R [<dest>]   # default: tools::R_user_dir("qesR", "cache")/build-inputs
export QESR_BENCH_SRC=<dest>/bench
export QESR_CACHE_DIR=<dest>/originals
Rscript data-raw/live_fetch.R              # the pinned originals, md5-verified
export QESR_DV_JSON_DIR=<dest>/dataverse_latest   # optional: checks the pins against Dataverse
```

`data-raw/inputs.csv` lists every downloadable input: its name, URL,
destination subfolder, file name, md5, licence and method.

- `fetch_inputs.R` downloads the `plain` rows through qesR's own HTTP client:
  one GET each, User-Agent `qesR/<ver> R/<ver>`, sequential and at least 1 s
  apart. It then checks each md5. A mismatch is an error on `strict` rows and
  only a warning on `warn` rows (Statistics Canada full tables, which are
  revised, and the current Dataverse JSON).
- It never requests a `manual` row. It prints the row's URL, where to save
  the file and the expected md5.
- The respondent-level originals are not in the manifest. `live_fetch.R`
  fills a qesR download cache with them, checking each against the md5
  pinned in the catalog.

### What needs a browser

- **2011 Census Profile** (`98-316-XWE2011001-101_CSV.zip`, row
  `statcan_profile2011_101`). Statistics Canada serves a Cloudflare
  JavaScript challenge to plain requests. A plain GET returns an HTML page,
  which `build_benchmarks.R` rejects by its md5. Open the URL in a browser
  and save the zip as `<dest>/bench/zips/98-316-XWE2011001-101_CSV.zip`. No
  copy of the real file or of its Quebec rows was available when the inputs
  were committed, so no extract is committed (`inputs/README.md`).
- **Text copies of the questionnaires** for `extract_questions.R`
  (`QESR_DOCS_TXT_DIR`). They are made by hand with macOS
  `textutil -convert txt` and `pdftotext -layout` from the files listed by
  `qes_docs()`. This only matters for drafting new wording.

## Builders

| Script | Inputs | Env vars | Writes | Clean clone | CI --check |
|---|---|---|---|---|---|
| `build_catalog.R` | `catalog/*_curated.csv`; Dataverse JSON: the committed snapshots `inputs/dataverse/` (pinned versions, `index.csv`) or the current dataset JSON | `QESR_DV_JSON_DIR` (optional; when set, every deposit's latest version must equal its pin) | `inst/extdata/catalog/studies.csv`, `files.csv`, `inst/extdata/VERSIONS` | yes, no request | R-CMD-check (ubuntu release) |
| `fetch_inputs.R` | `inputs.csv` | (argument: dest dir) | `<dest>/dataverse_latest`, `<dest>/bench` | yes (downloads) | no |
| `live_fetch.R` | the catalog's pinned files | `QESR_CACHE_DIR` | a qesR download cache | yes (downloads) | live (runs it) |
| `build_dictionary.R` | originals; `questions/*.csv`, `questions/missing_*.csv` | `QESR_CACHE_DIR` | `inst/extdata/dict/*.csv.gz`, `inst/extdata/demo/dict/*.csv.gz`, `VERSIONS` | yes (downloads) | live |
| `build_sources.R` | originals; the spec | `QESR_CACHE_DIR` | `inst/extdata/harmonize/gates.csv` | yes (downloads) | live |
| `project_marginals.R` | the spec, shipped dictionary, `gates.csv` | none | `harmonize/expected/marginals.csv` | yes, no request | R-CMD-check (also V-P1 in `spec_check.R`) |
| `build_relaxed.R` | the spec (`relaxed.csv`, `levels.csv`), shipped dictionary; with `--expected` the originals | `QESR_CACHE_DIR` (`--expected` only) | `harmonize/relaxed_maps.csv`, the `rx_` rows of `valuemaps.csv`; with `--expected`, `expected/relaxed_marginals.csv`, `expected/relaxed_hashes.csv` | yes (`--expected` downloads) | R-CMD-check (without `--expected`); V-R11 in the live tests |
| `build_hashes.R` | originals; the spec | `QESR_CACHE_DIR` | `harmonize/expected/hashes.csv` | yes (downloads) | live |
| `build_benchmarks.R` | Élections Québec JSON, Statistics Canada zips, the 2006 IVT (Internet Archive), the 2011 Profile zip | `QESR_BENCH_SRC` | `inst/extdata/validation/official_results.csv`, `official_turnout.csv`, `census_margins.csv` | no: the 2011 Profile needs a browser | no |
| `build_validation.R` | originals; the benchmarks | `QESR_CACHE_DIR` | `inst/extdata/validation/validation_report.csv` (`--out` for the CI artifact) | yes (downloads) | live (information only) |
| `build_legacy.R` (frozen) | `legacy_master_source_map.csv` and the 0.4.4 `get_qes()` names (falls back to `tests/testthat/fixtures/v044-get-qes-names.csv`) | `QESR_LEGACY_BASELINE` (default `data-raw/baselines/v044/`) | `data-raw/legacy_source_map.csv`, `inst/extdata/legacy/removed.csv` | no: the master source map comes from `make_legacy_baselines.R` | no |
| `compare_legacy.R` (frozen) | 0.4.4 and 0.5.0 builds (`.rds`, respondent-level, local only); originals | `QESR_LEGACY_BASELINE`, `QESR_LEGACY_BASELINE_050`, `QESR_TEST_DATA_DIR` | `dev/legacy-diff.md`, `inst/extdata/legacy/changes.csv`, the legacy table of `NEWS.md` | no: needs `make_legacy_baselines.R` | no |
| `make_legacy_baselines.R` | git tag `v0.4.4`, commit `aa1d3bd` | `QESR_TEST_DATA_DIR` (optional, 0.5.0 run) | `--out <dir>`: the baselines above | yes (installs old versions, downloads); never in CI | never |
| `make_demo.R` | none (fixed seed) | none | `inst/extdata/demo/data/qes_demo.sav`, `demo/catalog/*.csv` | yes | no: the `.sav` header holds a timestamp, so the md5 changes on every run |
| `make_citation.R` | `R/cite.R` | none | `inst/CITATION` | yes | tested by `test-contract-exports.R` |
| `make_favicons.R` | `man/figures/logo.svg` | none (needs magick, rsvg) | `man/figures/logo.png`, `pkgdown/favicon/` | yes | no |
| `readme_coverage.R` | the spec | none | the coverage grids of `README.md` | yes | V-P3 in `spec_check.R` |
| `spec_check.R` | the source tree; `origin/main` | none | with `--write-hash`, the Hash of `inst/extdata/harmonize/SPEC` | yes | R-CMD-check (`--release` on tags) |
| `extract_questions.R` | text copies of the documents; originals | `QESR_DOCS_TXT_DIR`, `QESR_CACHE_DIR` | drafts new rows of `questions/*.csv` (never changes existing ones) | no: the text copies are made by hand | no |
| `legacy_build_master.R` | originals | none (`--out-dir`) | a saved `get_qes_master()`, never committed | yes (downloads) | no |
| `check_vignette_pairs.R` | `vignettes/` | none | nothing (a check) | yes | R-CMD-check, pkgdown |
| `check_site.R` | `_pkgdown.yml`, a built site | none | nothing (a check) | yes | pkgdown |
| `build_tarball.sh` | the source tree | none | a tarball with normalised file modes; `--check` runs the CRAN incoming checks | yes | the `cran-incoming` job does the same on tags |
| `versions.R` | (helper sourced by `build_catalog.R` and `build_dictionary.R`) | | `VERSIONS` | | |

CI runs the live checks in `.github/workflows/live.yml`: weekly, on `v*`
tags and by hand. It works on the cached originals, with no request beyond
`live_fetch.R`'s fill.

## Run order

A full rebuild, after a change to the curated sources or a pin:

1. `build_catalog.R`: catalog and `VERSIONS`.
2. `live_fetch.R`: the pinned originals into `QESR_CACHE_DIR`.
3. `build_dictionary.R`: dictionary, `VERSIONS`.
4. `build_sources.R`: `gates.csv`.
5. `project_marginals.R`: `expected/marginals.csv`.
6. `build_relaxed.R --expected`: relaxed rows and their recorded results.
7. `build_hashes.R`: `expected/hashes.csv`.
8. `build_benchmarks.R`: official results and census margins (needs the
   manual 2011 Profile).
9. `build_validation.R`: the validation report (gated: see its header for
   `--accept-regressions`).
10. Frozen, only with `--check` or when the legacy record itself is wrong:
    `build_legacy.R`, then `compare_legacy.R`.
11. As needed: `make_demo.R`, `make_citation.R`, `make_favicons.R`,
    `readme_coverage.R`.
12. `spec_check.R --write-hash`, after bumping `Spec-Version` and adding a
    `CHANGES.csv` row (design.md section 5.11), then `spec_check.R`.

## Hand-curated sources (primary; edited by hand, reviewed in PR diffs)

No script writes these.

- `inst/extdata/catalog/elections.csv`, `enums.csv`, `name_map.csv`,
  `text_fixes.csv`, `type_fixes.csv`.
- `data-raw/catalog/studies_curated.csv` and `files_curated.csv`: the
  judgments `build_catalog.R` overlays on the Dataverse facts.
- `data-raw/questions/<study>.csv`, `<study>_values.csv`,
  `missing_labels.csv`, `missing_codes.csv`. `extract_questions.R` may add
  draft rows with `reviewed = FALSE`; every other edit is by hand.
- The harmonization spec in `inst/extdata/harmonize/`: `SPEC` (except its
  Hash line), `CHANGES.csv`, `targets.csv`, `levels.csv`, `crosswalk.csv`,
  `valuemaps.csv` (except its `rx_` rows), `waves.csv`, `weights.csv`,
  `pooled.csv`, `pooled_members.csv`, `relaxed.csv`, `legacy.csv`. The
  relaxed rows are declared by hand in `build_relaxed.R`, which writes
  `relaxed_maps.csv` and the `rx_` value maps.
- `data-raw/inputs.csv` and `data-raw/inputs/dataverse/index.csv` (the pins).
- Test fixtures in `tests/testthat/fixtures/`.

## Frozen artefacts

These record earlier versions or one-off captures. They are not rebuilt in
the normal order.

- `data-raw/legacy_source_map.csv`, `inst/extdata/legacy/removed.csv`
  (`build_legacy.R`) and `inst/extdata/legacy/changes.csv` with the legacy
  table of `NEWS.md` (`compare_legacy.R`): what qesR 0.4.4 and 0.5.0 did.
- The legacy baselines, regenerated only by `make_legacy_baselines.R`, which
  installs tag `v0.4.4` and commit `aa1d3bd` from `git archive` into
  temporary libraries and runs them under `LANGUAGE=en`. Its two CSVs
  (`legacy_master_source_map.csv`, `legacy_get_qes_names_all_studies.csv`)
  hold variable names only. They may be committed to
  `data-raw/baselines/v044/`, the default input of `build_legacy.R`, with a
  note that their qes2022 rows describe a CC BY-NC 4.0 deposit. Its `.rds`
  files are respondent-level data and are never committed. No copy of the
  master source map was available when this README was written, so
  `build_legacy.R --check` needs a run of `make_legacy_baselines.R` first.
- `inputs/dataverse/*.json`: the pinned versions' metadata (2026-10-05).
- The 2006 census table `97-551-XCB2006009.IVT`, an Internet Archive
  capture (md5-checked by `build_benchmarks.R`).
- `inst/extdata/demo/`: the synthetic demo, rebuilt only when the demo
  changes.

Never commit respondent-level files (originals, `.rds` built from them) or
the Élections Québec JSON (their terms: inst/COPYRIGHTS, section 3).
