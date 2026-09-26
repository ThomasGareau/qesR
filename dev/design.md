# qesR redesign: integrated design (approved)

- **Branch:** `redesign` (HEAD c154985, version 0.4.4).
- **Date:** 2026-09-26. **Revision 3.** Revision 2 followed two critic reviews (harmonization validity; API coherence, CRAN and slicing); §12.4 records what was accepted and what was rejected. Revision 3 records the owner's decisions of 2026-09-26 (§0.3): every recommendation OD1-OD16 accepted, the API trimmed to 13 new exports, a new data-access policy and a website slice W.
- **Inputs:** `dev/assessment.md`, `dev/downstream-impact.md`, and two synthesized track designs:
  - track (a), harmonization, based on *maintainer-first*;
  - track (b), API, HTTP, cache and catalog, based on *ces-familiar*.
- **Status:** approved by the owner on 2026-09-26 (§0.3). Implementation proceeds slice by slice on branch `redesign` (§11). Where the two tracks disagreed, this document picks one answer and records the other in §12.

Facts marked "verified" were checked offline against the cached originals: qes2012 SPSS/Stata, qes2014 SPSS/Stata, qes2018 `.dta`, qes2022 `.dta`, and the 2007/2018 panel `.sav` files. The cached DDI files, the 2018 questionnaire text and the 2014 technical note were also used. No network request was made for this document. The checks are in `scratchpad/design/integrate/chk.R`, `scratchpad/design/critic-validity/[a-o].R`, `scratchpad/design/critic-coherence/loc*.R` and `scratchpad/design/apply-critics/v1.R`, `v2.R`.

### ID namespaces used in this document

IDs from different lists never share a prefix:

| Prefix | Meaning | Where defined |
|---|---|---|
| `OD1`-`OD19` | owner decisions (all decided 2026-09-26) | §0.2, §0.3 |
| `P1`-`P10` | design rules | §1.1 |
| `V-S*`, `V-D*`, `V-P*`, `V-L*` | validator rules: spec, data, projection, live | §5.10 |
| `S0a`-`S5` | data-lane slices | §11 |
| `W` | website slice (refreshed at 0.6.0 and 0.7.0 as `W.1`, `W.2`) | §9.1, §11 |
| `HZ1`-`HZ9` | harmonization-lane slices | §11 |
| `R1`-`R9` | data needs: Claude fetches public items; the owner is asked only for permissions and non-public documents | §13.3 |
| `[A:D10]`, `[A:H3]`, `[A:K1]` ... | bug IDs from `dev/assessment.md` | assessment |
| `CV1`-`CV23`, `CC-A1`-`CC-F7` | critic findings on revision 1 | §12.4 |

---

## 0. Summary and decisions

### 0.1 What changes for users (10 bullets, tagged by release)

1. **[0.5.0] `get_qes()` returns the data and no longer writes to your workspace by default.** Write `qes2018 <- get_qes("qes2018")`. `assign_global = TRUE` still works. It now assigns into the *calling environment*, which is `.GlobalEnv` only when you call it at top level. The call signature is unchanged.
2. **[0.5.0] Data comes from the pinned original `.sav`/`.dta` file, checked by md5.**
   - Accents are no longer garbled: 1,805 damaged cells in 2018 and 2,517 in 2022 are fixed.
   - qes2022 dates are real dates.
   - No rows are lost under French messages.
   - Raw codes, column names and `is.na()` counts stay as in v0.4.4.
3. **[0.5.0] Codebooks, questions and search are offline and bilingual:**
   - `qes_codebook()`, `qes_question()` and `qes_search("souverain|sovereign")` use shipped metadata;
   - `qes_docs()` and `qes_download()` list and fetch the questionnaires and technical reports;
   - four helpers that return 0 rows today start working.
4. **[0.6.0, experimental] `qes_harmonize()` is a new cross-study engine driven by a reviewed spec.**
   - Every value traces to a study, wave, source variable, source code, spec row and spec version.
   - One harmonized variable ("target") means one question stimulus.
   - Intention and recall are separate targets, and so are lean-pushed intention, the 4-point and 0-10 interest scales, and "independent country" vs "sovereign country" vs 1995 partnership wording.
5. **[0.6.0] Each (study, target) cell carries a comparability grade** (`identical` / `comparable` / `approximate`) with an EN/FR reason.
   - `qes_spec()` (default view `"targets"`) shows which studies have what.
   - `min_grade =` restricts output to strictly comparable cells.
   - The answer options each study offered are recorded, so a party that was not on a study's list is shown as a structural zero, not as 0% support.
6. **[0.6.0] Missing data is never silent.**
   - Every NA has a reason (`dk`, `refused`, `dk_refused`, `inapplicable`, `not_voted`, `not_in_wave`, ...).
   - Unmapped codes are an error.
   - Sentinels such as −99, 98 and 99 can never leak into harmonized values.
7. **[0.6.0] Design is explicit:**
   - panels and the 2022 CPS/PES have waves (`layout = "long"`);
   - each wave declares its target population (the 1998 panel is francophones only);
   - weights come from a registry and are normalized per study × wave, with `weight_pre` and `weight_post` kept apart;
   - `qes_design()` hands the result to `survey`/`srvyr`;
   - the default study set is the QES election studies only, so CROP no longer makes up 60% of pooled rows.
8. **`get_qes_master()` keeps its signature and its 30 documented columns, with corrected values:**
   - [0.5.0] the 63 auto-stacked columns with invalid content are removed, and cells verified to be invalid are blanked;
   - [0.5.0] no respondent is dropped, whether by deduplication or by empty-row removal (qes2007_panel has 2,442 rows);
   - [0.7.0] it is rendered from the engine, and `vote_choice`/`turnout` mean reported vote everywhere;
   - v0.4.4 numbers are reproducible only by installing d1faad6.
9. **[0.5.0 study level, 0.6.0 cell level] Every result is versioned.**
   - `qes_provenance()` records the DOI, dataset version, file id, md5, catalog/dictionary/spec versions, spec hash and qesR version.
   - `qes_cite()` turns that into a citation.
10. **[0.5.0 or at the replacement's release] Eleven old helper names become soft-deprecated legacy wrappers that keep working indefinitely.** Each prints a one-time EN/FR message naming its replacement, and only once that replacement has shipped. Messages follow your language; returned data never does.

### 0.2 Owner decisions OD1-OD16 (all DECIDED 2026-09-26, as recommended)

The owner accepted every recommendation on 2026-09-26. Each row keeps the recommendation text, which is now the decision.

**What still waits on what** (all owner decisions are in):
- S0a-S3 and HZ1-HZ4 wait on nothing outside the design.
- S4 (interim master) needs data items R1, R2 and R9, which Claude fetches or builds itself (§13.3).
- S5 (0.5.0 on CRAN) needs S2b's all-study gate, which needs R1 and R2.
- The only items that can wait on the owner are permissions and non-public documents (R8, and any R-item that turns out not to be publicly fetchable).

| # | Decision | Decision of 2026-09-26 |
|---|---|---|
| OD1 | Status of `get_qes_master()`. Track (a) soft-deprecated it; track (b) kept it stable. | **DECIDED (as recommended).** **Stable, no deprecation message.** It is the fixed legacy schema; its docs point new work to `qes_harmonize()`. The one-time "values changed" message still applies. |
| OD2 | Release plan | **DECIDED (as recommended).** **Three CRAN releases.**<br>• **0.5.0:** data lane plus the interim master (§5.12): deletions and blanking of every cell verified invalid, with no new recoding logic.<br>• **0.6.0:** the engine, marked experimental, for the six cached studies.<br>• **0.7.0:** all 11 legacy studies in the spec, the master switched to the engine, and spec 1.0.0.<br>0.5.0 waits for R1/R2 so that `get_qes()` on originals is verified for all 11 studies. The alternative is to keep the engine on GitHub until 0.7.0. |
| OD3 | May qes2022 (CC BY-NC) value-label text, question wording or aggregate counts ship in the MIT tarball? | **DECIDED (as recommended).** **No, until the C-Dem PIs agree in writing.** Ship md5 hashes of labels, `wording_ref` only, and CI-only aggregates. qes2022 metadata is built at runtime from the user's own copy. |
| OD4 | Legacy `vote_choice`/`turnout`: recall-only, or recall else intention? | **DECIDED (as recommended).** **Recall-only.** Intention moves to the appended `vote_intent`/`turnout_intent` columns. In 0.5.0 the interim master blanks the intention-based cells (2022 and CROP). 1998 is filled from the `q3post` recall from 0.7.0. |
| OD5 | Legacy `sovereignty_support`/`sovereignty`: the independent-country item only, or spliced with other wordings? | **DECIDED (as recommended).** **Independent-country only.** NA for 2007, 2008 and 1998 (partnership wording) and for 2012p ("pays souverain"), from 0.5.0 on. Append `sov_partnership_1995`. |
| OD6 | Legacy `survey_weight` | **DECIDED (as recommended).** **Keep the v0.4.4 source variable and raw scale per study.** Append `weight_pre`/`weight_post` (raw); `legacy_column_map` points to the weight guide. The alternative is the recommended post-wave weight normalized to mean 1, which changes values (CROP 6.04 becomes 1). |
| OD7 | Legacy `political_interest` (the 0-10 legacy column) | **DECIDED (as recommended).** **Keep it populated.**<br>• Apply v0.4.4's intended 1→10, 2→7, 3→3, 4→0 map to the 4-point items. This fixes [A:H3], where 2018's raw 1-4 codes landed unconverted on the 0-10 column. The 2018 source uses the same code direction as 2012 and 2014 (verified).<br>• Pass the 2022 0-10 item through.<br>• Flag the column `approximate` in `legacy_column_map`.<br>The new API never pools the two instruments. |
| OD8 | Legacy `language` when a respondent reports several mother tongues (2014 QLANG 4-6, n = 115) | **DECIDED (as recommended).** **NA, with a message.** |
| OD9 | `get_decon("qes2022")` `votechoice`: intention (v0.4.4, the CES campaign frame) or recall? | **DECIDED (as recommended).** **Keep intention**, and add `attr(, "timing")`. |
| OD10 | qes2022 default weights: untrimmed or trimmed `*_weight_general`? | **DECIDED (as recommended).** **Untrimmed**, which reproduces the calibration margins. |
| OD11 | ADQ and CAQ as separate levels with no merged level? | **DECIDED (as recommended).** **Yes.** Also offer an optional lineage helper for ADQ → CAQ time series, graded `approximate` and never applied by default. |
| OD12 | 2022 `pes_turnout` = 5 ("wasn't registered", n = 3) and = 6 ("don't remember", n = 2) | **DECIDED (as recommended).** **5 → NA reason `not_registered`** (still `eligible_voter`, but excluded from official-denominator benchmarks); **6 → `dk`**. The earlier draft's `ineligible` for code 5 was wrong: "ineligible" is reserved for 2018 `q5` = 5 ("I was not eligible"). |
| OD13 | `cph` entry in DESCRIPTION ("Quebec Election Study" is not a legal person) | **DECIDED (as recommended).** **Drop it.** LICENSE names Thomas Gareau-Paquette. `inst/COPYRIGHTS` documents the shipped CC0 labels and wording by study, with DOIs. |
| OD14 | Ask QES/CECD and C-Dem for permission to deposit a harmonized cumulative file on Borealis? | **DECIDED (as recommended).** **Later (slice HZ9).** Nothing depends on it. |
| OD15 | Legacy `age_group` for qes2018_panel, whose source has only 3 bands (18-34/35-54/55+), while v0.4.4 populated all 1,250 rows | **DECIDED (as recommended).** **Keep it populated from `age_group3`**, flagged in `legacy_column_map`. This matches v0.4.4, where the column already mixed band schemes across studies. The alternative, NA, blanks data users had. |
| OD16 | May `inst/COPYRIGHTS` and the docs say that qes1998 covers "QC francophones 18+" before the definition of "francophone" is known (R2)? | **DECIDED (as recommended).** **Yes, quoting the codebook** ("retenir uniquement les francophones"), with the definition marked as pending. |

Questions that need documents rather than decisions are listed in §13.3 (data needs R1-R9).

### 0.3 Owner decisions of 2026-09-26

| # | Decision | Effect in this document |
|---|---|---|
| OD1-OD16 | **DECIDED:** every recommendation in §0.2 accepted. | §0.2 rows marked DECIDED; the S4/S5 dependency list no longer waits on the owner. |
| OD17 | **DECIDED: API trimmed to 13 new exports.** `qes_targets()`, `qes_crosswalk()` and `qes_spec()` fold into one exported metadata function, `qes_spec(view = )`. `qes_splice()` and `qes_join_raw()` stay internal (`.qes_splice()`, `.qes_join_raw()`) for now. | §2.1, §2.2 (13 new, 16 canonical, 27 exports, 28 Rd pages), §5.3, §9, §11 (HZ3, HZ4), §12.2, §13.1. |
| OD18 | **DECIDED: data access policy.** Claude fetches public data and documents itself, with plain requests carrying no personal information (User-Agent exactly `qesR/<ver> R/<ver>`, cached, sequential, at least 1 s apart, backoff on 429/503). The owner is asked only for permissions and for documents that are not public. If an item cannot be fetched with a plain request (login wall, WAF refusal, terms of use), it is not worked around: it is recorded as a request for the owner. | §13.3 rewritten from "data requests for the owner" to "data needs". |
| OD19 | **DECIDED: website slice W.** Rework the pkgdown site after S5 and refresh it at 0.6.0 and 0.7.0; built locally, not deployed. | §9.1 (site design), §10, §11 (slice W). |

Unchanged binding rules: all 14 current exports keep working; `get_qes()`/`get_qes_master()` keep their signatures; `assign_global` defaults to FALSE and data is always returned visibly; the 11 old helpers are soft-deprecated wrappers kept indefinitely.

---

## 1. Principles and binding constraints

### 1.1 Design rules

| # | Rule | Prevents |
|---|---|---|
| P1 | Every data read is identified by `(file_id, md5)` and checked against the catalog's `n_rows`/`n_cols`. A mismatch is an error, never a fallback. | [A:D2], [A:D3], [A:D5], [A:D9], [A:D10] |
| P2 | Nothing is harmonized unless a reviewed spec row says so. There is no name matching, case-insensitive fallback, regex on labels, `eval()` or pass-through of unmapped codes. | [A:H1], [A:H2], [A:H4], [A:H7] |
| P3 | One target is one stimulus. A different wording, scale, timing, election reference or push/lean format makes a new target. A target name never changes meaning: it is retired and replaced. | [A:H3], [A:H4] pooling |
| P4 | The locale changes what qesR *says* (conditions), never what it *returns*. Returned text depends only on arguments. | [A:D10], locale drift |
| P5 | Rows are never dropped, whether by deduplication or empty-row removal. IDs are declared, composite where needed, and asserted unique. | [A:H5] |
| P6 | Every NA carries a reason from a closed vocabulary. | silent missingness |
| P7 | Never branch on error or warning text; branch on condition class or HTTP status. | [A:D10], [A:D7] |
| P8 | qesR never calls `readRDS()`/`load()` on downloaded or cached content, never disables TLS, and never shells out. | [A:D1] |
| P9 | v0.4.4 semantics are kept unless a verified bug fix changes them, and every such change gets a NEWS line. | constraint 1, 8 |
| P10 | One implementation per concern: one reader, one CSV loader, one validator (run in four places), one message table, one condition tree, one enum file. | drift |

### 1.2 Binding constraints and how each is met

| # | Constraint | Mechanism |
|---|---|---|
| 1 | All 14 exports keep working; `get_qes()`/`get_qes_master()` keep their signatures; old names become soft-deprecated, kept indefinitely, documented in NEWS | • A `formals()` snapshot test covers all 14 exports against d1faad6, with an explicit allowed-diff table (§8.1): `assign_global` TRUE → FALSE in `get_qes`, `get_qes_master` and `get_decon` only (the codebook functions were already FALSE), plus `variables` and `lang` appended to `qes_codebook()`.<br>• The 11 legacy wrappers call internal implementations (§2.3) and emit `qesR_message_deprecated` once per session, in EN/FR, with no removal date.<br>• A registry `.qes_deprecated` drives both the messages and the contract test.<br>• NEWS has a section "Soft-deprecated names (kept indefinitely)". |
| 2 | `assign_global = FALSE`; visible return; explicit opt-in | • `return(data)`, with an `expect_visible()` test.<br>• Each canonical export is a thin wrapper `f <- function(<frozen args>) .f_impl(<args>, envir = parent.frame())`, and legacy wrappers call `.f_impl()` directly (§2.3), so opt-in assignment lands in the frame of whoever called the exported name.<br>• Attributes such as `saved_to` are set before assignment.<br>• A body scan of the installed namespace forbids `.GlobalEnv` (§8.1). |
| 3 | CRAN: no writes outside `tempdir()` by default; opt-in `R_user_dir` cache; guarded network code; offline tests; 0/0/1 | • The session cache lives in `tempdir()`; the disk cache is used only by option.<br>• Every Rd example and CRAN vignette runs offline on `qes_demo` and shipped metadata. `--as-cran` runs `\donttest{}`, so the package has exactly **one** network example, on the `qes_studies()` page: `qes_studies(check_updates = TRUE)`, a metadata-only call, inside `if (curl::has_internet())` and `tryCatch(qesR_error_network = ...)`.<br>• Tests use `skip_on_cran()` + `skip_if_offline()` + `QESR_LIVE`.<br>• A test confirms that `globalenv()`, `getwd()`, `~` and `R_user_dir` are unchanged, using the `.qes_catalog()` fixture seam (§8.1). |
| 4 | Light dependencies | • Imports: `curl`, `haven`, `jsonlite`, which is still three. In S2a `curl` comes in; `xml2` goes out in S3, when the offline codebook replaces the DDI path (§4.1).<br>• Specs are read with `utils::read.csv`/`read.dcf`, and md5 comes from `tools::md5sum`.<br>• Suggests: `testthat (>= 3.1.7)`, `withr`, `knitr`, `rmarkdown`, `survey`, `srvyr`.<br>• There is no tibble, yaml, rlang, cli, lifecycle, httr2, webfakes or dplyr. |
| 5 | Bilingual EN/FR docs in sync; bilingual-aware messages | • One message table with a key-parity test.<br>• Target, level and grade-reason text is EN/FR in the spec, and validator V-S6 requires both.<br>• The harmonization reference is generated in both languages from one source.<br>• Hand-written vignette pairs share identical code chunks, checked in CI (vignette sources are not installed, so this cannot run under `R CMD check`).<br>• `?qesR-fr` is the French entry point (§9). |
| 6 | No insecure TLS; never `readRDS` remote files | • The TLS fallback and shell-out are deleted in S0c ([A:D1]).<br>• The affine grammar is one regex that is never evaluated.<br>• Every file is md5-verified before use.<br>• Forbidden calls (`ssl_verifypeer`, `insecure`, `readRDS`, `load`, `system2`, `eval`, `parse`) are checked by a body scan of the installed namespace on CRAN and by a source grep in CI. |
| 7 | Licence: no derived microdata rows; 2022 is CC BY-NC; a deposit needs permission | • `inst/extdata/qes_master.csv` is deleted.<br>• The demo and all fixtures are synthetic.<br>• qes2022 ships no dictionary rows, no label text and no aggregates (label hashes only), and a test enforces this.<br>• A one-time `qesR_message_licence` is shown per study.<br>• `inst/COPYRIGHTS` is added.<br>• The deposit is conditional slice HZ9. |
| 8 | Reproducibility: versioned results | • Pinned `file_id` + md5 + dataset version.<br>• `VERSIONS` covers the catalog and dictionary.<br>• `SPEC` has semver plus a content hash, and CI enforces the bump rule.<br>• `qes_provenance()` and `qes_cite()`.<br>• A frozen spec can be passed as `spec = "<dir>"`.<br>• v0.4.4 is reproduced only by pinning d1faad6 (§5.11). |
| 9 | Politeness to Dataverse; no personal information | • One request per cold study (46 for a master build today, 11 after).<br>• Retries with backoff and `Retry-After`, sequential requests, at least 1 s per host.<br>• The User-Agent is exactly `qesR/<ver> R/<ver>`, with no e-mail and no GitHub URL (the repository path contains the owner's name).<br>• The URL builder takes only catalog fields, and a test checks that (§4.6). |

---

## 2. Public API

### 2.1 Vocabulary

| Name | Meaning | Where |
|---|---|---|
| `studies` | A vector of catalog codes. Trimmed and case-insensitive, never fuzzy; an unknown code raises an error that suggests near matches. `"2018"` is never auto-resolved. `"all"` is valid only on its own, and mixing it with codes is an error. | every new function that accepts several studies |
| `srvy`, `surveys` | The same meaning under the legacy names (`srvy` is a single code) | `get_qes`, `qes_codebook` (frozen position 1), `get_qes_master` |
| `x` | A study code, or an object carrying `qes_provenance` | `qes_question`, `qes_missing`, `qes_provenance`, `qes_cite`, `qes_design` (and the internal `.qes_splice`, `.qes_join_raw`) |
| `variables` | Raw variable names, exact match | metadata functions, `qes_missing` |
| `targets` | Harmonized target, family or set names, exact match. The three name spaces are disjoint (validator V-S15). | harmonization functions |
| `lang` | Language of **returned text**, with a fixed default. It never follows the locale or an option. For raw metadata (`qes_codebook`, `qes_question`, `qes_docs`), `lang = NULL` means the study's source language. For harmonized output and spec views, the default is `"en"`, because the spec always has both languages. | everywhere text is returned |
| `path` | An existing, user-chosen directory, with no default | `qes_download` |
| `quiet` | Suppresses progress and informational `qesR_message_*` output. It never suppresses warnings, errors, the deprecation notice (controlled only by `qesR.quiet_deprecated`) or the licence notice. | every function that prints or touches the network |

**Companion columns** use a double-underscore suffix: `<target>__na`, `<target>__src`, `<into>__target`, `<into>__grade`, and `<study>__<var>` for joined raw columns. Target, family and set names match `^[a-z][a-z0-9]*(_[a-z0-9]+)*$`, so they can never contain `__`, and no target name equals a study code (V-S15).

**Argument order:**
1. the primary input;
2. `variables`/`targets`;
3. the other arguments that select or shape data;
4. `lang`;
5. `quiet`.

`lang` and `quiet` are always passed by name in docs and examples.

**Naming rule:** everything new is `qes_<noun>` or `qes_<verb>`. A test checks that every export outside the frozen legacy list matches `^qes_[a-z_]+$`.

### 2.2 New exports (13)

Revised by OD17: the three spec views are one function, and splice/join are internal.

| Family | Signature | Returns | Purpose |
|---|---|---|---|
| Discovery | `qes_studies(family = NULL, check_updates = FALSE, quiet = FALSE)` | data.frame: every `studies.csv` column, plus `doi_url` and `waves` (NA until HZ4). With `check_updates = TRUE` it adds `latest_version`, `latest_md5` and `status` (`current`, `new_version_same_file`, `data_changed`, `deaccessioned`, `unreachable`) | Offline catalog. With `check_updates = TRUE` it makes one metadata request per study; a network failure becomes `status = "unreachable"`, never an error. `qes_demo` is not listed. |
| Discovery | `qes_search(pattern, studies = NULL, fields = c("variable","label","question","values","target"), regex = FALSE, lang = c("both","en","fr"))` | data.frame: `study, year, variable, label, question, question_lang, values, targets, matched_in` (shaped like `labelled::look_for`) | Offline search over the shipped dictionary plus any cached 2022 shard. Case- and accent-insensitive (§6.3). A footer names studies that are not searchable yet. `targets` is NA until HZ3. |
| Data | `qes_missing(x, variables = NULL, action = c("na","tagged"), types = NULL, quiet = FALSE)` | `x` with codes recoded; `attr(, "qes_missing_log")` = `variable, value, missing_type, n_set` | Types or blanks DK/refused and declared SPSS user-missing codes in raw `get_qes()` data.<br>• `types = NULL` means every type except `spoiled`, `not_selected`, `not_voted` and `not_registered`.<br>• Variables with no curated types are left untouched, and one message counts them. |
| Data | `qes_download(studies, path, what = c("data","docs"), role = NULL, version = c("pinned","latest"), overwrite = FALSE, lang = NULL, quiet = FALSE)` | invisible data.frame: `study, file_id, file_name, role, lang, md5, local_path, from_cache, downloaded` | Writes the original bytes into `path`, md5-verified before the final rename.<br>• `version = "latest"` is the only unpinned route: it raises `qesR_warning_unpinned` and records `pinned = FALSE`.<br>• Nothing is written unless at least one file matches. |
| Metadata | `qes_question(x, variables, lang = NULL)` | data.frame: `study, variable, question, question_lang, truncated, source, doc_ref, universe` | Exact wording. Truncation at 80 characters in the source is flagged rather than guessed ([A:K3]). |
| Metadata | `qes_docs(studies = NULL, role = NULL, lang = NULL)` | data.frame: `study, file_id, file_name, role, lang, format, bytes, md5, url` | Offline list of codebooks, questionnaires and reports, with curated roles ([A:K2]). Fetching goes through `qes_download(what = "docs")`. |
| Harmonization | `qes_harmonize(studies = NULL, targets = "core", layout = c("respondent","long"), values = c("factor","labelled","code"), missing = c("na","reasons"), min_grade = c("approximate","comparable","identical"), weights = c("normalized","raw"), unmapped = c("error","warn","na"), on_fail = c("stop","skip"), keep_source = FALSE, include_draft = FALSE, data = NULL, spec = NULL, lang = c("en","fr"), quiet = FALSE)` | data.frame of class `c("qes_harmonized","data.frame")`, returned visibly; details in §5.8 | The engine.<br>• `studies = NULL` means the QES election studies with coverage, and `"all"` means everything except `qes_demo`.<br>• `data` is a **named** list `list(<study> = data.frame)` of `get_qes()`-shaped frames. Unnamed input is an error. Waves come from `waves.csv` membership rules applied to each frame. |
| Harmonization | `qes_spec(view = c("targets","crosswalk","spec"), targets = NULL, studies = NULL, level = c("row","code"), format = c("qesR","retroharmonize"), spec = NULL, validate = c("error","report","none"), data = NULL, lang = c("en","fr"))` | By `view`:<br>• `"targets"` (default): data.frame, one row per target (`target, family, type, target_timing, label, definition, levels, status, added_in`), plus **one column per study** holding the best grade or `NA`.<br>• `"crosswalk"`: data.frame of spec rows joined to wording, grade, reason, gate, offered levels and weight; `print()` on a single target renders the generated reference section.<br>• `"spec"`: object of class `qes_spec` (list of tables, `version`, `hash`); `attr(, "check")` holds the problems table. | The one metadata entry point for the harmonization spec.<br>• `targets`/`studies` filter the `"targets"` and `"crosswalk"` views.<br>• `level` and `format` apply only to `view = "crosswalk"`; the `retroharmonize` format uses its column names (`var_name_orig`, `val_numeric_orig`, ...) with no dependency. `data` applies only to `view = "spec"` and adds the data checks. Supplying any of them with another view is a `qesR_error_input`.<br>• `spec = NULL` is the shipped spec; a directory (frozen or user-extended) or a `qes_spec` object selects another. Every view loads and validates the spec once per session (§5.10); `validate` says what a problem does.<br>• `qes_harmonize(spec =)` accepts the same values. |
| Harmonization | `qes_design(x, weight = NULL, engine = c("survey","srvyr"), pool = c("as_is","equal"))` | `survey.design2` or `tbl_svy` | Suggests only. Details in §5.3 ("Choosing a weight").<br>• `strata = study`, or study × wave in the long layout.<br>• `ids = ~qes_id` in the long layout, else `~1`.<br>• `pool = "equal"` is the **only** way to give each study × wave the same total. |
| Reproducibility | `qes_provenance(x, level = c("study","cell","spec"))` | data.frame (§5.9). `print()` gives a citable paragraph | Accepts objects or study codes; for codes it shows what would be used. Raises `qesR_error_no_provenance` after `merge()` drops the attribute. |
| Reproducibility | `qes_cite(x = NULL, style = c("text","bibtex","bibentry"), lang = "en")` | character or `bibentry` | With `NULL`, it cites qesR and the spec version. With codes or objects, it adds each dataset: the verbatim deposit title, authors, pinned version, UNF and doi.org link. `inst/CITATION` is a **static** `bibentry()` generated from the same builder in `data-raw/` (§9). |
| Cache | `qes_cache_info()` | data.frame: `study, file_id, md5, bytes, retrieved, kind (file/shard), path`; attributes `mode` and `dir` | Inspect the cache. |
| Cache | `qes_cache_clear(studies = NULL, older_than = NULL)` | invisible removed paths | Refuses any root without the `.qesR-cache` marker. It also clears the in-session memo. |

**Canonical surface: 16 functions.** These are the 13 above, plus `get_qes()`, `get_qes_master()` and `qes_codebook()`. The 11 legacy wrappers bring the total to **27 exports** (the 14 current exports keep working: 3 are canonical, 11 are legacy wrappers).

**Internal for now (OD17).** Two harmonization helpers are implemented and tested but not exported, and carry `@keywords internal` with `@noRd`:
- `.qes_splice(x, family, into = family, prefer = NULL)` adds `<into>`, `<into>__target` and `<into>__grade`, pooling across targets of one family. It refuses different level sets, and it warns and lists wording breaks.
- `.qes_join_raw(x, vars, data = NULL)` adds `<study>__<var>` columns in native codes (NA for other studies), matched by `(study, source_row)`. It replaces [A:H1] stacking.

Exporting either later is a MINOR change that needs no redesign: the signatures above are kept, and the naming test already allows `qes_[a-z_]+`. Until then, users pool across wordings by hand from the side-by-side targets, and join raw items with `merge()` on `study` and `source_row`, which the docs show.

**Rd pages (28):**
- **the 16 canonical functions;**
- **9 legacy pages**, with `@family legacy`. Wrappers share a page only when their arguments mean the same thing:
  - `get_codebook` + `get_qes_codebook`;
  - `get_codebook_files` + `get_qes_codebook_files`;
  - `format_codebook`, `get_value_labels`, `get_question`, `download_codebook`, `get_preview` and `get_qescodes`, one page each;
  - `get_decon` on its own page, keeping its 19-column dictionary;
- **`qesR-deprecated`:** the old-to-new table and message policy;
- **`qesR-package`:** options, conditions and `qes_demo`, with `\alias{qesR}`;
- **`qesR-fr`:** a French overview with a function table. A test checks that every canonical export is mentioned on it.

### 2.3 Back-compat map for all 14 current exports

All `formals()` match d1faad6 except for the allowed differences listed in §1.2, constraint 1.

**Frame rule (fixes [A:A1]).** Every export that can assign is a thin wrapper:

```r
get_qes <- function(srvy, file = NULL, assign_global = FALSE, with_codebook = TRUE, quiet = FALSE)
  .get_qes_impl(srvy, file, assign_global, with_codebook, quiet,
                envir = parent.frame(), assign_missing = missing(assign_global))
```

- Legacy wrappers call `.<canonical>_impl()` with their own `parent.frame()`, never the exported canonical function, whose `parent.frame()` would be the wrapper's frame.
- `get_qes_master()` calls `.get_qes_impl()` internally with `assign_missing = FALSE`, so the assignment-default notice never fires from inside it.
- Contract test: opt-in assignment through every entry point, called both at top level and inside a user function, lands in that caller's frame with `identical(assigned, returned)`.

**Opt-in assignment in `get_qes()` assigns two objects**, as v0.4.4 did (R/get_qes.R:39-41):
- `<code>` holds the data;
- `<code>_codebook` holds the codebook, when `with_codebook = TRUE`.

`<code>` is the **canonical** code (`get_qes(" QES2018 ", assign_global = TRUE)` assigns `qes2018`), and `attr(, "qes_survey_code")` stores the canonical code too. In v0.4.4 it stored the raw input, so this gets a NEWS line.

**Status of each export.** "0.5.0" is what ships in the data-lane release. "Later" is the engine-based end state (0.7.0 unless noted). A legacy wrapper's deprecation message starts in the release that ships its replacement.

| v0.4.4 export | Status | Implemented as | Behaviour change in 0.5.0 (NEWS) | Change later |
|---|---|---|---|---|
| `get_qes(srvy, file = NULL, assign_global = FALSE, with_codebook = TRUE, quiet = FALSE)` | **canonical, stable** | reader pipeline (§4) | • `assign_global` default was TRUE, and opt-in now assigns into the calling environment.<br>• Data comes from originals: U+FFFD cells are gone and qes2022 dates are POSIXct.<br>• Codes, names and `is.na()` counts are unchanged (§2.4).<br>• `file` matches data files only, and an ambiguous match is an error ([A:D6]).<br>• A one-time message explains the assignment default, only when `assign_global` was not supplied. | — |
| `get_qes_master(surveys = NULL, assign_global = FALSE, object_name = "qes_master", quiet = FALSE, strict = FALSE, save_path = NULL)` | **canonical, stable** (OD1) | interim master (§5.12) | • `assign_global` default and frame, as above.<br>• The 63 [A:H1] columns are removed.<br>• There is no dedup and no empty-row removal.<br>• Blanking list of §5.12 (conditional on OD4/OD5).<br>• `save_path` writes UTF-8, plus `<stem>_provenance.csv`; the `_variable_name_map.csv` sidecar is gone. | 0.7.0: engine plus the `legacy.csv` renderer; recall semantics everywhere; 1998 recall filled |
| `qes_codebook(srvy, file, assign_global = FALSE, quiet, refresh, layout, variables = NULL, lang = NULL)` | **canonical** (promoted) | §6 | • Offline and metadata-only ([A:D5]).<br>• Attributes are kept ([A:K1]) and columns are appended.<br>• `srvy` also accepts a data.frame or a `qes_codebook` (precedence in §6.2).<br>• `refresh` is a no-op, with a once-per-session `qesR_message_arg_ignored`.<br>• `file` selects the data file whose metadata is described (matching `get_qes(file =)`). | — |
| `get_codebook`, `get_qes_codebook` | soft-deprecated (0.5.0) | `.qes_codebook_impl()` | as `qes_codebook`; opt-in assignment now works through the wrapper ([A:A1]) | — |
| `format_codebook(codebook, layout)` | soft-deprecated (0.5.0) | `.qes_codebook_impl(codebook, layout =)` | • A plain data.frame is an error that names the fix, e.g. a codebook read back from CSV, which has lost its class ([A:A5]).<br>• The long layout keeps unlabelled variables ([A:K7]). | — |
| `get_value_labels(codebook, variable = NULL, long = FALSE)` | soft-deprecated (0.5.0) | legacy adapter over the long codebook, reshaped to the legacy list or data.frame | • An unknown variable is an error instead of `named list()` ([A:K6]).<br>• `''` labels are preserved. | — |
| `get_question(do, q, full = TRUE)` | soft-deprecated (0.5.0) | legacy adapter over `qes_question()`, returning `character(1)` | • Exact match ([A:A6]); truncation is flagged.<br>• `full = FALSE` returns the same full text as `TRUE` and emits a once-per-session `qesR_message_arg_ignored`, because it changes what is returned.<br>• A character `do` is looked up in the caller's frame, read only. | — |
| `get_codebook_files`, `get_qes_codebook_files` | soft-deprecated (0.5.0) | legacy adapter over `qes_docs()`, with columns remapped to `file_id, filename, extension, size, download_url` | They now return the documentation files ([A:K1], [A:K2]). | — |
| `download_codebook(srvy, dest_dir = tempdir(), file, quiet, refresh, overwrite)` | soft-deprecated (0.5.0) | legacy adapter over `qes_download(what = "docs", path = dest_dir)` | • Files are md5-verified.<br>• `file` is now a regex over **document** file names; in v0.4.4 it selected the data file ([A:A4]).<br>• `refresh` is a no-op with a message.<br>• `dest_dir` is created only on this legacy path, and only when files exist. | — |
| `get_preview(srvy, obs = 6L, file = NULL)` | soft-deprecated (0.5.0) | `head(.get_qes_impl(srvy, file, quiet = TRUE), obs)` | • `obs` must be a whole number ≥ 1.<br>• Data is served from the memo or cache. | — |
| `get_decon(srvy = "qes2022", assign_global = FALSE, quiet = FALSE)` | **stable in 0.5.0**; soft-deprecated from 0.7.0 | 0.5.0: interim (§5.12). 0.7.0: `qes_harmonize(srvy, targets = "decon")` plus the `decon` legacy profile (same 19 names) | • `assign_global` default and frame.<br>• Deletions and blanks only: 2022 −99 is blanked, not leaked (ideology mean about 4.96, not 2.64).<br>• 2018 `turnout`/`votechoice` (wrong source, [A:H6]) are blanked.<br>• `partylean`/`party_best` become NA columns with a message.<br>• OD9 applies. | 0.7.0: 2018 turnout from `q5` and vote from `q6`; 1998 returns real rows |
| `get_qescodes(detailed = FALSE)` | soft-deprecated (0.5.0) | legacy adapter over `qes_studies()`, with columns mapped to the legacy set | • Accented `name_fr`.<br>• `documentation` becomes a doi.org URL.<br>• The new 1998 codes are appended after the old 11, so `index` values stay stable. | — |

**Deprecation message.** It has class `qesR_message_deprecated`.
- It is shown once per session per function, and silenced only by `options(qesR.quiet_deprecated = TRUE)`; `quiet = TRUE` does not silence it.
- It is a message rather than `.Deprecated()`: a warning would break `expect_no_warning()`, clutter knitr output and become an error under `warn = 2`.
- Once-per-session flags live in one internal environment `.qes_once`, and the unexported `.qes_reset_once()` resets them for tests (§8.1).
- EN: "`get_codebook()` is soft-deprecated; use `qes_codebook()`. It keeps working and will not be removed."
- FR: "`get_codebook()` est obsolète (dépréciation douce) ; utilisez `qes_codebook()`. Elle continue de fonctionner et ne sera pas retirée."

### 2.4 Guarantees for `get_qes()` and `get_qes_master()`

**`get_qes()`**
1. The signature is frozen exactly: no trailing arguments and no `...`.
2. It always returns a base `data.frame`, visibly.
3. For every study, column names, raw codes and `is.na()` counts equal v0.4.4:
   - `.qes_unspss()` converts `haven_labelled_spss` columns to `haven_labelled`, keeping the codes and storing declared missing codes in `qes_na_values`/`qes_na_range`. Verified: 52/52 user-missing columns in qes2007_panel and 30/30 in qes2018_panel equal the v0.4.4 `.tab` values, and the qes2018_panel NA count is identical (13,293).
   - `name_map.csv` restores the three qes2007_panel names that the original `.sav` spells differently: `AffGénérale`→`AFFGÉN`, `propriété`→`PROPRIÉ`, `PropGénérale`→`PROPGÉN`.
   - The expected deviations are listed individually in NEWS: label text, date types (qes2022), and storage type.
   - This is verifiable now for the 6 cached studies. For qes2007, qes2008, qes2012_panel, CROP and qes1998 it needs R1 and R2, and 0.5.0 waits for that check (OD2).
4. It never harmonizes, recodes or drops a row.
5. Attributes: `qes_survey_code` (canonical code), `qes_codebook` (legacy; built offline when `with_codebook = TRUE`) and `qes_provenance`.
6. It always reads the pinned version. There is deliberately no `version` or `variables` argument; the unpinned route is `qes_download(version = "latest")`.
7. Opt-in assignment assigns `<code>` and `<code>_codebook` into the calling environment (§2.3).

**`get_qes_master()`**
1. The signature is frozen.
2. It returns visibly, and assigns into the caller's frame only on opt-in, after `saved_to` is set.
3. The 30 documented legacy columns always exist, in the v0.4.4 order and with the v0.4.4 types. A column with no valid source is all-NA and listed in `attr(, "legacy_na_columns")`. New columns are appended at the end, so positional code keeps working, and appended columns are never removed in a later release.
4. It keeps every source row: no dedup and no empty-row removal. `duplicates_removed = 0L`, `empty_rows_removed = 0L`, and `nrow == sum(n_rows)` is asserted per study.
5. It keeps all v0.4.4 attribute names (§5.12). `variable_name_map_path` is kept as `NULL`. It adds `qes_provenance`, `qes_spec`, `legacy_column_map` and `removed_columns`.
6. `save_path` writes one `.rds` (written, never read) or one UTF-8 CSV, plus `<stem>_provenance.csv`.
7. `strict = TRUE` means `on_fail = "stop"`. Failures carry root causes in `failed_surveys`.

---

## 3. Study catalog

### 3.1 Source of truth and files

`inst/extdata/catalog/` holds UTF-8, LF-terminated CSV files. They are hand-edited or produced by `data-raw/build_catalog.R` from the Dataverse JSON, and reviewed in PR diffs. `R/qes_catalog.R`'s hard-coded data.frame is deleted, and existing codes are frozen.

| File | Key | Purpose |
|---|---|---|
| `studies.csv` | `study` | One row per study |
| `files.csv` | `(study, file_id)` | Every referenced Dataverse file: data, label donor and documents |
| `elections.csv` | `election_id` | Public election dates (QC1994…QC2022, CA2006…), used for `election_date` and `days_to_election` |
| `name_map.csv` | `(file_id, source_name)` | Renames that restore v0.4.4 names |
| `enums.csv` | `(enum, value)` | **Every closed vocabulary in the package**: study and wave design, timings, roles, label sources, NA reasons, grades and rules, each with EN/FR labels. Validator V-S3 and the catalog lints read it (P10). |

`inst/extdata/VERSIONS` (DCF) holds:
- `catalog_version` and `dict_version` (semver: MAJOR when a pin changes, MINOR when rows are added, PATCH for text);
- `schema_version`;
- `built_from` (the `file_id:md5` list);
- `csv_md5` (the md5 of every shipped catalog and dictionary CSV).

A test recomputes `csv_md5` and compares `built_from` with `files.csv`.

**Access seam.** `.qes_catalog()` is the only function that reads the catalog. It is the second documented test seam, after `.qes_transport()`. Tests replace it with `local_mocked_bindings()` to serve a fixture catalog whose md5s match the synthetic fixtures (§8.1).

**`qes_demo`.** It lives in a parallel tree, `inst/extdata/demo/{catalog,harmonize,data}`:
- `.qes_catalog()` and `qes_spec()` merge that tree only when the code `qes_demo` is requested;
- it is excluded from the SPEC hash, from V-P1/V-P2 and from `qes_studies()`;
- it has its own lint.

The demo data is a small synthetic `.sav` whose variable names are a subset of real qes2014 names, so the legacy master can also run on it (§8.1).

### 3.2 `studies.csv` schema

The columns are grouped by role:
- **Identification:** `study`, `aliases` (`;`-list), `family` (`qes`, `durand_panel`, `crop_polls`, `polls_1998`), `title_deposit` (verbatim), `title_en`, `title_fr`, `authors` (`;`-list).
- **Timing and design:** `year`, `year_end`, `election_id` (→ `elections.csv`), `study_design` (`post`, `pre_post`, `panel`, `pooled_polls`, `pre`), `default_member` (TRUE for QES election studies).
- **Population:** `target_population_en`, `target_population_fr`, the default for its waves (e.g. "QC citizens 18+", "QC residents 16+", "QC francophones 18+").
- **Access:** `server`, `doi`, `dataset_version`, `data_file_id`, `label_file_id`.
- **Language:** `source_lang`.
- **Licensing:** `licence`, `licence_url`, `metadata_shipped` (derived from the licence; a test enforces it).
- **Notes:** `notes_en`, `notes_fr`.

Waves are not stored here. `qes_studies()` derives its `waves` column from `harmonize/waves.csv` (§5.3); until HZ4 that column is NA, and a test checks this.

Verified example rows (population column abbreviated):

```csv
study,family,year,election_id,study_design,default_member,server,doi,dataset_version,data_file_id,label_file_id,source_lang,licence,metadata_shipped,target_population_en
qes2022,qes,2022,QC2022,pre_post,TRUE,https://dataverse.harvard.edu,10.7910/DVN/PAQBDR,1.1,7449513,,en,CC BY-NC 4.0,FALSE,QC citizens 18+
qes2018,qes,2018,QC2018,post,TRUE,https://borealisdata.ca,10.5683/SP3/NWTGWS,1.0,425914,,fr,CC0 1.0,TRUE,QC residents 16+
qes2014,qes,2014,QC2014,post,TRUE,https://borealisdata.ca,10.5683/SP3/64F7WR,1.0,425916,,fr,CC0 1.0,TRUE,
qes2012,qes,2012,QC2012,post,TRUE,https://borealisdata.ca,10.5683/SP2/WXUPXT,1.0,425918,425917,en,CC0 1.0,TRUE,
qes2007_panel,durand_panel,2007,QC2007,panel,FALSE,https://borealisdata.ca,10.5683/SP3/NDS6VT,1.0,352415,,fr,CC0 1.0,TRUE,
qes2018_panel,durand_panel,2018,QC2018,panel,FALSE,https://borealisdata.ca,10.5683/SP3/XDDMMR,1.0,333052,,fr,CC0 1.0,TRUE,
qes1998,polls_1998,1998,QC1998,panel,FALSE,https://borealisdata.ca,10.5683/SP2/QFUAWG,1.0,329987,,fr,CC0 1.0,TRUE,QC francophones 18+
qes1998_crop,polls_1998,1998,QC1998,pooled_polls,FALSE,https://borealisdata.ca,10.5683/SP2/QFUAWG,1.0,286331,,fr,CC0 1.0,TRUE,
```

**Notes on these rows:**
- The `post` design for 2012, 2014 and 2018 is to be confirmed from the technical reports already in `codebooks/`; no data request is needed.
- Durand panels and CROP get corrected display names ([A:C2]).
- **qes1998 is francophones only** (codebook: "il a été décidé de retenir uniquement les francophones"). It pools two firms, CREATEC (1,057) and CROP (426). Its notes say so, and its waves carry `firm` as a stratum (§5.3).

### 3.3 `files.csv` schema

Columns:
- `study`, `file_id`, `role` (`data`, `label_donor`, `codebook`, `questionnaire`, `technical_report`, `methodology`), `lang`, `file_name`, `original_file_name`;
- `format` (`sav`, `zsav`, `por`, `dta`, `pdf`, `doc`, `docx`), `ingested`, `bytes`, `md5`, `checksum_type`, `unf`;
- `n_rows`, `n_cols`, `encoding`;
- `id_vars` (`;`-list; `.row` = position in the pinned file), `is_default`, `dataset_version`.

`bytes` and `md5` describe the **original** file. Dataverse's `md5` field equals the md5 of the `?format=original` bytes for all 8 cached originals (verified).

```csv
study,file_id,role,lang,original_file_name,format,bytes,md5,unf,n_rows,n_cols,id_vars,is_default
qes2022,7449513,data,,2022 Quebec Election Study v1.dta,dta,2839434,c51bafed57776ffa8d4c6f301f5c945b,UNF:6:I/DFDdqJv7wNEoyyRdxaIw==,1521,718,ResponseId,TRUE
qes2022,7449514,codebook,en,2022 Quebec Election Study Codebook v1.pdf,pdf,663655,42ad4bbe6eb0f8b4c7522ad376834334,,,,,FALSE
qes2018,425914,data,,Quebec Election Study 2018.dta,dta,6332459,d24f5b0be727d688ad305b8eb61f0d30,UNF:6:luhys2QSLNTONPOXO4LYpg==,3072,254,responseid,TRUE
qes2018,361045,methodology,fr,Rapport méthodologique de l'Étude électorale québécoise 2018,pdf,651382,3e17e3a5266e1f79c4cca1549b56c63c,,,,,FALSE
qes2012,425918,data,,Quebec Election Study 2012 (STATA).dta,dta,1131600,e5ec063d9b1b03b48f458be4d8ec3d7b,UNF:6:FtkoQZqZSTVu2BBxD5ufRg==,1505,177,quest,TRUE
qes2012,425917,label_donor,,Quebec Election Study 2012 (SPSS).sav,sav,384599,02f304b9bec665c95b18da25a634baa5,UNF:6:FtkoQZqZSTVu2BBxD5ufRg==,1505,177,QUEST,FALSE
qes2014,425916,data,,(SPSS twin),sav,,,,1517,140,.row,TRUE
qes2007_panel,352415,data,,complet_tous_repondants_2007.sav,sav,1599467,20f8c6fd4211744340f76293d18e5b6d,UNF:6:ASjoqrxxkLm0vvSA6lc9Fw==,2442,270,nompn;quest,TRUE
qes2018_panel,333052,data,,IPsos_oct_2018_17-057727_V.SAV,sav,,b6fc93f2918ae4a56de3d19e80d0bd15,,1250,72,method;id,TRUE
```

Empty cells are filled at slice S1 from the cached originals and JSON. The qes2022 key is `ResponseId` (0 duplicates, verified); `cps_ResponseId` in one proposal was wrong. qes2014 `QUEST` is constant, so its key is `.row`, which is stable because the file is md5-pinned.

### 3.4 Lints (offline tests)

- schemas, unique keys and foreign keys, and every enum column against `enums.csv`;
- exactly one default data file per study;
- md5 format;
- valid UTF-8 with no U+FFFD or C1 characters;
- `metadata_shipped = FALSE` ⇒ 0 dictionary rows;
- every `name_map` target exists in the v0.4.4 name manifest;
- every `id_vars` column exists in the dictionary.

---

## 4. Data access layer

### 4.1 HTTP (`R/http.R`, curl)

**Why curl.** `curl` gives the status, headers (`Retry-After`, `x-amzn-waf-action`) and body from one request. It has per-transfer stall timeouts, no shell-out and 0 R dependencies. The reason is recorded in `cran-comments.md`.

**Swap timing.** `curl` comes in at S2a. `xml2` leaves at **S3**, not S2: until the offline codebook lands, the DDI path in `R/qes_download.R` (all 12 `xml2::` calls) still builds `get_qes(with_codebook = TRUE)`'s `qes_codebook` attribute and the `get_codebook*` output. None of the 11 cached DDIs has a `qstnLit` element, so after S3 xml2 has no runtime job and moves to `data-raw/` only.

`jsonlite` stays in Imports, only for `check_updates` and `version = "latest"`; the one-line reason goes in `cran-comments.md`.

- **The only network seam:** `.qes_transport(url, dest, handle)`, wrapping `curl::curl_fetch_disk`/`curl_fetch_memory`.
- **Handle:**
  - `useragent = sprintf("qesR/%s R/%s", packageVersion("qesR"), getRversion())`;
  - `followlocation = TRUE`, `maxredirs = 5` (Borealis answers with a 303 to S3);
  - `connecttimeout = 30`, `low_speed_limit = 1024`, `low_speed_time = getOption("qesR.stall_timeout", 60)`;
  - TLS options are never touched.
- **Retries:**
  - retried: 408, 429, 500, 502, 503 and 504, plus curl transport errors (identified by class), up to `qesR.max_tries = 4` attempts;
  - wait: `Retry-After` (seconds or HTTP date), capped at 120 s, and a longer value is an error that says so; otherwise `min(60, 2^k) * runif(1, 0.5, 1.5)`;
  - other 4xx statuses fail at once. A 404 suggests `qes_studies(check_updates = TRUE)`.
- **Harvard WAF:** a 202 with `x-amzn-waf-action`, or a 403, raises `qesR_error_http_refused`. There is no retry and no bypass. The message explains the manual route: download the file in a browser and drop it into the cache as `<file_id>-<md5>.<ext>`, or set `qesR.cache_dir`. The file is accepted after md5 verification.
- **Politeness:** requests are sequential and at least 1 s apart per host. The root cause is kept in `parent`.
- **Progress:** one `message()` per download, silenced by `quiet`. There is no progress bar.
- **Endpoints (only these three):**
  - `{server}/api/access/datafile/{id}?format=original` for ingested data;
  - the same URL without `format` for documents;
  - `{server}/api/datasets/:persistentId/versions/{v}?persistentId=doi:{doi}`, used only for `check_updates` and `version = "latest"`, and memoized.

  URLs are built by `.qes_url(server, kind, file_id, doi, version)`, whose inputs are catalog fields only. DDI endpoints are used only in `data-raw/`.

### 4.2 Cache (`R/cache.R`)

| Mode (`qesR.cache` / `QESR_CACHE`) | Root |
|---|---|
| `"session"` (default) | `file.path(tempdir(), "qesR")` |
| `"disk"` (opt-in) | `tools::R_user_dir("qesR", "cache")`, created only on opt-in, with a message naming it |
| `"none"` | a per-call `tempfile()` |
| `qesR.cache_dir` / `QESR_CACHE_DIR` | an existing directory chosen by the user; implies disk mode. qesR always creates, marks and uses a `qesR/` **subdirectory** inside it, never the directory itself |

- **Marker:** each root created by qesR gets a `.qesR-cache` marker, and `qes_cache_clear()` refuses roots without it. Because a user-chosen directory is always used through a marked `qesR/` subdirectory, clearing works there too, and the user's own files are never touched.
- **Layout:** content-addressed, with no index file: `<root>/v1/<host>/<file_id>-<md5>.<ext>` and `<root>/v1/shards/qes2022-<md5>-s<schema>.{variables,values}.csv`. A re-pinned catalog can never be served stale bytes.
- **Writes:** `tempfile(tmpdir = dirname(dest), fileext = ".part")`, then an md5 check, then `file.rename()` (`file.copy` across volumes). A truncated file never gets its final name, and concurrent sessions are safe.
- **Reads:** the size must match, and the md5 is re-verified once per session per file (about 20 ms for 6 MB).
- **Memo:** an in-session memo of parsed data, keyed by md5 (`qesR.memo = TRUE`). Parsed data is never cached to disk.
- **Tip:** after the second network download in an interactive session, one `qesR_message_disk_cache_tip` suggests the disk cache. There is no prompt, because prompts break `Rscript` and knitr.
- **Management:** there is no LRU pruning; `qes_cache_info()` and `qes_cache_clear(older_than =)` cover it.

### 4.3 Checksums and pinning

- Every data or document read is `(file_id, md5)` from `files.csv`. A mismatch raises `qesR_error_checksum`, and the file is deleted.
- After reading, `n_rows` and `n_cols` are asserted; a mismatch raises `qesR_error_rowcount`.
- Dataset versions are pinned in `studies.csv`. Drift is detected by `qes_studies(check_updates = TRUE)` and by the weekly live CI job, and is never followed automatically.
- A deaccessioned file raises a 404 with an explanation; the weekly job catches it first.

### 4.4 Reader (`R/read.R`)

`.qes_read(study, file_id = NULL, cols = NULL)` is the only reader. `get_qes()`, `qes_codebook()`, the 2022 shard builder and `qes_harmonize()` all use it.

1. **Dispatch** on the catalog `format` only: `haven::read_sav(user_na = TRUE, encoding = files$encoding)` or `haven::read_dta(encoding =)`. Any other format is an error. There is no text, `.tab`, RDS or CSV reader for data.
2. **Post-processing:** `as.data.frame()`, then `.qes_unspss()` on labelled_spss columns (§2.4), then `name_map`, then the `n_rows`/`n_cols` assertion.
3. **Label precedence:**
   - the pinned file first;
   - then a same-UNF label donor (qes2012 SPSS);
   - then the reviewed supplement from `data-raw/questions/`, only where the file has no label (e.g. the qes2018 value labels, which the `.dta` lacks, taken from the questionnaire);
   - then NA.

   Labels never come from the DDI or from the variable name. The `ddi` origin is allowed only in `status = draft` spec rows (§5.5).
4. **Malformed labels:** a malformed variable label (qes2007_panel `ininum`, a 63-element vector) is coerced to its first element and recorded as `label_source = "file_malformed"`.
5. **Tripwire:** any U+FFFD or C1 character raises `qesR_warning_encoding`.
6. **Attributes:** `qes_survey_code`, `qes_provenance`, and optionally `qes_codebook`.

### 4.5 File choice per study

| Study | Pinned data file | Format | Rule and issues handled |
|---|---|---|---|
| qes2022 | 7449513 (Harvard) | dta | The original fixes 2,517 U+FFFD cells, dates (POSIXct) and column types. −99 stays in raw data because it is in the source. Metadata comes from a runtime shard (OD3). |
| qes2018 | 425914 | dta | The file has no value labels, so the reviewed supplement supplies them (accented). 1,805 U+FFFD cells are fixed. |
| qes2018_panel | 333052 | sav | `user_na = TRUE`, then `.qes_unspss()`; keys `method;id`. |
| qes2014 | 425916 | sav (SPSS twin) | v0.4.4 read the Stata twin 425915. The values are identical, and the SPSS labels are correct where the Stata twin's LANG/CODE labels are broken. Names equal v0.4.4. |
| qes2012 | 425918, plus donor 425917 | dta, with sav labels | The Stata twin keeps v0.4.4's lowercase names (`q25`, `pond`), which the paper uses. The SPSS donor supplies proper-case labels of up to 256 characters to the dictionary. Verified: same UNF, identical `tolower(names)`, identical row order. |
| qes2012_panel | 361043 | sav | Name map and user_na to be verified (R1). |
| qes2008 | 425919 (SPSS, the v0.4.4 twin) | sav | The twins' UNFs differ, so keep the v0.4.4 twin until both are compared (R1). |
| qes2007 | 425922 (Stata, the v0.4.4 twin) | dta | Same rule as qes2008 (R1). |
| qes2007_panel | 352415 | sav | Name map (3 names). Keys `nompn;quest`: 380 duplicate `quest` values across subsamples, 0 duplicates of the pair. |
| qes_crop_2007_2010 | 329990 | sav | CP850 encoding to be confirmed (R1). Fixes the `RESTE DU QU\u0090BEC` mojibake. |
| qes1998 | 329987 (panel) | sav | This is the file v0.4.4 loaded. It uses its own labels, not the CREATEC file's ([A:D2]). Its `intvote2` value labels are shifted in the source (§5.7), which is recorded in the dictionary. |
| qes1998_crop | 286331 | sav | New code. Resolves the 24,027 vs 24,026 discrepancy with `n_rows` (R1). |
| qes1998_createc | 316121 | sav | New code. |

`get_qes("qes1998", file = "CROP")` redirects to `qes1998_crop` with a message.

### 4.6 No personal information

- The UA contains only the package and R versions. No header, cookie, query parameter or API token is ever sent.
- The live CI uses the same client.
- **Tests (robust on any machine):**
  - the UA equals `sprintf("qesR/%s R/%s", packageVersion("qesR"), getRversion())` exactly;
  - `.qes_url()` is called with a fixture catalog row, and its output depends only on those fields: changing `USER`, `EMAIL`, `HOME` and `getwd()` via withr leaves it identical;
  - as a secondary check, no URL or header contains an environment value of 4 or more characters from `USER`/`EMAIL`/`LOGNAME`, matched as a whole token. Short or empty values are skipped, since an empty `USER` would match every string.

---

## 5. Harmonization layer

### 5.1 Spec files

All spec files live in `inst/extdata/harmonize/` as UTF-8 CSVs with LF endings (`.gitattributes: *.csv text eol=lf`).

```
SPEC               DCF: Spec-Version, Spec-Date, Schema-Version, Engine-Min, Hash, Licence
waves.csv          study x wave membership, timing, population, dates, mode, strata
weights.csv        weights registry
targets.csv        target definitions (EN/FR)
levels.csv         level sets (codes stable forever; EN/FR labels; exact aliases)
crosswalk.csv      one reviewed decision per (study, wave, target), incl. offered levels
valuemaps.csv      source code -> level or NA reason
legacy.csv         get_qes_master / get_decon column renderers
CHANGES.csv        spec changelog (EN/FR), source of the NEWS spec section
gates.csv          gate_var x source-non-NA counts for gated rows (CC0 studies)
expected/marginals.csv   projected unweighted harmonized marginals (CC0 studies)
expected/hashes.csv      md5 of each harmonized column per (study, wave, target), all studies
benchmarks_elections.csv added only after the owner verifies the official figures (R3)
```

**Ownership split with the catalog.**
- Waves and weights are harmonization decisions: they are versioned by the spec and included in its hash.
- Studies, files, elections, names and `enums.csv` belong to the catalog.
- Validator V-S4 enforces the references in both directions.

**Offline aggregates.**
- The dictionary (`dict/values.csv.gz`, §6.1) already holds per-code counts for CC0 studies, so there is no separate `sources/` folder.
- The engine's offline checks read the dictionary plus `gates.csv`.
- The 2022 equivalents live in build-ignored `data-raw/nc/` and run in GitHub CI only.

**Loader.** `.qes_read_csv()` is the only CSV loader, shared with the catalog and dictionary. It lands in **S1**, together with `enums.csv` and the NA vocabulary, because the catalog needs it first.

```r
utils::read.csv(f, colClasses = "character", na.strings = character(),
                encoding = "UTF-8", check.names = FALSE, strip.white = FALSE)
```

- There is no `fileEncoding` argument. With `fileEncoding = "UTF-8"`, a C locale set inside the session truncated a string to `"Tr"` on one path (re-checked by track a).
- Every column is asserted `validUTF8()` and passed through `enc2utf8()`.
- A per-table schema then types the columns and converts `""` to NA, except in columns flagged `keep_empty`, such as dictionary value labels.
- In code columns the literal `NA` is a token meaning system missing.
- Lists use `;` and arguments use `key=value;key=value`. There is no JSON and no R expression anywhere.

### 5.2 Validity model

- **Target:** one construct with one stimulus and one anchor source row. Example: `sov_indep`, "referendum vote, Quebec as an independent country", anchored on 2012 `q52`.
- **Family:** groups targets for discovery and splicing. Example: `sovereignty` = {`sov_indep`, `sov_sovereign_country`, `sov_partnership_1995`, `sov_favour`}.
- **Instrument:** a crosswalk column naming the concrete item format (`lr_0_10_slider`, `turnout_excuse_format`, `intent_lean_push`, ...).

**Grades**, set per crosswalk row against the target's anchor:

| Grade | Conditions (all must hold) | Verified example |
|---|---|---|
| `identical` | Same stem in every fielded language, same substantive options (the same `levels_offered`), same `dk_offered`, same universe rule and same mode family | 2014 `Q19` vs anchor 2012 `q52` (same FR stem) |
| `comparable` | Same construct and stimulus. Differences such as a temporal adverb, option order, whether DK is offered, or which minor parties are listed are not expected to move the marginals of the shared levels. Levels may collapse but are never inferred. | • 2018 `q26` and 2022 `cps_qc_referendum` ("today"/"aujourd'hui")<br>• 2014 `Q3` (no DK offered)<br>• 2018 `q27` ("la politique et les enjeux publics") |
| `approximate` | Same construct, but format, filter or mode is expected to move marginals | • 2018 `q5` and 2022 `pes_turnout` (face-saving formats)<br>• 2022 `cps_ideoself_1` (standalone slider without DK: 2.2% skipped vs 19-21% DK/refused in 2012-2018)<br>• years-of-schooling education (2007p, CROP, 1998) vs highest diploma<br>• any administrative-region → CMA mapping |
| `not_comparable` | A different construct, or a source that must not be used. It is recorded for the docs and **never mapped**. | • 2012 panel `interetrec` (derived from debate viewing)<br>• 2018 `q8` as "best party"<br>• 1998 `intvote2` (value labels shifted in the source)<br>• 2018p `independance` (a recode of `rts_q7`) |

**Grading rules:**
- The grade is the worse of the EN and FR judgments.
- Phone vs web is at best `comparable`.
- Grades are set by a human reviewer. `data-raw/suggest_grades.R` may suggest a grade but never writes the column.
- Validator V-S17 mechanically enforces the necessary conditions for `identical`: equal `levels_offered`, `dk_offered`, `instrument` and mode family to the anchor row.
- A different stimulus is never a grade; it is a different target (P3). Examples:
  - "pays souverain" (2012p) is `sov_sovereign_country`, not `sov_indep`;
  - a lean-pushed intention is `vote_prov_intent_push`.
- The default `min_grade = "approximate"` prints a once-per-call message listing the approximate cells included. Cells excluded by a stricter `min_grade` become NA with reason `below_grade`.

**Offered levels.**
- Each crosswalk row records `levels_offered`: the level names the instrument listed explicitly.
- Answers under "another party" map to `other`, which is a real answer, never NA.
- Levels that exist in the level set but were not offered (PCQ before 2022; PVQ, ON and PCQ in 2018 `q6`) are **structural zeros**.
- They are listed per cell in `qes_provenance(level = "cell")$levels_not_offered`, marked in the generated reference, and flagged in `print.qes_harmonized`, so a pooled "PCQ 0%" before 2022 cannot be misread.

**Timing, population and eligibility:**
- Each target declares `target_timing` (pre/post/any/static), `jurisdiction` and `election_ref_rule`.
- Each crosswalk row has `election_ref` (→ `catalog/elections.csv`), and validator V-S9 refuses timing mismatches (the [A:H4] 2022 case).
- Each wave declares `target_population` (§5.3). 1998 is "QC francophones 18+", so its constant "French" language is a **sample restriction**, not an imputation. Benchmarks (V-L2) skip it.
- `eligible_voter` means age ≥ 18 on the election date, plus citizenship where it was asked (2022 `cps_citizen`).
- qes2018 samples ages 16+: 251 minors (110 aged 16, 141 aged 17), 229 of whom were never asked `q5`. Its weights target the 16+ population with a "16-18" age cell. Normalizing within `eligible_voter` fixes the scale but is **not** an 18+ calibration, and the docs say so.
- Benchmarks and the legacy `turnout`/`vote_choice` columns are restricted to eligible voters. Benchmarks with an official denominator (registered electors) also exclude `not_registered`.

**Missing vocabulary.** It is defined once in `catalog/enums.csv` and shared with the dictionary's `missing_type` and with `qes_missing()`:

| Reason | Set by | `qes_missing` tag |
|---|---|---|
| `dk` | spec / dictionary | `d` |
| `refused` | spec / dictionary | `r` |
| `dk_refused` (one source code for "NSP/Refus" or "Ne sais pas/Pas certain": 2007p, CROP, 1998, 2012p `voteprov`, 2018p) | spec / dictionary | `b` |
| `no_answer` (web item nonresponse, e.g. 2022 −99) | spec / dictionary | `o` |
| `not_selected` (multi-select) | spec / dictionary | `s` |
| `inapplicable` (routed out) | spec / dictionary | `i` |
| `not_voted` | spec / dictionary | `v` |
| `spoiled` | spec / dictionary | `p` |
| `ineligible` (said they were not eligible, e.g. 2018 `q5` = 5) | spec / dictionary | `e` |
| `not_registered` (eligible but not on the list, 2022 `pes_turnout` = 5) | spec / dictionary | `g` |
| `not_in_wave` | spec / dictionary | `w` |
| `not_mappable` (the source category straddles target levels) | spec / dictionary | `m` |
| `user_na` (declared SPSS missing code of unknown meaning) | dictionary only | `u` |
| `sysmis`, `not_asked`, `below_grade`, `unmapped` | engine only | — |

- "Would not vote / would spoil / none" in an **intention** item is a substantive answer. It is the level `no_party` of the `party_qc_intent` set, never NA: it is 4.9% in CROP and 6-7% in 1998 and 2007p, and dropping it would distort intention shares.
- In a **recall** item, a voter who answers "spoiled" or "none" (2018 `q6` = 95, 2007 `q12` = 97, to be confirmed at R1) is `spoiled`.
- Harmonized output uses `missing = "reasons"` (`<target>__na` factor columns) rather than `haven::tagged_na()`, because categorical targets are factors and tagged NA works only on doubles.
- Raw data keeps its labelled doubles, so `qes_missing(action = "tagged")` is appropriate there.

### 5.3 `waves.csv` and `weights.csv`

**`waves.csv` columns:**
- `study, wave, wave_order, wave_timing (pre|post|between), wave_design (cross_section|panel_wave|poll_wave), election_ref`;
- `member_var, member_codes, n_cases`;
- `target_population_en, target_population_fr` (empty = the study default);
- `subsample_var, strata_var`;
- `fieldwork_start, fieldwork_end, date_var, date_format, mode (web|phone|mixed|var:<name>), notes`.

Membership is declared by rule on a **disposition or date variable**, never on a substantive item and never on a non-missing weight. `n_cases` is asserted.

```csv
study,wave,wave_order,wave_timing,wave_design,election_ref,member_var,member_codes,n_cases,subsample_var,strata_var,fieldwork_start,fieldwork_end,date_var,date_format,mode,notes
qes2022,cps,1,pre,panel_wave,QC2022,,,1521,,,2022-09-19,2022-09-23,cps_StartDate,posixct,web,
qes2022,pes,2,post,panel_wave,QC2022,pes_StartDate,!NA,1220,,,2022-10-04,2022-10-14,pes_StartDate,posixct,web,non-NA pes_StartDate = non-NA pes_weight_general (verified)
qes2018,post,1,post,cross_section,QC2018,,,3072,,,,2018-10-30,,,web,ages 16+
qes2018_panel,pre,1,pre,panel_wave,QC2018,,,1250,,,,,,,var:method,method 1-2 CATI (landline/cell; q1/q2 not asked) and 3 web; dates R4
qes2018_panel,post,2,post,panel_wave,QC2018,repondant_post,1,842,,,,,,,var:method,
qes2007_panel,pre,1,pre,panel_wave,QC2007,resultat,C,2050,nom_proj2,,2007-03-01,2007-03-22,s_date,yyyymmdd,phone,project 734A
qes2007_panel,post,2,post,panel_wave,QC2007,resultat_pst,CO,2054,nom_proj2,,2007-03-29,2007-04-13,s_date_pst,yyyymmdd,phone,1663 two-wave (= non-NA questpost) + 391 refusal conversions (734B)
qes2012_panel,pre,1,pre,panel_wave,QC2012,,,844,,,2012-08-24,2012-08-26,,,phone,file holds two-wave completers only (CROP)
qes2012_panel,post,2,post,panel_wave,QC2012,,,844,,,2012-09-10,2012-09-18,,,phone,
qes1998,panel,1,between,panel_wave,QC1998,,,1483,,firm,,,,,phone,francophones only; CREATEC 1057 + CROP 426
```

**Verified membership facts:**
- **qes2007_panel:**
  - `resultat == "C"` gives 2,050 pre-wave completes, and `resultat_pst == "CO"` gives 2,054 post-wave completes;
  - 1,663 respondents complete both waves, which is exactly the non-NA `questpost` count;
  - the other 391 post-wave respondents are pre-wave refusals (`resultat` B/R) from the refusal-conversion project (`nom_proj2 == "734B"`, 392 cases).

  The earlier rules (`intvote` non-NA = 2,049; `vote` in 0..9 = 2,055) were each off by one and used substantive items.
- **qes2018_panel:**
  - it has a pre wave: all 1,250 rows have `rv1a`/`rv1ab`, and its weight is `weight` ("WEB + CATI COMBINED", mean 1);
  - `q1`/`q2` are NA for exactly the 400 CATI cases, and `langfix` is non-NA only for CATI.
- **qes2012_panel:** the file holds only respondents who completed both waves (`participation` covers all 844: 572/206/66). Pre-wave estimates are therefore conditional on retention, and the docs say so.

Panel analyses such as the intent → recall stability check (§8.3) use only the two-wave respondents, and in 2007 the `subsample_var` separates panel members from refusal conversions.

**`weights.csv` columns:** `study, wave, weight_var, role (design|poststrat|raking|vote_calibrated|turnout_calibrated), scale (mean1|population), trim, calibrated_on, population, recommended, status (reviewed|needs_review), source_ref`.

```csv
study,wave,weight_var,role,scale,trim,calibrated_on,population,recommended,status,source_ref
qes2022,cps,cps_weight_general,raking,mean1,,age;gender;education;language,QC 18+ Census 2021,TRUE,reviewed,Codebook v1-1 p.7
qes2022,cps,cps_weight_general_trimmed,raking,mean1,0.2;5,age;gender;education;language,QC 18+ Census 2021,FALSE,reviewed,Codebook v1-1 p.7
qes2022,pes,pes_weight_general,raking,mean1,,age;gender;education;language,QC 18+ Census 2021,TRUE,reviewed,Codebook v1-1 p.7
qes2018,post,pond,poststrat,mean1,,sex x age;sex x region;sex x language;education,QC 16+ Census (age cell 16-18),TRUE,reviewed,Rapport méthodologique 2018 Tab. 12-14
qes2014,post,POND,poststrat,mean1,,sex;age;region;language,QC Census (latest),TRUE,reviewed,Léger technical note 2014 (Pondération)
qes2012,post,pond,,,,,,TRUE,needs_review,Stata twin name (SPSS donor spells POND); margins R4
qes2018_panel,pre,weight,poststrat,mean1,,,,TRUE,needs_review,file label WEIGHT - WEB + CATI COMBINED
qes2018_panel,post,weight_rts,poststrat,mean1,,,,TRUE,needs_review,DDI Weight_RTS
qes2012_panel,post,pond_post,poststrat,,,census,,TRUE,needs_review,DDI pondération_recensement
qes2012_panel,post,pondvote,vote_calibrated,,,reported vote,,FALSE,reviewed,DDI pondération par le vote déclaré
qes2012_panel,pre,pondam1,,mean1,,,,FALSE,needs_review,DDI "Pondération pré? à moyenne 1?" (R4)
qes2008,post,pond,poststrat,,,,,TRUE,needs_review,DDI Sans taux de participation
qes2008,post,pondx,turnout_calibrated,,,turnout,,FALSE,reviewed,DDI avec taux de participation
qes2007_panel,pre,pdspart2,vote_calibrated,,,previous vote,,FALSE,reviewed,file label (surpondération par le vote)
qes2007_panel,pre,pondam1,,mean1,,,,TRUE,needs_review,file label pondération à moyenne 1 (R4)
```

**Notes on these rows:**
- **qes2007_panel:** `pond_tot_am1` has a value for every row, including the 387 pre-only cases, so it is not a post-wave weight. The recommended 2007 post-wave weight is unknown (R4).
- **qes1998:** weights (`poids`, `ponder2` for CROP only, `ponderc`) are registered per firm at HZ5 (R2).
- **qes2014:** the `POND` margins come from the cached Léger note: "SEXE, ÂGE, RÉGION et LANGUE, selon le dernier recensement".

**Rules:**
- **V-S13:** a recommended weight never has role `vote_calibrated` or `turnout_calibrated`, and each study-wave has exactly one recommended weight.
- **Weight lint:** every `weight_var` must exist in the dictionary or shard names of the pinned file. This catches `POND` vs `pond` in 2012.
- **`needs_review` blocks output:** a target in a study-wave whose recommended weight is `needs_review` gets NA weights and a message until the row is reviewed (R4).

**Output:**
- **Respondent layout:** `weight_pre` and `weight_post` (plus `weight_pre_var`, `weight_post_var`). Each holds the recommended weight of the respondent's pre or post wave, or NA if the respondent is not in that wave or the study has no such wave. A post-only cross-section has `weight_pre = NA`.
- **Long layout:** a single `weight` column per row.
- **Weight guide:** `attr(, "qes_weight_guide")` = `target, study, target_timing, weight_column, weight_var`.
- **Timing message:** when the requested targets mix timings in a study whose weights differ, `qesR_message_weight_timing` is shown once per call. In qes2022, `pes_weight_general` is NA in 301 rows.

**`weights =` argument of `qes_harmonize()`:**
- `"normalized"` (the default) divides by the mean over members of the wave with a non-missing weight. This fixes scale only; it is **not** a pooling weight and does not fix CROP's row share, and the Rd page says so.
- `"raw"` gives the weights as deposited.
- Equal study totals are available only through `qes_design(pool = "equal")`, so there is one place for pooling.
- Raw variable names and means always go into provenance.

**Choosing a weight in `qes_design(weight = NULL)`:**
- Targets with `target_timing` `static` or `any` (socio-demographics, IDs) do not count as a timing.
- If all remaining targets share one timing, the matching weight column is used.
- Otherwise it raises `qesR_error_input`, naming `weight = "weight_pre"` or `"weight_post"`.
- The default `targets = "core"` mixes intention (pre) and recall (post), so the vignettes always pass `weight =` explicitly.

**Trimmed variants** have no argument. Users merge them from `get_qes()` by `source_row`; the internal `.qes_join_raw()` does the same and is the route if it is exported later (OD17).

### 5.4 `targets.csv`, `levels.csv`

**`targets.csv` columns:** `target, family, block (id|design|vote|party|attitudes|issues|socio), type (categorical|ordinal|numeric|date|string|weight), target_timing, jurisdiction, election_ref_rule, levels_id, valid_min, valid_max, anchor_row, derive_rule, derive_from, allow_constant, label_en, label_fr, description_en, description_fr, sets (;-list: core, decon, master_legacy, vote, ...), status (stable|experimental|retired), replaced_by, added_in`.

**`levels.csv` columns:** `levels_id, code (int, stable forever), name (ASCII key), label_en, label_fr, order, substantive, aliases`.

**`aliases`** hold exact strings compared after `.norm_label()`, which applies these steps in order:
1. NFC;
2. accent folding with a fixed table;
3. lower-casing;
4. unified apostrophes and squished whitespace.

The folding table is built at load time with `intToUtf8(<code points>, multiple = TRUE)`, never from non-ASCII literals, so it works when a C locale is set inside the session (verified: a literal `"Éé"` table makes `chartr()` fail under `Sys.setlocale("LC_ALL","C")`). It never uses `iconv` TRANSLIT, which gives `ma^itrise` on macOS. Aliases are used only by the contradiction check V-S8.

**Party codes.** They are **proposed** from the verified source labels:
- 2012 `q25`: 1 PLQ, 2 PQ, 3 CAQ, 4 QS, 5 PVQ, 6 ON, 96 other;
- 2014 `Q3`: the same list (96 "Un autre parti"), with no DK;
- 2018 `q6` (from the questionnaire, since the `.dta` is unlabelled): 1 PLQ, 2 PQ, 3 CAQ, 4 QS, 95 "J'ai annulé mon vote" (31), 96 "Un autre parti" (92), 99 refused (160);
- 2022 `cps_votechoice1`: 5 = PCQ;
- 2022 `pes_votechoice`: 5 = other, 6 = spoiled, 7 = PCQ.

The codes are final once slice HZ1 is merged, and never renumbered after that.

**Code collisions that forbid shared maps:**
- Code 3 is ADQ in 2007 `q12` and 2008 `q12a` (1 PLQ, 2 PQ, 3 ADQ), but CAQ from 2012 on.
- The 2007 panel order is different again (1 ADQ, 2 PLQ, 3 PQ).
- 2022 `cps_votelean` swaps the missing codes of `cps_votechoice1` (9 = DK and 10 = refused, against 9 = refused and 10 = DK).

None of these may share a `map_id`. Validator V-D3 (label check) blocks it where file labels exist, and review blocks it elsewhere.

```csv
levels_id,code,name,label_en,label_fr,order,substantive,aliases
party_qc,1,PLQ,PLQ,PLQ,1,TRUE,quebec liberal party;parti liberal du quebec
party_qc,2,PQ,PQ,PQ,2,TRUE,parti quebecois
party_qc,3,CAQ,CAQ,CAQ,3,TRUE,coalition avenir quebec
party_qc,4,QS,QS,QS,4,TRUE,quebec solidaire
party_qc,5,PVQ,PVQ,PVQ,5,TRUE,parti vert du quebec
party_qc,6,PCQ,PCQ,PCQ,6,TRUE,conservative party of quebec;parti conservateur du quebec
party_qc,7,ON,ON,ON,7,TRUE,option nationale
party_qc,8,ADQ,ADQ,ADQ,8,TRUE,action democratique du quebec
party_qc,90,other,Other party,Autre parti,90,TRUE,another party;un autre parti;another party (specify);another party (please specify)
party_qc_intent,95,no_party,Would not vote / spoil,Ne voterait pas / annulerait,95,TRUE,annulerait;ne voterait pas;aucun
pid_qc,97,none,None of these,Aucun de ceux-là,97,TRUE,none of these;rien de cela
interest4,1,very,Very interested,Très intéressé(e),1,TRUE,very interested;tres interesse(e)
interest4,4,not_at_all,Not at all interested,Pas du tout intéressé(e),4,TRUE,not at all interested;pas du tout interesse(e)
```

**Notes on levels:**
- `party_qc_intent` and `pid_qc` contain every `party_qc` level plus the row shown; the loader expands `inherits=party_qc`.
- Party labels are acronyms in both languages, so `vote_prov_recall == "PLQ"` works everywhere. Full names are in `description_*`.
- Non-ASCII characters in R code and tests use `\u` escapes; the CSV files are UTF-8.
- **ADQ and CAQ stay separate** (OD11). The optional `qes_party_lineage()` helper is internal until requested, and it maps ADQ → CAQ for time series at grade `approximate`.

### 5.5 `crosswalk.csv`, `valuemaps.csv`

**`crosswalk.csv`** has one reviewed decision per `(study, wave, target)`:
- **Rule:** `rule` (`map`, `numeric`, `weight`, `date`, `string`, `constant`, `fn:<name>`), `source_var` (exact case, as returned by `get_qes()` after the name map), `map_id`, `args` (`min=0;max=10;affine=x-1;from_label=TRUE`, `format=yyyymmdd`), `na_codes` (`98=dk;99=refused;-99=no_answer`).
- **Gate:** `gate_var`, `gate_codes`, and `gate_to`. `gate_to` is an NA reason, a level name, or a **per-code** list `code=outcome;...` on the gate variable, where `NA=` covers a missing gate value.
- **Output:** `primary` (≤ 1 per (study, target)).
- **Validity:** `grade`, `grade_reason_en`, `grade_reason_fr`, `instrument`, `election_ref`, `mode`, `dk_offered` (`explicit`/`volunteered`/`none`), `levels_offered` (`;`-list).
- **Wording:** `wording_en`, `wording_fr`, `wording_ref` (only `wording_ref` for qes2022 under OD3).
- **Review:** `evidence`, `notes_en`, `notes_fr`, `reviewed_by`, `reviewed_on`, `status` (`draft`/`review`/`stable`).

**Non-voters get the same NA reasons in every study:**
- "no" → `not_voted`;
- refused turnout → `refused`;
- "not eligible" → `ineligible`;
- "not registered" → `not_registered`;
- "don't remember" → `dk`.

`inapplicable` is reserved for routing that is not about voting, such as minors not asked.

Real rows follow. Every source fact was verified in the cached originals; qes2012 is keyed on the pinned Stata names. `levels_offered` uses party names (`PLQ;PQ;CAQ;QS;PVQ;ON;other`).

```csv
study,wave,target,rule,source_var,map_id,args,na_codes,gate_var,gate_codes,gate_to,primary,grade,instrument,election_ref,dk_offered,levels_offered,evidence
qes2012,post,vote_prov_recall,map,q25,vote_qes2012_q25,,98=dk;99=refused,q21,2;8;9,2=not_voted;8=dk;9=refused,TRUE,identical,vote_recall_v1,QC2012,explicit,PLQ;PQ;CAQ;QS;PVQ;ON;other,"anchor; q25 non-NA 1369 = q21==1; q21 2=117, 9=19"
qes2012,post,sov_indep,map,q52,sov_qes2012_q52,,8=dk;9=refused,,,,TRUE,identical,sov_indep_country,,explicit,yes;no,"anchor row"
qes2014,post,vote_prov_recall,map,Q3,vote_qes2014_q3,,99=refused,Q2,2;9,2=not_voted;9=refused,TRUE,comparable,vote_recall_v1,QC2014,none,PLQ;PQ;CAQ;QS;PVQ;ON;other,"Q3 non-NA 1352 = Q2==1; Q2 2=147, 9=18; no DK in Q2 or Q3"
qes2014,post,turnout_prov_recall,map,Q2,turnout_qes2014_q2,,9=refused,,,,TRUE,comparable,turnout_yesno,QC2014,none,yes;no,"1352/147/18; no DK (2012 q21 offers DK)"
qes2014,post,lr_self,numeric,Q32,,min=0;max=10,98=dk;99=refused,,,,TRUE,identical,lr_0_10,,explicit,,"endpoints labelled 0/10; 1195 valid"
qes2014,post,pid_prov,map,Q55,pid_qes2014_q55,,98=dk;99=refused,,,,TRUE,comparable,pid_prov_v1,,explicit,PLQ;PQ;CAQ;QS;ON;PVQ;none,"480/378/192/113/10/29; 97 none 181; strength Q56"
qes2018,post,vote_prov_recall,map,q6,vote_qes2018_q6,,99=refused,q5,1;2;3;5;99;NA,1=not_voted;2=not_voted;3=not_voted;5=ineligible;99=refused;NA=inapplicable,TRUE,comparable,vote_recall_v1,QC2018,none,PLQ;PQ;CAQ;QS;other,"q6 non-NA 2207 = q5==4; 95 annulé 31 -> spoiled; q5 NA 270 (229 minors); codes from questionnaire"
qes2018,post,lr_self,numeric,q36_1,,min=0;max=10,98=dk;99=refused,,,,TRUE,comparable,lr_0_10,,explicit,,"FR questionnaire Q36; 2490 valid"
qes2018,post,interest_4pt,map,q27,interest4_qes2018_q27,,98=dk;99=refused,,,,TRUE,comparable,interest_4pt,,explicit,very;quite;hardly;not_at_all,"stem adds 'et les enjeux publics'; 743/1422/687/160/40/20; same code direction as 2012/2014"
qes2018,post,income_native,map,q61,income_qes2018_q61,,99=refused,,,,TRUE,comparable,income_9brackets,,none,,"same 9 brackets as 2012 REVEN; asked if QAGE<2001; NA 270 (229 minors); no DK (2012 offers 98)"
qes2018,post,religion,map,q67,relig_qes2018_q67,,,q66,2;9,2=none;9=refused,TRUE,comparable,religion_denom,,explicit,,"q67 non-NA 1245 = q66==1; q66 2 -> 1737, 9 -> 90"
qes2022,pes,vote_prov_recall,map,pes_votechoice,vote_qes2022_pes,,,pes_turnout,2;3;4;5;6,2=not_voted;3=not_voted;4=not_voted;5=not_registered;6=dk,TRUE,comparable,vote_recall_v1,QC2022,none,PLQ;PQ;CAQ;QS;PCQ;other,"1109 = pes_turnout==1; 47/47/12/3/2"
qes2022,cps,vote_prov_intent,map,cps_votechoice1,vote_qes2022_cps,,,cps_turnout,3;4;5,3=inapplicable;4=inapplicable;5=inapplicable,TRUE,comparable,vote_intent_v1,QC2022,explicit,PLQ;PQ;CAQ;QS;PCQ;other,"asked if cps_turnout 1-2; 89 NA = cps_turnout 3 (62), 4 (26), 5 (1); no 'would not vote' option"
qes2022,cps,lr_self,numeric,cps_ideoself_1,,min=0;max=10,-99=no_answer,,,,TRUE,approximate,lr_0_10_slider,,none,,"standalone slider, no DK; -99 = 34 (2.2%)"
qes2022,cps,birth_year,numeric,cps_yob,,from_label=TRUE,-99=no_answer,,,,TRUE,identical,yob,,none,,"91 labels 1920-2010; codes 1..85, -99"
qes2018_panel,pre,vote_prov_intent,map,rv1a,vote_qes2018p_rv1a,,,,,,TRUE,comparable,vote_intent_v1,QC2018,volunteered,,"all 1250 rows; draft until codes reviewed"
qes2018_panel,pre,vote_prov_intent_push,map,rv1ab,vote_qes2018p_rv1ab,,,,,,TRUE,comparable,intent_lean_push,QC2018,volunteered,,"rv1a + lean push; draft"
qes2018_panel,post,turnout_prov_recall,map,rts_q1,turnout_qes2018p_rtsq1,,,,,,TRUE,approximate,turnout_excuse_format,QC2018,explicit,,"1 couldn't 56; 2 decided not 55; 3 voted 731"
qes2018_panel,post,vote_prov_recall,map,rts_q2,vote_qes2018p_rtsq2,,,rts_q1,1;2,1=not_voted;2=not_voted,TRUE,comparable,vote_recall_v1,QC2018,explicit,,"rts_q2 non-NA 731 = rts_q1==3"
qes2018_panel,post,sov_favour,map,rts_q7,sov_qes2018p_rtsq7,,5=dk,,,,TRUE,comparable,sov_favour_4pt,,explicit,,"1-2 favour 234; 3-4 oppose 546; 5 = 62"
qes2018_panel,post,sov_favour,none,independance,,,,,,,FALSE,not_comparable,,,,,"exact recode of rts_q7 (1-2 -> 1, 3-4 -> 0, 5/NA -> NA); never mapped"
qes2018_panel,post,lr_self,numeric,rts_q8,,min=0;max=10;from_label=TRUE,12=dk,,,,TRUE,comparable,lr_0_10,,explicit,,"codes 1-11 labelled '0'..'10'"
qes2007_panel,pre,vote_prov_intent,map,intvote1,vote_qes2007p_intvote1,,,,,,TRUE,comparable,vote_intent_v1,QC2007,volunteered,,"DK 329 before push"
qes2007_panel,pre,vote_prov_intent_push,map,intvote,vote_qes2007p_intvote,,,,,,TRUE,comparable,intent_lean_push,QC2007,volunteered,,"intvote1 + intvote2 push; DK 204"
qes2007_panel,post,vote_prov_recall,map,vote,vote_qes2007p_vote,,,,,,TRUE,comparable,vote_recall_v1,QC2007,volunteered,,"membership resultat_pst==CO; na_range 6..10"
qes2012_panel,pre,sov_sovereign_country,map,intvoteref,sov_qes2012p,,,,,,TRUE,identical,sov_sovereign_country,,volunteered,,"anchor; 'pays souverain'; 98/99 reversed"
```

- The qes2012_panel and 2018p `rv1a`/`rv1ab` rows are drafted from the DDI or the file and stay `draft` until R1 or code review.
- The `none` rule on `independance` is a documentation-only row (V-S12 forbids mapping `not_comparable`).
- For 2022 `cps_turnout` = 5 ("I already voted", n = 1), the CPS recall `cps_votechoice3` is not used. A single case does not justify a new reason, so it is `inapplicable`.
- 2022 `cps_votechoice2` ("If you decide to vote…", asked of `cps_turnout` 3-4) is a separate instrument (`intent_if_decide`), if it is used at all.

**`valuemaps.csv` columns:** `map_id, source_code, source_label, source_label_hash, source_label_origin, target_code, na_reason, alias_exception, note`.
- `source_label_origin` uses the single `label_source` enum (`file|label_donor|supplement|questionnaire|ddi`). `ddi` is allowed only in `draft` rows.
- For qes2022, `source_label` is empty and `source_label_hash = md5(.norm_label(label))`, so the label check still runs without shipping CC BY-NC text.
- `map_id` is shared across rows only when codes **and** labels agree (V-D3).

```csv
map_id,source_code,source_label,source_label_hash,source_label_origin,target_code,na_reason,alias_exception,note
vote_qes2022_pes,5,,<md5>,file,90,,,Another party
vote_qes2022_pes,6,,<md5>,file,,spoiled,,
vote_qes2022_pes,7,,<md5>,file,6,,,PCQ (code 5 in CPS)
vote_qes2022_cps,5,,<md5>,file,6,,,PCQ
vote_qes2022_cps,8,,<md5>,file,90,,,
vote_qes2022_cps,9,,<md5>,file,,refused,,cps_votelean has 9 = DK: never share this map
vote_qes2022_cps,10,,<md5>,file,,dk,,
vote_qes2018_q6,95,J'ai annulé mon vote,,questionnaire,,spoiled,,.dta unlabelled; questionnaire Q6
vote_qes2018_q6,96,Un autre parti,,questionnaire,90,,,
vote_qes2018p_rtsq2,6,Ou avez-vous annulé votre vote?,,file,,spoiled,,voter; counts as voted
vote_qes2007p_vote,7,A annulé son vote,,file,,spoiled,,producer user-missing
vote_qes2007p_vote,10,non rejoint,,file,,not_in_wave,,v0.4.4 counted these as abstainers [A:N1]
interest4_qes2018_q27,1,Très intéressé(e),,questionnaire,1,,,.dta unlabelled; same direction as 2012 Q67 and 2014 Q28
sov_qes2012p,99,Je ne sais pas,,file,,dk,,reversed convention
sov_qes2012p,98,Je préfère ne pas répondre,,file,,refused,,
```

### 5.6 Engine semantics (`R/hz-*.R`)

**Internal representation.** The only internal representation is a **cell table**: `(study, wave, source_row, target, code_or_value, na_reason)`. Layouts, value encodings, legacy rendering and splicing are pure functions of it and are tested separately.

**Steps, per study:**
1. **Read** through `.qes_read()` (§4.4); the md5 and `n_rows` must match. With user-supplied `data =` (a named list, §2.2), the engine takes a structural fingerprint (names, code sets, row count, label hashes), records `md5_verified = FALSE` and warns once.
2. **Data checks V-D1 to V-D3 and V-D7** (§5.10) run on the live data.
3. **Wave membership** is applied, then `id_vars` uniqueness is asserted. A duplicate raises `qesR_error_duplicate_id` and names the rows.
4. **Each crosswalk row** with `status == "stable"` (or `draft` with `include_draft`) and `grade >= min_grade` is processed:
   - `.canon(x)`: `unclass()` **before any NA test** ([A:N3]), `sprintf("%.15g")` for numbers (never `1e+05`), `enc2utf8(trimws())` for strings;
   - then the rule, then `na_codes`, then the gate (per code), then system NA → `sysmis`;
   - finally, an assertion that every NA has a reason.
5. **Unmapped codes:**
   - `"error"` (the default) raises `qesR_error_unmapped` with the codes and their n;
   - `"warn"` (used by the legacy renderers) sets NA with reason `unmapped` and warns once;
   - `"na"` does the same silently.

   Raw codes never pass through.
6. **Derived targets** are computed in dependency order:
   - `age`, then `age_group6` (includes 16-17) and `age_group3` (18-34/35-54/55+);
   - `eligible_voter`, `born_canada`, `income_rank`, `days_to_election`.

   `age_group3` collapses exactly from every 6-band source and is the only band scheme for qes2018_panel. A direct crosswalk row overrides a derivation, and when both exist a consistency check runs.
7. **Weights** come from the registry (§5.3).
8. **Layout:**
   - `respondent`: one row per source row, using primary crosswalk rows. Targets from different waves share a row (2022 `vote_prov_recall` from the PES and `vote_prov_intent` from the CPS).
   - `long`: one row per respondent × wave the respondent belongs to.
9. **Values:**
   - `factor` (the default; ordered for ordinal targets) uses levels in `lang`, **including levels unused in a study**, so `rbind` is safe across studies. Structural zeros are flagged (§5.2).
   - `labelled` gives `haven::labelled(<integer code>, labels = <named codes in lang>)`.
   - `code` gives ASCII names.

**Closed rule vocabulary:**

| rule | Behaviour |
|---|---|
| `map` | Exact code → level or reason via `valuemaps.csv`. String codes are allowed (2014 `LANG` "EN"/"FR"). |
| `numeric` | Pass-through within `[min,max]`. `affine=` accepts only `^(?:(-?[0-9.]+)\*)?x(?:([+-])([0-9.]+))?$` and is never evaluated. `from_label=TRUE` uses numeric value labels as values (`rts_q8`, `cps_yob`). A value outside the range and not in `na_codes` is unmapped. |
| `weight` | A number > 0, else `sysmis`. |
| `date` | `yyyymmdd`, `posixct` or `stata`. |
| `string` | Open text, passed through `enc2utf8`. |
| `constant` | Only where `allow_constant = TRUE`. This blocks the 1998 imputed "French"; 1998 language is handled as a sample restriction (§5.2). |
| `fn:<name>` | A registered, tested function `function(src, ctx) list(value, na_reason)`. At most 10% of rows (V-S10). Generic ones: `multiselect` (2022 `cps_lang_1..3`, where −99 = not selected), `bracket_midrank`, `age_from_birth_year`, `crop_wave_from_projet`. |
| `none` | A documentation-only row, allowed only with `not_comparable`. |

**Cross-instrument rescaling.** No rule rescales across instruments. There is no `interest_01` target; users who want to rescale do it explicitly on two targets. The legacy 0-10 `political_interest` is a legacy renderer (OD7), not a target.

### 5.7 Core target set (spec 1.0.0)

"✓" means the source was verified in the cached originals; the other entries are drafted.

| Block | Target(s) | Sources |
|---|---|---|
| id/design | `qes_id` (`<study>:<id_vars joined by ->`), `study`, `year`, `election_date`, `family`, `study_design`, `wave`, `target_timing`, `target_population`, `subsample`, `source_row`, `survey_mode`, `interview_date`, `interview_date_imputed`, `days_to_election`, `interview_lang`, `eligible_voter`, weights | 2022 `ResponseId` ✓; 2007p `nompn;quest` ✓ with `subsample` from `nom_proj2` ✓; 2014 `.row` ✓; 1998 `firm` |
| vote | `vote_prov_recall` (`party_qc`) | 2012 `q25` ✓ (anchor), 2014 `Q3` ✓ (comparable: no DK), 2018 `q6` ✓, 2022 `pes_votechoice` ✓, 2007p `vote` ✓, 2018p `rts_q2` ✓; 2007/2008 `q12` (97 "None" among voters → `spoiled`, R1), **1998 `q3post`** (post-election recall, fielded Dec 8-13) drafted |
| vote | `vote_prov_intent` (`party_qc_intent`) | 2022 `cps_votechoice1` ✓ (comparable; gated on `cps_turnout`), 2007p `intvote1` ✓, 2018p `rv1a` ✓, CROP `intvoteprova`, 1998 `vpl` (drafted, R2) |
| vote | `vote_prov_intent_push` (`party_qc_intent`), `vote_prov_lean` | 2007p `intvote` ✓, 2018p `rv1ab` ✓, CROP `intvoteprov`, 1998 `q7b` (drafted); 2022 `cps_votelean` ✓ (lean only, never mixed with `cps_votechoice1` codes). 1998 `intvote2` is `not_comparable` (shifted labels: "PLQ" = 277 = ADQ in `vpl`, "ADQ" = 325 = PLQ, "PARTI EGALITE" = 446 = PQ) |
| vote | `vote_prov_prev`, `vote_fed_recall`, `vote_fed_intent`, `vote_prov_hypo_nonvoter` | drafted (2018 `q5a` is the hypothetical, 95 = would spoil) |
| vote | `turnout_prov_recall` (yes/no) | 2012 `q21` ✓ (anchor), 2014 `Q2` ✓ (comparable: no DK), 2018 `q5` ✓ (approximate; 5 → `ineligible`), 2022 `pes_turnout` ✓ (approximate; OD12), 2018p `rts_q1` ✓ (approximate) |
| vote | `turnout_prov_likely` (ordinal) | 2022 `cps_turnout` ✓; **never** merged into recall |
| party | `pid_prov`, `pid_fed` (`pid_qc`, with `none`), `pid_prov_strength` | 2012 `q92` ✓, 2014 `Q55` ✓ / `Q56` ✓, 2018 `q56` ✓, 2022 `cps_provpid` ✓ / `cps_fedpid`. All `comparable` at most: 2012/2014 offer ON and Green; 2018 drops them; 2022 has no DK but adds Conservateur and "Another" |
| attitudes | `sov_indep` | 2012 `q52` ✓ (anchor), 2014 `Q19` ✓, 2018 `q26` ✓, 2022 `cps_qc_referendum` ✓ |
| attitudes | `sov_sovereign_country` | 2012p `intvoteref` ("pays souverain", anchor, draft); CROP `intvoterefa` ungraded until its full wording is known (R2) |
| attitudes | `sov_partnership_1995` | 2007/2008 `q19`, 2007p `intref1`, **1998 `voteref`** (CROP subsample only, 426 of 1,483) |
| attitudes | `sov_favour` | 2018p `rts_q7` ✓. `independance` is a recode of it and is never mapped |
| attitudes | separate `*_push` sovereignty instruments | 2007/2008 `q20`, CROP `intvoteref`/`intvoterefb`, 2007p `intref`, 1998 `q16b`. They are **never** rows of the `sov_*` targets above |
| attitudes | `lr_self` (0-10) | 2012 `q71` ✓ (anchor), 2014 `Q32` ✓, 2018 `q36_1` ✓, 2018p `rts_q8` ✓, 2022 `cps_ideoself_1` ✓ (**approximate**: slider, no DK) |
| attitudes | `interest_4pt`, `interest_0_10`, `interest_campaign_4pt` | 2012 `q67` ✓ (anchor), 2014 `Q28` ✓ (identical pending the stem check: "la politique en général"), 2018 `q27` ✓ (**comparable**: "et les enjeux publics"; same code direction); 2022 `cps_interest_1` ✓ (0-10 only, never binned); 2007p `interet` (campaign); 2012p `interetrec` not_comparable |
| socio | `birth_year`, `age`, `age_group6`, `age_group3` | 2022 `cps_yob` ✓/`cps_age_in_years`; 2018 `agecalc` (999 → refused); 2014 `QAGE` (9999); 2018p `age` ✓ (3 bands only, 4 = dk_refused) |
| socio | `gender` | all studies |
| socio | `education4`, `education_university` | diploma-based for 2012+; years of schooling (≤7 / 8-12 / 13-15 / 16+) for 2007p, CROP and 1998, graded `approximate`; 1998 `scol` 2 → `not_mappable` |
| socio | `income_native` (study-scoped text), `income_rank` (0-1, derived) | 2012 `reven`, 2014 `Q57`, 2018 `q61` ✓ (same 9 brackets as 2012), 2022 `cps_income` |
| socio | `lang_mother`, `lang_home`, `lang_interview` | 2014 `QLANG` ✓ vs `LANG` ✓; 2018 `qlangue`; 2022 `cps_lang_1..3` (`fn:multiselect`) / `cps_UserLanguage`; 1998 = sample restriction |
| socio | `region_admin` (17), `region_cma3` (Montréal CMA / Québec CMA / rest), `region_mtl2` (Montréal CMA / rest) | 2012 `REGIO` (CMA-based, direct), CROP `REG`, 2012p `reg`, 2018p `region` (Île / Couronne / Région de Québec / ROQ), 2007p `reg` (→ `region_mtl2` only; no Quebec CMA). `region_admin` from 2012 `q0qc`, 2014 `QREGION` ✓, 2018 `q0qc`. Admin → CMA mappings are `approximate` |
| socio | `religion`, `religious_attendance` | 2014 `Q62`/`Q63`, 2018 `q66`/`q67` ✓ |
| socio | `birthplace` (quebec/other_canada/abroad), `born_canada` | 2012 `q105`, 2014 `Q65` ✓, 2018 `q69` ✓ (2605/138/304/8/17), 2022 `cps_borncda` ✓ (direct `born_canada`) |
| socio | `citizen` | 2022 `cps_citizen` |

`region4` from the earlier draft is dropped: the Montréal and Québec CMAs cut across Montérégie, Laurentides, Lanaudière and Chaudière-Appalaches, so a four-group region cannot be built from the 17 administrative regions.

**Per-study coverage plan:**
- **Spec 0.x (0.6.0, experimental)** covers the table above for the six cached studies (2012, 2014, 2018, 2022, 2007p, 2018p).
- **Spec 1.0.0 (0.7.0)** adds 2007, 2008, 2012p, CROP and the three 1998 codes once R1/R2 arrive.
- **Issue items** (satisfaction with democracy, attachment to Quebec and Canada) come later as MINOR bumps, one target at a time after wording review.
- **`party_best`** gets no target until a per-issue item passes review.

### 5.8 `qes_harmonize()` output

**Leading columns:**
- respondent layout: `study, year, election_date, family, study_design, target_population, waves` (membership, e.g. `cps;pes`), `qes_id, subsample, source_row, survey_mode, interview_date` (first wave), `days_to_election, eligible_voter`;
- long layout: `wave`, `wave_timing` and `wave_design` replace `waves`.

These are followed by the targets, then their companion columns (`__na` with `missing = "reasons"`, `__src` with `keep_source = TRUE`), then the weight columns: `weight_pre, weight_post, weight_pre_var, weight_post_var` (respondent layout) or `weight, weight_var` (long layout).

**Attributes:**
- `qes_spec`: `version, hash, custom, engine`;
- `qes_provenance`;
- `qes_weight_guide`;
- `failed_studies`: `study, class, message, parent_message`.

**Printing.** `print.qes_harmonized` shows a header, then `head()`. The header gives the spec version, the studies, any approximate cells included, any structural zeros, and the licence when qes2022 is present.

### 5.9 Provenance

`qes_provenance(x, level)`:
- **study:** `study, doi, dataset_version, file_id, file_name, format, md5_expected, md5_observed, md5_verified, unf, n_rows, n_cols, pinned, retrieved_via (network|session_cache|disk_cache|local_demo|user_data), retrieved_at (UTC), licence, label_source, name_map_applied, reader, haven_version, catalog_version, dict_version`.
- **cell:** `study, wave, target, source_var, rule, map_id, grade, instrument, weight_var, levels_not_offered, included, n_valid, n_dk, n_refused, n_dk_refused, n_no_answer, n_inapplicable, n_not_voted, n_spoiled, n_ineligible, n_not_registered, n_not_in_wave, n_sysmis, n_unmapped, n_outside_universe, note`.
- **spec:** `spec_version, spec_hash, spec_custom, qesR_version, qesR_sha` (`RemoteSha` when installed from GitHub, else NA), `args, created`.

`get_qes()` objects carry the study level only. A replication log records `qes_provenance(d)`.

### 5.10 Validator (one implementation, four places)

`qes_spec(validate =)` runs these rules (in every view; `view = "spec"` returns the problems table):
- at runtime (schema and hash cached per session);
- in offline CRAN tests (on the shipped CC0 dictionary and `gates.csv`);
- in CI (`data-raw/spec_check.R`);
- in weekly live CI (on originals).

| id | Check | Where |
|---|---|---|
| V-S1 to V-S4 | Columns and types; unique keys; every enum column against `catalog/enums.csv`; referential integrity (targets, levels, maps, `file_id` → catalog, waves, elections) | runtime, tests, CI |
| V-S5 | ≤ 1 `primary` row per (study, target) | same |
| V-S6 | EN **and** FR present (labels, descriptions, levels, grade reasons, CHANGES) | tests, CI |
| V-S7 | Valid UTF-8 and NFC; no U+FFFD or C1 characters | tests, CI |
| V-S8 | **Alias contradiction:** a map row whose source label matches a *different* level's alias is an error unless `alias_exception` is set | tests, CI |
| V-S9 | Target timing vs wave timing; `election_ref` vs `election_ref_rule` | tests, CI |
| V-S10 | Every `fn:` has a registered function and a test file; `fn:` rows ≤ 10% | CI |
| V-S11 | `stable` requires `evidence`, reviewer, date and wording (or `wording_ref`). `ddi` label origin is only allowed in `draft`. Release builds refuse `draft`. | CI (tags) |
| V-S12 | `constant` only where allowed; `not_comparable` rows use rule `none` | tests, CI |
| V-S13 | Weight roles and one recommended weight per study-wave; no active target uses a `needs_review` weight in a released spec | tests, CI |
| V-S14 | Ordinal maps are monotone in source order unless a note explains otherwise | tests, CI |
| V-S15 | Target, family and set names are pairwise disjoint, match `^[a-z][a-z0-9]*(_[a-z0-9]+)*$`, and never equal a study code | tests, CI |
| V-S16 | `levels_offered` ⊆ the target's level set; every mapped target code is in `levels_offered` or is `other` | tests, CI |
| V-S17 | `identical` rows have the anchor's `levels_offered`, `dk_offered`, `instrument` and mode family | tests, CI |
| V-D1 | Source and gate variables exist, with exact case and a near-match hint | tests (dict), CI, runtime |
| V-D2 | Observed codes ⊆ mapped codes ∪ range ∪ `na_codes` ∪ gate codes. No sentinel (−99, 8, 9, 97, 98, 99, 998, 999, 9999) survives as a value unless it is a level. | same |
| V-D3 | The file label (or its hash) equals `source_label` (or `source_label_hash`). It cannot catch a label that is wrong in the source file itself (1998 `intvote2`); that case is `not_comparable` by review. | same |
| V-D4 | Producer user-missing codes are present and mapped explicitly | tests, CI |
| V-D5 | `id_vars` are unique | build, runtime |
| V-D6 | md5 and `n_rows` match the catalog pin | runtime, live CI |
| V-D7 | Universe identity: the source is non-NA exactly on gate-open rows, and every gate code has an outcome | tests (`gates.csv`), CI, runtime |
| V-D8 | Wave membership counts equal `n_cases` | tests (dict), CI, runtime |
| V-P1 | Projected marginals (spec + dictionary counts) == `expected/marginals.csv` | tests (CC0), CI (+ 2022 from `data-raw/nc`) |
| V-P2 | Spec content changed ⇒ SPEC bumped (§5.11), CHANGES row present, `Hash` updated | CI; the hash half runs on CRAN |
| V-P3 | Generated docs are current (`git diff --exit-code`) | CI only |
| V-L1 | Engine marginals and column hashes on real files == `expected/` | live CI |
| V-L2 | Weighted recall dissimilarity index vs official results ≤ recorded baseline + 2.0 points; skipped when the weight is vote-calibrated or the population is not the electorate (1998) | live CI (after R3) |
| V-L3 | Weighting-margin reproduction (2018 `pond` sex × French within 0.005 of Tab. 14) | live CI |
| V-L4 | Construct-validity directions (§8.3) | live CI |
| V-L5 | A new dataset version or md5 on Dataverse → the job fails and lists the drift | live CI |

**Known limit.** qes2018 has no file labels, so V-D3 cannot run there. Its protection is:
- V-S8, V-S14 and V-S16 against the questionnaire-sourced labels;
- V-D7, V-P1/V-L1 and V-L4;
- two-person review.

### 5.11 Versioning and reproducibility

- **Spec semver**, independent of the package version:
  - **MAJOR:** any existing key's value changes in `expected/marginals.csv` or `expected/hashes.csv` (a corrected map, gate, level set or weight role, or a grade crossing the default `min_grade`), or a target is retired.
  - **MINOR:** keys are only added (target, study, wave or row).
  - **PATCH:** text only (wording, notes, translations, evidence).
  - Corrections are MAJOR, because users compare `major.minor` to know whether their numbers could have moved.
  - Before 1.0.0 (the 0.6.x experimental line) the same rule applies to the minor digit, as usual in semver.
- **Enforcement.** CI diffs `expected/` against `git merge-base origin/main`. Rows that cannot be projected offline (`fn:` rows and weights) are covered by the live column hashes (V-L1). A hash change forces MAJOR.
- **Guarantee** (stated in `?qes_harmonize`): the same spec version and hash, the same source md5s and the same qesR version give identical output.
- **Freezing.** Copy `inst/extdata/harmonize/` from a release tag and pass `spec = "<dir>"`. SPEC's `Schema-Version`/`Engine-Min` say whether the installed engine can run it. There is no in-package archive.
- **Provenance** is written next to any `save_path` as `<stem>_provenance.csv`.
- **v0.4.4 results:**
  - They can only be reproduced by installing d1faad6 (`remotes::install_github("ThomasGareau/qesR", ref = "d1faad6")`); there is no compatibility flag.
  - `data-raw/compare_legacy.R` runs the old and new versions in separate libraries. It writes an aggregates-only "What changed" table into NEWS: legacy column × study, with n valid, mean or distribution, and cause ID.
  - Changes the owner's master-based files will see:
    - 2014 ideology n goes from 1,053 to 1,195;
    - 2018 ideology is present (2,490);
    - 2022 vote becomes recall;
    - language becomes mother tongue;
    - memo means move (PLQ 5.89 → 6.08).

    Several of these arrive only at 0.7.0; in 0.5.0 the affected cells are blank.
  - The article pipeline (raw codes) is unchanged; its identity check gives N = 4,477.

### 5.12 What `get_qes_master()` becomes

**0.5.0 (interim, OD2).** The old `master.R` code keeps running on the new reader, with deletions only and no new regexes or recodes.
- **Frozen sources.** Source discovery is replaced by `data-raw/legacy_source_map.csv`: the per-(column, study) source variable that v0.4.4 actually chose, taken from the `source_map` attribute of a clean d1faad6 build (R9). The interim master selects exactly those variables, so label-text changes in the new reader cannot change which variable is chosen. This is still a deletion: runtime discovery is removed.
- **Removed:**
  - [A:H1] stacking (63 columns, listed in `attr(, "removed_columns")` and in a once-per-session message);
  - dedup;
  - `.remove_empty_master_rows()`. Blanking more cells would otherwise make more rows all-empty and silently drop them.
- **Asserted:** `nrow == sum(n_rows)` per study (qes2007_panel has 2,442 rows).
- **Blanked:** each cell verified invalid, listed per (column, study) in `legacy_na_columns` and in NEWS:
  - `party_best` and `party_lean` in all studies;
  - `political_interest` in 2018 (the raw 1-4 codes land on the 0-10 column);
  - `ideology` in 2014 (endpoints lost);
  - `born_canada` in 2018, plus the 7 DK texts in 2014;
  - `language` in 2014 and 2022 (OD8);
  - `income` and `religion` wherever sentinels or raw codes appear;
  - `turnout` and `vote_choice` wherever the v0.4.4 source is an intention item: 2022, CROP and 1998 (OD4);
  - `sovereignty_support`/`sovereignty` in 2007, 2008 and 1998 (partnership wording) and in 2012p ("pays souverain") (OD5);
  - `vote_choice_text` in 2018.
- **Appended:** `vote_choice_timing` and `sovereignty_item`, per-study constants that document what those columns hold. They are kept in 0.7.0 (appended columns are never removed).
- **Message:** a once-per-session message: "Harmonized values changed in qesR 0.5.0; see NEWS. Results from qesR ≤ 0.4.4 are reproducible by pinning d1faad6." (EN/FR).
- **`get_decon()` interim:** the same mechanism. 2022 −99 is blanked, 2018 `turnout`/`votechoice` (wrong source, [A:H6]) are blanked, `partylean`/`party_best` are NA, and it has frozen sources.

**Gate:** `compare_legacy.R` against the R9 baseline shows that every difference from v0.4.4 is an intended deletion or blank. This needs R1/R2 for the five uncached studies.

**0.7.0 (legacy switch, only when all 11 legacy studies are in the spec):**

```r
qes_harmonize(studies = .or_default(surveys, <the 11 v0.4.4 codes>), targets = "master_legacy",
              layout = "respondent", min_grade = "approximate",
              weights = "raw", unmapped = "warn", on_fail = if (strict) "stop" else "skip",
              lang = "en")
```

`.or_default()` is an internal helper. `%||%` is base R only from 4.4, and the package declares `R (>= 4.1)`.

This is followed by the `legacy.csv` renderer (`profile, position, legacy_column, target, render, note`). Its renders are `as_is`, `legacy_party`, `int01`, `scale10`, `chr_level_en`, `na_column`, `constant:<value>` and `catalog:<field>`.

| Legacy column | Source (render) | Change users see |
|---|---|---|
| `qes_code`, `qes_year`, `qes_name_en` | catalog | Corrected Durand/CROP names |
| `respondent_id` (chr) | ID part of `qes_id` | Never synthetic except 2014 (`.row`); 2007p gets `674A-1234`-style IDs (from `nompn`) |
| `interview_start/_end/_recorded` | `interview_*` (ISO chr) | Real values |
| `language` | `lang_mother` | Always mother tongue; OD8; 1998 "French" is kept as the sample restriction |
| `citizenship`, `gender` | `citizen`, `gender` | Non-binary → NA in the legacy column only |
| `year_of_birth`, `age`, `age_group` | `birth_year`, `age`, `age_group6` (2018p: `age_group3`, OD15) | 16-17 kept |
| `province_territory` | constant "Quebec" | `region_cma3`/`region_admin` appended |
| `education` | `education4` | 1998 → NA; "maîtrise" fixed |
| `income` | `income_native` (label text, unchanged meaning) | Sentinels → NA; 2012 and 2018 filled; `income_rank` appended |
| `religion`, `born_canada` | `religion`, `born_canada` | 2018 "None" restored; other-province = Yes (138) |
| `political_interest` | OD7 legacy renderer | 2018 filled with the corrected conversion |
| `ideology` | `lr_self` | 2014 endpoints (+142); 2018 filled (2,490) |
| `turnout`, `vote_choice` | recall targets, eligible voters (OD4) | 2022 → PES recall; 1998 → `q3post` recall; CROP → NA with `vote_intent` appended; 2007p "non rejoint" → NA |
| `vote_choice_text` | "other" text of the recall item | 2018 no longer issue text ([A:H7]) |
| `party_best`, `party_lean` | `na_column` / `vote_prov_lean` | NA where the source was another construct |
| `sovereignty_support`, `sovereignty` | `sov_indep` (OD5) | 2007/2008/1998/2012p → NA; `sov_partnership_1995` appended |
| `federal_pid`, `provincial_pid` | `pid_fed`, `pid_prov` | 2014 filled from `Q55` |
| `survey_weight` | OD6 | |

**Appended columns** (0.5.0 ones first, never removed): `vote_choice_timing, sovereignty_item, family, study_design, wave, subsample, source_row, weight_pre, weight_post, vote_intent, turnout_intent, region_cma3, region_admin, income_rank, sov_partnership_1995`.

**Attributes kept:**
- `source_map` (plus `map_id`, `grade`, `file_md5`, `spec_version`);
- `loaded_surveys`;
- `failed_surveys` (with root causes);
- `duplicates_removed = 0L`;
- `empty_rows_removed = 0L`;
- `harmonized_variables`;
- `crossstudy_variables_added = character(0)`;
- `variable_name_map` (0 rows);
- `variable_name_map_path = NULL`;
- `saved_to`.

**Attributes added:** `qes_provenance`, `qes_spec`, `legacy_column_map` (`column, target, definition, studies_changed`), `legacy_na_columns` and `removed_columns`.

`get_decon()` follows the same two steps, using the `decon` profile.

### 5.13 Licence handling

- No respondent rows ship. `inst/extdata/qes_master.csv` is deleted, and `qes_demo` and all fixtures are synthetic.
- **CC0 studies:** labels, questionnaire wording and aggregate counts ship, attributed in `inst/COPYRIGHTS`.
- **qes2022** (CC BY-NC), until OD3 is decided:
  - label hashes only, and `wording_ref` only;
  - aggregates and plain label text live in build-ignored `data-raw/nc/`, used in GitHub CI;
  - dictionary data comes from the runtime shard;
  - `qesR_message_licence` is shown once per session when the study is loaded (not silenced by `quiet`);
  - `print()` of harmonized output names the licence.
- A test enforces "`metadata_shipped = FALSE` ⇒ no dictionary rows, no `source_label`, no `wording_*`, and no rows in `expected/marginals.csv`".
- **Borealis deposit (HZ9, conditional):**
  - only with written permission from QES/CECD and C-Dem;
  - separate CC0 and CC BY-NC files, keyed on spec version and hash;
  - read with haven or CSV, never RDS;
  - added as a catalog row with `source = "deposit"`.

---

## 6. Codebooks, labels, bilingual search

### 6.1 Dictionary (shipped, CC0 studies only)

The dictionary is `inst/extdata/dict/variables.csv.gz` and `values.csv.gz`, built by `data-raw/build_dictionary.R` from the pinned originals, the donor labels and the curated `data-raw/questions/<study>.csv`.

**`variables`**, keyed by `(study, variable)` (names as returned by `get_qes()`):
- identification: `position, source_name, type, measure (nominal|ordinal|interval|text|id|weight|admin), var_timing (pre|post|single|wave:<w>)`;
- text: `label, question_en, question_fr, question_truncated, universe_en, universe_fr`;
- missing codes: `na_values`;
- derivation: `derived_from` (e.g. 2018p `independance` → `rts_q7`), so a recode is never mapped as a second source;
- sources: `label_source` (the single enum `file|label_donor|supplement|questionnaire|file_malformed|none`), `question_source`, `doc_ref (file_id:item)`;
- review: `reviewed`.

**`values`**, keyed by `(study, variable, value)`:
- `label` (`keep_empty`, so a genuine `""` label survives, e.g. 2012 q64=96 and 2018 Q2_960=1);
- `label_source`, `label_lang`, `label_en`, `label_fr`;
- `missing_type` (the §5.2 vocabulary);
- `n` (unweighted; every observed code of variables with ≤ 50 distinct codes, plus every labelled code);
- `label_flag` (e.g. `shifted_in_source` for 1998 `intvote2`).

Example rows (verified counts):

```csv
study,variable,value,label,label_source,label_lang,missing_type,n
qes2012,q52,1,Yes,label_donor,en,,567
qes2012,q52,8,I don't know,label_donor,en,dk,156
qes2018,q26,8,Je ne sais pas,supplement,fr,dk,463
qes2018,q69,2,Ailleurs au Canada,supplement,fr,,138
qes2007_panel,interet,9,* NSP/Refus,file,fr,dk_refused,3
```

**qes2022 shard (OD3):**
- It is built on first use from the header of the user's cached, md5-verified `.dta` and written as CSV into the active cache. It is never RDS and never shipped.
- Labels of exactly 80 characters get `question_truncated = TRUE` (504 of 718 variables), with `doc_ref = "7449514"` (the codebook PDF).
- `qes_search()` picks the shard up automatically.

**Curation is phased:**
- (a) every variable referenced by a crosswalk row, or by the owner's paper (§2.3 of downstream-impact), gets `reviewed = TRUE`;
- (b) the rest follows, and `qes_search()` reports coverage per study.

### 6.2 `qes_codebook()`

- **Positions 1-6 are HEAD's.** `variables` and `lang` are appended.
- **What `srvy` accepts**, in order of precedence:
  1. a character study code: offline for shipped studies; for qes2022 it builds or loads the shard;
  2. an object of class `qes_codebook`, which is laid out again;
  3. a data.frame carrying `attr(, "qes_codebook")`: that attribute is used, filtered to the columns still present, so a user-subsetted frame gives a subsetted codebook;
  4. a data.frame carrying `qes_provenance` but no codebook: the codebook is rebuilt from the study code in the provenance, filtered the same way;
  5. anything else, including a codebook read back from CSV (class lost): `qesR_error_input`, naming the fix (`qes_codebook("<code>")`).

  NEWS records that `format_codebook(<plain data.frame>)` is now an error.
- **Columns by layout:**
  - **compact:** the legacy columns first (`variable, label, question, n_value_labels`), then `study, position, type, question_lang, question_truncated, value_labels` (`"1=Oui | 2=Non | 8=NSP"`), `missing_codes, targets` (NA until HZ3), `label_source, question_source, doc_ref`;
  - **wide:** the legacy wide shape;
  - **long:** one row per value: `study, variable, value, value_label, missing_type, is_declared_na, label, question`.
- **Attributes** are restored after every layout ([A:K1]): `survey_code, doi, doi_url, selected_data_file, files, codebook_files, qes_provenance`.
- **Text rules:**
  - `question` is in `lang`, or in the source language when `lang = NULL`;
  - it is NA when unknown, never the variable name ([A:K4]) and never a copy of `label` ([A:K5]);
  - there is no machine translation.

### 6.3 Bilingual search

- `qes_search()` folds with `.qes_fold()`, the same function as `.norm_label()` (§5.4): accents are folded **before** lower-casing, with a table built from code points at load time. A test asserts `Encoding()` of the table and runs the fold under a C locale.
- It searches variable names, labels, questions (EN and FR), value labels and target names.
- `lang = "both"` is the default for search only: search is a lookup, and matching in both languages is what bilingual users expect.
- `regex = TRUE` is opt-in.
- The result lists `targets` for each raw variable, so `qes_search("souverain")` leads to `sov_indep` and `sov_sovereign_country`.

---

## 7. Messages and errors

- **Message table.** `R/messages.R` holds `list(key = c(en = "...", fr = "..."))`, with `\u` escapes for non-ASCII characters. Keys are **added in the slice that first uses them** (about 70 by 0.7.0), not all up front.
  - The constructors are `.qes_msg()`, `.qes_abort()`, `.qes_warn()` and `.qes_inform()`, built on base `errorCondition()`/`warningCondition()`.
  - There is no rlang or cli, and no gettext/po.
- **Tests:** every key has both languages and the same `%` placeholders. Other tests assert condition class and fields, never message text.
- **Once-per-session state** (deprecation, licence, assign default, values changed, disk-cache tip, ignored argument) lives in the internal environment `.qes_once`. The unexported `.qes_reset_once()` clears it, and a test helper `local_qes_once()` resets it and defers the reset (`withr::defer()`), so message tests do not depend on test order.
- **Who controls which notice:**
  - `quiet = TRUE` silences progress and informational messages;
  - deprecation notices obey only `qesR.quiet_deprecated`;
  - licence notices are always shown once;
  - the assignment-default notice fires only when `assign_global` was not supplied at an exported entry point (§2.3).
- **Language resolution (messages only):**
  1. `getOption("qesR.lang")`;
  2. `QESR_LANG`;
  3. `LANGUAGE`;
  4. `Sys.getlocale("LC_MESSAGES")` (`LC_COLLATE` on Windows), matched with `^fr` case-insensitively;
  5. otherwise `"en"`.
- **Fields.** Every condition carries `id` (the key), `lang` and data fields. `conditionMessage()` appends the parent condition's message.

```
qesR_error
├── qesR_error_input             (arg, value)
├── qesR_error_unknown_study     (study, suggestions)          # utils::adist
├── qesR_error_unknown_variable  (study, variables, suggestions)
├── qesR_error_ambiguous_file    (study, pattern, candidates)
├── qesR_error_network           (url, attempts, parent)
│   ├── qesR_error_http          (status, retry_after, server_message)
│   │   └── qesR_error_http_refused   (WAF 202 / 403; no retry, no bypass)
│   ├── qesR_error_tls           (never followed by an insecure retry)
│   └── qesR_error_offline
├── qesR_error_source            (study, file_id)
│   ├── qesR_error_checksum      (expected, actual)
│   └── qesR_error_rowcount      (expected, actual)
├── qesR_error_spec              (problems = data.frame)
├── qesR_error_unmapped          (study, target, codes, n)
├── qesR_error_duplicate_id      (study, id_vars, rows)
├── qesR_error_cache             (path, reason)
└── qesR_error_no_provenance
qesR_warning: _partial (failures), _unpinned, _truncated, _encoding, _label_mismatch,
              _universe, _unmapped, _unverified_source
qesR_message: _download, _cached, _licence, _deprecated, _assign_default, _weight_timing,
              _approximate_cells, _structural_zeros, _legacy_columns, _values_changed,
              _disk_cache_tip, _arg_ignored
```

**Options:**

| Option (env var) | Default |
|---|---|
| `qesR.cache` (`QESR_CACHE`) | `"session"` |
| `qesR.cache_dir` (`QESR_CACHE_DIR`) | NULL |
| `qesR.lang` (`QESR_LANG`) | unset (use the locale) |
| `qesR.max_tries` | 4 |
| `qesR.stall_timeout` | 60 |
| `qesR.memo` | TRUE |
| `qesR.quiet_deprecated` | FALSE |

There is no insecure option.

**Examples:**
- EN: "Unknown study code 'QES 2022'. Did you mean 'qes2022'?"
- FR: "Code d'étude inconnu « QES 2022 ». Vouliez-vous dire « qes2022 » ?"

---

## 8. Testing and validation

### 8.1 Tier 0: offline, on CRAN, under 60 s (testthat edition 3, ≥ 3.1.7)

**Test seams.** There are two documented internal seams, both replaced with `local_mocked_bindings()`:
- `.qes_transport()` (network);
- `.qes_catalog()` (catalog). It serves a fixture catalog whose md5s match the synthetic fixtures written at test time, so every export, including `get_qes()` with a real-looking code and `get_qes_master()` with its default 11 studies, can run offline.

**Contract tests** (written in S0b; tests that only S0c can satisfy are marked with `skip("fixed in S0c")` until then):
- **`formals()` snapshot** of all 14 exports against d1faad6, with an allowed-diff table:
  - `get_qes$assign_global`, `get_qes_master$assign_global` and `get_decon$assign_global`: `TRUE` → `FALSE`;
  - `qes_codebook`: `variables`, `lang` appended.
- **Visibility:** `expect_visible()`.
- **No side effects:** `globalenv()`, `getwd()`, `~` and `R_user_dir` are unchanged after every export runs with default arguments against the fixture catalog.
- **Assignment:**
  - opt-in assignment lands in the caller's frame, at top level and inside a user function, directly and through each legacy wrapper, with `identical(assigned, returned)`;
  - `get_qes(assign_global = TRUE)` also assigns `<code>_codebook`, and uses the canonical code.
- **Legacy wrappers:** each emits `qesR_message_deprecated` exactly once, driven by `.qes_deprecated` (with `local_qes_once()`), and returns the legacy shape. `quiet = TRUE` does not silence it.
- **Legacy master:** attribute names, `empty_rows_removed = 0L`, and `nrow == sum(n_rows)`.
- **Legacy name manifests** (column names only, from v0.4.4 outputs).
- **Export-name grammar**, and every canonical export appears on `?qesR-fr`.
- **Export manifest:** `getNamespaceExports("qesR")` equals the 27 names of §2.2 (16 canonical plus 11 legacy), so an internal helper such as `.qes_splice()` cannot be exported by accident (OD17).
- **`CITATION` equality:** `qes_cite(NULL, "bibentry")` equals `readCitationFile(system.file("CITATION", package = "qesR"))`.

**Reader fixtures**, written at test time with `haven::write_sav`/`write_dta`, each under 20 KB and none derived from real rows:
- user-missing declarations (`is.na` parity after `.qes_unspss`);
- an 80-character label and a `''` value label;
- CP850 text;
- a multi-element label attribute;
- an upper/lowercase twin pair;
- accented long names (the name map);
- a Stata datetime;
- a character column containing `-99`;
- duplicate `quest` values across subsamples.

**HTTP**, by mocking `.qes_transport` with canned responses:
- 200; 303 → 200; 503 × 2 → 200; 429 with `Retry-After` (sleep mocked);
- 404 (no retry); WAF 202 → `qesR_error_http_refused`;
- a stall; an md5 mismatch (no final file remains);
- a connection error, whose `parent` is kept;
- the UA and privacy assertions (§4.6).

**Cache:**
- modes and roots, with `R_USER_CACHE_DIR` redirected by withr;
- a user-chosen directory gets a marked `qesR/` subdirectory;
- clearing a root without the marker is refused;
- writes are atomic.

**Catalog and dictionary lints** (§3.4); `VERSIONS` md5s; `enums.csv` coverage.

**Spec:**
- `test-hz-spec` (V-S1 to V-S9, V-S11 to V-S17, SPEC hash recompute);
- `test-hz-data` (V-D1 to V-D5, V-D7 and V-D8 against the dictionary and `gates.csv`);
- `test-hz-projection` (V-P1).

**Engine:**
- `.canon()`: labelled_spss with `na_range` 6..10 keeps codes 6 and 7; `1e5` formatting; the affine grammar accepts `x`, `x-1` and `0.5*x+2` and rejects `x^2`, `exp(x)` and `system()`;
- each rule; per-code gate outcomes; the three unmapped modes; every NA has a reason.

**End to end.** `.qes_synthetic()` is an internal helper that builds in-memory data from the spec, with real variable names and every mapped code and gate combination but no respondents. It feeds `qes_harmonize(data = list(<study> = ...))` for 2 studies × 2 waves. Checks:
- rows per layout; identical factor levels across studies;
- weight mean 1 per study-wave; unique `qes_id`;
- provenance counts sum to N; structural zeros flagged;
- `min_grade` blanking plus its message; `missing = "reasons"`;
- `lang = "fr"` gives identical codes;
- an unnamed `data` is an error.

**Regressions.** One named test per assessment ID, using real codes:
- [A:H3]: `q27` = 1 → very (same direction as 2012/2014); `Q32` 0/10 kept;
- [A:H4]: `q36_1`; `q69` = 2; `q66` = 2 → none; 2022 PES 7 / CPS 5 → PCQ;
- [A:N1], [A:N2], [A:H5], [A:H6];
- the new validity rows: 2022 `cps_turnout` gate; `pes_turnout` 5/6; 2007p membership 2,050/2,054/1,663; 2018p `independance` never mapped; 1998 `intvote2` not_comparable.

It also includes the ported `spec_legacy` fixture, which must raise the V-S8/V-S9/V-D1/V-D2/V-D3 errors.

**Locale identity:**
- `Sys.setlocale("LC_ALL","C")` **inside the session**, plus `LANGUAGE=fr`;
- reader, codebook, search, spec and harmonize output must be `identical()` to the default run, while message text switches;
- non-ASCII test strings are created with `intToUtf8()` or `\u` escapes **before** the locale switch, never as literals after it.

**Legacy tests:**
- `formals()`; names, order and types; attributes;
- `save_path` writes UTF-8 CSV and the provenance file;
- the interim blanking list (0.5.0) and the renderer (0.7.0);
- the interim master runs on `qes_demo` through the frozen source map.

**Demo:** its own test file. Every Rd example except the single network example runs on it.

**Forbidden calls on CRAN.** A test walks every function in `asNamespace("qesR")`, deparses its body and fails on:
- `ssl_verifypeer`, `insecure`;
- `readRDS`, `load`, `system2`, `eval`, `parse`;
- `.GlobalEnv`, `globalenv()`;
- `assign(` outside `.qes_assign`;
- `iconv(` with `TRANSLIT`;
- `conditionMessage` inside `grepl`.

This works on the installed package, where `R/` is absent. The same patterns are grepped over `R/` in CI.

### 8.2 Tier 1: live (`skip_on_cran`, `skip_if_offline`, `QESR_LIVE=true`; weekly CI with `actions/cache` keyed on the md5 pins)

- `QESR_TEST_DATA_DIR` lets the mocked transport serve local originals (e.g. `scratchpad/impact/orig`) with no network.
- **Per study:** md5, `n_rows`/`n_cols`, name manifest, and `as.numeric()` identity against haven reading the original. This protects the paper's N = 4,477 pipeline.
- **Known marginals:**
  - 2018: `q36_1` 2,490 valid; `q69` 2605/138/304/8/17; `q27` 743/1422/687/160/40/20; `q6` 490/392/700/342/31/92/160;
  - 2014: `Q32` 1,195; `Q55` 480/378/192/113/10/29/181/92/42;
  - 2022: `pes_turnout` 1109/47/47/12/3/2 and 301 NA;
  - 2007p: `resultat == "C"` 2,050, `resultat_pst == "CO"` 2,054, both 1,663;
  - keys: `(nompn, quest)` 2,442; `(method, id)` 1,250.
- **Universe identities:**
  - 2018 `q6` 2207 = `q5`==4;
  - 2022 `pes_votechoice` 1109; 2022 `cps_votechoice1` NA = `cps_turnout` 3-5 (89);
  - 2014 1352;
  - 2012 1369;
  - 2018 `q67` 1245;
  - 2018p `rts_q2` 731;
  - 2022 `pes_StartDate` non-NA = `pes_weight_general` non-NA (1,220).
- V-L1 and V-L3 to V-L5, plus `qes_studies(check_updates = TRUE)` reporting no unexpected status.

### 8.3 Tier 2: validity (release gate for spec MAJOR/MINOR)

| Check | Rule | Current values (provisional) |
|---|---|---|
| V-L2 recall vs official | Weighted dissimilarity index ≤ baseline + 2.0 points. Skipped for calibrated weights and for 1998 (francophones only). | Baselines 2012 8.0, 2014 5.6, 2018 3.2, 2022 8.0. **The official figures were recalled from memory, so they are not entered until R3.** Applying the CPS map to the PES item gives 17.4 and fails. |
| Turnout over-report | `info` only; fails if < 0 or > 35 points | +16.8 to +23.4 |
| V-L3 margins | 2018 `pond` sex × French within 0.005 of Rapport 2018 Tab. 14 | 0.375/0.376, 0.396/0.395, 0.112/0.112, 0.117/0.117 |
| V-L4 construct | Mean interest: voters > non-voters. `lr_self`: QS < CAQ. `sov_indep` yes: PQ − PLQ > 40 points. `pid_prov` = vote ≥ 55% | 2.92 > 2.23; 3.01 < 6.33; 74 vs 1; 70-72% |
| Panel | Time-invariant agreement ≥ 0.95; intent → recall stability ≥ 0.60, computed on **two-wave respondents only** (2007p: 1,663) | 2007p sex 0.984; stability 0.814 (to be recomputed on the 1,663) |

Benchmarks are coarse: a PQ/PLQ swap in 2012 (0.75 points apart) is invisible to them. That is why V-L4 and exact marginals (V-L1) carry the main weight.

Results are written as aggregates (CC0 studies only) to `inst/validation/validation_report.csv` and rendered in a pkgdown "Validation" article. qes2022 rows go only to the CI artifact until OD3 is decided.

### 8.4 CI

| Workflow | Trigger | Content |
|---|---|---|
| `R-CMD-check.yml` | every push and PR, all branches | Matrix: ubuntu devel/release/oldrel-1, R 4.1, windows-release, macos-release; `--as-cran`. On ubuntu-release it also runs `Rscript data-raw/spec_check.R`: V-S10, V-S11, V-P1 (with `data-raw/nc`), V-P2 against the merge-base, V-P3, the `fn:` budget, the source grep, and the vignette chunk-parity check (`knitr::purl`). These are CI-only because vignette and `R/` sources are not installed. This replaces `r-devel-check.yml`. |
| `live.yml` | weekly cron, `workflow_dispatch`, tags | Restores originals from the cache and fetches only missing files through the package client. Runs tier 1, V-L1 to V-L5 and the benchmarks. Uploads an aggregates-only report. A failure is the notification; there is no bot and no token. |
| `pkgdown.yml` | main | Builds the site, including the generated references and the articles (which download through the cache). |

---

## 9. Documentation

- **roxygen2 only** ([A:A3]).
  - The hand-written `man/` and NAMESPACE are regenerated in S0a, whose gate is an identical NAMESPACE and unchanged `\usage` lines.
  - Pages use `@family` per §2.2 and `@seealso` between old and new names.
  - `?qesR-deprecated` holds the old-to-new table.
  - Rd text is in English; the French entry point is `?qesR-fr`.
  - The `assign_global` `@param` text is updated on all four pages that have it: it now says "the calling environment; `.GlobalEnv` only when called at top level".
- **Examples:**
  - every example runs offline on `qes_demo` or shipped metadata;
  - the single network example is `qes_studies(check_updates = TRUE)` in `\donttest{}`, inside `if (curl::has_internet())` and `tryCatch(qesR_error_network = ...)`, because `--as-cran` runs `\donttest{}`.
- **`inst/CITATION`** is a static `bibentry()` file using `meta$Version`, generated by `data-raw/make_citation.R` from the `qes_cite()` builder. It never calls package internals, because `readCitationFile()` can run without the namespace loaded. A test keeps the two equal (§8.1).
- **Generated from the spec** (single source, EN/FR cannot drift):
  - `vignettes/harmonization-reference.Rmd` and `vignettes/fr-reference-harmonisation.Rmd`, each one `results = "asis"` chunk calling `.spec_reference_md(lang)`. They arrive **with the engine** (HZ3), so the engine never ships without its reference. They run offline on CRAN.
    - Each target section has the definition and levels, then a coverage table: study, wave, source, grade and reason, instrument, offered levels, wording or `wording_ref`, gate, weight and `dk_offered`.
    - Each section ends with its CHANGES history.
  - the target list on `?qes_spec` via roxygen `@eval .rd_targets()`;
  - `print(qes_spec("crosswalk", targets = target))`;
  - the NEWS spec section (from `CHANGES.csv`);
  - the README coverage table (from `qes_spec()`);
  - the website's harmonization reference and study catalog pages (§9.1).
- **CRAN vignettes (all execute offline):**

  | EN | FR | Content | From |
  |---|---|---|---|
  | `get-started` | `demarrage` | `qes_demo`; then `qes_studies()`, `qes_search()`, `qes_codebook("qes2014")`. The `qes_spec()` and `qes_harmonize("qes_demo", ..., weight = ...)` sections are added in 0.6.0 | 0.5.0 |
  | `citations` | `fr-citations` | `qes_cite()` over the catalog (reviewer request 3) | 0.5.0 |
  | `migrating-0.5` | `fr-migrer-0.5` | the §2.3 table, `x <- get_qes("x")`, pinning d1faad6 | 0.5.0 |
  | `harmonization-reference` | `fr-reference-harmonisation` | generated (above) | 0.6.0 |

- **EN/FR sync:**
  - hand-written pairs have identical chunk labels and code, and CI compares their `knitr::purl` output, so the prose may differ but the code may not;
  - the reference pair is generated;
  - `_strings.csv`/`knit_child` is not used.
- **pkgdown-only `vignettes/articles/`** (build-ignored):
  - the analysis articles, rebuilt at site build time. In 0.5.0 they use `get_qes_master()`. From 0.6.0 they use `qes_harmonize()` with QES-only defaults, `qes_design()` with stated weights, `min_grade`, and the sovereignty targets shown side by side with their wording breaks (no pooling across wordings while `.qes_splice()` is internal);
  - per-study variable pages;
  - the Validation article.

  This fixes [A:V1]: there is one data source and no shipped sample. It also fixes [A:V2]: designs are separated, estimates are weighted, structural zeros are marked, and the CROP and denominator statements are corrected.
- **pkgdown navigation:** FR entries are derived from the `fr-` prefix. This replaces the hand-maintained JS map in `_pkgdown.yml`. The full site design is §9.1.
- **NEWS 0.5.0** sections:
  - *Breaking default*: `assign_global` is now FALSE, and on opt-in the object lands in the calling environment (`.GlobalEnv` only at top level). Opt-in still also assigns `<code>_codebook`. `qes_survey_code` is now the canonical code.
  - *Changed outputs*: every [A:D*]/[A:H*]/[A:K*] fix, with affected variables and the interim blanking list. The `_variable_name_map.csv` sidecar is replaced by `<stem>_provenance.csv`, and `variable_name_map_path` is `NULL`. `format_codebook(<plain data.frame>)` is an error.
  - *Soft-deprecated names (kept indefinitely)*: the table, including arguments that became no-ops (`refresh`, `get_question(full = FALSE)`) and changed meaning (`download_codebook(file =)`).
  - *New*.
  - *Security*.
  - *Reproducibility*: "The owner's paper pinned v0.4.4 (d1faad6); results change."
- **`cran-comments.md`:**
  - opt-in assignment into `parent.frame()` is the pattern CRAN accepts;
  - the argument keeps its legacy name `assign_global` only for signature compatibility (reviewer request 5);
  - the one-line reasons for `curl` and `jsonlite`;
  - libcurl on Linux.
- **README:** no `save_path` into `getwd()`, and doi.org links only (this removes the 202 NOTE).

### 9.1 Website (slice W, OD19)

The pkgdown site is reworked in slice W, after S5, and refreshed in W.1 (0.6.0) and W.2 (0.7.0). It is **built locally and not deployed**: the build goes to a scratch copy (never `docs/` in the repo), and publishing to gh-pages stays the owner's action.

| Part | Design |
|---|---|
| Theme | Bootstrap 5 (`template: bootstrap: 5`), one light theme with a dark-mode toggle, system font stack, no custom JavaScript. The hand-maintained JS language map in `_pkgdown.yml` is deleted. |
| Logo | `man/figures/logo.png` shrunk to about 40 KB (§10) and used in the navbar, the README and as the favicon source (`pkgdown::build_favicons()` run locally, output committed under `pkgdown/favicon/`). |
| Bilingual navigation | Two navbar menus, "Guides" and "Guides (FR)", plus an EN/FR link on every article pointing to its pair. Pairs are found by the `fr-` prefix rule, and a build check fails on an article without a partner. The home page has a short French section linking to `?qesR-fr` and the FR articles. |
| Getting started | One path from install to **a correct weighted estimate**: install; `qes_studies()`; `x <- get_qes("qes2014")` (returned, not assigned); `qes_codebook()`/`qes_search()`; `qes_missing()`; a weighted proportion with the study's recommended weight and the population stated; then, from 0.6.0, `qes_spec()` → `qes_harmonize(..., min_grade = "comparable")` → `qes_design(weight = "weight_post")`. The CRAN vignette runs offline on `qes_demo`; the site version is the same file, so the code is identical. |
| Reference | Grouped by `@family`: Data; Studies and documents; Codebooks and search; Harmonization; Reproducibility; Cache; Package and French overview (`qesR-package`, `qesR-fr`); and a separate **Legacy** section holding the 9 legacy pages and `qesR-deprecated`, headed by the old-to-new table. A build check fails if an exported topic is missing from the index. |
| Harmonization reference | The generated reference vignette pair (§9), from the spec, in EN and FR; one section per target with its coverage table and CHANGES history. It appears from W.1. |
| Study catalog page | A pkgdown article generated at build time from `catalog/studies.csv` and `catalog/files.csv`: one row per study with year, family, design, population, n, licence, DOI link, pinned version and documents (`qes_docs()`), EN/FR. No hand-written copy of catalog facts. |
| Analysis articles | Website-only (`vignettes/articles/`, build-ignored): the analysis articles and the Validation article (§9). They download through the cache at build time, use stated weights and show structural zeros. |
| Search | pkgdown's built-in search, with `url:` set in `_pkgdown.yml` so the search index is built. |
| Removed | The stale 40k-row master and every page built from it (the variable dump, `inst/extdata/qes_master.csv`, root `qes_master.csv`), the French callouts that duplicated article text, and the per-study variable dump pages. Per-study variable information comes from `qes_codebook()` on the reference pages instead. |
| Checks | Local `pkgdown::check_pkgdown()` and a full build in a temporary copy; link check on internal links; no page reads a file outside the package or the cache. |

---

## 10. Repository cleanup

Dependencies were verified by grep on the current tree.

| Item | Today | Depends on it | Action | When |
|---|---|---|---|---|
| `inst/extdata/qes_master.csv` (224 KB, derived 2022 BY-NC rows) | shipped | 6 analysis vignettes plus FR mirrors (`system.file("extdata","qes_master.csv")`) | Delete, after the vignettes move to `articles/` | S5 |
| Root `qes_master.csv` (16 MB, tracked), `.rds`, `_source_map.csv`, `_variable_name_map.csv` | tracked, build-ignored | the vignettes (`"../qes_master.csv"`); nothing reads the others | `git rm --cached` after the vignettes move; removing it first would silently switch the site to the sample. It is not republished as a file, and it is not the R9 baseline (it was built by older code). | S5 |
| `qes_master_test*` | ignored | none | delete locally | S0b |
| `scripts/build_qes_master.R` | tracked | named in vignette error messages | Move to `data-raw/legacy_build_master.R` (used by `compare_legacy.R`) | S5 |
| `scripts/commit_and_push_*.sh`, `scripts/build_pkgdown_site.R` | tracked | nothing (CI does not use them) | delete | S0b |
| `codebooks/` (16 MB, 21 third-party files) | tracked, build-ignored | only dead code in `qes_download.R:1169-1224` | `git rm --cached` once S3 lands. Keep locally as `data-raw/` source material. The files stay on Dataverse and are listed by `qes_docs()`. | S3 |
| Insecure TLS fallback and shell-out in `R/qes_download.R` | code | the test that asserts the insecure retry | delete (small, independent diff; makes the branch releasable earlier) | **S0c** |
| Rest of `R/qes_download.R`: text reader, DDI scoring, PDF/DOC/Python scraping, ASCII override block; `R/assign_utils.R` | code | replaced by `http.R`, `cache.R`, `read.R`, `metadata.R` and `assign.R` | delete (about 3,800 lines; `R/` shrinks from 5,311 lines to about 1,500, plus the engine's roughly 400-line core) | S2a-S3 (DDI path and xml2 in S3) |
| `R/master.R`, `R/decon.R` | code | legacy functions | S4 makes deletions only (frozen sources). HZ6 replaces them with the renderer and deletes the old code. | S4, HZ6 |
| `.github/workflows/r-devel-check.yml` | one job on main | none | folded into `R-CMD-check.yml` | S0b |
| `qesR_0.*.tar.gz`, `..Rcheck/`, `docs/` (67 MB, stale) | ignored | none (the site is rebuilt in slice W, outside the repo) | delete locally; `git fetch` to refresh `origin/gh-pages` | S0b |
| `._*` AppleDouble (332 files; `._pack-*.idx` causes "non-monotonic index") | untracked | none | `dot_clean` the drive; add `._*` to `.gitignore` and `.Rbuildignore` | S0b |
| `man/figures/logo.png` (309 KB, 45% of the tarball) | shipped | pkgdown and README | shrink to about 40 KB; favicons from it in W | S5, W |
| DESCRIPTION | | | Drop `LazyData`; drop `pkgdown`, `dplyr` and `ggplot2` from Suggests; quote 'Dataverse'; cite the dataset DOIs in Description; bump to 0.5.0; OD13 | S5 |
| LICENSE, LICENSE.md, `inst/COPYRIGHTS`, `cran-comments.md` | reviewer request 6 open | | Set the copyright holder to the author and make both files match. Add COPYRIGHTS. Write the resubmission section answering all 6 points. | S5 |
| New: `data-raw/` (build-ignored) | | | `build_catalog.R`, `build_dictionary.R`, `questions/*.csv`, `make_demo.R`, `make_citation.R`, `build_sources.R`, `project_marginals.R`, `spec_check.R`, `add_study.R`, `suggest_grades.R`, `news_from_changes.R`, `compare_legacy.R`, `legacy_source_map.csv`, `check_metadata.R` (xml2 only here), `nc/` | S1+ |

The old tests are restored from `git show 397aa0c^:tests/...` and triaged in S0b. The test that asserts the insecure retry is deleted in S0c.

---

## 11. Implementation slices

Every slice is one mergeable PR. Each keeps the 14 exports working, keeps R CMD check at 0/0/≤1, and carries its own tests and docs.

**Sizes are human-effort estimates for a developer working by hand** (S < 1 day, M 1-3 days, L about a week, XL more than that). They measure scope; they are not a schedule and not an estimate of Claude-driven implementation time.

**Dependency graph.** The two lanes are not independent: the harmonization lane starts after S1 and joins the data lane at S2b and S3.

```
S0a -> S0b -> S0c -> S1 -> S2a -> S2b -> S2c
                      |           |  \-> S3 --------\
                      |           |  \-> S4 (R1,R2,R9; OD4/5/8/9/15) -> S5  = 0.5.0 (CRAN; also needs S2c, S3, OD2, OD3,
                      |           |                                         S2b 11-study gate = R1, R2)
                      |                                                         \-> W (website, local build) -> W.1 at 0.6.0 -> W.2 at 0.7.0
                      \-> HZ1 -> HZ2 (needs S3) -> HZ3 (needs S2b) -> HZ4  = 0.6.0 (CRAN, experimental; after S5)
                                                                    \-> HZ5 (R1, R2, R4) -> HZ6 = 0.7.0 (CRAN)
                                                                                           -> HZ7 (R3, R5) -> HZ8 -> HZ9 (OD14)
```

| Slice | Content | Depends on | Exit gate | Size (human) | Ships |
|---|---|---|---|---|---|
| **S0a** roxygen | roxygen2 migration only | — | NAMESPACE identical; Rd `\usage` lines unchanged | S | — |
| **S0b** Test net | • testthat 3<br>• restored and triaged 397aa0c tests<br>• contract tests and name manifests (`skip("fixed in S0c")` where needed)<br>• CI matrix<br>• local cleanup | S0a | contract tests green or explicitly skipped on unchanged code | M | — |
| **S0c** Assignment and conditions | • `.f_impl` wrappers and `.qes_assign()` (caller frame; default FALSE; visible return; `<code>_codebook`; canonical code)<br>• condition tree and `messages.R` with this slice's keys<br>• `.qes_once`<br>• `.qes_deprecated` registry<br>• insecure TLS fallback and shell-out deleted | S0b; owner edits the paper to `x <- get_qes("x")` or keeps it pinned | S0b skips removed and green | M | — |
| **S1** Catalog offline | • `catalog/*.csv` including `enums.csv`<br>• `.qes_read_csv()`, the NA vocabulary, `.qes_catalog()` seam and fixture catalog, `VERSIONS`<br>• `qes_studies()` (`waves` NA), `qes_docs()`, `qes_cite()`, static `inst/CITATION`<br>• the `qes_demo` tree<br>• `get_qescodes`/`get_codebook_files` wrappers | S0c | catalog lints; CITATION equality; citations vignette offline | M | — |
| **S2a** Transport and cache | • `http.R`, `cache.R`, `qes_cache_*()`<br>• the old download path moved onto `.qes_transport()`<br>• curl in | S1 | mock, cache and privacy tests | M | — |
| **S2b** Reader | • `read.R` (`.qes_unspss`, name map, label precedence)<br>• `get_qes()` on originals | S2a | **Merge gate:** tier-1 identity for 2012/2014/2018/2022 and both panels. **Release gate (checked in S5):** the same for qes2007, qes2008, qes2012_panel, CROP and 1998 (R1, R2) | M | — |
| **S2c** Download and provenance | `qes_download()`, `get_preview` wrapper, `qes_provenance()` (study level) | S2b | md5-before-rename tests | S | — |
| **S3** Metadata | • `build_dictionary.R` and `questions/*.csv` (phase a)<br>• `qes_codebook()`, `qes_question()`, `qes_search()`, `qes_missing()`, the 2022 shard<br>• codebook wrappers rewired<br>• DDI path deleted and **xml2 out**<br>• `codebooks/` untracked | S2b | [A:K1]-[A:K7] and [A:A6] tests; locale identity; dictionary < 1 MB | L | — |
| **S4** Interim legacy | • `get_qes_master()`/`get_decon()` on the new reader with frozen sources (`legacy_source_map.csv`)<br>• dedup and empty-row removal deleted; blanking (§5.12); timing columns; 2,442 rows<br>• `compare_legacy.R` | S2b; OD4, OD5, OD8, OD9, OD15; R1, R2, R9 | every diff from the R9 baseline is intended and listed | M | — |
| **S5** Release 0.5.0 | • vignettes (§9); analysis moved to `articles/`<br>• `inst/extdata/qes_master.csv` and the root masters removed<br>• NEWS, licence, COPYRIGHTS, OD13 (drop the `cph` person from Authors@R, set the LICENSE holder to Thomas Gareau-Paquette, and re-roxygenise so `man/qesR-package.Rd` `\author` no longer lists "Quebec Election Study [copyright holder]"), cran-comments, logo | S0a-S4; OD2, OD3; S2b release gate | `--as-cran` 0/0/1 on the matrix | M | **0.5.0, CRAN** |
| **W** Website | the §9.1 site: Bootstrap 5 theme and logo; EN/FR navigation; grouped reference with a Legacy section; getting-started path to a weighted estimate; study catalog page generated from the catalog CSVs; analysis articles website-only; search; stale master pages removed. **W.1** (with 0.6.0) adds the harmonization reference and engine-based articles; **W.2** (with 0.7.0) the Validation article and the engine-rendered master | S5 | local `pkgdown::build_site()` in a temporary copy with no errors; `check_pkgdown()` clean; every export indexed; every article paired EN/FR; nothing deployed | M | site (local) |
| **HZ1** Spec skeleton | • spec schema and verified rows (§5.5)<br>• `qes_spec(view = "spec")` with V-S*<br>• `.canon()`<br>• SPEC hash test | S1 | V-S1 to V-S17 green on the shipped spec | M | internal |
| **HZ2** Offline checks | • `build_sources.R`, `gates.csv`, `expected/`<br>• V-D* and V-P1 on the dictionary<br>• `data-raw/nc/` for 2022<br>• `.qes_synthetic()`<br>• ported `spec_legacy` regressions | HZ1, S3 | V-P1 exact on CC0 studies | M | internal |
| **HZ3** Engine | • `qes_harmonize()` (respondent layout); the `"targets"` and `"crosswalk"` views of `qes_spec()`<br>• cell-level provenance and structural zeros<br>• the six cached studies<br>• **generated reference vignette pair**<br>• **experimental** label | HZ2, S2b | V-L1 on the six studies; regression tests | L | internal |
| **HZ4** Waves and weights | • long layout; 2022 CPS/PES; panels, including the 2018p pre wave and 2007p disposition membership<br>• `weights.csv`, eligibility, `days_to_election`<br>• `qes_design()`; internal `.qes_join_raw()` and `.qes_splice()` (not exported, OD17) | HZ3 (`needs_review` weights stay NA until R4) | V-L3; the design tests | L | **0.6.0, CRAN (experimental)** |
| **HZ5** Remaining studies | qes2007, qes2008, qes2012_panel, CROP (poll waves), the 1998 split (firm strata, francophone population) | HZ4; R1, R2, R4 | V-D6/V-L1 on the new originals; name maps checked | XL | — |
| **HZ6** Legacy switch | • only at 11/11: `legacy.csv` renderer for `get_qes_master()`/`get_decon()`<br>• old internals deleted<br>• `get_decon` deprecation message on<br>• NEWS "What changed" table; spec 1.0.0 | HZ5; OD4-OD7, OD15 | `compare_legacy.R` table reviewed by the owner | M | **0.7.0, CRAN** |
| **HZ7** Live validation | • `live.yml` V-L1 to V-L5 with verified benchmarks<br>• Validation article | HZ6; R3, R5 | V-P3; V-L2 baselines recorded | M | 0.7.x |
| **HZ8** Breadth | issue targets one at a time (MINOR), MEDW 2012, CROP 2013-14, Durand 2008/2011 federal panels, a `qes2026` slot | HZ7; R7 | per-study runbook (§11.1) | XL | 0.8.x |
| **HZ9** Deposit (conditional) | harmonized cumulative file on Borealis, split by licence | written permission (OD14) | — | L | later |

**Release rules:**
- A `get_qes_master()` that silently covers only some studies through the engine never ships.
- A known-wrong value in the master never reaches CRAN: in 0.5.0 it is either removed or blanked.
- In 0.6.x, `get_qes_master()` and `qes_harmonize()` can give different values for the same concept. The master's docs and `legacy_column_map` say which is interim.

### 11.1 Adding a study (0 lines of R)

1. Add rows to `catalog/files.csv`, `catalog/studies.csv`, `harmonize/waves.csv` (with disposition-based membership and target population) and `harmonize/weights.csv`.
2. Run `Rscript data-raw/add_study.R <code>`. It:
   - verifies md5 and `n_rows`;
   - writes the dictionary and a worksheet per core target (variables, labels, codes and counts, ranked by exact alias hits, with `derived_from` candidates flagged);
   - appends crosswalk rows with **empty** target codes and `status = draft`.

   It never pre-fills a mapping.
3. Human review: pick the item from the questionnaire, map the codes, and fill in the grade, reasons, `instrument`, `dk_offered`, `levels_offered`, per-code gate outcomes and `evidence`; then set `stable`. A second reviewer is needed for `identical` rows that pool fielding languages.
4. Run `build_sources.R` and `project_marginals.R`, add a CHANGES row (MINOR), and bump SPEC. Spec-check and live CI must be green.

---

## 12. Judge scores and decision record

### 12.1 Scores

Judges: P = political scientist, C = CRAN maintainer, U = user ergonomics. Each scored out of 50, for a total out of 150.

| Track | Proposal | Total | P + C + U (where reported) | Role in this design |
|---|---|---|---|---|
| Harmonization | maintainer-first | **146** | 48 + 49 + 49 | Base: validator in four places, projection, semver bump rule, single gate, `fn:` budget, runbook, generated docs |
| Harmonization | crosswalk-minimal | 144 | 46 + 50 + 48 | `.canon()`, closed affine grammar, no pass-through, [A:N1]-[A:N3] rows, licence caution, SPEC hash test |
| Harmonization | analyst-first | 138 | 45 + 47 + 46 | explicit data labels, QES-only default, `qes_design()`, `source_row`, `qes_join_raw()`, construct tests |
| Harmonization | validity-first | 134 | 46 + 44 + 44 | graded words with reasons, weights registry, `election_ref`, `eligible_voter`, `dk_offered`, universe identities, DI baselines, `qes_splice()` |
| API | ces-familiar | **144** | 47 + 49 + 48 | Base: `get_qes()` as the only data verb, frozen signatures, `qes_codebook` promoted |
| API | cran-lean | 143 | — | `qes_demo`, atomic writes, WAF refusal class, no `...` |
| API | tidy-consistent | 140 | — | condition tree with `parent`, cache marker, structured universe, spec md5 in VERSIONS |
| API | metadata-rich | 132 | — | label-source tracking, the qes2012 Stata-plus-donor pin, malformed-label handling |

The integrated design was not re-scored. Revision 2 instead went through two critic reviews:
- harmonization validity: 23 findings, all checked offline against the cached data;
- API coherence, CRAN and slicing: 42 findings.

§12.4 records the outcome.

### 12.2 Cross-track conflicts resolved here

| Conflict | Track (a) | Track (b) | Chosen | Why |
|---|---|---|---|---|
| Harmonized unit name | `targets` | `concepts` | **`targets`** | The engine track owns the spec. retroharmonize also uses `*_target`. |
| Comparability metadata | grades `identical/comparable/approximate/not_comparable` + EN/FR reasons | `comparability` (4 values) + `confidence` + note | **grades** | One scale; `min_grade` filters on it; U found worded grades clearest. |
| Spec file layout | `inst/harmonize/` (9 CSVs + `sources/`) | `inst/extdata/crosswalk/` (3 CSVs) + `catalog/qes_weights.csv` | **`inst/extdata/{catalog,dict,harmonize,demo}/`**. Waves and weights sit in `harmonize/` (spec-versioned); studies, files, elections, names and enums in `catalog/`. | One tree. Each file has one owner. The spec directory stays freezable. |
| Crosswalk format | `crosswalk.csv` + shared `valuemaps.csv` + gate columns | retroharmonize-named variables/values files + `universe_var` | **track (a) format**, with a `qes_spec("crosswalk", format = "retroharmonize")` export | Shared maps (guarded by V-D3) cut review load. Export keeps interop. |
| Offline aggregates | `sources/<file>.codes.csv` | `n` in dictionary values, `n_orig` in crosswalk | **dictionary `n`** + `gates.csv` | One count table; no duplicated per-code counts. |
| qes2012 pinned file | SPSS twin, keys `Q25` | Stata twin + SPSS label donor | **Stata + donor**; crosswalk keys lowercase `q25` | Keeps v0.4.4 names used by the paper. V-D3 still works because `.norm_label()` lower-cases. Row order is identical (verified). |
| ID declaration | `id_vars` in `files.csv`, `;`-list | `respondent_key` in `studies.csv`, `+`-joined | **`id_vars` in `files.csv`, `;`** | IDs belong to a file; one list separator everywhere. |
| Row identifier | `source_row` | `row_in_source` | **`source_row`** | Shorter; the join key for `merge()` and the internal `.qes_join_raw()`. |
| Weight columns | `weight` + `weight_<wave>` | `weight_pre`/`weight_post` + `_var` + weight guide | **`weight_pre`/`weight_post` (respondent), `weight` (long)**, plus weight guide and timing message | Wave names differ by study (`cps`, `post`); timing names pool cleanly. |
| Weight default | normalized mean 1 | raw | **normalized** in `qes_harmonize()`; **raw** in the legacy master (OD6); equal study totals only via `qes_design(pool = "equal")` | Raw pooled scales (CROP 6.04) are a trap in new code; the legacy column keeps its meaning; one place for pooling. |
| Trimmed weights | `weights = "trimmed"` | `weights_other` column | **merge by `source_row`** (internal `.qes_join_raw()`, OD17) | No argument for a one-study feature. |
| Layout argument | `layout = respondent/long` | `panel = primary/long` | **`layout`**; respondent rows carry targets from all waves | Intent (CPS) and recall (PES) on one row; the legacy master agrees. |
| Default studies | QES election studies | all with coverage | **QES election studies** (`studies = NULL`); `"all"` opt-in | Avoids CROP dominating pooled estimates ([A:V1]/[A:V2]). |
| Label language | `labels = "en"` for data; option for inspection functions | `lang`; returned text never follows the locale | **`lang` everywhere, fixed defaults**; option and locale for messages only | One rule (P4). |
| Missing encoding | `missing = na/reasons` | `missing = na/tagged` | **reasons** for harmonized output; **tagged** only in `qes_missing()` on raw data | `tagged_na` works only on doubles; harmonized targets are factors. |
| Missing vocabulary | 14 reasons | 10 types | **one merged vocabulary** in `enums.csv` (§5.2) | Dictionary, spec and `qes_missing()` share it. |
| Strictness | `unmapped`, `on_fail` | `strict` | **`unmapped` + `on_fail`**; legacy `strict` maps to `on_fail` | Separates bad data from bad downloads. |
| Custom spec | `spec = dir / qes_spec` | `crosswalk = table / path` | **`spec`** | Covers freezing and extension with one argument. |
| Raw extras | `qes_join_raw()` + `keep_source` | `keep = list(...)` + `keep_source` | **post-hoc join** (internal `.qes_join_raw()` for now, OD17); companion `<target>__src` | Post-hoc joins avoid a list argument. |
| Spec views | `qes_targets`, `qes_coverage`, `qes_crosswalk`, `qes_describe` | `qes_crosswalk(level = concepts/variables/values)` | **`qes_spec(view = )`** (revised by OD17): `"targets"` (with coverage columns), `"crosswalk"` (print gives the reference section), `"spec"` | One function instead of four. |
| Spec validation export | `qes_spec()` + `qes_validate_spec()` | none | **`qes_spec(view = "spec", validate =)`** | One export, shared with the spec views. |
| Offline examples | `qes_synthetic()` exported, in memory | `qes_demo` shipped `.sav` in its own shards | **`qes_demo`** for all examples; `.qes_synthetic()` internal for tests | `qes_demo` exercises the full reader; one public mechanism. |
| `get_qes_master()` status | soft-deprecated | stable | **stable** (OD1) | Constraint 1 names it with `get_qes()` as fixed-underneath. 3 paper calls. |
| First CRAN release | 0.5.0 with surgical deletions | only after harmonization wiring | **0.5.0 with deletions plus blanking of verified-invalid cells** (OD2), then 0.6.0 engine, 0.7.0 legacy switch | Never ships a known-wrong value or partial engine coverage in the master. |
| Legacy `political_interest` | `10 × interest_01` (derived) | NA for 2022 | **legacy renderer only** (OD7); no pooled target | The legacy column stays populated; the new API never rescales across instruments. |
| Legacy `survey_weight` | post-wave recommended, mean 1 | v0.4.4 source, raw | **v0.4.4 source, raw** (OD6) | No silent change of meaning. |
| Imports | none new | curl in, xml2 out | **curl, haven, jsonlite** (xml2 out at S3) | DDI has no question text (0 `qstnLit`); curl exposes status and headers in one request. |
| CSV loader | `encoding = "UTF-8"`, `na.strings = character()` | `fileEncoding = "UTF-8"`, `na.strings = "NA"` | **track (a)**, plus `keep_empty` columns | Avoids the C-locale truncation. Keeps genuine `""` labels. |
| Golden values | `expected/marginals.csv` (offline) | golden hashes per column (live) | **both**, in `harmonize/expected/` | Marginals run on CRAN; hashes cover `fn:` rows, weights and 2022. |
| Deprecation wording | `qesR_deprecated`, `qesR.silence_deprecation` | `qesR_message_superseded`, `qesR.quiet_superseded` | **`qesR_message_deprecated`, `qesR.quiet_deprecated`** | Uses the owner's term; follows the `qesR_message_*` tree. |
| Reference vignette | CRAN vignette, generated | pkgdown article | **CRAN vignette** (EN/FR, generated, offline), shipped with the engine | Executes code (reviewer request 3); cannot drift. |

### 12.3 Rejected alternatives

1. A second data verb `qes_data()`. It would duplicate `get_qes()` (P, C, U).
2. Trailing `variables`/`version` arguments on `get_qes()`. They would break "keep call signatures".
3. Regex on labels, whether for recoding or as validation anchors; name- or label-based drafted mappings; automatic mapping discovery.
4. One pooled `vote_choice`, `sovereignty` or 0-10 interest in the new API; binning the 2022 slider into 4 points; a cross-instrument `rescale` rule; lean-pushed and unpushed intention in one target.
5. Composite-key truth tables; a universe-expression grammar; JSON in CSV cells; `eval()`.
6. Deduplication or empty-row removal of any kind.
7. tibble in Imports. A print method covers usability.
8. Locale- or option-driven data labels or metadata text.
9. An in-package spec archive; a PATCH bump for value corrections; CalVer.
10. A third master-like name (`qes_master()`).
11. Repurposing legacy `income` as a rank or midpoint.
12. A pooled cross-study weight or benchmark calibration by default.
13. LRU cache pruning and usage logs; an on-disk parsed-data (RDS) cache; an interactive disk-cache prompt.
14. webfakes plus mocks (two test mechanisms); rlang/cli conditions; gettext/po.
15. Runtime DDI, PDF or DOC scraping; runtime xml2.
16. Automatic failover to the Borealis 2022 deposit. It has a different UNF, so it is different data.
17. A demo row inside the real catalog; shipped synthetic `.sav` files per real study.
18. `knit_child`/`_strings.csv` for FR vignettes; full French Rd pages.
19. Three CI workflows plus an issue-opening bot.
20. Switching the legacy master to the engine after 4 flagship studies.
21. A `region4` target built from the 17 administrative regions.
22. Membership rules on substantive items or on non-missing weights.

### 12.4 Critic review of revision 1: decision record

Every finding was checked before it was applied. The validity findings were re-run offline (`scratchpad/design/apply-critics/v1.R`, `v2.R`, plus the 2018 questionnaire text and the 2014 Léger note), and the coherence findings were checked against `R/` and d1faad6. **No finding was rejected outright. Two were applied with a different fix (CV6, CV16). Where a critic offered alternatives, the choice is noted (CC-A8, CC-B2, CC-B5, CC-F5).** Two grades came out differently from the critic's proposal: CV2 is `comparable`, and CV22 stays `needs_review`.

**Validity findings (CV1-CV23)**

| # | Finding | Outcome |
|---|---|---|
| CV1 | 2018 `q27` is not reversed; its stem adds "enjeux publics" | Accepted (verified). "Reversed" notes deleted; graded `comparable`. |
| CV2 | 2014 PID `Q55`/`Q56` and 2018 income `q61` exist | Accepted (verified). Rows added; R6 and Q-c closed. `q61` graded `comparable`, not identical: it has no DK, while 2012 offers one. |
| CV3 | 2018p `independance` is a recode of `rts_q7` | Accepted (exact crosstab verified). `rts_q7` only; `derived_from` recorded. |
| CV4 | 2007p membership via `resultat`/`resultat_pst`; 391 refusal conversions | Accepted (2,050/2,054/1,663 = `questpost`; 734B = 392 verified). `subsample_var` added. |
| CV5 | 2018p pre wave missing | Accepted (`weight` mean 1 and CATI pattern verified). |
| CV6 | 2022 intent universe gated on `cps_turnout` | Accepted (89 = 62 + 26 + 1 verified). **Different fix:** code 5 ("I already voted", n = 1) → `inapplicable`, not a new `already_voted` reason, because one case does not justify a vocabulary entry. The `cps_votelean` code swap is recorded. |
| CV7 | `pes_turnout` 6 = don't remember; 5 = not registered, not ineligible | Accepted (labels verified). OD12 revised; `not_registered` added. |
| CV8 | Non-voters get different NA reasons by study | Accepted. Per-code `gate_to` everywhere. |
| CV9 | 2014 `Q3`/`Q2` offer no DK, so they are not `identical` | Accepted (verified). Graded `comparable`. V-S17 now enforces the rule mechanically. |
| CV10 | 2022 `lr_self` is approximate | Accepted (−99 = 34; 2012 DK/refused 301/1,505 verified). |
| CV11 | "Pays souverain", 1998 `voteref` and push items mixed into `sov_*` | Accepted (1998 `voteref` n = 426 verified in the DDI). `sov_sovereign_country` added; push items kept separate; CROP `intvoterefa` ungraded. |
| CV12 | 1998 is francophones only and two firms; its recall exists | Accepted. `target_population`, `firm` strata, V-L2 skip; legacy `vote_choice` filled from `q3post` at 0.7.0. |
| CV13 | 1998 `intvote2` labels shifted in source | Accepted (DDI counts verified). `not_comparable`, with a dictionary `label_flag`. |
| CV14 | `region4` cannot be built from admin regions | Accepted. `region_cma3`/`region_mtl2`; admin → CMA mappings are `approximate`. |
| CV15 | 2018p age has 3 bands; minors = 251 | Accepted (verified: 110 + 141). `age_group3` added; OD15 added. |
| CV16 | Offered party levels differ; code-3 collision | Accepted, **with a different fix** for part of it. `levels_offered` per crosswalk row, V-S16, and structural zeros at cell level and in print. The proposed respondent-level NA reason `not_offered` was **not** adopted: a respondent who chose "another party" gave a real answer (`other`), and no respondent value is missing. The code-3 collisions forbid shared maps. The lineage helper was added (OD11). 2007 `q12` = 97 → `spoiled` pending R1. |
| CV17 | `dk_refused`, `not_registered`, `no_party` | Accepted. |
| CV18 | Push variants on the wrong targets; 2022 intent `comparable` | Accepted. |
| CV19 | 2018 `q6` codes 95/96 | Accepted, with stronger evidence: the 2018 questionnaire itself lists 95 = "J'ai annulé mon vote" and 96 = "Un autre parti". |
| CV20 | 2022 PES membership via `pes_StartDate` | Accepted (1,220 = 1,220 verified). The weight-based exception is removed. |
| CV21 | 2012p file is two-wave only; dates known | Accepted. Dates entered; R4 narrowed. |
| CV22 | Education in years; weight corrections | Accepted (2014 Léger note, 2008/2012p DDI labels and 2007p labels verified). The 2008 `pond` stays `needs_review` until its original is seen (R1). |
| CV23 | PID answer lists differ | Accepted (2012/2014/2018/2022 lists verified). `comparable` at most; `none` level. |

**Coherence findings (CC-A1 to CC-F7)**

| # | Finding | Outcome |
|---|---|---|
| CC-A1 | The caller frame is lost through aliases | Accepted. `.f_impl` wrapper rule (§2.3). |
| CC-A2 | Opt-in also assigns `<srvy>_codebook` | Accepted (verified in R/get_qes.R). The canonical code is used for both. |
| CC-A3 | NEWS entry for the opt-in frame change | Accepted. |
| CC-A4 | Empty-row removal still drops rows | Accepted (verified at master.R:1147/1824). Deleted in S4. |
| CC-A5 | §2.3 mixed 0.5.0 and end state | Accepted. Split columns; the interim blanking is extended conditionally on OD4/OD5. |
| CC-A6 | Interim appended columns vanish later | Accepted. Carried into 0.7.0. |
| CC-A7 | Legacy adapters, and deprecation timing | Accepted. `get_decon` is deprecated from 0.7.0; `.qes_deprecated` registry added. |
| CC-A8 | One Rd page for 11 functions loses docs | Accepted. 9 legacy pages, grouped only where the arguments agree. |
| CC-A9 | Legacy arguments whose meaning disappears | Accepted. `qesR_message_arg_ignored`; NEWS. |
| CC-A10 | `variable_name_map_path` dropped silently | Accepted. |
| CC-A11 | Allowed formals diffs must be explicit | Accepted (verified in d1faad6: TRUE only in `get_qes`, `get_qes_master`, `get_decon`). |
| CC-B1 | ID namespace collisions | Accepted. Prefixes in the header table. |
| CC-B2 | Argument order violated | Accepted. **Reordered** before first release; `quiet` added to `qes_studies()`. |
| CC-B3 | Duplicated enums | Accepted. `enums.csv`; `study_design`/`wave_design`, `target_timing`/`wave_timing`/`var_timing`; one `label_source` enum. |
| CC-B4 | Weighting controls duplicated | Accepted. `equal_study` removed. |
| CC-B5 | `qes_design(weight = NULL)` errors on core | Accepted (option 1). Static/any targets are exempt, and the vignettes pass `weight`. |
| CC-B6 | Name spaces unconstrained | Accepted. V-S15. |
| CC-B7 | `study` vs `studies`; `"all"` | Accepted. |
| CC-B8 | `qes_codebook(<data.frame>)` underspecified | Accepted. Precedence list in §6.2. |
| CC-B9 | `values = "labelled"` | Accepted. Returns `haven::labelled`. |
| CC-B10 | Rd count; the null-default operator needs R 4.4 | Accepted. Recounted (32 in revision 2; 28 after OD17); `.or_default()`. |
| CC-C1 | Static grep cannot run in `R CMD check` | Accepted. Namespace body scan on CRAN; grep, chunk parity and V-P3 in CI. |
| CC-C2 | `--as-cran` runs `\donttest{}` | Accepted. One metadata-only network example. |
| CC-C3 | The privacy test is fragile | Accepted. |
| CC-C4 | CITATION must not call internals | Accepted. Static and generated, with an equality test. |
| CC-C5 | cran-comments wording | Accepted. |
| CC-C6 | User cache directory marker | Accepted. `qesR/` subdirectory. |
| CC-D1 | xml2 removal breaks codebooks until S3 | Accepted. xml2 out at S3. |
| CC-D2 | Dependency record | Recorded in cran-comments. |
| CC-D3 | Accent folding is locale-fragile | Accepted. `intToUtf8()` table; fold before lower-casing. |
| CC-E1 | No offline seam for real study codes | Accepted. `.qes_catalog()` fixture seam, and the demo also runs the legacy master. |
| CC-E2 | `qes_demo` status | Accepted. Parallel `demo/` tree. |
| CC-E3 | Once-per-session state makes tests order-dependent | Accepted. `.qes_once` plus `local_qes_once()`. |
| CC-E4 | `quiet` vs notices | Accepted. |
| CC-E5 | `data =` shape | Accepted. A named list. |
| CC-F1 | Slices silently depend on R1/R2 | Accepted. Merge and release gates split; 0.5.0 waits for R1/R2. |
| CC-F2 | Regex master on a changed reader | Accepted. Frozen `legacy_source_map.csv` (needs R9). |
| CC-F3 | Lanes not parallel | Accepted. Loader, enums and NA vocabulary moved to S1; graph redrawn. |
| CC-F4 | S0/S2 too large | Accepted. Split into S0a-c and S2a-c. |
| CC-F5 | Version numbering | Accepted. **Chose** 0.5.0 / 0.6.0 / 0.7.0; the reference vignettes move to HZ3. |
| CC-F6 | S4 depends on owner decisions | Accepted. OD4, OD5, OD8, OD9 and OD15 are listed. |
| CC-F7 | Insecure code deletion timing | Accepted. S0c. |

---

## 13. Risks, open questions, data requests

### 13.1 Risks

| Risk | Mitigation |
|---|---|
| Review bandwidth: about 40 targets × 13 studies gives roughly 400 crosswalk rows and 2-3k map rows for one maintainer; revision 2 adds `levels_offered` and per-code gates | Worksheets; shared `map_id`s guarded by V-D3; phasing; `status` shown in `qes_spec()`; V-S16/V-S17 automate part of the grading rule; a second reviewer only for cross-language `identical` rows |
| qes2018 has no file labels, so questionnaire and data could disagree | V-S8, V-S14, V-S16, V-D7, V-P1/V-L1 and V-L4; the questionnaire text is already cached and was used for `q6`, `q27`, `q56` and `q61` |
| A source file's own labels are wrong (1998 `intvote2`) | V-D3 cannot catch it. Cross-checks of counts against sibling variables are part of review and of `add_study.R` worksheets. |
| Label-text changes alter `as_factor()` output (2012 case and length, 2018 labels, 2022 dates) although codes are identical | NEWS lists the variables; the paper keeps its pin until it is edited |
| Spreadsheet damage to spec CSVs (CP1252, BOM, stripped zeros) | V-S7; `.gitattributes`; edit in LibreOffice or R |
| Dataverse republishes or deaccessions a file | V-D6/V-L5 fail loudly; the maintainer re-verifies and bumps the catalog |
| The Harvard WAF extends to `/api/access` | Classed refusal, cache, md5-verified manual seeding |
| Attributes lost through `rbind`/`merge` | Documented; `qes_provenance()` errors with a hint |
| "Harmonized" is not "comparable" (mode, question order, design family, target population) | `survey_mode`, `family`, `target_population`, structural zeros and grades in the output and docs |
| The interim master (0.5.0) runs old code on the new reader | Sources frozen from the R9 baseline; the S4 gate requires `compare_legacy.R` to show only intended differences |
| 0.6.x has two harmonized answers (interim master vs engine) | `legacy_column_map` and the master's docs say the master is interim until 0.7.0 |
| `curl` needs system libcurl on Linux source installs | It is ubiquitous; noted in `cran-comments.md` |
| A reviewer asks for "fail gracefully" | Offline examples on `qes_demo`; one guarded network example. A classed error is preferred to `invisible(NULL)`, which recreated the silent `tableA1` failure. |

### 13.2 Open questions (factual, answered by data requests)

- **Q-a (narrowed).** 2007 panel post-wave weight: which of `pond`, `pondam1` or `ponderation_totale` applies to the 2,054 post-wave completes? `pond_tot_am1` covers pre-only cases, so it is not one. Membership itself is resolved (§5.3). (R4)
- **Q-b.** Resolved: `independance` is a recode of `rts_q7`.
- **Q-c.** Resolved: 2014 PID is `Q55`/`Q56`; 2018 income is `q61`. The 2014 federal PID location is still to be read from the questionnaire already in `codebooks/`.
- **Q-d.** Do the qes2007/qes2008 twins differ only in labels, or also in data (different UNFs)? (R1)
- **Q-e.** CROP 2007-10: full wording of `intvoterefa`, and whether `QP4` refers to the 2003 or 2007 election in each monthly wave. (R2)
- **Q-f.** 1998: was "francophone" defined by mother tongue or interview language? (R2)
- **Q-g.** 2012 panel: is `pondam1` a pre-wave weight, and how does it relate to `pond_post`? (R4)

### 13.3 Data needs (policy OD18)

**Who gets what.** Claude fetches every public item below itself, with plain requests that carry no personal information (User-Agent exactly `qesR/<ver> R/<ver>`, sequential, cached, at least 1 s apart, backoff on 429/503 and `Retry-After`), and records each file with its source URL and md5 in the scratchpad `data/MANIFEST.md`. The owner is asked only for:
- permissions (R8);
- documents that are not public;
- any item that a plain request cannot fetch (login wall, WAF refusal, terms of use). Such an item is not worked around; it is recorded here as a request for the owner.

When this section was written, nothing below had been fetched. The item texts keep the original IDs so that references stay stable.

- **R1. Original files (`?format=original`)**, public on Dataverse; Claude fetches them. md5s from the cached dataset JSON:
  - qes2007 425921 (SPSS, `e1324bbb582cf6bc01eb9949ecb8b132`) and 425922 (Stata, `d2355b5d400b16cc999be0a5701463a7`);
  - qes2008 425919 (SPSS, `6a69b00b943bb357c3ed10091d936ba9`) and 425920 (Stata, `192547c91c1f1a86b5dcbaf2ee19c511`);
  - qes2012_panel 361043 (`8f90a9f97a33e01eff3748f05612e26c`);
  - CROP 2007-10 329990 (`534514c7414d462259de4421bb4579e5`);
  - 1998: 329987 panel (`a2a2b2fef6a2edb202beae0e63e59f0c`), 316121 CREATEC (`1aba610f71f1b5c46e03da2036e83330`) and 286331 CROP (`ceec0332eaa0eab117e4bcf5c4d8d672`).

  Purpose: twin choice, name-map checks, user-missing declarations, CP850 confirmation, `n_rows`, the dictionary, and 2007 `q12` = 97. **This is a hard prerequisite for S2b's release gate, S4 and S5 (0.5.0), not only for HZ5.**
- **R2. v0.4.4 `get_qes()` column names** (name manifests) for qes2008, qes2012_panel, qes2007_panel, qes2018_panel and CROP. Claude builds them by running d1faad6 in a temporary library, after confirming that its requests carry no personal information (if they do, only the User-Agent is patched in that temporary copy). Also these questionnaires, fetched by Claude where public and requested from the owner otherwise:
  - **CROP 2007-10:** the full `intvoterefa` wording and the `QP4` reference election per wave (Q-e);
  - **1998:** the CREATEC questionnaire, the definition of "francophone" (Q-f), and the weighting documentation per firm (`poids`, `ponder2`, `ponder3`, `ponderc`).

  Without full wording, these rows cannot be graded above `approximate`. R2 is also a prerequisite for 0.5.0.
- **R3. Official DGEQ results** (party shares of valid votes and turnout) for 1994-2022, from DGEQ or doi:10.7910/DVN/PNANLW; public, Claude fetches them. **Every benchmark figure in both track designs was recalled from memory**; none is entered until verified.
- **R4. Weighting documentation (narrowed)**, from the public deposits and technical reports where they exist; the owner is asked only for documents that are not public:
  - the 2007 panel post-wave weight (Q-a);
  - 2012 panel `pondam1` vs `pond_post` (Q-g);
  - 2012 QES `pond` (which margins);
  - CROP `XPOND`.

  Fieldwork dates and modes for the 2018 panel pre wave (there is no date variable in the file), qes2007, qes2008 and qes2012. The 2012 panel and 2007 panel dates are now known.
- **R5. Census margins** (StatCan, Quebec 18+ and 16+: sex × age, mother tongue, education) for 2006, 2011, 2016 and 2021, for margin tests where no published targets exist. Public; Claude fetches them.
- **R6.** Resolved (2014 PID `Q55`, 2018 income `q61`). The ID is kept so references stay stable.
- **R7.** MEDW 2012 (10.7910/DVN/06OOVN), CROP 2013-14 (10.5683/SP3/AQYTPS) and Durand 2008/2011 federal panels: files, questionnaires and encodings (slice HZ8). Claude fetches what is public.
- **R8. Permissions (not data); the only standing requests for the owner:**
  - C-Dem PIs, on redistributing qes2022 labels, wording and aggregates (OD3);
  - QES/CECD and C-Dem, on a harmonized deposit (OD14).
- **R9. A clean v0.4.4 legacy baseline.**
  - **What to run:** at d1faad6, in a fresh session with `LANGUAGE=en`, run `get_qes_master(assign_global = FALSE)` and `get_decon()` for each study it supports.
  - **What to save:** the returned objects' `source_map` attribute, `table(qes_code)` and column names, or the full `.rds` kept locally and never committed.
  - **Why the existing builds do not qualify:** the tracked `qes_master.csv` was built by older code, and the fresh 39,132-row build ran under French messages and lost panel rows ([A:D10]).
  - **What depends on it:** the frozen `legacy_source_map.csv` (S4) and the `compare_legacy.R` gate.
  - **Who:** Claude builds it in a temporary library and a fresh session (same privacy check as R2), writing only to the scratchpad. The owner is asked only if a plain request cannot fetch a file.

### 13.4 Owner requests from the slices

Local actions that the binding rules leave to the owner. Nothing here blocks a slice.

- **S0b cleanup (§10).** Delete the ignored, unused local files `qes_master_test.csv`, `qes_master_test.rds` and `qes_master_test_source_map.csv` in the repository root. They stay on disk until the owner confirms. Run `git fetch` to refresh the stale `origin/gh-pages` ref, and optionally `dot_clean -m .` to remove macOS `._*` files (they are git- and build-ignored, so they are harmless).
- **Local waldo (S0b, resolved).** An earlier session saw waldo 0.5.2 fail to load next to glue 1.8.0, which broke tests under `R CMD check` only. The system library now has waldo 0.6.2, and `R CMD check --as-cran` passes the tests with the default library (336 pass, 0 fail, 18 skip). Nothing to do.
- **Local HTML validator (S0b, optional).** `/usr/bin/tidy` is Apple's 2006 build and predates HTML5, so `R CMD check` adds a NOTE ("`<main>` is not recognized") for every Rd. Installing tidy-html5 (for example `brew install tidy-html5`) removes it. CRAN does not see this NOTE; it is explained in `cran-comments.md`.
- **Paper and the S0c assignment default (checked, nothing to do).** Every `get_qes()`/`get_qes_master()` call in the owner's paper scripts already assigns the result (`qes2012 <- get_qes("qes2012")`, `d <- get_qes_master()`), so the new `assign_global = FALSE` default does not change them. `build_chapter_figures.R` guards with `if (!exists("qes2022"))`, which also still works. The one-time `qesR_message_assign_default` note appears once per session in their output; `suppressMessages()` (already used in one script) or passing `assign_global = FALSE` hides it.
