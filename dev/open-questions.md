# Open questions for the owner

Gaps that the public deposits do not settle, recorded as the slices meet
them. Each item says what was done meanwhile (always the conservative
choice: an undocumented weight is registered but not applied, an
undocumented item is left unmapped or graded down) and what would close it.
Nothing here blocks a slice. Data requests follow the policy of design.md
section 0.3 (OD18): Claude fetches public documents itself; the items below
are either not in any public deposit or need the owner's judgement.

## Slice HZ7: live validation (qesR 0.7.0.9000, spec 1.0.0 unchanged)

Sources used: Élections Québec's archived results files of the general
elections of 1998 to 2022 (R3) and Statistics Canada census tables
98-10-0020-01, 98-10-0218-01 and 98-10-0384-01 (R5), all fetched with plain
requests (scratch `data/MANIFEST.md`); the pinned originals (offline cache);
the 2018 methodological report (file 361045) for the weighting cells.
`qes_studies(check_updates = TRUE)` was run once (13 metadata requests,
User-Agent `qesR/0.7.0 R/4.4.0`): every deposit is `current`.

### A. Decisions taken in the slice that the owner may want to change

V1. **Age cuts where 18+ is not published.** Gender 18+ and six age bands
    18+ only for 2016 and 2021 (single years, 98-10-0020-01). For 2006 and
    2011: gender 20+ (98-10-0384-01, private households; 2011 from the
    voluntary NHS) and five age bands from 25 (the tables have 15-19 and
    20-24 bands, so 18-24 cannot be formed). Mother tongue 20+
    (98-10-0218-01, outside institutions, single answers only; no 2006
    column). Education 25+. Respondents are cut the same way where their
    age is known (the `age` target, else election year minus year of
    birth, one year high for those born after election day; else the
    `age_group6` bands). Closing it needs a custom tabulation or the 2011
    topic-based table 98-311-XCB2011018, which a plain request could not
    fetch (MANIFEST, NEEDS OWNER 1). *To confirm.*
V2. **Education is compared as university or below**, not in the four
    levels of `education4`. The census counts the certificate completed;
    the surveys ask the level reached (a year of CEGEP or university
    counts), and a trades certificate (DEP) is secondary in 2018 and
    technical (college) in 2014. A three-group comparison gave 18-37 point
    indices driven by that boundary; with two groups `qes2022` (weighted on
    education) is 3.5 points from the census and the others 9-23. *To
    confirm.*
V3. **Census year.** The census on or before the study's latest year, for
    every row of the study (the CROP polls, 2007-2010, all get 2006; a
    pooled study whose rows would map to two censuses stops with an error
    rather than compare its early rows with the later census): 2006 for 2007-2010,
    2011 for 2012 and 2014, 2016 for 2018, 2021 for 2022. Mother tongue has
    no 2006 benchmark, so `qes2007`, `qes2008`, the 2007 panel and CROP have
    no mother-tongue comparison.
V4. **What is gated.** The weighted V-L2 indices (the design) and, beyond
    the design text, the weighted census indices, both at the recorded
    value + 2.0 points. Unweighted rows are recorded for information only
    (the studies whose weights are `needs_review` have only these). The
    turnout rule of section 8.3 (fail outside 0-35 points) applies to every
    row. V-L4 thresholds are those of section 8.3; `pid_prov` agreement is
    computed among partisans (a party, not "none") who reported a party
    vote, which gives 78-87% rather than the provisional 70-72% (the
    denominator of the provisional figure is not recorded). A
    newly reviewed weight gives a gated row with no record: the live test
    fails until `data-raw/build_validation.R` records it.
V5. **Where the report lives.** `inst/extdata/validation/validation_report.csv`
    (with the benchmark tables), not `inst/validation/` as section 8.3
    says, so that every shipped table is under `inst/extdata/`. qes2022
    rows are in `data-raw/nc/validation_qes2022.csv` (build-ignored, OD3)
    and in the CI artifact.
V6. **The website article shows qes2022 aggregates**, computed at build
    time from the downloaded file, as the existing analysis articles do;
    nothing of qes2022 ships in the package. *To confirm* while OD3 is open
    (the article can drop qes2022 rows if the owner prefers).
V7. **Version.** DESCRIPTION is 0.7.0.9000 with a NEWS section of its own,
    as in HZ5; nothing that qesR returns changed.

### B. Findings for review (no change made)

F1. **qes2018 mother tongue: English 16.7%, other languages 6.7% (weighted),
    against 7.3% and 14.0% at the 2016 census (20+).** The map follows the
    questionnaire with programmed answer values (file 367181: 1 French,
    2 English, 96 other) and the French share is right (V-L3). Table 14 of
    the methodological report calibrates "Anglais" to 22.9%, which is the
    whole non-French share, so the weight does not correct the split
    between English and other languages. Either the online panel
    over-represents English speakers, or codes 2 and 96 are swapped in the
    file. The deposit cannot settle it; the article describes the first
    reading. *Owner's judgement.*
F2. **qes2018 weighted education.** `pond` gives 32.4% with a university
    degree (codes 14-15), Table 15's "Universitaire", so the weighting
    counted incomplete university (code 13) outside university, while
    `education4` counts it as university (the level reached). Not a map
    error; recorded for the education grading.
F3. **qes2018 weighted age bands** are 4 to 5 points off the 2016 census
    for 35-44, 45-54 and 55-64: the weight's age cells (Table 12: 16-18,
    19-38, 39-58, 59-73, 74+) cut across ten-year bands. Not a map error.
F4. **V-L2 baselines of design section 8.3 confirmed** on the official
    figures: 8.0 (`qes2012`), 5.6 (`qes2014`), 3.2 (`qes2018`), 8.0
    (`qes2022`, recorded in `data-raw/nc/`). In 2022 the CAQ is
    under-reported by 8.0 points and every other party over-reported.

### C. Deferred (other slices)

- Licence terms of the benchmark sources (licence pages fetched
  2026-09-27, one plain request each). Statistics Canada: the Open Licence,
  named in `inst/COPYRIGHTS` with its "Adapted from Statistics Canada ...
  This does not constitute an endorsement" notice; no question left.
  Élections Québec: no open licence is published for
  donnees.electionsquebec.qc.ca (its open-data page names none); its site
  terms of use allow reproduction for non-profit purposes with source and
  copyright, and require written permission for other uses and for
  adaptations. `official_results.csv` sums minor parties (an adaptation)
  and qesR is MIT (commercial use allowed). **Owner request before a CRAN
  release:** ask Élections Québec for permission to redistribute these
  province-wide totals in qesR, or drop the file and fetch the results at
  build time. `inst/COPYRIGHTS` states the terms and marks this pending.
- Weighted validation of the studies whose weights are `needs_review`
  (qes2007, qes2008, the panels, CROP, 1998) waits for R4.
- A 2011 age-by-sex benchmark at 18+ (V1) and 2006 mother tongue.
- `age` derived from the year of birth and age groups derived from ages
  as harmonized targets (HZ4's deferral): validation derives them locally
  and does not add them to the spec.
- The panel stability check of section 8.3 (time-invariant agreement and
  intent-to-recall stability on two-wave respondents) is not part of
  V-L1 to V-L5 and was not implemented.

## Slice HZ6: the legacy switch (qesR 0.7.0, spec 1.0.0)

Sources used: the pinned originals (md5-verified, offline cache), the
deposited questionnaires and codebooks, the R9 baseline of qesR 0.4.4 and a
0.5.0 baseline built from commit aa1d3bd (the interim builders, whose values
0.6.0 kept). `dev/legacy-diff.md` (from `data-raw/compare_legacy.R`)
explains every difference from 0.5.0; 0 are unexplained.

### A. Decisions taken in the slice that the owner may want to change

H1. **The legacy builders apply the crosswalk rows in review.** No row of
    the spec is signed off yet (`status = stable`), and `qes_harmonize()`
    applies only signed-off rows by default. `get_qes_master()` and
    `get_decon()` call the engine with `include_draft = TRUE`: otherwise
    every column would be `NA`. The rows were checked against the originals
    and documents, and the legacy columns were always built from unreviewed
    code, so this is no step back; `attr(, "source_map")` gives each cell's
    grade, and `?get_qes_master` says so. *To confirm*, or to sign off the
    rows the legacy profile uses before the release. Choice made in the
    review fixes: the rows stay in review, so the replacement that the
    `get_decon()` notice names (and `?get_decon`, `?qesR-deprecated`, NEWS
    and both migration vignettes) is the working call
    `qes_harmonize(srvy, targets = "decon", include_draft = TRUE)`; a test
    checks that it returns values for `qes_demo`. Once the decon rows are
    signed off, `include_draft = TRUE` can be dropped from those texts.

H2. **Missing categories of `vote_choice`.** The 0.4.4 categories "Did not
    vote / None" and "Don't know / Refused" become `NA` (the engine's
    reasons `not_voted`, `spoiled`, `dk`, `refused`), in all eight studies
    that had them (for example `qes2007_panel`: 561 cells); `turnout` still
    says who voted. The alternative is to render those reasons as the old
    texts (a `recode` of NA reasons in `legacy.csv`), which would also add
    them to `qes2012` and `qes2014`, where 0.4.4 left nonvoters `NA`.

H3. **`gender` keeps "Non-binary" and "Other"** (qes2022, 6 cells), as
    0.5.0 did; design section 5.12 wrote "Non-binary -> NA in the legacy
    column only". Keeping them changes nothing from 0.5.0.

H4. **`age_group` of the 2007 and 2012 panels has six bands** (their
    questions have them; design section 5.12's `age_group6`), where 0.4.4
    and 0.5.0 used the producers' three-band recodes (`age3`, `age_3gr`).
    `qes2018_panel` keeps its three bands (OD15).

H5. **`qes2012` education code 10** (*Certificate and diploma*, 100
    respondents) is in neither questionnaire: `not_mappable` (0.4.4 put it
    in College); code 8 (the French questionnaire's *cours technique*) is
    College (0.4.4: University). The 2018 panel's trade certificate (d3 code
    4, 127) is College/CEGEP/Technical; 0.4.4 left its label as a value.

H6. **`get_decon()` types.** Its columns follow the targets: factors with
    the targets' English levels, `education` in four groups (the qes2022
    file's ten categories are gone), `yob` a number, `religion` and (except
    for qes2022, an amount) `income` text; `age` is a factor of bands for
    the panels and `qes1998`, which asked bands (as 0.4.4 had them), numbers
    elsewhere. `get_decon()` is soft-deprecated; its replacement
    `qes_harmonize(targets = "decon")` gives the same targets with reasons.

H7. **`respondent_id`** of `qes2007_panel` and `qes2018_panel` is the ID
    part of the engine's `qes_id` (`nompn-quest`, `method-id`), as design
    section 5.12 says; the CROP polls keep `QUEST` (0.4.4's choice; the
    engine identifies CROP rows by position, one `QUEST` repeats).

H8. **The 2007 panel's time-invariant items** (gender, education, mother
    tongue, income, six age bands) are read in whichever wave the respondent
    took part (crosswalk wave `*`, allowed from spec 1.0.0 for targets of
    timing `static` or `any`), so its 391 respondents reached only after the
    election keep them, as in 0.5.0. The existing `age_group3` row moved to
    wave `*` too (the MAJOR change of spec 1.0.0), which gives those 391 an
    age group and, for 304 of them, `eligible_voter`. The one row in no wave
    (neither interview completed) is now `NA` in every answer column.

### B. Deferred (other slices)

- `language` of `qes2022` stays `NA`: its mother-tongue question is three
  select-all items (`cps_lang_1..3`), which need the registered
  `fn:multiselect` rule (design section 5.6).
- `income` and `religion` of `qes2018` stay `NA` (as in 0.5.0): the file
  has no value labels, and a `string` row reads the file's labels; quoting
  the questionnaire's labels for text targets needs a schema change.
- `vote_choice_text` stays `NA` everywhere (as in 0.5.0): the "other party"
  text of the recall items is not a target yet.
- `party_lean` stays `NA` everywhere (render `na_column`, as in 0.5.0):
  design section 5.12 maps it to `vote_prov_lean`, which is not a target of
  the spec yet. It is filled when that target and its crosswalk rows are
  added.
- The appended columns `region_cma3`, `region_admin` and `income_rank` of
  design section 5.12 wait for their targets; the appended column is named
  `waves` (the engine's respondent-layout column), not `wave`.
- `qes2014` `Q57` wording keeps the dictionary's text, which carries a stray
  "3." from the questionnaire extraction (the crosswalk wording must equal
  the dictionary's question text); a dictionary patch would fix both.
- The website's analysis articles still read the master (W.2).

## Slice HZ5: the remaining studies (spec 0.3.0)

Sources used: the pinned originals (md5-verified), the deposited
questionnaires and codebooks of each study, and the dataset metadata of
each deposit (all public, fetched earlier; see the scratch MANIFEST).

### A. Documents that are not in the public deposits (R2 part 2, R4)

1. **Weights of qes2007, qes2008, qes1998 and the CROP polls (R4).** No
   weighting documentation is deposited for any of them. They are
   registered in `weights.csv` with status `needs_review`, so
   `qes_harmonize()` returns `NA` weights for these studies until the method
   is known:
   - qes2007 `pond` (mean 1; label *Pondération*);
   - qes2008 `pond` (label *Sans taux de participation*, recommended) and
     `pondx` (label *avec taux de participation*, registered as
     `turnout_calibrated`, never recommended);
   - qes1998: the sample design the weights must correct *is* documented:
     both firms' codebooks (CROP file 332049 and CREATEC file 332050, line
     8 of each) say that « les personnes indécises, qui disaient vouloir
     annuler leur vote ou qui refusaient de révéler leur vote ont été
     sur-sélectionnées », and the description of the CREATEC data file
     (316121) says « La variable de pondération « ponderc » est à
     privilégier par rapport à « poids », « ponder2 » et « ponder3 » » and
     that the reticent (« discrets ») were over-selected. What is not
     documented is which weight corrects the over-selection in the pooled
     file, per wave. The pooled file has four: `ponder3` (mean 1 within
     each firm, registered as recommended for both waves), `ponder2`
     (proportional to `ponder3` within each firm, with values for both
     firms: means 2.17 CREATEC, 10.44 CROP), `ponderc` (the CREATEC
     *Pondération principale*, mean 1 for CREATEC; CROP's rows are on
     another scale, mean 5.16, so normalizing it within a wave would give
     each CROP respondent about five times a CREATEC respondent's weight)
     and `poids` (16 values). All are `needs_review`, so 1998 estimates are
     unweighted and **over-represent the undecided, would-spoil and
     refusing respondents** (refusals alone are 17.5% of CREATEC's Q3 vote intention, `intvotep`, file 332050); the
     weight rows, the waves' notes, the notes of the intention, recall and
     turnout rows and NEWS say so. *Asked of the owner:* which weight
     (`ponderc`, `ponder2`, `ponder3` or `poids`) corrects the
     over-selection in the pooled file, for the pre- and for the
     post-election wave, and whether `ponderc` (preferred by the CREATEC
     deposit) should replace `ponder3` as the recommended weight;
   - CROP `XPOND` (population scale, mean 5.9 to 6.2 in each poll),
     registered once for every poll (wave `*`) and normalized within each
     poll when reviewed.

   *Needed:* the weighting method (variables and margins) of each, or the
   owner's decision to accept one as documented by its file label.

2. **CROP 2007-2010 referendum item (Q-e).** The codebook (file 341537)
   gives only the first words of `intvoterefa` ("Si un référendum avait lieu
   aujourd'hui vous demandant"); whether it asks about a sovereign country,
   an independent country or sovereignty-partnership is unknown, so it is
   **not mapped** to any sovereignty target (nor are the push `intvoterefb`
   and the combination `intvoteref`). *Needed:* the question wording.

3. **CROP 2007-2010 previous-vote item (Q-e).** `QP4`/`voteprec` ("Pour
   quel parti avez-vous voté aux dernières élections") does not say which
   election each poll refers to (2003 or 2007 before the 2008 election; 2008
   after it). Not mapped (the spec has no previous-vote target yet either).
   *Needed:* the reference election per poll.

4. **CROP 2007-2010 fieldwork.** The codebook dates each poll to its month
   only; `waves.csv` records the month's bounds and no interview date. The
   deposit metadata says "2007-01 to 2010-06" and "Panel", while the file
   holds 24 monthly cross-sections from June 2007 to January 2010.
   *Needed:* the fieldwork dates of each poll (optional).

5. **qes1998: definition of "francophone" (Q-f, OD16).** The firms'
   codebooks answer it firm by firm: CREATEC interviewed respondents whose
   mother tongue is French (1,057, file 316121), and CROP's post-election
   wave kept those whose language at home is French (426 of its 450, file
   286331). The pooled file holds 1,057 CREATEC and 426 CROP rows, but no
   variable links its CROP rows to the CROP file, so the pooled definition
   is inferred from the counts, not stated. The waves' `target_population`
   says "Francophone Quebec adults (CREATEC: mother tongue French; CROP:
   French spoken most often at home; the pooled file's definition is
   inferred from counts, unconfirmed)" (and the same in French), and
   `inst/COPYRIGHTS` uses the same wording and keeps the definition marked
   as pending. The catalog row of the CROP firm file (`qes1998_crop`) says
   "interviewed in French": that file holds all 450 respondents of CROP's
   pre-election poll, interviewed in French (codebook 332049), so it is
   left as is. *Needed:*
   confirmation, or the producer's statement.

6. **qes1998: CREATEC questionnaire.** Not deposited. The wording shipped
   for the pooled items is CROP's (its codebook prints the questions), and
   the grade reasons say so. The crosstabs show that CREATEC did not ask
   vote intention of 79 respondents (its likely nonvoters, by its own
   turnout question); they are `inapplicable` in the pushed intention.
   *Needed (optional):* the CREATEC questionnaire, to confirm the wording
   and the routing.

7. **qes2008 interview mode.** The deposit metadata says telephone
   (2008-12-09 to 12-15), and `waves.csv` follows it, but the deposited
   questionnaires (files 196358 and 197296) carry web instructions ("Veuillez
   cliquer sur la flèche pour quitter le sondage" after a quota message) and
   web-style options ("Je préfère ne pas répondre"). Grades are
   `comparable` either way (a phone study cannot be `identical` to a web
   anchor), and `dk_offered` is `unknown`. *Needed:* the collection mode.

8. **qes2007 web questionnaire.** The file mixes telephone (1,003) and web
   (1,172) interviews (`type`), but only the telephone script is deposited,
   so whether the web version showed "don't know" is unknown
   (`dk_offered = unknown` on every qes2007 row).

9. **qes2012_panel pre-election questionnaire.** Not deposited (the
   codebook gives the variable labels). The first intention question,
   `intvoteprov1`, reads "pour lequel des partis suivants voteriez-vous
   **ou seriez-vous tenté de voter**?", which already asks for a lean; it
   and the pushed `intvoteprov` are graded `approximate`. *Needed
   (optional):* the questionnaire, to confirm the stem and the options read.

### B. Decisions taken in the slice that the owner may want to change

10. **"None" among voters (qes2007 `q12` = 97, 12 cases; qes2008 `q12a` =
    97, 3 cases)** is mapped to `spoiled`, as design section 5.2 proposed
    (to be confirmed at R1): the originals only label the code *aucun* /
    *None*. The alternative is `not_mappable`. qes2007 `q12` = 95 (*n'a pas
    voté*, 4 respondents who had said they voted) is taken at its word
    (`not_voted`).

11. **The Parti Égalité (1998)** has no level in `party_qc`; its 9
    intention answers (and none in the recall) are `other`. Adding a level
    would be a MINOR spec change.

12. **1998 vote intention.** Design section 5.7 drafted `vpl` as the first
    intention question. Crosstabs show that `vpl` and `intvote` combine each
    firm's first question and its push (CROP `q7a_crop` + `q7b_crop`,
    CREATEC `r3_createc` + `r4_createc`), so `intvote` (which keeps the 79
    unasked CREATEC respondents apart from "would not vote") feeds
    `vote_prov_intent_push`, graded `approximate` (two firms' instruments
    pooled). The unpushed intention has no row: it sits in two variables,
    one per firm, and would need a registered `fn:` rule.

13. **CROP pushed intention.** The producer's `intvoteprov` code 7 holds
    the 1,105 "no party" answers of the push and 74 "another party" answers
    (69 of the first question, 5 of the push), so it is `not_mappable`
    (1,179 respondents, 4.9%), as for the 2007 panel. A registered `fn:`
    rule combining `intvoteprova` and `intvoteprovb` would recover them.

14. **The firms' own 1998 files are not harmonized.** `qes1998_crop` and
    `qes1998_createc` hold the same respondents as the pooled `qes1998`
    (plus the 24 CROP respondents whose home language is not French);
    harmonizing them too would count those respondents twice in
    `qes_harmonize("all")`. The split is represented instead by the firm as
    a stratum of `qes1998`. Design section 5.7 spoke of "the three 1998
    codes"; the owner may want the firm files in the spec (with their own
    wording and the 24 extra CROP respondents), which would need a rule
    against pooling them with `qes1998`.

### C. Departures from the design text (for the design record)

15. **1998 waves.** Design section 5.3 drafted one `between` wave. The
    codebooks date a pre-election wave (1998-11-18 to 11-23) and a
    post-election wave (12-08 to 12-13), and every row answered both, so
    `qes1998` has `pre` and `post` waves of 1,483 each (like
    `qes2012_panel`), and each question sits in the wave that asked it.

16. **Poll waves: wave `*`.** A crosswalk or weight row may name the wave
    `*` in a study whose waves are all poll waves (V-S4); it applies to
    every poll, each respondent belonging to exactly one (checked on the
    data by V-D8). Such a row takes its election reference from each wave
    (V-S9), and the leading `election_date` is then the wave's. The
    alternative, one row per poll and target (24 x 3 rows), was rejected as
    unreviewable.

17. **New leading column `stratum`**, after `subsample`: for pooled polls
    the poll's wave name (as in `waves`, e.g. `poll_2008_11`); for the 1998
    panel the file's `firme_post` code (`"1"` = CREATEC, `"2"` = CROP,
    documented in `?qes_harmonize` and `?qes_design`, EN and FR); `NA`
    elsewhere. `qes_design()` uses `<study>:<stratum>` as the stratum where
    it is set (this also answers, for these two studies, the HZ4 question
    about strata in the long layout), and `pool = "equal"` counts the polls
    of pooled polls as one study in the long layout too. For pooled polls
    the leading `year` is the year each poll began (2007 to 2010), not the
    catalog study year.

17a. **1998 `sov_partnership_1995` uses `q16a_crop`, not `voteref`**
    (design section 5.7 named `voteref`): `voteref` adds the `q16b` push
    (of the 32 `q16a` don't-knows, 6 become yes and 3 no; one refusal
    becomes no), which the target excludes. Crosstab on the original
    (file 329987).

17b. **Weights registered per wave, not per firm; `ponder2` not CROP-only;
    qes2008 downgraded.** Design section 5.3 lists the 1998 weights
    (`poids`, `ponder2` for CROP only, `ponderc`) as registered per firm.
    The registry is per study and wave, so they are registered for the
    `pre` wave (and `ponder3` for `post`), with `ponder3` recommended (mean
    1 within each firm, the one scale on which a within-wave normalization
    does not reweight the firms); `ponder2` has values for both firms
    (means 2.17 CREATEC, 10.44 CROP), not only CROP. Design section 5.3
    lists qes2008 `pondx` as `reviewed` (and `pond` with role `poststrat`)
    from the DDI; the slice registers both as `needs_review` with no role
    for `pond`, because a DDI variable label is not weighting
    documentation.

### D. Release

18. **Version.** Design OD2 makes 0.6.0 the engine for the six cached
    studies, and the HZ4 commit is 0.6.0 with spec 0.2.0. This slice's
    engine differs (wave `*`, the `stratum` column, per-poll `year`), so
    DESCRIPTION is now 0.6.0.9000 and NEWS has a `# qesR 0.6.0.9000`
    section for spec 0.3.0; the 0.6.0 section is as HZ4 left it.
    `Engine-Min` stays 0.6.0: the validator accepts only x.y.z, and any
    x.y.z above 0.6.0 would refuse the 0.6.0.9000 engine. When the release
    that ships spec 0.3.0 is numbered (0.7.0 by OD2), set `Engine-Min` to
    that number and rename the NEWS heading. *Owner's decision.*

### E. Deferred (other slices)

- Targets not yet in the spec, whose sources exist in these studies:
  gender, education, income, region, language, religion, the 0-10 interest
  scale (2007 `q14`/`q15`), party-identification strength (2007/2008
  `q71`), previous provincial vote (2008 `q13`, CROP `QP4`), federal vote
  (2007/2008 `q74`); and the push sovereignty instruments (2007/2008 `q20`,
  CROP `intvoterefb`, 1998 `q16b_crop`, 2007p `intref2`), each a target of
  its own. They are added target by target (MINOR), as design section 5.7
  plans.
- `fn:` rules (none registered yet): 1998 unpushed intention, CROP and 2007
  panel pushed intention without the merged code.
- English question text of qes2008 in the dictionary (the English
  questionnaire is a PDF; the crosswalk carries the English wording).
- The legacy master (`get_qes_master()`) switch to the engine: HZ6.
- The website articles are not rebuilt here (W.2); their prose was updated
  for the new studies.
