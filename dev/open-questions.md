# Open questions for the owner

Gaps that the public deposits do not settle, recorded as the slices meet
them. Each item says what was done meanwhile (always the conservative
choice: an undocumented weight is registered but not applied, an
undocumented item is left unmapped or graded down) and what would close it.
Nothing here blocks a slice. Data requests follow the policy of design.md
section 0.3 (OD18): Claude fetches public documents itself; the items below
are either not in any public deposit or need the owner's judgement.

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
