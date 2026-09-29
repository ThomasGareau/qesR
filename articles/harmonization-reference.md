# Harmonization Reference

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md)*

The harmonized variables (“targets”) of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
study by study. The whole page below is generated from the specification
that ships with qesR, so it always describes the rules the installed
version applies.

This reference is generated from the harmonization spec shipped with
qesR: version 4.2.0 of 2026-09-28, content hash
`02b3b7edc509deff0db16859bef7bfb6`. It is **experimental**: targets,
grades and mappings are reviewed study by study and may change. Nothing
on this page is written by hand;
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
returns the same information as data frames.

## How to read this reference

Each target is one question stimulus: a different wording, scale, timing
or format makes another target, and targets are never pooled. For each
study, the coverage table gives the source variable and its wave, the
comparability grade of the study’s question against the target’s anchor
question and the reason for it, the instrument (the item format), the
levels the question offered, the question wording (or, when the wording
cannot be shipped, the document and page that give it), the filter
question and what each of its codes means, the study’s recommended
weight (marked when it needs review: qes_harmonize() does not apply it,
and its weight columns are NA) and whether “don’t know” was offered.

A row with no mark is signed off by a reviewer (status stable; in spec
4.0.0, by an automated double review against the original files and
documents, not a human review), and
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applies it by default. A row marked “in review” was checked against the
original files and documents but is not signed off (the `review_note`
column of `qes_spec("crosswalk")` says why it is held); a row marked
“draft” is not yet checked.
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applies these only with `include_draft = TRUE`.

Licence: the description of qes2022 (labels, question text, counts) is
derived from “2022 Quebec Election Study” (Mahéo, Bélanger, Stephenson
and Harell, 2023, <https://doi.org/10.7910/DVN/PAQBDR>) and licensed CC
BY-NC 4.0 (<https://creativecommons.org/licenses/by-nc/4.0/>):
attribution, no commercial use. Adapted by qesR (extracted, reformatted,
typed and tabulated). It is not covered by qesR’s MIT licence. Changes
and list of files: system.file(“COPYRIGHTS”, package = “qesR”). Full
citation: qes_cite(“qes2022”).

### Comparability grades

- **`identical`** (Identical): the same stem in every language fielded,
  the same options, the same don’t-know option, universe and mode family
  as the anchor question;
- **`comparable`** (Comparable): the same construct and stimulus;
  differences (a temporal adverb, option order, whether don’t know is
  offered, which minor parties are listed) are not expected to move the
  shares of the common levels;
- **`approximate`** (Approximate): the same construct, but a format,
  filter or mode expected to move the shares;
  `qes_harmonize(min_grade = "comparable")` sets these cells to `NA`
  (reason `below_grade`);
- **`not_comparable`** (Not comparable): another construct, or a source
  that must not be used: recorded here, never mapped.

### Structural zeros

A level that a study’s question did not offer (a party missing from its
list, say) is a *structural zero*: its share in that study is 0 because
nobody could choose it, not because nobody supported it. In the coverage
tables such levels are listed after “not offered”.
`qes_provenance(x, level = "cell")$levels_not_offered` gives them for
harmonized data.

### Why a value is missing

Every missing value of a target carries one of these reasons
(`qes_harmonize(missing = "reasons")` adds them as `<target>__na`
columns):

| Code             | Label                                          |
|------------------|------------------------------------------------|
| `dk`             | Don’t know                                     |
| `refused`        | Refused                                        |
| `dk_refused`     | Don’t know or refused (one code)               |
| `no_answer`      | No answer (item nonresponse)                   |
| `not_selected`   | Not selected (multiple choice)                 |
| `inapplicable`   | Inapplicable (routed out)                      |
| `not_voted`      | Did not vote                                   |
| `spoiled`        | Spoiled ballot                                 |
| `ineligible`     | Not eligible to vote                           |
| `not_registered` | Not on the list of electors                    |
| `not_in_wave`    | Not in this wave                               |
| `not_mappable`   | Source category straddles target levels        |
| `sysmis`         | System missing                                 |
| `not_asked`      | Not asked in this study                        |
| `not_reviewed`   | Crosswalk row not yet signed off by a reviewer |
| `below_grade`    | Below the requested grade                      |
| `unmapped`       | Code not mapped                                |

## Targets

### Survey design

#### `survey_mode`: Interview mode

How the respondent was interviewed: web, telephone or mixed. A leading
column of every result of qes_harmonize(), filled from the mode of each
wave (waves.csv); crosswalk rows exist only for the waves whose mode
varies by respondent.

Family `interview_mode` · type Categorical · timing Any time · status
Experimental · added in spec 0.2.0

**Levels**

| Code | Name    | Label     |
|------|---------|-----------|
| 1    | `web`   | Web       |
| 2    | `phone` | Telephone |
| 3    | `mixed` | Mixed     |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `method` (pre) | `identical` (anchor) | Anchor row of the target. | interview_mode | web, phone; not offered: mixed | document 341538, method |  | `weight` | Not offered |
| qes2007 | `type` (post) | `identical` | The interview mode as the file records it. | interview_mode | web, phone; not offered: mixed | document 425921, type |  | `pond` | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.2.0 (2026-09-27): Waves, weights and eligibility: the new targets
  age (age in years as asked), birth_month, age_group3 (three age bands,
  from questions with these bands or bands that collapse into them
  exactly) and citizen, and survey_mode, which fills the interview mode
  of the qes2018_panel pre-election wave, where it varies by respondent.
  11 crosswalk rows: birth_year of qes2012, qes2014 and qes2018;
  birth_month and age of qes2018; age and citizen of qes2022; age_group3
  of qes2007_panel, qes2012_panel and qes2018_panel; survey_mode of
  qes2018_panel. With their value maps, gates.csv cells, expected
  marginals and column hashes. No existing row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.

### Vote and turnout

#### `vote_prov_recall`: Provincial vote (recall)

Party the respondent reports having voted for in the Quebec general
election of the study, asked after that election. Nonvoters, spoiled
ballots and respondents not eligible or not registered are missing
values with a reason, never a party.

Family `vote_prov` · type Categorical · timing Post-election · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name    | Label       |
|------|---------|-------------|
| 1    | `PLQ`   | PLQ         |
| 2    | `PQ`    | PQ          |
| 3    | `CAQ`   | CAQ         |
| 4    | `QS`    | QS          |
| 5    | `PVQ`   | PVQ         |
| 6    | `PCQ`   | PCQ         |
| 7    | `ON`    | ON          |
| 8    | `ADQ`   | ADQ         |
| 90   | `other` | Other party |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_votechoice` (pes) | `comparable` | Lists the four main parties and the Conservatives but not the Green party or Option nationale, no don’t-know option, spoiling is an option, and it follows a face-saving turnout question. | vote_recall_list | PLQ, PQ, CAQ, QS, PCQ, other; not offered: PVQ, ON, ADQ | Which party did you vote for? | pes_turnout: 2 = not_voted, 3 = not_voted, 4 = not_voted, 5 = not_registered, 6 = dk | `pes_weight_general` | Not offered |
| qes2018 | `q6` (post) | `comparable` | Only the four main parties are listed (no Green or Option nationale), there is no don’t-know option, spoiling is an option, and the item follows a face-saving turnout question. | vote_recall_list | PLQ, PQ, CAQ, QS, other; not offered: PVQ, PCQ, ON, ADQ | Which party did you vote for? | q5: 1 = not_voted, 2 = not_voted, 3 = not_voted, 5 = ineligible, 99 = refused, NA = inapplicable | `pond` | Not offered |
| qes2018_panel | `rts_q2` (post) | `comparable` | Party list with the leaders’ names, mixed telephone and web mode; the anchor is a web list without leaders. | vote_recall_list_leaders | PLQ, PQ, CAQ, QS, other; not offered: PVQ, PCQ, ON, ADQ | Et pour qui avez-vous voté? | rts_q1: 1 = not_voted, 2 = not_voted, 4 = dk_refused | `weight_rts` | Not documented |
| qes2014 | `Q3` (post) | `comparable` | Same party list as the anchor, but the stem does not name the election date and no don’t-know option is offered. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | Which party did you vote for? | Q2: 2 = not_voted, 9 = refused | `POND` | Not offered |
| qes2012 | `q25` (post) | `identical` (anchor) | Anchor row of the target. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | How did you vote in the last Quebec provincial election of September 4th, 2012? | q21: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Offered explicitly |
| qes2012_panel | `voteprov` (post) | `approximate` | Unprompted telephone recall (the options are not read) after a turnout question that probes election day or advance poll; the anchor is a web list. | vote_recall_unprompted | CAQ, PLQ, PQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | For which party did you vote for? (DO NOT READ) |  | `pond_post` (needs review, not applied) | Not offered |
| qes2008 | `q12a` (post) | `comparable` | Same question (party voted for); the list names the ADQ and not the CAQ or ON, which did not exist; the turnout question has no don’t-know code; telephone by the deposit metadata, the anchor is web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for? | q11: 2 = not_voted, 9 = refused |  | Not documented |
| qes2007 | `q12` (post) | `comparable` | Same question (party voted for, the parties named in the stem); the list names the ADQ and not the CAQ or ON, which did not exist; the study mixes telephone and web interviews (only the telephone script is deposited), the anchor is web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for? The Liberal Party, Parti Québécois, ADQ, Québec solidaire, the Green Party or another party? | q11: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Not documented |
| qes2007_panel | `vote` (post) | `approximate` | Unprompted telephone recall (options not read) after a two-step turnout question; the anchor is a web list. | vote_recall_unprompted | ADQ, PLQ, PQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Whom did you vote for? |  | `pond_tot_am1` (needs review, not applied) | Not offered |
| qes1998 | `q3post` (post) | `comparable` | Same question (party voted for, from a list read) by telephone, with the same stem in both firms’ questionnaires; CREATEC’s own Q3 uses other codes (1 PLQ, 2 PQ, 3 ADQ, 4 another party) with no Parti Égalité, and the pooled file puts them on CROP’s codes, where the Parti Égalité is starred (not read) and never chosen; the list names the ADQ, not the CAQ; the anchor is web. | vote_recall_list | ADQ, PLQ, PQ, other; not offered: CAQ, QS, PVQ, PCQ, ON | 3\. Pour lequel des partis suivants avez-vous voté? |  | `ponder3` (needs review, not applied) | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `vote_prov_intent`: Provincial vote intention

Party the respondent intends to vote for in the coming Quebec general
election, asked before it, at the first question, without the push
question for the undecided. Would not vote, none or would spoil is an
answer (level no_party), not a missing value.

Family `vote_prov` · type Categorical · timing Pre-election · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name       | Label                               |
|------|------------|-------------------------------------|
| 1    | `PLQ`      | PLQ                                 |
| 2    | `PQ`       | PQ                                  |
| 3    | `CAQ`      | CAQ                                 |
| 4    | `QS`       | QS                                  |
| 5    | `PVQ`      | PVQ                                 |
| 6    | `PCQ`      | PCQ                                 |
| 7    | `ON`       | ON                                  |
| 8    | `ADQ`      | ADQ                                 |
| 90   | `other`    | Other party                         |
| 95   | `no_party` | Would not vote / none / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1` (cps) | `approximate` | Web list with don’t know and refusal shown, asked only of respondents certain or likely to vote (the others were asked a separate item or none), with no would-not-vote option; the anchor is a telephone item asked of everyone. | vote_intent_list | PLQ, PQ, CAQ, QS, PCQ, other; not offered: PVQ, ON, ADQ, no_party | Which party do you think you will vote for? | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Offered explicitly |
| qes2018_panel | `rv1a` (pre) | `approximate` | Asks which party’s candidate the respondent would probably support in an election held tomorrow, and tells those who already voted in advance polls to give that vote, so part of the answers report a vote already cast; mixed mode (850 web, 400 telephone); the anchor asks which party they would vote for today, by telephone. | vote_intent_list | PLQ, PQ, CAQ, QS, other, no_party; not offered: PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? Si vous avez déjà voté par anticipation, veuillez indiquer pour quel parti. |  | `weight` | Not documented |
| qes2012_panel | `intvoteprov1` (pre) | `approximate` | The stem asks which party the respondent would vote for ‘or would be tempted to vote for’, which softens the first question toward a lean; the pre-election questionnaire is not deposited (wording from the variable label and the codebook, file 654292); telephone, like the anchor. | vote_intent_or_lean_list | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; not offered: PCQ, ADQ | Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? |  | `pondam1` (needs review, not applied) | Not documented |
| qes_crop_2007_2010 | `intvoteprova` (each poll) | `comparable` | Same first question as the anchor, word for word, by the same firm (CROP) and telephone mode, with the same parties and leaders read in rotation, and would-not-vote and don’t know/refusal not read; the polls are monthly omnibus polls between elections, not a campaign panel, and the wording is documented for three of the 24 polls only (CROP reports of May 2008, January 2009 and March 2009; no questionnaire is deposited). | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `XPOND` (needs review, not applied) | Volunteered only |
| qes2007_panel | `intvote1` (pre) | `identical` (anchor) | Anchor row of the target. | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `pondam1` (needs review, not applied) | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `vote_prov_intent_push`: Provincial vote intention, undecided pushed

Vote intention in which respondents undecided at the first question were
asked which party they lean toward (a push question), combined into one
variable. A different stimulus from vote_prov_intent: never pooled with
it.

Family `vote_prov` · type Categorical · timing Pre-election · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name       | Label                               |
|------|------------|-------------------------------------|
| 1    | `PLQ`      | PLQ                                 |
| 2    | `PQ`       | PQ                                  |
| 3    | `CAQ`      | CAQ                                 |
| 4    | `QS`       | QS                                  |
| 5    | `PVQ`      | PVQ                                 |
| 6    | `PCQ`      | PCQ                                 |
| 7    | `ON`       | ON                                  |
| 8    | `ADQ`      | ADQ                                 |
| 90   | `other`    | Other party                         |
| 95   | `no_party` | Would not vote / none / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `rv1ab` (pre) | `approximate` | Producer’s combination of rv1a with the push question rv1b, in mixed telephone and web mode; the anchor is a telephone push. The push filter is narrower than the anchor’s: respondents who said they would not vote or support no party (63) were not pushed, whereas the anchor pushed them, and 31 of the 233 undecided, all interviewed by telephone, were not asked rv1b and stay undecided. | intent_lean_push | PLQ, PQ, CAQ, QS, other, no_party; not offered: PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? (relance, rv1b : Et pour quel parti diriez-vous que vous auriez tendance à voter?) |  | `weight` | Not documented |
| qes2012_panel | `intvoteprov` (pre) | `approximate` | The producer’s combination of the first question (which already asks for a party the respondent ‘would be tempted to vote for’) and the push; the pre-election questionnaire is not deposited (wording from the variable label and the codebook, file 654292); telephone, like the anchor. | intent_lean_push | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; not offered: PCQ, ADQ | Q2+Q3 - Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? (relance : Peut-être que votre choix n’est pas définitif, mais y a-t-il tout de même un parti que vous seriez tenté d’appuyer?) |  | `pondam1` (needs review, not applied) | Not documented |
| qes_crop_2007_2010 | `intvoteprov` (each poll) | `comparable` | The producer’s combination of the first question and the push, as in the anchor, by the same firm and mode; CROP telephone polls; the deposited codebook (file 341537) gives only truncated labels, but CROP’s own La Presse reports (May 2008, January 2009; see dev/open-questions.md 1.1) print the same stem and push as the anchor, with parties and leaders read in rotation and don’t know recorded only if volunteered; not identical because these are monthly omnibus polls, most fielded outside a campaign (the reference election is 2008 or 2012 depending on the poll), on a regionally stratified sample (500 Montréal, 200 Québec, 300 elsewhere). | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… (relance : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter? Est-ce…) |  | `XPOND` (needs review, not applied) | Volunteered only |
| qes2007_panel | `intvote` (pre) | `identical` (anchor) | Anchor row of the target. | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? (relance, Q5 : pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `pondam1` (needs review, not applied) | Volunteered only |
| qes1998 | `intvote` (pre) | `approximate` | The producer’s combination of the first question and the push, but two firms’ telephone polls pooled in one variable (CREATEC and CROP, each with its own questionnaire; CREATEC’s is not deposited); CREATEC did not ask the 79 respondents who, at its turnout question, said they would probably not (22) or certainly not (35) vote, or did not know or refused (22), whereas CROP asked everyone. | intent_lean_push | ADQ, PLQ, PQ, other, no_party; not offered: CAQ, QS, PVQ, PCQ, ON | 7a. S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Est-ce… (relance, 7b : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `ponder3` (needs review, not applied) | Not documented |

**Not used**

- qes1998 `intvote2` (pre): `not_comparable`. The value labels are
  shifted in the source: the counts are those of vpl, whose code 1 is
  ADQ, 3 PLQ and 4 PQ, but intvote2 labels them PLQ, ADQ and Parti
  Égalité.

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `turnout_prov_recall`: Voted in the provincial election (recall)

Whether the respondent reports having voted in the Quebec general
election of the study, asked after that election. Respondents not
eligible or not registered are missing values with a reason.

Family `turnout_prov` · type Categorical · timing Post-election · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_turnout` (pes) | `approximate` | Face-saving turnout format with three ways of not voting, a not-registered option and a don’t-remember option; expected to move the share of voters. | turnout_excuse_format | yes, no | The Quebec election results were recently announced. In any election, some people are not able to vote because they are sick or busy, or for some other reason. Others do not want to vote. Did you vote in the recent Quebec election? |  | `pes_weight_general` | Offered explicitly |
| qes2018 | `q5` (post) | `approximate` | Face-saving turnout format with three ways of not voting and a not-eligible option, no don’t-know option; expected to move the share of voters. | turnout_excuse_format | yes, no | In each election we find that a lot of people were not able to vote because they were not registered, they were sick, or they did not have time. Which of the following statements best describes you? |  | `pond` | Not offered |
| qes2018_panel | `rts_q1` (post) | `approximate` | Face-saving format (wanted to vote but could not; decided not to vote; or went to vote), mixed telephone and web mode. | turnout_excuse_format | yes, no | À chaque élection, certaines personnes décident de ne pas voter, d’autres ne peuvent pas y aller pour différentes raisons. |  | `weight_rts` | Not documented |
| qes2014 | `Q2` (post) | `comparable` | Same yes/no format, but no don’t-know option (the anchor offers one) and the stem refers to ‘that election’ instead of naming its date. | turnout_yesno | yes, no | Did you vote in that provincial election? |  | `POND` | Not offered |
| qes2012 | `q21` (post) | `identical` (anchor) | Anchor row of the target. | turnout_yesno | yes, no | Did you vote in the last Quebec provincial election of September 4th, 2012? |  | `pond` | Offered explicitly |
| qes2012_panel | `participation` (post) | `comparable` | Same yes/no question; a yes is probed for election day or advance poll, both yes; no don’t-know code (the anchor offers one); telephone, the anchor is web. | turnout_yesno_probe | yes, no | First, can you tell me if you have voted in the recent Quebec election? (IF YES, PROBE) |  | `pond_post` (needs review, not applied) | Not offered |
| qes2008 | `q11` (post) | `comparable` | Same yes/no question, but no don’t-know code (the anchor offers one); telephone by the deposit metadata, the anchor is web. | turnout_yesno | yes, no | Did you vote in the last provincial election? |  |  | Not offered |
| qes2007 | `q11` (post) | `comparable` | Same yes/no question with a don’t-remember code; the study mixes telephone and web interviews (only the telephone script is deposited), the anchor is web. | turnout_yesno | yes, no | Did you vote in the provincial election? |  | `pond` | Not documented |
| qes2007_panel | `voteoui` (post) | `comparable` | Same yes/no question; a yes is probed for election day or advance poll, both yes; no don’t-know code (the anchor offers one); telephone, the anchor is web. | turnout_yesno_probe | yes, no | First, can you tell me if you have voted at the last Quebec election that was just held? |  | `pond_tot_am1` (needs review, not applied) | Not offered |
| qes1998 | `q1post` (post) | `comparable` | Same yes/no question by telephone, with the same stem for both firms; no don’t-know code (the anchor offers one); the anchor is web. | turnout_yesno | yes, no | 1\. Pouvez-vous me dire si vous avez voté à l’élection du 30 novembre dernier? |  | `ponder3` (needs review, not applied) | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 2.0.0 (2026-09-27): Review corrections. MAJOR (a corrected gate,
  changed column hashes): the religion rows of qes2012 (q103) and
  qes2014 (Q63) are gated on their filter question (q102, Q62): those
  who belong to no religion are inapplicable (842 and 845) and those who
  preferred not to answer the filter question refused (32 and 44), where
  they were sysmis; values unchanged. A gate now applies to rules
  weight, date and string as to map and numeric, a gate on another rule
  is a V-S1 error, and gates.csv holds the cells of the gated string
  rows for V-D7. The qes2007_panel reported turnout reads the
  post-election turnout question voteoui (yes on election day, yes in
  advance, no) in place of the producer’s recode avote, with the same
  values and column hash; graded comparable (the same probed yes/no
  question as qes2012_panel), not approximate. Text only: the education4
  grade reasons of qes2018 and qes2018_panel and the target description
  name the placement of the vocational diploma (DEP: secondary in
  qes2018, college in qes2018_panel and probably where no DEP option is
  offered); the qes1998 language note of legacy.csv says the constant is
  a mother tongue only for the CREATEC rows (the home language for the
  426 CROP rows; the pooled definition is not confirmed yet); the
  religion description and legacy note name the gate; the qes2022
  evidence notes give codes and codebook pages only (its licence, CC
  BY-NC, keeps its wording and labels out of the package).
- 2.0.1 (2026-09-27): Text only: the legacy.csv rows of
  sovereignty_support and sovereignty give qes2007_panel the reason of
  qes2007, qes2008 and qes1998 (it asked the 1995 question, see
  sov_partnership_1995) and give the CROP polls their own (the first
  referendum question, intvoterefa, has only a truncated label and no
  deposited questionnaire, so its wording is unknown and it is not
  mapped); the cause column of legacy.csv (attr(,
  “legacy_na_columns”)\$cause of get_qes_master() and get_decon()) has
  descriptive values (reported_vote_only, independence_question_only,
  no_valid_source, not_harmonized_yet) instead of internal decision ids,
  and the text of legacy.csv, crosswalk.csv, valuemaps.csv and this
  changelog says each decision in words. No row, code, grade, marginal
  or hash changed.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `turnout_prov_likely`: Likelihood of voting in the provincial election

How likely the respondent says they are to vote in the coming Quebec
general election, asked before it. Having already voted (in advance
polls) is an answer. A different stimulus from turnout_prov_recall
(reported turnout): never pooled with it.

Family `turnout_prov` · type Ordinal · timing Pre-election · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name            | Label               |
|------|-----------------|---------------------|
| 5    | `already_voted` | Already voted       |
| 1    | `certain`       | Certain to vote     |
| 2    | `likely`        | Likely to vote      |
| 3    | `unlikely`      | Unlikely to vote    |
| 4    | `certain_not`   | Certain not to vote |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_turnout` (cps) | `identical` (anchor) | Anchor row of the target. | turnout_likely | certain, likely, unlikely, certain_not, already_voted | The Quebec election is scheduled for October 3, 2022. In this election, are you… |  | `cps_weight_general` | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `vote_prov_intent_other`: Provincial vote intention: another party (text)

The party the respondent typed after choosing another party at the
vote-intention question (vote_prov_intent, level other), as typed. Open
text, not harmonized.

Family `vote_prov` · type Text · timing Pre-election · status
Experimental · added in spec 1.0.0

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1_8_TEXT` (cps) | `identical` (anchor) | Anchor row of the target. | open_text |  | Which party do you think you will vote for? \[Another party (please specify)\] | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

### Party identification

#### `pid_prov`: Provincial party identification

The provincial party the respondent usually thinks of themselves as
close to; none is an answer.

Family `party_id` · type Categorical · timing Any time · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name    | Label         |
|------|---------|---------------|
| 1    | `PLQ`   | PLQ           |
| 2    | `PQ`    | PQ            |
| 3    | `CAQ`   | CAQ           |
| 4    | `QS`    | QS            |
| 5    | `PVQ`   | PVQ           |
| 6    | `PCQ`   | PCQ           |
| 7    | `ON`    | ON            |
| 8    | `ADQ`   | ADQ           |
| 90   | `other` | Other party   |
| 97   | `none`  | None of these |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provpid` (cps) | `comparable` | Same stem as the anchor, but the list drops the Onist and Green options, adds Conservative and another party, and offers no don’t-know option. | pid_prov_list | PLQ, PQ, CAQ, QS, PCQ, other, none; not offered: PVQ, ON, ADQ | In provincial politics, do you usually think of yourself as a: |  | `cps_weight_general` | Not offered |
| qes2018 | `q56` (post) | `comparable` | Same stem as the anchor, but only the four main parties are listed (no Onist or Green). | pid_prov_list | PLQ, PQ, CAQ, QS, none; not offered: PVQ, PCQ, ON, ADQ, other | In provincial politics, do you usually think of yourself as a: |  | `pond` | Offered explicitly |
| qes2014 | `Q55` (post) | `identical` | Same stem and the same six parties, none, don’t know and refusal options as the anchor, in English and French, web. | pid_prov_list | PLQ, PQ, CAQ, QS, ON, PVQ, none; not offered: PCQ, ADQ, other | In provincial politics, do you usually think of yourself as a: |  | `POND` | Offered explicitly |
| qes2012 | `q92` (post) | `identical` (anchor) | Anchor row of the target. | pid_prov_list | PLQ, PQ, CAQ, QS, ON, PVQ, none; not offered: PCQ, ADQ, other | In provincial politics, do you usually think of yourself as a: |  | `pond` | Offered explicitly |
| qes2008 | `q70` (post) | `comparable` | Same construct (usual provincial party identification, none an answer); the stem reads ‘do you usually identify yourself with’ and the list names the ADQ; telephone by the deposit metadata, the anchor is web. | pid_prov_list | PLQ, PQ, ADQ, QS, PVQ, other, none; not offered: CAQ, PCQ, ON | In provincial politics, do you usually identify yourself with … |  |  | Not documented |
| qes2007 | `q70` (post) | `comparable` | Same construct (usual provincial party identification, none an answer); the stem reads ‘do you usually identify yourself with’ and the list names the ADQ; the study mixes telephone and web interviews (only the telephone script is deposited), the anchor is web. | pid_prov_list | PLQ, PQ, ADQ, QS, PVQ, other, none; not offered: CAQ, PCQ, ON | In provincial politics, do you usually identify yourself with … |  | `pond` | Not documented |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `pid_fed`: Federal party identification

The federal party the respondent usually thinks of themselves as close
to; none is an answer.

Family `party_id` · type Categorical · timing Any time · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name    | Label          |
|------|---------|----------------|
| 1    | `LPC`   | Liberal        |
| 2    | `CPC`   | Conservative   |
| 3    | `NDP`   | NDP            |
| 4    | `BQ`    | Bloc Québécois |
| 5    | `GPC`   | Green          |
| 6    | `PPC`   | PPC            |
| 90   | `other` | Another party  |
| 97   | `none`  | None of these  |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_fedpid` (cps) | `identical` (anchor) | Anchor row of the target. | pid_list | LPC, CPC, NDP, BQ, GPC, PPC, other, none | In federal politics, do you usually think of yourself as a: |  | `cps_weight_general` | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

### Attitudes

#### `sov_indep`: Referendum vote: independent country

How the respondent would vote in a referendum on whether Quebec should
become an independent country.

Family `sovereignty` · type Categorical · timing Any time · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name             | Label                        |
|------|------------------|------------------------------|
| 1    | `yes`            | Yes                          |
| 2    | `no`             | No                           |
| 95   | `would_not_vote` | Would not vote / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_qc_referendum` (cps) | `comparable` | The stem adds a temporal adverb; don’t know is offered but there is no prefer-not-to-answer option. | sov_indep_country | yes, no; not offered: would_not_vote | If there were today a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO? |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q26` (post) | `comparable` | The stem adds a temporal adverb (‘today’, ‘aujourd’hui’); otherwise the same wording and options as the anchor. | sov_indep_country | yes, no; not offered: would_not_vote | If there were today a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO? |  | `pond` | Offered explicitly |
| qes2014 | `Q19` (post) | `identical` | Same stem as the anchor in English and French, same options (yes, no, don’t know, prefer not to answer), asked of everyone, web. | sov_indep_country | yes, no; not offered: would_not_vote | If there were a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO? |  | `POND` | Offered explicitly |
| qes2012 | `q52` (post) | `identical` (anchor) | Anchor row of the target. | sov_indep_country | yes, no; not offered: would_not_vote | If there were a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO? |  | `pond` | Offered explicitly |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 2.0.1 (2026-09-27): Text only: the legacy.csv rows of
  sovereignty_support and sovereignty give qes2007_panel the reason of
  qes2007, qes2008 and qes1998 (it asked the 1995 question, see
  sov_partnership_1995) and give the CROP polls their own (the first
  referendum question, intvoterefa, has only a truncated label and no
  deposited questionnaire, so its wording is unknown and it is not
  mapped); the cause column of legacy.csv (attr(,
  “legacy_na_columns”)\$cause of get_qes_master() and get_decon()) has
  descriptive values (reported_vote_only, independence_question_only,
  no_valid_source, not_harmonized_yet) instead of internal decision ids,
  and the text of legacy.csv, crosswalk.csv, valuemaps.csv and this
  changelog says each decision in words. No row, code, grade, marginal
  or hash changed.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `sov_sovereign_country`: Referendum vote: sovereign country

How the respondent would vote in a referendum on whether Quebec should
become a sovereign country. A different stimulus from sov_indep
(sovereign, not independent): never pooled with it. The push question
asked of the undecided is not part of this target.

Family `sovereignty` · type Categorical · timing Any time · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name             | Label                        |
|------|------------------|------------------------------|
| 1    | `yes`            | Yes                          |
| 2    | `no`             | No                           |
| 95   | `would_not_vote` | Would not vote / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `intvoteref` (pre) | `identical` (anchor) | Anchor row of the target; wording from the variable label and the codebook only (the pre-election questionnaire is not deposited), so whether would-not-vote and don’t know were read is not documented. | sov_sovereign_country | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays souverain, voteriez-vous oui ou voteriez-vous non? |  | `pondam1` (needs review, not applied) | Not documented |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 3.0.0 (2026-09-27): Weights, waves and fieldwork from the documents
  found for the studies that lacked them (dev/open-questions.md). MAJOR
  (a corrected weight role and changed recommended weights): qes2008
  pond reproduces the official 2008 vote among voters and is registered
  as vote_calibrated, so qes2008 has no recommended weight (both its
  weights are calibrated on vote or turnout; V-S13 now allows none in
  that case only) and its harmonized weights stay NA; qes2007 pond (the
  authors’ book: sex, age, mother tongue and region, weighted within
  each mode) and the 2018 panel’s weight and weight_rts (Ipsos’s report
  and Durand and Blais 2020: sex, age, region, mother tongue and
  university education) are reviewed, so qes_harmonize() gives their
  weights, NA before. The 2007 panel’s post-election wave gets a
  recommended weight, pond_tot_am1, which needs review. The other
  weights keep their status, with what the files and documents now show:
  the 1998 decomposition (ponder3 = ponderc x poids / c; poids is the
  design factor of the stratified recontact; the deposit advises
  ponderc, which does not undo the over-selection), CROP’s XPOND (CROP’s
  reports: the 2006 Census, by sex, age, region and home language), the
  cells of the 2012 panel’s weights, the qes2012 margins. Waves: the
  fieldwork dates of the 24 CROP polls (CROP’s reports and the
  quebecpolitique.com archive), the election their previous-vote
  question QP4 refers to, and what is known of the wording of their
  first referendum question (a sovereign country, documented for 19
  polls; still not mapped, since a crosswalk row applies to every poll);
  the 2018 panel’s dates (2018-09-26 to 09-28, 2018-10-12 to 10-19) and
  modes; the 1998 francophone definition, confirmed by linking the
  pooled rows to the firms’ files; the evidence on the qes2008 mode
  (still telephone, as the deposit says). Text only: the qes2018
  lang_mother evidence (codes 2 and 96 are not swapped), the CROP gender
  evidence (one poll without SEXE), the sov_sovereign_country
  description (the push question is not part of the target), and the
  legacy.csv notes of the CROP sovereignty, turnout and vote columns and
  of the 1998 language and survey_weight columns. No map, gate, grade,
  marginal or column hash changed.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `sov_favour`: Favour Quebec independence (4 points)

How favourable or opposed the respondent is to Quebec independence, on a
four-point scale. Not a referendum vote.

Family `sovereignty` · type Ordinal · timing Any time · status
Experimental · added in spec 0.1.0

**Levels**

| Code | Name                  | Label               |
|------|-----------------------|---------------------|
| 1    | `very_favourable`     | Very favourable     |
| 2    | `somewhat_favourable` | Somewhat favourable |
| 3    | `somewhat_opposed`    | Somewhat opposed    |
| 4    | `very_opposed`        | Very opposed        |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `rts_q7` (post) | `identical` (anchor) | Anchor row of the target; wording from the codebook only (no questionnaire deposited), so whether don’t know was offered on the web is not documented. | sov_favour_4pt | very_favourable, somewhat_favourable, somewhat_opposed, very_opposed | En ce qui concerne l’indépendance du Québec, c’est-à-dire que le Québec ne fasse plus partie du Canada, êtes-vous personnellement |  | `weight_rts` | Not documented |

**Not used**

- qes2018_panel `independance` (post): `not_comparable`. A recode of
  rts_q7 (1-2 favour, 3-4 oppose, 5 and missing set to missing), not a
  separate item: never mapped.

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.

#### `lr_self`: Left-right self-placement (0-10)

Where the respondent places their own views on a scale from 0 (left) to
10 (right).

Family `left_right` · type Numeric · timing Any time · status
Experimental · added in spec 0.1.0

**Valid range**: 0-10

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_ideoself_1` (cps) | `approximate` | Standalone 0-10 item with a different stem, no don’t-know option and not preceded by the party placements; item nonresponse is far lower than the don’t-know share of the anchor. | lr_0_10 |  | In politics, people sometimes talk of left and right. Where would you place yourself on this scale, where 0 means left and 10 means right? |  | `cps_weight_general` | Not offered |
| qes2018 | `q36_1` (post) | `comparable` | Same stem as the anchor, preceded by the party placements, but the endpoints read ‘left’ and ‘right’ instead of ‘most left’ and ‘most right’. | lr_0_10 |  | And on the same scale, where would you place your own views, generally speaking? |  | `pond` | Offered explicitly |
| qes2018_panel | `rts_q8` (post) | `approximate` | Different stem, mixed telephone and web mode; the deposited wording is cut at 80 characters, so the endpoint labels cannot be compared. | lr_0_10 |  | On utilise souvent un axe gauche-droite pour situer les opinions politiques des gens. Sur une échelle de 0 |  | `weight_rts` | Not documented |
| qes2014 | `Q32` (post) | `identical` | Same stem and endpoint labels as the anchor in English and French, preceded by the same party placements, web. | lr_0_10 |  | And on the same scale, where would you place your own views, generally speaking? |  | `POND` | Offered explicitly |
| qes2012 | `q71` (post) | `identical` (anchor) | Anchor row of the target. | lr_0_10 |  | And on the same scale, where would you place your own views, generally speaking? |  | `pond` | Offered explicitly |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `interest_4pt`: Interest in politics (4 points)

How interested the respondent is in politics, on a four-point verbal
scale. Never rescaled to or pooled with 0-10 interest items.

Family `interest` · type Ordinal · timing Any time · status Experimental
· added in spec 0.1.0

**Levels**

| Code | Name         | Label                 |
|------|--------------|-----------------------|
| 1    | `very`       | Very interested       |
| 2    | `quite`      | Quite interested      |
| 3    | `hardly`     | Hardly interested     |
| 4    | `not_at_all` | Not at all interested |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q27` (post) | `comparable` | The stem adds ‘and public issues’; the options read very, somewhat, not very (assez, peu) and not at all interested. | interest_4pt | very, quite, hardly, not_at_all | How interested would you say you are in politics and public issues? Are you: |  | `pond` | Offered explicitly |
| qes2014 | `Q28` (post) | `comparable` | Same stem in English and French and the same French options, but the third English option reads ‘Not very interested’ where the anchor has ‘Hardly interested’. | interest_4pt | very, quite, hardly, not_at_all | How interested would you say you are in politics? Are you: |  | `POND` | Offered explicitly |
| qes2012 | `q67` (post) | `identical` (anchor) | Anchor row of the target. | interest_4pt | very, quite, hardly, not_at_all | How interested would you say you are in politics? Are you: |  | `pond` | Offered explicitly |

**Not used**

- qes2012_panel `interetrec` (pre): `not_comparable`. Not a question: a
  campaign-interest score the producer derived from how much of each of
  the four leaders’ debates the respondent watched.

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `sov_partnership_1995`: Referendum vote: the 1995 sovereignty-partnership question

How the respondent would vote if a referendum were held today on the
question of the 1995 referendum, sovereignty with an offer of
partnership to the rest of Canada. A different stimulus from sov_indep
and sov_sovereign_country: never pooled with them. The push question
asked of the undecided is not part of this target.

Family `sovereignty` · type Categorical · timing Any time · status
Experimental · added in spec 0.3.0

**Levels**

| Code | Name             | Label                        |
|------|------------------|------------------------------|
| 1    | `yes`            | Yes                          |
| 2    | `no`             | No                           |
| 95   | `would_not_vote` | Would not vote / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q19` (post) | `comparable` | Same stem and options as the anchor in English and French; the mode differs (telephone by the deposit metadata; the anchor study mixes telephone and web). | sov_partnership_1995 | yes, no, would_not_vote | If a referendum were held today on the same question as that asked in 1995, that is sovereignty with an offer of partnership with the rest of Canada, would you vote YES or would you vote NO? |  |  | Not documented |
| qes2007 | `q19` (post) | `identical` (anchor) | Anchor row of the target: the 1995 question read in full; would not vote/spoil, don’t know and refused are codes; don’t know is pushed at q20, which is not part of the target. | sov_partnership_1995 | yes, no, would_not_vote | If a referendum were held today on the same question as that asked in 1995, that is sovereignty with an offer of partnership with the rest of Canada, would you vote YES or would you vote NO? |  | `pond` | Not documented |
| qes2007_panel | `intref1` (pre) | `comparable` | Same question and options, by telephone; the stem says ‘accompagnée d’une offre de partenariat’ where the anchor says ‘assortie d’une offre’; would not vote, don’t know and refused are not read. | sov_partnership_1995 | yes, no, would_not_vote | document 352416, 13 |  | `pondam1` (needs review, not applied) | Volunteered only |
| qes1998 | `q16a_crop` (pre) | `comparable` | Same stem and options as the anchor, by telephone; asked by CROP only (426 of the 1,483), not by CREATEC. | sov_partnership_1995 | yes, no, would_not_vote | 16a. Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON | firme_post: 1 = inapplicable | `ponder3` (needs review, not applied) | Not documented |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.3.0 (2026-09-27): The remaining studies: qes2007, qes2008,
  qes_crop_2007_2010 and qes1998 join the spec, with their waves and
  weights, and qes2012_panel gains its vote and turnout rows. The pooled
  CROP polls have one wave per monthly poll (24), each with its election
  and its own weight normalization, and crosswalk rows of wave \* that
  apply to every poll. qes1998 is the pooled panel of the CREATEC and
  CROP polls, francophones only, with the firm as a stratum. The new
  target sov_partnership_1995 (the 1995 sovereignty-partnership
  question) has rows for qes2007, qes2008, qes1998 and qes2007_panel. 25
  mapped crosswalk rows and 2 documentation rows (qes1998 intvote2,
  whose value labels are shifted in the source, and qes2012_panel
  interetrec), with their value maps, gates.csv cells, expected
  marginals and column hashes. Every weight of these studies needs
  review (the deposits do not document which weight to apply; the 1998
  codebooks say the undecided, would-spoil and refusing respondents were
  over-selected, which the weight, wave and crosswalk notes of qes1998
  record). The post-election wave of qes2012_panel gains its interview
  date (ResLastCallDate_last). No existing row, code or grade changed.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `interest_0_10`: Interest in politics (0-10)

How interested the respondent is in politics in general, from 0 (no
interest) to 10 (a great deal). Never pooled with or rescaled to the
four-point item (interest_4pt).

Family `interest` · type Numeric · timing Any time · status Experimental
· added in spec 1.0.0

**Valid range**: 0-10

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_interest_1` (cps) | `approximate` | A slider from 0 to 10 on the web, with no don’t-know option; the anchor is a number read out on a scale, with don’t know and refusal volunteered. | interest_0_10_slider |  | How interested are you in politics generally? Set the slider to a number from 0 to 10, where 0 means no interest at all, and 10 means a great deal of interest. |  | `cps_weight_general` | Not offered |
| qes2007 | `q15` (post) | `identical` (anchor) | Anchor row of the target. | interest_0_10 |  | Et toujours avec la même échelle, quel est votre intérêt pour la politique en général ? (Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt) |  | `pond` | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `interest_election_0_10`: Interest in the provincial election (0-10)

How interested the respondent was in the Quebec general election that
has just taken place, from 0 (no interest) to 10 (a great deal), asked
after it. Interest in an election, not in politics in general.

Family `interest` · type Numeric · timing Post-election · status
Experimental · added in spec 1.0.0

**Valid range**: 0-10

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q14` (post) | `comparable` | Same question and scale; the mode needs confirming, the anchor mixes telephone and web. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  |  | Offered explicitly |
| qes2007 | `q14` (post) | `identical` (anchor) | Anchor row of the target. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  | `pond` | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.

#### `interest_campaign_4pt`: Interest in the election campaign (4 points)

How interested the respondent is in the current Quebec election
campaign, on a four-point verbal scale, asked during it. Interest in a
campaign, not in politics in general (interest_4pt).

Family `interest` · type Ordinal · timing Pre-election · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name         | Label                 |
|------|--------------|-----------------------|
| 1    | `very`       | Very interested       |
| 2    | `quite`      | Quite interested      |
| 3    | `hardly`     | Hardly interested     |
| 4    | `not_at_all` | Not at all interested |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2007_panel | `interet` (pre) | `identical` (anchor) | Anchor row of the target. | interest_4pt | very, quite, hardly, not_at_all | Personnellement, vous intéressez-vous beaucoup, assez, peu ou pas du tout à la présente campagne électorale au Québec? |  | `pondam1` (needs review, not applied) | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

### Sociodemographics

#### `birth_year`: Year of birth

Year the respondent was born.

Family `birth` · type Numeric · timing Time-invariant · status
Experimental · added in spec 0.1.0

**Valid range**: 1900-2010

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_yob` (cps) | `identical` (anchor) | Anchor row of the target. | yob_list |  | Finally, in what year were you born? |  | `cps_weight_general` | Not offered |
| qes2018 | `ageyear_1` (post) | `comparable` | Same question (year of birth); year and month are entered, with an option not to answer (age is then asked instead); the anchor offers a list of years without one. | yob_month_entry |  | In what year were you born? | agensp: 1 = refused | `pond` | Offered explicitly |
| qes2014 | `QAGE` (post) | `comparable` | Same question (year of birth); the year is typed in a box, with an option not to answer; the anchor offers a list of years without one. | yob_entry |  | In what year were you born? |  | `POND` | Offered explicitly |
| qes2012 | `agex` (post) | `comparable` | Same question (year of birth); the year is typed in a box, with an option not to answer; the anchor offers a list of years without one. | yob_entry |  | In what year were you born? |  | `pond` | Offered explicitly |
| qes2008 | `q75` (post) | `comparable` | Same question (year of birth); the year is entered, with an option not to answer; the anchor offers a list of years without one. | yob_entry |  | To make sure we are talking to a cross section of Quebeckers, we need to get a little information about your background. First, in what year were you born? (Example: 1972) I prefer not answering 9999 |  |  | Not documented |
| qes2007 | `q75` (post) | `comparable` | Same question (year of birth); the year is entered, with a refusal code; the anchor offers a list of years without one. | yob_entry |  | To make sure we are talking to a cross section of Quebeckers, we need to get a little information about your background. First, in what year were you born? Enter year of birth Refusal 9999 |  | `pond` | Not documented |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.2.0 (2026-09-27): Waves, weights and eligibility: the new targets
  age (age in years as asked), birth_month, age_group3 (three age bands,
  from questions with these bands or bands that collapse into them
  exactly) and citizen, and survey_mode, which fills the interview mode
  of the qes2018_panel pre-election wave, where it varies by respondent.
  11 crosswalk rows: birth_year of qes2012, qes2014 and qes2018;
  birth_month and age of qes2018; age and citizen of qes2022; age_group3
  of qes2007_panel, qes2012_panel and qes2018_panel; survey_mode of
  qes2018_panel. With their value maps, gates.csv cells, expected
  marginals and column hashes. No existing row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `birth_month`: Month of birth

Month the respondent was born (1 = January). Asked with the year of
birth in some studies; with birth_year, it tells whether a respondent
born 18 years before the election year was 18 on election day.

Family `birth` · type Numeric · timing Time-invariant · status
Experimental · added in spec 0.2.0

**Valid range**: 1-12

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `agemonth_1` (post) | `identical` (anchor) | Anchor row of the target. | yob_month_entry |  | In what year were you born? | agensp: 1 = refused | `pond` | Offered explicitly |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.2.0 (2026-09-27): Waves, weights and eligibility: the new targets
  age (age in years as asked), birth_month, age_group3 (three age bands,
  from questions with these bands or bands that collapse into them
  exactly) and citizen, and survey_mode, which fills the interview mode
  of the qes2018_panel pre-election wave, where it varies by respondent.
  11 crosswalk rows: birth_year of qes2012, qes2014 and qes2018;
  birth_month and age of qes2018; age and citizen of qes2022; age_group3
  of qes2007_panel, qes2012_panel and qes2018_panel; survey_mode of
  qes2018_panel. With their value maps, gates.csv cells, expected
  marginals and column hashes. No existing row, code or grade changed.

#### `age`: Age in years

The respondent’s age in years at the interview, as asked. Never computed
from the year of birth, which gives the age only to within a year.

Family `age_years` · type Numeric · timing Any time · status
Experimental · added in spec 0.2.0

**Valid range**: 15-115

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_age_in_years` (cps) | `identical` (anchor) | Anchor row of the target. | age_list |  | To make sure we are talking to a cross section of Canadians, we need to get a little information about your background. First, how old are you? |  | `cps_weight_general` | Not offered |
| qes2018 | `agenum` (post) | `approximate` | The same question (How old are you?, a list of ages) asked only of the respondents who did not give their year and month of birth; the anchor asks every respondent. | age_list |  | How old are you? | agensp: 0 = inapplicable | `pond` | Offered explicitly |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.2.0 (2026-09-27): Waves, weights and eligibility: the new targets
  age (age in years as asked), birth_month, age_group3 (three age bands,
  from questions with these bands or bands that collapse into them
  exactly) and citizen, and survey_mode, which fills the interview mode
  of the qes2018_panel pre-election wave, where it varies by respondent.
  11 crosswalk rows: birth_year of qes2012, qes2014 and qes2018;
  birth_month and age of qes2018; age and citizen of qes2022; age_group3
  of qes2007_panel, qes2012_panel and qes2018_panel; survey_mode of
  qes2018_panel. With their value maps, gates.csv cells, expected
  marginals and column hashes. No existing row, code or grade changed.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `age_group3`: Age group (3 bands)

The respondent’s age group at the interview in three bands: 18-34,
35-54, 55 and over. Built only from a question with these bands or with
bands that collapse into them exactly, never from a guess.

Family `age_bands` · type Ordinal · timing Any time · status
Experimental · added in spec 0.2.0

**Levels**

| Code | Name       | Label       |
|------|------------|-------------|
| 1    | `a18_34`   | 18-34       |
| 2    | `a35_54`   | 35-54       |
| 3    | `a55_plus` | 55 and over |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018_panel | `age` (pre) | `identical` (anchor) | Anchor row of the target. | age_3bands | a18_34, a35_54, a55_plus | document 341538, age |  | `weight` | Not documented |
| qes2012_panel | `age` (pre) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | document 654292, age |  | `pondam1` (needs review, not applied) | Not documented |
| qes_crop_2007_2010 | `QAGE` (each poll) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (needs review, not applied) | Not documented |
| qes2008 | `q0age` (post) | `comparable` | Seven age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64, 65-74 and 75 and over); the anchor offers the three bands. | age_7bands | a18_34, a35_54, a55_plus | How old are you? |  |  | Not documented |
| qes2007_panel | `age` (any wave) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Not documented |
| qes1998 | `age` (pre) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (needs review, not applied) | Not documented |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.2.0 (2026-09-27): Waves, weights and eligibility: the new targets
  age (age in years as asked), birth_month, age_group3 (three age bands,
  from questions with these bands or bands that collapse into them
  exactly) and citizen, and survey_mode, which fills the interview mode
  of the qes2018_panel pre-election wave, where it varies by respondent.
  11 crosswalk rows: birth_year of qes2012, qes2014 and qes2018;
  birth_month and age of qes2018; age and citizen of qes2022; age_group3
  of qes2007_panel, qes2012_panel and qes2018_panel; survey_mode of
  qes2018_panel. With their value maps, gates.csv cells, expected
  marginals and column hashes. No existing row, code or grade changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `citizen`: Canadian citizen

Whether the respondent is a Canadian citizen, where the study asked.
Only citizens may vote in Quebec elections.

Family `citizenship` · type Categorical · timing Any time · status
Experimental · added in spec 0.2.0

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_citizen` (cps) | `identical` (anchor) | Anchor row of the target. | citizen_status | yes, no | Are you a… |  | `cps_weight_general` | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 0.2.0 (2026-09-27): Waves, weights and eligibility: the new targets
  age (age in years as asked), birth_month, age_group3 (three age bands,
  from questions with these bands or bands that collapse into them
  exactly) and citizen, and survey_mode, which fills the interview mode
  of the qes2018_panel pre-election wave, where it varies by respondent.
  11 crosswalk rows: birth_year of qes2012, qes2014 and qes2018;
  birth_month and age of qes2018; age and citizen of qes2022; age_group3
  of qes2007_panel, qes2012_panel and qes2018_panel; survey_mode of
  qes2018_panel. With their value maps, gates.csv cells, expected
  marginals and column hashes. No existing row, code or grade changed.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `age_group6`: Age group (6 bands)

The respondent’s age group at the interview in six bands: 18-24, 25-34,
35-44, 45-54, 55-64, 65 and over. Built only from a question with these
bands or with bands that collapse into them exactly, never from a guess.

Family `age_bands` · type Ordinal · timing Any time · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name       | Label       |
|------|------------|-------------|
| 1    | `a18_24`   | 18-24       |
| 2    | `a25_34`   | 25-34       |
| 3    | `a35_44`   | 35-44       |
| 4    | `a45_54`   | 45-54       |
| 5    | `a55_64`   | 55-64       |
| 6    | `a65_plus` | 65 and over |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `age` (pre) | `comparable` | The six bands of the anchor, by telephone; the codebooks do not say whether don’t know was offered. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | document 654292, age |  | `pondam1` (needs review, not applied) | Not documented |
| qes_crop_2007_2010 | `QAGE` (each poll) | `identical` (anchor) | Anchor row of the target. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (needs review, not applied) | Not documented |
| qes2008 | `q0age` (post) | `comparable` | Seven age bands, collapsed exactly into the six of the target (65-74 and 75 and over into 65 and over); the anchor offers the six bands, by telephone. | age_7bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | How old are you? |  |  | Not documented |
| qes2007_panel | `age` (any wave) | `comparable` | The six bands of the anchor, by telephone; the codebooks do not say whether don’t know was offered. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Not documented |
| qes1998 | `age` (pre) | `comparable` | The six bands of the anchor, by telephone, asked by both firms; the anchor’s codebook does not say whether don’t know was offered, nor does this one. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (needs review, not applied) | Not documented |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `gender`: Gender

The respondent’s gender, as asked or, in some telephone surveys, as
recorded by the interviewer. Most studies offered only man and woman (a
question on sex, in some); qes2022 also offered non-binary and another
gender.

Family `sex_gender` · type Categorical · timing Time-invariant · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name        | Label          |
|------|-------------|----------------|
| 1    | `man`       | Man            |
| 2    | `woman`     | Woman          |
| 3    | `nonbinary` | Non-binary     |
| 4    | `other`     | Another gender |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_genderid` (cps) | `comparable` | A question on gender identity on the web that adds non-binary and another gender to the two options of the anchor; expected to move the shares of man and woman very little. | gender_4 | man, woman, nonbinary, other | Are you…? |  | `cps_weight_general` | Not offered |
| qes2018 | `qsexe` (post) | `comparable` | Same two options on the web and the same stems as the anchor (English gender, French sexe); the French questionnaire with programmed values adds Statistics Canada’s note that transgender, transsexual and intersex respondents choose the sex they identify with most; the file has no value labels. | gender_2 | man, woman; not offered: nonbinary, other | What is your gender? |  | `pond` | Not offered |
| qes2018_panel | `sexfix` (pre) | `comparable` | Same two options, by web (850) and telephone (400); the codebook gives only the label Sexe, so whether it was asked or recorded is not documented. | gender_2 | man, woman; not offered: nonbinary, other | Sexe: |  | `weight` | Not documented |
| qes2014 | `QSEXE` (post) | `comparable` | Same two options on the web and the same stems as the anchor in English (What is your gender?) and French (Quel est votre sexe?), plus a no-answer option that is not in the file; kept at comparable, the grade it had before the review, because an identical grade on a row that pools fielding languages needs a second (human) reviewer. | gender_2 | man, woman; not offered: nonbinary, other | What is your gender? |  | `POND` | Not offered |
| qes2012 | `sexe` (post) | `identical` (anchor) | Anchor row of the target. | gender_2 | man, woman; not offered: nonbinary, other | What is your gender? |  | `pond` | Not offered |
| qes2012_panel | `sexe` (pre) | `comparable` | Same two options, by telephone; the codebook gives only the variable name, so whether it was asked or recorded is not documented. | sex_recorded | man, woman; not offered: nonbinary, other | document 654292, sexe |  | `pondam1` (needs review, not applied) | Not documented |
| qes_crop_2007_2010 | `SEXE` (each poll) | `comparable` | Same two options, recorded by the interviewer (not asked), by telephone. | sex_recorded | man, woman; not offered: nonbinary, other | INSCRIRE LE SEXE DU REPONDANT |  | `XPOND` (needs review, not applied) | Not offered |
| qes2008 | `q76` (post) | `comparable` | Asked (Un homme / Une femme) with the same two options; the mode of the study needs confirming (telephone in the deposit’s metadata), the anchor is web. | gender_2 | man, woman; not offered: nonbinary, other | Are you…? |  |  | Not offered |
| qes2007 | `q76` (post) | `comparable` | Recorded by the telephone interviewer without asking (NE PAS LIRE), and on the web for the web respondents (only the telephone script is deposited); same two options. | sex_recorded | man, woman; not offered: nonbinary, other | (DO NOT READ) Enter respondent’s gender: |  | `pond` | Not offered |
| qes2007_panel | `sexe` (any wave) | `comparable` | Same two options, recorded by the interviewer (not asked), by telephone. | sex_recorded | man, woman; not offered: nonbinary, other | INSCRIRE LE SEXE DU RÉPONDANT |  |  | Not offered |
| qes1998 | `sexe_post` (pre) | `comparable` | Same two options, by telephone; the codebooks give only the label SEXE (Sexe du répondant in CROP’s), so whether it was asked or recorded by the interviewer is not documented. | sex_recorded | man, woman; not offered: nonbinary, other | SEXE |  | `ponder3` (needs review, not applied) | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `education4`: Education (4 groups)

The respondent’s highest level of education in four groups: primary or
less, secondary, college (CEGEP or technical), university (completed or
not). From the highest level completed where the study asks for it, from
years of schooling otherwise (graded approximate). A vocational or trade
credential (the DEP) has no group of its own and is not placed alike in
every study: secondary where the question lists the DEP as a secondary
diploma (qes2018), college where it is a trade certificate or technical
training (qes2018_panel and, probably, the studies with no DEP option);
see the grade reasons.

Family `education` · type Ordinal · timing Time-invariant · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name         | Label                      |
|------|--------------|----------------------------|
| 1    | `primary`    | Primary or less            |
| 2    | `secondary`  | Secondary                  |
| 3    | `college`    | College (CEGEP, technical) |
| 4    | `university` | University                 |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_edu` (cps) | `comparable` | Highest level completed on the web, with options that collapse exactly into the four groups (some university counts as university, as in the anchor); no don’t-know option. | edu_levels | primary, secondary, college, university | What is the highest level of education that you have completed? |  | `cps_weight_general` | Not offered |
| qes2018 | `qscol` (post) | `comparable` | Same stem as the anchor on the web, with finer options (each year of secondary school, the vocational diploma DEP, the CEGEP programmes) that collapse exactly into the four groups; the file has no value labels. The vocational diploma (DEP, code 9) is secondary here, whereas the trade certificate of qes2018_panel (d3 code 4) is college, and the questionnaires without a DEP option (the qes2014 anchor, qes2012, qes2007, qes2008) probably put DEP holders under technical training (college): a vocational or trade credential does not fall in the same group in every study. | edu_levels | primary, secondary, college, university | À quel niveau se situe la dernière année de scolarité que vous avez complétée? |  | `pond` | Offered explicitly |
| qes2018_panel | `d3` (pre) | `approximate` | Highest level reached in Statistics Canada’s categories, by telephone and web: a trade certificate or registered apprenticeship is one option, counted as college (technical), and university certificates below the bachelor’s as university; the labels are cut at 60 characters in the file. In Quebec this option mostly holds the vocational diploma (DEP), which qes2018 (qscol code 9) counts as secondary: a vocational or trade credential does not fall in the same group in every study. | edu_statcan | primary, secondary, college, university | Quel est le plus haut niveau de scolarité que vous avez atteint ? |  | `weight` | Not documented |
| qes2014 | `QSCOL` (post) | `identical` (anchor) | Anchor row of the target. | edu_levels | primary, secondary, college, university | What is the highest level of education that you have completed? |  | `POND` | Not offered |
| qes2012 | `scol` (post) | `comparable` | Same French question as the anchor, on the web; the English options follow a British scheme (further or higher education), and one code of the file is in neither questionnaire. | edu_levels | primary, secondary, college, university | What is the highest level of education that you have completed? |  | `pond` | Offered explicitly |
| qes_crop_2007_2010 | `scol` (each poll) | `approximate` | Years of schooling in four ranges named after the levels (7 or fewer, primary; 8 to 12, secondary; 13 to 15, CEGEP; 16 or more, university), by telephone, not the highest level completed. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  | `XPOND` (needs review, not applied) | Not documented |
| qes2008 | `q77` (post) | `comparable` | Level of education with and without diploma, options that collapse exactly into the four groups; the mode needs confirming, the anchor is web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  |  | Offered explicitly |
| qes2007 | `q77` (post) | `comparable` | Level of education with and without diploma, options that collapse exactly into the four groups; mixed telephone and web interviews, the anchor is web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  | `pond` | Volunteered only |
| qes2007_panel | `scol` (any wave) | `approximate` | Years of schooling in four ranges named after the levels (7 or fewer, primary; 8 to 12, secondary; 13 to 15, CEGEP; 16 or more, university), by telephone, not the highest level completed. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  |  | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 2.0.0 (2026-09-27): Review corrections. MAJOR (a corrected gate,
  changed column hashes): the religion rows of qes2012 (q103) and
  qes2014 (Q63) are gated on their filter question (q102, Q62): those
  who belong to no religion are inapplicable (842 and 845) and those who
  preferred not to answer the filter question refused (32 and 44), where
  they were sysmis; values unchanged. A gate now applies to rules
  weight, date and string as to map and numeric, a gate on another rule
  is a V-S1 error, and gates.csv holds the cells of the gated string
  rows for V-D7. The qes2007_panel reported turnout reads the
  post-election turnout question voteoui (yes on election day, yes in
  advance, no) in place of the producer’s recode avote, with the same
  values and column hash; graded comparable (the same probed yes/no
  question as qes2012_panel), not approximate. Text only: the education4
  grade reasons of qes2018 and qes2018_panel and the target description
  name the placement of the vocational diploma (DEP: secondary in
  qes2018, college in qes2018_panel and probably where no DEP option is
  offered); the qes1998 language note of legacy.csv says the constant is
  a mother tongue only for the CREATEC rows (the home language for the
  426 CROP rows; the pooled definition is not confirmed yet); the
  religion description and legacy note name the gate; the qes2022
  evidence notes give codes and codebook pages only (its licence, CC
  BY-NC, keeps its wording and labels out of the package).
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `lang_mother`: Mother tongue

The first language the respondent learned at home in childhood and still
understands: French, English or another language. A respondent who
reports two first languages is a missing value (reason not_mappable),
never assigned to one of them.

Family `language` · type Categorical · timing Time-invariant · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name      | Label   |
|------|-----------|---------|
| 1    | `french`  | French  |
| 2    | `english` | English |
| 3    | `other`   | Other   |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `qlangue` (post) | `comparable` | Same stem on the web, asking for the main language first learned (langue principale); the file has no value labels. | lang_first | french, english, other | Quelle est la langue principale que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `pond` | Offered explicitly |
| qes2018_panel | `s1` (pre) | `comparable` | Shorter stem (first language learned and still understood, without ‘at home in childhood’), by telephone and web; one language only. | lang_first | french, english, other | Quelle est la première langue que vous avez apprise et que vous comprenez toujours? |  | `weight` | Not documented |
| qes2014 | `QLANG` (post) | `comparable` | Same stem on the web, with three options for two first languages, which are missing values here (not_mappable). | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `POND` | Offered explicitly |
| qes2012 | `langu` (post) | `identical` (anchor) | Anchor row of the target. | lang_first | french, english, other | What is the language you first learned at home in your childhood and that you still understand? |  | `pond` | Offered explicitly |
| qes2012_panel | `lmat` (pre) | `comparable` | Mother tongue defined as the first language learned and still understood, by telephone; one language only. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire celle que vous avez appris à parler en premier et que vous comprenez toujours? |  | `pondam1` (needs review, not applied) | Not documented |
| qes_crop_2007_2010 | `lmat` (each poll) | `comparable` | Mother tongue asked without a definition, by telephone; one language only. | lang_mother | french, english, other | Quelle est votre langue maternelle? |  | `XPOND` (needs review, not applied) | Not documented |
| qes2008 | `langu` (post) | `comparable` | Same stem and options; the mode needs confirming, the anchor is web. | lang_first | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  |  | Offered explicitly |
| qes2007 | `langu` (post) | `comparable` | Same stem, with options for two first languages, which are missing values here (not_mappable); mixed telephone and web interviews, the anchor is web. | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  | `pond` | Volunteered only |
| qes2007_panel | `lmat` (any wave) | `comparable` | Mother tongue defined as the first language learned and still spoken, by telephone; one language only. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire la première langue que vous avez apprise et que vous pouvez encore parler? |  |  | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 2.0.0 (2026-09-27): Review corrections. MAJOR (a corrected gate,
  changed column hashes): the religion rows of qes2012 (q103) and
  qes2014 (Q63) are gated on their filter question (q102, Q62): those
  who belong to no religion are inapplicable (842 and 845) and those who
  preferred not to answer the filter question refused (32 and 44), where
  they were sysmis; values unchanged. A gate now applies to rules
  weight, date and string as to map and numeric, a gate on another rule
  is a V-S1 error, and gates.csv holds the cells of the gated string
  rows for V-D7. The qes2007_panel reported turnout reads the
  post-election turnout question voteoui (yes on election day, yes in
  advance, no) in place of the producer’s recode avote, with the same
  values and column hash; graded comparable (the same probed yes/no
  question as qes2012_panel), not approximate. Text only: the education4
  grade reasons of qes2018 and qes2018_panel and the target description
  name the placement of the vocational diploma (DEP: secondary in
  qes2018, college in qes2018_panel and probably where no DEP option is
  offered); the qes1998 language note of legacy.csv says the constant is
  a mother tongue only for the CREATEC rows (the home language for the
  426 CROP rows; the pooled definition is not confirmed yet); the
  religion description and legacy note name the gate; the qes2022
  evidence notes give codes and codebook pages only (its licence, CC
  BY-NC, keeps its wording and labels out of the package).
- 2.0.1 (2026-09-27): Text only: the legacy.csv rows of
  sovereignty_support and sovereignty give qes2007_panel the reason of
  qes2007, qes2008 and qes1998 (it asked the 1995 question, see
  sov_partnership_1995) and give the CROP polls their own (the first
  referendum question, intvoterefa, has only a truncated label and no
  deposited questionnaire, so its wording is unknown and it is not
  mapped); the cause column of legacy.csv (attr(,
  “legacy_na_columns”)\$cause of get_qes_master() and get_decon()) has
  descriptive values (reported_vote_only, independence_question_only,
  no_valid_source, not_harmonized_yet) instead of internal decision ids,
  and the text of legacy.csv, crosswalk.csv, valuemaps.csv and this
  changelog says each decision in words. No row, code, grade, marginal
  or hash changed.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.

#### `born_canada`: Born in Canada

Whether the respondent was born in Canada. From a question on birthplace
(Quebec, elsewhere in Canada, outside Canada) where the study asks that
one, collapsed exactly.

Family `birthplace` · type Categorical · timing Time-invariant · status
Experimental · added in spec 1.0.0

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_borncda` (cps) | `identical` (anchor) | Anchor row of the target. | born_canada | yes, no | Were you born in Canada? |  | `cps_weight_general` | Not offered |
| qes2018 | `q69` (post) | `comparable` | A question on birthplace (in Quebec, elsewhere in Canada, outside Canada), collapsed exactly into born in Canada or not, on the web; the anchor asks directly. The file has no value labels. | birthplace3 | yes, no | Où êtes-vous né(e)? |  | `pond` | Offered explicitly |
| qes2014 | `Q65` (post) | `comparable` | A question on birthplace (in Quebec, elsewhere in Canada, outside Canada), collapsed exactly into born in Canada or not, on the web; the anchor asks directly. | birthplace3 | yes, no | Where were you born? |  | `POND` | Offered explicitly |
| qes2012 | `q105` (post) | `comparable` | A question on birthplace (in Quebec, elsewhere in Canada, outside Canada), collapsed exactly into born in Canada or not, on the web; the anchor asks directly. | birthplace3 | yes, no | Where were you born? |  | `pond` | Offered explicitly |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `income_native`: Household income (each study’s own brackets)

The respondent’s household income before taxes as each study recorded
it: the text of the study’s own bracket (or the amount, where the study
asked for one). Brackets differ between studies, so the values are not
comparable across studies; don’t know and refusals are missing values.

Family `income` · type Text · timing Time-invariant · status
Experimental · added in spec 1.0.0

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_income` (cps) | `approximate` | The amount in dollars, typed on the web, not a bracket. | income_amount |  | What was your total household income, before taxes, for the year 2021? Be sure to include income from all sources, to the nearest thousand dollars. |  | `cps_weight_general` | Not offered |
| qes2018_panel | `d5` (pre) | `approximate` | Seven brackets from less than \$20,000 to \$150,000 and more, by telephone and web, not the anchor’s nine; the deposited question text is cut, so whether income is before taxes and for which year is not documented. | income_7brackets |  | Laquelle des catégories suivantes décrit le mieux le revenu total de votre foyer, c’est-à-dire le total des revenus |  | `weight` | Not documented |
| qes2014 | `Q57` (post) | `comparable` | The same nine brackets as the anchor, for the previous year, on the web; no don’t-know option. | income_9brackets |  | Parmi les catégories suivantes, laquelle reflète le mieux le revenu total avant impôt de tous les membres de votre foyer pour l’année 2013? 3. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Était-ce: |  | `POND` | Not offered |
| qes2012 | `reven` (post) | `identical` (anchor) | Anchor row of the target. | income_9brackets |  | And now your total household income before taxes for 2011. That includes income from all sources such as savings, pensions, rent, as well as wages. Was it: |  | `pond` | Offered explicitly |
| qes_crop_2007_2010 | `revenu` (each poll) | `approximate` | Five brackets of \$20,000 up to \$80,000 and more, by telephone, not the anchor’s nine. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu |  | `XPOND` (needs review, not applied) | Not documented |
| qes2008 | `q78` (post) | `approximate` | Ten brackets, from under \$20,000 then \$10,000 steps to more than \$100,000, not the anchor’s nine; asked about the year before the election (2007). | income_10brackets |  | And now what is your total household income before taxes for 2007? That includes income FROM ALL SOURCES such as savings, pensions, rent, as well as wages. Was it … |  |  | Offered explicitly |
| qes2007 | `q78` (post) | `approximate` | Ten brackets of \$10,000 up to more than \$100,000, not the anchor’s nine; asked about the year before the election. | income_10brackets |  | Et maintenant le revenu total de votre ménage avant impôts en 2006. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Est-ce…? |  | `pond` | Offered explicitly |
| qes2007_panel | `revenu` (any wave) | `approximate` | Five brackets of \$20,000 up to \$80,000 and more, by telephone, not the anchor’s nine. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu annuel total, avant impôts et déductions, de tous les membres de votre foyer, en vous incluant? Est-ce… |  |  | Volunteered only |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38
  crosswalk rows of qes1998, qes2007_panel, qes2012_panel and
  qes_crop_2007_2010 that the automated double review of spec 4.0.0
  signed off on their content are stable, and applied by
  qes_harmonize(), get_qes_master() and get_decon() by default; they
  were held in review only because the recommended weights of their
  waves need review. Their review_note still says the review was
  automated, not human, and says that the weight is tracked apart
  (dev/open-questions.md Q1). The release check V-S13 no longer fails a
  stable row on a study-wave whose recommended weight needs review; it
  still requires that a recommended weight is never calibrated on vote
  or turnout, and one recommended weight per study-wave (none where
  every weight is calibrated). The weights that need review stay
  unapplied: weight_pre and weight_post are NA there, with the message
  qesR_message_weight_review, and in get_qes_master() the reason
  not_reviewed with the cause weight_needs_review. qes2014 QSEXE
  (gender) is stable at comparable, the grade it had before the review
  (the English stem the review filled is kept); identical waits for a
  human second reviewer. Text only: the two documentation-only rows
  (rule none), qes2012_panel interetrec and qes1998 intvote2, have a
  wording_ref to their codebook entry, which V-S11 requires of a stable
  row; in legacy.csv, the note of each study’s survey_weight row names
  the weight and its registry status (the CROP XPOND and the
  qes2012_panel pond need review, the qes2008 pond is calibrated on the
  vote, the qes2007_panel pond is not registered), the weight_pre and
  weight_post definitions say NA where the weight needs review, and the
  intended blanks of get_qes_master() have a row of their own with a
  cause and a note (qes1998 education, qes2018 income and religion:
  invalid_044_source; qes2012_panel political_interest:
  not_comparable_source; qes2022 language: not_harmonized_yet). No value
  map, gate, level set, expected marginal or column hash changed: MINOR,
  rows are only added to the default output.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.

#### `religion`: Religion (each study’s own categories)

The respondent’s religion as each study recorded it: the text of the
study’s own category. Categories differ between studies. Where the
question was asked only of respondents who belong to a religion, the
others are missing values: reason inapplicable for those who said they
belong to none, refused for those who would not answer the filter
question, which is the gate of the crosswalk row.

Family `faith` · type Text · timing Time-invariant · status Experimental
· added in spec 1.0.0

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_religion` (cps) | `approximate` | A single question with a long list of denominations and none (no filter question), on the web; not the anchor’s six categories. | religion_list |  | Please tell me what is your religion, if you have one? |  | `cps_weight_general` | Not offered |
| qes2014 | `Q63` (post) | `comparable` | Same filter question and categories as the anchor, with a slightly longer question stem, on the web. | religion_list |  | Which religion do you belong to? | Q62: 2 = inapplicable, 9 = refused | `POND` | Not offered |
| qes2012 | `q103` (post) | `identical` (anchor) | Anchor row of the target. | religion_list |  | Which religion? | q102: 2 = inapplicable, 3 = refused | `pond` | Not offered |

**History**

- 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked
  against the original files, in review, for qes2012, qes2014, qes2018,
  qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level
  sets, waves and weights.
- 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of
  gate code and source code among wave members for the 28 projectable
  rows of the studies whose metadata ships, and expected/marginals.csv,
  the projected unweighted marginals of the 28 projectable rows of the
  studies whose metadata ships. No row, code or grade changed.
- 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5
  of each harmonized column (study, target) that qes_harmonize() gives
  on the pinned files, for the 35 mapped rows, checked on the original
  files by the live tests (V-L1). No row, code or grade changed.
- 1.0.0 (2026-09-27): The legacy switch (qesR 0.7.0): get_qes_master()
  and get_decon() are rendered from the engine, by the new table
  legacy.csv (the render of every legacy column from the targets,
  checked by the new validator rule V-S18). 13 new targets with 58
  crosswalk rows for the 11 studies: gender, education4 (four groups),
  lang_mother (two first languages are not_mappable: assigned to
  neither), born_canada, income_native and religion (each study’s own
  categories as text: rule string with the new argument from_label =
  TRUE, the value label of the code), pid_fed, interest_0_10,
  interest_election_0_10 and interest_campaign_4pt (never pooled with
  interest_4pt), age_group6, turnout_prov_likely and
  vote_prov_intent_other (qes2022), and the 2007 panel’s reported
  turnout (avote). A crosswalk row may name the wave \* for a target of
  timing static or any: it applies to the members of any wave of the
  study (the 2007 panel’s time-invariant items, answered in whichever
  wave the respondent took part). MAJOR: the 2007 panel’s age_group3 row
  moves from its pre-election wave to wave \*, so its 391 respondents
  reached only after the election now have an age group (their column
  hash changes); every other existing row, code, grade, marginal and
  hash is unchanged. Every new row is in review, checked against the
  original files and documents.
- 2.0.0 (2026-09-27): Review corrections. MAJOR (a corrected gate,
  changed column hashes): the religion rows of qes2012 (q103) and
  qes2014 (Q63) are gated on their filter question (q102, Q62): those
  who belong to no religion are inapplicable (842 and 845) and those who
  preferred not to answer the filter question refused (32 and 44), where
  they were sysmis; values unchanged. A gate now applies to rules
  weight, date and string as to map and numeric, a gate on another rule
  is a V-S1 error, and gates.csv holds the cells of the gated string
  rows for V-D7. The qes2007_panel reported turnout reads the
  post-election turnout question voteoui (yes on election day, yes in
  advance, no) in place of the producer’s recode avote, with the same
  values and column hash; graded comparable (the same probed yes/no
  question as qes2012_panel), not approximate. Text only: the education4
  grade reasons of qes2018 and qes2018_panel and the target description
  name the placement of the vocational diploma (DEP: secondary in
  qes2018, college in qes2018_panel and probably where no DEP option is
  offered); the qes1998 language note of legacy.csv says the constant is
  a mother tongue only for the CREATEC rows (the home language for the
  426 CROP rows; the pooled definition is not confirmed yet); the
  religion description and legacy note name the gate; the qes2022
  evidence notes give codes and codebook pages only (its licence, CC
  BY-NC, keeps its wording and labels out of the package).
- 4.0.0 (2026-09-28): Review sign-off. An automated double review
  checked the 132 crosswalk rows against the original files and
  documents (one pass on codes and data, one on wording and
  comparability, adjudicated where they disagreed); it is not a human
  review, and reviewed_by says so. reviewed_on is 2026-09-27, and the
  new crosswalk column review_note says what the review corrected and
  why a row stays in review (V-S11 requires it on a reviewed row left in
  review). 93 rows are signed off (status stable) and applied by
  qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998,
  qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended
  weights need review (a stable row there fails the release check
  V-S13), and the qes2014 gender row, whose grade was raised to
  identical and needs a second reviewer. get_qes_master() and
  get_decon() now apply signed-off rows only (include_draft = FALSE): a
  column whose question is in a row still in review is NA, reason
  not_reviewed in attr(, “legacy_na_columns”), and attr(, “source_map”)
  gains the status of each row. MAJOR (changed column hashes): the
  qes2022 income amount 0, a blank that the survey sent to the bracket
  follow-up cps_income2, is a missing value (no_answer); the qes2022
  typed other-party text is gated on cps_turnout as its parent row (3 to
  5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a
  and rv1ab comparable to approximate (the stem also asks those who
  voted in advance for their vote; a push filter narrower than the
  anchor’s); qes2014 QSEXE comparable to identical (the anchor’s stems
  in both languages); qes2014 QSCOL dk_offered none; qes2022
  cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP
  intentions dk_offered volunteered, with their French wording from
  CROP’s reports. Text only: the wording, grade reasons, evidence and
  notes of 36 rows corrected (each named in its review_note), among them
  the qes2007_panel counts of wave members and where its time-invariant
  items were asked. In gates.csv, typed text is counted as one token and
  an empty text as system missing.
- 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted
  by the owner on 2026-09-28; it carries the study’s licence, CC BY-NC
  4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get
  their wording_en and wording_fr, quoted from the study’s bilingual
  codebook (file 7449514; for the typed other-party text, the stem of
  cps_votechoice1 with its option), and its 65 value-map rows the value
  label of the pinned file (source_label) in place of the md5 of that
  label (source_label_hash). gates.csv gains the 252 cells of the 16
  projectable or gated qes2022 rows and expected/marginals.csv the 225
  marginal cells of its 15 projectable rows, the counts the
  build-ignored data-raw/nc/ held until now for CI (identical, rebuilt
  from the pinned file by data-raw/build_sources.R and
  data-raw/project_marginals.R). The release check V-S11 no longer
  forbids wording and labels for a study whose metadata does not ship
  (every study’s does). No row, map, gate, grade, level set, recorded
  marginal or column hash changed: MINOR, keys are only added to
  expected/.
