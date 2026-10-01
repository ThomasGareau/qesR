# Variable reference

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md)*

The harmonized variables (“targets”) of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
study by study. The page is generated from the rules that ship with
qesR, so it always describes what the installed version applies.

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
returns the same information as data frames, and
`qes_provenance(x, level = "spec")` the version of the rules that
produced harmonized data.

## How to read this reference

Each target is one question stimulus: a different wording, scale, timing
or format makes another target; a pooled variable (last chapter)
combines targets into one column and records which one each value comes
from. For each study, the coverage table of a target gives:

- **Source**: the variable and the wave that asked it;
- **Grade** and **Reason**: how comparable the study’s question is to
  the target’s anchor question, and why;
- **Instrument** and **Levels offered**: the item format and the answers
  it offered (the others are structural zeros);
- **Wording**: the question text, or the document and page that give it;
- **Filter**: the filter question, and what each of its codes means;
- **Weight**: the study’s recommended weight for that wave (marked when
  it is not usable yet: its weight columns are then `NA`);
- **Don’t know**: whether “don’t know” was offered.

A row marked *awaiting sign-off* is applied only if you ask for it
(`include_draft = TRUE`).

The wording and labels of `qes2022` quoted on this page come from *2022
Quebec Election Study* (Mahéo, Bélanger, Stephenson and Harell, 2023,
<https://doi.org/10.7910/DVN/PAQBDR>) and keep its licence, [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/). See [Citing qesR
and the
studies](https://thomasgareau.github.io/qesR/articles/citations.html#licences-and-attribution)
for the attribution.

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
wave; crosswalk rows exist only for the waves whose mode varies by
respondent.

Family `interview_mode` · type Categorical · timing Any time

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

### Vote and turnout

#### `vote_prov_recall`: Provincial vote (recall)

Party the respondent reports having voted for in the Quebec general
election of the study, asked after that election. Nonvoters, spoiled
ballots and respondents not eligible or not registered are missing
values with a reason, never a party.

Family `vote_prov` · type Categorical · timing Post-election

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
| qes2012_panel | `voteprov` (post) | `approximate` | Unprompted telephone recall (the options are not read) after a turnout question that probes election day or advance poll; the anchor is a web list. | vote_recall_unprompted | CAQ, PLQ, PQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | For which party did you vote for? (DO NOT READ) |  | `pond_post` (not usable yet) | Not offered |
| qes2008 | `q12a` (post) | `comparable` | Same question (party voted for); the list names the ADQ and not the CAQ or ON, which did not exist; the turnout question has no don’t-know code; telephone by the deposit metadata, the anchor is web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for? | q11: 2 = not_voted, 9 = refused |  | Not documented |
| qes2007 | `q12` (post) | `comparable` | Same question (party voted for, the parties named in the stem); the list names the ADQ and not the CAQ or ON, which did not exist; the study mixes telephone and web interviews (only the telephone script is deposited), the anchor is web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for? The Liberal Party, Parti Québécois, ADQ, Québec solidaire, the Green Party or another party? | q11: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Not documented |
| qes2007_panel | `vote` (post) | `approximate` | Unprompted telephone recall (options not read) after a two-step turnout question; the anchor is a web list. | vote_recall_unprompted | ADQ, PLQ, PQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Whom did you vote for? |  | `pond_tot_am1` (not usable yet) | Not offered |
| qes1998 | `q3post` (post) | `comparable` | Same question (party voted for, from a list read) by telephone, with the same stem in both firms’ questionnaires; CREATEC’s own Q3 uses other codes (1 PLQ, 2 PQ, 3 ADQ, 4 another party) with no Parti Égalité, and the pooled file puts them on CROP’s codes, where the Parti Égalité is starred (not read) and never chosen; the list names the ADQ, not the CAQ; the anchor is web. | vote_recall_list | ADQ, PLQ, PQ, other; not offered: CAQ, QS, PVQ, PCQ, ON | 3\. Pour lequel des partis suivants avez-vous voté? |  | `ponder3` (not usable yet) | Not offered |

#### `vote_prov_intent`: Provincial vote intention

Party the respondent intends to vote for in the coming Quebec general
election, asked before it, at the first question, without the push
question for the undecided. Would not vote, none or would spoil is an
answer (level no_party), not a missing value.

Family `vote_prov` · type Categorical · timing Pre-election

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
| qes2012_panel | `intvoteprov1` (pre) | `approximate` | The stem asks which party the respondent would vote for ‘or would be tempted to vote for’, which softens the first question toward a lean; the pre-election questionnaire is not deposited (wording from the variable label and the codebook, file 654292); telephone, like the anchor. | vote_intent_or_lean_list | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; not offered: PCQ, ADQ | Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? |  | `pondam1` (not usable yet) | Not documented |
| qes_crop_2007_2010 | `intvoteprova` (each poll) | `comparable` | Same first question as the anchor, word for word, by the same firm (CROP) and telephone mode, with the same parties and leaders read in rotation, and would-not-vote and don’t know/refusal not read; the polls are monthly omnibus polls between elections, not a campaign panel, and the wording is documented for three of the 24 polls only (CROP reports of May 2008, January 2009 and March 2009; no questionnaire is deposited). | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `XPOND` (not usable yet) | Volunteered only |
| qes2007_panel | `intvote1` (pre) | `identical` (anchor) | Anchor row of the target. | vote_intent_list | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… |  | `pondam1` (not usable yet) | Volunteered only |

#### `vote_prov_intent_push`: Provincial vote intention, undecided pushed

Vote intention in which respondents who named no party at the first
question (the undecided and, in some studies, also those who would not
vote, would vote for none or refused) were asked which party they lean
toward (a push question), combined into one variable. A different
stimulus from vote_prov_intent, and a separate target; the pooled
variable vote_choice uses it before vote_prov_intent, and
vote_choice\_\_type records which one a value comes from.

Family `vote_prov` · type Categorical · timing Pre-election

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
| qes2022 | `cps_votechoice1` + `cps_votechoice2` + `cps_votelean` (cps) | `approximate` | Web list with don’t know and refusal shown: the first question (cps_votechoice1) asked of respondents certain or likely to vote, a conditional one (‘if you decide to vote’, cps_votechoice2) asked of those unlikely to vote, and the lean question (cps_votelean) asked of those who did not know at either; no would-not-vote option; the anchor is a telephone item asked of everyone. | intent_lean_push | PLQ, PQ, CAQ, QS, PCQ, other; not offered: PVQ, ON, ADQ, no_party | Which party do you think you will vote for? \[If unlikely to vote:\] If you decide to vote, which party do you think you would vote for? \[If don’t know:\] Is there a party you are leaning towards? | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Offered explicitly |
| qes2018_panel | `rv1ab` (pre) | `approximate` | Producer’s combination of rv1a with the push question rv1b, in mixed telephone and web mode; the anchor is a telephone push. The push filter is narrower than the anchor’s: respondents who said they would not vote or support no party (63) were not pushed, whereas the anchor pushed them, and 31 of the 233 undecided, all interviewed by telephone, were not asked rv1b and stay undecided. rv1a, whose answer rv1ab keeps for codes 1-6 (1,017 respondents), tells those who already voted in advance polls to give that vote, so part of the answers report a vote already cast. | intent_lean_push | PLQ, PQ, CAQ, QS, other, no_party; not offered: PVQ, PCQ, ON, ADQ | En pensant à ce que vous ressentez maintenant, si une élection PROVINCIALE était tenue demain, le candidat de quel parti appuieriez-vous probablement? Si vous avez déjà voté par anticipation, veuillez indiquer pour quel parti. (relance, rv1b : Et pour quel parti diriez-vous que vous auriez tendance à voter?) |  | `weight` | Not documented |
| qes2012_panel | `intvoteprov` (pre) | `approximate` | The producer’s combination of the first question (which already asks for a party the respondent ‘would be tempted to vote for’) and the push; the pre-election questionnaire is not deposited (wording from the variable label and the codebook, file 654292); telephone, like the anchor. | intent_lean_push | PLQ, PQ, CAQ, QS, PVQ, ON, other, no_party; not offered: PCQ, ADQ | Q2+Q3 - Si des élections provinciales devaient avoir lieu aujourd’hui, pour lequel des partis suivants voteriez-vous ou seriez-vous tenté de voter? (relance : Peut-être que votre choix n’est pas définitif, mais y a-t-il tout de même un parti que vous seriez tenté d’appuyer?) |  | `pondam1` (not usable yet) | Not documented |
| qes_crop_2007_2010 | `intvoteprov` (each poll) | `comparable` | The producer’s combination of the first question and the push, as in the anchor, by the same firm and mode; CROP telephone polls; the deposited codebook (file 341537) gives only truncated labels, but CROP’s own La Presse reports (May 2008, January 2009) print the same stem and push as the anchor, with parties and leaders read in rotation and don’t know recorded only if volunteered; not identical because these are monthly omnibus polls, most fielded outside a campaign (the reference election is 2008 or 2012 depending on the poll), on a regionally stratified sample (500 Montréal, 200 Québec, 300 elsewhere). | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Voteriez-vous pour… (relance : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter? Est-ce…) |  | `XPOND` (not usable yet) | Volunteered only |
| qes2007_panel | `intvote` (pre) | `identical` (anchor) | Anchor row of the target. | intent_lean_push | ADQ, PLQ, PQ, QS, PVQ, other, no_party; not offered: CAQ, PCQ, ON | S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? (relance, Q5 : pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `pondam1` (not usable yet) | Volunteered only |
| qes1998 | `intvote` (pre) | `approximate` | The producer’s combination of the first question and the push, but two firms’ telephone polls pooled in one variable (CREATEC and CROP, each with its own questionnaire; CREATEC’s is not deposited); CREATEC did not ask the 79 respondents who, at its turnout question, said they would probably not (22) or certainly not (35) vote, or did not know or refused (22), whereas CROP asked everyone. | intent_lean_push | ADQ, PLQ, PQ, other, no_party; not offered: CAQ, QS, PVQ, PCQ, ON | 7a. S’il y avait des élections provinciales aujourd’hui au Québec, pour lequel des partis suivants voteriez-vous? Est-ce… (relance, 7b : Peut-être n’êtes-vous pas complètement décidé(e), mais actuellement pour lequel de ces partis seriez-vous tenté(e) de voter?) |  | `ponder3` (not usable yet) | Not documented |

**Not used**

- qes1998 `intvote2` (pre): `not_comparable`. The value labels are
  shifted in the source: the counts are those of vpl, whose code 1 is
  ADQ, 3 PLQ and 4 PQ, but intvote2 labels them PLQ, ADQ and Parti
  Égalité.

#### `turnout_prov_recall`: Voted in the provincial election (recall)

Whether the respondent reports having voted in the Quebec general
election of the study, asked after that election. Respondents not
eligible or not registered are missing values with a reason.

Family `turnout_prov` · type Categorical · timing Post-election

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
| qes2012_panel | `participation` (post) | `comparable` | Same yes/no question; a yes is probed for election day or advance poll, both yes; no don’t-know code (the anchor offers one); telephone, the anchor is web. | turnout_yesno_probe | yes, no | First, can you tell me if you have voted in the recent Quebec election? (IF YES, PROBE) |  | `pond_post` (not usable yet) | Not offered |
| qes2008 | `q11` (post) | `comparable` | Same yes/no question, but no don’t-know code (the anchor offers one); telephone by the deposit metadata, the anchor is web. | turnout_yesno | yes, no | Did you vote in the last provincial election? |  |  | Not offered |
| qes2007 | `q11` (post) | `comparable` | Same yes/no question with a don’t-remember code; the study mixes telephone and web interviews (only the telephone script is deposited), the anchor is web. | turnout_yesno | yes, no | Did you vote in the provincial election? |  | `pond` | Not documented |
| qes2007_panel | `voteoui` (post) | `comparable` | Same yes/no question; a yes is probed for election day or advance poll, both yes; no don’t-know code (the anchor offers one); telephone, the anchor is web. | turnout_yesno_probe | yes, no | First, can you tell me if you have voted at the last Quebec election that was just held? |  | `pond_tot_am1` (not usable yet) | Not offered |
| qes1998 | `q1post` (post) | `comparable` | Same yes/no question by telephone, with the same stem for both firms; no don’t-know code (the anchor offers one); the anchor is web. | turnout_yesno | yes, no | 1\. Pouvez-vous me dire si vous avez voté à l’élection du 30 novembre dernier? |  | `ponder3` (not usable yet) | Not offered |

#### `turnout_prov_likely`: Likelihood of voting in the provincial election

How likely the respondent says they are to vote in the coming Quebec
general election, asked before it. Having already voted (in advance
polls) is an answer. A different stimulus from turnout_prov_recall
(reported turnout), and a separate target; the pooled variable turnout
uses it only when asked to (types = list(turnout = c(“recall”,
“intention”))).

Family `turnout_prov` · type Ordinal · timing Pre-election

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

#### `vote_prov_intent_other`: Provincial vote intention: another party (text)

The party the respondent typed after choosing another party at the
vote-intention question (vote_prov_intent, level other), as typed. Open
text, not harmonized.

Family `vote_prov` · type Text · timing Pre-election

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_votechoice1_8_TEXT` (cps) | `identical` (anchor) | Anchor row of the target. | open_text |  | Which party do you think you will vote for? \[Another party (please specify)\] | cps_turnout: 3 = inapplicable, 4 = inapplicable, 5 = inapplicable, 6 = ineligible | `cps_weight_general` | Not offered |

#### `vote_prov_prev`: Provincial vote at the previous election (recall)

Party the respondent reports having voted for in the Quebec general
election before the study’s election (election_ref gives which one).
Recalled years later: known to lean toward the winner of that election.
Nonvoters, spoiled ballots and respondents not eligible then are missing
values with a reason, never a party.

Family `vote_prov_past` · type Categorical · timing Any time

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
| qes2022 | `cps_qc_vote_2018` (cps) | `comparable` | The 2018 vote, asked during the 2022 campaign after a separate turnout question (cps_qc_turnout_2018: no is not_voted, not eligible is ineligible); web, four parties listed; no don’t-know, refusal or spoiled-ballot option, so such answers were typed under ‘another party’ and stay in other. | vote_recall_prev_list | PLQ, PQ, CAQ, QS, other; not offered: PVQ, PCQ, ON, ADQ | Which party did you vote for in the Quebec election in 2018? | cps_qc_turnout_2018: 2 = not_voted, 3 = ineligible | `cps_weight_general` | Not offered |
| qes2018 | `q9` (post) | `comparable` (awaiting sign-off) | The 2014 vote (‘4 years ago’), asked after the 2018 election; the list names four parties (the others are ‘another party’); not asked of the 270 respondents who were not asked the 2018 vote either (under 18 in 2018 or age unknown). | vote_recall_prev_list | PLQ, PQ, CAQ, QS, other; not offered: PVQ, PCQ, ON, ADQ | Which party did you vote for 4 years ago in the previous provincial election, held on April 7, 2014? |  | `pond` | Offered explicitly |
| qes2014 | `Q6` (post) | `identical` (anchor) | Anchor row of the target: the 2012 vote, asked after the 2014 election. | vote_recall_prev_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | Which party did you vote for in the last provincial election, held on September 4, 2012? |  | `POND` | Not offered |
| qes2008 | `q13` (post) | `comparable` (awaiting sign-off) | The March 2007 vote, asked twenty months later, after the 2008 election, by telephone; the list names the parties of 2007; ‘aucun’ (none, 8 rows) could be a spoiled ballot or no vote, so it is not mapped; code 95 is ‘I did not vote / I spoiled my ballot’ in the English questionnaire (197296) and ‘n’a pas voté’ in the file, so its not_voted rows may include spoiled ballots; the 5 respondents born in 1990 (under 18 in March 2007) were asked and all answered 95, so they are not_voted, not ineligible, and those born in 1989 cannot be told apart without a birth date. | vote_recall_prev_list | PLQ, PQ, QS, PVQ, ADQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for at the provincial election of March 26, 2007? |  |  | Not documented |

#### `vote_fed_recall`: Federal vote at the last federal election (recall)

Party the respondent reports having voted for in the last Canadian
federal election before the study (each row’s wording names it: January
2006, October 2008, May 2011, 2021). Nonvoters, spoiled ballots and
respondents not eligible then are missing values with a reason, never a
party.

Family `vote_fed` · type Categorical · timing Any time

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

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_pastpartyvote` (cps) | `comparable` | The 2021 federal vote, asked during the 2022 provincial campaign after a separate turnout question (cps_pastvote); web, the PPC listed. | vote_fed_recall_list | LPC, CPC, NDP, BQ, GPC, PPC, other | Which party did you vote for in the 2021 Canadian federal election? | cps_pastvote: 2 = not_voted, 3 = ineligible | `cps_weight_general` | Not offered |
| qes2012 | `q27` (post) | `identical` (anchor) | Anchor row of the target: the May 2011 federal vote, after a separate turnout question (q22). | vote_fed_recall_list | LPC, CPC, NDP, BQ, GPC, other; not offered: PPC | And in the last Canadian federal election in May 2011? Did you vote for the: | q22: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Offered explicitly |
| qes2008 | `q74` (post) | `comparable` | The October 2008 federal vote, asked two months later, with not voting and a spoiled ballot as answers of the same question; telephone by the deposit metadata (mode to be confirmed). | vote_fed_recall_list | LPC, CPC, NDP, BQ, GPC, other; not offered: PPC | At the last FEDERAL election in OCTOBER 2008, for which party did you vote? |  |  | Not documented |
| qes2007 | `q74` (post) | `approximate` | The January 2006 federal vote, asked about 15 months later with not voting and a spoiled ballot as answers of the same question; the deposited telephone script says not to read the list (unprompted), but the study mixes telephone (1,003) and web (1,172) interviews and only the telephone script is deposited, so how the web version showed the options is unknown. | vote_fed_recall_unprompted | LPC, CPC, NDP, BQ, GPC, other; not offered: PPC | At the last FEDERAL election in January 2006, for which party did you vote? |  | `pond` | Not documented |

### Party identification

#### `pid_prov`: Provincial party identification

The provincial party the respondent usually thinks of themselves as
close to; none is an answer.

Family `party_id` · type Categorical · timing Any time

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

#### `pid_fed`: Federal party identification

The federal party the respondent usually thinks of themselves as close
to; none is an answer.

Family `party_id` · type Categorical · timing Any time

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
| qes2012 | `q94` (post) | `comparable` | Same question as the anchor; the list has no People’s Party (founded in 2018) and no other-party option; web. | pid_list | LPC, CPC, NDP, BQ, GPC, none; not offered: PPC, other | And in federal politics, do you usually think of yourself as a: |  | `pond` | Offered explicitly |

#### `pid_prov_strength`: Strength of provincial party identification

How strongly the respondent identifies with the provincial party named
at the party identification question (very, fairly, not very strongly),
asked of those who named a party. Those who named none, did not know or
refused were not asked: the value is missing, reason inapplicable.

Family `party_id` · type Ordinal · timing Any time

**Levels**

| Code | Name       | Label             |
|------|------------|-------------------|
| 1    | `very`     | Very strongly     |
| 2    | `fairly`   | Fairly strongly   |
| 3    | `not_very` | Not very strongly |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provpidstr` (cps) | `approximate` (awaiting sign-off) | The English version asks how strongly the respondent feels (very, fairly, not very strongly), but the French version, taken by 1,291 of 1,521 respondents (cps_UserLanguage FR-CA) who give 1,164 of the 1,352 answers, asks how close they feel (très proche, proche, pas très proche), the closeness wording graded approximate in 2007 and 2008; asked of those who named a party at cps_provpid (none of these, code 6, is routed out), web, with no don’t-know option, during the campaign. | pid_strength_3 | very, fairly, not_very | How strongly \${cps_provpid/ChoiceGroup/SelectedChoicesTextEntry} do you feel? | cps_provpid: 6 = inapplicable | `cps_weight_general` | Not offered |
| qes2018 | `q57` (post) | `comparable` | Same stem and options as the anchor, asked of those who named a party at q56, whose list offers four parties only. | pid_strength_3 | very, fairly, not_very | How strongly \[insert answer from Q56\] do you feel? | q56: 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `pond` | Offered explicitly |
| qes2014 | `Q56` (post) | `identical` | Same stem, options and filter as the anchor (asked of those who named a party at Q55), web. | pid_strength_3 | very, fairly, not_very | How strongly \[insert answer from Q55\] do you feel? | Q55: 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `POND` | Offered explicitly |
| qes2012 | `q93` (post) | `identical` (anchor) | Anchor row of the target. | pid_strength_3 | very, fairly, not_very | How strongly \[insert answer from Q92\] do you feel? | q92: 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `pond` | Offered explicitly |
| qes2008 | `q71` (post) | `approximate` | Asks how close the respondent feels to the party (très proche, assez proche, pas très proche), not how strongly they identify with it; asked of those who named one of the listed parties at q70 (another party, none, don’t know and refused are routed out); by telephone. | pid_closeness_3 | very, fairly, not_very | How close do you feel to ? Is it… | q70: 96 = inapplicable, 97 = inapplicable, 98 = inapplicable, 99 = inapplicable |  | Not documented |
| qes2007 | `q71` (post) | `approximate` | Asks how close the respondent feels to the party (très proche, assez proche, pas très proche), not how strongly they identify with it; asked of those who named one of the listed parties at q70 (another party, none, don’t know and refused are routed out); by telephone. | pid_closeness_3 | very, fairly, not_very | How close do you feel to ? Is it… | q70: 96 = inapplicable, 97 = inapplicable, 98 = inapplicable, 99 = inapplicable | `pond` | Not documented |

### Attitudes

#### `sov_indep`: Referendum vote: independent country

How the respondent would vote in a referendum on whether Quebec should
become an independent country.

Family `sovereignty` · type Categorical · timing Any time

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

#### `sov_sovereign_country`: Referendum vote: sovereign country

How the respondent would vote in a referendum on whether Quebec should
become a sovereign country. A different stimulus from sov_indep
(sovereign, not independent), and a separate target; the pooled variable
sov_support combines them and records the wording in
sov_support\_\_type. The push question asked of the undecided is not
part of this target.

Family `sovereignty` · type Categorical · timing Any time

**Levels**

| Code | Name             | Label                        |
|------|------------------|------------------------------|
| 1    | `yes`            | Yes                          |
| 2    | `no`             | No                           |
| 95   | `would_not_vote` | Would not vote / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2012_panel | `intvoteref` (pre) | `identical` (anchor) | Anchor row of the target; wording from the variable label and the codebook only (the pre-election questionnaire is not deposited), so whether would-not-vote and don’t know were read is not documented. | sov_sovereign_country | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays souverain, voteriez-vous oui ou voteriez-vous non? |  | `pondam1` (not usable yet) | Not documented |

#### `sov_favour`: Favour Quebec independence (4 points)

How favourable or opposed the respondent is to Quebec independence, on a
four-point scale. Not a referendum vote.

Family `sovereignty` · type Ordinal · timing Any time

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

#### `lr_self`: Left-right self-placement (0-10)

Where the respondent places their own views on a scale from 0 (left) to
10 (right).

Family `left_right` · type Numeric · timing Any time

**Valid range**: 0-10

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_ideoself_1` (cps) | `approximate` | Standalone 0-10 item with a different stem, no don’t-know option and not preceded by the party placements; item nonresponse is far lower than the don’t-know share of the anchor. | lr_0_10 |  | In politics, people sometimes talk of left and right. Where would you place yourself on this scale, where 0 means left and 10 means right? |  | `cps_weight_general` | Not offered |
| qes2018 | `q36_1` (post) | `comparable` | Same stem as the anchor, preceded by the party placements, but the endpoints read ‘left’ and ‘right’ instead of ‘most left’ and ‘most right’. | lr_0_10 |  | And on the same scale, where would you place your own views, generally speaking? |  | `pond` | Offered explicitly |
| qes2018_panel | `rts_q8` (post) | `approximate` | Different stem, mixed telephone and web mode; the deposited wording is cut at 80 characters, so the endpoint labels cannot be compared. | lr_0_10 |  | On utilise souvent un axe gauche-droite pour situer les opinions politiques des gens. Sur une échelle de 0 |  | `weight_rts` | Not documented |
| qes2014 | `Q32` (post) | `identical` | Same stem and endpoint labels as the anchor in English and French, preceded by the same party placements, web. | lr_0_10 |  | And on the same scale, where would you place your own views, generally speaking? |  | `POND` | Offered explicitly |
| qes2012 | `q71` (post) | `identical` (anchor) | Anchor row of the target. | lr_0_10 |  | And on the same scale, where would you place your own views, generally speaking? |  | `pond` | Offered explicitly |

#### `interest_4pt`: Interest in politics (4 points)

How interested the respondent is in politics, on a four-point verbal
scale. A separate target from the 0-10 interest items, never rescaled;
the pooled variable pol_interest scores it on 0-1, graded approximate at
most.

Family `interest` · type Ordinal · timing Any time

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

#### `sov_partnership_1995`: Referendum vote: the 1995 sovereignty-partnership question

How the respondent would vote if a referendum were held today on the
question of the 1995 referendum, sovereignty with an offer of
partnership to the rest of Canada. A different stimulus from sov_indep
and sov_sovereign_country, and a separate target; the pooled variable
sov_support combines them and records the wording in
sov_support\_\_type. The push question asked of the undecided is not
part of this target (sov_partnership_1995_push includes it).

Family `sovereignty` · type Categorical · timing Any time

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
| qes2007_panel | `intref1` (pre) | `comparable` | Same question and options, by telephone; the stem says ‘accompagnée d’une offre de partenariat’ where the anchor says ‘assortie d’une offre’; would not vote, don’t know and refused are not read. | sov_partnership_1995 | yes, no, would_not_vote | document 352416, 13 |  | `pondam1` (not usable yet) | Volunteered only |
| qes1998 | `q16a_crop` (pre) | `comparable` | Same stem and options as the anchor, by telephone; asked by CROP only (426 of the 1,483), not by CREATEC. | sov_partnership_1995 | yes, no, would_not_vote | 16a. Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON | firme_post: 1 = inapplicable | `ponder3` (not usable yet) | Not documented |

#### `interest_0_10`: Interest in politics (0-10)

How interested the respondent is in politics in general, from 0 (no
interest) to 10 (a great deal). A separate target from the four-point
item (interest_4pt), never rescaled; the pooled variable pol_interest
divides it by 10.

Family `interest` · type Numeric · timing Any time

**Valid range**: 0-10

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_interest_1` (cps) | `approximate` | A slider from 0 to 10 on the web, with no don’t-know option; the anchor is a number read out on a scale, with don’t know and refusal volunteered. | interest_0_10_slider |  | How interested are you in politics generally? Set the slider to a number from 0 to 10, where 0 means no interest at all, and 10 means a great deal of interest. |  | `cps_weight_general` | Not offered |
| qes2007 | `q15` (post) | `identical` (anchor) | Anchor row of the target. | interest_0_10 |  | Et toujours avec la même échelle, quel est votre intérêt pour la politique en général ? (Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt) |  | `pond` | Volunteered only |

#### `interest_election_0_10`: Interest in the provincial election (0-10)

How interested the respondent was in the Quebec general election that
has just taken place, from 0 (no interest) to 10 (a great deal), asked
after it. Interest in an election, not in politics in general.

Family `interest` · type Numeric · timing Post-election

**Valid range**: 0-10

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q14` (post) | `comparable` | Same question and scale; the mode needs confirming, the anchor mixes telephone and web. | interest_0_10 |  | Using a scale from zero to ten, where zero (0) means no interest at all and ten (10) means a great deal of interest, how interested were you in this PROVINCIAL election? |  |  | Offered explicitly |
| qes2007 | `q14` (post) | `identical` (anchor) | Anchor row of the target. | interest_0_10 |  | Sur une échelle de 0 à 10 où 0 veut dire aucun intérêt et 10 veut dire beaucoup d’intérêt, quel a été votre intérêt pour l’élection PROVINCIALE qui vient d’avoir lieu ? |  | `pond` | Volunteered only |

#### `interest_campaign_4pt`: Interest in the election campaign (4 points)

How interested the respondent is in the current Quebec election
campaign, on a four-point verbal scale, asked during it. Interest in a
campaign, not in politics in general (interest_4pt).

Family `interest` · type Ordinal · timing Pre-election

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
| qes2007_panel | `interet` (pre) | `identical` (anchor) | Anchor row of the target. | interest_4pt | very, quite, hardly, not_at_all | Personnellement, vous intéressez-vous beaucoup, assez, peu ou pas du tout à la présente campagne électorale au Québec? |  | `pondam1` (not usable yet) | Volunteered only |

#### `sov_partnership_1995_push`: Referendum vote: the 1995 question, undecided pushed

How the respondent would vote on the question of the 1995 referendum
(sovereignty with an offer of partnership to the rest of Canada), with
the respondents who did not know at the first question asked which way
they would be inclined to vote (a push question), combined into one
variable: the first answer where there is one, else the pushed answer. A
separate target from sov_partnership_1995, which has the first question
only; the pooled variable sov_support uses this one first.

Family `sovereignty` · type Categorical · timing Any time

**Levels**

| Code | Name             | Label                        |
|------|------------------|------------------------------|
| 1    | `yes`            | Yes                          |
| 2    | `no`             | No                           |
| 95   | `would_not_vote` | Would not vote / would spoil |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q19` + `q20` (post) | `comparable` | Same first question and push as the anchor (q19, then q20 for those who did not know); the mode differs (telephone by the deposit metadata; the anchor study mixes telephone and web). | sov_partnership_1995_push | yes, no, would_not_vote | If a referendum were held today on the same question as that asked in 1995, that is sovereignty with an offer of partnership with the rest of Canada, would you vote YES or would you vote NO? \[If don’t know:\] Even if you haven’t yet made up your mind, if a referendum were held today on this issue, would you be inclined to vote YES or to vote NO? |  |  | Not documented |
| qes2007 | `q19` + `q20` (post) | `identical` (anchor) | Anchor row of the target: the 1995 question read in full (q19), and, for the respondents who did not know, the push q20. | sov_partnership_1995_push | yes, no, would_not_vote | If a referendum were held today on the same question as that asked in 1995, that is sovereignty with an offer of partnership with the rest of Canada, would you vote YES or would you vote NO? \[If don’t know:\] Even if you haven’t yet made up your mind, if a referendum were held today on this issue, would you be inclined to vote YES or to vote NO? |  | `pond` | Not documented |
| qes2007_panel | `intref1` + `intref2` (pre) | `comparable` | Same question and push as the anchor, by telephone; the stem says ‘accompagnée d’une offre de partenariat’ where the anchor says ‘assortie d’une offre’; would not vote, don’t know and refused are not read. The push intref2 was asked of those who would not vote, did not know or refused (only those who did not know are pushed here, as in the anchor). | sov_partnership_1995_push | yes, no, would_not_vote | Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté accompagnée d’une offre de partenariat au reste du Canada, voteriez-vous Oui ou voteriez-vous Non? \[Si ne sait pas :\] Même si vous n’avez peut-être pas encore fait votre choix, s’il y avait un référendum aujourd’hui sur cette question, seriez-vous tenté(e) de voter Oui ou de voter Non? |  | `pondam1` (not usable yet) | Volunteered only |
| qes1998 | `q16a_crop` + `q16b_crop` (pre) | `comparable` | Same question and push as the anchor, by telephone; asked by CROP only (426 of the 1,483), not by CREATEC. The push q16b_crop was asked of those who would not vote, did not know or refused (only those who did not know are pushed here, as in the anchor). | sov_partnership_1995_push | yes, no, would_not_vote | 16a. Si un référendum avait lieu aujourd’hui sur la même question que celle qui a été posée lors du dernier référendum de 1995, c’est-à-dire sur la souveraineté assortie d’une offre de partenariat au reste du Canada, voteriez-vous OUI ou voteriez-vous NON \[Si ne sait pas :\] 16b. Même si vous n’avez peut-être pas encore fait votre choix, s’il y avait un référendum aujourd’hui sur cette question, seriez-vous tenté(e) de voter pour le OUI ou pour le NON? | firme_post: 1 = inapplicable | `ponder3` (not usable yet) | Not documented |

#### `satis_demo_qc`: Satisfaction with democracy in Quebec

How satisfied the respondent is, on the whole, with the way democracy
works in Quebec, on a four-point verbal scale (very, fairly, not very,
not at all satisfied).

Family `democracy_satisfaction` · type Ordinal · timing Any time

**Levels**

| Code | Name         | Label                |
|------|--------------|----------------------|
| 1    | `very`       | Very satisfied       |
| 2    | `fairly`     | Fairly satisfied     |
| 3    | `not_very`   | Not very satisfied   |
| 4    | `not_at_all` | Not at all satisfied |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_satis_prov` (cps) | `comparable` | Same construct and options; the stem asks ‘how satisfied are you’ (FR ‘quel est votre niveau de satisfaction’); web, with no don’t-know option (a skipped item is no_answer); asked during the campaign. | satis_4pt | very, fairly, not_very, not_at_all | On the whole, how satisfied are you with the way democracy works in Quebec? |  | `cps_weight_general` | Not offered |
| qes2018 | `q1` (post) | `comparable` | The stem asks ‘how satisfied are you’ (anchor: ‘are you satisfied’); the second option reads ‘Somewhat satisfied’ in English (anchor: ‘Fairly satisfied’) and the third ‘Peu satisfait(e)’ in French (anchor: ‘Pas très satisfait(e)’); web, don’t know and refusal shown. | satis_4pt | very, fairly, not_very, not_at_all | On the whole, how satisfied are you with the way democracy works in Quebec? Are you: |  | `pond` | Offered explicitly |
| qes2014 | `Q35` (post) | `identical` | Same stem and options as the anchor in English and French, web, with don’t know and refusal shown. | satis_4pt | very, fairly, not_very, not_at_all | On the whole, are you satisfied with the way democracy works in Quebec? Are you: |  | `POND` | Offered explicitly |
| qes2012 | `q74` (post) | `identical` (anchor) | Anchor row of the target. | satis_4pt | very, fairly, not_very, not_at_all | On the whole, are you satisfied with the way democracy works in Quebec? Are you: |  | `pond` | Offered explicitly |
| qes2008 | `q27` (post) | `comparable` | Same stem and options as the anchor in English and French; telephone by the deposit metadata (the deposited scripts read like a web one); don’t know and refusal are codes 8 and 9. | satis_4pt | very, fairly, not_very, not_at_all | On the whole, are you SATISFIED with the way democracy works in Quebec? Are you … |  |  | Not documented |
| qes2007 | `q27` (post) | `comparable` | Same stem and options as the anchor; the study mixes telephone and web interviews (only the telephone script is deposited), don’t know not read. | satis_4pt | very, fairly, not_very, not_at_all | On the whole, are you SATISFIED with the way democracy works in Quebec? Are you … |  | `pond` | Not documented |

#### `gov_satisfaction`: Satisfaction with the Quebec government

How satisfied the respondent is with the performance of the Quebec
government in office, on a four-point scale. The government changes by
design: each row’s wording names it (the Liberal government in 2012, the
PQ government in 2014, Philippe Couillard’s in 2018, the government
under François Legault in 2022).

Family `government_satisfaction` · type Ordinal · timing Any time

**Levels**

| Code | Name         | Label                |
|------|--------------|----------------------|
| 1    | `very`       | Very satisfied       |
| 2    | `fairly`     | Fairly satisfied     |
| 3    | `not_very`   | Not very satisfied   |
| 4    | `not_at_all` | Not at all satisfied |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_province_gov_sat` (cps) | `comparable` | The outgoing CAQ government, named by its leader (under François Legault); web, with no don’t-know option; asked during the campaign. | gov_satis_4pt | very, fairly, not_very, not_at_all | How satisfied are you with the performance of the Quebec government under François Legault? |  | `cps_weight_general` | Not offered |
| qes2018 | `q10` (post) | `comparable` | The outgoing Liberal government, named by its leader (Philippe Couillard’s); the stem asks for the overall level of satisfaction and the third option reads ‘Peu satisfait(e)’. | gov_satis_4pt | very, fairly, not_very, not_at_all | Overall, what is your level of satisfaction with the performance of Philippe Couillard’s liberal government? |  | `pond` | Offered explicitly |
| qes2014 | `Q10` (post) | `comparable` | Same stem and options as the anchor about the government in office, the outgoing PQ government of Pauline Marois (named by its party). | gov_satis_4pt | very, fairly, not_very, not_at_all | How satisfied are you with the performance of the PQ government in general? |  | `POND` | Offered explicitly |
| qes2012 | `q35` (post) | `identical` (anchor) | Anchor row of the target: the outgoing Liberal government of Jean Charest. | gov_satis_4pt | very, fairly, not_very, not_at_all | How satisfied are you with the performance of the provincial Liberal government in general? |  | `pond` | Offered explicitly |
| qes2007_panel | `satisf` (pre) | `approximate` | A bipolar scale (very or rather satisfied, rather or very dissatisfied) read by telephone during the campaign, about the present Quebec government (Jean Charest’s Liberal government); rather dissatisfied and very dissatisfied are taken as not very and not at all satisfied. | gov_satis_bipolar | very, fairly, not_very, not_at_all | Diriez-vous que vous êtes très satisfait(e), plutôt satisfait(e), plutôt insatisfait(e) ou très insatisfait(e) du présent gouvernement du Québec? |  | `pondam1` (not usable yet) | Volunteered only |

#### `econ_retro_qc`: Quebec’s economy over the past year

Whether the respondent thinks Quebec’s economy has gotten better, stayed
about the same or gotten worse over the past year (a retrospective,
sociotropic evaluation).

Family `economy_retrospective` · type Ordinal · timing Any time

**Levels**

| Code | Name     | Label          |
|------|----------|----------------|
| 1    | `better` | Better         |
| 2    | `same`   | About the same |
| 3    | `worse`  | Worse          |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_provecon` (cps) | `comparable` | Same question and options, web, with no don’t-know option; asked during the campaign, which the 2022 inflation shaped. | econ_retro_3 | better, same, worse | Over the past year, has Quebec’s economy… |  | `cps_weight_general` | Not offered |
| qes2018 | `q53` (post) | `identical` | Same stem and options as the anchor in English and French, web, with don’t know and refusal shown (codes 98 and 99). | econ_retro_3 | better, same, worse | Over the past year, has Quebec’s economy: gotten better, gotten worse, or stayed about the same? |  | `pond` | Offered explicitly |
| qes2014 | `Q52` (post) | `identical` | Same stem and options as the anchor in English and French, web, with don’t know and refusal shown. | econ_retro_3 | better, same, worse | Over the past year, has Quebec’s economy: gotten better, gotten worse, or stayed about the same? |  | `POND` | Offered explicitly |
| qes2012 | `q91` (post) | `identical` (anchor) | Anchor row of the target. | econ_retro_3 | better, same, worse | Over the past year, has Quebec’s economy: gotten better, gotten worse, or stayed about the same? |  | `pond` | Offered explicitly |
| qes2008 | `q47` (post) | `comparable` | Same question and options, by telephone, fielded in December 2008 at the start of the financial crisis. | econ_retro_3 | better, same, worse | Over the PAST YEAR, has QUÉBEC’s economy: gotten better, gotten worse, or stayed about the same? |  |  | Not documented |
| qes2007 | `q47` (post) | `comparable` | Same question and options; the study mixes telephone and web interviews (only the telephone script is deposited). | econ_retro_3 | better, same, worse | Over the PAST YEAR, has QUEBEC’s economy: gotten better, gotten worse, or stayed about the same? |  | `pond` | Not documented |

#### `attach_qc`: Attachment to Quebec

How attached the respondent feels to Quebec, on a four-point scale
(very, fairly, not very, not at all).

Family `attachment` · type Ordinal · timing Any time

**Levels**

| Code | Name         | Label               |
|------|--------------|---------------------|
| 1    | `very`       | Very attached       |
| 2    | `fairly`     | Fairly attached     |
| 3    | `not_very`   | Not very attached   |
| 4    | `not_at_all` | Not at all attached |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_qc_attach` (cps) | `comparable` | Same stem as the anchor; the French options read ‘Assez attaché(e)’ and ‘Peu attaché(e)’; web; don’t know is an option but there is no refusal option; asked during the campaign (pre-election), whereas the anchor is post-election. | attach_4pt | very, fairly, not_very, not_at_all | How attached do you feel to Quebec? |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q18` (post) | `comparable` | Same stem as the anchor; the English second option reads ‘Somewhat attached’ (anchor ‘Fairly attached’) and the French options read ‘Assez attaché(e)’ and ‘Peu attaché(e)’ (anchor ‘Plutôt attaché(e)’, ‘Pas très attaché(e)’). | attach_4pt | very, fairly, not_very, not_at_all | How attached do you feel to Quebec? |  | `pond` | Offered explicitly |
| qes2014 | `Q12` (post) | `comparable` | Same stem as the anchor; the French options read ‘Plutôt attaché(e)’ and ‘Pas très attaché(e)’. | attach_4pt | very, fairly, not_very, not_at_all | How attached do you feel to Quebec? |  | `POND` | Offered explicitly |
| qes2012 | `q1` (post) | `identical` (anchor) | Anchor row of the target. | attach_4pt | very, fairly, not_very, not_at_all | How attached do you feel to Quebec? |  | `pond` | Offered explicitly |

#### `attach_ca`: Attachment to Canada

How attached the respondent feels to Canada, on a four-point scale
(very, fairly, not very, not at all).

Family `attachment` · type Ordinal · timing Any time

**Levels**

| Code | Name         | Label               |
|------|--------------|---------------------|
| 1    | `very`       | Very attached       |
| 2    | `fairly`     | Fairly attached     |
| 3    | `not_very`   | Not very attached   |
| 4    | `not_at_all` | Not at all attached |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_can_attach` (cps) | `comparable` | Same stem and options as the anchor in English (without the leading ‘And’); the French options read ‘Assez attaché(e)’ and ‘Peu attaché(e)’ (anchor: ‘Plutôt attaché(e)’, ‘Pas très attaché(e)’); web; don’t know is an option (code 5); asked during the campaign, the anchor after the election. | attach_4pt | very, fairly, not_very, not_at_all | How attached do you feel to Canada? |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q19` (post) | `comparable` | Same stem as the anchor; option 2 reads ‘Somewhat attached’ in English (anchor ‘Fairly attached’), and the French options read ‘Assez attaché(e)’ and ‘Peu attaché(e)’ (anchor ‘Plutôt attaché(e)’, ‘Pas très attaché(e)’). The file has no value labels; codes come from the questionnaire with programmed answer values (367181). | attach_4pt | very, fairly, not_very, not_at_all | And how attached do you feel to Canada? |  | `pond` | Offered explicitly |
| qes2014 | `Q13` (post) | `comparable` | Same stem as the anchor; the French options read ‘Plutôt attaché(e)’ and ‘Pas très attaché(e)’. | attach_4pt | very, fairly, not_very, not_at_all | And how attached do you feel to Canada? |  | `POND` | Offered explicitly |
| qes2012 | `q2` (post) | `identical` (anchor) | Anchor row of the target. | attach_4pt | very, fairly, not_very, not_at_all | And how attached do you feel to Canada? |  | `pond` | Offered explicitly |

#### `identity_qc_ca`: Québécois or Canadian identity

How the respondent defines themselves, from Québécois only to Canadian
only (the five-point Moreno question); another self-definition is an
answer (level other).

Family `national_identity` · type Categorical · timing Any time

**Levels**

| Code | Name       | Label                          |
|------|------------|--------------------------------|
| 1    | `qc_only`  | Québécois only                 |
| 2    | `qc_first` | Québécois first, then Canadian |
| 3    | `equal`    | Equally Québécois and Canadian |
| 4    | `ca_first` | Canadian first, then Québécois |
| 5    | `ca_only`  | Canadian only                  |
| 90   | `other`    | Other                          |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_identity_qc_ca` (cps) | `comparable` | The French stem of the anchor, web, with ‘Cannot say’ (Je ne sais pas) as an option always shown last; the five options were shown from Canadian only to Québécois only for 741 respondents and in the reverse order for the other 780 (display-order variables pes_identity_qc_ca_DO_1 to *DO_6), one variable, so the order effect averages as in 2014, 2007 and 2008. Despite its pes* prefix, the item is in the campaign-period survey: every respondent answered it, those of the post-election wave and the 301 who did not take it alike (codebook p. 52, in the campaign section). | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only; not offered: other | People have different ways of defining themselves. Do you consider yourself to be \_\_\_\_\_\_\_\_\_\_ |  | `cps_weight_general` | Offered explicitly |
| qes2014 | `Q14A` + `Q14B` (post) | `comparable` | The anchor’s question in English and French, asked on a split ballot: half the sample (Q14A, SEL1 = 1) saw the options from Québécois only to Canadian only, the other half (Q14B) in reverse order; the two halves are combined, which averages the order effect. | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only; not offered: other | Which, if any, of the following best describes the way you think of yourself? (options in one order for half the sample, Q14A, in the reverse order for the other half, Q14B) |  | `POND` | Offered explicitly |
| qes2012 | `q3` (post) | `identical` (anchor) | Anchor row of the target. | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only; not offered: other | Which if any of the following best describes the way you think of yourself? |  | `pond` | Offered explicitly |
| qes2008 | `q18a` + `q18b` (post) | `comparable` | The French stem of the anchor, with the five options read in one order for half the sample (q18a) and in the reverse order for the other half (q18b), combined; another self-definition (96, ‘Other, specify’) is printed as an option in both questionnaires, with no reading instruction; by telephone (per the deposit metadata). | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only, other | People have different ways of defining themselves. Do you consider yourself to be… |  |  | Not documented |
| qes2007 | `q18a` + `q18b` (post) | `comparable` | The French stem of the anchor, with the five options read in one order for half the sample (q18a) and in the reverse order for the other half (q18b), combined; another self-definition (96) is volunteered; telephone and web. | identity_moreno_5 | qc_only, qc_first, equal, ca_first, ca_only, other | People have different ways of defining themselves. Do you consider yourself to be… |  | `pond` | Not documented |

#### `therm_leader_plq`: Rating of the PLQ leader (0-100)

How much the respondent likes the leader of the PLQ, from 0 (really
dislike) to 100 (really like); the leader changes by design, and each
row names the person rated. Not knowing the leader is don’t know (dk).

Family `leader_ratings` · type Numeric · timing Any time

**Valid range**: 0-100

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_1` (cps) | `approximate` | A 0-100 slider for each provincial leader, web, asked during the campaign; -99 is the explicit ‘Don’t know leader’ option (codebook 7449514, p. 26) and, per the codebook note under cps_intelligent, may also include a slider left blank (the file has no system missing), so it is read as don’t know. | therm_0_100 |  | Using the same scale, how do you feel about the provincial party leaders below? \[Dominique Anglade\] |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q33_a` (post) | `approximate` | A 0-10 scale multiplied by 10 (affine 10\*x): the same construct on a coarser scale; not knowing the person (97) is don’t know; web. | therm_0_10 |  | On a scale of 0 to 10, where 0 means you REALLY DISLIKE him or her and 10 means you REALLY LIKE him or her, how do you feel about… Philippe Couillard? |  | `pond` | Offered explicitly |
| qes2014 | `Q29A` (post) | `comparable` | Same stem and 0-100 scale as the anchor, web; not knowing the person (997) is don’t know. | therm_0_100 |  | On a scale where zero means you REALLY DISLIKE him and one hundred means you REALLY LIKE him, how do you feel about … |  | `POND` | Offered explicitly |
| qes2012 | `q68` (post) | `identical` (anchor) | Anchor row of the target. | therm_0_100 |  | On a scale where zero means you REALLY DISLIKE him and one hundred means you REALLY LIKE him, how do you feel about JEAN CHAREST? |  | `pond` | Offered explicitly |
| qes2008 | `q39` (post) | `comparable` | Same 0-100 scale; telephone by the deposit metadata (to be confirmed); not knowing any leader (995) or this one (997) is don’t know. The French wording matches the anchor; the deposited English questionnaire asks only ‘How do you feel about JEAN CHAREST?’ and does not define 0 and 100 (the anchor’s English does). | therm_0_100 |  | How do you feel about JEAN CHAREST? |  |  | Not documented |
| qes2007 | `q39` (post) | `comparable` | Same 0-100 scale; the study mixes telephone and web interviews; not knowing the leader or any leader (995, 997) is don’t know. | therm_0_100 |  | Now the party leaders. On the same scale, where zero means you REALLY DISLIKE the leader and one hundred means you REALLY LIKE the leader. How do you feel about JEAN CHAREST? |  | `pond` | Not documented |

#### `therm_leader_pq`: Rating of the PQ leader (0-100)

How much the respondent likes the leader of the PQ, from 0 (really
dislike) to 100 (really like); the leader changes by design, and each
row names the person rated. Not knowing the leader is don’t know (dk).

Family `leader_ratings` · type Numeric · timing Any time

**Valid range**: 0-100

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_2` (cps) | `approximate` | A 0-100 slider for each provincial leader, web, asked during the campaign; -99 is the explicit ‘Don’t know leader’ option (codebook 7449514, p. 26) and, per the codebook note under cps_intelligent, may also include a slider left blank (the file has no system missing), so it is read as don’t know. | therm_0_100 |  | Using the same scale, how do you feel about the provincial party leaders below? \[Paul St-Pierre Plamondon\] |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q33_b` (post) | `approximate` | A 0-10 scale multiplied by 10 (affine 10\*x): the same construct on a coarser scale; not knowing the person (97) is don’t know; web. | therm_0_10 |  | On a scale of 0 to 10, where 0 means you REALLY DISLIKE him or her and 10 means you REALLY LIKE him or her, how do you feel about… Jean-François Lisée? |  | `pond` | Offered explicitly |
| qes2014 | `Q29B` (post) | `comparable` | Same stem and 0-100 scale as the anchor, web; not knowing the person (997) is don’t know. | therm_0_100 |  | On a scale where zero means you REALLY DISLIKE him and one hundred means you REALLY LIKE him, how do you feel about … PAULINE MAROIS? |  | `POND` | Offered explicitly |
| qes2012 | `q68b` (post) | `identical` (anchor) | Anchor row of the target. | therm_0_100 |  | On the same scale, how do you feel about PAULINE MAROIS? |  | `pond` | Offered explicitly |
| qes2008 | `q40` (post) | `comparable` | Same 0-100 scale; telephone by the deposit metadata (to be confirmed); not knowing any leader (995) or this one (997) is don’t know. The French wording matches the anchor; the deposited English questionnaire asks only ‘How do you feel about PAULINE MAROIS?’ and does not define 0 and 100 (the anchor’s English does). | therm_0_100 |  | How do you feel about PAULINE MAROIS? |  |  | Not documented |
| qes2007 | `q40` (post) | `comparable` | Same 0-100 scale; the study mixes telephone and web interviews; not knowing the leader or any leader (995, 997) is don’t know. | therm_0_100 |  | Now the party leaders. On the same scale, where zero means you REALLY DISLIKE the leader and one hundred means you REALLY LIKE the leader. How do you feel about ANDRÉ BOISCLAIR? |  | `pond` | Not documented |

#### `therm_leader_caq`: Rating of the CAQ leader (0-100)

How much the respondent likes the leader of the CAQ, from 0 (really
dislike) to 100 (really like); the leader changes by design, and each
row names the person rated. Not knowing the leader is don’t know (dk).

Family `leader_ratings` · type Numeric · timing Any time

**Valid range**: 0-100

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_3` (cps) | `approximate` | A 0-100 slider for each provincial leader, web, asked during the campaign; -99 is the explicit ‘Don’t know leader’ option (codebook 7449514, p. 26) and, per the codebook note under cps_intelligent, may also include a slider left blank (the file has no system missing), so it is read as don’t know. | therm_0_100 |  | Using the same scale, how do you feel about the provincial party leaders below? \[François Legault\] |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q33_c` (post) | `approximate` | A 0-10 scale multiplied by 10 (affine 10\*x): the same construct on a coarser scale; not knowing the person (97) is don’t know; web. | therm_0_10 |  | On a scale of 0 to 10, where 0 means you REALLY DISLIKE him or her and 10 means you REALLY LIKE him or her, how do you feel about… François Legault? |  | `pond` | Offered explicitly |
| qes2014 | `Q29C` (post) | `comparable` | Same stem and 0-100 scale as the anchor, web; not knowing the person (997) is don’t know. | therm_0_100 |  | On a scale where zero means you REALLY DISLIKE him and one hundred means you REALLY LIKE him, how do you feel about … FRANÇOIS LEGAULT? |  | `POND` | Offered explicitly |
| qes2012 | `q68c` (post) | `identical` (anchor) | Anchor row of the target. | therm_0_100 |  | On the same scale, how do you feel about FRANÇOIS LEGAULT? |  | `pond` | Offered explicitly |

#### `therm_leader_qs`: Rating of the QS leader (0-100)

How much the respondent likes the leader of the QS, from 0 (really
dislike) to 100 (really like); the leader changes by design, and each
row names the person rated (QS has two spokespersons: the one the study
rated, or its candidate for premier when the study rated both). Not
knowing the leader is don’t know (dk).

Family `leader_ratings` · type Numeric · timing Any time

**Valid range**: 0-100

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_leadertherm_7` (cps) | `approximate` | A 0-100 slider for each provincial leader, web, asked during the campaign; -99 is the explicit ‘Don’t know leader’ option (codebook 7449514, p. 26) and, per the codebook note under cps_intelligent, may also include a slider left blank (the file has no system missing), so it is read as don’t know. | therm_0_100 |  | Using the same scale, how do you feel about the provincial party leaders below? \[Gabriel Nadeau-Dubois\] |  | `cps_weight_general` | Offered explicitly |
| qes2018 | `q33_d` (post) | `approximate` | A 0-10 scale multiplied by 10 (affine 10\*x): the same construct on a coarser scale; not knowing the person (97) is don’t know; web. | therm_0_10 |  | On a scale of 0 to 10, where 0 means you REALLY DISLIKE him or her and 10 means you REALLY LIKE him or her, how do you feel about… Manon Massé? |  | `pond` | Offered explicitly |
| qes2014 | `Q29D` (post) | `comparable` | Same stem and 0-100 scale as the anchor, web; not knowing the person (997) is don’t know. | therm_0_100 |  | On a scale where zero means you REALLY DISLIKE him and one hundred means you REALLY LIKE him, how do you feel about … FRANÇOISE DAVID? |  | `POND` | Offered explicitly |
| qes2012 | `q68d` (post) | `identical` (anchor) | Anchor row of the target. | therm_0_100 |  | On the same scale, how do you feel about AMIR KHADIR? |  | `pond` | Offered explicitly |
| qes2008 | `q42` (post) | `comparable` | Same 0-100 scale; telephone by the deposit metadata (to be confirmed); not knowing any leader (995) or this one (997) is don’t know. The French wording matches the anchor; the deposited English questionnaire asks only ‘How do you feel about FRANÇOISE DAVID?’ and does not define 0 and 100 (the anchor’s English does). | therm_0_100 |  | How do you feel about FRANÇOISE DAVID? |  |  | Not documented |
| qes2007 | `q42` (post) | `comparable` | Same 0-100 scale; the study mixes telephone and web interviews; not knowing the leader or any leader (995, 997) is don’t know. | therm_0_100 |  | Now the party leaders. On the same scale, where zero means you REALLY DISLIKE the leader and one hundred means you REALLY LIKE the leader. How do you feel about FRANÇOISE DAVID? |  | `pond` | Not documented |

#### `therm_leader_adq`: Rating of the ADQ leader (0-100)

How much the respondent likes the leader of the ADQ, from 0 (really
dislike) to 100 (really like); the leader changes by design, and each
row names the person rated. Not knowing the leader is don’t know (dk).

Family `leader_ratings` · type Numeric · timing Any time

**Valid range**: 0-100

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2008 | `q41` (post) | `comparable` | Same 0-100 scale; telephone by the deposit metadata (to be confirmed); not knowing any leader (995) or this one (997) is don’t know. The French wording matches the anchor; the deposited English questionnaire asks only ‘How do you feel about MARIO DUMONT?’ and does not define 0 and 100 (the anchor’s English does). | therm_0_100 |  | How do you feel about MARIO DUMONT? |  |  | Not documented |
| qes2007 | `q41` (post) | `identical` (anchor) | Anchor row of the target. | therm_0_100 |  | Now the party leaders. On the same scale, where zero means you REALLY DISLIKE the leader and one hundred means you REALLY LIKE the leader. How do you feel about MARIO DUMONT? |  | `pond` | Not documented |

### Issues

#### `mip_issue`: Most important issue of the election

Which issue, from the study’s closed list, was the most important to the
respondent personally in the Quebec general election of the study. The
lists change from election to election: an issue a study did not list is
a structural zero there (levels_not_offered), not an absence of concern.

Family `issues` · type Categorical · timing Any time

**Levels**

| Code | Name              | Label                          |
|------|-------------------|--------------------------------|
| 1    | `economy`         | The economy                    |
| 2    | `health`          | Health care                    |
| 3    | `environment`     | The environment                |
| 4    | `education`       | Education                      |
| 5    | `families`        | Aid to families                |
| 6    | `poverty`         | Poverty                        |
| 7    | `integrity`       | Integrity and corruption       |
| 8    | `taxes_finances`  | Taxes and public finances      |
| 9    | `sovereignty`     | Quebec sovereignty             |
| 10   | `secularism`      | State secularism (the charter) |
| 11   | `immigration`     | Immigration                    |
| 12   | `cost_of_living`  | Cost of living                 |
| 13   | `housing`         | Housing                        |
| 14   | `french_language` | The French language            |
| 90   | `other`           | Another issue                  |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_impissue_matrix` (cps) | `comparable` | The same question (‘for you personally, the most important’) about the study’s election, with the study’s own closed list: issues it did not list are structural zeros. Fourteen issues, asked during the campaign, web, no don’t-know option; Quebec City’s third road link and gun violence are other issues. | mip_closed_list | economy, health, environment, education, poverty, integrity, taxes_finances, sovereignty, immigration, cost_of_living, housing, french_language, other; not offered: families, secularism | What is the most important issue to you personally in this provincial election? |  | `cps_weight_general` | Not offered |
| qes2018 | `q2` (post) | `comparable` | The same question (‘for you personally, the most important’) about the study’s election, with the study’s own closed list: issues it did not list are structural zeros. Ten issues and another issue; integrity reads ‘of politicians and corruption’. | mip_closed_list | economy, health, environment, education, families, poverty, integrity, taxes_finances, sovereignty, immigration, other; not offered: secularism, cost_of_living, housing, french_language | Of the following issues, which was, for you personally, the most important in the provincial election held on October 1st? |  | `pond` | Offered explicitly |
| qes2014 | `Q1` (post) | `comparable` | The same question (‘for you personally, the most important’) about the study’s election, with the study’s own closed list: issues it did not list are structural zeros. Ten issues, among them the charter of secularism (the PQ’s Charte des valeurs). | mip_closed_list | economy, health, environment, education, families, poverty, integrity, taxes_finances, sovereignty, secularism; not offered: immigration, cost_of_living, housing, french_language, other | Of the following issues, which was, for you personally, the most important in the provincial election held on April 7? |  | `POND` | Offered explicitly |
| qes2012 | `q34bb` (post) | `identical` (anchor) | Anchor row of the target: eight issues, no other-issue option. | mip_closed_list | economy, health, environment, education, families, poverty, integrity, sovereignty; not offered: taxes_finances, secularism, immigration, cost_of_living, housing, french_language, other | Of the following issues, which was, for you personally, the most important in the provincial election of September 4? |  | `pond` | Offered explicitly |
| qes2008 | `q1` (post) | `comparable` | The same question (‘for you personally, the most important’) about the study’s election, with the study’s own closed list: issues it did not list are structural zeros. Six issues and another issue, by telephone. | mip_closed_list | economy, health, environment, education, families, poverty, other; not offered: integrity, taxes_finances, sovereignty, secularism, immigration, cost_of_living, housing, french_language | What was the most important issue to you personally in the provincial election on December 8? |  |  | Not offered |

### Sociodemographics

#### `birth_year`: Year of birth

Year the respondent was born.

Family `birth` · type Numeric · timing Time-invariant

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

#### `birth_month`: Month of birth

Month the respondent was born (1 = January). Asked with the year of
birth in some studies; with birth_year, it tells whether a respondent
born 18 years before the election year was 18 on election day.

Family `birth` · type Numeric · timing Time-invariant

**Valid range**: 1-12

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `agemonth_1` (post) | `identical` (anchor) | Anchor row of the target. | yob_month_entry |  | In what year were you born? | agensp: 1 = refused | `pond` | Offered explicitly |

#### `age`: Age in years

The respondent’s age in years at the interview, as asked. Never computed
from the year of birth, which gives the age only to within a year.

Family `age_years` · type Numeric · timing Any time

**Valid range**: 15-115

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_age_in_years` (cps) | `identical` (anchor) | Anchor row of the target. | age_list |  | To make sure we are talking to a cross section of Canadians, we need to get a little information about your background. First, how old are you? |  | `cps_weight_general` | Not offered |
| qes2018 | `agenum` (post) | `approximate` | The same question (How old are you?, a list of ages) asked only of the respondents who did not give their year and month of birth; the anchor asks every respondent. | age_list |  | How old are you? | agensp: 0 = inapplicable | `pond` | Offered explicitly |

#### `age_group3`: Age group (3 bands)

The respondent’s age group at the interview in three bands: 18-34,
35-54, 55 and over. Built from a question with these bands or with bands
that collapse into them exactly; where the study has no such question,
derived from the age (exact) or the year of birth (graded approximate:
an age at a band edge can be one year off).

Family `age_bands` · type Ordinal · timing Any time

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
| qes2012_panel | `age` (pre) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | document 654292, age |  | `pondam1` (not usable yet) | Not documented |
| qes_crop_2007_2010 | `QAGE` (each poll) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (not usable yet) | Not documented |
| qes2008 | `q0age` (post) | `comparable` | Seven age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64, 65-74 and 75 and over); the anchor offers the three bands. | age_7bands | a18_34, a35_54, a55_plus | How old are you? |  |  | Not documented |
| qes2007_panel | `age` (any wave) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Not documented |
| qes1998 | `age` (pre) | `comparable` | Six age bands, collapsed exactly into the three of the target (18-24 and 25-34, 35-44 and 45-54, 55-64 and 65 and over), by telephone; the anchor offers the three bands, by telephone and web. | age_6bands | a18_34, a35_54, a55_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (not usable yet) | Not documented |

#### `citizen`: Canadian citizen

Whether the respondent is a Canadian citizen, where the study asked.
Only citizens may vote in Quebec elections.

Family `citizenship` · type Categorical · timing Any time

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_citizen` (cps) | `identical` (anchor) | Anchor row of the target. | citizen_status | yes, no | Are you a… |  | `cps_weight_general` | Not offered |

#### `age_group6`: Age group (6 bands)

The respondent’s age group at the interview in six bands: 18-24, 25-34,
35-44, 45-54, 55-64, 65 and over. Built from a question with these bands
or with bands that collapse into them exactly; where the study has no
such question, derived from the age (exact) or the year of birth (graded
approximate: an age at a band edge can be one year off).

Family `age_bands` · type Ordinal · timing Any time

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
| qes2012_panel | `age` (pre) | `comparable` | The six bands of the anchor, by telephone; the codebooks do not say whether don’t know was offered. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | document 654292, age |  | `pondam1` (not usable yet) | Not documented |
| qes_crop_2007_2010 | `QAGE` (each poll) | `identical` (anchor) | Anchor row of the target. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’àges suivants appartenez-vous? |  | `XPOND` (not usable yet) | Not documented |
| qes2008 | `q0age` (post) | `comparable` | Seven age bands, collapsed exactly into the six of the target (65-74 and 75 and over into 65 and over); the anchor offers the six bands, by telephone. | age_7bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | How old are you? |  |  | Not documented |
| qes2007_panel | `age` (any wave) | `comparable` | The six bands of the anchor, by telephone; the codebooks do not say whether don’t know was offered. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | Auquel des groupes d’âges suivants appartenez-vous? |  |  | Not documented |
| qes1998 | `age` (pre) | `comparable` | The six bands of the anchor, by telephone, asked by both firms; the anchor’s codebook does not say whether don’t know was offered, nor does this one. | age_6bands | a18_24, a25_34, a35_44, a45_54, a55_64, a65_plus | A quel groupe d’age appartenez-vous? (LIRE SI NECESSAIRE) |  | `ponder3` (not usable yet) | Not documented |

#### `gender`: Gender

The respondent’s gender, as asked or, in some telephone surveys, as
recorded by the interviewer. Most studies offered only man and woman (a
question on sex, in some); qes2022 also offered non-binary and another
gender.

Family `sex_gender` · type Categorical · timing Time-invariant

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
| qes2012_panel | `sexe` (pre) | `comparable` | Same two options, by telephone; the codebook gives only the variable name, so whether it was asked or recorded is not documented. | sex_recorded | man, woman; not offered: nonbinary, other | document 654292, sexe |  | `pondam1` (not usable yet) | Not documented |
| qes_crop_2007_2010 | `SEXE` (each poll) | `comparable` | Same two options, recorded by the interviewer (not asked), by telephone. | sex_recorded | man, woman; not offered: nonbinary, other | INSCRIRE LE SEXE DU REPONDANT |  | `XPOND` (not usable yet) | Not offered |
| qes2008 | `q76` (post) | `comparable` | Asked (Un homme / Une femme) with the same two options; the mode of the study needs confirming (telephone in the deposit’s metadata), the anchor is web. | gender_2 | man, woman; not offered: nonbinary, other | Are you…? |  |  | Not offered |
| qes2007 | `q76` (post) | `comparable` | Recorded by the telephone interviewer without asking (NE PAS LIRE), and on the web for the web respondents (only the telephone script is deposited); same two options. | sex_recorded | man, woman; not offered: nonbinary, other | (DO NOT READ) Enter respondent’s gender: |  | `pond` | Not offered |
| qes2007_panel | `sexe` (any wave) | `comparable` | Same two options, recorded by the interviewer (not asked), by telephone. | sex_recorded | man, woman; not offered: nonbinary, other | INSCRIRE LE SEXE DU RÉPONDANT |  |  | Not offered |
| qes1998 | `sexe_post` (pre) | `comparable` | Same two options, by telephone; the codebooks give only the label SEXE (Sexe du répondant in CROP’s), so whether it was asked or recorded by the interviewer is not documented. | sex_recorded | man, woman; not offered: nonbinary, other | SEXE |  | `ponder3` (not usable yet) | Not offered |

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

Family `education` · type Ordinal · timing Time-invariant

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
| qes_crop_2007_2010 | `scol` (each poll) | `approximate` | Years of schooling in four ranges named after the levels (7 or fewer, primary; 8 to 12, secondary; 13 to 15, CEGEP; 16 or more, university), by telephone, not the highest level completed. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  | `XPOND` (not usable yet) | Not documented |
| qes2008 | `q77` (post) | `comparable` | Level of education with and without diploma, options that collapse exactly into the four groups; the mode needs confirming, the anchor is web. | edu_levels | primary, secondary, college, university | What is the highest level of education that you have completed? |  |  | Offered explicitly |
| qes2007 | `q77` (post) | `comparable` | Level of education with and without diploma, options that collapse exactly into the four groups; mixed telephone and web interviews, the anchor is web. | edu_levels | primary, secondary, college, university | Quel est votre niveau d’éducation ? |  | `pond` | Volunteered only |
| qes2007_panel | `scol` (any wave) | `approximate` | Years of schooling in four ranges named after the levels (7 or fewer, primary; 8 to 12, secondary; 13 to 15, CEGEP; 16 or more, university), by telephone, not the highest level completed. | edu_years | primary, secondary, college, university | Combien d’années d’études avez-vous complétées? |  |  | Volunteered only |

#### `lang_mother`: Mother tongue

The first language the respondent learned at home in childhood and still
understands: French, English or another language. A respondent who
reports two first languages is a missing value (reason not_mappable),
never assigned to one of them.

Family `language` · type Categorical · timing Time-invariant

**Levels**

| Code | Name      | Label   |
|------|-----------|---------|
| 1    | `french`  | French  |
| 2    | `english` | English |
| 3    | `other`   | Other   |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_lang_1` + `cps_lang_2` + `cps_lang_3` (cps) | `approximate` | A select-all question (the languages first learned and still understood): a respondent who ticked one option gets it; two or more (146) are not_mappable, as a second mother tongue elsewhere; web, asked during the campaign. | lang_first_multiselect | french, english, other | Which language(s) did you learn as a child and still understand today? (Select all that apply) |  | `cps_weight_general` | Not offered |
| qes2018 | `qlangue` (post) | `comparable` | Same stem on the web, asking for the main language first learned (langue principale); the file has no value labels. | lang_first | french, english, other | Quelle est la langue principale que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `pond` | Offered explicitly |
| qes2018_panel | `s1` (pre) | `comparable` | Shorter stem (first language learned and still understood, without ‘at home in childhood’), by telephone and web; one language only. | lang_first | french, english, other | Quelle est la première langue que vous avez apprise et que vous comprenez toujours? |  | `weight` | Not documented |
| qes2014 | `QLANG` (post) | `comparable` | Same stem on the web, with three options for two first languages, which are missing values here (not_mappable). | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours? |  | `POND` | Offered explicitly |
| qes2012 | `langu` (post) | `identical` (anchor) | Anchor row of the target. | lang_first | french, english, other | What is the language you first learned at home in your childhood and that you still understand? |  | `pond` | Offered explicitly |
| qes2012_panel | `lmat` (pre) | `comparable` | Mother tongue defined as the first language learned and still understood, by telephone; one language only. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire celle que vous avez appris à parler en premier et que vous comprenez toujours? |  | `pondam1` (not usable yet) | Not documented |
| qes_crop_2007_2010 | `lmat` (each poll) | `comparable` | Mother tongue asked without a definition, by telephone; one language only. | lang_mother | french, english, other | Quelle est votre langue maternelle? |  | `XPOND` (not usable yet) | Not documented |
| qes2008 | `langu` (post) | `comparable` | Same stem and options; the mode needs confirming, the anchor is web. | lang_first | french, english, other | What is the language you first learned at home in your childhood and that you still understand? |  |  | Offered explicitly |
| qes2007 | `langu` (post) | `comparable` | Same stem, with options for two first languages, which are missing values here (not_mappable); mixed telephone and web interviews, the anchor is web. | lang_first_multi | french, english, other | Quelle est la langue que vous avez apprise en premier lieu à la maison dans votre enfance et que vous comprenez toujours ? |  | `pond` | Volunteered only |
| qes2007_panel | `lmat` (any wave) | `comparable` | Mother tongue defined as the first language learned and still spoken, by telephone; one language only. | lang_first | french, english, other | Quelle est votre langue maternelle, c’est-à-dire la première langue que vous avez apprise et que vous pouvez encore parler? |  |  | Volunteered only |

#### `born_canada`: Born in Canada

Whether the respondent was born in Canada. From a question on birthplace
(Quebec, elsewhere in Canada, outside Canada) where the study asks that
one, collapsed exactly.

Family `birthplace` · type Categorical · timing Time-invariant

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

#### `income_native`: Household income (each study’s own brackets)

The respondent’s household income before taxes as each study recorded
it: the text of the study’s own bracket (or the amount, where the study
asked for one). Brackets differ between studies, so the values are not
comparable across studies; don’t know and refusals are missing values.

Family `income` · type Text · timing Time-invariant

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_income` (cps) | `approximate` | The amount in dollars, typed on the web, not a bracket. | income_amount |  | What was your total household income, before taxes, for the year 2021? Be sure to include income from all sources, to the nearest thousand dollars. |  | `cps_weight_general` | Not offered |
| qes2018_panel | `d5` (pre) | `approximate` | Seven brackets from less than \$20,000 to \$150,000 and more, by telephone and web, not the anchor’s nine; the deposited question text is cut, so whether income is before taxes and for which year is not documented. | income_7brackets |  | Laquelle des catégories suivantes décrit le mieux le revenu total de votre foyer, c’est-à-dire le total des revenus |  | `weight` | Not documented |
| qes2014 | `Q57` (post) | `comparable` | The same nine brackets as the anchor, for the previous year, on the web; no don’t-know option. | income_9brackets |  | Parmi les catégories suivantes, laquelle reflète le mieux le revenu total avant impôt de tous les membres de votre foyer pour l’année 2013? 3. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Était-ce: |  | `POND` | Not offered |
| qes2012 | `reven` (post) | `identical` (anchor) | Anchor row of the target. | income_9brackets |  | And now your total household income before taxes for 2011. That includes income from all sources such as savings, pensions, rent, as well as wages. Was it: |  | `pond` | Offered explicitly |
| qes_crop_2007_2010 | `revenu` (each poll) | `approximate` | Five brackets of \$20,000 up to \$80,000 and more, by telephone, not the anchor’s nine. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu |  | `XPOND` (not usable yet) | Not documented |
| qes2008 | `q78` (post) | `approximate` | Ten brackets, from under \$20,000 then \$10,000 steps to more than \$100,000, not the anchor’s nine; asked about the year before the election (2007). | income_10brackets |  | And now what is your total household income before taxes for 2007? That includes income FROM ALL SOURCES such as savings, pensions, rent, as well as wages. Was it … |  |  | Offered explicitly |
| qes2007 | `q78` (post) | `approximate` | Ten brackets of \$10,000 up to more than \$100,000, not the anchor’s nine; asked about the year before the election. | income_10brackets |  | Et maintenant le revenu total de votre ménage avant impôts en 2006. Ceci inclut les revenus de toutes les sources telles l’épargne, les pensions, les loyers, en plus des salaires. Est-ce…? |  | `pond` | Offered explicitly |
| qes2007_panel | `revenu` (any wave) | `approximate` | Five brackets of \$20,000 up to \$80,000 and more, by telephone, not the anchor’s nine. | income_5brackets |  | Dans laquelle des catégories suivantes se situe le revenu annuel total, avant impôts et déductions, de tous les membres de votre foyer, en vous incluant? Est-ce… |  |  | Volunteered only |

#### `religion`: Religion (each study’s own categories)

The respondent’s religion as each study recorded it: the text of the
study’s own category. Categories differ between studies. Where the
question was asked only of respondents who belong to a religion, the
others are missing values: reason inapplicable for those who said they
belong to none, refused for those who would not answer the filter
question, which is the gate of the crosswalk row.

Family `faith` · type Text · timing Time-invariant

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `cps_religion` (cps) | `approximate` | A single question with a long list of denominations and none (no filter question), on the web; not the anchor’s six categories. | religion_list |  | Please tell me what is your religion, if you have one? |  | `cps_weight_general` | Not offered |
| qes2014 | `Q63` (post) | `comparable` | Same filter question and categories as the anchor, with a slightly longer question stem, on the web. | religion_list |  | Which religion do you belong to? | Q62: 2 = inapplicable, 9 = refused | `POND` | Not offered |
| qes2012 | `q103` (post) | `identical` (anchor) | Anchor row of the target. | religion_list |  | Which religion? | q102: 2 = inapplicable, 3 = refused | `pond` | Not offered |

#### `region_cma3`: Region (Montreal and Quebec CMAs)

Where the respondent lives: the Montreal census metropolitan area (CMA),
the Quebec CMA or the rest of Quebec, from the region the study recorded
for its sampling, quotas or weighting (not a question of its own).

Family `region` · type Categorical · timing Time-invariant

**Levels**

| Code | Name         | Label          |
|------|--------------|----------------|
| 1    | `mtl_cma`    | Montreal CMA   |
| 2    | `quebec_cma` | Quebec CMA     |
| 3    | `rest`       | Rest of Quebec |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `regio` (post) | `identical` | The study’s sampling region: the same three areas as the anchor. | region_cma_sample | mtl_cma, quebec_cma, rest | document 425914, regio |  | `pond` | Not offered |
| qes2018_panel | `region` (pre) | `approximate` | The producer’s recoded region (Montreal island, its ‘Couronne’, the ‘Région de Québec’, rest of Quebec): the ring and the Quebec region are not documented as the census metropolitan areas. | region_recoded | mtl_cma, quebec_cma, rest | document 333052, region |  | `weight` | Not offered |
| qes2014 | `REGIO` (post) | `identical` | The study’s sampling region: the same three areas as the anchor. | region_cma_sample | mtl_cma, quebec_cma, rest | document 425916, REGIO |  | `POND` | Not offered |
| qes2012 | `regio` (post) | `identical` (anchor) | Anchor row of the target: the producer’s derived region variable REGIO (Montreal CMA, Quebec CMA, other regions; variable label empty), matching the RÉGION among POND’s weighting variables (technical report, file 196368: SEXE, ÂGE, RÉGION, LANGUE). Not a question: Q110 (17 administrative regions) is in the file as q0qc, but REGIO splits regions that straddle the CMAs, so it is not a recode of q0qc alone. Q111 (postal code) is not in the file. | region_cma_sample | mtl_cma, quebec_cma, rest | document 425917, REGIO; document 196368, RÉGION |  | `pond` | Not offered |
| qes2012_panel | `reg` (pre) | `comparable` | The panel’s sampling region in four areas (Montreal island, rest of the Montreal CMA, Quebec CMA, rest of Quebec), the first two joined exactly. | region_cma_sample | mtl_cma, quebec_cma, rest | document 361043, reg |  | `pondam1` (not usable yet) | Not offered |
| qes_crop_2007_2010 | `REG` (each poll) | `comparable` | CROP’s sampling region in four areas (Montreal island, rest of the Montreal CMA, Quebec CMA, rest of Quebec), the first two joined exactly. | region_cma_sample | mtl_cma, quebec_cma, rest | document 329990, REG |  | `XPOND` (not usable yet) | Not offered |
| qes2008 | `regio` (post) | `comparable` | Derived in the questionnaire (CALCM/CALCQ/CALCA -\> REGIO) from the screening questions on administrative region (Q0QC) and city (Q0QCA-Q0QCE), and used for quotas: the same three areas as the anchor, with the CMA parts defined by the questionnaire’s city lists. Graded comparable, not identical: the deposit records the mode as telephone (unconfirmed), unlike the web anchor. | region_cma_sample | mtl_cma, quebec_cma, rest | In which Quebec area do you live? \[+ In which city do you live?\] |  |  | Not offered |
| qes2007 | `nomx` (post) | `comparable` | The 21 sampling subgroups (administrative regions, those around the two metropolitan areas split into their CMA part and the rest), joined into the three areas: Montreal CMA = Montreal, Laval and the CMA parts of Lanaudière, Laurentides and Montérégie; Quebec CMA = Quebec CMA and the CMA part of Chaudière-Appalaches. | region_admin_cma | mtl_cma, quebec_cma, rest | document 425921, nomx |  | `pond` | Not offered |

#### `lang_home`: Language spoken most often at home

The language the respondent speaks most often at home: French, English
or another language.

Family `language` · type Categorical · timing Time-invariant

**Levels**

| Code | Name      | Label   |
|------|-----------|---------|
| 1    | `french`  | French  |
| 2    | `english` | English |
| 3    | `other`   | Other   |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2022 | `pes_langhome_1` + `pes_langhome_2` + `pes_langhome_3` + `pes_langhome_4` + `pes_langhome_5` + `pes_langhome_6` + `pes_langhome_7` + `pes_langhome_8` + `pes_langhome_9` + `pes_langhome_10` + `pes_langhome_11` + `pes_langhome_12` + `pes_langhome_13` + `pes_langhome_14` + `pes_langhome_15` + `pes_langhome_16` + `pes_langhome_17` (pes) | `approximate` | A select-all question (the languages usually spoken at home, 17 options), not the one spoken most often: a respondent who ticked options of one level gets it; English and French, or either with another language (171 in all), are not_mappable; web, asked after the election. | lang_home_multiselect | french, english, other | Which language(s) do you usually speak at home? |  | `pes_weight_general` | Not offered |
| qes2018 | `q70` (post) | `comparable` | Same question and list as the anchor, web, with don’t know and refusal shown. | lang_home_list | french, english, other | Which language do you speak most often at home? |  | `pond` | Offered explicitly |
| qes2014 | `Q66` (post) | `comparable` | Same question and list as the anchor, web, with don’t know and refusal shown. | lang_home_list | french, english, other | Which language do you speak most often at home? |  | `POND` | Offered explicitly |
| qes2012 | `q107` (post) | `identical` (anchor) | Anchor row of the target. | lang_home_list | french, english, other | Which language do you speak most often at home? |  | `pond` | Not documented |
| qes_crop_2007_2010 | `lusag` (each poll) | `comparable` | The language spoken most often in the household (‘dans votre foyer’), three options, by telephone, in each poll. | lang_home_3 | french, english, other | Quelle langue parle-t-on le plus souvent dans votre foyer? |  | `XPOND` (not usable yet) | Not documented |
| qes2008 | `q80` (post) | `comparable` | Same question as the anchor; the list is nearly the same (no Inuktitut, so Cree is code 13, and no don’t-know code); telephone by the deposit metadata, the anchor is web. | lang_home_list | french, english, other | What language do you speak most often at home? |  |  | Not offered |
| qes2007 | `q80` (post) | `comparable` | Same question and list as the anchor; the study mixes telephone and web interviews. | lang_home_list | french, english, other | What language do you speak most often at home? |  | `pond` | Not documented |
| qes2007_panel | `lusage` (any wave) | `comparable` | The language spoken most often in the household (‘dans votre foyer’), not the respondent’s own, three options, by telephone; one language only. | lang_home_3 | french, english, other | Quelle langue parle-t-on le plus souvent dans votre foyer? |  |  | Volunteered only |

#### `relig_attend`: Attendance at religious services

How often the respondent attends services at their place of worship, not
counting weddings and funerals, from every week to hardly ever or never.
Where the study asked only those who belong to a religion, those who do
not are hardly ever or never (graded approximate).

Family `faith` · type Ordinal · timing Any time

**Levels**

| Code | Name          | Label                |
|------|---------------|----------------------|
| 1    | `weekly`      | Every week           |
| 2    | `twice_month` | Twice a month        |
| 3    | `monthly`     | Once a month         |
| 4    | `yearly`      | Once or twice a year |
| 5    | `never`       | Hardly ever or never |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q68` (post) | `approximate` | Asked only of those who belong to a religion (the filter question): those who do not are set to hardly ever or never, and those who refused the filter are inapplicable. The anchor asks everyone, so the shares are not directly comparable. | attend_5_filtered | weekly, twice_month, monthly, yearly, never | Not counting weddings and funerals, how often do you attend services at your place of worship? | q66: 2 = never, 9 = inapplicable | `pond` | Offered explicitly |
| qes2014 | `Q64` (post) | `approximate` | Asked only of those who belong to a religion (the filter question): those who do not are set to hardly ever or never, and those who refused the filter are inapplicable. The anchor asks everyone, so the shares are not directly comparable. | attend_5_filtered | weekly, twice_month, monthly, yearly, never | Not counting weddings and funerals, how often do you attend services at your place of worship? | Q62: 2 = never, 9 = inapplicable | `POND` | Offered explicitly |
| qes2012 | `q104` (post) | `identical` (anchor) | Anchor row of the target: asked of every respondent. | attend_5 | weekly, twice_month, monthly, yearly, never | Not counting weddings and funerals, how often do you attend services at your place of worship? |  | `pond` | Offered explicitly |
| qes2008 | `q81` (post) | `comparable` | Same question and options, asked of every respondent, by telephone. | attend_5 | weekly, twice_month, monthly, yearly, never | Not counting weddings and funerals, how often do you attend services at your place of worship: is it…? |  |  | Not documented |
| qes2007 | `q81` (post) | `comparable` | Same question and options, asked of every respondent; the study mixes telephone and web interviews. | attend_5 | weekly, twice_month, monthly, yearly, never | Not counting weddings and funerals, how often do you attend services at your place of worship: every week, twice a month, once a month, once or twice a year, or hardly ever? |  | `pond` | Not documented |

#### `birthplace3`: Birthplace (Quebec, rest of Canada, abroad)

Where the respondent was born: in Quebec, elsewhere in Canada or outside
Canada. Where both are harmonized, born_canada is yes exactly where this
is quebec or other_canada.

Family `birthplace` · type Categorical · timing Time-invariant

**Levels**

| Code | Name           | Label               |
|------|----------------|---------------------|
| 1    | `quebec`       | Quebec              |
| 2    | `other_canada` | Elsewhere in Canada |
| 3    | `abroad`       | Outside Canada      |

**Coverage**

| Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don’t know |
|----|----|----|----|----|----|----|----|----|----|
| qes2018 | `q69` (post) | `comparable` | Same question and options, web, with don’t know shown. | birthplace_3 | quebec, other_canada, abroad | Where were you born? |  | `pond` | Offered explicitly |
| qes2014 | `Q65` (post) | `comparable` | Same question and options, web, with don’t know shown. | birthplace_3 | quebec, other_canada, abroad | Where were you born? |  | `POND` | Offered explicitly |
| qes2012 | `q105` (post) | `identical` (anchor) | Anchor row of the target. | birthplace_3 | quebec, other_canada, abroad | Where were you born? |  | `pond` | Offered explicitly |

## Pooled variables

A pooled variable is one column for every study that pools several
targets, its members: `vote_choice` pools the reported vote and the vote
intentions, `sov_support` the referendum wordings, `pol_interest` the
interest scales. The members stay targets, each one stimulus; the pooled
column records, row by row, which member its value comes from
(`<pooled>__type`), that member’s grade (`<pooled>__grade`, never
raised; a lossy transform caps it at approximate) and its item
(`<pooled>__item`, `study:wave:source variables`).
`qes_harmonize(targets = "vote_choice")` returns the pooled column and
its companions; `types = list(vote_choice = "recall")` keeps some
members only.

How a row gets its value: the members of the requested types are tried
in order of precedence. The first member whose cell has a value, or a
missing value that is an answer (don’t know, refused, did not vote, …),
sets the row. A member that did not ask the respondent (not in the wave,
not asked, not reviewed, below the grade, routed out, system missing, a
code that straddles levels) passes to the next. When every member
passes, the row is `NA` with the reason of the first usable member that
has a row in the study and wave, else of the first member that has a
row. In the respondent layout (one row per respondent), a study’s values
come from one wave: that of the first member it applies, so that one
weight column fits them; the long layout (`layout = "long"`, one row per
respondent and wave) keeps every wave.

### `vote_choice`: Provincial vote choice (pooled)

The party of the respondent’s vote in the Quebec general election of the
study, one variable for every study: the reported vote (recall), asked
after the election, where the study asked it; else the vote intention
with those who named no party (the undecided and, in some studies, those
who would not vote, would vote for none or refused) pushed toward the
party they lean to, so no_party is lower under intention_push than under
intention; else the vote intention at the first question. The wordings
differ from study to study and vote_choice\_\_type says which question
each value comes from. Would not vote, none or would spoil (level
no_party) is an answer only in intentions; in a reported vote, nonvoters
and spoiled ballots are missing values with a reason.

type Categorical

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

**Members**

| Precedence | Type | Member (target) | Default | Transform | Grade cap |
|----|----|----|----|----|----|
| 1 | `recall`: Reported vote (after the election). The act the study is about, and the one official results measure: used first. | [`vote_prov_recall`](#target-vote_prov_recall) | yes | `identity` |  |
| 2 | `intention_push`: Vote intention, undecided pushed. The first question plus a push put to those who named no party at it: a party named at the first question is never changed. Who was pushed differs by study: the undecided everywhere, and also those who would not vote, would vote for none or would spoil (qes1998, qes2007_panel, qes2012_panel, the CROP polls) or who refused (qes1998, qes2012_panel). The push can turn such an answer into a (softer) party or into another answer without a party: in qes2012_panel, of 172 don’t know 77 name a party, 1 would not vote and 1 refuses, of 30 refusals 6 name a party, 2 would not vote and 4 don’t know, and of 14 would not vote 3 name a party and 2 don’t know; in qes2007_panel, of 81 would not vote, none or spoil, 25 name a party and 14 don’t know or refuse; in qes1998, 5 would-not-vote answers and 20 refusals name a party, and 4 and 6 become don’t know. So no_party is lower under intention_push than under intention. In qes2022 it also adds the conditional intention of respondents unlikely to vote, who were not asked the first question (cps_votechoice2, ‘If you decide to vote, which party do you think you would vote for?’, then cps_votelean if they did not know: 62 respondents, 43 of whom name a party). | [`vote_prov_intent_push`](#target-vote_prov_intent_push) | yes | `identity` |  |
| 3 | `intention`: Vote intention (first question) | [`vote_prov_intent`](#target-vote_prov_intent) | yes | `identity` |  |

**Coverage**

| Study | `recall` | `intention_push` | `intention` | Respondent layout uses |
|----|----|----|----|----|
| `qes2022` | Comparable (pes) | Approximate (cps) | Approximate (cps) | `recall` |
| `qes2018` | Comparable (post) | — | — | `recall` |
| `qes2018_panel` | Comparable (post) | Approximate (pre) | Approximate (pre) | `recall` |
| `qes2014` | Comparable (post) | — | — | `recall` |
| `qes2012` | Identical (post) | — | — | `recall` |
| `qes2012_panel` | Approximate (post) | Approximate (pre) | Approximate (pre) | `recall` |
| `qes_crop_2007_2010` | — | Comparable (each poll) | Comparable (each poll) | `intention_push` |
| `qes2008` | Comparable (post) | — | — | `recall` |
| `qes2007` | Comparable (post) | — | — | `recall` |
| `qes2007_panel` | Approximate (post) | Identical (pre) | Identical (pre) | `recall` |
| `qes1998` | Comparable (post) | Approximate (pre) | — | `recall` |

Each cell gives the member’s grade in the study (capped by its grade
cap) and the wave that asked it; a dash means the study has no question
for the member. The last column is the member the respondent layout
takes the study’s values from, among the default members (the long
layout uses every wave).

### `sov_support`: Support for sovereignty (pooled)

How the respondent would vote (yes, no, would not vote) on Quebec
sovereignty, one variable for every study that asked about it, across
the referendum wordings: an independent country; a sovereign country;
the 1995 question (sovereignty with an offer of partnership), with the
undecided pushed where the study pushed them; and, collapsed to yes or
no, being favourable or opposed to Quebec independence. Support depends
on the wording: sov_support\_\_type says which wording each value comes
from.

type Categorical

**Levels**

| Code | Name             | Label                        |
|------|------------------|------------------------------|
| 1    | `yes`            | Yes                          |
| 2    | `no`             | No                           |
| 95   | `would_not_vote` | Would not vote / would spoil |

**Members**

| Precedence | Type | Member (target) | Default | Transform | Grade cap |
|----|----|----|----|----|----|
| 1 | `independence`: Referendum on an independent country | [`sov_indep`](#target-sov_indep) | yes | `identity` |  |
| 2 | `sovereign_country`: Referendum on a sovereign country | [`sov_sovereign_country`](#target-sov_sovereign_country) | yes | `identity` |  |
| 3 | `partnership_1995_push`: 1995 question, undecided pushed | [`sov_partnership_1995_push`](#target-sov_partnership_1995_push) | yes | `identity` |  |
| 4 | `partnership_1995`: 1995 question (sovereignty-partnership) | [`sov_partnership_1995`](#target-sov_partnership_1995) | yes | `identity` |  |
| 5 | `favour`: Favourable or opposed to independence (collapsed to yes or no). Four points collapsed to two: graded approximate at most. | [`sov_favour`](#target-sov_favour) | yes | `recode:very_favourable=yes,somewhat_favourable=yes,somewhat_opposed=no,very_opposed=no` | `approximate` |

**Coverage**

| Study | `independence` | `sovereign_country` | `partnership_1995_push` | `partnership_1995` | `favour` | Respondent layout uses |
|----|----|----|----|----|----|----|
| `qes2022` | Comparable (cps) | — | — | — | — | `independence` |
| `qes2018` | Comparable (post) | — | — | — | — | `independence` |
| `qes2018_panel` | — | — | — | — | Approximate (post) | `favour` |
| `qes2014` | Identical (post) | — | — | — | — | `independence` |
| `qes2012` | Identical (post) | — | — | — | — | `independence` |
| `qes2012_panel` | — | Identical (pre) | — | — | — | `sovereign_country` |
| `qes_crop_2007_2010` | — | — | — | — | — | — |
| `qes2008` | — | — | Comparable (post) | Comparable (post) | — | `partnership_1995_push` |
| `qes2007` | — | — | Identical (post) | Identical (post) | — | `partnership_1995_push` |
| `qes2007_panel` | — | — | Comparable (pre) | Comparable (pre) | — | `partnership_1995_push` |
| `qes1998` | — | — | Comparable (pre) | Comparable (pre) | — | `partnership_1995_push` |

Each cell gives the member’s grade in the study (capped by its grade
cap) and the wave that asked it; a dash means the study has no question
for the member. The last column is the member the respondent layout
takes the study’s values from, among the default members (the long
layout uses every wave).

### `pol_interest`: Interest in politics (pooled, 0-1)

How interested the respondent is in politics, on a scale from 0 (not at
all) to 1 (very), one variable for every study that asked: the
four-point items scored 1, 0.7, 0.3 and 0 (very, quite, hardly and not
at all interested), the 0-10 items divided by 10. Interest in politics
in general comes before interest in the campaign or the election.
pol_interest\_\_type says which item each value comes from; a scored
four-point item is graded approximate at most, since its scores are an
assumption.

type Numeric

**Valid range**: 0-1

**Members**

| Precedence | Type | Member (target) | Default | Transform | Grade cap |
|----|----|----|----|----|----|
| 1 | `general_4pt`: Interest in politics, four points (scored). Scored 1, 0.7, 0.3 and 0 (very, quite, hardly and not at all interested). | [`interest_4pt`](#target-interest_4pt) | yes | `score:very=1,quite=0.7,hardly=0.3,not_at_all=0` | `approximate` |
| 2 | `general_0_10`: Interest in politics, 0-10 | [`interest_0_10`](#target-interest_0_10) | yes | `affine:0.1*x` |  |
| 3 | `campaign_4pt`: Interest in the campaign, four points (scored) | [`interest_campaign_4pt`](#target-interest_campaign_4pt) | yes | `score:very=1,quite=0.7,hardly=0.3,not_at_all=0` | `approximate` |
| 4 | `election_0_10`: Interest in the election, 0-10. Interest in one election, not in politics: graded approximate at most. | [`interest_election_0_10`](#target-interest_election_0_10) | yes | `affine:0.1*x` | `approximate` |

**Coverage**

| Study | `general_4pt` | `general_0_10` | `campaign_4pt` | `election_0_10` | Respondent layout uses |
|----|----|----|----|----|----|
| `qes2022` | — | Approximate (cps) | — | — | `general_0_10` |
| `qes2018` | Approximate (post) | — | — | — | `general_4pt` |
| `qes2018_panel` | — | — | — | — | — |
| `qes2014` | Approximate (post) | — | — | — | `general_4pt` |
| `qes2012` | Approximate (post) | — | — | — | `general_4pt` |
| `qes2012_panel` | — | — | — | — | — |
| `qes_crop_2007_2010` | — | — | — | — | — |
| `qes2008` | — | — | — | Approximate (post) | `election_0_10` |
| `qes2007` | — | Identical (post) | — | Approximate (post) | `general_0_10` |
| `qes2007_panel` | — | — | Approximate (pre) | — | `campaign_4pt` |
| `qes1998` | — | — | — | — | — |

Each cell gives the member’s grade in the study (capped by its grade
cap) and the wave that asked it; a dash means the study has no question
for the member. The last column is the member the respondent layout
takes the study’s values from, among the default members (the long
layout uses every wave).

### `turnout`: Turnout in the provincial election (pooled)

Whether the respondent voted in the Quebec general election of the study
(yes, no): the reported turnout, asked after the election. With types =
list(turnout = c(“recall”, “intention”)), a study that asked no reported
turnout gives instead the likelihood of voting asked before the
election, collapsed to yes (certain or likely to vote, already voted) or
no (unlikely, certain not to vote), graded approximate at most: an
intention is not a turnout, so it is not used by default.

type Categorical

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Members**

| Precedence | Type | Member (target) | Default | Transform | Grade cap |
|----|----|----|----|----|----|
| 1 | `recall`: Reported turnout (after the election) | [`turnout_prov_recall`](#target-turnout_prov_recall) | yes | `identity` |  |
| 2 | `intention`: Likelihood of voting (before the election, collapsed). Not used by default: an intention to vote is not a turnout. | [`turnout_prov_likely`](#target-turnout_prov_likely) | no | `recode:certain=yes,likely=yes,already_voted=yes,unlikely=no,certain_not=no` | `approximate` |

**Coverage**

| Study | `recall` | `intention` | Respondent layout uses |
|----|----|----|----|
| `qes2022` | Approximate (pes) | Approximate (cps) | `recall` |
| `qes2018` | Approximate (post) | — | `recall` |
| `qes2018_panel` | Approximate (post) | — | `recall` |
| `qes2014` | Comparable (post) | — | `recall` |
| `qes2012` | Identical (post) | — | `recall` |
| `qes2012_panel` | Comparable (post) | — | `recall` |
| `qes_crop_2007_2010` | — | — | — |
| `qes2008` | Comparable (post) | — | `recall` |
| `qes2007` | Comparable (post) | — | `recall` |
| `qes2007_panel` | Comparable (post) | — | `recall` |
| `qes1998` | Comparable (post) | — | `recall` |

Each cell gives the member’s grade in the study (capped by its grade
cap) and the wave that asked it; a dash means the study has no question
for the member. The last column is the member the respondent layout
takes the study’s values from, among the default members (the long
layout uses every wave).

## Relaxed harmonization: qes_decon()

[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
puts one concept in one column for every study, even when the wording or
the answer options differ, with coarse common categories, under plain
column names. It trades exactness for coverage: a relaxed column has no
grade and does not claim that two studies asked the same question; the
targets above keep the strict, graded versions. A column is built from a
strict target or a pooled variable where one exists, recoded into the
column’s categories where needed, and from relaxed mappings of the
studies’ own questions where the strict layer has none. A relaxed
mapping is applied once a reviewer has signed it off (status `stable`).

### `citizenship`: Citizenship

Whether the respondent is a Canadian citizen.

Base: `target:citizen` · `recode:yes=citizen,no=not_citizen` · one value
per respondent

**How it is relaxed**: Canadian citizenship where a study recorded it:
asked directly in 2022, and taken in the 2018 panel from the screening
question on the right to vote in the coming Quebec election, so that
every respondent of that panel is a citizen.

**Levels**

| Code | Name          | Label                  |
|------|---------------|------------------------|
| 1    | `citizen`     | Canadian citizen       |
| 2    | `not_citizen` | Not a Canadian citizen |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2018_panel | `qa` (pre) | 1 = Canadian citizen; 2 = Source category straddles target levels (NA); 3 = Don’t know or refused (one code) (NA) | fills the study | OD-R12: the screening question asks whether the respondent may vote in the coming Quebec election, which requires Canadian citizenship; every respondent answered yes, so the column is constant in this study. | stable |

### `yob`: Year of birth

The respondent’s year of birth.

Base: `target:birth_year` · `identity` · one value per respondent

**How it is relaxed**: The year of birth as reported; the studies that
asked only an age group have none (see age_group).

**Valid range**: 1900-2010

**Relaxed mappings**

None: every study’s values come from the base.

### `age_group`: Age group

The respondent’s age group.

Base: `target:age_group3` · `identity` · one value per respondent

**How it is relaxed**: Three age groups (18-34, 35-54, 55 and over),
from the study’s own age bands or from the age at the start of
fieldwork; respondents under 18 are missing.

**Levels**

| Code | Name       | Label       |
|------|------------|-------------|
| 1    | `a18_34`   | 18-34       |
| 2    | `a35_54`   | 35-54       |
| 3    | `a55_plus` | 55 and over |

**Relaxed mappings**

None: every study’s values come from the base.

### `gender`: Gender

The respondent’s gender.

Base: `target:gender` · `identity` · one value per respondent

**How it is relaxed**: Gender or sex as each study asked it: man or
woman everywhere, and in 2022 also non-binary or another gender; the
questions differ, the categories do not.

**Levels**

| Code | Name        | Label          |
|------|-------------|----------------|
| 1    | `man`       | Man            |
| 2    | `woman`     | Woman          |
| 3    | `nonbinary` | Non-binary     |
| 4    | `other`     | Another gender |

**Relaxed mappings**

None: every study’s values come from the base.

### `education4`: Education (four groups)

The highest level of schooling the respondent completed, in four groups.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: Each study’s levels are grouped into four: no
high school diploma, high school, college (CEGEP, technical or trade
school) and university, including some university; the 2007 panel and
the CROP polls asked years of schooling, so their high school group also
holds those who left secondary school without a diploma, and their
college group may hold some respondents with some university but no
degree. qes1998 is missing (not mappable): the middle group of its
pooled file, 10 to 15 years of schooling, spans high school and college
(education, in three groups, includes it). The strict target of the same
name in qes_harmonize() groups differently: its lowest group is primary
or less.

**Levels**

| Code | Name          | Label                          |
|------|---------------|--------------------------------|
| 1    | `no_diploma`  | No high school diploma         |
| 2    | `high_school` | High school diploma            |
| 3    | `college`     | College, CEGEP or trade school |
| 4    | `university`  | University                     |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_edu` (cps) | 1-4 = No high school diploma; 5 = High school diploma; 6, 7 = College, CEGEP or trade school; 8-11 = University; -99 = No answer (item nonresponse) (NA) | fills the study |  | stable |
| qes2018 | `qscol` (post) | 1-7 = No high school diploma; 8 = High school diploma; 9-12 = College, CEGEP or trade school; 13-15 = University; 99 = Refused (NA) | fills the study | Code 9 (secondary 5 with a vocational diploma, DEP) is college (OD-R10); the strict education4 puts it with secondary. | stable |
| qes2014 | `QSCOL` (post) | 1-4 = No high school diploma; 5 = High school diploma; 6-8 = College, CEGEP or trade school; 9-11 = University; 99 = Refused (NA) | fills the study |  | stable |
| qes2012 | `scol` (post) | 1-4 = No high school diploma; 5 = High school diploma; 6-8 = College, CEGEP or trade school; 9-12 = University; 98 = Don’t know (NA); 99 = Refused (NA) | fills the study | Code 8 (post-secondary, not higher education; the French questionnaire’s technical course) is college; code 10 (certificate and diploma), in neither questionnaire, is university with code 9 (some higher education). | stable |
| qes2018_panel | `d3` (pre) | 1, 2 = No high school diploma; 3 = High school diploma; 4, 5 = College, CEGEP or trade school; 6-8 = University; 9 = Don’t know or refused (one code) (NA) | fills the study | Code 4 (registered apprenticeship or other trades certificate) is college (OD-R10); code 6 (university certificate below the bachelor’s) is university. | stable |
| qes2007_panel | `scol` (any wave) | 1 = No high school diploma; 2 = High school diploma; 3 = College, CEGEP or trade school; 4 = University; 9 = Refused (NA) | fills the study | Years of schooling, in bands named after a level: 7 years or less (primary) is no diploma, 8 to 12 years (secondary) is high school, 13 to 15 years (CEGEP, technical school) is college and 16 years or more is university. The high school group therefore also holds those who left secondary school without a diploma, and the college group may hold respondents with some university but no degree, whom other studies count as university. | stable |
| qes2007 | `q77` (post) | 1-4 = No high school diploma; 5 = High school diploma; 6, 7 = College, CEGEP or trade school; 8-11 = University; 98 = Don’t know (NA); 99 = Refused (NA) | fills the study |  | stable |
| qes2008 | `q77` (post) | 1-4 = No high school diploma; 5 = High school diploma; 6, 7 = College, CEGEP or trade school; 8-11 = University; 98 = Don’t know (NA); 99 = Refused (NA) | fills the study |  | stable |
| qes1998 | `scol` (pre) | 1-3 = Source category straddles target levels (NA); 9 = Don’t know or refused (one code) (NA) | fills the study | OD-R1: the pooled file groups years of schooling as 1-9, 10-15 and university; 10-15 years spans high school and college, and mapping only the other two groups would bias every share, so the study is left out of the four groups. The three groups of education include it (OD-R17). | stable |
| qes_crop_2007_2010 | `scol` (each poll) | 1 = No high school diploma; 2 = High school diploma; 3 = College, CEGEP or trade school; 4 = University; 9 = Refused (NA) | fills the study | Years of schooling in CROP’s four ranges (7 or fewer, primary; 8 to 12, secondary; 13 to 15, CEGEP or technical school; 16 or more, university): 7 or fewer is no diploma and 8 to 12 is high school, so that group also holds those who left secondary school without a diploma; grouping by years, not by highest level, can also put some university (14 or 15 years) in college. | stable |

### `education`: Education

The highest level of schooling the respondent completed, in three
groups.

Base: `column:education4` ·
`recode:no_diploma=no_diploma,high_school=high_school_college,college=high_school_college,university=university`
· one value per respondent

**How it is relaxed**: Each study’s levels are grouped into three: no
high school diploma, high school to college (a high school diploma,
CEGEP, technical or trade school) and university, including some
university. These are the groups of education4 with high school and
college joined, which lets qes1998 in. Where a study asked years of
schooling (the 2007 panel, the CROP polls and the CROP respondents of
qes1998), the bands do not follow diplomas: some respondents without a
diploma (10 years in qes1998; 8 to 10 years in the others, as Quebec
high school ends after 11 years) count as high school to college, and
some university students (14 or 15 years) are not counted in university.

**Levels**

| Code | Name                  | Label                          |
|------|-----------------------|--------------------------------|
| 1    | `no_diploma`          | No high school diploma         |
| 2    | `high_school_college` | High school to college (CEGEP) |
| 3    | `university`          | University                     |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes1998 | `scol` (pre) | 1 = No high school diploma; 2 = High school to college (CEGEP); 3 = University; 9 = Don’t know or refused (one code) (NA) | replaces the base | OD-R17: the pooled file’s three groups are labelled 1-9 years, 10-15 years and university or more, but the two firms built them differently. For the CREATEC respondents (firme_post = 1), scol recodes the highest level completed (r15: elementary or some secondary; secondary completed or technical, college or CEGEP; university, completed or not), so the three groups hold. For the CROP respondents (firme_post = 2), it groups years of schooling (CROP question 19: 7 or fewer, 8-9, 10-11, 12-15, 16 or more), so the match is approximate: Quebec high school ends after 11 years, so a respondent with 10 years has no diploma but counts as high school to college, and some respondents with 14 or 15 years are university students who count as high school to college. Code 9 is unlabelled and read as don’t know or refused. | stable |

### `income_cat`: Household income (thirds)

The household’s total income before taxes, in thirds of the study’s
respondents.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: Household income in thirds of each study’s own
respondents: income brackets are never split, so a third holds the whole
brackets whose midpoint falls in it and is rarely exactly a third; the
dollar limits differ from study to study.

**Levels**

| Code | Name     | Label              |
|------|----------|--------------------|
| 1    | `low`    | Low (bottom third) |
| 2    | `middle` | Middle             |
| 3    | `high`   | High (top third)   |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_income` (cps) | amount under 52200 = Low (bottom third); 52200 to under 95600 = Middle; 95600 or more = High (top third); -99, 0 pass to cps_income2: 1-3 = Low (bottom third); 4 = Middle; 5-8 = High (top third); -99 = No answer (item nonresponse) (NA) | fills the study | The amount (2021 income) in thirds of the 1,444 amounts given, unweighted: low under \$52,200, high \$95,600 or more. An amount of 0 or -99 (no amount) passes to the bracket question cps_income2, whose brackets go in by their midpoint against the same limits. | stable |
| qes2018 | `q61` (post) | 1-4 = Low (bottom third); 5, 6 = Middle; 7-9 = High (top third); 99 = Refused (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$72,000 or more (2017 income). | stable |
| qes2014 | `Q57` (post) | 1-4 = Low (bottom third); 5, 6 = Middle; 7-9 = High (top third); 99 = Refused (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$72,000 or more (2013 income). | stable |
| qes2012 | `reven` (post) | 1-4 = Low (bottom third); 5-7 = Middle; 8, 9 = High (top third); 98 = Don’t know (NA); 99 = Refused (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$88,000 or more (2011 income). | stable |
| qes2018_panel | `d5` (pre) | 1, 2 = Low (bottom third); 3, 4 = Middle; 5-7 = High (top third); 8 = Don’t know or refused (one code) (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$80,000 or more. | stable |
| qes2007_panel | `revenu` (any wave) | 1, 2 = Low (bottom third); 3 = Middle; 4, 5 = High (top third); 9 = Don’t know or refused (one code) (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$60,000 or more. | stable |
| qes2007 | `q78` (post) | 1-3 = Low (bottom third); 4-6 = Middle; 7-10 = High (top third); 98 = Don’t know (NA); 99 = Refused (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$70,000 or more (2006 income). | stable |
| qes2008 | `q78` (post) | 1-3 = Low (bottom third); 4-6 = Middle; 7-10 = High (top third); 98 = Don’t know (NA); 99 = Refused (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$70,000 or more (2007 income). | stable |
| qes_crop_2007_2010 | `revenu` (each poll) | 1, 2 = Low (bottom third); 3, 4 = Middle; 5 = High (top third); 9 = Don’t know or refused (one code) (NA) | fills the study | Thirds by the midpoint rule on the unweighted counts of the brackets (OD-R11). Low is under \$40,000, high \$80,000 or more; the thirds pool the 24 polls. | stable |

### `language`: Mother tongue

The respondent’s mother tongue: French, English or another language.

Base: `target:lang_mother` · `identity` · one value per respondent

**How it is relaxed**: The language first learned in childhood, in three
groups; a respondent who reported French and another language is French,
and English and a language other than French is English, and the 1998
CREATEC respondents are French by the design of their sample.

**Levels**

| Code | Name      | Label   |
|------|-----------|---------|
| 1    | `french`  | French  |
| 2    | `english` | English |
| 3    | `other`   | Other   |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_lang_2` (cps) | the first option ticked, in this order: cps_lang_2 (French), cps_lang_1 (English), cps_lang_3 (Other) | replaces the base | OD-R5: a respondent who reported two mother tongues is French when one is French, else English when one is English. | stable |
| qes2014 | `QLANG` (post) | 1, 4, 5 = French; 2, 6 = English; 3 = Other; 8 = Don’t know (NA); 9 = Refused (NA) | replaces the base | OD-R5: a respondent who reported two mother tongues is French when one is French, else English when one is English. | stable |
| qes2007 | `langu` (post) | 1, 4, 7 = French; 2, 5 = English; 3, 6 = Other; 9 = Don’t know or refused (one code) (NA) | replaces the base | OD-R5: a respondent who reported two mother tongues is French when one is French, else English when one is English. | stable |
| qes1998 | `firme_post` (pre) | 1 = French; 2 = Source category straddles target levels (NA) | fills the study | OD-R3: the CREATEC sample (firme_post = 1) holds only respondents whose mother tongue is French (codebook of the CREATEC file); the CROP respondents were screened on another criterion and have no mother tongue. | stable |

### `language_fr`: French as a mother tongue

Whether French is one of the respondent’s mother tongues.

Base: `column:language` · `recode:french=yes,english=no,other=no` · one
value per respondent

**How it is relaxed**: Yes when French is among the mother tongues
reported, so a respondent with two mother tongues can be yes in both
language_fr and language_eng.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_lang_2` (cps) | the first option ticked, in this order: cps_lang_2 (Yes), cps_lang_1 (No), cps_lang_3 (No) | replaces the base | Yes when this language is among the mother tongues reported. | stable |
| qes2014 | `QLANG` (post) | 1, 4, 5 = Yes; 2, 3, 6 = No; 8 = Don’t know (NA); 9 = Refused (NA) | replaces the base | Yes when this language is among the mother tongues reported. | stable |
| qes2007 | `langu` (post) | 1, 4, 7 = Yes; 2, 3, 5, 6 = No; 9 = Don’t know or refused (one code) (NA) | replaces the base | Yes when this language is among the mother tongues reported. | stable |

### `language_eng`: English as a mother tongue

Whether English is one of the respondent’s mother tongues.

Base: `column:language` · `recode:french=no,english=yes,other=no` · one
value per respondent

**How it is relaxed**: Yes when English is among the mother tongues
reported, so a respondent with two mother tongues can be yes in both
language_eng and language_fr.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_lang_1` (cps) | the first option ticked, in this order: cps_lang_1 (Yes), cps_lang_2 (No), cps_lang_3 (No) | replaces the base | Yes when this language is among the mother tongues reported. | stable |
| qes2014 | `QLANG` (post) | 1, 3, 5 = No; 2, 4, 6 = Yes; 8 = Don’t know (NA); 9 = Refused (NA) | replaces the base | Yes when this language is among the mother tongues reported. | stable |
| qes2007 | `langu` (post) | 1, 3, 4, 6 = No; 2, 5, 7 = Yes; 9 = Don’t know or refused (one code) (NA) | replaces the base | Yes when this language is among the mother tongues reported. | stable |

### `religion`: Religion

The respondent’s religion: Catholic, Protestant, other Christian,
another religion or none.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: The religion the respondent belongs to, in five
groups; the 2022 question offers a long list, where agnostic counts as
no religion, and the other studies first ask whether the respondent
belongs to a religion at all.

**Levels**

| Code | Name              | Label           |
|------|-------------------|-----------------|
| 1    | `catholic`        | Catholic        |
| 2    | `protestant`      | Protestant      |
| 3    | `other_christian` | Other Christian |
| 4    | `other`           | Other religion  |
| 5    | `none`            | No religion     |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_religion` (cps) | 1, 2 = No religion; 3-7, 22 = Other religion; 8, 9, 13, 15-21 = Protestant; 10 = Catholic; 11, 12, 14 = Other Christian; -99 = No answer (item nonresponse) (NA) | fills the study | Those who belong to no religion are none; Jewish, Muslim and other non-Christian religions are other. Agnostic counts as none; the Orthodox churches, Jehovah’s Witnesses and Mormons are other Christian; the other Christian denominations listed are Protestant. | stable |
| qes2018 | `q67` (post) | where q66: 2 = No religion; 9 = Refused (NA); else q67: 1 = Catholic; 2 = Protestant; 3 = Other Christian; 4, 5, 96 = Other religion; 99 = Refused (NA) | fills the study | Those who belong to no religion are none; Jewish, Muslim and other non-Christian religions are other. | stable |
| qes2014 | `Q63` (post) | where Q62: 2 = No religion; 9 = Refused (NA); else Q63: 1 = Catholic; 2 = Protestant; 3 = Other Christian; 4-6 = Other religion; 9 = Refused (NA) | fills the study | Those who belong to no religion are none; Jewish, Muslim and other non-Christian religions are other. | stable |
| qes2012 | `q103` (post) | where q102: 2 = No religion; 3 = Refused (NA); else q103: 1 = Catholic; 2 = Protestant; 3 = Other Christian; 4-6 = Other religion; 9 = Refused (NA) | fills the study | Those who belong to no religion are none; Jewish, Muslim and other non-Christian religions are other. | stable |

### `marital`: Marital status

The respondent’s marital status, in four groups.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: Married or living with a partner, separated or
divorced, widowed, or never married; 2012 and 2014 asked the official
civil status, with no common-law option, so some partners who live
together are never married there.

**Levels**

| Code | Name                 | Label                            |
|------|----------------------|----------------------------------|
| 1    | `married`            | Married or living with a partner |
| 2    | `separated_divorced` | Separated or divorced            |
| 3    | `widowed`            | Widowed                          |
| 4    | `never_married`      | Single, never married            |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `pes_married` (pes) | 1, 2 = Married or living with a partner; 3, 4 = Separated or divorced; 5 = Widowed; 6 = Single, never married; -99 = No answer (item nonresponse) (NA) | fills the study | Asked after the election: the respondents of the campaign wave only are missing (not in this wave). | stable |
| qes2018 | `qstat` (post) | 1, 6 = Married or living with a partner; 2, 4 = Separated or divorced; 3 = Single, never married; 5 = Widowed; 98 = Refused (NA) | fills the study | Married or in a civil union (1) and common-law partner (6) are married. | stable |
| qes2014 | `Q68` (post) | 1, 6 = Married or living with a partner; 2, 4 = Separated or divorced; 3 = Single, never married; 5 = Widowed; 9 = Refused (NA) | fills the study | OD-R7: the question asks the official civil status and lists no common-law option. Civil union (6) is married. Code 6 holds 309 of the 1,501 substantive answers, far more than the share of formal civil unions in Quebec, so most respondents living common-law probably chose it; any who answered single instead are never married. | stable |
| qes2012 | `q109` (post) | 1, 6 = Married or living with a partner; 2, 4 = Separated or divorced; 3 = Single, never married; 5 = Widowed; 9 = Refused (NA) | fills the study | OD-R7: the question asks the official civil status, with no common-law option; civil partnership (6) is married. Partners who live together could answer Single or civil partnership: 360 respondents (24%) chose civil partnership, far more than the legal civil unions in Quebec, so common-law partners are split between married and never married. | stable |

### `employment`: Employment

The respondent’s employment status: working, unemployed, retired,
student or other.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: The main employment status in five groups; a
respondent who gave two statuses, such as retired and working, takes the
one that is not work, and at home, unable to work and other statuses are
other.

**Levels**

| Code | Name         | Label                               |
|------|--------------|-------------------------------------|
| 1    | `working`    | Working (employee or self-employed) |
| 2    | `unemployed` | Unemployed                          |
| 3    | `retired`    | Retired                             |
| 4    | `student`    | Student                             |
| 5    | `other`      | At home, unable to work or other    |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `pes_employed` (pes) | 1-3 = Working (employee or self-employed); 4, 11 = Retired; 5 = Unemployed; 6, 9 = Student; 7, 8, 10, 12 = At home, unable to work or other; -99 = No answer (item nonresponse) (NA) | fills the study | OD-R9: a respondent who gave two statuses (student and working, retired and working, at home and working) takes the one that is not work; two jobs is working; at home, disabled and other statuses are other. Asked after the election: the respondents of the campaign wave only are missing (not in this wave). | stable |
| qes2018 | `qoccup` (post) | 1, 2, 8 = Working (employee or self-employed); 3, 11 = Retired; 4 = Unemployed; 5, 9 = Student; 6, 7, 10, 96 = At home, unable to work or other; 99 = Refused (NA) | fills the study | OD-R9: a respondent who gave two statuses (student and working, retired and working, at home and working) takes the one that is not work; two jobs is working; at home, disabled and other statuses are other. | stable |
| qes2014 | `Q58` (post) | 1, 2, 8 = Working (employee or self-employed); 3, 11 = Retired; 4 = Unemployed; 5, 9 = Student; 6, 7, 10, 96 = At home, unable to work or other; 99 = Refused (NA) | fills the study | OD-R9: a respondent who gave two statuses (student and working, retired and working, at home and working) takes the one that is not work; two jobs is working; at home, disabled and other statuses are other. | stable |
| qes2012 | `occup` (post) | 1, 2, 8 = Working (employee or self-employed); 3, 11 = Retired; 4 = Unemployed; 5, 9 = Student; 6, 7, 10, 96 = At home, unable to work or other; 99 = Refused (NA) | fills the study | OD-R9: a respondent who gave two statuses (student and working, retired and working, at home and working) takes the one that is not work; two jobs is working; at home, disabled and other statuses are other. | stable |
| qes2018_panel | `d4` (pre) | 1-3 = Working (employee or self-employed); 4 = Unemployed; 5 = Student; 6 = Retired; 7, 8 = At home, unable to work or other; 9 = Don’t know or refused (one code) (NA) | fills the study | Full time, part time and self-employed are working; outside the labour market (at home) and other are other. The item was asked of the 850 web respondents only: the file codes all 400 telephone respondents (method 1-2) as 9 (don’t know), so 400 of the 406 missing values are not asked, not real don’t-know answers (6 web respondents chose 9). | stable |
| qes2007_panel | `occup` (any wave) | 1, 2 = Working (employee or self-employed); 3 = Unemployed; 4 = At home, unable to work or other; 5 = Retired; 6 = Student; 9 = Refused (NA) | fills the study | Full time and part time are working; at home full time is other. | stable |
| qes2007 | `q79` (post) | 1, 2, 8 = Working (employee or self-employed); 3, 11 = Retired; 4 = Unemployed; 5, 9 = Student; 6, 7, 10, 96 = At home, unable to work or other; 99 = Refused (NA) | fills the study | OD-R9: a respondent who gave two statuses (student and working, retired and working, at home and working) takes the one that is not work; two jobs is working; at home, disabled and other statuses are other. | stable |
| qes2008 | `q79` (post) | 1, 2 = Working (employee or self-employed); 3 = Retired; 4 = Unemployed; 5 = Student; 6, 7, 96 = At home, unable to work or other; 99 = Refused (NA) | fills the study | One status per respondent (the 2008 questionnaire has no two-status codes): self-employed and working for pay are working; at home, disabled and other (specify) are other. | stable |
| qes1998 | `occup` (pre) | 1-3 = Source category straddles target levels (NA) | fills the study | OD-R2: the pooled file has full time, part time and not working; not working joins the unemployed, the retired, students and those at home, so the study is left out. | stable |
| qes_crop_2007_2010 | `Occup` (each poll) | 1, 2 = Working (employee or self-employed); 3 = Unemployed; 4 = At home, unable to work or other; 5 = Retired; 6 = Student; 9 = Refused (NA) | fills the study | Full time and part time are working; at home full time is other. | stable |

### `union`: Union membership

Whether the respondent, or in some studies anyone in the household,
belongs to a union.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: Whether the respondent belongs to a union in
2022, but whether the respondent or anyone in the household does in 2014
and 2018 (in 2018, respondents who live with their parents were asked
about their family: parents, brothers or sisters). Only these three
studies ask it, and 2022 asks it after the election, so its
campaign-only respondents are missing; the column is kept whatever its
number of studies (essential), as decided on 2026-10-01.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `pes_union` (pes) | 1 = Yes; 2 = No | fills the study | The respondent belongs to a union (not the household). Asked after the election: the respondents of the campaign wave only are missing (not in this wave). | stable |
| qes2018 | `q65a` (post) | 1 = Yes; 2 = No; 98 = Don’t know (NA); 99 = Refused (NA); Inapplicable (routed out) (NA), System missing (NA) pass to q65b: 1 = Yes; 2 = No; 98 = Don’t know (NA); 99 = Refused (NA) | fills the study | Q65 has two versions, split by QPARENTS: those who do not live with their parents (q65a, 2,558 respondents) were asked about their household, those who do (q65b, 514) about their family (parents, brothers or sisters). The household version is read first, then the family version for those who were not asked it. The respondent or anyone in the household (or family) belongs to a union. | stable |
| qes2014 | `Q61` (post) | 1 = Yes; 2 = No; 9 = Refused (NA) | fills the study | The respondent or anyone in the household belongs to a union. | stable |

### `region`: Region

Where the respondent lives: Montreal CMA, Quebec CMA or the rest of
Quebec.

Base: `target:region_cma3` · `identity` · one value per respondent

**How it is relaxed**: The Montreal census metropolitan area, the Quebec
City census metropolitan area or the rest of Quebec, from each study’s
region or sub-region variable, with the boundaries each study used.

**Levels**

| Code | Name         | Label          |
|------|--------------|----------------|
| 1    | `mtl_cma`    | Montreal CMA   |
| 2    | `quebec_cma` | Quebec CMA     |
| 3    | `rest`       | Rest of Quebec |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2007_panel | `reg2` (any wave) | 1-3, 5-11, 17-19, 21, 22 = Rest of Quebec; 4, 20 = Quebec CMA; 12-16 = Montreal CMA | fills the study | Sub-regions: the five parts of the Montreal CMA (in Lanaudière, Laurentides, Laval, Montérégie and Montréal) are the Montreal CMA, and the parts of the Quebec CMA in Capitale-Nationale and Chaudière-Appalaches are the Quebec CMA. | stable |

### `region_admin`: Administrative region

The administrative region where the respondent lives.

Base: relaxed mappings only · `relaxed_only` · one value per respondent

**How it is relaxed**: The 17 administrative regions of Quebec, where a
study recorded them or sub-regions that fit within them; the sub-regions
of the 2007 study and the 2007 panel that split a region are joined.

**Levels**

| Code | Name                      | Label                         |
|------|---------------------------|-------------------------------|
| 1    | `bas_saint_laurent`       | Bas-Saint-Laurent             |
| 2    | `saguenay_lac_saint_jean` | Saguenay–Lac-Saint-Jean       |
| 3    | `capitale_nationale`      | Capitale-Nationale            |
| 4    | `mauricie`                | Mauricie                      |
| 5    | `estrie`                  | Estrie                        |
| 6    | `montreal`                | Montréal                      |
| 7    | `outaouais`               | Outaouais                     |
| 8    | `abitibi_temiscamingue`   | Abitibi-Témiscamingue         |
| 9    | `cote_nord`               | Côte-Nord                     |
| 10   | `nord_du_quebec`          | Nord-du-Québec                |
| 11   | `gaspesie_iles`           | Gaspésie–Îles-de-la-Madeleine |
| 12   | `chaudiere_appalaches`    | Chaudière-Appalaches          |
| 13   | `laval`                   | Laval                         |
| 14   | `lanaudiere`              | Lanaudière                    |
| 15   | `laurentides`             | Laurentides                   |
| 16   | `monteregie`              | Montérégie                    |
| 17   | `centre_du_quebec`        | Centre-du-Québec              |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2018 | `q0qc` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 10 = Nord-du-Québec; 11 = Gaspésie–Îles-de-la-Madeleine; 12 = Chaudière-Appalaches; 13 = Laval; 14 = Lanaudière; 15 = Laurentides; 16 = Montérégie; 17 = Centre-du-Québec | fills the study |  | stable |
| qes2014 | `QREGION` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 10 = Nord-du-Québec; 11 = Gaspésie–Îles-de-la-Madeleine; 12 = Chaudière-Appalaches; 13 = Laval; 14 = Lanaudière; 15 = Laurentides; 16 = Montérégie; 17 = Centre-du-Québec | fills the study |  | stable |
| qes2012 | `q0qc` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 10 = Nord-du-Québec; 11 = Gaspésie–Îles-de-la-Madeleine; 12 = Chaudière-Appalaches; 13 = Laval; 14 = Lanaudière; 15 = Laurentides; 16 = Montérégie; 17 = Centre-du-Québec | fills the study |  | stable |
| qes2007_panel | `reg2` (any wave) | 1 = Abitibi-Témiscamingue; 2 = Bas-Saint-Laurent; 3, 4 = Chaudière-Appalaches; 5 = Côte-Nord; 6 = Estrie; 7 = Gaspésie–Îles-de-la-Madeleine; 8, 12 = Lanaudière; 9, 13 = Laurentides; 10 = Mauricie; 11, 15 = Montérégie; 14 = Laval; 16 = Montréal; 17 = Nord-du-Québec; 18 = Outaouais; 19, 20 = Capitale-Nationale; 21 = Saguenay–Lac-Saint-Jean; 22 = Centre-du-Québec | fills the study | Sub-regions that split a region (its part in a CMA and the rest) are joined. | stable |
| qes2007 | `nomx` (post) | 1 = Bas-Saint-Laurent; 2 = Saguenay–Lac-Saint-Jean; 3, 33 = Capitale-Nationale; 4 = Mauricie; 5 = Estrie; 6 = Montréal; 7 = Outaouais; 8 = Abitibi-Témiscamingue; 9 = Côte-Nord; 11 = Gaspésie–Îles-de-la-Madeleine; 12, 32 = Chaudière-Appalaches; 13 = Laval; 14, 24 = Lanaudière; 15, 25 = Laurentides; 16, 26 = Montérégie; 17 = Centre-du-Québec | fills the study | Sub-regions that split a region (its part in a CMA and the rest) are joined; Nord-du-Québec has no code. | stable |

### `born_canada`: Born in Canada

Whether the respondent was born in Canada.

Base: `target:born_canada` · `identity` · one value per respondent

**How it is relaxed**: Born in Canada or not, as each study asked it:
from the birthplace (Quebec, elsewhere in Canada or abroad), or asked
directly in 2022.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

None: every study’s values come from the base.

### `born_quebec`: Born in Quebec

Whether the respondent was born in Quebec.

Base: `target:birthplace3` ·
`recode:quebec=yes,other_canada=no,abroad=no` · one value per respondent

**How it is relaxed**: Born in Quebec or not, from the birthplace
question (Quebec, elsewhere in Canada or abroad); 2022 asked only
whether the respondent was born in Canada, so it is missing there.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

None: every study’s values come from the base.

### `vote_choice`: Provincial vote choice

The party of the respondent’s vote in the Quebec general election of the
study.

Base: `pooled:vote_choice` · `identity` · on the wave that asked it

**How it is relaxed**: The reported vote where a study asked it, else
the vote intention with the undecided pushed toward the party they lean
to, else the first intention question (the pooled vote_choice);
vote_type says which, and the parties are those each study offered.

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

**Relaxed mappings**

None: every study’s values come from the base.

### `vote_type`: Question of the vote choice

The question of vote_choice on the row.

Base: `pooled:vote_choice__type` · `identity` · on the wave that asked
it

**How it is relaxed**: Which question vote_choice comes from on each
row: the reported vote, the pushed intention or the first intention
question.

**Levels**

| Code | Name             | Label                              |
|------|------------------|------------------------------------|
| 1    | `recall`         | Reported vote (after the election) |
| 2    | `intention_push` | Vote intention (undecided pushed)  |
| 3    | `intention`      | Vote intention (first question)    |

**Relaxed mappings**

None: every study’s values come from the base.

### `turnout`: Turnout (reported)

Whether the respondent voted in the Quebec general election of the
study.

Base: `pooled:turnout` · `identity` · on the wave that asked it

**How it is relaxed**: Whether the respondent says they voted in the
Quebec general election of the study, asked after it; the wordings and
answer options differ, some offering several ways of not voting.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

None: every study’s values come from the base.

### `vote_prev`: Vote at the previous provincial election

The party the respondent voted for at the previous Quebec general
election.

Base: `target:vote_prov_prev` · `identity` · on the wave that asked it

**How it is relaxed**: The party of the reported vote at the Quebec
general election before the study; those who did not vote are missing,
the sources name the election recalled, and the 1998 question names only
the PLQ and the PQ.

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

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2007_panel | `voteprec` (pre) | 1 = ADQ; 2 = PLQ; 3 = PQ; 4 = Other party; 6 = Did not vote (NA); 8 = Don’t know (NA); 9 = Refused (NA) | fills the study | The election of April 2003 (QC2003); asked in the second pre-election poll only, so the other respondents are system missing. | stable |
| qes1998 | `vote94` (pre) | 1 = PLQ; 2 = PQ; 3 = Did not vote (NA); 4 = Don’t know or refused (one code) (NA); 9 = Source category straddles target levels (NA) | fills the study | The election of 1994 (QC1994). The pooled file names only the PLQ and the PQ; the unlabelled code 9 holds the other parties (the ADQ among them) and unknown answers, so 1994 ADQ voters are missing. | stable |
| qes_crop_2007_2010 | `QP4` (each poll) | 1 = ADQ; 2 = PLQ; 3 = PQ; 4 = QS; 5 = PVQ; 6 = Other party; 7 = Did not vote (NA); 9 = Don’t know or refused (one code) (NA) | fills the study | The last Quebec election before each poll: QC2007 for the polls of June 2007 to November 2008, QC2008 from January 2009. The original question (QP4); code 7 joins not voted and spoiled. | stable |

### `pid`: Provincial party identification

The Quebec party the respondent identifies with.

Base: `target:pid_prov` · `identity` · on the wave that asked it

**How it is relaxed**: The Quebec party the respondent identifies with,
or none, as each study asked it; the parties offered differ from study
to study.

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

**Relaxed mappings**

None: every study’s values come from the base.

### `lr`: Left-right self-placement (0-10)

The respondent’s position on a left-right scale.

Base: `target:lr_self` · `identity` · on the wave that asked it

**How it is relaxed**: Self-placement on a left-right scale from 0
(left) to 10 (right); a scale of another length would be rescaled to
0-10, and every study that asked one used 0 to 10.

**Valid range**: 0-10

**Relaxed mappings**

None: every study’s values come from the base.

### `interest`: Interest in politics

How interested the respondent is in politics.

Base: `pooled:pol_interest` · `bands:0.35,0.75:low,medium,high` · on the
wave that asked it

**How it is relaxed**: Interest on 0 to 1 (interest_01) in three bands,
low below 0.35 and high from 0.75; the 4-point and 0-10 questions do not
line up, and the 2008 study and the 2007 panel asked interest in the
election or the campaign, not in politics.

**Levels**

| Code | Name     | Label  |
|------|----------|--------|
| 1    | `low`    | Low    |
| 2    | `medium` | Medium |
| 3    | `high`   | High   |

**Relaxed mappings**

None: every study’s values come from the base.

### `interest_01`: Interest in politics (0-1)

How interested the respondent is in politics, on 0 to 1.

Base: `pooled:pol_interest` · `identity` · on the wave that asked it

**How it is relaxed**: Interest on 0 to 1 (the pooled pol_interest):
four-point answers scored 1, 0.7, 0.3 and 0, and 0-10 answers divided by
10.

**Valid range**: 0-1

**Relaxed mappings**

None: every study’s values come from the base.

### `sovereignty`: Sovereignty referendum vote

How the respondent would vote in a referendum on Quebec sovereignty.

Base: `pooled:sov_support` ·
`recode:yes=yes,no=no,would_not_vote=NA:not_mappable` · on the wave that
asked it

**How it is relaxed**: Yes or no in a referendum on Quebec sovereignty,
whatever the question (an independent country, a sovereign country, the
1995 question or being favourable to independence); sovereignty_type
says which, and would not vote is missing.

**Levels**

| Code | Name  | Label |
|------|-------|-------|
| 1    | `yes` | Yes   |
| 2    | `no`  | No    |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes_crop_2007_2010 | `intvoterefa` (each poll) | 1 = Yes; 2 = No; 3 = Source category straddles target levels (NA); 8 = Don’t know (NA); 9 = Refused (NA); Don’t know (NA) pass to intvoterefb: 1 = Yes; 2 = No; 3 = Source category straddles target levels (NA); 8 = Don’t know (NA); 9 = Refused (NA) | fills the study | OD-R8: the referendum question of the CROP polls. Its label is cut off in the file and the deposited codebook gives no more. CROP’s reports give the wording for 19 of the 24 polls: whether Quebec should become a sovereign country (« devienne un pays souverain »). It is not documented for the other five; in April 2009 CROP asked both a sovereign-country and a sovereignty-partnership question, and for May 2009 the press does not say which one gave the published result. For those who did not know, the push (intvoterefb) is used. The push was also asked of those who would not vote or refused, whose first answer is kept. Would not vote is missing. | stable |

### `sovereignty_type`: Question of the sovereignty vote

The question of sovereignty on the row.

Base: `pooled:sov_support__type` ·
`recode:independence=independence,sovereign_country=sovereign_country,partnership_1995_push=partnership_1995,partnership_1995=partnership_1995,favour=favour`
· on the wave that asked it

**How it is relaxed**: Which question sovereignty comes from on each
row: an independent country, a sovereign country, the 1995 question,
being favourable to independence, or the CROP polls’ question, whose
full wording was not deposited.

**Levels**

| Code | Name                | Label                                            |
|------|---------------------|--------------------------------------------------|
| 1    | `independence`      | Independent country                              |
| 2    | `sovereign_country` | Sovereign country                                |
| 3    | `partnership_1995`  | 1995 question (sovereignty-partnership)          |
| 4    | `favour`            | Favourable to independence                       |
| 5    | `crop_undocumented` | CROP referendum question (wording not deposited) |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes_crop_2007_2010 | `intvoterefa` (each poll) | every respondent = CROP referendum question (wording not deposited) | fills the study | OD-R8: every CROP poll respondent; sovereignty says whether they answered. | stable |

### `satis_democracy`: Satisfaction with democracy in Quebec

How satisfied the respondent is with the way democracy works in Quebec.

Base: `target:satis_demo_qc` · `identity` · on the wave that asked it

**How it is relaxed**: Satisfaction with the way democracy works in
Quebec, on four points, as each study asked it.

**Levels**

| Code | Name         | Label                |
|------|--------------|----------------------|
| 1    | `very`       | Very satisfied       |
| 2    | `fairly`     | Fairly satisfied     |
| 3    | `not_very`   | Not very satisfied   |
| 4    | `not_at_all` | Not at all satisfied |

**Relaxed mappings**

None: every study’s values come from the base.

### `gov_satisfaction`: Satisfaction with the Quebec government

How satisfied the respondent is with the Quebec government.

Base: `target:gov_satisfaction` · `identity` · on the wave that asked it

**How it is relaxed**: Satisfaction with the Quebec government of the
day, on four points; the 1998 question asks about the Bouchard
government, in its own words.

**Levels**

| Code | Name         | Label                |
|------|--------------|----------------------|
| 1    | `very`       | Very satisfied       |
| 2    | `fairly`     | Fairly satisfied     |
| 3    | `not_very`   | Not very satisfied   |
| 4    | `not_at_all` | Not at all satisfied |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes1998 | `satisf` (pre) | 1 = Very satisfied; 2 = Fairly satisfied; 3 = Not very satisfied; 4 = Not at all satisfied; 9 = Don’t know or refused (one code) (NA) | fills the study | Satisfaction with the Bouchard (PQ) government, in the pooled file’s words: very satisfied, rather satisfied, rather dissatisfied, very dissatisfied; code 9 is unlabelled and read as don’t know or refused. | stable |

### `econ_retro`: Quebec’s economy over the past year

The respondent’s view of Quebec’s economy over the past year.

Base: `target:econ_retro_qc` · `identity` · on the wave that asked it

**How it is relaxed**: Whether Quebec’s economy got better, stayed about
the same or got worse over the past year, as each study asked it.

**Levels**

| Code | Name     | Label          |
|------|----------|----------------|
| 1    | `better` | Better         |
| 2    | `same`   | About the same |
| 3    | `worse`  | Worse          |

**Relaxed mappings**

None: every study’s values come from the base.

### `econ_self`: Personal financial situation over the past year

The respondent’s view of their own financial situation over the past
year.

Base: relaxed mappings only · `relaxed_only` · on the wave that asked it

**How it is relaxed**: Whether the respondent’s own financial situation
got better, stayed about the same or got worse over the past year. Only
qes2022 asks it, during the campaign; the column is kept for analyses
within that study (essential), as cesR’s econ_self, although it has
fewer than three studies. The 2012 and 2014 question on one’s own
finances if Quebec became independent is another question and is not
used.

**Levels**

| Code | Name     | Label          |
|------|----------|----------------|
| 1    | `better` | Better         |
| 2    | `same`   | About the same |
| 3    | `worse`  | Worse          |

**Relaxed mappings**

| Study | Source | Recode | Use | Notes | status |
|----|----|----|----|----|----|
| qes2022 | `cps_ownfin` (cps) | 1 = Better; 2 = Worse; 3 = About the same | fills the study | Asked during the campaign of every respondent; the file has no missing value. Not the 2012 and 2014 question on one’s own finances if Quebec became independent (q85, Q47), which asks about another situation. | stable |

### `identity`: Québécois and Canadian identity

How the respondent sees themselves, as Québécois, Canadian or both.

Base: `target:identity_qc_ca` · `identity` · on the wave that asked it

**How it is relaxed**: Québécois only, Québécois first, both equally,
Canadian first or Canadian only, from one question or from the two
orders of a split ballot.

**Levels**

| Code | Name       | Label                          |
|------|------------|--------------------------------|
| 1    | `qc_only`  | Québécois only                 |
| 2    | `qc_first` | Québécois first, then Canadian |
| 3    | `equal`    | Equally Québécois and Canadian |
| 4    | `ca_first` | Canadian first, then Québécois |
| 5    | `ca_only`  | Canadian only                  |
| 90   | `other`    | Other                          |

**Relaxed mappings**

None: every study’s values come from the base.

### `attach_quebec`: Attachment to Quebec

How attached the respondent feels to Quebec.

Base: `target:attach_qc` · `identity` · on the wave that asked it

**How it is relaxed**: Attachment to Quebec on four points, as each
study asked it.

**Levels**

| Code | Name         | Label               |
|------|--------------|---------------------|
| 1    | `very`       | Very attached       |
| 2    | `fairly`     | Fairly attached     |
| 3    | `not_very`   | Not very attached   |
| 4    | `not_at_all` | Not at all attached |

**Relaxed mappings**

None: every study’s values come from the base.

### `attach_canada`: Attachment to Canada

How attached the respondent feels to Canada.

Base: `target:attach_ca` · `identity` · on the wave that asked it

**How it is relaxed**: Attachment to Canada on four points, as each
study asked it.

**Levels**

| Code | Name         | Label               |
|------|--------------|---------------------|
| 1    | `very`       | Very attached       |
| 2    | `fairly`     | Fairly attached     |
| 3    | `not_very`   | Not very attached   |
| 4    | `not_at_all` | Not at all attached |

**Relaxed mappings**

None: every study’s values come from the base.
