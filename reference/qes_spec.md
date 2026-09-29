# The harmonization spec (experimental)

`qes_spec()` is the one entry point to the harmonization specification:
the reviewed rules that say, study by study, which question feeds each
harmonized variable ("target") of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
how its codes map to the target's levels, why each missing value is
missing and how comparable each study's question is to the target's
anchor question.

## Usage

``` r
qes_spec(
  view = c("targets", "crosswalk", "spec"),
  targets = NULL,
  studies = NULL,
  level = c("row", "code"),
  format = c("qesR", "retroharmonize"),
  spec = NULL,
  validate = c("error", "report", "none"),
  data = NULL,
  lang = c("en", "fr")
)
```

## Arguments

- view:

  `"targets"`, `"crosswalk"` or `"spec"`.

- targets:

  Target, family or set names (views `"targets"` and `"crosswalk"`; the
  set `"decon"` holds the targets of the columns of
  [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md));
  `NULL` (default) is every target.

- studies:

  Study codes (views `"targets"` and `"crosswalk"`); `NULL` (default) is
  every study the spec covers.

- level:

  `"row"` (default) or `"code"` (view `"crosswalk"` only).

- format:

  `"qesR"` (default) or `"retroharmonize"` (view `"crosswalk"` only;
  always one row per code).

- spec:

  `NULL` (the spec shipped with qesR), the path of a spec directory (for
  example one copied from a release), or a `qes_spec` object.

- validate:

  What a problem found by the checks does: `"error"` (default) raises
  `qesR_error_spec`, `"report"` returns the result with the problems in
  `attr(, "check")` (view `"spec"`), `"none"` skips the checks.

- data:

  A named list of data frames, one per study, named by study code, as
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  returns them (view `"spec"` only): the data checks then also run on
  them, and their problems are added to `attr(, "check")`.

- lang:

  Language of returned text (labels, definitions, grade reasons, notes):
  `"en"` (default) or `"fr"`.

## Value

By `view`:

- `"targets"`: a data frame with columns `target`, `family`, `type`,
  `target_timing`, `label`, `definition`, `levels` (`code=label`),
  `status`, `added_in`, then one column per study.

- `"crosswalk"`: a data frame of class `qes_crosswalk`. With
  `level = "row"`: `study`, `wave`, `target`, `source_var`, `rule`,
  `map_id`, `args`, `na_codes`, `gate`, `primary`, `grade`,
  `grade_reason`, `instrument`, `election_ref`, `mode`, `dk_offered`,
  `levels_offered`, `levels_not_offered`, `wording`, `wording_ref`,
  `weight_var`, `status`, `reviewed_by`, `reviewed_on`, `review_note`
  (what the review corrected, and why a reviewed row is held in review),
  `evidence`, `notes`. With `level = "code"`: `study`, `wave`, `target`,
  `variable`, `source_code`, `source_label`, `origin` (`map`,
  `na_codes`, `range` or `gate`), `target_code`, `target_level`,
  `target_label`, `na_reason`, `note`. A `wave` of `"*"` marks a row
  that applies to every poll of pooled polls (the CROP polls of
  2007-2010, one wave per poll), or, in a panel, a question that does
  not change over the study (gender, education), asked in whichever wave
  the respondent took part (the 2007 panel).

- `"spec"`: an object of class `qes_spec`: a list with the spec
  `version`, its content `hash`, `custom` (`TRUE` when it is not the
  shipped spec) and `tables` (targets, levels, crosswalk, valuemaps,
  waves, weights, changes, gates, expected, hashes, legacy: the renderer
  of
  [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  and
  [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md));
  `attr(, "check")` holds the problems table (`rule`, `severity`,
  `table`, `row`, `key`, `detail`).

## Details

- `view = "targets"` (default): one row per target, with its definition
  and levels, and one column per study giving the best comparability
  grade of that study's question (`NA`: no question for the target).

- `view = "crosswalk"`: one row per crosswalk row (`level = "row"`): the
  source variable, grade and its reason, instrument, offered levels and
  the levels not offered (structural zeros), filter question, wording
  (or the document that gives it), recommended weight and review status.
  With `level = "code"`, one row per source code, with the level or NA
  reason it maps to; `format = "retroharmonize"` gives that table under
  the column names of the retroharmonize package's crosswalk tables.
  Printing the view of a single target shows its section of the
  generated reference.

- `view = "spec"`: the spec itself, as an object of class `qes_spec`
  (tables, version and content hash), checked by the validator and the
  data checks; this is what `spec =` of
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
  accepts.

The spec is loaded and checked once per session. The shipped spec passes
every check; a spec directory of your own is checked the same way.

## Experimental

The spec is experimental: targets, grades and mappings are reviewed
study by study and may change. Its version and content hash are recorded
in every result of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md).

## En français

`qes_spec()` (expérimental) montre la spécification d'harmonisation : la
vue `"targets"` donne une ligne par cible et, pour chaque étude, son
niveau de comparabilité ; la vue `"crosswalk"` donne la question source,
la raison du niveau, les niveaux offerts et non offerts, la question
filtre et le libellé (une vague `"*"` désigne une ligne qui s'applique à
chaque sondage des sondages CROP regroupés) ; la vue `"spec"` renvoie la
spécification vérifiée, dont la table `tables$legacy`, qui rend les
colonnes de
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
et de
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md)
à partir des cibles. L'ensemble de cibles `"decon"` réunit les cibles
des colonnes de
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md).
`lang = "fr"` donne les étiquettes, définitions et raisons en français.
[`vignette("fr-reference-harmonisation", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md)
en est la référence complète.

## Targets in the shipped spec (version 4.2.0)

Generated from the spec by roxygen; `qes_spec()` gives the same list
with each study's grade.

- `vote_prov_recall`:

  Provincial vote (recall) (family `vote_prov`; sets `core`, `vote`,
  `decon`; categorical). Studies: qes2012, qes2014, qes2018, qes2022,
  qes2018_panel, qes2007_panel, qes2007, qes2008, qes2012_panel,
  qes1998.

- `vote_prov_intent`:

  Provincial vote intention (family `vote_prov`; sets `core`, `vote`,
  `decon`; categorical). Studies: qes2022, qes2018_panel, qes2007_panel,
  qes2012_panel, qes_crop_2007_2010.

- `vote_prov_intent_push`:

  Provincial vote intention, undecided pushed (family `vote_prov`; sets
  `core`, `vote`; categorical). Studies: qes2018_panel, qes2007_panel,
  qes2012_panel, qes_crop_2007_2010, qes1998.

- `turnout_prov_recall`:

  Voted in the provincial election (recall) (family `turnout_prov`; sets
  `core`, `vote`, `decon`; categorical). Studies: qes2012, qes2014,
  qes2018, qes2022, qes2018_panel, qes2007, qes2008, qes2012_panel,
  qes1998, qes2007_panel.

- `sov_indep`:

  Referendum vote: independent country (family `sovereignty`; sets
  `core`; categorical). Studies: qes2012, qes2014, qes2018, qes2022.

- `sov_sovereign_country`:

  Referendum vote: sovereign country (family `sovereignty`; sets `core`;
  categorical). Studies: qes2012_panel.

- `sov_favour`:

  Favour Quebec independence (4 points) (family `sovereignty`; sets
  `core`; ordinal). Studies: qes2018_panel.

- `lr_self`:

  Left-right self-placement (0-10) (family `left_right`; sets `core`,
  `decon`; numeric). Studies: qes2012, qes2014, qes2018, qes2022,
  qes2018_panel.

- `pid_prov`:

  Provincial party identification (family `party_id`; sets `core`,
  `decon`; categorical). Studies: qes2012, qes2014, qes2018, qes2022,
  qes2007, qes2008.

- `interest_4pt`:

  Interest in politics (4 points) (family `interest`; sets `core`,
  `decon`; ordinal). Studies: qes2012, qes2014, qes2018.

- `birth_year`:

  Year of birth (family `birth`; sets `core`, `decon`; numeric).
  Studies: qes2022, qes2012, qes2014, qes2018, qes2007, qes2008.

- `birth_month`:

  Month of birth (family `birth`; no set; numeric). Studies: qes2018.

- `age`:

  Age in years (family `age_years`; sets `core`, `decon`; numeric).
  Studies: qes2018, qes2022.

- `age_group3`:

  Age group (3 bands) (family `age_bands`; sets `core`; ordinal).
  Studies: qes2018_panel, qes2012_panel, qes2007_panel, qes2008,
  qes_crop_2007_2010, qes1998.

- `citizen`:

  Canadian citizen (family `citizenship`; sets `core`, `decon`;
  categorical). Studies: qes2022.

- `survey_mode`:

  Interview mode (family `interview_mode`; no set; categorical).
  Studies: qes2018_panel, qes2007.

- `sov_partnership_1995`:

  Referendum vote: the 1995 sovereignty-partnership question (family
  `sovereignty`; sets `core`; categorical). Studies: qes2007, qes2008,
  qes1998, qes2007_panel.

- `turnout_prov_likely`:

  Likelihood of voting in the provincial election (family
  `turnout_prov`; sets `vote`, `decon`; ordinal). Studies: qes2022.

- `vote_prov_intent_other`:

  Provincial vote intention: another party (text) (family `vote_prov`;
  sets `decon`; string). Studies: qes2022.

- `pid_fed`:

  Federal party identification (family `party_id`; sets `core`, `decon`;
  categorical). Studies: qes2022.

- `interest_0_10`:

  Interest in politics (0-10) (family `interest`; sets `core`, `decon`;
  numeric). Studies: qes2007, qes2022.

- `interest_election_0_10`:

  Interest in the provincial election (0-10) (family `interest`; no set;
  numeric). Studies: qes2007, qes2008.

- `interest_campaign_4pt`:

  Interest in the election campaign (4 points) (family `interest`; no
  set; ordinal). Studies: qes2007_panel.

- `age_group6`:

  Age group (6 bands) (family `age_bands`; sets `core`; ordinal).
  Studies: qes_crop_2007_2010, qes1998, qes2007_panel, qes2012_panel,
  qes2008.

- `gender`:

  Gender (family `sex_gender`; sets `core`, `decon`; categorical).
  Studies: qes2012, qes2014, qes2018, qes2022, qes2007, qes2008,
  qes2007_panel, qes2012_panel, qes2018_panel, qes_crop_2007_2010,
  qes1998.

- `education4`:

  Education (4 groups) (family `education`; sets `core`, `decon`;
  ordinal). Studies: qes2014, qes2012, qes2018, qes2022, qes2007,
  qes2008, qes2018_panel, qes2007_panel, qes_crop_2007_2010.

- `lang_mother`:

  Mother tongue (family `language`; sets `core`; categorical). Studies:
  qes2012, qes2014, qes2007, qes2008, qes2018, qes2007_panel,
  qes2012_panel, qes2018_panel, qes_crop_2007_2010.

- `born_canada`:

  Born in Canada (family `birthplace`; sets `core`, `decon`;
  categorical). Studies: qes2022, qes2012, qes2014, qes2018.

- `income_native`:

  Household income (each study's own brackets) (family `income`; sets
  `decon`; string). Studies: qes2012, qes2014, qes2007, qes2008,
  qes2007_panel, qes2018_panel, qes_crop_2007_2010, qes2022.

- `religion`:

  Religion (each study's own categories) (family `faith`; sets `decon`;
  string). Studies: qes2012, qes2014, qes2022.

## See also

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
and
[`vignette("harmonization-reference", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md),
the reference generated from the spec.

Other harmonization:
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md),
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)

## Examples

``` r
# which studies have which target, and how comparable they are
qes_spec()
#>                    target         family        type target_timing
#> 1        vote_prov_recall      vote_prov categorical          post
#> 2        vote_prov_intent      vote_prov categorical           pre
#> 3   vote_prov_intent_push      vote_prov categorical           pre
#> 4     turnout_prov_recall   turnout_prov categorical          post
#> 5               sov_indep    sovereignty categorical           any
#> 6   sov_sovereign_country    sovereignty categorical           any
#> 7              sov_favour    sovereignty     ordinal           any
#> 8                 lr_self     left_right     numeric           any
#> 9                pid_prov       party_id categorical           any
#> 10           interest_4pt       interest     ordinal           any
#> 11             birth_year          birth     numeric        static
#> 12            birth_month          birth     numeric        static
#> 13                    age      age_years     numeric           any
#> 14             age_group3      age_bands     ordinal           any
#> 15                citizen    citizenship categorical           any
#> 16            survey_mode interview_mode categorical           any
#> 17   sov_partnership_1995    sovereignty categorical           any
#> 18    turnout_prov_likely   turnout_prov     ordinal           pre
#> 19 vote_prov_intent_other      vote_prov      string           pre
#> 20                pid_fed       party_id categorical           any
#> 21          interest_0_10       interest     numeric           any
#> 22 interest_election_0_10       interest     numeric          post
#> 23  interest_campaign_4pt       interest     ordinal           pre
#> 24             age_group6      age_bands     ordinal           any
#> 25                 gender     sex_gender categorical        static
#> 26             education4      education     ordinal        static
#> 27            lang_mother       language categorical        static
#> 28            born_canada     birthplace categorical        static
#> 29          income_native         income      string        static
#> 30               religion          faith      string        static
#>                                                         label
#> 1                                    Provincial vote (recall)
#> 2                                   Provincial vote intention
#> 3                 Provincial vote intention, undecided pushed
#> 4                   Voted in the provincial election (recall)
#> 5                        Referendum vote: independent country
#> 6                          Referendum vote: sovereign country
#> 7                       Favour Quebec independence (4 points)
#> 8                            Left-right self-placement (0-10)
#> 9                             Provincial party identification
#> 10                            Interest in politics (4 points)
#> 11                                              Year of birth
#> 12                                             Month of birth
#> 13                                               Age in years
#> 14                                        Age group (3 bands)
#> 15                                           Canadian citizen
#> 16                                             Interview mode
#> 17 Referendum vote: the 1995 sovereignty-partnership question
#> 18            Likelihood of voting in the provincial election
#> 19            Provincial vote intention: another party (text)
#> 20                               Federal party identification
#> 21                                Interest in politics (0-10)
#> 22                 Interest in the provincial election (0-10)
#> 23               Interest in the election campaign (4 points)
#> 24                                        Age group (6 bands)
#> 25                                                     Gender
#> 26                                       Education (4 groups)
#> 27                                              Mother tongue
#> 28                                             Born in Canada
#> 29               Household income (each study's own brackets)
#> 30                     Religion (each study's own categories)
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                definition
#> 1                                                                                                                                                                                                                                                                                                                                                                        Party the respondent reports having voted for in the Quebec general election of the study, asked after that election. Nonvoters, spoiled ballots and respondents not eligible or not registered are missing values with a reason, never a party.
#> 2                                                                                                                                                                                                                                                                                                                                                            Party the respondent intends to vote for in the coming Quebec general election, asked before it, at the first question, without the push question for the undecided. Would not vote, none or would spoil is an answer (level no_party), not a missing value.
#> 3                                                                                                                                                                                                                                                                                                                                                                                            Vote intention in which respondents undecided at the first question were asked which party they lean toward (a push question), combined into one variable. A different stimulus from vote_prov_intent: never pooled with it.
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                        Whether the respondent reports having voted in the Quebec general election of the study, asked after that election. Respondents not eligible or not registered are missing values with a reason.
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   How the respondent would vote in a referendum on whether Quebec should become an independent country.
#> 6                                                                                                                                                                                                                                                                                                                                                         How the respondent would vote in a referendum on whether Quebec should become a sovereign country. A different stimulus from sov_indep (sovereign, not independent): never pooled with it. The push question asked of the undecided is not part of this target.
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       How favourable or opposed the respondent is to Quebec independence, on a four-point scale. Not a referendum vote.
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Where the respondent places their own views on a scale from 0 (left) to 10 (right).
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        The provincial party the respondent usually thinks of themselves as close to; none is an answer.
#> 10                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      How interested the respondent is in politics, on a four-point verbal scale. Never rescaled to or pooled with 0-10 interest items.
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Year the respondent was born.
#> 12                                                                                                                                                                                                                                                                                                                                                                                                               Month the respondent was born (1 = January). Asked with the year of birth in some studies; with birth_year, it tells whether a respondent born 18 years before the election year was 18 on election day.
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                            The respondent's age in years at the interview, as asked. Never computed from the year of birth, which gives the age only to within a year.
#> 14                                                                                                                                                                                                                                                                                                                                                                                                                  The respondent's age group at the interview in three bands: 18-34, 35-54, 55 and over. Built only from a question with these bands or with bands that collapse into them exactly, never from a guess.
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Whether the respondent is a Canadian citizen, where the study asked. Only citizens may vote in Quebec elections.
#> 16                                                                                                                                                                                                                                                                                                                                                                              How the respondent was interviewed: web, telephone or mixed. A leading column of every result of qes_harmonize(), filled from the mode of each wave (waves.csv); crosswalk rows exist only for the waves whose mode varies by respondent.
#> 17                                                                                                                                                                                                                                                                                      How the respondent would vote if a referendum were held today on the question of the 1995 referendum, sovereignty with an offer of partnership to the rest of Canada. A different stimulus from sov_indep and sov_sovereign_country: never pooled with them. The push question asked of the undecided is not part of this target.
#> 18                                                                                                                                                                                                                                                                                                                                                                  How likely the respondent says they are to vote in the coming Quebec general election, asked before it. Having already voted (in advance polls) is an answer. A different stimulus from turnout_prov_recall (reported turnout): never pooled with it.
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                       The party the respondent typed after choosing another party at the vote-intention question (vote_prov_intent, level other), as typed. Open text, not harmonized.
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          The federal party the respondent usually thinks of themselves as close to; none is an answer.
#> 21                                                                                                                                                                                                                                                                                                                                                                                                                                               How interested the respondent is in politics in general, from 0 (no interest) to 10 (a great deal). Never pooled with or rescaled to the four-point item (interest_4pt).
#> 22                                                                                                                                                                                                                                                                                                                                                                                                            How interested the respondent was in the Quebec general election that has just taken place, from 0 (no interest) to 10 (a great deal), asked after it. Interest in an election, not in politics in general.
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                            How interested the respondent is in the current Quebec election campaign, on a four-point verbal scale, asked during it. Interest in a campaign, not in politics in general (interest_4pt).
#> 24                                                                                                                                                                                                                                                                                                                                                                                               The respondent's age group at the interview in six bands: 18-24, 25-34, 35-44, 45-54, 55-64, 65 and over. Built only from a question with these bands or with bands that collapse into them exactly, never from a guess.
#> 25                                                                                                                                                                                                                                                                                                                                                                                             The respondent's gender, as asked or, in some telephone surveys, as recorded by the interviewer. Most studies offered only man and woman (a question on sex, in some); qes2022 also offered non-binary and another gender.
#> 26 The respondent's highest level of education in four groups: primary or less, secondary, college (CEGEP or technical), university (completed or not). From the highest level completed where the study asks for it, from years of schooling otherwise (graded approximate). A vocational or trade credential (the DEP) has no group of its own and is not placed alike in every study: secondary where the question lists the DEP as a secondary diploma (qes2018), college where it is a trade certificate or technical training (qes2018_panel and, probably, the studies with no DEP option); see the grade reasons.
#> 27                                                                                                                                                                                                                                                                                                                                                                       The first language the respondent learned at home in childhood and still understands: French, English or another language. A respondent who reports two first languages is a missing value (reason not_mappable), never assigned to one of them.
#> 28                                                                                                                                                                                                                                                                                                                                                                                                                                               Whether the respondent was born in Canada. From a question on birthplace (Quebec, elsewhere in Canada, outside Canada) where the study asks that one, collapsed exactly.
#> 29                                                                                                                                                                                                                                                                                                                            The respondent's household income before taxes as each study recorded it: the text of the study's own bracket (or the amount, where the study asked for one). Brackets differ between studies, so the values are not comparable across studies; don't know and refusals are missing values.
#> 30                                                                                                                                                                                                                The respondent's religion as each study recorded it: the text of the study's own category. Categories differ between studies. Where the question was asked only of respondents who belong to a religion, the others are missing values: reason inapplicable for those who said they belong to none, refused for those who would not answer the filter question, which is the gate of the crosswalk row.
#>                                                                                                         levels
#> 1                                          1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party
#> 2  1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party; 95=Would not vote / none / would spoil
#> 3  1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party; 95=Would not vote / none / would spoil
#> 4                                                                                                  1=Yes; 2=No
#> 5                                                                 1=Yes; 2=No; 95=Would not vote / would spoil
#> 6                                                                 1=Yes; 2=No; 95=Would not vote / would spoil
#> 7                                 1=Very favourable; 2=Somewhat favourable; 3=Somewhat opposed; 4=Very opposed
#> 8                                                                                                         <NA>
#> 9                        1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party; 97=None of these
#> 10                         1=Very interested; 2=Quite interested; 3=Hardly interested; 4=Not at all interested
#> 11                                                                                                        <NA>
#> 12                                                                                                        <NA>
#> 13                                                                                                        <NA>
#> 14                                                                             1=18-34; 2=35-54; 3=55 and over
#> 15                                                                                                 1=Yes; 2=No
#> 16                                                                                 1=Web; 2=Telephone; 3=Mixed
#> 17                                                                1=Yes; 2=No; 95=Would not vote / would spoil
#> 18             5=Already voted; 1=Certain to vote; 2=Likely to vote; 3=Unlikely to vote; 4=Certain not to vote
#> 19                                                                                                        <NA>
#> 20      1=Liberal; 2=Conservative; 3=NDP; 4=Bloc Québécois; 5=Green; 6=PPC; 90=Another party; 97=None of these
#> 21                                                                                                        <NA>
#> 22                                                                                                        <NA>
#> 23                         1=Very interested; 2=Quite interested; 3=Hardly interested; 4=Not at all interested
#> 24                                                  1=18-24; 2=25-34; 3=35-44; 4=45-54; 5=55-64; 6=65 and over
#> 25                                                              1=Man; 2=Woman; 3=Non-binary; 4=Another gender
#> 26                                  1=Primary or less; 2=Secondary; 3=College (CEGEP, technical); 4=University
#> 27                                                                                1=French; 2=English; 3=Other
#> 28                                                                                                 1=Yes; 2=No
#> 29                                                                                                        <NA>
#> 30                                                                                                        <NA>
#>          status added_in     qes2022     qes2018 qes2018_panel    qes2014
#> 1  experimental    0.1.0  comparable  comparable    comparable comparable
#> 2  experimental    0.1.0 approximate        <NA>   approximate       <NA>
#> 3  experimental    0.1.0        <NA>        <NA>   approximate       <NA>
#> 4  experimental    0.1.0 approximate approximate   approximate comparable
#> 5  experimental    0.1.0  comparable  comparable          <NA>  identical
#> 6  experimental    0.1.0        <NA>        <NA>          <NA>       <NA>
#> 7  experimental    0.1.0        <NA>        <NA>     identical       <NA>
#> 8  experimental    0.1.0 approximate  comparable   approximate  identical
#> 9  experimental    0.1.0  comparable  comparable          <NA>  identical
#> 10 experimental    0.1.0        <NA>  comparable          <NA> comparable
#> 11 experimental    0.1.0   identical  comparable          <NA> comparable
#> 12 experimental    0.2.0        <NA>   identical          <NA>       <NA>
#> 13 experimental    0.2.0   identical approximate          <NA>       <NA>
#> 14 experimental    0.2.0        <NA>        <NA>     identical       <NA>
#> 15 experimental    0.2.0   identical        <NA>          <NA>       <NA>
#> 16 experimental    0.2.0        <NA>        <NA>     identical       <NA>
#> 17 experimental    0.3.0        <NA>        <NA>          <NA>       <NA>
#> 18 experimental    1.0.0   identical        <NA>          <NA>       <NA>
#> 19 experimental    1.0.0   identical        <NA>          <NA>       <NA>
#> 20 experimental    1.0.0   identical        <NA>          <NA>       <NA>
#> 21 experimental    1.0.0 approximate        <NA>          <NA>       <NA>
#> 22 experimental    1.0.0        <NA>        <NA>          <NA>       <NA>
#> 23 experimental    1.0.0        <NA>        <NA>          <NA>       <NA>
#> 24 experimental    1.0.0        <NA>        <NA>          <NA>       <NA>
#> 25 experimental    1.0.0  comparable  comparable    comparable comparable
#> 26 experimental    1.0.0  comparable  comparable   approximate  identical
#> 27 experimental    1.0.0        <NA>  comparable    comparable comparable
#> 28 experimental    1.0.0   identical  comparable          <NA> comparable
#> 29 experimental    1.0.0 approximate        <NA>   approximate comparable
#> 30 experimental    1.0.0 approximate        <NA>          <NA> comparable
#>       qes2012 qes2012_panel qes_crop_2007_2010     qes2008     qes2007
#> 1   identical   approximate               <NA>  comparable  comparable
#> 2        <NA>   approximate         comparable        <NA>        <NA>
#> 3        <NA>   approximate         comparable        <NA>        <NA>
#> 4   identical    comparable               <NA>  comparable  comparable
#> 5   identical          <NA>               <NA>        <NA>        <NA>
#> 6        <NA>     identical               <NA>        <NA>        <NA>
#> 7        <NA>          <NA>               <NA>        <NA>        <NA>
#> 8   identical          <NA>               <NA>        <NA>        <NA>
#> 9   identical          <NA>               <NA>  comparable  comparable
#> 10  identical          <NA>               <NA>        <NA>        <NA>
#> 11 comparable          <NA>               <NA>  comparable  comparable
#> 12       <NA>          <NA>               <NA>        <NA>        <NA>
#> 13       <NA>          <NA>               <NA>        <NA>        <NA>
#> 14       <NA>    comparable         comparable  comparable        <NA>
#> 15       <NA>          <NA>               <NA>        <NA>        <NA>
#> 16       <NA>          <NA>               <NA>        <NA>   identical
#> 17       <NA>          <NA>               <NA>  comparable   identical
#> 18       <NA>          <NA>               <NA>        <NA>        <NA>
#> 19       <NA>          <NA>               <NA>        <NA>        <NA>
#> 20       <NA>          <NA>               <NA>        <NA>        <NA>
#> 21       <NA>          <NA>               <NA>        <NA>   identical
#> 22       <NA>          <NA>               <NA>  comparable   identical
#> 23       <NA>          <NA>               <NA>        <NA>        <NA>
#> 24       <NA>    comparable          identical  comparable        <NA>
#> 25  identical    comparable         comparable  comparable  comparable
#> 26 comparable          <NA>        approximate  comparable  comparable
#> 27  identical    comparable         comparable  comparable  comparable
#> 28 comparable          <NA>               <NA>        <NA>        <NA>
#> 29  identical          <NA>        approximate approximate approximate
#> 30  identical          <NA>               <NA>        <NA>        <NA>
#>    qes2007_panel     qes1998
#> 1    approximate  comparable
#> 2      identical        <NA>
#> 3      identical approximate
#> 4     comparable  comparable
#> 5           <NA>        <NA>
#> 6           <NA>        <NA>
#> 7           <NA>        <NA>
#> 8           <NA>        <NA>
#> 9           <NA>        <NA>
#> 10          <NA>        <NA>
#> 11          <NA>        <NA>
#> 12          <NA>        <NA>
#> 13          <NA>        <NA>
#> 14    comparable  comparable
#> 15          <NA>        <NA>
#> 16          <NA>        <NA>
#> 17    comparable  comparable
#> 18          <NA>        <NA>
#> 19          <NA>        <NA>
#> 20          <NA>        <NA>
#> 21          <NA>        <NA>
#> 22          <NA>        <NA>
#> 23     identical        <NA>
#> 24    comparable  comparable
#> 25    comparable  comparable
#> 26   approximate        <NA>
#> 27    comparable        <NA>
#> 28          <NA>        <NA>
#> 29   approximate        <NA>
#> 30          <NA>        <NA>

# how each study's question maps to one target
xw <- qes_spec("crosswalk", targets = "vote_prov_recall")
xw[, c("study", "source_var", "grade", "levels_not_offered")]
#>            study     source_var       grade levels_not_offered
#> 1        qes2012            q25   identical            PCQ;ADQ
#> 2        qes2014             Q3  comparable            PCQ;ADQ
#> 3        qes2018             q6  comparable     PVQ;PCQ;ON;ADQ
#> 4        qes2022 pes_votechoice  comparable         PVQ;ON;ADQ
#> 5  qes2018_panel         rts_q2  comparable     PVQ;PCQ;ON;ADQ
#> 6  qes2007_panel           vote approximate         CAQ;PCQ;ON
#> 7        qes2007            q12  comparable         CAQ;PCQ;ON
#> 8        qes2008           q12a  comparable         CAQ;PCQ;ON
#> 9  qes2012_panel       voteprov approximate            PCQ;ADQ
#> 10       qes1998         q3post  comparable  CAQ;QS;PVQ;PCQ;ON
xw # prints the target's section of the reference
#> ## `vote_prov_recall`: Provincial vote (recall)
#> 
#> Party the respondent reports having voted for in the Quebec general election of the study, asked after that election. Nonvoters, spoiled ballots and respondents not eligible or not registered are missing values with a reason, never a party.
#> 
#> Family `vote_prov` · type Categorical · timing Post-election · status Experimental · added in spec 0.1.0
#> 
#> **Levels**
#> 
#> | Code | Name | Label |
#> |---|---|---|
#> | 1 | `PLQ` | PLQ |
#> | 2 | `PQ` | PQ |
#> | 3 | `CAQ` | CAQ |
#> | 4 | `QS` | QS |
#> | 5 | `PVQ` | PVQ |
#> | 6 | `PCQ` | PCQ |
#> | 7 | `ON` | ON |
#> | 8 | `ADQ` | ADQ |
#> | 90 | `other` | Other party |
#> 
#> **Coverage**
#> 
#> | Study | Source | Grade | Reason | Instrument | Levels offered | Wording | Filter | Weight | Don't know |
#> |---|---|---|---|---|---|---|---|---|---|
#> | qes2022 | `pes_votechoice` (pes) | `comparable` | Lists the four main parties and the Conservatives but not the Green party or Option nationale, no don't-know option, spoiling is an option, and it follows a face-saving turnout question. | vote_recall_list | PLQ, PQ, CAQ, QS, PCQ, other; not offered: PVQ, ON, ADQ | Which party did you vote for? | pes_turnout: 2 = not_voted, 3 = not_voted, 4 = not_voted, 5 = not_registered, 6 = dk | `pes_weight_general` | Not offered |
#> | qes2018 | `q6` (post) | `comparable` | Only the four main parties are listed (no Green or Option nationale), there is no don't-know option, spoiling is an option, and the item follows a face-saving turnout question. | vote_recall_list | PLQ, PQ, CAQ, QS, other; not offered: PVQ, PCQ, ON, ADQ | Which party did you vote for? | q5: 1 = not_voted, 2 = not_voted, 3 = not_voted, 5 = ineligible, 99 = refused, NA = inapplicable | `pond` | Not offered |
#> | qes2018_panel | `rts_q2` (post) | `comparable` | Party list with the leaders' names, mixed telephone and web mode; the anchor is a web list without leaders. | vote_recall_list_leaders | PLQ, PQ, CAQ, QS, other; not offered: PVQ, PCQ, ON, ADQ | Et pour qui avez-vous voté? | rts_q1: 1 = not_voted, 2 = not_voted, 4 = dk_refused | `weight_rts` | Not documented |
#> | qes2014 | `Q3` (post) | `comparable` | Same party list as the anchor, but the stem does not name the election date and no don't-know option is offered. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | Which party did you vote for? | Q2: 2 = not_voted, 9 = refused | `POND` | Not offered |
#> | qes2012 | `q25` (post) | `identical` (anchor) | Anchor row of the target. | vote_recall_list | PLQ, PQ, CAQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | How did you vote in the last Quebec provincial election of September 4th, 2012? | q21: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Offered explicitly |
#> | qes2012_panel | `voteprov` (post) | `approximate` | Unprompted telephone recall (the options are not read) after a turnout question that probes election day or advance poll; the anchor is a web list. | vote_recall_unprompted | CAQ, PLQ, PQ, QS, PVQ, ON, other; not offered: PCQ, ADQ | For which party did you vote for? (DO NOT READ) |  | `pond_post` (needs review, not applied) | Not offered |
#> | qes2008 | `q12a` (post) | `comparable` | Same question (party voted for); the list names the ADQ and not the CAQ or ON, which did not exist; the turnout question has no don't-know code; telephone by the deposit metadata, the anchor is web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for? | q11: 2 = not_voted, 9 = refused |  | Not documented |
#> | qes2007 | `q12` (post) | `comparable` | Same question (party voted for, the parties named in the stem); the list names the ADQ and not the CAQ or ON, which did not exist; the study mixes telephone and web interviews (only the telephone script is deposited), the anchor is web. | vote_recall_list | PLQ, PQ, ADQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Which party did you vote for? The Liberal Party, Parti Québécois, ADQ, Québec solidaire, the Green Party or another party? | q11: 2 = not_voted, 8 = dk, 9 = refused | `pond` | Not documented |
#> | qes2007_panel | `vote` (post) | `approximate` | Unprompted telephone recall (options not read) after a two-step turnout question; the anchor is a web list. | vote_recall_unprompted | ADQ, PLQ, PQ, QS, PVQ, other; not offered: CAQ, PCQ, ON | Whom did you vote for? |  | `pond_tot_am1` (needs review, not applied) | Not offered |
#> | qes1998 | `q3post` (post) | `comparable` | Same question (party voted for, from a list read) by telephone, with the same stem in both firms' questionnaires; CREATEC's own Q3 uses other codes (1 PLQ, 2 PQ, 3 ADQ, 4 another party) with no Parti Égalité, and the pooled file puts them on CROP's codes, where the Parti Égalité is starred (not read) and never chosen; the list names the ADQ, not the CAQ; the anchor is web. | vote_recall_list | ADQ, PLQ, PQ, other; not offered: CAQ, QS, PVQ, PCQ, ON | 3. Pour lequel des partis suivants avez-vous voté? |  | `ponder3` (needs review, not applied) | Not offered |
#> 
#> **History**
#> 
#> - 0.1.0 (2026-09-27): First spec: 11 core targets with rows checked against the original files, in review, for qes2012, qes2014, qes2018, qes2022, qes2007_panel, qes2012_panel and qes2018_panel, their level sets, waves and weights.
#> - 0.1.1 (2026-09-27): Offline checks: gates.csv, the joint counts of gate code and source code among wave members for the 28 projectable rows of the studies whose metadata ships, and expected/marginals.csv, the projected unweighted marginals of the 28 projectable rows of the studies whose metadata ships. No row, code or grade changed.
#> - 0.1.2 (2026-09-27): Engine column hashes: expected/hashes.csv, the md5 of each harmonized column (study, target) that qes_harmonize() gives on the pinned files, for the 35 mapped rows, checked on the original files by the live tests (V-L1). No row, code or grade changed.
#> - 4.0.0 (2026-09-28): Review sign-off. An automated double review checked the 132 crosswalk rows against the original files and documents (one pass on codes and data, one on wording and comparability, adjudicated where they disagreed); it is not a human review, and reviewed_by says so. reviewed_on is 2026-09-27, and the new crosswalk column review_note says what the review corrected and why a row stays in review (V-S11 requires it on a reviewed row left in review). 93 rows are signed off (status stable) and applied by qes_harmonize() by default. 39 stay in review: the 38 rows of qes1998, qes2007_panel, qes2012_panel and qes_crop_2007_2010, whose recommended weights need review (a stable row there fails the release check V-S13), and the qes2014 gender row, whose grade was raised to identical and needs a second reviewer. get_qes_master() and get_decon() now apply signed-off rows only (include_draft = FALSE): a column whose question is in a row still in review is NA, reason not_reviewed in attr(, "legacy_na_columns"), and attr(, "source_map") gains the status of each row. MAJOR (changed column hashes): the qes2022 income amount 0, a blank that the survey sent to the bracket follow-up cps_income2, is a missing value (no_answer); the qes2022 typed other-party text is gated on cps_turnout as its parent row (3 to 5 inapplicable, 6 ineligible). Grades and metadata: qes2018_panel rv1a and rv1ab comparable to approximate (the stem also asks those who voted in advance for their vote; a push filter narrower than the anchor's); qes2014 QSEXE comparable to identical (the anchor's stems in both languages); qes2014 QSCOL dk_offered none; qes2022 cps_ideoself_1 instrument lr_0_10 (no slider is documented); the CROP intentions dk_offered volunteered, with their French wording from CROP's reports. Text only: the wording, grade reasons, evidence and notes of 36 rows corrected (each named in its review_note), among them the qes2007_panel counts of wave members and where its time-invariant items were asked. In gates.csv, typed text is counted as one token and an empty text as system missing.
#> - 4.1.0 (2026-09-28): Content sign-off apart from the weights. The 38 crosswalk rows of qes1998, qes2007_panel, qes2012_panel and qes_crop_2007_2010 that the automated double review of spec 4.0.0 signed off on their content are stable, and applied by qes_harmonize(), get_qes_master() and get_decon() by default; they were held in review only because the recommended weights of their waves need review. Their review_note still says the review was automated, not human, and says that the weight is tracked apart (dev/open-questions.md Q1). The release check V-S13 no longer fails a stable row on a study-wave whose recommended weight needs review; it still requires that a recommended weight is never calibrated on vote or turnout, and one recommended weight per study-wave (none where every weight is calibrated). The weights that need review stay unapplied: weight_pre and weight_post are NA there, with the message qesR_message_weight_review, and in get_qes_master() the reason not_reviewed with the cause weight_needs_review. qes2014 QSEXE (gender) is stable at comparable, the grade it had before the review (the English stem the review filled is kept); identical waits for a human second reviewer. Text only: the two documentation-only rows (rule none), qes2012_panel interetrec and qes1998 intvote2, have a wording_ref to their codebook entry, which V-S11 requires of a stable row; in legacy.csv, the note of each study's survey_weight row names the weight and its registry status (the CROP XPOND and the qes2012_panel pond need review, the qes2008 pond is calibrated on the vote, the qes2007_panel pond is not registered), the weight_pre and weight_post definitions say NA where the weight needs review, and the intended blanks of get_qes_master() have a row of their own with a cause and a note (qes1998 education, qes2018 income and religion: invalid_044_source; qes2012_panel political_interest: not_comparable_source; qes2022 language: not_harmonized_yet). No value map, gate, level set, expected marginal or column hash changed: MINOR, rows are only added to the default output.
#> - 4.2.0 (2026-09-28): The metadata of qes2022 ships (decision OD3 lifted by the owner on 2026-09-28; it carries the study's licence, CC BY-NC 4.0, inst/COPYRIGHTS section 2). The 18 qes2022 crosswalk rows get their wording_en and wording_fr, quoted from the study's bilingual codebook (file 7449514; for the typed other-party text, the stem of cps_votechoice1 with its option), and its 65 value-map rows the value label of the pinned file (source_label) in place of the md5 of that label (source_label_hash). gates.csv gains the 252 cells of the 16 projectable or gated qes2022 rows and expected/marginals.csv the 225 marginal cells of its 15 projectable rows, the counts the build-ignored data-raw/nc/ held until now for CI (identical, rebuilt from the pinned file by data-raw/build_sources.R and data-raw/project_marginals.R). The release check V-S11 no longer forbids wording and labels for a study whose metadata does not ship (every study's does). No row, map, gate, grade, level set, recorded marginal or column hash changed: MINOR, keys are only added to expected/.
#> 

# code by code, in French
qes_spec("crosswalk", targets = "sov_indep", studies = "qes2014",
         level = "code", lang = "fr")
#>     study wave    target variable source_code               source_label origin
#> 1 qes2014 post sov_indep      Q19           1                        Oui    map
#> 2 qes2014 post sov_indep      Q19           2                        Non    map
#> 3 qes2014 post sov_indep      Q19           8             Je ne sais pas    map
#> 4 qes2014 post sov_indep      Q19           9 Je préfère ne pas répondre    map
#>   target_code target_level target_label na_reason note
#> 1           1          yes          Oui      <NA> <NA>
#> 2           2           no          Non      <NA> <NA>
#> 3          NA         <NA>         <NA>        dk <NA>
#> 4          NA         <NA>         <NA>   refused <NA>

# the checked spec itself
s <- qes_spec("spec")
s
#> qesR harmonization spec 4.2.0 (2026-09-28), content hash 02b3b7edc509deff0db16859bef7bfb6
#>   targets: 30
#>   levels: 62
#>   crosswalk: 132
#>   valuemaps: 599
#>   waves: 39
#>   weights: 26
#>   changes: 12
#>   gates: 1268
#>   expected: 1174
#>   hashes: 127
#>   legacy: 100
#> Check: 0 error(s), 0 warning(s), 0 note(s).
```
