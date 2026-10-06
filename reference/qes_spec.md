# Harmonization rules and coverage (experimental)

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
  view = c("targets", "crosswalk", "spec", "pooled", "relaxed", "relaxed_maps"),
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

  `"targets"`, `"crosswalk"`, `"spec"`, `"pooled"`, `"relaxed"` or
  `"relaxed_maps"`.

- targets:

  Target, family or set names (views `"targets"` and `"crosswalk"`; the
  set `"decon"` holds the targets of the columns of
  [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md));
  pooled variables (view `"pooled"`); columns of
  [`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
  (views `"relaxed"` and `"relaxed_maps"`); `NULL` (default) is every
  one.

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

- `"pooled"`: a data frame with columns `pooled`, `label`, `definition`,
  `type` (of the pooled variable), `levels` (`code=label`, or the range
  of a numeric one), `type_name`, `type_label`, `member`, `precedence`,
  `default`, `transform`, `grade_cap`, `note`, `status`, `added_in`,
  then one column per study.

- `"relaxed"`: a data frame with columns `column`, `position`, `label`,
  `type`, `levels`, `base`, `transform`, `timing`, `relaxed` (how the
  column was relaxed), `definition`, `essential`, `status`, `added_in`,
  then one column per study.

- `"relaxed_maps"`: a data frame with columns `study`, `wave`, `column`,
  `source_var`, `rule`, `map_id`, `args`, `na_codes`, `gate`,
  `override`, `recode`, `wording`, `wording_ref`, `notes`, `evidence`,
  `status`, `reviewed_by`, `reviewed_on`, `review_note`, `added_in`.

- `"spec"`: an object of class `qes_spec`: a list with the spec
  `version`, its content `hash`, `custom` (`TRUE` when it is not the
  shipped spec) and `tables` (targets, levels, crosswalk, valuemaps,
  waves, weights, changes, gates, expected, hashes, legacy: the renderer
  of
  [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  and
  [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
  pooled and pooled_members: the pooled variables, relaxed and
  relaxed_maps: the relaxed layer of
  [`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md),
  rx_expected and rx_hashes: its recorded results); `attr(, "check")`
  holds the problems table (`rule`, `severity`, `table`, `row`, `key`,
  `detail`).

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

- `view = "pooled"`: one row per member of each pooled variable (such as
  `vote_choice`, which pools the reported vote and the vote intentions):
  its type name (the value of `<pooled>__type`), precedence (the member
  tried first is 1), whether it is used by default, the transform of its
  values and its grade cap, and one column per study giving the member's
  grade there (capped; `NA`: no question). `targets` selects pooled
  variables (or the set `"pooled"`).

- `view = "relaxed"`: one row per column of
  [`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md),
  the relaxed harmonization: its label, levels, base (the strict target
  or pooled variable it reuses, if any), the transform of the base's
  values, how it was relaxed, and one column per study saying where the
  study's values come from (`"strict"`: the base; `"relaxed"`: a relaxed
  mapping of the study's own question, with its status when it is not
  signed off yet; `NA`: none; a column built on another column takes
  that column's source). `targets` selects columns.

- `view = "relaxed_maps"`: one row per relaxed mapping (study, wave and
  column): the source variable, rule, value map, gate, whether it
  replaces the base, the recode in words, the wording or the document
  that gives it, notes and review status. `targets` selects columns.

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
La vue `"pooled"` décrit les variables regroupées (comme `vote_choice`,
qui réunit le vote déclaré et les intentions de vote) : leurs membres,
leur ordre de priorité et le niveau de chaque membre dans chaque étude.
`lang = "fr"` donne les étiquettes, définitions et raisons en français.
Les vues `"relaxed"` et `"relaxed_maps"` décrivent l'harmonisation
souple de
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
: ses colonnes, leur base et la façon dont elles sont assouplies, puis
l'appariement souple propre à chaque étude.
[`vignette("fr-reference-harmonisation", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md)
en est la référence complète.

## Targets in the shipped spec (version 4.7.0)

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
  qes2012_panel, qes_crop_2007_2010, qes1998, qes2022.

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
  categorical). Studies: qes2022, qes2012.

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
  qes2012_panel, qes2018_panel, qes_crop_2007_2010, qes2022.

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

- `sov_partnership_1995_push`:

  Referendum vote: the 1995 question, undecided pushed (family
  `sovereignty`; no set; categorical). Studies: qes2007, qes2008,
  qes2007_panel, qes1998.

- `satis_demo_qc`:

  Satisfaction with democracy in Quebec (family
  `democracy_satisfaction`; sets `attitudes`; ordinal). Studies:
  qes2012, qes2014, qes2018, qes2022, qes2007, qes2008.

- `gov_satisfaction`:

  Satisfaction with the Quebec government (family
  `government_satisfaction`; sets `attitudes`; ordinal). Studies:
  qes2012, qes2014, qes2018, qes2022, qes2007_panel.

- `econ_retro_qc`:

  Quebec's economy over the past year (family `economy_retrospective`;
  sets `attitudes`; ordinal). Studies: qes2012, qes2014, qes2018,
  qes2022, qes2007, qes2008.

- `attach_qc`:

  Attachment to Quebec (family `attachment`; sets `attitudes`; ordinal).
  Studies: qes2012, qes2014, qes2018, qes2022.

- `attach_ca`:

  Attachment to Canada (family `attachment`; sets `attitudes`; ordinal).
  Studies: qes2012, qes2014, qes2018, qes2022.

- `identity_qc_ca`:

  Québécois or Canadian identity (family `national_identity`; sets
  `attitudes`; categorical). Studies: qes2012, qes2014, qes2007,
  qes2008, qes2022.

- `pid_prov_strength`:

  Strength of provincial party identification (family `party_id`; sets
  `attitudes`; ordinal). Studies: qes2012, qes2014, qes2018, qes2022,
  qes2007, qes2008.

- `vote_prov_prev`:

  Provincial vote at the previous election (recall) (family
  `vote_prov_past`; sets `vote`; categorical). Studies: qes2014,
  qes2008, qes2018, qes2022.

- `therm_leader_plq`:

  Rating of the PLQ leader (0-100) (family `leader_ratings`; sets
  `attitudes`; numeric). Studies: qes2012, qes2007, qes2008, qes2014,
  qes2018, qes2022.

- `therm_leader_pq`:

  Rating of the PQ leader (0-100) (family `leader_ratings`; sets
  `attitudes`; numeric). Studies: qes2012, qes2007, qes2008, qes2014,
  qes2018, qes2022.

- `therm_leader_caq`:

  Rating of the CAQ leader (0-100) (family `leader_ratings`; sets
  `attitudes`; numeric). Studies: qes2012, qes2014, qes2018, qes2022.

- `therm_leader_qs`:

  Rating of the QS leader (0-100) (family `leader_ratings`; sets
  `attitudes`; numeric). Studies: qes2012, qes2007, qes2008, qes2014,
  qes2018, qes2022.

- `therm_leader_adq`:

  Rating of the ADQ leader (0-100) (family `leader_ratings`; sets
  `attitudes`; numeric). Studies: qes2007, qes2008.

- `region_cma3`:

  Region (Montreal and Quebec CMAs) (family `region`; sets `socio`;
  categorical). Studies: qes2012, qes2008, qes2014, qes2018,
  qes2012_panel, qes_crop_2007_2010, qes2018_panel, qes2007.

- `lang_home`:

  Language spoken most often at home (family `language`; sets `socio`;
  categorical). Studies: qes2012, qes2014, qes2018, qes2007, qes2008,
  qes2007_panel, qes_crop_2007_2010, qes2022.

- `relig_attend`:

  Attendance at religious services (family `faith`; sets `socio`;
  ordinal). Studies: qes2012, qes2007, qes2008, qes2014, qes2018.

- `birthplace3`:

  Birthplace (Quebec, rest of Canada, abroad) (family `birthplace`; sets
  `socio`; categorical). Studies: qes2012, qes2014, qes2018.

- `vote_fed_recall`:

  Federal vote at the last federal election (recall) (family `vote_fed`;
  sets `vote`; categorical). Studies: qes2012, qes2007, qes2008,
  qes2022.

- `mip_issue`:

  Most important issue of the election (family `issues`; sets
  `attitudes`; categorical). Studies: qes2012, qes2008, qes2014,
  qes2018, qes2022.

Pooled variables (one column that pools several targets;
`qes_spec("pooled")` gives each study's grade):

- `vote_choice`:

  Provincial vote choice (pooled) (pooled, categorical; sets `pooled`,
  `core`, `vote`). Members, in order of precedence: `vote_prov_recall`
  (recall), `vote_prov_intent_push` (intention_push), `vote_prov_intent`
  (intention).

- `sov_support`:

  Support for sovereignty (pooled) (pooled, categorical; sets `pooled`,
  `core`). Members, in order of precedence: `sov_indep` (independence),
  `sov_sovereign_country` (sovereign_country),
  `sov_partnership_1995_push` (partnership_1995_push),
  `sov_partnership_1995` (partnership_1995), `sov_favour` (favour).

- `pol_interest`:

  Interest in politics (pooled, 0-1) (pooled, numeric; sets `pooled`,
  `core`). Members, in order of precedence: `interest_4pt`
  (general_4pt), `interest_0_10` (general_0_10), `interest_campaign_4pt`
  (campaign_4pt), `interest_election_0_10` (election_0_10).

- `turnout`:

  Turnout in the provincial election (pooled) (pooled, categorical; sets
  `pooled`, `vote`). Members, in order of precedence:
  `turnout_prov_recall` (recall), `turnout_prov_likely` (intention, not
  by default).

## See also

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
and
[`vignette("harmonization-reference", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md),
the reference generated from the spec.

Other harmonization:
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md),
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md),
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
[`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md)

## Examples

``` r
# which studies have which target, and how comparable they are
qes_spec()
#>                       target                  family        type target_timing
#> 1           vote_prov_recall               vote_prov categorical          post
#> 2           vote_prov_intent               vote_prov categorical           pre
#> 3      vote_prov_intent_push               vote_prov categorical           pre
#> 4        turnout_prov_recall            turnout_prov categorical          post
#> 5                  sov_indep             sovereignty categorical           any
#> 6      sov_sovereign_country             sovereignty categorical           any
#> 7                 sov_favour             sovereignty     ordinal           any
#> 8                    lr_self              left_right     numeric           any
#> 9                   pid_prov                party_id categorical           any
#> 10              interest_4pt                interest     ordinal           any
#> 11                birth_year                   birth     numeric        static
#> 12               birth_month                   birth     numeric        static
#> 13                       age               age_years     numeric           any
#> 14                age_group3               age_bands     ordinal           any
#> 15                   citizen             citizenship categorical           any
#> 16               survey_mode          interview_mode categorical           any
#> 17      sov_partnership_1995             sovereignty categorical           any
#> 18       turnout_prov_likely            turnout_prov     ordinal           pre
#> 19    vote_prov_intent_other               vote_prov      string           pre
#> 20                   pid_fed                party_id categorical           any
#> 21             interest_0_10                interest     numeric           any
#> 22    interest_election_0_10                interest     numeric          post
#> 23     interest_campaign_4pt                interest     ordinal           pre
#> 24                age_group6               age_bands     ordinal           any
#> 25                    gender              sex_gender categorical        static
#> 26                education4               education     ordinal        static
#> 27               lang_mother                language categorical        static
#> 28               born_canada              birthplace categorical        static
#> 29             income_native                  income      string        static
#> 30                  religion                   faith      string        static
#> 31 sov_partnership_1995_push             sovereignty categorical           any
#> 32             satis_demo_qc  democracy_satisfaction     ordinal           any
#> 33          gov_satisfaction government_satisfaction     ordinal           any
#> 34             econ_retro_qc   economy_retrospective     ordinal           any
#> 35                 attach_qc              attachment     ordinal           any
#> 36                 attach_ca              attachment     ordinal           any
#> 37            identity_qc_ca       national_identity categorical           any
#> 38         pid_prov_strength                party_id     ordinal           any
#> 39            vote_prov_prev          vote_prov_past categorical           any
#> 40          therm_leader_plq          leader_ratings     numeric           any
#> 41           therm_leader_pq          leader_ratings     numeric           any
#> 42          therm_leader_caq          leader_ratings     numeric           any
#> 43           therm_leader_qs          leader_ratings     numeric           any
#> 44          therm_leader_adq          leader_ratings     numeric           any
#> 45               region_cma3                  region categorical        static
#> 46                 lang_home                language categorical        static
#> 47              relig_attend                   faith     ordinal           any
#> 48               birthplace3              birthplace categorical        static
#> 49           vote_fed_recall                vote_fed categorical           any
#> 50                 mip_issue                  issues categorical           any
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
#> 31       Referendum vote: the 1995 question, undecided pushed
#> 32                      Satisfaction with democracy in Quebec
#> 33                    Satisfaction with the Quebec government
#> 34                        Quebec's economy over the past year
#> 35                                       Attachment to Quebec
#> 36                                       Attachment to Canada
#> 37                             Québécois or Canadian identity
#> 38                Strength of provincial party identification
#> 39          Provincial vote at the previous election (recall)
#> 40                           Rating of the PLQ leader (0-100)
#> 41                            Rating of the PQ leader (0-100)
#> 42                           Rating of the CAQ leader (0-100)
#> 43                            Rating of the QS leader (0-100)
#> 44                           Rating of the ADQ leader (0-100)
#> 45                          Region (Montreal and Quebec CMAs)
#> 46                         Language spoken most often at home
#> 47                           Attendance at religious services
#> 48                Birthplace (Quebec, rest of Canada, abroad)
#> 49         Federal vote at the last federal election (recall)
#> 50                       Most important issue of the election
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                definition
#> 1                                                                                                                                                                                                                                                                                                                                                                        Party the respondent reports having voted for in the Quebec general election of the study, asked after that election. Nonvoters, spoiled ballots and respondents not eligible or not registered are missing values with a reason, never a party.
#> 2                                                                                                                                                                                                                                                                                                                                                            Party the respondent intends to vote for in the coming Quebec general election, asked before it, at the first question, without the push question for the undecided. Would not vote, none or would spoil is an answer (level no_party), not a missing value.
#> 3                                                                                                                                                 Vote intention in which respondents who named no party at the first question (the undecided and, in some studies, also those who would not vote, would vote for none or refused) were asked which party they lean toward (a push question), combined into one variable. A different stimulus from vote_prov_intent, and a separate target; the pooled variable vote_choice uses it before vote_prov_intent, and vote_choice__type records which one a value comes from.
#> 4                                                                                                                                                                                                                                                                                                                                                                                                                        Whether the respondent reports having voted in the Quebec general election of the study, asked after that election. Respondents not eligible or not registered are missing values with a reason.
#> 5                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   How the respondent would vote in a referendum on whether Quebec should become an independent country.
#> 6                                                                                                                                                                                                                                                            How the respondent would vote in a referendum on whether Quebec should become a sovereign country. A different stimulus from sov_indep (sovereign, not independent), and a separate target; the pooled variable sov_support combines them and records the wording in sov_support__type. The push question asked of the undecided is not part of this target.
#> 7                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       How favourable or opposed the respondent is to Quebec independence, on a four-point scale. Not a referendum vote.
#> 8                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     Where the respondent places their own views on a scale from 0 (left) to 10 (right).
#> 9                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        The provincial party the respondent usually thinks of themselves as close to; none is an answer.
#> 10                                                                                                                                                                                                                                                                                                                                                                                             How interested the respondent is in politics, on a four-point verbal scale. A separate target from the 0-10 interest items, never rescaled; the pooled variable pol_interest scores it on 0-1, graded approximate at most.
#> 11                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          Year the respondent was born.
#> 12                                                                                                                                                                                                                                                                                                                                                                                                               Month the respondent was born (1 = January). Asked with the year of birth in some studies; with birth_year, it tells whether a respondent born 18 years before the election year was 18 on election day.
#> 13                                                                                                                                                                                                                                                                                                                                                                                                                                                                            The respondent's age in years at the interview, as asked. Never computed from the year of birth, which gives the age only to within a year.
#> 14                                                                                                                                                                                                                                                                                  The respondent's age group at the interview in three bands: 18-34, 35-54, 55 and over. Built from a question with these bands or with bands that collapse into them exactly; where the study has no such question, derived from the age (exact) or the year of birth (graded approximate: an age at a band edge can be one year off).
#> 15                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       Whether the respondent is a Canadian citizen, where the study asked. Only citizens may vote in Quebec elections.
#> 16                                                                                                                                                                                                                                                                                                                                                                              How the respondent was interviewed: web, telephone or mixed. A leading column of every result of qes_harmonize(), filled from the mode of each wave (waves.csv); crosswalk rows exist only for the waves whose mode varies by respondent.
#> 17                                                                                                                                                   How the respondent would vote if a referendum were held today on the question of the 1995 referendum, sovereignty with an offer of partnership to the rest of Canada. A different stimulus from sov_indep and sov_sovereign_country, and a separate target; the pooled variable sov_support combines them and records the wording in sov_support__type. The push question asked of the undecided is not part of this target (sov_partnership_1995_push includes it).
#> 18                                                                                                                                                                                                                                                      How likely the respondent says they are to vote in the coming Quebec general election, asked before it. Having already voted (in advance polls) is an answer. A different stimulus from turnout_prov_recall (reported turnout), and a separate target; the pooled variable turnout uses it only when asked to (types = list(turnout = c("recall", "intention"))).
#> 19                                                                                                                                                                                                                                                                                                                                                                                                                                                       The party the respondent typed after choosing another party at the vote-intention question (vote_prov_intent, level other), as typed. Open text, not harmonized.
#> 20                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          The federal party the respondent usually thinks of themselves as close to; none is an answer.
#> 21                                                                                                                                                                                                                                                                                                                                                                                      How interested the respondent is in politics in general, from 0 (no interest) to 10 (a great deal). A separate target from the four-point item (interest_4pt), never rescaled; the pooled variable pol_interest divides it by 10.
#> 22                                                                                                                                                                                                                                                                                                                                                                                                            How interested the respondent was in the Quebec general election that has just taken place, from 0 (no interest) to 10 (a great deal), asked after it. Interest in an election, not in politics in general.
#> 23                                                                                                                                                                                                                                                                                                                                                                                                                            How interested the respondent is in the current Quebec election campaign, on a four-point verbal scale, asked during it. Interest in a campaign, not in politics in general (interest_4pt).
#> 24                                                                                                                                                                                                                                                               The respondent's age group at the interview in six bands: 18-24, 25-34, 35-44, 45-54, 55-64, 65 and over. Built from a question with these bands or with bands that collapse into them exactly; where the study has no such question, derived from the age (exact) or the year of birth (graded approximate: an age at a band edge can be one year off).
#> 25                                                                                                                                                                                                                                                                                                                                                                                             The respondent's gender, as asked or, in some telephone surveys, as recorded by the interviewer. Most studies offered only man and woman (a question on sex, in some); qes2022 also offered non-binary and another gender.
#> 26 The respondent's highest level of education in four groups: primary or less, secondary, college (CEGEP or technical), university (completed or not). From the highest level completed where the study asks for it, from years of schooling otherwise (graded approximate). A vocational or trade credential (the DEP) has no group of its own and is not placed alike in every study: secondary where the question lists the DEP as a secondary diploma (qes2018), college where it is a trade certificate or technical training (qes2018_panel and, probably, the studies with no DEP option); see the grade reasons.
#> 27                                                                                                                                                                                                                                                                                                                                                                       The first language the respondent learned at home in childhood and still understands: French, English or another language. A respondent who reports two first languages is a missing value (reason not_mappable), never assigned to one of them.
#> 28                                                                                                                                                                                                                                                                                                                                                                                                                                               Whether the respondent was born in Canada. From a question on birthplace (Quebec, elsewhere in Canada, outside Canada) where the study asks that one, collapsed exactly.
#> 29                                                                                                                                                                                                                                                                                                                            The respondent's household income before taxes as each study recorded it: the text of the study's own bracket (or the amount, where the study asked for one). Brackets differ between studies, so the values are not comparable across studies; don't know and refusals are missing values.
#> 30                                                                                                                                                                                                                The respondent's religion as each study recorded it: the text of the study's own category. Categories differ between studies. Where the question was asked only of respondents who belong to a religion, the others are missing values: reason inapplicable for those who said they belong to none, refused for those who would not answer the filter question, which is the gate of the crosswalk row.
#> 31                                                                                                                     How the respondent would vote on the question of the 1995 referendum (sovereignty with an offer of partnership to the rest of Canada), with the respondents who did not know at the first question asked which way they would be inclined to vote (a push question), combined into one variable: the first answer where there is one, else the pushed answer. A separate target from sov_partnership_1995, which has the first question only; the pooled variable sov_support uses this one first.
#> 32                                                                                                                                                                                                                                                                                                                                                                                                                                                    How satisfied the respondent is, on the whole, with the way democracy works in Quebec, on a four-point verbal scale (very, fairly, not very, not at all satisfied).
#> 33                                                                                                                                                                                                                                                                                                How satisfied the respondent is with the performance of the Quebec government in office, on a four-point scale. The government changes by design: each row's wording names it (the Liberal government in 2012, the PQ government in 2014, Philippe Couillard's in 2018, the government under François Legault in 2022).
#> 34                                                                                                                                                                                                                                                                                                                                                                                                                                                  Whether the respondent thinks Quebec's economy has gotten better, stayed about the same or gotten worse over the past year (a retrospective, sociotropic evaluation).
#> 35                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               How attached the respondent feels to Quebec, on a four-point scale (very, fairly, not very, not at all).
#> 36                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               How attached the respondent feels to Canada, on a four-point scale (very, fairly, not very, not at all).
#> 37                                                                                                                                                                                                                                                                                                                                                                                                                                                      How the respondent defines themselves, from Québécois only to Canadian only (the five-point Moreno question); another self-definition is an answer (level other).
#> 38                                                                                                                                                                                                                                                                                                                              How strongly the respondent identifies with the provincial party named at the party identification question (very, fairly, not very strongly), asked of those who named a party. Those who named none, did not know or refused were not asked: the value is missing, reason inapplicable.
#> 39                                                                                                                                                                                                                                                                                         Party the respondent reports having voted for in the Quebec general election before the study's election (election_ref gives which one). Recalled years later: known to lean toward the winner of that election. Nonvoters, spoiled ballots and respondents not eligible then are missing values with a reason, never a party.
#> 40                                                                                                                                                                                                                                                                                                                                                                                                       How much the respondent likes the leader of the PLQ, from 0 (really dislike) to 100 (really like); the leader changes by design, and each row names the person rated. Not knowing the leader is don't know (dk).
#> 41                                                                                                                                                                                                                                                                                                                                                                                                        How much the respondent likes the leader of the PQ, from 0 (really dislike) to 100 (really like); the leader changes by design, and each row names the person rated. Not knowing the leader is don't know (dk).
#> 42                                                                                                                                                                                                                                                                                                                                                                                                       How much the respondent likes the leader of the CAQ, from 0 (really dislike) to 100 (really like); the leader changes by design, and each row names the person rated. Not knowing the leader is don't know (dk).
#> 43                                                                                                                                                                                                                                                                                            How much the respondent likes the leader of the QS, from 0 (really dislike) to 100 (really like); the leader changes by design, and each row names the person rated (QS has two spokespersons: the one the study rated, or its candidate for premier when the study rated both). Not knowing the leader is don't know (dk).
#> 44                                                                                                                                                                                                                                                                                                                                                                                                       How much the respondent likes the leader of the ADQ, from 0 (really dislike) to 100 (really like); the leader changes by design, and each row names the person rated. Not knowing the leader is don't know (dk).
#> 45                                                                                                                                                                                                                                                                                                                                                                                                   Where the respondent lives: the Montreal census metropolitan area (CMA), the Quebec CMA or the rest of Quebec, from the region the study recorded for its sampling, quotas or weighting (not a question of its own).
#> 46                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            The language the respondent speaks most often at home: French, English or another language.
#> 47                                                                                                                                                                                                                                                                                                                                           How often the respondent attends services at their place of worship, not counting weddings and funerals, from every week to hardly ever or never. Where the study asked only those who belong to a religion, those who do not are hardly ever or never (graded approximate).
#> 48                                                                                                                                                                                                                                                                                                                                                                                                                                           Where the respondent was born: in Quebec, elsewhere in Canada or outside Canada. Where both are harmonized, born_canada is yes exactly where this is quebec or other_canada.
#> 49                                                                                                                                                                                                                                                                                                                          Party the respondent reports having voted for in the last Canadian federal election before the study (each row's wording names it: January 2006, October 2008, May 2011, 2021). Nonvoters, spoiled ballots and respondents not eligible then are missing values with a reason, never a party.
#> 50                                                                                                                                                                                                                                                                                                                      Which issue, from the study's closed list, was the most important to the respondent personally in the Quebec general election of the study. The lists change from election to election: an issue a study did not list is a structural zero there (levels_not_offered), not an absence of concern.
#>                                                                                                                                                                                                                                                                                                   levels
#> 1                                                                                                                                                                                                                                    1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party
#> 2                                                                                                                                                                                            1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party; 95=Would not vote / none / would spoil
#> 3                                                                                                                                                                                            1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party; 95=Would not vote / none / would spoil
#> 4                                                                                                                                                                                                                                                                                            1=Yes; 2=No
#> 5                                                                                                                                                                                                                                                           1=Yes; 2=No; 95=Would not vote / would spoil
#> 6                                                                                                                                                                                                                                                           1=Yes; 2=No; 95=Would not vote / would spoil
#> 7                                                                                                                                                                                                                           1=Very favourable; 2=Somewhat favourable; 3=Somewhat opposed; 4=Very opposed
#> 8                                                                                                                                                                                                                                                                                                   <NA>
#> 9                                                                                                                                                                                                                  1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party; 97=None of these
#> 10                                                                                                                                                                                                                   1=Very interested; 2=Quite interested; 3=Hardly interested; 4=Not at all interested
#> 11                                                                                                                                                                                                                                                                                                  <NA>
#> 12                                                                                                                                                                                                                                                                                                  <NA>
#> 13                                                                                                                                                                                                                                                                                                  <NA>
#> 14                                                                                                                                                                                                                                                                       1=18-34; 2=35-54; 3=55 and over
#> 15                                                                                                                                                                                                                                                                                           1=Yes; 2=No
#> 16                                                                                                                                                                                                                                                                           1=Web; 2=Telephone; 3=Mixed
#> 17                                                                                                                                                                                                                                                          1=Yes; 2=No; 95=Would not vote / would spoil
#> 18                                                                                                                                                                                                       5=Already voted; 1=Certain to vote; 2=Likely to vote; 3=Unlikely to vote; 4=Certain not to vote
#> 19                                                                                                                                                                                                                                                                                                  <NA>
#> 20                                                                                                                                                                                                1=Liberal; 2=Conservative; 3=NDP; 4=Bloc Québécois; 5=Green; 6=PPC; 90=Another party; 97=None of these
#> 21                                                                                                                                                                                                                                                                                                  <NA>
#> 22                                                                                                                                                                                                                                                                                                  <NA>
#> 23                                                                                                                                                                                                                   1=Very interested; 2=Quite interested; 3=Hardly interested; 4=Not at all interested
#> 24                                                                                                                                                                                                                                            1=18-24; 2=25-34; 3=35-44; 4=45-54; 5=55-64; 6=65 and over
#> 25                                                                                                                                                                                                                                                        1=Man; 2=Woman; 3=Non-binary; 4=Another gender
#> 26                                                                                                                                                                                                                            1=Primary or less; 2=Secondary; 3=College (CEGEP, technical); 4=University
#> 27                                                                                                                                                                                                                                                                          1=French; 2=English; 3=Other
#> 28                                                                                                                                                                                                                                                                                           1=Yes; 2=No
#> 29                                                                                                                                                                                                                                                                                                  <NA>
#> 30                                                                                                                                                                                                                                                                                                  <NA>
#> 31                                                                                                                                                                                                                                                          1=Yes; 2=No; 95=Would not vote / would spoil
#> 32                                                                                                                                                                                                                    1=Very satisfied; 2=Fairly satisfied; 3=Not very satisfied; 4=Not at all satisfied
#> 33                                                                                                                                                                                                                    1=Very satisfied; 2=Fairly satisfied; 3=Not very satisfied; 4=Not at all satisfied
#> 34                                                                                                                                                                                                                                                                   1=Better; 2=About the same; 3=Worse
#> 35                                                                                                                                                                                                                        1=Very attached; 2=Fairly attached; 3=Not very attached; 4=Not at all attached
#> 36                                                                                                                                                                                                                        1=Very attached; 2=Fairly attached; 3=Not very attached; 4=Not at all attached
#> 37                                                                                                                                                     1=Québécois only; 2=Québécois first, then Canadian; 3=Equally Québécois and Canadian; 4=Canadian first, then Québécois; 5=Canadian only; 90=Other
#> 38                                                                                                                                                                                                                                               1=Very strongly; 2=Fairly strongly; 3=Not very strongly
#> 39                                                                                                                                                                                                                                   1=PLQ; 2=PQ; 3=CAQ; 4=QS; 5=PVQ; 6=PCQ; 7=ON; 8=ADQ; 90=Other party
#> 40                                                                                                                                                                                                                                                                                                  <NA>
#> 41                                                                                                                                                                                                                                                                                                  <NA>
#> 42                                                                                                                                                                                                                                                                                                  <NA>
#> 43                                                                                                                                                                                                                                                                                                  <NA>
#> 44                                                                                                                                                                                                                                                                                                  <NA>
#> 45                                                                                                                                                                                                                                                        1=Montreal CMA; 2=Quebec CMA; 3=Rest of Quebec
#> 46                                                                                                                                                                                                                                                                          1=French; 2=English; 3=Other
#> 47                                                                                                                                                                                                         1=Every week; 2=Twice a month; 3=Once a month; 4=Once or twice a year; 5=Hardly ever or never
#> 48                                                                                                                                                                                                                                                     1=Quebec; 2=Elsewhere in Canada; 3=Outside Canada
#> 49                                                                                                                                                                                                                  1=Liberal; 2=Conservative; 3=NDP; 4=Bloc Québécois; 5=Green; 6=PPC; 90=Another party
#> 50 1=The economy; 2=Health care; 3=The environment; 4=Education; 5=Aid to families; 6=Poverty; 7=Integrity and corruption; 8=Taxes and public finances; 9=Quebec sovereignty; 10=State secularism (the charter); 11=Immigration; 12=Cost of living; 13=Housing; 14=The French language; 90=Another issue
#>          status added_in     qes2022     qes2018 qes2018_panel     qes2014
#> 1  experimental    0.1.0  comparable  comparable    comparable  comparable
#> 2  experimental    0.1.0 approximate        <NA>   approximate        <NA>
#> 3  experimental    0.1.0 approximate        <NA>   approximate        <NA>
#> 4  experimental    0.1.0 approximate approximate   approximate  comparable
#> 5  experimental    0.1.0  comparable  comparable          <NA>   identical
#> 6  experimental    0.1.0        <NA>        <NA>          <NA>        <NA>
#> 7  experimental    0.1.0        <NA>        <NA>     identical        <NA>
#> 8  experimental    0.1.0 approximate  comparable   approximate   identical
#> 9  experimental    0.1.0  comparable  comparable          <NA>   identical
#> 10 experimental    0.1.0        <NA>  comparable          <NA>  comparable
#> 11 experimental    0.1.0   identical  comparable          <NA>  comparable
#> 12 experimental    0.2.0        <NA>   identical          <NA>        <NA>
#> 13 experimental    0.2.0   identical approximate          <NA>        <NA>
#> 14 experimental    0.2.0        <NA>        <NA>     identical        <NA>
#> 15 experimental    0.2.0   identical        <NA>          <NA>        <NA>
#> 16 experimental    0.2.0        <NA>        <NA>     identical        <NA>
#> 17 experimental    0.3.0        <NA>        <NA>          <NA>        <NA>
#> 18 experimental    1.0.0   identical        <NA>          <NA>        <NA>
#> 19 experimental    1.0.0   identical        <NA>          <NA>        <NA>
#> 20 experimental    1.0.0   identical        <NA>          <NA>        <NA>
#> 21 experimental    1.0.0 approximate        <NA>          <NA>        <NA>
#> 22 experimental    1.0.0        <NA>        <NA>          <NA>        <NA>
#> 23 experimental    1.0.0        <NA>        <NA>          <NA>        <NA>
#> 24 experimental    1.0.0        <NA>        <NA>          <NA>        <NA>
#> 25 experimental    1.0.0  comparable  comparable    comparable  comparable
#> 26 experimental    1.0.0  comparable  comparable   approximate   identical
#> 27 experimental    1.0.0 approximate  comparable    comparable  comparable
#> 28 experimental    1.0.0   identical  comparable          <NA>  comparable
#> 29 experimental    1.0.0 approximate        <NA>   approximate  comparable
#> 30 experimental    1.0.0 approximate        <NA>          <NA>  comparable
#> 31 experimental    4.3.0        <NA>        <NA>          <NA>        <NA>
#> 32 experimental    4.3.0  comparable  comparable          <NA>   identical
#> 33 experimental    4.3.0  comparable  comparable          <NA>  comparable
#> 34 experimental    4.3.0  comparable   identical          <NA>   identical
#> 35 experimental    4.3.0  comparable  comparable          <NA>  comparable
#> 36 experimental    4.3.0  comparable  comparable          <NA>  comparable
#> 37 experimental    4.3.0  comparable        <NA>          <NA>  comparable
#> 38 experimental    4.3.0 approximate  comparable          <NA>   identical
#> 39 experimental    4.3.0  comparable  comparable          <NA>   identical
#> 40 experimental    4.3.0 approximate approximate          <NA>  comparable
#> 41 experimental    4.3.0 approximate approximate          <NA>  comparable
#> 42 experimental    4.3.0 approximate approximate          <NA>  comparable
#> 43 experimental    4.3.0 approximate approximate          <NA>  comparable
#> 44 experimental    4.3.0        <NA>        <NA>          <NA>        <NA>
#> 45 experimental    4.3.0        <NA>   identical   approximate   identical
#> 46 experimental    4.3.0 approximate  comparable          <NA>  comparable
#> 47 experimental    4.3.0        <NA> approximate          <NA> approximate
#> 48 experimental    4.3.0        <NA>  comparable          <NA>  comparable
#> 49 experimental    4.3.0  comparable        <NA>          <NA>        <NA>
#> 50 experimental    4.3.0  comparable  comparable          <NA>  comparable
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
#> 20 comparable          <NA>               <NA>        <NA>        <NA>
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
#> 31       <NA>          <NA>               <NA>  comparable   identical
#> 32  identical          <NA>               <NA>  comparable  comparable
#> 33  identical          <NA>               <NA>        <NA>        <NA>
#> 34  identical          <NA>               <NA>  comparable  comparable
#> 35  identical          <NA>               <NA>        <NA>        <NA>
#> 36  identical          <NA>               <NA>        <NA>        <NA>
#> 37  identical          <NA>               <NA>  comparable  comparable
#> 38  identical          <NA>               <NA> approximate approximate
#> 39       <NA>          <NA>               <NA>  comparable        <NA>
#> 40  identical          <NA>               <NA>  comparable  comparable
#> 41  identical          <NA>               <NA>  comparable  comparable
#> 42  identical          <NA>               <NA>        <NA>        <NA>
#> 43  identical          <NA>               <NA>  comparable  comparable
#> 44       <NA>          <NA>               <NA>  comparable   identical
#> 45  identical    comparable         comparable  comparable  comparable
#> 46  identical          <NA>         comparable  comparable  comparable
#> 47  identical          <NA>               <NA>  comparable  comparable
#> 48  identical          <NA>               <NA>        <NA>        <NA>
#> 49  identical          <NA>               <NA>  comparable approximate
#> 50  identical          <NA>               <NA>  comparable        <NA>
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
#> 31    comparable  comparable
#> 32          <NA>        <NA>
#> 33   approximate        <NA>
#> 34          <NA>        <NA>
#> 35          <NA>        <NA>
#> 36          <NA>        <NA>
#> 37          <NA>        <NA>
#> 38          <NA>        <NA>
#> 39          <NA>        <NA>
#> 40          <NA>        <NA>
#> 41          <NA>        <NA>
#> 42          <NA>        <NA>
#> 43          <NA>        <NA>
#> 44          <NA>        <NA>
#> 45          <NA>        <NA>
#> 46    comparable        <NA>
#> 47          <NA>        <NA>
#> 48          <NA>        <NA>
#> 49          <NA>        <NA>
#> 50          <NA>        <NA>

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

# the pooled variables: which member each study uses, and its grade
pv <- qes_spec("pooled", targets = "vote_choice")
pv[, c("type_name", "member", "precedence", "qes2012", "qes2022")]
#>        type_name                member precedence   qes2012     qes2022
#> 1         recall      vote_prov_recall          1 identical  comparable
#> 2 intention_push vote_prov_intent_push          2      <NA> approximate
#> 3      intention      vote_prov_intent          3      <NA> approximate

# the relaxed columns of qes_decon(), and each study's relaxed mapping
rx <- qes_spec("relaxed")
rx[, c("column", "base", "transform", "qes2022", "qes1998")]
#>              column                     base
#> 1       citizenship           target:citizen
#> 2               yob        target:birth_year
#> 3         age_group        target:age_group3
#> 4            gender            target:gender
#> 5        education4                     <NA>
#> 6         education        column:education4
#> 7        income_cat                     <NA>
#> 8          language       target:lang_mother
#> 9       language_fr          column:language
#> 10     language_eng          column:language
#> 11         religion                     <NA>
#> 12          marital                     <NA>
#> 13       employment                     <NA>
#> 14            union                     <NA>
#> 15           region       target:region_cma3
#> 16     region_admin                     <NA>
#> 17      born_canada       target:born_canada
#> 18      born_quebec       target:birthplace3
#> 19      vote_choice       pooled:vote_choice
#> 20        vote_type pooled:vote_choice__type
#> 21          turnout           pooled:turnout
#> 22        vote_prev    target:vote_prov_prev
#> 23              pid          target:pid_prov
#> 24               lr           target:lr_self
#> 25         interest      pooled:pol_interest
#> 26      interest_01      pooled:pol_interest
#> 27      sovereignty       pooled:sov_support
#> 28 sovereignty_type pooled:sov_support__type
#> 29  satis_democracy     target:satis_demo_qc
#> 30 gov_satisfaction  target:gov_satisfaction
#> 31       econ_retro     target:econ_retro_qc
#> 32        econ_self                     <NA>
#> 33         identity    target:identity_qc_ca
#> 34    attach_quebec         target:attach_qc
#> 35    attach_canada         target:attach_ca
#>                                                                                                                                                      transform
#> 1                                                                                                                            recode:yes=citizen,no=not_citizen
#> 2                                                                                                                                                     identity
#> 3                                                                                                                                                     identity
#> 4                                                                                                                                                     identity
#> 5                                                                                                                                                 relaxed_only
#> 6                                               recode:no_diploma=no_diploma,high_school=high_school_college,college=high_school_college,university=university
#> 7                                                                                                                                                 relaxed_only
#> 8                                                                                                                                                     identity
#> 9                                                                                                                        recode:french=yes,english=no,other=no
#> 10                                                                                                                       recode:french=no,english=yes,other=no
#> 11                                                                                                                                                relaxed_only
#> 12                                                                                                                                                relaxed_only
#> 13                                                                                                                                                relaxed_only
#> 14                                                                                                                                                relaxed_only
#> 15                                                                                                                                                    identity
#> 16                                                                                                                                                relaxed_only
#> 17                                                                                                                                                    identity
#> 18                                                                                                                 recode:quebec=yes,other_canada=no,abroad=no
#> 19                                                                                                                                                    identity
#> 20                                                                                                                                                    identity
#> 21                                                                                                                                                    identity
#> 22                                                                                                                                                    identity
#> 23                                                                                                                                                    identity
#> 24                                                                                                                                                    identity
#> 25                                                                                                                             bands:0.55,0.85:low,medium,high
#> 26                                                                                                                                                    identity
#> 27                                                                                                            recode:yes=yes,no=no,would_not_vote=NA:not_voted
#> 28 recode:independence=independence,sovereign_country=sovereign_country,partnership_1995_push=partnership_1995,partnership_1995=partnership_1995,favour=favour
#> 29                                                                                                                                                    identity
#> 30                                                                                                                                                    identity
#> 31                                                                                                                                                    identity
#> 32                                                                                                                                                relaxed_only
#> 33                                                                                                                                                    identity
#> 34                                                                                                                                                    identity
#> 35                                                                                                                                                    identity
#>    qes2022 qes1998
#> 1   strict    <NA>
#> 2   strict    <NA>
#> 3   strict  strict
#> 4   strict  strict
#> 5  relaxed relaxed
#> 6  relaxed relaxed
#> 7  relaxed    <NA>
#> 8  relaxed relaxed
#> 9  relaxed relaxed
#> 10 relaxed relaxed
#> 11 relaxed    <NA>
#> 12 relaxed    <NA>
#> 13 relaxed relaxed
#> 14 relaxed    <NA>
#> 15    <NA>    <NA>
#> 16    <NA>    <NA>
#> 17  strict    <NA>
#> 18    <NA>    <NA>
#> 19  strict  strict
#> 20  strict  strict
#> 21  strict  strict
#> 22  strict relaxed
#> 23  strict    <NA>
#> 24  strict    <NA>
#> 25  strict    <NA>
#> 26  strict    <NA>
#> 27  strict  strict
#> 28  strict  strict
#> 29  strict    <NA>
#> 30  strict relaxed
#> 31  strict    <NA>
#> 32 relaxed    <NA>
#> 33  strict    <NA>
#> 34  strict    <NA>
#> 35  strict    <NA>
qes_spec("relaxed_maps", targets = "education")[, c("study", "source_var", "recode", "status")]
#>     study source_var
#> 1 qes1998       scol
#>                                                                                                                      recode
#> 1 1 = No high school diploma; 2 = High school to college (CEGEP); 3 = University; 9 = Don't know or refused (one code) (NA)
#>   status
#> 1 stable

# the checked spec itself
s <- qes_spec("spec")
s
#> qesR harmonization spec 4.7.0 (2026-10-05), content hash 30697daa0042aab3078507b665867748
#>   targets: 50
#>   levels: 169
#>   crosswalk: 236
#>   valuemaps: 1754
#>   waves: 39
#>   weights: 26
#>   changes: 18
#>   gates: 3209
#>   expected: 2914
#>   hashes: 269
#>   legacy: 102
#>   pooled: 4
#>   pooled_members: 14
#>   relaxed: 35
#>   relaxed_maps: 65
#>   rx_expected: 2684
#>   rx_hashes: 385
#> Check: 0 error(s), 0 warning(s), 0 note(s).
```
