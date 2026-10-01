# Harmonize variables across studies (experimental)

`qes_harmonize()` builds one data frame from several studies, with one
column per harmonized variable ("target") and one row per respondent. It
does only what the reviewed harmonization spec says: for each study and
target, one question of that study, its codes mapped one by one to the
target's levels, and a reason for every missing value. Nothing is
matched by name or guessed, and a code the spec does not map is an
error, never passed through.
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
shows the spec: which studies have which target, how comparable each
study's question is, and the exact mapping.

## Usage

``` r
qes_harmonize(
  studies = NULL,
  targets = "core",
  layout = c("respondent", "long"),
  values = c("factor", "labelled", "code"),
  missing = c("na", "reasons"),
  min_grade = c("approximate", "comparable", "identical"),
  weights = c("normalized", "raw"),
  unmapped = c("error", "warn", "na"),
  on_fail = c("stop", "skip"),
  keep_source = FALSE,
  include_draft = FALSE,
  data = NULL,
  spec = NULL,
  lang = c("en", "fr"),
  quiet = FALSE,
  types = NULL
)
```

## Arguments

- studies:

  Study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)).
  `NULL` (default) means the Quebec Election Studies the spec covers, or
  the studies named in `data` when it is given; `"all"` means every
  study the spec covers.

- targets:

  Target, family, set or pooled variable names (see
  [`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md));
  the default `"core"` is the core set (with the pooled variables
  `vote_choice`, `sov_support` and `pol_interest`), `"decon"` the
  targets of the columns of
  [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
  and `"pooled"` every pooled variable.

- layout:

  `"respondent"` (default): one row per respondent of each study's file.
  `"long"`: one row per respondent and wave (see *Waves*).

- values:

  How categorical targets are returned: `"factor"` (default; ordered for
  ordinal targets, with the target's levels in every study),
  `"labelled"`
  ([`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
  integer codes that are stable across versions) or `"code"` (the ASCII
  level names). Numeric targets are numbers.

- missing:

  `"na"` (default) or `"reasons"`, which adds a factor column
  `<target>__na` giving the reason of each missing value.

- min_grade:

  The lowest comparability grade kept: `"approximate"` (default, every
  graded cell), `"comparable"` or `"identical"`.

- weights:

  `"normalized"` (default): each wave's recommended weight divided by
  its mean over the wave's members, so it has mean 1 in each study and
  wave. `"raw"`: as deposited. See *Weights*.

- unmapped:

  What a source code the spec does not map does: `"error"` (default)
  raises an error of class `qesR_error_unmapped`; `"warn"` and `"na"`
  set it to `NA` with reason `unmapped`, with or without a warning.

- on_fail:

  `"stop"` (default): a study that cannot be read or checked stops the
  call. `"skip"` leaves it out with a warning; the result's attribute
  `failed_studies` gives the reason.

- keep_source:

  If `TRUE`, adds `<target>__src`, the source code of each value as
  text.

- include_draft:

  If `TRUE`, also applies crosswalk rows not yet signed off by a
  reviewer (status `"review"` or `"draft"`); a message counts the cells
  that use them. `FALSE` (default) leaves them `NA` (reason
  `not_reviewed`), with a warning when that leaves the whole result `NA`
  (see *Experimental*).

- data:

  `NULL` (read the pinned files) or a named list of data frames as
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  returns them, named by study code.

- spec:

  `NULL` (the spec shipped with qesR), the path of a spec directory, or
  a `qes_spec` object (see
  [`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)).

- lang:

  Language of returned labels (factor levels, variable labels and target
  populations): `"en"` (default) or `"fr"`. Codes and reasons do not
  depend on it.

- quiet:

  If `TRUE`, no progress or informational messages.

- types:

  `NULL` (default: the default members of each pooled variable) or a
  named list, pooled variable -\> the type names of the members to use
  (a member's target name is accepted too), for example
  `list(vote_choice = "recall")` or
  `list(turnout = c("recall", "intention"))`. The order given does not
  matter: the members are tried in the spec's order of precedence. See
  *Pooled variables*.

## Value

A data frame of class `qes_harmonized`, returned visibly, one row per
respondent of each study's file (no row is dropped), or per respondent
and wave in the long layout. Leading columns: `study`, `year` (the
study's year; for pooled polls, the year the respondent's poll began),
`election_date`, `family`, `study_design`, `target_population`, `waves`
(the waves the respondent belongs to, `;`-separated; in the long layout
`wave`, `wave_timing` and `wave_design` instead), `qes_id`
(`<study>:<identifier>`, unique in the respondent layout), `subsample`,
`stratum` (the independent sample within the study, see *Waves*: the
poll's wave name for pooled polls, `"1"` = CREATEC and `"2"` = CROP for
`qes1998`; `NA` for a study drawn as one sample), `source_row` (the row
in the study's file, for joining raw variables with
[`merge()`](https://rdrr.io/r/base/merge.html) on `study` and
`source_row`), `survey_mode`, `interview_date` (of the respondent's
first wave in the respondent layout), `days_to_election` and
`eligible_voter`. Then one column per target, carrying its label in
`attr(, "label")`, then one per pooled variable, then the `__type`,
`__grade` and `__item` companions of each pooled variable, then the
`__na` and `__src` companions (targets, then pooled variables), then the
weight columns: `weight_pre`, `weight_post`, `weight_pre_var` and
`weight_post_var` (respondent layout) or `weight` and `weight_var` (long
layout). Attributes: `qes_spec` (spec `version`, `hash`, `custom`,
`engine`), `qes_provenance` (see
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md),
with levels `"study"`, `"cell"`, `"spec"` and, with pooled variables,
`"pooled"`), `qes_weight_guide` (`target` or pooled variable, `study`,
`wave`, `target_timing`, `weight_column`, `weight_var`, `weight_status`)
and `failed_studies` (`study`, `class`, `message`, `parent_message`).

Results for different studies built with the same spec can be combined
with [`rbind()`](https://rdrr.io/r/base/cbind.html), which also combines
their provenance. It is an error (class `qesR_error_input`) if the spec
content hashes differ, the parts were built with different `lang`,
`layout`, `values`, `missing`, `weights` or `types`, their columns
differ (different `targets` or `keep_source`), or a study appears twice;
harmonizing all the studies in one call is simpler.

## Experimental

The engine and its spec are experimental: targets, grades and mappings
are reviewed study by study and may still change. A crosswalk row is
applied only once a reviewer has signed it off (status `"stable"`); rows
checked against the files but not yet signed off (status `"review"` or
`"draft"`) are applied only with `include_draft = TRUE`, and a message
says so. In the shipped spec every row but three is signed off, after an
automated double review against the original files and documents (not a
human review; the crosswalk's `reviewed_by` says so, and `review_note`
what the review corrected). The three others (the strength of provincial
party identification of `qes2022` and the previous provincial vote of
`qes2008` and `qes2018`) are in review, and applied only with
`include_draft = TRUE`. A row's sign-off is about its content: the
recommended weights that still need review (those of `qes1998`,
`qes2007_panel`, `qes2012_panel` and the CROP polls) do not hold the
rows of their waves, but are themselves `NA` until they are reviewed
(see *Weights*). A row in review has its values missing by default
(reason `not_reviewed`), and `qes_spec("crosswalk")$review_note` says
why it is held. When no cell of the result is applied for that reason, a
warning of class `qesR_warning_all_unreviewed` gives the number of
values left `NA` and names `include_draft = TRUE`; otherwise a message
counts the cells (study and target) and values left out.

Every result records the spec version and content hash
(`attr(, "qes_spec")`). The same spec version and hash, the same pinned
files and the same qesR version give the same data values and the same
study and cell provenance; only the `created` time of the spec-level
provenance (`qes_provenance(x, level = "spec")`) differs between calls.
To keep a spec fixed, copy `inst/extdata/harmonize/` of a qesR release
and pass its path as `spec`.

## Targets and comparability

A target is one question stimulus: a different wording, scale, timing or
format makes another target (vote intention and reported vote, the
"independent country" and "sovereign country" referendum questions, the
four-point and 0-10 interest scales are all separate targets). A pooled
variable (see *Pooled variables*) combines such targets into one column,
and records row by row which one each value comes from. Each study's
question gets a grade against the target's anchor question:

- `identical`: the same question, options and format;

- `comparable`: the same stimulus, with differences (option order,
  whether "don't know" is offered, minor parties listed) not expected to
  move the shares of the common answers;

- `approximate`: the same construct, in a format or with a filter
  expected to move them.

`min_grade` keeps only cells at or above a grade; the others become `NA`
with reason `below_grade`. By default approximate cells are included,
and a message lists them.

Answer options that a study's question did not offer (the Conservative
party before 2022, say) are *structural zeros*: their share in that
study is 0 because nobody could choose them, not because nobody
supported them. The factor levels are the same in every study, so these
levels are there with no respondent;
`qes_provenance(x, level = "cell")$levels_not_offered` lists them, and
printing the result names them.

## Missing values

Every missing value has a reason, in this order (the levels of the
`<target>__na` columns and the `n_<reason>` columns of
`qes_provenance(x, level = "cell")`); generated from the catalog
vocabulary:

- `dk`:

  Don't know.

- `refused`:

  Refused.

- `dk_refused`:

  Don't know or refused (one code).

- `no_answer`:

  No answer (item nonresponse).

- `not_selected`:

  Not selected (multiple choice).

- `inapplicable`:

  Inapplicable (routed out).

- `not_voted`:

  Did not vote.

- `spoiled`:

  Spoiled ballot.

- `ineligible`:

  Not eligible to vote.

- `not_registered`:

  Not on the list of electors.

- `not_in_wave`:

  Not in this wave.

- `not_mappable`:

  Source category straddles target levels.

- `sysmis`:

  System missing.

- `not_asked`:

  Not asked in this study.

- `not_reviewed`:

  Crosswalk row not yet signed off by a reviewer.

- `below_grade`:

  Below the requested grade.

- `unmapped`:

  Code not mapped.

`not_asked` means the study has no question for the target;
`not_reviewed` that it has one, whose crosswalk row is not yet signed
off by a reviewer (applied with `include_draft = TRUE`).
`missing = "reasons"` adds a factor column `<target>__na` with the
reason of each missing value.

## Pooled variables

A pooled variable is one column, for every study, that pools several
targets (its members) with a precedence among them. `vote_choice` is the
provincial vote choice: the reported vote (recall) where the study asked
it, else the vote intention with those who named no party pushed (the
undecided and, in some studies, those who would not vote or refused),
else the vote intention at the first question, whatever the wording of
each study. `sov_support` pools the referendum wordings on sovereignty
(an independent country, a sovereign country, the 1995 question, and,
collapsed to yes or no, being favourable to independence),
`pol_interest` the interest scales on 0 to 1 (four-point items scored 1,
0.7, 0.3 and 0; 0-10 items divided by 10) and `turnout` the reported
turnout (the likelihood of voting only when asked for). The first three
are in the default set `"core"`; `qes_spec("pooled")` lists their
members.

A row's value comes from the first member, by precedence, that asked the
respondent: a member that has a value, or a missing value that is an
answer (don't know, refused, did not vote), sets the row; a member that
did not ask the respondent, or whose answer cannot be used (not in the
wave, not asked in the study, not reviewed, below `min_grade`, routed
out, system missing, a code that straddles levels: `not_mappable`, such
as CROP's code 7 of the pushed intention), passes to the next. So a
respondent who did not vote is `NA` (reason `not_voted`) in
`vote_choice`, never given their earlier intention. When every member
passes, the row is `NA` with the reason, `__type`, `__grade` and
`__item` of the first usable member that has a row in the study and
wave, else of the first member that has a row: with
`min_grade = "comparable"`, a study whose only members are approximate
gets `NA` (reason `below_grade`) with that member's type and grade
(`approximate`), which say why the row is empty. With each pooled column
come `<pooled>__type` (the member's type name, such as `recall` or
`intention_push`), `<pooled>__grade` (the member row's grade, capped at
approximate for a transform that loses information, such as scoring a
four-point scale; never raised) and `<pooled>__item` (the source:
`<study>:<wave>:<variables>`), and `<pooled>__na` and `<pooled>__src`
with `missing = "reasons"` and `keep_source = TRUE`. `types` keeps some
members only, for example `types = list(vote_choice = "recall")`; the
precedence stays the spec's. The members are harmonized too, but are
columns of the result only when `targets` names them.

In the respondent layout (one row per respondent), a study's values of a
pooled variable come from one wave: that of the first member the study
applies (the post-election recall of a pre/post study, say), so that one
weight column fits them; the respondents of its other waves are `NA`
(reason `not_in_wave`). The long layout keeps every wave, each row with
the members of its own wave.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
then weights each study with the column of that wave. A message (class
`qesR_message_pooled`) and `qes_provenance(x, level = "pooled")` say
which member each study used, and how many rows it gave.

A target a study has no row for can be derived from other targets
(targets.csv `derive_rule`): the age groups `age_group3` and
`age_group6` from the age, else the year of birth (graded approximate:
an age at a band edge can be one year off), so that every study has an
age group. A direct row always wins, and cell provenance says
`rule = "derive:age_band"`.

## Data

By default each study is read from its pinned original file, checked by
md5, as
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads it. `data` gives the frames instead, for example ones you already
read: `data = list(qes2018 = get_qes("qes2018"))`. Their origin cannot
be verified, so a warning says so and `qes_provenance(x)$md5_verified`
is `FALSE`. Give them as
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
returns them (labelled codes, not factors). The synthetic study
`"qes_demo"` is harmonized with the rows of `qes2014`, whose variables
it copies; targets it has no question for are `not_asked`.

## Waves

Each study has one or more waves (a post-election survey, the waves of a
panel, the campaign-period and post-election surveys of 2022), declared
in the spec with the rule that says who took part in each: a disposition
or date variable, never an answer. A question belongs to the wave that
asked it, so a respondent outside that wave is `NA` with reason
`not_in_wave`. The respondent layout has one row per respondent, and
`waves` lists the waves each respondent took part in. The long layout
(`layout = "long"`) has one row per respondent and wave (a respondent in
no wave keeps one row, with `wave` `NA`): a value sits on the row of the
wave that asked it and the respondent's other rows are `not_in_wave`,
except the time-invariant targets (year and month of birth), which are
repeated on each of the respondent's rows.

Pooled polls (the monthly CROP polls of 2007-2010) have one wave per
poll: each respondent belongs to one poll, whose questions the spec maps
once for all the polls. In the same way, a question that does not change
over a panel (gender, education, mother tongue) is mapped once for all
its waves when each respondent answered it in whichever wave they took
part (the 2007 panel). Each poll refers to the next general election,
which `election_date` gives row by row, `year` is the year the poll
began, and its weight is normalized within the poll. `stratum` gives the
independent sample a respondent was drawn in, where a study pools
several: for pooled polls the poll's wave name (as in `waves`); for the
1998 panel the polling firm's code in the file, `firme_post` (`"1"` =
CREATEC, `"2"` = CROP; each firm interviewed francophones only).
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
uses it.

`interview_date` is the wave's interview date where the file has one
(`NA` otherwise; the fieldwork dates of each wave are in
`qes_spec("spec")$tables$waves`), and `days_to_election` the number of
days from it to the election (negative after the election).
`survey_mode` is `"web"`, `"phone"` or `"mixed"`: the wave's mode, or,
where it varies by respondent (the 2018 panel's first wave), the
respondent's own.

## Weights

Each wave has at most one recommended weight in the spec's registry, and
never one calibrated on the vote. The respondent layout has `weight_pre`
and `weight_post`, the recommended weights of the respondent's
pre-election and post-election waves (`NA` outside them), with the name
of the source variable in `weight_pre_var` and `weight_post_var`; the
long layout has `weight` and `weight_var`. `weights = "normalized"`
(default) divides each wave's weight by its mean over the wave's members
with a weight, so it has mean 1 in each study and wave; this fixes the
scale only (it does not give studies equal shares when pooled: see
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)).
`weights = "raw"` keeps the weights as deposited. A weight that is
registered but not accepted yet (registry status `needs_review`) is
`NA`, with a message, until it is reviewed; the raw variables are still
in the data read by
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
to join by `source_row`.

A pre-election question (vote intention) is weighted with `weight_pre`
and a post-election one (reported vote) with `weight_post`.
`attr(, "qes_weight_guide")` gives, for each target and study, the wave
that asked it and the weight column and variable that fit it, and a
message says when the targets of a study need different weights. For a
question that covers every wave of a panel (wave `"*"`, such as gender
in the 2007 panel), the guide names a weight only when those waves share
it; otherwise `weight_column` and `weight_var` are `NA`, since each
respondent's weight depends on the waves they took part in.

## Eligibility

`eligible_voter` says whether the respondent could vote in the study's
election: 18 or older on election day and, where the study asked, a
Canadian citizen. It comes from the age targets of the spec (year and
month of birth, age, age group) and the citizenship target, read
whatever `targets` and `min_grade` ask for (but, like any target, only
from rows signed off by a reviewer, or with `include_draft = TRUE`). It
is `TRUE` or `FALSE` only when the answers settle it: someone born 18
years before the election year whose month of birth is unknown, or aged
17 when interviewed before the election, is `NA`, as is a respondent
whose study has no age question in the spec yet. The 2018 study sampled
people aged 16 and over, so some of its respondents are `FALSE`. Its
weights target the population aged 16 and over; keeping only eligible
voters does not make them an 18-and-over calibration.

## Conditions

Besides those of every qesR function (see
[qesR-package](https://thomasgareau.github.io/qesR/reference/qesR-package.md)),
`qes_harmonize()` raises these classed conditions:

- errors: `qesR_error_unmapped` (a source code the spec does not map,
  with `unmapped = "error"`; fields `study`, `target`, `codes`, `n`,
  `unmapped`), `qesR_error_spec` (an invalid spec, or `data` that fail
  the spec's checks; field `problems`, a data frame of the failed rules)
  and `qesR_error_duplicate_id` (identifier variables that do not
  identify the rows of a study uniquely; fields `study`, `id_vars`,
  `rows`);

- warnings: `qesR_warning_all_unreviewed` (no cell applied, see
  `include_draft`), `qesR_warning_unmapped` (codes set to `NA` with
  `unmapped = "warn"`; field `unmapped`), `qesR_warning_partial`
  (studies left out with `on_fail = "skip"`; field `failed`) and
  `qesR_warning_unverified_source` (`data` not read by qesR from the
  pinned files; field `study`), and `qesR_warning_label_mismatch` and
  `qesR_warning_universe` (such `data` whose value labels differ from
  the pinned file's, or whose answers do not fit the question's
  universe; fields `study`, `problems`);

- messages, silenced by `quiet = TRUE`: `qesR_message_pooled` (which
  member of each pooled variable each study used; field `pooled`, the
  pooled provenance), `qesR_message_unreviewed_skipped` (cells left `NA`
  because their rows are not signed off),
  `qesR_message_unreviewed_cells` (cells that use rows not signed off,
  with `include_draft = TRUE`), `qesR_message_approximate_cells` (cells
  graded approximate are included), `qesR_message_structural_zeros`
  (levels a question did not offer), `qesR_message_weight_review`
  (recommended weights that need review, left `NA`) and
  `qesR_message_weight_timing` (targets of one study that need different
  weights; not sent when all of the study's weights need review).

[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
sends `qesR_message_design_dropped` (fields `study`, `n`) when it leaves
out rows without the chosen weight.

## En français

`qes_harmonize()` (expérimental) construit un seul tableau à partir de
plusieurs études, une colonne par variable harmonisée (« cible ») et une
ligne par répondant, en appliquant uniquement la spécification révisée :
une question par étude et par cible, ses codes appariés un à un aux
niveaux de la cible, et un motif pour chaque valeur manquante. Un code
non apparié est une erreur. Chaque cellule (étude, cible) porte un
niveau de comparabilité (`identical`, `comparable`, `approximate`) ;
`min_grade` écarte les cellules sous un niveau donné. Les options qu'une
question n'offrait pas sont des zéros structurels, pas un appui nul.
`lang = "fr"` donne les étiquettes des niveaux en français ; les codes
sont les mêmes. `targets = "decon"` demande les cibles des colonnes de
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md).
La disposition longue (`layout = "long"`) donne une ligne par personne
et par vague ; une question invariante d'un panel (genre, scolarité) est
lue dans la vague à laquelle la personne a participé (panel de 2007) ;
les sondages CROP regroupés ont une vague par sondage, et `stratum`
donne l'échantillon indépendant d'où vient la personne : pour les
sondages regroupés, le nom de la vague du sondage (comme dans `waves`) ;
pour le panel de 1998, le code de la firme dans le fichier, `firme_post`
(`"1"` = CREATEC, `"2"` = CROP). Chaque sondage se rapporte à l'élection
générale suivante, que `election_date` donne ligne par ligne, `year` est
l'année où il a commencé, et sa pondération est normalisée à l'intérieur
du sondage. Les pondérations recommandées de chaque vague sont dans
`weight_pre` et `weight_post` (ou `weight` en disposition longue),
ramenées à une moyenne de 1 par étude et par vague ; une pondération
encore à réviser (statut `needs_review`) vaut NA.
`attr(, "qes_weight_guide")` donne, pour chaque cible et étude, la
colonne de pondération qui convient ; pour une question qui couvre
toutes les vagues d'un panel (vague `"*"`), elle vaut NA quand ces
vagues n'ont pas la même pondération. `eligible_voter` indique si la
personne pouvait voter (18 ans le jour du scrutin et, là où l'étude l'a
demandé, citoyenneté canadienne) ; `interview_date`, `days_to_election`
et `survey_mode` décrivent l'entrevue.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
en fait un plan de sondage. Des résultats portant sur des études
différentes se combinent avec
[`rbind()`](https://rdrr.io/r/base/cbind.html) s'ils ont été construits
avec la même spécification, les mêmes `lang`, `layout`, `values`,
`missing` et `weights` et les mêmes colonnes (sinon, une erreur de
classe `qesR_error_input`). Une ligne de correspondance n'est appliquée
qu'une fois approuvée par un réviseur (statut `"stable"`) ;
`include_draft = TRUE` applique aussi les lignes vérifiées mais pas
encore approuvées (statut `"review"` ou `"draft"`). Dans la
spécification livrée, toutes les lignes sauf trois sont approuvées,
après une double révision automatisée sur les fichiers et documents
originaux (et non une révision humaine ; la colonne `reviewed_by` le
dit) ; les trois autres (la force de l'identification partisane
provinciale de `qes2022` et le vote provincial précédent de `qes2008` et
de `qes2018`) sont en révision. L'approbation d'une ligne porte sur son
contenu : les pondérations recommandées encore à réviser (celles de
`qes1998`, `qes2007_panel`, `qes2012_panel` et des sondages CROP) ne
retiennent pas les lignes de leurs vagues, mais valent elles-mêmes NA
jusqu'à leur révision. Sans `include_draft = TRUE`, les valeurs d'une
ligne en révision sont NA (motif `not_reviewed`),
`qes_spec("crosswalk")$review_note` dit pourquoi elle est retenue, et un
avertissement de classe `qesR_warning_all_unreviewed` le signale quand
tout le résultat est NA. Les autres conditions ont des classes (section
*Conditions*) : erreurs `qesR_error_unmapped`, `qesR_error_spec` et
`qesR_error_duplicate_id` (variables d'identification qui n'identifient
pas les lignes de façon unique) ; avertissements
`qesR_warning_unmapped`, `qesR_warning_partial` et
`qesR_warning_unverified_source` ; messages `qesR_message_*`, masqués
par `quiet = TRUE` ;
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
envoie `qesR_message_design_dropped` quand il écarte des lignes sans la
pondération choisie. Une variable regroupée (section *Pooled variables*)
réunit plusieurs cibles en une seule colonne pour toutes les études :
`vote_choice` est le vote déclaré là où l'étude l'a demandé, sinon
l'intention de vote avec relance des personnes qui n'ont nommé aucun
parti (les indécis et, dans certaines études, celles qui ne voteraient
pas ou refusaient), sinon l'intention de vote ; `sov_support` réunit les
libellés référendaires, `pol_interest` les échelles d'intérêt ramenées
de 0 à 1 et `turnout` la participation déclarée. Le premier membre, par
ordre de priorité, qui a interrogé la personne donne la valeur ;
`<variable>__type` indique ce membre, `<variable>__grade` son niveau de
comparabilité et `<variable>__item` sa question ;
`types = list(vote_choice = "recall")` ne garde que certains membres.
Les groupes d'âge sont dérivés de l'âge ou de l'année de naissance là où
l'étude n'a pas de question par tranches. Voir
[`vignette("fr-reference-harmonisation", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md).

## See also

[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
to use the weights in a survey design,
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
for the targets and the mapping of each study,
[`vignette("harmonization-reference", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md)
for the reference generated from the spec,
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
for where each value came from.

Other harmonization:
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md),
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md),
[`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md),
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)

## Examples

``` r
# the synthetic demonstration study, harmonized with the signed-off
# qes2014 rows (its gender row, still in review, is left NA)
h <- qes_harmonize("qes_demo")
#> Cells graded approximate are included (min_grade = "approximate"): qes_demo age_group3, qes_demo age_group6. Their question format is expected to move the shares; min_grade = "comparable" sets them to NA.
#> Levels a study's question did not offer are structural zeros, not an absence of support: vote_prov_recall: qes_demo (PCQ, ADQ); sov_indep: qes_demo (would_not_vote); gender: qes_demo (nonbinary, other). qes_provenance(x, level = "cell") lists them.
#> Pooled variables take each study's values from the first of their members, by precedence, that asked the respondent: vote_choice: recall (qes_demo); sov_support: independence (qes_demo); pol_interest: general_4pt (qes_demo). The __type column gives each row's member; qes_provenance(x, level = "pooled") counts them.
h
#> qesR harmonized data (experimental): 60 rows from 'qes_demo'; spec 4.5.0 (content hash 42816ecc38259a8deb01cd590d1c6a2b).
#> Approximate cells: qes_demo age_group3, qes_demo age_group6.
#> Structural zeros (levels not offered): vote_prov_recall: qes_demo (PCQ, ADQ); sov_indep: qes_demo (would_not_vote); gender: qes_demo (nonbinary, other).
#> Pooled variables (member types used, by study): vote_choice: recall (qes_demo); sov_support: independence (qes_demo); pol_interest: general_4pt (qes_demo).
#>      study year election_date family study_design     target_population waves
#> 1 qes_demo 2014    2014-04-07   demo         post None (synthetic data)  post
#> 2 qes_demo 2014    2014-04-07   demo         post None (synthetic data)  post
#> 3 qes_demo 2014    2014-04-07   demo         post None (synthetic data)  post
#> 4 qes_demo 2014    2014-04-07   demo         post None (synthetic data)  post
#> 5 qes_demo 2014    2014-04-07   demo         post None (synthetic data)  post
#> 6 qes_demo 2014    2014-04-07   demo         post None (synthetic data)  post
#>       qes_id subsample stratum source_row survey_mode interview_date
#> 1 qes_demo:1      <NA>    <NA>          1         web           <NA>
#> 2 qes_demo:2      <NA>    <NA>          2         web           <NA>
#> 3 qes_demo:3      <NA>    <NA>          3         web           <NA>
#> 4 qes_demo:4      <NA>    <NA>          4         web           <NA>
#> 5 qes_demo:5      <NA>    <NA>          5         web           <NA>
#> 6 qes_demo:6      <NA>    <NA>          6         web           <NA>
#>   days_to_election eligible_voter vote_prov_recall vote_prov_intent
#> 1               NA           TRUE              CAQ             <NA>
#> 2               NA           TRUE               QS             <NA>
#> 3               NA           TRUE              PLQ             <NA>
#> 4               NA           TRUE               PQ             <NA>
#> 5               NA           TRUE               QS             <NA>
#> 6               NA           TRUE              CAQ             <NA>
#>   vote_prov_intent_push turnout_prov_recall sov_indep sov_sovereign_country
#> 1                  <NA>                 Yes        No                  <NA>
#> 2                  <NA>                 Yes        No                  <NA>
#> 3                  <NA>                 Yes       Yes                  <NA>
#> 4                  <NA>                 Yes       Yes                  <NA>
#> 5                  <NA>                 Yes        No                  <NA>
#> 6                  <NA>                 Yes       Yes                  <NA>
#>   sov_favour lr_self pid_prov      interest_4pt birth_year age  age_group3
#> 1       <NA>       0     <NA> Hardly interested       1958  NA 55 and over
#> 2       <NA>       3     <NA> Hardly interested       1951  NA 55 and over
#> 3       <NA>       7     <NA>  Quite interested       1939  NA 55 and over
#> 4       <NA>      NA     <NA>   Very interested       1933  NA 55 and over
#> 5       <NA>       0     <NA>  Quite interested       1961  NA       35-54
#> 6       <NA>       6     <NA>   Very interested       1976  NA       35-54
#>   citizen sov_partnership_1995 pid_fed interest_0_10  age_group6 gender
#> 1    <NA>                 <NA>    <NA>            NA       55-64  Woman
#> 2    <NA>                 <NA>    <NA>            NA       55-64  Woman
#> 3    <NA>                 <NA>    <NA>            NA 65 and over    Man
#> 4    <NA>                 <NA>    <NA>            NA 65 and over    Man
#> 5    <NA>                 <NA>    <NA>            NA       45-54    Man
#> 6    <NA>                 <NA>    <NA>            NA       35-44  Woman
#>   education4 lang_mother born_canada vote_choice sov_support pol_interest
#> 1       <NA>        <NA>        <NA>         CAQ          No          0.3
#> 2       <NA>        <NA>        <NA>          QS          No          0.3
#> 3       <NA>        <NA>        <NA>         PLQ         Yes          0.7
#> 4       <NA>        <NA>        <NA>          PQ         Yes          1.0
#> 5       <NA>        <NA>        <NA>          QS          No          0.7
#> 6       <NA>        <NA>        <NA>         CAQ         Yes          1.0
#>   vote_choice__type vote_choice__grade vote_choice__item sov_support__type
#> 1            recall         comparable  qes_demo:post:Q3      independence
#> 2            recall         comparable  qes_demo:post:Q3      independence
#> 3            recall         comparable  qes_demo:post:Q3      independence
#> 4            recall         comparable  qes_demo:post:Q3      independence
#> 5            recall         comparable  qes_demo:post:Q3      independence
#> 6            recall         comparable  qes_demo:post:Q3      independence
#>   sov_support__grade sov_support__item pol_interest__type pol_interest__grade
#> 1          identical qes_demo:post:Q19        general_4pt         approximate
#> 2          identical qes_demo:post:Q19        general_4pt         approximate
#> 3          identical qes_demo:post:Q19        general_4pt         approximate
#> 4          identical qes_demo:post:Q19        general_4pt         approximate
#> 5          identical qes_demo:post:Q19        general_4pt         approximate
#> 6          identical qes_demo:post:Q19        general_4pt         approximate
#>   pol_interest__item weight_pre weight_post weight_pre_var weight_post_var
#> 1  qes_demo:post:Q28         NA   1.2593217           <NA>            POND
#> 2  qes_demo:post:Q28         NA   0.6685168           <NA>            POND
#> 3  qes_demo:post:Q28         NA   1.3786438           <NA>            POND
#> 4  qes_demo:post:Q28         NA   0.9836078           <NA>            POND
#> 5  qes_demo:post:Q28         NA   0.9091710           <NA>            POND
#> 6  qes_demo:post:Q28         NA   1.1417860           <NA>            POND
#> ... and 54 more row(s).
table(h$vote_prov_recall, useNA = "ifany")
#> 
#>         PLQ          PQ         CAQ          QS         PVQ         PCQ 
#>          14          13           8           7           1           0 
#>          ON         ADQ Other party        <NA> 
#>           0           0           1          16 

# why values are missing, and the grade of each cell
h <- qes_harmonize("qes_demo", targets = c("vote", "sov_indep"),
                   missing = "reasons", quiet = TRUE)
table(h$vote_prov_recall__na)
#> 
#>             dk        refused     dk_refused      no_answer   not_selected 
#>              0              5              0              0              0 
#>   inapplicable      not_voted        spoiled     ineligible not_registered 
#>              0             11              0              0              0 
#>    not_in_wave   not_mappable         sysmis      not_asked   not_reviewed 
#>              0              0              0              0              0 
#>    below_grade       unmapped 
#>              0              0 
cells <- qes_provenance(h, level = "cell")
cells[, c("study", "target", "source_var", "grade", "n_valid")]
#>      study                target source_var      grade n_valid
#> 1 qes_demo      vote_prov_recall         Q3 comparable      44
#> 2 qes_demo      vote_prov_intent       <NA>       <NA>       0
#> 3 qes_demo vote_prov_intent_push       <NA>       <NA>       0
#> 4 qes_demo   turnout_prov_recall         Q2 comparable      57
#> 5 qes_demo             sov_indep        Q19  identical      55
#> 6 qes_demo   turnout_prov_likely       <NA>       <NA>       0
#> 7 qes_demo        vote_prov_prev         Q6  identical       0
#> 8 qes_demo       vote_fed_recall       <NA>       <NA>       0

# French labels, same codes
h_fr <- qes_harmonize("qes_demo", targets = "interest_4pt", lang = "fr", quiet = TRUE)
levels(h_fr$interest_4pt)
#> [1] "Très intéressé(e)"        "Plutôt intéressé(e)"     
#> [3] "Pas très intéressé(e)"    "Pas du tout intéressé(e)"

# weights and eligibility: every question of the demonstration study was
# asked after the election, so weight_post is its weight (mean 1)
h <- qes_harmonize("qes_demo", targets = "sov_indep", quiet = TRUE)
h[1:3, c("study", "waves", "eligible_voter", "sov_indep", "weight_post", "weight_post_var")]
#>      study waves eligible_voter sov_indep weight_post weight_post_var
#> 1 qes_demo  post           TRUE        No   1.2593217            POND
#> 2 qes_demo  post           TRUE        No   0.6685168            POND
#> 3 qes_demo  post           TRUE       Yes   1.3786438            POND
attr(h, "qes_weight_guide")
#>      target    study wave target_timing weight_column weight_var weight_status
#> 1 sov_indep qes_demo post           any   weight_post       POND      reviewed

# one row per respondent and wave
l <- qes_harmonize("qes_demo", targets = "sov_indep", layout = "long", quiet = TRUE)
table(l$wave, l$wave_timing)
#>       
#>        post
#>   post   60

# one vote choice for every study: here the reported vote of the
# demonstration study (a qes2014 stand-in)
v <- qes_harmonize("qes_demo", targets = "vote_choice", missing = "reasons", quiet = TRUE)
table(v$vote_choice, v$vote_choice__type, useNA = "ifany")
#>                                      
#>                                       recall intention_push intention
#>   PLQ                                     14              0         0
#>   PQ                                      13              0         0
#>   CAQ                                      8              0         0
#>   QS                                       7              0         0
#>   PVQ                                      1              0         0
#>   PCQ                                      0              0         0
#>   ON                                       0              0         0
#>   ADQ                                      0              0         0
#>   Other party                              1              0         0
#>   Would not vote / none / would spoil      0              0         0
#>   <NA>                                    16              0         0
qes_provenance(v, level = "pooled")[, c("study", "type", "member", "grade", "n_value")]
#>      study           type                member      grade n_value
#> 1 qes_demo         recall      vote_prov_recall comparable      44
#> 2 qes_demo intention_push vote_prov_intent_push       <NA>       0
#> 3 qes_demo      intention      vote_prov_intent       <NA>       0
# recall only, or intentions only
v2 <- qes_harmonize("qes_demo", targets = "vote_choice",
                    types = list(vote_choice = "intention"), quiet = TRUE)

# which rows are signed off (stable), and which are still in review
xw <- qes_spec("crosswalk")
table(xw$status)
#> 
#> review stable 
#>      3    233 
xw[xw$status == "review", c("study", "target", "source_var", "grade")]
#>       study            target     source_var       grade
#> 171 qes2022 pid_prov_strength cps_provpidstr approximate
#> 175 qes2008    vote_prov_prev            q13  comparable
#> 176 qes2018    vote_prov_prev             q9  comparable
```
