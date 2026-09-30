# Build the Legacy Stacked Master QES Dataset

Reads the Quebec Election Studies of qesR 0.4.4 and stacks them in one
data frame with the 0.4.4 columns: one row per respondent of each study,
and the same 30 harmonized columns for every study.

## Usage

``` r
get_qes_master(
  surveys = NULL,
  assign_global = FALSE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
)
```

## Arguments

- surveys:

  Character vector of qesR survey codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)).
  Defaults to the 11 studies of qesR 0.4.4; `"all"` on its own means the
  same 11. The 1998 firms' own files (`qes1998_crop`, `qes1998_createc`)
  raise an error: their respondents are in `qes1998`. Codes are trimmed
  and case-insensitive. `"qes_demo"` builds the master of the synthetic
  demonstration study, offline.

- assign_global:

  If TRUE, also assign the result as `object_name` into the environment
  `get_qes_master()` was called from (the global environment only when
  called at top level), after `saved_to` is set. Defaults to FALSE.

- object_name:

  Object name used when `assign_global = TRUE`. Defaults to
  `"qes_master"`.

- quiet:

  If TRUE, suppress informational output while downloading.

- strict:

  If TRUE, stop when any study fails. If FALSE, return partial results
  and record failures in attributes.

- save_path:

  Optional output path for writing the master file: `.rds` writes an RDS
  file, any other extension a UTF-8 CSV file. The provenance of every
  study read is written next to it, as `<stem>_provenance.csv`.

## Value

A data frame, returned visibly: the 30 documented columns of qesR 0.4.4
in their order and type, then the appended columns `vote_choice_timing`,
`sovereignty_item` (0.5.0), `family`, `study_design`, `waves`,
`subsample`, `source_row`, `weight_pre`, `weight_post`, `vote_intent`,
`turnout_intent` and `sov_partnership_1995` (0.7.0). Attributes:

- `source_map`: the source of every column of every study (`qes_code`,
  `qes_year`, `qes_name_en`, `harmonized_variable`, `source_variable`,
  `target`, `map_id`, `grade`, `status` (of the crosswalk row: `stable`,
  or `review` for a row held in review), `render`, `file_md5`,
  `spec_version`);

- `loaded_surveys`, `failed_surveys`: the studies read, and one line per
  failed study, `"<code>: <reason>"`. qesR's own part of the reason is
  always in English, whatever the message language; a root cause raised
  by R itself (for example a download error) keeps the text R reported.
  The full conditions are in the `failures` field of the `strict = TRUE`
  error;

- `duplicates_removed` and `empty_rows_removed`: always `0L`;

- `harmonized_variables`: the columns after `qes_code`, `qes_year` and
  `qes_name_en`;

- `crossstudy_variables_added` (always empty) and `variable_name_map`
  (no rows): kept for code written for 0.4.4;

- `legacy_na_columns`: one row per column and study whose cells are all
  `NA` (`column`, `study`, `reason`, `n_cells`, `cause`, `basis`);
  `reason` is `"no_source"` (the spec has no applied question for it in
  the study, the file lacks the variable, or the study has no registered
  weight), `"not_reviewed"` (the study has the question, but its
  crosswalk row is not signed off by a reviewer yet: `cause` is then
  `"not_signed_off"` and `basis` says why the row is held; or, for
  `weight_pre` and `weight_post`, the study's recommended weight is
  registered but still needs review: `cause` is then
  `"weight_needs_review"` and `basis` names the weight), `"na_column"`
  (no valid source in any study) or `"all_missing"` (the question's
  every answer is a missing value); `cause` names the rule behind it
  where there is one: `"reported_vote_only"` (`vote_choice` and
  `turnout` hold only the reported vote and turnout, which the CROP
  polls did not ask), `"independence_question_only"`
  (`sovereignty_support` and `sovereignty` hold only the referendum
  question on an independent country, which the study did not ask),
  `"no_valid_source"` (no study has a valid source for the column),
  `"invalid_044_source"` (the source qesR 0.4.4 read was verified wrong
  in 0.5.0, and the spec has no row for the column in the study since),
  `"not_comparable_source"` (the study's only source is graded
  `not_comparable`), `"not_harmonized_yet"` or `"legacy_frozen"` (reason
  `"no_source"`: the study's question is harmonized since spec 4.3.0, in
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  but the legacy column keeps the `NA` of qesR 0.7.1: `federal_pid` of
  `qes2012` and `language` of `qes2022`); `basis` says in words why the
  column is `NA` in the study. 0.5.0's `"blanked"` reason is gone: no
  value is blanked after it is read;

- `legacy_column_map`: what each column means (`column`, `target`,
  `definition`, `studies_changed`, `flag`, `note`, `render`);
  `studies_changed` lists the studies whose values differ from qesR
  0.4.4, and `flag` is `"approximate"` for columns that mix instruments;

- `removed_columns`: the names of the 70 columns no longer built;

- `qes_provenance`: the file read for each study, with the cell and spec
  levels (see
  [`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md));

- `qes_spec`: the spec version and content hash that built the data;

- `saved_to`: the output path when `save_path` is given.

## Details

`get_qes_master()` is the fixed legacy schema of qesR 0.4.4. It is kept
stable, with the same arguments, columns (in the same order) and column
types, so that code written for 0.4.4 keeps working; columns added since
are appended after the 30 and are never removed. New work should use the
harmonization engine
([`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
experimental), which keeps intention and recall, the sovereignty
wordings and the four-point and 0-10 interest scales apart, gives every
missing value a reason and every cell a comparability grade, or the
study files themselves
([`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)).

`get_qes_master()` returns the data and assigns nothing unless
`assign_global = TRUE`: write `master <- get_qes_master()`. The first
call in a session that leaves `assign_global` unset prints a one-time
note about this change from qesR 0.4.4.

## How it is built

Since qesR 0.7.0 the master is rendered from the harmonization engine.
Each study is harmonized by
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
from its pinned original file (checked by md5), and each legacy column
is rendered from the targets it needs, as the renderer table of the spec
says (`qes_spec("spec")$tables$legacy`): for example `vote_choice` is
the reported vote (target `vote_prov_recall`) with the party labels of
0.4.4, `sovereignty_support` is 1 or 0 for the referendum on an
independent country (`sov_indep`), and `political_interest` puts the
four-point interest items on 0-10 as 10, 7, 3 and 0. A code the spec
does not map is `NA`, never passed through. Every row of every file is
kept: there is no de-duplication and no removal of empty rows, so each
study contributes exactly its number of respondents (`qes2007_panel`:
2,442 rows).

The engine applies only the crosswalk rows signed off by a reviewer
(status `stable`), as
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
does by default. The rows were signed off after an automated double
review against the original files and documents (not a human review);
since spec 4.1.0 every reviewed row is signed off, and the rows added in
spec 4.3.0, not reviewed yet, are never read (the legacy columns stay as
they were). A row still in review would not be applied, and the columns
it would fill would be `NA`: `attr(, "legacy_na_columns")` lists such
columns with the reason `not_reviewed` and says why each row is held,
and a message names them. The recommended weights that still need review
(those of `qes1998`, `qes2007_panel`, `qes2012_panel` and the CROP
polls, registered but not accepted yet:
`qes_spec("spec")$tables$weights` says what is known of each) are not
applied: `weight_pre` and `weight_post` are `NA` there, with the reason
`not_reviewed` and the cause `weight_needs_review`. `survey_weight`
keeps each study's own weight, as in qesR 0.4.4, and is not reviewed: in
some studies it is a weight that needs review (`qes2012_panel`, the CROP
polls) or one calibrated on the vote (`qes2008`), and
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
never uses it; the note of each study's row of
`qes_spec("spec")$tables$legacy` says which weight it is.
`attr(, "source_map")` gives the grade and review status of each
column's question in each study, and `attr(, "legacy_column_map")` says
what each column holds.

## What changed in 0.7.0

Compared with 0.5.0 (NEWS has a table of counts, column by column, for
qesR 0.4.4, 0.5.0 and 0.7.0):

- `vote_choice` and `turnout` are the reported vote and turnout in every
  study that asked them: `qes2022` from its post-election wave,
  `qes1998` from its post-election recontact (once its rows are signed
  off); the pooled CROP polls asked only vote intentions and stay `NA`.

- Only crosswalk rows signed off by a reviewer are applied (see "How it
  is built"): the columns of the rows still in review are `NA`.

- `income` of `qes2022`: an amount of 0 (a blank that the survey sent to
  a follow-up question in brackets) is `NA`. Nonvoters, spoiled ballots,
  "don't know" and refusals are `NA` in `vote_choice` (0.4.4 had the
  categories "Did not vote / None" and "Don't know / Refused");
  `turnout` says who voted.

- Columns filled where the spec has the question: `ideology` for
  `qes2014` (with its 0 and 10 answers) and `qes2018`,
  `political_interest` for `qes2018` (the four-point item as 10, 7, 3
  and 0), `born_canada` for `qes2018`, `provincial_pid` for `qes2007`,
  `qes2008`, `qes2012`, `qes2014` and `qes2018`, `language` for
  `qes2014` (the mother tongue, not the language of the interview),
  `income` for `qes2012`, `year_of_birth` for `qes2022`, and `age` for
  the `qes2018` respondents who gave their age but not their year of
  birth.

- `age_group` has six bands in the 2007 and 2012 panels, whose question
  has them (0.4.4 used a three-band recode); `education` maps the labels
  0.4.4 left as they were (`maîtrise` is University; the 2018 panel's
  trade certificates are College/CEGEP/Technical) and, for `qes2012`,
  puts the French questionnaire's *cours technique* in College and
  leaves the one code that is in neither questionnaire `NA`; `language`
  is `NA` for respondents who report two first languages (they are
  assigned to neither).

- `respondent_id` joins each study's identifier variables:
  `qes2007_panel` (project and questionnaire number, unique) and
  `qes2018_panel` (interview mode and id, where 0.5.0 made one up).

- `province_territory` is "Quebec" in every study (0.4.4 put the
  `qes2018_panel` region there).

- Columns appended: `family`, `study_design`, `waves`, `subsample`,
  `source_row` (to join raw variables from
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)),
  `weight_pre` and `weight_post` (the spec's recommended weights, as
  deposited; `NA` where they are not reviewed yet), `vote_intent`,
  `turnout_intent` and `sov_partnership_1995`.

Results from qesR 0.4.4 can be reproduced only by installing that
version
(`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`). 0.5.0
and 0.6.0 were development versions and were never released: a result
computed with one of them is reproducible by installing the commit it
was built from, which `packageDescription("qesR")$RemoteSha` records for
an installation from GitHub.

A message says so once per session (class `qesR_message_values_changed`,
and `qesR_message_legacy_columns` for the 70 columns 0.4.4 stacked by
name, gone since 0.5.0).

## En français

`get_qes_master()` est le fichier fusionné hérité de qesR 0.4.4 : mêmes
arguments, mêmes 30 colonnes dans le même ordre, mêmes types ; les
colonnes ajoutées depuis suivent les 30 et ne seront jamais retirées.
Depuis qesR 0.7.0, il est produit par le moteur d'harmonisation
([`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md))
: chaque colonne est rendue à partir des cibles de la spécification,
selon la table `legacy` de la spécification
(`qes_spec("spec")$tables$legacy`). `vote_choice` et `turnout` sont le
vote et la participation déclarés après l'élection dans toutes les
études qui les ont demandés (les sondages CROP n'ont demandé que
l'intention de vote : `NA`) ; `sovereignty_support` vaut 1 ou 0 pour le
référendum sur un pays indépendant ; `political_interest` place les
échelles d'intérêt en quatre points sur 0-10 (10, 7, 3 et 0). Un code
que la spécification n'apparie pas vaut `NA`. Aucune ligne n'est retirée
(`qes2007_panel` : 2 442 lignes). `attr(, "legacy_column_map")` décrit
chaque colonne et `attr(, "source_map")` donne la question de chaque
colonne dans chaque étude, avec son niveau de comparabilité et son
statut de révision. Seules les lignes de correspondance approuvées par
un réviseur (statut `stable`) sont appliquées, comme le fait
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
par défaut ; elles ont été approuvées après une double révision
automatisée sur les fichiers et documents originaux (et non une révision
humaine), et toutes les lignes révisées le sont depuis la spécification
4.1.0 ; les lignes ajoutées dans la 4.3.0, pas encore révisées, ne sont
jamais lues. Les colonnes d'une ligne encore en révision vaudraient `NA`
; `attr(, "legacy_na_columns")` les énumérerait avec le motif
`not_reviewed` en disant pourquoi la ligne est retenue, et un message
les nommerait. Les pondérations recommandées encore à réviser (celles de
`qes1998`, `qes2007_panel`, `qes2012_panel` et des sondages CROP,
enregistrées mais pas encore acceptées) ne sont pas appliquées :
`weight_pre` et `weight_post` y valent `NA`, avec le motif
`not_reviewed` et la cause `weight_needs_review`. `survey_weight` garde
la pondération propre à chaque étude, comme dans qesR 0.4.4, et n'est
pas révisée : dans certaines études, c'est une pondération à réviser
(`qes2012_panel`, sondages CROP) ou calée sur le vote (`qes2008`), et
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
ne l'utilise jamais ; la note de la ligne de chaque étude dans
`qes_spec("spec")$tables$legacy` dit laquelle.

Autres changements de 0.7.0 par rapport à 0.5.0 :

- les abstentionnistes, bulletins rejetés, « ne sait pas » et refus
  valent `NA` dans `vote_choice` ;

- `income` de `qes2022` : un montant de 0 (un champ laissé vide, que le
  questionnaire renvoyait à une question de relance par tranches) vaut
  `NA` ;

- des colonnes sont remplies là où la spécification a la question
  (`ideology`, `political_interest`, `born_canada`, `provincial_pid`,
  `language` de 2014 : la langue maternelle, `income` de 2012,
  `year_of_birth` de 2022) ;

- `age_group` a six tranches dans les panels de 2007 et de 2012 ;
  `education` classe les libellés que 0.4.4 laissait tels quels
  (`maîtrise` : University ; certificat de métier :
  College/CEGEP/Technical) et, pour `qes2012`, met le *cours technique*
  dans College ; `language` vaut `NA` pour qui déclare deux premières
  langues (attribuées à aucune des deux) ;

- `respondent_id` joint les variables d'identification de l'étude
  (`qes2007_panel` et `qes2018_panel`) ;

- `province_territory` vaut « Quebec » dans toutes les études ;

- colonnes ajoutées : `family`, `study_design`, `waves`, `subsample`,
  `source_row` (pour joindre des variables brutes de
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)),
  `weight_pre` et `weight_post` (pondérations recommandées de la
  spécification, telles que déposées ; `NA` si elles ne sont pas encore
  révisées), `vote_intent`, `turnout_intent` et `sov_partnership_1995`.

Les résultats de qesR 0.4.4 ne se reproduisent qu'en installant cette
version (`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`)
; 0.5.0 et 0.6.0 étaient des versions de développement, jamais publiées
: un résultat calculé avec l'une d'elles se reproduit en installant le
commit dont il provient, que `packageDescription("qesR")$RemoteSha`
enregistre pour une installation depuis GitHub. Pour de nouvelles
analyses, utilisez
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md).

## See also

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
for harmonized data with grades and reasons,
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
for the study files,
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
and
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
to record and cite the files read.

Other data:
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)

## Examples

``` r
# the synthetic demonstration study, offline
demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
#> get_qes_master() no longer appends the 70 columns that qesR 0.4.4 built by stacking variables that share a name across studies; attr(, "removed_columns") lists them. Read those items from each study with get_qes(). This note is shown once per session.
#> get_qes_master() returns its result and no longer assigns it into your workspace by default. Write `qes_master <- get_qes_master(...)`, or pass `assign_global = TRUE`. This note is shown once per session.
head(demo_master[, c("qes_code", "gender", "turnout", "vote_choice")])
#>   qes_code gender turnout vote_choice
#> 1 qes_demo  Woman       1         CAQ
#> 2 qes_demo  Woman       1          QS
#> 3 qes_demo    Man       1         PLQ
#> 4 qes_demo    Man       1          PQ
#> 5 qes_demo    Man       1          QS
#> 6 qes_demo  Woman       1         CAQ
attr(demo_master, "legacy_column_map")[1:5, c("column", "target", "definition")]
#>            column target
#> 1        qes_code   <NA>
#> 2        qes_year   <NA>
#> 3     qes_name_en   <NA>
#> 4   respondent_id   <NA>
#> 5 interview_start   <NA>
#>                                                                                                                                                            definition
#> 1                                                                                                                                                    qesR study code.
#> 2                                                                                        Year of the study; for qes_crop_2007_2010, the year each monthly poll began.
#> 3                                                                                                                   English name of the study, from the qesR catalog.
#> 4 Respondent identifier: the study's identifier variables joined by '-' (qesR's qes_id without its study prefix), or <study>_<row> where the file has none (qes2014).
#> 5                                                                        Interview start date-time as text, where the file has one (qes2022 and, as a date, qes2014).
```
