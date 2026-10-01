# Build the Merged QES File

Reads 11 Quebec Election Studies, from 1998 to 2022, and stacks them in
one data frame: one row per respondent of each study, and the same 30
harmonized columns for every study.

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
  Defaults to the 11 studies of the merged file (`qes2022`, `qes2018`,
  `qes2018_panel`, `qes2014`, `qes2012`, `qes2012_panel`,
  `qes_crop_2007_2010`, `qes2008`, `qes2007`, `qes2007_panel` and
  `qes1998`); `"all"` on its own means the same 11. The 1998 firms' own
  files (`qes1998_crop`, `qes1998_createc`) raise an error: their
  respondents are in `qes1998`. Codes are trimmed and case-insensitive.
  `"qes_demo"` builds the master of the synthetic demonstration study,
  offline.

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

A data frame, returned visibly: the 30 documented columns, in a fixed
order and type, then the appended columns `vote_choice_timing`,
`sovereignty_item`, `family`, `study_design`, `waves`, `subsample`,
`source_row` (to join raw variables from
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)),
`weight_pre` and `weight_post` (the spec's recommended weights, as
deposited), `vote_intent`, `turnout_intent` and `sov_partnership_1995`.
Attributes:

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
  (no rows): kept so that code reading them keeps working;

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
  `"invalid_044_source"` (the source the first versions of qesR read for
  the column was verified wrong, and the spec has no row for it in the
  study), `"not_comparable_source"` (the study's only source is graded
  `not_comparable`), `"not_harmonized_yet"` or `"legacy_frozen"` (reason
  `"no_source"`: the study's question is now harmonized in
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  but the frozen column keeps its `NA`: `federal_pid` of `qes2012` and
  `language` of `qes2022`); `basis` says in words why the column is `NA`
  in the study. No value is blanked after it is read;

- `legacy_column_map`: what each column means (`column`, `target`,
  `definition`, `studies_changed`, `flag`, `note`, `render`);
  `studies_changed` lists the studies whose values differ from those of
  the first published version of the merged file (see
  [`vignette("migrating-0.7", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md)),
  and `flag` is `"approximate"` for columns that mix instruments;

- `removed_columns`: the names of the 70 columns that stacked same-named
  variables across studies, which are not built;

- `qes_provenance`: the file read for each study, with the cell and spec
  levels (see
  [`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md));

- `qes_spec`: the spec version and content hash that built the data;

- `saved_to`: the output path when `save_path` is given.

## Details

`get_qes_master()` has a fixed layout: its arguments, its first 30
columns (in their order) and their types do not change, so that code
written against it keeps working; columns added later come after the 30
and are never removed. It is a quick, flat overview. For an analysis you
will publish, prefer
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
(experimental), which keeps intention and recall, the sovereignty
wordings and the four-point and 0-10 interest scales apart, gives every
missing value a reason and every cell a comparability grade, or the
study files themselves
([`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)).
[`vignette("migrating-0.7", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md)
says how its values compare with those of earlier versions of qesR, and
how to reproduce those.

`get_qes_master()` returns the data and assigns nothing unless
`assign_global = TRUE`: write `master <- get_qes_master()`. The first
call in a session that leaves `assign_global` unset prints a one-time
note saying so. The first call in a session also prints one-time notes
on how its values and columns compare with those of earlier versions
(classes `qesR_message_values_changed` and
`qesR_message_legacy_columns`).

## How it is built

Each study is harmonized by
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
from its pinned original file (checked by md5), and each column is
rendered from the targets it needs, as the renderer table of the spec
says (`qes_spec("spec")$tables$legacy`): for example `vote_choice` is
the reported vote (target `vote_prov_recall`) with the master's own
party labels, `sovereignty_support` is 1 or 0 for the referendum on an
independent country (`sov_indep`), and `political_interest` puts the
four-point interest items on 0-10 as 10, 7, 3 and 0. A code the spec
does not map is `NA`, never passed through. Every row of every file is
kept: there is no de-duplication and no removal of empty rows, so each
study contributes exactly its number of respondents (`qes2007_panel`:
2,442 rows). Variables that share a name across studies are not stacked:
they often hold different questions; read them from each study with
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md).

The engine applies only the crosswalk rows signed off by a reviewer
(status `stable`), as
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
does by default. The rows were signed off after an automated double
review against the original files and documents (not a human review); a
row that nobody has reviewed yet is never read. A row still in review
would not be applied, and the columns it would fill would be `NA`:
`attr(, "legacy_na_columns")` lists such columns with the reason
`not_reviewed` and says why each row is held, and a message names them.
The recommended weights that still need review (those of `qes1998`,
`qes2007_panel`, `qes2012_panel` and the CROP polls, registered but not
accepted yet: `qes_spec("spec")$tables$weights` says what is known of
each) are not applied: `weight_pre` and `weight_post` are `NA` there,
with the reason `not_reviewed` and the cause `weight_needs_review`.
`survey_weight` is each study's own weight, on its own scale, and is not
reviewed: in some studies it is a weight that needs review
(`qes2012_panel`, the CROP polls) or one calibrated on the vote
(`qes2008`), and
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
never uses it; the note of each study's row of
`qes_spec("spec")$tables$legacy` says which weight it is. Do not pool
weighted estimates across studies with it. `attr(, "source_map")` gives
the grade and review status of each column's question in each study, and
`attr(, "legacy_column_map")` says what each column holds.

## En français

`get_qes_master()` empile 11 Études électorales québécoises, de 1998 à
2022, dans un seul tableau : une ligne par répondant de chaque étude, et
les mêmes 30 colonnes harmonisées pour toutes les études. Son format est
fixe : mêmes arguments, mêmes 30 premières colonnes dans le même ordre,
mêmes types ; les colonnes ajoutées par la suite suivent les 30 et ne
seront jamais retirées. C'est une vue d'ensemble rapide et à plat ; pour
une analyse que vous publierez, préférez
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
qui garde séparés l'intention et le vote déclaré, les formulations de la
souveraineté et les échelles d'intérêt, et donne un motif à chaque
valeur manquante et un niveau de comparabilité à chaque cellule.
[`vignette("fr-migrer-0.7", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md)
compare ses valeurs à celles des versions antérieures de qesR et dit
comment reproduire celles-ci.

Chaque colonne est rendue à partir des cibles de la spécification
d'harmonisation, selon la table `legacy` de la spécification
(`qes_spec("spec")$tables$legacy`). `vote_choice` et `turnout` sont le
vote et la participation déclarés après l'élection dans toutes les
études qui les ont demandés (les sondages CROP n'ont demandé que
l'intention de vote : `NA`) ; `sovereignty_support` vaut 1 ou 0 pour le
référendum sur un pays indépendant ; `political_interest` place les
échelles d'intérêt en quatre points sur 0-10 (10, 7, 3 et 0). Un code
que la spécification n'apparie pas vaut `NA`. Aucune ligne n'est retirée
(`qes2007_panel` : 2 442 lignes), et les variables de même nom d'une
étude à l'autre ne sont pas empilées. `attr(, "legacy_column_map")`
décrit chaque colonne et `attr(, "source_map")` donne la question de
chaque colonne dans chaque étude, avec son niveau de comparabilité et
son statut de révision. Seules les lignes de correspondance approuvées
par un réviseur (statut `stable`) sont appliquées, comme le fait
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
par défaut ; elles ont été approuvées après une double révision
automatisée sur les fichiers et documents originaux (et non une révision
humaine) ; une ligne que personne n'a encore révisée n'est jamais lue.
Les colonnes d'une ligne encore en révision vaudraient `NA` ;
`attr(, "legacy_na_columns")` les énumérerait avec le motif
`not_reviewed` en disant pourquoi la ligne est retenue, et un message
les nommerait. Les pondérations recommandées encore à réviser (celles de
`qes1998`, `qes2007_panel`, `qes2012_panel` et des sondages CROP,
enregistrées mais pas encore acceptées) ne sont pas appliquées :
`weight_pre` et `weight_post` y valent `NA`, avec le motif
`not_reviewed` et la cause `weight_needs_review`. `survey_weight` est la
pondération propre à chaque étude, sur sa propre échelle, et n'est pas
révisée : dans certaines études, c'est une pondération à réviser
(`qes2012_panel`, sondages CROP) ou calée sur le vote (`qes2008`), et
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
ne l'utilise jamais ; la note de la ligne de chaque étude dans
`qes_spec("spec")$tables$legacy` dit laquelle. Ne combinez pas
d'estimations pondérées entre études avec elle.

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
