# The codebook of a study

`qes_codebook()` describes every variable of a study: its label, the
question asked (in English or French), its value labels and which codes
mean "don't know", "refused" or another reason for a missing answer. The
description of every study ships with qesR, so the codebook needs no
download and no network.

## Usage

``` r
qes_codebook(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long"),
  variables = NULL,
  lang = NULL
)
```

## Arguments

- srvy:

  A study code from
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
  (trimmed and case-insensitive), or an object to describe again: a
  codebook returned by `qes_codebook()` (laid out again with `layout`),
  or a data frame returned by
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
  whose codebook is limited to the columns it still has (base R's `[`
  drops the attributes that record the study; a subset made with it is a
  plain data frame). A plain data frame (for example a codebook read
  back from a CSV file, which has lost its class) is an error of class
  `qesR_error_input`: rebuild it with `qes_codebook("<code>")`.

- file:

  Optional regular expression choosing which data file of the study the
  codebook describes, as in
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md).
  The description is that of the pinned file, with the pinned file's
  variable names (the attribute `variable_names_file` names that file):
  the twin files of a deposit hold the same variables, but their names
  can differ in case (`qes2012`'s SPSS twin has `Q0QC` where the pinned
  Stata file has `q0qc`). For a codebook whose names match data read
  with `get_qes(file = )`, describe that data: `qes_codebook(<data>)`,
  or use the codebook
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  attaches.

- assign_global:

  If TRUE, also assign the returned codebook as `<code>_codebook` into
  the environment the function was called from (the global environment
  only when called at top level), where `<code>` is the canonical study
  code. Defaults to FALSE.

- quiet:

  If TRUE, suppress informational output.

- refresh:

  Ignored: the description is shipped with qesR. Setting it prints a
  one-time note.

- layout:

  `"compact"` (default: one row per variable), `"wide"` (the same, with
  the value labels as a list column) or `"long"` (one row per value).

- variables:

  Optional character vector of variable names (exact match) to describe;
  an unknown name is an error of class `qesR_error_unknown_variable`
  that suggests near matches.

- lang:

  Language of the question text: `NULL` (default) gives the study's own
  language (French for most studies, English for `qes2012` and
  `qes2022`), or the other language when only that one is known; `"en"`
  or `"fr"` gives that language only (`NA` where it is unknown). Labels
  are always those of the file.

## Value

A data frame of class `qes_codebook`, returned visibly.

With `layout = "compact"`, one row per variable: `variable`, `label`
(the file's variable label), `question`, `n_value_labels` (the columns
of qesR 0.4.4, first and in that order), then `study`, `position` (in
the data), `type` (`numeric`, `character`, `date`, `datetime` or
`logical`), `question_lang`, `question_truncated`, `value_labels`
(`"1=Oui | 2=Non"`), `missing_codes` (`"8=dk | 9=refused"`), `targets`
(the harmonized targets the variable feeds in
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
`;`-separated, `NA` for none, as in
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)),
`label_source` (`file`, `label_donor`, `supplement`, `file_malformed` or
`none`), `question_source` and `doc_ref`. `layout = "wide"` has the same
columns with `value_labels` as a list of named character vectors.
`layout = "long"` has one row per value: `variable`, `value`,
`value_label`, `label`, `question`, then `study`, `missing_type` and
`is_declared_na` (the code is declared missing in the SPSS file); a
variable with no value labels keeps one row whose `value` is `NA`.

Every layout has the attributes `survey_code`, `doi`, `doi_url`,
`selected_data_file` (the Dataverse name of the data file described: for
an ingested file, the `.tab` copy, as in qesR 0.4.4; the header printed
by the codebook also names the original `.sav` or `.dta` that
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads and
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
reports), `variable_names_file` (the data file whose variable names the
codebook uses), `files` (the study's files), `codebook_files` (its
documents, as
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
lists them) and `qes_provenance`. The codebook of `qes2022` also has the
attribute `licence_notice`: the attribution and licence (CC BY-NC 4.0)
of its description, named by the study; keep it with any copy you share.

## Where the description comes from

Variable and value labels are those of the study's pinned data file, as
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads it (the complete labels of the SPSS twin for `qes2012`). A label
that only repeats the variable's name is not a label: `label` is then
`NA`. Question text comes from the questionnaires deposited with the
study (`question_source = "questionnaire"`; `doc_ref` holds one or more
`<file_id>:<question>` references separated by `;`, e.g.
`"352010:Q19;352009:Q19"`), and for `qes2022` from its bilingual
codebook (`question_source = "codebook"`, `doc_ref`
`"7449514:<question>"`; the item of a grid is its stem followed by the
item in brackets); `question` is `NA` when the document does not give
it, never a copy of the variable name or of `label`, and never a machine
translation. `qes2018`'s data file has no value labels for most
variables: they come from the study's French questionnaire with its
programmed answer codes (`label_source = "supplement"`).

The CC0 studies' description is in the public domain. The description of
`qes2022` (its labels, question text and answer counts) is derived from
the 2022 Quebec Election Study and carries its licence, CC BY-NC 4.0:
cite the study (`qes_cite("qes2022")`) and do not use it commercially
(see the file `COPYRIGHTS` of the installed package). A printed codebook
of `qes2022` ends its header with this licence and the attribution it
requires, which its attribute `licence_notice` also holds, so that a
saved codebook keeps it. Its variable labels are those of its Stata
file, which cuts them at 80 characters; its question text, from the
codebook, is not cut.

## En français

`qes_codebook()` décrit chaque variable d'une étude : son étiquette, la
question posée (en anglais ou en français), ses étiquettes de valeurs et
les codes qui signifient « ne sait pas », « refus » ou une autre raison
de non-réponse. La description de chaque étude est livrée avec qesR :
aucun téléchargement n'est nécessaire. Le texte des questions vient des
questionnaires déposés et, pour `qes2022`, de son livre de codes
bilingue (`doc_ref` : une ou plusieurs références `<file_id>:<question>`
séparées par `;`) ; il vaut `NA` lorsqu'il est inconnu, sans traduction
automatique. `lang = "fr"` donne le texte français. La description de
`qes2022` (étiquettes, texte des questions, effectifs) est tirée de
l'Étude électorale québécoise 2022 et reste sous sa licence, CC BY-NC
4.0 : citez l'étude (`qes_cite("qes2022")`) et n'en faites pas d'usage
commercial (voir le fichier `COPYRIGHTS` du package installé) ; le
codebook imprimé de `qes2022` rappelle cette licence et l'attribution
qu'elle exige, que garde aussi son attribut `licence_notice` (à
conserver avec le codebook enregistré). L'attribut `selected_data_file`
garde le nom Dataverse du fichier décrit (la copie `.tab` d'un fichier
ingéré, comme dans qesR 0.4.4) ; l'en-tête affiché nomme aussi le
fichier original (`.sav` ou `.dta`) que
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
lit et que
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
indique.

## See also

[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
for the exact wording of some variables,
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
to find variables across studies,
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
to set the missing codes to `NA`.

Other codebooks and search:
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md),
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md),
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)

## Examples

``` r
cb <- qes_codebook("qes2014")
head(cb[, c("variable", "label", "question", "value_labels")])
#> <qes_codebook>
#> Variables: 6 
#> Codebook/support files: 0 
#>   variable                                    label
#> 1    QUEST                                     <NA>
#> 2     SDAT                          Date d'entrevue
#> 3     LANG Préfèreriez-vous répondre à ce questi...
#> 4    GREET Ce sondage en ligne est mené au nom d...
#> 5     QAGE En quelle année êtes-vous né(e)? / En...
#> 6    SMAGE                                      Age
#>                                   question                    value_labels
#> 1                                     <NA>                            <NA>
#> 2                                     <NA>                            <NA>
#> 3 Préfèreriez-vous répondre à ce questi...        EN=English | FR=Français
#> 4                                     <NA>                      1=continue
#> 5         En quelle année êtes-vous né(e)? 9999=Je préfère ne pas répondre
#> 6                                     <NA>                            <NA>

# one row per value, for two variables, with the English question text
qes_codebook("qes2014", layout = "long", variables = c("Q2", "Q19"), lang = "en")
#> <qes_codebook> survey: qes2014
#> DOI: 10.5683/SP3/64F7WR 
#> Data file: Quebec Election Study 2014.sav (Dataverse: Quebec Election Study 2014 (SPSS).tab) 
#> Variables: 2 
#> Rows: 7 
#> Codebook/support files: 3 
#>   variable value                value_label
#> 1       Q2     1                        Oui
#> 2       Q2     2                        Non
#> 3       Q2     9 Je préfère ne pas répondre
#> 4      Q19     1                        Oui
#> 5      Q19     2                        Non
#> 6      Q19     8             Je ne sais pas
#> 7      Q19     9 Je préfère ne pas répondre
#>                                      label
#> 1 Avez-vous voté à cette élection provi...
#> 2 Avez-vous voté à cette élection provi...
#> 3 Avez-vous voté à cette élection provi...
#> 4 Si un référendum sur l'indépendance a...
#> 5 Si un référendum sur l'indépendance a...
#> 6 Si un référendum sur l'indépendance a...
#> 7 Si un référendum sur l'indépendance a...
#>                                   question   study missing_type is_declared_na
#> 1 Did you vote in that provincial elect... qes2014         <NA>          FALSE
#> 2 Did you vote in that provincial elect... qes2014         <NA>          FALSE
#> 3 Did you vote in that provincial elect... qes2014      refused          FALSE
#> 4 If there were a referendum on indepen... qes2014         <NA>          FALSE
#> 5 If there were a referendum on indepen... qes2014         <NA>          FALSE
#> 6 If there were a referendum on indepen... qes2014           dk          FALSE
#> 7 If there were a referendum on indepen... qes2014      refused          FALSE

# the codebook of data you have read
demo <- get_qes("qes_demo", quiet = TRUE)
qes_codebook(demo, variables = c("Q19", "Q28"))
#> <qes_codebook> survey: qes_demo
#> Data file: qes_demo.sav 
#> Variables: 2 
#> Codebook/support files: 0 
#>   variable                                    label
#> 1      Q19 Si un référendum sur l'indépendance a...
#> 2      Q28 Quel est votre intérêt pour la politi...
#>                                   question n_value_labels    study position
#> 1 Si un référendum sur l’indépendance a...              4 qes_demo        8
#> 2 Quel est votre intérêt pour la politi...              6 qes_demo        9
#>      type question_lang question_truncated
#> 1 numeric            fr              FALSE
#> 2 numeric            fr              FALSE
#>                               value_labels    missing_codes      targets
#> 1 1=Oui | 2=Non | 8=Je ne sais pas | 9=... 8=dk | 9=refused    sov_indep
#> 2 1=Très intéressé(e) | 2=Plutôt intére... 8=dk | 9=refused interest_4pt
#>   label_source question_source               doc_ref
#> 1         file   questionnaire 352010:Q19;352009:Q19
#> 2         file   questionnaire 352010:Q28;352009:Q28
```
