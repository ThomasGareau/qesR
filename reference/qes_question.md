# The exact wording of survey questions

`qes_question()` returns the question asked for each of the variables
you name, as the study's questionnaire (for `qes2022`, its codebook)
gives it, with where it comes from. Variable names must match exactly;
there is no partial or fuzzy matching.

## Usage

``` r
qes_question(x, variables, lang = NULL)
```

## Arguments

- x:

  A study code from
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md),
  or a data frame returned by
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  (its study is read from its attributes).

- variables:

  Character vector of variable names (exact match). An unknown name is
  an error of class `qesR_error_unknown_variable` that suggests near
  matches.

- lang:

  `NULL` (default): the study's own language, or the other language when
  only that one is known; `"en"` or `"fr"`: that language only (`NA`
  where it is unknown). There is no machine translation.

## Value

A data frame with one row per variable: `study`, `variable`, `question`
(`NA` when unknown), `question_lang`, `truncated` (`TRUE` when the
source cut the text), `source` (`"questionnaire"`, or `"file"` when the
text is the data file's own label), `doc_ref` (the documents that give
the text: one or more `<file_id>:<item>` references, the Dataverse file
id and the question number, separated by `;`, e.g.
`"352010:Q19;352009:Q19"` for the English and French questionnaires; a
bare file id when the item is not known) and `universe` (who was asked,
when the questionnaire says so). For `qes2022`, the attribute
`licence_notice` gives the attribution and licence (CC BY-NC 4.0) of the
wording.

## Details

The wording comes from the metadata of
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
which ships with qesR for every study: no download and no network.
`doc_ref` names the document and question (see
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)).
A text the source cut has `truncated = TRUE`; qesR never guesses the end
of a cut question. The wording of `qes2022` is quoted from its codebook
under the study's licence, CC BY-NC 4.0 (see `qes_cite("qes2022")`); the
result then has the attribute `licence_notice`, the attribution that
licence requires.

## En français

`qes_question()` renvoie le libellé exact de la question posée pour
chaque variable nommée, tel que le donne le questionnaire de l'étude,
avec sa source. Les noms de variables doivent correspondre exactement.
`lang = "fr"` donne le libellé français, `lang = "en"` l'anglais ; par
défaut, la langue de l'étude. Un libellé coupé par la source est signalé
par `truncated = TRUE`. Le libellé vient des métadonnées de
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
livrées avec qesR pour chaque étude (pour `qes2022`, le livre de codes
bilingue de l'étude, cité sous sa licence CC BY-NC 4.0 ; le résultat
porte alors l'attribut `licence_notice`, l'attribution qu'exige cette
licence) : aucun téléchargement n'est nécessaire. `doc_ref` donne une ou
plusieurs références `<file_id>:<question>` séparées par `;` (par
exemple `"352010:Q19;352009:Q19"`, questionnaires anglais et français).

## See also

[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)
for every variable of a study,
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
to find variables by topic.

Other codebooks and search:
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md),
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)

## Examples

``` r
qes_question("qes2014", c("Q2", "Q19"))
#>     study variable
#> 1 qes2014       Q2
#> 2 qes2014      Q19
#>                                                                                                                                                           question
#> 1                                                                                                                     Avez-vous voté à cette élection provinciale?
#> 2 Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON?
#>   question_lang truncated        source               doc_ref universe
#> 1            fr     FALSE questionnaire   352010:Q2;352009:Q2     <NA>
#> 2            fr     FALSE questionnaire 352010:Q19;352009:Q19     <NA>
qes_question("qes2014", "Q19", lang = "en")
#>     study variable
#> 1 qes2014      Q19
#>                                                                                                                           question
#> 1 If there were a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO?
#>   question_lang truncated        source               doc_ref universe
#> 1            en     FALSE questionnaire 352010:Q19;352009:Q19     <NA>
```
