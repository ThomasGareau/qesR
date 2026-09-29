# Search variables across studies, in English and French

`qes_search()` finds the variables whose name, label, question text or
value labels contain `pattern`, in every study whose description is at
hand, with no network request. The search ignores case and accents:
`"souverain"` finds "Souveraineté", `"quebec"` finds "Québec". Several
terms separated by `|` find any of them (`"souverain|sovereign"`).

## Usage

``` r
qes_search(
  pattern,
  studies = NULL,
  fields = c("variable", "label", "question", "values", "target"),
  regex = FALSE,
  lang = c("both", "en", "fr")
)
```

## Arguments

- pattern:

  A single string. Unless `regex = TRUE` it is matched as plain text;
  `|` separates alternative terms.

- studies:

  Optional character vector of study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md));
  `NULL` or `"all"` searches every study.

- fields:

  Which fields to search: any of `"variable"` (names), `"label"`
  (variable labels), `"question"` (question text), `"values"` (value
  labels) and `"target"` (the names and labels, in the language(s)
  `lang` asks for, of the harmonized targets a variable feeds; see
  [`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)).

- regex:

  If TRUE, `pattern` is a regular expression (Perl syntax), matched
  ignoring case and accents.

- lang:

  `"both"` (default) searches English and French text; `"en"` or `"fr"`
  only the text in that language (variable names are always searched).
  It also chooses the language of the `question` column.

## Value

A data frame of class `qes_search`, one row per matching variable:
`study`, `year`, `variable`, `label`, `question`, `question_lang`,
`values` (`"1=Oui | 2=Non"`), `targets` (the harmonized targets the
variable feeds in
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
`;`-separated, `NA` for none) and `matched_in` (the fields that matched,
separated by `;`). Attributes: `not_searchable` (the requested studies
whose description does not ship with qesR: none of the catalog's
studies), `coverage` (per study: `n_variables`, `n_label`, `n_question`
and `n_reviewed`, the variables whose wording was checked by hand
against the questionnaire) and, when `qes2022` rows are found,
`licence_notice` (the attribution and licence, CC BY-NC 4.0, of their
description, named by study; printed after the results).

## Details

Every study is searchable offline: its description ships with qesR (for
`qes2022`, under the study's licence, CC BY-NC 4.0; see
`qes_cite("qes2022")`). Question text is known for the variables whose
questionnaire or codebook was matched to the data (see the `coverage`
attribute).

## En français

`qes_search()` trouve les variables dont le nom, l'étiquette, le texte
de la question ou les étiquettes de valeurs contiennent `pattern`, sans
requête réseau, en ignorant la casse et les accents, dans toutes les
études (la description de `qes2022` est livrée sous la licence de
l'étude, CC BY-NC 4.0 : un résultat qui en contient porte son
attribution dans l'attribut `licence_notice`, affichée après les
résultats). Par défaut (`lang = "both"`), la recherche porte sur le
français et l'anglais ; `lang = "fr"` la limite aux textes français.
Plusieurs termes séparés par `|` trouvent l'un ou l'autre.

## See also

[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)
and
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
for the variables found.

Other codebooks and search:
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md),
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)

## Examples

``` r
hits <- qes_search("souverain|sovereign")
head(hits[, c("study", "variable", "question")])
#>     study            variable                                 question
#> 1 qes2022 cps_impissue_matrix What is the most important issue to y...
#> 2 qes2022             pes_q23 Did you vote in the 1995 referendum o...
#> 3 qes2018                  q2 Parmi les enjeux suivants, lequel éta...
#> 4 qes2018         q2_96_other                                     <NA>
#> 5 qes2018                 q23 Avez-vous voté lors du référendum de ...
#> 6 qes2014                  Q1 Parmi les enjeux suivants, lequel éta...

# French question text only, in two studies; accents are optional
# ("interet" also finds the accented spelling)
qes_search("interet pour la politique", studies = c("qes2014", "qes2018"),
           fields = "question", lang = "fr")
#>     study year variable                                    label
#> 1 qes2014 2014      Q28 Quel est votre intérêt pour la politi...
#> 2 qes2018 2018      q27                                     <NA>
#>                                   question question_lang
#> 1 Quel est votre intérêt pour la politi...            fr
#> 2 Quel est votre intérêt pour la politi...            fr
#>                                     values      targets matched_in
#> 1 1=Très intéressé(e) | 2=Plutôt intére... interest_4pt   question
#> 2 1=Très intéressé(e) | 2=Assez intéres... interest_4pt   question
```
