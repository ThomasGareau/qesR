# Set "don't know", "refused" and other missing codes to NA

In the data
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
returns, the codes that stand for a missing answer ("don't know",
"refused", SPSS user-missing codes such as 8 or 9, `-99` in `qes2022`)
are kept as values, as the files deposit them. `qes_missing()` turns
them into `NA`, or into tagged `NA` values that remember the reason,
using the missing type the codebook records for each code (see
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
column `missing_codes`).

## Usage

``` r
qes_missing(
  x,
  variables = NULL,
  action = c("na", "tagged"),
  types = NULL,
  quiet = FALSE
)
```

## Arguments

- x:

  A data frame returned by
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  (its study is read from its attributes; a data frame that has lost
  them is an error of class `qesR_error_no_provenance`).

- variables:

  Optional character vector of column names (exact match) to recode;
  `NULL` recodes every column.

- action:

  `"na"` (default) sets the codes to `NA`; `"tagged"` sets them, in
  numeric columns, to
  [`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html)
  values whose tag is the letter of their type (other columns get a
  plain `NA`).

- types:

  Optional character vector of missing types to recode. `NULL` (default)
  means every type except `spoiled`, `not_selected`, `not_voted` and
  `not_registered`.

- quiet:

  If TRUE, suppress the message that counts the variables with no typed
  codes.

## Value

`x`, returned visibly, with the codes recoded and the attribute
`qes_missing_log`: a data frame with one row per recoded code and
variable (`variable`, `value`, `missing_type`, `n_set`, the number of
values set to `NA`). Labels and other attributes are kept.

## Details

Missing types, and their tag for `action = "tagged"`: `dk` (d),
`refused` (r), `dk_refused` (b, one code for both), `no_answer` (o),
`not_selected` (s, an option not ticked in a multiple-choice question),
`inapplicable` (i), `not_voted` (v), `spoiled` (p), `ineligible` (e),
`not_registered` (g), `not_in_wave` (w), `not_mappable` (m) and
`user_na` (u, a code the SPSS file declares missing without saying why).
A code has one type. The codes of each study were typed from their
labels, by hand; a variable whose codebook types none of its codes, and
whose file declares none, is left unchanged, and a message counts those
variables. Some files declare missing a code that is an answer ("Un
autre parti", or "ne voterait pas/annulerait" in a voting-intention
item): such a code has no missing type and is never changed. A declared
code for "did not vote" or a spoiled ballot has the type `not_voted` or
`spoiled`, so the default leaves it alone.

`qes_missing()` uses the codebook
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
attached to `x`, else the study's metadata shipped with qesR. It never
downloads anything.

## En français

`qes_missing()` remplace par `NA` les codes qui représentent une
non-réponse (« ne sait pas », « refus », codes manquants déclarés dans
le fichier SPSS, `-99` dans `qes2022`), selon le type que le codebook
attribue à chaque code. Avec `action = "tagged"`, les colonnes
numériques reçoivent des `NA` étiquetés
([`haven::tagged_na()`](https://haven.tidyverse.org/reference/tagged_na.html))
qui conservent la raison. Par défaut, les types `spoiled`,
`not_selected`, `not_voted` et `not_registered`, qui sont des réponses,
ne sont pas touchés. Un code déclaré manquant dans le fichier mais qui
est une réponse (« Un autre parti », « ne voterait pas/annulerait » dans
une intention de vote) n'a pas de type et n'est jamais modifié. La
fonction ne télécharge rien.

## See also

[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)
for the codes of each variable.

Other codebooks and search:
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md),
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)

## Examples

``` r
demo <- get_qes("qes_demo", quiet = TRUE)
table(demo$Q19, useNA = "ifany")
#> 
#>  1  2  8  9 
#> 22 33  3  2 
clean <- qes_missing(demo, variables = c("Q19", "Q28"))
table(clean$Q19, useNA = "ifany")
#> 
#>    1    2 <NA> 
#>   22   33    5 
attr(clean, "qes_missing_log")
#>   variable value missing_type n_set
#> 1      Q19     8           dk     3
#> 2      Q19     9      refused     2
#> 3      Q28     8           dk     1
#> 4      Q28     9      refused     0

# keep the reason: "don't know" and "refused" become different NAs
tagged <- qes_missing(demo, variables = "Q19", action = "tagged")
table(haven::na_tag(tagged$Q19), useNA = "ifany")
#> 
#>    d    r <NA> 
#>    3    2   55 
```
