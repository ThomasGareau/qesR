# Get a Quebec Election Study codebook (older name)

Soft-deprecated: use
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
which takes the same arguments. `get_codebook()` and
`get_qes_codebook()` keep working and will not be removed; each prints a
one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).
They return what
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)
returns: the codebook is built offline from the metadata shipped with
qesR (see
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)),
the columns of qesR 0.4.4 come first and new columns follow, and the
attributes (`survey_code`, `doi`, `files`, `codebook_files`, ...) are
kept in every layout. `refresh` no longer changes the result.

## Usage

``` r
get_codebook(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
)

get_qes_codebook(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
)
```

## Arguments

- srvy:

  A qesR survey code from
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md).
  Codes are trimmed and case-insensitive.

- file:

  Optional regular expression for choosing one file in multi-file
  datasets.

- assign_global:

  If TRUE, also assign the returned codebook as `<code>_codebook` into
  the environment the function was called from (the global environment
  only when called at top level), where `<code>` is the canonical study
  code. Defaults to FALSE.

- quiet:

  If TRUE, suppress informational output.

- refresh:

  Ignored (the codebook is built offline); setting it prints a one-time
  note.

- layout:

  One of `"compact"`, `"wide"`, or `"long"`.

## Value

A `qes_codebook` data frame, returned visibly (see
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)).

## See also

[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
and
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
cb <- get_codebook("qes2014")
#> `get_codebook()` is soft-deprecated; use `qes_codebook()`. It keeps working and will not be removed.
head(cb[, 1:4])
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
#>                                   question n_value_labels
#> 1                                     <NA>              0
#> 2                                     <NA>              0
#> 3 Préfèreriez-vous répondre à ce questi...              2
#> 4                                     <NA>              1
#> 5         En quelle année êtes-vous né(e)?              1
#> 6                                     <NA>              0
```
