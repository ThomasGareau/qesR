# Get survey question text (older name)

Soft-deprecated: use
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md).
`get_question()` keeps working and will not be removed; it prints a
one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).

## Usage

``` r
get_question(do, q, full = TRUE)
```

## Arguments

- do:

  A data.frame, or the name of one, looked up (read only) in the calling
  environment and its enclosing environments: the workspace, for a call
  at top level or from a function or
  [`sapply()`](https://rdrr.io/r/base/lapply.html) lambda defined there.

- q:

  Column name whose question text should be returned.

- full:

  Ignored: the full question text is always returned. Setting it to
  `FALSE` prints a one-time note.

## Value

A character scalar with question text, or `NA_character_` (with a
warning of class `qesR_warning`) when none exists.

## Details

`q` must name a column of the data exactly (ignoring case only when that
names a single column): `get_question(d, "q1")` never answers for `q10`.
An unknown name is an error of class `qesR_error_unknown_variable` that
suggests near matches. The text returned is the question of
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
when the study is known, else the column's label; when the source cut
the question, a warning of class `qesR_warning_truncated` says so.
`full` no longer changes the result: the text is always the most
complete one qesR has.

## See also

[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md),
and
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
demo <- get_qes("qes_demo", quiet = TRUE)
get_question(demo, "Q19")
#> `get_question()` is soft-deprecated; use `qes_question()`. It keeps working and will not be removed.
#> [1] "Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON?"
```
