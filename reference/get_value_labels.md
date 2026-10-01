# Get value labels from a codebook (older name)

Soft-deprecated: use `qes_codebook(layout = "long")`, which has one row
per value with its label and missing type. `get_value_labels()` keeps
working and will not be removed; it prints a one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).

## Usage

``` r
get_value_labels(codebook, variable = NULL, long = FALSE)
```

## Arguments

- codebook:

  A `qes_codebook` object from
  [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md).

- variable:

  Optional variable name. If `NULL`, returns mappings for all variables
  that have value labels.

- long:

  If TRUE, return a long data frame with `variable`, `value`, and
  `value_label` columns.

## Value

A named list of value-label vectors (named by code), or a long data
frame when `long = TRUE`.

## Details

A variable that is not in the codebook is an error of class
`qesR_error_unknown_variable` that suggests near matches (qesR 0.4.4
returned an empty list). A label that is an empty string in the file is
kept as `""`.

## See also

[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
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
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md)

## Examples

``` r
cb <- qes_codebook("qes2014", variables = c("Q2", "Q19"))
get_value_labels(cb, "Q19")
#> `get_value_labels()` is soft-deprecated; use `qes_codebook(layout = "long")`. It keeps working and will not be removed.
#> $Q19
#>                            1                            2 
#>                        "Oui"                        "Non" 
#>                            8                            9 
#>             "Je ne sais pas" "Je préfère ne pas répondre" 
#> 
```
