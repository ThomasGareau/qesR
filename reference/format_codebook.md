# Reformat a qesR codebook (legacy)

Soft-deprecated: use `qes_codebook(codebook, layout = )`, which lays a
codebook out again. `format_codebook()` keeps working and will not be
removed; it prints a one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).

## Usage

``` r
format_codebook(codebook, layout = c("compact", "wide", "long"))
```

## Arguments

- codebook:

  A `qes_codebook` object from
  [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md).

- layout:

  One of `"compact"`, `"wide"`, or `"long"`.

## Value

The codebook in the requested layout (see
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)).

## Details

The long layout keeps the variables that have no value labels, with one
row whose `value` is `NA`. A plain data frame, for example a codebook
read back from a CSV file (which loses its class and attributes), is an
error of class `qesR_error_input`: rebuild it with
`qes_codebook("<code>")`.

## See also

[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
and
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
cb <- qes_codebook("qes2014", variables = c("Q2", "Q19"))
format_codebook(cb, layout = "long")
#> `format_codebook()` is soft-deprecated; use `qes_codebook(codebook, layout = )`. It keeps working and will not be removed.
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
#> 1 Avez-vous voté à cette élection provi... qes2014         <NA>          FALSE
#> 2 Avez-vous voté à cette élection provi... qes2014         <NA>          FALSE
#> 3 Avez-vous voté à cette élection provi... qes2014      refused          FALSE
#> 4 Si un référendum sur l’indépendance a... qes2014         <NA>          FALSE
#> 5 Si un référendum sur l’indépendance a... qes2014         <NA>          FALSE
#> 6 Si un référendum sur l’indépendance a... qes2014           dk          FALSE
#> 7 Si un référendum sur l’indépendance a... qes2014      refused          FALSE
```
