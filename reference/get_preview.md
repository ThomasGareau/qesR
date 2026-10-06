# Preview a Quebec Election Study (older name)

Loads a study and returns the first observations.

## Usage

``` r
get_preview(srvy, obs = 6L, file = NULL)
```

## Arguments

- srvy:

  A qesR survey code from
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md).

- obs:

  Number of observations to return: a whole number of at least 1
  (otherwise an error of class `qesR_error_input`).

- file:

  Optional regular expression for choosing one file in multi-file
  datasets.

## Value

A base data frame with the first `obs` rows, and the attributes of
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
data (`qes_survey_code`, `qes_provenance`, `qes_codebook`).

## Details

Soft-deprecated: use `head(get_qes(srvy), obs)`. `get_preview()` keeps
working and will not be removed; it prints a one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).
It is exactly [`head()`](https://rdrr.io/r/utils/head.html) of what
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
returns, so the data comes from the same pinned file, is served from
memory or the download cache when the study was read before in the
session, and keeps the attributes `qes_survey_code`, `qes_provenance`
and `qes_codebook`.

## See also

[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
# the synthetic demonstration study, offline
get_preview("qes_demo", obs = 3)
#> `get_preview()` is soft-deprecated; use `head(get_qes(srvy), obs)`. It keeps working and will not be removed.
#>   QUEST LANG QAGE QSEXE QREGION Q2 Q3 Q19 Q28 Q32      POND
#> 1     0   EN 1958     2      16  1  3   2   3   0 1.2593217
#> 2     0   FR 1951     2       6  1  4   2   3   3 0.6685168
#> 3     0   FR 1939     1       3  1  1   1   2   7 1.3786438

# the same rows, with the current function
head(get_qes("qes_demo", quiet = TRUE), 3)
#>   QUEST LANG QAGE QSEXE QREGION Q2 Q3 Q19 Q28 Q32      POND
#> 1     0   EN 1958     2      16  1  3   2   3   0 1.2593217
#> 2     0   FR 1951     2       6  1  4   2   3   3 0.6685168
#> 3     0   FR 1939     1       3  1  1   1   2   7 1.3786438
```
