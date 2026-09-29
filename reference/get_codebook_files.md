# Get codebook files (legacy)

Returns the documentation files (codebooks, questionnaires and reports)
deposited with a study.

## Usage

``` r
get_codebook_files(
  srvy = NULL,
  codebook = NULL,
  file = NULL,
  quiet = FALSE,
  refresh = FALSE
)

get_qes_codebook_files(
  srvy = NULL,
  codebook = NULL,
  file = NULL,
  quiet = FALSE,
  refresh = FALSE
)
```

## Arguments

- srvy:

  A qesR survey code. Required if `codebook` is NULL.

- codebook:

  A `qes_codebook` object. Its study is used when it records one;
  otherwise the file manifest attached to it is returned.

- file:

  Ignored: documents do not depend on which data file is read.

- quiet:

  If TRUE, suppress informational output.

- refresh:

  Ignored: the list comes from the catalog shipped with qesR.

## Value

A data frame of codebook/support files with the columns `file_id`,
`filename`, `extension`, `size` and `download_url`.

## Details

Soft-deprecated: use
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md).
`get_codebook_files()` and `get_qes_codebook_files()` keep working and
will not be removed; each prints a one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).
They return every document deposited with the study from the offline
catalog, with the qesR 0.4.4 columns, and make no network request.
`file` and `refresh` no longer change the result.

## See also

[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md),
and
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
get_codebook_files(srvy = "qes2022")
#>   file_id                                   filename extension   size
#> 1 7449514 2022 Quebec Election Study Codebook v1.pdf       pdf 663655
#>                                                download_url
#> 1 https://dataverse.harvard.edu/api/access/datafile/7449514
```
