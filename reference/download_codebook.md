# Download codebook files (legacy)

Downloads a study's documentation files (codebooks, questionnaires and
reports) into a local directory.

## Usage

``` r
download_codebook(
  srvy,
  dest_dir = tempdir(),
  file = NULL,
  quiet = FALSE,
  refresh = FALSE,
  overwrite = FALSE
)
```

## Arguments

- srvy:

  A qesR survey code.

- dest_dir:

  Directory where files should be downloaded. Created if needed, only
  when there is a file to download.

- file:

  Optional regular expression matched (ignoring case) against the
  document file names; only matching documents are downloaded.

- quiet:

  If TRUE, suppress informational output.

- refresh:

  Ignored: the list comes from the catalog shipped with qesR.

- overwrite:

  If TRUE, overwrite existing files in `dest_dir`.

## Value

A data frame with the columns `file_id`, `filename`, `extension`,
`size`, `download_url`, `local_path` and `downloaded`.

## Details

Soft-deprecated: use
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
with `what = "docs"`. `download_codebook()` keeps working and will not
be removed; it prints a one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).
It now downloads the documents listed in the offline catalog (the same
files as
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md)),
checks each one against its md5 checksum before giving it its final
name, and makes no metadata request. `file` now selects documents by
name; in qesR 0.4.4 it selected a data file. `refresh` no longer changes
the result. `dest_dir` is created only when there is at least one file
to download. A file already in `dest_dir` is kept as it is unless
`overwrite = TRUE`.

## See also

[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md),
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md),
and
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
# the documents it would download for a study, offline
get_codebook_files("qes2018")[, c("filename", "size")]
#> `get_codebook_files()` is soft-deprecated; use `qes_docs()`. It keeps working and will not be removed.
#>                                                           filename   size
#> 1                                Quebec Election Study 2018 EN.doc 232960
#> 2                                Quebec Election Study 2018 FR.doc 242176
#> 3 Quebec Election Study 2018 FR with programmed answer values.docx  73638
#> 4     Rapport méthodologique de l'Étude électorale québécoise 2018 651382

# a `file` pattern that matches no document: nothing is downloaded and
# no folder is created
download_codebook("qes2018", dest_dir = file.path(tempdir(), "qes_docs"),
                  file = "^no such document$")
#> `download_codebook()` is soft-deprecated; use `qes_download(what = "docs")`. It keeps working and will not be removed.
#> No codebook/support files found for 'qes2018'.
#> [1] file_id      filename     extension    size         download_url
#> [6] local_path   downloaded  
#> <0 rows> (or 0-length row.names)
```
