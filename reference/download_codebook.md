# Download Codebook Files

Downloads codebook/support files (PDFs, questionnaires, metadata files)
into a local directory.

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

  Directory where files should be downloaded.

- file:

  Optional file selector passed to
  [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md).

- quiet:

  If TRUE, suppress informational output.

- refresh:

  If TRUE, force a fresh codebook metadata download first.

- overwrite:

  If TRUE, overwrite existing files in `dest_dir`.

## Value

A data frame containing source file metadata and local download paths.
