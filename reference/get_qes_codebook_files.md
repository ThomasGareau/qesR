# Alias for get_codebook_files

Backward-compatible alias for
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md).

## Usage

``` r
get_qes_codebook_files(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE)
```

## Arguments

- srvy:

  A qesR survey code. Required if `codebook` is NULL.

- codebook:

  A `qes_codebook` object.

- file:

  Optional file selector passed to
  [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md)
  when `codebook` is not provided.

- quiet:

  If TRUE, suppress informational output when downloading.

- refresh:

  If TRUE, force a fresh codebook download when `codebook` is not
  provided.

## Value

A data frame of codebook/support files.
