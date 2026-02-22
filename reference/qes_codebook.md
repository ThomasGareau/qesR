# Alias for get_codebook

Convenience alias for
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md).

## Usage

``` r
qes_codebook(
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
  [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md).

- file:

  Optional regular expression for choosing one file in multi-file
  datasets.

- assign_global:

  If TRUE, assign the codebook to `<srvy>_codebook` in the global
  environment.

- quiet:

  If TRUE, suppress informational output.

- refresh:

  If TRUE, force a fresh download instead of using cache.

- layout:

  One of `"compact"`, `"wide"`, or `"long"`.

## Value

A `qes_codebook` data frame.
