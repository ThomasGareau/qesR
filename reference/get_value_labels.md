# Get Value Labels from a Codebook

Extracts value-label mappings from a `qes_codebook` object.

## Usage

``` r
get_value_labels(codebook, variable = NULL, long = FALSE)
```

## Arguments

- codebook:

  A `qes_codebook` object from
  [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md).

- variable:

  Optional variable name. If `NULL`, returns mappings for all variables.

- long:

  If TRUE, return a long data frame with `variable`, `value`, and
  `value_label` columns.

## Value

A named list of value-label vectors, or a long data frame when
`long = TRUE`.
