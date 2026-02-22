# Preview a Quebec Election Study

Loads a study and returns the first observations.

## Usage

``` r
get_preview(srvy, obs = 6L, file = NULL)
```

## Arguments

- srvy:

  A qesR survey code from
  [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md).

- obs:

  Number of observations to return.

- file:

  Optional regular expression for choosing one file in multi-file
  datasets.

## Value

A data frame/tibble preview (with attached codebook metadata).
