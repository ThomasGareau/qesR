# Download and Load a Quebec Election Study

Downloads a study file from Dataverse, applies labels when available,
and attaches a codebook.

## Usage

``` r
get_qes(srvy, file = NULL, assign_global = TRUE, with_codebook = TRUE, quiet = FALSE)
```

## Arguments

- srvy:

  A qesR survey code from
  [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md).

- file:

  Optional regular expression for choosing one file in multi-file
  datasets.

- assign_global:

  If TRUE, assign the result into the global environment using `srvy`.

- with_codebook:

  If TRUE, attach codebook metadata and assign `<srvy>_codebook` when
  `assign_global = TRUE`.

- quiet:

  If TRUE, suppress informational output.

## Value

A labelled data frame/tibble for the selected survey.
