# Create a Prepared Non-Exhaustive qesR Dataset

Builds a deconstructed teaching/testing dataset with standardized
columns from a selected Quebec election study.

## Usage

``` r
get_decon(srvy = "qes2022", assign_global = TRUE, quiet = FALSE)
```

## Arguments

- srvy:

  A qesR survey code. Defaults to `"qes2022"`.

- assign_global:

  If TRUE, assign the result as `decon` in the global environment.

- quiet:

  If TRUE, suppress informational output while downloading.

## Value

A data frame named `decon` when assigned globally.
