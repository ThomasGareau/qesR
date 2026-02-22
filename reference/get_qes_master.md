# Build a Harmonized Stacked Master QES Dataset

Downloads multiple Quebec Election Study datasets, harmonizes shared
variables, stacks all rows into one master data frame, de-duplicates
respondents within the same study year, and drops rows that are empty
across harmonized variables.

## Usage

``` r
get_qes_master(
  surveys = NULL,
  assign_global = TRUE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
)
```

## Arguments

- surveys:

  Character vector of qesR survey codes. Defaults to all studies from
  [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md).

- assign_global:

  If TRUE, assign the result to `.GlobalEnv` using `object_name`.

- object_name:

  Object name used when `assign_global = TRUE`. Defaults to
  `"qes_master"`.

- quiet:

  If TRUE, suppress informational output while downloading.

- strict:

  If TRUE, stop when any study fails. If FALSE, return partial results
  and record failures in attributes.

- save_path:

  Optional output path for writing the master file. Use `.rds` for RDS
  output; otherwise CSV is written.

## Value

A harmonized stacked data frame with attributes: `source_map`,
`loaded_surveys`, `failed_surveys`, and `harmonized_variables`.
Additional attributes `duplicates_removed` and `empty_rows_removed`
report row filtering. When `save_path` is provided, the output path is
stored in `saved_to`.
