# Merged Dataset Workflow

`qesR` provides a merged harmonized dataset through
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md).

## Build merged data

``` r
library(qesR)

master <- get_qes_master(strict = FALSE)
dim(master)
head(master)
```

## What it harmonizes

The merged data includes cross-study fields such as:

- `respondent_id`
- `age`, `age_group`, `year_of_birth`
- `gender`, `education`, `language`
- `turnout`, `vote_choice`, `party_lean`
- `survey_weight`

It also records source-variable provenance:

``` r
source_map <- attr(master, "source_map")
head(source_map)
```

## Quality controls in merge

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
applies:

- within-year respondent de-duplication for multi-study years
- derived age groups where age or year-of-birth are available
- removal of rows empty across harmonized variables

## Save merged data

``` r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```
