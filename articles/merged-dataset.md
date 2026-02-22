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

- study metadata: `qes_code`, `qes_year`, `qes_name_en`
- respondent/interview fields: `respondent_id`, `interview_start`,
  `interview_end`, `interview_recorded`
- demographics: `year_of_birth`, `age`, `age_group`, `gender`,
  `education`, `language`, `province_territory`
- political variables: `turnout`, `vote_choice`, `vote_choice_text`,
  `party_best`, `party_lean`, `ideology`, `political_interest`
- additional fields where available: `citizenship`, `born_canada`,
  `income`, `religion`, `federal_pid`, `provincial_pid`, `survey_weight`

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
also keeps the merge logic explicit:

- uses cross-study variable maps and value normalization for
  comparability
- derives `age_group` where age or year-of-birth is available
- retains panel and non-panel respondents as separate observations
- stores source-variable provenance in `attr(master, "source_map")`

It also records source-variable provenance:

``` r
source_map <- attr(master, "source_map")
head(source_map)
```

## Quality controls in merge

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
applies:

- respondent de-duplication within the same survey code
- derived age groups where age or year-of-birth are available
- removal of rows empty across harmonized variables

## Save merged data

``` r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```
