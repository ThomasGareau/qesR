---
title: qesR
---

<p>
  <img src="logo.png" alt="qesR logo" width="220" />
</p>

Access Quebec Election Study datasets in R.

## Installation

Install the development version from GitHub:

```r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("ThomasGareau/qesR")
```

## Using the package

The package provides access to QES surveys through:

- `get_qes(srvy)`: download a survey by code
- `get_qescodes()`: list survey codes and metadata
- `get_preview(srvy, obs)`: preview a survey
- `get_question(data, variable)`: retrieve question text
- `get_codebook(srvy)`: get survey codebook
- `get_qes_master()`: build a harmonized merged dataset

## Examples

```r
library(qesR)

# list survey call codes
get_qescodes()

# load one survey
qes2022 <- get_qes("qes2022")

# preview observations
get_preview("qes2022", 10)

# retrieve question text
get_question(qes2022, "cps_age_in_years", full = TRUE)

# build merged cross-study dataset
master <- get_qes_master(strict = FALSE)
head(master)
```

## Details

- downloaded survey files are read and returned in labelled form
- codebook metadata is attached and available through `get_codebook()`
- merged dataset creation includes harmonization, de-duplication, and filtering of empty rows
- source-variable provenance is available via `attr(master, "source_map")`

## Merged dataset

`qesR` includes a merged harmonized dataset workflow:

```r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```

What it harmonizes:

Current harmonized columns in `qes_master`:

`qes_code`, `qes_year`, `qes_name_en`, `respondent_id`, `interview_start`, `interview_end`, `interview_recorded`, `language`, `citizenship`, `year_of_birth`, `age`, `age_group`, `gender`, `province_territory`, `education`, `income`, `religion`, `born_canada`, `political_interest`, `ideology`, `turnout`, `vote_choice`, `vote_choice_text`, `party_best`, `party_lean`, `sovereignty_support`, `sovereignty`, `federal_pid`, `provincial_pid`, `survey_weight`, `definibin`, `ethn1`, `influperso`, `luentend`, `occup`, `patron`, `pondam1`, `q10`, `q16`, `q17`, `q18`, `q18a`, `q18b`, `q20`, `q20b`, `q22`, `q23`, `q24`, `q3`, `q31`, `q33`, `q35`, `q37`, `q38`, `q39`, `q4`, `q40`, `q41`, `q42`, `q43`, `q44`, `q45`, `q46`, `q47`, `q48`, `q49`, `q50`, `q51`, `q53`, `q54`, `q55`, `q56`, `q58`, `q59`, `q60`, `q61b`, `q61d`, `q62a`, `q62b`, `q64`, `q65`, `q66`, `q68`, `q7`, `q70`, `q72`, `q73`, `q74`, `q79`, `q8`, `q80`, `q81`, `q9`, `raison1`, `raison2`, `s_jse`, `satisf`, `sefie`, `sondbons`, `voteprec`

Harmonization applies value normalization, derived age groups, within-study de-duplication, and removal of rows that are empty across harmonized variables.

See: [Merged Dataset](articles/merged-dataset.html)

## Survey code table

| year | code | name |
|---|---|---|
| 2022 | `qes2022` | Quebec Election Study 2022 |
| 2018 | `qes2018` | Quebec Election Study 2018 |
| 2018 | `qes2018_panel` | Quebec Election Study 2018 Panel |
| 2014 | `qes2014` | Quebec Election Study 2014 |
| 2012 | `qes2012` | Quebec Election Study 2012 |
| 2012 | `qes2012_panel` | Quebec Election Study 2012 Panel |
| 2007-2010 | `qes_crop_2007_2010` | CROP Quebec Opinion Polls (2007-2010) |
| 2008 | `qes2008` | Quebec Election Study 2008 |
| 2007 | `qes2007` | Quebec Election Study 2007 |
| 2007 | `qes2007_panel` | Quebec Election Study 2007 Panel |
| 1998 | `qes1998` | Quebec Elections 1998 |

## Citations

Full citations for all studies are listed in:

- [Study Citations](articles/study-citations.html)

## Use cases

Use cases available in separate tabs:

- [Evolution of sovereignty attitudes](articles/analysis-sovereignty.html)
- [Vote-choice patterns over time](articles/analysis-vote-choice.html)

## Version française

Use the `FR` button in the top bar to switch any main page to its French version.
