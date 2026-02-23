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

`qes_code`, `qes_year`, `qes_name_en`, `respondent_id`, `interview_start`, `interview_end`, `interview_recorded`, `language`, `citizenship`, `year_of_birth`, `age`, `age_group`, `gender`, `province_territory`, `education`, `income`, `religion`, `born_canada`, `political_interest`, `ideology`, `turnout`, `vote_choice`, `vote_choice_text`, `party_best`, `party_lean`, `sovereignty_support`, `sovereignty`, `federal_pid`, `provincial_pid`, `survey_weight`, `definibin`, `ethn1`, `influperso`, `luentend`, `occup`, `patron`, `pondam1`, `issue_family_support`, `info_source_primary`, `info_source_secondary`, `party_best_health`, `identity_self_1`, `identity_self_2`, `referendum_intent`, `party_best_anticorruption`, `elites_out_of_touch`, `trust_government`, `gov_tax_waste`, `issue_education`, `feeling_qs_0_100`, `election_timing_eval`, `pr_reform_support`, `democracy_needs_parties`, `parties_all_same`, `feeling_charest_0_100`, `qc_difference_view`, `feeling_boisclair_0_100`, `feeling_dumont_0_100`, `feeling_david_0_100`, `feeling_mckay_0_100`, `leader_most_competent`, `fed_gov_too_interventionist`, `leader_closest_people`, `qc_economy_change`, `federalist_sovereignist_self`, `future_outlook`, `privatize_hydro_support`, `private_healthcare_support`, `gov_role_environment`, `profits_help_poor`, `party_best_health_alt`, `powers_for_quebec`, `party_best_environment`, `party_best_poverty`, `qc_voice_federal`, `party_best_qc_identity`, `party_best_caisse_depot`, `decision_level_a`, `decision_level_b`, `feeling_unions_0_100`, `feeling_business_0_100`, `same_sex_marriage`, `family_values_priority`, `issue_tax_cuts`, `provincial_pid_item`, `provincial_party_lean`, `party_named`, `vote_federal_2006`, `employment_status`, `issue_qc_status`, `home_language`, `gov_role_jobs_income`, `issue_poverty`, `vote_reason_1`, `vote_reason_2`, `interview_weekday`, `gov_satisfaction`, `polling_trust`, `polls_eval`, `previous_vote_2003`

Harmonization applies value normalization, derived age groups, within-study de-duplication, and removal of rows that are empty across harmonized variables.

Opaque legacy variables (for example `q10`, `q16`, `voteprec`) are renamed to readable merged names. The old-to-new mapping is available via `attr(master, "variable_name_map")` and in the saved sidecar file `qes_master_variable_name_map.csv`.

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

## Use case examples

Use case examples available in separate tabs:

- [Evolution of sovereignty attitudes](articles/analysis-sovereignty.html)
- [Vote-choice patterns over time](articles/analysis-vote-choice.html)

## Version française

Use the `FR` button in the top bar to switch any main page to its French version.
