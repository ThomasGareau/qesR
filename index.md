![qesR logo](logo.png)

Access Quebec Election Study datasets in R.

## Installation

Install the development version from GitHub:

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("ThomasGareau/qesR")
```

## Using the package

The package provides access to QES surveys through:

- `get_qes(srvy)`: download a survey by code
- [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md):
  list survey codes and metadata
- `get_preview(srvy, obs)`: preview a survey
- `get_question(data, variable)`: retrieve question text
- `get_codebook(srvy)`: get survey codebook
- [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md):
  build a harmonized merged dataset

## Examples

``` r
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
- codebook metadata is attached and available through
  [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md)
- merged dataset creation includes harmonization, de-duplication, and
  filtering of empty rows
- source-variable provenance is available via
  `attr(master, "source_map")`

## Merged dataset

`qesR` includes a merged harmonized dataset workflow:

``` r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```

See: [Merged
Dataset](https://thomasgareau.github.io/qesR/articles/merged-dataset.md)

## Survey code table

| year      | code                 | name                                  |
|-----------|----------------------|---------------------------------------|
| 2022      | `qes2022`            | Quebec Election Study 2022            |
| 2018      | `qes2018`            | Quebec Election Study 2018            |
| 2018      | `qes2018_panel`      | Quebec Election Study 2018 Panel      |
| 2014      | `qes2014`            | Quebec Election Study 2014            |
| 2012      | `qes2012`            | Quebec Election Study 2012            |
| 2012      | `qes2012_panel`      | Quebec Election Study 2012 Panel      |
| 2007-2010 | `qes_crop_2007_2010` | CROP Quebec Opinion Polls (2007-2010) |
| 2008      | `qes2008`            | Quebec Election Study 2008            |
| 2007      | `qes2007`            | Quebec Election Study 2007            |
| 2007      | `qes2007_panel`      | Quebec Election Study 2007 Panel      |
| 1998      | `qes1998`            | Quebec Elections 1998                 |

## Citations

Full citations for all studies are listed in:

- [Study
  Citations](https://thomasgareau.github.io/qesR/articles/study-citations.md)

## Analyses

Analyses available in separate tabs:

- [Evolution of sovereignty
  attitudes](https://thomasgareau.github.io/qesR/articles/analysis-sovereignty.md)
- [Demographic
  structure](https://thomasgareau.github.io/qesR/articles/analysis-demography.md)
- [Turnout and vote-choice
  patterns](https://thomasgareau.github.io/qesR/articles/analysis-turnout-vote.md)
- [Cross-year merged
  trends](https://thomasgareau.github.io/qesR/articles/analysis-master.md)
