# Getting Started with qesR

`qesR` helps you access Quebec Election Study data by survey code, plus
metadata, codebooks, and a merged harmonized dataset.

## Install

``` r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
library(qesR)
```

## Using the package

Core calls:

- `get_qes(srvy)` download a survey by code
- [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md)
  list available codes
- `get_preview(srvy, obs)` preview observations
- `get_question(data, variable)` retrieve survey question text
- `get_codebook(srvy)` retrieve codebook
- [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  build merged harmonized data

## List available studies

``` r
get_qescodes()
get_qescodes(detailed = TRUE)
```

## Download one study

``` r
qes2022 <- get_qes("qes2022")
names(qes2022)[1:20]
```

## Retrieve the codebook in different layouts

``` r
cb_compact <- get_codebook("qes2022", layout = "compact")
cb_wide <- get_codebook("qes2022", layout = "wide")
cb_long <- get_codebook("qes2022", layout = "long")
```

## Get a preview and question text

``` r
get_preview("qes2022", 10)
get_question(qes2022, "cps_age_in_years")
get_question(qes2022, "cps_age_in_years", full = TRUE)
```

## Build a harmonized master dataset

``` r
master <- get_qes_master(
  surveys = c("qes2022", "qes2018", "qes2014", "qes2007", "qes1998"),
  strict = FALSE
)

dim(master)
head(master)
```

## Details

- Surveys are loaded in labelled form with available variable/value
  labels.
- Codebook metadata can be inspected in compact, wide, or long layout.
- Merged data includes harmonized variables, de-duplication, and
  source-map tracing.

## Save the master dataset

``` r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```
