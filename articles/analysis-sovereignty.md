# Use case: Evolution of Sovereignty Attitudes

This page uses the merged file to estimate sovereignty support over
time.

Show code used in this page

``` r
library(qesR)
library(dplyr)
library(ggplot2)

master <- get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE) %>%
  mutate(qes_year_num = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))) %>%
  filter(qes_code != "qes_crop_2007_2010")

master$sovereignty_support <- suppressWarnings(as.numeric(master$sovereignty_support))
master$sovereignty_support[!(master$sovereignty_support %in% c(0, 1))] <- NA_real_

# Summarize yearly support, confidence intervals, then plot.
```

## Load merged data

## Dedicated sovereignty question

### Results table

| Study year | N respondents | N with sovereignty item | Sovereignty support (%) | 95% CI (%)   |
|-----------:|--------------:|------------------------:|------------------------:|:-------------|
|       1998 |          1483 |                     381 |                    43.3 | 38.3 to 48.3 |
|       2007 |          4237 |                    3534 |                    43.2 | 41.6 to 44.8 |
|       2008 |          1151 |                     989 |                    44.9 | 41.8 to 48.0 |
|       2012 |          2349 |                    2066 |                    40.1 | 38.0 to 42.2 |
|       2014 |          1517 |                    1353 |                    34.1 | 31.6 to 36.7 |
|       2018 |          4322 |                    3338 |                    33.6 | 32.0 to 35.2 |
|       2022 |          1521 |                    1284 |                    36.4 | 33.7 to 39.0 |

Sovereignty support by study year

### Trend plot

![Line chart of sovereignty support trend over study
years.](analysis-sovereignty_files/figure-html/unnamed-chunk-6-1.png)

## Notes

- The sovereignty trend uses the dedicated sovereignty item from each
  study.
- Confidence intervals are binomial 95% intervals using normal
  approximation.
- `n_with_measure` in the table shows where coverage is thinner.
