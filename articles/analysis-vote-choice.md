# Use case: Vote Choice Over Time

Show code used in this page

``` r
library(qesR)
library(dplyr)
library(ggplot2)

master <- get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE) %>%
  mutate(
    qes_year_num = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year)),
    vote_choice_raw = trimws(as.character(vote_choice)),
    vote_choice_party = dplyr::case_when(
      vote_choice_raw %in% c("CAQ", "ADQ") ~ "CAQ/ADQ",
      vote_choice_raw == "PLQ" ~ "PLQ",
      vote_choice_raw == "PQ" ~ "PQ",
      vote_choice_raw == "QS" ~ "QS",
      vote_choice_raw == "PCQ" ~ "PCQ",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(qes_code != "qes_crop_2007_2010")

# Compute yearly shares and confidence intervals, then plot.
```

## Table 1: major-party vote-choice shares by year

| Study year | N with vote-choice item | CAQ/ADQ (%) | PLQ (%) | PQ (%) | QS (%) | PCQ (%) |
|-----------:|------------------------:|------------:|--------:|-------:|-------:|--------:|
|       1998 |                    1048 |        26.4 |    31.0 |   42.6 |    0.0 |     0.0 |
|       2007 |                    2823 |        35.0 |    28.3 |   32.3 |    4.4 |     0.0 |
|       2008 |                     870 |        16.6 |    40.5 |   38.6 |    4.4 |     0.0 |
|       2012 |                    1813 |        26.2 |    24.5 |   41.5 |    7.8 |     0.0 |
|       2014 |                    1241 |        22.3 |    39.4 |   27.7 |   10.6 |     0.0 |
|       2018 |                    2582 |        37.3 |    26.7 |   19.4 |   16.6 |     0.0 |
|       2022 |                    1196 |        37.6 |    10.9 |   13.7 |   22.0 |    15.8 |

Major-party vote-choice shares by year (including PCQ)

## Figure 1: major-party vote-choice trend

![Line chart of vote-choice shares for CAQ/ADQ, PLQ, PQ, QS and PCQ
across study
years.](analysis-vote-choice_files/figure-html/unnamed-chunk-5-1.png)

## Notes

- Shares use respondents with a non-missing `vote_choice` value.
- Confidence intervals are binomial 95% intervals (normal
  approximation).
- The trend figure is restricted to CAQ/ADQ, PLQ, PQ, QS, and PCQ.
