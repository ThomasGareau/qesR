# Use case example: Vote Choice Over Time

Show code used in this page

``` r
library(dplyr)
library(ggplot2)
library(knitr)

master_paths <- c("qes_master.csv", "../qes_master.csv")
master <- read.csv(master_paths[file.exists(master_paths)][1], stringsAsFactors = FALSE)

master <- master %>%
  mutate(
    qes_year_num = as.integer(substr(as.character(qes_year), 1, 4)),
    vote_choice_raw = trimws(as.character(vote_choice)),
    vote_choice_party = dplyr::case_when(
      vote_choice_raw %in% c("CAQ", "ADQ") ~ "CAQ/ADQ",
      vote_choice_raw == "PLQ" ~ "PLQ",
      vote_choice_raw == "PQ" ~ "PQ",
      vote_choice_raw == "QS" ~ "QS",
      vote_choice_raw == "PCQ" ~ "PCQ",
      TRUE ~ NA_character_
    )
  )

major_parties <- c("CAQ/ADQ", "PLQ", "PQ", "QS", "PCQ")
years <- sort(unique(master$qes_year_num[!is.na(master$qes_year_num)]))

vote_base <- master %>%
  filter(!is.na(qes_year_num), !is.na(vote_choice_party), nzchar(vote_choice_party))

vote_den <- vote_base %>%
  group_by(qes_year_num) %>%
  summarise(n_with_vote_choice = n(), .groups = "drop")

vote_party <- vote_base %>%
  group_by(qes_year_num, vote_choice_party) %>%
  summarise(n_party = n(), .groups = "drop")

grid <- expand.grid(
  qes_year_num = years,
  vote_choice_party = major_parties,
  stringsAsFactors = FALSE
)

vote_party <- merge(grid, vote_party, by = c("qes_year_num", "vote_choice_party"), all.x = TRUE, sort = TRUE)
vote_party$n_party[is.na(vote_party$n_party)] <- 0
vote_party <- merge(vote_party, vote_den, by = "qes_year_num", all.x = TRUE, sort = TRUE)

vote_party <- vote_party %>%
  mutate(
    share = ifelse(n_with_vote_choice > 0, n_party / n_with_vote_choice, NA_real_),
    se = ifelse(n_with_vote_choice > 0, sqrt(pmax(share * (1 - share), 0) / n_with_vote_choice), NA_real_),
    ci_low = ifelse(!is.na(se), pmax(0, share - 1.96 * se), NA_real_),
    ci_high = ifelse(!is.na(se), pmin(1, share + 1.96 * se), NA_real_),
    year_index = match(qes_year_num, years)
  )

vote_table <- vote_party %>%
  group_by(qes_year_num) %>%
  summarise(
    n_with_vote_choice = first(n_with_vote_choice),
    caq_adq = 100 * share[vote_choice_party == "CAQ/ADQ"],
    plq = 100 * share[vote_choice_party == "PLQ"],
    pq = 100 * share[vote_choice_party == "PQ"],
    qs = 100 * share[vote_choice_party == "QS"],
    pcq = 100 * share[vote_choice_party == "PCQ"],
    .groups = "drop"
  )

knitr::kable(vote_table)
```

## Table 1: vote-choice shares by year

| Study year | N with vote-choice item | CAQ/ADQ (%) | PLQ (%) | PQ (%) | QS (%) | PCQ (%) |
|-----------:|------------------------:|------------:|--------:|-------:|-------:|--------:|
|       1998 |                    1048 |        26.4 |    31.0 |   42.6 |    0.0 |     0.0 |
|       2007 |                    6852 |        33.6 |    27.7 |   33.9 |    4.9 |     0.0 |
|       2008 |                    8902 |        19.3 |    38.6 |   36.4 |    5.6 |     0.0 |
|       2009 |                    6121 |        10.4 |    40.6 |   41.2 |    7.7 |     0.0 |
|       2010 |                     730 |         7.8 |    41.6 |   41.2 |    9.3 |     0.0 |
|       2012 |                    1813 |        26.2 |    24.5 |   41.5 |    7.8 |     0.0 |
|       2014 |                    1241 |        22.3 |    39.4 |   27.7 |   10.6 |     0.0 |
|       2018 |                    2582 |        37.3 |    26.7 |   19.4 |   16.6 |     0.0 |
|       2022 |                    1196 |        37.6 |    10.9 |   13.7 |   22.0 |    15.8 |

Major-party vote-choice shares by year

## Figure 1: vote-choice trend

![Line chart of vote-choice shares for CAQ/ADQ, PLQ, PQ, QS and PCQ
across study
years.](analysis-vote-choice_files/figure-html/unnamed-chunk-5-1.png)

## Notes

- Shares use respondents with a non-missing `vote_choice` value.
- Years 2009 and 2010 come from `qes_crop_2007_2010` after splitting by
  collection year.
- Confidence intervals are binomial 95% intervals (normal
  approximation).
- The trend figure is restricted to CAQ/ADQ, PLQ, PQ, QS, and PCQ.
