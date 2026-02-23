# Use case: Vote Choice Over Time

Show code used in this page

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)

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

major_parties <- c("CAQ/ADQ", "PLQ", "PQ", "QS", "PCQ")

vote_base <- master %>%
  filter(!is.na(qes_year_num), !is.na(vote_choice_party), nzchar(vote_choice_party))

vote_den <- vote_base %>%
  group_by(qes_year_num) %>%
  summarise(n_with_vote_choice = n(), .groups = "drop")

vote_party <- vote_base %>%
  group_by(qes_year_num, vote_choice_party) %>%
  summarise(n_party = n(), .groups = "drop")

grid <- expand.grid(
  qes_year_num = sort(unique(vote_den$qes_year_num)),
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
    ci_high = ifelse(!is.na(se), pmin(1, share + 1.96 * se), NA_real_)
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
  ) %>%
  arrange(qes_year_num)

knitr::kable(
  vote_table %>%
    transmute(
      `Study year` = qes_year_num,
      `N with vote-choice item` = n_with_vote_choice,
      `CAQ/ADQ (%)` = round(caq_adq, 1),
      `PLQ (%)` = round(plq, 1),
      `PQ (%)` = round(pq, 1),
      `QS (%)` = round(qs, 1),
      `PCQ (%)` = round(pcq, 1)
    )
)

vote_party$vote_choice_party <- factor(vote_party$vote_choice_party, levels = major_parties)
ymax_vote <- suppressWarnings(max(vote_party$ci_high, vote_party$share, na.rm = TRUE))
ymax_vote <- if (is.finite(ymax_vote)) min(1, ymax_vote + 0.05) else 1

ggplot(vote_party, aes(x = qes_year_num, y = share, color = vote_choice_party, group = vote_choice_party)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high, fill = vote_choice_party), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.1) +
  geom_smooth(method = "loess", se = FALSE, span = 0.9, linewidth = 0.8, linetype = "dashed") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%"), limits = c(0, ymax_vote)) +
  scale_x_continuous(breaks = sort(unique(vote_party$qes_year_num))) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 9))
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
