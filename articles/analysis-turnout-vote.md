# Analysis: Turnout and Vote Choice Patterns

``` r
library(qesR)
library(dplyr)
library(ggplot2)
```

This example uses the merged dataset to show turnout and vote-choice
analyses.

## Load merged data

``` r
master <- get_qes_master(strict = FALSE)
```

## Turnout by age group

``` r
recode_turnout <- function(x) {
  txt <- tolower(trimws(as.character(x)))
  out <- rep(NA_real_, length(txt))
  out[txt %in% c("1", "yes", "voted", "certain to vote", "i already voted")] <- 1
  out[txt %in% c("0", "2", "no", "did not vote", "certain not to vote")] <- 0
  out
}

turnout_age <- master %>%
  mutate(turnout_bin = recode_turnout(turnout)) %>%
  filter(!is.na(turnout_bin), !is.na(qes_year), !is.na(age_group), nzchar(age_group)) %>%
  group_by(qes_year, age_group) %>%
  summarise(turnout_rate = mean(turnout_bin), n = n(), .groups = "drop")

ggplot(turnout_age, aes(x = qes_year, y = turnout_rate, color = age_group, group = age_group)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Turnout by age group across studies",
    x = "Study year",
    y = "Turnout rate",
    color = "Age group"
  ) +
  theme_minimal(base_size = 12)
```

## Top vote-choice categories by year

``` r
top_votes <- master %>%
  mutate(vote_choice = trimws(as.character(vote_choice))) %>%
  filter(!is.na(vote_choice), nzchar(vote_choice), !is.na(qes_year)) %>%
  group_by(qes_year, vote_choice) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(qes_year) %>%
  slice_max(order_by = n, n = 5, with_ties = FALSE) %>%
  ungroup()

ggplot(top_votes, aes(x = reorder(vote_choice, n), y = n, fill = qes_year)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ qes_year, scales = "free_y") +
  coord_flip() +
  labs(
    title = "Most frequent vote-choice categories by study",
    x = "Vote choice",
    y = "Respondents"
  ) +
  theme_minimal(base_size = 12)
```
