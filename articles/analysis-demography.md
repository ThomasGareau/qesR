# Analysis: Demographic Structure Across Studies

``` r
library(qesR)
library(dplyr)
library(ggplot2)
```

This example uses the merged dataset to compare age and education
profiles by study year.

## Load merged data

``` r
master <- get_qes_master(strict = FALSE)
```

## Age-group composition by year

``` r
age_dist <- master %>%
  filter(!is.na(qes_year), !is.na(age_group), nzchar(age_group)) %>%
  group_by(qes_year, age_group) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(p = n / sum(n)) %>%
  ungroup()

ggplot(age_dist, aes(x = qes_year, y = p, fill = age_group)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Age-group composition across QES studies",
    x = "Study year",
    y = "Share of respondents",
    fill = "Age group"
  ) +
  theme_minimal(base_size = 12)
```

## Education profile by year

``` r
edu_dist <- master %>%
  mutate(education = trimws(as.character(education))) %>%
  filter(!is.na(qes_year), !is.na(education), nzchar(education)) %>%
  group_by(qes_year, education) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(qes_year) %>%
  mutate(p = n / sum(n)) %>%
  ungroup()

ggplot(edu_dist, aes(x = qes_year, y = p, color = education, group = education)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 1.3) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Education categories over time",
    x = "Study year",
    y = "Share of respondents",
    color = "Education"
  ) +
  theme_minimal(base_size = 12)
```
