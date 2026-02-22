# Analysis: Descriptive Statistics

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)
```

## Load merged data

``` r
master <- if (file.exists("qes_master.csv")) {
  read.csv("qes_master.csv", stringsAsFactors = FALSE)
} else {
  get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE)
}

master <- master %>%
  mutate(
    qes_year_num = suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))),
    age_group = trimws(as.character(age_group)),
    education = trimws(as.character(education)),
    turnout = tolower(trimws(as.character(turnout)))
  )
```

## Table 1: sample sizes by study

``` r
sample_sizes <- master %>%
  group_by(qes_year, qes_code, qes_name_en) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(qes_year, qes_code)

knitr::kable(
  sample_sizes,
  caption = "Sample sizes in the merged dataset"
)
```

| qes_year  | qes_code           | qes_name_en                           |     n |
|:----------|:-------------------|:--------------------------------------|------:|
| 1998      | qes1998            | Quebec Elections 1998                 |  1483 |
| 2007      | qes2007            | Quebec Election Study 2007            |  2175 |
| 2007      | qes2007_panel      | Quebec Election Study 2007 Panel      |  2062 |
| 2007-2010 | qes_crop_2007_2010 | CROP Quebec Opinion Polls (2007-2010) | 23930 |
| 2008      | qes2008            | Quebec Election Study 2008            |  1151 |
| 2012      | qes2012            | Quebec Election Study 2012            |  1505 |
| 2012      | qes2012_panel      | Quebec Election Study 2012 Panel      |   844 |
| 2014      | qes2014            | Quebec Election Study 2014            |  1517 |
| 2018      | qes2018            | Quebec Election Study 2018            |  3072 |
| 2018      | qes2018_panel      | Quebec Election Study 2018 Panel      |  1250 |
| 2022      | qes2022            | Quebec Election Study 2022            |  1521 |

Sample sizes in the merged dataset

## Table 2: age-group composition

``` r
age_table <- master %>%
  filter(!is.na(qes_year_num), !is.na(age_group), nzchar(age_group)) %>%
  group_by(qes_year_num, age_group) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup() %>%
  arrange(qes_year_num, desc(pct))

knitr::kable(
  head(age_table, 24),
  digits = 1,
  caption = "Age-group distribution (top rows)"
)
```

| qes_year_num | age_group            |   n |  pct |
|-------------:|:---------------------|----:|-----:|
|         1998 | DE 35 A 44 ANS       | 395 | 26.6 |
|         1998 | DE 45 A 54 ANS       | 299 | 20.2 |
|         1998 | DE 25 A 34 ANS       | 247 | 16.7 |
|         1998 | 65 ANS ET PLUS       | 234 | 15.8 |
|         1998 | DE 55 A 64 ANS       | 179 | 12.1 |
|         1998 | DE 18 A 24 ANS       | 128 |  8.6 |
|         1998 | REFUS/PAS DE REPONSE |   1 |  0.1 |
|         2007 | 35-54 ans            | 914 | 21.8 |
|         2007 | 55 ans et plus       | 709 | 16.9 |
|         2007 | 55-64                | 517 | 12.3 |
|         2007 | 18-34 ans            | 439 | 10.5 |
|         2007 | 45-54                | 422 | 10.1 |
|         2007 | 25-34                | 403 |  9.6 |
|         2007 | 35-44                | 320 |  7.6 |
|         2007 | 65+                  | 314 |  7.5 |
|         2007 | 18-24                | 159 |  3.8 |
|         2008 | 35-44                | 239 | 21.2 |
|         2008 | 45-54                | 238 | 21.2 |
|         2008 | 65+                  | 217 | 19.3 |
|         2008 | 25-34                | 171 | 15.2 |
|         2008 | 55-64                | 170 | 15.1 |
|         2008 | 18-24                |  90 |  8.0 |
|         2012 | 55+                  | 378 | 16.1 |
|         2012 | 35-54                | 339 | 14.4 |

Age-group distribution (top rows)

## Figure 1: age-group profile over time

``` r
ggplot(age_table, aes(x = qes_year_num, y = pct, color = age_group, group = age_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.6) +
  labs(
    title = "Age-group composition across study years",
    x = "Study year",
    y = "Percent of respondents",
    color = "Age group"
  ) +
  theme_minimal(base_size = 12)
```

![Line chart of age-group composition across study
years.](analysis-descriptive_files/figure-html/unnamed-chunk-6-1.png)

## Figure 2: turnout proxy by year

``` r
turnout_year <- master %>%
  filter(!is.na(qes_year_num)) %>%
  mutate(turnout_bin = case_when(
    turnout %in% c("1", "yes", "voted", "certain to vote", "i already voted") ~ 1,
    turnout %in% c("0", "2", "no", "did not vote", "certain not to vote") ~ 0,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(turnout_bin)) %>%
  group_by(qes_year_num) %>%
  summarise(turnout_rate = mean(turnout_bin), n = n(), .groups = "drop")

ggplot(turnout_year, aes(x = qes_year_num, y = turnout_rate)) +
  geom_line(linewidth = 1, color = "#0f4c5c") +
  geom_point(size = 2.5, color = "#e36414") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%")) +
  labs(
    title = "Turnout proxy across study years",
    x = "Study year",
    y = "Share of respondents"
  ) +
  theme_minimal(base_size = 12)
```

![Line chart of turnout proxy across study
years.](analysis-descriptive_files/figure-html/unnamed-chunk-7-1.png)
