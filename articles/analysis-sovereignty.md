# Analysis: Evolution of Sovereignty Attitudes

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)
```

This page shows a concrete trend output. It uses the merged file and
builds a sovereignty proxy from party preference fields.

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
    vote_choice = tolower(trimws(as.character(vote_choice))),
    party_lean = tolower(trimws(as.character(party_lean)))
  )
```

## Sovereignty proxy

Here we classify respondents as `sovereignty_proxy = 1` when party
preference mentions a party typically associated with greater Quebec
sovereignty (`parti québécois` or `québec solidaire`).

``` r
is_sov_pref <- function(x) {
  grepl("parti quebecois|parti québécois|quebec solidaire|québec solidaire", x)
}

sov <- master %>%
  mutate(
    sovereignty_proxy = dplyr::if_else(
      is_sov_pref(vote_choice) | is_sov_pref(party_lean),
      1,
      0,
      missing = 0
    )
  ) %>%
  filter(!is.na(qes_year_num)) %>%
  group_by(qes_year_num) %>%
  summarise(
    support_share = mean(sovereignty_proxy, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(qes_year_num)
```

### Results table

``` r
knitr::kable(
  sov %>%
    mutate(
      support_share = round(100 * support_share, 1)
    ) %>%
    rename(
      year = qes_year_num,
      respondents = n,
      sovereignty_support_pct = support_share
    ),
  caption = "Sovereignty proxy by study year (percent of respondents)"
)
```

| year | sovereignty_support_pct | respondents |
|-----:|------------------------:|------------:|
| 1998 |                    30.1 |        1483 |
| 2007 |                     0.0 |       28167 |
| 2008 |                     0.0 |        1151 |
| 2012 |                     0.0 |        2349 |
| 2014 |                     0.0 |        1517 |
| 2018 |                     4.5 |        4322 |
| 2022 |                    30.4 |        1521 |

Sovereignty proxy by study year (percent of respondents)

### Trend plot

``` r
ggplot(sov, aes(x = qes_year_num, y = support_share)) +
  geom_line(linewidth = 1, color = "#0f4c5c") +
  geom_point(size = 2.5, color = "#e36414") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%")) +
  scale_x_continuous(breaks = sort(unique(sov$qes_year_num))) +
  labs(
    title = "Evolution of sovereignty proxy across QES studies",
    x = "Study year",
    y = "Share of respondents"
  ) +
  theme_minimal(base_size = 12)
```

![Line chart of sovereignty proxy trend over study
years.](analysis-sovereignty_files/figure-html/unnamed-chunk-6-1.png)

## Notes

- This is a descriptive proxy based on party preference fields.
- For publication-grade causal claims, build a study-specific
  sovereignty item harmonization.
