# Use case: Evolution of Sovereignty Attitudes

This page uses the merged file to estimate sovereignty support over
time.

Show code used in this page

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)

master <- get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE) %>%
  mutate(qes_year_num = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))) %>%
  filter(qes_code != "qes_crop_2007_2010")

master$sovereignty_support <- suppressWarnings(as.numeric(master$sovereignty_support))
master$sovereignty_support[!(master$sovereignty_support %in% c(0, 1))] <- NA_real_

sov <- master %>%
  filter(!is.na(qes_year_num)) %>%
  group_by(qes_year_num) %>%
  summarise(
    respondents = n(),
    n_with_measure = sum(!is.na(sovereignty_support)),
    support_share = ifelse(n_with_measure > 0, mean(sovereignty_support, na.rm = TRUE), NA_real_),
    se = ifelse(n_with_measure > 0, sqrt(pmax(support_share * (1 - support_share), 0) / n_with_measure), NA_real_),
    ci_low = ifelse(!is.na(se), pmax(0, support_share - 1.96 * se), NA_real_),
    ci_high = ifelse(!is.na(se), pmin(1, support_share + 1.96 * se), NA_real_),
    .groups = "drop"
  )

knitr::kable(
  sov %>%
    transmute(
      `Study year` = qes_year_num,
      `N respondents` = respondents,
      `N with sovereignty item` = n_with_measure,
      `Sovereignty support (%)` = round(100 * support_share, 1)
    )
)

ymax_sov <- suppressWarnings(max(sov$ci_high, sov$support_share, na.rm = TRUE))
ymax_sov <- if (is.finite(ymax_sov)) min(1, ymax_sov + 0.05) else 1

ggplot(sov, aes(x = qes_year_num, y = support_share)) +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), fill = "#9fb6ce", alpha = 0.35) +
  geom_line(linewidth = 1, color = "#12355b") +
  geom_point(size = 2.3, color = "#b5651d") +
  geom_smooth(method = "loess", se = TRUE, span = 0.9, color = "#12355b", fill = "#c9d6e4", alpha = 0.25, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%"), limits = c(0, ymax_sov)) +
  scale_x_continuous(breaks = sort(unique(sov$qes_year_num))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(size = 9))
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
