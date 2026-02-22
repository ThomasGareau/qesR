# Analysis: Evolution of Sovereignty Attitudes

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)
```

This page uses the merged file to estimate sovereignty support over
time.

## Load merged data

``` r
master <- if (file.exists("qes_master.csv")) {
  read.csv("qes_master.csv", stringsAsFactors = FALSE)
} else {
  get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE)
}

master <- master %>%
  mutate(
    qes_year_num = suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year)))
  )

# CROP polling waves are useful for other questions, but the sovereignty trend
# below is restricted to election-study files.
master <- master %>%
  filter(qes_code != "qes_crop_2007_2010")

if (!("sovereignty_support" %in% names(master))) {
  master$sovereignty_support <- NA_real_
}
```

## Build sovereignty indicator

``` r
normalize_text <- function(x) {
  y <- trimws(as.character(x))
  y <- iconv(y, from = "", to = "ASCII//TRANSLIT")
  y <- tolower(y)
  y <- gsub("['`’]", "", y, perl = TRUE)
  y <- gsub("[^a-z0-9]+", " ", y, perl = TRUE)
  trimws(y)
}

is_sov_party <- function(x) {
  y <- normalize_text(x)
  !is.na(y) & grepl("\\bpq\\b|parti quebecois|quebec solidaire|\\bqs\\b|option nationale", y)
}

has_party_text <- function(x) {
  y <- normalize_text(x)
  !is.na(y) & grepl("[a-z]", y)
}

sov_item <- suppressWarnings(as.numeric(master$sovereignty_support))
sov_item[!(sov_item %in% c(0, 1))] <- NA_real_

party_yes <- is_sov_party(master$vote_choice) | is_sov_party(master$party_lean)
party_info <- has_party_text(master$vote_choice) | has_party_text(master$party_lean)
sov_party <- ifelse(party_info, ifelse(party_yes, 1, 0), NA_real_)

master <- master %>%
  mutate(
    sovereignty_final = ifelse(!is.na(sov_item), sov_item, sov_party)
  )

all_years <- data.frame(
  qes_year_num = sort(unique(master$qes_year_num[!is.na(master$qes_year_num)]))
)

sov <- master %>%
  filter(!is.na(qes_year_num)) %>%
  group_by(qes_year_num) %>%
  summarise(
    respondents = n(),
    n_with_measure = sum(!is.na(sovereignty_final)),
    support_share = ifelse(n_with_measure > 0, mean(sovereignty_final, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

sov <- merge(all_years, sov, by = "qes_year_num", all.x = TRUE, sort = TRUE)
```

### Results table

``` r
knitr::kable(
  sov %>%
    mutate(
      sovereignty_support_pct = round(100 * support_share, 1)
    ) %>%
    select(
      year = qes_year_num,
      respondents,
      n_with_measure,
      sovereignty_support_pct
    ),
  caption = "Sovereignty support by study year"
)
```

| year | respondents | n_with_measure | sovereignty_support_pct |
|-----:|------------:|---------------:|------------------------:|
| 1998 |        1483 |           1483 |                    30.1 |
| 2007 |        4237 |           4206 |                    40.6 |
| 2008 |        1151 |           1142 |                    42.6 |
| 2012 |        2349 |           2349 |                    41.1 |
| 2014 |        1517 |           1460 |                    57.9 |
| 2018 |        4322 |            842 |                    29.3 |
| 2022 |        1521 |           1506 |                    35.6 |

Sovereignty support by study year

### Trend plot

``` r
ggplot(sov, aes(x = qes_year_num, y = support_share)) +
  geom_line(linewidth = 1, color = "#12355b", na.rm = TRUE) +
  geom_point(size = 2.3, color = "#b5651d", na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%")) +
  scale_x_continuous(breaks = sort(unique(sov$qes_year_num))) +
  labs(
    title = "Sovereignty support across QES studies",
    x = "Study year",
    y = "Share of respondents"
  ) +
  theme_minimal(base_size = 12)
```

![Line chart of sovereignty support trend over study
years.](analysis-sovereignty_files/figure-html/unnamed-chunk-6-1.png)

## Notes

- Direct sovereignty items are used when available.
- Party-based fallback is used when a direct item is missing.
- `n_with_measure` in the table shows where coverage is thinner.
