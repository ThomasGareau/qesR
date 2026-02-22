# Analyse : statistiques descriptives

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)
```

## Chargement des données fusionnées

``` r
master <- if (file.exists("qes_master.csv")) {
  read.csv("qes_master.csv", stringsAsFactors = FALSE)
} else {
  get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE)
}

master <- master %>%
  mutate(
    annee = suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))),
    age_group = trimws(as.character(age_group)),
    turnout = tolower(trimws(as.character(turnout)))
  )
```

## Taille des échantillons

``` r
tailles <- master %>%
  group_by(qes_year, qes_code, qes_name_en) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(qes_year, qes_code)

knitr::kable(tailles, caption = "Taille d'échantillon par étude")
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

Taille d’échantillon par étude

## Répartition des groupes d’âge

``` r
age_dist <- master %>%
  filter(!is.na(annee), !is.na(age_group), nzchar(age_group)) %>%
  group_by(annee, age_group) %>%
  summarise(n = n(), .groups = "drop_last") %>%
  mutate(pct = 100 * n / sum(n)) %>%
  ungroup()

ggplot(age_dist, aes(x = annee, y = pct, color = age_group, group = age_group)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Répartition des groupes d'âge dans le temps",
    x = "Année d'étude",
    y = "Pourcentage",
    color = "Groupe d'âge"
  ) +
  theme_minimal(base_size = 12)
```

![Graphique de la répartition des groupes d'âge selon
l'année.](fr-analyse-descriptive_files/figure-html/unnamed-chunk-5-1.png)

## Participation électorale (proxy)

``` r
turnout_annee <- master %>%
  filter(!is.na(annee)) %>%
  mutate(turnout_bin = case_when(
    turnout %in% c("1", "yes", "voted", "certain to vote", "i already voted") ~ 1,
    turnout %in% c("0", "2", "no", "did not vote", "certain not to vote") ~ 0,
    TRUE ~ NA_real_
  )) %>%
  filter(!is.na(turnout_bin)) %>%
  group_by(annee) %>%
  summarise(taux = mean(turnout_bin), n = n(), .groups = "drop")

knitr::kable(
  turnout_annee %>% mutate(taux = round(100 * taux, 1)),
  col.names = c("Année", "Taux de participation (%)", "N"),
  caption = "Proxy de participation par année"
)
```

| Année | Taux de participation (%) |    N |
|------:|--------------------------:|-----:|
|  1998 |                      87.0 | 1374 |
|  2018 |                      15.3 | 1826 |
|  2022 |                      98.0 | 1322 |

Proxy de participation par année
