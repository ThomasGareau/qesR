# Exemple de cas d'usage : évolution des attitudes liées à la souveraineté

Cette page utilise le fichier fusionne pour estimer l’appui a la
souverainete dans le temps.

Afficher le code utilise dans cette page

``` r
library(dplyr)
library(ggplot2)
library(knitr)

master_paths <- c("qes_master.csv", "../qes_master.csv")
master <- read.csv(master_paths[file.exists(master_paths)][1], stringsAsFactors = FALSE)

master <- master %>%
  mutate(
    qes_year_chr = trimws(as.character(qes_year)),
    qes_year_num = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year_chr))
  )

is_range <- grepl("^[0-9]{4}-[0-9]{4}$", master$qes_year_chr)
start_year <- suppressWarnings(as.numeric(sub("^([0-9]{4})-([0-9]{4})$", "\\1", master$qes_year_chr)))
end_year <- suppressWarnings(as.numeric(sub("^([0-9]{4})-([0-9]{4})$", "\\2", master$qes_year_chr)))

master$periode_etude <- ifelse(
  is_range,
  master$qes_year_chr,
  ifelse(!is.na(master$qes_year_num), as.character(master$qes_year_num), master$qes_year_chr)
)
master$ordre_periode <- ifelse(
  is_range & is.finite(start_year) & is.finite(end_year),
  (start_year + end_year) / 2,
  master$qes_year_num
)

master$sovereignty_support <- suppressWarnings(as.numeric(master$sovereignty_support))
master$sovereignty_support[!(master$sovereignty_support %in% c(0, 1))] <- NA_real_

toutes_periodes <- master %>%
  filter(!is.na(periode_etude), nzchar(periode_etude), !is.na(ordre_periode)) %>%
  distinct(periode_etude, ordre_periode)

sov <- master %>%
  filter(!is.na(periode_etude), nzchar(periode_etude), !is.na(ordre_periode)) %>%
  group_by(periode_etude, ordre_periode) %>%
  summarise(
    repondants = n(),
    n_avec_mesure = sum(!is.na(sovereignty_support)),
    part_appui = ifelse(n_avec_mesure > 0, mean(sovereignty_support, na.rm = TRUE), NA_real_),
    se = ifelse(n_avec_mesure > 0, sqrt(pmax(part_appui * (1 - part_appui), 0) / n_avec_mesure), NA_real_),
    ic_bas = ifelse(!is.na(se), pmax(0, part_appui - 1.96 * se), NA_real_),
    ic_haut = ifelse(!is.na(se), pmin(1, part_appui + 1.96 * se), NA_real_),
    .groups = "drop"
  )

sov <- merge(toutes_periodes, sov, by = c("periode_etude", "ordre_periode"), all.x = TRUE, sort = TRUE)
sov <- sov %>% arrange(ordre_periode, periode_etude)
sov$indice_periode <- seq_len(nrow(sov))

knitr::kable(sov)
```

Les estimations ci-dessous utilisent la question directe sur la
souverainete harmonisee entre les periodes.

## Tableau 1 : appui a la souverainete par periode d’etude

| Periode d’etude | N repondants | N avec item souverainete | Appui a la souverainete (%) | IC 95% (%)  |
|:----------------|-------------:|-------------------------:|----------------------------:|:------------|
| 1998            |         1483 |                      381 |                        43.3 | 38.3 a 48.3 |
| 2007            |         9244 |                     8207 |                        40.1 | 39.0 a 41.1 |
| 2008            |        11162 |                    10379 |                        40.1 | 39.2 a 41.0 |
| 2009            |         8008 |                     7455 |                        39.3 | 38.2 a 40.4 |
| 2010            |         1000 |                      923 |                        39.9 | 36.7 a 43.0 |
| 2012            |         2349 |                     2066 |                        40.1 | 38.0 a 42.2 |
| 2014            |         1517 |                     1353 |                        34.1 | 31.6 a 36.7 |
| 2018            |         4322 |                     3338 |                        33.6 | 32.0 a 35.2 |
| 2022            |         1521 |                     1284 |                        36.4 | 33.7 a 39.0 |

Appui a la souverainete par periode d’etude

## Figure 1 : tendance de l’appui a la souverainete

![Graphique de l'evolution de l'appui a la souverainete par periode
d'etude.](fr-analyse-souverainete_files/figure-html/unnamed-chunk-5-1.png)

## Notes

- La tendance utilise la question directe sur la souverainete de chaque
  etude.
- Le jeu `qes_crop_2007_2010` est ventile en 2007, 2008, 2009 et 2010 a
  partir de sa variable de date de collecte.
- Les intervalles de confiance sont des IC95 binomiaux (approximation
  normale).
- La colonne `N avec item souverainete` indique la couverture disponible
  selon la periode.
