# Cas d'usage : évolution des attitudes liées à la souveraineté

Cette page présente l’évolution de l’appui à la souveraineté dans le
fichier fusionné.

Afficher le code utilisé dans cette page

``` r
library(qesR)
library(dplyr)
library(ggplot2)

master <- get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE) %>%
  mutate(annee = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))) %>%
  filter(qes_code != "qes_crop_2007_2010")

master$sovereignty_support <- suppressWarnings(as.numeric(master$sovereignty_support))
master$sovereignty_support[!(master$sovereignty_support %in% c(0, 1))] <- NA_real_

# Résumer l'appui annuel, calculer les IC95, puis tracer.
```

## Chargement

## Question directe sur la souveraineté

| Annee d’etude | N repondants | N avec item souverainete | Appui a la souverainete (%) | IC 95% (%)  |
|--------------:|-------------:|-------------------------:|----------------------------:|:------------|
|          1998 |         1483 |                      381 |                        43.3 | 38.3 a 48.3 |
|          2007 |         4237 |                     3534 |                        43.2 | 41.6 a 44.8 |
|          2008 |         1151 |                      989 |                        44.9 | 41.8 a 48.0 |
|          2012 |         2349 |                     2066 |                        40.1 | 38.0 a 42.2 |
|          2014 |         1517 |                     1353 |                        34.1 | 31.6 a 36.7 |
|          2018 |         4322 |                     3338 |                        33.6 | 32.0 a 35.2 |
|          2022 |         1521 |                     1284 |                        36.4 | 33.7 a 39.0 |

Appui à la souveraineté par année

![Graphique de l'évolution de l'appui à la souveraineté par
année.](fr-analyse-souverainete_files/figure-html/unnamed-chunk-6-1.png)

## Notes

- L’analyse repose uniquement sur la variable directe d’appui à la
  souveraineté.
- Les intervalles de confiance sont des IC95 binomiaux (approximation
  normale).
- La colonne `N avec mesure` indique la couverture disponible selon
  l’année.
