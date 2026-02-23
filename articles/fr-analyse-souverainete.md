# Exemple de cas d'usage : évolution des attitudes liées à la souveraineté

Cette page présente l’évolution de l’appui à la souveraineté dans le
fichier fusionné.

Afficher le code utilisé dans cette page

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)

master <- get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE) %>%
  mutate(annee = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))) %>%
  filter(qes_code != "qes_crop_2007_2010")

master$sovereignty_support <- suppressWarnings(as.numeric(master$sovereignty_support))
master$sovereignty_support[!(master$sovereignty_support %in% c(0, 1))] <- NA_real_

sov <- master %>%
  filter(!is.na(annee)) %>%
  group_by(annee) %>%
  summarise(
    repondants = n(),
    n_mesure = sum(!is.na(sovereignty_support)),
    taux = ifelse(n_mesure > 0, mean(sovereignty_support, na.rm = TRUE), NA_real_),
    se = ifelse(n_mesure > 0, sqrt(pmax(taux * (1 - taux), 0) / n_mesure), NA_real_),
    ci_bas = ifelse(!is.na(se), pmax(0, taux - 1.96 * se), NA_real_),
    ci_haut = ifelse(!is.na(se), pmin(1, taux + 1.96 * se), NA_real_),
    .groups = "drop"
  )

knitr::kable(
  sov %>%
    transmute(
      `Annee d'etude` = annee,
      `N repondants` = repondants,
      `N avec item souverainete` = n_mesure,
      `Appui a la souverainete (%)` = round(100 * taux, 1)
    )
)

ymax_sov <- suppressWarnings(max(sov$ci_haut, sov$taux, na.rm = TRUE))
ymax_sov <- if (is.finite(ymax_sov)) min(1, ymax_sov + 0.05) else 1

ggplot(sov, aes(x = annee, y = taux)) +
  geom_ribbon(aes(ymin = ci_bas, ymax = ci_haut), fill = "#9fb6ce", alpha = 0.35) +
  geom_line(linewidth = 1, color = "#12355b") +
  geom_point(size = 2.3, color = "#b5651d") +
  geom_smooth(method = "loess", se = TRUE, span = 0.9, color = "#12355b", fill = "#c9d6e4", alpha = 0.25, linetype = "dashed", linewidth = 0.8) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%"), limits = c(0, ymax_sov)) +
  scale_x_continuous(breaks = sort(unique(sov$annee))) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(size = 9))
```

## Chargement

## Question directe sur la souveraineté

## Tableau 1 : appui à la souveraineté par année

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

## Figure 1 : tendance de l’appui à la souveraineté

![Graphique de l'évolution de l'appui à la souveraineté par
année.](fr-analyse-souverainete_files/figure-html/unnamed-chunk-6-1.png)

## Notes

- L’analyse repose uniquement sur la variable directe d’appui à la
  souveraineté.
- Les intervalles de confiance sont des IC95 binomiaux (approximation
  normale).
- La colonne `N avec mesure` indique la couverture disponible selon
  l’année.
