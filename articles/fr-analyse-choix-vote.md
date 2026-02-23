# Cas d'usage : évolution du choix de vote

Afficher le code utilisé dans cette page

``` r
library(qesR)
library(dplyr)
library(ggplot2)

master <- get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE) %>%
  mutate(
    annee = as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year)),
    vote_choice_brut = trimws(as.character(vote_choice)),
    choix_vote_parti = dplyr::case_when(
      vote_choice_brut %in% c("CAQ", "ADQ") ~ "CAQ/ADQ",
      vote_choice_brut == "PLQ" ~ "PLQ",
      vote_choice_brut == "PQ" ~ "PQ",
      vote_choice_brut == "QS" ~ "QS",
      vote_choice_brut == "PCQ" ~ "PCQ",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(qes_code != "qes_crop_2007_2010")

# Calcul des parts annuelles et des intervalles de confiance, puis tracé.
```

## Tableau 1 : parts de choix de vote des principaux partis par annee

| Annee d’etude | N avec item choix de vote | CAQ/ADQ (%) | PLQ (%) | PQ (%) | QS (%) | PCQ (%) |
|--------------:|--------------------------:|------------:|--------:|-------:|-------:|--------:|
|          1998 |                      1048 |        26.4 |    31.0 |   42.6 |    0.0 |     0.0 |
|          2007 |                      2823 |        35.0 |    28.3 |   32.3 |    4.4 |     0.0 |
|          2008 |                       870 |        16.6 |    40.5 |   38.6 |    4.4 |     0.0 |
|          2012 |                      1813 |        26.2 |    24.5 |   41.5 |    7.8 |     0.0 |
|          2014 |                      1241 |        22.3 |    39.4 |   27.7 |   10.6 |     0.0 |
|          2018 |                      2582 |        37.3 |    26.7 |   19.4 |   16.6 |     0.0 |
|          2022 |                      1196 |        37.6 |    10.9 |   13.7 |   22.0 |    15.8 |

Parts de choix de vote des principaux partis par annee (incluant le PCQ)

## Figure 1 : tendance du choix de vote des principaux partis

![Graphique des parts de choix de vote pour la CAQ/ADQ, le PLQ, le PQ,
QS et le PCQ selon
l'annee.](fr-analyse-choix-vote_files/figure-html/unnamed-chunk-5-1.png)

## Notes

- Les parts utilisent les répondants avec une valeur non manquante de
  `vote_choice`.
- Les intervalles de confiance sont des IC95 binomiaux (approximation
  normale).
- La figure est limitée à la CAQ/ADQ, au PLQ, au PQ, à QS et au PCQ.
