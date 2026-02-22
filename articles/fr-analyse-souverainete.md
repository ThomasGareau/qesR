# Analyse : évolution des attitudes liées à la souveraineté

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)
```

Cette page présente l’évolution de l’appui à la souveraineté dans le
fichier fusionné.

## Chargement

``` r
master <- if (file.exists("qes_master.csv")) {
  read.csv("qes_master.csv", stringsAsFactors = FALSE)
} else {
  get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE)
}

master <- master %>%
  mutate(
    annee = suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year)))
  )

# Les vagues CROP sont exclues ici pour conserver une série basée sur les
# enquêtes électorales.
master <- master %>%
  filter(qes_code != "qes_crop_2007_2010")

if (!("sovereignty_support" %in% names(master))) {
  master$sovereignty_support <- NA_real_
}
```

## Indicateur d’appui à la souveraineté

``` r
normaliser <- function(x) {
  y <- trimws(as.character(x))
  y <- iconv(y, from = "", to = "ASCII//TRANSLIT")
  y <- tolower(y)
  y <- gsub("['`’]", "", y, perl = TRUE)
  y <- gsub("[^a-z0-9]+", " ", y, perl = TRUE)
  trimws(y)
}

est_pref_souverainiste <- function(x) {
  y <- normaliser(x)
  !is.na(y) & grepl("\\bpq\\b|parti quebecois|quebec solidaire|\\bqs\\b|option nationale", y)
}

a_texte_parti <- function(x) {
  y <- normaliser(x)
  !is.na(y) & grepl("[a-z]", y)
}

sov_item <- suppressWarnings(as.numeric(master$sovereignty_support))
sov_item[!(sov_item %in% c(0, 1))] <- NA_real_

pref_oui <- est_pref_souverainiste(master$vote_choice) | est_pref_souverainiste(master$party_lean)
info_parti <- a_texte_parti(master$vote_choice) | a_texte_parti(master$party_lean)
sov_parti <- ifelse(info_parti, ifelse(pref_oui, 1, 0), NA_real_)

master <- master %>%
  mutate(
    appui_souverainete = ifelse(!is.na(sov_item), sov_item, sov_parti)
  )

annees <- data.frame(
  annee = sort(unique(master$annee[!is.na(master$annee)]))
)

sov <- master %>%
  filter(!is.na(annee)) %>%
  group_by(annee) %>%
  summarise(
    repondants = n(),
    n_mesure = sum(!is.na(appui_souverainete)),
    taux = ifelse(n_mesure > 0, mean(appui_souverainete, na.rm = TRUE), NA_real_),
    .groups = "drop"
  )

sov <- merge(annees, sov, by = "annee", all.x = TRUE, sort = TRUE)
```

``` r
knitr::kable(
  sov %>%
    mutate(
      appui_souverainete_pct = round(100 * taux, 1)
    ) %>%
    select(
      annee,
      repondants,
      n_mesure,
      appui_souverainete_pct
    ),
  col.names = c("Année", "Répondants", "N avec mesure", "Appui souveraineté (%)"),
  caption = "Appui à la souveraineté par année"
)
```

| Année | Répondants | N avec mesure | Appui souveraineté (%) |
|------:|-----------:|--------------:|-----------------------:|
|  1998 |       1483 |          1483 |                   30.1 |
|  2007 |       4237 |          4206 |                   40.6 |
|  2008 |       1151 |          1142 |                   42.6 |
|  2012 |       2349 |          2349 |                   41.1 |
|  2014 |       1517 |          1460 |                   57.9 |
|  2018 |       4322 |           842 |                   29.3 |
|  2022 |       1521 |          1506 |                   35.6 |

Appui à la souveraineté par année

``` r
ggplot(sov, aes(x = annee, y = taux)) +
  geom_line(linewidth = 1, color = "#12355b", na.rm = TRUE) +
  geom_point(size = 2.3, color = "#b5651d", na.rm = TRUE) +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%")) +
  labs(
    title = "Évolution de l'appui à la souveraineté",
    x = "Année d'étude",
    y = "Part des répondants"
  ) +
  theme_minimal(base_size = 12)
```

![Graphique de l'évolution de l'appui à la souveraineté par
année.](fr-analyse-souverainete_files/figure-html/unnamed-chunk-6-1.png)

## Notes

- Les questions directes sur la souveraineté sont utilisées en priorité.
- Quand elles sont absentes, un indicateur basé sur la préférence
  partisane est utilisé.
- La colonne `N avec mesure` indique la couverture disponible selon
  l’année.
