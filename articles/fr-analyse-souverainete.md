# Analyse : évolution des attitudes liées à la souveraineté

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)
```

## Chargement

``` r
master <- if (file.exists("qes_master.csv")) {
  read.csv("qes_master.csv", stringsAsFactors = FALSE)
} else {
  get_qes_master(assign_global = FALSE, strict = FALSE, quiet = TRUE)
}

master <- master %>%
  mutate(
    annee = suppressWarnings(as.integer(sub("^([0-9]{4}).*$", "\\1", qes_year))),
    vote_choice = tolower(trimws(as.character(vote_choice))),
    party_lean = tolower(trimws(as.character(party_lean)))
  )
```

## Proxy souverainiste

``` r
est_pref_souverainiste <- function(x) {
  grepl("parti quebecois|parti québécois|quebec solidaire|québec solidaire", x)
}

sov <- master %>%
  mutate(
    appui_souverainete_proxy = dplyr::if_else(
      est_pref_souverainiste(vote_choice) | est_pref_souverainiste(party_lean),
      1, 0, missing = 0
    )
  ) %>%
  filter(!is.na(annee)) %>%
  group_by(annee) %>%
  summarise(taux = mean(appui_souverainete_proxy, na.rm = TRUE), n = n(), .groups = "drop") %>%
  arrange(annee)
```

``` r
knitr::kable(
  sov %>% mutate(taux = round(100 * taux, 1)),
  col.names = c("Année", "Part appui souveraineté (%)", "N"),
  caption = "Évolution du proxy d'appui à la souveraineté"
)
```

| Année | Part appui souveraineté (%) |     N |
|------:|----------------------------:|------:|
|  1998 |                        30.1 |  1483 |
|  2007 |                         0.0 | 28167 |
|  2008 |                         0.0 |  1151 |
|  2012 |                         0.0 |  2349 |
|  2014 |                         0.0 |  1517 |
|  2018 |                         4.5 |  4322 |
|  2022 |                        30.4 |  1521 |

Évolution du proxy d’appui à la souveraineté

``` r
ggplot(sov, aes(x = annee, y = taux)) +
  geom_line(linewidth = 1, color = "#0f4c5c") +
  geom_point(size = 2.5, color = "#e36414") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%")) +
  labs(
    title = "Évolution du proxy de souveraineté",
    x = "Année d'étude",
    y = "Part des répondants"
  ) +
  theme_minimal(base_size = 12)
```

![Graphique de l'évolution du proxy de souveraineté par
année.](fr-analyse-souverainete_files/figure-html/unnamed-chunk-6-1.png)
