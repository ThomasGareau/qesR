# Exemple de cas d'usage : évolution du choix de vote

Afficher le code utilisé dans cette page

``` r
library(qesR)
library(dplyr)
library(ggplot2)
library(knitr)

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

partis_majeurs <- c("CAQ/ADQ", "PLQ", "PQ", "QS", "PCQ")

vote_base <- master %>%
  filter(!is.na(annee), !is.na(choix_vote_parti), nzchar(choix_vote_parti))

vote_den <- vote_base %>%
  group_by(annee) %>%
  summarise(n_avec_choix_vote = n(), .groups = "drop")

vote_parti <- vote_base %>%
  group_by(annee, choix_vote_parti) %>%
  summarise(n_parti = n(), .groups = "drop")

grille <- expand.grid(
  annee = sort(unique(vote_den$annee)),
  choix_vote_parti = partis_majeurs,
  stringsAsFactors = FALSE
)

vote_parti <- merge(grille, vote_parti, by = c("annee", "choix_vote_parti"), all.x = TRUE, sort = TRUE)
vote_parti$n_parti[is.na(vote_parti$n_parti)] <- 0
vote_parti <- merge(vote_parti, vote_den, by = "annee", all.x = TRUE, sort = TRUE)

vote_parti <- vote_parti %>%
  mutate(
    part = ifelse(n_avec_choix_vote > 0, n_parti / n_avec_choix_vote, NA_real_),
    se = ifelse(n_avec_choix_vote > 0, sqrt(pmax(part * (1 - part), 0) / n_avec_choix_vote), NA_real_),
    ci_bas = ifelse(!is.na(se), pmax(0, part - 1.96 * se), NA_real_),
    ci_haut = ifelse(!is.na(se), pmin(1, part + 1.96 * se), NA_real_)
  )

table_vote <- vote_parti %>%
  group_by(annee) %>%
  summarise(
    n_avec_choix_vote = first(n_avec_choix_vote),
    caq_adq = 100 * part[choix_vote_parti == "CAQ/ADQ"],
    plq = 100 * part[choix_vote_parti == "PLQ"],
    pq = 100 * part[choix_vote_parti == "PQ"],
    qs = 100 * part[choix_vote_parti == "QS"],
    pcq = 100 * part[choix_vote_parti == "PCQ"],
    .groups = "drop"
  ) %>%
  arrange(annee)

  knitr::kable(
  table_vote %>%
    transmute(
      `Annee d'etude` = annee,
      `N avec item choix de vote` = n_avec_choix_vote,
      `CAQ/ADQ (%)` = round(caq_adq, 1),
      `PLQ (%)` = round(plq, 1),
      `PQ (%)` = round(pq, 1),
      `QS (%)` = round(qs, 1),
      `PCQ (%)` = round(pcq, 1)
    )
)

vote_parti$choix_vote_parti <- factor(vote_parti$choix_vote_parti, levels = partis_majeurs)

ggplot(vote_parti, aes(x = annee, y = part, color = choix_vote_parti, group = choix_vote_parti)) +
  geom_ribbon(aes(ymin = ci_bas, ymax = ci_haut, fill = choix_vote_parti), alpha = 0.15, linewidth = 0) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 2.1) +
  geom_smooth(method = "loess", se = FALSE, span = 0.9, linewidth = 0.8, linetype = "dashed") +
  scale_y_continuous(labels = function(x) paste0(round(100 * x, 0), "%")) +
  scale_x_continuous(breaks = sort(unique(vote_parti$annee))) +
  theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(), axis.text.x = element_text(size = 9))
```

## Tableau 1 : parts de choix de vote par annee

| Annee d’etude | N avec item choix de vote | CAQ/ADQ (%) | PLQ (%) | PQ (%) | QS (%) | PCQ (%) |
|--------------:|--------------------------:|------------:|--------:|-------:|-------:|--------:|
|          1998 |                      1048 |        26.4 |    31.0 |   42.6 |    0.0 |     0.0 |
|          2007 |                      2823 |        35.0 |    28.3 |   32.3 |    4.4 |     0.0 |
|          2008 |                       870 |        16.6 |    40.5 |   38.6 |    4.4 |     0.0 |
|          2012 |                      1813 |        26.2 |    24.5 |   41.5 |    7.8 |     0.0 |
|          2014 |                      1241 |        22.3 |    39.4 |   27.7 |   10.6 |     0.0 |
|          2018 |                      2582 |        37.3 |    26.7 |   19.4 |   16.6 |     0.0 |
|          2022 |                      1196 |        37.6 |    10.9 |   13.7 |   22.0 |    15.8 |

Parts de choix de vote des principaux partis par annee

## Figure 1 : tendance du choix de vote

![Graphique des parts de choix de vote pour la CAQ/ADQ, le PLQ, le PQ,
QS et le PCQ selon
l'annee.](fr-analyse-choix-vote_files/figure-html/unnamed-chunk-5-1.png)

## Notes

- Les parts utilisent les répondants avec une valeur non manquante de
  `vote_choice`.
- Les intervalles de confiance sont des IC95 binomiaux (approximation
  normale).
- La figure est limitée à la CAQ/ADQ, au PLQ, au PQ, à QS et au PCQ.
