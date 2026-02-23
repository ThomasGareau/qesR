# Démarrage avec qesR

`qesR` permet de télécharger les études électorales québécoises, leurs
codebooks et un fichier fusionné harmonisé.

## Installation

``` r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("ThomasGareau/qesR")
library(qesR)
```

## Fonctions principales

- [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md)
  : liste des codes d’études
- `get_qes("qes2022")` : téléchargement d’une étude
- `get_codebook("qes2022")` : codebook
- `get_preview("qes2022", 10)` : aperçu rapide
- `get_question(obj, variable)` : texte de question
- [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  : données fusionnées harmonisées

## Exemple rapide

``` r
codes <- get_qescodes(detailed = TRUE)
qes2022 <- get_qes("qes2022")
cb <- get_codebook("qes2022")
master <- get_qes_master(strict = FALSE)
```

## Version française complète

Voir aussi :

- Données fusionnées : `fr-donnees-fusionnees`
- Citations : `fr-citations-etudes`
- Exemples de cas d’usage : `fr-analyse-souverainete`,
  `fr-analyse-descriptive`
