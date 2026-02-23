# Données fusionnées harmonisées

`qesR` offre un fichier fusionné inter-études via
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md).

## Construire la base fusionnée

``` r
library(qesR)

master <- get_qes_master(strict = FALSE)
dim(master)
head(master)
```

## Variables harmonisées

Le fichier fusionné inclut notamment :

- métadonnées d’étude : `qes_code`, `qes_year`, `qes_name_en`
- champs répondant/entrevue : `respondent_id`, `interview_start`,
  `interview_end`, `interview_recorded`
- variables sociodémographiques : `year_of_birth`, `age`, `age_group`,
  `gender`, `education`, `language`, `province_territory`
- variables politiques : `turnout`, `vote_choice`, `vote_choice_text`,
  `party_best`, `party_lean`, `ideology`, `political_interest`,
  `sovereignty_support`, `sovereignty`
- champs additionnels (si disponibles) : `citizenship`, `born_canada`,
  `income`, `religion`, `federal_pid`, `provincial_pid`, `survey_weight`

La fusion applique aussi :

- une normalisation des valeurs pour rendre les réponses comparables
- la dérivation de `age_group` lorsque l’âge ou l’année de naissance est
  disponible
- la conservation des répondants panel et non-panel (pas de fusion entre
  études)
- le suivi de la provenance via `attr(master, "source_map")`

La déduplication s’applique uniquement à l’intérieur d’un même code
d’étude (pas entre enquêtes panel et non-panel).

## Exporter

``` r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```
