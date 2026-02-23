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

Colonnes harmonisées actuelles de `qes_master` :

`qes_code`, `qes_year`, `qes_name_en`, `respondent_id`,
`interview_start`, `interview_end`, `interview_recorded`, `language`,
`citizenship`, `year_of_birth`, `age`, `age_group`, `gender`,
`province_territory`, `education`, `income`, `religion`, `born_canada`,
`political_interest`, `ideology`, `turnout`, `vote_choice`,
`vote_choice_text`, `party_best`, `party_lean`, `sovereignty_support`,
`sovereignty`, `federal_pid`, `provincial_pid`, `survey_weight`,
`definibin`, `ethn1`, `influperso`, `luentend`, `occup`, `patron`,
`pondam1`, `q10`, `q16`, `q17`, `q18`, `q18a`, `q18b`, `q20`, `q20b`,
`q22`, `q23`, `q24`, `q3`, `q31`, `q33`, `q35`, `q37`, `q38`, `q39`,
`q4`, `q40`, `q41`, `q42`, `q43`, `q44`, `q45`, `q46`, `q47`, `q48`,
`q49`, `q50`, `q51`, `q53`, `q54`, `q55`, `q56`, `q58`, `q59`, `q60`,
`q61b`, `q61d`, `q62a`, `q62b`, `q64`, `q65`, `q66`, `q68`, `q7`, `q70`,
`q72`, `q73`, `q74`, `q79`, `q8`, `q80`, `q81`, `q9`, `raison1`,
`raison2`, `s_jse`, `satisf`, `sefie`, `sondbons`, `voteprec`

La fusion applique aussi :

- une normalisation des valeurs pour rendre les réponses comparables
- la dérivation de `age_group` lorsque l’âge ou l’année de naissance est
  disponible
- la conservation des répondants panel et non-panel (pas de fusion entre
  études)
- le suivi de la provenance via `attr(master, "source_map")`
- la liste des variables inter-études ajoutées via
  `attr(master, "crossstudy_variables_added")`

La déduplication s’applique uniquement à l’intérieur d’un même code
d’étude (pas entre enquêtes panel et non-panel).

## Exporter

``` r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```
