# Harmoniser entre études

*[English
version](https://thomasgareau.github.io/qesR/articles/harmonization.md)*

Cette page va de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
jusqu’à une estimation pondérée, étude par étude, et montre ce que qesR
consigne en chemin : le niveau de comparabilité de la question de chaque
étude, le motif de chaque valeur manquante, la pondération qui convient
à chaque question et la provenance de chaque cellule. Elle est
construite à la construction du site, à partir des fichiers de données
complets, que qesR télécharge de leurs dépôts Dataverse par son cache.

Le moteur d’harmonisation est **expérimental**. Sa spécification
indique, pour chaque étude et chaque variable harmonisée (« cible »),
quelle question alimente la cible et comment chacun de ses codes
correspond aux niveaux de la cible ; rien n’est apparié par le nom. Dans
la spécification 4.3.0, toutes les lignes sauf trois sont approuvées,
après une double révision automatisée sur les fichiers et documents
originaux (et non une révision humaine), et
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
les applique par défaut. Les pondérations recommandées de `qes1998`,
`qes2007_panel`, `qes2012_panel` et des sondages CROP restent à réviser
et valent donc `NA` ; leurs réponses sont harmonisées comme les autres.

``` r

library(qesR)
tr <- function(en, fr) if (identical(params$lang, "fr")) fr else en
# percentages with one decimal, in the page's style
pct <- function(x) formatC(100 * x, format = "f", digits = 1, decimal.mark = tr(".", ","))
```

## Quelles études posent la question

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
donne une ligne par cible et une colonne par étude, chaque cellule
donnant le niveau de comparabilité de la question de cette étude par
rapport à la question d’ancrage de la cible (la grille complète est sur
la [page de
couverture](https://thomasgareau.github.io/qesR/articles/fr-couverture.md)).
Deux cibles ici : la question référendaire sur un Québec pays
indépendant, et le vote déclaré après l’élection.

``` r

grid <- qes_spec(targets = c("sov_indep", "vote_prov_recall"), lang = params$lang)
knitr::kable(grid[, c("target", "label", "qes2022", "qes2018", "qes2014", "qes2012")])
```

| target | label | qes2022 | qes2018 | qes2014 | qes2012 |
|:---|:---|:---|:---|:---|:---|
| vote_prov_recall | Vote provincial (rappel) | comparable | comparable | comparable | identical |
| sov_indep | Vote référendaire : pays indépendant | comparable | comparable | identical | identical |

La vue de correspondance (`"crosswalk"`) dit sur quelle question porte
chaque niveau, et pourquoi :

``` r

xw <- qes_spec("crosswalk", targets = "sov_indep", lang = params$lang)
knitr::kable(xw[, c("study", "wave", "source_var", "grade", "grade_reason")])
```

| study | wave | source_var | grade | grade_reason |
|:---|:---|:---|:---|:---|
| qes2012 | post | q52 | identical | Ligne d’ancrage de la cible. |
| qes2014 | post | Q19 | identical | Même énoncé que l’ancrage en anglais et en français, mêmes options (oui, non, je ne sais pas, je préfère ne pas répondre), posé à tous, Web. |
| qes2018 | post | q26 | comparable | L’énoncé ajoute un adverbe de temps (« today », « aujourd’hui ») ; sinon même libellé et mêmes options que l’ancrage. |
| qes2022 | cps | cps_qc_referendum | comparable | L’énoncé ajoute un adverbe de temps ; « je ne sais pas » est offert, mais pas « je préfère ne pas répondre ». |

`identical` signifie la même question, les mêmes options et le même
univers que la question d’ancrage ; `comparable`, le même stimulus avec
des différences qui ne devraient pas modifier les proportions ;
`approximate`, une différence qui peut les modifier. La [référence de
l’harmonisation](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md)
donne le libellé et les niveaux de la question de chaque étude.

## Harmoniser

`studies = NULL`, la valeur par défaut, retient les Études électorales
québécoises qui ont une question pour au moins une des cibles ; les
panels Durand, les sondages CROP et le panel de 1998 sont laissés de
côté à moins de les nommer. `qes2008` n’a aucune pondération recommandée
(ses deux pondérations sont calées sur le vote ou sur la participation)
: ses lignes n’ont pas de pondération et
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
les laisse de côté plus bas, avec un message. `layout = "long"` donne
une ligne par répondant et par vague : chaque réponse se trouve sur la
ligne de la vague qui l’a posée, à côté de la pondération de cette
vague. `missing = "reasons"` ajoute une colonne `<cible>__na` avec le
motif de chaque valeur manquante.

``` r

h <- qes_harmonize(targets = c("sov_indep", "vote_prov_recall"), layout = "long",
                   missing = "reasons", lang = params$lang)
#> Utilisation de la copie en cache de « 2022 Quebec Election Study v1.dta ».
#> Utilisation de la copie en cache de « Quebec Election Study 2018.dta ».
#> Utilisation de la copie en cache de « Quebec Election Study 2014.sav ».
#> Utilisation de la copie en cache de « Quebec Election Study 2012 (STATA).dta ».
#> Utilisation de la copie en cache de « Quebec Election Study 2012 (SPSS).sav ».
#> Utilisation de la copie en cache de « Quebec Election Study 2008 (SPSS).sav ».
#> Utilisation de la copie en cache de « Quebec Election Study 2007 (SPSS).sav ».
#> Les niveaux que la question d'une étude n'offrait pas sont des zéros structurels, pas une absence d'appui : vote_prov_recall: qes2022 (PVQ, ON, ADQ), qes2018 (PVQ, PCQ, ON, ADQ), qes2014 (PCQ, ADQ), qes2012 (PCQ, ADQ), qes2008 (CAQ, PCQ, ON), qes2007 (CAQ, PCQ, ON); sov_indep: qes2022 (would_not_vote), qes2018 (would_not_vote), qes2014 (would_not_vote), qes2012 (would_not_vote). qes_provenance(x, level = "cell") les énumère.
table(h$study, h$wave)
#>          
#>            cps  pes post
#>   qes2007    0    0 2175
#>   qes2008    0    0 1151
#>   qes2012    0    0 1505
#>   qes2014    0    0 1517
#>   qes2018    0    0 3072
#>   qes2022 1521 1220    0
```

Les messages font partie du résultat. Le premier énumère les *zéros
structurels* : les niveaux que la question d’une étude n’offrait pas,
comme un parti absent de sa liste. Leur part dans cette étude est nulle
parce que personne ne pouvait les choisir, et non parce que personne ne
les appuyait. `qes2022` a posé la question référendaire dans sa vague de
campagne (`cps`) et la question du vote déclaré dans sa vague
postélectorale (`pes`), qui ont des pondérations différentes.

## Pourquoi des valeurs manquent

Chaque valeur manquante a un motif. Nombre de réponses valides (`valid`)
et de chaque motif, pour la question référendaire :

``` r

reasons <- table(h$study, h$sov_indep__na)
reasons <- reasons[, colSums(reasons) > 0, drop = FALSE]
knitr::kable(cbind(valid = tapply(!is.na(h$sov_indep), h$study, sum), reasons[, , drop = FALSE]))
```

|         | valid |  dk | refused | not_in_wave | not_asked |
|:--------|------:|----:|--------:|------------:|----------:|
| qes2007 |     0 |   0 |       0 |           0 |      2175 |
| qes2008 |     0 |   0 |       0 |           0 |      1151 |
| qes2012 |  1323 | 156 |      26 |           0 |         0 |
| qes2014 |  1353 | 148 |      16 |           0 |         0 |
| qes2018 |  2558 | 463 |      51 |           0 |         0 |
| qes2022 |  1284 | 237 |       0 |        1220 |         0 |

`dk` est « je ne sais pas » et `refused` un refus de répondre ;
`not_in_wave` marque les lignes postélectorales de `qes2022`, puisque la
question a été posée dans l’autre vague.
[`?qes_harmonize`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
énumère tous les motifs.

## Niveaux de comparabilité

`min_grade` ne garde que les cellules d’un niveau donné ou meilleur et
met les autres à `NA`, avec le motif `below_grade`. Avec
`min_grade = "identical"`, il ne reste que les études qui ont posé la
question d’ancrage elle-même :

``` r

strict <- qes_harmonize(targets = "sov_indep", layout = "long", missing = "reasons",
                        min_grade = "identical", quiet = TRUE,
                        lang = params$lang)
table(strict$study, strict$sov_indep__na)[, c("dk", "refused", "below_grade")]
#>          
#>             dk refused below_grade
#>   qes2007    0       0           0
#>   qes2008    0       0           0
#>   qes2012  156      26           0
#>   qes2014  148      16           0
#>   qes2018    0       0        3072
#>   qes2022    0       0        1521
```

`qes2018` et `qes2022` ont ajouté « aujourd’hui » à la question (et
`qes2022` n’offrait pas « préfère ne pas répondre ») : les deux sont
classées `comparable` et sont écartées ici. Qu’une cellule `comparable`
ait sa place dans votre analyse est un jugement que la raison du niveau
aide à porter ; qesR ne le porte jamais à votre place.

## Pondérations

Chaque vague a au plus une pondération recommandée, normalisée à une
moyenne de 1 dans chaque étude et chaque vague.
`attr(, "qes_weight_guide")` indique de quelle vague vient chaque cible
et quelle pondération lui convient :

``` r

guide <- attr(h, "qes_weight_guide")
knitr::kable(guide[, c("target", "study", "wave", "weight_column", "weight_var")])
```

| target           | study   | wave | weight_column | weight_var         |
|:-----------------|:--------|:-----|:--------------|:-------------------|
| vote_prov_recall | qes2022 | pes  | weight        | pes_weight_general |
| sov_indep        | qes2022 | cps  | weight        | cps_weight_general |
| vote_prov_recall | qes2018 | post | weight        | pond               |
| sov_indep        | qes2018 | post | weight        | pond               |
| vote_prov_recall | qes2014 | post | weight        | POND               |
| sov_indep        | qes2014 | post | weight        | POND               |
| vote_prov_recall | qes2012 | post | weight        | pond               |
| sov_indep        | qes2012 | post | weight        | pond               |
| vote_prov_recall | qes2008 | post | weight        | NA                 |
| sov_indep        | qes2008 | NA   | NA            | NA                 |
| vote_prov_recall | qes2007 | post | weight        | pond               |
| sov_indep        | qes2007 | NA   | NA            | NA                 |

En disposition longue, la colonne `weight` contient, sur chaque ligne,
la pondération de la vague de cette ligne : un seul plan de sondage sert
donc aux deux cibles.

## Une estimation pondérée

[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
transmet les données au package `survey` : chaque étude est une strate
et, en disposition longue, le répondant est l’unité d’échantillonnage.
Les estimations sont calculées dans chaque étude avec `svyby()` ; le
tableau signale les niveaux qu’une étude n’offrait pas au lieu
d’afficher 0.

``` r

d <- qes_design(h)
#> 1151 ligne(s) sans valeur de « weight » sont laissées hors du plan (hors d'une vague ayant cette pondération, ou pondération à réviser) : qes2008 1151.
```

``` r

cells <- qes_provenance(h, level = "cell")
spec <- qes_spec("spec")

# Weighted percentage of each level of `target` by study, with its
# standard error; levels the study's question did not offer are marked.
share_table <- function(design, target) {
  est <- survey::svyby(stats::as.formula(paste0("~", target)), ~study, design,
                       survey::svymean, na.rm = TRUE)
  lv <- levels(h[[target]])
  tg <- spec$tables$targets
  names_lv <- spec$tables$levels$name[spec$tables$levels$levels_id ==
                                        tg$levels_id[tg$target == target]]
  out <- sapply(lv, function(l) {
    paste0(pct(est[[paste0(target, l)]]), " (", pct(est[[paste0("se.", target, l)]]), ")")
  })
  out <- matrix(out, nrow = nrow(est), dimnames = list(est$study, lv))
  for (s in rownames(out)) {
    off <- cells$levels_not_offered[cells$study == s & cells$target == target]
    gone <- lv[names_lv %in% strsplit(off, ";", fixed = TRUE)[[1]]]
    out[s, gone] <- tr("not offered", "non offert")
  }
  out
}
knitr::kable(share_table(d, "sov_indep"))
```

|         | Oui        | Non        | N’irait pas voter / annulerait |
|:--------|:-----------|:-----------|:-------------------------------|
| qes2007 | 0,0 (0,0)  | 0,0 (0,0)  | 0,0 (0,0)                      |
| qes2012 | 40,4 (1,5) | 59,6 (1,5) | non offert                     |
| qes2014 | 34,8 (1,5) | 65,2 (1,5) | non offert                     |
| qes2018 | 34,6 (1,0) | 65,4 (1,0) | non offert                     |
| qes2022 | 34,3 (1,8) | 65,7 (1,8) | non offert                     |

Chaque cellule est un pourcentage pondéré, avec son erreur type entre
parenthèses. Aucune étude n’offrait « n’irait pas voter » comme réponse
à cette question : ce niveau est un zéro structurel partout. Les erreurs
types traitent chaque échantillon pondéré comme un échantillon
probabiliste. Les quatre études sont des enquêtes Web dont les
pondérations s’ajustent aux marges du recensement : lisez les erreurs
types comme un ordre de grandeur seulement (voir
[`?qes_design`](https://thomasgareau.github.io/qesR/reference/qes_design.md)).

Le vote déclaré décrit l’électorat : gardez les répondants qui pouvaient
voter. `eligible_voter` vaut `TRUE` pour les personnes de 18 ans ou plus
le jour de l’élection et, là où la question est posée, de citoyenneté
canadienne. `qes2018` a échantillonné les personnes de 16 ans et plus :

``` r

table(h$study[h$wave %in% c("post", "pes")],
      h$eligible_voter[h$wave %in% c("post", "pes")], useNA = "ifany")
#>          
#>           FALSE TRUE <NA>
#>   qes2007     0 2133   42
#>   qes2008     0 1142    9
#>   qes2012     0 1484   21
#>   qes2014     0 1517    0
#>   qes2018   255 2799   18
#>   qes2022     0 1220    0
voters <- subset(d, eligible_voter %in% TRUE)
knitr::kable(share_table(voters, "vote_prov_recall"))
```

|  | PLQ | PQ | CAQ | QS | PVQ | PCQ | ON | ADQ | Autre parti |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| qes2007 | 25,3 (1,3) | 31,1 (1,4) | non offert | 4,8 (0,7) | 6,5 (0,8) | non offert | non offert | 31,6 (1,4) | 0,6 (0,2) |
| qes2012 | 25,0 (1,4) | 38,6 (1,6) | 25,5 (1,4) | 6,5 (0,8) | 1,0 (0,3) | non offert | 2,3 (0,4) | non offert | 1,1 (0,3) |
| qes2014 | 35,9 (1,6) | 29,8 (1,6) | 23,1 (1,4) | 8,2 (0,8) | 1,0 (0,3) | non offert | 0,7 (0,3) | non offert | 1,3 (0,3) |
| qes2018 | 23,3 (1,0) | 19,6 (0,9) | 35,8 (1,2) | 16,3 (0,9) | non offert | non offert | non offert | non offert | 5,1 (0,6) |
| qes2022 | 17,1 (2,0) | 15,6 (1,2) | 33,0 (1,9) | 17,2 (1,4) | non offert | 13,5 (1,2) | non offert | non offert | 3,5 (1,0) |

Garder les électeurs admissibles ne recalibre pas les pondérations, qui
visent la population que chaque étude a échantillonnée. Un parti marqué
« non offert » ne figurait pas dans la liste de réponses de cette étude
: sa part n’y est donc pas comparable à celle des autres études.

Regrouper les études en une seule estimation est possible
(`qes_design(pool = "equal")` donne le même total à chaque étude), mais
les études diffèrent par la population, le mode et le libellé :
comparez-les côte à côte, comme ci-dessus, avant de les regrouper.

## Provenance

`qes_provenance(level = "cell")` donne, pour chaque étude et chaque
cible, la ligne de correspondance appliquée, son niveau et son statut de
révision, la pondération, les niveaux non offerts et le nombre de
valeurs valides et de chaque motif de valeur manquante :

``` r

knitr::kable(cells[, c("study", "target", "source_var", "grade", "status", "weight_var",
                       "levels_not_offered", "n_valid", "n_dk", "n_not_in_wave")])
```

| study | target | source_var | grade | status | weight_var | levels_not_offered | n_valid | n_dk | n_not_in_wave |
|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|
| qes2022 | vote_prov_recall | pes_votechoice | comparable | stable | pes_weight_general | PVQ;ON;ADQ | 1101 | 2 | 301 |
| qes2022 | sov_indep | cps_qc_referendum | comparable | stable | cps_weight_general | would_not_vote | 1284 | 237 | 0 |
| qes2018 | vote_prov_recall | q6 | comparable | stable | pond | PVQ;PCQ;ON;ADQ | 2016 | 0 | 0 |
| qes2018 | sov_indep | q26 | comparable | stable | pond | would_not_vote | 2558 | 463 | 0 |
| qes2014 | vote_prov_recall | Q3 | comparable | stable | POND | PCQ;ADQ | 1283 | 0 | 0 |
| qes2014 | sov_indep | Q19 | identical | stable | POND | would_not_vote | 1353 | 148 | 0 |
| qes2012 | vote_prov_recall | q25 | identical | stable | pond | PCQ;ADQ | 1274 | 0 | 0 |
| qes2012 | sov_indep | q52 | identical | stable | pond | would_not_vote | 1323 | 156 | 0 |
| qes2008 | vote_prov_recall | q12a | comparable | stable | NA | CAQ;PCQ;ON | 898 | 2 | 0 |
| qes2008 | sov_indep | NA | NA | NA | NA | NA | 0 | 0 | 0 |
| qes2007 | vote_prov_recall | q12 | comparable | stable | pond | CAQ;PCQ;ON | 1727 | 8 | 0 |
| qes2007 | sov_indep | NA | NA | NA | NA | NA | 0 | 0 | 0 |

Ces comptes portent sur les répondants de l’étude, comme dans la
disposition par répondant : `n_valid` et les comptes des motifs
s’additionnent au nombre de répondants de l’étude, alors que le tableau
de la section *Pourquoi des valeurs manquent* compte les lignes de la
disposition longue. Tous les répondants de `qes2022` ont participé à la
vague de campagne : aucun n’est donc `not_in_wave` pour `sov_indep`,
alors que 301 n’ont pas participé à la vague postélectorale et sont
`not_in_wave` pour `vote_prov_recall`.

`level = "study"` nomme le fichier fixé de chaque étude, et
`level = "spec"` la version de la spécification et l’empreinte de son
contenu ; la même spécification, les mêmes fichiers et la même version
de qesR donnent les mêmes valeurs.
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
cite qesR avec la spécification, puis chaque jeu de données :

``` r

spec_record <- qes_provenance(h, level = "spec")
spec_record[, c("spec_version", "spec_hash", "qesR_version")]
#>   spec_version                        spec_hash qesR_version
#> 1        4.3.0 506f691e420e8d5d5de3657eef556d3d        0.8.0
cat(qes_cite(h, lang = params$lang), sep = "\n\n")
#> Gareau-Paquette, Thomas, 2026, "qesR: Access Quebec Election Study Datasets", package R, version 0.8.0, https://github.com/ThomasGareau/qesR; spécification d'harmonisation 4.3.0 (empreinte du contenu 506f691e420e8d5d5de3657eef556d3d)
#> 
#> Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, "2022 Quebec Election Study", https://doi.org/10.7910/DVN/PAQBDR, Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [licence : CC BY-NC 4.0, https://creativecommons.org/licenses/by-nc/4.0/]
#> 
#> Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, "Étude électorale québécoise 2018", https://doi.org/10.5683/SP3/NWTGWS, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg==
#> 
#> Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2014", https://doi.org/10.5683/SP3/64F7WR, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw==
#> 
#> Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, "Étude électorale québécoise 2012", https://doi.org/10.5683/SP2/WXUPXT, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ==
#> 
#> Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2008", https://doi.org/10.5683/SP2/8KEYU3, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg==
#> 
#> Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, "Étude électorale québécoise 2007", https://doi.org/10.5683/SP2/6XGOKA, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg==
```

## Cinq cibles sur la souveraineté, une variable regroupée

Une cible correspond à un seul stimulus de question. D’autres études ont
posé des questions sur la souveraineté avec d’autres libellés : elles
alimentent d’autres cibles, jamais fusionnées avec `sov_indep`.

``` r

sov <- qes_spec("crosswalk", targets = "sovereignty", lang = params$lang)
sov <- sov[sov$rule != "none", ]
rownames(sov) <- NULL
knitr::kable(sov[, c("target", "study", "source_var", "grade", "weight_var")])
```

| target | study | source_var | grade | weight_var |
|:---|:---|:---|:---|:---|
| sov_indep | qes2012 | q52 | identical | pond |
| sov_indep | qes2014 | Q19 | identical | POND |
| sov_indep | qes2018 | q26 | comparable | pond |
| sov_indep | qes2022 | cps_qc_referendum | comparable | cps_weight_general |
| sov_favour | qes2018_panel | rts_q7 | identical | weight_rts |
| sov_sovereign_country | qes2012_panel | intvoteref | identical | pondam1 |
| sov_partnership_1995 | qes2007 | q19 | identical | pond |
| sov_partnership_1995 | qes2008 | q19 | comparable | NA |
| sov_partnership_1995 | qes1998 | q16a_crop | comparable | ponder3 |
| sov_partnership_1995 | qes2007_panel | intref1 | comparable | pondam1 |
| sov_partnership_1995_push | qes2007 | q19 | identical | pond |
| sov_partnership_1995_push | qes2008 | q19 | comparable | NA |
| sov_partnership_1995_push | qes2007_panel | intref1 | comparable | pondam1 |
| sov_partnership_1995_push | qes1998 | q16a_crop | comparable | ponder3 |

`sov_sovereign_country` porte sur un « pays souverain »
(`qes2012_panel`), `sov_favour` est une échelle d’appui en quatre points
(`qes2018_panel`) et `sov_partnership_1995` reprend la question du
référendum de 1995, la souveraineté assortie d’une offre de partenariat
au reste du Canada (`qes2007`, `qes2008`, `qes2007_panel` et les
répondants CROP de `qes1998`). Parmi ces études, seules `qes2007` et
`qes2018_panel` ont une pondération révisée ; `qes2008` n’en a aucune à
recommander et les pondérations des autres sont encore à réviser :
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
renvoie `NA` pour celles-ci et
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
laisse ces répondants de côté, avec un message. Placez les cibles côte à
côte et lisez chacune à la lumière de sa propre question.

Pour suivre l’appui dans toutes les études malgré tout, la variable
regroupée `sov_support` prend la question référendaire de chaque étude,
quel que soit son libellé, et indique de laquelle vient chaque valeur
(`sov_support__type`), avec le niveau de comparabilité de cette question
(`sov_support__grade`) ; l’échelle favorable ou opposé de
`qes2018_panel` est regroupée en oui ou non, au niveau approximate.
`vote_choice` fait de même pour le vote (le vote déclaré, sinon
l’intention de vote) et `pol_interest` pour l’intérêt pour la politique
:

``` r

pooled <- qes_spec("pooled", targets = "sov_support", lang = params$lang)
knitr::kable(pooled[, c("type_name", "member", "precedence", "default", "grade_cap")])
```

| type_name | member | precedence | default | grade_cap |
|:---|:---|---:|:---|:---|
| independence | sov_indep | 1 | TRUE | NA |
| sovereign_country | sov_sovereign_country | 2 | TRUE | NA |
| partnership_1995_push | sov_partnership_1995_push | 3 | TRUE | NA |
| partnership_1995 | sov_partnership_1995 | 4 | TRUE | NA |
| favour | sov_favour | 5 | TRUE | approximate |
