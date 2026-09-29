# Validation par les résultats officiels et le recensement

*[English
version](https://thomasgareau.github.io/qesR/articles/validation.md)*

Jusqu’où les études harmonisées rejoignent-elles ce que l’on sait de
l’électorat ? Cette page compare, pour chaque étude que couvre la
spécification d’harmonisation :

1.  le **vote déclaré** aux résultats officiels de chaque élection
    générale, publiés par Élections Québec ;
2.  la **participation déclarée** à la participation officielle ;
3.  le **genre, l’âge, la langue maternelle et la scolarité** des
    répondants au recensement qui précède l’étude (Statistique Canada) ;
4.  quelques **relations entre les réponses** que toute mesure valide
    devrait montrer (validité de construit).

La page est construite à la construction du site : les fichiers de
données sont téléchargés depuis leurs dépôts Dataverse par le cache de
qesR et harmonisés avec
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md).
Les données de référence se trouvent dans `inst/extdata/validation/` du
dépôt source de qesR, chaque ligne avec sa source ; les marges du
recensement sont aussi fournies avec le paquet, les résultats officiels
d’Élections Québec et le rapport consigné ne le sont pas (leurs
conditions d’utilisation exigent l’autorisation écrite d’Élections
Québec, qui est en attente). Les mêmes vérifications tournent chaque
semaine sur les fichiers épinglés et échouent lorsque l’indice pondéré
du vote déclaré (vérification V-L2 de la spécification) ou un indice
pondéré du recensement dépasse de plus de 2 points sa valeur consignée,
lorsque la surdéclaration de la participation sort de l’intervalle de 0
à 35 points, ou lorsqu’une direction de validité de construit
(vérification V-L4) échoue. Un indice qui baisse, ou qui monte moins,
passe.

``` r

library(qesR)
library(ggplot2)
```

``` r

tr <- function(en, fr) if (identical(params$lang, "fr")) fr else en
num <- function(x, digits = 1, sign = FALSE) {
  formatC(x, format = "f", digits = digits, flag = if (sign) "+" else "",
          decimal.mark = tr(".", ","))
}
# every study the specification covers, harmonized from the pinned files,
# against the benchmarks that ship with qesR
report <- qesR:::.qes_validation_run("all")
report <- qesR:::.qes_validation_gate(report, qesR:::.qes_validation_recorded())
studies <- qes_studies()
report$year <- studies$year[match(report$study, studies$study)]
report$weighting <- ifelse(report$weight == "none", tr("unweighted", "non pondéré"),
                           tr("weighted", "pondéré"))
# one row per study and check: the weighted value where the study has a
# reviewed weight, the unweighted one otherwise
best <- function(x) {
  x <- x[order(x$study, x$variable, x$weight == "none"), ]
  x[!duplicated(paste(x$study, x$variable)), ]
}
```

## 1. Vote déclaré et résultats officiels

Pour chaque étude, le tableau donne l’**indice de dissimilarité** entre
la distribution du vote déclaré et les parts officielles des votes
valides : la moitié de la somme des écarts absolus entre les deux
distributions, en points. C’est la part des répondants qui devraient
changer de parti pour que les deux concordent ; 0 est une concordance
parfaite. Les partis que la question de l’étude n’énumérait pas sont
comptés comme « autre parti » du côté officiel, puisque c’est la seule
réponse que leurs électeurs pouvaient donner.

``` r

idx <- report[report$check == "recall" & is.na(report$level), ]
wide <- reshape(idx[, c("study", "year", "reference", "weighting", "n", "value")],
                idvar = c("study", "year", "reference"), timevar = "weighting",
                direction = "wide", drop = "n")
wide <- merge(wide, aggregate(n ~ study, idx, max), by = "study")
wide <- wide[order(wide$year, wide$study), ]
un <- wide[[paste0("value.", tr("unweighted", "non pondéré"))]]
we <- wide[[paste0("value.", tr("weighted", "pondéré"))]]
knitr::kable(
  data.frame(wide$study, wide$reference, wide$n, num(un),
             ifelse(is.na(we), "", num(we))),
  col.names = tr(c("Study", "Election", "N (reported a party)", "Index, unweighted", "Index, weighted"),
                 c("Étude", "Élection", "N (ont nommé un parti)", "Indice, non pondéré", "Indice, pondéré")),
  align = c("l", "l", "r", "r", "r")
)
```

| Étude | Élection | N (ont nommé un parti) | Indice, non pondéré | Indice, pondéré |
|:---|:---|---:|---:|---:|
| qes1998 | QC1998 | 1126 | 9,4 |  |
| qes2007 | QC2007 | 1727 | 7,7 | 7,3 |
| qes2007_panel | QC2007 | 1494 | 4,5 |  |
| qes2008 | QC2008 | 898 | 3,2 |  |
| qes2012 | QC2012 | 1274 | 11,0 | 8,0 |
| qes2012_panel | QC2012 | 633 | 8,7 |  |
| qes2014 | QC2014 | 1283 | 4,9 | 5,6 |
| qes2018 | QC2018 | 2016 | 3,2 | 3,2 |
| qes2018_panel | QC2018 | 704 | 5,5 | 5,5 |
| qes2022 | QC2022 | 1101 | 9,0 | 8,0 |

L’indice pondéré utilise la pondération que la spécification recommande
pour la vague qui a posé la question ; `qes2007`, `qes2012`, `qes2014`,
`qes2018`, `qes2022` et le panel de 2018 ont pour l’instant une
pondération révisée. Les pondérations de `qes2007_panel`, de
`qes2012_panel`, des sondages CROP et de `qes1998` ne sont pas assez
documentées pour être utilisées (leurs lignes sont `needs_review`), et
les deux pondérations de `qes2008` sont calées sur le vote ou sur la
participation : leur indice est non pondéré. `qes1998` n’a interrogé que
des francophones, une population que les résultats officiels ne
décrivent pas : l’étude est montrée à titre indicatif, sans être
vérifiée.

``` r

# the weighted distribution where the study has a reviewed weight
shown <- best(idx[idx$study != "qes1998", ])
lv <- report[report$check == "recall" & !is.na(report$level) &
               paste(report$study, report$weight) %in% paste(shown$study, shown$weight), ]
lv$party <- ifelse(lv$level == "other", tr("Other", "Autre"), lv$level)
lv$label <- paste0(lv$study, " (", lv$weighting, ")")
lv$label <- factor(lv$label, unique(lv$label[order(lv$year, lv$study)]))
ggplot(lv, aes(x = value, y = party)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_segment(aes(x = 0, xend = value, yend = party), colour = "grey70") +
  geom_point(size = 2.2, colour = "#0468b9") +
  facet_wrap(~label, ncol = 3, scales = "free_y") +
  labs(
    x = tr("Reported minus official share (points)", "Part déclarée moins part officielle (points)"),
    y = NULL
  ) +
  theme_minimal(base_size = 11)
```

![Pour chaque étude, l'écart en points entre la part du vote déclaré de
chaque parti et sa part officielle des votes valides ; pondéré lorsque
l'étude a une pondération
révisée.](fr-validation_files/figure-html/recall-plot-1.png)

## 2. Participation déclarée

``` r

to <- best(report[report$check == "turnout", ])
to <- to[order(to$year, to$study), ]
# qes1998 (francophones only) is shown but not checked
checked <- to[!(to$status %in% "skipped"), ]
knitr::kable(
  data.frame(to$study, to$reference, to$weighting, to$n, num(to$estimate), num(to$benchmark),
             num(to$value, sign = TRUE)),
  col.names = tr(c("Study", "Election", "Weighting", "N", "Reported turnout (%)", "Official turnout (%)", "Difference (points)"),
                 c("Étude", "Élection", "Pondération", "N", "Participation déclarée (%)", "Participation officielle (%)", "Écart (points)")),
  align = c("l", "l", "l", "r", "r", "r", "r")
)
```

| Étude | Élection | Pondération | N | Participation déclarée (%) | Participation officielle (%) | Écart (points) |
|:---|:---|:---|---:|---:|---:|---:|
| qes1998 | QC1998 | non pondéré | 1483 | 87,4 | 78,3 | +9,1 |
| qes2007 | QC2007 | pondéré | 2162 | 90,7 | 71,2 | +19,5 |
| qes2007_panel | QC2007 | non pondéré | 2054 | 85,3 | 71,2 | +14,1 |
| qes2008 | QC2008 | non pondéré | 1131 | 87,2 | 57,4 | +29,8 |
| qes2012 | QC2012 | pondéré | 1486 | 93,2 | 74,6 | +18,6 |
| qes2012_panel | QC2012 | non pondéré | 844 | 92,2 | 74,6 | +17,6 |
| qes2014 | QC2014 | pondéré | 1499 | 88,9 | 71,4 | +17,5 |
| qes2018 | QC2018 | pondéré | 2635 | 83,2 | 66,5 | +16,8 |
| qes2018_panel | QC2018 | pondéré | 842 | 83,7 | 66,5 | +17,2 |
| qes2022 | QC2022 | pondéré | 1215 | 90,0 | 66,2 | +23,8 |

La participation officielle est le nombre de bulletins déposés sur le
nombre d’électeurs inscrits. La participation déclarée est la part des
répondants qui disent avoir voté parmi ceux qui ont répondu oui ou non ;
les personnes non inscrites ou non admissibles, et celles qui ne
savaient pas ou n’ont pas voulu répondre, sont laissées de côté.

## 3. Répondants et recensement

Chaque étude est comparée au recensement qui précède son élection (2006,
2011, 2016 ou 2021). Le genre et l’âge sont comparés chez les 18 ans et
plus ; pour la langue maternelle et la scolarité, dont les tableaux
n’ont pas de coupure à 18 ans, la coupure d’âge publiée la plus proche
est utilisée, et les répondants sont coupés de la même façon lorsque
leur âge est connu :

| Marge | Recensement | Population comparée | Tableau |
|----|----|----|----|
| Genre, âge (six tranches) | 2016, 2021 | 18 ans et plus | 98-10-0020-01 |
| Genre, âge (six tranches) | 2011 | 18 ans et plus | Profil du recensement 98-316-XWE2011001 |
| Genre, âge (six tranches) | 2006 | 18 ans et plus | 97-551-XCB2006009 |
| Langue maternelle | 2011, 2016, 2021 | 20 ans et plus, hors établissements, une seule langue maternelle | 98-10-0218-01 |
| Scolarité | 2006 à 2021 | 25 ans et plus, ménages privés | 98-10-0384-01 |

Les effectifs de scolarité de 2011 viennent de l’Enquête nationale
auprès des ménages, qui était facultative. Le tableau de 2006
97-551-XCB2006009 n’est plus sur le site de Statistique Canada ; son
fichier a été récupéré dans l’Internet Archive. Le seul tableau de 2006
trouvé avec la langue maternelle selon l’âge compte les résidents des
établissements et beaucoup plus de langues maternelles multiples : 2006
n’a donc pas de comparaison de la langue maternelle. La langue
maternelle est comparée parmi les personnes qui déclarent une seule
langue maternelle : les réponses multiples du recensement et les
réponses des enquêtes qui nomment deux langues sont laissées de côté. La
scolarité est comparée en deux groupes : universitaire (tout certificat,
diplôme ou grade universitaire) ou non.

``` r

cz <- report[report$check == "census" & is.na(report$level), ]
vars <- c(gender = tr("Gender", "Genre"), age_group6 = tr("Age", "Âge"),
          lang_mother = tr("Mother tongue", "Langue maternelle"),
          education = tr("Education", "Scolarité"))
cz$cell <- num(cz$value)
cw <- reshape(cz[, c("study", "year", "reference", "variable", "weighting", "cell")],
              idvar = c("study", "year", "reference", "weighting"), timevar = "variable",
              direction = "wide")
names(cw) <- sub("^cell\\.", "", names(cw))
for (v in names(vars)) if (is.null(cw[[v]])) cw[[v]] <- NA
cw <- cw[order(cw$year, cw$study, cw$weighting), ]
out <- cw[, c("study", "reference", "weighting", names(vars))]
out[is.na(out)] <- ""
knitr::kable(
  out, row.names = FALSE,
  col.names = c(tr(c("Study", "Census", "Weighting"), c("Étude", "Recensement", "Pondération")), unname(vars)),
  align = c("l", "l", "l", "r", "r", "r", "r")
)
```

| Étude | Recensement | Pondération | Genre | Âge | Langue maternelle | Scolarité |
|:---|:---|:---|---:|---:|---:|---:|
| qes_crop_2007_2010 | Census 2006 | non pondéré | 6,9 | 5,9 |  | 13,5 |
| qes2007 | Census 2006 | non pondéré | 5,2 | 11,2 |  | 20,8 |
| qes2007 | Census 2006 | pondéré | 0,1 | 6,6 |  | 20,5 |
| qes2007_panel | Census 2006 | non pondéré | 5,7 | 6,1 |  | 10,6 |
| qes2008 | Census 2006 | non pondéré | 2,1 | 3,8 |  | 22,9 |
| qes2012 | Census 2011 | non pondéré | 2,3 | 15,9 | 4,5 | 18,5 |
| qes2012 | Census 2011 | pondéré | 0,0 | 0,1 | 0,9 | 17,6 |
| qes2012_panel | Census 2011 | non pondéré | 8,8 | 11,9 | 9,4 |  |
| qes2014 | Census 2011 | non pondéré | 6,5 | 4,7 | 8,1 | 21,3 |
| qes2014 | Census 2011 | pondéré | 0,0 | 0,1 | 4,8 | 9,1 |
| qes2018 | Census 2016 | non pondéré | 0,2 | 14,1 | 8,6 | 18,7 |
| qes2018 | Census 2016 | pondéré | 0,1 | 8,3 | 9,3 | 17,6 |
| qes2018_panel | Census 2016 | non pondéré | 0,1 |  | 8,5 |  |
| qes2018_panel | Census 2016 | pondéré | 0,3 |  | 8,7 |  |
| qes2022 | Census 2021 | non pondéré | 1,2 | 2,5 |  | 19,4 |
| qes2022 | Census 2021 | pondéré | 0,0 | 1,2 |  | 3,5 |

Chaque cellule est un indice de dissimilarité en points (0 est une
concordance parfaite) ; une cellule vide est une marge que l’étude n’a
pas mesurée, ou que les tableaux du recensement recueillis ne publient
pas pour cette année (la langue maternelle en 2006).

``` r

cl <- report[report$check == "census" & !is.na(report$level) & report$weight != "none", ]
cl$variable <- factor(vars[cl$variable], unname(vars))
cl$study <- factor(cl$study, unique(cl$study[order(cl$year)]))
ggplot(cl, aes(x = value, y = level, colour = study, shape = study)) +
  geom_vline(xintercept = 0, colour = "grey50") +
  geom_point(size = 2.2) +
  facet_wrap(~variable, ncol = 2, scales = "free_y") +
  labs(
    x = tr("Weighted survey share minus census share (points)",
           "Part pondérée de l'enquête moins part au recensement (points)"),
    y = NULL, colour = tr("Study", "Étude"), shape = tr("Study", "Étude")
  ) +
  theme_minimal(base_size = 11)
```

![Pour les études dont la pondération est révisée, l'écart en points
entre la part pondérée de chaque catégorie de genre, d'âge, de langue
maternelle et de scolarité et sa part au
recensement.](fr-validation_files/figure-html/census-plot-1.png)

## 4. Relations entre les réponses

Ces vérifications ne se comparent à aucune source extérieure. Elles
demandent si les réponses harmonisées sont liées entre elles comme des
décennies de recherche disent qu’elles devraient l’être ; une erreur de
correspondance (deux codes de partis inversés, une échelle renversée)
les ferait échouer.

``` r

k <- report[report$check == "construct", ]
k <- k[order(k$year, k$study, k$variable), ]
what <- c(interest_4pt = tr("Voters are more interested than nonvoters (mean, 1-4)",
                            "Les votants sont plus intéressés que les abstentionnistes (moyenne, 1-4)"),
          lr_self = tr("QS voters are to the left of CAQ voters (mean, 0-10)",
                       "Les électeurs de QS sont à gauche de ceux de la CAQ (moyenne, 0-10)"),
          sov_indep = tr("Yes to independence: PQ voters more than 40 points above PLQ voters (%)",
                         "Oui à l'indépendance : électeurs du PQ plus de 40 points au-dessus de ceux du PLQ (%)"),
          pid_prov = tr("Partisans who voted for their party: at least 55% (%)",
                        "Partisans qui ont voté pour leur parti : au moins 55 % (%)"))
knitr::kable(
  data.frame(k$study, what[k$variable], num(k$estimate),
             ifelse(is.na(k$benchmark), "", num(k$benchmark)),
             ifelse(k$status == "pass", tr("yes", "oui"), tr("no", "non"))),
  col.names = tr(c("Study", "Expected", "Value", "Compared with", "Holds"),
                 c("Étude", "Attendu", "Valeur", "Comparé à", "Vérifié")),
  align = c("l", "l", "r", "r", "l")
)
```

| Étude | Attendu | Valeur | Comparé à | Vérifié |
|:---|:---|---:|---:|:---|
| qes2007 | Partisans qui ont voté pour leur parti : au moins 55 % (%) | 83,4 |  | oui |
| qes2008 | Partisans qui ont voté pour leur parti : au moins 55 % (%) | 86,9 |  | oui |
| qes2012 | Les votants sont plus intéressés que les abstentionnistes (moyenne, 1-4) | 2,9 | 2,3 | oui |
| qes2012 | Les électeurs de QS sont à gauche de ceux de la CAQ (moyenne, 0-10) | 3,1 | 6,3 | oui |
| qes2012 | Partisans qui ont voté pour leur parti : au moins 55 % (%) | 83,0 |  | oui |
| qes2012 | Oui à l’indépendance : électeurs du PQ plus de 40 points au-dessus de ceux du PLQ (%) | 84,5 | 1,3 | oui |
| qes2014 | Les votants sont plus intéressés que les abstentionnistes (moyenne, 1-4) | 3,0 | 2,3 | oui |
| qes2014 | Les électeurs de QS sont à gauche de ceux de la CAQ (moyenne, 0-10) | 3,4 | 5,7 | oui |
| qes2014 | Partisans qui ont voté pour leur parti : au moins 55 % (%) | 84,2 |  | oui |
| qes2014 | Oui à l’indépendance : électeurs du PQ plus de 40 points au-dessus de ceux du PLQ (%) | 87,4 | 2,9 | oui |
| qes2018 | Les votants sont plus intéressés que les abstentionnistes (moyenne, 1-4) | 3,0 | 2,5 | oui |
| qes2018 | Les électeurs de QS sont à gauche de ceux de la CAQ (moyenne, 0-10) | 3,7 | 6,0 | oui |
| qes2018 | Partisans qui ont voté pour leur parti : au moins 55 % (%) | 82,9 |  | oui |
| qes2018 | Oui à l’indépendance : électeurs du PQ plus de 40 points au-dessus de ceux du PLQ (%) | 85,9 | 2,5 | oui |
| qes2018_panel | Les électeurs de QS sont à gauche de ceux de la CAQ (moyenne, 0-10) | 3,8 | 5,7 | oui |
| qes2022 | Les électeurs de QS sont à gauche de ceux de la CAQ (moyenne, 0-10) | 3,7 | 5,5 | oui |
| qes2022 | Partisans qui ont voté pour leur parti : au moins 55 % (%) | 78,6 |  | oui |
| qes2022 | Oui à l’indépendance : électeurs du PQ plus de 40 points au-dessus de ceux du PLQ (%) | 82,0 | 2,2 | oui |

## Lire les écarts

**La participation est surdéclarée dans toutes les études**, ici de 14 à
30 points (sans `qes1998`, limitée aux francophones). Deux causes
s’additionnent, qu’on ne peut pas séparer avec ces données : des
abstentionnistes disent avoir voté, et les personnes qui votent
acceptent plus volontiers de répondre aux enquêtes électorales. Les
formats de question qui ménagent la face de `qes2018` et de `qes2022`
(plusieurs façons de dire qu’on n’a pas voté) visent à réduire la
première cause ; les pondérations calées sur l’âge, le genre, la
scolarité et la langue ne corrigent ni l’une ni l’autre. Les deux taux
ne comptent pas non plus les mêmes personnes : le taux officiel porte
sur les électeurs inscrits, dont certains ont déménagé ou sont décédés,
alors que les enquêtes interrogent les adultes qu’elles ont joints.
Toute analyse de la participation tirée de ces études décrit les
répondants, pas l’électorat.

**Le vote déclaré est un rappel, pas le vote.** Il est demandé de
quelques jours à quelques semaines après l’élection, à des répondants
dont la participation est elle-même sélective. On dit souvent que le
rappel dérive vers le gagnant, mais on ne voit pas ici une simple prime
au gagnant : dans `qes2012`, le PQ, qui a gagné par moins d’un point,
est surdéclaré de 6,9 points et le PLQ sous-déclaré de 6,3 ; dans
`qes2022`, la CAQ, qui a gagné, est sous-déclarée de 8,0 points. Qui
répond compte au moins autant que la mémoire : les panels en ligne
tendent à surreprésenter les personnes engagées en politique et
scolarisées, et les électeurs de certains partis sont peut-être plus
difficiles à joindre. La pondération sur les caractéristiques
sociodémographiques ne l’efface pas et peut déplacer l’indice dans un
sens ou dans l’autre (comparez les deux colonnes du premier tableau).

**La question compte.** Un parti que la question n’énumérait pas ne
pouvait être déclaré que comme « autre » (le Parti vert dans `qes2018`
et `qes2022`, le Parti conservateur dans `qes2018`). Les deux panels
Durand de 2007 et de 2012 demandent le vote sans lire les partis, par
téléphone ; les autres études lisent ou montrent une liste. Les cotes de
`qes_spec(view = "crosswalk")` consignent ces différences.

**Les échantillons ne sont pas la population du recensement.** Le
recensement compte les résidents, y compris des personnes qui ne sont
pas citoyennes et ne peuvent pas voter ; `qes2022` n’a échantillonné que
des citoyens. Les tableaux du recensement sur la langue maternelle et la
scolarité portent sur les ménages privés, et leurs coupures d’âge
diffèrent de 18 ans et plus (section 3). Trois écarts ressortent.
Scolarité : chaque étude compte plus de répondants ayant fait des études
universitaires que le recensement ne compte de titulaires d’un titre
universitaire, en partie parce que les enquêtes demandent le niveau
atteint (une année d’université compte) et le recensement le titre
obtenu, et en partie, très probablement, parce que les personnes plus
scolarisées acceptent plus volontiers de répondre aux enquêtes ;
`qes2022`, pondérée sur la scolarité, est la seule étude proche du
recensement. Langue maternelle dans `qes2018` : les anglophones sont
surreprésentés et les allophones sous-représentés ; la pondération de
2018 corrige le français par rapport à toutes les autres langues (son
rapport méthodologique, tableau 14), pas la répartition entre l’anglais
et les autres langues. Âge dans `qes2018` : sa pondération corrige l’âge
par cellules générationnelles (16-18, 19-38, 39-58, 59-73, 74 et plus :
tableau 12 du même rapport), qui chevauchent les tranches de dix ans
utilisées ici ; à l’intérieur de celles-ci, l’échantillon pondéré compte
encore trop peu de personnes de 35 à 54 ans et trop de personnes de 55 à
64 ans.

**Ces repères sont grossiers.** Un indice peut cacher des erreurs qui se
compensent : en 2012, le PQ et le PLQ ont terminé à moins d’un point
l’un de l’autre, si bien qu’une étude qui aurait inversé leurs codes
concorderait à peu près aussi bien avec les résultats officiels. C’est
pourquoi l’harmonisation est aussi vérifiée par les effectifs exacts de
chaque réponse dans les fichiers originaux et par les relations de la
section 4, et pourquoi chaque ligne de correspondance est révisée par
rapport au questionnaire.

## Sources

- Élections Québec, résultats officiels des élections générales de 1998
  à 2022, votes par parti et bulletins déposés pour l’ensemble du
  Québec, tirés de ses fichiers de résultats archivés (par exemple
  <https://donnees.electionsquebec.qc.ca/production/provincial/resultats/archives/gen2022-10-03/resultats.json>).
- Statistique Canada, tableaux du recensement
  [98-10-0020-01](https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=9810002001)
  (âge et genre, 2016 et 2021),
  [98-10-0218-01](https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=9810021801)
  (langue maternelle selon l’âge, 2011 à 2021) et
  [98-10-0384-01](https://www150.statcan.gc.ca/t1/tbl1/fr/tv.action?pid=9810038401)
  (plus haut niveau de scolarité selon l’âge et le genre, 2006 à 2021) ;
  le Profil du recensement de 2011 (catalogue 98-316-XWE2011001 : âge
  selon le sexe) ; et le tableau de 2006 97-551-XCB2006009 (âge selon le
  sexe ; la copie du fichier de Statistique Canada conservée par
  l’Internet Archive).

Les tableaux de référence se trouvent dans `inst/extdata/validation/` du
dépôt source, avec la source de chaque ligne ; le paquet installé
(`system.file("extdata", "validation", package = "qesR")`) n’a que les
marges du recensement. Les résultats consignés de chaque étude s’y
trouvent dans `validation_report.csv` ; ceux de `qes2022` gardent la
licence de l’étude, CC BY-NC 4.0 (le fichier `COPYRIGHTS` du package).
