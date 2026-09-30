# Qui vote ? L'écart d'âge et ce que les enquêtes surestiment

*[English
version](https://thomasgareau.github.io/qesR/articles/turnout.md)*

Les enquêtes surestiment la participation. En 2022, 90 % des répondants
de l’Étude électorale québécoise ont dit avoir voté ; la participation
officielle était de 66 %. L’écart est présent à chaque élection, parce
que les personnes qui votent répondent aussi davantage aux enquêtes, et
que certaines qui n’ont pas voté disent l’avoir fait. Dans les enquêtes,
les jeunes déclarent voter moins que les aînés à chaque élection : en
2018, 70 % des répondants de 18 à 34 ans, contre 91 % de ceux de 55 ans
et plus. L’écart est le plus grand chez les personnes les moins
intéressées par la politique.

## Les données

`turnout` est la participation déclarée regroupée, demandée après
l’élection, et `pol_interest` met l’intérêt pour la politique sur une
même échelle de 0 à 1, à travers les questions à quatre points et de 0 à
10 :

``` r

h <- qes_harmonize(
  studies = qz_studies,
  targets = c("turnout", "age_group3", "pol_interest"),
  missing = "reasons", quiet = TRUE
)
```

``` r

table(h$study, h$pol_interest__type)
#>                     
#>                      general_4pt general_0_10 campaign_4pt election_0_10
#>   qes_crop_2007_2010           0            0            0             0
#>   qes1998                      0            0            0             0
#>   qes2007                      0         2175            0             0
#>   qes2007_panel                0            0         2050             0
#>   qes2008                      0            0            0          1151
#>   qes2012                   1505            0            0             0
#>   qes2012_panel                0            0            0             0
#>   qes2014                   1517            0            0             0
#>   qes2018                   3072            0            0             0
#>   qes2018_panel                0            0            0             0
#>   qes2022                      0         1521            0             0
d18 <- qes_design(h[h$study == "qes2018", ], weight = "weight_post")
svyciprop(~I(turnout == "Yes"), subset(d18, !is.na(turnout)), method = "logit")
#>                            2.5% 97.5%
#> I(turnout == "Yes") 0.832 0.815 0.847
```

## Participation déclarée et participation officielle

![Graphique en haltères, une rangée par étude de 1998 à 2022 : la
participation officielle de l'élection en trait et la participation
déclarée par les répondants de l'étude en point avec son intervalle de
confiance. Toutes les études sont au-dessus de la participation
officielle, de +9 pts à +30 pts. Valeurs dans la vue en
tableau.](fr-participation_files/figure-html/overreport-light.png)![Graphique
en haltères, une rangée par étude de 1998 à 2022 : la participation
officielle de l'élection en trait et la participation déclarée par les
répondants de l'étude en point avec son intervalle de confiance. Toutes
les études sont au-dessus de la participation officielle, de +9 pts à
+30 pts. Valeurs dans la vue en
tableau.](fr-participation_files/figure-html/overreport-dark.png)

Source : qesR, variable regroupée turnout (déclarée, après l'élection),
toutes les études qui l'ont demandée ; participation officielle
d'Élections Québec (bulletins déposés sur électeurs inscrits). Points :
participation déclarée avec intervalles de confiance à 95 % (logit),
pondérée avec la pondération postélectorale de chaque étude ; creux :
non pondéré, pondération en révision. Les sondages de 1998 n'ont
interrogé que des francophones, et leur recontact a surreprésenté les
indécis et les refus. L'étude de 2018 a aussi interrogé des jeunes de 16
et 17 ans, qui ne pouvaient pas voter ; ils sont laissés de côté. Les
répondants ne sont pas la liste des électeurs inscrits : une partie de
l'écart n'est pas une erreur.

Vue en tableau

| Élection | Étude | Participation déclarée, % \[IC à 95 %\] | n | Participation officielle, % | Écart | Pondération |
|---:|:---|:---|---:|:---|:---|:---|
| 1998 | Sondages de 1998 | 87,4 \[85,6 ; 89,0\] | 1483 | 78,3 | +9 pts | non pondéré (pondération en révision) |
| 2007 | EEQ 2007 | 90,7 \[89,0 ; 92,2\] | 2162 | 71,2 | +20 pts | pondéré |
| 2007 | Panel 2007 | 85,3 \[83,7 ; 86,8\] | 2054 | 71,2 | +14 pts | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | 87,2 \[85,1 ; 89,0\] | 1131 | 57,4 | +30 pts | non pondéré (pondération en révision) |
| 2012 | EEQ 2012 | 93,2 \[91,8 ; 94,4\] | 1486 | 74,6 | +19 pts | pondéré |
| 2012 | Panel 2012 | 92,2 \[90,2 ; 93,8\] | 844 | 74,6 | +18 pts | non pondéré (pondération en révision) |
| 2014 | EEQ 2014 | 88,9 \[86,8 ; 90,8\] | 1499 | 71,4 | +17 pts | pondéré |
| 2018 | EEQ 2018 | 83,2 \[81,5 ; 84,7\] | 2635 | 66,4 | +17 pts | pondéré |
| 2018 | Panel 2018 | 83,7 \[80,1 ; 86,7\] | 842 | 66,4 | +17 pts | pondéré |
| 2022 | EEQ 2022 | 90,0 \[87,3 ; 92,1\] | 1215 | 66,1 | +24 pts | pondéré |

Les enquêtes surestiment la participation de 9 à 30 pointsParticipation
déclarée dans chaque étude (point, IC à 95 %) et participation
officielle de l'élection (trait)

À remarquer :

- Toutes les études sont au-dessus de la participation officielle, de
  +9 pts à +30 pts.
- L’écart est le plus grand en 2008, l’élection où la participation
  officielle est la plus basse (57 %), et la participation déclarée
  varie beaucoup moins que la participation officielle.
- Les pondérations révisées s’ajustent aux marges du recensement (âge,
  sexe, région, langue et, dans certaines études, scolarité), pas à la
  participation : la pondération n’efface donc pas l’écart.

## L’écart d’âge, élection par élection

![Graphique linéaire avec bande de confiance de l'écart de participation
déclarée entre les répondants de 55 ans et plus et ceux de 18 à 34 ans,
en points, à chaque élection de 1998 à 2022. L'écart est positif à
chaque élection : +11 pts en 2007, +21 pts en 2018 (le plus grand) et
+8 pts en 2022. Valeurs dans la vue en
tableau.](fr-participation_files/figure-html/age-light.png)![Graphique
linéaire avec bande de confiance de l'écart de participation déclarée
entre les répondants de 55 ans et plus et ceux de 18 à 34 ans, en
points, à chaque élection de 1998 à 2022. L'écart est positif à chaque
élection : +11 pts en 2007, +21 pts en 2018 (le plus grand) et +8 pts en
2022. Valeurs dans la vue en
tableau.](fr-participation_files/figure-html/age-dark.png)

Source : qesR, variables regroupées turnout et age_group3 ; une Étude
électorale québécoise par élection et les sondages de 1998 (francophones
seulement). Bande : intervalle de confiance à 95 % de la différence, à
partir de la covariance des estimations des deux groupes d'âge. Pondéré
avec la pondération postélectorale de chaque étude ; creux (1998, 2008)
: non pondéré, pondération en révision. L'axe horizontal est en années,
avec une coupure entre 1998 et 2007. Les parts de chaque groupe d'âge et
les panels Durand sont dans la vue en tableau. La participation
officielle selon l'âge ne fait pas partie des références conservées avec
qesR : l'écart est un écart entre déclarations.

Vue en tableau

| Élection | Étude | 18 à 34 ans, % | 35 à 54 ans, % | 55 ans et plus, % | Écart, 55+ moins 18-34 \[IC à 95 %\] | Pondération |
|---:|:---|:---|:---|:---|:---|:---|
| 1998 | Sondages de 1998 | 81,6 | 88,8 | 90,3 | +8,7 pts \[3,9 ; 13,6\] | non pondéré (pondération en révision) |
| 2007 | EEQ 2007 | 81,6 | 88,8 | 90,3 | +11,0 pts \[6,9 ; 15,1\] | pondéré |
| 2007 | Panel 2007 | 81,6 | 88,8 | 90,3 | +17,5 pts \[12,9 ; 22,2\] | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | 81,6 | 88,8 | 90,3 | +14,5 pts \[9,0 ; 20,0\] | non pondéré (pondération en révision) |
| 2012 | EEQ 2012 | 81,6 | 88,8 | 90,3 | +7,0 pts \[3,5 ; 10,4\] | pondéré |
| 2012 | Panel 2012 | 81,6 | 88,8 | 90,3 | +4,7 pts \[-1,3 ; 10,7\] | non pondéré (pondération en révision) |
| 2014 | EEQ 2014 | 81,6 | 88,8 | 90,3 | +13,1 pts \[7,6 ; 18,5\] | pondéré |
| 2018 | EEQ 2018 | 81,6 | 88,8 | 90,3 | +21,2 pts \[17,1 ; 25,2\] | pondéré |
| 2018 | Panel 2018 | 81,6 | 88,8 | 90,3 | +17,5 pts \[8,3 ; 26,8\] | pondéré |
| 2022 | EEQ 2022 | 81,6 | 88,8 | 90,3 | +8,4 pts \[2,6 ; 14,2\] | pondéré |

Les aînés déclarent voter plus que les jeunes à chaque élection ;
l'écart a culminé à 21 points en 2018Participation déclarée des 55 ans
et plus moins celle des 18 à 34 ans, en points, avec son intervalle de
confiance à 95 %

À remarquer :

- À chaque élection, les 55 ans et plus déclarent la participation la
  plus élevée, au-dessus des 18 à 34 ans de +11 pts en 2007, +21 pts en
  2018 et +8 pts en 2022.
- L’écart est le plus grand en 2018, quand la participation officielle
  est tombée à 66 % : la baisse se voit chez les jeunes et à peine chez
  les aînés.
- En 2022, les 18 à 34 ans déclarent avoir voté autant que les 35 à 54
  ans, avec un intervalle large (269 jeunes répondants).

## L’intérêt pour la politique et l’écart d’âge

![Graphique linéaire, une ligne par groupe d'âge (18 à 34 ans, 35 à 54
ans, 55 ans et plus), de l'écart de participation déclarée entre les
répondants très et peu intéressés par la politique, en points, à chaque
Étude électorale québécoise de 2007 à 2022, avec intervalles de
confiance. Avant 55 ans, l'écart atteint +30 pts ; chez les 55 ans et
plus, il est d'au plus +11 pts. Valeurs dans la vue en
tableau.](fr-participation_files/figure-html/interest-light.png)![Graphique
linéaire, une ligne par groupe d'âge (18 à 34 ans, 35 à 54 ans, 55 ans
et plus), de l'écart de participation déclarée entre les répondants très
et peu intéressés par la politique, en points, à chaque Étude électorale
québécoise de 2007 à 2022, avec intervalles de confiance. Avant 55 ans,
l'écart atteint +30 pts ; chez les 55 ans et plus, il est d'au plus
+11 pts. Valeurs dans la vue en
tableau.](fr-participation_files/figure-html/interest-dark.png)

Source : qesR, variables regroupées turnout et pol_interest, age_group3
; EEQ 2007 à 2022, pondérées avec la pondération postélectorale de
chaque étude. Intérêt sur l'échelle regroupée de 0 à 1 : faible de 0 à
0,4 (pas du tout ou pas très intéressé), moyen de 0,5 à 0,7, élevé de
0,8 à 1. Les questions à quatre points (2012 à 2018) et de 0 à 10 (2007,
2022) ne coupent pas de la même façon : comparer les groupes à
l'intérieur d'une étude. Traits : intervalles de confiance à 95 % de la
différence ; un écart dont la cellule faible ou élevée compte moins de
30 répondants n'est pas tracé. La participation de chaque cellule est
dans la vue en tableau.

Vue en tableau

| Élection | Étude | Âge | Intérêt | Participation déclarée, % \[IC à 95 %\] | n | Question d'intérêt (type) | Pondération |
|---:|:---|:---|:---|:---|---:|:---|:---|
| 2007 | EEQ 2007 | 18 à 34 ans | Intérêt faible | 75,7 \[65,4 ; 83,7\] | 117 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 18 à 34 ans | Intérêt moyen | 87,0 \[80,8 ; 91,5\] | 246 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 18 à 34 ans | Intérêt élevé | 87,0 \[78,9 ; 92,2\] | 197 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 35 à 54 ans | Intérêt faible | 82,2 \[72,8 ; 88,8\] | 127 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 35 à 54 ans | Intérêt moyen | 92,9 \[88,2 ; 95,8\] | 369 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 35 à 54 ans | Intérêt élevé | 94,4 \[89,8 ; 97,0\] | 238 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 55 ans et plus | Intérêt faible | 91,3 \[83,4 ; 95,6\] | 110 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 55 ans et plus | Intérêt moyen | 97,1 \[94,1 ; 98,6\] | 310 | general_0_10 | pondéré |
| 2007 | EEQ 2007 | 55 ans et plus | Intérêt élevé | 95,9 \[92,9 ; 97,7\] | 403 | general_0_10 | pondéré |
| 2012 | EEQ 2012 | 18 à 34 ans | Intérêt faible | 77,1 \[69,6 ; 83,2\] | 174 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 18 à 34 ans | Intérêt moyen | 95,2 \[91,6 ; 97,3\] | 236 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 18 à 34 ans | Intérêt élevé | 96,2 \[90,0 ; 98,6\] | 119 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 35 à 54 ans | Intérêt faible | 89,3 \[84,4 ; 92,8\] | 211 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 35 à 54 ans | Intérêt moyen | 94,9 \[91,2 ; 97,1\] | 292 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 35 à 54 ans | Intérêt élevé | 97,9 \[93,6 ; 99,3\] | 126 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 55 ans et plus | Intérêt faible | 97,4 \[89,8 ; 99,4\] | 64 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 55 ans et plus | Intérêt moyen | 94,6 \[90,0 ; 97,2\] | 162 | general_4pt | pondéré |
| 2012 | EEQ 2012 | 55 ans et plus | Intérêt élevé | 99,1 \[93,8 ; 99,9\] | 88 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 18 à 34 ans | Intérêt faible | 66,4 \[56,4 ; 75,1\] | 138 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 18 à 34 ans | Intérêt moyen | 86,0 \[78,4 ; 91,3\] | 167 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 18 à 34 ans | Intérêt élevé | 96,3 \[90,1 ; 98,6\] | 84 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 35 à 54 ans | Intérêt faible | 81,4 \[73,8 ; 87,1\] | 171 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 35 à 54 ans | Intérêt moyen | 96,2 \[92,4 ; 98,1\] | 279 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 35 à 54 ans | Intérêt élevé | 95,2 \[87,9 ; 98,2\] | 126 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 55 ans et plus | Intérêt faible | 89,6 \[80,0 ; 94,9\] | 86 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 55 ans et plus | Intérêt moyen | 94,3 \[87,9 ; 97,4\] | 269 | general_4pt | pondéré |
| 2014 | EEQ 2014 | 55 ans et plus | Intérêt élevé | 96,3 \[90,6 ; 98,6\] | 169 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 18 à 34 ans | Intérêt faible | 60,6 \[53,8 ; 67,0\] | 269 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 18 à 34 ans | Intérêt moyen | 75,2 \[69,6 ; 80,1\] | 317 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 18 à 34 ans | Intérêt élevé | 80,2 \[71,4 ; 86,8\] | 120 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 35 à 54 ans | Intérêt faible | 73,4 \[65,5 ; 80,0\] | 159 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 35 à 54 ans | Intérêt moyen | 80,8 \[75,1 ; 85,5\] | 241 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 35 à 54 ans | Intérêt élevé | 94,8 \[88,8 ; 97,7\] | 114 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 55 ans et plus | Intérêt faible | 83,8 \[78,0 ; 88,2\] | 226 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 55 ans et plus | Intérêt moyen | 92,2 \[89,6 ; 94,2\] | 697 | general_4pt | pondéré |
| 2018 | EEQ 2018 | 55 ans et plus | Intérêt élevé | 94,8 \[92,0 ; 96,6\] | 456 | general_4pt | pondéré |
| 2022 | EEQ 2022 | 18 à 34 ans | Intérêt faible | 73,9 \[61,9 ; 83,1\] | 95 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 18 à 34 ans | Intérêt moyen | 94,0 \[81,8 ; 98,2\] | 105 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 18 à 34 ans | Intérêt élevé | 91,9 \[78,9 ; 97,2\] | 67 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 35 à 54 ans | Intérêt faible | 69,5 \[50,9 ; 83,4\] | 80 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 35 à 54 ans | Intérêt moyen | 84,7 \[76,8 ; 90,3\] | 150 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 35 à 54 ans | Intérêt élevé | 97,3 \[93,1 ; 99,0\] | 164 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 55 ans et plus | Intérêt faible | 91,1 \[80,5 ; 96,2\] | 67 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 55 ans et plus | Intérêt moyen | 94,0 \[87,5 ; 97,2\] | 194 | general_0_10 | pondéré |
| 2022 | EEQ 2022 | 55 ans et plus | Intérêt élevé | 96,5 \[92,7 ; 98,3\] | 284 | general_0_10 | pondéré |

Avant 55 ans, l'intérêt pour la politique fait varier la participation
déclarée de 30 points au plus ; chez les 55 ans et plus, de 11 au
plusParticipation déclarée des très intéressés moins celle des peu
intéressés, dans chaque groupe d'âge, en points, avec intervalles de
confiance à 95 %

À remarquer :

- Dans chaque groupe d’âge et à chaque élection, les très intéressés
  déclarent une participation plus élevée que les peu intéressés. Entre
  intérêt moyen et élevé, l’ordre n’est pas stable : les groupes plus
  âgés sont près du plafond (plus de 90 % chez les 55 ans et plus).
- L’écart d’âge est le plus grand chez les moins intéressés : en 2018,
  +23 pts entre aînés et jeunes peu intéressés, et +15 pts chez les très
  intéressés. En 2014, jeunes et aînés très intéressés déclarent la même
  participation.
- L’étude de 2022 a mesuré l’intérêt pendant la campagne sur une échelle
  de 0 à 10 et la participation après l’élection : ses niveaux ne sont
  pas ceux de 2018.

## À propos des données

- **Études.** Toutes les études qui ont demandé la participation
  déclarée après l’élection (les sondages CROP ne l’ont pas demandée).
  Participation officielle : bulletins déposés sur électeurs inscrits,
  d’Élections Québec.
- **Variables.** `turnout` (regroupée ; par défaut la participation
  déclarée seulement ;
  `types = list(turnout = c("recall", "intention"))` ajouterait la
  probabilité de voter là où une étude n’a pas demandé la participation
  déclarée), `age_group3`, et `pol_interest`, l’intérêt pour la
  politique de 0 à 1 : les questions à quatre points notées 1, 0,7, 0,3
  et 0 (niveau `approximate`, les notes étant une hypothèse), les
  questions de 0 à 10 divisées par 10. `pol_interest__type` dit de
  quelle question vient chaque valeur.
- **La participation officielle selon l’âge** est publiée par Élections
  Québec pour certaines élections, mais elle ne fait pas partie des
  références conservées avec qesR : cette page ne la présente pas.
- **Pondérations.** La pondération postélectorale recommandée de chaque
  étude, par
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md).
  Les sondages de 1998, l’étude de 2008 et les panels Durand de 2007 et
  2012 sont non pondérés (pondérations en révision) et tracés en creux ;
  le panel 2018 utilise sa pondération postélectorale révisée.
- **Population.** Les sondages de 1998 n’ont interrogé que des
  francophones, et leur recontact a surreprésenté les indécis et les
  refus. L’étude de 2018 a aussi interrogé des jeunes de 16 et 17 ans,
  qui ne pouvaient pas voter ; ils sont laissés de côté dans les
  estimations de participation.
