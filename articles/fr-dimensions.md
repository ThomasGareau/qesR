# Deux dimensions de la concurrence : souveraineté et gauche-droite

*[English
version](https://thomasgareau.github.io/qesR/articles/dimensions.md)*

Pendant quarante ans, le PQ et le PLQ ont divisé le Québec sur la
question nationale. Les Études électorales québécoises montrent que cet
axe ne classe plus les partis comme avant. En 2012, 84 % des électeurs
du PQ auraient voté Oui à un pays indépendant, contre 1 % de ceux du
PLQ. En 2022, le vote pour le Oui était divisé : le PQ en a obtenu 35 %,
Québec solidaire 20 % et la CAQ 34 %, alors qu’en 2012 le PQ seul en
obtenait 73 %. Sur l’axe gauche-droite, les électeurs de Québec
solidaire sont les plus à gauche (3,7 sur 10 en 2022) et ceux des
conservateurs les plus à droite (6,7).

## Les données

Trois variables harmonisées : le vote déclaré regroupé, le vote
référendaire regroupé (`sov_support`) et l’autopositionnement sur l’axe
gauche-droite (`lr_self`, de 0 à 10) :

``` r

h <- qes_harmonize(
  studies = qz_studies,
  targets = c("vote_choice", "sov_support", "lr_self"),
  types = list(vote_choice = "recall"),
  missing = "reasons", quiet = TRUE
)
```

`lr_self` a été demandé dans les Études électorales québécoises de 2012
à 2022 ; en 2022, lui et la question référendaire ont été posés pendant
la campagne, le vote déclaré après l’élection : la page garde donc les
répondants des deux vagues et les pondère avec la pondération
postélectorale. Pour un parti dans une étude :

``` r

d22 <- qes_design(h[h$study == "qes2022", ], weight = "weight_post")
svyby(~lr_self, ~vote_choice, subset(d22, vote_choice %in% c("CAQ", "QS")),
      svymean, na.rm = TRUE)
#>     vote_choice  lr_self         se
#> CAQ         CAQ 5.549020 0.09808683
#> QS           QS 3.658518 0.19725417
```

## Les électorats sur deux axes, de 2012 à 2022

![Deux graphiques linéaires, de 2012 à 2022, une ligne par parti (PLQ,
PQ, CAQ, QS ; le PCQ en 2022 seulement) : à gauche la part des électeurs
du parti qui voteraient Oui à l'indépendance, à droite leur
autopositionnement gauche-droite moyen, avec intervalles de confiance.
Les électeurs du PQ passent de 84 % à 82 % de Oui ; ceux de la CAQ, de
19 % à 38 % ; ceux de QS, de 63 % à 44 %. Les électeurs du PLQ passent
de 6,5 à 4,8 sur l'axe gauche-droite ; ceux de QS restent les plus à
gauche, à 3,7 en 2022. Valeurs dans la vue en
tableau.](fr-dimensions_files/figure-html/paths-light.png)![Deux
graphiques linéaires, de 2012 à 2022, une ligne par parti (PLQ, PQ, CAQ,
QS ; le PCQ en 2022 seulement) : à gauche la part des électeurs du parti
qui voteraient Oui à l'indépendance, à droite leur autopositionnement
gauche-droite moyen, avec intervalles de confiance. Les électeurs du PQ
passent de 84 % à 82 % de Oui ; ceux de la CAQ, de 19 % à 38 % ; ceux de
QS, de 63 % à 44 %. Les électeurs du PLQ passent de 6,5 à 4,8 sur l'axe
gauche-droite ; ceux de QS restent les plus à gauche, à 3,7 en 2022.
Valeurs dans la vue en
tableau.](fr-dimensions_files/figure-html/paths-dark.png)

Source : qesR, variables regroupées vote_choice (vote déclaré) et
sov_support (indépendance), lr_self ; EEQ 2012, 2014, 2018 et 2022,
pondérées avec la pondération postélectorale de chaque étude. Les partis
sont côte à côte à chaque élection. Traits : intervalles de confiance à
95 % (logit pour la part, Wald pour la moyenne). Les deux panneaux ont
leur propre échelle. Le PCQ n'était proposé qu'en 2022.

Vue en tableau

| Parti | Élection | Étude | Gauche-droite, moyenne \[IC à 95 %\] | n (gauche-droite) | Oui, % \[IC à 95 %\] | n (Oui/Non) | Pondération |
|:---|---:|:---|:---|---:|:---|---:|:---|
| PLQ | 2012 | EEQ 2012 | 6,52 \[6,24 ; 6,79\] | 228 | 1,3 \[0,4 ; 4,0\] | 269 | pondéré |
| PLQ | 2014 | EEQ 2014 | 6,29 \[6,01 ; 6,56\] | 393 | 2,9 \[1,3 ; 6,5\] | 482 | pondéré |
| PLQ | 2018 | EEQ 2018 | 5,92 \[5,68 ; 6,16\] | 432 | 2,5 \[1,3 ; 4,8\] | 477 | pondéré |
| PLQ | 2022 | EEQ 2022 | 4,80 \[4,19 ; 5,42\] | 140 | 2,2 \[0,8 ; 5,7\] | 141 | pondéré |
| PQ | 2012 | EEQ 2012 | 4,39 \[4,16 ; 4,61\] | 422 | 84,5 \[80,4 ; 87,8\] | 448 | pondéré |
| PQ | 2014 | EEQ 2014 | 4,67 \[4,28 ; 5,06\] | 289 | 87,4 \[82,2 ; 91,3\] | 299 | pondéré |
| PQ | 2018 | EEQ 2018 | 4,56 \[4,33 ; 4,78\] | 342 | 85,9 \[81,6 ; 89,4\] | 344 | pondéré |
| PQ | 2022 | EEQ 2022 | 4,52 \[4,25 ; 4,79\] | 204 | 82,0 \[74,6 ; 87,6\] | 174 | pondéré |
| QS | 2012 | EEQ 2012 | 3,11 \[2,68 ; 3,53\] | 89 | 62,6 \[49,4 ; 74,1\] | 87 | pondéré |
| QS | 2014 | EEQ 2014 | 3,41 \[2,93 ; 3,88\] | 118 | 63,4 \[52,3 ; 73,3\] | 110 | pondéré |
| QS | 2018 | EEQ 2018 | 3,70 \[3,42 ; 3,98\] | 302 | 56,2 \[49,8 ; 62,4\] | 280 | pondéré |
| QS | 2022 | EEQ 2022 | 3,66 \[3,27 ; 4,05\] | 207 | 43,8 \[34,8 ; 53,2\] | 171 | pondéré |
| CAQ | 2012 | EEQ 2012 | 6,29 \[6,04 ; 6,55\] | 261 | 18,8 \[14,5 ; 24,0\] | 298 | pondéré |
| CAQ | 2014 | EEQ 2014 | 5,68 \[5,39 ; 5,98\] | 223 | 21,0 \[15,7 ; 27,4\] | 244 | pondéré |
| CAQ | 2018 | EEQ 2018 | 6,00 \[5,82 ; 6,17\] | 601 | 31,0 \[27,1 ; 35,3\] | 574 | pondéré |
| CAQ | 2022 | EEQ 2022 | 5,55 \[5,36 ; 5,74\] | 351 | 37,8 \[29,9 ; 46,4\] | 300 | pondéré |
| PCQ | 2022 | EEQ 2022 | 6,73 \[6,38 ; 7,07\] | 143 | 21,5 \[14,9 ; 30,1\] | 137 | pondéré |

Les électeurs de la CAQ et de QS se sont rapprochés sur la souveraineté
(38 % et 44 % de Oui en 2022), et ceux du PLQ se sont déplacés vers la
gaucheOù se situent les électeurs de chaque parti sur la souveraineté et
sur l'axe gauche-droite, de 2012 à 2022, avec intervalles de confiance à
95 %

À remarquer :

- Les électeurs du PQ et de QS sont les souverainistes ; ceux du PLQ
  sont presque tous fédéralistes à chaque élection (2 % de Oui en 2022).
- Les deux partis les plus récents se sont rapprochés sur la
  souveraineté : les électeurs de la CAQ passent de 19 % de Oui en 2012
  à 38 % en 2022, ceux de QS de 63 % à 44 %.
- Les électeurs du PLQ se sont déplacés vers la gauche, de 6,5 à 4,8 ;
  les électeurs à droite du centre sont maintenant ceux de la CAQ et du
  PCQ.
- Les électeurs du PQ et de QS sont proches sur l’axe gauche-droite (1,3
  point d’écart en 2012, 0,9 en 2022), mais leur écart sur la
  souveraineté est passé de 22 à 38 points : la question nationale les
  sépare maintenant plus que l’axe gauche-droite.

## Pour qui votent souverainistes et fédéralistes

![Graphiques linéaires en deux panneaux, les répondants qui voteraient
Oui (à gauche) et Non (à droite) à un référendum, de 2007 à 2022 : la
part du vote déclaré de chaque camp allée à chaque parti. La part du PQ
chez les électeurs du Oui passe de 73 % en 2012 à 35 % en 2022, tandis
que QS monte à 20 % et la CAQ à 34 % ; celle du PLQ chez les électeurs
du Non passe de 46 % en 2012 à 29 % en 2022. Valeurs dans la vue en
tableau.](fr-dimensions_files/figure-html/where-light.png)![Graphiques
linéaires en deux panneaux, les répondants qui voteraient Oui (à gauche)
et Non (à droite) à un référendum, de 2007 à 2022 : la part du vote
déclaré de chaque camp allée à chaque parti. La part du PQ chez les
électeurs du Oui passe de 73 % en 2012 à 35 % en 2022, tandis que QS
monte à 20 % et la CAQ à 34 % ; celle du PLQ chez les électeurs du Non
passe de 46 % en 2012 à 29 % en 2022. Valeurs dans la vue en
tableau.](fr-dimensions_files/figure-html/where-dark.png)

Source : qesR, variables regroupées vote_choice (vote déclaré) et
sov_support ; EEQ 2007 à 2022, pondérées avec la pondération
postélectorale de chaque étude sauf 2008 (creux : non pondérée,
pondération en révision). 2007 et 2008 ont posé la question de 1995,
2012 à 2022 la question sur un pays indépendant : les lignes
s'interrompent au changement. Traits : intervalles de confiance à 95 %
(logit). Les autres partis (PV, ON et autres) sont dans la vue en
tableau.

Vue en tableau

| Élection | Étude | Vote référendaire | Parti | Part du vote déclaré \[IC à 95 %\] | n | Pondération |
|---:|:---|:---|:---|:---|---:|:---|
| 2007 | EEQ 2007 | Oui | PLQ | 3,9 \[2,5 ; 6,2\] | 735 | pondéré |
| 2007 | EEQ 2007 | Oui | PQ | 58,1 \[53,7 ; 62,3\] | 735 | pondéré |
| 2007 | EEQ 2007 | Oui | ADQ | 25,4 \[21,8 ; 29,4\] | 735 | pondéré |
| 2007 | EEQ 2007 | Oui | QS | 7,7 \[5,6 ; 10,5\] | 735 | pondéré |
| 2007 | EEQ 2007 | Oui | CAQ | — | 735 | pondéré |
| 2007 | EEQ 2007 | Oui | PCQ | — | 735 | pondéré |
| 2007 | EEQ 2007 | Oui | Autres | 4,9 \[3,4 ; 7,0\] | 735 | pondéré |
| 2007 | EEQ 2007 | Non | PLQ | 44,4 \[40,3 ; 48,5\] | 913 | pondéré |
| 2007 | EEQ 2007 | Non | PQ | 9,4 \[7,0 ; 12,4\] | 913 | pondéré |
| 2007 | EEQ 2007 | Non | ADQ | 35,5 \[31,8 ; 39,5\] | 913 | pondéré |
| 2007 | EEQ 2007 | Non | QS | 2,2 \[1,3 ; 3,9\] | 913 | pondéré |
| 2007 | EEQ 2007 | Non | CAQ | — | 913 | pondéré |
| 2007 | EEQ 2007 | Non | PCQ | — | 913 | pondéré |
| 2007 | EEQ 2007 | Non | Autres | 8,5 \[6,3 ; 11,3\] | 913 | pondéré |
| 2008 | EEQ 2008 | Oui | PLQ | 10,0 \[7,4 ; 13,4\] | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Oui | PQ | 73,3 \[68,6 ; 77,4\] | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Oui | ADQ | 6,9 \[4,8 ; 9,9\] | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Oui | QS | 6,9 \[4,8 ; 9,9\] | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Oui | CAQ | — | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Oui | PCQ | — | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Oui | Autres | 2,8 \[1,6 ; 5,0\] | 389 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | PLQ | 66,7 \[62,3 ; 70,9\] | 451 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | PQ | 7,3 \[5,2 ; 10,1\] | 451 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | ADQ | 21,7 \[18,2 ; 25,8\] | 451 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | QS | 1,6 \[0,7 ; 3,2\] | 451 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | CAQ | — | 451 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | PCQ | — | 451 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non | Autres | 2,7 \[1,5 ; 4,6\] | 451 | non pondéré (pondération en révision) |
| 2012 | EEQ 2012 | Oui | PLQ | 0,8 \[0,2 ; 2,4\] | 529 | pondéré |
| 2012 | EEQ 2012 | Oui | PQ | 73,3 \[68,9 ; 77,2\] | 529 | pondéré |
| 2012 | EEQ 2012 | Oui | ADQ | — | 529 | pondéré |
| 2012 | EEQ 2012 | Oui | QS | 9,6 \[7,1 ; 12,8\] | 529 | pondéré |
| 2012 | EEQ 2012 | Oui | CAQ | 10,9 \[8,4 ; 14,2\] | 529 | pondéré |
| 2012 | EEQ 2012 | Oui | PCQ | n.p. | 529 | pondéré |
| 2012 | EEQ 2012 | Oui | Autres | 5,5 \[3,9 ; 7,7\] | 529 | pondéré |
| 2012 | EEQ 2012 | Non | PLQ | 45,8 \[41,4 ; 50,3\] | 635 | pondéré |
| 2012 | EEQ 2012 | Non | PQ | 10,2 \[8,0 ; 13,0\] | 635 | pondéré |
| 2012 | EEQ 2012 | Non | ADQ | — | 635 | pondéré |
| 2012 | EEQ 2012 | Non | QS | 4,3 \[2,8 ; 6,6\] | 635 | pondéré |
| 2012 | EEQ 2012 | Non | CAQ | 35,9 \[31,9 ; 40,2\] | 635 | pondéré |
| 2012 | EEQ 2012 | Non | PCQ | n.p. | 635 | pondéré |
| 2012 | EEQ 2012 | Non | Autres | 3,7 \[2,4 ; 5,6\] | 635 | pondéré |
| 2014 | EEQ 2014 | Oui | PLQ | 3,1 \[1,4 ; 6,9\] | 416 | pondéré |
| 2014 | EEQ 2014 | Oui | PQ | 67,5 \[62,1 ; 72,4\] | 416 | pondéré |
| 2014 | EEQ 2014 | Oui | ADQ | — | 416 | pondéré |
| 2014 | EEQ 2014 | Oui | QS | 12,9 \[10,1 ; 16,4\] | 416 | pondéré |
| 2014 | EEQ 2014 | Oui | CAQ | 12,8 \[9,5 ; 17,0\] | 416 | pondéré |
| 2014 | EEQ 2014 | Oui | PCQ | n.p. | 416 | pondéré |
| 2014 | EEQ 2014 | Oui | Autres | 3,7 \[2,0 ; 6,7\] | 416 | pondéré |
| 2014 | EEQ 2014 | Non | PLQ | 59,7 \[55,4 ; 63,8\] | 756 | pondéré |
| 2014 | EEQ 2014 | Non | PQ | 5,6 \[3,9 ; 8,1\] | 756 | pondéré |
| 2014 | EEQ 2014 | Non | ADQ | — | 756 | pondéré |
| 2014 | EEQ 2014 | Non | QS | 4,3 \[3,0 ; 6,3\] | 756 | pondéré |
| 2014 | EEQ 2014 | Non | CAQ | 28,0 \[24,3 ; 32,1\] | 756 | pondéré |
| 2014 | EEQ 2014 | Non | PCQ | n.p. | 756 | pondéré |
| 2014 | EEQ 2014 | Non | Autres | 2,3 \[1,4 ; 3,8\] | 756 | pondéré |
| 2018 | EEQ 2018 | Oui | PLQ | 1,7 \[0,9 ; 3,4\] | 652 | pondéré |
| 2018 | EEQ 2018 | Oui | PQ | 45,3 \[41,2 ; 49,5\] | 652 | pondéré |
| 2018 | EEQ 2018 | Oui | ADQ | — | 652 | pondéré |
| 2018 | EEQ 2018 | Oui | QS | 23,3 \[20,0 ; 27,0\] | 652 | pondéré |
| 2018 | EEQ 2018 | Oui | CAQ | 28,3 \[24,7 ; 32,3\] | 652 | pondéré |
| 2018 | EEQ 2018 | Oui | PCQ | n.p. | 652 | pondéré |
| 2018 | EEQ 2018 | Oui | Autres | 1,3 \[0,6 ; 2,7\] | 652 | pondéré |
| 2018 | EEQ 2018 | Non | PLQ | 40,2 \[37,1 ; 43,3\] | 1109 | pondéré |
| 2018 | EEQ 2018 | Non | PQ | 4,4 \[3,3 ; 5,8\] | 1109 | pondéré |
| 2018 | EEQ 2018 | Non | ADQ | — | 1109 | pondéré |
| 2018 | EEQ 2018 | Non | QS | 10,7 \[8,9 ; 12,8\] | 1109 | pondéré |
| 2018 | EEQ 2018 | Non | CAQ | 37,0 \[34,0 ; 40,1\] | 1109 | pondéré |
| 2018 | EEQ 2018 | Non | PCQ | n.p. | 1109 | pondéré |
| 2018 | EEQ 2018 | Non | Autres | 7,8 \[6,2 ; 9,8\] | 1109 | pondéré |
| 2022 | EEQ 2022 | Oui | PLQ | 1,2 \[0,4 ; 3,0\] | 360 | pondéré |
| 2022 | EEQ 2022 | Oui | PQ | 35,3 \[29,5 ; 41,6\] | 360 | pondéré |
| 2022 | EEQ 2022 | Oui | ADQ | — | 360 | pondéré |
| 2022 | EEQ 2022 | Oui | QS | 20,1 \[15,3 ; 26,1\] | 360 | pondéré |
| 2022 | EEQ 2022 | Oui | CAQ | 33,8 \[26,7 ; 41,8\] | 360 | pondéré |
| 2022 | EEQ 2022 | Oui | PCQ | 9,0 \[6,1 ; 13,1\] | 360 | pondéré |
| 2022 | EEQ 2022 | Oui | Autres | 0,5 \[0,1 ; 2,4\] | 360 | pondéré |
| 2022 | EEQ 2022 | Non | PLQ | 28,5 \[22,8 ; 35,0\] | 590 | pondéré |
| 2022 | EEQ 2022 | Non | PQ | 4,2 \[2,8 ; 6,2\] | 590 | pondéré |
| 2022 | EEQ 2022 | Non | ADQ | — | 590 | pondéré |
| 2022 | EEQ 2022 | Non | QS | 14,0 \[11,0 ; 17,5\] | 590 | pondéré |
| 2022 | EEQ 2022 | Non | CAQ | 30,2 \[25,6 ; 35,1\] | 590 | pondéré |
| 2022 | EEQ 2022 | Non | PCQ | 17,8 \[14,3 ; 21,8\] | 590 | pondéré |
| 2022 | EEQ 2022 | Non | Autres | 5,4 \[2,9 ; 10,0\] | 590 | pondéré |

La part du PQ chez les électeurs du Oui est passée de 73 % à 35 % au
profit de QS et de la CAQ ; le vote pour le Non s'est fragmentéComment
ont voté les répondants qui voteraient Oui et Non à un référendum : la
part de chaque parti dans le vote déclaré du camp, avec intervalles de
confiance à 95 %

À remarquer :

- En 2007 et en 2012, le PQ détenait la majeure partie du vote pour le
  Oui ; en 2018, il en avait moins de la moitié, Québec solidaire et la
  CAQ en prenant leur part.
- Le vote pour le Non s’est fragmenté : la part du PLQ est passée de
  46 % en 2012 à 29 % en 2022 ; la CAQ en a obtenu 36 % en 2012, 37 % en
  2018 et 30 % en 2022, et le PCQ 18 % en 2022. En 2007, l’ADQ en
  obtenait déjà 36 %. L’EEQ 2022 sous-estime la CAQ d’environ 8 points
  (voir [Les enquêtes et les résultats
  officiels](https://thomasgareau.github.io/qesR/articles/fr-enquetes-resultats.md)),
  ce qui touche les parts de 2022.
- La CAQ est le seul parti qui obtient une grande part des deux camps.

## À propos des données

- **Études.** Les Études électorales québécoises de 2012 à 2022 pour
  l’autopositionnement gauche-droite, et de 2007 à 2022 pour le vote
  référendaire ; les panels Durand sont exclus, puisqu’ils ont posé
  d’autres libellés de la question référendaire.
- **Variables.** `vote_choice` restreint au vote déclaré, `sov_support`
  en part de Oui parmi Oui et Non, et `lr_self`, l’autopositionnement de
  la personne de 0 (gauche) à 10 (droite). Les études de 2007 et de 2008
  ont posé la question de 1995, les autres la question sur un pays
  indépendant : les rangées du graphique à barres ne forment pas une
  seule série.
- **Moment.** En 2022, les questions référendaire et gauche-droite ont
  été posées pendant la campagne et le vote déclaré après l’élection ;
  les estimations gardent les répondants des deux vagues, pondérés avec
  la pondération postélectorale.
- **Pondérations.** La pondération postélectorale recommandée de chaque
  étude, par
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
  ; 2008 est non pondérée (ses pondérations sont calées sur le vote et
  en révision).
