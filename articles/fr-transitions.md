# Changer d'idée pendant la campagne : les transitions des panels

*[English
version](https://thomasgareau.github.io/qesR/articles/transitions.md)*

Les enquêtes transversales disent où le vote a abouti ; les panels
disent qui a bougé. Le panel Durand de 2018 et l’Étude électorale
québécoise de 2022 ont interrogé les mêmes personnes pendant la campagne
et après le vote. La plupart des électeurs ont fait ce qu’ils avaient
dit : 88 % des personnes qui comptaient voter CAQ en 2022, et qui ont
voté, ont déclaré un vote CAQ. Québec solidaire en a gardé moins, 71 %.
En 2018, les indécis ont penché vers le parti gagnant : 37 % \[25 ; 51\]
des 75 indécis du panel ont voté CAQ, et 27 % n’ont pas voté.

## Les données

La variable regroupée `vote_choice` en disposition longue garde une
ligne par répondant et par vague : l’intention (avec relance des
indécis, type `intention_push`) avant l’élection et le vote déclaré
(`recall`) après.

``` r

h <- qes_harmonize(
  studies = c("qes1998", "qes2007_panel", "qes2012_panel", "qes2018_panel", "qes2022"),
  targets = "vote_choice",
  layout = "long", missing = "reasons", quiet = TRUE
)
```

``` r

with(h[h$study == "qes2022", ], table(wave, vote_choice__type))
#>      vote_choice__type
#> wave  recall intention_push intention
#>   cps      0           1521         0
#>   pes   1220              0         0
```

Les répondants sont appariés par `qes_id` à l’intérieur d’une étude, et
chaque transition est pondérée avec la pondération postélectorale du
répondant.

## Où ont abouti les intentions de vote

![Deux cartes de chaleur (panel 2018, EEQ 2022) : les rangées sont
l'intention de vote pendant la campagne, avec le nombre de répondants,
les colonnes le vote déclaré après l'élection, chaque case le
pourcentage de la rangée. La diagonale domine : en 2022, 83 % des
personnes qui comptaient voter CAQ ont déclaré un vote CAQ et 67 % de
celles qui comptaient voter QS, un vote QS ; 24 % des indécis n'ont pas
voté. Valeurs dans la vue en
tableau.](fr-transitions_files/figure-html/matrix-light.png)![Deux
cartes de chaleur (panel 2018, EEQ 2022) : les rangées sont l'intention
de vote pendant la campagne, avec le nombre de répondants, les colonnes
le vote déclaré après l'élection, chaque case le pourcentage de la
rangée. La diagonale domine : en 2022, 83 % des personnes qui comptaient
voter CAQ ont déclaré un vote CAQ et 67 % de celles qui comptaient voter
QS, un vote QS ; 24 % des indécis n'ont pas voté. Valeurs dans la vue en
tableau.](fr-transitions_files/figure-html/matrix-dark.png)

Source : qesR, variable regroupée vote_choice en disposition longue,
répondants interrogés aux deux vagues du panel Durand 2018 et de l'EEQ
2022 (vagues de campagne et postélectorale), pondérés avec la
pondération postélectorale. Rangées : l'intention avec relance des
indécis (intention_push), avec le nombre de répondants ; indécis :
toujours aucun parti après la relance. Les deux blocs ont les mêmes
rangées et colonnes ; les cases grises n'ont pas de valeur : le panel
2018 ne proposait pas le PCQ (n.p.), la question de campagne de 2022 n'a
pas de réponse « aucun / ne voterait pas », et les rangées de moins de
30 répondants ne sont pas tracées. Les intervalles de confiance à 95 %
et les panels de 1998, 2007 et 2012 (non pondérés) sont dans la vue en
tableau.

Vue en tableau

| Élection | Étude | Intention | Vote déclaré | % de la rangée \[IC à 95 %\] | n (rangée) | Pondération |
|---:|:---|:---|:---|:---|---:|:---|
| 1998 | Sondages de 1998 | PLQ | PLQ | 80,1 \[75,3 ; 84,1\] | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | PQ | 5,1 \[3,2 ; 8,2\] | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | ADQ | 4,5 \[2,7 ; 7,5\] | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | QS | — | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | CAQ | — | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | PCQ | — | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | Autres | 0,3 \[0,0 ; 2,2\] | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PLQ | N'a pas voté | 10,0 \[7,1 ; 13,8\] | 311 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | PLQ | 3,9 \[2,5 ; 6,2\] | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | PQ | 85,7 \[82,1 ; 88,7\] | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | ADQ | 2,5 \[1,4 ; 4,5\] | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | QS | — | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | CAQ | — | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | PCQ | — | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | Autres | 0,9 \[0,3 ; 2,4\] | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | N'a pas voté | 6,9 \[4,9 ; 9,7\] | 433 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | PLQ | 13,1 \[9,5 ; 17,8\] | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | PQ | 15,4 \[11,5 ; 20,4\] | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | ADQ | 56,0 \[49,9 ; 61,9\] | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | QS | — | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | CAQ | — | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | PCQ | — | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | Autres | 1,2 \[0,4 ; 3,5\] | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | N'a pas voté | 14,3 \[10,5 ; 19,1\] | 259 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | PLQ | n \< 30 | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | PQ | n \< 30 | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | ADQ | n \< 30 | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | QS | — | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | CAQ | — | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | PCQ | — | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | Autres | n \< 30 | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | N'a pas voté | n \< 30 | 26 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | PLQ | n \< 30 | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | PQ | n \< 30 | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | ADQ | n \< 30 | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | QS | — | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | CAQ | — | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | PCQ | — | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | Autres | n \< 30 | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Aucun / ne voterait pas | N'a pas voté | n \< 30 | 22 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | PLQ | 38,3 \[28,4 ; 49,3\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | PQ | 28,4 \[19,7 ; 39,1\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | ADQ | 13,6 \[7,7 ; 22,9\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | QS | — | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | CAQ | — | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | PCQ | — | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | Autres | 2,5 \[0,6 ; 9,3\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Indécis | N'a pas voté | 17,3 \[10,5 ; 27,1\] | 81 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | PLQ | 73,2 \[68,6 ; 77,3\] | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | PQ | 3,0 \[1,7 ; 5,3\] | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | ADQ | 8,1 \[5,8 ; 11,2\] | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | QS | 0,5 \[0,1 ; 2,0\] | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | CAQ | — | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | PCQ | — | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | Autres | 0,8 \[0,2 ; 2,3\] | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | N'a pas voté | 14,4 \[11,3 ; 18,3\] | 395 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | PLQ | 3,8 \[2,3 ; 6,1\] | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | PQ | 67,4 \[62,7 ; 71,8\] | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | ADQ | 9,0 \[6,6 ; 12,3\] | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | QS | 1,5 \[0,7 ; 3,3\] | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | CAQ | — | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | PCQ | — | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | Autres | 1,8 \[0,8 ; 3,6\] | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | N'a pas voté | 16,5 \[13,2 ; 20,5\] | 399 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | PLQ | 4,3 \[2,7 ; 6,9\] | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | PQ | 11,1 \[8,3 ; 14,7\] | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | ADQ | 71,9 \[67,1 ; 76,2\] | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | QS | 1,1 \[0,4 ; 2,8\] | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | CAQ | — | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | PCQ | — | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | Autres | 1,4 \[0,6 ; 3,2\] | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | N'a pas voté | 10,3 \[7,6 ; 13,8\] | 370 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | PLQ | 5,1 \[1,9 ; 12,7\] | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | PQ | 21,5 \[13,8 ; 31,9\] | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | ADQ | 13,9 \[7,9 ; 23,4\] | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | QS | 41,8 \[31,4 ; 52,9\] | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | CAQ | — | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | PCQ | — | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | Autres | 10,1 \[5,1 ; 19,0\] | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | N'a pas voté | 7,6 \[3,5 ; 15,9\] | 79 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | PLQ | 13,9 \[8,4 ; 22,1\] | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | PQ | 15,8 \[9,9 ; 24,3\] | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | ADQ | 12,9 \[7,6 ; 20,9\] | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | QS | 3,0 \[1,0 ; 8,8\] | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | CAQ | — | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | PCQ | — | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | Autres | 33,7 \[25,1 ; 43,4\] | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | N'a pas voté | 20,8 \[14,0 ; 29,8\] | 101 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | PLQ | n \< 30 | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | PQ | n \< 30 | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | ADQ | n \< 30 | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | QS | n \< 30 | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | CAQ | — | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | PCQ | — | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | Autres | n \< 30 | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Aucun / ne voterait pas | N'a pas voté | n \< 30 | 29 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | PLQ | 19,2 \[12,8 ; 27,9\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | PQ | 26,9 \[19,3 ; 36,2\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | ADQ | 23,1 \[16,0 ; 32,1\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | QS | 0,0 | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | CAQ | — | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | PCQ | — | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | Autres | 4,8 \[2,0 ; 11,0\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Indécis | N'a pas voté | 26,0 \[18,4 ; 35,2\] | 104 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | PLQ | 82,3 \[75,5 ; 87,5\] | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | PQ | 3,2 \[1,3 ; 7,4\] | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | ADQ | — | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | QS | 1,3 \[0,3 ; 4,9\] | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | CAQ | 6,3 \[3,4 ; 11,4\] | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | PCQ | n.p. | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | Autres | 0,6 \[0,1 ; 4,4\] | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | N'a pas voté | 6,3 \[3,4 ; 11,4\] | 158 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | PLQ | 1,0 \[0,2 ; 3,7\] | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | PQ | 89,0 \[84,1 ; 92,6\] | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | ADQ | — | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | QS | 2,4 \[1,0 ; 5,6\] | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | CAQ | 1,0 \[0,2 ; 3,7\] | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | PCQ | n.p. | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | Autres | 0,0 | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | N'a pas voté | 6,7 \[4,0 ; 10,9\] | 210 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | PLQ | 0,0 | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | PQ | 27,7 \[16,8 ; 42,0\] | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | ADQ | — | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | QS | 57,4 \[43,1 ; 70,7\] | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | CAQ | 4,3 \[1,1 ; 15,5\] | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | PCQ | n.p. | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | Autres | 4,3 \[1,1 ; 15,5\] | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | N'a pas voté | 6,4 \[2,1 ; 18,0\] | 47 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | PLQ | 6,6 \[3,7 ; 11,5\] | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | PQ | 9,0 \[5,5 ; 14,4\] | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | ADQ | — | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | QS | 2,4 \[0,9 ; 6,2\] | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | CAQ | 72,5 \[65,2 ; 78,7\] | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | PCQ | n.p. | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | Autres | 1,2 \[0,3 ; 4,7\] | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | N'a pas voté | 8,4 \[5,0 ; 13,7\] | 167 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | PLQ | 10,5 \[4,0 ; 24,9\] | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | PQ | 18,4 \[9,0 ; 33,9\] | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | ADQ | — | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | QS | 10,5 \[4,0 ; 24,9\] | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | CAQ | 5,3 \[1,3 ; 18,8\] | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | PCQ | n.p. | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | Autres | 50,0 \[34,6 ; 65,4\] | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | N'a pas voté | 5,3 \[1,3 ; 18,8\] | 38 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | PLQ | n \< 30 | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | PQ | n \< 30 | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | ADQ | — | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | QS | n \< 30 | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | CAQ | n \< 30 | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | PCQ | n.p. | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | Autres | n \< 30 | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Aucun / ne voterait pas | N'a pas voté | n \< 30 | 8 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | PLQ | 29,5 \[19,4 ; 42,1\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | PQ | 23,0 \[14,1 ; 35,1\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | ADQ | — | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | QS | 3,3 \[0,8 ; 12,2\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | CAQ | 14,8 \[7,9 ; 26,0\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | PCQ | n.p. | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | Autres | 4,9 \[1,6 ; 14,2\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Indécis | N'a pas voté | 24,6 \[15,4 ; 36,9\] | 61 | non pondéré (pondération en révision) |
| 2018 | Panel 2018 | PLQ | PLQ | 79,3 \[72,6 ; 84,7\] | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | PQ | 0,0 | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | ADQ | — | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | QS | 0,8 \[0,2 ; 3,6\] | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | CAQ | 5,8 \[3,1 ; 10,7\] | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | PCQ | n.p. | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | Autres | 2,0 \[0,9 ; 4,6\] | 226 | pondéré |
| 2018 | Panel 2018 | PLQ | N'a pas voté | 12,1 \[8,0 ; 17,9\] | 226 | pondéré |
| 2018 | Panel 2018 | PQ | PLQ | 0,7 \[0,2 ; 2,8\] | 115 | pondéré |
| 2018 | Panel 2018 | PQ | PQ | 69,3 \[56,0 ; 80,0\] | 115 | pondéré |
| 2018 | Panel 2018 | PQ | ADQ | — | 115 | pondéré |
| 2018 | Panel 2018 | PQ | QS | 0,0 | 115 | pondéré |
| 2018 | Panel 2018 | PQ | CAQ | 11,7 \[5,5 ; 23,1\] | 115 | pondéré |
| 2018 | Panel 2018 | PQ | PCQ | n.p. | 115 | pondéré |
| 2018 | Panel 2018 | PQ | Autres | 0,0 | 115 | pondéré |
| 2018 | Panel 2018 | PQ | N'a pas voté | 18,3 \[9,6 ; 32,0\] | 115 | pondéré |
| 2018 | Panel 2018 | QS | PLQ | 0,0 | 109 | pondéré |
| 2018 | Panel 2018 | QS | PQ | 9,6 \[5,1 ; 17,6\] | 109 | pondéré |
| 2018 | Panel 2018 | QS | ADQ | — | 109 | pondéré |
| 2018 | Panel 2018 | QS | QS | 67,8 \[56,9 ; 77,1\] | 109 | pondéré |
| 2018 | Panel 2018 | QS | CAQ | 11,1 \[6,0 ; 19,7\] | 109 | pondéré |
| 2018 | Panel 2018 | QS | PCQ | n.p. | 109 | pondéré |
| 2018 | Panel 2018 | QS | Autres | 3,0 \[0,7 ; 11,4\] | 109 | pondéré |
| 2018 | Panel 2018 | QS | N'a pas voté | 8,5 \[4,1 ; 16,9\] | 109 | pondéré |
| 2018 | Panel 2018 | CAQ | PLQ | 2,5 \[0,8 ; 7,4\] | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | PQ | 0,0 | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | ADQ | — | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | QS | 0,5 \[0,1 ; 3,2\] | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | CAQ | 85,5 \[79,0 ; 90,3\] | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | PCQ | n.p. | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | Autres | 0,9 \[0,3 ; 3,0\] | 219 | pondéré |
| 2018 | Panel 2018 | CAQ | N'a pas voté | 10,6 \[6,6 ; 16,5\] | 219 | pondéré |
| 2018 | Panel 2018 | Autres | PLQ | 0,0 | 34 | pondéré |
| 2018 | Panel 2018 | Autres | PQ | 0,0 | 34 | pondéré |
| 2018 | Panel 2018 | Autres | ADQ | — | 34 | pondéré |
| 2018 | Panel 2018 | Autres | QS | 3,7 \[0,9 ; 14,1\] | 34 | pondéré |
| 2018 | Panel 2018 | Autres | CAQ | 0,0 | 34 | pondéré |
| 2018 | Panel 2018 | Autres | PCQ | n.p. | 34 | pondéré |
| 2018 | Panel 2018 | Autres | Autres | 85,3 \[69,5 ; 93,7\] | 34 | pondéré |
| 2018 | Panel 2018 | Autres | N'a pas voté | 11,0 \[4,0 ; 26,6\] | 34 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | PLQ | 1,6 \[0,2 ; 10,6\] | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | PQ | 2,0 \[0,3 ; 13,2\] | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | ADQ | — | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | QS | 4,2 \[1,0 ; 15,8\] | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | CAQ | 22,0 \[10,0 ; 41,8\] | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | PCQ | n.p. | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | Autres | 2,8 \[0,6 ; 11,2\] | 37 | pondéré |
| 2018 | Panel 2018 | Aucun / ne voterait pas | N'a pas voté | 67,4 \[48,4 ; 82,0\] | 37 | pondéré |
| 2018 | Panel 2018 | Indécis | PLQ | 13,4 \[7,0 ; 24,1\] | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | PQ | 7,9 \[3,5 ; 17,1\] | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | ADQ | — | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | QS | 9,0 \[3,2 ; 22,8\] | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | CAQ | 36,9 \[24,9 ; 50,8\] | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | PCQ | n.p. | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | Autres | 5,9 \[2,3 ; 14,5\] | 75 | pondéré |
| 2018 | Panel 2018 | Indécis | N'a pas voté | 26,7 \[16,4 ; 40,4\] | 75 | pondéré |
| 2022 | EEQ 2022 | PLQ | PLQ | 78,3 \[67,4 ; 86,3\] | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | PQ | 3,7 \[1,4 ; 9,6\] | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | ADQ | — | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | QS | 6,8 \[2,9 ; 15,3\] | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | CAQ | 1,6 \[0,6 ; 4,1\] | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | PCQ | 1,1 \[0,3 ; 4,6\] | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | Autres | 0,4 \[0,1 ; 2,8\] | 126 | pondéré |
| 2022 | EEQ 2022 | PLQ | N'a pas voté | 7,9 \[3,6 ; 16,8\] | 126 | pondéré |
| 2022 | EEQ 2022 | PQ | PLQ | 2,1 \[0,7 ; 6,0\] | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | PQ | 81,5 \[72,7 ; 88,0\] | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | ADQ | — | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | QS | 5,2 \[2,2 ; 12,0\] | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | CAQ | 4,0 \[1,7 ; 9,4\] | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | PCQ | 1,2 \[0,2 ; 7,9\] | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | Autres | 0,0 | 152 | pondéré |
| 2022 | EEQ 2022 | PQ | N'a pas voté | 5,9 \[2,4 ; 13,7\] | 152 | pondéré |
| 2022 | EEQ 2022 | QS | PLQ | 11,2 \[4,8 ; 23,9\] | 221 | pondéré |
| 2022 | EEQ 2022 | QS | PQ | 8,4 \[5,3 ; 13,1\] | 221 | pondéré |
| 2022 | EEQ 2022 | QS | ADQ | — | 221 | pondéré |
| 2022 | EEQ 2022 | QS | QS | 66,8 \[55,9 ; 76,2\] | 221 | pondéré |
| 2022 | EEQ 2022 | QS | CAQ | 2,2 \[0,7 ; 6,6\] | 221 | pondéré |
| 2022 | EEQ 2022 | QS | PCQ | 0,9 \[0,2 ; 3,7\] | 221 | pondéré |
| 2022 | EEQ 2022 | QS | Autres | 4,2 \[0,7 ; 21,6\] | 221 | pondéré |
| 2022 | EEQ 2022 | QS | N'a pas voté | 6,3 \[3,4 ; 11,3\] | 221 | pondéré |
| 2022 | EEQ 2022 | CAQ | PLQ | 1,9 \[0,8 ; 4,6\] | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | PQ | 6,0 \[4,0 ; 9,0\] | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | ADQ | — | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | QS | 2,6 \[1,4 ; 5,0\] | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | CAQ | 83,0 \[78,0 ; 87,0\] | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | PCQ | 0,3 \[0,0 ; 2,0\] | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | Autres | 0,1 \[0,0 ; 1,0\] | 379 | pondéré |
| 2022 | EEQ 2022 | CAQ | N'a pas voté | 6,1 \[3,7 ; 10,1\] | 379 | pondéré |
| 2022 | EEQ 2022 | PCQ | PLQ | 6,2 \[2,7 ; 13,4\] | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | PQ | 2,0 \[0,7 ; 5,5\] | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | ADQ | — | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | QS | 2,3 \[0,7 ; 7,9\] | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | CAQ | 4,0 \[1,9 ; 8,5\] | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | PCQ | 71,8 \[60,7 ; 80,8\] | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | Autres | 0,8 \[0,2 ; 3,5\] | 172 | pondéré |
| 2022 | EEQ 2022 | PCQ | N'a pas voté | 12,8 \[5,8 ; 25,9\] | 172 | pondéré |
| 2022 | EEQ 2022 | Autres | PLQ | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | PQ | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | ADQ | — | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | QS | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | CAQ | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | PCQ | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | Autres | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Autres | N'a pas voté | n \< 30 | 27 | pondéré |
| 2022 | EEQ 2022 | Indécis | PLQ | 16,2 \[9,0 ; 27,4\] | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | PQ | 9,8 \[5,0 ; 18,4\] | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | ADQ | — | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | QS | 13,4 \[7,2 ; 23,5\] | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | CAQ | 13,1 \[7,2 ; 22,5\] | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | PCQ | 11,7 \[5,3 ; 24,0\] | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | Autres | 12,3 \[5,4 ; 25,5\] | 79 | pondéré |
| 2022 | EEQ 2022 | Indécis | N'a pas voté | 23,6 \[14,2 ; 36,6\] | 79 | pondéré |

La plupart des électeurs ont fait ce qu'ils avaient dit ; en 2018, la
CAQ a puisé de tous les côtés, dont 12 % des personnes qui comptaient
voter PQDe l'intention pendant la campagne (rangées) au vote déclaré
après l'élection (colonnes) : le pourcentage de chaque rangée

À remarquer :

- La diagonale : la plupart des électeurs ont déclaré le parti pour
  lequel ils comptaient voter.
- En 2018, la CAQ a attiré de partout : 12 % des personnes qui
  comptaient voter PQ, 11 % de celles qui comptaient voter QS, 22 % de
  celles et ceux qui disaient ne pas vouloir voter et 37 % des indécis.
- En 2022, les passages d’un parti à l’autre sont faibles, et QS a perdu
  le plus : 11 % des personnes qui comptaient voter QS ont voté PLQ et
  8 % PQ.

## La fidélité : ont voté comme prévu

![Graphique à points groupé par parti (PLQ, PQ, ADQ, QS, CAQ, PCQ), une
rangée par étude par panel de 1998 à 2022 : la part des personnes qui
comptaient voter pour le parti pendant la campagne qui ont déclaré avoir
voté pour lui, parmi les votants, avec intervalles de confiance. Le PLQ,
le PQ et la CAQ sont entre 79 % et 96 % ; QS, dans le panel 2007, en a
gardé 45 %. En 2022, la CAQ en a gardé 88 % et le PLQ 85 %. Valeurs dans
la vue en
tableau.](fr-transitions_files/figure-html/loyal-light.png)![Graphique à
points groupé par parti (PLQ, PQ, ADQ, QS, CAQ, PCQ), une rangée par
étude par panel de 1998 à 2022 : la part des personnes qui comptaient
voter pour le parti pendant la campagne qui ont déclaré avoir voté pour
lui, parmi les votants, avec intervalles de confiance. Le PLQ, le PQ et
la CAQ sont entre 79 % et 96 % ; QS, dans le panel 2007, en a gardé
45 %. En 2022, la CAQ en a gardé 88 % et le PLQ 85 %. Valeurs dans la
vue en tableau.](fr-transitions_files/figure-html/loyal-dark.png)

Source : qesR, variable regroupée vote_choice en disposition longue,
répondants interrogés avant et après l'élection qui ont déclaré un vote.
Traits : intervalles de confiance à 95 % (logit). Pondéré avec la
pondération postélectorale (panel 2018, EEQ 2022) ; creux (panels 1998,
2007 et 2012) : non pondéré, pondération en révision. Les études
diffèrent par leur plan (l'EEQ 2022 est une enquête de campagne et
postélectorale, les autres des panels téléphoniques) : comparer à
l'intérieur d'un parti avec prudence. Les cellules de moins de 30
répondants ne sont pas tracées ; les autres partis sont dans la vue en
tableau.

Vue en tableau

| Élection | Étude | Intention | A voté comme prévu, % \[IC à 95 %\] | n | Pondération |
|---:|:---|:---|:---|---:|:---|
| 1998 | Sondages de 1998 | PLQ | 88,9 \[84,7 ; 92,1\] | 280 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | 92,1 \[89,0 ; 94,3\] | 403 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | 65,3 \[58,8 ; 71,3\] | 222 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | n \< 30 | 23 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | 85,5 \[81,3 ; 88,9\] | 338 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | 80,8 \[76,2 ; 84,7\] | 333 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | 80,1 \[75,5 ; 84,1\] | 332 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | 45,2 \[34,2 ; 56,7\] | 73 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | 42,5 \[32,2 ; 53,5\] | 80 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | 87,8 \[81,5 ; 92,2\] | 148 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | 95,4 \[91,4 ; 97,6\] | 196 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | 61,4 \[46,4 ; 74,5\] | 44 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | 79,1 \[71,9 ; 84,8\] | 153 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | 52,8 \[36,7 ; 68,3\] | 36 | non pondéré (pondération en révision) |
| 2018 | Panel 2018 | PLQ | 90,2 \[84,4 ; 94,0\] | 201 | pondéré |
| 2018 | Panel 2018 | PQ | 84,8 \[71,9 ; 92,4\] | 102 | pondéré |
| 2018 | Panel 2018 | QS | 74,1 \[63,1 ; 82,8\] | 101 | pondéré |
| 2018 | Panel 2018 | CAQ | 95,7 \[90,6 ; 98,1\] | 199 | pondéré |
| 2018 | Panel 2018 | Autres | n \< 30 | 29 | pondéré |
| 2022 | EEQ 2022 | PLQ | 85,1 \[75,3 ; 91,4\] | 112 | pondéré |
| 2022 | EEQ 2022 | PQ | 86,7 \[78,7 ; 92,0\] | 147 | pondéré |
| 2022 | EEQ 2022 | QS | 71,3 \[59,3 ; 81,0\] | 206 | pondéré |
| 2022 | EEQ 2022 | CAQ | 88,4 \[84,2 ; 91,6\] | 359 | pondéré |
| 2022 | EEQ 2022 | PCQ | 82,3 \[73,9 ; 88,5\] | 159 | pondéré |
| 2022 | EEQ 2022 | Autres | n \< 30 | 24 | pondéré |

Le PLQ, le PQ et la CAQ gardent de 79 % à 96 % des personnes qui
comptaient voter pour eux ; QS en garde moins, jusqu'à 45 % en 2007Part
des personnes qui comptaient voter pour chaque parti pendant la campagne
qui ont déclaré avoir voté pour lui, parmi les votants, selon l'étude
par panel, avec intervalles de confiance à 95 %

À remarquer :

- Les tiers partis gardent le moins : l’ADQ en 1998 (65 %), QS en 2007
  (45 %) et en 2012 (61 %).
- Le PLQ garde entre 85 % et 90 % des personnes qui comptaient voter
  pour lui à chaque élection.
- La fidélité n’est pas un trait fixe d’un parti : la CAQ a gardé 79 %
  des personnes qui comptaient voter pour elle en 2012 et 96 % en 2018,
  l’ADQ 65 % en 1998 et 80 % en 2007. Les études diffèrent : les
  sondages de 1998 et les panels de 2007 et 2012 sont non pondérés, le
  panel 2018 est pondéré, et l’EEQ 2022 est une enquête de campagne et
  postélectorale plutôt qu’un panel téléphonique.

## Où sont allés les indécis

![Barres horizontales empilées à 100 %, une par étude par panel de 1998
à 2022 : ce qu'ont déclaré après l'élection les répondants encore
indécis pendant la campagne, en commençant par ceux qui n'ont pas voté.
Entre 17 % et 27 % n'ont pas voté ; en 2018, 37 % ont voté CAQ ; en
2022, 24 % n'ont pas voté et 13 % ont voté CAQ. Valeurs dans la vue en
tableau.](fr-transitions_files/figure-html/undecided-light.png)![Barres
horizontales empilées à 100 %, une par étude par panel de 1998 à 2022 :
ce qu'ont déclaré après l'élection les répondants encore indécis pendant
la campagne, en commençant par ceux qui n'ont pas voté. Entre 17 % et
27 % n'ont pas voté ; en 2018, 37 % ont voté CAQ ; en 2022, 24 % n'ont
pas voté et 13 % ont voté CAQ. Valeurs dans la vue en
tableau.](fr-transitions_files/figure-html/undecided-dark.png)

Source : qesR, variable regroupée vote_choice en disposition longue ;
les indécis sont les répondants qui n'ont nommé aucun parti pendant la
campagne, même après la question de relance, et qui ont répondu après
l'élection. Pondéré (panel 2018, EEQ 2022) ou non pondéré (panels 1998,
2007 et 2012 ; pondérations en révision). Segment vide : n'a pas voté.
Nombres : parts de 8 % ou plus ; les intervalles de confiance à 95 %
sont dans la vue en tableau, et ils sont larges : chaque rangée repose
sur moins de 110 répondants. Dans les panels 2007 et 2018, ne sait pas
et refus forment un seul code.

Vue en tableau

| Élection | Étude | Vote déclaré | Part des indécis, % \[IC à 95 %\] | n | Pondération |
|---:|:---|:---|:---|---:|:---|
| 1998 | Sondages de 1998 | PLQ | 38,3 \[28,4 ; 49,3\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PQ | 28,4 \[19,7 ; 39,1\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | ADQ | 13,6 \[7,7 ; 22,9\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | QS | — | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | CAQ | — | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | PCQ | — | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Autres | 2,5 \[0,6 ; 9,3\] | 81 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | N'a pas voté | 17,3 \[10,5 ; 27,1\] | 81 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PLQ | 19,2 \[12,8 ; 27,9\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PQ | 26,9 \[19,3 ; 36,2\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | ADQ | 23,1 \[16,0 ; 32,1\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | QS | 0,0 | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | CAQ | — | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | PCQ | — | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Autres | 4,8 \[2,0 ; 11,0\] | 104 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | N'a pas voté | 26,0 \[18,4 ; 35,2\] | 104 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PLQ | 29,5 \[19,4 ; 42,1\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PQ | 23,0 \[14,1 ; 35,1\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | ADQ | — | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | QS | 3,3 \[0,8 ; 12,2\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | CAQ | 14,8 \[7,9 ; 26,0\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | PCQ | n.p. | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Autres | 4,9 \[1,6 ; 14,2\] | 61 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | N'a pas voté | 24,6 \[15,4 ; 36,9\] | 61 | non pondéré (pondération en révision) |
| 2018 | Panel 2018 | PLQ | 13,4 \[7,0 ; 24,1\] | 75 | pondéré |
| 2018 | Panel 2018 | PQ | 7,9 \[3,5 ; 17,1\] | 75 | pondéré |
| 2018 | Panel 2018 | ADQ | — | 75 | pondéré |
| 2018 | Panel 2018 | QS | 9,0 \[3,2 ; 22,8\] | 75 | pondéré |
| 2018 | Panel 2018 | CAQ | 36,9 \[24,9 ; 50,8\] | 75 | pondéré |
| 2018 | Panel 2018 | PCQ | n.p. | 75 | pondéré |
| 2018 | Panel 2018 | Autres | 5,9 \[2,3 ; 14,5\] | 75 | pondéré |
| 2018 | Panel 2018 | N'a pas voté | 26,7 \[16,4 ; 40,4\] | 75 | pondéré |
| 2022 | EEQ 2022 | PLQ | 16,2 \[9,0 ; 27,4\] | 79 | pondéré |
| 2022 | EEQ 2022 | PQ | 9,8 \[5,0 ; 18,4\] | 79 | pondéré |
| 2022 | EEQ 2022 | ADQ | — | 79 | pondéré |
| 2022 | EEQ 2022 | QS | 13,4 \[7,2 ; 23,5\] | 79 | pondéré |
| 2022 | EEQ 2022 | CAQ | 13,1 \[7,2 ; 22,5\] | 79 | pondéré |
| 2022 | EEQ 2022 | PCQ | 11,7 \[5,3 ; 24,0\] | 79 | pondéré |
| 2022 | EEQ 2022 | Autres | 12,3 \[5,4 ; 25,5\] | 79 | pondéré |
| 2022 | EEQ 2022 | N'a pas voté | 23,6 \[14,2 ; 36,6\] | 79 | pondéré |

De 17 % à 27 % des indécis de la campagne n'ont pas voté ; en 2018, 37 %
d'entre eux ont voté CAQ, le parti gagnantCe qu'ont déclaré après
l'élection les répondants encore indécis pendant la campagne, selon
l'étude par panel

À remarquer :

- Entre 17 % et 27 % des indécis n’ont pas voté.
- Ceux qui ont voté ont penché vers le parti gagnant en 2018 (la CAQ) ;
  en 2022, ils se sont répartis presque également entre cinq partis.

## À propos des données

- **Études.** Les études qui ont interrogé les mêmes répondants avant et
  après l’élection : les sondages de 1998 (CROP et CREATEC), les panels
  Durand de 2007, 2012 et 2018, et l’EEQ 2022 (vagues de campagne et
  postélectorale). Seuls les répondants des deux vagues sont utilisés.
- **Variable.** `vote_choice` en disposition longue, une ligne par
  vague. Avant l’élection, l’intention avec relance des indécis vers un
  parti (`intention_push` ; dans le panel 2007, quelques répondants
  n’avaient pas de question de relance et gardent leur première réponse,
  type `intention`) ; après, le vote déclaré (`recall`). Ne voterait
  pas, aucun ou annulerait est le niveau `no_party` de l’intention ; n’a
  pas voté est le motif de valeur manquante `not_voted` du vote déclaré.
- **Pondérations.** La pondération postélectorale de chaque répondant,
  par
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
  en disposition longue (la personne est l’unité d’échantillonnage). Les
  panels de 1998, 2007 et 2012 sont non pondérés (pondérations en
  révision) et tracés en creux.
- **Attrition.** Les répondants qui ont quitté le panel après la
  campagne ne sont pas dans ces tableaux ; si le départ est lié au
  changement d’idée, les transitions sont biaisées vers la stabilité.
