# De deux partis à quatre : le réalignement du système partisan québécois, 1998-2022

*[English
version](https://thomasgareau.github.io/qesR/articles/realignment.md)*

En 1998, le PQ et le PLQ ont obtenu 86 % des votes valides. En 2022,
cinq partis ont obtenu chacun plus de 12 %, et le nombre effectif de
partis (résultats officiels) est passé de 2,6 à 4,0. Les enquêtes
montrent où cela s’est produit : chez les francophones. Leur nombre
effectif de partis est passé de 2,8 en 1998 (les sondages CROP et
CREATEC, qui n’ont interrogé que des francophones et sont non pondérés)
à 3,9 dans l’Étude électorale québécoise de 2022, leur vote se divisant
entre le PQ, la CAQ, Québec solidaire et le PLQ. Les non-francophones
ont continué de voter PLQ : 72 % d’entre eux en 2018, contre 12 % des
francophones. La CAQ a gagné cette année-là avec 41 % du vote
francophone et 12 % du vote des autres électeurs.

## Les données

Toutes les figures de cette page utilisent une seule variable regroupée,
`vote_choice`, restreinte au vote déclaré après l’élection (`types`) :

``` r

h <- qes_harmonize(
  studies = qz_studies,
  targets = c("vote_choice", "lang_mother", "age_group3"),
  types = list(vote_choice = "recall"),
  missing = "reasons", quiet = TRUE
)
```

Une part pondérée et son intervalle de confiance, à l’intérieur d’une
étude, c’est un
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
et un appel au package survey :

``` r

d18 <- qes_design(h[h$study == "qes2018", ], weight = "weight_post")
d18 <- subset(d18, !is.na(vote_choice) & !is.na(lang_mother))
svyby(~I(vote_choice == "CAQ"), ~lang_mother, d18, svyciprop,
      method = "logit", vartype = "ci")
#>         lang_mother I(vote_choice == "CAQ")       ci_l      ci_u
#> French       French               0.4114613 0.38603333 0.4373710
#> English     English               0.1101960 0.07564846 0.1578270
#> Other         Other               0.1717929 0.10011622 0.2788822
```

Les figures répètent ce calcul pour chaque étude, avec les fonctions de
[`_data.R`](https://github.com/ThomasGareau/qesR/blob/main/vignettes/articles/_data.R).
Les estimations de chaque étude sont faites à l’intérieur de cette
étude, jamais en regroupant des études.

## Les francophones se sont fragmentés ; les non-francophones sont restés au PLQ

![Graphique linéaire du nombre effectif de partis à chaque élection
québécoise de 1998 à 2022 : le résultat officiel, les répondants
francophones et les répondants non francophones. Chez les francophones,
il passe de 2,8 en 1998 à 3,9 en 2022 ; chez les non-francophones, il
reste entre 1,3 et 2,5 jusqu'en 2018 et atteint 2,9 en 2022, avec un
intervalle large. Valeurs dans la vue en
tableau.](fr-realignement_files/figure-html/enp-light.png)![Graphique
linéaire du nombre effectif de partis à chaque élection québécoise de
1998 à 2022 : le résultat officiel, les répondants francophones et les
répondants non francophones. Chez les francophones, il passe de 2,8 en
1998 à 3,9 en 2022 ; chez les non-francophones, il reste entre 1,3 et
2,5 jusqu'en 2018 et atteint 2,9 en 2022, avec un intervalle large.
Valeurs dans la vue en
tableau.](fr-realignement_files/figure-html/enp-dark.png)

Source : qesR, variable regroupée vote_choice (vote déclaré) d'une Étude
électorale québécoise par élection et des sondages de 1998 (CROP et
CREATEC, francophones seulement, non pondérés ; leur recontact a
surreprésenté les indécis et les refus) ; résultats officiels
d'Élections Québec. Nombre effectif de partis = 1 / somme des carrés des
parts, le PV, ON et les autres partis comptant pour un. Bandes :
intervalles de confiance à 95 %. Pondéré avec la pondération
postélectorale de chaque étude ; points creux (1998, 2008) : non
pondéré, pondération en révision. L'axe horizontal est en années, avec
une coupure entre 1998 et 2007. Les panels Durand sont dans la vue en
tableau.

Vue en tableau

| Élection | Étude | Groupe | Nombre effectif \[IC à 95 %\] | n | Officiel, tous les électeurs | Pondération |
|---:|:---|:---|:---|---:|:---|:---|
| 1998 | Sondages de 1998 | Francophones | 2,78 \[2,67 ; 2,88\] | 1126 | 2,58 | non pondéré (pondération en révision) |
| 1998 | Sondages de 1998 | Non-francophones | n \< 30 | 0 | 2,58 | non pondéré (pondération en révision) |
| 2007 | EEQ 2007 | Francophones | 3,54 \[3,39 ; 3,70\] | 1525 | 3,47 | pondéré |
| 2007 | EEQ 2007 | Non-francophones | 2,53 \[1,94 ; 3,12\] | 163 | 3,47 | pondéré |
| 2007 | Panel 2007 | Francophones | 3,45 \[3,34 ; 3,56\] | 1337 | 3,47 | non pondéré (pondération en révision) |
| 2007 | Panel 2007 | Non-francophones | 1,78 \[1,49 ; 2,06\] | 155 | 3,47 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Francophones | 3,12 \[2,96 ; 3,29\] | 771 | 3,03 | non pondéré (pondération en révision) |
| 2008 | EEQ 2008 | Non-francophones | 1,58 \[1,32 ; 1,84\] | 127 | 3,03 | non pondéré (pondération en révision) |
| 2012 | EEQ 2012 | Francophones | 3,12 \[2,93 ; 3,31\] | 1094 | 3,60 | pondéré |
| 2012 | EEQ 2012 | Non-francophones | 2,14 \[1,75 ; 2,53\] | 180 | 3,60 | pondéré |
| 2012 | Panel 2012 | Francophones | 3,42 \[3,20 ; 3,64\] | 576 | 3,60 | non pondéré (pondération en révision) |
| 2012 | Panel 2012 | Non-francophones | 2,31 \[1,59 ; 3,03\] | 57 | 3,60 | non pondéré (pondération en révision) |
| 2014 | EEQ 2014 | Francophones | 3,68 \[3,51 ; 3,84\] | 981 | 3,38 | pondéré |
| 2014 | EEQ 2014 | Non-francophones | 1,34 \[1,15 ; 1,54\] | 201 | 3,38 | pondéré |
| 2018 | EEQ 2018 | Francophones | 3,62 \[3,45 ; 3,78\] | 1656 | 3,88 | pondéré |
| 2018 | EEQ 2018 | Non-francophones | 1,85 \[1,63 ; 2,06\] | 359 | 3,88 | pondéré |
| 2018 | Panel 2018 | Francophones | 3,48 \[3,11 ; 3,85\] | 560 | 3,88 | pondéré |
| 2018 | Panel 2018 | Non-francophones | 2,02 \[1,64 ; 2,40\] | 143 | 3,88 | pondéré |
| 2022 | EEQ 2022 | Francophones | 3,92 \[3,64 ; 4,20\] | 883 | 3,99 | pondéré |
| 2022 | EEQ 2022 | Non-francophones | 2,93 \[1,94 ; 3,92\] | 121 | 3,99 | pondéré |

Le vote francophone s'est fragmenté, de 2,8 à 3,9 partis effectifs ; le
vote non francophone est resté concentré jusqu'en 2018Nombre effectif de
partis à chaque élection : résultat officiel et vote déclaré selon la
langue maternelle, avec intervalles de confiance à 95 %

À remarquer :

- Sur l’ensemble de la période, les deux lignes montent, la ligne
  officielle de 2,6 à 4,0 et celle des francophones de 2,8 à 3,9, mais
  pas d’une élection à l’autre : de 2008 à 2012, le nombre officiel
  monte (3,0 à 3,6) alors que celui des francophones reste à 3,1, et en
  2014 le nombre officiel baisse (3,4) alors que celui des francophones
  monte (3,7). Le premier sommet, en 2007, est la lutte à trois du PQ,
  du PLQ et de l’ADQ.
- Chez les non-francophones, le vote reste concentré, avec un nombre
  effectif entre 1,3 et 2,5 de 2007 à 2018. Il atteint 2,9 en 2022,
  quand le PLQ tombe à 54 % de leur vote, mais l’intervalle va de 1,9 à
  3,9 : seuls 121 votants non francophones ont répondu, et l’ampleur du
  saut est incertaine.
- La ligne des enquêtes suit de près la ligne officielle, sans se
  confondre avec elle : les enquêtes surestiment certains partis (voir
  [Les enquêtes et les résultats
  officiels](https://thomasgareau.github.io/qesR/articles/fr-enquetes-resultats.md)).

## Le vote de chaque parti, chez les francophones et les autres

![Cinq petits graphiques linéaires, un par parti (PLQ, PQ, QS, ADQ,
CAQ), de la part du vote déclaré chez les francophones et chez les
non-francophones à chaque élection de 1998 à 2022, avec le résultat
officiel en trait. Le PLQ obtient 72 % du vote non francophone en 2018
contre 12 % du vote francophone ; la CAQ obtient 41 % des francophones
et 12 % des autres. Valeurs dans la vue en
tableau.](fr-realignement_files/figure-html/lang-light.png)![Cinq petits
graphiques linéaires, un par parti (PLQ, PQ, QS, ADQ, CAQ), de la part
du vote déclaré chez les francophones et chez les non-francophones à
chaque élection de 1998 à 2022, avec le résultat officiel en trait. Le
PLQ obtient 72 % du vote non francophone en 2018 contre 12 % du vote
francophone ; la CAQ obtient 41 % des francophones et 12 % des autres.
Valeurs dans la vue en
tableau.](fr-realignement_files/figure-html/lang-dark.png)

Source : qesR, variable regroupée vote_choice (vote déclaré), une Étude
électorale québécoise par élection et les sondages de 1998 (francophones
seulement). Trait : part officielle des votes valides, tous les
électeurs. Barres : intervalles de confiance à 95 % (logit). Pondéré ;
points creux (1998, 2008) : non pondéré, pondération en révision. Les
cellules de moins de 30 répondants ne sont pas tracées. L'axe horizontal
est en années, avec une coupure entre 1998 et 2007. L'ADQ a fusionné
avec la CAQ en 2012 ; les deux restent des partis distincts ici. Le PCQ,
les panels Durand et les autres partis sont dans la vue en tableau.

Vue en tableau

| Parti | Élection | Étude | Groupe | Part du vote déclaré \[IC à 95 %\] | n | Officiel, tous les électeurs | Pondération | Niveau |
|:---|---:|:---|:---|:---|---:|:---|:---|:---|
| PLQ | 1998 | Sondages de 1998 | Francophones | 34,1 \[31,4 ; 36,9\] | 1126 | 43,6 | non pondéré (pondération en révision) | comparable |
| PLQ | 2007 | EEQ 2007 | Francophones | 20,1 \[17,9 ; 22,6\] | 1525 | 33,1 | pondéré | comparable |
| PLQ | 2007 | EEQ 2007 | Non-francophones | 58,4 \[48,1 ; 68,1\] | 163 | 33,1 | pondéré | comparable |
| PLQ | 2007 | Panel 2007 | Francophones | 23,5 \[21,3 ; 25,8\] | 1337 | 33,1 | non pondéré (pondération en révision) | approximatif |
| PLQ | 2007 | Panel 2007 | Non-francophones | 73,5 \[66,1 ; 79,9\] | 155 | 33,1 | non pondéré (pondération en révision) | approximatif |
| PLQ | 2008 | EEQ 2008 | Francophones | 32,7 \[29,5 ; 36,1\] | 771 | 42,1 | non pondéré (pondération en révision) | comparable |
| PLQ | 2008 | EEQ 2008 | Non-francophones | 78,7 \[70,8 ; 85,0\] | 127 | 42,1 | non pondéré (pondération en révision) | comparable |
| PLQ | 2012 | EEQ 2012 | Francophones | 15,5 \[13,2 ; 18,0\] | 1094 | 31,2 | pondéré | identique |
| PLQ | 2012 | EEQ 2012 | Non-francophones | 65,5 \[57,3 ; 72,8\] | 180 | 31,2 | pondéré | identique |
| PLQ | 2012 | Panel 2012 | Francophones | 22,7 \[19,5 ; 26,3\] | 576 | 31,2 | non pondéré (pondération en révision) | approximatif |
| PLQ | 2012 | Panel 2012 | Non-francophones | 63,2 \[50,0 ; 74,6\] | 57 | 31,2 | non pondéré (pondération en révision) | approximatif |
| PLQ | 2014 | EEQ 2014 | Francophones | 24,2 \[21,3 ; 27,4\] | 981 | 41,5 | pondéré | comparable |
| PLQ | 2014 | EEQ 2014 | Non-francophones | 86,0 \[78,0 ; 91,4\] | 201 | 41,5 | pondéré | comparable |
| PLQ | 2018 | EEQ 2018 | Francophones | 12,2 \[10,7 ; 13,9\] | 1656 | 24,8 | pondéré | comparable |
| PLQ | 2018 | EEQ 2018 | Non-francophones | 71,8 \[66,6 ; 76,6\] | 359 | 24,8 | pondéré | comparable |
| PLQ | 2018 | Panel 2018 | Francophones | 16,1 \[13,0 ; 19,7\] | 560 | 24,8 | pondéré | comparable |
| PLQ | 2018 | Panel 2018 | Non-francophones | 67,8 \[59,0 ; 75,5\] | 143 | 24,8 | pondéré | comparable |
| PLQ | 2022 | EEQ 2022 | Francophones | 6,1 \[4,4 ; 8,4\] | 883 | 14,4 | pondéré | comparable |
| PLQ | 2022 | EEQ 2022 | Non-francophones | 54,0 \[40,5 ; 67,0\] | 121 | 14,4 | pondéré | comparable |
| PQ | 1998 | Sondages de 1998 | Francophones | 45,9 \[43,0 ; 48,8\] | 1126 | 42,9 | non pondéré (pondération en révision) | comparable |
| PQ | 2007 | EEQ 2007 | Francophones | 34,1 \[31,3 ; 37,1\] | 1525 | 28,3 | pondéré | comparable |
| PQ | 2007 | EEQ 2007 | Non-francophones | 15,0 \[8,3 ; 25,4\] | 163 | 28,3 | pondéré | comparable |
| PQ | 2007 | Panel 2007 | Francophones | 33,4 \[31,0 ; 36,0\] | 1337 | 28,3 | non pondéré (pondération en révision) | approximatif |
| PQ | 2007 | Panel 2007 | Non-francophones | 7,7 \[4,4 ; 13,1\] | 155 | 28,3 | non pondéré (pondération en révision) | approximatif |
| PQ | 2008 | EEQ 2008 | Francophones | 42,3 \[38,8 ; 45,8\] | 771 | 35,2 | non pondéré (pondération en révision) | comparable |
| PQ | 2008 | EEQ 2008 | Non-francophones | 7,9 \[4,3 ; 14,0\] | 127 | 35,2 | non pondéré (pondération en révision) | comparable |
| PQ | 2012 | EEQ 2012 | Francophones | 46,4 \[43,1 ; 49,8\] | 1094 | 31,9 | pondéré | identique |
| PQ | 2012 | EEQ 2012 | Non-francophones | 6,0 \[3,4 ; 10,2\] | 180 | 31,9 | pondéré | identique |
| PQ | 2012 | Panel 2012 | Francophones | 41,3 \[37,4 ; 45,4\] | 576 | 31,9 | non pondéré (pondération en révision) | approximatif |
| PQ | 2012 | Panel 2012 | Non-francophones | 8,8 \[3,7 ; 19,4\] | 57 | 31,9 | non pondéré (pondération en révision) | approximatif |
| PQ | 2014 | EEQ 2014 | Francophones | 35,7 \[32,3 ; 39,3\] | 981 | 25,4 | pondéré | comparable |
| PQ | 2014 | EEQ 2014 | Non-francophones | 4,1 \[1,4 ; 11,6\] | 201 | 25,4 | pondéré | comparable |
| PQ | 2018 | EEQ 2018 | Francophones | 23,4 \[21,3 ; 25,7\] | 1656 | 17,1 | pondéré | comparable |
| PQ | 2018 | EEQ 2018 | Non-francophones | 2,6 \[1,4 ; 5,1\] | 359 | 17,1 | pondéré | comparable |
| PQ | 2018 | Panel 2018 | Francophones | 18,0 \[14,6 ; 22,1\] | 560 | 17,1 | pondéré | comparable |
| PQ | 2018 | Panel 2018 | Non-francophones | 2,9 \[1,1 ; 7,5\] | 143 | 17,1 | pondéré | comparable |
| PQ | 2022 | EEQ 2022 | Francophones | 19,6 \[17,0 ; 22,6\] | 883 | 14,6 | pondéré | comparable |
| PQ | 2022 | EEQ 2022 | Non-francophones | 1,7 \[0,5 ; 6,1\] | 121 | 14,6 | pondéré | comparable |
| ADQ | 1998 | Sondages de 1998 | Francophones | 18,1 \[16,0 ; 20,5\] | 1126 | 11,8 | non pondéré (pondération en révision) | comparable |
| ADQ | 2007 | EEQ 2007 | Francophones | 34,4 \[31,7 ; 37,4\] | 1525 | 30,8 | pondéré | comparable |
| ADQ | 2007 | EEQ 2007 | Non-francophones | 12,1 \[7,0 ; 20,1\] | 163 | 30,8 | pondéré | comparable |
| ADQ | 2007 | Panel 2007 | Francophones | 34,6 \[32,1 ; 37,1\] | 1337 | 30,8 | non pondéré (pondération en révision) | approximatif |
| ADQ | 2007 | Panel 2007 | Non-francophones | 11,6 \[7,4 ; 17,7\] | 155 | 30,8 | non pondéré (pondération en révision) | approximatif |
| ADQ | 2008 | EEQ 2008 | Francophones | 17,8 \[15,2 ; 20,6\] | 771 | 16,4 | non pondéré (pondération en révision) | comparable |
| ADQ | 2008 | EEQ 2008 | Non-francophones | 5,5 \[2,6 ; 11,1\] | 127 | 16,4 | non pondéré (pondération en révision) | comparable |
| QS | 2007 | EEQ 2007 | Francophones | 5,3 \[4,0 ; 7,0\] | 1525 | 3,6 | pondéré | comparable |
| QS | 2007 | EEQ 2007 | Non-francophones | 1,7 \[0,4 ; 7,1\] | 163 | 3,6 | pondéré | comparable |
| QS | 2007 | Panel 2007 | Francophones | 4,0 \[3,0 ; 5,2\] | 1337 | 3,6 | non pondéré (pondération en révision) | approximatif |
| QS | 2007 | Panel 2007 | Non-francophones | 2,6 \[1,0 ; 6,7\] | 155 | 3,6 | non pondéré (pondération en révision) | approximatif |
| QS | 2008 | EEQ 2008 | Francophones | 4,7 \[3,4 ; 6,4\] | 771 | 3,8 | non pondéré (pondération en révision) | comparable |
| QS | 2008 | EEQ 2008 | Non-francophones | 1,6 \[0,4 ; 6,1\] | 127 | 3,8 | non pondéré (pondération en révision) | comparable |
| QS | 2012 | EEQ 2012 | Francophones | 6,4 \[5,0 ; 8,1\] | 1094 | 6,0 | pondéré | identique |
| QS | 2012 | EEQ 2012 | Non-francophones | 6,9 \[3,6 ; 12,8\] | 180 | 6,0 | pondéré | identique |
| QS | 2012 | Panel 2012 | Francophones | 6,9 \[5,1 ; 9,3\] | 576 | 6,0 | non pondéré (pondération en révision) | approximatif |
| QS | 2012 | Panel 2012 | Non-francophones | 8,8 \[3,7 ; 19,4\] | 57 | 6,0 | non pondéré (pondération en révision) | approximatif |
| QS | 2014 | EEQ 2014 | Francophones | 9,2 \[7,5 ; 11,2\] | 981 | 7,6 | pondéré | comparable |
| QS | 2014 | EEQ 2014 | Non-francophones | 4,7 \[2,1 ; 10,4\] | 201 | 7,6 | pondéré | comparable |
| QS | 2018 | EEQ 2018 | Francophones | 18,9 \[16,9 ; 20,9\] | 1656 | 16,1 | pondéré | comparable |
| QS | 2018 | EEQ 2018 | Non-francophones | 4,9 \[3,0 ; 7,9\] | 359 | 16,1 | pondéré | comparable |
| QS | 2018 | Panel 2018 | Francophones | 14,7 \[11,4 ; 18,9\] | 560 | 16,1 | pondéré | comparable |
| QS | 2018 | Panel 2018 | Non-francophones | 5,3 \[2,8 ; 9,9\] | 143 | 16,1 | pondéré | comparable |
| QS | 2022 | EEQ 2022 | Francophones | 18,8 \[16,2 ; 21,7\] | 883 | 15,4 | pondéré | comparable |
| QS | 2022 | EEQ 2022 | Non-francophones | 11,5 \[5,9 ; 21,3\] | 121 | 15,4 | pondéré | comparable |
| CAQ | 2012 | EEQ 2012 | Francophones | 27,4 \[24,5 ; 30,4\] | 1094 | 27,1 | pondéré | identique |
| CAQ | 2012 | EEQ 2012 | Non-francophones | 16,8 \[11,3 ; 24,2\] | 180 | 27,1 | pondéré | identique |
| CAQ | 2012 | Panel 2012 | Francophones | 25,2 \[21,8 ; 28,9\] | 576 | 27,1 | non pondéré (pondération en révision) | approximatif |
| CAQ | 2012 | Panel 2012 | Non-francophones | 10,5 \[4,8 ; 21,5\] | 57 | 27,1 | non pondéré (pondération en révision) | approximatif |
| CAQ | 2014 | EEQ 2014 | Francophones | 27,7 \[24,5 ; 31,1\] | 981 | 23,1 | pondéré | comparable |
| CAQ | 2014 | EEQ 2014 | Non-francophones | 4,2 \[1,7 ; 10,2\] | 201 | 23,1 | pondéré | comparable |
| CAQ | 2018 | EEQ 2018 | Francophones | 41,1 \[38,6 ; 43,7\] | 1656 | 37,4 | pondéré | comparable |
| CAQ | 2018 | EEQ 2018 | Non-francophones | 12,5 \[9,2 ; 16,7\] | 359 | 37,4 | pondéré | comparable |
| CAQ | 2018 | Panel 2018 | Francophones | 45,1 \[40,2 ; 50,2\] | 560 | 37,4 | pondéré | comparable |
| CAQ | 2018 | Panel 2018 | Non-francophones | 16,2 \[10,5 ; 24,2\] | 143 | 37,4 | pondéré | comparable |
| CAQ | 2022 | EEQ 2022 | Francophones | 39,5 \[35,9 ; 43,1\] | 883 | 41,0 | pondéré | comparable |
| CAQ | 2022 | EEQ 2022 | Non-francophones | 12,2 \[4,3 ; 30,2\] | 121 | 41,0 | pondéré | comparable |
| PCQ | 2022 | EEQ 2022 | Francophones | 14,7 \[12,2 ; 17,6\] | 883 | 12,9 | pondéré | comparable |
| PCQ | 2022 | EEQ 2022 | Non-francophones | 9,4 \[5,1 ; 16,7\] | 121 | 12,9 | pondéré | comparable |
| Autres | 1998 | Sondages de 1998 | Francophones | 1,9 \[1,2 ; 2,8\] | 1126 | 1,8 | non pondéré (pondération en révision) | comparable |
| Autres | 2007 | EEQ 2007 | Francophones | 5,9 \[4,6 ; 7,6\] | 1525 | 4,1 | pondéré | comparable |
| Autres | 2007 | EEQ 2007 | Non-francophones | 12,8 \[7,3 ; 21,4\] | 163 | 4,1 | pondéré | comparable |
| Autres | 2007 | Panel 2007 | Francophones | 4,6 \[3,6 ; 5,8\] | 1337 | 4,1 | non pondéré (pondération en révision) | approximatif |
| Autres | 2007 | Panel 2007 | Non-francophones | 4,5 \[2,2 ; 9,2\] | 155 | 4,1 | non pondéré (pondération en révision) | approximatif |
| Autres | 2008 | EEQ 2008 | Francophones | 2,6 \[1,7 ; 4,0\] | 771 | 2,6 | non pondéré (pondération en révision) | comparable |
| Autres | 2008 | EEQ 2008 | Non-francophones | 6,3 \[3,2 ; 12,1\] | 127 | 2,6 | non pondéré (pondération en révision) | comparable |
| Autres | 2012 | EEQ 2012 | Francophones | 4,3 \[3,3 ; 5,7\] | 1094 | 3,6 | pondéré | identique |
| Autres | 2012 | EEQ 2012 | Non-francophones | 4,9 \[2,5 ; 9,4\] | 180 | 3,6 | pondéré | identique |
| Autres | 2012 | Panel 2012 | Francophones | 3,8 \[2,5 ; 5,7\] | 576 | 3,6 | non pondéré (pondération en révision) | approximatif |
| Autres | 2012 | Panel 2012 | Non-francophones | 8,8 \[3,7 ; 19,4\] | 57 | 3,6 | non pondéré (pondération en révision) | approximatif |
| Autres | 2014 | EEQ 2014 | Francophones | 3,2 \[2,2 ; 4,7\] | 981 | 2,0 | pondéré | comparable |
| Autres | 2014 | EEQ 2014 | Non-francophones | 0,9 \[0,3 ; 2,4\] | 201 | 2,0 | pondéré | comparable |
| Autres | 2018 | EEQ 2018 | Francophones | 4,3 \[3,3 ; 5,6\] | 1656 | 3,1 | pondéré | comparable |
| Autres | 2018 | EEQ 2018 | Non-francophones | 8,2 \[5,6 ; 11,8\] | 359 | 3,1 | pondéré | comparable |
| Autres | 2018 | Panel 2018 | Francophones | 6,0 \[4,0 ; 9,1\] | 560 | 3,1 | pondéré | comparable |
| Autres | 2018 | Panel 2018 | Non-francophones | 7,7 \[4,3 ; 13,5\] | 143 | 3,1 | pondéré | comparable |
| Autres | 2022 | EEQ 2022 | Francophones | 1,3 \[0,7 ; 2,5\] | 883 | 1,7 | pondéré | comparable |
| Autres | 2022 | EEQ 2022 | Non-francophones | 11,2 \[5,1 ; 22,7\] | 121 | 1,7 | pondéré | comparable |

Les non-francophones votent PLQ (72 % en 2018) ; les victoires de la CAQ
sont francophones (41 % des francophones, 12 % des autres)Part du vote
déclaré chez les francophones et les non-francophones, selon le parti,
avec intervalles de confiance à 95 % et résultat officiel

À remarquer :

- Le PLQ est resté le parti des non-francophones : 58 % de leur vote en
  2007, 86 % en 2014, tandis que son vote francophone passait de 34 % en
  1998 (les sondages de 1998) à 6 % en 2022, bien sous sa part
  officielle.
- Le PQ a perdu les quelques électeurs non francophones qu’il avait
  (15 % en 2007, 2 % en 2022). Québec solidaire a progressé d’abord chez
  les francophones, puis chez les autres en 2022.
- Les victoires de la CAQ sont des victoires francophones ; comme l’ADQ
  avant elle, elle rejoint à peine les autres électeurs (12 % en 2022).

## L’écart d’âge, parti par parti

![Six petits graphiques, un par parti (PLQ, PQ, ADQ, QS, CAQ, PCQ) : la
part du parti chez les 18 à 34 ans moins sa part chez les 55 ans et
plus, en points, à chaque élection de 1998 à 2022, avec intervalles de
confiance. Le PLQ est sous zéro à chaque élection. En 2022, Québec
solidaire est à +31 pts et la CAQ à −34 pts. Valeurs dans la vue en
tableau.](fr-realignement_files/figure-html/age-light.png)![Six petits
graphiques, un par parti (PLQ, PQ, ADQ, QS, CAQ, PCQ) : la part du parti
chez les 18 à 34 ans moins sa part chez les 55 ans et plus, en points, à
chaque élection de 1998 à 2022, avec intervalles de confiance. Le PLQ
est sous zéro à chaque élection. En 2022, Québec solidaire est à +31 pts
et la CAQ à −34 pts. Valeurs dans la vue en
tableau.](fr-realignement_files/figure-html/age-dark.png)

Source : qesR, variables regroupées vote_choice (vote déclaré) et
age_group3 ; une Étude électorale québécoise par élection et les
sondages de 1998 (francophones seulement). Au-dessus de zéro : le parti
fait mieux chez les jeunes. Barres : intervalles de confiance à 95 % de
la différence, à partir de la covariance des estimations des deux
groupes d'âge. Pondéré avec la pondération postélectorale de chaque
étude ; creux (1998, 2008) : non pondéré, pondération en révision. L'axe
horizontal est en années, avec une coupure entre 1998 et 2007. Les parts
de chaque groupe d'âge et les panels Durand sont dans la vue en tableau.

Vue en tableau

| Parti | Élection | Étude | 18 à 34 ans, % | 55 ans et plus, % | Écart, 18-34 moins 55+ \[IC à 95 %\] | Pondération |
|:---|---:|:---|:---|:---|:---|:---|
| PLQ | 1998 | Sondages de 1998 | 25,0 | 55,1 | −30,1 pts \[-37,7 ; -22,6\] | non pondéré (pondération en révision) |
| PLQ | 2007 | EEQ 2007 | 25,0 | 55,1 | −20,3 pts \[-26,8 ; -13,9\] | pondéré |
| PLQ | 2007 | Panel 2007 | 25,0 | 55,1 | −25,7 pts \[-31,6 ; -19,8\] | non pondéré (pondération en révision) |
| PLQ | 2008 | EEQ 2008 | 25,0 | 55,1 | −20,0 pts \[-28,3 ; -11,7\] | non pondéré (pondération en révision) |
| PLQ | 2012 | EEQ 2012 | 25,0 | 55,1 | −14,9 pts \[-21,7 ; -8,0\] | pondéré |
| PLQ | 2012 | Panel 2012 | 25,0 | 55,1 | −25,1 pts \[-33,0 ; -17,2\] | non pondéré (pondération en révision) |
| PLQ | 2014 | EEQ 2014 | 25,0 | 55,1 | −5,0 pts \[-13,0 ; 3,0\] | pondéré |
| PLQ | 2018 | EEQ 2018 | 25,0 | 55,1 | −12,0 pts \[-16,6 ; -7,5\] | pondéré |
| PLQ | 2018 | Panel 2018 | 25,0 | 55,1 | −17,5 pts \[-27,1 ; -7,9\] | pondéré |
| PLQ | 2022 | EEQ 2022 | 25,0 | 55,1 | −4,8 pts \[-14,8 ; 5,1\] | pondéré |
| PQ | 1998 | Sondages de 1998 | 25,0 | 55,1 | +9,8 pts \[1,9 ; 17,7\] | non pondéré (pondération en révision) |
| PQ | 2007 | EEQ 2007 | 25,0 | 55,1 | +6,8 pts \[-0,1 ; 13,8\] | pondéré |
| PQ | 2007 | Panel 2007 | 25,0 | 55,1 | +3,4 pts \[-3,2 ; 10,0\] | non pondéré (pondération en révision) |
| PQ | 2008 | EEQ 2008 | 25,0 | 55,1 | −0,2 pts \[-8,7 ; 8,2\] | non pondéré (pondération en révision) |
| PQ | 2012 | EEQ 2012 | 25,0 | 55,1 | +2,8 pts \[-4,9 ; 10,4\] | pondéré |
| PQ | 2012 | Panel 2012 | 25,0 | 55,1 | −7,3 pts \[-18,2 ; 3,5\] | non pondéré (pondération en révision) |
| PQ | 2014 | EEQ 2014 | 25,0 | 55,1 | −13,9 pts \[-21,5 ; -6,3\] | pondéré |
| PQ | 2018 | EEQ 2018 | 25,0 | 55,1 | −8,2 pts \[-12,4 ; -3,9\] | pondéré |
| PQ | 2018 | Panel 2018 | 25,0 | 55,1 | −6,4 pts \[-13,7 ; 0,8\] | pondéré |
| PQ | 2022 | EEQ 2022 | 25,0 | 55,1 | −4,4 pts \[-10,6 ; 1,9\] | pondéré |
| ADQ | 1998 | Sondages de 1998 | 25,0 | 55,1 | +19,6 pts \[13,4 ; 25,8\] | non pondéré (pondération en révision) |
| ADQ | 2007 | EEQ 2007 | 25,0 | 55,1 | +3,5 pts \[-2,9 ; 9,8\] | pondéré |
| ADQ | 2007 | Panel 2007 | 25,0 | 55,1 | +13,0 pts \[6,4 ; 19,5\] | non pondéré (pondération en révision) |
| ADQ | 2008 | EEQ 2008 | 25,0 | 55,1 | +11,0 pts \[4,4 ; 17,6\] | non pondéré (pondération en révision) |
| QS | 2007 | EEQ 2007 | 25,0 | 55,1 | +3,2 pts \[0,2 ; 6,2\] | pondéré |
| QS | 2007 | Panel 2007 | 25,0 | 55,1 | +3,9 pts \[1,0 ; 6,7\] | non pondéré (pondération en révision) |
| QS | 2008 | EEQ 2008 | 25,0 | 55,1 | +5,3 pts \[1,3 ; 9,4\] | non pondéré (pondération en révision) |
| QS | 2012 | EEQ 2012 | 25,0 | 55,1 | +7,4 pts \[3,2 ; 11,5\] | pondéré |
| QS | 2012 | Panel 2012 | 25,0 | 55,1 | +11,0 pts \[3,7 ; 18,2\] | non pondéré (pondération en révision) |
| QS | 2014 | EEQ 2014 | 25,0 | 55,1 | +12,0 pts \[7,3 ; 16,8\] | pondéré |
| QS | 2018 | EEQ 2018 | 25,0 | 55,1 | +22,7 pts \[17,6 ; 27,7\] | pondéré |
| QS | 2018 | Panel 2018 | 25,0 | 55,1 | +19,8 pts \[9,6 ; 30,0\] | pondéré |
| QS | 2022 | EEQ 2022 | 25,0 | 55,1 | +30,6 pts \[22,7 ; 38,6\] | pondéré |
| CAQ | 2012 | EEQ 2012 | 25,0 | 55,1 | −1,2 pts \[-7,7 ; 5,4\] | pondéré |
| CAQ | 2012 | Panel 2012 | 25,0 | 55,1 | +11,1 pts \[1,1 ; 21,1\] | non pondéré (pondération en révision) |
| CAQ | 2014 | EEQ 2014 | 25,0 | 55,1 | +3,2 pts \[-4,3 ; 10,8\] | pondéré |
| CAQ | 2018 | EEQ 2018 | 25,0 | 55,1 | −9,4 pts \[-14,9 ; -3,9\] | pondéré |
| CAQ | 2018 | Panel 2018 | 25,0 | 55,1 | −2,1 pts \[-14,8 ; 10,6\] | pondéré |
| CAQ | 2022 | EEQ 2022 | 25,0 | 55,1 | −33,8 pts \[-42,1 ; -25,5\] | pondéré |
| PCQ | 2022 | EEQ 2022 | 25,0 | 55,1 | +9,6 pts \[3,9 ; 15,3\] | pondéré |

En 2022, Québec solidaire a gagné les jeunes et la CAQ les aînés : des
écarts d'âge de +31 pts et −34 ptsPart du vote déclaré de chaque parti
chez les 18 à 34 ans moins sa part chez les 55 ans et plus, en points,
avec intervalles de confiance à 95 %

À remarquer :

- En 2007, les jeunes votaient PQ (36 %) et ADQ (30 %) ; en 2018 et en
  2022, leur premier choix était Québec solidaire (37 % en 2022), qui
  attire peu les électeurs de 55 ans et plus (6 %).
- La CAQ est devenue le parti des aînés : 48 % du vote des 55 ans et
  plus en 2022, contre 15 % de celui des 18 à 34 ans.
- Jusqu’en 2012, le PQ faisait aussi bien ou mieux chez les jeunes (de
  +10 pts en 1998 et de +7 pts en 2007) ; à partir de 2014, il fait
  mieux chez les aînés (−14 pts en 2014). Le PLQ fait mieux chez les
  aînés à chaque élection.

## À propos des données

- **Études.** Une Étude électorale québécoise par élection (2007, 2008,
  2012, 2014, 2018, 2022) et les sondages de 1998, qui n’ont interrogé
  que des francophones. Les panels Durand de 2007, 2012 et 2018 sont
  dans les vues en tableau. Les sondages CROP n’ont pas demandé le vote
  déclaré.
- **Variable.** `vote_choice` avec
  `types = list(vote_choice = "recall")` : le vote déclaré après
  l’élection, dans toutes les études. La question source de chaque étude
  est `vote_choice__item` (par exemple qes2018:post:q6). Les répondants
  qui n’ont pas voté, ont annulé leur bulletin, ne savent pas ou
  refusent sont `NA`, avec un motif dans `vote_choice__na`, et sont
  exclus du dénominateur.
- **Niveaux de comparabilité.** La question du vote déclaré a le niveau
  `identical` en 2012, `approximate` dans les panels 2007 et 2012,
  `comparable` ailleurs : les listes de partis diffèrent
  (`qes_spec("crosswalk", targets = "vote_prov_recall")`). Un parti
  qu’une étude ne proposait pas, ou qui ne se présentait pas, n’est pas
  tracé.
- **Langue maternelle.** `lang_mother`, « français » contre toute autre
  réponse. En 2022, elle vient d’une question à choix multiples (les
  langues apprises dans l’enfance et encore comprises), de niveau
  `approximate` : les 146 répondants qui ont coché deux langues ou plus
  (dont 117 le français et l’anglais) n’y ont pas de langue maternelle
  et sont exclus de la répartition selon la langue.
- **Pondérations.** La pondération postélectorale recommandée de chaque
  étude, par
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md).
  Les sondages de 1998, l’étude de 2008 et les panels Durand de 2007 et
  2012 sont non pondérés (pondérations en révision) et tracés en creux ;
  le panel 2018 utilise sa pondération postélectorale révisée. Le
  recontact des sondages de 1998 a aussi surreprésenté les indécis et
  les refus.
