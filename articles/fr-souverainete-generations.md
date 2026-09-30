# Un écart générationnel inversé : l'appui à la souveraineté, 1998-2022

*[English
version](https://thomasgareau.github.io/qesR/articles/sovereignty-generations.md)*

En 2007, les plus jeunes électeurs francophones étaient les plus
souverainistes : 58 % des personnes nées de 1975 à 1989 auraient voté
Oui à la question de 1995, contre 38 % des personnes nées en 1944 ou
avant. Cette question offrait un partenariat avec le Canada et recueille
plus de Oui que la question sur un pays indépendant posée à partir de
2012 : les niveaux de 2007 et de 2022 ne se comparent pas, l’ordre des
cohortes, si. Sur la question de l’indépendance, l’ordre s’est inversé
depuis 2012 : la cohorte née de 1975 à 1989 est passée de 54 % de Oui à
38 % en 2022, et les francophones nés en 1990 ou après de 53 % à 30 %,
la part la plus basse de toutes les cohortes, tandis que la cohorte née
de 1945 à 1959 est restée entre 50 % et 54 %. La cohorte qui portait le
Oui dans les années 2000 n’a pas gardé son avance. La cohorte la plus
jeune est ouverte, et sa composition change : nés de 1990 à 1994 (18 à
22 ans) en 2012, de 1990 à 2004 en 2022, et en 2018 elle compte des
jeunes de 16 et 17 ans.

## Une variable, plusieurs questions

`sov_support` regroupe les questions référendaires de toutes les études
qui en ont posé une. Le libellé change d’une étude à l’autre, et
`sov_support__type` dit à quelle question répond chaque valeur :

``` r

h <- qes_harmonize(
  studies = qz_studies,
  targets = c("sov_support", "birth_year", "lang_mother"),
  missing = "reasons", quiet = TRUE
)
```

``` r

table(h$study, h$sov_support__type)
#>                     
#>                      independence sovereign_country partnership_1995_push
#>   qes_crop_2007_2010            0                 0                     0
#>   qes1998                       0                 0                  1483
#>   qes2007                       0                 0                  2175
#>   qes2007_panel                 0                 0                  2050
#>   qes2008                       0                 0                  1151
#>   qes2012                    1505                 0                     0
#>   qes2012_panel                 0               844                     0
#>   qes2014                    1517                 0                     0
#>   qes2018                    3072                 0                     0
#>   qes2018_panel                 0                 0                     0
#>   qes2022                    1521                 0                     0
#>                     
#>                      partnership_1995 favour
#>   qes_crop_2007_2010                0      0
#>   qes1998                           0      0
#>   qes2007                           0      0
#>   qes2007_panel                     0      0
#>   qes2008                           0      0
#>   qes2012                           0      0
#>   qes2012_panel                     0      0
#>   qes2014                           0      0
#>   qes2018                           0      0
#>   qes2018_panel                     0    842
#>   qes2022                           0      0
```

L’appui dépend du libellé : cette page compare donc les cohortes à
l’intérieur d’un même libellé et d’une même étude, et trace les libellés
en séries distinctes.

## L’appui selon l’étude et le libellé

![Graphique à points avec intervalles de confiance de la part qui
voterait Oui, parmi celles et ceux qui voteraient Oui ou Non, dans
chaque étude de 1998 à 2022, la forme de chaque point donnant le libellé
de la question. Dans les Études électorales québécoises, l'appui est de
43 % à 46 % sur la question de 1995 (2007, 2008) et de 34 % à 40 % sur
un pays indépendant (2012 à 2022). Le point de 1998 (francophones
seulement, non pondéré) est à part. Valeurs dans la vue en
tableau.](fr-souverainete-generations_files/figure-html/wording-light.png)![Graphique
à points avec intervalles de confiance de la part qui voterait Oui,
parmi celles et ceux qui voteraient Oui ou Non, dans chaque étude de
1998 à 2022, la forme de chaque point donnant le libellé de la question.
Dans les Études électorales québécoises, l'appui est de 43 % à 46 % sur
la question de 1995 (2007, 2008) et de 34 % à 40 % sur un pays
indépendant (2012 à 2022). Le point de 1998 (francophones seulement, non
pondéré) est à part. Valeurs dans la vue en
tableau.](fr-souverainete-generations_files/figure-html/wording-dark.png)

Source : qesR, variable regroupée sov_support, toutes les études qui ont
posé une question référendaire (tous les répondants). La ligne relie la
question sur l'indépendance dans les Études électorales québécoises
(juste à gauche de chaque élection) ; les panels Durand sont juste à
droite. Le point de 1998 ne vient que du volet CROP des sondages de 1998
(CREATEC n'a pas posé la question) : francophones seulement, non
pondéré, d'un recontact qui a surreprésenté les indécis et les refus ;
il n'est donc pas relié aux autres et n'est pas comparable au résultat
de 1995, qui compte tous les électeurs. Barres : intervalles de
confiance à 95 % (logit). Pondéré avec la pondération de la vague qui a
posé la question ; creux : non pondéré, pondération en révision.
Favorable ou opposé (panel 2018) est regroupé en oui ou non, niveau
approximatif. L'axe horizontal est en années, avec une coupure entre
1998 et 2007.

Vue en tableau

| Élection | Étude | Question | Oui, % \[IC à 95 %\] | n | Pondération | Niveau |
|---:|:---|:---|:---|---:|:---|:---|
| 1998 | Sondages de 1998 (volet CROP, francophones) | La question de 1995 (partenariat) | 43,4 \[38,5 ; 48,5\] | 380 | non pondéré (pondération en révision) | comparable |
| 2007 | EEQ 2007 | La question de 1995 (partenariat) | 42,5 \[39,9 ; 45,2\] | 2011 | pondéré | identique |
| 2007 | Panel 2007 | La question de 1995 (partenariat) | 44,8 \[42,6 ; 47,1\] | 1899 | non pondéré (pondération en révision) | comparable |
| 2008 | EEQ 2008 | La question de 1995 (partenariat) | 45,8 \[42,7 ; 48,8\] | 1038 | non pondéré (pondération en révision) | comparable |
| 2012 | EEQ 2012 | Un pays indépendant | 40,4 \[37,5 ; 43,4\] | 1323 | pondéré | identique |
| 2012 | Panel 2012 | Un pays souverain | 35,1 \[31,8 ; 38,6\] | 743 | non pondéré (pondération en révision) | identique |
| 2014 | EEQ 2014 | Un pays indépendant | 34,8 \[31,8 ; 37,9\] | 1353 | pondéré | identique |
| 2018 | EEQ 2018 | Un pays indépendant | 34,6 \[32,6 ; 36,7\] | 2558 | pondéré | comparable |
| 2018 | Panel 2018 | Favorable à l'indépendance | 31,7 \[27,8 ; 35,8\] | 780 | pondéré | approximatif |
| 2022 | EEQ 2022 | Un pays indépendant | 34,3 \[30,8 ; 37,9\] | 1284 | pondéré | comparable |

Le Oui recueille moins de votes sur l'indépendance que sur la question
de 1995 : 34 à 40 % contre 43 à 46 %Voterait Oui à un référendum, parmi
celles et ceux qui voteraient Oui ou Non, selon l'étude et le libellé de
la question, avec intervalles de confiance à 95 %

À remarquer :

- La question de 1995, qui offrait un partenariat avec le Canada,
  recueille plus de Oui qu’une question sur un pays indépendant : ce
  sont deux séries distinctes, et la bande grise marque le changement de
  question dans les Études électorales québécoises.
- Sur la question de l’indépendance, l’appui dans les Études électorales
  québécoises passe de 40 % en 2012 à environ 34 % à partir de 2014.
- Les panels Durand ont posé d’autres libellés ; chacun est un point à
  part.

## Les cohortes dans le temps

![Graphique linéaire de la part des francophones qui voteraient Oui à ce
que le Québec devienne un pays indépendant, pour cinq cohortes de
naissance, aux élections de 2012, 2014, 2018 et 2022 ; les cohortes nées
en 1945-1959 et en 1990 ou après sont mises en évidence avec des bandes
de confiance. Les personnes nées en 1990 ou après passent de 53 % à 30 %
; celles nées en 1945-1959 restent entre 50 % et 54 %. Valeurs dans la
vue en
tableau.](fr-souverainete-generations_files/figure-html/cohorts-light.png)![Graphique
linéaire de la part des francophones qui voteraient Oui à ce que le
Québec devienne un pays indépendant, pour cinq cohortes de naissance,
aux élections de 2012, 2014, 2018 et 2022 ; les cohortes nées en
1945-1959 et en 1990 ou après sont mises en évidence avec des bandes de
confiance. Les personnes nées en 1990 ou après passent de 53 % à 30 % ;
celles nées en 1945-1959 restent entre 50 % et 54 %. Valeurs dans la vue
en
tableau.](fr-souverainete-generations_files/figure-html/cohorts-dark.png)

Source : qesR, variables regroupées sov_support (type independence),
birth_year et lang_mother ; EEQ 2012, 2014, 2018 et 2022, répondants
francophones, chaque étude pondérée avec la pondération de la vague qui
a posé la question. Bandes : intervalles de confiance à 95 % (logit) ;
les intervalles de chaque cohorte sont dans la figure suivante et dans
la vue en tableau, qui donne aussi tous les répondants et les études de
2007 et 2008 (question de 1995). La ligne de la cohorte la plus âgée
repose sur moins de 100 répondants en 2012, 2014, 2022. La cohorte la
plus jeune est ouverte : nés en 1990-1994 (18 à 22 ans) en 2012, nés en
1990-2004 en 2022 ; l'étude de 2018 a interrogé les personnes de 16 ans
et plus.

Vue en tableau

| Répondants | Élection | Étude | Question | Cohorte | Oui, % \[IC à 95 %\] | n | Pondération |
|:---|---:|:---|:---|:---|:---|---:|:---|
| Tous les répondants | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1944 ou avant | 35,1 \[29,9 ; 40,8\] | 381 | pondéré |
| Tous les répondants | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1945-1959 | 44,7 \[40,2 ; 49,4\] | 673 | pondéré |
| Tous les répondants | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1960-1974 | 41,6 \[36,4 ; 46,9\] | 486 | pondéré |
| Tous les répondants | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1975-1989 | 47,9 \[42,2 ; 53,7\] | 438 | pondéré |
| Tous les répondants | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1944 ou avant | 39,7 \[33,3 ; 46,5\] | 209 | non pondéré (pondération en révision) |
| Tous les répondants | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1945-1959 | 53,8 \[47,8 ; 59,8\] | 262 | non pondéré (pondération en révision) |
| Tous les répondants | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1960-1974 | 43,9 \[38,7 ; 49,3\] | 330 | non pondéré (pondération en révision) |
| Tous les répondants | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1975-1989 | 46,0 \[39,4 ; 52,7\] | 213 | non pondéré (pondération en révision) |
| Tous les répondants | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1990 ou après | n \< 30 | 5 | non pondéré (pondération en révision) |
| Tous les répondants | 2012 | EEQ 2012 | Un pays indépendant | nés en 1944 ou avant | 24,0 \[16,2 ; 34,1\] | 91 | pondéré |
| Tous les répondants | 2012 | EEQ 2012 | Un pays indépendant | nés en 1945-1959 | 42,8 \[36,4 ; 49,4\] | 258 | pondéré |
| Tous les répondants | 2012 | EEQ 2012 | Un pays indépendant | nés en 1960-1974 | 39,2 \[34,2 ; 44,3\] | 394 | pondéré |
| Tous les répondants | 2012 | EEQ 2012 | Un pays indépendant | nés en 1975-1989 | 46,0 \[41,2 ; 50,9\] | 439 | pondéré |
| Tous les répondants | 2012 | EEQ 2012 | Un pays indépendant | nés en 1990 ou après | 45,4 \[36,9 ; 54,2\] | 141 | pondéré |
| Tous les répondants | 2014 | EEQ 2014 | Un pays indépendant | nés en 1944 ou avant | 26,3 \[18,2 ; 36,5\] | 128 | pondéré |
| Tous les répondants | 2014 | EEQ 2014 | Un pays indépendant | nés en 1945-1959 | 43,6 \[37,4 ; 49,9\] | 354 | pondéré |
| Tous les répondants | 2014 | EEQ 2014 | Un pays indépendant | nés en 1960-1974 | 31,4 \[26,1 ; 37,2\] | 393 | pondéré |
| Tous les répondants | 2014 | EEQ 2014 | Un pays indépendant | nés en 1975-1989 | 29,0 \[23,6 ; 35,1\] | 307 | pondéré |
| Tous les répondants | 2014 | EEQ 2014 | Un pays indépendant | nés en 1990 ou après | 41,6 \[33,4 ; 50,2\] | 171 | pondéré |
| Tous les répondants | 2018 | EEQ 2018 | Un pays indépendant | nés en 1944 ou avant | 30,3 \[25,4 ; 35,7\] | 356 | pondéré |
| Tous les répondants | 2018 | EEQ 2018 | Un pays indépendant | nés en 1945-1959 | 37,2 \[33,4 ; 41,1\] | 627 | pondéré |
| Tous les répondants | 2018 | EEQ 2018 | Un pays indépendant | nés en 1960-1974 | 36,8 \[32,5 ; 41,3\] | 498 | pondéré |
| Tous les répondants | 2018 | EEQ 2018 | Un pays indépendant | nés en 1975-1989 | 33,9 \[29,1 ; 39,0\] | 387 | pondéré |
| Tous les répondants | 2018 | EEQ 2018 | Un pays indépendant | nés en 1990 ou après | 31,5 \[27,4 ; 36,0\] | 655 | pondéré |
| Tous les répondants | 2022 | EEQ 2022 | Un pays indépendant | nés en 1944 ou avant | 40,4 \[21,9 ; 62,2\] | 60 | pondéré |
| Tous les répondants | 2022 | EEQ 2022 | Un pays indépendant | nés en 1945-1959 | 40,7 \[34,0 ; 47,6\] | 318 | pondéré |
| Tous les répondants | 2022 | EEQ 2022 | Un pays indépendant | nés en 1960-1974 | 35,6 \[29,4 ; 42,4\] | 319 | pondéré |
| Tous les répondants | 2022 | EEQ 2022 | Un pays indépendant | nés en 1975-1989 | 31,7 \[25,7 ; 38,3\] | 303 | pondéré |
| Tous les répondants | 2022 | EEQ 2022 | Un pays indépendant | nés en 1990 ou après | 26,7 \[20,8 ; 33,5\] | 283 | pondéré |
| Francophones | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1944 ou avant | 38,1 \[32,5 ; 44,0\] | 343 | pondéré |
| Francophones | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1945-1959 | 51,2 \[46,4 ; 56,0\] | 588 | pondéré |
| Francophones | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1960-1974 | 51,7 \[46,0 ; 57,3\] | 406 | pondéré |
| Francophones | 2007 | EEQ 2007 | La question de 1995 (partenariat) | nés en 1975-1989 | 57,8 \[51,8 ; 63,5\] | 379 | pondéré |
| Francophones | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1944 ou avant | 45,4 \[38,2 ; 52,9\] | 174 | non pondéré (pondération en révision) |
| Francophones | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1945-1959 | 62,1 \[55,5 ; 68,3\] | 219 | non pondéré (pondération en révision) |
| Francophones | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1960-1974 | 50,5 \[44,7 ; 56,4\] | 275 | non pondéré (pondération en révision) |
| Francophones | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1975-1989 | 52,7 \[45,5 ; 59,9\] | 182 | non pondéré (pondération en révision) |
| Francophones | 2008 | EEQ 2008 | La question de 1995 (partenariat) | nés en 1990 ou après | n \< 30 | 4 | non pondéré (pondération en révision) |
| Francophones | 2012 | EEQ 2012 | Un pays indépendant | nés en 1944 ou avant | 37,0 \[25,6 ; 50,0\] | 62 | pondéré |
| Francophones | 2012 | EEQ 2012 | Un pays indépendant | nés en 1945-1959 | 53,6 \[46,4 ; 60,6\] | 214 | pondéré |
| Francophones | 2012 | EEQ 2012 | Un pays indépendant | nés en 1960-1974 | 46,2 \[40,6 ; 51,9\] | 327 | pondéré |
| Francophones | 2012 | EEQ 2012 | Un pays indépendant | nés en 1975-1989 | 54,0 \[48,8 ; 59,2\] | 374 | pondéré |
| Francophones | 2012 | EEQ 2012 | Un pays indépendant | nés en 1990 ou après | 53,5 \[44,1 ; 62,6\] | 120 | pondéré |
| Francophones | 2014 | EEQ 2014 | Un pays indépendant | nés en 1944 ou avant | 33,2 \[23,1 ; 45,2\] | 90 | pondéré |
| Francophones | 2014 | EEQ 2014 | Un pays indépendant | nés en 1945-1959 | 50,7 \[43,9 ; 57,5\] | 280 | pondéré |
| Francophones | 2014 | EEQ 2014 | Un pays indépendant | nés en 1960-1974 | 40,5 \[34,2 ; 47,2\] | 288 | pondéré |
| Francophones | 2014 | EEQ 2014 | Un pays indépendant | nés en 1975-1989 | 38,1 \[31,2 ; 45,6\] | 214 | pondéré |
| Francophones | 2014 | EEQ 2014 | Un pays indépendant | nés en 1990 ou après | 50,8 \[41,1 ; 60,4\] | 124 | pondéré |
| Francophones | 2018 | EEQ 2018 | Un pays indépendant | nés en 1944 ou avant | 38,2 \[32,2 ; 44,4\] | 280 | pondéré |
| Francophones | 2018 | EEQ 2018 | Un pays indépendant | nés en 1945-1959 | 49,5 \[44,9 ; 54,2\] | 471 | pondéré |
| Francophones | 2018 | EEQ 2018 | Un pays indépendant | nés en 1960-1974 | 49,0 \[43,7 ; 54,3\] | 362 | pondéré |
| Francophones | 2018 | EEQ 2018 | Un pays indépendant | nés en 1975-1989 | 43,5 \[37,6 ; 49,6\] | 288 | pondéré |
| Francophones | 2018 | EEQ 2018 | Un pays indépendant | nés en 1990 ou après | 37,4 \[32,5 ; 42,5\] | 544 | pondéré |
| Francophones | 2022 | EEQ 2022 | Un pays indépendant | nés en 1944 ou avant | 41,2 \[26,8 ; 57,2\] | 51 | pondéré |
| Francophones | 2022 | EEQ 2022 | Un pays indépendant | nés en 1945-1959 | 54,2 \[47,5 ; 60,8\] | 248 | pondéré |
| Francophones | 2022 | EEQ 2022 | Un pays indépendant | nés en 1960-1974 | 44,4 \[37,8 ; 51,1\] | 243 | pondéré |
| Francophones | 2022 | EEQ 2022 | Un pays indépendant | nés en 1975-1989 | 38,1 \[31,8 ; 44,7\] | 245 | pondéré |
| Francophones | 2022 | EEQ 2022 | Un pays indépendant | nés en 1990 ou après | 30,5 \[24,2 ; 37,6\] | 215 | pondéré |

Les francophones nés en 1990 ou après sont passés de 53 % à 30 % de Oui
; ceux nés en 1945-1959 sont restés à 50-54 %Francophones qui voteraient
Oui à un pays indépendant, selon la cohorte de naissance, de 2012 à 2022
; deux cohortes avec bandes de confiance à 95 %, les autres en gris

À remarquer :

- La cohorte la plus jeune (née en 1990 ou après) passe de 53 % de Oui
  en 2012 à 30 % en 2022.
- La cohorte née de 1975 à 1989 passe de 54 % à 38 %. La cohorte la plus
  âgée (née en 1944 ou avant) ne change pas de façon détectable : 62
  francophones de cette cohorte ont répondu en 2012 et 51 en 2022, et la
  cohorte diminue aussi avec la mortalité.
- La cohorte née de 1945 à 1959, qui a voté aux référendums de 1980 et
  de 1995, est la plus souverainiste en 2022 (54 %).

## Le gradient s’inverse

![Graphiques à points avec intervalles de confiance en six panneaux, un
par étude de 2007 à 2022 : la part des francophones qui voteraient Oui
dans chaque cohorte de naissance. En 2007, la cohorte la plus jeune (née
en 1975-1989) est +20 pts au-dessus de celle née en 1944 ou avant ; en
2022, la plus jeune (née en 1990 ou après) est à −24 pts de celle née en
1945-1959. Valeurs dans la vue en
tableau.](fr-souverainete-generations_files/figure-html/gradient-light.png)![Graphiques
à points avec intervalles de confiance en six panneaux, un par étude de
2007 à 2022 : la part des francophones qui voteraient Oui dans chaque
cohorte de naissance. En 2007, la cohorte la plus jeune (née en
1975-1989) est +20 pts au-dessus de celle née en 1944 ou avant ; en
2022, la plus jeune (née en 1990 ou après) est à −24 pts de celle née en
1945-1959. Valeurs dans la vue en
tableau.](fr-souverainete-generations_files/figure-html/gradient-dark.png)

Source : qesR, variables regroupées sov_support, birth_year et
lang_mother ; une Étude électorale québécoise par élection, répondants
francophones. 2007 et 2008 ont posé la question de 1995, avec relance
des indécis ; 2012 à 2022 ont posé la question sur un pays indépendant :
comparer l'ordre des cohortes dans un panneau, pas les niveaux d'une
question à l'autre. Barres : intervalles de confiance à 95 % (logit).
Pondéré ; creux (2008) : non pondéré, pondération en révision. Les
cohortes de moins de 30 répondants ne sont pas tracées.

Vue en tableau

| Élection | Question | Cohorte | Oui, % \[IC à 95 %\] | n | Pondération |
|---:|:---|:---|:---|---:|:---|
| 2007 | La question de 1995 (partenariat) | nés en 1944 ou avant | 38,1 \[32,5 ; 44,0\] | 343 | pondéré |
| 2007 | La question de 1995 (partenariat) | nés en 1945-1959 | 51,2 \[46,4 ; 56,0\] | 588 | pondéré |
| 2007 | La question de 1995 (partenariat) | nés en 1960-1974 | 51,7 \[46,0 ; 57,3\] | 406 | pondéré |
| 2007 | La question de 1995 (partenariat) | nés en 1975-1989 | 57,8 \[51,8 ; 63,5\] | 379 | pondéré |
| 2008 | La question de 1995 (partenariat) | nés en 1944 ou avant | 45,4 \[38,2 ; 52,9\] | 174 | non pondéré (pondération en révision) |
| 2008 | La question de 1995 (partenariat) | nés en 1945-1959 | 62,1 \[55,5 ; 68,3\] | 219 | non pondéré (pondération en révision) |
| 2008 | La question de 1995 (partenariat) | nés en 1960-1974 | 50,5 \[44,7 ; 56,4\] | 275 | non pondéré (pondération en révision) |
| 2008 | La question de 1995 (partenariat) | nés en 1975-1989 | 52,7 \[45,5 ; 59,9\] | 182 | non pondéré (pondération en révision) |
| 2012 | Un pays indépendant | nés en 1944 ou avant | 37,0 \[25,6 ; 50,0\] | 62 | pondéré |
| 2012 | Un pays indépendant | nés en 1945-1959 | 53,6 \[46,4 ; 60,6\] | 214 | pondéré |
| 2012 | Un pays indépendant | nés en 1960-1974 | 46,2 \[40,6 ; 51,9\] | 327 | pondéré |
| 2012 | Un pays indépendant | nés en 1975-1989 | 54,0 \[48,8 ; 59,2\] | 374 | pondéré |
| 2012 | Un pays indépendant | nés en 1990 ou après | 53,5 \[44,1 ; 62,6\] | 120 | pondéré |
| 2014 | Un pays indépendant | nés en 1944 ou avant | 33,2 \[23,1 ; 45,2\] | 90 | pondéré |
| 2014 | Un pays indépendant | nés en 1945-1959 | 50,7 \[43,9 ; 57,5\] | 280 | pondéré |
| 2014 | Un pays indépendant | nés en 1960-1974 | 40,5 \[34,2 ; 47,2\] | 288 | pondéré |
| 2014 | Un pays indépendant | nés en 1975-1989 | 38,1 \[31,2 ; 45,6\] | 214 | pondéré |
| 2014 | Un pays indépendant | nés en 1990 ou après | 50,8 \[41,1 ; 60,4\] | 124 | pondéré |
| 2018 | Un pays indépendant | nés en 1944 ou avant | 38,2 \[32,2 ; 44,4\] | 280 | pondéré |
| 2018 | Un pays indépendant | nés en 1945-1959 | 49,5 \[44,9 ; 54,2\] | 471 | pondéré |
| 2018 | Un pays indépendant | nés en 1960-1974 | 49,0 \[43,7 ; 54,3\] | 362 | pondéré |
| 2018 | Un pays indépendant | nés en 1975-1989 | 43,5 \[37,6 ; 49,6\] | 288 | pondéré |
| 2018 | Un pays indépendant | nés en 1990 ou après | 37,4 \[32,5 ; 42,5\] | 544 | pondéré |
| 2022 | Un pays indépendant | nés en 1944 ou avant | 41,2 \[26,8 ; 57,2\] | 51 | pondéré |
| 2022 | Un pays indépendant | nés en 1945-1959 | 54,2 \[47,5 ; 60,8\] | 248 | pondéré |
| 2022 | Un pays indépendant | nés en 1960-1974 | 44,4 \[37,8 ; 51,1\] | 243 | pondéré |
| 2022 | Un pays indépendant | nés en 1975-1989 | 38,1 \[31,8 ; 44,7\] | 245 | pondéré |
| 2022 | Un pays indépendant | nés en 1990 ou après | 30,5 \[24,2 ; 37,6\] | 215 | pondéré |

En 2007, les plus jeunes francophones formaient la cohorte la plus
souverainiste ; en 2022, les plus jeunes étaient la moins
souverainisteFrancophones qui voteraient Oui, selon la cohorte de
naissance, dans chaque Étude électorale québécoise, avec intervalles de
confiance à 95 %

À remarquer :

- En 2007, sur la question de 1995, l’appui diminue avec l’âge : la
  cohorte née de 1975 à 1989 est +20 pts au-dessus de la plus âgée.
- En 2018, la cohorte la plus jeune et la plus âgée sont à égalité ; en
  2022, la plus jeune est la moins souverainiste, à −24 pts de la
  cohorte née de 1945 à 1959 (54 %, n = 248).
- Les intervalles de la cohorte la plus âgée (née en 1944 ou avant) sont
  les plus larges : moins de 100 francophones de cette cohorte ont
  répondu en 2012, 2014 et 2022.

## À propos des données

- **Études.** Toutes les études qui ont posé une question référendaire :
  les sondages de 1998, les Études électorales québécoises de 2007 à
  2022 et les panels Durand. Les sondages CROP n’en ont pas posé.
- **Variable.** `sov_support`, le vote référendaire regroupé, en part de
  Oui parmi celles et ceux qui voteraient Oui ou Non ; celles et ceux
  qui n’iraient pas voter ou annuleraient, ne savent pas ou refusent
  sont exclus. `sov_support__item` nomme la question de chaque étude, et
  `qes_spec("pooled")` l’ordre de priorité des libellés lorsqu’une étude
  en a posé plusieurs.
- **Libellés.** Un pays indépendant (2012 à 2022), un pays souverain
  (panel 2012), la question de 1995 sur la souveraineté assortie d’une
  offre de partenariat, avec relance des indécis (1998, 2007, panel
  2007, 2008), et favorable ou opposé à l’indépendance (panel 2018),
  regroupé en oui ou non et de niveau `approximate`.
- **Moment et population.** L’étude de 2022 (vague de campagne), les
  sondages de 1998 et les panels de 2007 et 2012 ont posé la question
  avant l’élection ; les autres études, après. Chacune est pondérée avec
  la pondération de la vague qui a posé la question. L’étude de 2018 a
  interrogé les personnes de 16 ans et plus.
- **Cohortes.** À partir de `birth_year`, demandée par les Études
  électorales québécoises de 2007 à 2022. Les francophones sont les
  répondants dont `lang_mother` est le français ; en 2022, celles et
  ceux qui ont coché deux langues sont exclus.
- **Pondérations.** La pondération recommandée de la vague qui a posé la
  question, par
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md).
  Les sondages de 1998, l’étude de 2008 et les panels Durand de 2007 et
  2012 sont non pondérés (pondérations en révision) et tracés en creux ;
  le panel 2018 utilise sa pondération révisée.
