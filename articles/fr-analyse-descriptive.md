# Analyse : statistiques descriptives

## Chargement des données fusionnées

## Taille des échantillons

| qes_year  | qes_code           | qes_name_en                           |     n |
|:----------|:-------------------|:--------------------------------------|------:|
| 1998      | qes1998            | Quebec Elections 1998                 |  1483 |
| 2007      | qes2007            | Quebec Election Study 2007            |  2175 |
| 2007      | qes2007_panel      | Quebec Election Study 2007 Panel      |  2062 |
| 2007-2010 | qes_crop_2007_2010 | CROP Quebec Opinion Polls (2007-2010) | 24026 |
| 2008      | qes2008            | Quebec Election Study 2008            |  1151 |
| 2012      | qes2012            | Quebec Election Study 2012            |  1505 |
| 2012      | qes2012_panel      | Quebec Election Study 2012 Panel      |   844 |
| 2014      | qes2014            | Quebec Election Study 2014            |  1517 |
| 2018      | qes2018            | Quebec Election Study 2018            |  3072 |
| 2018      | qes2018_panel      | Quebec Election Study 2018 Panel      |  1250 |
| 2022      | qes2022            | Quebec Election Study 2022            |  1521 |

Taille d’échantillon par étude

## Répartition des groupes d’âge

| annee | age_group            |   n |  pct |
|------:|:---------------------|----:|-----:|
|  1998 | 65 ANS ET PLUS       | 234 | 15.8 |
|  1998 | DE 18 A 24 ANS       | 128 |  8.6 |
|  1998 | DE 25 A 34 ANS       | 247 | 16.7 |
|  1998 | DE 35 A 44 ANS       | 395 | 26.6 |
|  1998 | DE 45 A 54 ANS       | 299 | 20.2 |
|  1998 | DE 55 A 64 ANS       | 179 | 12.1 |
|  1998 | REFUS/PAS DE REPONSE |   1 |  0.1 |
|  2007 | 18-24                | 159 |  3.8 |
|  2007 | 18-34 ans            | 439 | 10.5 |
|  2007 | 25-34                | 403 |  9.6 |
|  2007 | 35-44                | 320 |  7.6 |
|  2007 | 35-54 ans            | 914 | 21.8 |
|  2007 | 45-54                | 422 | 10.1 |
|  2007 | 55 ans et plus       | 709 | 16.9 |
|  2007 | 55-64                | 517 | 12.3 |
|  2007 | 65+                  | 314 |  7.5 |
|  2008 | 18-24                |  90 |  8.0 |
|  2008 | 25-34                | 171 | 15.2 |
|  2008 | 35-44                | 239 | 21.2 |
|  2008 | 45-54                | 238 | 21.2 |
|  2008 | 55-64                | 170 | 15.1 |
|  2008 | 65+                  | 217 | 19.3 |
|  2012 | 18-24                | 240 | 10.2 |
|  2012 | 18-34                | 127 |  5.4 |
|  2012 | 25-34                | 305 | 13.0 |
|  2012 | 35-44                | 328 | 14.0 |
|  2012 | 35-54                | 339 | 14.4 |
|  2012 | 45-54                | 315 | 13.4 |

Répartition des groupes d’âge (premières lignes)

![Carte thermique de la répartition des groupes d'âge selon
l'année.](fr-analyse-descriptive_files/figure-html/unnamed-chunk-6-1.png)

## Participation électorale par année

| Année | Répondants | N avec mesure |      Taux |        SE |    IC bas |   IC haut | Taux (%) | IC bas (%) | IC haut (%) |
|------:|-----------:|--------------:|----------:|----------:|----------:|----------:|---------:|-----------:|------------:|
|  1998 |       1483 |          1374 | 0.8704512 | 0.0090593 | 0.8526950 | 0.8882075 |     87.0 |       85.3 |        88.8 |
|  2007 |       4237 |          3897 | 0.8894021 | 0.0050241 | 0.8795549 | 0.8992493 |     88.9 |       88.0 |        89.9 |
|  2008 |       1151 |          1131 | 0.8717949 | 0.0099410 | 0.8523106 | 0.8912792 |     87.2 |       85.2 |        89.1 |
|  2012 |       2349 |          2330 | 0.9214592 | 0.0055732 | 0.9105357 | 0.9323828 |     92.1 |       91.1 |        93.2 |
|  2014 |       1517 |          1499 | 0.9019346 | 0.0076815 | 0.8868789 | 0.9169903 |     90.2 |       88.7 |        91.7 |
|  2018 |       4322 |          3481 | 0.8440103 | 0.0061499 | 0.8319565 | 0.8560642 |     84.4 |       83.2 |        85.6 |
|  2022 |       1521 |          1322 | 0.9803328 | 0.0038189 | 0.9728477 | 0.9878179 |     98.0 |       97.3 |        98.8 |

Indicateur de participation par année

![Graphique de l'indicateur de participation électorale selon
l'année.](fr-analyse-descriptive_files/figure-html/unnamed-chunk-8-1.png)

## Notes

- `qes_crop_2007_2010` est exclu des tableaux et graphiques par année
  parce que ce fichier ne contient pas d’année au niveau des répondants.
