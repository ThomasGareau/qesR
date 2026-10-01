# Couverture par étude

*[English
version](https://thomasgareau.github.io/qesR/articles/coverage.md)*

Quelles variables harmonisées (« cibles ») de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
chaque étude possède, et à quel point sa question est comparable. La
grille et le tableau des études ci-dessous sont générés à partir des
règles livrées avec qesR.

Chaque cellule donne le niveau de comparabilité de la question de
l’étude pour la cible, par rapport à la question d’ancrage de la cible ;
un tiret signifie que l’étude n’a pas de question pour la cible. Le nom
d’une cible mène à sa section de la [référence des
variables](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md),
qui donne la question, son libellé, les niveaux offerts et la raison de
son niveau.
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
renvoie la même grille sous forme de tableau.

## Cibles par étude

[TABLE]

Pour une étude de plusieurs vagues, la vague qui a posé la question est
entre parenthèses.

\* La question de l’étude n’offrait pas tous les niveaux de la cible :
les niveaux non offerts sont des zéros structurels, listés dans la
référence.

† En attente d’approbation : appliquée seulement si vous la demandez
(`qes_harmonize(include_draft = TRUE)`).

## Variables regroupées par étude

[TABLE]

Chaque cellule donne le type de membre dont une variable regroupée tire
les valeurs d’une étude en disposition par répondant, et son niveau ; un
tiret signifie qu’aucun membre de ses types par défaut n’a de question
dans l’étude.

## Études

| Étude | Vagues et pondérations recommandées | Cibles | Identique | Comparable | Approximatif |
|----|----|----|----|----|----|
| `qes2022` | cps (n = 1 521) : `cps_weight_general` ; pes (n = 1 220) : `pes_weight_general` | 35 | 7 | 14 | 14 |
| `qes2018` | post (n = 3 072) : `pond` | 29 | 3 | 19 | 7 |
| `qes2018_panel` | pre (n = 1 250) : `weight` ; post (n = 842) : `weight_rts` | 13 | 3 | 3 | 7 |
| `qes2014` | post (n = 1 517) : `POND` | 30 | 9 | 20 | 1 |
| `qes2012` | post (n = 1 505) : `pond` | 31 | 27 | 4 | 0 |
| `qes2012_panel` | pre (n = 844) : `pondam1` (pas encore utilisable) ; post (n = 844) : `pond_post` (pas encore utilisable) | 10 | 1 | 6 | 3 |
| `qes_crop_2007_2010` | 24 vagues de sondage, de poll_2007_06 à poll_2010_01 (n = 1 000 à 1 004 chacune) : `XPOND` (pas encore utilisable) | 10 | 1 | 7 | 2 |
| `qes2008` | post (n = 1 151) : aucune pondération recommandée | 27 | 0 | 25 | 2 |
| `qes2007` | post (n = 2 175) : `pond` | 25 | 6 | 16 | 3 |
| `qes2007_panel` | pre (n = 2 050) : `pondam1` (pas encore utilisable) ; post (n = 2 054) : `pond_tot_am1` (pas encore utilisable) | 15 | 3 | 8 | 4 |
| `qes1998` | pre (n = 1 483) : `ponder3` (pas encore utilisable) ; post (n = 1 483) : `ponder3` (pas encore utilisable) | 8 | 0 | 7 | 1 |

`n` est le nombre de répondants de chaque vague. Une pondération marquée
*pas encore utilisable* n’est pas assez documentée pour être utilisée :
ses colonnes de pondération valent `NA`.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
utilise la pondération de la vague d’où vient chaque cible.

Non harmonisées : `qes1998_crop`, `qes1998_createc` (leurs répondants
sont dans `qes1998`).
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
les lit.

## La même grille dans R

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
renvoie la grille sous forme de tableau, une ligne par cible et une
colonne par étude, chaque cellule donnant le niveau de comparabilité de
la question de cette étude :

``` r

library(qesR)
qes_spec(lang = params$lang)
```

Un niveau compare la question d’une étude à la question d’ancrage de la
cible. [Comment fonctionne
l’harmonisation](https://thomasgareau.github.io/qesR/articles/fr-harmonisation.md)
montre comment les niveaux, les valeurs manquantes et les pondérations
se rendent jusqu’à une estimation.
