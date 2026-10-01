# qesR en français

*[English version](https://thomasgareau.github.io/qesR/index.md)*

Les Études électorales québécoises, avec les panels et les sondages qui
les ont accompagnées, couvrent sept élections provinciales, de 1998 à
2022, et leurs données sont publiques. Les utiliser ensemble est
toutefois une autre affaire. Elles se trouvent dans des dépôts
distincts, dans des fichiers SPSS ou Stata, chacune avec son propre
codebook, et le libellé d’une même question a souvent changé d’une étude
à l’autre. qesR les réunit dans R. Chaque étude se charge par son code,
à partir de son fichier original, vérifié avant usage ; son codebook, le
libellé de ses questions et une recherche dans toutes les études
fonctionnent sans réseau, en français et en anglais. qesR harmonise
aussi les 11 études de 1998 à 2022 en un seul tableau, question par
question, avec un niveau de comparabilité pour la question de chaque
étude et un motif pour chaque valeur manquante. Autrement dit, une
question sur 25 ans d’élections québécoises peut être examinée dans
chaque étude, puis comparée d’une étude à l’autre, sans recoder chaque
fichier à la main.

[Démarrage](https://thomasgareau.github.io/qesR/articles/fr-demarrage.md)
mène d’un code d’étude à une estimation pondérée. La page s’exécute sans
réseau, sur une petite étude synthétique livrée avec qesR.

## Ce que montrent les études

Deux résultats tirés des exemples. Chaque estimation est faite dans une
seule étude, avec la pondération de cette étude lorsqu’elle en a une et
un intervalle de confiance à 95 % ; rien n’est regroupé d’une étude à
l’autre.

[![Graphique linéaire du nombre effectif de partis à chaque élection
québécoise de 1998 à 2022 : le résultat officiel, les répondants
francophones et les répondants non francophones. Chez les francophones,
il passe de 2,8 en 1998 à 3,9 en 2022 ; chez les non-francophones, il
reste entre 1,3 et 2,5 jusqu'en 2018 et atteint 2,9 en 2022, avec un
intervalle large. Détails sur la page du
réalignement.](fr-realignement_files/figure-html/enp-light.png)![Graphique
linéaire du nombre effectif de partis à chaque élection québécoise de
1998 à 2022 : le résultat officiel, les répondants francophones et les
répondants non francophones. Chez les francophones, il passe de 2,8 en
1998 à 3,9 en 2022 ; chez les non-francophones, il reste entre 1,3 et
2,5 jusqu'en 2018 et atteint 2,9 en 2022, avec un intervalle large.
Détails sur la page du
réalignement.](fr-realignement_files/figure-html/enp-dark.png)](https://thomasgareau.github.io/qesR/articles/fr-realignement.md)

Le vote francophone s'est fragmenté, de 2,8 à 3,9 partis effectifs ; le
vote non francophone est resté beaucoup plus concentré

[![Graphique linéaire de la part des francophones qui voteraient Oui à
ce que le Québec devienne un pays indépendant, pour cinq cohortes de
naissance, aux élections de 2012, 2014, 2018 et 2022. Les personnes nées
en 1990 ou après passent de 53 % à 30 %, le niveau le plus bas ; celles
nées en 1945-1959 restent entre 50 % et 54 %. Détails sur la page de la
souveraineté.](fr-souverainete-generations_files/figure-html/cohorts-light.png)![Graphique
linéaire de la part des francophones qui voteraient Oui à ce que le
Québec devienne un pays indépendant, pour cinq cohortes de naissance,
aux élections de 2012, 2014, 2018 et 2022. Les personnes nées en 1990 ou
après passent de 53 % à 30 %, le niveau le plus bas ; celles nées en
1945-1959 restent entre 50 % et 54 %. Détails sur la page de la
souveraineté.](fr-souverainete-generations_files/figure-html/cohorts-dark.png)](https://thomasgareau.github.io/qesR/articles/fr-souverainete-generations.md)

Les francophones nés en 1990 ou après sont passés de 53 % à 30 % de
Oui ; ceux nés de 1945 à 1959 sont restés entre 50 et 54 %

## Exemples

Chaque page confronte une croyance répandue sur les élections
québécoises aux données des études.

- [De deux partis à
  quatre](https://thomasgareau.github.io/qesR/articles/fr-realignement.md) :
  le vote s’est fragmenté de 1998 à 2022, mais surtout chez les
  francophones, et à l’intérieur de chaque camp sur la souveraineté
  plutôt qu’entre eux.
- [Les jeunes sont-ils encore les plus
  souverainistes ?](https://thomasgareau.github.io/qesR/articles/fr-souverainete-generations.md) :
  les plus jeunes francophones étaient les plus enclins à voter Oui en
  2007 et les moins enclins en 2022, surtout à la suite d’un changement
  à l’intérieur des cohortes.
- [Deux dimensions : souveraineté et
  gauche-droite](https://thomasgareau.github.io/qesR/articles/fr-dimensions.md) :
  la question nationale structure moins le vote qu’en 2012, et l’axe
  gauche-droite n’a pas pris sa place.
- [Qui
  vote ?](https://thomasgareau.github.io/qesR/articles/fr-participation.md) :
  les jeunes déclarent voter moins que leurs aînés à chaque élection
  depuis 1998, et l’intérêt pour la politique n’explique qu’une petite
  part de l’écart.
- [Les élections québécoises se jouent-elles pendant la
  campagne ?](https://thomasgareau.github.io/qesR/articles/fr-transitions.md) :
  les mêmes répondants, interrogés avant et après le vote, montrent
  qu’une campagne déplace beaucoup d’électeurs, mais, une fois les gains
  et les pertes additionnés, peu de votes.
- [Les enquêtes sous-estiment-elles les
  libéraux ?](https://thomasgareau.github.io/qesR/articles/fr-enquetes-resultats.md) :
  le vote libéral a été sous-déclaré de 2007 à 2014, mais l’écart s’est
  estompé en 2018 et en 2022, et le gagnant est plus souvent sous-estimé
  que surestimé.
- [Recettes](https://thomasgareau.github.io/qesR/articles/fr-recettes.md) :
  le code, pas à pas, pour savoir si le clivage linguistique du vote
  libéral s’est resserré à mesure que la question de la souveraineté
  reculait.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

Une fois qesR accepté sur le CRAN, `install.packages("qesR")` fera de
même. Une première session :

``` r

library(qesR)
options(qesR.lang = "fr")             # messages et erreurs en français
qes_studies()                         # les études, sans réseau
qes2018 <- get_qes("qes2018")         # une étude, à partir de son fichier original
qes_search("souverain|sovereign")     # une question, dans toutes les études
h <- qes_harmonize(targets = "vote_choice")  # le vote, dans six études
d <- qes_decon(lang = "fr")           # toutes les études en un seul tableau, souple
```

## Citer qesR

Un travail qui utilise qesR devrait citer le package et chaque étude
dont il se sert.
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
écrit les deux, avec le DOI de chaque jeu de données :

``` r

qes_cite("qes2018", lang = "fr")      # qesR et l'étude de 2018
qes_cite("qes2018", style = "bibtex")
```

[Citer qesR et les
études](https://thomasgareau.github.io/qesR/articles/fr-citations.md)
donne la citation de chaque étude.

## Licence

Le code de qesR est sous licence MIT. Les données ne font pas partie du
package : qesR télécharge chaque étude depuis son dépôt sur Borealis ou
le Harvard Dataverse. La plupart des études sont diffusées sous CC0 ;
l’étude de 2022, et les métadonnées que qesR en livre, sont sous CC
BY-NC 4.0 (attribution, pas d’usage commercial). [Détails et attribution
requise](https://thomasgareau.github.io/qesR/articles/fr-citations.html#licences-et-attribution).

## Aussi sur ce site

Le [catalogue des
études](https://thomasgareau.github.io/qesR/articles/fr-etudes.md),
[comment fonctionne
l’harmonisation](https://thomasgareau.github.io/qesR/articles/fr-harmonisation.md),
[un seul fichier pour toutes les études avec
`qes_decon()`](https://thomasgareau.github.io/qesR/articles/fr-decon.md),
la [couverture de chaque
étude](https://thomasgareau.github.io/qesR/articles/fr-couverture.md),
la [référence des
variables](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md),
la [validation par les résultats
officiels](https://thomasgareau.github.io/qesR/articles/fr-validation.md),
[le fichier
fusionné](https://thomasgareau.github.io/qesR/articles/fr-donnees-fusionnees.md)
et l’[aperçu des fonctions en
français](https://thomasgareau.github.io/qesR/reference/qesR-fr.md).
Pour du code écrit avec une version antérieure de qesR : [passer à la
version
actuelle](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md).
