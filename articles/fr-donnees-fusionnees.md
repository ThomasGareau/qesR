# Le fichier fusionné (get_qes_master)

*[English
version](https://thomasgareau.github.io/qesR/articles/merged-dataset.md)*

**Cette page télécharge 11 études** (environ 16 Mo) la première fois
qu’elle s’exécute. `options(qesR.cache = "disk")` les garde sur le
disque pour les sessions suivantes.

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
empile 11 études, de 1998 à 2022, dans un seul data.frame : une ligne
par répondant de chaque étude, et les mêmes 30 colonnes pour toutes les
études (choix de vote, participation, souveraineté, identification
partisane, idéologie, intérêt pour la politique et les variables
sociodémographiques habituelles, avec le code, l’année et les
identifiants de l’étude). Son format est fixe : les noms, l’ordre et les
types des 30 colonnes ne changent pas, et les colonnes ajoutées par la
suite viennent après elles. Le code écrit pour lui continue de
fonctionner.

## Quand l’utiliser, et quand préférer `qes_harmonize()`

Le fichier fusionné donne une vue d’ensemble rapide et à plat : un
appel, un tableau, une colonne par concept. Pour garder cette forme,
certaines colonnes réunissent des questions différentes, et aucune
colonne n’indique à quel point la réponse de chaque étude est
comparable.

Préférez
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
pour une analyse que vous publierez. Il garde séparé ce que le fichier
fusionné réunit (un vote déclaré et une intention de vote, les
formulations de la souveraineté, une échelle d’intérêt en quatre points
et une de 0 à 10), donne à la question de chaque étude un niveau de
comparabilité et à chaque valeur manquante un motif, et fournit la
pondération de chaque vague ;
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
en fait ensuite un plan de sondage. [Comment fonctionne
l’harmonisation](https://thomasgareau.github.io/qesR/articles/fr-harmonisation.md)
l’explique, et la colonne `target` de
`attr(master, "legacy_column_map")` nomme la variable harmonisée
derrière chaque colonne du fichier fusionné.

Utilisez
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
pour les questions d’une étude que ni l’un ni l’autre ne contient.

## Construire le fichier fusionné

L’étude de démonstration synthétique fonctionne hors ligne :

``` r

library(qesR)
demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
dim(demo_master)
#> [1] 60 42
head(demo_master[, c("qes_code", "age_group", "gender", "turnout", "vote_choice")])
#>   qes_code age_group gender turnout vote_choice
#> 1 qes_demo     55-64  Woman       1         CAQ
#> 2 qes_demo     55-64  Woman       1          QS
#> 3 qes_demo       65+    Man       1         PLQ
#> 4 qes_demo       65+    Man       1          PQ
#> 5 qes_demo     45-54    Man       1          QS
#> 6 qes_demo     35-44  Woman       1         CAQ
```

Les vraies études sont lues à partir de leurs fichiers originaux :

``` r

master <- get_qes_master(quiet = TRUE)
dim(master)
#> [1] 40987    42
table(master$qes_code)
#> 
#> qes_crop_2007_2010            qes1998            qes2007      qes2007_panel 
#>              24027               1483               2175               2442 
#>            qes2008            qes2012      qes2012_panel            qes2014 
#>               1151               1505                844               1517 
#>            qes2018      qes2018_panel            qes2022 
#>               3072               1250               1521
```

Par défaut, et avec `surveys = "all"`,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
construit les 11 études. Les fichiers des deux firmes de 1998,
`qes1998_crop` et `qes1998_createc`, n’en font pas partie (leurs
répondants sont déjà dans `qes1998`) ; lisez-les avec
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md).

## Ce que contient chaque colonne

- Chaque étude est harmonisée par
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  et chaque colonne est construite à partir des variables harmonisées («
  cibles ») dont elle a besoin. Un code que les règles d’harmonisation
  n’apparient pas vaut `NA`, jamais transmis tel quel.
  `attr(master, "source_map")` donne, pour chaque colonne de chaque
  étude, la question lue, sa cible et son niveau de comparabilité.
- Tous les répondants de chaque fichier sont conservés : ni
  dédoublonnage, ni retrait des lignes vides. Les répondants des panels
  et des enquêtes transversales restent des lignes distinctes.
- `vote_choice` et `turnout` sont le vote et la participation déclarés
  dans toutes les études ; les sondages CROP ne posaient que des
  intentions de vote (dans `vote_intent`). `sovereignty_support` est
  seulement le référendum sur un pays indépendant, et `party_best` et
  `party_lean` valent `NA` partout. `attr(master, "legacy_na_columns")`
  énumère chaque colonne et étude qui vaut `NA` d’un bout à l’autre,
  avec la raison et, dans `basis`, l’explication en mots (en anglais ;
  [`?get_qes_master`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  énumère les causes).
- `vote_choice_timing` et `sovereignty_item` indiquent ce que
  contiennent `vote_choice` et `sovereignty_support` dans chaque étude.
- Les variables qui portent le même nom d’une étude à l’autre ne sont
  pas empilées : elles contiennent souvent des questions différentes.
  Lisez ces questions dans chaque étude avec
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md).

``` r

head(attr(demo_master, "source_map")[, c("qes_code", "harmonized_variable", "source_variable", "target", "grade")])
#>   qes_code harmonized_variable source_variable target grade
#> 1 qes_demo            qes_code            <NA>   <NA>  <NA>
#> 2 qes_demo            qes_year            <NA>   <NA>  <NA>
#> 3 qes_demo         qes_name_en            <NA>   <NA>  <NA>
#> 4 qes_demo       respondent_id            <NA>   <NA>  <NA>
#> 5 qes_demo     interview_start            <NA>   <NA>  <NA>
#> 6 qes_demo       interview_end            <NA>   <NA>  <NA>
attr(demo_master, "legacy_na_columns")[, c("column", "study", "reason", "cause")]
#>                  column    study      reason              cause
#> 1       interview_start qes_demo   no_source               <NA>
#> 2         interview_end qes_demo   na_column               <NA>
#> 3    interview_recorded qes_demo   na_column               <NA>
#> 4              language qes_demo   no_source               <NA>
#> 5           citizenship qes_demo   no_source               <NA>
#> 6             education qes_demo   no_source               <NA>
#> 7                income qes_demo   no_source               <NA>
#> 8              religion qes_demo   no_source               <NA>
#> 9           born_canada qes_demo   no_source               <NA>
#> 10     vote_choice_text qes_demo   na_column not_harmonized_yet
#> 11           party_best qes_demo   na_column    no_valid_source
#> 12           party_lean qes_demo   na_column    no_valid_source
#> 13          federal_pid qes_demo   no_source               <NA>
#> 14       provincial_pid qes_demo   no_source               <NA>
#> 15            subsample qes_demo all_missing               <NA>
#> 16           weight_pre qes_demo   no_source               <NA>
#> 17          vote_intent qes_demo   no_source               <NA>
#> 18       turnout_intent qes_demo   no_source               <NA>
#> 19 sov_partnership_1995 qes_demo   no_source               <NA>
```

`attr(master, "legacy_column_map")` décrit chaque colonne (en anglais)
et signale celles qui mêlent des instruments (`political_interest`,
`age_group`, `education`).

## Pondérations

`survey_weight` est la pondération propre à chaque étude, sur sa propre
échelle, et n’est pas révisée : ne combinez pas d’estimations pondérées
entre études avec elle. `weight_pre` et `weight_post` sont les
pondérations recommandées des règles d’harmonisation ; elles valent `NA`
là où une étude n’a pas de pondération validée (le [tableau des
pondérations](https://thomasgareau.github.io/qesR/articles/fr-etudes.html#ponderations)
décrit la pondération de chaque étude). Calculez chaque estimation à
l’intérieur d’une seule étude.

## Exporter le fichier fusionné

`save_path` écrit un fichier CSV en UTF-8 ou un fichier RDS, et à côté
`<racine>_provenance.csv`, le relevé du fichier lu pour chaque étude.

``` r

get_qes_master(save_path = file.path(tempdir(), "qes_master.csv"), strict = FALSE)
get_qes_master(save_path = file.path(tempdir(), "qes_master.rds"), strict = FALSE)
```

Pour du code écrit avec une version antérieure de qesR, et pour en
reproduire les résultats, voir [le guide de mise à
niveau](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md).
