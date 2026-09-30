# Le fichier fusionné hérité

*[English
version](https://thomasgareau.github.io/qesR/articles/merged-dataset.md)*

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
empile les Études électorales québécoises de qesR 0.4.4 dans un seul
data.frame, avec les 30 colonnes harmonisées de 0.4.4 : mêmes noms, même
ordre, mêmes types. C’est le format hérité, figé et stable pour le code
écrit pour 0.4.4.

``` r

library(qesR)
qes_studies()$study
#>  [1] "qes2022"            "qes2018"            "qes2018_panel"     
#>  [4] "qes2014"            "qes2012"            "qes2012_panel"     
#>  [7] "qes_crop_2007_2010" "qes2008"            "qes2007"           
#> [10] "qes2007_panel"      "qes1998"            "qes1998_crop"      
#> [13] "qes1998_createc"
```

Par défaut, et avec `surveys = "all"`,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
construit les 11 études de qesR 0.4.4. Les fichiers des deux firmes de
1998, `qes1998_crop` et `qes1998_createc`, ne font pas partie du fichier
fusionné (leurs répondants sont déjà dans `qes1998`) ; lisez-les avec
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
comme l’explique
[`?get_qes_master`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md).

## Construire la base fusionnée

L’étude de démonstration synthétique fonctionne hors ligne :

``` r

demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
#> Les valeurs ont changé dans qesR 0.7.0 : get_qes_master() est maintenant produit par le moteur d'harmonisation (qes_harmonize()), de sorte que vote_choice et turnout sont le vote et la participation déclarés dans toutes les études qui les ont demandés (les sondages CROP n'ont demandé que l'intention : vote_intent), et que les codes que la spécification n'apparie pas valent NA (language, la langue maternelle, vaut NA pour les personnes qui ont donné deux premières langues) ; attr(, "legacy_column_map") décrit chaque colonne et NEWS énumère les changements. Les résultats d'une version antérieure se reproduisent en l'installant (pour 0.4.4 : remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")). Cette note s'affiche une fois par session.
#> get_qes_master() n'ajoute plus les 70 colonnes que qesR 0.4.4 construisait en empilant des variables de même nom d'une étude à l'autre ; attr(, "removed_columns") les énumère. Lisez ces questions dans chaque étude avec get_qes(). Cette note s'affiche une fois par session.
#> get_qes_master() renvoie son résultat et ne l'assigne plus par défaut dans votre espace de travail. Écrivez `qes_master <- get_qes_master(...)`, ou passez `assign_global = TRUE`. Cette note s'affiche une fois par session.
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

Les vraies études sont lues à partir de leurs fichiers originaux
retenus. Ce bloc s’exécute à la construction du site, par le cache de
téléchargement de qesR :

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

## Construction

- Depuis qesR 0.7.0, le fichier fusionné est produit par le moteur
  d’harmonisation : chaque étude est harmonisée par
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
  et chaque colonne est rendue à partir des cibles dont elle a besoin,
  selon la table de rendu de la spécification
  (`qes_spec("spec")$tables$legacy`). Un code que la spécification
  n’apparie pas vaut `NA`, jamais transmis tel quel.
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
  avec la raison, la règle en cause s’il y en a une (`cause` :
  `reported_vote_only`, `independence_question_only`, `no_valid_source`,
  `not_harmonized_yet` ou `legacy_frozen` : la question de l’étude est
  harmonisée depuis la spécification 4.3.0, dans
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  mais la colonne garde le `NA` de qesR 0.7.1) et, dans `basis`,
  l’explication en mots (en anglais).
- `vote_choice_timing` et `sovereignty_item` indiquent ce que
  contiennent `vote_choice` et `sovereignty_support` dans chaque étude.
- qesR 0.4.4 ajoutait aussi 70 colonnes en empilant des variables de
  même nom d’une étude à l’autre ; elles mêlaient des questions
  différentes et ne sont plus construites
  (`attr(master, "removed_columns")`). Lisez ces questions dans chaque
  étude avec
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
`age_group`, `education`). `survey_weight` est la pondération propre à
chaque étude, sur sa propre échelle : ne combinez pas d’estimations
pondérées entre études sans la remettre à l’échelle.

Les résultats de qesR 0.4.4 ne se reproduisent qu’avec cette version
(`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`). 0.5.0
et 0.6.0 étaient des versions de développement, jamais publiées : un
résultat calculé avec l’une d’elles se reproduit en installant le commit
dont il provient, que `packageDescription("qesR")$RemoteSha` enregistre
pour une installation depuis GitHub
(`remotes::install_github("ThomasGareau/qesR", ref = "<sha>")`).

## Exporter

`save_path` écrit un fichier CSV en UTF-8 ou un fichier RDS, et à côté
`<racine>_provenance.csv`, le relevé du fichier lu pour chaque étude.

``` r

get_qes_master(save_path = file.path(tempdir(), "qes_master.csv"), strict = FALSE)
get_qes_master(save_path = file.path(tempdir(), "qes_master.rds"), strict = FALSE)
```
