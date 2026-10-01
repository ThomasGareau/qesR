# Démarrage avec qesR

*[English
version](https://thomasgareau.github.io/qesR/articles/get-started.md)*

qesR charge dans R les Études électorales québécoises et d’autres
enquêtes électorales québécoises à partir d’un code d’étude. Chaque code
d’étude lit le fichier de données original de l’étude, toujours la même
version, vérifié avant usage. Le catalogue des études, leurs documents,
leurs citations et la description de chaque variable de chaque étude
sont livrés avec le package.

Cette page va d’un code d’étude à une estimation pondérée. Tous ses
blocs de code s’exécutent sans connexion réseau : ils utilisent le
catalogue, les métadonnées livrées et `qes_demo`, une petite étude
synthétique fournie avec qesR. Les blocs qui téléchargeraient une vraie
étude sont montrés, mais pas exécutés.

## Installation

Installez qesR depuis GitHub :

``` r

# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

``` r

library(qesR)
```

## Quelles études sont offertes ?

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
énumère les études, avec leur année, leur famille, leur devis, leur
population cible, leur licence, leur DOI et la version de ses données
que qesR lit.

``` r

studies <- qes_studies()
studies[, c("study", "year", "family", "study_design", "licence")]
#>                 study year       family study_design      licence
#> 1             qes2022 2022          qes     pre_post CC BY-NC 4.0
#> 2             qes2018 2018          qes         post      CC0 1.0
#> 3       qes2018_panel 2018 durand_panel        panel      CC0 1.0
#> 4             qes2014 2014          qes         post      CC0 1.0
#> 5             qes2012 2012          qes         post      CC0 1.0
#> 6       qes2012_panel 2012 durand_panel        panel      CC0 1.0
#> 7  qes_crop_2007_2010 2007   crop_polls pooled_polls      CC0 1.0
#> 8             qes2008 2008          qes         post      CC0 1.0
#> 9             qes2007 2007          qes         post      CC0 1.0
#> 10      qes2007_panel 2007 durand_panel        panel      CC0 1.0
#> 11            qes1998 1998   polls_1998        panel      CC0 1.0
#> 12       qes1998_crop 1998   polls_1998        panel      CC0 1.0
#> 13    qes1998_createc 1998   polls_1998        panel      CC0 1.0
```

Toutes ne sont pas des Études électorales québécoises (famille `qes`) :
les panels Durand, les sondages CROP et les sondages de 1998 ont leurs
propres devis et populations.
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
énumère les livres de codes, questionnaires et rapports de chaque étude.

``` r

qes_docs("qes2014")[, c("role", "lang", "file_name")]
#>               role lang
#> 1    questionnaire   en
#> 2    questionnaire   fr
#> 3 technical_report   fr
#>                                                                 file_name
#> 1                                       Quebec Election Study 2014 EN.doc
#> 2                                       Quebec Election Study 2014 FR.doc
#> 3 Quebec Election Study 2014 - Technical Report - Post-Election Study.doc
```

## Charger une étude

[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
retourne les données. Elle n’écrit rien dans votre espace de travail :
assignez vous-même le résultat. L’étude de démonstration est lue dans le
package ; une vraie étude est téléchargée une fois par session (ou
gardée d’une session à l’autre avec `options(qesR.cache = "disk")`).

``` r

demo <- get_qes("qes_demo", quiet = TRUE)
#> get_qes() renvoie son résultat et ne l'assigne plus par défaut dans votre espace de travail. Écrivez `qes_demo <- get_qes(...)`, ou passez `assign_global = TRUE`. Cette note s'affiche une fois par session.
dim(demo)
#> [1] 60 11
head(demo[, c("QSEXE", "Q2", "Q3", "Q19")])
#>   QSEXE Q2 Q3 Q19
#> 1     2  1  3   2
#> 2     2  1  4   2
#> 3     1  1  1   1
#> 4     1  1  2   1
#> 5     1  1  4   2
#> 6     2  1  3   1
```

``` r

qes2014 <- get_qes("qes2014")
```

Les colonnes gardent les codes et les étiquettes du fichier (colonnes
`labelled` de haven). Les codes « ne sait pas » et « refus » restent des
valeurs tant que vous ne demandez pas à
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
de les remplacer par `NA` ; la fonction consigne ce qu’elle a changé.

``` r

demo_clean <- qes_missing(demo, quiet = TRUE)
attr(demo_clean, "qes_missing_log")
#>   variable value missing_type n_set
#> 1     QAGE  9999      refused     0
#> 2       Q2     9      refused     3
#> 3       Q3    99      refused     2
#> 4      Q19     8           dk     3
#> 5      Q19     9      refused     2
#> 6      Q28     8           dk     1
#> 7      Q28     9      refused     0
#> 8      Q32    98           dk     5
#> 9      Q32    99      refused     0
```

## Codebooks, questions et recherche, sans réseau

Le codebook de chaque étude est livré avec qesR : étiquettes des
variables, texte des questions en français et en anglais tiré des
questionnaires déposés (pour l’étude de 2022, de son livre de codes
bilingue), étiquettes de valeurs et codes manquants.

``` r

cb <- qes_codebook("qes2014")
head(cb[, c("variable", "label", "n_value_labels")])
#> <qes_codebook>
#> Variables: 6 
#> Codebook/support files: 0 
#>   variable                                    label n_value_labels
#> 1    QUEST                                     <NA>              0
#> 2     SDAT                          Date d'entrevue              0
#> 3     LANG Préfèreriez-vous répondre à ce questi...              2
#> 4    GREET Ce sondage en ligne est mené au nom d...              1
#> 5     QAGE En quelle année êtes-vous né(e)? / En...              1
#> 6    SMAGE                                      Age              0
qes_codebook("qes2014", layout = "long", variables = "Q19")
#> <qes_codebook> survey: qes2014
#> DOI: 10.5683/SP3/64F7WR 
#> Data file: Quebec Election Study 2014.sav (Dataverse: Quebec Election Study 2014 (SPSS).tab) 
#> Variables: 1 
#> Rows: 4 
#> Codebook/support files: 3 
#>   variable value                value_label
#> 1      Q19     1                        Oui
#> 2      Q19     2                        Non
#> 3      Q19     8             Je ne sais pas
#> 4      Q19     9 Je préfère ne pas répondre
#>                                      label
#> 1 Si un référendum sur l'indépendance a...
#> 2 Si un référendum sur l'indépendance a...
#> 3 Si un référendum sur l'indépendance a...
#> 4 Si un référendum sur l'indépendance a...
#>                                   question   study missing_type is_declared_na
#> 1 Si un référendum sur l’indépendance a... qes2014         <NA>          FALSE
#> 2 Si un référendum sur l’indépendance a... qes2014         <NA>          FALSE
#> 3 Si un référendum sur l’indépendance a... qes2014           dk          FALSE
#> 4 Si un référendum sur l’indépendance a... qes2014      refused          FALSE
```

[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
donne le libellé exact, dans l’une ou l’autre langue :

``` r

qes_question("qes2014", "Q19", lang = "en")$question
#> [1] "If there were a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO?"
qes_question("qes2014", "Q19", lang = "fr")$question
#> [1] "Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON?"
```

[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
cherche dans toutes les études à la fois, sans tenir compte de la casse
ni des accents :

``` r

hits <- qes_search("souverain|sovereign")
head(hits[, c("study", "variable", "label")])
#>     study            variable                                    label
#> 1 qes2022 cps_impissue_matrix What is the most important issue to y...
#> 2 qes2022             pes_q23 Did you vote in the 1995 referendum o...
#> 3 qes2018                  q2                                     <NA>
#> 4 qes2018         q2_96_other                                     <NA>
#> 5 qes2018                 q23                                     <NA>
#> 6 qes2014                  Q1 Parmi les enjeux suivants, lequel éta...
```

Le codebook de l’étude de 2022 est sous licence CC BY-NC 4.0
(attribution, pas d’usage commercial) ; voir
[`vignette("fr-citations", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-citations.md).

## Une estimation pondérée

Quelle part des adultes québécois aurait voté OUI à un référendum sur
l’indépendance, juste après l’élection de 2014 ? La question Q19 de
`qes2014` le demande (son libellé figure plus haut). Une estimation
correcte précise quatre choses, et les étapes précédentes donnent
chacune d’elles.

1.  **La population.** Le catalogue donne le devis et la population
    cible de chaque étude : `qes2014` est une enquête postélectorale qui
    vise la population adulte du Québec, et rien de plus large. Ses
    répondants viennent d’un panel en ligne à adhésion volontaire, et
    non d’un échantillon probabiliste.
2.  **La pondération.** `POND` est la pondération de `qes2014`. Son
    rapport technique (`qes_docs("qes2014")`) indique qu’elle ajuste
    l’échantillon au dernier recensement selon le sexe, l’âge, la région
    et la langue. L’estimation ne décrit la population adulte que dans
    la mesure où cet ajustement corrige le panel.

``` r

studies[studies$study == "qes2014",
        c("study", "study_design", "target_population_en", "target_population_fr")]
#>     study study_design    target_population_en        target_population_fr
#> 4 qes2014         post Quebec adult population Population adulte du Québec
qes_codebook("qes2014", variables = "POND")[, c("variable", "label")]
#> <qes_codebook>
#> Variables: 1 
#> Codebook/support files: 0 
#>   variable       label
#> 1     POND Pondération
```

3.  **La question et ses codes.** Q19 code OUI 1, NON 2, « je ne sais
    pas » 8 et « je préfère ne pas répondre » 9 (le codebook long plus
    haut).
4.  **Le dénominateur.**
    [`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
    a mis les codes 8 et 9 à `NA`. La part ci-dessous est calculée parmi
    les répondants qui ont répondu OUI ou NON, et elle le dit.

Sur l’étude de démonstration, qui reprend ces noms et ces codes :

``` r

answered <- demo_clean$Q19 %in% c(1, 2)
yes <- demo_clean$Q19[answered] == 1
weight <- demo_clean$POND[answered]
c(
  respondents = sum(answered),
  unweighted = mean(yes),
  weighted = sum(weight * yes) / sum(weight)
)
#> respondents  unweighted    weighted 
#>  55.0000000   0.4000000   0.4158442
```

Les données de démonstration sont synthétiques : ces nombres ne
décrivent aucune population, ils montrent le calcul. Sur le vrai
fichier, les mêmes lignes donnent 34,8 % de OUI parmi les 1 353
répondants qui ont répondu OUI ou NON (34,1 % sans pondération),
pondérés selon la population adulte du Québec de 2014.

``` r

qes2014 <- qes_missing(get_qes("qes2014"))
answered <- qes2014$Q19 %in% c(1, 2)
yes <- qes2014$Q19[answered] == 1
weight <- qes2014$POND[answered]
sum(weight * yes) / sum(weight)
```

C’est une estimation ponctuelle. `qes2014` est un panel en ligne non
probabiliste : son rapport technique (`qes_docs("qes2014")`) ne donne
pas de marge d’erreur, et une erreur type fondée sur le plan de sondage,
celle du package `survey` par exemple, n’aurait pas son sens habituel.
Calculez chaque estimation dans une seule étude : les études diffèrent
par leur devis, leur population et le libellé de leurs questions, et
leurs pondérations sont chacune sur leur propre échelle.

## Données harmonisées entre études

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
construit un seul tableau à partir de plusieurs études, une colonne par
variable harmonisée (« cible »), selon des règles livrées avec qesR.
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
montre quelle question de chaque étude alimente une cible et à quel
point elle est comparable à la question d’ancrage de la cible :

``` r

xw <- qes_spec("crosswalk", targets = "sov_indep", lang = params$lang)
xw[, c("study", "wave", "source_var", "grade", "weight_var")]
#>     study wave        source_var      grade         weight_var
#> 1 qes2012 post               q52  identical               pond
#> 2 qes2014 post               Q19  identical               POND
#> 3 qes2018 post               q26 comparable               pond
#> 4 qes2022  cps cps_qc_referendum comparable cps_weight_general
```

Certaines études n’ont pas encore de pondération utilisable ; leurs
colonnes de pondération valent `NA`, et
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
vous prévient quand il écarte des lignes. Sur l’étude de démonstration,
qui tient lieu de `qes2014` :

``` r

h <- qes_harmonize("qes_demo", targets = c("sov_indep", "vote_prov_recall"),
                   missing = "reasons", quiet = TRUE, lang = params$lang)
h[1:4, c("study", "eligible_voter", "sov_indep", "sov_indep__na", "weight_post")]
#>      study eligible_voter sov_indep sov_indep__na weight_post
#> 1 qes_demo           TRUE       Non          <NA>   1.2593217
#> 2 qes_demo           TRUE       Non          <NA>   0.6685168
#> 3 qes_demo           TRUE       Oui          <NA>   1.3786438
#> 4 qes_demo           TRUE       Oui          <NA>   0.9836078
attr(h, "qes_weight_guide")[, c("target", "study", "weight_column", "weight_var")]
#>             target    study weight_column weight_var
#> 1 vote_prov_recall qes_demo   weight_post       POND
#> 2        sov_indep qes_demo   weight_post       POND
```

Chaque valeur manquante a un motif (`sov_indep__na`). `eligible_voter`
indique si la personne pouvait voter à l’élection, et la pondération
recommandée de chaque vague a une moyenne de 1 : ces questions ont été
posées après l’élection, donc `weight_post` est leur pondération, comme
l’indique le guide des pondérations.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
transmet les données au package `survey` :

``` r

if (requireNamespace("survey", quietly = TRUE)) {
  d <- qes_design(h, weight = "weight_post")
  survey::svymean(~sov_indep, d, na.rm = TRUE)
}
#>                                            mean     SE
#> sov_indepOui                            0.41584 0.0718
#> sov_indepNon                            0.58416 0.0718
#> sov_indepN'irait pas voter / annulerait 0.00000 0.0000
```

L’erreur type traite l’échantillon pondéré comme un échantillon
probabiliste ; pour un panel à participation volontaire comme qes2014,
lisez-la seulement comme un ordre de grandeur (voir
[`?qes_design`](https://thomasgareau.github.io/qesR/reference/qes_design.md)).

Une question préélectorale (l’intention de vote) prend plutôt
`weight_pre`, et les deux vagues d’une étude peuvent avoir des
pondérations différentes :
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
demande de choisir quand les cibles en demandent de différentes. La
référence de chaque cible, générée à partir de la spécification, est
[`vignette("fr-reference-harmonisation", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md).

## Provenance et citations

Les données retournées par
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
indiquent de quel fichier elles viennent.
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
renvoie cette information, et
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
cite qesR et les jeux de données utilisés (voir
[`vignette("fr-citations", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-citations.md)).

``` r

qes_provenance(demo)
#> qes_demo : fichier 0 (qes_demo.sav), données synthétiques fournies avec qesR.
#> Somme md5 e956e315800690cb0894c86ed85c8bea, vérifiée. 60 lignes, 11 colonnes.
#> Obtenu le 2026-10-01 06:27:44 UTC (local_demo). Lu avec
#> haven::read_sav(user_na = TRUE), haven 2.5.5. Licence : CC0 1.0. Catalogue
#> qesR 2.4.1.
#> 
#> as.data.frame() donne toutes les colonnes.
qes_cite("qes2014")
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.9.0, https://github.com/ThomasGareau/qesR"                
#> [2] "Bélanger, Éric; Nadeau, Richard, 2023, \"Étude électorale québécoise 2014\", https://doi.org/10.5683/SP3/64F7WR, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw=="
```

[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
enregistre les fichiers originaux eux-mêmes (fichier de données et
documents), vérifiés, dans un dossier de votre choix :

``` r

dir <- file.path(tempdir(), "qes_originals")
dir.create(dir)
qes_download("qes2014", path = dir, what = c("data", "docs"))
```

## Messages et erreurs

Les messages, avertissements et erreurs s’affichent en français avec
`options(qesR.lang = "fr")` ; les données retournées sont les mêmes dans
les deux langues. Les erreurs ont des classes : un script peut y réagir
sans lire leur texte.

``` r

tryCatch(
  get_qes("QES 2014"),
  qesR_error_unknown_study = function(e) e$suggestions
)
#> [1] "qes2014"
```

## Du code écrit pour une version antérieure

Voir *Passer de qesR 0.4.4 à la version actuelle*,
[`vignette("fr-migrer-0.7", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md).
