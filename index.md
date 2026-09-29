# qesR

qesR loads the Quebec Election Studies and related Quebec election
surveys into R by study code, each from its original data file, pinned
and checked by md5. It also harmonizes the 11 studies of 1998 to 2022
into one data frame, question by question, with a comparability grade
for every study’s question and a reason for every missing value.
Codebooks, question wording and search work offline, in English and
French.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")

# once accepted on CRAN:
# install.packages("qesR")
```

## One data frame across 11 studies (experimental)

``` r

library(qesR)

qes_spec()                        # which study has which variable, and its grade
h <- qes_harmonize(studies = c("qes2012", "qes2014", "qes2018", "qes2022"),
                   targets = c("vote_prov_recall", "turnout_prov_recall"),
                   min_grade = "comparable", missing = "reasons")
# both targets were asked after the election: the post-election weight
d <- qes_design(h, weight = "weight_post")   # for the survey package
```

`qes2007`, `qes2012`, `qes2014`, `qes2018`, `qes2022` and the 2018 panel
`qes2018_panel` have reviewed weights so far. The other studies (the
Durand panels `qes2007_panel` and `qes2012_panel`, `qes2008`, whose two
weights are calibrated on the vote or on turnout, the CROP polls and the
1998 polls) have `NA` weights, and
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
leaves out every row without the chosen weight; `studies = NULL` means
the six Quebec Election Studies. Estimate within one study before
comparing studies.

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
reads each study from its pinned file and maps each study’s question to
a harmonized variable (“target”) following a specification that ships
with qesR:

- **One question stimulus per target.** A reported vote and a vote
  intention are different targets, and so are the sovereignty questions
  with different wordings; nothing is pooled across wordings behind your
  back.
- **A grade for every study’s question**: identical, comparable or
  approximate, with the reason, in English and French.
  `min_grade = "comparable"` drops the approximate ones. A party a study
  did not list is a structural zero, not 0% support.
- **A reason for every missing value** (`dk`, `refused`, `not_voted`,
  `not_in_wave`, …), and each respondent’s waves, eligibility and
  weights;
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
  hands the result to `survey` or `srvyr`.
- **Checked against what is known.** The harmonized studies are compared
  with the official results of Élections Québec and the census margins
  of Statistics Canada: see [Validation against official
  results](https://thomasgareau.github.io/qesR/articles/validation.md).

Every row of the specification is signed off, after an automated double
review against the original files and documents (not a human review),
and is applied by default; the weights that still need review
(`qes1998`, `qes2007_panel`, `qes2012_panel` and the CROP polls) are
`NA` until they are reviewed, but they do not hold back the answers. The
grade of each study’s question for each target (I identical, C
comparable, A approximate; a dash: no question in the specification);
each target links to its questions, wordings and grades in the
[harmonization
reference](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md):

|  | `qes2022` | `qes2018` | `qes2018_panel` | `qes2014` | `qes2012` | `qes2012_panel` | `qes_crop_2007_2010` | `qes2008` | `qes2007` | `qes2007_panel` | `qes1998` |
|----|----|----|----|----|----|----|----|----|----|----|----|
| [`survey_mode`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-survey_mode) | — | — | I | — | — | — | — | — | I | — | — |
| [`vote_prov_recall`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-vote_prov_recall) | C | C | C | C | I | A | — | C | C | A | C |
| [`vote_prov_intent`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-vote_prov_intent) | A | — | A | — | — | A | C | — | — | I | — |
| [`vote_prov_intent_push`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-vote_prov_intent_push) | — | — | A | — | — | A | C | — | — | I | A |
| [`turnout_prov_recall`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-turnout_prov_recall) | A | A | A | C | I | C | — | C | C | C | C |
| [`turnout_prov_likely`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-turnout_prov_likely) | I | — | — | — | — | — | — | — | — | — | — |
| [`vote_prov_intent_other`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-vote_prov_intent_other) | I | — | — | — | — | — | — | — | — | — | — |
| [`pid_prov`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-pid_prov) | C | C | — | I | I | — | — | C | C | — | — |
| [`pid_fed`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-pid_fed) | I | — | — | — | — | — | — | — | — | — | — |
| [`sov_indep`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-sov_indep) | C | C | — | I | I | — | — | — | — | — | — |
| [`sov_sovereign_country`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-sov_sovereign_country) | — | — | — | — | — | I | — | — | — | — | — |
| [`sov_favour`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-sov_favour) | — | — | I | — | — | — | — | — | — | — | — |
| [`lr_self`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-lr_self) | A | C | A | I | I | — | — | — | — | — | — |
| [`interest_4pt`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-interest_4pt) | — | C | — | C | I | — | — | — | — | — | — |
| [`sov_partnership_1995`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-sov_partnership_1995) | — | — | — | — | — | — | — | C | I | C | C |
| [`interest_0_10`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-interest_0_10) | A | — | — | — | — | — | — | — | I | — | — |
| [`interest_election_0_10`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-interest_election_0_10) | — | — | — | — | — | — | — | C | I | — | — |
| [`interest_campaign_4pt`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-interest_campaign_4pt) | — | — | — | — | — | — | — | — | — | I | — |
| [`birth_year`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-birth_year) | I | C | — | C | C | — | — | C | C | — | — |
| [`birth_month`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-birth_month) | — | I | — | — | — | — | — | — | — | — | — |
| [`age`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-age) | I | A | — | — | — | — | — | — | — | — | — |
| [`age_group3`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-age_group3) | — | — | I | — | — | C | C | C | — | C | C |
| [`citizen`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-citizen) | I | — | — | — | — | — | — | — | — | — | — |
| [`age_group6`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-age_group6) | — | — | — | — | — | C | I | C | — | C | C |
| [`gender`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-gender) | C | C | C | C | I | C | C | C | C | C | C |
| [`education4`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-education4) | C | C | A | I | C | — | A | C | C | A | — |
| [`lang_mother`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-lang_mother) | — | C | C | C | I | C | C | C | C | C | — |
| [`born_canada`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-born_canada) | I | C | — | C | C | — | — | — | — | — | — |
| [`income_native`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-income_native) | A | — | A | C | I | — | A | A | A | A | — |
| [`religion`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.html#target-religion) | A | — | — | C | I | — | — | — | — | — | — |

[Coverage by
study](https://thomasgareau.github.io/qesR/articles/coverage.md) adds
each study’s waves and recommended weights, and [Harmonizing across
studies](https://thomasgareau.github.io/qesR/articles/harmonization.md)
goes from
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
to a weighted estimate on the full data files.

## From a study code to a weighted estimate

``` r

qes_studies()                               # the studies, offline
qes2014 <- get_qes("qes2014")               # returned, not assigned for you
qes_search("indépendant|independent")       # find the question
qes_question("qes2014", "Q19", lang = "en") # read its wording
qes2014 <- qes_missing(qes2014)             # "don't know", "refused" to NA

# share YES among those who answered YES or NO, with the study's weight:
# a point estimate; qes2014 is an opt-in online panel, with no margin of error
answered <- qes2014$Q19 %in% c(1, 2)
weighted.mean(qes2014$Q19[answered] == 1, w = qes2014$POND[answered])
```

[Getting
started](https://thomasgareau.github.io/qesR/articles/get-started.md)
walks through each step: which population the estimate describes, why
this weight, and who is in the denominator. It runs offline on
`qes_demo`, a small synthetic study that ships with qesR.

## What else qesR gives you

- **The original files, checked.**
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  reads the pinned SPSS or Stata file of a study and returns it with its
  codes and labels.
  [`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
  saves the files themselves, and
  [`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
  records which file a result came from.
- **Codebooks and search, offline and bilingual.**
  [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
  and
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
  work without a network connection for every study.
  [`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
  knows which codes mean “don’t know” or “refused”.
- **Citations.**
  [`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
  cites qesR and each dataset with its DOI, version and UNF.
- **Nothing written behind your back.** Data are returned, not assigned
  into your workspace; downloads go to a temporary cache unless you
  choose `options(qesR.cache = "disk")`.

## Coming from qesR 0.4.4

Every function of qesR 0.4.4 keeps its name and its arguments, and
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
returns the same codes, but it now returns the data instead of creating
the object: write `qes2018 <- get_qes("qes2018")`. The legacy merged
file,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md),
keeps its 30 columns but is now rendered from the harmonization engine,
so estimates computed from it change. [Moving from qesR 0.4.4 to
0.7.0](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md)
says what changed, column by column, and how to reproduce a 0.4.4
result.

## Where to go next

- [Study
  catalog](https://thomasgareau.github.io/qesR/articles/studies.md):
  every study code, with its design, population, size, licence, DOI,
  pinned file and documents, generated from the catalog that ships with
  the package.
- [Study
  citations](https://thomasgareau.github.io/qesR/articles/citations.md):
  how to cite qesR and each dataset you use.
- Harmonization (experimental): [harmonizing across
  studies](https://thomasgareau.github.io/qesR/articles/harmonization.md),
  [coverage by
  study](https://thomasgareau.github.io/qesR/articles/coverage.md),
  [validation against official
  results](https://thomasgareau.github.io/qesR/articles/validation.md)
  and the [harmonization
  reference](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md).
- Examples built from the full data files: [respondents by
  study](https://thomasgareau.github.io/qesR/articles/analysis-descriptive.md),
  [support for
  independence](https://thomasgareau.github.io/qesR/articles/analysis-sovereignty.md)
  and [reported
  vote](https://thomasgareau.github.io/qesR/articles/analysis-vote-choice.md).
- [Reference](https://thomasgareau.github.io/qesR/reference/index.md):
  every function, grouped by task. The functions of qesR 0.4.4 are under
  *Legacy and deprecated*; they keep working.

Not every study is a Quebec Election Study: the Durand panels, the CROP
polls and the 1998 polls are listed under their own titles and authors.
Check each study’s design and population in the catalog before comparing
them.

## En français

qesR charge dans R les Études électorales québécoises et d’autres
enquêtes électorales québécoises à partir d’un code d’étude, chacune
fixée à un fichier de données original vérifié par md5. Il harmonise
aussi les 11 études de 1998 à 2022 en un seul tableau, question par
question, avec un niveau de comparabilité pour la question de chaque
étude et un motif pour chaque valeur manquante (expérimental ; la grille
plus haut donne le niveau de chaque étude pour chaque cible). Les
codebooks, les libellés des questions et la recherche fonctionnent sans
réseau, en français et en anglais. Les messages et les erreurs
s’affichent en français avec `options(qesR.lang = "fr")` ; les données
retournées ne dépendent jamais de la langue.

Chaque fonction de qesR 0.4.4 garde son nom et ses arguments, et
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
retourne les mêmes codes, mais il retourne maintenant les données au
lieu de créer l’objet : écrivez `qes2018 <- get_qes("qes2018")`. Le
fichier fusionné hérité,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md),
garde ses 30 colonnes mais est maintenant produit par le moteur
d’harmonisation : les estimations qui en viennent changent. [Passer de
qesR 0.4.4 à
0.7.0](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md)
dit ce qui a changé, colonne par colonne, et comment reproduire un
résultat de 0.4.4.

Pour installer qesR :

``` r

# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
# une fois le paquet accepté sur le CRAN :
# install.packages("qesR")
```

Les données ne font pas partie du package : qesR les télécharge de
Borealis et du Harvard Dataverse. La plupart des études sont sous CC0
1.0, celle de 2022 sous CC BY-NC 4.0 (attribution, pas d’usage
commercial) ; `qes_studies()$licence` donne la licence de chaque étude.

**Licence.** La licence MIT de qesR couvre le code du package seulement.
Les métadonnées de l’étude de 2022 que qesR livre (étiquettes, texte des
questions, effectifs, et les libellés, étiquettes et effectifs de
l’harmonisation qui en sont tirés) sont tirées de Mahéo, Bélanger,
Stephenson et Harell (2023), *2022 Quebec Election Study*, Harvard
Dataverse, V1.1, <https://doi.org/10.7910/DVN/PAQBDR>, et restent sous
licence [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/deed.fr)
(attribution, pas d’usage commercial), ce qui n’implique aucune
approbation de qesR par les auteurs ; les métadonnées des autres études
sont sous CC0 1.0, et les effectifs du recensement viennent de
Statistique Canada (Licence ouverte de Statistique Canada). Le fichier
`COPYRIGHTS` du package donne chaque fichier, sa source et sa licence.

- [Démarrage](https://thomasgareau.github.io/qesR/articles/fr-demarrage.md)
  : du code d’étude à une estimation pondérée
- [Catalogue des
  études](https://thomasgareau.github.io/qesR/articles/fr-etudes.md)
- [Citations des
  études](https://thomasgareau.github.io/qesR/articles/fr-citations.md)
- [Passer de qesR 0.4.4 à
  0.7.0](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md)
- Harmonisation (expérimental) : [harmoniser entre
  études](https://thomasgareau.github.io/qesR/articles/fr-harmonisation.md),
  [couverture par
  étude](https://thomasgareau.github.io/qesR/articles/fr-couverture.md),
  [validation par les résultats
  officiels](https://thomasgareau.github.io/qesR/articles/fr-validation.md),
  [référence de
  l’harmonisation](https://thomasgareau.github.io/qesR/articles/fr-reference-harmonisation.md)
- [Le fichier fusionné
  hérité](https://thomasgareau.github.io/qesR/articles/fr-donnees-fusionnees.md)
- Exemples : [répondants par
  étude](https://thomasgareau.github.io/qesR/articles/fr-analyse-descriptive.md),
  [appui à
  l’indépendance](https://thomasgareau.github.io/qesR/articles/fr-analyse-souverainete.md),
  [vote
  déclaré](https://thomasgareau.github.io/qesR/articles/fr-analyse-choix-vote.md)
- [Aperçu des fonctions en
  français](https://thomasgareau.github.io/qesR/reference/qesR-fr.md)
  (`?qesR-fr`)

## Data licences

The data are not part of the package: qesR downloads them from Borealis
and the Harvard Dataverse. Most studies are released under CC0 1.0. The
2022 study is CC BY-NC 4.0 (attribution, no commercial use).
`qes_studies()$licence` gives the licence of each study.

## Licence

The MIT licence of qesR covers the package code only. The metadata of
the 2022 study that qesR ships (labels, question text, answer counts,
and the harmonization wording, labels and counts derived from them) are
derived from Mahéo, Bélanger, Stephenson and Harell (2023), *2022 Quebec
Election Study*, Harvard Dataverse, V1.1,
<https://doi.org/10.7910/DVN/PAQBDR>, and keep its licence, [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/): attribution, no
commercial use; this does not imply that the authors endorse qesR. The
metadata of the other studies are CC0 1.0, and the census counts come
from Statistics Canada (Statistics Canada Open Licence). The file
`COPYRIGHTS` of the package lists each file, its source and its licence.
