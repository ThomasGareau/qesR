---
title: qesR
---

# qesR <img src="logo.png" align="right" height="139" alt="qesR logo" />

qesR loads the Quebec Election Studies and related Quebec election surveys
into R by study code. Each code points to one Dataverse deposit, pinned to a
dataset version and to one original data file that is checked against its
md5 checksum before use. The catalog of the studies, their documents, their
citations and the description of every variable of the studies released
under CC0 ship with the package, so codebooks, question wording and search
work offline, in English and French.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")

# from CRAN, once qesR is accepted there
install.packages("qesR")
```

## From a study code to a weighted estimate

```r
library(qesR)

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

[Getting started](articles/get-started.html) walks through each step: which
population the estimate describes, why this weight, and who is in the
denominator. It runs offline on `qes_demo`, a small synthetic study that
ships with qesR.

## What qesR gives you

- **The original files, checked.** `get_qes()` reads the pinned SPSS or
  Stata file of a study and returns it with its codes and labels.
  `qes_download()` saves the files themselves, and `qes_provenance()` records
  which file a result came from.
- **Codebooks and search, offline and bilingual.** `qes_codebook()`,
  `qes_question()` and `qes_search()` work without a network connection for
  every CC0 study. `qes_missing()` knows which codes mean "don't know" or
  "refused".
- **Citations.** `qes_cite()` cites qesR and each dataset with its DOI,
  version and UNF.
- **Harmonized data, experimental.** `qes_harmonize()` builds one data frame
  from several studies from a specification checked against the original
  files: a comparability grade for each study's question, a reason for every
  missing value, each wave's weight, and `qes_design()` for the `survey`
  package. It applies only the rows a reviewer has signed off, and none is
  signed off yet: `include_draft = TRUE` applies the others too.
- **Nothing written behind your back.** Data are returned, not assigned into
  your workspace; downloads go to a temporary cache unless you choose
  `options(qesR.cache = "disk")`.
- **Code written for qesR 0.4.4 keeps running.** The old functions and the
  legacy merged file, `get_qes_master()`, are kept;
  [Moving from qesR 0.4.4](articles/migrating-0.5.html) says what changed.

## Where to go next

- [Study catalog](articles/studies.html): every study code, with its design,
  population, size, licence, DOI, pinned file and documents, generated from
  the catalog that ships with the package.
- [Study citations](articles/citations.html): how to cite qesR and each
  dataset you use.
- Harmonization (experimental):
  [harmonizing across studies](articles/harmonization.html), from
  `qes_harmonize()` to a weighted estimate;
  [coverage by study](articles/coverage.html), the grade of every
  harmonized variable in every study; and the
  [harmonization reference](articles/harmonization-reference.html), both
  generated from the specification.
- Examples built from the full data files:
  [respondents by study](articles/analysis-descriptive.html),
  [support for independence](articles/analysis-sovereignty.html) and
  [reported vote](articles/analysis-vote-choice.html).
- [Reference](reference/index.html): every function, grouped by task. The
  functions of qesR 0.4.4 are under *Legacy and deprecated*; they keep
  working.

Not every study is a Quebec Election Study: the Durand panels, the CROP polls
and the 1998 polls are listed under their own titles and authors. Check each
study's design and population in the catalog before comparing them.

## En français

qesR charge dans R les Études électorales québécoises et d'autres enquêtes
électorales québécoises à partir d'un code d'étude, chacune fixée à un
fichier de données original vérifié par md5. Les codebooks, les libellés des
questions et la recherche fonctionnent sans réseau, en français et en
anglais. Les messages et les erreurs s'affichent en français avec
`options(qesR.lang = "fr")` ; les données retournées ne dépendent jamais de
la langue.

Les données ne font pas partie du package : qesR les télécharge de Borealis
et du Harvard Dataverse. La plupart des études sont sous CC0 1.0, celle de
2022 sous CC BY-NC 4.0 (attribution, pas d'usage commercial), et qesR ne livre
aucune de ses métadonnées ; `qes_studies()$licence` donne la licence de
chaque étude.

- [Démarrage](articles/fr-demarrage.html) : du code d'étude à une
  estimation pondérée
- [Catalogue des études](articles/fr-etudes.html)
- [Citations des études](articles/fr-citations.html)
- [Passer de qesR 0.4.4 à 0.5.0](articles/fr-migrer-0.5.html)
- Harmonisation (expérimental) :
  [harmoniser entre études](articles/fr-harmonisation.html),
  [couverture par étude](articles/fr-couverture.html),
  [référence de l'harmonisation](articles/fr-reference-harmonisation.html)
- [Le fichier fusionné hérité](articles/fr-donnees-fusionnees.html)
- Exemples : [répondants par étude](articles/fr-analyse-descriptive.html),
  [appui à l'indépendance](articles/fr-analyse-souverainete.html),
  [vote déclaré](articles/fr-analyse-choix-vote.html)
- [Aperçu des fonctions en français](reference/qesR-fr.html) (`?qesR-fr`)

## Data licences

The data are not part of the package: qesR downloads them from Borealis and
the Harvard Dataverse. Most studies are released under CC0 1.0. The 2022
study is CC BY-NC 4.0 (attribution, no commercial use), so qesR ships none of
its metadata. `qes_studies()$licence` gives the licence of each study. The
package's own code is MIT-licensed.
