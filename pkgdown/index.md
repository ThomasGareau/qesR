---
title: qesR
---

# qesR <img src="logo.png" align="right" height="139" alt="qesR logo" />

qesR loads the Quebec Election Studies and related Quebec election surveys
into R by study code. Each code points to one Dataverse deposit, and the
package ships an offline catalog of the studies (with the dataset version and
data file each code is pinned to), their documents and their citations.

## Installation

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

## A first session

```r
library(qesR)

# Which studies are there? (offline)
studies <- qes_studies()
studies[, c("study", "year", "title_en", "licence")]

# Load one study. The data is returned, not written into your workspace.
qes2018 <- get_qes("qes2018")

# Its codebook, its deposited documents and its citation
cb <- qes_codebook("qes2018")
qes_docs("qes2018")
qes_cite("qes2018")
```

The data comes back with its value labels (haven `labelled` columns), and the
codebook gives each variable's label and question text.

## Where to go next

- [Getting started](articles/get-started.html): loading a study, codebooks,
  errors you can handle in scripts.
- [Study catalog](articles/studies.html): every study code, with its design,
  population, size, licence, DOI and documents. The page is generated from the
  catalog that ships with the package, the same data as `qes_studies()`.
- [Study citations](articles/study-citations.html): how to cite qesR and each
  dataset you use.
- [Merged dataset](articles/merged-dataset.html): `get_qes_master()` stacks
  the studies of qesR 0.4.4 into one harmonized file.
- [Reference](reference/index.html): every function. The functions of qesR
  0.4.4 are grouped under *Legacy functions*; they keep working.

Not every study is a Quebec Election Study: the Durand panels, the CROP polls
and the 1998 polls are listed under their own titles and authors. Check each
study's design and population in the catalog before comparing them.

## En français

qesR charge les Études électorales québécoises et d'autres enquêtes
électorales québécoises dans R à partir d'un code d'étude. Les messages et
les erreurs sont aussi offerts en français avec `options(qesR.lang = "fr")`.

- [Démarrage](articles/fr-demarrage.html)
- [Catalogue des études](articles/fr-etudes.html)
- [Citations des études](articles/fr-citations-etudes.html)
- [Données fusionnées](articles/fr-donnees-fusionnees.html)

## Data licences

The data are not part of the package: qesR downloads them from Borealis and
the Harvard Dataverse. Most studies are released under CC0 1.0. The 2022 study
is CC BY-NC 4.0 (attribution, no commercial use). `qes_studies()$licence`
gives the licence of each study.
