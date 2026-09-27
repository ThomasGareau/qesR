# qesR

<p align="center">
  <img src="man/figures/logo.png" alt="qesR logo" width="180" />
</p>

qesR loads the Quebec Election Studies and related Quebec election surveys
into R by study code. Each code points to one Dataverse deposit (Borealis or
the Harvard Dataverse), pinned to a dataset version and to one original
SPSS or Stata data file, checked against its md5 checksum before use. The
catalog of studies, their documents, their citations and the description of
every variable of the studies released under CC0 ship with the package, so
codebooks, question wording and search work offline, in English and French.

Une présentation en français suit la version anglaise (section *En
français*).

## Installation

```r
install.packages("qesR")

# development version
# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

## A first session

```r
library(qesR)

# the studies: code, year, family, design, population, licence, DOI, version
qes_studies()

# their codebooks, questionnaires and reports
qes_docs("qes2018")

# load a study: get_qes() returns the data; assign it yourself
qes2018 <- get_qes("qes2018")

# the synthetic demonstration study ships with qesR (no download)
demo <- get_qes("qes_demo")
```

`get_qes()` returns a data frame with the codes and labels of the file
(`haven` labelled columns). It writes nothing into your workspace: with
`assign_global = TRUE` it assigns into the environment you call it from. A
study is downloaded once per session; `options(qesR.cache = "disk")` keeps
the files between sessions.

## Codebooks, questions and search

```r
# the codebook, offline for every CC0 study
cb <- qes_codebook("qes2018")
qes_codebook("qes2018", layout = "long", variables = c("q26", "q27"))

# the exact wording of a question, in English or French
qes_question("qes2018", "q26", lang = "en")

# search every study, ignoring case and accents
qes_search("souverain|sovereign")

# set "don't know", "refused" and declared missing codes to NA
qes2018_clean <- qes_missing(qes2018)
```

The 2022 study is licensed CC BY-NC 4.0, so qesR ships none of its
metadata: its codebook is built from your own copy of the data file the
first time you ask for it.

## Provenance and citations

```r
# which file the data came from: DOI, version, file id, md5, retrieval
qes_provenance(qes2018)

# cite qesR and the datasets you used (text, BibTeX or bibentry)
qes_cite("qes2018")
qes_cite(c("qes2018", "qes2022"), style = "bibtex")

# save the original files, md5-checked, in a folder you choose
# (here a temporary one; use a folder of your project to keep them)
dir <- file.path(tempdir(), "qes_originals")
dir.create(dir)
qes_download("qes2018", path = dir, what = c("data", "docs"))
```

## The legacy merged file

`get_qes_master()` stacks the 11 studies of qesR 0.4.4 in one data frame
with the 30 harmonized columns of 0.4.4 (same names, order and types).
Since 0.5.0 it keeps every respondent, drops the columns that stacked
different questions under one name, and sets values verified to be wrong to
`NA`; attributes say what changed and why.

```r
master <- get_qes_master()
head(attr(master, "legacy_na_columns"))
```

Results computed with qesR 0.4.4 change under 0.5.0. See
`vignette("migrating-0.5", package = "qesR")` and NEWS; to reproduce a 0.4.4
result exactly, install that version:
`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`.

## Messages, errors and the network

- Messages, warnings and errors are in English or French
  (`options(qesR.lang = "fr")`). The data returned never depends on the
  language.
- Errors have classes (`qesR_error_unknown_study`, `qesR_error_network`,
  ...), so scripts can handle them with `tryCatch()`; see `?qesR`.
- Requests go only to the Dataverse servers in the catalog, one at a time
  and at least one second apart, with certificate checks and the
  User-Agent `qesR/<version> R/<version>`. A request that fails for a
  passing reason is retried a few times, honouring `Retry-After`.
- The functions of qesR 0.4.4 (`get_codebook()`, `get_question()`,
  `get_preview()`, ...) keep working and print a one-time notice naming
  their replacement; see `?qesR-deprecated`.

## Documentation

- Website: <https://thomasgareau.github.io/qesR/>
- Vignettes: `vignette("get-started", package = "qesR")`,
  `vignette("citations", package = "qesR")`,
  `vignette("migrating-0.5", package = "qesR")`.

## Data and licences

The data are not part of the package: qesR downloads them from their
deposits. The 2022 study is licensed CC BY-NC 4.0 (attribution, no
commercial use); the other studies are CC0 1.0. `qes_studies()$licence`
gives each study's licence, and `qes_cite()` its citation, with its DOI
link (for example <https://doi.org/10.5683/SP3/NWTGWS> for the 2018 study).
The package's own code is MIT-licensed.

## En français

qesR charge dans R les Études électorales québécoises et d'autres enquêtes
électorales québécoises à partir d'un code d'étude. Chaque code désigne un
dépôt Dataverse (Borealis ou Harvard Dataverse), fixé à une version du jeu
de données et à un fichier de données original SPSS ou Stata, vérifié par
sa somme de contrôle md5 avant usage. Le catalogue des études, leurs
documents, leurs citations et la description de chaque variable des études
sous licence CC0 sont livrés avec le package : codebooks, libellés des
questions et recherche fonctionnent sans réseau, en français et en anglais.

```r
library(qesR)
options(qesR.lang = "fr")          # messages et erreurs en français

qes_studies()                      # les études
qes2018 <- get_qes("qes2018")      # les données sont retournées : assignez-les
qes_codebook("qes2018")            # codebook, sans réseau
qes_question("qes2018", "q26", lang = "fr")
qes_search("souverain|sovereign")  # recherche dans toutes les études
qes2018 <- qes_missing(qes2018)    # « ne sait pas » et « refus » en NA
```

L'étude de 2022 est sous licence CC BY-NC 4.0 : qesR ne livre aucune de
ses métadonnées, et son codebook est construit à partir de votre copie du
fichier de données la première fois que vous le demandez.

### Provenance et citations

```r
qes_provenance(qes2018)            # fichier d'origine : DOI, version, md5
qes_cite("qes2018", lang = "fr")   # citation de qesR et du jeu de données

# enregistrer les fichiers originaux, vérifiés par md5, dans un dossier de
# votre choix (ici un dossier temporaire ; prenez un dossier de votre projet
# pour les garder)
dir <- file.path(tempdir(), "qes_originals")
dir.create(dir)
qes_download("qes2018", path = dir, what = c("data", "docs"))
```

### Le fichier fusionné hérité

`get_qes_master()` empile les 11 études de qesR 0.4.4 dans un seul tableau,
avec les 30 colonnes harmonisées de 0.4.4 (mêmes noms, ordre et types).
Depuis 0.5.0, il garde tous les répondants, retire les colonnes qui
empilaient des questions différentes sous un même nom et met à `NA` les
valeurs dont l'erreur a été vérifiée ; ses attributs disent ce qui a changé
et pourquoi.

```r
master <- get_qes_master()
head(attr(master, "legacy_na_columns"))
```

### Messages, erreurs et réseau

- `get_qes()` n'écrit rien dans votre espace de travail par défaut ; avec
  `assign_global = TRUE`, l'objet est assigné dans l'environnement d'où la
  fonction est appelée.
- Les messages, avertissements et erreurs sont en français ou en anglais
  (`options(qesR.lang = "fr")`). Les données retournées ne dépendent jamais
  de la langue.
- Les erreurs ont des classes (`qesR_error_unknown_study`,
  `qesR_error_network`, ...) : un script peut les traiter avec
  `tryCatch()` ; voir `?qesR`.
- Les requêtes vont seulement aux serveurs Dataverse du catalogue, une à la
  fois et à au moins une seconde d'intervalle, avec vérification des
  certificats et le User-Agent `qesR/<version> R/<version>`. Une requête qui
  échoue pour une raison passagère est reprise quelques fois, en respectant
  `Retry-After`.
- Les fonctions de qesR 0.4.4 continuent de fonctionner et affichent une
  note unique qui nomme leur remplacement (`?qesR-deprecated`).
- Les résultats de `get_qes_master()` changent avec 0.5.0 : voir
  `vignette("fr-migrer-0.5", package = "qesR")` ; pour reproduire
  exactement un résultat de 0.4.4, installez cette version
  (`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`).

### Documentation, données et licences

- Site web : <https://thomasgareau.github.io/qesR/>
- La page `?qesR-fr` présente toutes les fonctions en français. Guides :
  `vignette("fr-demarrage", package = "qesR")`,
  `vignette("fr-citations", package = "qesR")`,
  `vignette("fr-migrer-0.5", package = "qesR")`.

Les données ne font pas partie du package : qesR les télécharge depuis
leurs dépôts. L'étude de 2022 est sous licence CC BY-NC 4.0 (attribution,
pas d'usage commercial) ; les autres études sont sous licence CC0 1.0.
`qes_studies()$licence` donne la licence de chaque étude, et `qes_cite()` sa
citation, avec le lien de son DOI. Le code du package est sous licence MIT.
