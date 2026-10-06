# qesR

<p align="center">
  <img src="man/figures/logo.png" alt="qesR logo" width="180" />
</p>

qesR loads the Quebec Election Studies, with the panels and polls that
accompanied them, into R: seven provincial elections from 1998 to 2022,
each study by its code, from one original file of a pinned version of its
deposit, checked against its md5 checksum. Their codebooks, question
wording and a search across all studies ship with the package and work
offline, in English and French. qesR also harmonizes 11 of the studies into
one data frame, so that a question about 25 years of Quebec elections can
be answered within each study and then compared.

<p align="center">
  <a href="https://thomasgareau.github.io/qesR/articles/sovereignty-generations.html"><img src="https://thomasgareau.github.io/qesR/articles/sovereignty-generations_files/figure-html/gradient-light.png" alt="Dot charts in six panels, one per Quebec Election Study from 2007 to 2022, of the share of francophones who would vote Yes in each birth cohort: in 2007 the youngest cohort is the most likely to vote Yes, in 2022 the least likely." width="600" /></a>
</p>

In 2007 the youngest francophones were the cohort most likely to vote Yes
in a referendum; in 2022 they were the least likely. The
[website](https://thomasgareau.github.io/qesR/) has this and other worked
examples on 25 years of Quebec elections, each estimate made within one
study.

Une présentation en français suit la version anglaise (section *En
français*).

## Installation

```r
# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")

# once accepted on CRAN:
# install.packages("qesR")
```

## A first session

```r
library(qesR)

# the studies: code, year, family, design, licence and title (as.data.frame()
# gives every column: population, DOI, pinned version, ...)
qes_studies()

# their codebooks, questionnaires and reports
qes_docs("qes2018")

# load a study (downloads it the first time): get_qes() returns the data;
# assign it yourself
qes2018 <- get_qes("qes2018")

# the synthetic demonstration study ships with qesR (no download)
demo <- get_qes("qes_demo")

# every harmonized study in one flat data frame, cesR style (relaxed);
# downloads the 11 studies (about 16 MB) the first time
d <- qes_decon()
```

`get_qes()` returns a data frame with the codes and labels of the file
(`haven` labelled columns). It writes nothing into your workspace: with
`assign_global = TRUE` it assigns into the environment you call it from. A
study is downloaded once per session; `options(qesR.cache = "disk")` keeps
the files between sessions.

## Codebooks, questions and search

```r
# the codebook, offline for every study
cb <- qes_codebook("qes2018")
qes_codebook("qes2018", layout = "long", variables = c("q26", "q27"))

# the exact wording of a question, in English or French
qes_question("qes2018", "q26", lang = "en")

# search every study, ignoring case and accents
qes_search("souverain|sovereign")

# set "don't know", "refused" and declared missing codes to NA
qes2018_clean <- qes_missing(qes2018)
```

The 2022 study is licensed CC BY-NC 4.0. Its metadata (codebook, question
text, value labels and counts) ships with qesR under that licence
(attribution, no commercial use), not under qesR's MIT licence:
`system.file("COPYRIGHTS", package = "qesR")` lists the files.

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

## The merged file

`get_qes_master()` stacks 11 studies in one data frame with 30 harmonized
columns in a fixed layout (names, order and types do not change). It keeps
every respondent, is built from the harmonized variables below, so
`vote_choice` is the reported vote in every study and a code the
specification does not map is `NA`, and its attributes say what each
column holds and which question it read in each study. For an analysis you
will publish, prefer `qes_harmonize()`.

```r
master <- get_qes_master()
head(attr(master, "legacy_column_map"))
```

Code written for qesR 0.4.4, and how to reproduce its results:
`vignette("migrating-0.7", package = "qesR")`.

## Harmonized data (experimental)

`qes_harmonize()` builds one data frame from several studies, from a
specification that ships with qesR: for each study and harmonized variable
("target"), one question, its codes mapped one by one, with a comparability
grade, a reason for every missing value and each wave's weight;
`qes_design()` turns the result into a design for the `survey` package, and
`qes_spec()` shows the specification. The rows of the specification were
signed off after an automated double review against the original files and
documents (not a human review). Not every study has a validated weight: the
[weights table](https://thomasgareau.github.io/qesR/articles/studies.html#weights)
says which do; the others have weight columns of `NA`. The rules are
experimental and may change from one version to the next: keep
`qes_provenance(h, level = "spec")` with your results and pin the version
of qesR you used.

A pooled variable puts the questions of one topic in one column for every
study, with the question each value comes from: `vote_choice` is the
provincial vote choice (the reported vote, else the vote intention),
`sov_support` support for sovereignty across the referendum wordings, and
`pol_interest` interest in politics on 0 to 1.

```r
h <- qes_harmonize(targets = c("sov_indep", "vote_prov_recall"))
v <- qes_harmonize("all", targets = "vote_choice", layout = "long")
table(v$study, v$vote_choice__type)  # recall or intention, study by study
qes_spec()                         # which study has which target, and its grade
```

The grade of each study's question for each target (I identical, C
comparable, A approximate; a dash: no question in the specification):

<!-- coverage: start (generated by data-raw/readme_coverage.R) -->

|  | `qes2022` | `qes2018` | `qes2018_panel` | `qes2014` | `qes2012` | `qes2012_panel` | `qes_crop_2007_2010` | `qes2008` | `qes2007` | `qes2007_panel` | `qes1998` |
|---|---|---|---|---|---|---|---|---|---|---|---|
| `survey_mode` | — | — | I | — | — | — | — | — | I | — | — |
| `vote_prov_recall` | C | C | C | C | I | A | — | C | C | A | C |
| `vote_prov_intent` | A | — | A | — | — | A | C | — | — | I | — |
| `vote_prov_intent_push` | A | — | A | — | — | A | C | — | — | I | A |
| `turnout_prov_recall` | A | A | A | C | I | C | — | C | C | C | C |
| `turnout_prov_likely` | I | — | — | — | — | — | — | — | — | — | — |
| `vote_prov_intent_other` | I | — | — | — | — | — | — | — | — | — | — |
| `vote_prov_prev` | C | C | — | I | — | — | — | C | — | — | — |
| `vote_fed_recall` | C | — | — | — | I | — | — | C | A | — | — |
| `pid_prov` | C | C | — | I | I | — | — | C | C | — | — |
| `pid_fed` | I | — | — | — | C | — | — | — | — | — | — |
| `pid_prov_strength` | A | C | — | I | I | — | — | A | A | — | — |
| `sov_indep` | C | C | — | I | I | — | — | — | — | — | — |
| `sov_sovereign_country` | — | — | — | — | — | I | — | — | — | — | — |
| `sov_favour` | — | — | I | — | — | — | — | — | — | — | — |
| `lr_self` | A | C | A | I | I | — | — | — | — | — | — |
| `interest_4pt` | — | C | — | C | I | — | — | — | — | — | — |
| `sov_partnership_1995` | — | — | — | — | — | — | — | C | I | C | C |
| `interest_0_10` | A | — | — | — | — | — | — | — | I | — | — |
| `interest_election_0_10` | — | — | — | — | — | — | — | C | I | — | — |
| `interest_campaign_4pt` | — | — | — | — | — | — | — | — | — | I | — |
| `sov_partnership_1995_push` | — | — | — | — | — | — | — | C | I | C | C |
| `satis_demo_qc` | C | C | — | I | I | — | — | C | C | — | — |
| `gov_satisfaction` | C | C | — | C | I | — | — | — | — | A | — |
| `econ_retro_qc` | C | I | — | I | I | — | — | C | C | — | — |
| `attach_qc` | C | C | — | C | I | — | — | — | — | — | — |
| `attach_ca` | C | C | — | C | I | — | — | — | — | — | — |
| `identity_qc_ca` | C | — | — | C | I | — | — | C | C | — | — |
| `therm_leader_plq` | A | A | — | C | I | — | — | C | C | — | — |
| `therm_leader_pq` | A | A | — | C | I | — | — | C | C | — | — |
| `therm_leader_caq` | A | A | — | C | I | — | — | — | — | — | — |
| `therm_leader_qs` | A | A | — | C | I | — | — | C | C | — | — |
| `therm_leader_adq` | — | — | — | — | — | — | — | C | I | — | — |
| `mip_issue` | C | C | — | C | I | — | — | C | — | — | — |
| `birth_year` | I | C | — | C | C | — | — | C | C | — | — |
| `birth_month` | — | I | — | — | — | — | — | — | — | — | — |
| `age` | I | A | — | — | — | — | — | — | — | — | — |
| `age_group3` | — | — | I | — | — | C | C | C | — | C | C |
| `citizen` | I | — | — | — | — | — | — | — | — | — | — |
| `age_group6` | — | — | — | — | — | C | I | C | — | C | C |
| `gender` | C | C | C | C | I | C | C | C | C | C | C |
| `education4` | C | C | A | I | C | — | A | C | C | A | — |
| `lang_mother` | A | C | C | C | I | C | C | C | C | C | — |
| `born_canada` | I | C | — | C | C | — | — | — | — | — | — |
| `income_native` | A | — | A | C | I | — | A | A | A | A | — |
| `religion` | A | — | — | C | I | — | — | — | — | — | — |
| `region_cma3` | — | I | A | I | I | C | C | C | C | — | — |
| `lang_home` | A | C | — | C | I | — | C | C | C | C | — |
| `relig_attend` | — | A | — | A | I | — | — | C | C | — | — |
| `birthplace3` | — | C | — | C | I | — | — | — | — | — | — |
| `vote_choice` (pooled) | C | C | C | C | I | A | C | C | C | A | C |
| `sov_support` (pooled) | C | C | A | I | I | I | — | C | I | C | C |
| `pol_interest` (pooled) | A | A | — | A | A | — | — | A | I | A | — |
| `turnout` (pooled) | A | A | A | C | I | C | — | C | C | C | C |

<!-- coverage: end -->

The [website](https://thomasgareau.github.io/qesR/) has the full grid, with
each study's waves and weights (*Coverage by study*), and an article that
goes from `qes_harmonize()` to a weighted estimate.

### Relaxed harmonization

`qes_decon()` returns one flat data frame for every study, in the style of
cesR: plain column names (`education`, `income_cat`, `religion`,
`vote_choice`, `sovereignty`, `lr`...) and one concept per column even
where the wording or the answer options differ, in coarse common
categories (education in three groups, income in thirds of each study's
respondents, a referendum vote of yes or no whatever the question). It
trades exactness for coverage: its columns carry no grade, each says how
it was relaxed and where each study's values come from, and
`qes_harmonize()` keeps the strict versions. The relaxed mappings of the
studies' own questions were signed off by an automated double review
against the original files and documents (not a human review), the five
added in 0.9.1 (the 1998 education, union membership and personal
finances) included.

```r
d <- qes_decon()
table(d$study, d$sovereignty, useNA = "ifany")
attr(d$sovereignty, "relaxed")                          # how it was relaxed
attr(d, "decon_sources")[, c("column", "study", "source_var", "recode")]
```

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
- Older function names (`get_codebook()`, `get_question()`,
  `get_preview()`, ...) keep working and print a one-time notice naming
  their replacement; see `?qesR-deprecated`.

## Documentation

- Website: <https://thomasgareau.github.io/qesR/>
- Vignettes: `vignette("get-started", package = "qesR")`,
  `vignette("citations", package = "qesR")`,
  `vignette("migrating-0.7", package = "qesR")`.

## Data and licences

The data are not part of the package: qesR downloads them from their
deposits. The 2022 study is licensed CC BY-NC 4.0 (attribution, no
commercial use); the other studies are CC0 1.0. `qes_studies()$licence`
gives each study's licence, and `qes_cite()` its citation, with its DOI
link (for example <https://doi.org/10.5683/SP3/NWTGWS> for the 2018 study).

## Licence

The MIT licence of qesR (file `LICENSE`) covers the package code only. The
metadata and benchmark counts qesR ships keep the licence of their source:

- The metadata of the 2022 study (variable and value labels, question
  text, answer counts, and the harmonization wording, labels and counts
  derived from them) are derived from Mahéo, Bélanger, Stephenson and
  Harell (2023), *2022 Quebec Election Study*, Harvard Dataverse, V1.1,
  <https://doi.org/10.7910/DVN/PAQBDR>, and licensed
  [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/):
  attribution, no commercial use. qesR extracted, reformatted and tabulated
  them; this does not imply that the authors endorse qesR.
- The metadata of the other studies come from deposits released under
  CC0 1.0, and the census counts used as validation benchmarks are adapted
  from Statistics Canada under the Statistics Canada Open Licence.

The file `COPYRIGHTS` (`system.file("COPYRIGHTS", package = "qesR")`) lists
each file, its source and its licence.

## En français

qesR charge dans R les Études électorales québécoises, avec les panels et
les sondages qui les ont accompagnées : sept élections provinciales, de
1998 à 2022, chaque étude par son code, à partir d'un fichier original
d'une version fixée de son dépôt, vérifié par sa somme de contrôle md5.
Leurs codebooks, le libellé de leurs questions et une recherche dans toutes
les études sont livrés avec le package et fonctionnent sans réseau, en
français et en anglais. qesR harmonise aussi 11 de ces études en un seul
tableau, de sorte qu'une question sur 25 ans d'élections québécoises peut
être examinée dans chaque étude, puis comparée.

Le [site web](https://thomasgareau.github.io/qesR/articles/fr-accueil.html)
présente des exemples sur 25 ans d'élections québécoises : en 2007, les
plus jeunes francophones formaient la cohorte la plus encline à voter Oui ;
en 2022, la moins encline.

```r
# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
# une fois le paquet accepté sur le CRAN :
# install.packages("qesR")

library(qesR)
options(qesR.lang = "fr")          # messages et erreurs en français

qes_studies()                      # les études
qes2018 <- get_qes("qes2018")      # les données sont retournées : assignez-les
qes_codebook("qes2018")            # codebook, sans réseau
qes_question("qes2018", "q26", lang = "fr")
qes_search("souverain|sovereign")  # recherche dans toutes les études
qes2018 <- qes_missing(qes2018)    # « ne sait pas » et « refus » en NA
d <- qes_decon(lang = "fr")        # toutes les études, un seul tableau (souple)
```

L'étude de 2022 est sous licence CC BY-NC 4.0. Ses métadonnées (codebook,
texte des questions, étiquettes de valeurs et effectifs) sont livrées avec
qesR sous cette licence (attribution, pas d'usage commercial), et non sous
la licence MIT de qesR : `system.file("COPYRIGHTS", package = "qesR")` en
donne la liste.

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

### Le fichier fusionné

`get_qes_master()` empile 11 études dans un seul tableau, avec 30 colonnes
harmonisées dans un format fixe (noms, ordre et types ne changent pas). Il
garde tous les répondants, est construit à partir des variables
harmonisées décrites plus bas, de sorte que `vote_choice` est le vote
déclaré dans toutes les études et qu'un code que la spécification
n'apparie pas vaut `NA`, et ses attributs disent ce que contient chaque
colonne et quelle question elle a lue dans chaque étude. Pour une analyse
que vous publierez, préférez `qes_harmonize()`.

```r
master <- get_qes_master()
head(attr(master, "legacy_column_map"))
```

### Données harmonisées (expérimental)

`qes_harmonize()` construit un seul tableau à partir de plusieurs études,
selon une spécification livrée avec qesR : pour chaque étude et chaque
variable harmonisée (« cible »), une question, dont les codes sont mis en
correspondance un à un, avec un niveau de comparabilité, un motif pour
chaque valeur manquante et la pondération de chaque vague ; `qes_design()`
en fait un plan de sondage pour le package `survey`, et `qes_spec()` montre
la spécification. Les lignes de la spécification ont été approuvées après
une double révision automatisée sur les fichiers et documents originaux (et
non une révision humaine). Toutes les études n'ont pas une pondération
validée : le [tableau des
pondérations](https://thomasgareau.github.io/qesR/articles/fr-etudes.html#ponderations)
dit lesquelles ; les autres ont des colonnes de pondération à `NA`. Les
règles sont expérimentales et peuvent changer d'une version à l'autre :
conservez `qes_provenance(h, level = "spec")` avec vos résultats et fixez
la version de qesR utilisée.

Une variable regroupée réunit les questions d'un même sujet en une seule
colonne pour toutes les études, en indiquant la question d'où vient chaque
valeur : `vote_choice` est le choix de vote provincial (le vote déclaré,
sinon l'intention de vote), `sov_support` l'appui à la souveraineté à
travers les libellés référendaires, et `pol_interest` l'intérêt pour la
politique de 0 à 1.

```r
h <- qes_harmonize(targets = c("sov_indep", "vote_prov_recall"), lang = "fr")
v <- qes_harmonize("all", targets = "vote_choice", layout = "long", lang = "fr")
table(v$study, v$vote_choice__type)  # rappel ou intention, étude par étude
qes_spec(lang = "fr")              # quelle étude a quelle cible, et son niveau
```

Le niveau de comparabilité de la question de chaque étude pour chaque
cible (I identique, C comparable, A approximatif ; un tiret : pas de
question dans la spécification) :

<!-- coverage: start (generated by data-raw/readme_coverage.R) -->

|  | `qes2022` | `qes2018` | `qes2018_panel` | `qes2014` | `qes2012` | `qes2012_panel` | `qes_crop_2007_2010` | `qes2008` | `qes2007` | `qes2007_panel` | `qes1998` |
|---|---|---|---|---|---|---|---|---|---|---|---|
| `survey_mode` | — | — | I | — | — | — | — | — | I | — | — |
| `vote_prov_recall` | C | C | C | C | I | A | — | C | C | A | C |
| `vote_prov_intent` | A | — | A | — | — | A | C | — | — | I | — |
| `vote_prov_intent_push` | A | — | A | — | — | A | C | — | — | I | A |
| `turnout_prov_recall` | A | A | A | C | I | C | — | C | C | C | C |
| `turnout_prov_likely` | I | — | — | — | — | — | — | — | — | — | — |
| `vote_prov_intent_other` | I | — | — | — | — | — | — | — | — | — | — |
| `vote_prov_prev` | C | C | — | I | — | — | — | C | — | — | — |
| `vote_fed_recall` | C | — | — | — | I | — | — | C | A | — | — |
| `pid_prov` | C | C | — | I | I | — | — | C | C | — | — |
| `pid_fed` | I | — | — | — | C | — | — | — | — | — | — |
| `pid_prov_strength` | A | C | — | I | I | — | — | A | A | — | — |
| `sov_indep` | C | C | — | I | I | — | — | — | — | — | — |
| `sov_sovereign_country` | — | — | — | — | — | I | — | — | — | — | — |
| `sov_favour` | — | — | I | — | — | — | — | — | — | — | — |
| `lr_self` | A | C | A | I | I | — | — | — | — | — | — |
| `interest_4pt` | — | C | — | C | I | — | — | — | — | — | — |
| `sov_partnership_1995` | — | — | — | — | — | — | — | C | I | C | C |
| `interest_0_10` | A | — | — | — | — | — | — | — | I | — | — |
| `interest_election_0_10` | — | — | — | — | — | — | — | C | I | — | — |
| `interest_campaign_4pt` | — | — | — | — | — | — | — | — | — | I | — |
| `sov_partnership_1995_push` | — | — | — | — | — | — | — | C | I | C | C |
| `satis_demo_qc` | C | C | — | I | I | — | — | C | C | — | — |
| `gov_satisfaction` | C | C | — | C | I | — | — | — | — | A | — |
| `econ_retro_qc` | C | I | — | I | I | — | — | C | C | — | — |
| `attach_qc` | C | C | — | C | I | — | — | — | — | — | — |
| `attach_ca` | C | C | — | C | I | — | — | — | — | — | — |
| `identity_qc_ca` | C | — | — | C | I | — | — | C | C | — | — |
| `therm_leader_plq` | A | A | — | C | I | — | — | C | C | — | — |
| `therm_leader_pq` | A | A | — | C | I | — | — | C | C | — | — |
| `therm_leader_caq` | A | A | — | C | I | — | — | — | — | — | — |
| `therm_leader_qs` | A | A | — | C | I | — | — | C | C | — | — |
| `therm_leader_adq` | — | — | — | — | — | — | — | C | I | — | — |
| `mip_issue` | C | C | — | C | I | — | — | C | — | — | — |
| `birth_year` | I | C | — | C | C | — | — | C | C | — | — |
| `birth_month` | — | I | — | — | — | — | — | — | — | — | — |
| `age` | I | A | — | — | — | — | — | — | — | — | — |
| `age_group3` | — | — | I | — | — | C | C | C | — | C | C |
| `citizen` | I | — | — | — | — | — | — | — | — | — | — |
| `age_group6` | — | — | — | — | — | C | I | C | — | C | C |
| `gender` | C | C | C | C | I | C | C | C | C | C | C |
| `education4` | C | C | A | I | C | — | A | C | C | A | — |
| `lang_mother` | A | C | C | C | I | C | C | C | C | C | — |
| `born_canada` | I | C | — | C | C | — | — | — | — | — | — |
| `income_native` | A | — | A | C | I | — | A | A | A | A | — |
| `religion` | A | — | — | C | I | — | — | — | — | — | — |
| `region_cma3` | — | I | A | I | I | C | C | C | C | — | — |
| `lang_home` | A | C | — | C | I | — | C | C | C | C | — |
| `relig_attend` | — | A | — | A | I | — | — | C | C | — | — |
| `birthplace3` | — | C | — | C | I | — | — | — | — | — | — |
| `vote_choice` (pooled) | C | C | C | C | I | A | C | C | C | A | C |
| `sov_support` (pooled) | C | C | A | I | I | I | — | C | I | C | C |
| `pol_interest` (pooled) | A | A | — | A | A | — | — | A | I | A | — |
| `turnout` (pooled) | A | A | A | C | I | C | — | C | C | C | C |

<!-- coverage: end -->

Le [site web](https://thomasgareau.github.io/qesR/) donne la grille
complète, avec les vagues et les pondérations de chaque étude (*Couverture
par étude*), et un article qui va de `qes_harmonize()` à une estimation
pondérée.

### Harmonisation souple

`qes_decon()` renvoie un seul tableau pour toutes les études, à la manière
de cesR : des noms de colonnes simples (`education`, `income_cat`,
`religion`, `vote_choice`, `sovereignty`, `lr`...) et un concept par
colonne même quand le libellé ou les choix de réponse diffèrent, en
catégories communes larges (la scolarité en trois groupes, le revenu en
tiers des répondants de chaque étude, un vote référendaire oui ou non
quelle que soit la question). Elle échange l'exactitude contre la
couverture : ses colonnes n'ont pas de niveau de comparabilité, chacune
dit comment elle a été assouplie et d'où viennent les valeurs de chaque
étude, et `qes_harmonize()` garde les versions strictes. Les appariements
souples des questions propres aux études ont été approuvés par une double
révision automatisée sur les fichiers et les documents originaux (et non
par une révision humaine), y compris les cinq ajoutés dans la version
0.9.1 (la scolarité de 1998, l'appartenance à un syndicat et la situation
financière personnelle).

```r
d <- qes_decon(lang = "fr")
table(d$study, d$sovereignty, useNA = "ifany")
attr(d$sovereignty, "relaxed")                          # comment elle a été assouplie
attr(d, "decon_sources")[, c("column", "study", "source_var", "recode")]
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
- Les anciens noms de fonctions continuent de fonctionner et affichent une
  note unique qui nomme leur remplacement (`?qesR-deprecated`).
- Du code écrit pour qesR 0.4.4, et comment en reproduire les résultats :
  `vignette("fr-migrer-0.7", package = "qesR")`.

### Documentation, données et licences

- Site web : <https://thomasgareau.github.io/qesR/>
- La page `?qesR-fr` présente toutes les fonctions en français. Guides :
  `vignette("fr-demarrage", package = "qesR")`,
  `vignette("fr-citations", package = "qesR")`,
  `vignette("fr-migrer-0.7", package = "qesR")`.

Les données ne font pas partie du package : qesR les télécharge depuis
leurs dépôts. L'étude de 2022 est sous licence CC BY-NC 4.0 (attribution,
pas d'usage commercial) ; les autres études sont sous licence CC0 1.0.
`qes_studies()$licence` donne la licence de chaque étude, et `qes_cite()` sa
citation, avec le lien de son DOI.

### Licence

La licence MIT de qesR (fichier `LICENSE`) couvre le code du package
seulement. Les métadonnées et les effectifs de référence livrés avec qesR
gardent la licence de leur source :

- Les métadonnées de l'étude de 2022 (étiquettes de variables et de
  valeurs, texte des questions, effectifs des réponses, et les libellés,
  étiquettes et effectifs de l'harmonisation qui en sont tirés) sont tirées
  de Mahéo, Bélanger, Stephenson et Harell (2023), *2022 Quebec Election
  Study*, Harvard Dataverse, V1.1, <https://doi.org/10.7910/DVN/PAQBDR>, et
  sont sous licence
  [CC BY-NC 4.0](https://creativecommons.org/licenses/by-nc/4.0/deed.fr) :
  attribution, pas d'usage commercial. qesR les a extraites, mises en forme
  et compilées, ce qui n'implique aucune approbation de
  qesR par les auteurs.
- Les métadonnées des autres études viennent de dépôts sous licence
  CC0 1.0, et les effectifs du recensement qui servent de repères de
  validation sont adaptés de Statistique Canada selon la Licence
  ouverte de Statistique Canada.

Le fichier `COPYRIGHTS` (`system.file("COPYRIGHTS", package = "qesR")`)
donne chaque fichier, sa source et sa licence.
