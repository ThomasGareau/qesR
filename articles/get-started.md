# Getting Started with qesR

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-demarrage.md)*

qesR loads the Quebec Election Studies and related Quebec election
surveys into R by study code. Each code points to one Dataverse deposit,
pinned to a dataset version and to one original data file that is
checked against its md5 checksum before use. The catalog of studies,
their documents, their citations and the description of every variable
of every study ship with the package.

This page goes from a study code to a weighted estimate. Every chunk on
it runs without a network connection: it uses the catalog, the shipped
metadata and `qes_demo`, a small synthetic study that ships with qesR.
The chunks that would download a real study are shown but not run.

## Install

Install qesR from GitHub; once it is accepted on CRAN,
`install.packages("qesR")` will do.

``` r

# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
# once accepted on CRAN:
# install.packages("qesR")
```

``` r

library(qesR)
```

## Which studies are there?

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
lists every study, with its year, family, design, target population,
licence, DOI and the dataset version it is pinned to.

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

Not every study is a Quebec Election Study (family `qes`): the Durand
panels, the CROP polls and the 1998 polls have their own designs and
populations.
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
lists the codebooks, questionnaires and reports of each study.

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

## Load a study

[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
returns the data. It writes nothing into your workspace, so assign the
result yourself. The demonstration study is read from the package; a
real study is downloaded once per session (or kept between sessions with
`options(qesR.cache = "disk")`).

``` r

demo <- get_qes("qes_demo", quiet = TRUE)
#> get_qes() returns its result and no longer assigns it into your workspace by default. Write `qes_demo <- get_qes(...)`, or pass `assign_global = TRUE`. This note is shown once per session.
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

Columns keep the codes and labels of the file (`haven` labelled
columns). Codes such as “don’t know” and “refused” are values until you
ask
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
to set them to `NA`; it records what it changed.

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

## Codebooks, questions and search, offline

The codebook of every study ships with qesR: variable labels, question
text in English and French from the deposited questionnaires (for the
2022 study, its bilingual codebook), value labels and missing codes.

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
gives the exact wording, in either language:

``` r

qes_question("qes2014", "Q19", lang = "en")$question
#> [1] "If there were a referendum on independence that asked whether Quebec should be an independent country, would you vote YES or NO?"
qes_question("qes2014", "Q19", lang = "fr")$question
#> [1] "Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON?"
```

[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
searches every study at once, ignoring case and accents:

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

The 2022 study is licensed CC BY-NC 4.0. Its codebook ships with qesR
too, under that licence (attribution, no commercial use), not under
qesR’s MIT licence, which covers the package code only. It is derived
from Mahéo, Bélanger, Stephenson and Harell (2023), *2022 Quebec
Election Study*, Harvard Dataverse,
<https://doi.org/10.7910/DVN/PAQBDR>; a printed codebook of `qes2022`
repeats this notice, which codebooks,
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
and
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
results that include `qes2022` also keep in their attribute
`licence_notice`, and `system.file("COPYRIGHTS", package = "qesR")`
lists the files.

## A weighted estimate

What share of Quebec adults would have voted YES in a referendum on
independence, just after the 2014 election? Question Q19 of `qes2014`
asks it (its wording is above). A correct estimate states four things,
and the steps above give each of them.

1.  **The population.** The catalog gives each study’s design and target
    population: `qes2014` is a post-election survey that targets the
    Quebec adult population, nothing wider. Its respondents come from an
    opt-in online panel, not from a probability sample.
2.  **The weight.** `POND` is the weight of `qes2014`. Its technical
    report (`qes_docs("qes2014")`) says it adjusts the sample to the
    latest census by sex, age, region and language. The estimate
    describes the adult population only to the extent that this
    adjustment corrects the panel.

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

3.  **The question and its codes.** Q19 codes YES as 1, NO as 2, “don’t
    know” as 8 and “refused” as 9 (the long codebook above).
4.  **The denominator.**
    [`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
    set codes 8 and 9 to `NA`. The share below is among respondents who
    answered YES or NO, and says so.

On the demonstration study, which copies these names and codes:

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

The demonstration data are synthetic, so these numbers describe no
population; they show the computation. On the real file the same lines
give 34.8% YES among the 1,353 respondents who answered YES or NO (34.1%
unweighted), weighted to the Quebec adult population of 2014.

``` r

qes2014 <- qes_missing(get_qes("qes2014"))
answered <- qes2014$Q19 %in% c(1, 2)
yes <- qes2014$Q19[answered] == 1
weight <- qes2014$POND[answered]
sum(weight * yes) / sum(weight)
```

This is a point estimate. `qes2014` is a non-probability online panel:
its technical report (`qes_docs("qes2014")`) gives no margin of error,
and a design-based standard error, from the `survey` package for
instance, would not have its usual meaning. Compute each estimate within
one study: studies differ in design, population and wording, and their
weights are on their own scales.

## Harmonized data across studies (experimental)

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
builds one data frame from several studies, one column per harmonized
variable (“target”), from a reviewed specification only.
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
shows which question of each study feeds a target and how comparable it
is to the target’s anchor question:

``` r

xw <- qes_spec("crosswalk", targets = "sov_indep", lang = params$lang)
xw[, c("study", "wave", "source_var", "grade", "weight_var")]
#>     study wave        source_var      grade         weight_var
#> 1 qes2012 post               q52  identical               pond
#> 2 qes2014 post               Q19  identical               POND
#> 3 qes2018 post               q26 comparable               pond
#> 4 qes2022  cps cps_qc_referendum comparable cps_weight_general
```

A row is applied once a reviewer has signed it off. In specification
4.1.0 every row is signed off, after an automated double review against
the original files and documents (not a human review); a row still in
review would give `NA` with the reason `not_reviewed`, unless
`include_draft = TRUE`. The recommended weights of `qes1998`,
`qes2007_panel`, `qes2012_panel` and the CROP polls still need review,
so their weights are `NA`. On the demonstration study, which stands in
for `qes2014`:

``` r

h <- qes_harmonize("qes_demo", targets = c("sov_indep", "vote_prov_recall"),
                   missing = "reasons", quiet = TRUE, lang = params$lang)
h[1:4, c("study", "eligible_voter", "sov_indep", "sov_indep__na", "weight_post")]
#>      study eligible_voter sov_indep sov_indep__na weight_post
#> 1 qes_demo           TRUE        No          <NA>   1.2593217
#> 2 qes_demo           TRUE        No          <NA>   0.6685168
#> 3 qes_demo           TRUE       Yes          <NA>   1.3786438
#> 4 qes_demo           TRUE       Yes          <NA>   0.9836078
attr(h, "qes_weight_guide")[, c("target", "study", "weight_column", "weight_var")]
#>             target    study weight_column weight_var
#> 1 vote_prov_recall qes_demo   weight_post       POND
#> 2        sov_indep qes_demo   weight_post       POND
```

Every missing value has a reason (`sov_indep__na`). `eligible_voter`
says whether the respondent could vote in the election, and each wave’s
recommended weight has mean 1: these questions were asked after the
election, so `weight_post` is their weight, as the weight guide says.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
hands the data to the `survey` package:

``` r

if (requireNamespace("survey", quietly = TRUE)) {
  d <- qes_design(h, weight = "weight_post")
  survey::svymean(~sov_indep, d, na.rm = TRUE)
}
#>                                          mean     SE
#> sov_indepYes                          0.41584 0.0718
#> sov_indepNo                           0.58416 0.0718
#> sov_indepWould not vote / would spoil 0.00000 0.0000
```

The standard error treats the weighted sample as a probability sample;
for an opt-in panel such as qes2014, read it as a rough guide only (see
[`?qes_design`](https://thomasgareau.github.io/qesR/reference/qes_design.md)).

A pre-election question (vote intention) takes `weight_pre` instead, and
a study’s two waves can have different weights:
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
asks you to choose when the targets need different ones. The reference
of every target, generated from the specification, is
[`vignette("harmonization-reference", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md).

## Provenance and citations

Data returned by
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
records which file it was read from.
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
returns that record, and
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
cites qesR and the datasets you used (see
[`vignette("citations", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/citations.md)).

``` r

qes_provenance(demo)
#> qes_demo: file 0 (qes_demo.sav), synthetic data shipped with qesR. md5
#> e956e315800690cb0894c86ed85c8bea, verified. 60 rows, 11 columns. Retrieved on
#> 2026-09-29 16:22:45 UTC (local_demo). Read with haven::read_sav(user_na =
#> TRUE), haven 2.5.5. Licence: CC0 1.0. qesR catalog 2.3.0.
#> 
#> as.data.frame() gives every column.
qes_cite("qes2014")
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.7.1, https://github.com/ThomasGareau/qesR"                
#> [2] "Bélanger, Éric; Nadeau, Richard, 2023, \"Étude électorale québécoise 2014\", https://doi.org/10.5683/SP3/64F7WR, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw=="
```

[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
saves the original files themselves (data file and documents),
md5-checked, in a folder you choose:

``` r

dir <- file.path(tempdir(), "qes_originals")
dir.create(dir)
qes_download("qes2014", path = dir, what = c("data", "docs"))
```

## Messages and errors

Messages, warnings and errors can be shown in French with
`options(qesR.lang = "fr")`; the data returned is the same in both
languages. Errors have classes, so a script can react to them without
reading their text:

``` r

tryCatch(
  get_qes("QES 2014"),
  qesR_error_unknown_study = function(e) e$suggestions
)
#> [1] "qes2014"
```

## Coming from qesR 0.4.4

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
and the other functions of qesR 0.4.4 keep working; since qesR 0.7.0 the
master is rendered from the harmonization engine. What changed, and how
to keep results reproducible, is in
[`vignette("migrating-0.7", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md).
