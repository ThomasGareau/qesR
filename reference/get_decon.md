# Create a Prepared Non-Exhaustive qesR Dataset

Builds a small teaching dataset with 19 standardized columns from one
Quebec Election Study of qesR 0.4.4.

## Usage

``` r
get_decon(srvy = "qes2022", assign_global = FALSE, quiet = FALSE)
```

## Arguments

- srvy:

  A qesR survey code. Defaults to `"qes2022"`. Codes are trimmed and
  case-insensitive. The 11 studies of qesR 0.4.4 are available, and
  `"qes_demo"`, the synthetic demonstration study; the 1998 firms' own
  files (`qes1998_crop`, `qes1998_createc`) raise an error of class
  `qesR_error_input` (their respondents are in `qes1998`).

- assign_global:

  If TRUE, also assign the result as `decon` into the environment
  `get_decon()` was called from (the global environment only when called
  at top level). Defaults to FALSE.

- quiet:

  If TRUE, suppress informational output while downloading.

## Value

A data frame with the 19 columns above, returned visibly, with the
attributes `timing` (what `turnout`, `votechoice` and `votechoice_text`
hold: `"pre"`, `"post"` or `NA` where the column has no value),
`source_map` (`qes_code`, `column`, `source_variable`, `target`,
`grade`, `status` of the crosswalk row), `legacy_na_columns` (as for
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md))
and `qes_provenance` (the file read; see
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)).

## Details

`get_decon()` is soft-deprecated since qesR 0.7.0: it keeps working and
will not be removed, and a message names its replacement once per
session. The same variables, with a reason for every missing value and a
grade for every study's question, come from
`qes_harmonize(srvy, targets = "decon")`. Both apply only the crosswalk
rows signed off by a reviewer (status `stable`); a column whose question
is in a row still in review is `NA` (reason `not_reviewed` in
`attr(, "legacy_na_columns")`, which says why the row is held), and
`include_draft = TRUE` in
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applies those rows too. The rows were signed off after an automated
double review against the original files and documents (not a human
review). A row that nobody has reviewed yet is never read by
`get_decon()` or
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md),
even for their columns' targets: the legacy columns stay as they were.

For one data frame of every study with plain, cesR-style columns,
harmonized in a relaxed way (one concept per column even where the
questions differ, in coarse common categories), see
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md).
It is a different dataset: `get_decon()` keeps the 19 columns of qesR
0.4.4, one study at a time, and its values do not change.

`get_decon()` returns the data and assigns nothing unless
`assign_global = TRUE`: write `decon <- get_decon("qes2022")`. The first
call in a session that leaves `assign_global` unset prints a one-time
note about this change from qesR 0.4.4.

## Columns

`qes_code`, then `citizenship`, `yob` (year of birth), `age`, `gender`,
`province_territory`, `education`, `political_interest`, `turnout`,
`votechoice`, `votechoice_text`, `party_best`, `partylean`, `fed_pid`,
`prov_pid`, `ideology`, `income`, `religion` and `born_canada`. Since
qesR 0.7.0 each column is rendered from a target of the harmonization
engine
([`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)):
categorical columns are factors with the target's English levels (the
same levels in every study), numbers stay numbers, and `income` and
`religion` are each study's own categories as text (the `qes2022` income
amount stays a number). `turnout` and `votechoice` are the reported
turnout and vote, asked after the election, except for `qes2022`, where
they are the likelihood of voting and the vote intention asked during
the campaign, as in qesR 0.4.4; `attr(, "timing")` says which (`"post"`
or `"pre"`).

## What changed in 0.7.0

The columns hold the targets of the harmonization spec (see
`attr(, "source_map")`): the factor levels are the targets' English
labels rather than each file's labels (`"Man"` rather than `"A man"`),
`education` has four groups, `province_territory` is `"Quebec"`, `yob`
is a number, `income` and `religion` are text (the `qes2022` income
amount stays a number), and codes the spec does not map (such as the
`-99` of `qes2022`) are `NA`. `turnout` and `votechoice` are filled for
every study that asked the reported vote (`qes2018` from `q5` and `q6`),
and `get_decon("qes1998")` returns real rows (its columns from the spec
are `NA` until its rows are signed off). `party_best` and `partylean`
are `NA` everywhere (their 0.4.4 sources were other questions in each
study). Only signed-off crosswalk rows are applied (see above).

## En français

`get_decon()` construit un petit jeu de données d'enseignement de 19
colonnes à partir d'une étude de qesR 0.4.4. Depuis qesR 0.7.0, elle est
obsolète (dépréciation douce) : elle continue de fonctionner et ne sera
pas retirée, et un message nomme son remplacement une fois par session,
`qes_harmonize(srvy, targets = "decon")`. Les deux n'appliquent que les
lignes de correspondance approuvées par un réviseur (statut `stable`) ;
une colonne dont la question est dans une ligne encore en révision vaut
`NA` (motif `not_reviewed` dans `attr(, "legacy_na_columns")`, qui dit
pourquoi la ligne est retenue), et `include_draft = TRUE` dans
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applique aussi ces lignes. Les lignes ont été approuvées après une
double révision automatisée sur les fichiers et documents originaux (et
non une révision humaine) ; une ligne que personne n'a encore révisée
n'est jamais lue. Chaque colonne est rendue à partir d'une cible du
moteur d'harmonisation : facteurs aux niveaux anglais des cibles,
nombres, et texte pour `income` et `religion` (le montant du revenu de
`qes2022` reste un nombre). `turnout` et `votechoice` sont la
participation et le vote déclarés après l'élection, sauf pour `qes2022`,
où ce sont la probabilité de voter et l'intention de vote pendant la
campagne, comme dans qesR 0.4.4 ; `attr(, "timing")` l'indique (`"post"`
ou `"pre"`). Pour un seul tableau de toutes les études, aux colonnes
simples à la manière de cesR et harmonisées de façon souple, voir
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
; `get_decon()` garde ses 19 colonnes et ses valeurs.

## See also

[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
for every study in one data frame, harmonized in a relaxed way;
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
# the synthetic demonstration study, offline
decon <- get_decon("qes_demo", quiet = TRUE)
#> `get_decon()` is soft-deprecated; use `qes_harmonize(srvy, targets = "decon")`. It keeps working and will not be removed.
#> Values changed in qesR 0.7.0: get_decon() is now rendered from the harmonization engine (qes_harmonize(srvy, targets = "decon")), with the crosswalk rows signed off by a reviewer, so turnout and votechoice are the reported turnout and vote in every study that asked them (for qes2022, still the campaign-period likelihood of voting and vote intention), and codes the spec does not map are NA (the -99 of qes2022); attr(, "source_map") gives the question and target behind each column, attr(, "timing") what turnout and votechoice hold ("post" or "pre"), and NEWS lists the changes. Results of an earlier version are reproducible by installing it (for 0.4.4: remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")). This note is shown once per session.
#> In get_decon(), categorical columns are factors with the English levels of the harmonized targets, party_best and partylean are NA in every study, and turnout and votechoice are the reported turnout and vote where the study asked them (NA for the CROP polls), except for qes2022 (the campaign-period likelihood of voting and vote intention; attr(, "timing")). This note is shown once per session.
#> get_decon() returns its result and no longer assigns it into your workspace by default. Write `decon <- get_decon(...)`, or pass `assign_global = TRUE`. This note is shown once per session.
head(decon)
#>   qes_code citizenship  yob age gender province_territory education
#> 1 qes_demo        <NA> 1958  56  Woman             Quebec      <NA>
#> 2 qes_demo        <NA> 1951  63  Woman             Quebec      <NA>
#> 3 qes_demo        <NA> 1939  75    Man             Quebec      <NA>
#> 4 qes_demo        <NA> 1933  81    Man             Quebec      <NA>
#> 5 qes_demo        <NA> 1961  53    Man             Quebec      <NA>
#> 6 qes_demo        <NA> 1976  38  Woman             Quebec      <NA>
#>   political_interest turnout votechoice votechoice_text party_best partylean
#> 1                  3     Yes        CAQ            <NA>       <NA>      <NA>
#> 2                  3     Yes         QS            <NA>       <NA>      <NA>
#> 3                  7     Yes        PLQ            <NA>       <NA>      <NA>
#> 4                 10     Yes         PQ            <NA>       <NA>      <NA>
#> 5                  7     Yes         QS            <NA>       <NA>      <NA>
#> 6                 10     Yes        CAQ            <NA>       <NA>      <NA>
#>   fed_pid prov_pid ideology income religion born_canada
#> 1    <NA>     <NA>        0   <NA>     <NA>        <NA>
#> 2    <NA>     <NA>        3   <NA>     <NA>        <NA>
#> 3    <NA>     <NA>        7   <NA>     <NA>        <NA>
#> 4    <NA>     <NA>       NA   <NA>     <NA>        <NA>
#> 5    <NA>     <NA>        0   <NA>     <NA>        <NA>
#> 6    <NA>     <NA>        6   <NA>     <NA>        <NA>
attr(decon, "source_map")[, c("column", "source_variable", "target")]
#>                column source_variable              target
#> 1            qes_code            <NA>                <NA>
#> 2         citizenship            <NA>                <NA>
#> 3                 yob            QAGE          birth_year
#> 4                 age            QAGE          birth_year
#> 5              gender           QSEXE              gender
#> 6  province_territory            <NA>                <NA>
#> 7           education            <NA>                <NA>
#> 8  political_interest             Q28        interest_4pt
#> 9             turnout              Q2 turnout_prov_recall
#> 10         votechoice              Q3    vote_prov_recall
#> 11    votechoice_text            <NA>                <NA>
#> 12         party_best            <NA>                <NA>
#> 13          partylean            <NA>                <NA>
#> 14            fed_pid            <NA>                <NA>
#> 15           prov_pid            <NA>                <NA>
#> 16           ideology             Q32             lr_self
#> 17             income            <NA>                <NA>
#> 18           religion            <NA>                <NA>
#> 19        born_canada            <NA>                <NA>

# the replacement
h <- qes_harmonize("qes_demo", targets = "decon", quiet = TRUE)
names(h)
#>  [1] "study"                  "year"                   "election_date"         
#>  [4] "family"                 "study_design"           "target_population"     
#>  [7] "waves"                  "qes_id"                 "subsample"             
#> [10] "stratum"                "source_row"             "survey_mode"           
#> [13] "interview_date"         "days_to_election"       "eligible_voter"        
#> [16] "vote_prov_recall"       "vote_prov_intent"       "turnout_prov_recall"   
#> [19] "lr_self"                "pid_prov"               "interest_4pt"          
#> [22] "birth_year"             "age"                    "citizen"               
#> [25] "turnout_prov_likely"    "vote_prov_intent_other" "pid_fed"               
#> [28] "interest_0_10"          "gender"                 "education4"            
#> [31] "born_canada"            "income_native"          "religion"              
#> [34] "weight_pre"             "weight_post"            "weight_pre_var"        
#> [37] "weight_post_var"       
```
