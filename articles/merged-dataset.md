# The legacy merged file

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-donnees-fusionnees.md)*

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
stacks the Quebec Election Studies of qesR 0.4.4 in one data frame, with
the 30 harmonized columns of 0.4.4: same names, order and types. It is
the fixed legacy schema, kept stable for code written for 0.4.4.

``` r

library(qesR)
qes_studies()$study
#>  [1] "qes2022"            "qes2018"            "qes2018_panel"     
#>  [4] "qes2014"            "qes2012"            "qes2012_panel"     
#>  [7] "qes_crop_2007_2010" "qes2008"            "qes2007"           
#> [10] "qes2007_panel"      "qes1998"            "qes1998_crop"      
#> [13] "qes1998_createc"
```

By default, and with `surveys = "all"`,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
builds the 11 studies of qesR 0.4.4. The 1998 firm files `qes1998_crop`
and `qes1998_createc` are not in the master (their respondents are
already in `qes1998`); read them with
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
as
[`?get_qes_master`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
explains.

## Build merged data

The synthetic demonstration study runs offline:

``` r

demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
#> Values changed in qesR 0.7.0: get_qes_master() is now rendered from the harmonization engine (qes_harmonize()), so vote_choice and turnout are the reported vote and turnout in every study that asked them (the CROP polls asked only an intention: vote_intent), and codes the spec does not map are NA (language, the mother tongue, is NA for respondents who gave two first languages); attr(, "legacy_column_map") says what each column holds and NEWS lists the changes. Results of an earlier version are reproducible by installing it (for 0.4.4: remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")). This note is shown once per session.
#> get_qes_master() no longer appends the 70 columns that qesR 0.4.4 built by stacking variables that share a name across studies; attr(, "removed_columns") lists them. Read those items from each study with get_qes(). This note is shown once per session.
#> get_qes_master() returns its result and no longer assigns it into your workspace by default. Write `qes_master <- get_qes_master(...)`, or pass `assign_global = TRUE`. This note is shown once per session.
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

The real studies are read from their pinned original files. This chunk
runs when the website is built, through qesR’s download cache:

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

## How it is built

- Since qesR 0.7.0 the master is rendered from the harmonization engine:
  each study is harmonized by
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
  and each column is rendered from the targets it needs, as the renderer
  table of the spec says (`qes_spec("spec")$tables$legacy`). A code the
  spec does not map is `NA`, never passed through.
  `attr(master, "source_map")` gives, for every column of every study,
  the question read, its target and its comparability grade.
- Every respondent of every file is kept: there is no de-duplication and
  no removal of empty rows. Panel and cross-section respondents stay
  separate rows.
- `vote_choice` and `turnout` are the reported vote and turnout in every
  study; the CROP polls asked only vote intentions (in `vote_intent`).
  `sovereignty_support` is the referendum on an independent country
  only, and `party_best` and `party_lean` are `NA` everywhere.
  `attr(master, "legacy_na_columns")` lists each column and study that
  is `NA` throughout, with the reason, the rule behind it where there is
  one (`cause`: `reported_vote_only`, `independence_question_only`,
  `no_valid_source` or `not_harmonized_yet`) and, in `basis`, why in
  words.
- `vote_choice_timing` and `sovereignty_item` say what `vote_choice` and
  `sovereignty_support` hold in each study.
- qesR 0.4.4 also appended 70 columns by stacking variables that share a
  name across studies; they mixed different questions and are no longer
  built (`attr(master, "removed_columns")`). Read those items from each
  study with
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

`attr(master, "legacy_column_map")` says what each column means and
flags the columns that mix instruments (`political_interest`,
`age_group`, `education`). `survey_weight` is each study’s own weight on
its own scale: do not pool weighted estimates across studies without
rescaling.

Results from qesR 0.4.4 are reproducible only with that version
(`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`). 0.5.0
and 0.6.0 were development versions and were never released: a result
computed with one of them is reproducible by installing the commit it
was built from, which `packageDescription("qesR")$RemoteSha` records for
an installation from GitHub
(`remotes::install_github("ThomasGareau/qesR", ref = "<sha>")`).

## Save merged data

`save_path` writes a UTF-8 CSV file or an RDS file, and next to it
`<stem>_provenance.csv`, the record of the file read for each study.

``` r

get_qes_master(save_path = file.path(tempdir(), "qes_master.csv"), strict = FALSE)
get_qes_master(save_path = file.path(tempdir(), "qes_master.rds"), strict = FALSE)
```
