# The merged file (get_qes_master)

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-donnees-fusionnees.md)*

[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
stacks 11 Quebec Election Studies, from 1998 to 2022, in one data frame:
one row per respondent of each study, and the same 30 columns for every
study (vote choice, turnout, sovereignty, party identification,
ideology, political interest and the usual demographics, with the
study’s code, year and identifiers). Its layout is fixed: the names,
order and types of the 30 columns do not change, and columns added later
come after them. Code written against it keeps working.

## When to use it, and when to prefer `qes_harmonize()`

The merged file is a quick, flat overview: one call, one data frame, one
column per concept. To keep that shape, some columns put different
questions in one column, and it has no column that says how comparable
each study’s answer is.

Prefer
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
for an analysis you will publish. It keeps apart what the merged file
joins (a reported vote and a vote intention, the sovereignty wordings, a
four-point and a 0-10 interest scale), gives each study’s question a
comparability grade and each missing value a reason, and carries each
wave’s weight;
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
then makes a survey design of it. [How harmonization
works](https://thomasgareau.github.io/qesR/articles/harmonization.md)
explains it, and the `target` column of
`attr(master, "legacy_column_map")` names the harmonized variable behind
each column of the merged file.

Use
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
for the questions of one study that neither has.

## Build the merged file

The synthetic demonstration study runs offline:

``` r

library(qesR)
demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
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

The real studies are read from their original files:

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

By default, and with `surveys = "all"`,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
builds all 11 studies. The 1998 firm files `qes1998_crop` and
`qes1998_createc` are not in it (their respondents are already in
`qes1998`); read them with
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md).

## What each column holds

- Each study is harmonized by
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  and each column is built from the harmonized variables (“targets”) it
  needs. A code the harmonization rules do not map is `NA`, never passed
  through. `attr(master, "source_map")` gives, for every column of every
  study, the question read, its target and its comparability grade.
- Every respondent of every file is kept: there is no de-duplication and
  no removal of empty rows. Panel and cross-section respondents stay
  separate rows.
- `vote_choice` and `turnout` are the reported vote and turnout in every
  study; the CROP polls asked only vote intentions (in `vote_intent`).
  `sovereignty_support` is the referendum on an independent country
  only, and `party_best` and `party_lean` are `NA` everywhere.
  `attr(master, "legacy_na_columns")` lists each column and study that
  is `NA` throughout, with the reason and, in `basis`, why in words
  ([`?get_qes_master`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  lists the causes).
- `vote_choice_timing` and `sovereignty_item` say what `vote_choice` and
  `sovereignty_support` hold in each study.
- Variables that share a name across studies are not stacked: they often
  hold different questions. Read those items from each study with
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
`age_group`, `education`).

## Weights

`survey_weight` is each study’s own weight, on its own scale, and is not
reviewed: do not pool weighted estimates across studies with it.
`weight_pre` and `weight_post` are the recommended weights of the
harmonization rules, `NA` where a study’s weight still needs review.
Compute each estimate within one study.

## Save the merged file

`save_path` writes a UTF-8 CSV file or an RDS file, and next to it
`<stem>_provenance.csv`, the record of the file read for each study.

``` r

get_qes_master(save_path = file.path(tempdir(), "qes_master.csv"), strict = FALSE)
get_qes_master(save_path = file.path(tempdir(), "qes_master.rds"), strict = FALSE)
```

For code written for an earlier version of qesR, and to reproduce its
results, see [the upgrading
guide](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md).
