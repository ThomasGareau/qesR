# Upgrading from qesR 0.4.4

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-migrer-0.7.md)*

This guide is for code written for qesR 0.4.4, such as the replication
scripts of a published paper. Every function of 0.4.4 keeps its name and
its arguments, but one change can stop that code:
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
no longer creates the object for you (first row below). What else
changes is what some of it produces, which depends on what the code
uses:

| Your code uses | Now | What to do |
|----|----|----|
| `get_qes("qes2018")` alone, then the object `qes2018` | error “object ‘qes2018’ not found” | write `qes2018 <- get_qes("qes2018")` ([below](#assign-the-result-yourself)) |
| raw codes from [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md) ([`as.numeric()`](https://rdrr.io/r/base/numeric.html), ranges of valid codes) | same numbers | nothing |
| label text from [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md) ([`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html), text columns) | some text changes | [check the labels you use](#get_qes-the-same-codes-read-from-the-original-files) |
| [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md) | values change | [read the column table](#get_qes_master-what-changed), or keep 0.4.4 to reproduce |
| [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md) | values change; one-time notice | as for the master |
| the other helpers of 0.4.4 | same results as their replacements; one-time notice | nothing, or [rename them](#old-names-new-names) |

Every chunk on this page runs offline, on the synthetic study
`qes_demo`. The complete list of changes is in the Changelog
(`news(package = "qesR")`).

``` r

library(qesR)
```

## Before you upgrade: record the version

A published result should say which qesR produced it. Before upgrading,
record the version you have, and keep it with the results:

``` r

packageVersion("qesR")
packageDescription("qesR")$RemoteSha   # the commit, when installed from GitHub
```

To reproduce a 0.4.4 result, install 0.4.4, preferably in a separate
library or an renv project so that new work can use the current version:

``` r

remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")
```

qesR 0.5.0 and 0.6.0 were development versions and were never released:
a result computed with one of them is reproduced by installing the
commit it was built from, which `packageDescription("qesR")$RemoteSha`
records for an installation from GitHub
(`remotes::install_github("ThomasGareau/qesR", ref = "<commit>")`).

## Assign the result yourself

In 0.4.4, `get_qes("qes2018")` created an object `qes2018` in your
workspace. It now returns the data and writes nothing: assign it.

``` r

qes_demo <- get_qes("qes_demo", quiet = TRUE)
#> get_qes() returns its result and no longer assigns it into your workspace by default. Write `qes_demo <- get_qes(...)`, or pass `assign_global = TRUE`. This note is shown once per session.
dim(qes_demo)
#> [1] 60 11
```

A script that calls `get_qes("qes2018")` on its own line and then uses
`qes2018` stops with “object ‘qes2018’ not found”. Look also for such
calls inside [`tryCatch()`](https://rdrr.io/r/base/conditions.html) or
[`try()`](https://rdrr.io/r/base/try.html): there the error is caught,
and the part of the script that used the object is skipped without a
visible error.

`assign_global = TRUE` still assigns, into the environment the function
is called from: the global environment at top level, the function’s own
frame inside a function. It still assigns the codebook too, as
`<code>_codebook`.

``` r

f <- function() {
  get_qes("qes_demo", assign_global = TRUE, quiet = TRUE)
  c(data = exists("qes_demo", inherits = FALSE),
    codebook = exists("qes_demo_codebook", inherits = FALSE))
}
f()
#>     data codebook 
#>     TRUE     TRUE
```

## `get_qes()`: the same codes, read from the original files

[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads each study’s original SPSS or Stata file, checked before use.
Column names, codes and [`is.na()`](https://rdrr.io/r/base/NA.html)
counts are those of 0.4.4 for all 11 studies, so code that works on
codes ([`as.numeric()`](https://rdrr.io/r/base/numeric.html), ranges of
valid codes, `%in%`) gives the same numbers and the same N. Text changes
where the old conversion damaged it or took labels from elsewhere:

- accents are repaired (1,805 cells in `qes2018`, 2,517 in `qes2022`);
- `qes2012` labels keep their original case and length (0.4.4 lowercased
  them and cut them at 80 characters);
- the `qes2022` interview dates are date-times, not text;
- codes are stored as doubles instead of integers.

[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html)
and anything else built from label text can therefore give different
text. The data record which file they came from:

``` r

qes_provenance(qes_demo)
#> qes_demo: file 0 (qes_demo.sav), synthetic data shipped with qesR. md5
#> e956e315800690cb0894c86ed85c8bea, verified. 60 rows, 11 columns. Retrieved on
#> 2026-10-01 01:20:22 UTC (local_demo). Read with haven::read_sav(user_na =
#> TRUE), haven 2.5.5. Licence: CC0 1.0. qesR catalog 2.4.1.
#> 
#> as.data.frame() gives every column.
```

## `get_qes_master()`: what changed

The master keeps its 30 documented columns, in the same order and with
the same types; new columns are appended after them and are never
removed. Its values changed in two ways.

**Nothing is dropped, and nothing is stacked by name:**

- every respondent of every file is kept: 40,987 rows. 0.4.4 dropped 380
  `qes2007_panel` respondents and 1 CROP respondent as duplicates, and,
  in a session with French messages, lost 1,208 `qes2007_panel` and 616
  `qes2018_panel` rows while reading them;
- the 70 columns 0.4.4 appended by stacking variables that share a name
  across studies are removed, because they held different questions in
  different studies (`attr(, "removed_columns")` lists them);
- `qes2018` `turnout` is 0 rather than `NA` for the 336 respondents who
  said they did not vote (`q5` codes 1 and 3): its valid N goes from
  2,303 to 2,639 and its mean from 0.96 to 0.84;
- `qes_name_en` uses each study’s own title for the Durand panels, the
  CROP polls and the 1998 polls (for example “Panel Survey on the 2018
  Quebec Election”): select studies with `qes_code`, not with the name;
- label text is fixed: `qes2012` `religion` keeps its original case,
  CROP `income` and `province_territory` read their accented letters
  correctly (“20 000 \$ à 39 999 \$”, “Quebec”), and the `qes2022`
  `interview_*` columns lose their trailing `.000`.

**Each column is built from the harmonized variables.** The master is
built by
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
for all 11 studies: each column comes from the harmonized variables
(“targets”) it needs, and a code the harmonization rules do not map is
`NA`.

`qes1998`, the Durand panels and the CROP polls have no usable weight
yet: their answers are filled, but `weight_pre` and `weight_post` are
`NA` there, and `survey_weight` keeps each study’s own weight, as in
0.4.4. The [merged
file](https://thomasgareau.github.io/qesR/articles/merged-dataset.html)
page describes every column. The columns that change estimates:

| Column | Now | Changed in |
|----|----|----|
| `vote_choice` | The vote reported after the election, in every study that asked it (0.4.4 used the campaign intention for `qes2022` and `qes1998`). Non-voters, spoiled ballots, “don’t know” and refusals are `NA`: the categories “Did not vote / None” and “Don’t know / Refused” are gone, and `turnout` says who voted. The CROP polls asked only intentions: `NA`, with the intention in the appended `vote_intent`. | every study |
| `turnout` | Reported turnout, 1 or 0 (`qes2022` from its post-election wave; the 2018 panel from its turnout question). | `qes2022`, `qes2018`, `qes2018_panel`, `qes2007_panel` |
| `sovereignty_support`, `sovereignty` | The referendum on Quebec becoming an independent country only. Other wordings are `NA`; the 1995 sovereignty-partnership question is in the appended `sov_partnership_1995`. | the studies without that question |
| `ideology` | 0-10 with its end points: 0.4.4 lost the 0 and 10 answers of `qes2014` (142 respondents) and found no source in `qes2018`. | `qes2014`, `qes2018` |
| `political_interest` | The four-point questions as 10, 7, 3 and 0; 0.4.4 left the 2018 codes 1 to 4 unconverted and reversed. Mixes a 4-point and a 0-10 question across studies. | `qes2018`, `qes2012_panel` |
| `language` | The mother tongue; 0.4.4 used the interview language in `qes2014` and the interface language in `qes2022`. Two first languages are `NA`. | `qes2014`, `qes2022`, `qes2007`, `qes2007_panel`, `qes2018_panel` |
| `income`, `religion` | Each study’s own categories; `-99` and raw codes are `NA`; `qes2012` income is filled. | `income`: 9 studies; `religion`: `qes2012`, `qes2014`, `qes2018`, `qes2022` |
| `age_group`, `education`, `born_canada` | Categories corrected: six age bands in the 2007 and 2012 panels, *maîtrise* as University, people born elsewhere in Canada as born in Canada, and similar cases listed in NEWS. | a few studies each |
| `provincial_pid` | Filled where the study asked it (0.4.4 found a source only in `qes2022`). | `qes2007`, `qes2008`, `qes2012`, `qes2014`, `qes2018` |
| `party_best`, `party_lean` | `NA` everywhere: each study’s source was another question. | `party_best`: `qes2022`, `qes2018`, `qes2014`, `qes2012`, `qes2007`; `party_lean`: every study but `qes1998` |
| `survey_weight` | Unchanged: each study’s own weight, on its own scale. Do not pool weighted estimates across studies with it. | none |

Three consequences for pooled analyses: an estimate that pooled
`vote_choice` across studies mixed intentions and reported votes in
0.4.4, and one that pooled `ideology` across 2012, 2014 and 2022 lost
the extreme answers of 2014. Estimates of that kind move most. Third, a
filter on non-missing values now keeps studies that 0.4.4 left empty:
columns that were `NA` for a whole study are filled, `ideology` for
`qes2018`, `provincial_pid` for `qes2007`, `qes2008`, `qes2012`,
`qes2014` and `qes2018`, `income` for `qes2012` and `year_of_birth` for
`qes2022`. A complete-case sample such as
`subset(master, !is.na(vote_choice) & !is.na(ideology))` now contains
`qes2018`; if the script also reads `qes2018` on its own, those
respondents are counted twice. Select studies explicitly, with
`qes_code %in% c(...)`, and compare `table(sample$qes_code)` for the
analysis sample with the counts you published.

The attributes of the master say what each column holds, which studies’
values changed and why a column is empty in a study:

``` r

master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
#> Values changed in qesR 0.7.0: get_qes_master() is now rendered from the harmonization engine (qes_harmonize()), so vote_choice and turnout are the reported vote and turnout in every study that asked them (the CROP polls asked only an intention: vote_intent), and codes the spec does not map are NA (language, the mother tongue, is NA for respondents who gave two first languages); attr(, "legacy_column_map") says what each column holds and NEWS lists the changes. Results of an earlier version are reproducible by installing it (for 0.4.4: remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")). This note is shown once per session.
#> get_qes_master() no longer appends the 70 columns that qesR 0.4.4 built by stacking variables that share a name across studies; attr(, "removed_columns") lists them. Read those items from each study with get_qes(). This note is shown once per session.
#> get_qes_master() returns its result and no longer assigns it into your workspace by default. Write `qes_master <- get_qes_master(...)`, or pass `assign_global = TRUE`. This note is shown once per session.
map <- attr(master, "legacy_column_map")
map[map$column %in% c("vote_choice", "ideology", "sovereignty_support"),
    c("column", "target", "flag")]
#>                 column           target flag
#> 20            ideology          lr_self <NA>
#> 22         vote_choice vote_prov_recall <NA>
#> 26 sovereignty_support        sov_indep <NA>
attr(master, "legacy_na_columns")[1:5, c("column", "study", "reason")]
#>               column    study    reason
#> 1    interview_start qes_demo no_source
#> 2      interview_end qes_demo na_column
#> 3 interview_recorded qes_demo na_column
#> 4           language qes_demo no_source
#> 5        citizenship qes_demo no_source
unique(master[, c("qes_code", "vote_choice_timing", "sovereignty_item")])
#>   qes_code vote_choice_timing sovereignty_item
#> 1 qes_demo               post        sov_indep
```

`map$studies_changed` names, for each column, the studies whose values
differ from 0.4.4, and `attr(master, "source_map")` the question each
column read in each study. The Changelog counts the changed cells,
column by column and study by study.

[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md)
is soft-deprecated and built from the same targets: its categorical
columns are factors with English levels, and for `qes2022` its `turnout`
and `votechoice` stay the campaign-period likelihood and intention
(`attr(, "timing")` says `"pre"`).

## From the master to `qes_harmonize()`

For new work,
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
gives what the master cannot: one column per question stimulus (a
reported vote and an intention are different targets, and so are the
sovereignty wordings), a comparability grade for each study’s question,
a reason for every missing value and each wave’s weight. The `target`
column of the map above names the target of each master column. The
master’s `ideology` and `vote_choice` become:

``` r

h <- qes_harmonize("qes_demo", targets = c("lr_self", "vote_prov_recall"),
                   missing = "reasons", quiet = TRUE, lang = params$lang)
table(h$lr_self__na, useNA = "ifany")[c("dk", "refused")]
#> 
#>      dk refused 
#>       5       0
attr(h, "qes_weight_guide")[, c("target", "study", "weight_column")]
#>             target    study weight_column
#> 1 vote_prov_recall qes_demo   weight_post
#> 2          lr_self qes_demo   weight_post
```

Compute each estimate within one study, with the weight of the wave that
asked the question. The mean left-right position of each party’s voters
in the demonstration study:

``` r

ok <- !is.na(h$lr_self) & !is.na(h$vote_prov_recall)
d <- h[ok, ]
means <- sapply(split(d, d$vote_prov_recall, drop = TRUE), function(p) {
  weighted.mean(p$lr_self, p$weight_post)
})
round(means, 2)
#>         PLQ          PQ         CAQ          QS         PVQ Other party 
#>        5.39        6.70        5.56        2.53        6.00        9.00
```

The pooled variable `vote_choice` is the closest to the master’s
`vote_choice` across studies: with
`types = list(vote_choice = "recall")` it is the reported vote, as in
the master (whose party labels are those of 0.4.4); by default it also
takes the vote intention of the studies that asked no reported vote, and
`vote_choice__type` says which.

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
shows which study has which target and how comparable its question is,
and
[`vignette("harmonization-reference", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md)
gives each target’s questions, wordings and grades.

## Old names, new names

The eleven helpers of 0.4.4 below keep working and will not be removed.
Each prints a one-time notice naming its replacement
(`options(qesR.quiet_deprecated = TRUE)` hides the notices).

| 0.4.4 | Replacement | Note |
|----|----|----|
| [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md), [`get_qes_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md) | [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md) | same arguments; offline; `refresh` is ignored |
| `format_codebook(cb, layout)` | `qes_codebook(cb, layout = )` | a plain data frame (for example a codebook read back from CSV) is an error |
| `get_value_labels(cb, variable)` | `qes_codebook(layout = "long")` | an unknown variable is an error, not an empty list |
| `get_question(data, q)` | `qes_question(study, variables)` | exact names only; `full = FALSE` no longer changes the result |
| [`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md), [`get_qes_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md) | [`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md) | lists every document, offline |
| [`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md) | `qes_download(what = "docs")` | `file` now selects documents by name; files are md5-checked |
| `get_preview(srvy, obs)` | `head(get_qes(srvy), obs)` |  |
| [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md) | [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md) | the 1998 firm files are added after the 11 old codes |
| [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md) | `qes_harmonize(srvy, targets = "decon")` | soft-deprecated; values changed (above) |

[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
and
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)
keep their names. A legacy function returns what its replacement
returns. The first call of
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md)
in a new session also prints this notice (it is not shown below, because
it appears only once per session):

> [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md)
> is soft-deprecated; use
> [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md).
> It keeps working and will not be removed.

``` r

cb <- get_codebook("qes_demo")
identical(cb, qes_codebook("qes_demo"))
#> [1] TRUE
```

## Keeping new results reproducible

Record what each result was computed from, and cite it:

``` r

qes_provenance(h, level = "spec")[, c("spec_version", "spec_hash", "qesR_version")]
#>   spec_version                        spec_hash qesR_version
#> 1        4.3.1 cd62e566cd051bde5b236c3511555c91        0.8.0
qes_cite("qes2014")
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.8.0, https://github.com/ThomasGareau/qesR"                
#> [2] "Bélanger, Éric; Nadeau, Richard, 2023, \"Étude électorale québécoise 2014\", https://doi.org/10.5683/SP3/64F7WR, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw=="
```

`qes_provenance(x)` gives the file of each study (DOI, dataset version),
and `level = "spec"` the version of the harmonization rules that
produced harmonized data. The same qesR version reproduces a result
exactly, since it fixes both.
