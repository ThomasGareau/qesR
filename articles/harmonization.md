# Harmonizing across studies

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-harmonisation.md)*

This page goes from
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
to a weighted estimate, study by study, and shows what qesR records on
the way: the comparability grade of each study’s question, the reason
for every missing value, the weight that fits each question, and the
provenance of each cell. It is built when the website is built, from the
full data files, which qesR downloads from their Dataverse deposits
through its cache.

The harmonization engine is **experimental**. Its specification says,
for each study and harmonized variable (“target”), which question feeds
the target and how each of its codes maps to the target’s levels;
nothing is matched by name. In specification 4.1.0 every row is signed
off, after an automated double review against the original files and
documents (not a human review), and
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applies them by default. The recommended weights of `qes1998`,
`qes2007_panel`, `qes2012_panel` and the CROP polls still need review,
so their weights are `NA`; their answers are harmonized like the
others’.

``` r

library(qesR)
tr <- function(en, fr) if (identical(params$lang, "fr")) fr else en
# percentages with one decimal, in the page's style
pct <- function(x) formatC(100 * x, format = "f", digits = 1, decimal.mark = tr(".", ","))
```

## Which studies have the question

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
gives one row per target and one column per study, each cell the grade
of that study’s question against the target’s anchor question (the full
grid is on the [coverage
page](https://thomasgareau.github.io/qesR/articles/coverage.md)). Two
targets here: the referendum question on Quebec becoming an independent
country, and the vote reported after the election.

``` r

grid <- qes_spec(targets = c("sov_indep", "vote_prov_recall"), lang = params$lang)
knitr::kable(grid[, c("target", "label", "qes2022", "qes2018", "qes2014", "qes2012")])
```

| target | label | qes2022 | qes2018 | qes2014 | qes2012 |
|:---|:---|:---|:---|:---|:---|
| vote_prov_recall | Provincial vote (recall) | comparable | comparable | comparable | identical |
| sov_indep | Referendum vote: independent country | comparable | comparable | identical | identical |

The crosswalk view says which question each grade is about, and why:

``` r

xw <- qes_spec("crosswalk", targets = "sov_indep", lang = params$lang)
knitr::kable(xw[, c("study", "wave", "source_var", "grade", "grade_reason")])
```

| study | wave | source_var | grade | grade_reason |
|:---|:---|:---|:---|:---|
| qes2012 | post | q52 | identical | Anchor row of the target. |
| qes2014 | post | Q19 | identical | Same stem as the anchor in English and French, same options (yes, no, don’t know, prefer not to answer), asked of everyone, web. |
| qes2018 | post | q26 | comparable | The stem adds a temporal adverb (‘today’, ‘aujourd’hui’); otherwise the same wording and options as the anchor. |
| qes2022 | cps | cps_qc_referendum | comparable | The stem adds a temporal adverb; don’t know is offered but there is no prefer-not-to-answer option. |

`identical` means the same question, options and universe as the anchor;
`comparable` the same stimulus with differences that should not move the
shares; `approximate` a difference that can. The [harmonization
reference](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md)
gives the wording and levels of every study’s question.

## Harmonize

`studies = NULL`, the default, takes the Quebec Election Studies that
have a question for at least one of the targets; the Durand panels, the
CROP polls and the 1998 panel are left out unless you name them.
`qes2008` has no recommended weight (both of its weights are calibrated
on the vote or on turnout), so its rows have no weight and
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
leaves them out below, with a message. `layout = "long"` gives one row
per respondent and wave, so each answer sits on the row of the wave that
asked it, next to that wave’s weight. `missing = "reasons"` adds a
column `<target>__na` with the reason of every missing value.

``` r

h <- qes_harmonize(targets = c("sov_indep", "vote_prov_recall"), layout = "long",
                   missing = "reasons", lang = params$lang)
#> Using the cached copy of '2022 Quebec Election Study v1.dta'.
#> Using the cached copy of 'Quebec Election Study 2018.dta'.
#> Using the cached copy of 'Quebec Election Study 2014.sav'.
#> Using the cached copy of 'Quebec Election Study 2012 (STATA).dta'.
#> Using the cached copy of 'Quebec Election Study 2012 (SPSS).sav'.
#> Using the cached copy of 'Quebec Election Study 2008 (SPSS).sav'.
#> Using the cached copy of 'Quebec Election Study 2007 (SPSS).sav'.
#> Levels a study's question did not offer are structural zeros, not an absence of support: vote_prov_recall: qes2022 (PVQ, ON, ADQ), qes2018 (PVQ, PCQ, ON, ADQ), qes2014 (PCQ, ADQ), qes2012 (PCQ, ADQ), qes2008 (CAQ, PCQ, ON), qes2007 (CAQ, PCQ, ON); sov_indep: qes2022 (would_not_vote), qes2018 (would_not_vote), qes2014 (would_not_vote), qes2012 (would_not_vote). qes_provenance(x, level = "cell") lists them.
table(h$study, h$wave)
#>          
#>            cps  pes post
#>   qes2007    0    0 2175
#>   qes2008    0    0 1151
#>   qes2012    0    0 1505
#>   qes2014    0    0 1517
#>   qes2018    0    0 3072
#>   qes2022 1521 1220    0
```

The messages are part of the result. The first lists the *structural
zeros*: levels a study’s question did not offer, such as a party missing
from its list. Their share in that study is 0 because nobody could
choose them, not because nobody supported them. `qes2022` asked the
referendum question in its campaign-period wave (`cps`) and the reported
vote in its post-election wave (`pes`), which have different weights.

## Why values are missing

Every missing value has a reason. Counts of the valid answers and of
each reason, for the referendum question:

``` r

reasons <- table(h$study, h$sov_indep__na)
reasons <- reasons[, colSums(reasons) > 0, drop = FALSE]
knitr::kable(cbind(valid = tapply(!is.na(h$sov_indep), h$study, sum), reasons[, , drop = FALSE]))
```

|         | valid |  dk | refused | not_in_wave | not_asked |
|:--------|------:|----:|--------:|------------:|----------:|
| qes2007 |     0 |   0 |       0 |           0 |      2175 |
| qes2008 |     0 |   0 |       0 |           0 |      1151 |
| qes2012 |  1323 | 156 |      26 |           0 |         0 |
| qes2014 |  1353 | 148 |      16 |           0 |         0 |
| qes2018 |  2558 | 463 |      51 |           0 |         0 |
| qes2022 |  1284 | 237 |       0 |        1220 |         0 |

`dk` is “don’t know” and `refused` a refusal; `not_in_wave` marks the
`qes2022` post-election rows, since the question was asked in the other
wave.
[`?qes_harmonize`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
lists every reason.

## Grades

`min_grade` keeps only the cells at or above a grade and sets the others
to `NA`, with reason `below_grade`. With `min_grade = "identical"`, only
the studies that asked the anchor question itself remain:

``` r

strict <- qes_harmonize(targets = "sov_indep", layout = "long", missing = "reasons",
                        min_grade = "identical", quiet = TRUE,
                        lang = params$lang)
table(strict$study, strict$sov_indep__na)[, c("dk", "refused", "below_grade")]
#>          
#>             dk refused below_grade
#>   qes2007    0       0           0
#>   qes2008    0       0           0
#>   qes2012  156      26           0
#>   qes2014  148      16           0
#>   qes2018    0       0        3072
#>   qes2022    0       0        1521
```

`qes2018` and `qes2022` added “today” to the question (and `qes2022`
offered no “prefer not to answer”), so both are graded `comparable` and
are dropped here. Whether a `comparable` cell belongs in your analysis
is a judgement the grade reason helps you make; qesR never makes it for
you.

## Weights

Each wave has at most one recommended weight, normalized to mean 1 in
each study and wave. `attr(, "qes_weight_guide")` says which wave each
target came from and which weight fits it:

``` r

guide <- attr(h, "qes_weight_guide")
knitr::kable(guide[, c("target", "study", "wave", "weight_column", "weight_var")])
```

| target           | study   | wave | weight_column | weight_var         |
|:-----------------|:--------|:-----|:--------------|:-------------------|
| vote_prov_recall | qes2022 | pes  | weight        | pes_weight_general |
| sov_indep        | qes2022 | cps  | weight        | cps_weight_general |
| vote_prov_recall | qes2018 | post | weight        | pond               |
| sov_indep        | qes2018 | post | weight        | pond               |
| vote_prov_recall | qes2014 | post | weight        | POND               |
| sov_indep        | qes2014 | post | weight        | POND               |
| vote_prov_recall | qes2012 | post | weight        | pond               |
| sov_indep        | qes2012 | post | weight        | pond               |
| vote_prov_recall | qes2008 | post | weight        | NA                 |
| sov_indep        | qes2008 | NA   | NA            | NA                 |
| vote_prov_recall | qes2007 | post | weight        | pond               |
| sov_indep        | qes2007 | NA   | NA            | NA                 |

In the long layout the `weight` column holds, on each row, the weight of
that row’s wave, so one design serves both targets.

## A weighted estimate

[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
hands the data to the `survey` package: each study is a stratum and, in
the long layout, the respondent is the sampling unit. Estimates are
computed within each study with `svyby()`; the table marks the levels a
study did not offer instead of printing a 0.

``` r

d <- qes_design(h)
#> 1151 row(s) with no value of 'weight' are left out of the design (not in a wave with that weight, or its weight needs review): qes2008 1151.
```

``` r

cells <- qes_provenance(h, level = "cell")
spec <- qes_spec("spec")

# Weighted percentage of each level of `target` by study, with its
# standard error; levels the study's question did not offer are marked.
share_table <- function(design, target) {
  est <- survey::svyby(stats::as.formula(paste0("~", target)), ~study, design,
                       survey::svymean, na.rm = TRUE)
  lv <- levels(h[[target]])
  tg <- spec$tables$targets
  names_lv <- spec$tables$levels$name[spec$tables$levels$levels_id ==
                                        tg$levels_id[tg$target == target]]
  out <- sapply(lv, function(l) {
    paste0(pct(est[[paste0(target, l)]]), " (", pct(est[[paste0("se.", target, l)]]), ")")
  })
  out <- matrix(out, nrow = nrow(est), dimnames = list(est$study, lv))
  for (s in rownames(out)) {
    off <- cells$levels_not_offered[cells$study == s & cells$target == target]
    gone <- lv[names_lv %in% strsplit(off, ";", fixed = TRUE)[[1]]]
    out[s, gone] <- tr("not offered", "non offert")
  }
  out
}
knitr::kable(share_table(d, "sov_indep"))
```

|         | Yes        | No         | Would not vote / would spoil |
|:--------|:-----------|:-----------|:-----------------------------|
| qes2007 | 0.0 (0.0)  | 0.0 (0.0)  | 0.0 (0.0)                    |
| qes2012 | 40.4 (1.5) | 59.6 (1.5) | not offered                  |
| qes2014 | 34.8 (1.5) | 65.2 (1.5) | not offered                  |
| qes2018 | 34.6 (1.0) | 65.4 (1.0) | not offered                  |
| qes2022 | 34.3 (1.8) | 65.7 (1.8) | not offered                  |

Each cell is a weighted percentage with its standard error in
parentheses. No study offered “would not vote” as an answer to this
question, so that level is a structural zero everywhere. The standard
errors treat each weighted sample as a probability sample. The four
studies are web surveys whose weights adjust to census margins, so read
the standard errors as a rough guide only (see
[`?qes_design`](https://thomasgareau.github.io/qesR/reference/qes_design.md)).

Reported vote describes the electorate, so keep the respondents who
could vote: `eligible_voter` is `TRUE` for those aged 18 or older on
election day and, where asked, Canadian citizens. `qes2018` sampled
people aged 16 and over:

``` r

table(h$study[h$wave %in% c("post", "pes")],
      h$eligible_voter[h$wave %in% c("post", "pes")], useNA = "ifany")
#>          
#>           FALSE TRUE <NA>
#>   qes2007     0 2133   42
#>   qes2008     0 1142    9
#>   qes2012     0 1484   21
#>   qes2014     0 1517    0
#>   qes2018   255 2799   18
#>   qes2022     0 1220    0
voters <- subset(d, eligible_voter %in% TRUE)
knitr::kable(share_table(voters, "vote_prov_recall"))
```

|  | PLQ | PQ | CAQ | QS | PVQ | PCQ | ON | ADQ | Other party |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| qes2007 | 25.3 (1.3) | 31.1 (1.4) | not offered | 4.8 (0.7) | 6.5 (0.8) | not offered | not offered | 31.6 (1.4) | 0.6 (0.2) |
| qes2012 | 25.0 (1.4) | 38.6 (1.6) | 25.5 (1.4) | 6.5 (0.8) | 1.0 (0.3) | not offered | 2.3 (0.4) | not offered | 1.1 (0.3) |
| qes2014 | 35.9 (1.6) | 29.8 (1.6) | 23.1 (1.4) | 8.2 (0.8) | 1.0 (0.3) | not offered | 0.7 (0.3) | not offered | 1.3 (0.3) |
| qes2018 | 23.3 (1.0) | 19.6 (0.9) | 35.8 (1.2) | 16.3 (0.9) | not offered | not offered | not offered | not offered | 5.1 (0.6) |
| qes2022 | 17.1 (2.0) | 15.6 (1.2) | 33.0 (1.9) | 17.2 (1.4) | not offered | 13.5 (1.2) | not offered | not offered | 3.5 (1.0) |

Keeping eligible voters does not recalibrate the weights, which target
the population each study sampled. A party marked “not offered” was not
on that study’s list of answers, so its share there cannot be compared
with the other studies.

Pooling studies into one estimate is possible
(`qes_design(pool = "equal")` gives each study the same total), but the
studies differ in population, mode and wording; compare them side by
side, as above, before pooling.

## Provenance

`qes_provenance(level = "cell")` gives, for each study and target, the
crosswalk row applied, its grade and review status, the weight, the
levels not offered and the count of valid values and of each missing
reason:

``` r

knitr::kable(cells[, c("study", "target", "source_var", "grade", "status", "weight_var",
                       "levels_not_offered", "n_valid", "n_dk", "n_not_in_wave")])
```

| study | target | source_var | grade | status | weight_var | levels_not_offered | n_valid | n_dk | n_not_in_wave |
|:---|:---|:---|:---|:---|:---|:---|---:|---:|---:|
| qes2022 | vote_prov_recall | pes_votechoice | comparable | stable | pes_weight_general | PVQ;ON;ADQ | 1101 | 2 | 301 |
| qes2022 | sov_indep | cps_qc_referendum | comparable | stable | cps_weight_general | would_not_vote | 1284 | 237 | 0 |
| qes2018 | vote_prov_recall | q6 | comparable | stable | pond | PVQ;PCQ;ON;ADQ | 2016 | 0 | 0 |
| qes2018 | sov_indep | q26 | comparable | stable | pond | would_not_vote | 2558 | 463 | 0 |
| qes2014 | vote_prov_recall | Q3 | comparable | stable | POND | PCQ;ADQ | 1283 | 0 | 0 |
| qes2014 | sov_indep | Q19 | identical | stable | POND | would_not_vote | 1353 | 148 | 0 |
| qes2012 | vote_prov_recall | q25 | identical | stable | pond | PCQ;ADQ | 1274 | 0 | 0 |
| qes2012 | sov_indep | q52 | identical | stable | pond | would_not_vote | 1323 | 156 | 0 |
| qes2008 | vote_prov_recall | q12a | comparable | stable | NA | CAQ;PCQ;ON | 898 | 2 | 0 |
| qes2008 | sov_indep | NA | NA | NA | NA | NA | 0 | 0 | 0 |
| qes2007 | vote_prov_recall | q12 | comparable | stable | pond | CAQ;PCQ;ON | 1727 | 8 | 0 |
| qes2007 | sov_indep | NA | NA | NA | NA | NA | 0 | 0 | 0 |

These counts are per respondent of the study, as in the respondent
layout, so `n_valid` and the counts of the reasons add up to the study’s
respondents; the table in *Why values are missing* counts rows of the
long layout instead. Every `qes2022` respondent took part in the
campaign wave, so none is `not_in_wave` for `sov_indep`, while 301 did
not take part in the post-election wave and are `not_in_wave` for
`vote_prov_recall`.

`level = "study"` names the pinned file of each study, and
`level = "spec"` the specification version and its content hash; the
same specification, the same files and the same qesR version give the
same values.
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
cites qesR with the specification, then each dataset:

``` r

spec_record <- qes_provenance(h, level = "spec")
spec_record[, c("spec_version", "spec_hash", "qesR_version")]
#>   spec_version                        spec_hash qesR_version
#> 1        4.2.0 02b3b7edc509deff0db16859bef7bfb6        0.7.1
cat(qes_cite(h, lang = params$lang), sep = "\n\n")
#> Gareau-Paquette, Thomas, 2026, "qesR: Access Quebec Election Study Datasets", R package version 0.7.1, https://github.com/ThomasGareau/qesR; harmonization spec 4.2.0 (content hash 02b3b7edc509deff0db16859bef7bfb6)
#> 
#> Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, "2022 Quebec Election Study", https://doi.org/10.7910/DVN/PAQBDR, Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [licence: CC BY-NC 4.0, https://creativecommons.org/licenses/by-nc/4.0/]
#> 
#> Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, "Étude électorale québécoise 2018", https://doi.org/10.5683/SP3/NWTGWS, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg==
#> 
#> Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2014", https://doi.org/10.5683/SP3/64F7WR, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw==
#> 
#> Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, "Étude électorale québécoise 2012", https://doi.org/10.5683/SP2/WXUPXT, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ==
#> 
#> Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2008", https://doi.org/10.5683/SP2/8KEYU3, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg==
#> 
#> Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, "Étude électorale québécoise 2007", https://doi.org/10.5683/SP2/6XGOKA, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg==
```

## Four sovereignty questions, never pooled

A target is one question stimulus. Other studies asked about sovereignty
with other wordings, so they feed other targets, which are never merged
with `sov_indep`:

``` r

sov <- qes_spec("crosswalk", targets = "sovereignty", lang = params$lang)
sov <- sov[sov$rule != "none", ]
rownames(sov) <- NULL
knitr::kable(sov[, c("target", "study", "source_var", "grade", "weight_var")])
```

| target | study | source_var | grade | weight_var |
|:---|:---|:---|:---|:---|
| sov_indep | qes2012 | q52 | identical | pond |
| sov_indep | qes2014 | Q19 | identical | POND |
| sov_indep | qes2018 | q26 | comparable | pond |
| sov_indep | qes2022 | cps_qc_referendum | comparable | cps_weight_general |
| sov_favour | qes2018_panel | rts_q7 | identical | weight_rts |
| sov_sovereign_country | qes2012_panel | intvoteref | identical | pondam1 |
| sov_partnership_1995 | qes2007 | q19 | identical | pond |
| sov_partnership_1995 | qes2008 | q19 | comparable | NA |
| sov_partnership_1995 | qes1998 | q16a_crop | comparable | ponder3 |
| sov_partnership_1995 | qes2007_panel | intref1 | comparable | pondam1 |

`sov_sovereign_country` asks about a “sovereign country”
(`qes2012_panel`), `sov_favour` is a four-point scale of favour
(`qes2018_panel`), and `sov_partnership_1995` repeats the question of
the 1995 referendum, sovereignty with an offer of partnership to the
rest of Canada (`qes2007`, `qes2008`, `qes2007_panel` and the CROP
respondents of `qes1998`). Only `qes2007` and `qes2018_panel` among them
have reviewed weights; `qes2008` has none to recommend and the others’
weights still need review, so
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
returns `NA` for them and
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
leaves these respondents out, with a message. Put the targets side by
side and read each one against its own question.
