# Coverage by study

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-couverture.md)*

Which harmonized variable (“target”) of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
each study has, and how comparable its question is. The grid and the
table of studies below are generated from the specification that ships
with qesR when the site is built; none of it is written by hand.

This grid is generated from the harmonization spec shipped with qesR:
version 4.2.0 of 2026-09-28, content hash
`02b3b7edc509deff0db16859bef7bfb6`. It is **experimental**. Each cell
gives the comparability grade of the study’s question for the target,
against the target’s anchor question; a dash means the study has no
question for the target in the spec. A target’s name links to its
section of the [harmonization
reference](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md),
which gives the question, its wording, the levels it offered and the
reason for its grade.
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
returns the same grid as a data frame.

## Targets by study

[TABLE]

For a study with more than one wave, the wave that asked the question is
in parentheses.

\* The study’s question did not offer every level of the target: the
levels it did not offer are structural zeros, listed in the reference.

All 129 cells use crosswalk rows signed off by a reviewer (status
stable), which
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applies by default; the crosswalk’s `reviewed_by` says who or what
reviewed each row (spec 4.0.0: an automated double review against the
original files and documents, not a human review).

## Studies

| Study | Waves and recommended weights | Targets | Identical | Comparable | Approximate |
|----|----|----|----|----|----|
| `qes2022` | cps (n = 1,521): `cps_weight_general`; pes (n = 1,220): `pes_weight_general` | 18 | 7 | 5 | 6 |
| `qes2018` | post (n = 3,072): `pond` | 13 | 1 | 10 | 2 |
| `qes2018_panel` | pre (n = 1,250): `weight`; post (n = 842): `weight_rts` | 12 | 3 | 3 | 6 |
| `qes2014` | post (n = 1,517): `POND` | 13 | 4 | 9 | 0 |
| `qes2012` | post (n = 1,505): `pond` | 13 | 10 | 3 | 0 |
| `qes2012_panel` | pre (n = 844): `pondam1` (needs review, not applied); post (n = 844): `pond_post` (needs review, not applied) | 9 | 1 | 5 | 3 |
| `qes_crop_2007_2010` | 24 poll waves, poll_2007_06 to poll_2010_01 (n = 1,000 to 1,004 each): `XPOND` (needs review, not applied) | 8 | 1 | 5 | 2 |
| `qes2008` | post (n = 1,151): no recommended weight | 12 | 0 | 11 | 1 |
| `qes2007` | post (n = 2,175): `pond` | 12 | 4 | 7 | 1 |
| `qes2007_panel` | pre (n = 2,050): `pondam1` (needs review, not applied); post (n = 2,054): `pond_tot_am1` (needs review, not applied) | 12 | 3 | 6 | 3 |
| `qes1998` | pre (n = 1,483): `ponder3` (needs review, not applied); post (n = 1,483): `ponder3` (needs review, not applied) | 7 | 0 | 6 | 1 |

`n` is the number of respondents of each wave. A weight that needs
review is not applied:
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
returns `NA` for it until its documentation is checked.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
uses the weight of the wave each target came from.

Studies of the catalog that are not in the spec yet: `qes1998_crop`,
`qes1998_createc`.
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads them;
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
does not cover them yet.

## The same grid in R

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
returns the grid as a data frame, one row per target and one column per
study, each cell the grade of that study’s question:

``` r

library(qesR)
qes_spec(lang = params$lang)
```

A grade compares a study’s question with the target’s anchor question.
[Harmonizing across
studies](https://thomasgareau.github.io/qesR/articles/harmonization.md)
shows how grades, missing values and weights carry into an estimate.
