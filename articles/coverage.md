# Coverage by study

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-couverture.md)*

Which harmonized variable (“target”) of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
each study has, and how comparable its question is. The grid and the
table of studies below are generated from the rules that ship with qesR.

Each cell gives the comparability grade of the study’s question for the
target, against the target’s anchor question; a dash means the study has
no question for the target. A target’s name links to its section of the
[variable
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

† Awaiting sign-off: applied only if you ask for it
(`qes_harmonize(include_draft = TRUE)`).

## Pooled variables by study

[TABLE]

Each cell gives the member type a pooled variable takes a study’s values
from in the respondent layout, and its grade; a dash means no member of
its default types has a question in the study.

## Studies

| Study | Waves and recommended weights | Targets | Identical | Comparable | Approximate |
|----|----|----|----|----|----|
| `qes2022` | cps (n = 1,521): `cps_weight_general`; pes (n = 1,220): `pes_weight_general` | 35 | 7 | 14 | 14 |
| `qes2018` | post (n = 3,072): `pond` | 29 | 3 | 19 | 7 |
| `qes2018_panel` | pre (n = 1,250): `weight`; post (n = 842): `weight_rts` | 13 | 3 | 3 | 7 |
| `qes2014` | post (n = 1,517): `POND` | 30 | 9 | 20 | 1 |
| `qes2012` | post (n = 1,505): `pond` | 31 | 27 | 4 | 0 |
| `qes2012_panel` | pre (n = 844): `pondam1` (not usable yet); post (n = 844): `pond_post` (not usable yet) | 10 | 1 | 6 | 3 |
| `qes_crop_2007_2010` | 24 poll waves, poll_2007_06 to poll_2010_01 (n = 1,000 to 1,004 each): `XPOND` (not usable yet) | 10 | 1 | 7 | 2 |
| `qes2008` | post (n = 1,151): no recommended weight | 27 | 0 | 25 | 2 |
| `qes2007` | post (n = 2,175): `pond` | 25 | 6 | 16 | 3 |
| `qes2007_panel` | pre (n = 2,050): `pondam1` (not usable yet); post (n = 2,054): `pond_tot_am1` (not usable yet) | 15 | 3 | 8 | 4 |
| `qes1998` | pre (n = 1,483): `ponder3` (not usable yet); post (n = 1,483): `ponder3` (not usable yet) | 8 | 0 | 7 | 1 |

`n` is the number of respondents of each wave. A weight marked *not
usable yet* is not documented well enough to use: its weight columns are
`NA`.
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
uses the weight of the wave each target came from.

Not harmonized: `qes1998_crop`, `qes1998_createc` (their respondents are
in `qes1998`).
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads them.

## The same grid in R

[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
returns the grid as a data frame, one row per target and one column per
study, each cell the grade of that study’s question:

``` r

library(qesR)
qes_spec(lang = params$lang)
```

A grade compares a study’s question with the target’s anchor question.
[How harmonization
works](https://thomasgareau.github.io/qesR/articles/harmonization.md)
shows how grades, missing values and weights carry into an estimate.
