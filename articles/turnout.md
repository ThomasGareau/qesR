# Who votes? The age gap in turnout, and what surveys overstate

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-participation.md)*

Surveys overstate turnout. In 2022, 90% of the respondents of the Quebec
Election Study said they had voted; the official turnout was 66%. The
gap is there at every election, because people who vote also answer
surveys more, and some who did not vote say they did. Within the
surveys, the young report voting less than the old at every election: in
2018, 70% of respondents aged 18 to 34 against 91% of those aged 55 and
over. The gap is widest among those least interested in politics.

## The data

`turnout` is the pooled reported turnout, asked after the election, and
`pol_interest` puts interest in politics on one scale from 0 to 1 across
the four-point and the 0-10 questions:

``` r

h <- qes_harmonize(
  studies = qz_studies,
  targets = c("turnout", "age_group3", "pol_interest"),
  missing = "reasons", quiet = TRUE
)
```

``` r

table(h$study, h$pol_interest__type)
#>                     
#>                      general_4pt general_0_10 campaign_4pt election_0_10
#>   qes_crop_2007_2010           0            0            0             0
#>   qes1998                      0            0            0             0
#>   qes2007                      0         2175            0             0
#>   qes2007_panel                0            0         2050             0
#>   qes2008                      0            0            0          1151
#>   qes2012                   1505            0            0             0
#>   qes2012_panel                0            0            0             0
#>   qes2014                   1517            0            0             0
#>   qes2018                   3072            0            0             0
#>   qes2018_panel                0            0            0             0
#>   qes2022                      0         1521            0             0
d18 <- qes_design(h[h$study == "qes2018", ], weight = "weight_post")
svyciprop(~I(turnout == "Yes"), subset(d18, !is.na(turnout)), method = "logit")
#>                            2.5% 97.5%
#> I(turnout == "Yes") 0.832 0.815 0.847
```

## Reported and official turnout

![Dumbbell chart, one row per study from 1998 to 2022: the official
turnout of the election as a tick and the turnout reported by the
study's respondents as a dot with its confidence interval. Every study
is above the official turnout, by +9 pts to +30 pts. Values in the table
view.](turnout_files/figure-html/overreport-light.png)![Dumbbell chart,
one row per study from 1998 to 2022: the official turnout of the
election as a tick and the turnout reported by the study's respondents
as a dot with its confidence interval. Every study is above the official
turnout, by +9 pts to +30 pts. Values in the table
view.](turnout_files/figure-html/overreport-dark.png)

Source: qesR, pooled turnout (reported, after the election), every study
that asked it; official turnout of Élections Québec (ballots cast over
registered electors). Dots: reported turnout with 95% confidence
intervals (logit), weighted with each study's post-election weight;
hollow: unweighted, weight under review. The 1998 polls interviewed
francophones only, and their recontact over-selected undecided voters
and refusers. The 2018 study also sampled 16- and 17-year-olds, who
could not vote; they are left out. Respondents are not the list of
registered electors, so part of the gap is not error.

Table view

| Election | Study | Reported turnout, % \[95% CI\] | n | Official turnout, % | Gap | Weighting |
|---:|:---|:---|---:|:---|:---|:---|
| 1998 | 1998 polls | 87.4 \[85.6, 89.0\] | 1483 | 78.3 | +9 pts | unweighted (weight under review) |
| 2007 | QES 2007 | 90.7 \[89.0, 92.2\] | 2162 | 71.2 | +20 pts | weighted |
| 2007 | 2007 panel | 85.3 \[83.7, 86.8\] | 2054 | 71.2 | +14 pts | unweighted (weight under review) |
| 2008 | QES 2008 | 87.2 \[85.1, 89.0\] | 1131 | 57.4 | +30 pts | unweighted (weight under review) |
| 2012 | QES 2012 | 93.2 \[91.8, 94.4\] | 1486 | 74.6 | +19 pts | weighted |
| 2012 | 2012 panel | 92.2 \[90.2, 93.8\] | 844 | 74.6 | +18 pts | unweighted (weight under review) |
| 2014 | QES 2014 | 88.9 \[86.8, 90.8\] | 1499 | 71.4 | +17 pts | weighted |
| 2018 | QES 2018 | 83.2 \[81.5, 84.7\] | 2635 | 66.4 | +17 pts | weighted |
| 2018 | 2018 panel | 83.7 \[80.1, 86.7\] | 842 | 66.4 | +17 pts | weighted |
| 2022 | QES 2022 | 90.0 \[87.3, 92.1\] | 1215 | 66.1 | +24 pts | weighted |

Surveys overstate turnout by 9 to 30 pointsReported turnout in each
study (dot, 95% CI) against the official turnout of the election (tick)

What to notice:

- Every study is above the official turnout, from +9 pts to +30 pts.
- The gap is largest in 2008, the election with the lowest official
  turnout (57%), and the reported turnout moves much less than the
  official one.
- The reviewed weights adjust to census margins (age, sex, region,
  language and, in some studies, education), not to turnout, so
  weighting does not remove the gap.

## The age gap, election by election

![Line chart with a confidence band of the gap in reported turnout
between respondents aged 55 and over and those aged 18 to 34, in points,
at each election from 1998 to 2022. The gap is positive at every
election: +11 pts in 2007, +21 pts in 2018 (the largest) and +8 pts in
2022. Values in the table
view.](turnout_files/figure-html/age-light.png)![Line chart with a
confidence band of the gap in reported turnout between respondents aged
55 and over and those aged 18 to 34, in points, at each election from
1998 to 2022. The gap is positive at every election: +11 pts in 2007,
+21 pts in 2018 (the largest) and +8 pts in 2022. Values in the table
view.](turnout_files/figure-html/age-dark.png)

Source: qesR, pooled turnout and age_group3; one Quebec Election Study
per election and the 1998 polls (francophones only). Band: 95%
confidence interval of the difference, from the covariance of the two
age groups' estimates. Weighted with each study's post-election weight;
hollow (1998, 2008): unweighted, weight under review. The x axis is in
years, with a break between 1998 and 2007. The shares of each age group
and the Durand panels are in the table view. Official turnout by age is
not among the benchmarks kept with qesR, so the gap is a gap between
reports.

Table view

| Election | Study | 18 to 34, % | 35 to 54, % | 55 and over, % | Gap, 55+ minus 18-34 \[95% CI\] | Weighting |
|---:|:---|:---|:---|:---|:---|:---|
| 1998 | 1998 polls | 81.6 | 88.8 | 90.3 | +8.7 pts \[3.9, 13.6\] | unweighted (weight under review) |
| 2007 | QES 2007 | 81.6 | 88.8 | 90.3 | +11.0 pts \[6.9, 15.1\] | weighted |
| 2007 | 2007 panel | 81.6 | 88.8 | 90.3 | +17.5 pts \[12.9, 22.2\] | unweighted (weight under review) |
| 2008 | QES 2008 | 81.6 | 88.8 | 90.3 | +14.5 pts \[9.0, 20.0\] | unweighted (weight under review) |
| 2012 | QES 2012 | 81.6 | 88.8 | 90.3 | +7.0 pts \[3.5, 10.4\] | weighted |
| 2012 | 2012 panel | 81.6 | 88.8 | 90.3 | +4.7 pts \[-1.3, 10.7\] | unweighted (weight under review) |
| 2014 | QES 2014 | 81.6 | 88.8 | 90.3 | +13.1 pts \[7.6, 18.5\] | weighted |
| 2018 | QES 2018 | 81.6 | 88.8 | 90.3 | +21.2 pts \[17.1, 25.2\] | weighted |
| 2018 | 2018 panel | 81.6 | 88.8 | 90.3 | +17.5 pts \[8.3, 26.8\] | weighted |
| 2022 | QES 2022 | 81.6 | 88.8 | 90.3 | +8.4 pts \[2.6, 14.2\] | weighted |

The old report voting more than the young at every election; the gap
peaked at 21 points in 2018Reported turnout of the 55 and over minus
that of the 18 to 34, in points, with its 95% confidence interval

What to notice:

- At every election the 55 and over report the highest turnout, above
  the 18 to 34 by +11 pts in 2007, +21 pts in 2018 and +8 pts in 2022.
- The gap is widest in 2018, when official turnout fell to 66%: the drop
  shows among the young and barely among the old.
- In 2022 the 18 to 34 report voting as often as the 35 to 54, with a
  wide interval (269 young respondents).

## Interest in politics and the age gap

![Line chart, one line per age group (18 to 34, 35 to 54, 55 and over),
of the gap in reported turnout between respondents with high and with
low interest in politics, in points, at each Quebec Election Study from
2007 to 2022, with confidence intervals. Under 55 the gap reaches +30
pts; among the 55 and over it is +11 pts at most. Values in the table
view.](turnout_files/figure-html/interest-light.png)![Line chart, one
line per age group (18 to 34, 35 to 54, 55 and over), of the gap in
reported turnout between respondents with high and with low interest in
politics, in points, at each Quebec Election Study from 2007 to 2022,
with confidence intervals. Under 55 the gap reaches +30 pts; among the
55 and over it is +11 pts at most. Values in the table
view.](turnout_files/figure-html/interest-dark.png)

Source: qesR, pooled turnout and pol_interest, age_group3; QES 2007 to
2022, weighted with each study's post-election weight. Interest on the
pooled 0-1 scale: low 0 to 0.4 (not at all or hardly interested), some
0.5 to 0.7, high 0.8 to 1. The four-point items (2012 to 2018) and the
0-10 items (2007, 2022) do not cut the same way: compare the groups
within a study. Lines: 95% confidence intervals of the difference; a gap
whose low or high cell has fewer than 30 respondents is not drawn. The
turnout of each cell is in the table view.

Table view

| Election | Study | Age | Interest | Reported turnout, % \[95% CI\] | n | Interest question (type) | Weighting |
|---:|:---|:---|:---|:---|---:|:---|:---|
| 2007 | QES 2007 | 18 to 34 | Low interest | 75.7 \[65.4, 83.7\] | 117 | general_0_10 | weighted |
| 2007 | QES 2007 | 18 to 34 | Some interest | 87.0 \[80.8, 91.5\] | 246 | general_0_10 | weighted |
| 2007 | QES 2007 | 18 to 34 | High interest | 87.0 \[78.9, 92.2\] | 197 | general_0_10 | weighted |
| 2007 | QES 2007 | 35 to 54 | Low interest | 82.2 \[72.8, 88.8\] | 127 | general_0_10 | weighted |
| 2007 | QES 2007 | 35 to 54 | Some interest | 92.9 \[88.2, 95.8\] | 369 | general_0_10 | weighted |
| 2007 | QES 2007 | 35 to 54 | High interest | 94.4 \[89.8, 97.0\] | 238 | general_0_10 | weighted |
| 2007 | QES 2007 | 55 and over | Low interest | 91.3 \[83.4, 95.6\] | 110 | general_0_10 | weighted |
| 2007 | QES 2007 | 55 and over | Some interest | 97.1 \[94.1, 98.6\] | 310 | general_0_10 | weighted |
| 2007 | QES 2007 | 55 and over | High interest | 95.9 \[92.9, 97.7\] | 403 | general_0_10 | weighted |
| 2012 | QES 2012 | 18 to 34 | Low interest | 77.1 \[69.6, 83.2\] | 174 | general_4pt | weighted |
| 2012 | QES 2012 | 18 to 34 | Some interest | 95.2 \[91.6, 97.3\] | 236 | general_4pt | weighted |
| 2012 | QES 2012 | 18 to 34 | High interest | 96.2 \[90.0, 98.6\] | 119 | general_4pt | weighted |
| 2012 | QES 2012 | 35 to 54 | Low interest | 89.3 \[84.4, 92.8\] | 211 | general_4pt | weighted |
| 2012 | QES 2012 | 35 to 54 | Some interest | 94.9 \[91.2, 97.1\] | 292 | general_4pt | weighted |
| 2012 | QES 2012 | 35 to 54 | High interest | 97.9 \[93.6, 99.3\] | 126 | general_4pt | weighted |
| 2012 | QES 2012 | 55 and over | Low interest | 97.4 \[89.8, 99.4\] | 64 | general_4pt | weighted |
| 2012 | QES 2012 | 55 and over | Some interest | 94.6 \[90.0, 97.2\] | 162 | general_4pt | weighted |
| 2012 | QES 2012 | 55 and over | High interest | 99.1 \[93.8, 99.9\] | 88 | general_4pt | weighted |
| 2014 | QES 2014 | 18 to 34 | Low interest | 66.4 \[56.4, 75.1\] | 138 | general_4pt | weighted |
| 2014 | QES 2014 | 18 to 34 | Some interest | 86.0 \[78.4, 91.3\] | 167 | general_4pt | weighted |
| 2014 | QES 2014 | 18 to 34 | High interest | 96.3 \[90.1, 98.6\] | 84 | general_4pt | weighted |
| 2014 | QES 2014 | 35 to 54 | Low interest | 81.4 \[73.8, 87.1\] | 171 | general_4pt | weighted |
| 2014 | QES 2014 | 35 to 54 | Some interest | 96.2 \[92.4, 98.1\] | 279 | general_4pt | weighted |
| 2014 | QES 2014 | 35 to 54 | High interest | 95.2 \[87.9, 98.2\] | 126 | general_4pt | weighted |
| 2014 | QES 2014 | 55 and over | Low interest | 89.6 \[80.0, 94.9\] | 86 | general_4pt | weighted |
| 2014 | QES 2014 | 55 and over | Some interest | 94.3 \[87.9, 97.4\] | 269 | general_4pt | weighted |
| 2014 | QES 2014 | 55 and over | High interest | 96.3 \[90.6, 98.6\] | 169 | general_4pt | weighted |
| 2018 | QES 2018 | 18 to 34 | Low interest | 60.6 \[53.8, 67.0\] | 269 | general_4pt | weighted |
| 2018 | QES 2018 | 18 to 34 | Some interest | 75.2 \[69.6, 80.1\] | 317 | general_4pt | weighted |
| 2018 | QES 2018 | 18 to 34 | High interest | 80.2 \[71.4, 86.8\] | 120 | general_4pt | weighted |
| 2018 | QES 2018 | 35 to 54 | Low interest | 73.4 \[65.5, 80.0\] | 159 | general_4pt | weighted |
| 2018 | QES 2018 | 35 to 54 | Some interest | 80.8 \[75.1, 85.5\] | 241 | general_4pt | weighted |
| 2018 | QES 2018 | 35 to 54 | High interest | 94.8 \[88.8, 97.7\] | 114 | general_4pt | weighted |
| 2018 | QES 2018 | 55 and over | Low interest | 83.8 \[78.0, 88.2\] | 226 | general_4pt | weighted |
| 2018 | QES 2018 | 55 and over | Some interest | 92.2 \[89.6, 94.2\] | 697 | general_4pt | weighted |
| 2018 | QES 2018 | 55 and over | High interest | 94.8 \[92.0, 96.6\] | 456 | general_4pt | weighted |
| 2022 | QES 2022 | 18 to 34 | Low interest | 73.9 \[61.9, 83.1\] | 95 | general_0_10 | weighted |
| 2022 | QES 2022 | 18 to 34 | Some interest | 94.0 \[81.8, 98.2\] | 105 | general_0_10 | weighted |
| 2022 | QES 2022 | 18 to 34 | High interest | 91.9 \[78.9, 97.2\] | 67 | general_0_10 | weighted |
| 2022 | QES 2022 | 35 to 54 | Low interest | 69.5 \[50.9, 83.4\] | 80 | general_0_10 | weighted |
| 2022 | QES 2022 | 35 to 54 | Some interest | 84.7 \[76.8, 90.3\] | 150 | general_0_10 | weighted |
| 2022 | QES 2022 | 35 to 54 | High interest | 97.3 \[93.1, 99.0\] | 164 | general_0_10 | weighted |
| 2022 | QES 2022 | 55 and over | Low interest | 91.1 \[80.5, 96.2\] | 67 | general_0_10 | weighted |
| 2022 | QES 2022 | 55 and over | Some interest | 94.0 \[87.5, 97.2\] | 194 | general_0_10 | weighted |
| 2022 | QES 2022 | 55 and over | High interest | 96.5 \[92.7, 98.3\] | 284 | general_0_10 | weighted |

Under 55, interest in politics moves reported turnout by up to 30
points; among the 55 and over, by 11 at mostReported turnout of the
highly interested minus that of the least interested, within each age
group, in points, with 95% confidence intervals

What to notice:

- In every age group and at every election, the highly interested report
  more turnout than the least interested. Between some and high interest
  the order is not stable: the older groups are near the ceiling (above
  90% among the 55 and over).
- The age gap is widest among the least interested: in 2018, +23 pts
  between the old and the young with low interest, and +15 pts among
  those with high interest. In 2014 highly interested young and old
  report the same turnout.
- The 2022 study asked interest during the campaign on a 0-10 scale and
  turnout after the election: its bands are not those of 2018.

## About the data

- **Studies.** Every study that asked the reported turnout after the
  election (the CROP polls asked none). Official turnout: ballots cast
  over registered electors, from Élections Québec.
- **Variables.** `turnout` (pooled; by default the reported turnout
  only; `types = list(turnout = c("recall", "intention"))` would add the
  likelihood of voting where a study asked no reported turnout),
  `age_group3`, and `pol_interest`, interest in politics on 0 to 1: the
  four-point items scored 1, 0.7, 0.3 and 0 (graded `approximate`, the
  scores being an assumption), the 0-10 items divided by 10.
  `pol_interest__type` says which item each value comes from.
- **Official turnout by age** is published by Élections Québec for some
  elections, but it is not among the benchmarks kept with qesR, so this
  page does not show it.
- **Weights.** Each study’s recommended post-election weight, through
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md).
  The 1998 polls, the 2008 study and the 2007 and 2012 Durand panels are
  unweighted (weights under review) and drawn hollow; the 2018 panel
  uses its reviewed post-election weight.
- **Population.** The 1998 polls interviewed francophones only, and
  their recontact over-selected undecided voters and refusers. The 2018
  study also sampled 16- and 17-year-olds, who could not vote; they are
  left out of the turnout estimates.
