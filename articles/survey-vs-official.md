# How far off are surveys? Reported vote and intentions against the official results

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-enquetes-resultats.md)*

Survey estimates of the vote come close to the official results, but
rarely onto them. In 2022 the Quebec Election Study had the CAQ at 33%
of the reported vote, weighted; the CAQ took 41% of the valid votes. The
PQ is over-reported in most studies, and the PLQ under-reported until
2014. Weighting to census margins brings some studies closer and not
others: the index of dissimilarity between the reported and the official
vote of the 2012 study goes from 11.0 points unweighted to 8.0 weighted,
and that of 2014 from 4.9 to 5.6.

## The data

``` r

h <- qes_harmonize(studies = qz_studies, targets = "vote_choice", missing = "reasons", quiet = TRUE)
```

`vote_choice` with its default precedence: the reported vote where a
study asked it, else the intention (the CROP polls), as
`vote_choice__type` records:

``` r

table(h$study, h$vote_choice__type)
#>                     
#>                      recall intention_push intention
#>   qes_crop_2007_2010      0          22848      1179
#>   qes1998              1483              0         0
#>   qes2007              2175              0         0
#>   qes2007_panel        2054              0         0
#>   qes2008              1151              0         0
#>   qes2012              1505              0         0
#>   qes2012_panel         844              0         0
#>   qes2014              1517              0         0
#>   qes2018              3072              0         0
#>   qes2018_panel         842              0         0
#>   qes2022              1220              0         0
```

## Reported vote against the official result, party by party

![Five small charts, one per party (PLQ, PQ, QS, ADQ, CAQ): the official
share of valid votes at each election from 2007 to 2022 as a line with a
tick, and each study's reported vote as a dot with its confidence
interval (circles: Quebec Election Studies; squares: Durand panels). In
2022 the QES has the CAQ at 33% against 41% officially, and QS at 17%
against 15%. Values in the table
view.](survey-vs-official_files/figure-html/parties-light.png)![Five
small charts, one per party (PLQ, PQ, QS, ADQ, CAQ): the official share
of valid votes at each election from 2007 to 2022 as a line with a tick,
and each study's reported vote as a dot with its confidence interval
(circles: Quebec Election Studies; squares: Durand panels). In 2022 the
QES has the CAQ at 33% against 41% officially, and QS at 17% against
15%. Values in the table
view.](survey-vs-official_files/figure-html/parties-dark.png)

Source: qesR, pooled vote_choice (reported vote) of the Quebec Election
Studies (circles, just left of each election) and the Durand panels
(squares, just right); official results of Élections Québec. Dots:
weighted with each study's post-election weight, 95% confidence
intervals (logit). Hollow: unweighted, weight under review (the 2008
study and the 2007 and 2012 panels); the 2018 panel uses its reviewed
post-election weight. The 1998 polls interviewed francophones only and
are left out. The table view gives every party, with the unweighted
estimates.

Table view

| Party | Election | Study | Reported vote, % \[95% CI\] | n | Official, % of valid votes | Difference | Weighting |
|:---|---:|:---|:---|---:|:---|:---|:---|
| PLQ | 2007 | QES 2007 | 25.4 \[23.4, 27.5\] | 1727 | 33.1 | −7.7 pts | unweighted |
| PLQ | 2007 | QES 2007 | 25.8 \[23.3, 28.4\] | 1727 | 33.1 | −7.3 pts | weighted |
| PLQ | 2007 | 2007 panel | 28.6 \[26.4, 31.0\] | 1494 | 33.1 | −4.4 pts | unweighted |
| PLQ | 2008 | QES 2008 | 39.2 \[36.1, 42.4\] | 898 | 42.1 | −2.9 pts | unweighted |
| PLQ | 2012 | QES 2012 | 21.8 \[19.6, 24.2\] | 1274 | 31.2 | −9.4 pts | unweighted |
| PLQ | 2012 | QES 2012 | 24.9 \[22.2, 27.8\] | 1274 | 31.2 | −6.3 pts | weighted |
| PLQ | 2012 | 2012 panel | 26.4 \[23.1, 30.0\] | 633 | 31.2 | −4.8 pts | unweighted |
| PLQ | 2014 | QES 2014 | 38.1 \[35.5, 40.8\] | 1283 | 41.5 | −3.4 pts | unweighted |
| PLQ | 2014 | QES 2014 | 35.9 \[32.8, 39.1\] | 1283 | 41.5 | −5.6 pts | weighted |
| PLQ | 2018 | QES 2018 | 24.3 \[22.5, 26.2\] | 2016 | 24.8 | −0.5 pts | unweighted |
| PLQ | 2018 | QES 2018 | 23.3 \[21.4, 25.3\] | 2016 | 24.8 | −1.6 pts | weighted |
| PLQ | 2018 | 2018 panel | 28.4 \[25.2, 31.9\] | 704 | 24.8 | +3.6 pts | unweighted |
| PLQ | 2018 | 2018 panel | 26.9 \[23.3, 30.9\] | 704 | 24.8 | +2.1 pts | weighted |
| PLQ | 2022 | QES 2022 | 13.3 \[11.4, 15.4\] | 1101 | 14.4 | −1.1 pts | unweighted |
| PLQ | 2022 | QES 2022 | 17.1 \[13.6, 21.4\] | 1101 | 14.4 | +2.8 pts | weighted |
| PQ | 2007 | QES 2007 | 30.2 \[28.0, 32.4\] | 1727 | 28.3 | +1.8 pts | unweighted |
| PQ | 2007 | QES 2007 | 30.9 \[28.2, 33.6\] | 1727 | 28.3 | +2.5 pts | weighted |
| PQ | 2007 | 2007 panel | 30.8 \[28.5, 33.2\] | 1494 | 28.3 | +2.4 pts | unweighted |
| PQ | 2008 | QES 2008 | 37.4 \[34.3, 40.6\] | 898 | 35.2 | +2.2 pts | unweighted |
| PQ | 2012 | QES 2012 | 40.0 \[37.3, 42.7\] | 1274 | 31.9 | +8.0 pts | unweighted |
| PQ | 2012 | QES 2012 | 38.8 \[35.8, 41.9\] | 1274 | 31.9 | +6.9 pts | weighted |
| PQ | 2012 | 2012 panel | 38.4 \[34.7, 42.2\] | 633 | 31.9 | +6.4 pts | unweighted |
| PQ | 2014 | QES 2014 | 26.8 \[24.5, 29.3\] | 1283 | 25.4 | +1.4 pts | unweighted |
| PQ | 2014 | QES 2014 | 29.8 \[26.9, 33.0\] | 1283 | 25.4 | +4.4 pts | weighted |
| PQ | 2018 | QES 2018 | 19.4 \[17.8, 21.2\] | 2016 | 17.1 | +2.4 pts | unweighted |
| PQ | 2018 | QES 2018 | 19.6 \[17.8, 21.5\] | 2016 | 17.1 | +2.5 pts | weighted |
| PQ | 2018 | 2018 panel | 15.3 \[12.9, 18.2\] | 704 | 17.1 | −1.7 pts | unweighted |
| PQ | 2018 | 2018 panel | 14.9 \[12.1, 18.2\] | 704 | 17.1 | −2.2 pts | weighted |
| PQ | 2022 | QES 2022 | 18.7 \[16.5, 21.1\] | 1101 | 14.6 | +4.1 pts | unweighted |
| PQ | 2022 | QES 2022 | 15.6 \[13.4, 18.1\] | 1101 | 14.6 | +1.0 pts | weighted |
| ADQ | 2007 | QES 2007 | 33.8 \[31.6, 36.0\] | 1727 | 30.8 | +2.9 pts | unweighted |
| ADQ | 2007 | QES 2007 | 31.6 \[29.0, 34.3\] | 1727 | 30.8 | +0.8 pts | weighted |
| ADQ | 2007 | 2007 panel | 32.1 \[29.8, 34.5\] | 1494 | 30.8 | +1.3 pts | unweighted |
| ADQ | 2008 | QES 2008 | 16.0 \[13.8, 18.6\] | 898 | 16.4 | −0.3 pts | unweighted |
| QS | 2007 | QES 2007 | 4.3 \[3.5, 5.4\] | 1727 | 3.6 | +0.7 pts | unweighted |
| QS | 2007 | QES 2007 | 4.8 \[3.6, 6.2\] | 1727 | 3.6 | +1.1 pts | weighted |
| QS | 2007 | 2007 panel | 3.8 \[3.0, 4.9\] | 1494 | 3.6 | +0.2 pts | unweighted |
| QS | 2008 | QES 2008 | 4.2 \[3.1, 5.8\] | 898 | 3.8 | +0.5 pts | unweighted |
| QS | 2012 | QES 2012 | 7.5 \[6.2, 9.1\] | 1274 | 6.0 | +1.5 pts | unweighted |
| QS | 2012 | QES 2012 | 6.5 \[5.1, 8.2\] | 1274 | 6.0 | +0.5 pts | weighted |
| QS | 2012 | 2012 panel | 7.1 \[5.3, 9.4\] | 633 | 6.0 | +1.1 pts | unweighted |
| QS | 2014 | QES 2014 | 10.2 \[8.7, 12.0\] | 1283 | 7.6 | +2.6 pts | unweighted |
| QS | 2014 | QES 2014 | 8.2 \[6.8, 9.9\] | 1283 | 7.6 | +0.6 pts | weighted |
| QS | 2018 | QES 2018 | 17.0 \[15.4, 18.7\] | 2016 | 16.1 | +0.9 pts | unweighted |
| QS | 2018 | QES 2018 | 16.3 \[14.6, 18.0\] | 2016 | 16.1 | +0.1 pts | weighted |
| QS | 2018 | 2018 panel | 12.4 \[10.1, 15.0\] | 704 | 16.1 | −3.7 pts | unweighted |
| QS | 2018 | 2018 panel | 12.8 \[10.0, 16.2\] | 704 | 16.1 | −3.3 pts | weighted |
| QS | 2022 | QES 2022 | 19.0 \[16.8, 21.4\] | 1101 | 15.4 | +3.6 pts | unweighted |
| QS | 2022 | QES 2022 | 17.2 \[14.7, 20.1\] | 1101 | 15.4 | +1.8 pts | weighted |
| CAQ | 2012 | QES 2012 | 25.4 \[23.1, 27.9\] | 1274 | 27.1 | −1.6 pts | unweighted |
| CAQ | 2012 | QES 2012 | 25.4 \[22.8, 28.1\] | 1274 | 27.1 | −1.7 pts | weighted |
| CAQ | 2012 | 2012 panel | 23.9 \[20.7, 27.3\] | 633 | 27.1 | −3.2 pts | unweighted |
| CAQ | 2014 | QES 2014 | 21.6 \[19.4, 23.9\] | 1283 | 23.1 | −1.5 pts | unweighted |
| CAQ | 2014 | QES 2014 | 23.1 \[20.5, 26.0\] | 1283 | 23.1 | +0.1 pts | weighted |
| CAQ | 2018 | QES 2018 | 34.7 \[32.7, 36.8\] | 2016 | 37.4 | −2.7 pts | unweighted |
| CAQ | 2018 | QES 2018 | 35.8 \[33.6, 38.1\] | 2016 | 37.4 | −1.6 pts | weighted |
| CAQ | 2018 | 2018 panel | 37.4 \[33.9, 41.0\] | 704 | 37.4 | −0.1 pts | unweighted |
| CAQ | 2018 | 2018 panel | 39.1 \[34.8, 43.5\] | 704 | 37.4 | +1.7 pts | weighted |
| CAQ | 2022 | QES 2022 | 33.1 \[30.3, 35.9\] | 1101 | 41.0 | −7.9 pts | unweighted |
| CAQ | 2022 | QES 2022 | 33.0 \[29.3, 36.9\] | 1101 | 41.0 | −8.0 pts | weighted |
| PCQ | 2012 | QES 2012 | n.l. | 1274 |  |  | unweighted |
| PCQ | 2012 | QES 2012 | n.l. | 1274 |  |  | weighted |
| PCQ | 2012 | 2012 panel | n.l. | 633 |  |  | unweighted |
| PCQ | 2014 | QES 2014 | n.l. | 1283 |  |  | unweighted |
| PCQ | 2014 | QES 2014 | n.l. | 1283 |  |  | weighted |
| PCQ | 2018 | QES 2018 | n.l. | 2016 |  |  | unweighted |
| PCQ | 2018 | QES 2018 | n.l. | 2016 |  |  | weighted |
| PCQ | 2018 | 2018 panel | n.l. | 704 |  |  | unweighted |
| PCQ | 2018 | 2018 panel | n.l. | 704 |  |  | weighted |
| PCQ | 2022 | QES 2022 | 13.2 \[11.3, 15.3\] | 1101 | 12.9 | +0.3 pts | unweighted |
| PCQ | 2022 | QES 2022 | 13.5 \[11.3, 16.2\] | 1101 | 12.9 | +0.6 pts | weighted |
| Other | 2007 | QES 2007 | 6.3 \[5.3, 7.6\] | 1727 | 4.1 | +2.2 pts | unweighted |
| Other | 2007 | QES 2007 | 7.0 \[5.6, 8.8\] | 1727 | 4.1 | +2.9 pts | weighted |
| Other | 2007 | 2007 panel | 4.6 \[3.7, 5.8\] | 1494 | 4.1 | +0.5 pts | unweighted |
| Other | 2008 | QES 2008 | 3.1 \[2.2, 4.5\] | 898 | 2.6 | +0.5 pts | unweighted |
| Other | 2012 | QES 2012 | 5.3 \[4.2, 6.6\] | 1274 | 3.8 | +1.5 pts | unweighted |
| Other | 2012 | QES 2012 | 4.4 \[3.4, 5.7\] | 1274 | 3.8 | +0.6 pts | weighted |
| Other | 2012 | 2012 panel | 4.3 \[2.9, 6.1\] | 633 | 3.8 | +0.5 pts | unweighted |
| Other | 2014 | QES 2014 | 3.3 \[2.4, 4.4\] | 1283 | 2.4 | +0.9 pts | unweighted |
| Other | 2014 | QES 2014 | 3.0 \[2.1, 4.2\] | 1283 | 2.4 | +0.5 pts | weighted |
| Other | 2018 | QES 2018 | 4.6 \[3.7, 5.6\] | 2016 | 4.6 | 0.0 pts | unweighted |
| Other | 2018 | QES 2018 | 5.1 \[4.1, 6.3\] | 2016 | 4.6 | +0.5 pts | weighted |
| Other | 2018 | 2018 panel | 6.5 \[4.9, 8.6\] | 704 | 4.6 | +1.9 pts | unweighted |
| Other | 2018 | 2018 panel | 6.4 \[4.5, 9.0\] | 704 | 4.6 | +1.8 pts | weighted |
| Other | 2022 | QES 2022 | 2.8 \[2.0, 4.0\] | 1101 | 1.7 | +1.1 pts | unweighted |
| Other | 2022 | QES 2022 | 3.5 \[2.0, 6.1\] | 1101 | 1.7 | +1.8 pts | weighted |

Surveys under-report the PLQ from 2007 to 2014, and the CAQ by 8 points
in 2022Reported vote in each study (dot, 95% CI) against the official
share of valid votes (line and tick), by party

What to notice:

- The PQ is over-reported in most studies: +7 pts in 2012, +4 pts in
  2014.
- The PLQ is under-reported at every election from 2007 to 2014, by −6
  pts in 2014, the year it won.
- The winner is not always over-reported: in 2022 the CAQ is 8 points
  short of its result.

## Does weighting help?

![Dumbbell chart, one row per study with both estimates (2007 to 2022):
the index of dissimilarity between the reported and the official vote,
unweighted (hollow ring) and weighted (filled dot). Weighting takes 2012
from 11.0 to 8.0 points and 2022 from 9.0 to 8.0, and moves 2014 from
4.9 to 5.6. Values in the table view, with the studies that have no
reviewed
weight.](survey-vs-official_files/figure-html/dissim-light.png)![Dumbbell
chart, one row per study with both estimates (2007 to 2022): the index
of dissimilarity between the reported and the official vote, unweighted
(hollow ring) and weighted (filled dot). Weighting takes 2012 from 11.0
to 8.0 points and 2022 from 9.0 to 8.0, and moves 2014 from 4.9 to 5.6.
Values in the table view, with the studies that have no reviewed
weight.](survey-vs-official_files/figure-html/dissim-dark.png)

Source: qesR, pooled vote_choice (reported vote); official results of
Élections Québec. Index of dissimilarity: half the sum over parties of
the absolute difference between the reported and the official share, in
points (0: identical; the share of voters that would have to change
party). A party a study did not list counts in its Other. The 2008 study
and the 2007 and 2012 panels have no reviewed weight: their unweighted
index is in the table view.

Table view

| Election | Study      | Weighting  | Index of dissimilarity (points) |
|---------:|:-----------|:-----------|:--------------------------------|
|     2007 | QES 2007   | unweighted | 7.7                             |
|     2007 | QES 2007   | weighted   | 7.3                             |
|     2007 | 2007 panel | unweighted | 4.4                             |
|     2008 | QES 2008   | unweighted | 3.2                             |
|     2012 | QES 2012   | unweighted | 11.0                            |
|     2012 | QES 2012   | weighted   | 8.0                             |
|     2012 | 2012 panel | unweighted | 8.0                             |
|     2014 | QES 2014   | unweighted | 4.9                             |
|     2014 | QES 2014   | weighted   | 5.6                             |
|     2018 | QES 2018   | unweighted | 3.2                             |
|     2018 | QES 2018   | weighted   | 3.2                             |
|     2018 | 2018 panel | unweighted | 5.5                             |
|     2018 | 2018 panel | weighted   | 5.5                             |
|     2022 | QES 2022   | unweighted | 9.0                             |
|     2022 | QES 2022   | weighted   | 8.0                             |

Weighting cuts the 2012 error from 11 to 8 points, but does not bring
every study closer to the resultIndex of dissimilarity between the
reported and the official vote, unweighted and weighted, for the studies
with a reviewed weight

What to notice:

- Weighting brings 2012 and 2022 closer to the official result, by 3.0
  and 1.1 points, and moves 2014 slightly away: it adjusts the sample to
  census margins, not to the vote.
- The unweighted 2008 study and 2007 panel, at 3.2 and 4.4 points, are
  closer to the official result than most weighted studies (from 5.5 to
  8.0; only 2018 matches them, at 3.2): weighting to census margins does
  not guarantee a closer vote.

## Two and a half years of CROP polls around the 2008 election

![Line chart of monthly CROP vote intentions from June 2007 to January
2010 for the PLQ, PQ, ADQ and QS, each as a three-month mean over the
faint monthly values, with a line at the election of 8 December 2008 and
the official results as diamonds. The ADQ falls from 29% in June 2007 to
14% in November 2008; the PLQ is at 41% in the last poll before the
election and took 42%; the PQ leads in March to May and in October 2009.
Values in the table
view.](survey-vs-official_files/figure-html/crop-light.png)![Line chart
of monthly CROP vote intentions from June 2007 to January 2010 for the
PLQ, PQ, ADQ and QS, each as a three-month mean over the faint monthly
values, with a line at the election of 8 December 2008 and the official
results as diamonds. The ADQ falls from 29% in June 2007 to 14% in
November 2008; the PLQ is at 41% in the last poll before the election
and took 42%; the PQ leads in March to May and in October 2009. Values
in the table view.](survey-vs-official_files/figure-html/crop-dark.png)

Source: qesR, pooled vote_choice of the CROP polls (the intention, with
the undecided pushed where the poll pushed them), about 1,000
respondents a month, among those who named a party. Unweighted: the
polls' weight (XPOND) is under review. Bold lines: the mean of the polls
within a month and a half of each poll; faint lines: each month. The 95%
confidence intervals (logit) of every poll are in the table view.
Diamonds: official results of 8 December 2008.

Table view

| Poll (month) | Party | Intention, % \[95% CI\] | n   |
|:-------------|:------|:------------------------|:----|
| 2007-06      | PLQ   | 25.8 \[23.0, 28.8\]     | 869 |
| 2007-06      | PQ    | 29.6 \[26.6, 32.7\]     | 869 |
| 2007-06      | ADQ   | 29.5 \[26.5, 32.6\]     | 869 |
| 2007-06      | QS    | 6.4 \[5.0, 8.3\]        | 869 |
| 2007-06      | CAQ   | —                       |     |
| 2007-06      | PCQ   | —                       |     |
| 2007-06      | Other | 8.7 \[7.0, 10.8\]       | 869 |
| 2007-08      | PLQ   | 25.0 \[22.2, 28.0\]     | 865 |
| 2007-08      | PQ    | 34.6 \[31.5, 37.8\]     | 865 |
| 2007-08      | ADQ   | 29.7 \[26.8, 32.8\]     | 865 |
| 2007-08      | QS    | 4.3 \[3.1, 5.8\]        | 865 |
| 2007-08      | CAQ   | —                       |     |
| 2007-08      | PCQ   | —                       |     |
| 2007-08      | Other | 6.5 \[5.0, 8.3\]        | 865 |
| 2007-09      | PLQ   | 21.6 \[19.0, 24.5\]     | 866 |
| 2007-09      | PQ    | 31.4 \[28.4, 34.6\]     | 866 |
| 2007-09      | ADQ   | 34.8 \[31.7, 38.0\]     | 866 |
| 2007-09      | QS    | 5.5 \[4.2, 7.3\]        | 866 |
| 2007-09      | CAQ   | —                       |     |
| 2007-09      | PCQ   | —                       |     |
| 2007-09      | Other | 6.7 \[5.2, 8.6\]        | 866 |
| 2007-10      | PLQ   | 27.6 \[24.7, 30.7\]     | 847 |
| 2007-10      | PQ    | 31.8 \[28.7, 35.0\]     | 847 |
| 2007-10      | ADQ   | 30.5 \[27.5, 33.6\]     | 847 |
| 2007-10      | QS    | 3.8 \[2.7, 5.3\]        | 847 |
| 2007-10      | CAQ   | —                       |     |
| 2007-10      | PCQ   | —                       |     |
| 2007-10      | Other | 6.4 \[4.9, 8.2\]        | 847 |
| 2007-11      | PLQ   | 26.6 \[23.8, 29.6\]     | 884 |
| 2007-11      | PQ    | 35.1 \[32.0, 38.3\]     | 884 |
| 2007-11      | ADQ   | 27.4 \[24.5, 30.4\]     | 884 |
| 2007-11      | QS    | 4.4 \[3.2, 6.0\]        | 884 |
| 2007-11      | CAQ   | —                       |     |
| 2007-11      | PCQ   | —                       |     |
| 2007-11      | Other | 6.6 \[5.1, 8.4\]        | 884 |
| 2008-01      | PLQ   | 29.3 \[26.3, 32.3\]     | 882 |
| 2008-01      | PQ    | 36.1 \[32.9, 39.3\]     | 882 |
| 2008-01      | ADQ   | 24.6 \[21.9, 27.6\]     | 882 |
| 2008-01      | QS    | 5.1 \[3.8, 6.8\]        | 882 |
| 2008-01      | CAQ   | —                       |     |
| 2008-01      | PCQ   | —                       |     |
| 2008-01      | Other | 5.0 \[3.7, 6.6\]        | 882 |
| 2008-02      | PLQ   | 31.4 \[28.3, 34.6\]     | 845 |
| 2008-02      | PQ    | 33.1 \[30.0, 36.4\]     | 845 |
| 2008-02      | ADQ   | 23.8 \[21.0, 26.8\]     | 845 |
| 2008-02      | QS    | 5.1 \[3.8, 6.8\]        | 845 |
| 2008-02      | CAQ   | —                       |     |
| 2008-02      | PCQ   | —                       |     |
| 2008-02      | Other | 6.6 \[5.1, 8.5\]        | 845 |
| 2008-03      | PLQ   | 32.8 \[29.8, 36.0\]     | 875 |
| 2008-03      | PQ    | 32.2 \[29.2, 35.4\]     | 875 |
| 2008-03      | ADQ   | 22.4 \[19.8, 25.3\]     | 875 |
| 2008-03      | QS    | 6.1 \[4.7, 7.8\]        | 875 |
| 2008-03      | CAQ   | —                       |     |
| 2008-03      | PCQ   | —                       |     |
| 2008-03      | Other | 6.5 \[5.1, 8.4\]        | 875 |
| 2008-04      | PLQ   | 36.2 \[33.0, 39.6\]     | 817 |
| 2008-04      | PQ    | 31.0 \[27.9, 34.2\]     | 817 |
| 2008-04      | ADQ   | 18.6 \[16.1, 21.4\]     | 817 |
| 2008-04      | QS    | 5.6 \[4.2, 7.4\]        | 817 |
| 2008-04      | CAQ   | —                       |     |
| 2008-04      | PCQ   | —                       |     |
| 2008-04      | Other | 8.6 \[6.8, 10.7\]       | 817 |
| 2008-05      | PLQ   | 37.9 \[34.7, 41.2\]     | 849 |
| 2008-05      | PQ    | 35.8 \[32.6, 39.1\]     | 849 |
| 2008-05      | ADQ   | 13.9 \[11.7, 16.4\]     | 849 |
| 2008-05      | QS    | 6.0 \[4.6, 7.8\]        | 849 |
| 2008-05      | CAQ   | —                       |     |
| 2008-05      | PCQ   | —                       |     |
| 2008-05      | Other | 6.4 \[4.9, 8.2\]        | 849 |
| 2008-06      | PLQ   | 33.3 \[30.3, 36.5\]     | 877 |
| 2008-06      | PQ    | 35.5 \[32.4, 38.7\]     | 877 |
| 2008-06      | ADQ   | 17.0 \[14.6, 19.6\]     | 877 |
| 2008-06      | QS    | 8.0 \[6.4, 10.0\]       | 877 |
| 2008-06      | CAQ   | —                       |     |
| 2008-06      | PCQ   | —                       |     |
| 2008-06      | Other | 6.3 \[4.8, 8.1\]        | 877 |
| 2008-08      | PLQ   | 40.8 \[37.5, 44.2\]     | 818 |
| 2008-08      | PQ    | 33.4 \[30.2, 36.7\]     | 818 |
| 2008-08      | ADQ   | 16.6 \[14.2, 19.3\]     | 818 |
| 2008-08      | QS    | 3.9 \[2.8, 5.5\]        | 818 |
| 2008-08      | CAQ   | —                       |     |
| 2008-08      | PCQ   | —                       |     |
| 2008-08      | Other | 5.3 \[3.9, 7.0\]        | 818 |
| 2008-09      | PLQ   | 39.7 \[36.6, 43.0\]     | 911 |
| 2008-09      | PQ    | 33.6 \[30.6, 36.7\]     | 911 |
| 2008-09      | ADQ   | 16.9 \[14.6, 19.5\]     | 911 |
| 2008-09      | QS    | 4.5 \[3.3, 6.1\]        | 911 |
| 2008-09      | CAQ   | —                       |     |
| 2008-09      | PCQ   | —                       |     |
| 2008-09      | Other | 5.3 \[4.0, 6.9\]        | 911 |
| 2008-10      | PLQ   | 36.9 \[33.7, 40.2\]     | 849 |
| 2008-10      | PQ    | 33.9 \[30.8, 37.2\]     | 849 |
| 2008-10      | ADQ   | 16.1 \[13.8, 18.8\]     | 849 |
| 2008-10      | QS    | 5.5 \[4.2, 7.3\]        | 849 |
| 2008-10      | CAQ   | —                       |     |
| 2008-10      | PCQ   | —                       |     |
| 2008-10      | Other | 7.5 \[5.9, 9.5\]        | 849 |
| 2008-11      | PLQ   | 41.3 \[38.0, 44.6\]     | 870 |
| 2008-11      | PQ    | 33.0 \[29.9, 36.2\]     | 870 |
| 2008-11      | ADQ   | 13.7 \[11.5, 16.1\]     | 870 |
| 2008-11      | QS    | 4.1 \[3.0, 5.7\]        | 870 |
| 2008-11      | CAQ   | —                       |     |
| 2008-11      | PCQ   | —                       |     |
| 2008-11      | Other | 7.9 \[6.3, 9.9\]        | 870 |
| 2009-01      | PLQ   | 39.3 \[36.1, 42.7\]     | 839 |
| 2009-01      | PQ    | 35.4 \[32.2, 38.7\]     | 839 |
| 2009-01      | ADQ   | 13.2 \[11.1, 15.7\]     | 839 |
| 2009-01      | QS    | 6.4 \[5.0, 8.3\]        | 839 |
| 2009-01      | CAQ   | —                       |     |
| 2009-01      | PCQ   | —                       |     |
| 2009-01      | Other | 5.6 \[4.2, 7.4\]        | 839 |
| 2009-03      | PLQ   | 32.7 \[29.5, 36.0\]     | 798 |
| 2009-03      | PQ    | 40.4 \[37.0, 43.8\]     | 798 |
| 2009-03      | ADQ   | 10.8 \[8.8, 13.1\]      | 798 |
| 2009-03      | QS    | 8.6 \[6.9, 10.8\]       | 798 |
| 2009-03      | CAQ   | —                       |     |
| 2009-03      | PCQ   | —                       |     |
| 2009-03      | Other | 7.5 \[5.9, 9.6\]        | 798 |
| 2009-04      | PLQ   | 36.0 \[32.8, 39.2\]     | 851 |
| 2009-04      | PQ    | 40.9 \[37.6, 44.2\]     | 851 |
| 2009-04      | ADQ   | 8.8 \[7.1, 10.9\]       | 851 |
| 2009-04      | QS    | 6.0 \[4.6, 7.8\]        | 851 |
| 2009-04      | CAQ   | —                       |     |
| 2009-04      | PCQ   | —                       |     |
| 2009-04      | Other | 8.3 \[6.7, 10.4\]       | 851 |
| 2009-05      | PLQ   | 36.7 \[33.5, 40.1\]     | 830 |
| 2009-05      | PQ    | 38.1 \[34.8, 41.4\]     | 830 |
| 2009-05      | ADQ   | 10.2 \[8.4, 12.5\]      | 830 |
| 2009-05      | QS    | 7.5 \[5.9, 9.5\]        | 830 |
| 2009-05      | CAQ   | —                       |     |
| 2009-05      | PCQ   | —                       |     |
| 2009-05      | Other | 7.5 \[5.9, 9.5\]        | 830 |
| 2009-06      | PLQ   | 38.9 \[35.6, 42.3\]     | 814 |
| 2009-06      | PQ    | 38.5 \[35.2, 41.8\]     | 814 |
| 2009-06      | ADQ   | 9.8 \[8.0, 12.1\]       | 814 |
| 2009-06      | QS    | 7.1 \[5.5, 9.1\]        | 814 |
| 2009-06      | CAQ   | —                       |     |
| 2009-06      | PCQ   | —                       |     |
| 2009-06      | Other | 5.7 \[4.3, 7.5\]        | 814 |
| 2009-08      | PLQ   | 41.1 \[37.8, 44.4\]     | 840 |
| 2009-08      | PQ    | 36.2 \[33.0, 39.5\]     | 840 |
| 2009-08      | ADQ   | 8.1 \[6.4, 10.1\]       | 840 |
| 2009-08      | QS    | 8.6 \[6.9, 10.7\]       | 840 |
| 2009-08      | CAQ   | —                       |     |
| 2009-08      | PCQ   | —                       |     |
| 2009-08      | Other | 6.1 \[4.6, 7.9\]        | 840 |
| 2009-09      | PLQ   | 41.2 \[37.8, 44.6\]     | 794 |
| 2009-09      | PQ    | 36.5 \[33.2, 39.9\]     | 794 |
| 2009-09      | ADQ   | 7.6 \[5.9, 9.6\]        | 794 |
| 2009-09      | QS    | 6.8 \[5.2, 8.8\]        | 794 |
| 2009-09      | CAQ   | —                       |     |
| 2009-09      | PCQ   | —                       |     |
| 2009-09      | Other | 7.9 \[6.2, 10.0\]       | 794 |
| 2009-10      | PLQ   | 36.6 \[33.3, 40.0\]     | 803 |
| 2009-10      | PQ    | 41.5 \[38.1, 44.9\]     | 803 |
| 2009-10      | ADQ   | 9.2 \[7.4, 11.4\]       | 803 |
| 2009-10      | QS    | 6.7 \[5.2, 8.7\]        | 803 |
| 2009-10      | CAQ   | —                       |     |
| 2009-10      | PCQ   | —                       |     |
| 2009-10      | Other | 6.0 \[4.5, 7.8\]        | 803 |
| 2010-01      | PLQ   | 38.8 \[35.4, 42.2\]     | 784 |
| 2010-01      | PQ    | 38.4 \[35.0, 41.9\]     | 784 |
| 2010-01      | ADQ   | 7.3 \[5.6, 9.3\]        | 784 |
| 2010-01      | QS    | 8.7 \[6.9, 10.9\]       | 784 |
| 2010-01      | CAQ   | —                       |     |
| 2010-01      | PCQ   | —                       |     |
| 2010-01      | Other | 6.9 \[5.3, 8.9\]        | 784 |

The ADQ fell from 29% to 14% before the 2008 election; the PLQ led from
August 2008, and the PQ passed it in 2009Monthly vote intentions in the
CROP polls, June 2007 to January 2010: three-month means over the
monthly values, and the official result

What to notice:

- The ADQ, official opposition after March 2007, falls through 2008 and
  ends near its election score.
- The PLQ leads from August 2008 to the election, and its November 2008
  poll (41%) is close to its result (42%). In June 2008 the PQ was still
  ahead (35% against 33%).
- In 2009 the PQ overtakes the PLQ in the spring (March to May) and in
  October, and the two are level in January 2010.

## About the data

- **Studies.** Every study that asked the reported vote after the
  election, except the 1998 polls, which interviewed francophones only;
  the CROP polls for intentions.
- **Variable.** `vote_choice`, the reported vote (`recall`) in every
  study but the CROP polls, whose values are intentions
  (`intention_push`, or `intention` where no push was asked). Shares are
  among respondents who named a party; would not vote, did not vote,
  don’t know and refused are left out.
- **Official results.** The share of valid votes of each party, from
  Élections Québec, kept in the qesR source repository
  (`inst/extdata/validation/official_results.csv`). The PV, ON and other
  parties are “Other”; a party a study did not list counts in its Other.
- **Weights.** Each study’s recommended post-election weight, through
  [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md);
  the unweighted estimate is shown next to it. The 2008 study, the 2007
  and 2012 Durand panels and the CROP polls are unweighted (weights
  under review); the 2018 panel uses its reviewed post-election weight.
  [Validation against official
  results](https://thomasgareau.github.io/qesR/articles/validation.md)
  runs these checks on every study every week.
