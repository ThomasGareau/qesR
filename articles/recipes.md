# Recipes: pooled variables in five minutes

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-recettes.md)*

Each recipe below runs as is on the full data files, which qesR
downloads once through its cache. The other example pages are built from
these same steps.

``` r

library(qesR)
library(survey)
library(ggplot2)
```

## 1. One vote-choice variable for every study

`vote_choice` pools the reported vote, the pushed intention and the
plain intention of every study into one column; `vote_choice__type` says
which question each row answers.

``` r

studies <- c("qes1998", "qes2007", "qes2007_panel", "qes2008", "qes_crop_2007_2010",
             "qes2012", "qes2012_panel", "qes2014", "qes2018", "qes2018_panel", "qes2022")
h <- qes_harmonize(studies = studies, targets = c("vote_choice", "lang_mother"),
                   layout = "long", quiet = TRUE)
table(h$study, h$vote_choice__type)
#>                     
#>                      recall intention_push intention
#>   qes_crop_2007_2010      0          22848      1179
#>   qes1998              1483           1483         0
#>   qes2007              2175              0         0
#>   qes2007_panel        2054           1997        53
#>   qes2008              1151              0         0
#>   qes2012              1505              0         0
#>   qes2012_panel         844            844         0
#>   qes2014              1517              0         0
#>   qes2018              3072              0         0
#>   qes2018_panel         842           1250         0
#>   qes2022              1220           1521         0
```

In the long layout a pre/post study has one row per wave: the intention
before the election and the reported vote after it. In the default
respondent layout each study’s pooled value comes from one wave, so that
one weight fits it.

## 2. Keep one type only

`types` restricts a pooled variable to some of its members. The reported
vote only:

``` r

r <- qes_harmonize(studies = studies, targets = c("vote_choice", "lang_mother"),
                   types = list(vote_choice = "recall"), quiet = TRUE)
table(r$study, r$vote_choice__type, useNA = "ifany")
#>                     
#>                      recall intention_push intention  <NA>
#>   qes_crop_2007_2010      0              0         0 24027
#>   qes1998              1483              0         0     0
#>   qes2007              2175              0         0     0
#>   qes2007_panel        2054              0         0   388
#>   qes2008              1151              0         0     0
#>   qes2012              1505              0         0     0
#>   qes2012_panel         844              0         0     0
#>   qes2014              1517              0         0     0
#>   qes2018              3072              0         0     0
#>   qes2018_panel         842              0         0   408
#>   qes2022              1220              0         0   301
```

Within a study-wave the members are tried in a fixed order (recall, then
the intention with the undecided pushed, then the plain intention), and
the first one that asked the respondent sets the row.
`qes_spec("pooled")` lists that order.

## 3. Where each value comes from

``` r

unique(r[!is.na(r$vote_choice__item), c("study", "vote_choice__item", "vote_choice__grade")])
#>               study           vote_choice__item vote_choice__grade
#> 1           qes1998         qes1998:post:q3post         comparable
#> 1484        qes2007            qes2007:post:q12         comparable
#> 3659  qes2007_panel     qes2007_panel:post:vote        approximate
#> 6101        qes2008           qes2008:post:q12a         comparable
#> 31279       qes2012            qes2012:post:q25          identical
#> 32784 qes2012_panel qes2012_panel:post:voteprov        approximate
#> ... and 4 more row(s).
qes_provenance(r, level = "pooled")
#>                 study      pooled   type           member precedence wave
#> 1             qes1998 vote_choice recall vote_prov_recall          1 post
#> 2             qes2007 vote_choice recall vote_prov_recall          1 post
#> 3       qes2007_panel vote_choice recall vote_prov_recall          1 post
#> 4             qes2008 vote_choice recall vote_prov_recall          1 post
#> 5  qes_crop_2007_2010 vote_choice recall vote_prov_recall          1 <NA>
#> 6             qes2012 vote_choice recall vote_prov_recall          1 post
#> 7       qes2012_panel vote_choice recall vote_prov_recall          1 post
#> 8             qes2014 vote_choice recall vote_prov_recall          1 post
#> 9             qes2018 vote_choice recall vote_prov_recall          1 post
#> 10      qes2018_panel vote_choice recall vote_prov_recall          1 post
#> 11            qes2022 vote_choice recall vote_prov_recall          1  pes
#>                           item       grade used_in_layout included n_value
#> 1          qes1998:post:q3post  comparable           TRUE     TRUE    1126
#> 2             qes2007:post:q12  comparable           TRUE     TRUE    1727
#> 3      qes2007_panel:post:vote approximate           TRUE     TRUE    1494
#> 4            qes2008:post:q12a  comparable           TRUE     TRUE     898
#> 5                         <NA>        <NA>           TRUE    FALSE       0
#> 6             qes2012:post:q25   identical           TRUE     TRUE    1274
#> 7  qes2012_panel:post:voteprov approximate           TRUE     TRUE     633
#> 8              qes2014:post:Q3  comparable           TRUE     TRUE    1283
#> 9              qes2018:post:q6  comparable           TRUE     TRUE    2016
#> 10   qes2018_panel:post:rts_q2  comparable           TRUE     TRUE     704
#> 11  qes2022:pes:pes_votechoice  comparable           TRUE     TRUE    1101
#>    n_answer_na n_fallthrough
#> 1          355             2
#> 2          448             0
#> 3          560             0
#> 4          253             0
#> 5            0             0
#> 6          231             0
#> 7          211             0
#> 8          234             0
#> 9          786           270
#> 10         138             0
#> 11         119             0
```

The grade is the grade of the study’s question (`identical`,
`comparable`, `approximate`); the harmonization reference gives the
reasons.

## 4. A weighted share with its confidence interval, in one study

``` r

d <- qes_design(r[r$study == "qes2018", ], weight = "weight_post")
d <- subset(d, !is.na(vote_choice))
svyciprop(~I(vote_choice == "CAQ"), d, method = "logit")
#>                                2.5% 97.5%
#> I(vote_choice == "CAQ") 0.358 0.336 0.381
```

## 5. By group

``` r

svyby(~I(vote_choice == "PLQ"), ~lang_mother, subset(d, !is.na(lang_mother)),
      svyciprop, method = "logit", vartype = "ci")
#>         lang_mother I(vote_choice == "PLQ")      ci_l      ci_u
#> French       French               0.1223020 0.1070069 0.1394418
#> English     English               0.7418131 0.6814276 0.7942093
#> Other         Other               0.6416905 0.5267904 0.7423376
```

## 6. Every study, without pooling them

Studies differ in population, mode, wording and weighting: estimate
within each study, then compare. Studies whose weight is still under
review have `NA` weights, and
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
leaves their rows out.

``` r

weighted <- c("qes2007", "qes2012", "qes2014", "qes2018", "qes2018_panel", "qes2022")
# 2007: the CAQ did not yet exist (it was founded in 2011), so the loop starts in 2012
caq <- do.call(rbind, lapply(setdiff(weighted, "qes2007"), function(s) {
  ds <- qes_design(r[r$study == s, ], weight = "weight_post")
  ds <- subset(ds, !is.na(vote_choice))
  est <- svyciprop(~I(vote_choice == "CAQ"), ds, method = "logit")
  ci <- attr(est, "ci")
  data.frame(study = s, pct = 100 * as.numeric(est), lo = 100 * ci[[1]], hi = 100 * ci[[2]])
}))
caq
#>           study      pct       lo       hi
#> 1       qes2012 25.37535 22.78580 28.15190
#> 2       qes2014 23.12423 20.47336 26.00610
#> 3       qes2018 35.81207 33.57731 38.11022
#> 4 qes2018_panel 39.08423 34.80571 43.53747
#> 5       qes2022 33.02279 29.33798 36.92854
```

Pooling makes sense for a model that keeps the studies apart, such as a
regression with a term for each study: the coefficient of a French
mother tongue is then estimated within studies (on the logit scale,
common to all of them). (A single PLQ share pooled over 2007-2022 would
mean nothing.) `pool = "equal"` gives every study the same total weight;
by default a study counts in proportion to its number of respondents:

``` r

dp <- qes_design(r[r$study %in% weighted, ], weight = "weight_post", pool = "equal")
dp <- subset(dp, !is.na(vote_choice) & !is.na(lang_mother))
m <- svyglm(I(vote_choice == "PLQ") ~ I(lang_mother == "French") + study, dp,
            family = quasibinomial())
summary(m)$coefficients[1:2, ]
#>                                  Estimate Std. Error   t value      Pr(>|t|)
#> (Intercept)                     0.9868092  0.1253390   7.87312  3.921822e-15
#> I(lang_mother == "French")TRUE -2.5083612  0.1013489 -24.74976 2.769547e-130
```

## 7. Sovereignty: one variable, several wordings

`sov_support` pools the referendum questions; compare within one wording
(`sov_support__type`):

``` r

s <- qes_harmonize(studies = studies, targets = "sov_support", quiet = TRUE)
table(s$study, s$sov_support__type)
#>                     
#>                      independence sovereign_country partnership_1995_push
#>   qes_crop_2007_2010            0                 0                     0
#>   qes1998                       0                 0                  1483
#>   qes2007                       0                 0                  2175
#>   qes2007_panel                 0                 0                  2050
#>   qes2008                       0                 0                  1151
#>   qes2012                    1505                 0                     0
#>   qes2012_panel                 0               844                     0
#>   qes2014                    1517                 0                     0
#>   qes2018                    3072                 0                     0
#>   qes2018_panel                 0                 0                     0
#>   qes2022                    1521                 0                     0
#>                     
#>                      partnership_1995 favour
#>   qes_crop_2007_2010                0      0
#>   qes1998                           0      0
#>   qes2007                           0      0
#>   qes2007_panel                     0      0
#>   qes2008                           0      0
#>   qes2012                           0      0
#>   qes2012_panel                     0      0
#>   qes2014                           0      0
#>   qes2018                           0      0
#>   qes2018_panel                     0    842
#>   qes2022                           0      0
```

## 8. The ADQ and the CAQ in one time series

The harmonized columns keep the ADQ and the CAQ apart.
[`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md)
adds a column that joins them, for time series; the rows fielded before
the merger of 2012 are graded `approximate` there.

``` r

l <- qes_party_lineage(r, cols = "vote_choice")
table(l$study, l$vote_choice_lineage)[, c("PLQ", "PQ", "ADQ/CAQ", "QS")]
#>                     
#>                      PLQ  PQ ADQ/CAQ  QS
#>   qes_crop_2007_2010   0   0       0   0
#>   qes1998            384 517     204   0
#>   qes2007            439 521     583  75
#>   qes2007_panel      428 460     480  57
#>   qes2008            352 336     144  38
#>   qes2012            278 509     324  96
#>   qes2012_panel      167 243     151  45
#>   qes2014            489 344     277 131
#>   qes2018            490 392     700 342
#>   qes2018_panel      200 108     263  87
#>   qes2022            146 206     364 209
```

## 9. A first figure: the ADQ and the CAQ as one series

Recipe 8’s lineage column, weighted within each study, against the
official result. `p` is a plain ggplot2 chart, ready to adapt (print it
with `p`); the site draws the same chart with the theme of its example
pages, below.

``` r

qes <- c("qes2007", "qes2008", "qes2012", "qes2014", "qes2018", "qes2022")
lineage <- do.call(rbind, lapply(qes, function(s) {
  x <- l[l$study == s, ]
  weighted <- any(!is.na(x$weight_post))
  if (!weighted) x$weight_post <- 1   # qes2008: weight under review, so unweighted
  ds <- subset(qes_design(x, weight = "weight_post"), !is.na(vote_choice_lineage))
  est <- svyciprop(~I(vote_choice_lineage == "ADQ/CAQ"), ds, method = "logit")
  ci <- attr(est, "ci")
  data.frame(study = s, year = as.integer(substr(x$year[1], 1, 4)), weighted = weighted,
             pct = 100 * as.numeric(est), lo = 100 * ci[[1]], hi = 100 * ci[[2]])
}))
# the official share of valid votes of the ADQ (2007, 2008) and of the CAQ
# (2012 on), from Élections Québec
official <- data.frame(year = c(2007, 2008, 2012, 2014, 2018, 2022),
                       pct = c(30.8, 16.4, 27.1, 23.1, 37.4, 41.0))
fr <- identical(params$lang, "fr")
lab_series <- if (fr) c(lineage = "ADQ (2007, 2008), puis CAQ : vote déclaré", official = "Résultat officiel") else
  c(lineage = "ADQ (2007, 2008), then CAQ: reported vote", official = "Official result")
lab_weight <- if (fr) c(`TRUE` = "pondéré", `FALSE` = "non pondéré (pondération en révision)") else
  c(`TRUE` = "weighted", `FALSE` = "unweighted (weight under review)")

p <- ggplot(lineage, aes(year, pct)) +
  geom_line(data = official, aes(colour = "official")) +
  geom_point(data = official, aes(colour = "official"), shape = 45, size = 9) +
  geom_linerange(aes(ymin = lo, ymax = hi, colour = "lineage")) +
  geom_point(aes(colour = "lineage", fill = weighted), shape = 21, size = 3, stroke = 1) +
  scale_colour_manual(values = c(lineage = "#1d97b0", official = "grey20"), labels = lab_series) +
  scale_fill_manual(values = c(`TRUE` = "#1d97b0", `FALSE` = "white"), labels = lab_weight) +
  scale_x_continuous(breaks = c(2007, 2012, 2014, 2018, 2022)) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x = NULL, y = if (fr) "% du vote déclaré" else "% of the reported vote", colour = NULL, fill = NULL)
lineage
#>     study year weighted      pct       lo       hi
#> 1 qes2007 2007     TRUE 31.59171 28.96400 34.34257
#> 2 qes2008 2008    FALSE 16.03563 13.77454 18.58787
#> 3 qes2012 2012     TRUE 25.37535 22.78580 28.15190
#> 4 qes2014 2014     TRUE 23.12423 20.47336 26.00610
#> 5 qes2018 2018     TRUE 35.81207 33.57731 38.11022
#> 6 qes2022 2022     TRUE 33.02279 29.33798 36.92854
```

![Dot chart with confidence intervals of the share of the reported vote
for the ADQ in 2007 and 2008 and for the CAQ from 2012 to 2022, in the
Quebec Election Studies, with the official result as a line with ticks.
In 2022 the study has 33% against 41% officially. Values in the table
view.](recipes_files/figure-html/lineage-light.png)![Dot chart with
confidence intervals of the share of the reported vote for the ADQ in
2007 and 2008 and for the CAQ from 2012 to 2022, in the Quebec Election
Studies, with the official result as a line with ticks. In 2022 the
study has 33% against 41% officially. Values in the table
view.](recipes_files/figure-html/lineage-dark.png)

Source: qesR, pooled vote_choice (reported vote) with
qes_party_lineage(); official results of Élections Québec. The ADQ
merged into the CAQ in 2012: the lineage column joins them for a time
series, and its rows before the merger are graded approximate. Weighted
with each study's post-election weight; hollow (2008): unweighted,
weight under review.

Table view

| Election | Study | Party | Reported vote, % \[95% CI\] | Official, % of valid votes | Difference | Weighting |
|---:|:---|:---|:---|:---|:---|:---|
| 2007 | QES 2007 | ADQ | 31.6 \[29.0, 34.3\] | 30.8 | +0.8 pts | weighted |
| 2008 | QES 2008 | ADQ | 31.6 \[29.0, 34.3\] | 16.4 | −0.4 pts | unweighted (weight under review) |
| 2012 | QES 2012 | CAQ | 31.6 \[29.0, 34.3\] | 27.1 | −1.7 pts | weighted |
| 2014 | QES 2014 | CAQ | 31.6 \[29.0, 34.3\] | 23.1 | 0.0 pts | weighted |
| 2018 | QES 2018 | CAQ | 31.6 \[29.0, 34.3\] | 37.4 | −1.6 pts | weighted |
| 2022 | QES 2022 | CAQ | 31.6 \[29.0, 34.3\] | 41.0 | −8.0 pts | weighted |

Read as one lineage, the ADQ/CAQ vote is within 2 points of its result
until 2018, and 8 points short in 2022Share of the reported vote for the
ADQ (2007, 2008) and the CAQ (2012 on), one Quebec Election Study per
election, with 95% confidence intervals and the official result

## 10. Respondents by study

``` r

n <- aggregate(qes_id ~ study + year, data = r, FUN = length)
names(n)[3] <- "respondents"
n[order(n$year, n$study), ]
#>                 study year respondents
#> 1             qes1998 1998        1483
#> 2  qes_crop_2007_2010 2007        5007
#> 3             qes2007 2007        2175
#> 4       qes2007_panel 2007        2442
#> 5  qes_crop_2007_2010 2008       10012
#> 6             qes2008 2008        1151
#> 7  qes_crop_2007_2010 2009        8008
#> 8  qes_crop_2007_2010 2010        1000
#> 9             qes2012 2012        1505
#> 10      qes2012_panel 2012         844
#> 11            qes2014 2014        1517
#> 12            qes2018 2018        3072
#> 13      qes2018_panel 2018        1250
#> 14            qes2022 2022        1521
```
