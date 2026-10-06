---
title: "qesR: Quebec Election Studies in R"
---

# qesR <img src="logo.svg" align="right" height="139" alt="qesR logo" />

*[Version française](articles/fr-accueil.html)*

qesR loads the Quebec Election Studies, with the panels and polls that
accompanied them, into R: seven provincial elections from 1998 to 2022,
each study by its code, from its original file. Their codebooks, question
wording and a search across all studies ship with the package and work
offline, in English and French. qesR also harmonizes 11 of the studies into
one data frame, so that a question about 25 years of Quebec elections can be
answered within each study and then compared.

```{=html}
<div class="qesr-hero">
<figure>
<figcaption>In 2007 the youngest francophones were the cohort most likely to vote Yes; in 2022 they were the least likely</figcaption>
<a href="articles/sovereignty-generations.html"><picture class="qesr-pic qesr-light"><source media="(max-width: 576px)" srcset="articles/sovereignty-generations_files/figure-html/gradient-light-narrow.png"><img class="qesr-img qesr-light" src="articles/sovereignty-generations_files/figure-html/gradient-light.png" alt="Dot charts in six panels, one per Quebec Election Study from 2007 to 2022, of the share of francophones who would vote Yes in each birth cohort. In 2007 the youngest cohort is the most likely to vote Yes; in 2022 it is the least likely. Details on the sovereignty page." width="672" height="461" loading="lazy"></picture><picture class="qesr-pic qesr-dark"><source media="(max-width: 576px)" srcset="articles/sovereignty-generations_files/figure-html/gradient-dark-narrow.png"><img class="qesr-img qesr-dark" src="articles/sovereignty-generations_files/figure-html/gradient-dark.png" alt="Dot charts in six panels, one per Quebec Election Study from 2007 to 2022, of the share of francophones who would vote Yes in each birth cohort. In 2007 the youngest cohort is the most likely to vote Yes; in 2022 it is the least likely. Details on the sovereignty page." width="672" height="461" loading="lazy"></picture></a>
</figure>
<figure>
<figcaption>Party choice is far less tied to the referendum vote than in 2012, and left-right has not taken its place</figcaption>
<a href="articles/dimensions.html"><picture class="qesr-pic qesr-light"><source media="(max-width: 576px)" srcset="articles/dimensions_files/figure-html/sorting-light-narrow.png"><img class="qesr-img qesr-light" src="articles/dimensions_files/figure-html/sorting-light.png" alt="Line charts in two panels, all voters and francophone voters, 2012 to 2022, of how much of the variation in the referendum vote and in left-right placement lies between the parties' electorates: the referendum vote is far less tied to party choice in 2022 than in 2012, and left-right placement did not rise. Details on the dimensions page." width="672" height="403" loading="lazy"></picture><picture class="qesr-pic qesr-dark"><source media="(max-width: 576px)" srcset="articles/dimensions_files/figure-html/sorting-dark-narrow.png"><img class="qesr-img qesr-dark" src="articles/dimensions_files/figure-html/sorting-dark.png" alt="Line charts in two panels, all voters and francophone voters, 2012 to 2022, of how much of the variation in the referendum vote and in left-right placement lies between the parties' electorates: the referendum vote is far less tied to party choice in 2022 than in 2012, and left-right placement did not rise. Details on the dimensions page." width="672" height="403" loading="lazy"></picture></a>
</figure>
</div>
```

Each estimate is made within one study, with that study's weight where it
has a validated one and a 95% confidence interval; nothing is pooled across
studies. [Get started](articles/get-started.html) goes from a study code to
a weighted estimate, offline, on a small synthetic study that ships with
qesR.

## Examples

Each page takes a common belief about Quebec elections and sets it against
the studies.

- [From two parties to four](articles/realignment.html): the vote
  fragmented, mainly among francophones, and within each sovereignty camp
  rather than across them.
- [Are young Quebecers still the most pro-independence?](articles/sovereignty-generations.html):
  the youngest francophones were the most likely to vote Yes in 2007 and
  the least likely in 2022, mostly through change within cohorts.
- [Two dimensions of competition](articles/dimensions.html): the national
  question structures the vote less than in 2012, and the left-right scale
  has not taken its place.
- [Who votes?](articles/turnout.html): the young report voting less at
  every election since 1998, and interest in politics accounts for only a
  small part of the gap.
- [Are Quebec elections decided during the campaign?](articles/transitions.html):
  the same respondents before and after the vote show that a campaign moves
  many voters but, on net, few votes.
- [Do surveys miss the Liberals?](articles/survey-vs-official.html): the
  Liberal vote was under-reported from 2007 to 2014, but the gap faded in
  2018 and 2022.
- [Recipes: the language divide in the Liberal vote](articles/recipes.html):
  the code, step by step, from one harmonized variable to an estimate for
  every study.

## Installation

```r
# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

Once qesR is accepted on CRAN, `install.packages("qesR")` will do the same.
A first session (the lines marked "downloads" fetch studies from their
deposits the first time; `options(qesR.cache = "disk")` keeps them):

```r
library(qesR)
qes_studies()                         # the studies, offline
qes2018 <- get_qes("qes2018")         # downloads: one study, from its original file
qes_search("souverain|sovereign")     # a question, in every study, offline
h <- qes_harmonize(targets = "vote_choice")  # downloads: the vote, in six studies
d <- qes_decon()                      # downloads: every study in one flat file, relaxed
```

The harmonization rules are experimental and may change from one version of
qesR to the next. Keep `qes_provenance(h, level = "spec")` with your results
and pin the version of qesR you used.

## Citing qesR

Work that uses qesR should cite the package and each study it draws on.
`qes_cite()` writes both, with each dataset's DOI:

```r
qes_cite("qes2018")                   # qesR and the 2018 study
qes_cite("qes2018", style = "bibtex")
```

[Citing qesR and the studies](articles/citations.html) gives the citation
of every study.

## Licence

The code of qesR is under the MIT licence. The data are not part of the
package: qesR downloads each study from its deposit on Borealis or the
Harvard Dataverse. Most studies are released under CC0; the 2022 study,
and the metadata qesR ships from it, are under CC BY-NC 4.0 (attribution,
no commercial use). [Details and the required
attribution](articles/citations.html#licences-and-attribution).
