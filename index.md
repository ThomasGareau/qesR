# qesR

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-accueil.md)*

The Quebec Election Studies, with the panels and polls that accompanied
them, cover seven provincial elections, from 1998 to 2022, and their
data are public. Using them together is another matter. They sit in
separate deposits, in SPSS or Stata files, each with a codebook of its
own, and the wording of a question often changed from one study to the
next. qesR brings them into R. Each study loads by its code, from its
original file, checked before use; its codebook, its question wording
and a search across all studies work offline, in English and French.
qesR also harmonizes the 11 studies of 1998 to 2022 into one data frame,
question by question, with a comparability grade for each study’s
question and a reason for each missing value. In other words, a question
about 25 years of Quebec elections can be answered within each study and
then compared, without recoding every file by hand.

[Get
started](https://thomasgareau.github.io/qesR/articles/get-started.md)
goes from a study code to a weighted estimate. It runs offline, on a
small synthetic study that ships with qesR.

## What the studies show

Two results from the examples. Each estimate is made within one study,
with that study’s weight where it has one and a 95% confidence interval;
nothing is pooled across studies.

[![Line chart of the effective number of parties at each Quebec election
from 1998 to 2022: the official result, francophone respondents and
non-francophone respondents. Among francophones it rises from 2.8 in
1998 to 3.9 in 2022; among non-francophones it stays between 1.3 and 2.5
until 2018 and reaches 2.9 in 2022, with a wide interval. Details on the
realignment
page.](articles/realignment_files/figure-html/enp-light.png)![Line chart
of the effective number of parties at each Quebec election from 1998 to
2022: the official result, francophone respondents and non-francophone
respondents. Among francophones it rises from 2.8 in 1998 to 3.9 in
2022; among non-francophones it stays between 1.3 and 2.5 until 2018 and
reaches 2.9 in 2022, with a wide interval. Details on the realignment
page.](articles/realignment_files/figure-html/enp-dark.png)](https://thomasgareau.github.io/qesR/articles/realignment.md)

The francophone vote fragmented, from 2.8 to 3.9 effective parties; the
non-francophone vote stayed far more concentrated

[![Line chart of the share of francophones who would vote Yes to Quebec
becoming an independent country, for five birth cohorts, at the
elections of 2012, 2014, 2018 and 2022. Those born in 1990 or after go
from 53% to 30%, the lowest level; those born 1945-1959 stay between 50%
and 54%. Details on the sovereignty
page.](articles/sovereignty-generations_files/figure-html/cohorts-light.png)![Line
chart of the share of francophones who would vote Yes to Quebec becoming
an independent country, for five birth cohorts, at the elections of
2012, 2014, 2018 and 2022. Those born in 1990 or after go from 53% to
30%, the lowest level; those born 1945-1959 stay between 50% and 54%.
Details on the sovereignty
page.](articles/sovereignty-generations_files/figure-html/cohorts-dark.png)](https://thomasgareau.github.io/qesR/articles/sovereignty-generations.md)

Francophones born in 1990 or after went from 53% to 30% Yes; those born
1945-59 stayed between 50% and 54%

## Examples

Each page takes a common belief about Quebec elections and sets it
against the studies.

- [From two parties to
  four](https://thomasgareau.github.io/qesR/articles/realignment.md):
  the vote fragmented from 1998 to 2022, but mainly among francophones,
  and within each sovereignty camp rather than across them.
- [Are young Quebecers still the most
  pro-independence?](https://thomasgareau.github.io/qesR/articles/sovereignty-generations.md):
  the youngest francophones were the most likely to vote Yes in 2007 and
  the least likely in 2022, mostly through change within cohorts.
- [Two dimensions: sovereignty and
  left-right](https://thomasgareau.github.io/qesR/articles/dimensions.md):
  the national question structures the vote less than in 2012, and the
  left-right scale has not taken its place.
- [Who votes?](https://thomasgareau.github.io/qesR/articles/turnout.md):
  the young report voting less at every election since 1998, and
  interest in politics accounts for only a small part of the gap.
- [Are Quebec elections decided during the
  campaign?](https://thomasgareau.github.io/qesR/articles/transitions.md):
  the same respondents before and after the vote show that a campaign
  moves many voters but, on net, few votes.
- [Do surveys miss the
  Liberals?](https://thomasgareau.github.io/qesR/articles/survey-vs-official.md):
  the Liberal vote was under-reported from 2007 to 2014, but the gap
  faded in 2018 and 2022, and the winner falls short more often than
  not.
- [Recipes](https://thomasgareau.github.io/qesR/articles/recipes.md):
  the code, step by step, to ask whether the language divide in the
  Liberal vote narrowed as the sovereignty question receded.

## Installation

``` r

# install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

Once qesR is accepted on CRAN, `install.packages("qesR")` will do the
same. A first session:

``` r

library(qesR)
qes_studies()                         # the studies, offline
qes2018 <- get_qes("qes2018")         # one study, from its original file
qes_search("souverain|sovereign")     # a question, in every study
h <- qes_harmonize(targets = "vote_choice")  # the vote, in six studies
d <- qes_decon()                      # every study in one flat file, relaxed
```

## Citing qesR

Work that uses qesR should cite the package and each study it draws on.
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
writes both, with each dataset’s DOI:

``` r

qes_cite("qes2018")                   # qesR and the 2018 study
qes_cite("qes2018", style = "bibtex")
```

[Citing qesR and the
studies](https://thomasgareau.github.io/qesR/articles/citations.md)
gives the citation of every study.

## Licence

The code of qesR is under the MIT licence. The data are not part of the
package: qesR downloads each study from its deposit on Borealis or the
Harvard Dataverse. Most studies are released under CC0; the 2022 study,
and the metadata qesR ships from it, are under CC BY-NC 4.0
(attribution, no commercial use). [Details and the required
attribution](https://thomasgareau.github.io/qesR/articles/citations.html#licences-and-attribution).

## Also on this site

The [study
catalog](https://thomasgareau.github.io/qesR/articles/studies.md), [how
harmonization
works](https://thomasgareau.github.io/qesR/articles/harmonization.md),
[one file for every study with
`qes_decon()`](https://thomasgareau.github.io/qesR/articles/decon.md),
the [coverage of each
study](https://thomasgareau.github.io/qesR/articles/coverage.md), the
[variable
reference](https://thomasgareau.github.io/qesR/articles/harmonization-reference.md),
the [validation against official
results](https://thomasgareau.github.io/qesR/articles/validation.md),
[the merged
file](https://thomasgareau.github.io/qesR/articles/merged-dataset.md)
and the [function
reference](https://thomasgareau.github.io/qesR/reference/index.md). For
code written with an earlier version of qesR:
[upgrading](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md).
