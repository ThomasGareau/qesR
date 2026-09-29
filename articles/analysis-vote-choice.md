# Example: reported vote

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-analyse-choix-vote.md)*

This page shows the reported provincial vote in the studies of the
legacy merged file,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md).
It is built when the website is built: the data files are downloaded
from their Dataverse deposits through qesR’s cache.

For estimates with the harmonization engine (experimental), with a
comparability grade for each study’s question and the weight of the wave
that asked it, see [Harmonizing across
studies](https://thomasgareau.github.io/qesR/articles/harmonization.md).

Since qesR 0.7.0, `vote_choice` is rendered from the harmonization
engine: it holds the vote reported after the election in every study
that asked it (`vote_choice_timing` is `"post"`), `qes2022` and
`qes1998` included, and is `NA` for respondents who did not vote, did
not know or refused to say. The CROP polls asked only vote intentions,
so they are not shown. Each estimate is computed within one study, with
that study’s own weight (`survey_weight`); it is a point estimate that
ignores the survey’s design. The Quebec Election Studies, the Durand
panels and the 1998 polls are shown apart, because their designs differ.
The 1998 polls sampled francophones only, and the firms over-selected
the undecided and those who refused to say; their weight
(`survey_weight`, as read by 0.4.4) is not reviewed, so their shares are
not comparable with the other studies or with the official results.

``` r

library(qesR)
library(ggplot2)
```

``` r

tr <- function(en, fr) if (identical(params$lang, "fr")) fr else en

master <- get_qes_master(quiet = TRUE)
studies <- qes_studies()
master$family <- studies$family[match(master$qes_code, studies$study)]
master$year <- as.integer(master$qes_year)
```

## Table 1: reported vote by study

Shares are among respondents who reported voting for a party, including
“Other party”. Respondents who did not vote, did not know or refused to
say are `NA` and left out of the denominator.

Two kinds of cell are structural zeros, not estimates, and are not shown
as 0:

- **—** the party did not run in that election, according to the
  official results of Élections Québec kept in the qesR source
  repository (`inst/extdata/validation/official_results.csv`; not in the
  package build). The ADQ merged into the CAQ in 2012, and Option
  nationale ran only in 2012 and 2014.
- **n.l.** the party ran, but the study’s question did not list it as an
  answer, so its voters could only answer “Other party”. The answers
  each question offered come from the harmonization specification
  (`qes_spec("crosswalk", targets = "vote_prov_recall")`): the 2012,
  2014 and 2018 studies omit the Conservative Party (PCQ), and the 2018
  and 2022 studies the Green Party (PVQ).

A 0.0 is a party that was listed but that no respondent of the study
reported.

``` r

parties <- c("PLQ", "PQ", "ADQ", "CAQ", "QS", "PVQ", "ON", "PCQ", "Other party")
# Parties with candidates at each general election: the official results of
# Elections Quebec, in the qesR source repository (not in the package build;
# the package installed from the source tree has them). A party that did
# not run cannot be reported.
official_path <- system.file("extdata", "validation", "official_results.csv",
                             package = "qesR")
if (!nzchar(official_path)) {
  official_path <- file.path("..", "..", "inst", "extdata", "validation", "official_results.csv")
}
official <- read.csv(official_path)
ran <- split(official$party, official$election_id)
# The answers each study's question offered, from the harmonization spec
# (the levels of the reported-vote target); "other" is "Other party"
xw <- qes_spec("crosswalk", targets = "vote_prov_recall")
offered <- lapply(setNames(strsplit(xw$levels_offered, ";"), xw$study),
                  function(x) sub("^other$", "Other party", x))

voters <- master[master$vote_choice %in% parties & !is.na(master$survey_weight), ]
vote <- do.call(rbind, lapply(split(voters, voters$qes_code), function(d) {
  w <- tapply(d$survey_weight, factor(d$vote_choice, levels = parties), sum)
  w[is.na(w)] <- 0
  election <- studies$election_id[match(d$qes_code[1], studies$study)]
  status <- ifelse(!parties %in% c(ran[[election]], "Other party"), "not_run",
                   ifelse(parties %in% offered[[d$qes_code[1]]], "offered", "not_listed"))
  data.frame(study = d$qes_code[1], year = d$year[1], family = d$family[1],
             n = nrow(d), party = parties, pct = round(100 * as.numeric(w) / sum(w), 1),
             status = status)
}))
vote$cell <- ifelse(vote$status == "not_run", "\u2014",
                    ifelse(vote$status == "not_listed", tr("n.l.", "n.p."),
                           formatC(vote$pct, format = "f", digits = 1,
                                   decimal.mark = tr(".", ","))))
vote_wide <- reshape(vote[, c("study", "year", "family", "n", "party", "cell")],
                     idvar = c("study", "year", "family", "n"),
                     timevar = "party", direction = "wide")
names(vote_wide) <- sub("^cell\\.", "", names(vote_wide))
vote_wide <- vote_wide[order(vote_wide$family, vote_wide$year), ]
knitr::kable(
  vote_wide, row.names = FALSE, align = c("l", "r", "l", rep("r", 1 + length(parties))),
  col.names = c(tr(c("Study", "Year", "Family", "N voters"),
                   c("Étude", "Année", "Famille", "N votants")),
                tr(parties, c(parties[-length(parties)], "Autre parti")))
)
```

| Study | Year | Family | N voters | PLQ | PQ | ADQ | CAQ | QS | PVQ | ON | PCQ | Other party |
|:---|---:|:---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| qes2007_panel | 2007 | durand_panel | 1494 | 29.8 | 29.6 | 32.1 | — | 3.7 | 4.7 | — | — | 0.2 |
| qes2012_panel | 2012 | durand_panel | 633 | 26.2 | 37.0 | — | 24.3 | 6.2 | 3.0 | 3.0 | n.l. | 0.2 |
| qes2018_panel | 2018 | durand_panel | 704 | 28.0 | 15.1 | — | 39.7 | 12.4 | n.l. | — | n.l. | 4.7 |
| qes1998 | 1998 | polls_1998 | 1126 | 29.9 | 49.7 | 18.6 | — | — | — | — | — | 1.8 |
| qes2007 | 2007 | qes | 1727 | 25.8 | 30.9 | 31.6 | — | 4.8 | 6.4 | — | — | 0.6 |
| qes2008 | 2008 | qes | 898 | 42.1 | 35.3 | 16.4 | — | 3.8 | 2.2 | — | — | 0.3 |
| qes2012 | 2012 | qes | 1274 | 24.9 | 38.8 | — | 25.4 | 6.5 | 1.0 | 2.3 | n.l. | 1.1 |
| qes2014 | 2014 | qes | 1283 | 35.9 | 29.8 | — | 23.1 | 8.2 | 1.0 | 0.7 | n.l. | 1.3 |
| qes2018 | 2018 | qes | 2016 | 23.3 | 19.6 | — | 35.8 | 16.3 | n.l. | — | n.l. | 5.1 |
| qes2022 | 2022 | qes | 1101 | 18.3 | 15.4 | — | 33.0 | 16.4 | n.l. | — | 13.3 | 3.6 |

## Figure 1: the main parties

``` r

main <- vote[vote$status == "offered" & vote$party %in% c("PLQ", "PQ", "ADQ", "CAQ", "QS"), ]
families <- c(qes = tr("Quebec Election Studies", "Études électorales québécoises"),
              durand_panel = tr("Durand panels", "Panels Durand"),
              polls_1998 = tr("1998 polls", "Sondages de 1998"))
main$family <- factor(families[main$family], levels = families)
ggplot(main, aes(x = year, y = pct, colour = party, shape = party)) +
  # a line only where a party has more than one year in the family (the
  # 1998 polls have one)
  geom_line(data = function(d) {
    d[ave(seq_along(d$year), d$family, d$party,
          FUN = function(i) length(unique(d$year[i]))) > 1, ]
  }) +
  geom_point(size = 2.4) +
  facet_wrap(~family, ncol = 1) +
  scale_colour_manual(values = c(PLQ = "#d71920", PQ = "#004c9d", ADQ = "#6d9eeb",
                                 CAQ = "#00a3c7", QS = "#ff6f00")) +
  scale_x_continuous(breaks = sort(unique(main$year))) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(
    x = tr("Year of the study", "Année de l'étude"),
    y = tr("Share of reported votes, weighted (%)", "Part des votes déclarés, pondérée (%)"),
    colour = tr("Party", "Parti"),
    shape = tr("Party", "Parti")
  ) +
  theme_minimal(base_size = 12)
```

![Weighted reported vote share of the PLQ, PQ, ADQ, CAQ and QS in each
study, by year, with the Quebec Election Studies, the Durand panels and
the 1998 polls in separate
panels.](analysis-vote-choice_files/figure-html/vote-plot-1.png)

## Notes

- A line joins studies of the same family only; the ADQ and the CAQ are
  separate parties, and their lines are not joined.
- qesR 0.4.4 coded “Did not vote / None” and “Don’t know / Refused” as
  categories of `vote_choice`, and used the campaign-period vote
  intention for `qes2022` and `qes1998`; since 0.7.0 these respondents
  are `NA` and both studies give the reported vote
  ([`vignette("migrating-0.7", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md)).
- Reported votes differ from official results: compare them with the
  results published by Élections Québec, not with each other across
  designs.
