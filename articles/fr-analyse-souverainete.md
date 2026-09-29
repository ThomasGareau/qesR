# Exemple : appui à l'indépendance

*[English
version](https://thomasgareau.github.io/qesR/articles/analysis-sovereignty.md)*

Cette page estime l’appui à l’indépendance du Québec dans les études du
fichier fusionné hérité,
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md).
Elle est construite en même temps que le site : les fichiers de données
sont téléchargés de leurs dépôts Dataverse par le cache de qesR.

Pour des estimations avec le moteur d’harmonisation (expérimental), avec
un niveau de comparabilité pour la question de chaque étude et la
pondération de la vague qui l’a posée, voir [Harmoniser entre
études](https://thomasgareau.github.io/qesR/articles/fr-harmonisation.md).

Une question sur la souveraineté n’est comparable d’une étude à l’autre
que si elle demande la même chose. Depuis qesR 0.5.0,
`sovereignty_support` ne contient qu’une question : le vote du répondant
à un référendum sur un Québec pays indépendant. Les études qui posaient
une autre question (la question de 1995 sur la souveraineté assortie
d’une offre de partenariat, un « pays souverain », des questions de
relance posées aux indécis, ou une échelle favorable/opposé) ont `NA`
dans cette colonne. Chaque estimation est calculée à l’intérieur d’une
étude, avec la pondération propre à cette étude (`survey_weight`) ;
c’est une estimation ponctuelle qui ne tient pas compte du plan de
sondage.

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

# Weighted share of each value of x, within one study
wshare <- function(x, w) {
  ok <- !is.na(x) & !is.na(w)
  if (!any(ok)) return(NULL)
  tapply(w[ok], x[ok], sum) / sum(w[ok])
}
```

## Tableau 1 : les études qui posent la question sur l’indépendance

`attr(master, "legacy_na_columns")` indique pourquoi une colonne vaut
`NA` dans une étude ; `attr(master, "source_map")` nomme la variable que
lit la colonne de chaque étude.

``` r

sources <- attr(master, "source_map")
sources <- sources[sources$harmonized_variable == "sovereignty_support",
                   c("qes_code", "source_variable")]
blanked <- attr(master, "legacy_na_columns")
blanked <- blanked[blanked$column == "sovereignty_support", c("study", "cause")]
items <- merge(sources, blanked, by.x = "qes_code", by.y = "study", all.x = TRUE)
items$year <- studies$year[match(items$qes_code, studies$study)]
items$in_column <- ifelse(is.na(items$cause), tr("yes", "oui"), tr("no", "non"))
items <- items[order(items$year, items$qes_code), c("qes_code", "year", "source_variable", "in_column")]
knitr::kable(
  items, row.names = FALSE,
  col.names = tr(c("Study", "Year", "Source variable", "Independence question"),
                 c("Étude", "Année", "Variable source", "Question sur l'indépendance"))
)
```

| Étude              | Année | Variable source   | Question sur l’indépendance |
|:-------------------|------:|:------------------|:----------------------------|
| qes1998            |  1998 | NA                | non                         |
| qes_crop_2007_2010 |  2007 | NA                | non                         |
| qes2007            |  2007 | NA                | non                         |
| qes2007_panel      |  2007 | NA                | non                         |
| qes2008            |  2008 | NA                | non                         |
| qes2012            |  2012 | q52               | oui                         |
| qes2012_panel      |  2012 | NA                | non                         |
| qes2014            |  2014 | Q19               | oui                         |
| qes2018            |  2018 | q26               | oui                         |
| qes2018_panel      |  2018 | NA                | non                         |
| qes2022            |  2022 | cps_qc_referendum | oui                         |

Le libellé dans les quatre études qui la posent :

``` r

asked <- items$qes_code[items$in_column == tr("yes", "oui")]
wording <- do.call(rbind, lapply(asked, function(s) {
  v <- items$source_variable[items$qes_code == s]
  q <- qes_question(s, v, lang = params$lang)
  # a study documented in one language only: show that language
  if (is.na(q$question)) q <- qes_question(s, v, lang = tr("fr", "en"))
  q[, c("study", "variable", "question", "question_lang", "truncated")]
}))
knitr::kable(
  wording, row.names = FALSE,
  col.names = tr(c("Study", "Variable", "Question", "Language", "Truncated"),
                 c("Étude", "Variable", "Question", "Langue", "Tronquée"))
)
```

| Étude | Variable | Question | Langue | Tronquée |
|:---|:---|:---|:---|:---|
| qes2012 | q52 | Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? | fr | FALSE |
| qes2014 | Q19 | Si un référendum sur l’indépendance avait lieu vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? | fr | FALSE |
| qes2018 | q26 | Si un référendum sur l’indépendance avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? | fr | FALSE |
| qes2022 | cps_qc_referendum | Si un référendum sur l’indépendance avait lieu aujourd’hui vous demandant si vous voulez que le Québec devienne un pays indépendant, voteriez-vous OUI ou voteriez-vous NON? | fr | FALSE |

## Tableau 2 : l’appui à l’indépendance

La part qui voterait OUI parmi les répondants qui ont répondu OUI ou
NON. Les « ne sait pas » et les refus sont exclus.

``` r

sov <- do.call(rbind, lapply(split(master, master$qes_code), function(d) {
  s <- wshare(d$sovereignty_support, d$survey_weight)
  if (is.null(s) || !"1" %in% names(s)) return(NULL)
  data.frame(study = d$qes_code[1], year = d$year[1],
             n = sum(!is.na(d$sovereignty_support)), pct = round(100 * s[["1"]], 1))
}))
sov <- sov[order(sov$year), ]
knitr::kable(
  sov, row.names = FALSE, format.args = list(decimal.mark = tr(".", ",")),
  col.names = tr(c("Study", "Year", "N answering", "YES (%)"),
                 c("Étude", "Année", "N répondants", "OUI (%)"))
)
```

| Étude   | Année | N répondants | OUI (%) |
|:--------|------:|-------------:|--------:|
| qes2012 |  2012 |         1323 |    40,4 |
| qes2014 |  2014 |         1353 |    34,8 |
| qes2018 |  2018 |         2558 |    34,6 |
| qes2022 |  2022 |         1284 |    34,3 |

``` r

# points only: the question's wording and timing change between studies
# (see the notes), so the studies do not form one series
ggplot(sov, aes(x = year, y = pct)) +
  geom_point(size = 2.5, colour = "#12355b") +
  scale_x_continuous(breaks = sov$year) +
  scale_y_continuous(limits = c(0, 100)) +
  labs(
    x = tr("Year of the study", "Année de l'étude"),
    y = tr("Would vote YES, weighted (%)", "Voterait OUI, pondéré (%)")
  ) +
  theme_minimal(base_size = 12)
```

![Part pondérée qui voterait OUI à un référendum sur l'indépendance,
dans les Études électorales québécoises qui posaient la
question.](fr-analyse-souverainete_files/figure-html/support-plot-1.png)

## Remarques

- Les quatre études sont des Études électorales québécoises, mais leur
  mode et leur population diffèrent : `qes2018` couvre les personnes de
  16 ans et plus, et `qes2022` est un panel en ligne de citoyens de 18
  ans et plus
  ([`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)).
- La question n’a été posée ni au même moment ni dans les mêmes mots :
  `qes2022` l’a posée pendant la campagne (sa vague `cps_`), les autres
  après l’élection, et les questions de 2018 et de 2022 ajoutent «
  aujourd’hui » (tableau 1). La figure montre un point par étude et ne
  les relie pas en une série.
- Le libellé de `qes2022` est cité, en français et en anglais et sans
  coupure, d’après son livre de codes bilingue (`qes_docs("qes2022")`).
  Ce libellé est distribué sous la licence de l’étude, CC BY-NC 4.0
  (`qes_cite("qes2022")`).
- Pour les autres études, lisez leur propre question avec
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  et
  [`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
  ; la comparer à la question sur l’indépendance demande de la prudence,
  car le libellé diffère.
