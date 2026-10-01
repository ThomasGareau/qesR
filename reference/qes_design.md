# Harmonized data as a survey design (experimental)

`qes_design()` turns the result of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
into a survey design object of the survey package (or a `tbl_svy` of
srvyr), with the weight column that fits the targets, one stratum per
study, and, in the long layout, the respondent as the sampling unit.

## Usage

``` r
qes_design(
  x,
  weight = NULL,
  engine = c("survey", "srvyr"),
  pool = c("as_is", "equal")
)
```

## Arguments

- x:

  Harmonized data returned by
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  with its weight columns and attributes.

- weight:

  The weight column: `NULL` (default, chosen from the waves of the
  targets, see *Choosing a weight*), `"weight_pre"` or `"weight_post"`
  (respondent layout) or `"weight"` (long layout).

- engine:

  `"survey"` (default) returns a `survey.design2` object of the survey
  package; `"srvyr"` a `tbl_svy` of the srvyr package. The package must
  be installed.

- pool:

  `"as_is"` (default) keeps the weights of `x`; `"equal"` rescales them
  so that every study (every study and wave in the long layout; the
  polls of pooled polls count as one) has the same total.

## Value

A `survey.design2` (engine `"survey"`) or `tbl_svy` (engine `"srvyr"`)
object whose data are the rows of `x` with a value of the weight, as a
plain data frame, with the added column `qes_stratum` (the study, or
`<study>:<stratum>` where the `stratum` column is set), and
`weight_auto` when each study takes its own weight column (see *Choosing
a weight*). The weight is the column named by `weight`, or
`weight_auto`.

## Choosing a weight

Each wave of a study has at most one recommended weight (see
`qes_spec("spec")$tables$weights`).
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
returns it as `weight_pre` (the pre-election wave) and `weight_post`
(the post-election wave), or as `weight` in the long layout, normalized
to mean 1 in each study and wave by default. A pre-election question
such as vote intention is weighted with `weight_pre`, a post-election
one such as reported vote with `weight_post`.

With `weight = NULL`, `qes_design()` looks at the wave each target of
`x` was taken from, as `attr(x, "qes_weight_guide")` records it (targets
that do not depend on the moment of the interview, such as the year of
birth, do not count; a question that can be asked in any wave, such as
sovereignty, counts through the wave that asked it): if all these waves
call for the same column, it uses that column. If they call for both,
but each study's call for one only (a pooled variable such as
`vote_choice` takes the post-election recall of most studies and the
pre-election intention of the CROP polls), each study gets its own, in a
new column `weight_auto` that the design uses. If one study's targets
call for both (the default `targets = "core"` does, for the studies with
two waves), it stops and asks you to choose. With no such target, it
uses the one weight column that has values, and asks you to choose when
both have values. In the long layout the weight is the `weight` column.

Rows without a value of the weight (respondents outside the waves that
have it, or waves whose weight needs review) are left out of the design,
with a message that counts them by study.

## Pooling studies

Normalized weights have mean 1 in each study and wave, so a pooled
estimate gives each study a share proportional to its number of
respondents. `pool = "equal"` rescales the weights so that each study
(each study and wave in the long layout; the polls of pooled polls count
as one) has the same total; it is the only place where qesR changes the
relative size of studies. Whether either is meaningful depends on the
question: studies differ in population, mode, wording and sampling, and
the grades of
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
say how comparable each study's question is.

## Design

Studies are independent samples, so each study is a stratum. A study
made of several independent samples is divided further, by its `stratum`
column: the 1998 panel by polling firm (`stratum` 1 = CREATEC, 2 =
CROP), the pooled CROP polls of 2007-2010 by poll (`stratum` is the
poll's wave name, as in `waves`). In the respondent layout each row is
its own sampling unit (`ids = ~1`). In the long layout a respondent
interviewed in two waves appears in two rows of one study, so the
respondent (`qes_id`) is the sampling unit and the variance accounts for
the two answers of a panel respondent being related. The studies are
opt-in online panels or telephone samples whose weights adjust to census
margins; standard errors computed from these designs treat the weighted
sample as a probability sample and are only as good as that assumption.

## En français

`qes_design()` (expérimental) transforme le résultat de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
en plan de sondage du package survey (ou en `tbl_svy` de srvyr). Chaque
étude est une strate ; en disposition longue, la personne (`qes_id`) est
l'unité d'échantillonnage. Une étude formée de plusieurs échantillons
indépendants est divisée selon sa colonne `stratum` : le panel de 1998
par firme de sondage (1 = CREATEC, 2 = CROP), les sondages CROP de
2007-2010 par sondage (`stratum` est le nom de la vague du sondage,
comme dans `waves`). Avec `weight = NULL`, la pondération est choisie
d'après la vague d'où vient chaque cible (`attr(x, "qes_weight_guide")`)
: `weight_pre` pour des cibles de vagues préélectorales, `weight_post`
pour des cibles de vagues postélectorales ; les cibles fixes, comme
l'année de naissance, ne comptent pas ; si les études appellent des
colonnes différentes mais chacune une seule (une variable regroupée
comme `vote_choice`), chaque étude reçoit la sienne dans une nouvelle
colonne `weight_auto` ; si une même étude mêle les deux, la fonction
demande de choisir. Chaque vague a au plus une pondération recommandée.
Les lignes sans valeur de pondération sont laissées hors du plan, avec
un message. `pool = "equal"` donne le même total à chaque étude (et
vague en disposition longue ; les sondages regroupés comptent pour une
seule étude).

## See also

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
for the weight columns and `attr(, "qes_weight_guide")`, which says
which weight fits each target.

Other harmonization:
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md),
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
[`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md),
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)

## Examples

``` r
h <- qes_harmonize("qes_demo", targets = c("sov_indep", "vote_prov_recall"), quiet = TRUE)
# every target of the demonstration study was asked after the election
attr(h, "qes_weight_guide")
#>             target    study wave target_timing weight_column weight_var
#> 1 vote_prov_recall qes_demo post          post   weight_post       POND
#> 2        sov_indep qes_demo post           any   weight_post       POND
#>   weight_status
#> 1      reviewed
#> 2      reviewed
if (requireNamespace("survey", quietly = TRUE)) {
  d <- qes_design(h, weight = "weight_post")
  survey::svymean(~sov_indep, d, na.rm = TRUE)
}
#>                                          mean     SE
#> sov_indepYes                          0.41584 0.0718
#> sov_indepNo                           0.58416 0.0718
#> sov_indepWould not vote / would spoil 0.00000 0.0000
```
