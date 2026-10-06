# Join the parties of one lineage (ADQ and CAQ) in harmonized data

`qes_party_lineage()` adds, for each party column of harmonized data, a
column that joins the parties of a lineage: by default the Action
démocratique du Québec (ADQ) and the Coalition avenir Québec (CAQ), into
which the ADQ merged on 2012-01-21, as one level `"ADQ/CAQ"`. It is a
view for time series of the Quebec parties, such as vote choice from
1998 to 2022: the harmonized columns keep the ADQ and the CAQ apart
(they are different parties, and no study offers both), and so do the
targets and pooled variables of the spec.

## Usage

``` r
qes_party_lineage(x, cols = NULL, lineage = "adq_caq")
```

## Arguments

- x:

  Harmonized data returned by
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  or the relaxed data of
  [`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
  (its party columns are `vote_choice`, `vote_prev` and `pid`; relaxed
  columns carry no grade, so no `__grade` column is added).

- cols:

  The party columns to join (targets or pooled variables whose levels
  are Quebec parties). `NULL` (default) means every such column of `x`.

- lineage:

  The lineages to apply: `"adq_caq"` (default, ADQ and CAQ as
  `"ADQ/CAQ"`) and, optionally, `"on_qs"` (Option nationale as Quebec
  solidaire, which it joined in December 2017).

## Value

`x` with, for each column `<col>` of `cols`, the columns `<col>_lineage`
(a factor, or the ASCII code `"ADQ_CAQ"` when `x` was built with
`values = "code"`; `"labelled"` data get codes as text) and, for data of
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
`<col>_lineage__grade`, placed at the end.

## Details

The lineage is a comparison across parties, not across questions, so the
new column's grade (`<col>_lineage__grade`) is `approximate` for every
row fielded before the merger (`year` before 2012: the ADQ era, the CROP
polls of 2007-2010 included); elsewhere it is the grade of the column's
own cell (`<col>__grade` for a pooled variable, else the grade of the
study's cell in `qes_provenance(x, level = "cell")`).

## En français

`qes_party_lineage()` ajoute, pour chaque colonne de partis de données
harmonisées, une colonne qui réunit les partis d'une même filiation :
par défaut l'Action démocratique du Québec (ADQ) et la Coalition avenir
Québec (CAQ), avec laquelle l'ADQ a fusionné le 21 janvier 2012, en un
seul niveau « ADQ/CAQ ». C'est une vue pour les séries chronologiques ;
les colonnes harmonisées gardent l'ADQ et la CAQ distinctes. Le niveau
de comparabilité de la nouvelle colonne est `approximate` pour les
lignes recueillies avant la fusion (`year` avant 2012). Elle s'applique
aussi aux données souples de
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
(`vote_choice`, `vote_prev`, `pid`), sans niveau de comparabilité.

## See also

[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
and its `vote_choice` pooled variable.

Other harmonization:
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md),
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md),
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)

## Examples

``` r
h <- qes_harmonize("qes_demo", targets = "vote_choice", quiet = TRUE)
h <- qes_party_lineage(h)
table(h$vote_choice_lineage, useNA = "ifany")
#> 
#>                                 PLQ                                  PQ 
#>                                  14                                  13 
#>                             ADQ/CAQ                                  QS 
#>                                   8                                   7 
#>                                 PVQ                                 PCQ 
#>                                   1                                   0 
#>                                  ON                         Other party 
#>                                   0                                   1 
#> Would not vote / none / would spoil                                <NA> 
#>                                   0                                  16 
```
