# Where a study's data came from

`qes_provenance()` returns the record of which file a result was read
from: the study, its DOI and dataset version, the Dataverse file id and
name, the md5 checksum expected and observed, the file's UNF and size,
whether the file is the one qesR pins, where and when it was retrieved,
its licence, where its labels came from, the reader and the versions of
haven and of the qesR catalog. Printing it gives one paragraph per file,
ready for a replication log or a methods section.

## Usage

``` r
qes_provenance(x, level = c("study", "cell", "spec"))
```

## Arguments

- x:

  A data frame returned by
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  (or
  [`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md)),
  the result of
  [`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md),
  or a character vector of study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)).
  For study codes, the record shows what
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  would read: nothing has been retrieved or checked yet, so
  `md5_observed`, `md5_verified`, `retrieved_via`, `retrieved_at` and
  `label_source` are `NA`.

- level:

  `"study"` (default): one row per file. `"cell"` and `"spec"` describe
  data harmonized by
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md);
  for other objects they are an error of class
  `qesR_error_no_provenance`.

## Value

A data frame of class `qes_provenance`. For `level = "study"`, one row
per file, with the columns `study`, `doi`, `dataset_version`, `file_id`,
`file_name`, `format`, `md5_expected`, `md5_observed`, `md5_verified`,
`unf`, `n_rows`, `n_cols`, `pinned`, `retrieved_via` (`"network"`,
`"session_cache"`, `"disk_cache"`, `"local_demo"` for the shipped demo
study, or `"user_data"` for a file
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
found already in place), `retrieved_at` (UTC), `licence`,
`label_source`, `label_file_id`, `name_map_applied`, `reader`,
`haven_version`, `catalog_version` and `dict_version`. Columns that do
not apply (the reader of a document, say) are `NA`. For data read from
frames given to
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
in `data`, `md5_verified` is `FALSE` and `retrieved_via` is
`"user_data"`.

For `level = "cell"` (harmonized data), one row per study and target:
`study`, `wave`, `target`, `source_var`, `rule`, `map_id`, `grade`,
`status` (of the crosswalk row: `stable`, or `review` and `draft` for
rows not yet signed off), `instrument`, `weight_var` (the wave's
recommended weight), `weight_status` (`reviewed`, or `needs_review` when
the weight is not reviewed yet and is `NA` in the data),
`weight_mean_raw` (the mean of the raw weight over the wave's members),
`levels_not_offered` (structural zeros, `;`-separated; empty when every
level was offered, `NA` for targets without levels), `included` (`FALSE`
when no row was applied), `excluded` (why not: `no_row`, the study has
no question for the target; `not_reviewed`, its row is not yet signed
off and `include_draft = FALSE`; `not_in_data`, the variable is not in
the demonstration data; `below_grade`, its grade is below `min_grade`;
`NA` when included), `n_valid`, one count `n_<reason>` per NA reason
(they sum with `n_valid` to the study's rows), `n_outside_universe` (for
targets about an election: the wave's members who could not vote in it,
`eligible_voter` `FALSE`; `NA` for other targets, and `NA` when
eligibility is not available for any member of the wave, for example
when its age rows are not signed off and `include_draft = FALSE`) and
`note`.

For `level = "spec"`, one row (one per
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
call for results combined with
[`rbind()`](https://rdrr.io/r/base/cbind.html)): `spec_version`,
`spec_hash`, `spec_custom`, `qesR_version`, `qesR_sha` (the commit of a
GitHub installation, else `NA`), `args` (the arguments of the call, with
a fingerprint of any data given in `data`) and `created` (UTC).

## Details

Data returned by
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
(and files saved by
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md))
carry this record in their attribute `qes_provenance`. R drops such
attributes in some operations, notably
[`merge()`](https://rdrr.io/r/base/merge.html); `qes_provenance()` then
raises an error of class `qesR_error_no_provenance`. Record the
provenance before merging, or pass the study codes.

## En français

`qes_provenance()` indique de quel fichier provient un résultat :
l'étude, son DOI et la version du jeu de données, l'identifiant et le
nom du fichier Dataverse, la somme md5 attendue et observée, l'UNF et la
taille du fichier, s'il s'agit du fichier retenu par qesR, où et quand
il a été obtenu, sa licence, la source des étiquettes, le lecteur et les
versions de haven et du catalogue de qesR. L'affichage donne un
paragraphe par fichier, dans la langue des messages. Avec des codes
d'étude, la fonction montre ce que
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
lirait, sans rien télécharger. Un objet qui a perdu ses attributs (par
exemple après [`merge()`](https://rdrr.io/r/base/merge.html)) produit
une erreur de classe `qesR_error_no_provenance`.

## See also

[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
to cite the studies,
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
for the pinned versions.

Other reproducibility:
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)

## Examples

``` r
demo <- get_qes("qes_demo", assign_global = FALSE, quiet = TRUE)
qes_provenance(demo)
#> qes_demo: file 0 (qes_demo.sav), synthetic data shipped with qesR. md5
#> e956e315800690cb0894c86ed85c8bea, verified. 60 rows, 11 columns. Retrieved on
#> 2026-09-29 16:20:14 UTC (local_demo). Read with haven::read_sav(user_na =
#> TRUE), haven 2.5.5. Licence: CC0 1.0. qesR catalog 2.3.0.
#> 
#> as.data.frame() gives every column.

prov <- as.data.frame(qes_provenance(demo))
prov[, c("study", "file_id", "md5_verified", "retrieved_via")]
#>      study file_id md5_verified retrieved_via
#> 1 qes_demo       0         TRUE    local_demo

# what get_qes() would read, before anything is downloaded
qes_provenance(c("qes2018", "qes2014"))
#> qes2018: file 425914 (Quebec Election Study 2018.dta) of the Dataverse
#> dataset https://doi.org/10.5683/SP3/NWTGWS, version 1.0. Expected md5
#> d24f5b0be727d688ad305b8eb61f0d30 (not yet checked). 3072 rows, 254 columns.
#> Licence: CC0 1.0. qesR catalog 2.3.0.
#> 
#> qes2014: file 425916 (Quebec Election Study 2014.sav) of the Dataverse
#> dataset https://doi.org/10.5683/SP3/64F7WR, version 1.0. Expected md5
#> 9549423b32526a2f11a3d954a87c6861 (not yet checked). 1517 rows, 140 columns.
#> Licence: CC0 1.0. qesR catalog 2.3.0.
#> 
#> as.data.frame() gives every column.

# merge() drops the record: keep it first
prov <- qes_provenance(demo)
merged <- merge(demo, data.frame(extra_column = 1))
try(qes_provenance(merged))
#> Error : `x` does not record which study it comes from (it may have lost its attributes, for example through merge()). Pass study codes instead.
```
