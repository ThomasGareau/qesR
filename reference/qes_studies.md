# List the studies qesR can load

`qes_studies()` returns the offline study catalog: one row per study
code, with the deposit title and authors, design, target population,
licence, DOI and the pinned dataset version and data file. It reads only
metadata shipped with the package and makes no network request, unless
`check_updates = TRUE`.

## Usage

``` r
qes_studies(family = NULL, check_updates = FALSE, quiet = FALSE)
```

## Arguments

- family:

  Optional character vector of study families to keep: `"qes"` (Quebec
  Election Studies), `"durand_panel"`, `"crop_polls"` or `"polls_1998"`.

- check_updates:

  If `TRUE`, ask Dataverse whether each deposit has a newer published
  version and whether the pinned data file changed. This makes one
  metadata request per deposit (the three 1998 studies share one), one
  at a time. A study that cannot be checked gets
  `status = "unreachable"`; the call never fails because of the network.

- quiet:

  If `TRUE`, do not print progress messages.

## Value

A data frame with one row per study and the columns of the catalog's
`studies.csv`: `study`, `aliases`, `family`, `title_deposit` (verbatim),
`title_en`, `title_fr`, `authors` (`;`-separated), `year`, `year_end`,
`election_id`, `study_design`, `default_member`, `target_population_en`,
`target_population_fr`, `server`, `doi`, `dataset_version` (pinned),
`data_file_id` (pinned), `label_file_id`, `source_lang`, `licence`,
`licence_url`, `metadata_shipped`, `publisher`, `citation_year`,
`dataset_unf`, `notes_en`, `notes_fr`; then `doi_url` and `waves`, the
waves of the study in the harmonization spec (`;`-separated, in field
order, such as `"cps;pes"`; `NA` for a study the spec does not cover
yet). With `check_updates = TRUE` it adds `latest_version`, `latest_md5`
(of the pinned data file in the latest version) and `status`:
`"current"`, `"new_version_same_file"`, `"data_changed"`,
`"deaccessioned"` or `"unreachable"`.

The text columns do not depend on the session language: `title_en` and
`title_fr` (and `notes_en`, `notes_fr`) are both always there.

The data frame has class `c("qes_studies", "data.frame")`. It prints
compactly: `study`, `year`, `family`, `study_design`, `licence` and the
title in the session language (`status` too after
`check_updates = TRUE`);
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) or
[`names()`](https://rdrr.io/r/base/names.html) gives every column. A
subset of its columns is a plain data frame.

*En français* : le tableau (classe `c("qes_studies", "data.frame")`)
s'affiche de façon compacte : `study`, `year`, `family`, `study_design`,
`licence` et le titre dans la langue de la session (`status` aussi avec
`check_updates = TRUE`) ;
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) ou
[`names()`](https://rdrr.io/r/base/names.html) donne toutes les
colonnes. Un sous-ensemble de ses colonnes est un data frame ordinaire.

## Details

Studies that are not Quebec Election Studies (the three Durand panels,
the CROP polls and the 1998 polls) are listed under their own titles and
authors. The 1998 deposit holds three surveys, each with its own code:
`qes1998` (the combined CROP-CREATEC panel file), `qes1998_crop` and
`qes1998_createc`. All three cover francophones only, each with its own
definition (see `notes_en`).

## See also

[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
for the documents of each study,
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
to cite them.

Other studies and documents:
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md),
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)

## Examples

``` r
studies <- qes_studies()
studies[, c("study", "year", "title_en", "licence")]
#>                 study year
#> 1             qes2022 2022
#> 2             qes2018 2018
#> 3       qes2018_panel 2018
#> 4             qes2014 2014
#> 5             qes2012 2012
#> 6       qes2012_panel 2012
#> 7  qes_crop_2007_2010 2007
#> 8             qes2008 2008
#> 9             qes2007 2007
#> 10      qes2007_panel 2007
#> 11            qes1998 1998
#> 12       qes1998_crop 1998
#> 13    qes1998_createc 1998
#>                                                      title_en      licence
#> 1                                  Quebec Election Study 2022 CC BY-NC 4.0
#> 2                                  Quebec Election Study 2018      CC0 1.0
#> 3                    Panel Survey on the 2018 Quebec Election      CC0 1.0
#> 4                                  Quebec Election Study 2014      CC0 1.0
#> 5                                  Quebec Election Study 2012      CC0 1.0
#> 6                    Panel Survey on the 2012 Quebec Election      CC0 1.0
#> 7  CROP Polls on Quebec Provincial Vote Intentions, 2007-2010      CC0 1.0
#> 8                                  Quebec Election Study 2008      CC0 1.0
#> 9                                  Quebec Election Study 2007      CC0 1.0
#> 10                   Panel Survey on the 2007 Quebec Election      CC0 1.0
#> 11     1998 Quebec General Election Polls: CROP-CREATEC Panel      CC0 1.0
#> 12                   1998 Quebec General Election Polls: CROP      CC0 1.0
#> 13                1998 Quebec General Election Polls: CREATEC      CC0 1.0

# Quebec Election Studies only
qes_studies(family = "qes")$study
#> [1] "qes2022" "qes2018" "qes2014" "qes2012" "qes2008" "qes2007"

# \donttest{
# Ask Dataverse whether the pinned versions are still current (one
# metadata request per deposit). A deposit that cannot be reached is
# reported as "unreachable", never as an error.
if (curl::has_internet()) {
  tryCatch(
    qes_studies(check_updates = TRUE, quiet = TRUE)[, c("study", "status")],
    qesR_error_network = function(e) conditionMessage(e)
  )
}
#>                 study  status
#> 1             qes2022 current
#> 2             qes2018 current
#> 3       qes2018_panel current
#> 4             qes2014 current
#> 5             qes2012 current
#> 6       qes2012_panel current
#> 7  qes_crop_2007_2010 current
#> 8             qes2008 current
#> 9             qes2007 current
#> 10      qes2007_panel current
#> 11            qes1998 current
#> 12       qes1998_crop current
#> 13    qes1998_createc current
# }
```
