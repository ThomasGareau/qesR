# Load a Quebec Election Study

Reads a study's data file, as its authors deposited it, and returns it
with its labels.

## Usage

``` r
get_qes(
  srvy,
  file = NULL,
  assign_global = FALSE,
  with_codebook = TRUE,
  quiet = FALSE
)
```

## Arguments

- srvy:

  A qesR survey code from
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md),
  or `"qes_demo"` for the small synthetic study shipped with the
  package. Codes are trimmed and case-insensitive (`" QES2018 "` is
  `"qes2018"`); an unknown code is an error of class
  `qesR_error_unknown_study` that suggests near matches.

- file:

  Optional regular expression that chooses one of the study's data files
  by name, when its deposit holds several (for example
  `get_qes("qes2014", file = "dta")` reads the Stata version instead of
  the default SPSS file). It is matched, ignoring case, against data
  files only (`get_qes("qes2012", file = "SPSS")` reads the SPSS twin
  whose labels complete the default Stata file's). A pattern that is not
  a valid regular expression is an error of class `qesR_error_input`. A
  pattern that matches no data file of the study but the data file of
  another study of the same deposit reads that study, with a message:
  `get_qes("qes1998", file = "CROP")` reads `qes1998_crop`. A pattern
  that matches no file, or several, is an error of class
  `qesR_error_ambiguous_file`.

- assign_global:

  If TRUE, also assign the data as `<code>` into the environment
  `get_qes()` was called from (the global environment only when called
  at top level), where `<code>` is the canonical study code. With
  `with_codebook = TRUE`, the codebook is assigned as `<code>_codebook`
  too. Defaults to FALSE. The data is returned either way.

- with_codebook:

  If TRUE, attach the study's codebook for the columns read (see
  [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md);
  built offline, with no request) as the `qes_codebook` attribute (and
  assign `<code>_codebook` when `assign_global = TRUE`).

- quiet:

  If TRUE, suppress informational output.

## Value

A base data frame, returned visibly, with attributes `qes_survey_code`
(the canonical study code), `qes_provenance` (a one-row data frame
recording the DOI, dataset version, file id, file name, md5, UNF,
dimensions, where the file came from and when, the licence, the source
of the labels and the reader used; see
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md))
and, with `with_codebook = TRUE`, `qes_codebook`.

## Details

`get_qes()` returns the data. It does not write anything into your
workspace unless you ask for it with `assign_global = TRUE`: write
`qes2018 <- get_qes("qes2018")`. A real study is downloaded once per
session, or kept between sessions with `options(qesR.cache = "disk")`
(see
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md)).
The first top-level call in a session (console, `Rscript` or
[`source()`](https://rdrr.io/r/base/source.html)) prints a one-time note
that the data must be assigned; passing `assign_global` (TRUE or FALSE)
or `quiet = TRUE` avoids it.

## Which file is read

Each study is pinned to one data file of one Dataverse dataset version
(see
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)).
`get_qes()` reads the **original** upload of that file (SPSS `.sav` or
Stata `.dta`), not the tab-delimited copy Dataverse makes of it, after
checking it against the md5 checksum recorded in the package catalog. A
file that fails the check is an error (class `qesR_error_checksum`) and
is never used; so is a file whose number of rows or columns differs from
the catalog (`qesR_error_rowcount`). The file is downloaded once and
kept in the download cache (see
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md));
within a session the parsed data is also kept in memory, so a second
call makes no request (`options(qesR.memo = FALSE)` turns this off).

The data is returned as deposited: column names, codes and missing
values (`NA`) are those of the file, and no row is dropped or recoded.
The one exception: three columns of `qes2007_panel` are named `AFFGÉN`,
`PROPRIÉ` and `PROPGÉN` (`AffGénérale`, `propriété` and `PropGénérale`
in the file), the names Dataverse gives them. `qes2022` dates are
date-times, and labels are those of the file (below).

## Labels and missing values

Labelled columns are
[`haven::labelled()`](https://haven.tidyverse.org/reference/labelled.html)
vectors; convert one with
[`haven::as_factor()`](https://forcats.tidyverse.org/reference/as_factor.html).
Variable and value labels come from the data file itself, never from
Dataverse's metadata or from a variable name. For `qes2012`, whose Stata
file (the one read, with its lowercase names) has labels that Stata
lowercased and cut at 80 characters, the complete labels of the SPSS
twin of the same data are used. A few labels of the CROP files typed in
another character set are corrected (for example "RESTE DU QUÉBEC").
`qes2018`'s data file has value labels for only a few variables; the
codebook (see
[`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md))
gives the others, from the study's questionnaire, without changing the
data.

Codes that an SPSS file declares as user-missing (such as 8 or 9 for
"Don't know") are kept as values; the declaration is kept in the column
attributes `qes_na_values` and `qes_na_range`.
[`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
sets these codes, and the "don't know" and "refused" codes the codebook
types, to `NA`.

The `qes_codebook` attribute is built offline from the metadata shipped
with qesR, for every study (`qes2022`'s under the study's licence, CC
BY-NC 4.0; see `qes_cite("qes2022")`).

## See also

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
for the study codes and their pinned files,
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
for the record of the file read,
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
to save the original files,
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md)
for the download cache.

Other data:
[`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)

## Examples

``` r
# the synthetic demonstration study ships with qesR: no download
demo <- get_qes("qes_demo", quiet = TRUE)
dim(demo)
#> [1] 60 11
attr(demo, "qes_provenance")[, c("study", "file_name", "md5_verified")]
#>      study    file_name md5_verified
#> 1 qes_demo qes_demo.sav         TRUE
```
