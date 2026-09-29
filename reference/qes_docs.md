# List the documents of each study

`qes_docs()` lists the codebooks, questionnaires and technical or
methodological reports deposited with each study, from the catalog
shipped with qesR. It makes no network request. Each row gives the
Dataverse file id, the deposited file name, a curated role and language,
the size and md5 checksum, and a download URL.

## Usage

``` r
qes_docs(studies = NULL, role = NULL, lang = NULL)
```

## Arguments

- studies:

  Optional character vector of study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md));
  `NULL` lists every study, `"all"` too. Codes are trimmed and
  case-insensitive. The synthetic `"qes_demo"` is accepted and has no
  documents (zero rows).

- role:

  Optional character vector of roles to keep: `"codebook"`,
  `"questionnaire"`, `"technical_report"` or `"methodology"`.

- lang:

  Optional character vector of document languages to keep (`"en"`,
  `"fr"`). `NULL` keeps documents in every language.

## Value

A data frame with one row per document and the columns `study`,
`file_id`, `file_name`, `role`, `lang`, `format` (`pdf`, `doc` or
`docx`), `bytes`, `md5` and `url`. Documents are listed in catalog
order.

## See also

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
for the studies.

Other studies and documents:
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md),
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)

## Examples

``` r
qes_docs("qes2018")
#>     study file_id
#> 1 qes2018  361049
#> 2 qes2018  361050
#> 3 qes2018  367181
#> 4 qes2018  361045
#>                                                          file_name
#> 1                                Quebec Election Study 2018 EN.doc
#> 2                                Quebec Election Study 2018 FR.doc
#> 3 Quebec Election Study 2018 FR with programmed answer values.docx
#> 4     Rapport méthodologique de l'Étude électorale québécoise 2018
#>            role lang format  bytes                              md5
#> 1 questionnaire   en    doc 232960 e263d7864f9a3ae434295bba954fe931
#> 2 questionnaire   fr    doc 242176 5b1dd8036dca45bf5860f288da7de622
#> 3      codebook   fr   docx  73638 c8f58722d652368ff3488f3c638a35c6
#> 4   methodology   fr    pdf 651382 3e17e3a5266e1f79c4cca1549b56c63c
#>                                                  url
#> 1 https://borealisdata.ca/api/access/datafile/361049
#> 2 https://borealisdata.ca/api/access/datafile/361050
#> 3 https://borealisdata.ca/api/access/datafile/367181
#> 4 https://borealisdata.ca/api/access/datafile/361045

# French questionnaires of the Quebec Election Studies
qes_docs(qes_studies(family = "qes")$study, role = "questionnaire", lang = "fr")
#>     study file_id                         file_name          role lang format
#> 1 qes2018  361050 Quebec Election Study 2018 FR.doc questionnaire   fr    doc
#> 2 qes2014  352009 Quebec Election Study 2014 FR.doc questionnaire   fr    doc
#> 3 qes2012  196370 Quebec Election Study 2012 FR.doc questionnaire   fr    doc
#> 4 qes2008  196358 Quebec Election Study 2008 FR.doc questionnaire   fr    doc
#> 5 qes2007  192422 Quebec Election Study 2007 FR.doc questionnaire   fr    doc
#>    bytes                              md5
#> 1 242176 5b1dd8036dca45bf5860f288da7de622
#> 2 149504 084106a7bf6fa379b3ca0c9247200772
#> 3 265728 e9f4642a4a787d5cf4230105c682a2d4
#> 4  86016 451182a24109811f46eeba2bb34be272
#> 5 121344 a6ecbace0da2314d4c7065b8bdea81f7
#>                                                  url
#> 1 https://borealisdata.ca/api/access/datafile/361050
#> 2 https://borealisdata.ca/api/access/datafile/352009
#> 3 https://borealisdata.ca/api/access/datafile/196370
#> 4 https://borealisdata.ca/api/access/datafile/196358
#> 5 https://borealisdata.ca/api/access/datafile/192422
```
