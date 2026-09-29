# List Quebec Election Study Survey Codes

Returns a data frame of qesR survey call codes, with optional detailed
metadata.

## Usage

``` r
get_qescodes(detailed = FALSE)
```

## Arguments

- detailed:

  If TRUE, include year, names, DOI, and documentation columns.

## Value

A data frame of qesR survey codes (cesR-style by default).

## Details

Soft-deprecated: use
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md),
which returns the full catalog. `get_qescodes()` keeps working and will
not be removed; it prints a one-time notice (see
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)).
Its first 11 rows are the qesR 0.4.4 codes, in the same order and with
the same `index`; codes added since (the 1998 CROP and CREATEC surveys)
follow. Names now carry their accents and the Durand panels, CROP and
1998 surveys are listed under their own titles; `documentation` is the
DOI link.

## See also

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md),
and
[qesR-deprecated](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
for the legacy functions and their replacements.

Other legacy:
[`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md),
[`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md),
[`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md),
[`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md),
[`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md),
[`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md),
[`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md),
[`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)

## Examples

``` r
get_qescodes()
#> `get_qescodes()` is soft-deprecated; use `qes_studies()`. It keeps working and will not be removed.
#>    index    qes_survey_code    get_qes_call_char
#> 1      1            qes2022            "qes2022"
#> 2      2            qes2018            "qes2018"
#> 3      3      qes2018_panel      "qes2018_panel"
#> 4      4            qes2014            "qes2014"
#> 5      5            qes2012            "qes2012"
#> 6      6      qes2012_panel      "qes2012_panel"
#> 7      7 qes_crop_2007_2010 "qes_crop_2007_2010"
#> 8      8            qes2008            "qes2008"
#> 9      9            qes2007            "qes2007"
#> 10    10      qes2007_panel      "qes2007_panel"
#> 11    11            qes1998            "qes1998"
#> 12    12       qes1998_crop       "qes1998_crop"
#> 13    13    qes1998_createc    "qes1998_createc"
get_qescodes(detailed = TRUE)
#>    index    qes_survey_code    get_qes_call_char      year
#> 1      1            qes2022            "qes2022"      2022
#> 2      2            qes2018            "qes2018"      2018
#> 3      3      qes2018_panel      "qes2018_panel"      2018
#> 4      4            qes2014            "qes2014"      2014
#> 5      5            qes2012            "qes2012"      2012
#> 6      6      qes2012_panel      "qes2012_panel"      2012
#> 7      7 qes_crop_2007_2010 "qes_crop_2007_2010" 2007-2010
#> 8      8            qes2008            "qes2008"      2008
#> 9      9            qes2007            "qes2007"      2007
#> 10    10      qes2007_panel      "qes2007_panel"      2007
#> 11    11            qes1998            "qes1998"      1998
#> 12    12       qes1998_crop       "qes1998_crop"      1998
#> 13    13    qes1998_createc    "qes1998_createc"      1998
#>                                                       name_en
#> 1                                  Quebec Election Study 2022
#> 2                                  Quebec Election Study 2018
#> 3                    Panel Survey on the 2018 Quebec Election
#> 4                                  Quebec Election Study 2014
#> 5                                  Quebec Election Study 2012
#> 6                    Panel Survey on the 2012 Quebec Election
#> 7  CROP Polls on Quebec Provincial Vote Intentions, 2007-2010
#> 8                                  Quebec Election Study 2008
#> 9                                  Quebec Election Study 2007
#> 10                   Panel Survey on the 2007 Quebec Election
#> 11     1998 Quebec General Election Polls: CROP-CREATEC Panel
#> 12                   1998 Quebec General Election Polls: CROP
#> 13                1998 Quebec General Election Polls: CREATEC
#>                                                                                     name_fr
#> 1                                                          Étude électorale québécoise 2022
#> 2                                                          Étude électorale québécoise 2018
#> 3                                           Sondage panel sur l'élection québécoise de 2018
#> 4                                                          Étude électorale québécoise 2014
#> 5                                                          Étude électorale québécoise 2012
#> 6                                           Sondage panel sur l'élection québécoise de 2012
#> 7               Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010
#> 8                                                          Étude électorale québécoise 2008
#> 9                                                          Étude électorale québécoise 2007
#> 10                                          Sondage panel sur l'élection québécoise de 2007
#> 11 Sondages électoraux sur les élections générales québécoises de 1998 : panel CROP-CREATEC
#> 12               Sondages électoraux sur les élections générales québécoises de 1998 : CROP
#> 13            Sondages électoraux sur les élections générales québécoises de 1998 : CREATEC
#>                   doi                            doi_url
#> 1  10.7910/DVN/PAQBDR https://doi.org/10.7910/DVN/PAQBDR
#> 2  10.5683/SP3/NWTGWS https://doi.org/10.5683/SP3/NWTGWS
#> 3  10.5683/SP3/XDDMMR https://doi.org/10.5683/SP3/XDDMMR
#> 4  10.5683/SP3/64F7WR https://doi.org/10.5683/SP3/64F7WR
#> 5  10.5683/SP2/WXUPXT https://doi.org/10.5683/SP2/WXUPXT
#> 6  10.5683/SP3/RKHPVL https://doi.org/10.5683/SP3/RKHPVL
#> 7  10.5683/SP3/IRZ1PF https://doi.org/10.5683/SP3/IRZ1PF
#> 8  10.5683/SP2/8KEYU3 https://doi.org/10.5683/SP2/8KEYU3
#> 9  10.5683/SP2/6XGOKA https://doi.org/10.5683/SP2/6XGOKA
#> 10 10.5683/SP3/NDS6VT https://doi.org/10.5683/SP3/NDS6VT
#> 11 10.5683/SP2/QFUAWG https://doi.org/10.5683/SP2/QFUAWG
#> 12 10.5683/SP2/QFUAWG https://doi.org/10.5683/SP2/QFUAWG
#> 13 10.5683/SP2/QFUAWG https://doi.org/10.5683/SP2/QFUAWG
#>                         documentation
#> 1  https://doi.org/10.7910/DVN/PAQBDR
#> 2  https://doi.org/10.5683/SP3/NWTGWS
#> 3  https://doi.org/10.5683/SP3/XDDMMR
#> 4  https://doi.org/10.5683/SP3/64F7WR
#> 5  https://doi.org/10.5683/SP2/WXUPXT
#> 6  https://doi.org/10.5683/SP3/RKHPVL
#> 7  https://doi.org/10.5683/SP3/IRZ1PF
#> 8  https://doi.org/10.5683/SP2/8KEYU3
#> 9  https://doi.org/10.5683/SP2/6XGOKA
#> 10 https://doi.org/10.5683/SP3/NDS6VT
#> 11 https://doi.org/10.5683/SP2/QFUAWG
#> 12 https://doi.org/10.5683/SP2/QFUAWG
#> 13 https://doi.org/10.5683/SP2/QFUAWG
```
