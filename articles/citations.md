# Citing qesR and the studies

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-citations.md)*

When you publish results computed with qesR, cite both the package and
each dataset you used.
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
writes these citations from the catalog that ships with qesR, so this
page needs no network access. The last section gives the licence of each
study and the attribution the 2022 study requires.

``` r

library(qesR)
```

## Citing qesR

``` r

qes_cite()
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.9.1, https://github.com/ThomasGareau/qesR"
```

`citation("qesR")` gives the same reference.

## Citing the datasets

Each study code reads one version of one Dataverse deposit. The citation
gives the authors, the year, the deposit title as published, the DOI,
the repository, the version and the dataset’s UNF (a checksum of its
data). The three 1998 surveys share one deposit, so their citations also
name the data file.

| Code | Year | Study | Licence | Citation |
|:---|:---|:---|:---|:---|
| `qes2022` | 2022 | Quebec Election Study 2022 | CC BY-NC 4.0 | Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, “2022 Quebec Election Study”, <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== \[licence: CC BY-NC 4.0, <https://creativecommons.org/licenses/by-nc/4.0/>\] |
| `qes2018` | 2018 | Quebec Election Study 2018 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, “Étude électorale québécoise 2018”, <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg== |
| `qes2018_panel` | 2018 | Panel Survey on the 2018 Quebec Election | CC0 1.0 | Durand, Claire; Blais, André, 2023, “Sondage panel sur l’élection québécoise de 2018”, <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, V1, UNF:6:ECsYwSg8SlYcGPle+8FjWw== |
| `qes2014` | 2014 | Quebec Election Study 2014 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard, 2023, “Étude électorale québécoise 2014”, <https://doi.org/10.5683/SP3/64F7WR>, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw== |
| `qes2012` | 2012 | Quebec Election Study 2012 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, “Étude électorale québécoise 2012”, <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ== |
| `qes2012_panel` | 2012 | Panel Survey on the 2012 Quebec Election | CC0 1.0 | Durand, Claire; Goyder, John, 2023, “Sondage panel sur l’élection québécoise de 2012”, <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, V1, UNF:6:/ACwE8qVPCB013O9cweCqQ== |
| `qes_crop_2007_2010` | 2007-2010 | CROP Polls on Quebec Provincial Vote Intentions, 2007-2010 | CC0 1.0 | Durand, Claire, 2023, “Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010”, <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, V1, UNF:6:Yaloq+G6EVBknLlAk44JoQ== |
| `qes2008` | 2008 | Quebec Election Study 2008 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard, 2023, “Étude électorale québécoise 2008”, <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg== |
| `qes2007` | 2007 | Quebec Election Study 2007 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, “Étude électorale québécoise 2007”, <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg== |
| `qes2007_panel` | 2007 | Panel Survey on the 2007 Quebec Election | CC0 1.0 | Durand, Claire; Goyder, John, 2023, “Sondage panel sur l’élection québécoise de 2007”, <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, V1, UNF:6:ASjoqrxxkLm0vvSA6lc9Fw== |
| `qes1998` | 1998 | 1998 Quebec General Election Polls: CROP-CREATEC Panel | CC0 1.0 | Durand, Claire, 2023, “Sondages électoraux sur les élections générales québécoises de 1998”, <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== \[file: Total_panel_election_QC1998.sav\] |
| `qes1998_crop` | 1998 | 1998 Quebec General Election Polls: CROP | CC0 1.0 | Durand, Claire, 2023, “Sondages électoraux sur les élections générales québécoises de 1998”, <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== \[file: Total_sondages_election_CROP1998.sav\] |
| `qes1998_createc` | 1998 | 1998 Quebec General Election Polls: CREATEC | CC0 1.0 | Durand, Claire, 2023, “Sondages électoraux sur les élections générales québécoises de 1998”, <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== \[file: Total_sondages_election_CREATEC1998.sav\] |

## Citing only what you used

Pass study codes, or the data returned by
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
to cite exactly the datasets in an analysis:

``` r

qes_cite(c("qes2018", "qes2022"))
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.9.1, https://github.com/ThomasGareau/qesR"                                                                                                                                        
#> [2] "Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, \"Étude électorale québécoise 2018\", https://doi.org/10.5683/SP3/NWTGWS, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg=="                                                                            
#> [3] "Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, \"2022 Quebec Election Study\", https://doi.org/10.7910/DVN/PAQBDR, Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [licence: CC BY-NC 4.0, https://creativecommons.org/licenses/by-nc/4.0/]"
```

``` r

qes2018 <- get_qes("qes2018")
qes_cite(qes2018)
```

For a reference manager, ask for BibTeX:

``` r

cat(qes_cite("qes2018", style = "bibtex"), sep = "\n\n")
#> @Manual{qesR,
#>   title = {{qesR}: Access Quebec Election Study Datasets},
#>   author = {Thomas Gareau-Paquette},
#>   year = {2026},
#>   note = {R package version 0.9.1},
#>   url = {https://github.com/ThomasGareau/qesR},
#> }
#> 
#> @Misc{qes2018,
#>   title = {Étude électorale québécoise 2018},
#>   author = {Éric Bélanger and Richard Nadeau and Valérie-Anne Mahéo and Jean-François Daoust},
#>   year = {2023},
#>   publisher = {Borealis},
#>   version = {V1},
#>   doi = {10.5683/SP3/NWTGWS},
#>   url = {https://doi.org/10.5683/SP3/NWTGWS},
#>   note = {UNF:6:luhys2QSLNTONPOXO4LYpg==},
#> }
```

## Licences and attribution

The data are not part of qesR: it downloads them from Borealis and the
Harvard Dataverse. Most studies are released under CC0 1.0 (public
domain). The 2022 study is released under CC BY-NC 4.0, which requires
attribution and rules out commercial use. `qes_studies()$licence` gives
the licence of each study.

The MIT licence of qesR covers the package code only. The metadata of
the 2022 study that qesR ships (labels, question text, answer counts,
and the harmonization wording, labels and counts derived from them) are
derived from Mahéo, Bélanger, Stephenson and Harell (2023), *2022 Quebec
Election Study*, Harvard Dataverse, V1.1,
<https://doi.org/10.7910/DVN/PAQBDR>, and keep its licence, [CC BY-NC
4.0](https://creativecommons.org/licenses/by-nc/4.0/): attribution, no
commercial use. This does not imply that the authors endorse qesR. Cite
the study when you use this metadata, as when you use its data. A
printed codebook of `qes2022` repeats this notice, and codebooks,
[`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
and
[`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
results that include `qes2022` keep it in their attribute
`licence_notice`.

The metadata of the other studies are CC0 1.0, and the census counts
come from Statistics Canada (Statistics Canada Open Licence).
`system.file("COPYRIGHTS", package = "qesR")` lists each file of the
package, its source and its licence.
