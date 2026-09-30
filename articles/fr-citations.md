# Citations des études

*[English
version](https://thomasgareau.github.io/qesR/articles/citations.md)*

Lorsque vous publiez des résultats obtenus avec qesR, citez le package
et chacun des jeux de données utilisés.
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
produit ces citations à partir du catalogue fourni avec qesR : cette
page n’a donc besoin d’aucun accès au réseau. La version anglaise de
cette page est
[`vignette("citations", package = "qesR")`](https://thomasgareau.github.io/qesR/articles/citations.md).

``` r

library(qesR)
```

## Citer qesR

``` r

qes_cite()
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.8.0, https://github.com/ThomasGareau/qesR"
```

`citation("qesR")` donne la même référence.

## Citer les jeux de données

Chaque code d’étude renvoie à un dépôt Dataverse, fixé à une version
précise. La citation donne les auteurs, l’année, le titre du dépôt tel
que publié, le DOI, le dépôt, la version et l’UNF du jeu de données (une
somme de contrôle de ses données). Les trois sondages de 1998 partagent
un même dépôt : leur citation nomme donc aussi le fichier de données.

| Code | Année | Étude | Licence | Citation |
|:---|:---|:---|:---|:---|
| `qes2022` | 2022 | Étude électorale québécoise 2022 | CC BY-NC 4.0 | Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, “2022 Quebec Election Study”, <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== \[licence : CC BY-NC 4.0, <https://creativecommons.org/licenses/by-nc/4.0/>\] |
| `qes2018` | 2018 | Étude électorale québécoise 2018 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, “Étude électorale québécoise 2018”, <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg== |
| `qes2018_panel` | 2018 | Sondage panel sur l’élection québécoise de 2018 | CC0 1.0 | Durand, Claire; Blais, André, 2023, “Sondage panel sur l’élection québécoise de 2018”, <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, V1, UNF:6:ECsYwSg8SlYcGPle+8FjWw== |
| `qes2014` | 2014 | Étude électorale québécoise 2014 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard, 2023, “Étude électorale québécoise 2014”, <https://doi.org/10.5683/SP3/64F7WR>, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw== |
| `qes2012` | 2012 | Étude électorale québécoise 2012 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, “Étude électorale québécoise 2012”, <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ== |
| `qes2012_panel` | 2012 | Sondage panel sur l’élection québécoise de 2012 | CC0 1.0 | Durand, Claire; Goyder, John, 2023, “Sondage panel sur l’élection québécoise de 2012”, <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, V1, UNF:6:/ACwE8qVPCB013O9cweCqQ== |
| `qes_crop_2007_2010` | 2007-2010 | Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010 | CC0 1.0 | Durand, Claire, 2023, “Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010”, <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, V1, UNF:6:Yaloq+G6EVBknLlAk44JoQ== |
| `qes2008` | 2008 | Étude électorale québécoise 2008 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard, 2023, “Étude électorale québécoise 2008”, <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg== |
| `qes2007` | 2007 | Étude électorale québécoise 2007 | CC0 1.0 | Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, “Étude électorale québécoise 2007”, <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg== |
| `qes2007_panel` | 2007 | Sondage panel sur l’élection québécoise de 2007 | CC0 1.0 | Durand, Claire; Goyder, John, 2023, “Sondage panel sur l’élection québécoise de 2007”, <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, V1, UNF:6:ASjoqrxxkLm0vvSA6lc9Fw== |
| `qes1998` | 1998 | Sondages électoraux sur les élections générales québécoises de 1998 : panel CROP-CREATEC | CC0 1.0 | Durand, Claire, 2023, “Sondages électoraux sur les élections générales québécoises de 1998”, <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== \[fichier : Total_panel_election_QC1998.sav\] |
| `qes1998_crop` | 1998 | Sondages électoraux sur les élections générales québécoises de 1998 : CROP | CC0 1.0 | Durand, Claire, 2023, “Sondages électoraux sur les élections générales québécoises de 1998”, <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== \[fichier : Total_sondages_election_CROP1998.sav\] |
| `qes1998_createc` | 1998 | Sondages électoraux sur les élections générales québécoises de 1998 : CREATEC | CC0 1.0 | Durand, Claire, 2023, “Sondages électoraux sur les élections générales québécoises de 1998”, <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== \[fichier : Total_sondages_election_CREATEC1998.sav\] |

L’étude de 2022 est diffusée sous licence CC BY-NC 4.0, qui exige
l’attribution et exclut l’usage commercial ; les autres études sont sous
CC0 (domaine public). La licence de chaque étude figure dans
`qes_studies()$licence`. Les métadonnées de l’étude de 2022 que qesR
livre (son codebook, le texte des questions, les étiquettes de valeurs
et les effectifs) sont sous la même licence et demandent la même
citation.

## Citer seulement ce que vous avez utilisé

Passez des codes d’étude, ou les données renvoyées par
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md),
pour citer exactement les jeux de données d’une analyse :

``` r

qes_cite(c("qes2018", "qes2022"))
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.8.0, https://github.com/ThomasGareau/qesR"                                                                                                                                        
#> [2] "Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, \"Étude électorale québécoise 2018\", https://doi.org/10.5683/SP3/NWTGWS, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg=="                                                                            
#> [3] "Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, \"2022 Quebec Election Study\", https://doi.org/10.7910/DVN/PAQBDR, Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [licence: CC BY-NC 4.0, https://creativecommons.org/licenses/by-nc/4.0/]"
```

``` r

qes2018 <- get_qes("qes2018")
qes_cite(qes2018)
```

Pour un gestionnaire de références, demandez le format BibTeX :

``` r

cat(qes_cite("qes2018", style = "bibtex"), sep = "\n\n")
#> @Manual{qesR,
#>   title = {{qesR}: Access Quebec Election Study Datasets},
#>   author = {Thomas Gareau-Paquette},
#>   year = {2026},
#>   note = {R package version 0.8.0},
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
