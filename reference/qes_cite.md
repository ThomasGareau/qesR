# Cite qesR and the studies it loads

`qes_cite()` returns the citation of qesR and, for each study you name
or load, the citation of its Dataverse deposit: authors, year, the
verbatim deposit title, the DOI link, the publisher, the pinned dataset
version and the dataset UNF, as Dataverse writes it. Everything comes
from the catalog shipped with qesR; no network request is made.

## Usage

``` r
qes_cite(x = NULL, style = c("text", "bibtex", "bibentry"), lang = "en")
```

## Arguments

- x:

  `NULL` to cite qesR only; a character vector of study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md));
  a data frame returned by
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  or
  [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
  or the result of
  [`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
  or
  [`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md),
  whose studies are read from its provenance record. A data frame that
  has lost those attributes (for example after
  [`merge()`](https://rdrr.io/r/base/merge.html)) is an error of class
  `qesR_error_no_provenance`. The synthetic study `qes_demo` has no
  deposit and adds nothing.

- style:

  `"text"` (default) for plain-text citations, `"bibtex"` for BibTeX
  entries, or `"bibentry"` for a
  [`utils::bibentry()`](https://rdrr.io/r/utils/bibentry.html) object.

- lang:

  Language of the few words qesR adds to the text citations (`"en"` or
  `"fr"`). Titles and author names are always as deposited. It never
  follows the session language.

## Value

For `"text"` and `"bibtex"`, a character vector with one element per
citation: qesR first, then each study in the order given. For
`"bibentry"`, a `bibentry` object with the same entries; its keys are
`qesR` and the study codes. `qes_cite(style = "bibentry")` with no study
is the same object as `citation("qesR")`.

## Details

The three 1998 surveys (`qes1998`, `qes1998_crop`, `qes1998_createc`)
share one deposit, so their citations add the data file used.

A study released under a licence other than CC0 has its licence and the
licence's URL at the end of its text citation: `qes2022` is licensed CC
BY-NC 4.0, which also covers the description of it that qesR ships (see
the file `COPYRIGHTS` of the installed package).

For data from
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md),
the qesR citation also gives the harmonization spec version and content
hash the values depend on.

## En français

`qes_cite()` donne la citation de qesR et, pour chaque étude nommée ou
chargée, celle de son dépôt Dataverse. Une étude diffusée sous une autre
licence que CC0 termine sa citation texte par sa licence et l'adresse de
celle-ci : `qes2022` est sous licence CC BY-NC 4.0, qui couvre aussi la
description de l'étude livrée avec qesR (voir le fichier `COPYRIGHTS` du
package installé).

## See also

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
for the catalog, and `citation("qesR")`.

Other reproducibility:
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)

## Examples

``` r
qes_cite()
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.9.0, https://github.com/ThomasGareau/qesR"
qes_cite(c("qes2018", "qes2014"))
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", R package version 0.9.0, https://github.com/ThomasGareau/qesR"                                                            
#> [2] "Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, \"Étude électorale québécoise 2018\", https://doi.org/10.5683/SP3/NWTGWS, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg=="
#> [3] "Bélanger, Éric; Nadeau, Richard, 2023, \"Étude électorale québécoise 2014\", https://doi.org/10.5683/SP3/64F7WR, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw=="                                            
qes_cite("qes1998_crop", lang = "fr")
#> [1] "Gareau-Paquette, Thomas, 2026, \"qesR: Access Quebec Election Study Datasets\", package R, version 0.9.0, https://github.com/ThomasGareau/qesR"                                                                                  
#> [2] "Durand, Claire, 2023, \"Sondages électoraux sur les élections générales québécoises de 1998\", https://doi.org/10.5683/SP2/QFUAWG, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== [fichier : Total_sondages_election_CROP1998.sav]"
cat(qes_cite("qes2022", style = "bibtex"), sep = "\n\n")
#> @Manual{qesR,
#>   title = {{qesR}: Access Quebec Election Study Datasets},
#>   author = {Thomas Gareau-Paquette},
#>   year = {2026},
#>   note = {R package version 0.9.0},
#>   url = {https://github.com/ThomasGareau/qesR},
#> }
#> 
#> @Misc{qes2022,
#>   title = {2022 Quebec Election Study},
#>   author = {Valérie-Anne Mahéo and Éric Bélanger and Laura B Stephenson and Allison Harell},
#>   year = {2023},
#>   publisher = {Harvard Dataverse},
#>   version = {V1.1},
#>   doi = {10.7910/DVN/PAQBDR},
#>   url = {https://doi.org/10.7910/DVN/PAQBDR},
#>   note = {UNF:6:I/DFDdqJv7wNEoyyRdxaIw==},
#> }
```
