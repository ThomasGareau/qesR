# Catalogue des études

*[English
version](https://thomasgareau.github.io/qesR/articles/studies.md)*

Toutes les études que qesR peut charger : leur devis, leur population,
leur taille, leur licence et leurs documents.
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
et
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
renvoient les mêmes informations.

| Code | Année | Famille | Étude (DOI) | Devis | Population | n | Licence | Livre de codes hors ligne |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|
| [`qes2022`](#qes2022) | 2022 | Étude électorale québécoise | [Étude électorale québécoise 2022](https://doi.org/10.7910/DVN/PAQBDR) | Vagues préélectorale et postélectorale | Citoyens canadiens de 18 ans et plus résidant au Québec (panel en ligne) | 1 521 | CC BY-NC 4.0 | oui |
| [`qes2018`](#qes2018) | 2018 | Étude électorale québécoise | [Étude électorale québécoise 2018](https://doi.org/10.5683/SP3/NWTGWS) | Transversal postélectoral | Population québécoise de 16 ans et plus | 3 072 | CC0 1.0 | oui |
| [`qes2018_panel`](#qes2018_panel) | 2018 | Sondage panel électoral de Durand | [Sondage panel sur l’élection québécoise de 2018](https://doi.org/10.5683/SP3/XDDMMR) | Panel | Population adulte du Québec | 1 250 | CC0 1.0 | oui |
| [`qes2014`](#qes2014) | 2014 | Étude électorale québécoise | [Étude électorale québécoise 2014](https://doi.org/10.5683/SP3/64F7WR) | Transversal postélectoral | Population adulte du Québec | 1 517 | CC0 1.0 | oui |
| [`qes2012`](#qes2012) | 2012 | Étude électorale québécoise | [Étude électorale québécoise 2012](https://doi.org/10.5683/SP2/WXUPXT) | Transversal postélectoral | Population adulte du Québec | 1 505 | CC0 1.0 | oui |
| [`qes2012_panel`](#qes2012_panel) | 2012 | Sondage panel électoral de Durand | [Sondage panel sur l’élection québécoise de 2012](https://doi.org/10.5683/SP3/RKHPVL) | Panel | Population adulte du Québec | 844 | CC0 1.0 | oui |
| [`qes_crop_2007_2010`](#qes_crop_2007_2010) | 2007-2010 | Sondages CROP d’intentions de vote | [Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010](https://doi.org/10.5683/SP3/IRZ1PF) | Sondages regroupés | Population adulte du Québec | 24 027 | CC0 1.0 | oui |
| [`qes2008`](#qes2008) | 2008 | Étude électorale québécoise | [Étude électorale québécoise 2008](https://doi.org/10.5683/SP2/8KEYU3) | Transversal postélectoral | Population adulte du Québec | 1 151 | CC0 1.0 | oui |
| [`qes2007`](#qes2007) | 2007 | Étude électorale québécoise | [Étude électorale québécoise 2007](https://doi.org/10.5683/SP2/6XGOKA) | Transversal postélectoral | Population adulte du Québec | 2 175 | CC0 1.0 | oui |
| [`qes2007_panel`](#qes2007_panel) | 2007 | Sondage panel électoral de Durand | [Sondage panel sur l’élection québécoise de 2007](https://doi.org/10.5683/SP3/NDS6VT) | Panel | Population adulte du Québec | 2 442 | CC0 1.0 | oui |
| [`qes1998`](#qes1998) | 1998 | Sondages électoraux de 1998 | [Sondages électoraux sur les élections générales québécoises de 1998 : panel CROP-CREATEC](https://doi.org/10.5683/SP2/QFUAWG) | Panel | Population adulte francophone du Québec | 1 483 | CC0 1.0 | oui |
| [`qes1998_crop`](#qes1998_crop) | 1998 | Sondages électoraux de 1998 | [Sondages électoraux sur les élections générales québécoises de 1998 : CROP](https://doi.org/10.5683/SP2/QFUAWG) | Panel | Adultes du Québec interviewés en français | 450 | CC0 1.0 | oui |
| [`qes1998_createc`](#qes1998_createc) | 1998 | Sondages électoraux de 1998 | [Sondages électoraux sur les élections générales québécoises de 1998 : CREATEC](https://doi.org/10.5683/SP2/QFUAWG) | Panel | Adultes du Québec de langue maternelle française | 1 057 | CC0 1.0 | oui |

`n` est le nombre de répondants du fichier de données. Toutes les études
ne sont pas des Études électorales québécoises : celles des familles
Sondage panel électoral de Durand, Sondages CROP d’intentions de vote et
Sondages électoraux de 1998 figurent sous leur propre titre, et leurs
devis et leurs populations diffèrent ; vérifiez les deux avant de
comparer des études. Les licences, et l’attribution que demande l’étude
de 2022, sont dans [Citer qesR et les
études](https://thomasgareau.github.io/qesR/articles/fr-citations.md).

## Codes d’étude

[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
donne 13 codes d’étude. `qes1998` est le fichier de panel qui réunit les
deux sondages de 1998, qui se chargent aussi séparément (`qes1998_crop`
et `qes1998_createc`). Les données harmonisées de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
et de
[`qes_decon()`](https://thomasgareau.github.io/qesR/reference/qes_decon.md)
couvrent 11 de ces codes : toutes les études sauf les deux sondages de
1998 pris séparément.

Les sondages de 1998 n’ont interrogé que des francophones, CREATEC selon
la langue maternelle et CROP selon la langue d’usage. Les fichiers n’ont
pas de question sur la langue maternelle que qesR harmonise pour 1998 ;
les exemples comptent donc tous les répondants de 1998 comme
francophones.

## Pondérations

Chaque vague d’une étude harmonisée a une pondération recommandée, ou
aucune. Seule une pondération vérifiée à partir de la documentation de
l’étude et de son fichier est utilisée : 6 des 11 études en ont une.
Pour les autres, les colonnes de pondération de
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
valent `NA`, et les exemples tracent leurs estimations en points creux,
marqués « non pondéré : aucune pondération validée ».
`qes_spec("spec")$tables$weights` donne toutes les pondérations de
chaque étude, avec leur source.

| Étude | Vague | Pondération recommandée | Statut | Calée sur (selon la documentation, en anglais) |
|:---|:---|:---|:---|:---|
| [`qes2022`](#qes2022) | cps | `cps_weight_general` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | age; gender; education; language |
| [`qes2022`](#qes2022) | pes | `pes_weight_general` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | age; gender; education; language |
| [`qes2018`](#qes2018) | post | `pond` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | sex x age; sex x region; sex x language; age x education |
| [`qes2014`](#qes2014) | post | `POND` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | sex; age; region; language |
| [`qes2012`](#qes2012) | post | `pond` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | sex; age; region; language |
| [`qes2018_panel`](#qes2018_panel) | pre | `weight` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | sex (sexfix); age (3 bands); region (fsa_tabl, 5 regions); mother tongue (s1, French vs other); education (scolU, university or not) |
| [`qes2018_panel`](#qes2018_panel) | post | `weight_rts` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | sex (sexfix); age (3 bands); region (fsa_tabl, 5 regions); mother tongue (s1, French vs other); education (scolU, university or not) |
| [`qes2012_panel`](#qes2012_panel) | pre | `pondam1` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | sex; age (6 bands); region (reg, 4); mother tongue (lmat, 3) |
| [`qes2012_panel`](#qes2012_panel) | post | `pond_post` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | sex; age (6 bands); region (reg, 4); mother tongue (lmat, 3) |
| [`qes2007_panel`](#qes2007_panel) | pre | `pondam1` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | poll (nompn); sex; age (6 bands); region (reg, 3); home language (lusage2, 2); education (scol, 4) |
| [`qes2007_panel`](#qes2007_panel) | post | `pond_tot_am1` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | poll (nompn); sex; age (6 bands); region (reg, 3); home language (lusage2, 2); education (scol, 4) |
| [`qes2007`](#qes2007) | post | `pond` | pondéré : validée, dans les colonnes de pondération de [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md) | sex; age; region; mother tongue |
| [`qes2008`](#qes2008) | post | aucune recommandée | non pondéré : les pondérations du fichier (`pond`, `pondx`) sont calées sur le vote déclaré | sex; age; region (reg, 5); mother tongue; education; reported vote |
| [`qes1998`](#qes1998) | pre | `ponder3` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | pre-election intention stratum (vpl, through poids) x each firm’s weight ponderc (CROP: 1996 Census; CREATEC: undocumented) |
| [`qes1998`](#qes1998) | post | `ponder3` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | pre-election intention stratum (vpl, through poids) x each firm’s weight ponderc (CROP: 1996 Census; CREATEC: undocumented) |
| [`qes_crop_2007_2010`](#qes_crop_2007_2010) | chaque sondage | `XPOND` | non pondéré : pondération non validée (colonnes de pondération à `NA`) | sex; age (18-34, 35-54, 55+); region (Montréal CMA, Québec CMA, elsewhere); home language (French, English or other) |

## Par étude

La ligne Dépôt renvoie au DOI du dépôt. Les liens des documents
téléchargent le fichier depuis Dataverse ;
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
les enregistre dans un dossier de votre choix.

### `qes2022` : Étude électorale québécoise 2022

- **Étude** : Étude électorale québécoise ; Vagues préélectorale et
  postélectorale ; Citoyens canadiens de 18 ans et plus résidant au
  Québec (panel en ligne)
- **Dépôt** : <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse,
  version 1.1 ; licence [Attribution - Pas d’utilisation commerciale 4.0
  (CC BY-NC 4.0)](http://creativecommons.org/licenses/by-nc/4.0)
- **Fichier de données** : `2022 Quebec Election Study v1.dta` (Fichier
  de données Stata), 1 521 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)),
  sous la licence de l’étude
- **Note** : Deux vagues : période de campagne (1 521 réponses valides)
  et recontact postélectoral (1 220 conservées), selon la note
  méthodologique du livre de codes. Licence CC BY-NC 4.0.
- **Documents** (`qes_docs("qes2022")`) :
  - [2022 Quebec Election Study Codebook
    v1.pdf](https://dataverse.harvard.edu/api/access/datafile/7449514) :
    Livre de codes, Anglais, PDF
- **Citer** : `qes_cite("qes2022")`

### `qes2018` : Étude électorale québécoise 2018

- **Étude** : Étude électorale québécoise ; Transversal postélectoral ;
  Population québécoise de 16 ans et plus
- **Dépôt** : <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Quebec Election Study 2018.dta` (Fichier de
  données Stata), 3 072 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Sondage postélectoral en ligne. Comprend 251 répondants de
  16 ou 17 ans (note du dépôt).
- **Documents** (`qes_docs("qes2018")`) :
  - [Quebec Election Study 2018
    EN.doc](https://borealisdata.ca/api/access/datafile/361049) :
    Questionnaire, Anglais, DOC
  - [Quebec Election Study 2018
    FR.doc](https://borealisdata.ca/api/access/datafile/361050) :
    Questionnaire, Français, DOC
  - [Quebec Election Study 2018 FR with programmed answer
    values.docx](https://borealisdata.ca/api/access/datafile/367181) :
    Livre de codes, Français, DOCX
  - [Rapport méthodologique de l’Étude électorale québécoise
    2018](https://borealisdata.ca/api/access/datafile/361045) : Rapport
    méthodologique, Français, PDF
- **Citer** : `qes_cite("qes2018")`

### `qes2018_panel` : Sondage panel sur l’élection québécoise de 2018

- **Étude** : Sondage panel électoral de Durand ; Panel ; Population
  adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `IPsos_oct_2018_17-057727_V.SAV` (Fichier
  système SPSS), 1 250 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Panel Ipsos, deux vagues, téléphone et Web (métadonnées du
  dépôt). Ne fait pas partie des Études électorales québécoises.
- **Documents** (`qes_docs("qes2018_panel")`) :
  - [LivredeCodes_SondagePanelQC2018.pdf](https://borealisdata.ca/api/access/datafile/341538) :
    Livre de codes, Français, PDF
- **Citer** : `qes_cite("qes2018_panel")`

### `qes2014` : Étude électorale québécoise 2014

- **Étude** : Étude électorale québécoise ; Transversal postélectoral ;
  Population adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP3/64F7WR>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Quebec Election Study 2014.sav` (Fichier
  système SPSS), 1 517 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Sondage postélectoral en ligne. Les fichiers SPSS et Stata
  contiennent les mêmes données ; le fichier SPSS est retenu.
- **Documents** (`qes_docs("qes2014")`) :
  - [Quebec Election Study 2014
    EN.doc](https://borealisdata.ca/api/access/datafile/352010) :
    Questionnaire, Anglais, DOC
  - [Quebec Election Study 2014
    FR.doc](https://borealisdata.ca/api/access/datafile/352009) :
    Questionnaire, Français, DOC
  - [Quebec Election Study 2014 - Technical Report - Post-Election
    Study.doc](https://borealisdata.ca/api/access/datafile/352011) :
    Rapport technique, Français, DOC
- **Citer** : `qes_cite("qes2014")`

### `qes2012` : Étude électorale québécoise 2012

- **Étude** : Étude électorale québécoise ; Transversal postélectoral ;
  Population adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Quebec Election Study 2012 (STATA).dta`
  (Fichier de données Stata), 1 505 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Sondage postélectoral en ligne, mené avec le Scotland
  Opinion Survey 2012 (description du dépôt). Le fichier Stata est lu ;
  le fichier SPSS de même UNF fournit les étiquettes.
- **Documents** (`qes_docs("qes2012")`) :
  - [Quebec Election Study 2012
    EN.doc](https://borealisdata.ca/api/access/datafile/196367) :
    Questionnaire, Anglais, DOC
  - [Quebec Election Study 2012
    FR.doc](https://borealisdata.ca/api/access/datafile/196370) :
    Questionnaire, Français, DOC
  - [Rapport technique - Etude sur la politique -
    Quebec-Ecosse.doc](https://borealisdata.ca/api/access/datafile/196368) :
    Rapport technique, Français, DOC
- **Citer** : `qes_cite("qes2012")`

### `qes2012_panel` : Sondage panel sur l’élection québécoise de 2012

- **Étude** : Sondage panel électoral de Durand ; Panel ; Population
  adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `repondants_post_2012.sav` (Fichier système
  SPSS), 844 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Panel téléphonique CROP, deux vagues (métadonnées du
  dépôt). Le fichier contient les répondants des deux vagues. Ne fait
  pas partie des Études électorales québécoises.
- **Documents** (`qes_docs("qes2012_panel")`) :
  - [Livre de codes - Panel Québec
    2012.pdf](https://borealisdata.ca/api/access/datafile/654292) :
    Livre de codes, Français, PDF
  - [Questionnaire-post_electionqc-2012_EN.docx](https://borealisdata.ca/api/access/datafile/654290) :
    Questionnaire, Anglais, DOCX
  - [Questionnaire-post_electionqc-2012_FR.docx](https://borealisdata.ca/api/access/datafile/654291) :
    Questionnaire, Français, DOCX
- **Citer** : `qes_cite("qes2012_panel")`

### `qes_crop_2007_2010` : Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010

- **Étude** : Sondages CROP d’intentions de vote ; Sondages regroupés ;
  Population adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `intvotetotal_juin2007_jan2010.sav` (Fichier
  système SPSS), 24 027 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Sondages téléphoniques mensuels CROP réunis dans un
  fichier, environ 1 000 répondants chacun (métadonnées du dépôt). Ne
  fait pas partie des Études électorales québécoises.
- **Documents** (`qes_docs("qes_crop_2007_2010")`) :
  - [LivredeCodes_SondagesCROP_2007-2010.pdf](https://borealisdata.ca/api/access/datafile/341537) :
    Livre de codes, Français, PDF
- **Citer** : `qes_cite("qes_crop_2007_2010")`

### `qes2008` : Étude électorale québécoise 2008

- **Étude** : Étude électorale québécoise ; Transversal postélectoral ;
  Population adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Quebec Election Study 2008 (SPSS).sav`
  (Fichier système SPSS), 1 151 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Sondage postélectoral téléphonique (métadonnées du dépôt).
  Les fichiers SPSS et Stata ont des UNF différents ; le fichier SPSS
  est retenu.
- **Documents** (`qes_docs("qes2008")`) :
  - [Quebec Election Study 2008
    FR.doc](https://borealisdata.ca/api/access/datafile/196358) :
    Questionnaire, Français, DOC
  - [Quebec Election study 2008
    ENG.pdf](https://borealisdata.ca/api/access/datafile/197296) :
    Questionnaire, Anglais, PDF
- **Citer** : `qes_cite("qes2008")`

### `qes2007` : Étude électorale québécoise 2007

- **Étude** : Étude électorale québécoise ; Transversal postélectoral ;
  Population adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Quebec Election Study 2007 (SPSS).sav`
  (Fichier système SPSS), 2 175 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Sondage postélectoral, téléphone et Web (métadonnées du
  dépôt). Les fichiers SPSS et Stata contiennent les mêmes données
  (leurs UNF diffèrent) ; le fichier SPSS, dont les étiquettes sont
  complètes, est retenu. Les deux stockent la plupart des codes de
  réponse sous forme de texte (« 01 ») ; qesR les lit comme des nombres.
- **Documents** (`qes_docs("qes2007")`) :
  - [Quebec Election Study 2007
    ENG.doc](https://borealisdata.ca/api/access/datafile/192423) :
    Questionnaire, Anglais, DOC
  - [Quebec Election Study 2007
    FR.doc](https://borealisdata.ca/api/access/datafile/192422) :
    Questionnaire, Français, DOC
- **Citer** : `qes_cite("qes2007")`

### `qes2007_panel` : Sondage panel sur l’élection québécoise de 2007

- **Étude** : Sondage panel électoral de Durand ; Panel ; Population
  adulte du Québec
- **Dépôt** : <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `complet_tous_repondants_2007.sav` (Fichier
  système SPSS), 2 442 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Panel téléphonique CROP : deux sondages préélectoraux, puis
  un recontact postélectoral (métadonnées du dépôt). Les lignes sont
  identifiées par le projet et le numéro de questionnaire. Ne fait pas
  partie des Études électorales québécoises.
- **Documents** (`qes_docs("qes2007_panel")`) :
  - [LivredeCodes_SondagePanel-QC2007.pdf](https://borealisdata.ca/api/access/datafile/352416) :
    Livre de codes, Français, PDF
  - [Questionnaire_post-sondage-panel-electionqc-2007_EN.doc](https://borealisdata.ca/api/access/datafile/354955) :
    Questionnaire, Anglais, DOC
  - [Questionnaire_post-sondage-panel-electionqc-2007_FR.doc](https://borealisdata.ca/api/access/datafile/354956) :
    Questionnaire, Français, DOC
- **Citer** : `qes_cite("qes2007_panel")`

### `qes1998` : Sondages électoraux sur les élections générales québécoises de 1998 : panel CROP-CREATEC

- **Étude** : Sondages électoraux de 1998 ; Panel ; Population adulte
  francophone du Québec
- **Dépôt** : <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Total_panel_election_QC1998.sav` (Fichier
  système SPSS), 1 483 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Fichier panel réunissant les sondages CROP et CREATEC (1
  483 questionnaires valides), chacun avec une vague préélectorale et
  une vague postélectorale par téléphone. Francophones seulement : le
  livre de codes indique qu’il a été décidé de « retenir uniquement les
  francophones » ; la définition exacte retenue pour le fichier combiné
  reste à préciser. Ne fait pas partie des Études électorales
  québécoises.
- **Documents** (`qes_docs("qes1998")`) :
  - [LivredeCodes_Panel_QC1998.pdf](https://borealisdata.ca/api/access/datafile/332051) :
    Livre de codes, Français, PDF
- **Citer** : `qes_cite("qes1998")`

### `qes1998_crop` : Sondages électoraux sur les élections générales québécoises de 1998 : CROP

- **Étude** : Sondages électoraux de 1998 ; Panel ; Adultes du Québec
  interviewés en français
- **Dépôt** : <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Total_sondages_election_CROP1998.sav`
  (Fichier système SPSS), 450 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Fichier CROP : sondage préélectoral de 450 répondants
  interviewés en français et sa vague postélectorale, qui ne retient que
  les personnes dont la langue d’usage est le français (N=426), selon la
  description du livre de codes. Les indécis, les personnes qui disaient
  vouloir annuler leur vote et celles qui refusaient de révéler leur
  vote ont été sur-sélectionnés.
- **Documents** (`qes_docs("qes1998_crop")`) :
  - [LivredeCodes_CROP_1998.pdf](https://borealisdata.ca/api/access/datafile/332049) :
    Livre de codes, Français, PDF
- **Citer** : `qes_cite("qes1998_crop")`

### `qes1998_createc` : Sondages électoraux sur les élections générales québécoises de 1998 : CREATEC

- **Étude** : Sondages électoraux de 1998 ; Panel ; Adultes du Québec de
  langue maternelle française
- **Dépôt** : <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, version
  1.0 ; licence [Transfert dans le domaine public (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Fichier de données** : `Total_sondages_election_CREATEC1998.sav`
  (Fichier système SPSS), 1 057 lignes
- **Livre de codes hors ligne** : oui, livré avec qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note** : Fichier CREATEC : sondage téléphonique préélectoral de 1
  057 répondants de langue maternelle française et sa vague
  postélectorale ; les indécis et les refus ont été suréchantillonnés.
  Le dépôt recommande la pondération ponderc (descriptions du livre de
  codes et du fichier).
- **Documents** (`qes_docs("qes1998_createc")`) :
  - [LivredeCodes_CREATEC_1998.pdf](https://borealisdata.ca/api/access/datafile/332050) :
    Livre de codes, Français, PDF
- **Citer** : `qes_cite("qes1998_createc")`

## Citer

[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
donne la citation de chaque étude ; voir [Citer qesR et les
études](https://thomasgareau.github.io/qesR/articles/fr-citations.md).
