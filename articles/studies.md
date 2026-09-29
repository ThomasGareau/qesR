# Study catalog

*[Version
française](https://thomasgareau.github.io/qesR/articles/fr-etudes.md)*

Every study that qesR can load, with what qesR pins for it: the deposit
and its version, the data file and its md5 checksum, and the documents.
This page is generated when the site is built, from the catalog that
ships with the package (`inst/extdata/catalog/`), the same data as
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
and
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md).
None of it is copied by hand.

| Code | Year | Family | Study (DOI) | Design | Population | n | Licence | Offline codebook |
|:---|:---|:---|:---|:---|:---|---:|:---|:---|
| [`qes2022`](#qes2022) | 2022 | Quebec Election Study | [Quebec Election Study 2022](https://doi.org/10.7910/DVN/PAQBDR) | Pre- and post-election waves | Canadian citizens aged 18+ residing in Quebec (online panel) | 1,521 | CC BY-NC 4.0 | yes |
| [`qes2018`](#qes2018) | 2018 | Quebec Election Study | [Quebec Election Study 2018](https://doi.org/10.5683/SP3/NWTGWS) | Post-election cross-section | Quebec population aged 16 and over | 3,072 | CC0 1.0 | yes |
| [`qes2018_panel`](#qes2018_panel) | 2018 | Durand election panel survey | [Panel Survey on the 2018 Quebec Election](https://doi.org/10.5683/SP3/XDDMMR) | Panel | Quebec adult population | 1,250 | CC0 1.0 | yes |
| [`qes2014`](#qes2014) | 2014 | Quebec Election Study | [Quebec Election Study 2014](https://doi.org/10.5683/SP3/64F7WR) | Post-election cross-section | Quebec adult population | 1,517 | CC0 1.0 | yes |
| [`qes2012`](#qes2012) | 2012 | Quebec Election Study | [Quebec Election Study 2012](https://doi.org/10.5683/SP2/WXUPXT) | Post-election cross-section | Quebec adult population | 1,505 | CC0 1.0 | yes |
| [`qes2012_panel`](#qes2012_panel) | 2012 | Durand election panel survey | [Panel Survey on the 2012 Quebec Election](https://doi.org/10.5683/SP3/RKHPVL) | Panel | Quebec adult population | 844 | CC0 1.0 | yes |
| [`qes_crop_2007_2010`](#qes_crop_2007_2010) | 2007-2010 | CROP vote-intention polls | [CROP Polls on Quebec Provincial Vote Intentions, 2007-2010](https://doi.org/10.5683/SP3/IRZ1PF) | Pooled polls | Quebec adult population | 24,027 | CC0 1.0 | yes |
| [`qes2008`](#qes2008) | 2008 | Quebec Election Study | [Quebec Election Study 2008](https://doi.org/10.5683/SP2/8KEYU3) | Post-election cross-section | Quebec adult population | 1,151 | CC0 1.0 | yes |
| [`qes2007`](#qes2007) | 2007 | Quebec Election Study | [Quebec Election Study 2007](https://doi.org/10.5683/SP2/6XGOKA) | Post-election cross-section | Quebec adult population | 2,175 | CC0 1.0 | yes |
| [`qes2007_panel`](#qes2007_panel) | 2007 | Durand election panel survey | [Panel Survey on the 2007 Quebec Election](https://doi.org/10.5683/SP3/NDS6VT) | Panel | Quebec adult population | 2,442 | CC0 1.0 | yes |
| [`qes1998`](#qes1998) | 1998 | 1998 election polls | [1998 Quebec General Election Polls: CROP-CREATEC Panel](https://doi.org/10.5683/SP2/QFUAWG) | Panel | Francophone adult population of Quebec | 1,483 | CC0 1.0 | yes |
| [`qes1998_crop`](#qes1998_crop) | 1998 | 1998 election polls | [1998 Quebec General Election Polls: CROP](https://doi.org/10.5683/SP2/QFUAWG) | Panel | Quebec adults interviewed in French | 450 | CC0 1.0 | yes |
| [`qes1998_createc`](#qes1998_createc) | 1998 | 1998 election polls | [1998 Quebec General Election Polls: CREATEC](https://doi.org/10.5683/SP2/QFUAWG) | Panel | Quebec adults whose mother tongue is French | 1,057 | CC0 1.0 | yes |

`n` is the number of rows in the pinned data file. Studies of the
families Durand election panel survey, CROP vote-intention polls and
1998 election polls are not Quebec Election Studies: they are listed
under their own titles, and their designs and populations differ, so
check both before comparing studies. Codes sharing one deposit:
`qes1998`, `qes1998_crop` and `qes1998_createc`. Studies not in the
public domain: `qes2022` (CC BY-NC 4.0); the metadata qesR ships for
them keeps their licence (the file `COPYRIGHTS` of the package lists
it). Studies whose metadata qesR does not ship: none.

## By study

The Deposit line links to the deposit’s DOI. The document links download
the file from Dataverse;
[`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
saves them, md5-checked, in a folder you choose.

### `qes2022`: Quebec Election Study 2022

- **Study**: Quebec Election Study; Pre- and post-election waves;
  Canadian citizens aged 18+ residing in Quebec (online panel)
- **Deposit**: <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse,
  version 1.1; licence [Attribution-NonCommercial 4.0 (CC BY-NC
  4.0)](http://creativecommons.org/licenses/by-nc/4.0)
- **Pinned data file**: `2022 Quebec Election Study v1.dta` (Stata data
  file), 1,521 rows × 718 columns, md5
  `c51bafed57776ffa8d4c6f301f5c945b`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)),
  under the study’s licence
- **Note**: Two waves: campaign period (1,521 valid) and post-election
  recontact (1,220 kept), per the codebook’s methodological note.
  Licensed CC BY-NC 4.0.
- **Documents** (`qes_docs("qes2022")`):
  - [2022 Quebec Election Study Codebook
    v1.pdf](https://dataverse.harvard.edu/api/access/datafile/7449514):
    Codebook, English, PDF
- **Cite**: `qes_cite("qes2022")`

### `qes2018`: Quebec Election Study 2018

- **Study**: Quebec Election Study; Post-election cross-section; Quebec
  population aged 16 and over
- **Deposit**: <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Quebec Election Study 2018.dta` (Stata data
  file), 3,072 rows × 254 columns, md5
  `d24f5b0be727d688ad305b8eb61f0d30`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Online post-election survey. Includes 251 respondents aged
  16-17 (deposit note).
- **Documents** (`qes_docs("qes2018")`):
  - [Quebec Election Study 2018
    EN.doc](https://borealisdata.ca/api/access/datafile/361049):
    Questionnaire, English, DOC
  - [Quebec Election Study 2018
    FR.doc](https://borealisdata.ca/api/access/datafile/361050):
    Questionnaire, French, DOC
  - [Quebec Election Study 2018 FR with programmed answer
    values.docx](https://borealisdata.ca/api/access/datafile/367181):
    Codebook, French, DOCX
  - [Rapport méthodologique de l’Étude électorale québécoise
    2018](https://borealisdata.ca/api/access/datafile/361045):
    Methodological report, French, PDF
- **Cite**: `qes_cite("qes2018")`

### `qes2018_panel`: Panel Survey on the 2018 Quebec Election

- **Study**: Durand election panel survey; Panel; Quebec adult
  population
- **Deposit**: <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `IPsos_oct_2018_17-057727_V.SAV` (SPSS system
  file), 1,250 rows × 72 columns, md5 `b6fc93f2918ae4a56de3d19e80d0bd15`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Ipsos panel, two waves, telephone and web (deposit
  metadata). Not a Quebec Election Study.
- **Documents** (`qes_docs("qes2018_panel")`):
  - [LivredeCodes_SondagePanelQC2018.pdf](https://borealisdata.ca/api/access/datafile/341538):
    Codebook, French, PDF
- **Cite**: `qes_cite("qes2018_panel")`

### `qes2014`: Quebec Election Study 2014

- **Study**: Quebec Election Study; Post-election cross-section; Quebec
  adult population
- **Deposit**: <https://doi.org/10.5683/SP3/64F7WR>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Quebec Election Study 2014.sav` (SPSS system
  file), 1,517 rows × 140 columns, md5
  `9549423b32526a2f11a3d954a87c6861`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Online post-election survey. The SPSS and Stata files hold
  the same data; the SPSS file is pinned.
- **Documents** (`qes_docs("qes2014")`):
  - [Quebec Election Study 2014
    EN.doc](https://borealisdata.ca/api/access/datafile/352010):
    Questionnaire, English, DOC
  - [Quebec Election Study 2014
    FR.doc](https://borealisdata.ca/api/access/datafile/352009):
    Questionnaire, French, DOC
  - [Quebec Election Study 2014 - Technical Report - Post-Election
    Study.doc](https://borealisdata.ca/api/access/datafile/352011):
    Technical report, French, DOC
- **Cite**: `qes_cite("qes2014")`

### `qes2012`: Quebec Election Study 2012

- **Study**: Quebec Election Study; Post-election cross-section; Quebec
  adult population
- **Deposit**: <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Quebec Election Study 2012 (STATA).dta` (Stata
  data file), 1,505 rows × 177 columns, md5
  `e5ec063d9b1b03b48f458be4d8ec3d7b`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Online post-election survey, run with the Scotland Opinion
  Survey 2012 (deposit description). The Stata file is read; the SPSS
  file with the same UNF supplies labels.
- **Documents** (`qes_docs("qes2012")`):
  - [Quebec Election Study 2012
    EN.doc](https://borealisdata.ca/api/access/datafile/196367):
    Questionnaire, English, DOC
  - [Quebec Election Study 2012
    FR.doc](https://borealisdata.ca/api/access/datafile/196370):
    Questionnaire, French, DOC
  - [Rapport technique - Etude sur la politique -
    Quebec-Ecosse.doc](https://borealisdata.ca/api/access/datafile/196368):
    Technical report, French, DOC
- **Cite**: `qes_cite("qes2012")`

### `qes2012_panel`: Panel Survey on the 2012 Quebec Election

- **Study**: Durand election panel survey; Panel; Quebec adult
  population
- **Deposit**: <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `repondants_post_2012.sav` (SPSS system file),
  844 rows × 79 columns, md5 `8f90a9f97a33e01eff3748f05612e26c`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: CROP telephone panel, two waves (deposit metadata). The file
  holds respondents who completed both waves. Not a Quebec Election
  Study.
- **Documents** (`qes_docs("qes2012_panel")`):
  - [Livre de codes - Panel Québec
    2012.pdf](https://borealisdata.ca/api/access/datafile/654292):
    Codebook, French, PDF
  - [Questionnaire-post_electionqc-2012_EN.docx](https://borealisdata.ca/api/access/datafile/654290):
    Questionnaire, English, DOCX
  - [Questionnaire-post_electionqc-2012_FR.docx](https://borealisdata.ca/api/access/datafile/654291):
    Questionnaire, French, DOCX
- **Cite**: `qes_cite("qes2012_panel")`

### `qes_crop_2007_2010`: CROP Polls on Quebec Provincial Vote Intentions, 2007-2010

- **Study**: CROP vote-intention polls; Pooled polls; Quebec adult
  population
- **Deposit**: <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `intvotetotal_juin2007_jan2010.sav` (SPSS system
  file), 24,027 rows × 19 columns, md5
  `534514c7414d462259de4421bb4579e5`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Monthly CROP telephone polls pooled in one file, about 1,000
  respondents each (deposit metadata). Not a Quebec Election Study.
- **Documents** (`qes_docs("qes_crop_2007_2010")`):
  - [LivredeCodes_SondagesCROP_2007-2010.pdf](https://borealisdata.ca/api/access/datafile/341537):
    Codebook, French, PDF
- **Cite**: `qes_cite("qes_crop_2007_2010")`

### `qes2008`: Quebec Election Study 2008

- **Study**: Quebec Election Study; Post-election cross-section; Quebec
  adult population
- **Deposit**: <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Quebec Election Study 2008 (SPSS).sav` (SPSS
  system file), 1,151 rows × 63 columns, md5
  `6a69b00b943bb357c3ed10091d936ba9`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Telephone post-election survey (deposit metadata). The SPSS
  and Stata files differ in UNF; the SPSS file read by qesR 0.4.4 is
  pinned.
- **Documents** (`qes_docs("qes2008")`):
  - [Quebec Election Study 2008
    FR.doc](https://borealisdata.ca/api/access/datafile/196358):
    Questionnaire, French, DOC
  - [Quebec Election study 2008
    ENG.pdf](https://borealisdata.ca/api/access/datafile/197296):
    Questionnaire, English, PDF
- **Cite**: `qes_cite("qes2008")`

### `qes2007`: Quebec Election Study 2007

- **Study**: Quebec Election Study; Post-election cross-section; Quebec
  adult population
- **Deposit**: <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Quebec Election Study 2007 (SPSS).sav` (SPSS
  system file), 2,175 rows × 93 columns, md5
  `e1324bbb582cf6bc01eb9949ecb8b132`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: Post-election survey, telephone and web (deposit metadata).
  The SPSS and Stata files hold the same data (their UNFs differ); the
  SPSS file, whose labels are complete, is pinned. Both store most
  answer codes as text (“01”); qesR reads them as numbers, as qesR 0.4.4
  did.
- **Documents** (`qes_docs("qes2007")`):
  - [Quebec Election Study 2007
    ENG.doc](https://borealisdata.ca/api/access/datafile/192423):
    Questionnaire, English, DOC
  - [Quebec Election Study 2007
    FR.doc](https://borealisdata.ca/api/access/datafile/192422):
    Questionnaire, French, DOC
- **Cite**: `qes_cite("qes2007")`

### `qes2007_panel`: Panel Survey on the 2007 Quebec Election

- **Study**: Durand election panel survey; Panel; Quebec adult
  population
- **Deposit**: <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `complet_tous_repondants_2007.sav` (SPSS system
  file), 2,442 rows × 270 columns, md5
  `20f8c6fd4211744340f76293d18e5b6d`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: CROP telephone panel: two pre-election surveys, then a
  post-election recontact (deposit metadata). Rows are keyed by project
  and questionnaire number. Not a Quebec Election Study.
- **Documents** (`qes_docs("qes2007_panel")`):
  - [LivredeCodes_SondagePanel-QC2007.pdf](https://borealisdata.ca/api/access/datafile/352416):
    Codebook, French, PDF
  - [Questionnaire_post-sondage-panel-electionqc-2007_EN.doc](https://borealisdata.ca/api/access/datafile/354955):
    Questionnaire, English, DOC
  - [Questionnaire_post-sondage-panel-electionqc-2007_FR.doc](https://borealisdata.ca/api/access/datafile/354956):
    Questionnaire, French, DOC
- **Cite**: `qes_cite("qes2007_panel")`

### `qes1998`: 1998 Quebec General Election Polls: CROP-CREATEC Panel

- **Study**: 1998 election polls; Panel; Francophone adult population of
  Quebec
- **Deposit**: <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Total_panel_election_QC1998.sav` (SPSS system
  file), 1,483 rows × 42 columns, md5 `a2a2b2fef6a2edb202beae0e63e59f0c`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: The panel file combining the CROP and CREATEC polls (1,483
  valid questionnaires), each with a pre- and a post-election wave by
  telephone. Francophones only: the codebook says it was decided to keep
  only francophones (« retenir uniquement les francophones »); the exact
  definition used for the pooled file is pending. Not a Quebec Election
  Study.
- **Documents** (`qes_docs("qes1998")`):
  - [LivredeCodes_Panel_QC1998.pdf](https://borealisdata.ca/api/access/datafile/332051):
    Codebook, French, PDF
- **Cite**: `qes_cite("qes1998")`

### `qes1998_crop`: 1998 Quebec General Election Polls: CROP

- **Study**: 1998 election polls; Panel; Quebec adults interviewed in
  French
- **Deposit**: <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Total_sondages_election_CROP1998.sav` (SPSS
  system file), 450 rows × 37 columns, md5
  `ceec0332eaa0eab117e4bcf5c4d8d672`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: CROP file: pre-election poll of 450 respondents interviewed
  in French and its post-election wave, which kept only respondents
  whose language of use is French (N=426), per the codebook description.
  Undecided respondents, those who said they would spoil their ballot
  and those who refused to reveal their vote were oversampled.
- **Documents** (`qes_docs("qes1998_crop")`):
  - [LivredeCodes_CROP_1998.pdf](https://borealisdata.ca/api/access/datafile/332049):
    Codebook, French, PDF
- **Cite**: `qes_cite("qes1998_crop")`

### `qes1998_createc`: 1998 Quebec General Election Polls: CREATEC

- **Study**: 1998 election polls; Panel; Quebec adults whose mother
  tongue is French
- **Deposit**: <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, version
  1.0; licence [Public domain dedication (CC0
  1.0)](http://creativecommons.org/publicdomain/zero/1.0)
- **Pinned data file**: `Total_sondages_election_CREATEC1998.sav` (SPSS
  system file), 1,057 rows × 49 columns, md5
  `1aba610f71f1b5c46e03da2036e83330`
- **Offline codebook**: yes, shipped with qesR
  ([`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md),
  [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md))
- **Note**: CREATEC file: telephone pre-election poll of 1,057
  respondents whose mother tongue is French and its post-election wave;
  undecided and refusing respondents were oversampled. The deposit
  recommends the weight ponderc (codebook and file descriptions).
- **Documents** (`qes_docs("qes1998_createc")`):
  - [LivredeCodes_CREATEC_1998.pdf](https://borealisdata.ca/api/access/datafile/332050):
    Codebook, French, PDF
- **Cite**: `qes_cite("qes1998_createc")`

## Citing

[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
gives the citation of each study; see [Study
citations](https://thomasgareau.github.io/qesR/articles/citations.md).
