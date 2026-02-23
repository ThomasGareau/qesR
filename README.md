# qesR

<p align="center">
  <img src="man/figures/logo.png" alt="qesR logo" width="220" />
</p>

Access Quebec Election Study datasets in R using simple survey-code calls.

This package mirrors the core ergonomics of `cesR` while targeting studies
listed in the Quebec opinion portal:
<https://csdc-cecd.ca/portail-quebecois-sur-lopinion-publique/#section1>

## Installation

Install from GitHub:

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

Install from a local source tarball:

```r
install.packages("/path/to/qesR_0.4.4.tar.gz", repos = NULL, type = "source")
```

Install from a local package folder:

```r
install.packages("/path/to/qesR", repos = NULL, type = "source")
```

## Merged Dataset

`qesR` includes a harmonized merged file across studies through
`get_qes_master()`.

```r
library(qesR)

master <- get_qes_master(strict = FALSE)
head(master)
```

The merged data can be saved directly:

```r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```

What it harmonizes:

Current harmonized columns in `qes_master`:

`qes_code`, `qes_year`, `qes_name_en`, `respondent_id`, `interview_start`, `interview_end`, `interview_recorded`, `language`, `citizenship`, `year_of_birth`, `age`, `age_group`, `gender`, `province_territory`, `education`, `income`, `religion`, `born_canada`, `political_interest`, `ideology`, `turnout`, `vote_choice`, `vote_choice_text`, `party_best`, `party_lean`, `sovereignty_support`, `sovereignty`, `federal_pid`, `provincial_pid`, `survey_weight`, `definibin`, `ethn1`, `influperso`, `luentend`, `occup`, `patron`, `pondam1`, `q10`, `q16`, `q17`, `q18`, `q18a`, `q18b`, `q20`, `q20b`, `q22`, `q23`, `q24`, `q3`, `q31`, `q33`, `q35`, `q37`, `q38`, `q39`, `q4`, `q40`, `q41`, `q42`, `q43`, `q44`, `q45`, `q46`, `q47`, `q48`, `q49`, `q50`, `q51`, `q53`, `q54`, `q55`, `q56`, `q58`, `q59`, `q60`, `q61b`, `q61d`, `q62a`, `q62b`, `q64`, `q65`, `q66`, `q68`, `q7`, `q70`, `q72`, `q73`, `q74`, `q79`, `q8`, `q80`, `q81`, `q9`, `raison1`, `raison2`, `s_jse`, `satisf`, `sefie`, `sondbons`, `voteprec`

Harmonization rules include:

- text normalization for common response fields (so values are comparable across studies)
- derived `age_group` for respondents with valid age/year-of-birth information
- source-variable tracking via `attr(master, "source_map")`
- list of automatically added cross-study fields via `attr(master, "crossstudy_variables_added")`

Deduplication in `get_qes_master()` is applied **within the same survey code**
only (e.g., duplicate IDs inside one file). Respondents are not removed across
panel vs. non-panel studies.

## Website

Project website (GitHub Pages): <https://thomasgareau.github.io/qesR/>

The site includes researcher-focused tabs for:

- merged dataset workflow
- study citations
- 2 use-case tabs (sovereignty, vote choice)
- an `FR`/`EN` button in the top bar for page-by-page language switching

## Usage

```r
library(qesR)

# list available study codes
get_qescodes()

# detailed metadata table (DOI, documentation links, names)
get_qescodes(detailed = TRUE)

# load one study and assign object qes2018 into .GlobalEnv
qes2018 <- get_qes("qes2018")

# qes2018 is returned with variable/value labels (when available)
# and a codebook is attached:
cb <- attr(qes2018, "qes_codebook")
head(cb)

# you can also fetch the codebook directly
qes2018_codebook <- get_codebook("qes2018")
# compact layout (default): variable, label, question, n_value_labels

# wide layout: adds list-column with value labels
qes2018_codebook_wide <- get_codebook("qes2018", layout = "wide")

# long layout: one row per value label
qes2018_codebook_long <- get_codebook("qes2018", layout = "long")

# reformat an existing codebook object
format_codebook(qes2018_codebook, layout = "wide")
get_value_labels(qes2018_codebook, long = TRUE)

# alias
qes2018_codebook <- qes_codebook("qes2018")
qes2018_codebook <- get_qes_codebook("qes2018")

# codebook/support files (PDFs, questionnaires, metadata)
get_codebook_files(codebook = qes2018_codebook)
get_qes_codebook_files(codebook = qes2018_codebook)
download_codebook("qes2018", dest_dir = tempdir())

# preview first 10 rows
get_preview("qes2018", 10)

# retrieve question text from labels/codebook
get_question(qes2018, "some_variable")

# attempt fuller question recovery if metadata appears truncated
get_question(qes2018, "some_variable", full = TRUE)
# (uses `pdftotext` or `gs` when available)

# cesR-like prepared non-exhaustive dataset
decon <- get_decon("qes2022")
head(decon)

# harmonized stacked master dataset across studies
qes_master <- get_qes_master()
head(qes_master)
# includes derived age_group and harmonized education/turnout/vote fields

# inspect which source variable fed each harmonized field
head(attr(qes_master, "source_map"))
```

## Study Citations

| Year | Code | Study | Citation | Documentation |
|---|---|---|---|---|
| 2022 | `qes2022` | 2022 Quebec Election Study | Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, "2022 Quebec Election Study", <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse, V1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [fileUNF] | <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PAQBDR> |
| 2018 | `qes2018` | Étude électorale québécoise 2018 | Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, "Étude électorale québécoise 2018", <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/NWTGWS> |
| 2018 | `qes2018_panel` | Sondage panel sur l'élection québécoise de 2018 | Durand, Claire; Blais, André, 2023, "Sondage panel sur l'élection québécoise de 2018", <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, V1, UNF:6:ECsYwSg8SlYcGPle+8FjWw== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/XDDMMR> |
| 2014 | `qes2014` | Étude électorale québécoise 2014 | Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2014", <https://doi.org/10.5683/SP3/64F7WR>, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/64F7WR> |
| 2012 | `qes2012` | Étude électorale québécoise 2012 | Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, "Étude électorale québécoise 2012", <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/WXUPXT> |
| 2012 | `qes2012_panel` | Sondage panel sur l'élection québécoise de 2012 | Durand, Claire; Goyder, John, 2023, "Sondage panel sur l'élection québécoise de 2012", <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, V1, UNF:6:/ACwE8qVPCB013O9cweCqQ== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/RKHPVL> |
| 2007-2010 | `qes_crop_2007_2010` | Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010 | Durand, Claire, 2023, "Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010", <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, V1, UNF:6:Yaloq+G6EVBknLlAk44JoQ== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IRZ1PF> |
| 2008 | `qes2008` | Étude électorale québécoise 2008 | Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2008", <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/8KEYU3> |
| 2007 | `qes2007` | Étude électorale québécoise 2007 | Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, "Étude électorale québécoise 2007", <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/6XGOKA> |
| 2007 | `qes2007_panel` | Sondage panel sur l'élection québécoise de 2007 | Durand, Claire; Goyder, John, 2023, "Sondage panel sur l'élection québécoise de 2007", <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, V1, UNF:6:ASjoqrxxkLm0vvSA6lc9Fw== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/NDS6VT> |
| 1998 | `qes1998` | Sondages électoraux sur les élections générales québécoises de 1998 | Durand, Claire, 2023, "Sondages électoraux sur les élections générales québécoises de 1998", <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== [fileUNF] | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/QFUAWG> |
