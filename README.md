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

- Study metadata: `qes_code`, `qes_year`, `qes_name_en`
- Respondent and interview fields: `respondent_id`, `interview_start`, `interview_end`, `interview_recorded`
- Core demographics: `year_of_birth`, `age`, `age_group`, `gender`, `education`, `language`, `province_territory`
- Political behavior and attitudes: `turnout`, `vote_choice`, `vote_choice_text`, `party_best`, `party_lean`, `ideology`, `political_interest`
- Additional cross-study fields where available: `citizenship`, `born_canada`, `income`, `religion`, `federal_pid`, `provincial_pid`, `survey_weight`

Harmonization rules include:

- text normalization for common response fields (so values are comparable across studies)
- derived `age_group` for respondents with valid age/year-of-birth information
- source-variable tracking via `attr(master, "source_map")`

Deduplication in `get_qes_master()` is applied **within the same survey code**
only (e.g., duplicate IDs inside one file). Respondents are not removed across
panel vs. non-panel studies.

## Website

Project website (GitHub Pages): <https://thomasgareau.github.io/qesR/>

The site includes researcher-focused tabs for:

- merged dataset workflow
- study citations
- 2 analysis tabs (sovereignty + descriptive statistics)
- full French pages for all core sections

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

## Command-Line Scripts

Build the master dataset from the terminal (writes CSV, RDS, and source map):

```bash
Rscript scripts/build_qes_master.R \
  --out-dir . \
  --out-prefix qes_master
```

Build for a subset of studies:

```bash
Rscript scripts/build_qes_master.R \
  --surveys qes2022,qes2018,qes1998 \
  --out-dir . \
  --out-prefix qes_master_subset
```

Build the website locally:

```bash
Rscript scripts/build_pkgdown_site.R
```

## Study Citations

The citations below follow the exact repository citation strings:

1. Mahéo, Valérie-Anne; Bélanger, Éric; Stephenson, Laura B; Harell, Allison, 2023, "2022 Quebec Election Study", <https://doi.org/10.7910/DVN/PAQBDR>, Harvard Dataverse, V1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw== [fileUNF]
2. Bélanger, Éric; Nadeau, Richard; Mahéo, Valérie-Anne; Daoust, Jean-François, 2023, "Étude électorale québécoise 2018", <https://doi.org/10.5683/SP3/NWTGWS>, Borealis, V1, UNF:6:luhys2QSLNTONPOXO4LYpg== [fileUNF]
3. Durand, Claire; Blais, André, 2023, "Sondage panel sur l'élection québécoise de 2018", <https://doi.org/10.5683/SP3/XDDMMR>, Borealis, V1, UNF:6:ECsYwSg8SlYcGPle+8FjWw== [fileUNF]
4. Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2014", <https://doi.org/10.5683/SP3/64F7WR>, Borealis, V1, UNF:6:OoiAJ3ShbycsxmWCefqrjw== [fileUNF]
5. Bélanger, Éric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, "Étude électorale québécoise 2012", <https://doi.org/10.5683/SP2/WXUPXT>, Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ== [fileUNF]
6. Durand, Claire; Goyder, John, 2023, "Sondage panel sur l'élection québécoise de 2012", <https://doi.org/10.5683/SP3/RKHPVL>, Borealis, V1, UNF:6:/ACwE8qVPCB013O9cweCqQ== [fileUNF]
7. Durand, Claire, 2023, "Sondages CROP sur les intentions de vote provinciales québécoises 2007-2010", <https://doi.org/10.5683/SP3/IRZ1PF>, Borealis, V1, UNF:6:Yaloq+G6EVBknLlAk44JoQ== [fileUNF]
8. Bélanger, Éric; Nadeau, Richard, 2023, "Étude électorale québécoise 2008", <https://doi.org/10.5683/SP2/8KEYU3>, Borealis, V1, UNF:6:6wfopjsb0foTuDDWQPDfXg== [fileUNF]
9. Bélanger, Éric; Nadeau, Richard; Crête, Jean; Stephenson, Laura; Tanguay, Brian, 2023, "Étude électorale québécoise 2007", <https://doi.org/10.5683/SP2/6XGOKA>, Borealis, V1, UNF:6:fNjQ+LF7dCVuIrjEyQuOyg== [fileUNF]
10. Durand, Claire; Goyder, John, 2023, "Sondage panel sur l'élection québécoise de 2007", <https://doi.org/10.5683/SP3/NDS6VT>, Borealis, V1, UNF:6:ASjoqrxxkLm0vvSA6lc9Fw== [fileUNF]
11. Durand, Claire, 2023, "Sondages électoraux sur les élections générales québécoises de 1998", <https://doi.org/10.5683/SP2/QFUAWG>, Borealis, V1, UNF:6:zeXNn+A0b1j0DtgUq2cYjg== [fileUNF]
