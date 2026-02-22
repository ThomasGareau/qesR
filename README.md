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

## Website

Project website (GitHub Pages): <https://thomasgareau.github.io/qesR/>

The site includes researcher-focused tabs for:

- merged dataset workflow
- study citations
- analysis tabs by topic (including sovereignty attitudes over time)

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

The following studies are used in `qesR` and in the merged dataset workflow.
Each citation includes DOI and repository source.

| Year | Code | Study | Citation | Documentation |
|---|---|---|---|---|
| 2022 | `qes2022` | Quebec Election Study 2022 | Quebec Election Study 2022 (2022). Data set. Harvard Dataverse. <https://doi.org/10.7910/DVN/PAQBDR> | <https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/PAQBDR> |
| 2018 | `qes2018` | Quebec Election Study 2018 | Quebec Election Study 2018 (2018). Data set. Borealis. <https://doi.org/10.5683/SP3/NWTGWS> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/NWTGWS> |
| 2018 | `qes2018_panel` | Quebec Election Study 2018 Panel | Quebec Election Study 2018 Panel (2018). Data set. Borealis. <https://doi.org/10.5683/SP3/XDDMMR> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/XDDMMR> |
| 2014 | `qes2014` | Quebec Election Study 2014 | Quebec Election Study 2014 (2014). Data set. Borealis. <https://doi.org/10.5683/SP3/64F7WR> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/64F7WR> |
| 2012 | `qes2012` | Quebec Election Study 2012 | Quebec Election Study 2012 (2012). Data set. Borealis. <https://doi.org/10.5683/SP2/WXUPXT> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/WXUPXT> |
| 2012 | `qes2012_panel` | Quebec Election Study 2012 Panel | Quebec Election Study 2012 Panel (2012). Data set. Borealis. <https://doi.org/10.5683/SP3/RKHPVL> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/RKHPVL> |
| 2007-2010 | `qes_crop_2007_2010` | CROP Quebec Opinion Polls (2007-2010) | CROP Quebec Opinion Polls (2007-2010) (2007-2010). Data set. Borealis. <https://doi.org/10.5683/SP3/IRZ1PF> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/IRZ1PF> |
| 2008 | `qes2008` | Quebec Election Study 2008 | Quebec Election Study 2008 (2008). Data set. Borealis. <https://doi.org/10.5683/SP2/8KEYU3> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/8KEYU3> |
| 2007 | `qes2007` | Quebec Election Study 2007 | Quebec Election Study 2007 (2007). Data set. Borealis. <https://doi.org/10.5683/SP2/6XGOKA> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/6XGOKA> |
| 2007 | `qes2007_panel` | Quebec Election Study 2007 Panel | Quebec Election Study 2007 Panel (2007). Data set. Borealis. <https://doi.org/10.5683/SP3/NDS6VT> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP3/NDS6VT> |
| 1998 | `qes1998` | Quebec Elections 1998 | Quebec Elections 1998 (1998). Data set. Borealis. <https://doi.org/10.5683/SP2/QFUAWG> | <https://borealisdata.ca/dataset.xhtml?persistentId=doi:10.5683/SP2/QFUAWG> |
