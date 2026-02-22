# qesR

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
install.packages("/path/to/qesR_0.4.2.tar.gz", repos = NULL, type = "source")
```

Install from a local package folder:

```r
install.packages("/path/to/qesR", repos = NULL, type = "source")
```

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
```

## Included Quebec study codes

- `qes2022`
- `qes2018`
- `qes2018_panel`
- `qes2014`
- `qes2012`
- `qes2012_panel`
- `qes_crop_2007_2010`
- `qes2008`
- `qes2007`
- `qes2007_panel`
- `qes1998`

Each code maps to a Dataverse DOI and documentation page.
