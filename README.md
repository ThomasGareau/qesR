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

Harmonized fields include demographics (age, gender, education, income), vote choice, party identification, sovereignty attitudes, leader thermometers, and more. Use `colnames(master)` to see the full list.

Source-variable provenance is available via `attr(master, "source_map")` and old-to-new variable name mappings via `attr(master, "variable_name_map")`.

Deduplication in `get_qes_master()` is applied **within the same survey code**
only (e.g., duplicate IDs inside one file). Respondents are not removed across
panel vs. non-panel studies.

## Website

Project website (GitHub Pages): <https://thomasgareau.github.io/qesR/>

The site includes merged dataset workflow documentation, study citations, and use-case analysis examples. French documentation is accessible via the `FR` toggle in the navigation bar.

## Usage

```r
library(qesR)

# list the studies: codes, titles, authors, DOI, licence, pinned version
qes_studies()

# their codebooks, questionnaires and reports
qes_docs("qes2018")

# load one study: get_qes() returns the data; assign it yourself
qes2018 <- get_qes("qes2018")

# qes2018 is returned with variable/value labels (when available)
# and a codebook is attached:
cb <- attr(qes2018, "qes_codebook")
head(cb)

# you can also fetch the codebook directly
qes2018_codebook <- qes_codebook("qes2018")
# compact layout (default): variable, label, question, n_value_labels

# wide layout: adds list-column with value labels
qes2018_codebook_wide <- qes_codebook("qes2018", layout = "wide")

# long layout: one row per value label
qes2018_codebook_long <- qes_codebook("qes2018", layout = "long")

# reformat an existing codebook object
format_codebook(qes2018_codebook, layout = "wide")
get_value_labels(qes2018_codebook, long = TRUE)

# soft-deprecated aliases of qes_codebook(): they keep working and print a
# one-time notice (see ?qesR-deprecated)
qes2018_codebook <- get_codebook("qes2018")
qes2018_codebook <- get_qes_codebook("qes2018")

# download the documents (PDFs, questionnaires)
download_codebook("qes2018", dest_dir = tempdir())

# preview first 10 rows
head(qes2018, 10)

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

### Workspace, messages and errors

- Functions return their result and write nothing into your workspace by
  default. `assign_global = TRUE` still assigns, into the environment you call
  the function from.
- Messages, warnings and errors are available in English and French:
  `options(qesR.lang = "fr")` (or the `QESR_LANG` environment variable). The
  data returned never depends on the language.
- Errors have classes such as `qesR_error_unknown_study` or
  `qesR_error_network`, so scripts can handle them with `tryCatch()`. See
  `?qesR` for the list.
- Downloads use plain HTTPS requests with certificate checks, at most one per
  second per server; qesR never retries without TLS verification. A request
  that fails for a passing reason (timeout, HTTP 429 or 503, ...) is retried a
  few times with growing pauses, honouring the server's `Retry-After`.

Les messages, avertissements et erreurs sont aussi offerts en français
(`options(qesR.lang = "fr")`). Les fonctions renvoient leurs résultats sans
rien écrire dans votre espace de travail, sauf avec `assign_global = TRUE`.
Les requêtes qui échouent pour une raison passagère sont reprises quelques
fois, avec des pauses croissantes, en respectant le `Retry-After` du serveur.

## Citing the studies

`qes_cite()` writes the citation of qesR and of each dataset from the catalog
that ships with the package (authors, year, deposit title, DOI, repository,
pinned version and UNF):

```r
qes_cite()                          # qesR
qes_cite(c("qes2018", "qes2022"))   # qesR and two datasets
qes_cite("qes2018", style = "bibtex")
```

The full table is in `vignette("study-citations", package = "qesR")`. The 2022
study is licensed CC BY-NC 4.0; the others are CC0.
