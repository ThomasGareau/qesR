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

`get_qes_master()` stacks the 11 studies of qesR 0.4.4 in one data frame
with the 30 harmonized columns of 0.4.4 (same names, order and types). It is
the fixed legacy schema: kept stable for code written for 0.4.4.

```r
library(qesR)

master <- get_qes_master(strict = FALSE)
head(master)
```

The merged data can be saved directly (UTF-8 CSV or RDS, with a
`<stem>_provenance.csv` file recording the file read for each study):

```r
get_qes_master(save_path = "qes_master.csv", strict = FALSE)
get_qes_master(save_path = "qes_master.rds", strict = FALSE)
```

Each column reads the variable qesR 0.4.4 read (`attr(master, "source_map")`)
and converts it as 0.4.4 did. Since qesR 0.5.0 the master changes by
deletion, apart from the reader changes listed in NEWS (for example
`qes2018` `turnout` is 0, not `NA`, for respondents who said they did not
vote):

- every respondent of every file is kept (no de-duplication, no removal of
  empty rows; `qes2007_panel` has its 2,442 respondents);
- the 70 columns 0.4.4 appended by stacking variables that share a name
  across studies are gone (`attr(master, "removed_columns")`);
- values verified to be wrong are `NA`, for example vote intentions in
  `vote_choice`, sovereignty questions other than the referendum on an
  independent country, and `party_best`/`party_lean` everywhere;
  `attr(master, "legacy_na_columns")` lists each column and study with the
  reason, and `attr(master, "legacy_column_map")` says what each column means.

Results from qesR 0.4.4 are reproducible only with that version
(`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`).

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

# codebooks, question text and search work offline (the metadata of every
# CC0 study ships with qesR; qes2022's is built from your copy of its data)
qes2018_codebook <- qes_codebook("qes2018")
# compact layout (default): variable, label, question, n_value_labels, then
# study, position, type, question_lang, value_labels, missing_codes, ...

# long layout: one row per value, with its missing type
qes_codebook("qes2018", layout = "long", variables = c("q26", "q27"))

# the exact wording of a question, in French or English
qes_question("qes2018", "q26", lang = "en")

# find variables across studies, ignoring case and accents
qes_search("souverain|sovereign")

# set "don't know", "refused" and declared missing codes to NA
qes2018_clean <- qes_missing(qes2018)

# soft-deprecated helpers keep working and print a one-time notice
# (see ?qesR-deprecated)
qes2018_codebook <- get_codebook("qes2018")
get_value_labels(qes2018_codebook, "q26")
get_question(qes2018, "q26")

# save the original files (data file and documents), md5-checked,
# in a folder you have created
dir.create("originals")
qes_download("qes2018", path = "originals", what = c("data", "docs"))

# which file the data came from: DOI, version, file id, md5, retrieval
qes_provenance(qes2018)

# preview first 10 rows
head(qes2018, 10)

# cesR-like prepared non-exhaustive dataset
decon <- get_decon("qes2022")
head(decon)

# legacy stacked master dataset across studies (the qesR 0.4.4 columns)
qes_master <- get_qes_master()
head(qes_master)

# which source variable fed each column, and why a column is NA
head(attr(qes_master, "source_map"))
head(attr(qes_master, "legacy_na_columns"))
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
