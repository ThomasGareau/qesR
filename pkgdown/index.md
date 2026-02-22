---
title: qesR
---

`qesR` provides a single workflow to download, document, and harmonize Quebec Election Study datasets from Dataverse.

## Install

```r
if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
remotes::install_github("ThomasGareau/qesR")
```

## Core Workflows

### 1. Pull a study with labels and codebook

```r
library(qesR)

qes2022 <- get_qes("qes2022")
cb <- get_codebook("qes2022", layout = "compact")
head(cb)
```

### 2. Build a harmonized master dataset

```r
master <- get_qes_master(
  surveys = c("qes2022", "qes2018", "qes2014", "qes2007", "qes1998"),
  strict = FALSE
)

head(master)
head(attr(master, "source_map"))
```

## Analysis Examples

Use the tutorials in the website navbar:

- `Getting Started`: installation and first queries
- `Harmonized Master Analysis`: cross-year examples (age, turnout, vote)
- `Codebook Exploration`: variable labels and value labels workflows

## Reproducibility

The package includes command-line scripts:

- `scripts/build_qes_master.R`: export harmonized master data
- `scripts/build_pkgdown_site.R`: build website locally

Deployment to GitHub Pages is handled by `.github/workflows/pkgdown.yml`.
