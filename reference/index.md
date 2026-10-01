# Package index

## Package overview

What qesR does, its options and its conditions, in English and in French
(`?qesR-fr`), with a table of every function.

- [`qesR`](https://thomasgareau.github.io/qesR/reference/qesR-package.md)
  [`qesR-package`](https://thomasgareau.github.io/qesR/reference/qesR-package.md)
  : qesR: Access Quebec Election Study Datasets
- [`qesR-fr`](https://thomasgareau.github.io/qesR/reference/qesR-fr.md)
  : qesR en français

## Data

Load a study from its original data file, or stack 11 studies in the
merged file. Data are returned, never written into your workspace unless
you ask.

- [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  : Download and Load a Quebec Election Study
- [`get_qes_master()`](https://thomasgareau.github.io/qesR/reference/get_qes_master.md)
  : Build the Merged QES File

## Studies and documents

The offline catalog of the studies qesR can load, their codebooks,
questionnaires and reports, and the original files themselves. No
network request unless you ask for one.

- [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)
  : List the studies qesR can load
- [`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
  : List the documents of each study
- [`qes_download()`](https://thomasgareau.github.io/qesR/reference/qes_download.md)
  : Download the original files of a study

## Codebooks and search

What each variable measures, the exact question asked, in English and
French, and which codes mean “don’t know” or “refused”. Offline for
every study.

- [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md)
  : The codebook of a study
- [`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md)
  : The exact wording of survey questions
- [`qes_search()`](https://thomasgareau.github.io/qesR/reference/qes_search.md)
  : Search variables across studies, in English and French
- [`qes_missing()`](https://thomasgareau.github.io/qesR/reference/qes_missing.md)
  : Set "don't know", "refused" and other missing codes to NA

## Harmonization

One data frame across the 11 studies:
[`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
shows which studies have which harmonized variable and how comparable
each study’s question is;
[`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
applies the rules, with a reason for every missing value and each
respondent’s waves and weights;
[`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
turns the result into a survey design. See [How harmonization
works](https://thomasgareau.github.io/qesR/articles/harmonization.md).

- [`qes_spec()`](https://thomasgareau.github.io/qesR/reference/qes_spec.md)
  : The harmonization spec (experimental)
- [`qes_harmonize()`](https://thomasgareau.github.io/qesR/reference/qes_harmonize.md)
  : Harmonize variables across studies (experimental)
- [`qes_design()`](https://thomasgareau.github.io/qesR/reference/qes_design.md)
  : Harmonized data as a survey design (experimental)
- [`qes_party_lineage()`](https://thomasgareau.github.io/qesR/reference/qes_party_lineage.md)
  : Join the parties of one lineage (ADQ and CAQ) in harmonized data

## Reproducibility

Which file a result was read from, and how to cite qesR and each
dataset.

- [`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
  : Where a study's data came from
- [`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
  : Cite qesR and the studies it loads

## Cache

Where downloaded files are kept, and how to list or delete them.

- [`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md)
  : List the files in the download cache
- [`qes_cache_clear()`](https://thomasgareau.github.io/qesR/reference/qes_cache_clear.md)
  : Delete files from the download cache

## Older function names

Earlier names of qesR functions. They keep working, with the same
arguments, and will not be removed; each prints a one-time notice naming
its replacement. [The upgrading
guide](https://thomasgareau.github.io/qesR/articles/migrating-0.7.md)
says what changed in their results.

| Older name | Replacement |
|----|----|
| [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md), [`get_qes_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md) | [`qes_codebook()`](https://thomasgareau.github.io/qesR/reference/qes_codebook.md) |
| [`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md) | `qes_codebook(codebook, layout = )` |
| [`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md) | `qes_codebook(layout = "long")` |
| [`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md) | [`qes_question()`](https://thomasgareau.github.io/qesR/reference/qes_question.md) |
| [`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md), [`get_qes_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md) | [`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md) |
| [`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md) | `qes_download(what = "docs")` |
| [`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md) | `head(get_qes())` |
| [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md) | [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md) |
| [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md) | `qes_harmonize(srvy, targets = "decon")` |

- [`qesR-deprecated`](https://thomasgareau.github.io/qesR/reference/qesR-deprecated.md)
  : Soft-deprecated qesR functions
- [`get_qescodes()`](https://thomasgareau.github.io/qesR/reference/get_qescodes.md)
  : List Quebec Election Study Survey Codes
- [`get_preview()`](https://thomasgareau.github.io/qesR/reference/get_preview.md)
  : Preview a Quebec Election Study
- [`get_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md)
  [`get_qes_codebook()`](https://thomasgareau.github.io/qesR/reference/get_codebook.md)
  : Get a Quebec Election Study codebook (older name)
- [`format_codebook()`](https://thomasgareau.github.io/qesR/reference/format_codebook.md)
  : Reformat a qesR codebook (older name)
- [`get_value_labels()`](https://thomasgareau.github.io/qesR/reference/get_value_labels.md)
  : Get value labels from a codebook (older name)
- [`get_question()`](https://thomasgareau.github.io/qesR/reference/get_question.md)
  : Get survey question text (older name)
- [`get_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md)
  [`get_qes_codebook_files()`](https://thomasgareau.github.io/qesR/reference/get_codebook_files.md)
  : Get codebook files (older name)
- [`download_codebook()`](https://thomasgareau.github.io/qesR/reference/download_codebook.md)
  : Download codebook files (older name)
- [`get_decon()`](https://thomasgareau.github.io/qesR/reference/get_decon.md)
  : Create a Prepared Non-Exhaustive qesR Dataset
