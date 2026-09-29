# Download the original files of a study

`qes_download()` saves the original files of one or more studies in a
directory of your choice: the data file as its authors deposited it
(SPSS or Stata), and, with `what = "docs"`, the codebooks,
questionnaires and reports. Every file is checked against the md5
checksum recorded in the qesR catalog before it gets its final name. Use
it to keep a copy of the exact files behind an analysis; to load a study
into R, use
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md).
For example,
`qes_download("qes2018", path = dir, what = c("data", "docs"), lang = "fr")`
saves the 2018 data file and its French documents into `dir`, a folder
you have created.

## Usage

``` r
qes_download(
  studies,
  path,
  what = c("data", "docs"),
  role = NULL,
  version = c("pinned", "latest"),
  overwrite = FALSE,
  lang = NULL,
  quiet = FALSE
)
```

## Arguments

- studies:

  A character vector of study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)),
  or `"all"`. Codes are trimmed and case-insensitive. `"qes_demo"`, the
  synthetic study shipped with qesR, copies its data file with no
  download.

- path:

  An existing directory to save the files in. There is no default.

- what:

  `"data"` (default) for the data file, `"docs"` for the documents, or
  `c("data", "docs")` for both.

- role:

  Optional character vector of file roles. With `role = NULL`, `"data"`
  means each study's pinned data file (the one
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  reads) and `"docs"` every document. Naming roles keeps only files of
  those roles: `"codebook"`, `"questionnaire"`, `"technical_report"` and
  `"methodology"` for documents; `"data"` for every data file of the
  study, including a copy of the same data in another format, and
  `"label_donor"` for the file whose labels
  [`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
  uses for `qes2012`.

- version:

  `"pinned"` (default) or `"latest"` (see *Pinned and latest versions*).

- overwrite:

  If `TRUE`, replace files of the same name whose content differs.
  Default `FALSE`.

- lang:

  Optional character vector of document languages to keep (`"en"`,
  `"fr"`). `NULL` keeps every language. Data files have no language and
  are not filtered.

- quiet:

  If `TRUE`, do not print progress messages.

## Value

Invisibly, a data frame with one row per file: `study`, `file_id`,
`file_name` (as deposited), `role`, `lang`, `md5` (the checksum the file
was checked against), `local_path`, `from_cache` (`TRUE` when the bytes
came from the download cache rather than the network), `downloaded`
(`FALSE` for a file already in `path`) and `pinned`. Its attribute
`qes_provenance` records the study-level provenance of each file (see
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)).
With no matching file, a data frame with no rows (and an empty
provenance record).

## What is written

Apart from the download cache (see
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md);
by default in [`tempdir()`](https://rdrr.io/r/base/tempfile.html)),
nothing is written outside `path`, and nothing at all unless at least
one file matches the request. `path` must already exist:
`qes_download()` never creates a directory. Each file keeps its
deposited name (with its extension added when the deposit omits it).
When the locale cannot write that name (an accented name in the C
locale, for example), the file is named by its Dataverse file id
instead, with the same extension.

Before writing anything, `qes_download()` looks at what is already in
`path`. A file that is already there with the expected md5 is kept and
not downloaded again (`downloaded = FALSE`). A file of the same name
with other content is an error of class `qesR_error_input`, and then
nothing is written, unless `overwrite = TRUE`.

Each file is fetched through the download cache (see
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md)),
so a file already downloaded in the session (for example by
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md))
is not requested again. It is then written into `path` under a temporary
`.part` name, checked against its md5, and only then renamed: a damaged
or partial copy never gets the final name. A file that fails the check
is an error of class `qesR_error_checksum`. When several files are
requested and one fails, the files completed before it stay in `path`.

## Pinned and latest versions

By default (`version = "pinned"`), the files are those of the dataset
version pinned by the qesR catalog, the files
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
reads. `version = "latest"` asks Dataverse (one metadata request per
deposit) for its latest published version and downloads the matching
files, each checked against the md5 Dataverse gives. This is the only
way qesR reaches files it has not pinned: it raises a warning of class
`qesR_warning_unpinned`, and the result records `pinned = FALSE`.
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
keeps reading the pinned files. A file that is not in the latest
version, or a deaccessioned deposit, is an error of class
`qesR_error_source`, and then nothing is written.

## En français

`qes_download()` enregistre les fichiers originaux d'une ou de plusieurs
études dans un dossier de votre choix : le fichier de données tel que
déposé (SPSS ou Stata) et, avec `what = "docs"`, les livres de codes,
questionnaires et rapports. Chaque fichier est vérifié par sa somme md5
avant de recevoir son nom définitif. Hormis le cache de téléchargement
(voir
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md)
; par défaut dans [`tempdir()`](https://rdrr.io/r/base/tempfile.html)),
rien n'est écrit hors de `path`, qui doit déjà exister, et rien du tout
si aucun fichier ne correspond à la demande. Chaque fichier garde son
nom de dépôt (avec son extension si le dépôt l'omet), ou prend son
identifiant Dataverse, avec la même extension, si la locale ne peut pas
écrire ce nom (un nom accentué dans la locale C, par exemple). Un
fichier déjà présent avec la bonne somme md5 est conservé ; un fichier
du même nom au contenu différent est une erreur (rien n'est écrit), sauf
avec `overwrite = TRUE`. Si plusieurs fichiers sont demandés et que l'un
échoue, les fichiers déjà terminés restent dans `path`. Avec
`role = NULL`, `"data"` désigne le fichier de données retenu par qesR
(celui que lit
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md))
et `"docs"` tous les documents ; nommer des rôles retient tous les
fichiers de ces rôles (`role = "data"` inclut aussi une copie des mêmes
données dans un autre format, et `"label_donor"` le fichier dont
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
tire les étiquettes de `qes2012`). `version = "latest"` télécharge la
dernière version publiée du dépôt, vérifiée par la somme md5 que donne
Dataverse, avec un avertissement de classe `qesR_warning_unpinned` ; le
résultat indique alors `pinned = FALSE`.

## See also

[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md)
to list the documents,
[`get_qes()`](https://thomasgareau.github.io/qesR/reference/get_qes.md)
to load a study,
[`qes_provenance()`](https://thomasgareau.github.io/qesR/reference/qes_provenance.md)
and
[`qes_cite()`](https://thomasgareau.github.io/qesR/reference/qes_cite.md)
to record and cite the files.

Other studies and documents:
[`qes_docs()`](https://thomasgareau.github.io/qesR/reference/qes_docs.md),
[`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)

## Examples

``` r
# the synthetic demonstration study ships with qesR: no download
dir <- file.path(tempdir(), "qes-files")
dir.create(dir)
files <- qes_download("qes_demo", path = dir)
#> 1 file(s) saved in '/tmp/Rtmps5g1bn/qes-files', 0 already there.
files[, c("study", "file_name", "md5", "downloaded")]
#>      study    file_name                              md5 downloaded
#> 1 qes_demo qes_demo.sav e956e315800690cb0894c86ed85c8bea       TRUE
qes_provenance(files)
#> qes_demo: file 0 (qes_demo.sav), synthetic data shipped with qesR. md5
#> e956e315800690cb0894c86ed85c8bea, verified. 60 rows, 11 columns. Retrieved on
#> 2026-09-29 16:20:17 UTC (local_demo). Licence: CC0 1.0. qesR catalog 2.3.0.
#> 
#> as.data.frame() gives every column.
unlink(dir, recursive = TRUE)
```
