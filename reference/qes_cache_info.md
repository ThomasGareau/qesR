# List the files in the download cache

`qes_cache_info()` lists the data files and documents that qesR has kept
in its download cache, with the study each belongs to. It only reads the
cache directory: it makes no network request and creates nothing.

## Usage

``` r
qes_cache_info()
```

## Value

A data frame with one row per cached file: `study`, `file_id`, `md5`,
`bytes`, `retrieved` (modification time), `kind` (always `"file"`: qesR
0.5.0 to 0.7.0 also listed the `qes2022` metadata they built, `"shard"`,
which now ships with the package) and `path`. Attributes `mode` (the
cache mode) and `dir` (the cache directory, `NA` in mode `"none"`). The
data frame has class `c("qes_cache_info", "data.frame")` and prints
compactly: the mode, directory and total size once, then each file with
its size and its path relative to the cache directory; the `path` column
itself is absolute, and
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) gives
every column. A subset of its columns is a plain data frame.

*En français* : une ligne par fichier en cache, de classe
`c("qes_cache_info", "data.frame")`. L'affichage donne une fois le mode,
le dossier et la taille totale, puis chaque fichier avec sa taille et
son chemin relatif au dossier du cache ; la colonne `path` reste
absolue, et
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) donne
toutes les colonnes.

## Where files are kept

qesR keeps each downloaded file under the name `<file_id>-<md5>.<ext>`,
taken from its catalog entry, after checking it against the catalog md5.
Where depends on the option `qesR.cache` (or the environment variable
`QESR_CACHE`):

- `"session"` (default):

  a `qesR` folder in
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html), deleted when R
  exits. Nothing is written outside the session's temporary directory.

- `"disk"`:

  `tools::R_user_dir("qesR", "cache")`, kept between sessions. qesR
  creates it only when you choose this mode, and says so.

- `"none"`:

  nothing is kept: each download goes to a new temporary folder.

Setting `qesR.cache_dir` (or `QESR_CACHE_DIR`) to an existing directory
keeps files there instead, and implies `"disk"` unless `qesR.cache` says
otherwise. qesR then works in a `qesR` subfolder that it creates and
marks, and never touches the other files of that directory.

A file you download yourself in a browser (for example when a server
refuses automated requests; see
[qesR-package](https://thomasgareau.github.io/qesR/reference/qesR-package.md))
can be put in the cache under the name the error message gives: qesR
uses it once its md5 matches the catalog.

## See also

[`qes_cache_clear()`](https://thomasgareau.github.io/qesR/reference/qes_cache_clear.md)
to delete cached files.

Other cache:
[`qes_cache_clear()`](https://thomasgareau.github.io/qesR/reference/qes_cache_clear.md)

## Examples

``` r
info <- qes_cache_info()
attr(info, "mode")
#> [1] "disk"
attr(info, "dir")
#> [1] "/home/runner/.cache/qesR-site/qesR"
```
