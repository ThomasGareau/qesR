# Delete files from the download cache

`qes_cache_clear()` deletes cached files: all of them, those of some
studies, or those older than a given age. It also forgets the data and
metadata kept in memory for this session. It deletes files only in a
directory that qesR created and marked as its cache (it holds a
`.qesR-cache` file) and refuses any other directory.

## Usage

``` r
qes_cache_clear(studies = NULL, older_than = NULL)
```

## Arguments

- studies:

  Optional character vector of study codes (see
  [`qes_studies()`](https://thomasgareau.github.io/qesR/reference/qes_studies.md)).
  `NULL` (default) clears every study.

- older_than:

  Optional age: a number of days, or a
  [difftime](https://rdrr.io/r/base/difftime.html). Only files retrieved
  longer ago than this are deleted.

## Value

The paths of the deleted files, invisibly.

## See also

[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md),
which also describes where files are kept.

Other cache:
[`qes_cache_info()`](https://thomasgareau.github.io/qesR/reference/qes_cache_info.md)

## Examples

``` r
# these examples work on the session cache only, so that running them
# never deletes a disk cache you have opted in to
op <- options(qesR.cache = "session")

# delete files kept more than 30 days
qes_cache_clear(older_than = 30)

# delete everything in the cache
qes_cache_clear()

options(op)
```
