#!/usr/bin/env Rscript

# Fill a qesR download cache with every pinned data file (and label donor) of
# the catalog, for the live tests (design.md sections 8.2 and 8.4;
# .github/workflows/live.yml).
#
# Usage (from the package root):
#   QESR_CACHE_DIR=<dir> Rscript data-raw/live_fetch.R
#
# A file already in <dir> and matching its pinned md5 is not requested again;
# a missing one is downloaded through qesR's own client: one plain GET with
# the User-Agent "qesR/<ver> R/<ver>" and nothing else, sequential requests at
# least 1 s apart per host, backoff on 429 and 503, md5-verified before it is
# kept. <dir> can then be passed to the tests as QESR_TEST_DATA_DIR.

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (!nzchar(cache_dir)) {
  stop("Set QESR_CACHE_DIR to the cache directory to fill.", call. = FALSE)
}
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
options(qesR.cache_dir = cache_dir, qesR.cache = "disk")

files <- .qes_catalog()$files
files <- files[files$role %in% c("data", "label_donor"), , drop = FALSE]
for (i in seq_len(nrow(files))) {
  row <- files[i, , drop = FALSE]
  server <- .qes_study_row(row$study)$server
  path <- .qes_cache_fetch(row, server, quiet = TRUE)
  cat(sprintf("%-20s %-8s %s\n", row$study, row$file_id, basename(path)))
}
