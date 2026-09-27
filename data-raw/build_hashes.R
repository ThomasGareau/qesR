#!/usr/bin/env Rscript

# Column hashes of the harmonization engine on the pinned files (design.md
# sections 5.1, 5.10 and 5.11, slice HZ3: V-L1).
#
# Usage (from the package root):
#   QESR_CACHE_DIR=<dir> Rscript data-raw/build_hashes.R [--check]
#
# <dir> is a qesR download cache (see ?qes_cache_info) holding the pinned
# original files; a file missing there is downloaded through qesR's own
# client (User-Agent "qesR/<ver> R/<ver>", md5-verified, cached). The package
# is loaded from the source tree with pkgload, so the output is the engine's
# (R/hz-engine.R) on the same reader as get_qes().
#
# It harmonizes every study the spec covers, every target, every row of the
# spec that is not documentation-only (include_draft = TRUE, so rows still in
# review are covered too), with values = "code" and missing = "reasons", and
#   1. checks V-L1 on marginals: among each wave's members, the count of
#      every level and NA reason of every projectable row equals
#      expected/marginals.csv (CC0 studies) or data-raw/nc/marginals_<study>.csv
#      (a study whose metadata cannot ship, qes2022, OD3; skipped with a note
#      when the build-ignored file is absent);
#   2. writes inst/extdata/harmonize/expected/hashes.csv: for each (study,
#      target) cell, the md5 of the column (each row's level name or number,
#      or "NA:<reason>", in file order; .qes_hz_column_md5()). A hash is not
#      data: it ships for every study, qes2022 included.
# With --check it writes nothing and fails when the file differs.
#
# expected/ is part of the spec: after a change, bump SPEC by the rule of
# section 5.11 (a changed hash is MAJOR) and record the content hash
# (data-raw/spec_check.R --write-hash).

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (nzchar(cache_dir)) {
  options(qesR.cache_dir = cache_dir, qesR.cache = "disk")
}

spec_dir <- normalizePath(file.path("inst", "extdata", "harmonize"), mustWork = TRUE)
spec <- .qes_spec_load(spec_dir)
spec$custom <- FALSE
# the recorded Hash is refreshed afterwards (spec_check.R --write-hash); the
# column hashes do not depend on it
spec$hash_recorded <- spec$hash
cat_ <- .qes_catalog()
shipped <- cat_$studies$study[cat_$studies$metadata_shipped %in% TRUE]
failed <- FALSE

h <- qes_harmonize("all", targets = spec$tables$targets$target, values = "code",
                   missing = "reasons", include_draft = TRUE, spec = spec, quiet = TRUE)

# ---- 1. V-L1 on marginals -------------------------------------------------------------
cell <- qes_provenance(h, level = "cell")
cell <- cell[cell$included & cell$rule %in% c("map", "numeric"), , drop = FALSE]
engine_marginals <- function(study) {
  rows <- cell[cell$study == study, , drop = FALSE]
  out <- lapply(seq_len(nrow(rows)), function(k) {
    t <- rows$target[k]
    in_s <- h$study == study
    member <- vapply(strsplit(ifelse(is.na(h$waves[in_s]), "", h$waves[in_s]), ";", fixed = TRUE),
                     function(w) rows$wave[k] %in% w, logical(1))
    v <- h[[t]][in_s][member]
    v <- if (is.numeric(v)) .qes_code_chr(v) else as.character(v)
    r <- as.character(h[[paste0(t, "__na")]][in_s][member])
    key <- paste(ifelse(is.na(v), "", v), ifelse(is.na(r), "", r), sep = "\x1f")
    n <- table(key)
    parts <- strsplit(names(n), "\x1f", fixed = TRUE)
    data.frame(study = study, wave = rows$wave[k], target = t, source_var = rows$source_var[k],
               value = vapply(parts, function(p) if (nzchar(p[1])) p[1] else NA_character_, character(1)),
               na_reason = vapply(parts, function(p) if (length(p) > 1L && nzchar(p[2])) p[2] else NA_character_, character(1)),
               n = as.integer(n), stringsAsFactors = FALSE)
  })
  if (length(out) > 0L) do.call(rbind, out) else NULL
}
key_of <- function(x) paste(x$study, x$wave, x$target, x$source_var,
                            ifelse(is.na(x$value), "", x$value), ifelse(is.na(x$na_reason), "", x$na_reason), sep = "|")
compare <- function(study, expected, what) {
  got <- engine_marginals(study)
  if (is.null(got)) return(invisible())
  e <- stats::setNames(expected$n, key_of(expected))
  g <- stats::setNames(got$n, key_of(got))
  keys <- union(names(e), names(g))
  bad <- keys[is.na(e[keys]) | is.na(g[keys]) | e[keys] != g[keys]]
  if (length(bad) > 0L) {
    cat(sprintf("V-L1 error (%s): %d marginal cell(s) differ, e.g. %s\n", what, length(bad),
                paste(utils::head(sprintf("%s engine %s expected %s", bad, g[bad], e[bad]), 3L), collapse = "; ")))
    failed <<- TRUE
  } else {
    cat(sprintf("V-L1 marginals %s: %d cells equal.\n", what, length(keys)))
  }
}
for (study in unique(cell$study)) {
  if (study %in% shipped) {
    compare(study, spec$tables$expected[spec$tables$expected$study == study, , drop = FALSE],
            paste(study, "expected/marginals.csv"))
  } else {
    marg <- file.path("data-raw", "nc", sprintf("marginals_%s.csv", study))
    if (!file.exists(marg)) {
      cat(sprintf("V-L1 marginals %s: no %s (build-ignored); skipped.\n", study, marg))
      next
    }
    compare(study, .qes_read_csv(marg, "spec_expected"), paste(study, marg))
  }
}

# ---- 2. expected/hashes.csv ---------------------------------------------------------------
hashes <- .qes_hz_hashes(h)
path <- file.path(spec_dir, "expected", "hashes.csv")
if (check_only) {
  tmp <- tempfile(fileext = ".csv")
  .qes_write_csv(hashes, tmp)
  if (!file.exists(path) || !identical(.qes_read_csv(path), .qes_read_csv(tmp))) {
    cat("Out of date:", path, "\n")
    failed <- TRUE
  }
} else {
  .qes_write_csv(hashes, path)
  cat(sprintf("%s: %d column hashes for %d studies.\n", path, nrow(hashes), length(unique(hashes$study))))
}

if (failed) {
  quit(status = 1L)
}
cat(if (check_only) "Column hashes are current.\n" else "Column hashes written.\n")
