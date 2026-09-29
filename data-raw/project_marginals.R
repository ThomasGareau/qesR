#!/usr/bin/env Rscript

# Projected marginals of the harmonization spec (design.md sections 5.1, 5.10
# and 5.11, slice HZ2): the unweighted counts of every level and NA reason of
# every projectable crosswalk row (rule map or numeric), computed offline from
# the spec, the shipped dictionary and gates.csv (R/hz-data.R,
# .qes_project_marginals()).
#
# Usage (from the package root; no data file is read):
#   Rscript data-raw/project_marginals.R [--check]
#
# Writes
#   inst/extdata/harmonize/expected/marginals.csv
#       every study with crosswalk rows (all of them ship their metadata;
#       qes2022's counts carry its licence, CC BY-NC 4.0, inst/COPYRIGHTS);
#       V-P1 compares the projection with it in the tests and in CI, and
#       data-raw/spec_check.R diffs it against the merge-base for the MAJOR
#       rule of section 5.11.
# Every projectable row must be projected: a row the dictionary cannot
# project needs cells in gates.csv (run data-raw/build_sources.R first).
# With --check it writes nothing and fails when a file differs.
#
# expected/ is part of the spec: after a change, bump SPEC by the rule of
# section 5.11 and record the hash (data-raw/spec_check.R --write-hash).

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

spec_dir <- normalizePath(file.path("inst", "extdata", "harmonize"), mustWork = TRUE)
spec <- .qes_spec_load(spec_dir)
cat_ <- .qes_catalog()
shipped <- cat_$studies$study[cat_$studies$metadata_shipped %in% TRUE]
failed <- FALSE

write_or_check <- function(x, path) {
  if (check_only) {
    new_path <- tempfile(fileext = ".csv")
    .qes_write_csv(x, new_path)
    same <- file.exists(path) && identical(.qes_read_csv(path), .qes_read_csv(new_path))
    if (!same) {
      cat("Out of date:", path, "\n")
      failed <<- TRUE
    }
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    .qes_write_csv(x, path)
  }
}

report <- function(proj, what) {
  un <- attr(proj, "unprojected")
  if (!is.null(un)) {
    cat(sprintf("%s: %d row(s) cannot be projected:\n", what, nrow(un)))
    print(un, right = FALSE)
    failed <<- TRUE
  }
  cat(sprintf("%s: %d marginal cells for %d rows.\n", what, nrow(proj),
              nrow(unique(proj[, c("study", "wave", "target")]))))
  attr(proj, "unprojected") <- NULL
  proj
}

unshipped <- setdiff(unique(spec$tables$crosswalk$study), shipped)
if (length(unshipped) > 0L) {
  stop(sprintf("No shipped metadata for %s.", paste(unshipped, collapse = ", ")), call. = FALSE)
}
studies <- intersect(unique(spec$tables$crosswalk$study), shipped)
proj <- report(.qes_project_marginals(spec, .qes_hz_sources_shipped(spec), studies), "expected/marginals.csv")
write_or_check(proj, file.path(spec_dir, "expected", "marginals.csv"))

if (failed) {
  quit(status = 1L)
}
cat(if (check_only) "The projected marginals are current.\n" else "Projected marginals written.\n")
