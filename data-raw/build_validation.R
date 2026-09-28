#!/usr/bin/env Rscript

# The validation report: the harmonized studies against the official results
# of Élections Québec and the census margins of Statistics Canada (design.md
# sections 5.10 and 8.3, slice HZ7; R/validation.R).
#
# Usage (from the package root):
#   QESR_CACHE_DIR=<dir> Rscript data-raw/build_validation.R [--check] [--out <file>]
#                                                            [--accept-regressions]
#
# <dir> is a qesR download cache (see ?qes_cache_info) holding the pinned
# original files; a file missing there is downloaded through qesR's own
# client (User-Agent "qesR/<ver> R/<ver>", md5-verified, cached). The package
# is loaded from the source tree with pkgload.
#
# It harmonizes every study the spec covers and writes the report:
#   * inst/extdata/validation/validation_report.csv: the rows of the studies
#     whose metadata ships (CC0). These are the recorded baselines of the
#     gated rows (V-L2 recall and the census margins, weighted): the live
#     test (tests/testthat/test-validation-live.R) fails when a value rises
#     more than 2.0 points above them;
#   * data-raw/nc/validation_qes2022.csv: the rows of qes2022, whose
#     aggregates do not ship (OD3; data-raw/ is build-ignored).
# With --out <file> it also writes the whole report there (the aggregates-only
# artifact of the weekly live job, .github/workflows/live.yml), each row with
# its recorded baseline and gate status. Its qes2022 rows are CC BY-NC 4.0,
# not MIT: the job uploads data-raw/nc/README.md, their licence and
# attribution notice, with the report.
# With --check it writes neither recorded file and fails when the report
# differs from them (the recorded baselines are stale). The weekly job runs it
# for information only: what fails the job is the 2.0-point gate of the live
# test.
#
# How a baseline may move. Before writing, the new report is gated against
# the record it replaces (with the qes2022 rows of data-raw/nc/ when
# present). A gated row (weighted V-L2 recall or weighted census index) that
# rises more than 2.0 points above its recorded value stops the script with
# status 1 and nothing is written, unless --accept-regressions is given: the
# accepted rows are then printed and written anyway. A fall, a smaller rise,
# a change to an ungated row and a new row are recorded without a flag.

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
accept_regressions <- "--accept-regressions" %in% args
out_file <- if ("--out" %in% args) args[[which(args == "--out") + 1L]] else NULL
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (nzchar(cache_dir)) {
  options(qesR.cache_dir = cache_dir, qesR.cache = "disk")
}

report <- .qes_validation_run("all")
cat_ <- .qes_catalog()$studies
shipped <- cat_$study[cat_$metadata_shipped %in% TRUE]
files <- list(
  list(path = file.path("inst", "extdata", "validation", "validation_report.csv"),
       rows = report$study %in% shipped),
  list(path = file.path("data-raw", "nc", "validation_qes2022.csv"),
       rows = report$study == "qes2022")
)
stopifnot(all(report$study %in% c(shipped, "qes2022")))

gate_cols <- c("study", "check", "variable", "weight", "value", "baseline")
if (!check_only) {
  # gate the new report against the record it replaces
  nc_old <- files[[2]]$path
  old_extra <- if (file.exists(nc_old)) .qes_read_csv(nc_old, "validation_report") else NULL
  old_gate <- .qes_validation_gate(report, .qes_validation_recorded(old_extra))
  regressed <- old_gate[old_gate$status %in% "fail", gate_cols]
  if (nrow(regressed)) {
    cat("Gated rows more than 2.0 points above the recorded value:\n")
    print(regressed, row.names = FALSE)
    if (!accept_regressions) {
      cat("Nothing written. Rerun with --accept-regressions to record them as the new baselines.\n")
      quit(status = 1L)
    }
    cat("Accepted (--accept-regressions): these rows are recorded as the new baselines.\n")
  }
}

failed <- FALSE
for (f in files) {
  x <- report[f$rows, , drop = FALSE]
  rownames(x) <- NULL
  if (check_only) {
    tmp <- tempfile(fileext = ".csv")
    .qes_write_csv(x, tmp)
    same <- file.exists(f$path) && identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(f$path)))
    if (!same) {
      failed <- TRUE
      cat(f$path, ": differs from the report on the pinned files\n")
    }
  } else {
    .qes_write_csv(x, f$path)
    cat("wrote", f$path, "(", nrow(x), "rows )\n")
  }
}

if (!is.null(out_file)) {
  nc <- file.path("data-raw", "nc", "validation_qes2022.csv")
  extra <- if (file.exists(nc)) .qes_read_csv(nc, "validation_report") else NULL
  gated <- .qes_validation_gate(report, .qes_validation_recorded(extra))
  utils::write.csv(gated, out_file, row.names = FALSE, fileEncoding = "UTF-8")
  cat("wrote", out_file, "\n")
  bad <- gated[gated$status %in% "fail", gate_cols]
  if (nrow(bad)) {
    print(bad, row.names = FALSE)
  }
}
if (failed) quit(status = 1L)
if (check_only) cat("Validation: the recorded report equals the report on the pinned files.\n")
