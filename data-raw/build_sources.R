#!/usr/bin/env Rscript

# Aggregates of the pinned data files for the offline checks of the
# harmonization spec (design.md sections 5.1 and 5.10, slice HZ2).
#
# Usage (from the package root):
#   QESR_CACHE_DIR=<dir> Rscript data-raw/build_sources.R [--check]
#
# <dir> is a qesR download cache (see ?qes_cache_info) holding the pinned
# original files; a file missing there is downloaded through qesR's own
# client (User-Agent "qesR/<ver> R/<ver>", md5-verified, cached). The package
# is loaded from the source tree with pkgload, so the counts come from the
# same reader as get_qes() (.qes_read()) and the same code as the checks
# (R/hz-data.R).
#
# For every study with crosswalk rows it
#   1. reads the pinned file and runs the data checks V-D1 to V-D5, V-D7 and
#      V-D8 on it (the live form of the checks); any error stops the build;
#   2. for a study whose metadata ships (catalog metadata_shipped = TRUE):
#      keeps, for inst/extdata/harmonize/gates.csv, the joint counts of gate
#      code and source code among the wave's members of every projectable
#      row (rule map or numeric). The dictionary's per-code counts are not
#      used in their place: it lists every observed code only for numeric
#      columns with at most 50 codes. Then checks that the projection from
#      the dictionary and gates.csv equals the projection from the file for
#      every projectable row (V-P1 is exact);
#   3. for a study whose metadata cannot ship (qes2022, OD3): writes to the
#      build-ignored data-raw/nc/ the same aggregates with label hashes in
#      place of labels, for CI:
#        sources_<study>_variables.csv  variable, type, na_values (every
#                                       variable of the file);
#        sources_<study>_values.csv     per-code counts of the variables the
#                                       spec names, md5 of each normalized
#                                       label, and the label read as a number
#                                       (for rows with from_label = TRUE);
#        gates_<study>.csv              the cells of every projectable row.
# With --check it writes nothing and fails when a file differs from what it
# would write.
#
# gates.csv is part of the spec: after a change, run
# data-raw/project_marginals.R, bump SPEC and record the hash
# (data-raw/spec_check.R --write-hash).

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (nzchar(cache_dir)) {
  options(qesR.cache_dir = cache_dir, qesR.cache = "disk")
}

spec_dir <- normalizePath(file.path("inst", "extdata", "harmonize"), mustWork = TRUE)
nc_dir <- file.path("data-raw", "nc")
spec <- .qes_spec_load(spec_dir)
cat_ <- .qes_catalog()
shipped <- cat_$studies$study[cat_$studies$metadata_shipped %in% TRUE]
studies <- unique(c(spec$tables$crosswalk$study, spec$tables$waves$study))
failed <- FALSE
stale <- character(0)

# the dictionary sources, without gates.csv (the offline projection adds the
# cells kept here)
dict_sources <- .qes_hz_sources_shipped(spec)
dict_sources$gates <- dict_sources$gates[0, , drop = FALSE]

compare_or_write <- function(x, path) {
  if (check_only) {
    old <- if (file.exists(path)) .qes_read_csv(path) else NULL
    new_path <- tempfile(fileext = ".csv")
    .qes_write_csv(x, new_path)
    new <- .qes_read_csv(new_path)
    if (!identical(old, new)) {
      stale <<- c(stale, path)
    }
  } else {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    .qes_write_csv(x, path)
  }
}

gates_out <- list()
for (study in studies) {
  data <- .qes_read(study)
  frames <- stats::setNames(list(data), study)
  problems <- .qes_data_check_frames(spec, frames)
  errors <- problems[problems$severity == "error", , drop = FALSE]
  cat(sprintf("%-15s %5d rows: %d data-check error(s)\n", study, nrow(data), nrow(errors)))
  if (nrow(errors) > 0L) {
    print(errors[, c("rule", "key", "detail")], right = FALSE)
    failed <- TRUE
  }
  data_sources <- .qes_hz_sources_data(spec, frames)

  if (study %in% shipped) {
    # the cells of every projectable row (.qes_hz_cells_data() counts only those)
    gates_out[[study]] <- data_sources$gates
    # V-P1 is exact: the dictionary with these cells projects what the file gives
    offline <- dict_sources
    offline$gates <- gates_out[[study]]
    p_off <- .qes_project_marginals(spec, offline, study)
    p_data <- .qes_project_marginals(spec, data_sources, study)
    if (!is.null(attr(p_off, "unprojected")) || !identical(
      p_off[, names(p_off)], p_data[, names(p_data)]
    )) {
      cat(sprintf("  the offline projection of %s differs from the file's\n", study))
      print(attr(p_off, "unprojected"))
      failed <- TRUE
    }
  } else {
    vars <- data_sources$variables
    vals <- data_sources$values
    compare_or_write(vars[, c("study", "variable", "type", "na_values")],
                     file.path(nc_dir, sprintf("sources_%s_variables.csv", study)))
    compare_or_write(vals[, c("study", "variable", "value", "label_hash", "label_number", "missing_type", "n")],
                     file.path(nc_dir, sprintf("sources_%s_values.csv", study)))
    compare_or_write(data_sources$gates, file.path(nc_dir, sprintf("gates_%s.csv", study)))
  }
}

gates <- if (length(gates_out) > 0L) do.call(rbind, unname(gates_out)) else dict_sources$gates
gates <- .qes_hz_sort_gates(gates)
compare_or_write(gates, file.path(spec_dir, "gates.csv"))
cat(sprintf("gates.csv: %d cells for %d rows.\n", nrow(gates),
            nrow(unique(gates[, c("study", "wave", "source_var", "gate_var")]))))

if (length(stale) > 0L) {
  cat("Out of date:", paste(stale, collapse = ", "), "\n")
  failed <- TRUE
}
if (failed) {
  quit(status = 1L)
}
cat(if (check_only) "The source aggregates match the pinned files.\n" else "Source aggregates written.\n")
