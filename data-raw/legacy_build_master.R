#!/usr/bin/env Rscript

# Build the legacy stacked master (get_qes_master()) and save it to files, for
# local comparisons (data-raw/compare_legacy.R) and for the website's analysis
# articles when they are built from a saved copy. Moved here from
# scripts/build_qes_master.R in 0.5.0 (design.md section 10).
#
# The output is never committed: the repository ignores /qes_master*.csv and
# /qes_master*.rds, and nothing in the package or its vignettes reads these
# files.
#
# Usage:
#   Rscript data-raw/legacy_build_master.R --out-dir <dir>
#   Rscript data-raw/legacy_build_master.R --out-dir <dir> --surveys qes2022,qes2018
#   Rscript data-raw/legacy_build_master.R --out-dir <dir> --strict --out-prefix qes_master_all
#
# --out-dir has no default: the script never writes into the working
# directory unless you name it. Each run writes <prefix>.csv (UTF-8),
# <prefix>.rds and, next to each, <prefix>_provenance.csv (the file read for
# each study), plus <prefix>_source_map.csv (the frozen source of each column).

usage <- paste(
  "Usage:",
  "  Rscript data-raw/legacy_build_master.R --out-dir PATH [options]",
  "",
  "Options:",
  "  --out-dir PATH          Output directory (required; created if needed)",
  "  --surveys CODE1,CODE2   Optional subset of study codes (default: the 11 of 0.4.4)",
  "  --out-prefix NAME       Output file prefix (default: qes_master)",
  "  --strict                Fail if any study fails to load",
  "  --quiet                 Reduce logging",
  "  --help, -h              Show this help",
  sep = "\n"
)

parse_args <- function(args) {
  opts <- list(surveys = NULL, out_dir = NULL, out_prefix = "qes_master", strict = FALSE, quiet = FALSE)
  value <- function(i) {
    if (i > length(args)) stop("Missing value for ", args[[i - 1L]], call. = FALSE)
    args[[i]]
  }
  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]
    if (identical(a, "--surveys")) {
      i <- i + 1L
      opts$surveys <- trimws(strsplit(value(i), ",", fixed = TRUE)[[1]])
      opts$surveys <- opts$surveys[nzchar(opts$surveys)]
    } else if (identical(a, "--out-dir")) {
      i <- i + 1L
      opts$out_dir <- value(i)
    } else if (identical(a, "--out-prefix")) {
      i <- i + 1L
      opts$out_prefix <- value(i)
    } else if (identical(a, "--strict")) {
      opts$strict <- TRUE
    } else if (identical(a, "--quiet")) {
      opts$quiet <- TRUE
    } else if (a %in% c("--help", "-h")) {
      cat(usage, "\n")
      quit(save = "no", status = 0)
    } else {
      stop("Unknown argument: ", a, call. = FALSE)
    }
    i <- i + 1L
  }
  if (is.null(opts$out_dir)) {
    stop("--out-dir is required.\n\n", usage, call. = FALSE)
  }
  opts
}

# Load qesR from this repository's source when run as a script from it, else
# the installed package.
load_qesR <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  repo_dir <- if (length(file_arg)) {
    normalizePath(file.path(dirname(sub("^--file=", "", file_arg[[1]])), ".."), mustWork = FALSE)
  }
  if (!is.null(repo_dir) && file.exists(file.path(repo_dir, "DESCRIPTION")) &&
      requireNamespace("pkgload", quietly = TRUE)) {
    pkgload::load_all(repo_dir, quiet = TRUE)
  } else {
    suppressPackageStartupMessages(library(qesR))
  }
  invisible(TRUE)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  load_qesR()
  dir.create(opts$out_dir, recursive = TRUE, showWarnings = FALSE)

  out_csv <- file.path(opts$out_dir, paste0(opts$out_prefix, ".csv"))
  out_rds <- file.path(opts$out_dir, paste0(opts$out_prefix, ".rds"))
  out_map <- file.path(opts$out_dir, paste0(opts$out_prefix, "_source_map.csv"))

  master <- get_qes_master(
    surveys = opts$surveys,
    assign_global = FALSE,
    quiet = opts$quiet,
    strict = opts$strict,
    save_path = out_csv
  )
  saveRDS(master, out_rds)
  src_map <- attr(master, "source_map", exact = TRUE)
  if (is.data.frame(src_map) && nrow(src_map) > 0L) {
    utils::write.csv(src_map, out_map, row.names = FALSE, na = "", fileEncoding = "UTF-8")
  }

  failed <- attr(master, "failed_surveys", exact = TRUE)
  cat(sprintf("Rows: %s  Columns: %s\n", nrow(master), ncol(master)))
  cat(sprintf("Loaded studies: %s\n", paste(attr(master, "loaded_surveys", exact = TRUE), collapse = ", ")))
  cat(sprintf("Saved: %s, %s, %s\n", out_csv, out_rds, out_map))
  if (length(failed) > 0L) {
    cat("\nFailed studies:\n", paste0("- ", failed, collapse = "\n"), "\n", sep = "")
  }
}

main()
