#!/usr/bin/env Rscript

# Build and save a harmonized stacked QES master dataset.
#
# Usage examples:
#   Rscript scripts/build_qes_master.R
#   Rscript scripts/build_qes_master.R --surveys qes2022,qes2018 --out-dir data
#   Rscript scripts/build_qes_master.R --strict --out-prefix qes_master_all

parse_args <- function(args) {
  opts <- list(
    surveys = NULL,
    out_dir = getwd(),
    out_prefix = "qes_master",
    strict = FALSE,
    quiet = FALSE
  )

  i <- 1L
  while (i <= length(args)) {
    a <- args[[i]]

    if (identical(a, "--surveys")) {
      i <- i + 1L
      if (i > length(args)) stop("Missing value for --surveys", call. = FALSE)
      val <- trimws(args[[i]])
      if (!nzchar(val)) stop("--surveys cannot be empty", call. = FALSE)
      opts$surveys <- strsplit(val, ",", fixed = TRUE)[[1]]
      opts$surveys <- trimws(opts$surveys)
      opts$surveys <- opts$surveys[nzchar(opts$surveys)]
    } else if (identical(a, "--out-dir")) {
      i <- i + 1L
      if (i > length(args)) stop("Missing value for --out-dir", call. = FALSE)
      opts$out_dir <- args[[i]]
    } else if (identical(a, "--out-prefix")) {
      i <- i + 1L
      if (i > length(args)) stop("Missing value for --out-prefix", call. = FALSE)
      opts$out_prefix <- args[[i]]
    } else if (identical(a, "--strict")) {
      opts$strict <- TRUE
    } else if (identical(a, "--quiet")) {
      opts$quiet <- TRUE
    } else if (identical(a, "--help") || identical(a, "-h")) {
      cat(
        paste(
          "Usage:",
          "  Rscript scripts/build_qes_master.R [options]",
          "",
          "Options:",
          "  --surveys CODE1,CODE2   Optional subset of survey codes",
          "  --out-dir PATH          Output directory (default: current directory)",
          "  --out-prefix NAME       Output prefix (default: qes_master)",
          "  --strict                Fail if any survey fails to download",
          "  --quiet                 Reduce logging",
          "  --help, -h              Show this help",
          sep = "\n"
        ),
        "\n"
      )
      quit(save = "no", status = 0)
    } else {
      stop(sprintf("Unknown argument: %s", a), call. = FALSE)
    }

    i <- i + 1L
  }

  opts
}

load_qesR <- function() {
  cmd_args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", cmd_args, value = TRUE)
  script_path <- if (length(file_arg) > 0L) normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE) else NULL
  repo_dir <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE) else NULL

  # Prefer local source so the script uses latest in-repo code.
  if (!is.null(repo_dir) && dir.exists(repo_dir) && file.exists(file.path(repo_dir, "DESCRIPTION"))) {
    if (!requireNamespace("devtools", quietly = TRUE)) {
      stop("devtools is required to load local qesR source from this repo.", call. = FALSE)
    }
    devtools::load_all(repo_dir, quiet = TRUE)
    if (exists("get_qes_master", mode = "function")) return(invisible(TRUE))
  }

  if (requireNamespace("qesR", quietly = TRUE)) {
    suppressPackageStartupMessages(library(qesR))
    if (exists("get_qes_master", mode = "function")) return(invisible(TRUE))
  }

  if (!exists("get_qes_master", mode = "function")) {
    stop("Failed to load qesR with get_qes_master().", call. = FALSE)
  }

  invisible(TRUE)
}

main <- function() {
  opts <- parse_args(commandArgs(trailingOnly = TRUE))
  load_qesR()

  if (!dir.exists(opts$out_dir)) {
    dir.create(opts$out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  out_csv <- file.path(opts$out_dir, paste0(opts$out_prefix, ".csv"))
  out_rds <- file.path(opts$out_dir, paste0(opts$out_prefix, ".rds"))
  out_map <- file.path(opts$out_dir, paste0(opts$out_prefix, "_source_map.csv"))

  master <- get_qes_master(
    surveys = opts$surveys,
    assign_global = FALSE,
    quiet = opts$quiet,
    strict = opts$strict
  )

  utils::write.csv(master, out_csv, row.names = FALSE, na = "")
  saveRDS(master, out_rds)

  src_map <- attr(master, "source_map", exact = TRUE)
  if (is.data.frame(src_map) && nrow(src_map) > 0L) {
    utils::write.csv(src_map, out_map, row.names = FALSE, na = "")
  }

  failed <- attr(master, "failed_surveys", exact = TRUE)
  loaded <- attr(master, "loaded_surveys", exact = TRUE)
  dedup_removed <- attr(master, "duplicates_removed", exact = TRUE)
  empty_removed <- attr(master, "empty_rows_removed", exact = TRUE)

  cat(sprintf("Master rows: %s\n", nrow(master)))
  cat(sprintf("Master columns: %s\n", ncol(master)))
  cat(sprintf("Loaded studies: %s\n", length(loaded)))
  cat(sprintf("Failed studies: %s\n", length(failed)))
  if (is.null(dedup_removed)) dedup_removed <- 0
  if (is.null(empty_removed)) empty_removed <- 0
  cat(sprintf("Duplicate respondents removed: %s\n", dedup_removed))
  cat(sprintf("All-empty rows removed: %s\n", empty_removed))
  cat(sprintf("Saved CSV: %s\n", out_csv))
  cat(sprintf("Saved RDS: %s\n", out_rds))
  if (file.exists(out_map)) {
    cat(sprintf("Saved source map: %s\n", out_map))
  }

  if (length(failed) > 0L) {
    cat("\nFailed survey details:\n")
    cat(paste0("- ", failed, collapse = "\n"), "\n")
  }
}

main()
