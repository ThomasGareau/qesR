#!/usr/bin/env Rscript

if (!requireNamespace("pkgdown", quietly = TRUE)) {
  stop("pkgdown is required. Install it with install.packages('pkgdown').", call. = FALSE)
}

cmd_args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", cmd_args, value = TRUE)
script_path <- if (length(file_arg) > 0L) normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE) else NULL
repo_dir <- if (!is.null(script_path)) normalizePath(file.path(dirname(script_path), ".."), mustWork = FALSE) else getwd()

pkgdown::build_site_github_pages(pkg = repo_dir, new_process = FALSE, install = FALSE)
