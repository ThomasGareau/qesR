#!/usr/bin/env Rscript

# Regenerate the legacy baselines read by the FROZEN scripts
# data-raw/build_legacy.R and data-raw/compare_legacy.R (design.md sections
# 5.11 and 5.12; the R9 baseline run). A maintainer's tool: never run in CI.
#
# Usage (from the root of a git clone of qesR, with the tag and commit):
#   Rscript data-raw/make_legacy_baselines.R --out <dir> [--only 044|050]
#                                            [--ref044 v0.4.4] [--ref050 aa1d3bd]
#
# For each reference it exports the tree with `git archive` (nothing in the
# working tree, the index or the refs changes), installs it into a library
# of its own under a temporary directory with R CMD INSTALL (its Imports,
# haven, jsonlite and xml2, must already be installed), and runs the old
# package in a fresh Rscript --vanilla session with LANGUAGE=en and an
# English UTF-8 locale. The old versions download the pinned files through
# their own clients (plain public requests, as they always did); with
# QESR_TEST_DATA_DIR set, the 0.5.0-era build reads its originals from that
# qesR download cache instead.
#
# Writes into <dir>:
#   from qesR 0.4.4 (tag v0.4.4):
#     legacy_master_source_map.csv           attr(, "source_map") of
#                                            get_qes_master(): variable names
#                                            only, no respondent data;
#     legacy_get_qes_names_all_studies.csv   study, name: the column names
#                                            get_qes() returned for the 11
#                                            studies; names only;
#     legacy_master_v044.rds                 get_qes_master();
#     legacy_decon_qes2022.rds               get_decon("qes2022");
#     legacy_decon_all_other_studies.rds     get_decon() of the 10 others, a
#                                            named list;
#   from the 0.5.0 interim builders (commit aa1d3bd of the redesign branch):
#     legacy_master_050.rds, legacy_decon_050.rds (a named list).
# The two CSVs hold variable names only and may be committed to
# data-raw/baselines/v044/, the default input of build_legacy.R (their
# qes2022 rows describe a CC BY-NC 4.0 deposit; see that folder's README).
# The .rds files are respondent-level data: never commit them. Point
# QESR_LEGACY_BASELINE (the 0.4.4 files) and QESR_LEGACY_BASELINE_050 at
# <dir> to run compare_legacy.R.

args <- commandArgs(trailingOnly = TRUE)
opt <- function(name, default = NULL) {
  i <- match(name, args)
  if (is.na(i) || i == length(args)) default else args[i + 1L]
}
if (nzchar(Sys.getenv("CI")) || nzchar(Sys.getenv("GITHUB_ACTIONS"))) {
  stop("make_legacy_baselines.R is a maintainer's tool and does not run in CI.", call. = FALSE)
}
out <- opt("--out")
if (is.null(out)) stop("Give the output directory: --out <dir>.", call. = FALSE)
only <- opt("--only", "both")
ref044 <- opt("--ref044", "v0.4.4")
ref050 <- opt("--ref050", "aa1d3bd")
dir.create(out, recursive = TRUE, showWarnings = FALSE)
out <- normalizePath(out)
if (!file.exists("DESCRIPTION") || system2("git", c("rev-parse", "--git-dir"), stdout = FALSE, stderr = FALSE) != 0L) {
  stop("Run from the root of a git clone of qesR.", call. = FALSE)
}

work <- tempfile("qesR-legacy-")
dir.create(work)
on.exit(unlink(work, recursive = TRUE), add = TRUE)
rscript <- file.path(R.home("bin"), "Rscript")

install_ref <- function(ref) {
  tag <- gsub("[^A-Za-z0-9._-]", "_", ref)
  src <- file.path(work, paste0("src-", tag))
  lib <- file.path(work, paste0("lib-", tag))
  tarball <- file.path(work, paste0(tag, ".tar"))
  dir.create(src)
  dir.create(lib)
  if (system2("git", c("-c", "core.fileMode=false", "archive", "--format=tar", "-o", shQuote(tarball), ref)) != 0L) {
    stop(sprintf("git archive %s failed (is the tag or commit in this clone?).", ref), call. = FALSE)
  }
  utils::untar(tarball, exdir = src)
  if (system2(file.path(R.home("bin"), "R"), c("CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-test-load",
                                                paste0("--library=", shQuote(lib)), shQuote(src))) != 0L) {
    stop(sprintf("R CMD INSTALL of %s failed.", ref), call. = FALSE)
  }
  lib
}

run_with <- function(lib, code) {
  script <- file.path(work, "run.R")
  writeLines(code, script, useBytes = TRUE)
  env <- c(LANGUAGE = "en", LC_ALL = "en_US.UTF-8", LANG = "en_US.UTF-8",
           R_LIBS = paste(c(lib, .libPaths()), collapse = .Platform$path.sep),
           QESR_BASELINE_LIB = lib, QESR_BASELINE_OUT = out)
  status <- system2(rscript, c("--vanilla", shQuote(script)), env = paste0(names(env), "=", shQuote(env)))
  if (status != 0L) stop("The baseline session failed; see its output above.", call. = FALSE)
}

prelude <- c(
  'lib <- Sys.getenv("QESR_BASELINE_LIB"); out <- Sys.getenv("QESR_BASELINE_OUT")',
  'library(qesR, lib.loc = lib)',
  'studies <- c("qes2022", "qes2018", "qes2018_panel", "qes2014", "qes2012", "qes2012_panel",',
  '             "qes_crop_2007_2010", "qes2008", "qes2007", "qes2007_panel", "qes1998")',
  'csv <- function(x, f) utils::write.csv(x, file.path(out, f), row.names = FALSE, fileEncoding = "UTF-8")',
  'cat("qesR", as.character(utils::packageVersion("qesR", lib.loc = lib)), "from", lib, "\\n")'
)

if (only %in% c("both", "044")) {
  lib <- install_ref(ref044)
  run_with(lib, c(prelude,
    'stopifnot(utils::packageVersion("qesR", lib.loc = lib) == "0.4.4")',
    'm <- get_qes_master(assign_global = FALSE, quiet = TRUE)',
    'saveRDS(m, file.path(out, "legacy_master_v044.rds"))',
    'csv(attr(m, "source_map"), "legacy_master_source_map.csv")',
    'nm <- do.call(rbind, lapply(studies, function(s) {',
    '  d <- get_qes(s, assign_global = FALSE, quiet = TRUE)',
    '  data.frame(study = s, name = names(d), stringsAsFactors = FALSE)',
    '}))',
    'csv(nm, "legacy_get_qes_names_all_studies.csv")',
    'dec <- stats::setNames(lapply(studies, function(s) get_decon(s, assign_global = FALSE, quiet = TRUE)), studies)',
    'saveRDS(dec[["qes2022"]], file.path(out, "legacy_decon_qes2022.rds"))',
    'saveRDS(dec[setdiff(studies, "qes2022")], file.path(out, "legacy_decon_all_other_studies.rds"))'
  ))
}

if (only %in% c("both", "050")) {
  lib <- install_ref(ref050)
  run_with(lib, c(prelude,
    'cache <- Sys.getenv("QESR_TEST_DATA_DIR")',
    'if (nzchar(cache)) options(qesR.cache_dir = cache, qesR.cache = "disk")',
    'options(qesR.lang = "en", qesR.quiet_deprecated = TRUE)',
    'm <- get_qes_master(assign_global = FALSE, quiet = TRUE)',
    'saveRDS(m, file.path(out, "legacy_master_050.rds"))',
    'dec <- stats::setNames(lapply(studies, function(s) get_decon(s, assign_global = FALSE, quiet = TRUE)), studies)',
    'saveRDS(dec, file.path(out, "legacy_decon_050.rds"))'
  ))
}

files <- list.files(out, pattern = "^legacy_.*[.](csv|rds)$", full.names = TRUE)
cat("\nWrote:\n")
for (f in files) {
  cat(sprintf("  %s  %s%s\n", unname(tools::md5sum(f)), basename(f),
              if (grepl("[.]rds$", f)) "   (respondent-level: never commit)" else ""))
}
