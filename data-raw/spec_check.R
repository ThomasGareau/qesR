#!/usr/bin/env Rscript

# Spec and source checks for CI and maintainers (design.md sections 5.10,
# 5.11 and 8.4, slice HZ1). CI runs it on ubuntu-release; locally, from the
# package root:
#
#   Rscript data-raw/spec_check.R              # check
#   Rscript data-raw/spec_check.R --release    # check as for a release tag
#   Rscript data-raw/spec_check.R --write-hash # after editing the spec:
#                                              # record the content hash in SPEC
#
# What it checks, on the source tree (R/ and the spec sources are not
# installed, so R CMD check cannot):
#   1. the one validator (.qes_spec_check(), R/hz-validate.R) on
#      inst/extdata/harmonize/, with the source-tree parts of V-S10 (every
#      fn: rule has tests/testthat/test-hz-fn-<name>.R) and, with --release,
#      the release rules of V-S11 and V-S13 (no draft row; no stable row on a
#      study-wave whose recommended weight needs review);
#   2. V-P2 against the merge-base with origin/main: when the spec content
#      changed, Spec-Version must be higher and have a CHANGES.csv row;
#   3. the source grep: the calls that tests/testthat/test-forbidden-calls.R
#      looks for in the installed namespace, searched in the code (comments
#      excluded) of R/*.R.
# It prints every problem and exits with status 1 when one is an error.
#
# The spec is edited by hand (in R or LibreOffice, saved as UTF-8 CSV with LF
# line endings). After any change: bump Spec-Version in SPEC by the rule of
# design.md section 5.11, add a CHANGES.csv row, then run --write-hash.
# Checks still to come: V-P1 and the MAJOR rule on expected/ (slice HZ2),
# V-P3 on the generated reference (slice HZ3).

args <- commandArgs(trailingOnly = TRUE)
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

dir <- normalizePath(file.path("inst", "extdata", "harmonize"), mustWork = TRUE)
failed <- FALSE

# ---- --write-hash ----------------------------------------------------------------
if ("--write-hash" %in% args) {
  path <- file.path(dir, "SPEC")
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  hash <- .qes_spec_hash(dir)
  lines <- sub("^Hash: .*$", paste0("Hash: ", hash), lines)
  con <- file(path, open = "wb")
  writeBin(charToRaw(paste0(paste(enc2utf8(lines), collapse = "\n"), "\n")), con)
  close(con)
  cat("SPEC Hash:", hash, "\n")
}

# ---- 1. the validator ------------------------------------------------------------
spec <- .qes_spec_load(dir)
spec$custom <- FALSE
problems <- .qes_spec_check(spec, release = "--release" %in% args, source_dir = ".")
if (nrow(problems) > 0L) {
  print(problems, right = FALSE)
}
n_err <- sum(problems$severity == "error")
cat(sprintf("Spec %s, hash %s: %d error(s), %d warning(s), %d note(s).\n",
            spec$version, spec$hash, n_err, sum(problems$severity == "warning"),
            sum(problems$severity == "note")))
failed <- failed || n_err > 0L

# ---- 2. V-P2 against the merge-base ---------------------------------------------------
git <- function(...) {
  out <- tryCatch(suppressWarnings(system2("git", c(...), stdout = TRUE, stderr = FALSE)),
                  error = function(e) character(0))
  if (!is.null(attr(out, "status")) && attr(out, "status") != 0L) character(0) else out
}
base <- git("merge-base", "HEAD", "origin/main")
if (length(base) == 1L &&
    length(git("ls-tree", "--name-only", base, "inst/extdata/harmonize/SPEC")) == 1L) {
  old <- tempfile("spec-base-")
  dir.create(old)
  files <- git("ls-tree", "-r", "--name-only", base, "inst/extdata/harmonize")
  for (f in files) {
    dest <- file.path(old, sub("^inst/extdata/harmonize/", "", f))
    dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
    writeLines(git("show", paste0(base, ":", f)), dest, useBytes = TRUE)
  }
  old_hash <- .qes_spec_hash(old)
  old_version <- read.dcf(file.path(old, "SPEC"))[1, "Spec-Version"]
  if (!identical(old_hash, spec$hash)) {
    if (package_version(spec$version) <= package_version(old_version)) {
      cat(sprintf("V-P2 error: the spec content changed since %s but Spec-Version is %s (was %s).\n",
                  substr(base, 1, 8), spec$version, old_version))
      failed <- TRUE
    } else {
      cat(sprintf("V-P2: spec %s -> %s since the merge-base.\n", old_version, spec$version))
    }
  } else {
    cat("V-P2: spec unchanged since the merge-base.\n")
  }
} else {
  cat("V-P2: no spec at the merge-base with origin/main (or no git history); skipped.\n")
}

# ---- 3. the source grep --------------------------------------------------------------
# Same patterns as tests/testthat/test-forbidden-calls.R. `allow` names the
# files where a pattern is expected; `pending` rules are reported, not failed,
# until the slice that removes their last offender.
rules <- list(
  list(pattern = "(?<![A-Za-z0-9_.])load\\s*\\(", what = "load()"),
  list(pattern = "(?<![A-Za-z0-9_.])(eval|parse)\\s*\\(", what = "eval() or parse()"),
  list(pattern = "\\.GlobalEnv|globalenv\\s*\\(", what = "the global environment"),
  list(pattern = "grepl\\s*\\([^)]*conditionMessage", what = "branching on message text"),
  list(pattern = "ssl_verifypeer|--insecure|\\binsecure|download\\.file\\.extra", what = "insecure TLS"),
  list(pattern = "\\bsystem2?\\s*\\(", what = "a shell-out"),
  list(pattern = "Sys\\.which\\s*\\(\\s*\"(curl|wget)", what = "a shell-out to curl or wget"),
  list(pattern = "(?<![A-Za-z0-9_.])assign\\s*\\(", what = "assign()", allow = "R/assign.R"),
  list(pattern = "\\breadRDS\\s*\\(", what = "readRDS()"),
  list(pattern = "iconv\\s*\\([^)]*TRANSLIT", what = "iconv() transliteration", pending = "HZ6")
)
code_lines <- function(path) {
  pd <- utils::getParseData(parse(path, keep.source = TRUE, encoding = "UTF-8"), includeText = TRUE)
  pd <- pd[pd$terminal & pd$token != "COMMENT", , drop = FALSE]
  lines <- split(pd$text, pd$line1)
  vapply(lines, paste, character(1), collapse = " ")
}
for (path in sort(list.files("R", pattern = "\\.R$", full.names = TRUE))) {
  code <- code_lines(path)
  for (r in rules) {
    hit <- grepl(r$pattern, code, perl = TRUE)
    if (!any(hit) || path %in% r$allow) next
    where <- paste0(path, ":", names(code)[hit], collapse = ", ")
    if (!is.null(r$pending)) {
      cat(sprintf("source grep (pending %s): %s in %s\n", r$pending, r$what, where))
    } else {
      cat(sprintf("source grep error: %s in %s\n", r$what, where))
      failed <- TRUE
    }
  }
}

if (failed) {
  quit(status = 1L)
}
cat("Spec and source checks passed.\n")
