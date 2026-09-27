#!/usr/bin/env Rscript

# Spec and source checks for CI and maintainers (design.md sections 5.10,
# 5.11 and 8.4, slices HZ1 and HZ2). CI runs it on ubuntu-release; locally,
# from the package root:
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
#   2. the data checks V-D1 to V-D4, V-D7 and V-D8 (R/hz-data.R) and V-P1,
#      the projected marginals against expected/marginals.csv, on the shipped
#      dictionary and gates.csv, and on the aggregates of the studies whose
#      metadata cannot ship (qes2022, OD3) in the build-ignored data-raw/nc/
#      (sources_*.csv, gates_*.csv, against marginals_*.csv). When those
#      files are absent the check is skipped with a note, and is an error
#      only with QESR_REQUIRE_NC=true;
#   3. V-P2 against the merge-base with origin/main: when the spec content
#      changed, Spec-Version must be higher and have a CHANGES.csv row; and
#      the MAJOR rule of section 5.11 on expected/marginals.csv and
#      expected/hashes.csv: when a recorded marginal or column hash changed
#      or disappeared, the major digit (the minor digit before 1.0.0) must be
#      higher;
#   4. V-P3, the generated documentation is current: the target list of
#      man/qes_spec.Rd (roxygen @eval .rd_targets()) names the spec version
#      and every target (re-run roxygen otherwise), and the coverage grids
#      of README.md are the grid of the spec (re-run
#      data-raw/readme_coverage.R otherwise). The reference vignettes
#      are generated when they are built, so they cannot be stale;
#   5. the source grep: the calls that tests/testthat/test-forbidden-calls.R
#      looks for in the installed namespace, searched in the code (comments
#      excluded) of R/*.R.
# It prints every problem and exits with status 1 when one is an error.
#
# The spec is edited by hand (in R or LibreOffice, saved as UTF-8 CSV with LF
# line endings). After any change: bump Spec-Version in SPEC by the rule of
# design.md section 5.11, add a CHANGES.csv row, then run --write-hash.
# When a change moves counts, rebuild the aggregates first
# (data-raw/build_sources.R, then data-raw/project_marginals.R), and the
# column hashes on the originals (data-raw/build_hashes.R).

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

# ---- 2. data checks and V-P1 --------------------------------------------------------------
report <- function(p, what) {
  if (nrow(p) > 0L) {
    print(p, right = FALSE)
  }
  n <- sum(p$severity == "error")
  cat(sprintf("%s: %d error(s), %d warning(s).\n", what, n, sum(p$severity == "warning")))
  n > 0L
}
sources <- .qes_hz_sources_shipped(spec)
failed <- report(.qes_data_check(spec, sources), "Data checks (dictionary, gates.csv)") || failed
failed <- report(.qes_projection_check(spec, sources), "V-P1 (expected/marginals.csv)") || failed
cat_ <- .qes_catalog()
closed <- setdiff(unique(spec$tables$crosswalk$study),
                  cat_$studies$study[cat_$studies$metadata_shipped %in% TRUE])
for (study in closed) {
  nc <- .qes_hz_sources_read(file.path("data-raw", "nc"), study)
  marg <- file.path("data-raw", "nc", sprintf("marginals_%s.csv", study))
  if (is.null(nc) || !file.exists(marg)) {
    # the owner may keep these aggregates out of the repository (OD3): the
    # check is then skipped, unless QESR_REQUIRE_NC=true asks for it
    required <- identical(Sys.getenv("QESR_REQUIRE_NC"), "true")
    cat(sprintf("V-D/V-P1 %s: no aggregates in data-raw/nc (run data-raw/build_sources.R and data-raw/project_marginals.R); %s.\n",
                study, if (required) "required, error" else "skipped"))
    failed <- failed || required
    next
  }
  failed <- report(.qes_data_check(spec, nc, studies = study), sprintf("Data checks (%s, data-raw/nc)", study)) || failed
  failed <- report(.qes_projection_check(spec, nc, .qes_read_csv(marg, "spec_expected"), studies = study),
                   sprintf("V-P1 (%s, data-raw/nc)", study)) || failed
}

# ---- 3. V-P2 and the MAJOR rule against the merge-base -------------------------------------
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
  ov <- package_version(old_version)
  nv <- package_version(spec$version)
  # MAJOR: the major digit, or the minor digit before 1.0.0
  bumped <- if (ov$major >= 1L) nv$major > ov$major else (nv$major > ov$major || nv$minor > ov$minor)
  old_marg <- file.path(old, "expected", "marginals.csv")
  if (file.exists(old_marg)) {
    change <- .qes_expected_change(.qes_read_csv(old_marg, "spec_expected"), spec$tables$expected)
    if (identical(as.character(change), "major") && !bumped) {
      cat(sprintf("MAJOR rule error: recorded marginals changed (%s) but Spec-Version %s -> %s is not a major bump.\n",
                  paste(utils::head(attr(change, "keys"), 5L), collapse = "; "), old_version, spec$version))
      failed <- TRUE
    } else {
      cat(sprintf("MAJOR rule: expected/marginals.csv change since the merge-base is '%s'.\n", change))
    }
  }
  old_hash <- file.path(old, "expected", "hashes.csv")
  if (file.exists(old_hash)) {
    oh <- .qes_read_csv(old_hash, "spec_hashes")
    nh <- spec$tables$hashes
    key <- function(x) paste(x$study, x$wave, x$target, x$source_var, sep = "|")
    moved <- key(oh)[is.na(match(paste(key(oh), oh$md5), paste(key(nh), nh$md5)))]
    if (length(moved) > 0L && !bumped) {
      cat(sprintf("MAJOR rule error: column hashes changed or disappeared (%s) but Spec-Version %s -> %s is not a major bump.\n",
                  paste(utils::head(moved, 5L), collapse = "; "), old_version, spec$version))
      failed <- TRUE
    } else {
      cat(sprintf("MAJOR rule: %d column hash(es) changed or disappeared since the merge-base.\n", length(moved)))
    }
  }
} else {
  cat("V-P2: no spec at the merge-base with origin/main (or no git history); skipped.\n")
}

# ---- 4. V-P3: generated documentation ------------------------------------------------------
rd_path <- file.path("man", "qes_spec.Rd")
if (file.exists(rd_path)) {
  rd <- paste(readLines(rd_path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  missing_targets <- spec$tables$targets$target[!vapply(spec$tables$targets$target, function(t) {
    grepl(sprintf("\\code{%s}}", t), rd, fixed = TRUE)
  }, logical(1))]
  current <- grepl(sprintf("Targets in the shipped spec (version %s)", spec$version), rd, fixed = TRUE)
  if (!current || length(missing_targets) > 0L) {
    cat(sprintf("V-P3 error: %s is not current (spec %s%s); run roxygen2::roxygenise().\n", rd_path, spec$version,
                if (length(missing_targets) > 0L) paste0(", missing ", paste(missing_targets, collapse = ", ")) else ""))
    failed <- TRUE
  } else {
    cat("V-P3: the target list of ?qes_spec is current.\n")
  }
} else {
  cat("V-P3 error: man/qes_spec.Rd is missing; run roxygen2::roxygenise().\n")
  failed <- TRUE
}

# The coverage grids of README.md (between the "coverage" markers) are the
# grid of the spec, .spec_readme_md(); data-raw/readme_coverage.R writes them.
readme <- if (file.exists("README.md")) enc2utf8(readLines("README.md", encoding = "UTF-8", warn = FALSE)) else character(0)
start <- grep("^<!-- coverage: start", readme)
end <- grep("^<!-- coverage: end", readme)
grid <- c("", strsplit(sub("\n$", "", .spec_readme_md(spec)), "\n", fixed = TRUE)[[1]], "")
readme_ok <- length(start) > 0L && length(start) == length(end) && all(end > start) &&
  all(vapply(seq_along(start), function(k) identical(readme[seq_len(end[k] - start[k] - 1L) + start[k]], grid), logical(1)))
if (readme_ok) {
  cat("V-P3: the coverage grid of README.md is current.\n")
} else {
  cat("V-P3 error: the coverage grid of README.md is not current (or its markers are missing); run Rscript data-raw/readme_coverage.R.\n")
  failed <- TRUE
}

# ---- 5. the source grep --------------------------------------------------------------
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
