#!/usr/bin/env Rscript

# Download the public build inputs listed in data-raw/inputs.csv.
#
# Usage (from the package root):
#   Rscript data-raw/fetch_inputs.R [<dest dir>] [--refresh]
#
# <dest dir> defaults to tools::R_user_dir("qesR", "cache")/build-inputs.
# Each row of inputs.csv names a file, its URL, the subfolder of <dest dir>
# it goes to, its expected md5, its licence and its method:
#   plain   downloaded here through qesR's own HTTP client (R/http.R): one
#           plain GET, User-Agent "qesR/<ver> R/<ver>" and nothing else, no
#           cookie, no personal information; requests are sequential and at
#           least 1 s apart (whatever the host), a part file is renamed only
#           when complete;
#   manual  never requested (the source needs a browser); printed with its
#           URL, destination and expected md5.
# A file already present is not requested again unless --refresh is given
# (rows whose md5_check is "strict" are re-requested when their md5 differs).
# After the download every file's md5 is compared with inputs.csv: a
# mismatch on a "strict" row is an error (exit status 1); on a "warn" row
# (Statistics Canada full tables, which are revised; Dataverse dataset JSON,
# which follows the deposit) it is only reported, since the builders check
# what they derive from the file.
#
# The respondent-level originals are not listed here: fill a qesR download
# cache with data-raw/live_fetch.R (QESR_CACHE_DIR), which verifies each file
# against the md5 pinned in the catalog. At the end the script prints the
# environment variables the builders read.

args <- commandArgs(trailingOnly = TRUE)
refresh <- "--refresh" %in% args
pos <- args[!startsWith(args, "--")]
dest_root <- if (length(pos) > 0L) pos[1] else file.path(tools::R_user_dir("qesR", "cache"), "build-inputs")
dest_root <- normalizePath(dest_root, mustWork = FALSE)
dir.create(dest_root, recursive = TRUE, showWarnings = FALSE)

if (!file.exists("DESCRIPTION") || !file.exists(file.path("data-raw", "inputs.csv"))) {
  stop("Run from the package root.", call. = FALSE)
}
inputs <- utils::read.csv(file.path("data-raw", "inputs.csv"), colClasses = "character",
                          na.strings = character(), encoding = "UTF-8")

# qesR's own client when the source tree loads; curl otherwise, with the same
# User-Agent and nothing else.
use_pkg <- requireNamespace("pkgload", quietly = TRUE) &&
  isTRUE(tryCatch({
    pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
    TRUE
  }, error = function(e) FALSE))
if (use_pkg) {
  get_file <- function(url, path) .qes_fetch(url, path)
  agent <- .qes_user_agent()
} else {
  agent <- sprintf("qesR/%s R/%s", read.dcf("DESCRIPTION", "Version")[1, 1], getRversion())
  get_file <- function(url, path) {
    part <- paste0(path, ".part")
    on.exit(unlink(part))
    status <- system2("curl", c("-sSfL", "--retry", "2", "-A", shQuote(agent), "-o", shQuote(part), shQuote(url)))
    if (status != 0L) stop(sprintf("curl failed (status %d) for %s", status, url), call. = FALSE)
    file.rename(part, path)
  }
}
cat(sprintf("Destination: %s\nClient: %s, User-Agent \"%s\"\n\n", dest_root,
            if (use_pkg) "qesR (R/http.R)" else "curl", agent))

md5_of <- function(path) if (file.exists(path)) unname(tools::md5sum(path)) else NA_character_
last_request <- -Inf
status <- character(nrow(inputs))
got_md5 <- rep(NA_character_, nrow(inputs))
for (i in seq_len(nrow(inputs))) {
  r <- inputs[i, , drop = FALSE]
  dir <- file.path(dest_root, r$dest)
  path <- file.path(dir, r$file)
  if (r$method != "plain") {
    status[i] <- if (file.exists(path)) "manual, present" else "manual, missing"
    got_md5[i] <- md5_of(path)
    next
  }
  have <- md5_of(path)
  current <- !is.na(have) && !refresh && (r$md5_check != "strict" || identical(have, r$md5))
  if (current) {
    status[i] <- "present"
    got_md5[i] <- have
    next
  }
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  wait <- 1.1 - (as.numeric(Sys.time()) - last_request)
  if (wait > 0) Sys.sleep(wait)
  res <- tryCatch(get_file(r$url, path), error = function(e) e)
  last_request <- as.numeric(Sys.time())
  if (inherits(res, "error")) {
    status[i] <- paste("failed:", gsub("\\s+", " ", conditionMessage(res)))
  } else {
    status[i] <- "downloaded"
  }
  got_md5[i] <- md5_of(path)
}

match_ <- ifelse(is.na(got_md5), "-", ifelse(got_md5 == inputs$md5, "ok",
                 ifelse(inputs$md5_check == "strict", "MISMATCH", "differs (warn)")))
report <- data.frame(name = inputs$name, method = inputs$method, status = substr(status, 1, 60),
                     md5 = match_, stringsAsFactors = FALSE)
print(report, row.names = FALSE, right = FALSE)

manual <- inputs[inputs$method != "plain", , drop = FALSE]
if (nrow(manual) > 0L) {
  cat("\nManual downloads (open the URL in a browser, save the file at the path shown):\n")
  for (i in seq_len(nrow(manual))) {
    m <- manual[i, , drop = FALSE]
    cat(sprintf("  %s\n    url:  %s\n    save: %s\n    md5:  %s\n    %s\n", m$name, m$url,
                file.path(dest_root, m$dest, m$file), m$md5, m$note))
  }
}

cat("\nEnvironment for the builders:\n")
cat(sprintf("  export QESR_DV_JSON_DIR=%s   # optional: build_catalog.R checks the pins against it\n",
            shQuote(file.path(dest_root, "dataverse_latest"))))
cat(sprintf("  export QESR_BENCH_SRC=%s\n", shQuote(file.path(dest_root, "bench"))))
cat(sprintf("  export QESR_CACHE_DIR=%s   # then: Rscript data-raw/live_fetch.R\n",
            shQuote(file.path(dest_root, "originals"))))

bad <- grepl("^failed", status) | match_ == "MISMATCH"
if (any(bad)) {
  cat(sprintf("\n%d input(s) failed or have the wrong md5: %s\n", sum(bad), paste(inputs$name[bad], collapse = ", ")))
  quit(save = "no", status = 1L)
}
cat("\nAll plain inputs are present; md5 as reported above.\n")
