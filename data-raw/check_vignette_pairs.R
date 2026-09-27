#!/usr/bin/env Rscript

# EN/FR vignette pairs (design.md sections 1.2 constraint 5 and 9).
#
# Every French vignette or article is named fr-*.Rmd and links to its English
# partner in its first lines ("*[English version](<partner>.html)*"); the
# partner links back ("*[Version française](fr-....html)*"). This script checks
# that every page has a partner that links back and that both pages run the
# same code: their knitr::purl() output, chunk labels and options included,
# must be identical, so the prose may differ but the code may not. This holds
# for the vignettes that ship with the package (vignettes/*.Rmd) and for the
# website-only articles (vignettes/articles/). Text that must differ by
# language (table headers, figure alt text) is chosen in the code from
# params$lang.
#
# Vignette sources are not installed with the package, so this cannot run
# under R CMD check. It runs in CI (.github/workflows/R-CMD-check.yml) and
# locally:
#   Rscript data-raw/check_vignette_pairs.R [<package root>]

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1]] else "."

partner_link <- function(path) {
  lines <- enc2utf8(readLines(path, n = 40L, encoding = "UTF-8", warn = FALSE))
  hit <- regmatches(lines, regexpr("\\[(English version|Version française)\\]\\(([^)]+)\\.html\\)", lines))
  if (!length(hit)) return(NA_character_)
  sub("^.*\\(([^)]+)\\.html\\).*$", "\\1", hit[[1]])
}

purl_code <- function(path) {
  out <- tempfile(fileext = ".R")
  on.exit(unlink(out))
  # (recent xfun versions warn about knitr internals; not about the page)
  suppressWarnings(knitr::purl(path, output = out, documentation = 1L, quiet = TRUE))
  code <- readLines(out, encoding = "UTF-8", warn = FALSE)
  # purl() writes a parameterized page's `params` first; the pages of a pair
  # differ there by design (lang = "en" / "fr"), so compare from the first
  # chunk on
  first <- grep("^## ----", code)[1]
  if (!is.na(first)) code <- code[first:length(code)]
  code[nzchar(trimws(code))]
}

check_dir <- function(dir, compare_code) {
  problems <- character(0)
  rmd <- list.files(dir, pattern = "\\.Rmd$")
  stems <- sub("\\.Rmd$", "", rmd)
  for (stem in stems) {
    path <- file.path(dir, paste0(stem, ".Rmd"))
    partner <- partner_link(path)
    if (is.na(partner)) {
      problems <- c(problems, sprintf("%s: no link to its %s partner", path,
                                      if (startsWith(stem, "fr-")) "English" else "French"))
      next
    }
    if (startsWith(stem, "fr-") == startsWith(partner, "fr-")) {
      problems <- c(problems, sprintf("%s: its partner %s is in the same language", path, partner))
      next
    }
    if (!partner %in% stems) {
      problems <- c(problems, sprintf("%s: partner %s.Rmd does not exist", path, partner))
      next
    }
    back <- partner_link(file.path(dir, paste0(partner, ".Rmd")))
    if (!identical(back, stem)) {
      problems <- c(problems, sprintf("%s: partner %s links to %s, not back", path, partner, back))
      next
    }
    if (compare_code && startsWith(stem, "fr-")) {
      en <- purl_code(file.path(dir, paste0(partner, ".Rmd")))
      fr <- purl_code(path)
      if (!identical(en, fr)) {
        problems <- c(problems, sprintf(
          "%s and %s.Rmd run different code (first difference at code line %d)",
          path, partner, which(!(c(en, rep("", max(0, length(fr) - length(en)))) ==
                                   c(fr, rep("", max(0, length(en) - length(fr))))))[1]
        ))
      }
    }
  }
  problems
}

# Relative links between pages ("](<stem>.html)"): pkgdown renders the
# vignettes and the articles side by side, so each stem must be a page of
# either folder.
check_links <- function(root) {
  dirs <- file.path(root, c("vignettes", file.path("vignettes", "articles")))
  paths <- unlist(lapply(dirs, list.files, pattern = "\\.Rmd$", full.names = TRUE))
  stems <- sub("\\.Rmd$", "", basename(paths))
  problems <- character(0)
  for (path in paths) {
    text <- enc2utf8(readLines(path, encoding = "UTF-8", warn = FALSE))
    hits <- unlist(regmatches(text, gregexpr("\\]\\(([A-Za-z0-9._-]+)\\.html(#[^)]*)?\\)", text)))
    targets <- unique(sub("^\\]\\(([A-Za-z0-9._-]+)\\.html.*$", "\\1", hits))
    for (t in setdiff(targets, stems)) {
      problems <- c(problems, sprintf("%s: link to %s.html, which is not a page", path, t))
    }
  }
  problems
}

problems <- c(
  check_dir(file.path(root, "vignettes"), compare_code = TRUE),
  check_dir(file.path(root, "vignettes", "articles"), compare_code = TRUE),
  check_links(root)
)
if (length(problems)) {
  cat("EN/FR vignette pairs:\n", paste0("- ", problems, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 1L)
}
cat("EN/FR vignette pairs: all pages paired; every pair runs identical code; links resolve.\n")
