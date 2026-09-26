# Forbidden calls (design.md section 8.1, P8, constraint 6). Walks every
# function in the installed namespace, deparses it and looks for calls that
# the design rules out. This works where R/ is absent (an installed package).
# A grep of the R/ sources for the same patterns arrives with
# data-raw/spec_check.R (slice HZ1, design.md section 8.4); until then this
# test is the only check.
#
# Rules with no offender today are enforced now. Each pending rule has its own
# test, skipped until the slice that removes its last offender; that slice
# deletes the skip() line.

namespace_sources <- function() {
  ns <- asNamespace("qesR")
  fns <- Filter(function(nm) is.function(get(nm, envir = ns)), ls(ns, all.names = TRUE))
  stats::setNames(lapply(fns, function(nm) {
    paste(deparse(get(nm, envir = ns)), collapse = "\n")
  }), fns)
}

offenders <- function(pattern, allow = character(0)) {
  src <- namespace_sources()
  hits <- names(src)[vapply(src, function(s) grepl(pattern, s, perl = TRUE), logical(1))]
  setdiff(hits, allow)
}

expect_no_offender <- function(pattern, allow = character(0)) {
  expect_identical(offenders(pattern, allow), character(0), info = pattern)
}

test_that("no function loads code or objects from outside the package", {
  expect_no_offender("(?<![A-Za-z0-9_.])load\\s*\\(")
  expect_no_offender("(?<![A-Za-z0-9_.])(eval|parse)\\s*\\(")
})

test_that("no function touches the global environment", {
  expect_no_offender("\\.GlobalEnv|globalenv\\s*\\(")
})

test_that("no function branches on condition message text", {
  expect_no_offender("grepl\\s*\\([^)]*conditionMessage")
})

test_that("no insecure TLS option or retry", {
  expect_no_offender("ssl_verifypeer|--insecure|\\binsecure|download\\.file\\.extra")
})

test_that("no shell-out to curl or wget", {
  expect_no_offender("\\bsystem2?\\s*\\(\\s*\"(curl|wget)")
  expect_no_offender("Sys\\.which\\s*\\(\\s*\"(curl|wget)")
})

test_that("no shell-out", {
  skip("fixed in S3 (PDF/DOC text extraction)")
  expect_no_offender("\\bsystem2?\\s*\\(")
})

test_that("assign() is used only by .qes_assign()", {
  expect_no_offender("(?<![A-Za-z0-9_.])assign\\s*\\(", allow = ".qes_assign")
})

test_that("readRDS() is never called", {
  expect_no_offender("\\breadRDS\\s*\\(")
})

test_that("no iconv() transliteration", {
  skip("fixed in S3 (DDI label tokens) and HZ6 (legacy master text)")
  expect_no_offender("iconv\\s*\\([^)]*TRANSLIT")
})
