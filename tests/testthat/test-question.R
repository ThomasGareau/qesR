# Restored from 397aa0c^ in slice S0b. Triage:
# * The object-name test no longer writes to .GlobalEnv: since c154985,
#   get_question("name") looks the object up in the calling frame, so the
#   test defines it there. This is deliberate (assessment.md section 5.9):
#   a global object read from inside a user function, which v0.4.4 found,
#   is no longer found, so S0c/S3 must not treat that as a regression.
# * The close-match test is skipped until slice S3, which makes the column
#   match exact and suggests near matches in the error ([A:A6]). Today a
#   unique prefix ("response" -> "ResponseId") is silently accepted. An
#   ambiguous prefix already raises "Close matches"; that is tested now.

test_that("get_question works for data.frame and object name", {
  dat <- data.frame(x = 1:3)
  attr(dat$x, "label") <- "Example question text"

  expect_true(identical(get_question(dat, "x"), "Example question text"))

  lookup <- function() {
    tmp_qes_obj <- dat
    get_question("tmp_qes_obj", "x")
  }
  expect_true(identical(lookup(), "Example question text"))
  expect_false(exists("tmp_qes_obj", envir = globalenv(), inherits = FALSE))
})

test_that("get_question does not find objects outside the calling frame", {
  dat <- data.frame(x = 1:3)
  attr(dat$x, "label") <- "Example question text"
  # the object lives in an enclosing frame, not in the frame that calls
  # get_question(); inherits = FALSE must not reach it
  outer <- function() {
    tmp_qes_hidden <- dat
    inner <- function() get_question("tmp_qes_hidden", "x")
    inner()
  }
  withr::local_language("en")
  expect_error(outer(), "not found")
})

test_that("get_question reports missing label", {
  dat <- data.frame(x = 1:3)

  expect_warning(
    out <- get_question(dat, "x"),
    "No question label"
  )
  expect_true(is.na(out))
})

test_that("get_question resolves column name case-insensitively", {
  dat <- data.frame(ResponseId = 1:3)
  attr(dat$ResponseId, "label") <- "Response ID"

  expect_true(identical(get_question(dat, "responseid"), "Response ID"))
})

test_that("get_question lists close matches for an ambiguous prefix", {
  withr::local_language("en")
  dat <- data.frame(ResponseId = 1:3, ResponseTime = 1:3)
  expect_error(get_question(dat, "response"), "Close matches")
})

test_that("get_question provides close-match suggestions", {
  skip("fixed in S3: exact column match with near-match suggestions ([A:A6])")
  dat <- data.frame(ResponseId = 1:3, stringsAsFactors = FALSE)

  expect_error(
    get_question(dat, "response"),
    "Close matches"
  )
})
