# Restored from 397aa0c^ in slice S0b. Triage:
# * The object-name test no longer writes to .GlobalEnv: get_question("name")
#   looks the object up in the calling frame and its enclosing frames (read
#   only), so a function or sapply() lambda defined at top level finds a
#   workspace object, as v0.4.4 did.
# * Slice S3 made the column match exact (case aside) and suggests near
#   matches in the error ([A:A6]): a prefix is never accepted.

test_that("get_question works for data.frame and object name", {
  local_qes_notices_shown()
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

test_that("get_question finds objects in enclosing frames", {
  local_qes_notices_shown()
  dat <- data.frame(x = 1:3)
  attr(dat$x, "label") <- "Example question text"
  # the object lives in an enclosing frame, not in the frame that calls
  # get_question()
  outer <- function() {
    tmp_qes_hidden <- dat
    inner <- function() get_question("tmp_qes_hidden", "x")
    inner()
  }
  expect_identical(outer(), "Example question text")
})

test_that("get_question finds a workspace object from a function and from sapply()", {
  local_qes_notices_shown()
  dat <- data.frame(x = 1:3, y = 4:6)
  attr(dat$x, "label") <- "Question x"
  attr(dat$y, "label") <- "Question y"
  ws <- globalenv()
  nm <- "tmp_qes_ws_obj"
  stopifnot(!exists(nm, envir = ws, inherits = FALSE))
  assign(nm, dat, envir = ws)
  withr::defer(rm(list = nm, envir = ws))

  # closures whose enclosure is the workspace, as at top level
  f <- function(v) get_question("tmp_qes_ws_obj", v)
  environment(f) <- ws
  expect_identical(f("x"), "Question x")
  lambda <- function(v) get_question("tmp_qes_ws_obj", v)
  environment(lambda) <- ws
  expect_identical(
    unname(sapply(c("x", "y"), lambda)),
    c("Question x", "Question y")
  )
})

test_that("get_question skips non-list objects and reports missing names", {
  local_qes_notices_shown()
  err <- expect_error(
    get_question("tmp_qes_no_such_object", "x"),
    class = "qesR_error_input"
  )
  expect_identical(err$arg, "do")
  expect_identical(err$value, "tmp_qes_no_such_object")

  dat <- data.frame(x = 1:3)
  attr(dat$x, "label") <- "Outer text"
  outer <- function() {
    tmp_qes_shadow <- dat
    inner <- function() {
      tmp_qes_shadow <- "not a data frame"
      get_question("tmp_qes_shadow", "x")
    }
    inner()
  }
  # mode = "list": the character in the inner frame is skipped
  expect_identical(outer(), "Outer text")

  only_chr <- function() {
    tmp_qes_chr <- "text"
    get_question("tmp_qes_chr", "x")
  }
  err <- expect_error(only_chr(), class = "qesR_error_input")
  expect_identical(err$arg, "do")
})

test_that("get_question reports missing label", {
  local_qes_notices_shown()
  dat <- data.frame(x = 1:3)

  expect_warning(
    out <- get_question(dat, "x"),
    class = "qesR_warning"
  )
  expect_true(is.na(out))
})

test_that("get_question never returns the variable name as the question ([A:K4])", {
  local_qes_notices_shown()
  # qes2018: the file's label of 253 of its 254 columns is the variable name
  dat <- data.frame(responseid = 1:3, lang = 1:3)
  attr(dat$responseid, "label") <- "responseid"
  attr(dat$lang, "label") <- "  Interview   language "
  w <- expect_warning(out <- get_question(dat, "responseid"), class = "qesR_warning")
  expect_identical(out, NA_character_)
  expect_identical(w$id, "question_missing")
  # a label is returned with its white space squished
  expect_identical(get_question(dat, "lang"), "Interview language")
  # the same data, known to be qes2018: the dictionary has no label either
  attr(dat, "qes_survey_code") <- "qes2018"
  expect_warning(out <- get_question(dat, "responseid"), class = "qesR_warning")
  expect_identical(out, NA_character_)
  # variable.labels are cleaned the same way
  dat2 <- data.frame(v1 = 1:2)
  attr(dat2, "variable.labels") <- c(v1 = "V1")
  expect_warning(out <- get_question(dat2, "v1"), class = "qesR_warning")
  expect_identical(out, NA_character_)
})

test_that("get_question resolves column name case-insensitively", {
  local_qes_notices_shown()
  dat <- data.frame(ResponseId = 1:3)
  attr(dat$ResponseId, "label") <- "Response ID"

  expect_true(identical(get_question(dat, "responseid"), "Response ID"))
})

test_that("get_question lists close matches for an ambiguous prefix", {
  local_qes_notices_shown()
  dat <- data.frame(ResponseId = 1:3, ResponseTime = 1:3)
  err <- expect_error(get_question(dat, "response"), class = "qesR_error_unknown_variable")
  expect_identical(err$variables, "response")
  expect_setequal(err$suggestions, c("ResponseId", "ResponseTime"))
})

test_that("get_question provides close-match suggestions", {
  local_qes_notices_shown()
  dat <- data.frame(ResponseId = 1:3, stringsAsFactors = FALSE)

  err <- expect_error(get_question(dat, "response"), class = "qesR_error_unknown_variable")
  expect_identical(err$suggestions, "ResponseId")
})

test_that("get_question('q1') never answers for q10 ([A:A6])", {
  local_qes_notices_shown()
  dat <- data.frame(q10 = 1, q36_1 = 2)
  attr(dat$q10, "label") <- "Question ten"
  err <- expect_error(get_question(dat, "q1"), class = "qesR_error_unknown_variable")
  expect_identical(err$variables, "q1")
  expect_true("q10" %in% err$suggestions)
  expect_error(get_question(dat, "q36"), class = "qesR_error_unknown_variable")
})

test_that("get_question() gives the questionnaire wording of a study's variable", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  expect_identical(get_question(demo, "Q19"), qes_question("qes_demo", "Q19")$question)
  # without the codebook attribute, the study recorded on the data is used
  attr(demo, "qes_codebook") <- NULL
  expect_identical(get_question(demo, "Q19"), qes_question("qes_demo", "Q19")$question)
})
