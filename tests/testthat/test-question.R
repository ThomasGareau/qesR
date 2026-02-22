test_that("get_question works for data.frame and object name", {
  dat <- data.frame(x = 1:3)
  attr(dat$x, "label") <- "Example question text"

  expect_true(identical(get_question(dat, "x"), "Example question text"))

  assign("tmp_qes_obj", dat, envir = .GlobalEnv)
  on.exit(rm(tmp_qes_obj, envir = .GlobalEnv), add = TRUE)

  expect_true(identical(get_question("tmp_qes_obj", "x"), "Example question text"))
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

test_that("get_question provides close-match suggestions", {
  dat <- data.frame(ResponseId = 1:3, stringsAsFactors = FALSE)

  expect_error(
    get_question(dat, "response"),
    "Close matches"
  )
})
