test_that("get_question full mode can fall back to label when question is truncated", {
  dat <- data.frame(x = 1:3)
  attr(dat$x, "qes_question") <- "To make sure we are talking to"
  attr(dat$x, "label") <- "How old are you?"

  expect_true(identical(get_question(dat, "x", full = TRUE), "How old are you?"))
  expect_true(identical(get_question(dat, "x", full = FALSE), "To make sure we are talking to"))
})
