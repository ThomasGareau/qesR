# Slice S3: get_question(full =) no longer changes the result (design.md
# section 2.3): the text is always the most complete one qesR has, and
# `full = FALSE` prints a once-per-session note. A question cut in its source
# file is flagged (qesR_warning_truncated), never completed by a guess.

test_that("get_question(full = FALSE) returns the same text, with a one-time note", {
  withr::local_options(qesR.quiet_deprecated = TRUE)
  dat <- data.frame(x = 1:3)
  attr(dat$x, "label") <- "How old are you?"
  expect_identical(get_question(dat, "x", full = TRUE), "How old are you?")
  local_qes_once()
  expect_message(
    out <- get_question(dat, "x", full = FALSE),
    class = "qesR_message_arg_ignored"
  )
  expect_identical(out, "How old are you?")
  expect_no_message(get_question(dat, "x", full = FALSE), class = "qesR_message_arg_ignored")
})

test_that("a truncated question is returned as is, with a warning", {
  local_qes_notices_shown()
  label <- strrep("a", 80)
  cb <- data.frame(variable = "x", label = label, question = label, stringsAsFactors = FALSE)
  class(cb) <- c("qes_codebook", "data.frame")
  dat <- data.frame(x = 1)
  dict <- getFromNamespace(".qes_codebook_source", "qesR")(cb)$dict
  dict$variables$question_truncated <- TRUE
  dict$variables$doc_ref <- "7449514"
  attr(cb, "qes_dict") <- c(dict, list(lang = NULL, layout = "compact"))
  attr(dat, "qes_codebook") <- cb
  w <- expect_warning(out <- get_question(dat, "x"), class = "qesR_warning_truncated")
  expect_identical(out, label)
  expect_identical(w$doc_ref, "7449514")
})
