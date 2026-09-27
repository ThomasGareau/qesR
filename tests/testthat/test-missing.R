# qes_missing() (slice S3, design.md sections 2.2 and 5.2).

test_that("qes_missing() sets typed codes to NA and logs what it did", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  out <- qes_missing(demo, variables = c("Q19", "Q28"), quiet = TRUE)
  expect_identical(names(out), names(demo))
  expect_identical(nrow(out), nrow(demo))
  q19 <- unclass(demo$Q19)
  expect_identical(sum(is.na(out$Q19)), sum(q19 %in% c(8, 9)))
  expect_false(any(unclass(out$Q19) %in% c(8, 9)))
  # labels and attributes are kept
  expect_identical(attr(out$Q19, "labels"), attr(demo$Q19, "labels"))
  expect_identical(attr(out$Q19, "label"), attr(demo$Q19, "label"))
  expect_identical(attr(out, "qes_provenance"), attr(demo, "qes_provenance"))
  # untouched columns are identical
  expect_identical(out$Q2, demo$Q2)
  log <- attr(out, "qes_missing_log")
  expect_identical(names(log), c("variable", "value", "missing_type", "n_set"))
  expect_identical(sum(log$n_set[log$variable == "Q19"]), sum(q19 %in% c(8, 9)))
  expect_setequal(log$missing_type, c("dk", "refused"))
})

test_that("action = 'tagged' keeps the reason in a tagged NA", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  out <- qes_missing(demo, variables = "Q19", action = "tagged", quiet = TRUE)
  tags <- haven::na_tag(out$Q19)
  expect_identical(sum(tags %in% "d"), sum(unclass(demo$Q19) == 8))
  expect_identical(sum(tags %in% "r"), sum(unclass(demo$Q19) == 9))
  expect_true(all(is.na(out$Q19[!is.na(tags)])))
})

test_that("types selects the missing types; spoiled and not_selected are kept by default", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  only_dk <- qes_missing(demo, variables = "Q19", types = "dk", quiet = TRUE)
  expect_identical(sum(is.na(only_dk$Q19)), sum(unclass(demo$Q19) == 8))
  expect_error(qes_missing(demo, types = "nope"), class = "qesR_error_input")
  expect_error(qes_missing(demo, types = "sysmis"), class = "qesR_error_input")
  expect_error(qes_missing(demo, action = "drop"), class = "qesR_error_input")
  # the default leaves spoiled ballots alone (qes2018 q6 = 95)
  d <- data.frame(q6 = haven::labelled(c(1, 95, 99, NA), labels = c(A = 1, B = 95, C = 99)))
  attr(d, "qes_survey_code") <- "qes2018"
  out <- qes_missing(d, quiet = TRUE)
  expect_identical(unclass(out$q6)[1:3], c(1, 95, NA))
  out <- qes_missing(d, types = c("spoiled", "refused"), quiet = TRUE)
  expect_true(all(is.na(unclass(out$q6))[2:3]))
})

test_that("declared SPSS missing codes are user_na; untyped variables are counted", {
  local_qes_notices_shown()
  x <- haven::labelled(c(1, 2, 7, 97, NA), labels = c(Oui = 1, Non = 2))
  attr(x, "qes_na_values") <- c(97)
  d <- data.frame(a = x, b = c(1, 2, 3, 4, 5))
  attr(d$a, "qes_na_values") <- 97
  attr(d, "qes_survey_code") <- "qes_demo"
  expect_message(out <- qes_missing(d), class = "qesR_message_missing_untyped")
  expect_identical(unclass(out$a)[1:4], c(1, 2, 7, NA))
  log <- attr(out, "qes_missing_log")
  expect_identical(log$missing_type, "user_na")
  expect_identical(log$n_set, 1L)
  expect_identical(out$b, d$b)
  expect_no_message(qes_missing(d, quiet = TRUE))
})

test_that("declared codes with a known meaning keep it: not_voted is left alone by default", {
  local_qes_notices_shown()
  # qes1998 q2post: 8 "n'a pas vot\u00e9" is declared missing in the SPSS file
  x <- haven::labelled(c(1, 2, 8, 8, NA), labels = c(a = 1, b = 2, "n'a pas vot\u00e9" = 8))
  attr(x, "qes_na_values") <- 8
  d <- data.frame(q2post = seq_len(5))
  d$q2post <- x
  attr(d, "qes_survey_code") <- "qes1998"
  out <- qes_missing(d, quiet = TRUE)
  expect_identical(as.numeric(unclass(out$q2post)), c(1, 2, 8, 8, NA))
  out <- qes_missing(d, types = "not_voted", quiet = TRUE)
  expect_identical(as.numeric(unclass(out$q2post)), c(1, 2, NA, NA, NA))
  expect_identical(attr(out, "qes_missing_log")$missing_type, "not_voted")
})

test_that("a declared code that is an answer is never recoded", {
  local_qes_notices_shown()
  # qes2007_panel vote: 6 "Un autre parti" is declared missing (6..10) but
  # is an answer; 7 is a spoiled ballot, 10 not reached in the post wave
  x <- haven::labelled(c(1, 6, 7, 10, 9), labels = c(ADQ = 1, "Un autre parti" = 6, "A annul\u00e9 son vote" = 7, "non rejoint" = 10))
  attr(x, "qes_na_range") <- c(6, 10)
  d <- data.frame(vote = seq_len(5))
  d$vote <- x
  attr(d, "qes_survey_code") <- "qes2007_panel"
  all_types <- getFromNamespace(".qes_missing_types", "qesR")()
  out <- qes_missing(d, types = all_types, quiet = TRUE)
  expect_identical(as.numeric(unclass(out$vote)), c(1, 6, NA, NA, NA))
  log <- attr(out, "qes_missing_log")
  expect_false("6" %in% log$value)
  expect_identical(log$missing_type[log$value == "7"], "spoiled")
  expect_identical(log$missing_type[log$value == "10"], "not_in_wave")
  # the default recodes neither the answer nor the spoiled ballot
  out <- qes_missing(d, quiet = TRUE)
  expect_identical(as.numeric(unclass(out$vote)), c(1, 6, 7, NA, NA))
})

test_that("qes_missing() needs data that records its study", {
  expect_error(qes_missing(data.frame(a = 1)), class = "qesR_error_no_provenance")
  expect_error(qes_missing("qes2014"), class = "qesR_error_input")
  demo <- suppressMessages(get_qes("qes_demo", quiet = TRUE))
  err <- expect_error(qes_missing(demo, variables = "q19"), class = "qesR_error_unknown_variable")
  expect_true("Q19" %in% err$suggestions)
})
