# fn:multiselect (R/hz-data.R): a select-all-that-apply question stored as
# one 0/1 variable per option, read into one level (spec 4.3.0).

test_that("one level ticked gives it; two levels are not_mappable; none is no_answer", {
  d <- data.frame(a = c(1, -99, 1, -99, NA, -99), b = c(-99, 1, 1, -99, NA, -99), c = c(-99, -99, -99, -99, NA, 1))
  row <- data.frame(args = "select=a:english,b:french,c:other;selected=1", stringsAsFactors = FALSE)
  res <- .qes_hz_fn_multiselect(d$a, list(data = d, row = row))
  expect_identical(res$value, c("english", "french", NA, NA, NA, "other"))
  expect_identical(res$na_reason, c(NA, NA, "not_mappable", "no_answer", "sysmis", NA))
})

test_that("options of one level ticked together give that level", {
  d <- data.frame(a = c(-99, 1), b = c(1, 1), c = c(1, -99))
  row <- data.frame(args = "select=a:english,b:other,c:other", stringsAsFactors = FALSE)
  res <- .qes_hz_fn_multiselect(d$a, list(data = d, row = row))
  expect_identical(res$value, c("other", NA))
  expect_identical(res$na_reason, c(NA, "not_mappable"))
})

test_that("the registered function reads every variable of its row", {
  expect_true("multiselect" %in% names(.qes_hz_fns))
  xw <- data.frame(rule = "fn:multiselect", source_var = "cps_lang_1",
                   args = "select=cps_lang_1:english,cps_lang_2:french,cps_lang_3:other;selected=1",
                   stringsAsFactors = FALSE)
  expect_identical(.qes_hz_row_vars(xw, 1L), c("cps_lang_1", "cps_lang_2", "cps_lang_3"))
  xw <- hz_spec()$tables$crosswalk
  i <- which(xw$study == "qes2022" & xw$target == "lang_mother")
  expect_identical(xw$rule[i], "fn:multiselect")
})
