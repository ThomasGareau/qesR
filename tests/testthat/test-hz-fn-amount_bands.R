# fn:amount_bands (R/hz-data.R, spec 4.4.0, relaxed rows only): an amount
# in bands, with a bracket question for those who gave no amount; and the
# option first=TRUE of fn:multiselect.

amount_spec <- function() {
  s <- hz_spec()
  ps <- .qes_rx_pseudo_spec(s)
  ps$tables$valuemaps <- rbind(ps$tables$valuemaps, .qes_apply_schema(data.frame(
    map_id = "rx_test_brackets", source_code = c("1", "2", "3", "-99"), source_label = NA_character_,
    source_label_hash = NA_character_, source_label_origin = NA_character_,
    target_code = c("1", "2", "3", NA), na_reason = c(NA, NA, NA, "no_answer"), alias_exception = NA_character_,
    note = NA_character_, stringsAsFactors = FALSE
  ), "spec_valuemaps"))
  ps
}

test_that("amounts fall in their band; the codes of na_codes pass to the brackets", {
  ps <- amount_spec()
  row <- data.frame(target = "income_cat", args = "breaks=100,200;levels=low,middle,high;then=b:rx_test_brackets",
                    na_codes = "-99=no_answer;0=no_answer", stringsAsFactors = FALSE)
  d <- data.frame(a = c(50, 100, 199, 200, 5000, 0, -99, 0, -99, NA, -5),
                  b = c(NA, NA, NA, NA, NA, 1, 3, -99, NA, NA, NA))
  res <- .qes_hz_fn_amount_bands(d$a, list(data = d, row = row, spec = ps))
  expect_identical(res$value, c("low", "middle", "middle", "high", "high", "low", "high", NA, NA, NA, NA))
  expect_identical(res$na_reason, c(NA, NA, NA, NA, NA, NA, NA, "no_answer", "no_answer", "sysmis", "unmapped"))
})

test_that("the registered function reads the amount and the bracket question", {
  expect_true("amount_bands" %in% names(.qes_hz_fns))
  xw <- data.frame(rule = "fn:amount_bands", source_var = "cps_income",
                   args = "breaks=52200,95600;levels=low,middle,high;then=cps_income2:rx_x",
                   stringsAsFactors = FALSE)
  expect_identical(.qes_hz_row_vars(xw, 1L), c("cps_income", "cps_income2"))
  rm <- .qes_rx_tables(hz_spec())$maps
  i <- which(rm$study == "qes2022" & rm$column == "income_cat")
  expect_identical(rm$rule[i], "fn:amount_bands")
})

test_that("multiselect with first=TRUE takes the first option ticked, in order", {
  d <- data.frame(a = c(1, -99, 1, -99, NA), b = c(-99, 1, 1, -99, NA), c = c(-99, -99, 1, -99, NA))
  row <- data.frame(args = "select=b:french,a:english,c:other;selected=1;first=TRUE", stringsAsFactors = FALSE)
  res <- .qes_hz_fn_multiselect(d$b, list(data = d, row = row))
  expect_identical(res$value, c("english", "french", "french", NA, NA))
  expect_identical(res$na_reason, c(NA, NA, NA, "no_answer", "sysmis"))
})
