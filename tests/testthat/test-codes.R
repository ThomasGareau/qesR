test_that("get_qescodes returns expected structure", {
  codes <- get_qescodes()

  expect_s3_class(codes, "data.frame")
  expect_true(identical(nrow(codes), 11L))

  required_cols <- c(
    "index",
    "qes_survey_code",
    "get_qes_call_char"
  )

  expect_true(all(required_cols %in% names(codes)))
})

test_that("get_qescodes detailed mode adds metadata columns", {
  codes <- get_qescodes(detailed = TRUE)
  required_cols <- c("year", "name_en", "name_fr", "doi", "doi_url", "documentation")
  expect_true(all(required_cols %in% names(codes)))
})

test_that("qes2022 has insecure retry fallback enabled", {
  catalog <- qesR:::.qes_catalog
  row <- catalog[catalog$qes_survey_code == "qes2022", , drop = FALSE]
  expect_true(isTRUE(row$allow_insecure_retry))
})
