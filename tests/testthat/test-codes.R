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

test_that("legacy qes study DOIs match current Borealis records", {
  catalog <- qesR:::.qes_catalog

  expected <- c(
    qes2018_panel = "10.5683/SP3/XDDMMR",
    qes2014 = "10.5683/SP3/64F7WR",
    qes2012 = "10.5683/SP2/WXUPXT",
    qes2012_panel = "10.5683/SP3/RKHPVL",
    qes_crop_2007_2010 = "10.5683/SP3/IRZ1PF",
    qes2008 = "10.5683/SP2/8KEYU3",
    qes2007 = "10.5683/SP2/6XGOKA",
    qes2007_panel = "10.5683/SP3/NDS6VT",
    qes1998 = "10.5683/SP2/QFUAWG"
  )

  for (code in names(expected)) {
    row <- catalog[catalog$qes_survey_code == code, , drop = FALSE]
    expect_true(identical(row$doi, expected[[code]]))
  }
})
