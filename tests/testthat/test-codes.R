# Restored from 397aa0c^ in slice S0b. Triage:
# * "qes2022 has insecure retry fallback enabled" is deleted: it locked in the
#   insecure TLS retry ([A:D1]), which slice S0c removes.
# * The row count is now "at least 11": slice S1 appends the 1998 codes after
#   the old 11 (design.md section 2.3). The first 11 are pinned in
#   test-contract-legacy.R.
# * The DOI test reads the exported get_qescodes(detailed = TRUE) instead of
#   the internal `.qes_catalog` table, which becomes a function in slice S1.

test_that("get_qescodes returns expected structure", {
  codes <- get_qescodes()

  expect_s3_class(codes, "data.frame")
  expect_gte(nrow(codes), 11L)

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

test_that("legacy qes study DOIs match current Borealis records", {
  catalog <- get_qescodes(detailed = TRUE)

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
    expect_identical(row$doi, expected[[code]], info = code)
  }
})
