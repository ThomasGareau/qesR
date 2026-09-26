# Restored from 397aa0c^ in slice S0b; switched to condition classes in slice
# S0c. No test matches message text (design.md section 7): tests assert the
# condition class and its fields.

# Each test runs against the offline fake, so a regression that reaches the
# transport is caught (no request logged) instead of downloading on CRAN.

test_that("get_qes validates survey code", {
  log <- local_fake_dataverse()
  expect_error(get_qes("not_a_real_code", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_error(
    withr::with_options(list(qesR.quiet_deprecated = TRUE), get_codebook("not_a_real_code", quiet = TRUE)),
    class = "qesR_error_unknown_study"
  )
  expect_length(log$urls, 0L)
})

test_that("get_preview validates obs before download", {
  log <- local_fake_dataverse()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  err <- expect_error(get_preview("qes2018", obs = 0), class = "qesR_error_input")
  expect_identical(err$arg, "obs")
  expect_error(get_preview("qes2018", obs = NA), class = "qesR_error_input")
  expect_error(get_preview("qes2018", obs = 2.5), class = "qesR_error_input")
  expect_error(get_preview("qes2018", obs = Inf), class = "qesR_error_input")
  expect_length(log$urls, 0L)
})

test_that("unknown codes carry the code and near-match suggestions", {
  log <- local_fake_dataverse()
  err <- expect_error(get_qes("QES 2022", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_s3_class(err, "qesR_error")
  expect_identical(err$study, "QES 2022")
  expect_true("qes2022" %in% err$suggestions)

  # "2018" is never auto-resolved, only suggested
  err <- expect_error(get_qes("2018", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_true(all(c("qes2018", "qes2018_panel") %in% err$suggestions))

  err <- expect_error(get_qes("zzzzzzzzzz", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_identical(err$suggestions, character(0))
  expect_length(log$urls, 0L)
})

test_that("study codes are trimmed and case-insensitive, never fuzzy", {
  local_fake_dataverse()
  dat <- get_qes(" QES2018 ", assign_global = FALSE, quiet = TRUE)
  expect_identical(attr(dat, "qes_survey_code"), "qes2018")
  expect_error(get_qes("qes201", quiet = TRUE), class = "qesR_error_unknown_study")
})

test_that("invalid argument types raise qesR_error_input", {
  log <- local_fake_dataverse()
  for (bad in list(NA_character_, "", " ", 1, c("qes2018", "qes2014"))) {
    err <- expect_error(get_qes(bad, quiet = TRUE), class = "qesR_error_input")
    expect_identical(err$arg, "srvy")
  }
  expect_error(get_qes_master(surveys = character(0), quiet = TRUE), class = "qesR_error_input")
  expect_error(get_qes_master(surveys = c("all", "qes2018"), quiet = TRUE), class = "qesR_error_input")
  err <- expect_error(
    get_qes_master(surveys = c("qes2018", "nope"), quiet = TRUE),
    class = "qesR_error_unknown_study"
  )
  expect_identical(err$study, "nope")
  expect_length(log$urls, 0L)
})

test_that("a file pattern that matches nothing raises qesR_error_ambiguous_file", {
  local_fake_dataverse()
  err <- expect_error(
    get_qes("qes2018", file = "no_such_file", quiet = TRUE),
    class = "qesR_error_ambiguous_file"
  )
  expect_identical(err$study, "qes2018")
  expect_identical(err$pattern, "no_such_file")
  expect_true("qes2018.sav" %in% err$candidates)
})
