# Restored from 397aa0c^ in slice S0b. Triage: kept. The messages are matched
# in English under withr::local_language("en"); slice S0c switches these
# tests to condition classes (qesR_error_unknown_study, qesR_error_input),
# after which no test matches message text (design.md section 7).

# Each test runs against the offline fake, so a regression that reaches the
# transport is caught (no request logged) instead of downloading on CRAN.

test_that("get_qes validates survey code", {
  log <- local_fake_dataverse()
  withr::local_language("en")
  expect_error(
    get_qes("not_a_real_code", quiet = TRUE),
    "Unknown survey code"
  )

  expect_error(
    get_codebook("not_a_real_code", quiet = TRUE),
    "Unknown survey code"
  )
  expect_length(log$urls, 0L)
})

test_that("get_preview validates obs before download", {
  log <- local_fake_dataverse()
  withr::local_language("en")
  expect_error(
    get_preview("qes2018", obs = 0),
    "obs"
  )
  expect_length(log$urls, 0L)
})

test_that("unknown codes raise qesR_error_unknown_study", {
  skip("fixed in S0c: condition classes")
  local_fake_dataverse()
  expect_error(get_qes("not_a_real_code", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_error(get_codebook("not_a_real_code", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_error(get_preview("qes2018", obs = 0), class = "qesR_error_input")
})
