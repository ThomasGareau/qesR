test_that("get_qes validates survey code", {
  expect_error(
    get_qes("not_a_real_code", quiet = TRUE),
    "Unknown survey code"
  )

  expect_error(
    get_codebook("not_a_real_code", quiet = TRUE),
    "Unknown survey code"
  )
})

test_that("get_preview validates obs before download", {
  expect_error(
    get_preview("qes2018", obs = 0),
    "obs"
  )
})
