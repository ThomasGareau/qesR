test_that(".build_decon maps and preserves row count", {
  dat <- data.frame(
    cps_citizen = c(1, 2),
    cps_age_in_years = c(30, 45),
    cps_genderid = c(1, 2),
    cps_province = c(11, 11),
    cps_edu = c(3, 4),
    stringsAsFactors = FALSE
  )

  out <- qesR:::.build_decon(dat, srvy = "qes2022")

  expect_s3_class(out, "data.frame")
  expect_true(identical(nrow(out), 2L))
  expect_true(all(c("qes_code", "citizenship", "age", "gender") %in% names(out)))
  expect_true(all(out$qes_code == "qes2022"))
})
