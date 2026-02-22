test_that(".read_text_table handles malformed quoted strings", {
  tmp <- tempfile(fileext = ".tab")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(
    c(
      "a\tb",
      "1\t\"unterminated",
      "2\tok"
    ),
    tmp
  )

  out <- qesR:::.read_text_table(tmp)

  expect_s3_class(out, "data.frame")
  expect_true(identical(ncol(out), 2L))
  expect_true(identical(nrow(out), 2L))
})
