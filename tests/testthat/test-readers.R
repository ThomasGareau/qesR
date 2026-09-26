# Restored from 397aa0c^ in slice S0b. Triage: skipped. `.read_text_table()`
# returns 0 rows for this input in any language ([A:D10]: it branches on
# English warning text and misses the "incomplete final line" case). Slice S2b
# reads the pinned original .sav/.dta files instead and deletes the text
# reader; the row-count check then lives in the reader tests (design.md P1).

test_that(".read_text_table handles malformed quoted strings", {
  skip("fixed in S2b: text reader replaced by the original-file reader ([A:D10])")
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
