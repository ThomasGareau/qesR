test_that("codebook API is exported", {
  exports <- getNamespaceExports("qesR")
  needed <- c(
    "get_decon",
    "get_qes_master",
    "get_codebook",
    "format_codebook",
    "get_value_labels",
    "qes_codebook",
    "get_qes_codebook",
    "get_codebook_files",
    "get_qes_codebook_files",
    "download_codebook"
  )

  expect_true(all(needed %in% exports))
})
