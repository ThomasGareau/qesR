# qes_provenance() at study level (slice S2c; design.md sections 2.2 and
# 5.9) and the get_preview() wrapper. Offline: the demo study ships with
# qesR, and other data comes from the fake Dataverse.

provenance_cols <- c(
  "study", "doi", "dataset_version", "file_id", "file_name", "format",
  "md5_expected", "md5_observed", "md5_verified", "unf", "n_rows", "n_cols",
  "pinned", "retrieved_via", "retrieved_at", "licence", "label_source",
  "label_file_id", "name_map_applied", "reader", "haven_version",
  "catalog_version", "dict_version"
)

test_that("qes_provenance() returns the record get_qes() attaches", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  prov <- qes_provenance(demo)
  expect_s3_class(prov, c("qes_provenance", "data.frame"), exact = TRUE)
  expect_identical(names(prov), provenance_cols)
  expect_identical(nrow(prov), 1L)
  expect_identical(prov$study, "qes_demo")
  expect_identical(prov$md5_observed, prov$md5_expected)
  expect_true(prov$md5_verified)
  expect_true(prov$pinned)
  expect_identical(prov$retrieved_via, "local_demo")
  expect_identical(as.data.frame(prov), as.data.frame(attr(demo, "qes_provenance")))
  # a qes_provenance object is its own record
  expect_identical(qes_provenance(prov), prov)
})

test_that("for study codes it shows what get_qes() would read, without reading it", {
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  prov <- qes_provenance(c(" QES2012 ", "qes2007_panel"))
  expect_identical(names(prov), provenance_cols)
  expect_identical(prov$study, c("qes2012", "qes2007_panel"))
  files <- qesR:::.qes_catalog()$files
  expect_identical(prov$file_id, c("425918", "352415"))
  expect_identical(prov$md5_expected, files$md5[match(prov$file_id, files$file_id)])
  expect_true(all(is.na(prov$md5_observed)))
  expect_true(all(is.na(prov$md5_verified)))
  expect_true(all(is.na(prov$retrieved_via)))
  expect_true(all(is.na(prov$retrieved_at)))
  expect_true(all(prov$pinned))
  # the qes2012 labels come from the SPSS twin; the 2007 panel restores 3 names
  expect_identical(prov$label_file_id, c("425917", NA))
  expect_identical(prov$name_map_applied, c(0L, 3L))
  expect_identical(prov$reader, c("haven::read_dta()", "haven::read_sav(user_na = TRUE)"))
  expect_identical(qes_provenance("all")$study, qes_studies()$study)
})

test_that("an object without its record is an error, with a hint", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  merged <- merge(demo, data.frame(extra_column = 1))
  expect_error(qes_provenance(merged), class = "qesR_error_no_provenance")
  expect_error(qes_provenance(NULL), class = "qesR_error_no_provenance")
  expect_error(qes_provenance(1:3), class = "qesR_error_no_provenance")
  expect_error(qes_provenance("2018"), class = "qesR_error_unknown_study")
  # selecting rows keeps the record
  expect_identical(qes_provenance(demo[1:5, ])$file_id, "0")
})

test_that("cell and spec levels belong to harmonized data", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  err <- expect_error(qes_provenance(demo, level = "cell"), class = "qesR_error_no_provenance")
  expect_identical(err$level, "cell")
  expect_error(qes_provenance("qes2018", level = "spec"), class = "qesR_error_no_provenance")
  expect_error(qes_provenance(demo, level = "row"), class = "qesR_error_input")
})

test_that("print() writes one paragraph per file, in the message language", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  prov <- qes_provenance(demo)
  withr::local_options(qesR.lang = "en")
  out <- capture.output(res <- withVisible(print(prov)))
  expect_false(res$visible)
  expect_identical(res$value, prov)
  text <- paste(out, collapse = " ")
  expect_match(text, "qes_demo", fixed = TRUE)
  expect_match(text, prov$md5_observed, fixed = TRUE)
  expect_match(text, "haven::read_sav(user_na = TRUE)", fixed = TRUE)

  codes <- paste(capture.output(print(qes_provenance("qes2018"))), collapse = " ")
  expect_match(codes, "https://doi.org/", fixed = TRUE)
  expect_match(codes, qes_provenance("qes2018")$md5_expected, fixed = TRUE)

  withr::local_options(qesR.lang = "fr")
  fr <- paste(capture.output(print(prov)), collapse = " ")
  expect_false(identical(fr, text))
  expect_match(fr, prov$md5_observed, fixed = TRUE)

  # the returned value never depends on the language
  expect_identical(qes_provenance("qes2018")[, setdiff(provenance_cols, "retrieved_at")],
    withr::with_options(list(qesR.lang = "en"), qes_provenance("qes2018"))[, setdiff(provenance_cols, "retrieved_at")])

  # a record missing columns prints as a plain data frame
  expect_output(print(prov[, c("study", "file_id")]), "qes_demo")
})

test_that("get_preview() is head() of get_qes(): same record, served from memory", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  withr::local_options(qesR.memo = TRUE)
  qesR:::.qes_memo_clear()
  withr::defer(qesR:::.qes_memo_clear())
  full <- get_qes("qes2018", quiet = TRUE)
  n <- length(log$urls)
  prev <- get_preview("qes2018", obs = 2)
  expect_length(log$urls, n)
  expect_identical(nrow(prev), 2L)
  expect_identical(prev, utils::head(full, 2L))
  expect_identical(attr(prev, "qes_provenance"), attr(full, "qes_provenance"))
  expect_identical(attr(prev, "qes_survey_code"), "qes2018")
  expect_identical(qes_provenance(prev)$file_id, "425914")
})
