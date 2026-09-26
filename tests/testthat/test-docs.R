# qes_docs() and the get_codebook_files() adapters (slice S1, design.md
# sections 2.2 and 2.3).

doc_cols <- c("study", "file_id", "file_name", "role", "lang", "format", "bytes", "md5", "url")

test_that("qes_docs() lists every deposited document offline", {
  d <- qes_docs()
  expect_identical(names(d), doc_cols)
  expect_true(all(d$role %in% c("codebook", "questionnaire", "technical_report", "methodology")))
  expect_setequal(unique(d$study), qes_studies()$study)
  expect_true(all(grepl("^[0-9a-f]{32}$", d$md5)))
  expect_true(all(d$url == sprintf(
    "%s/api/access/datafile/%s",
    qes_studies()$server[match(d$study, qes_studies()$study)], d$file_id
  )))
  expect_identical(qes_docs("all"), d)
})

test_that("qes_docs() filters by study, role and language", {
  d <- qes_docs("qes2018")
  expect_identical(d$file_id, c("361049", "361050", "367181", "361045"))
  expect_identical(d$role, c("questionnaire", "questionnaire", "codebook", "methodology"))
  expect_identical(qes_docs("qes2018", lang = "en")$file_id, "361049")
  expect_identical(qes_docs(c("qes2014", "qes2012"), role = "technical_report")$study, c("qes2014", "qes2012"))
  expect_identical(qes_docs(" QES2022 ")$file_id, "7449514")
  # each 1998 survey lists its own codebook
  expect_identical(qes_docs("qes1998_createc")$file_name, "LivredeCodes_CREATEC_1998.pdf")
  expect_identical(nrow(qes_docs("qes2018", role = "technical_report")), 0L)
})

test_that("qes_docs() validates its arguments", {
  expect_error(qes_docs("qes2019"), class = "qesR_error_unknown_study")
  expect_error(qes_docs(role = "data"), class = "qesR_error_input")
  expect_error(qes_docs(lang = "de"), class = "qesR_error_input")
})

test_that("qes_docs() is served by the .qes_catalog() seam", {
  local_fixture_catalog()
  d <- qes_docs()
  expect_identical(d$file_id, c("102", "202"))
  expect_identical(d$url[1], "https://dataverse.example.org/api/access/datafile/102")
})

test_that("get_codebook_files() returns the documents with the v0.4.4 columns", {
  local_qes_notices_shown()
  files <- get_codebook_files("qes2014")
  expect_identical(names(files), v044_codebook_files_cols)
  docs <- qes_docs("qes2014")
  expect_identical(files$file_id, docs$file_id)
  expect_identical(files$filename, docs$file_name)
  expect_identical(files$extension, docs$format)
  expect_identical(files$size, docs$bytes)
  expect_identical(files$download_url, docs$url)
  expect_identical(get_qes_codebook_files("qes2014"), files)
})

test_that("get_codebook_files() lists the whole shared 1998 deposit, as in 0.4.4", {
  local_qes_notices_shown()
  files <- get_codebook_files("qes1998")
  expect_identical(nrow(files), 3L)
  expect_setequal(files$file_id, c("332049", "332050", "332051"))
  # the new codes list only their own codebook
  expect_identical(get_codebook_files("qes1998_crop")$file_id, "332049")
})

test_that("get_codebook_files() reads the study of a codebook", {
  local_qes_notices_shown()
  cb <- structure(
    data.frame(variable = "x", stringsAsFactors = FALSE),
    class = c("qes_codebook", "data.frame"),
    survey_code = "qes2018"
  )
  expect_identical(get_codebook_files(codebook = cb)$file_id, qes_docs("qes2018")$file_id)
  expect_error(get_codebook_files(), class = "qesR_error_input")
})

test_that("get_codebook_files() notes once that file and refresh are ignored", {
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  expect_identical(
    count_class(get_codebook_files("qes2014", file = "SPSS", refresh = TRUE), "qesR_message_arg_ignored"),
    2L
  )
  expect_identical(
    count_class(get_codebook_files("qes2014", file = "SPSS", refresh = TRUE), "qesR_message_arg_ignored"),
    0L
  )
  expect_identical(
    count_class(get_codebook_files("qes2014"), "qesR_message_arg_ignored"),
    0L
  )
})

test_that("the codebook-file wrappers announce qes_docs() once each", {
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = NULL)
  expect_identical(count_class(get_codebook_files("qes2014"), "qesR_message_deprecated"), 1L)
  expect_identical(count_class(get_qes_codebook_files("qes2014"), "qesR_message_deprecated"), 1L)
  expect_identical(count_class(get_codebook_files("qes2014"), "qesR_message_deprecated"), 0L)
})
