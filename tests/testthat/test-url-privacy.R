# URLs are built from catalog fields only (design.md sections 4.1 and 4.6).

url <- function(...) getFromNamespace(".qes_url", "qesR")(...)

test_that(".qes_url() builds the three Dataverse endpoints", {
  expect_identical(
    url("https://borealisdata.ca", "original", file_id = "425916"),
    "https://borealisdata.ca/api/access/datafile/425916?format=original"
  )
  expect_identical(
    url("https://borealisdata.ca", "file", file_id = "361045"),
    "https://borealisdata.ca/api/access/datafile/361045"
  )
  expect_identical(
    url("https://dataverse.harvard.edu", "dataset", doi = "10.7910/DVN/PAQBDR", version = "1.1"),
    "https://dataverse.harvard.edu/api/datasets/:persistentId/versions/1.1?persistentId=doi:10.7910/DVN/PAQBDR"
  )
  expect_error(url("http://borealisdata.ca", "file", file_id = "1"))
  expect_error(url("https://borealisdata.ca", "file", file_id = "1;rm"))
  expect_error(url("https://borealisdata.ca", "dataset", doi = "10.1/x", version = "latest"))
})

test_that("URLs depend on catalog fields only, not on the user or machine", {
  build <- function() {
    c(
      qes_docs()$url,
      url("https://borealisdata.ca", "original", file_id = "425916"),
      url("https://borealisdata.ca", "dataset", doi = "10.5683/SP3/NWTGWS", version = ":latest-published")
    )
  }
  base <- build()
  dir <- withr::local_tempdir()
  withr::with_envvar(
    c(USER = "someone-else", EMAIL = "someone@example.org", HOME = dir, LOGNAME = "someone-else"),
    withr::with_dir(dir, expect_identical(build(), base))
  )
  for (var in c("USER", "EMAIL", "LOGNAME")) {
    value <- Sys.getenv(var)
    if (nchar(value) >= 4L) {
      pattern <- paste0("(^|[^A-Za-z0-9])", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", value), "([^A-Za-z0-9]|$)")
      expect_false(any(grepl(pattern, base, perl = TRUE)), info = var)
    }
  }
})
