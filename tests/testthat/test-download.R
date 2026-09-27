# qes_download() and the download_codebook() wrapper (slice S2c; design.md
# sections 2.2, 2.3, 4.2 and 4.3). Everything is offline: files come from the
# fake Dataverse (helper-fake-dataverse.R) or from canned responses, and are
# written only into temporary directories.

part_files_in <- function(dir) {
  list.files(dir, pattern = "\\.part$", all.files = TRUE)
}

# ---- selection -----------------------------------------------------------------------

test_that("role = NULL selects the pinned data file and every document", {
  select <- qesR:::.qes_download_select
  data <- select("qes2014", "data")
  expect_identical(data$file_id, "425916")
  docs <- select("qes2014", "docs")
  expect_setequal(docs$file_id, c("352010", "352009", "352011"))
  both <- select(c("qes2014", "qes2012"), c("data", "docs"))
  expect_identical(unique(both$study), c("qes2014", "qes2012"))
  expect_identical(nrow(both), 4L + 4L)
})

test_that("naming roles selects every file of those roles, twins and donors included", {
  select <- qesR:::.qes_download_select
  expect_setequal(select("qes2014", "data", role = "data")$file_id, c("425916", "425915"))
  expect_identical(select("qes2012", "data", role = "label_donor")$file_id, "425917")
  q <- select("qes2014", c("data", "docs"), role = "questionnaire")
  expect_setequal(q$file_id, c("352010", "352009"))
  fr <- select("qes2018", c("data", "docs"), lang = "fr")
  # lang filters documents only: the data file stays
  expect_true("425914" %in% fr$file_id)
  expect_true(all(fr$lang[fr$role != "data"] == "fr"))
})

test_that("local names keep the deposited name, safe, with the format's extension", {
  name <- qesR:::.qes_download_name
  row <- data.frame(
    file_id = "361045", file_name = "Rapport méthodologique 2018", original_file_name = "Rapport méthodologique 2018",
    format = "pdf", ingested = FALSE, stringsAsFactors = FALSE
  )
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    expect_identical(name(row), "Rapport méthodologique 2018.pdf")
  } else {
    expect_identical(name(row), "361045.pdf")
  }
  row$file_name <- "a/b\\c:d.pdf"
  expect_identical(name(row), "a_b_c_d.pdf")
  row$file_name <- ".."
  expect_identical(name(row), "361045.pdf")
  data_row <- data.frame(
    file_id = "425914", file_name = "Quebec Election Study 2018.tab",
    original_file_name = "Quebec Election Study 2018.dta", format = "dta", ingested = TRUE,
    stringsAsFactors = FALSE
  )
  expect_identical(name(data_row), "Quebec Election Study 2018.dta")
  row$file_name <- "IPsos.SAV"
  row$format <- "sav"
  expect_identical(name(row), "IPsos.SAV")
})

test_that("a name the native encoding cannot hold falls back to the file id", {
  # what happens in the C locale, whatever the locale the tests run in
  local_mocked_bindings(
    .qes_native_name_ok = function(x) !grepl("[^ -~]", x, useBytes = TRUE),
    .package = "qesR"
  )
  name <- qesR:::.qes_download_name
  row <- data.frame(
    file_id = "361045", file_name = "Rapport méthodologique 2018", original_file_name = "Rapport méthodologique 2018",
    format = "pdf", ingested = FALSE, stringsAsFactors = FALSE
  )
  expect_identical(name(row), "361045.pdf")
  row$file_name <- "Questionnaire québécois.docx"
  row$format <- NA_character_
  expect_identical(name(row), "361045.docx")
  row$file_name <- "Questionnaire 2018.docx"
  expect_identical(name(row), "Questionnaire 2018.docx")
  # no transliteration: the check is on the name as deposited
  expect_true(qesR:::.qes_native_name_ok("plain ascii.pdf"))
})

test_that("documents with accented names download in a locale that cannot write them", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  local_mocked_bindings(
    .qes_native_name_ok = function(x) !grepl("[^ -~]", x, useBytes = TRUE),
    .package = "qesR"
  )
  dir <- withr::local_tempdir()
  out <- qes_download("qes2018", path = dir, what = "docs", lang = "fr", quiet = TRUE)
  expect_setequal(out$file_id, c("361050", "367181", "361045"))
  expect_true(all(file.exists(out$local_path)))
  expect_false(any(grepl("[^ -~]", basename(out$local_path), useBytes = TRUE)))
  expect_true("361045.pdf" %in% basename(out$local_path))
  expect_identical(unname(tools::md5sum(out$local_path)), out$md5)
  expect_length(part_files_in(dir), 0L)
})

# ---- pinned downloads ------------------------------------------------------------------

test_that("the pinned data file is saved, md5-checked, and the result is invisible", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dir <- withr::local_tempdir()
  expect_invisible(out <- qes_download("qes2018", path = dir, quiet = TRUE))
  expect_identical(
    names(out),
    c("study", "file_id", "file_name", "role", "lang", "md5", "local_path",
      "from_cache", "downloaded", "pinned")
  )
  expect_identical(out$study, "qes2018")
  expect_identical(out$file_id, "425914")
  expect_identical(out$file_name, "qes2018.sav")
  expect_true(out$downloaded)
  expect_true(out$pinned)
  expect_false(out$from_cache)
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "qes2018.sav")
  expect_identical(normalizePath(out$local_path), normalizePath(file.path(dir, "qes2018.sav")))
  expect_identical(unname(tools::md5sum(out$local_path)), out$md5)
  expect_identical(out$md5, log$catalog$files$md5[log$catalog$files$file_id == "425914"])
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/425914?format=original")
})

test_that("documents are saved with their deposited names and filtered by role and language", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dir <- withr::local_tempdir()
  out <- qes_download("qes2018", path = dir, what = "docs", lang = "fr", quiet = TRUE)
  expect_true(all(out$lang == "fr"))
  expect_setequal(out$file_id, c("361050", "367181", "361045"))
  expect_true(all(file.exists(out$local_path)))
  # the methodological report is deposited without an extension; a locale
  # that cannot write its accented name gets the file id instead
  report <- if (isTRUE(l10n_info()[["UTF-8"]])) {
    "Rapport méthodologique de l'Étude électorale québécoise 2018.pdf"
  } else {
    "361045.pdf"
  }
  expect_true(report %in% basename(out$local_path))
  expect_identical(unname(tools::md5sum(out$local_path)), out$md5)
  expect_true(all(grepl("^https://borealisdata\\.ca/api/access/datafile/[0-9]+$", log$urls)))

  dir2 <- withr::local_tempdir()
  q <- qes_download("qes2018", path = dir2, what = c("data", "docs"), role = "questionnaire", quiet = TRUE)
  expect_identical(q$role, c("questionnaire", "questionnaire"))
  expect_length(list.files(dir2), 2L)
})

test_that("nothing is written, and nothing requested, when no file matches", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dir <- withr::local_tempdir()
  expect_message(
    out <- qes_download("qes2022", path = dir, what = "docs", lang = "fr"),
    class = "qesR_message_download"
  )
  expect_identical(nrow(out), 0L)
  expect_identical(names(out), names(qesR:::.qes_download_empty()))
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(log$urls, 0L)
  expect_silent(out <- qes_download("qes_demo", path = dir, what = "docs", quiet = TRUE))
  prov <- qes_provenance(out)
  expect_s3_class(prov, "qes_provenance")
  expect_identical(nrow(prov), 0L)
  expect_identical(names(prov), names(qesR:::.qes_provenance_empty()))
  expect_identical(qes_cite(out), qes_cite())
})

test_that("invalid arguments are input errors and create nothing", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dir <- withr::local_tempdir()
  missing_dir <- file.path(dir, "not-there")
  expect_error(qes_download("qes2018"), class = "qesR_error_input")
  err <- expect_error(qes_download("qes2018", path = missing_dir), class = "qesR_error_input")
  expect_identical(err$arg, "path")
  expect_false(dir.exists(missing_dir))
  expect_error(qes_download("qes2018", path = c(dir, dir)), class = "qesR_error_input")
  expect_error(qes_download("qes2018", path = dir, what = "codebook"), class = "qesR_error_input")
  err <- expect_error(qes_download("qes2018", path = dir, role = "codebook"), class = "qesR_error_input")
  expect_identical(err$arg, "role")
  expect_error(qes_download("qes2018", path = dir, version = "newest"), class = "qesR_error_input")
  expect_error(qes_download("qes2018", path = dir, version = c("pinned", "pinned")), class = "qesR_error_input")
  expect_error(qes_download("qes2018", path = dir, lang = "de"), class = "qesR_error_input")
  expect_error(qes_download("qes2018", path = dir, overwrite = NA), class = "qesR_error_input")
  expect_error(qes_download("2018", path = dir), class = "qesR_error_unknown_study")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(log$urls, 0L)
})

test_that("a file already there with the expected md5 is kept, not downloaded again", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dir <- withr::local_tempdir()
  first <- qes_download("qes2018", path = dir, quiet = TRUE)
  n <- length(log$urls)
  before <- file.info(first$local_path)$mtime
  n_kept <- count_class(again <- qes_download("qes2018", path = dir), "qesR_message_cached")
  expect_identical(n_kept, 1L)
  expect_false(again$downloaded)
  expect_length(log$urls, n)
  expect_identical(file.info(again$local_path)$mtime, before)
  prov <- attr(again, "qes_provenance")
  expect_identical(prov$retrieved_via, "user_data")
  expect_true(prov$md5_verified)
})

test_that("a different file of the same name stops everything unless overwrite = TRUE", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dir <- withr::local_tempdir()
  mine <- file.path(dir, "qes2018.sav")
  writeLines("my own file", mine)
  err <- expect_error(
    qes_download("qes2018", path = dir, what = c("data", "docs"), quiet = TRUE),
    class = "qesR_error_input"
  )
  expect_identical(err$arg, "overwrite")
  expect_identical(normalizePath(err$paths), normalizePath(mine))
  # checked before anything is written: the documents were not saved either
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "qes2018.sav")
  expect_identical(readLines(mine), "my own file")
  expect_length(log$urls, 0L)

  out <- qes_download("qes2018", path = dir, overwrite = TRUE, quiet = TRUE)
  expect_true(out$downloaded)
  expect_identical(unname(tools::md5sum(mine)), out$md5)
})

# ---- md5 before the final name (the slice's exit gate) ------------------------------------

test_that("a download that fails its md5 check never gets its final name", {
  local_qes_notices_shown()
  local_fake_dataverse()
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      fake_response(url, dest, body = "tampered bytes")
    },
    .package = "qesR"
  )
  dir <- withr::local_tempdir()
  err <- expect_error(
    qes_download("qes2018", path = dir, what = "docs", role = "codebook", quiet = TRUE),
    class = "qesR_error_checksum"
  )
  expect_identical(err$file_id, "367181")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("the copy into `path` is checked again before it is renamed", {
  local_qes_notices_shown()
  local_fake_dataverse()
  bad <- withr::local_tempfile()
  writeLines("changed after the cache checked it", bad)
  testthat::local_mocked_bindings(
    .qes_local_file = function(study_row, file_row, quiet = TRUE) {
      structure(bad, retrieved_via = "session_cache")
    },
    .package = "qesR"
  )
  dir <- withr::local_tempdir()
  expect_error(qes_download("qes2018", path = dir, quiet = TRUE), class = "qesR_error_checksum")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
  expect_length(part_files_in(dir), 0L)
  # an existing file is not replaced by a copy that fails the check
  mine <- file.path(dir, "qes2018.sav")
  writeLines("mine", mine)
  expect_error(qes_download("qes2018", path = dir, overwrite = TRUE, quiet = TRUE), class = "qesR_error_checksum")
  expect_identical(readLines(mine), "mine")
  expect_length(part_files_in(dir), 0L)
})

test_that("an interrupted download leaves no file in `path`", {
  local_qes_notices_shown()
  local_fake_dataverse()
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      if (!is.null(dest)) writeLines("half a file", dest)
      stop(fake_curl_error("curl_error_url_malformat", "interrupted"))
    },
    .package = "qesR"
  )
  dir <- withr::local_tempdir()
  expect_error(qes_download("qes2018", path = dir, quiet = TRUE), class = "qesR_error_network")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

# ---- sources and provenance -------------------------------------------------------------------

test_that("qes_demo is copied from the package with no request", {
  local_qes_notices_shown()
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  dir <- withr::local_tempdir()
  out <- qes_download("qes_demo", path = dir, quiet = TRUE)
  expect_identical(out$file_name, "qes_demo.sav")
  expect_identical(
    unname(tools::md5sum(out$local_path)),
    unname(tools::md5sum(system.file("extdata", "demo", "data", "qes_demo.sav", package = "qesR")))
  )
  prov <- qes_provenance(out)
  expect_s3_class(prov, "qes_provenance")
  expect_identical(prov$retrieved_via, "local_demo")
  expect_true(prov$pinned)
  expect_true(is.na(prov$reader))
  expect_error(qes_download("qes_demo", path = dir, version = "latest"), class = "qesR_error_input")
})

test_that("a file already in the download cache is copied from it", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  withr::local_options(qesR.cache = "disk", qesR.cache_dir = withr::local_tempdir())
  dir <- withr::local_tempdir()
  first <- qes_download("qes2018", path = dir, quiet = TRUE)
  expect_false(first$from_cache)
  expect_identical(attr(first, "qes_provenance")$retrieved_via, "network")
  dir2 <- withr::local_tempdir()
  second <- qes_download("qes2018", path = dir2, quiet = TRUE)
  expect_true(second$from_cache)
  expect_identical(attr(second, "qes_provenance")$retrieved_via, "disk_cache")
  expect_length(log$urls, 1L)
})

test_that("the result records study-level provenance and can be cited", {
  local_qes_notices_shown()
  local_fake_dataverse()
  dir <- withr::local_tempdir()
  out <- qes_download(c("qes2018", "qes2014"), path = dir, what = c("data", "docs"), role = c("data", "codebook"), quiet = TRUE)
  prov <- qes_provenance(out)
  expect_identical(nrow(prov), nrow(out))
  expect_identical(prov$file_id, out$file_id)
  expect_true(all(prov$md5_verified))
  expect_identical(prov$md5_observed, out$md5)
  expect_true(all(prov$pinned))
  expect_identical(attr(prov$retrieved_at, "tzone"), "UTC")
  cites <- qes_cite(out)
  expect_length(cites, 3L)
  expect_identical(qes_cite(prov), cites)
})

# ---- version = "latest" -------------------------------------------------------------------------

# A server for the fixture catalog (tests/testthat/fixtures/catalog): the
# dataset metadata `json` for every deposit, and the bodies in `bodies`
# (named by file id) for file requests.
local_latest_server <- function(json, bodies = list(), .env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$urls <- character(0)
  local_clear_latest_memo(.env = .env)
  withr::local_options(qesR.cache = "none", .local_envir = .env)
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      log$urls <- c(log$urls, url)
      if (grepl("/api/datasets/", url, fixed = TRUE)) {
        return(fake_response(url, dest, write = function(path) jsonlite::write_json(json, path, auto_unbox = TRUE)))
      }
      id <- sub("^.*/api/access/datafile/([0-9]+).*$", "\\1", url)
      body <- bodies[[id]]
      if (is.null(body)) {
        stop(sprintf("test server: no file %s", id), call. = FALSE)
      }
      fake_response(url, dest, body = body)
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR",
    .env = .env
  )
  log
}

md5_of <- function(text) {
  f <- tempfile()
  on.exit(unlink(f))
  writeBin(charToRaw(text), f)
  unname(tools::md5sum(f))
}

latest_dataset <- function(files, version = c(2L, 0L), state = "RELEASED") {
  list(
    status = "OK",
    data = list(
      versionNumber = version[1], versionMinorNumber = version[2], versionState = state,
      files = files
    )
  )
}

replaced_data_file <- function(body = "new data bytes", md5 = md5_of(body), original = "a_v2.sav") {
  list(
    restricted = FALSE,
    dataFile = list(
      id = 111L, filename = "a_v2.tab", originalFileName = original, tabularData = TRUE,
      originalFileSize = nchar(body), md5 = md5, previousDataFileId = 101L, rootDataFileId = 101L
    )
  )
}

test_that("version = 'latest' fetches the replacing file, checked, with a warning", {
  local_qes_notices_shown()
  local_fixture_catalog()
  log <- local_latest_server(latest_dataset(list(replaced_data_file())), list("111" = "new data bytes"))
  dir <- withr::local_tempdir()
  expect_warning(
    out <- qes_download("qes2018", path = dir, version = "latest", quiet = TRUE),
    class = "qesR_warning_unpinned"
  )
  expect_identical(out$file_id, "111")
  expect_identical(out$file_name, "a_v2.sav")
  expect_identical(out$md5, md5_of("new data bytes"))
  expect_false(out$pinned)
  expect_identical(readBin(out$local_path, "raw", 100), charToRaw("new data bytes"))
  prov <- qes_provenance(out)
  expect_false(prov$pinned)
  expect_identical(prov$dataset_version, "2.0")
  expect_true(is.na(prov$unf))
  expect_identical(log$urls, c(
    "https://dataverse.example.org/api/datasets/:persistentId/versions/:latest-published?persistentId=doi:10.9999/FIX/AAAAAA&includeDeaccessioned=true",
    "https://dataverse.example.org/api/access/datafile/111?format=original"
  ))
})

test_that("a latest file that fails its md5, is missing or is deaccessioned writes nothing", {
  local_qes_notices_shown()
  local_fixture_catalog()
  dir <- withr::local_tempdir()

  local_latest_server(
    latest_dataset(list(replaced_data_file(md5 = strrep("0", 32)))),
    list("111" = "new data bytes")
  )
  expect_warning(
    expect_error(
      qes_download("qes2018", path = dir, version = "latest", quiet = TRUE),
      class = "qesR_error_checksum"
    ),
    class = "qesR_warning_unpinned"
  )
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)

  log <- local_latest_server(latest_dataset(list(list(dataFile = list(id = 999L, md5 = strrep("a", 32), filename = "other.pdf")))))
  err <- expect_error(
    qes_download("qes2018", path = dir, version = "latest", quiet = TRUE),
    class = "qesR_error_source"
  )
  expect_identical(err$file_id, "101")
  expect_length(log$urls, 1L)

  local_latest_server(latest_dataset(list(replaced_data_file()), state = "DEACCESSIONED"))
  expect_error(
    qes_download("qes2018", path = dir, version = "latest", quiet = TRUE),
    class = "qesR_error_source"
  )

  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      stop(fake_curl_error("curl_error_couldnt_resolve_host", "offline"))
    },
    .package = "qesR"
  )
  local_clear_latest_memo()
  err <- expect_error(
    qes_download("qes2018", path = dir, version = "latest", quiet = TRUE),
    class = "qesR_error_source"
  )
  expect_s3_class(err$parent, "qesR_error_offline")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("a latest file of another format is saved and recorded with that format", {
  local_qes_notices_shown()
  local_fixture_catalog()
  local_latest_server(
    latest_dataset(list(replaced_data_file(original = "a_v2.dta"))),
    list("111" = "new data bytes")
  )
  dir <- withr::local_tempdir()
  out <- suppressWarnings(qes_download("qes2018", path = dir, version = "latest", quiet = TRUE))
  expect_identical(basename(out$local_path), "a_v2.dta")
  expect_identical(qes_provenance(out)$format, "dta")
})

test_that("files written before a latest file fails stay, with the unpinned warning", {
  local_qes_notices_shown()
  local_fixture_catalog()
  bad_doc <- list(dataFile = list(
    id = 102L, filename = "a_fr.pdf", md5 = strrep("0", 32), filesize = 5L, rootDataFileId = -1L
  ))
  local_latest_server(
    latest_dataset(list(replaced_data_file(), bad_doc)),
    list("111" = "new data bytes", "102" = "other")
  )
  dir <- withr::local_tempdir()
  expect_warning(
    expect_error(
      qes_download("qes2018", path = dir, what = c("data", "docs"), version = "latest", quiet = TRUE),
      class = "qesR_error_checksum"
    ),
    class = "qesR_warning_unpinned"
  )
  expect_identical(list.files(dir, all.files = TRUE, no.. = TRUE), "a_v2.sav")
})

test_that("version and level take the match.arg() default and abbreviations", {
  local_qes_notices_shown()
  dir <- withr::local_tempdir()
  out <- qes_download("qes_demo", path = dir, version = c("pinned", "latest"), quiet = TRUE)
  expect_true(out$pinned)
  unlink(out$local_path)
  out <- qes_download("qes_demo", path = dir, version = "pin", quiet = TRUE)
  expect_true(out$pinned)
  expect_identical(nrow(qes_provenance("qes_demo", level = c("study", "cell", "spec"))), 1L)
  expect_identical(nrow(qes_provenance("qes_demo", level = "st")), 1L)
  expect_error(qes_provenance("qes_demo", level = "x"), class = "qesR_error_input")
})

test_that("a failure to write into `path` is an input error about `path`", {
  local_qes_notices_shown()
  dir <- withr::local_tempdir()
  testthat::local_mocked_bindings(
    .qes_finish_part = function(part, dest, verify = NULL) {
      .qes_abort("cache_write", class = "qesR_error_cache", args = list(dest),
        data = list(path = dest, reason = "write"))
    },
    .package = "qesR"
  )
  err <- expect_error(qes_download("qes_demo", path = dir, quiet = TRUE), class = "qesR_error_input")
  expect_identical(err$arg, "path")
  expect_s3_class(err$parent, "qesR_error_cache")
  expect_length(list.files(dir, all.files = TRUE, no.. = TRUE), 0L)
})

test_that("an unchanged file in the latest version is found by its id", {
  local_qes_notices_shown()
  local_fixture_catalog()
  body <- "pinned bytes"
  same <- list(dataFile = list(
    id = 102L, filename = "a_fr.pdf", md5 = md5_of(body), filesize = nchar(body), rootDataFileId = -1L
  ))
  local_latest_server(latest_dataset(list(same), version = c(1L, 1L)), list("102" = body))
  dir <- withr::local_tempdir()
  out <- suppressWarnings(qes_download("qes2018", path = dir, what = "docs", version = "latest", quiet = TRUE))
  expect_identical(out$file_id, "102")
  expect_identical(basename(out$local_path), "a_fr.pdf")
  expect_identical(qes_provenance(out)$dataset_version, "1.1")
})

# ---- download_codebook() (legacy) --------------------------------------------------------------

test_that("download_codebook() saves the catalog documents, md5-checked, with the 0.4.4 columns", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  dest <- file.path(withr::local_tempdir(), "new", "dir")
  out <- download_codebook("qes2018", dest_dir = dest, quiet = TRUE)
  expect_identical(names(out), v044_download_codebook_cols)
  expect_identical(out$file_id, qes_docs("qes2018")$file_id)
  expect_true(all(out$downloaded))
  expect_true(all(file.exists(out$local_path)))
  md5 <- log$catalog$files$md5[match(out$file_id, log$catalog$files$file_id)]
  expect_identical(unname(tools::md5sum(out$local_path)), md5)
  expect_false(any(grepl("/metadata/ddi", log$urls, fixed = TRUE)))

  # an existing file is kept as it is unless overwrite = TRUE (as in 0.4.4)
  writeLines("mine", out$local_path[1])
  again <- download_codebook("qes2018", dest_dir = dest, quiet = TRUE)
  expect_false(any(again$downloaded))
  expect_identical(readLines(out$local_path[1]), "mine")
  again <- download_codebook("qes2018", dest_dir = dest, quiet = TRUE, overwrite = TRUE)
  expect_true(all(again$downloaded))
  expect_identical(unname(tools::md5sum(out$local_path)), md5)
})

test_that("download_codebook(file =) selects documents by name; dest_dir is created only when needed", {
  local_qes_notices_shown()
  local_fake_dataverse()
  base <- withr::local_tempdir()
  out <- download_codebook("qes2018", dest_dir = file.path(base, "a"), file = "EN\\.doc$", quiet = TRUE)
  expect_identical(out$filename, "Quebec Election Study 2018 EN.doc")
  none <- file.path(base, "b")
  expect_message(
    empty <- download_codebook("qes2018", dest_dir = none, file = "no such document"),
    class = "qesR_message_download"
  )
  expect_identical(names(empty), v044_download_codebook_cols)
  expect_identical(nrow(empty), 0L)
  expect_false(dir.exists(none))
  expect_error(download_codebook("qes2018", dest_dir = none, file = "("), class = "qesR_error_input")
})

test_that("download_codebook() lists the whole 1998 deposit, as in 0.4.4", {
  local_qes_notices_shown()
  local_fake_dataverse()
  out <- download_codebook("qes1998", dest_dir = withr::local_tempdir(), quiet = TRUE)
  expect_setequal(out$file_id, c("332051", "332049", "332050"))
})

test_that("download_codebook(refresh = TRUE) says once that refresh is ignored", {
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  local_fake_dataverse()
  dest <- withr::local_tempdir()
  n <- count_class(
    download_codebook("qes2018", dest_dir = dest, quiet = TRUE, refresh = TRUE),
    "qesR_message_arg_ignored"
  )
  expect_identical(n, 1L)
})

# ---- live (never on CRAN) ------------------------------------------------------------------

test_that("live: qes_download() saves one small document, md5-verified", {
  skip_on_cran()
  skip_if_offline("borealisdata.ca")
  skip_if_not(identical(Sys.getenv("QESR_LIVE"), "true"), "set QESR_LIVE=true for live tests")
  local_qes_notices_shown()
  withr::local_options(qesR.cache = "none")
  catalog <- getFromNamespace(".qes_catalog", "qesR")()
  docs <- catalog$files[catalog$files$role != "data" & !catalog$files$ingested, , drop = FALSE]
  docs <- docs[grepl("borealisdata", catalog$studies$server[match(docs$study, catalog$studies$study)]), , drop = FALSE]
  row <- docs[which.min(docs$bytes), , drop = FALSE]
  dir <- withr::local_tempdir()
  out <- qes_download(row$study, path = dir, what = "docs", role = row$role, lang = row$lang, quiet = TRUE)
  expect_true(row$file_id %in% out$file_id)
  expect_identical(unname(tools::md5sum(out$local_path)), out$md5)
  expect_length(list.files(dir, pattern = "\\.part$", all.files = TRUE), 0L)
})
