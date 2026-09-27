# qes_studies(check_updates = TRUE), offline (slice S1, design.md section 2.2).
# The transport is replaced by canned Dataverse responses; nothing reaches the
# network: the seam is .qes_transport(url, dest, handle) (slice S2a).

latest_json <- function(version, files, state = "RELEASED") {
  v <- strsplit(version, ".", fixed = TRUE)[[1]]
  list(
    status = "OK",
    data = list(
      versionNumber = as.integer(v[1]),
      versionMinorNumber = as.integer(v[2]),
      versionState = state,
      files = lapply(names(files), function(id) {
        list(dataFile = list(id = as.integer(id), md5 = files[[id]]))
      })
    )
  )
}

local_update_server <- function(responses, .env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$urls <- character(0)
  local_clear_latest_memo(.env = .env)
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      log$urls <- c(log$urls, url)
      doi <- sub("&.*$", "", sub("^.*persistentId=doi:", "", url))
      res <- responses[[doi]]
      if (is.null(res)) {
        stop(fake_curl_error("curl_error_couldnt_resolve_host", "offline"))
      }
      fake_response(url, dest, write = function(path) jsonlite::write_json(res, path, auto_unbox = TRUE))
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR",
    .env = .env
  )
  log
}

test_that("each deposit is checked once and compared with its pins", {
  local_fixture_catalog()
  log <- local_update_server(list(
    "10.9999/FIX/AAAAAA" = latest_json("1.0", list("101" = "0123456789abcdef0123456789abcdef")),
    "10.9999/FIX/BBBBBB" = latest_json("3.0", list("201" = "00112233445566778899aabbccddeeff"))
  ))
  s <- qes_studies(check_updates = TRUE, quiet = TRUE)
  expect_identical(names(s)[31:33], c("latest_version", "latest_md5", "status"))
  expect_identical(s$status, c("current", "new_version_same_file"))
  expect_identical(s$latest_version, c("1.0", "3.0"))
  expect_identical(length(log$urls), 2L)
  expect_identical(
    log$urls[1],
    "https://dataverse.example.org/api/datasets/:persistentId/versions/:latest-published?persistentId=doi:10.9999/FIX/AAAAAA&includeDeaccessioned=true"
  )
})

test_that("a changed or missing data file, a deaccession and a failure are reported", {
  local_fixture_catalog()
  local_update_server(list(
    "10.9999/FIX/AAAAAA" = latest_json("1.1", list("101" = "ffffffffffffffffffffffffffffffff"))
  ))
  s <- qes_studies(check_updates = TRUE, quiet = TRUE)
  expect_identical(s$status, c("data_changed", "unreachable"))
  expect_identical(s$latest_md5, c("ffffffffffffffffffffffffffffffff", NA))

  local_update_server(list(
    "10.9999/FIX/AAAAAA" = latest_json("2.0", list("999" = "ffffffffffffffffffffffffffffffff")),
    "10.9999/FIX/BBBBBB" = latest_json("2.1", list("201" = "00112233445566778899aabbccddeeff"), state = "DEACCESSIONED")
  ))
  s <- qes_studies(check_updates = TRUE, quiet = TRUE)
  expect_identical(s$status, c("data_changed", "deaccessioned"))
})

test_that("the three 1998 studies share one request, and quiet silences progress", {
  log <- local_update_server(list())
  n <- count_class(
    s <- qes_studies(family = "polls_1998", check_updates = TRUE),
    "qesR_message_download"
  )
  expect_identical(n, 1L)
  expect_identical(length(log$urls), 1L)
  expect_identical(s$status, rep("unreachable", 3L))
  expect_silent(qes_studies(family = "polls_1998", check_updates = TRUE, quiet = TRUE))
})

test_that("an answer is memoized for the session, a failure is not", {
  local_fixture_catalog()
  log <- local_update_server(list(
    "10.9999/FIX/AAAAAA" = latest_json("1.0", list("101" = "0123456789abcdef0123456789abcdef"))
  ))
  qes_studies(check_updates = TRUE, quiet = TRUE)
  expect_identical(length(log$urls), 2L)
  s <- qes_studies(check_updates = TRUE, quiet = TRUE)
  # the answered deposit is not asked again; the unreachable one is
  expect_identical(length(log$urls), 3L)
  expect_identical(s$status, c("current", "unreachable"))
  qes_cache_clear()
  qes_studies(check_updates = TRUE, quiet = TRUE)
  expect_identical(length(log$urls), 5L)
})
