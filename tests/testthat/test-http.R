# Interim transport (slice S0c; design.md sections 4.1, 4.6 and P8).
# The network call itself (.qes_download_url) is mocked: nothing here
# touches the network.

test_that("the User-Agent is exactly qesR/<version> R/<version>", {
  expect_identical(
    qesR:::.qes_user_agent(),
    sprintf("qesR/%s R/%s", utils::packageVersion("qesR"), getRversion())
  )
})

test_that("a request carries the package User-Agent, and the option is restored", {
  seen <- NULL
  local_mocked_bindings(
    .qes_download_url = function(url, destfile, quiet = TRUE) {
      seen <<- getOption("HTTPUserAgent")
      writeLines("ok", destfile)
      0L
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR"
  )
  withr::local_options(HTTPUserAgent = "before")
  dest <- withr::local_tempfile()
  qesR:::.qes_fetch_file("https://borealisdata.ca/api/access/datafile/1", dest)
  expect_identical(seen, qesR:::.qes_user_agent())
  expect_identical(getOption("HTTPUserAgent"), "before")
  expect_true(file.exists(dest))
})

test_that("a failed request is a qesR_error_network with its root cause, and leaves no file", {
  local_mocked_bindings(
    .qes_download_url = function(url, destfile, quiet = TRUE) {
      writeLines("partial", destfile)
      warning("HTTP status was '503 Service Unavailable'")
      stop("cannot open URL")
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR"
  )
  dest <- withr::local_tempfile()
  url <- "https://borealisdata.ca/api/access/datafile/2"
  err <- expect_error(qesR:::.qes_fetch_file(url, dest), class = "qesR_error_network")
  expect_s3_class(err, "qesR_error")
  expect_identical(err$url, url)
  expect_identical(err$attempts, 1L)
  expect_s3_class(err$parent, "error")
  expect_length(err$warnings, 1L)
  expect_false(file.exists(dest))
})

test_that("a failed request is never retried, with or without TLS checks", {
  calls <- 0L
  local_mocked_bindings(
    .qes_download_url = function(url, destfile, quiet = TRUE) {
      calls <<- calls + 1L
      stop("SSL certificate problem: unable to get local issuer certificate")
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR"
  )
  withr::local_options(download.file.extra = NULL)
  dest <- withr::local_tempfile()
  expect_error(
    qesR:::.qes_fetch_file("https://dataverse.harvard.edu/api/access/datafile/3", dest),
    class = "qesR_error_network"
  )
  expect_identical(calls, 1L)
  expect_null(getOption("download.file.extra"))
})

test_that("warnings of a successful request are passed on", {
  local_mocked_bindings(
    .qes_download_url = function(url, destfile, quiet = TRUE) {
      writeLines("ok", destfile)
      warning("a server note")
      0L
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR"
  )
  dest <- withr::local_tempfile()
  expect_warning(
    qesR:::.qes_fetch_file("https://borealisdata.ca/api/access/datafile/4", dest),
    "a server note"
  )
})

test_that("requests to one host are at least one second apart", {
  waits <- numeric(0)
  # a controlled clock: sleeping advances it, and the test moves it by hand
  now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  local_mocked_bindings(
    .qes_download_url = function(url, destfile, quiet = TRUE) {
      writeLines("ok", destfile)
      0L
    },
    .qes_now = function() now,
    .qes_sleep = function(seconds) {
      waits <<- c(waits, seconds)
      now <<- now + seconds
    },
    .package = "qesR"
  )
  state <- qesR:::.qes_http_state
  rm(list = ls(state, all.names = TRUE), envir = state)
  withr::defer(rm(list = ls(state, all.names = TRUE), envir = state))

  dest <- withr::local_tempfile()
  qesR:::.qes_fetch_file("https://borealisdata.ca/api/access/datafile/5", dest)
  expect_length(waits, 0L)
  now <- now + 0.2
  qesR:::.qes_fetch_file("https://borealisdata.ca/api/access/datafile/6", dest)
  expect_length(waits, 1L)
  expect_equal(waits[1], 0.8, tolerance = 1e-6)
  # another host has its own clock
  qesR:::.qes_fetch_file("https://dataverse.harvard.edu/api/access/datafile/7", dest)
  expect_length(waits, 1L)
  # a request after more than a second does not wait
  now <- now + 1.5
  qesR:::.qes_fetch_file("https://borealisdata.ca/api/access/datafile/8", dest)
  expect_length(waits, 1L)
})

test_that("request URLs contain no personal information from the environment", {
  withr::local_envvar(
    USER = "qesrprivacyuser",
    LOGNAME = "qesrprivacylogname",
    EMAIL = "qesr.privacy@example.org",
    HOME = withr::local_tempdir()
  )
  log <- local_fake_dataverse()
  get_qes("qes2018", assign_global = FALSE, quiet = TRUE)
  expect_gt(length(log$urls), 0L)
  for (value in c(
    "qesrprivacyuser", "qesrprivacylogname", "qesr.privacy@example.org",
    "qesr.privacy%40example.org", "qesr.privacy", "example.org"
  )) {
    expect_false(any(grepl(value, log$urls, fixed = TRUE)), info = value)
  }
  expect_true(all(startsWith(log$urls, "https://borealisdata.ca/api/")))
})
