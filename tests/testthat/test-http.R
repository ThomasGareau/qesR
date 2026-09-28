# HTTP transport (slice S2a; design.md sections 4.1, 4.6 and 8.1).
# The network seam .qes_transport() is replaced with canned responses and the
# sleep and clock seams with fakes: nothing here touches the network or waits.

# Replace the transport with `responses`, a list served in order (each a
# function(url, dest) returning a response, or a condition to signal). Records
# every call and every sleep. Returns the log environment.
local_transport <- function(responses, .env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$urls <- character(0)
  log$dests <- list()
  log$handles <- list()
  log$sleeps <- numeric(0)
  # a fake clock that only sleeping moves
  log$now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  state <- qesR:::.qes_http_state
  rm(list = ls(state, all.names = TRUE), envir = state)
  withr::defer(rm(list = ls(state, all.names = TRUE), envir = state), envir = .env)
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      log$urls <- c(log$urls, url)
      log$dests[[length(log$dests) + 1L]] <- dest
      log$handles[[length(log$handles) + 1L]] <- handle
      i <- length(log$urls)
      if (i > length(responses)) {
        stop("test transport: no response left", call. = FALSE)
      }
      r <- responses[[i]]
      if (inherits(r, "condition")) {
        stop(r)
      }
      r(url, dest)
    },
    .qes_handle = function() qesR:::.qes_handle_options(),
    .qes_sleep = function(seconds) {
      log$sleeps <- c(log$sleeps, seconds)
      log$now <- log$now + seconds
    },
    .qes_now = function() log$now,
    .package = "qesR",
    .env = .env
  )
  log
}

ok <- function(body = "payload", headers = list()) {
  function(url, dest) fake_response(url, dest, 200L, headers, body = body)
}

status <- function(code, headers = list(), body = "") {
  function(url, dest) fake_response(url, dest, code, headers, body = body)
}

u <- "https://borealisdata.ca/api/access/datafile/1"

part_files <- function(dir) {
  list.files(dir, pattern = "\\.part$", all.files = TRUE)
}

# ---- identity and privacy ------------------------------------------------------

test_that("the User-Agent is exactly qesR/<version> R/<version>", {
  ua <- qesR:::.qes_user_agent()
  expect_identical(ua, sprintf("qesR/%s R/%s", utils::packageVersion("qesR"), getRversion()))
  expect_false(grepl("@", ua, fixed = TRUE))
  expect_true(grepl("^qesR/[0-9.-]+ R/[0-9.]+$", ua))
})

test_that("the curl handle follows redirects, has timeouts and sends only the User-Agent", {
  opts <- qesR:::.qes_handle_options()
  expect_setequal(
    names(opts),
    c("useragent", "followlocation", "maxredirs", "connecttimeout", "low_speed_limit", "low_speed_time")
  )
  expect_identical(opts$useragent, qesR:::.qes_user_agent())
  expect_true(opts$followlocation)
  expect_identical(opts$maxredirs, 5L)
  expect_identical(opts$connecttimeout, 30L)
  expect_identical(opts$low_speed_limit, 1024L)
  expect_identical(opts$low_speed_time, 60L)
  withr::local_options(qesR.stall_timeout = 15)
  expect_identical(qesR:::.qes_handle_options()$low_speed_time, 15L)
  # a real handle can be built from these options
  expect_s3_class(qesR:::.qes_handle(), "curl_handle")
  withr::local_options(qesR.stall_timeout = -1)
  expect_error(qesR:::.qes_handle_options(), class = "qesR_error_input")
})

test_that("each request is sent with the package handle", {
  log <- local_transport(list(ok()))
  qesR:::.qes_request(u)
  expect_identical(log$handles[[1]]$useragent, qesR:::.qes_user_agent())
})

test_that("the User-Agent and URLs carry no personal information", {
  withr::local_envvar(
    USER = "qesrprivacyuser", LOGNAME = "qesrprivacylogname",
    EMAIL = "qesr.privacy@example.org", HOME = withr::local_tempdir()
  )
  log <- local_fake_dataverse()
  get_qes("qes2018", assign_global = FALSE, quiet = TRUE)
  expect_gt(length(log$urls), 0L)
  sent <- c(log$urls, qesR:::.qes_user_agent(), unlist(qesR:::.qes_handle_options()))
  for (value in c(
    "qesrprivacyuser", "qesrprivacylogname", "qesr.privacy@example.org",
    "qesr.privacy%40example.org", "qesr.privacy", "example.org"
  )) {
    expect_false(any(grepl(value, sent, fixed = TRUE)), info = value)
  }
  expect_true(all(startsWith(log$urls, "https://borealisdata.ca/api/")))
})

test_that("no header or URL contains this machine's user name or e-mail", {
  sent <- c(qesR:::.qes_user_agent(), unlist(lapply(qesR:::.qes_handle_options(), as.character)))
  for (var in c("USER", "EMAIL", "LOGNAME")) {
    value <- Sys.getenv(var)
    if (nchar(value) >= 4L) {
      pattern <- paste0("(^|[^A-Za-z0-9])", gsub("([.|()\\^{}+$*?\\[\\]\\\\])", "\\\\\\1", value), "([^A-Za-z0-9]|$)")
      expect_false(any(grepl(pattern, sent, perl = TRUE)), info = var)
    }
  }
})

# ---- success ---------------------------------------------------------------------

test_that("a 200 is written through a .part file to its destination", {
  log <- local_transport(list(ok("hello")))
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "out.txt")
  qesR:::.qes_fetch(u, dest)
  expect_identical(readLines(dest, warn = FALSE), "hello")
  expect_match(basename(log$dests[[1]]), "\\.part$")
  expect_identical(
    normalizePath(dirname(log$dests[[1]]), winslash = "/"),
    normalizePath(dir, winslash = "/")
  )
  expect_length(part_files(dir), 0L)
})

test_that("a request without a destination returns the body in memory", {
  local_transport(list(ok('{"status":"OK","data":{"x":1}}')))
  expect_identical(qesR:::.qes_fetch_json(u)$data$x, 1L)
})

test_that("a redirect is followed by curl; a 3xx that reaches qesR is an error", {
  # curl follows redirects itself (followlocation), so the transport returns
  # the final 200 and its URL
  log <- local_transport(list(function(url, dest) {
    r <- fake_response(url, dest, 200L, body = "ok")
    r$url <- "https://s3.example.org/bucket/file?signature=x"
    r
  }))
  res <- qesR:::.qes_request(u)
  expect_identical(res$status, 200L)
  log <- local_transport(list(status(303L, list(location = "https://elsewhere.example.org"))))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_http")
  expect_identical(err$status, 303L)
  expect_identical(length(log$urls), 1L)
})

test_that("a successful download says so once, unless quiet", {
  local_transport(list(ok(), ok()))
  dir <- withr::local_tempdir()
  expect_identical(
    count_class(qesR:::.qes_fetch(u, file.path(dir, "a"), quiet = FALSE, what = "a.sav"), "qesR_message_download"),
    1L
  )
  expect_silent(qesR:::.qes_fetch(u, file.path(dir, "b"), quiet = TRUE, what = "b.sav"))
})

# ---- retries -------------------------------------------------------------------------

test_that("503 twice then 200 retries with exponential backoff", {
  log <- local_transport(list(status(503L), status(503L), ok()))
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "f")
  qesR:::.qes_fetch(u, dest)
  expect_identical(length(log$urls), 3L)
  expect_length(log$sleeps, 2L)
  expect_true(log$sleeps[1] >= 1 && log$sleeps[1] <= 3)
  expect_true(log$sleeps[2] >= 2 && log$sleeps[2] <= 6)
  expect_true(file.exists(dest))
  expect_length(part_files(dir), 0L)
})

test_that("the backoff does not touch the random number stream", {
  withr::local_seed(1)
  before <- .Random.seed
  local_transport(list(status(503L), ok()))
  qesR:::.qes_request(u)
  expect_identical(.Random.seed, before)
})

test_that("429 with Retry-After in seconds waits that long", {
  log <- local_transport(list(status(429L, list(`retry-after` = "7")), ok()))
  qesR:::.qes_request(u)
  expect_identical(log$sleeps, 7)
})

test_that("Retry-After as an HTTP date is honoured", {
  log <- local_transport(list(status(503L, list(`retry-after` = "Thu, 01 Jan 2026 12:00:30 GMT")), ok()))
  log$now <- as.POSIXct("2026-01-01 12:00:00", tz = "GMT")
  qesR:::.qes_request(u)
  expect_identical(log$sleeps, 30)
})

test_that("a Retry-After date in the past waits the one-second floor", {
  log <- local_transport(list(status(503L, list(`retry-after` = "Thu, 01 Jan 2026 11:59:00 GMT")), ok()))
  log$now <- as.POSIXct("2026-01-01 12:00:00", tz = "GMT")
  qesR:::.qes_request(u)
  expect_identical(log$sleeps, 1)
})

test_that("a Retry-After over 120 seconds is an error, not a long wait", {
  log <- local_transport(list(status(503L, list(`retry-after` = "600"))))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_http")
  expect_identical(err$retry_after, 600)
  expect_identical(err$status, 503L)
  expect_length(log$sleeps, 0L)
})

test_that("retries stop after qesR.max_tries attempts", {
  withr::local_options(qesR.max_tries = 3)
  log <- local_transport(rep(list(status(502L)), 5))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_http")
  expect_identical(err$attempts, 3L)
  expect_identical(length(log$urls), 3L)
  expect_length(log$sleeps, 2L)
  withr::local_options(qesR.max_tries = 0)
  expect_error(qesR:::.qes_request(u), class = "qesR_error_input")
})

test_that("a 404 fails at once and points to the update check", {
  log <- local_transport(list(status(404L, list(`content-type` = "application/json"),
    body = '{"status":"ERROR","message":"File not found for id 1"}')))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_http")
  expect_identical(err$id, "http_not_found")
  expect_identical(err$status, 404L)
  expect_identical(err$server_message, "File not found for id 1")
  expect_identical(length(log$urls), 1L)
  expect_length(log$sleeps, 0L)
})

test_that("other 4xx statuses fail at once", {
  log <- local_transport(list(status(400L)))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_http")
  expect_identical(err$status, 400L)
  expect_identical(length(log$urls), 1L)
})

test_that("a WAF challenge (202) or a 403 is refused without retry, and no file is left", {
  for (r in list(
    status(202L, list(`x-amzn-waf-action` = "challenge"), body = "<html>challenge</html>"),
    status(403L)
  )) {
    log <- local_transport(list(r))
    dir <- withr::local_tempdir()
    dest <- file.path(dir, "f.dta")
    err <- expect_error(
      qesR:::.qes_fetch(u, dest, manual_path = dest),
      class = "qesR_error_http_refused"
    )
    expect_s3_class(err, "qesR_error_http")
    expect_s3_class(err, "qesR_error_network")
    expect_identical(err$manual_path, dest)
    expect_identical(length(log$urls), 1L)
    expect_length(log$sleeps, 0L)
    expect_false(file.exists(dest))
    expect_length(part_files(dir), 0L)
  }
  # a plain 202 without the WAF header is a success
  local_transport(list(status(202L, body = "ok")))
  expect_identical(qesR:::.qes_request(u)$status, 202L)
})

test_that("an invalid stall timeout is an input error, not a failed transfer", {
  log <- local_transport(list(ok()))
  withr::local_options(qesR.stall_timeout = "x")
  expect_error(qesR:::.qes_request(u), class = "qesR_error_input")
  expect_length(log$urls, 0L)
})

test_that("an interrupted transfer leaves no .part file", {
  log <- local_transport(list(function(url, dest) {
    writeLines("half a file", dest)
    # what Ctrl-C signals: an interrupt, which is not an error
    signalCondition(structure(class = c("interrupt", "condition"), list(message = "", call = NULL)))
    stop("not reached")
  }))
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "f")
  tryCatch(qesR:::.qes_fetch(u, dest), interrupt = function(i) NULL)
  expect_identical(length(log$urls), 1L)
  expect_false(file.exists(dest))
  expect_length(part_files(dir), 0L)
})

test_that("a stall is retried, then fails with its root cause", {
  log <- local_transport(rep(list(fake_curl_error("curl_error_operation_timedout", "stalled")), 4))
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "f")
  err <- expect_error(qesR:::.qes_fetch(u, dest), class = "qesR_error_network")
  expect_identical(err$attempts, 4L)
  expect_s3_class(err$parent, "curl_error_operation_timedout")
  expect_identical(length(log$urls), 4L)
  expect_false(file.exists(dest))
  expect_length(part_files(dir), 0L)
})

test_that("a connection error that clears is retried", {
  log <- local_transport(list(fake_curl_error("curl_error_couldnt_connect"), ok()))
  qesR:::.qes_request(u)
  expect_identical(length(log$urls), 2L)
})

test_that("an unresolvable host is qesR_error_offline, with no retry", {
  log <- local_transport(list(fake_curl_error("curl_error_couldnt_resolve_host")))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_offline")
  expect_s3_class(err, "qesR_error_network")
  expect_s3_class(err$parent, "curl_error_couldnt_resolve_host")
  expect_identical(length(log$urls), 1L)
})

test_that("a TLS failure is qesR_error_tls and is never retried", {
  log <- local_transport(list(fake_curl_error("curl_error_peer_failed_verification", "SSL certificate problem")))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_tls")
  expect_s3_class(err$parent, "curl_error_peer_failed_verification")
  expect_identical(length(log$urls), 1L)
})

test_that("a failure that is not a transport error is not retried, and keeps its cause", {
  log <- local_transport(list(simpleError("something else")))
  err <- expect_error(qesR:::.qes_request(u), class = "qesR_error_network")
  expect_identical(conditionMessage(err$parent), "something else")
  expect_identical(err$url, u)
  expect_identical(length(log$urls), 1L)
})

test_that("a rejected download (md5 check) leaves neither the file nor the part", {
  local_transport(list(ok("bytes")))
  dir <- withr::local_tempdir()
  dest <- file.path(dir, "f")
  verify <- function(part) stop("rejected")
  expect_error(qesR:::.qes_fetch(u, dest, verify = verify), "rejected")
  expect_false(file.exists(dest))
  expect_length(part_files(dir), 0L)
})

# ---- politeness ------------------------------------------------------------------------

test_that("requests to one host are at least one second apart", {
  waits <- numeric(0)
  now <- as.POSIXct("2026-01-01 00:00:00", tz = "UTC")
  local_transport(rep(list(ok()), 4))
  local_mocked_bindings(
    .qes_now = function() now,
    .qes_sleep = function(seconds) {
      waits <<- c(waits, seconds)
      now <<- now + seconds
    },
    .package = "qesR"
  )
  qesR:::.qes_request("https://borealisdata.ca/api/access/datafile/5")
  expect_length(waits, 0L)
  now <- now + 0.2
  qesR:::.qes_request("https://borealisdata.ca/api/access/datafile/6")
  expect_length(waits, 1L)
  expect_equal(waits[1], 0.8, tolerance = 1e-6)
  # another host has its own clock
  qesR:::.qes_request("https://dataverse.harvard.edu/api/access/datafile/7")
  expect_length(waits, 1L)
  # a request after more than a second does not wait
  now <- now + 1.5
  qesR:::.qes_request("https://borealisdata.ca/api/access/datafile/8")
  expect_length(waits, 1L)
})

# ---- live (never on CRAN) ----------------------------------------------------------------

test_that("live: one small document downloads, md5-verified, into the cache", {
  skip_on_cran()
  skip_if_offline("borealisdata.ca")
  skip_if_not(identical(Sys.getenv("QESR_LIVE"), "true"), "set QESR_LIVE=true for live tests")
  withr::local_options(qesR.cache_dir = withr::local_tempdir())
  files <- getFromNamespace(".qes_catalog", "qesR")()$files
  studies <- getFromNamespace(".qes_catalog", "qesR")()$studies
  docs <- files[files$role != "data" & !files$ingested, , drop = FALSE]
  docs <- merge(docs, studies[, c("study", "server")], by = "study")
  docs <- docs[grepl("borealisdata", docs$server), , drop = FALSE]
  row <- docs[which.min(docs$bytes), , drop = FALSE]
  path <- suppressMessages(qesR:::.qes_cache_fetch(row, row$server, quiet = TRUE))
  expect_identical(unname(tools::md5sum(path)), row$md5)
  expect_identical(qes_cache_info()$path, as.character(path))
})
