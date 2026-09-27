# Download cache (slice S2a; design.md sections 4.2 and 8.1). Every test
# points the cache at a temporary directory; the transport is mocked.

# Clear the cache settings and session state for the calling test.
local_cache_settings <- function(..., .env = parent.frame()) {
  withr::local_options(qesR.cache = NULL, qesR.cache_dir = NULL, .local_envir = .env)
  withr::local_envvar(QESR_CACHE = NA, QESR_CACHE_DIR = NA, .local_envir = .env)
  withr::local_options(list(...), .local_envir = .env)
  state <- qesR:::.qes_cache_state
  rm(list = ls(state, all.names = TRUE), envir = state)
  withr::defer(rm(list = ls(state, all.names = TRUE), envir = state), envir = .env)
  invisible()
}

# A user-chosen cache directory (implies disk mode). Returns the directory.
local_cache_dir <- function(.env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  local_cache_settings(qesR.cache_dir = dir, .env = .env)
  dir
}

# A transport that serves `body` for every request, counting requests.
local_serve <- function(body, status = 200L, .env = parent.frame()) {
  log <- new.env(parent = emptyenv())
  log$urls <- character(0)
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      log$urls <- c(log$urls, url)
      fake_response(url, dest, status, body = body)
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR",
    .env = .env
  )
  log
}

md5_of <- function(text) {
  path <- withr::local_tempfile()
  writeBin(charToRaw(text), path)
  unname(tools::md5sum(path))
}

# A files.csv-like row for a document whose bytes are `text`.
doc_row <- function(text, study = "qes2018", file_id = "102", role = "questionnaire",
                    name = "a_fr.pdf", format = "pdf", ingested = FALSE) {
  data.frame(
    study = study, file_id = file_id, role = role, original_file_name = name,
    format = format, ingested = ingested, bytes = nchar(text, type = "bytes"),
    md5 = md5_of(text), stringsAsFactors = FALSE
  )
}

server <- "https://borealisdata.ca"
fetch <- function(row, quiet = TRUE) qesR:::.qes_cache_fetch(row, server, quiet = quiet)

# ---- modes and roots -----------------------------------------------------------------

test_that("the default mode is the session cache in tempdir()", {
  local_cache_settings()
  expect_identical(qesR:::.qes_cache_mode(), "session")
  expect_identical(qesR:::.qes_cache_root(), file.path(tempdir(), "qesR"))
  info <- qes_cache_info()
  expect_identical(attr(info, "mode"), "session")
  expect_identical(attr(info, "dir"), file.path(tempdir(), "qesR"))
  expect_identical(
    names(info),
    c("study", "file_id", "md5", "bytes", "retrieved", "kind", "path")
  )
})

test_that("the mode comes from the option, then QESR_CACHE; unknown modes are errors", {
  local_cache_settings()
  withr::local_envvar(QESR_CACHE = "none")
  expect_identical(qesR:::.qes_cache_mode(), "none")
  withr::local_options(qesR.cache = "disk")
  expect_identical(qesR:::.qes_cache_mode(), "disk")
  withr::local_options(qesR.cache = "always")
  expect_error(qesR:::.qes_cache_mode(), class = "qesR_error_input")
})

test_that("disk mode uses R_user_dir, created only on opt-in, with a message", {
  local_cache_settings()
  user <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = user)
  expect_length(list.files(user, recursive = TRUE, all.files = TRUE), 0L)
  withr::local_options(qesR.cache = "disk")
  root <- qesR:::.qes_cache_root()
  expect_identical(root, tools::R_user_dir("qesR", "cache"))
  # listing does not create it
  qes_cache_info()
  expect_false(dir.exists(root))
  local_serve("document bytes")
  expect_identical(count_class(fetch(doc_row("document bytes"), quiet = FALSE), "qesR_message_cached"), 1L)
  expect_true(file.exists(file.path(root, ".qesR-cache")))
})

test_that("a user-chosen directory gets a marked qesR/ subdirectory, and nothing else", {
  dir <- local_cache_dir()
  writeLines("mine", file.path(dir, "my-notes.txt"))
  expect_identical(qesR:::.qes_cache_mode(), "disk")
  expect_identical(qesR:::.qes_cache_root(), file.path(normalizePath(dir, winslash = "/"), "qesR"))
  local_serve("document bytes")
  path <- suppressMessages(fetch(doc_row("document bytes")))
  root <- file.path(normalizePath(dir, winslash = "/"), "qesR")
  expect_true(startsWith(path, root))
  expect_true(file.exists(file.path(root, ".qesR-cache")))
  expect_identical(
    sort(list.files(dir, all.files = TRUE, no.. = TRUE)),
    c("my-notes.txt", "qesR")
  )
  qes_cache_clear()
  expect_true(file.exists(file.path(dir, "my-notes.txt")))
  expect_false(file.exists(path))
})

test_that("an explicit session or none mode overrides the cache directory", {
  local_cache_dir()
  withr::local_options(qesR.cache = "session")
  expect_identical(qesR:::.qes_cache_root(), file.path(tempdir(), "qesR"))
})

test_that("a cache directory must exist", {
  local_cache_settings(qesR.cache_dir = file.path(withr::local_tempdir(), "nope"))
  err <- expect_error(qes_cache_info(), class = "qesR_error_cache")
  expect_identical(err$reason, "missing")
  local_cache_settings(qesR.cache_dir = 3)
  expect_error(qes_cache_info(), class = "qesR_error_input")
})

test_that("qesR never writes into an existing unmarked qesR/ folder", {
  dir <- local_cache_dir()
  dir.create(file.path(dir, "qesR"))
  writeLines("user data", file.path(dir, "qesR", "thesis.R"))
  local_serve("document bytes")
  err <- expect_error(fetch(doc_row("document bytes")), class = "qesR_error_cache")
  expect_identical(err$reason, "unmarked")
  expect_identical(list.files(file.path(dir, "qesR")), "thesis.R")
})

test_that("qes_cache_clear() refuses a root without the marker", {
  dir <- local_cache_dir()
  dir.create(file.path(dir, "qesR", "v1", "borealisdata.ca"), recursive = TRUE)
  victim <- file.path(dir, "qesR", "v1", "borealisdata.ca", "102-fedcba9876543210fedcba9876543210.pdf")
  writeLines("x", victim)
  err <- expect_error(qes_cache_clear(), class = "qesR_error_cache")
  expect_identical(err$reason, "unmarked")
  expect_true(file.exists(victim))
})

# ---- fetching ----------------------------------------------------------------------------

test_that("a file is downloaded once, under <host>/<file_id>-<md5>.<ext>, then served from the cache", {
  local_cache_dir()
  log <- local_serve("document bytes")
  row <- doc_row("document bytes")
  path <- suppressMessages(fetch(row))
  expect_identical(basename(path), paste0("102-", row$md5, ".pdf"))
  expect_identical(basename(dirname(path)), "borealisdata.ca")
  expect_identical(basename(dirname(dirname(path))), "v1")
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/102")
  n <- count_class(again <- fetch(row, quiet = FALSE), "qesR_message_cached")
  expect_identical(n, 1L)
  expect_identical(again, path)
  expect_length(log$urls, 1L)
  expect_silent(fetch(row, quiet = TRUE))
})

test_that("ingested data files are fetched as originals", {
  local_cache_dir()
  log <- local_serve("data bytes")
  row <- doc_row("data bytes", file_id = "101", role = "data", name = "a.sav", format = "sav", ingested = TRUE)
  path <- fetch(row)
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/101?format=original")
  expect_match(path, "\\.sav$")
})

test_that("an ingested label donor is fetched as its original too", {
  local_cache_dir()
  log <- local_serve("donor bytes")
  row <- doc_row("donor bytes", study = "qes2012", file_id = "425917", role = "label_donor",
    name = "b.sav", format = "sav", ingested = TRUE)
  fetch(row)
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/425917?format=original")
})

test_that("an md5 mismatch is qesR_error_checksum and nothing is kept (atomic write)", {
  dir <- local_cache_dir()
  local_serve("tampered bytes")
  row <- doc_row("document bytes")
  err <- expect_error(fetch(row), class = "qesR_error_checksum")
  expect_s3_class(err, "qesR_error_source")
  expect_identical(err$expected, row$md5)
  expect_identical(err$actual, md5_of("tampered bytes"))
  expect_identical(err$file_id, "102")
  left <- list.files(dir, recursive = TRUE, all.files = TRUE)
  expect_identical(left, "qesR/.qesR-cache")
})

test_that("a failed transfer leaves no partial file", {
  dir <- local_cache_dir()
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      writeLines("half a file", dest)
      stop(fake_curl_error("curl_error_url_malformat"))
    },
    .package = "qesR"
  )
  expect_error(fetch(doc_row("document bytes")), class = "qesR_error_network")
  expect_identical(list.files(dir, recursive = TRUE, all.files = TRUE), "qesR/.qesR-cache")
})

test_that("a corrupted cached file is replaced; md5 is checked once per session", {
  local_cache_dir()
  log <- local_serve("document bytes")
  row <- doc_row("document bytes")
  path <- fetch(row)
  # same size, different bytes: caught by the md5 check of a new session
  writeBin(charToRaw("DOCUMENT BYTES"), path)
  expect_identical(fetch(row), path)
  expect_length(log$urls, 1L) # verified earlier this session: size check only
  state <- qesR:::.qes_cache_state
  rm(list = ls(state, all.names = TRUE), envir = state)
  expect_identical(fetch(row), path)
  expect_length(log$urls, 2L)
  expect_identical(unname(tools::md5sum(path)), row$md5)
  # a wrong size is caught at once
  writeBin(charToRaw("short"), path)
  fetch(row)
  expect_length(log$urls, 3L)
})

test_that("a file put in the cache by hand is used after its md5 check", {
  local_cache_dir()
  row <- doc_row("document bytes")
  path <- qesR:::.qes_cache_path(qesR:::.qes_cache_root(), server, row$file_id, row$md5, "pdf")
  qesR:::.qes_cache_prepare(qesR:::.qes_cache_root(), "session")
  dir.create(dirname(path), recursive = TRUE)
  writeBin(charToRaw("document bytes"), path)
  log <- local_serve("never served")
  expect_identical(fetch(row), path)
  expect_length(log$urls, 0L)
})

test_that("a file saved by hand under a fresh qesR.cache_dir is used, as the refusal message says", {
  dir <- local_cache_dir()
  row <- doc_row("document bytes")
  rel <- file.path("qesR", "v1", "borealisdata.ca", paste0("102-", row$md5, ".pdf"))
  dir.create(dirname(file.path(dir, rel)), recursive = TRUE)
  writeBin(charToRaw("document bytes"), file.path(dir, rel))
  file.create(file.path(dir, "qesR", ".DS_Store"))
  log <- local_serve("never served")
  path <- fetch(row)
  expect_identical(normalizePath(path), normalizePath(file.path(dir, rel)))
  expect_length(log$urls, 0L)
  expect_true(file.exists(file.path(dir, "qesR", ".qesR-cache")))
})

test_that("a qesR/ folder with anything but cache-named files is still not used", {
  dir <- local_cache_dir()
  dir.create(file.path(dir, "qesR", "v1", "borealisdata.ca"), recursive = TRUE)
  writeLines("mine", file.path(dir, "qesR", "v1", "borealisdata.ca", "notes.txt"))
  local_serve("document bytes")
  expect_error(fetch(doc_row("document bytes")), class = "qesR_error_cache")
  expect_false(file.exists(file.path(dir, "qesR", ".qesR-cache")))
})

test_that("a wrong file saved by hand is reported, not silently replaced by a refusal", {
  local_cache_dir()
  row <- doc_row("document bytes")
  root <- qesR:::.qes_cache_root()
  qesR:::.qes_cache_prepare(root, "disk", quiet = TRUE)
  path <- qesR:::.qes_cache_path(root, server, row$file_id, row$md5, "pdf")
  dir.create(dirname(path), recursive = TRUE)
  writeBin(charToRaw("the .tab version"), path)
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      fake_response(url, dest, 403L, body = "")
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR"
  )
  expect_identical(count_class(try(fetch(row, quiet = FALSE), silent = TRUE), "qesR_message_cached"), 1L)
  writeBin(charToRaw("the .tab version"), path)
  err <- expect_error(fetch(row), class = "qesR_error_checksum")
  expect_identical(err$id, "checksum_cached")
  expect_identical(err$path, path)
  expect_identical(err$actual, md5_of("the .tab version"))
  expect_s3_class(err$parent, "qesR_error_http_refused")
})

test_that("a refused request names the cache path for a manual download", {
  local_cache_dir()
  testthat::local_mocked_bindings(
    .qes_transport = function(url, dest = NULL, handle = NULL) {
      fake_response(url, dest, 202L, list(`x-amzn-waf-action` = "challenge"), body = "<html/>")
    },
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR"
  )
  row <- doc_row("document bytes")
  err <- expect_error(fetch(row), class = "qesR_error_http_refused")
  expect_identical(err$id, "http_refused_manual")
  expect_identical(basename(err$manual_path), paste0("102-", row$md5, ".pdf"))
  expect_false(file.exists(err$manual_path))
  expect_match(
    conditionMessage(err),
    paste0("<folder>/qesR/v1/borealisdata.ca/102-", row$md5, ".pdf"),
    fixed = TRUE
  )
})

test_that("mode none keeps nothing: each call downloads to a new temporary folder", {
  local_cache_settings(qesR.cache = "none")
  log <- local_serve("document bytes")
  row <- doc_row("document bytes")
  a <- fetch(row)
  b <- fetch(row)
  expect_true(isTRUE(attr(a, "transient")))
  expect_length(log$urls, 2L)
  expect_false(identical(dirname(a), dirname(b)))
  expect_true(startsWith(normalizePath(a), normalizePath(tempdir())))
  info <- qes_cache_info()
  expect_identical(nrow(info), 0L)
  expect_true(is.na(attr(info, "dir")))
  expect_identical(qes_cache_clear(), character(0))
})

test_that("the disk-cache tip is shown once, after the second download, in interactive sessions", {
  local_cache_settings()
  local_qes_once()
  root <- file.path(tempdir(), "qesR")
  withr::defer(qes_cache_clear())
  local_serve("x")
  testthat::local_mocked_bindings(.qes_interactive = function() TRUE, .package = "qesR")
  rows <- lapply(1:3, function(i) doc_row(paste0("doc ", i), file_id = as.character(900 + i)))
  tips <- vapply(rows, function(r) {
    testthat::local_mocked_bindings(
      .qes_transport = function(url, dest = NULL, handle = NULL) {
        fake_response(url, dest, body = sub("^.*datafile/90", "doc ", url))
      },
      .package = "qesR"
    )
    count_class(fetch(r, quiet = FALSE), "qesR_message_disk_cache_tip")
  }, integer(1))
  expect_identical(tips, c(0L, 1L, 0L))
})

# ---- listing and clearing ------------------------------------------------------------------

# Put fake files in the cache (no network): the fixture catalog's qes2018
# questionnaire (102) and qes_fixture_b codebook (202), plus a 2022-style shard.
seed_cache <- function() {
  root <- qesR:::.qes_cache_root()
  qesR:::.qes_cache_prepare(root, "session")
  host <- file.path(root, "v1", "dataverse.example.org")
  dir.create(host, recursive = TRUE)
  dir.create(file.path(root, "v1", "shards"))
  a <- file.path(host, "102-fedcba9876543210fedcba9876543210.pdf")
  b <- file.path(host, "202-ffeeddccbbaa99887766554433221100.pdf")
  s <- qesR:::.qes_cache_shard_path(root, "qes2018", "0123456789abcdef0123456789abcdef", 1, "variables")
  writeLines("a", a)
  writeLines("bb", b)
  writeLines("variable", s)
  writeLines("partial", file.path(host, "qesR-123.part"))
  list(root = root, a = a, b = b, shard = s)
}

test_that("qes_cache_info() lists files and shards with their study", {
  local_cache_dir()
  local_fixture_catalog()
  paths <- seed_cache()
  info <- qes_cache_info()
  expect_identical(nrow(info), 3L)
  expect_identical(info$study, c("qes2018", "qes2018", "qes_fixture_b"))
  expect_identical(info$kind, c("file", "shard", "file"))
  expect_identical(info$file_id, c("102", NA, "202"))
  expect_identical(info$md5[2], "0123456789abcdef0123456789abcdef")
  expect_identical(info$bytes, as.numeric(file.size(c(paths$a, paths$shard, paths$b))))
  expect_s3_class(info$retrieved, "POSIXct")
  expect_identical(attr(info, "mode"), "disk")
  expect_identical(attr(info, "dir"), paths$root)
})

test_that("qes_cache_clear(studies =) removes only those studies", {
  local_cache_dir()
  local_fixture_catalog()
  paths <- seed_cache()
  removed <- withVisible(qes_cache_clear(studies = "QES2018"))
  expect_false(removed$visible)
  expect_setequal(removed$value, c(paths$a, paths$shard))
  expect_identical(qes_cache_info()$study, "qes_fixture_b")
  expect_error(qes_cache_clear(studies = "nope"), class = "qesR_error_unknown_study")
})

test_that("qes_cache_clear(older_than =) removes only old files", {
  local_cache_dir()
  local_fixture_catalog()
  paths <- seed_cache()
  Sys.setFileTime(paths$b, Sys.time() - 40 * 86400)
  expect_identical(qes_cache_clear(older_than = 30), paths$b)
  expect_identical(qes_cache_clear(older_than = as.difftime(365, units = "days")), character(0))
  expect_identical(nrow(qes_cache_info()), 2L)
  expect_error(qes_cache_clear(older_than = -1), class = "qesR_error_input")
  expect_error(qes_cache_clear(older_than = "old"), class = "qesR_error_input")
})

test_that("qes_cache_clear(older_than =) also removes old interrupted downloads", {
  local_cache_dir()
  local_fixture_catalog()
  paths <- seed_cache()
  part <- file.path(dirname(paths$a), "qesR-123.part")
  Sys.setFileTime(part, Sys.time() - 40 * 86400)
  expect_identical(qes_cache_clear(older_than = 30), part)
  expect_identical(nrow(qes_cache_info()), 3L)
})

test_that("qes_cache_clear() never follows a symbolic link out of the cache", {
  skip_on_os("windows")
  dir <- local_cache_dir()
  local_fixture_catalog()
  paths <- seed_cache()
  mine <- withr::local_tempdir()
  writeLines("my thesis", file.path(mine, "thesis.tex"))
  writeLines("x", file.path(mine, paste0("1-", strrep("a", 32), ".pdf")))
  file.symlink(mine, file.path(paths$root, "v1", "linked.host"))
  file.symlink(file.path(mine, "thesis.tex"), file.path(dirname(paths$a), paste0("9-", strrep("b", 32), ".pdf")))
  expect_false(any(grepl("linked.host", qes_cache_info()$path)))
  removed <- qes_cache_clear()
  expect_false(any(grepl("linked.host", removed)))
  expect_true(file.exists(file.path(mine, "thesis.tex")))
  expect_true(file.exists(file.path(mine, paste0("1-", strrep("a", 32), ".pdf"))))
})

test_that("qes_cache_clear() empties the cache, keeps the marked root, and clears the memos", {
  local_cache_dir()
  local_fixture_catalog()
  paths <- seed_cache()
  qesR:::.qes_memo_set("0123456789abcdef0123456789abcdef", data.frame(x = 1), study = "qes2018")
  memo <- qesR:::.qes_latest_memo
  memo[["https://example.org/x"]] <- list(version = "1.0")
  removed <- qes_cache_clear()
  expect_length(removed, 4L) # three files and the stale .part
  expect_true(file.exists(file.path(paths$root, ".qesR-cache")))
  expect_identical(list.files(paths$root, recursive = TRUE, all.files = TRUE), ".qesR-cache")
  expect_null(qesR:::.qes_memo_get("0123456789abcdef0123456789abcdef"))
  expect_length(ls(memo), 0L)
})

test_that("the memo is keyed by md5, respects qesR.memo and is cleared by study", {
  local_cache_settings()
  withr::defer(qesR:::.qes_memo_clear())
  qesR:::.qes_memo_set("aaaa", 1, study = "qes2018")
  qesR:::.qes_memo_set("bbbb", 2, study = "qes2022")
  expect_identical(qesR:::.qes_memo_get("aaaa"), 1)
  qesR:::.qes_memo_clear(studies = "qes2018")
  expect_null(qesR:::.qes_memo_get("aaaa"))
  expect_identical(qesR:::.qes_memo_get("bbbb"), 2)
  withr::local_options(qesR.memo = FALSE)
  expect_null(qesR:::.qes_memo_get("bbbb"))
})

test_that("listing and clearing never create a cache directory", {
  local_cache_settings()
  user <- withr::local_tempdir()
  withr::local_envvar(R_USER_CACHE_DIR = user)
  withr::local_options(qesR.cache = "disk")
  qes_cache_info()
  qes_cache_clear()
  expect_length(list.files(user, recursive = TRUE, all.files = TRUE, include.dirs = TRUE), 0L)
})
