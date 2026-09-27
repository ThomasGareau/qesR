# Offline stand-in for Dataverse, used by the contract tests (slices S0b, S2b).
#
# qesR reaches the network only through the internal `.qes_transport(url,
# dest, handle)`, and reads its catalog only through `.qes_catalog()`
# (design.md section 8.1). `local_fake_dataverse()` replaces both for the
# calling test:
#
#   * every study of the shipped catalog gets one synthetic data file, written
#     with haven::write_sav() at test time (`data` or fake_study_data(<code>));
#     the fixture catalog pins it (md5, size, rows, columns) in place of the
#     real data file, under the real file id, so get_qes() with a real code
#     reads it through the real reader;
#   * the fake server answers the requests qesR makes:
#       <server>/api/access/datafile/<id>?format=original   a data file
#       <server>/api/access/datafile/<id>/metadata/ddi      its DDI XML
#       <server>/api/access/datafile/<id>                   a document
#     Any other URL is an error, so a test that reaches the network by
#     mistake fails instead of downloading.
#
# All data is synthetic: a few rows built below, never derived from real
# respondents. The download cache is off ("none") and so is the in-memory
# memo, so every test downloads its own files.

fake_study_data <- function(code) {
  out <- data.frame(
    quest = sprintf("%s-%02d", code, 1:4),
    age = c(25, 40, 61, 33),
    sexe = haven::labelled(c(1, 2, 1, 2), labels = c(Homme = 1, Femme = 2)),
    q1 = haven::labelled(c(1, 2, 3, 1), labels = c("Parti A" = 1, "Parti B" = 2, "Parti C" = 3)),
    poids = c(1, 1.1, 0.9, 1),
    stringsAsFactors = FALSE
  )
  attr(out$age, "label") <- "Age du repondant"
  attr(out$sexe, "label") <- "Sexe du repondant"
  attr(out$q1, "label") <- "Intention de vote"
  attr(out$poids, "label") <- "Ponderation"
  out
}

.fake_xml_escape <- function(x) {
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  gsub(">", "&gt;", x, fixed = TRUE)
}

.fake_ddi <- function(data) {
  vars <- vapply(names(data), function(nm) {
    x <- data[[nm]]
    label <- attr(x, "label", exact = TRUE) %||% nm
    labels <- attr(x, "labels", exact = TRUE)
    cats <- if (length(labels) > 0L) {
      paste0(
        "<catgry><catValu>", unname(labels), "</catValu><labl>",
        .fake_xml_escape(names(labels)), "</labl></catgry>",
        collapse = ""
      )
    } else {
      ""
    }
    sprintf(
      "<var ID=\"%s\" name=\"%s\"><labl>%s</labl><qstn><qstnLit>%s?</qstnLit></qstn>%s</var>",
      nm, nm, .fake_xml_escape(label), .fake_xml_escape(label), cats
    )
  }, character(1))
  paste0("<codeBook><dataDscr>", paste(vars, collapse = ""), "</dataDscr></codeBook>")
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# A catalog (as .qes_catalog() returns it) whose data files are the
# synthetic `.sav` files written into `dir`: one per study, from `data` or
# fake_study_data(). Label donors, text fixes and type fixes are dropped
# (they describe the real files). Returns list(catalog, demo_catalog, paths)
# where `paths` maps each data file id to its local file.
fake_catalog <- function(data = list(), dir = tempfile("fake-dv-")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  load <- getFromNamespace(".qes_load_catalog", "qesR")
  catalog <- load(catalog_dir())
  demo_catalog <- load(catalog_dir(), demo_dir = system.file("extdata", "demo", "catalog", package = "qesR"))
  files <- catalog$files
  keep <- !(files$role %in% c("data", "label_donor"))
  data_rows <- list()
  paths <- character(0)
  for (code in catalog$studies$study) {
    d <- data[[code]] %||% fake_study_data(code)
    row <- files[files$study == code & files$role == "data" & files$is_default %in% TRUE, , drop = FALSE]
    path <- file.path(dir, paste0(row$file_id, ".sav"))
    haven::write_sav(d, path)
    row$file_name <- paste0(code, ".tab")
    row$original_file_name <- paste0(code, ".sav")
    row$format <- "sav"
    row$ingested <- TRUE
    row$bytes <- file.size(path)
    row$md5 <- unname(tools::md5sum(path))
    row$unf <- paste0("UNF:6:fake-", code)
    row$n_rows <- nrow(d)
    row$n_cols <- ncol(d)
    row$encoding <- NA_character_
    data_rows[[code]] <- row
    paths[[row$file_id]] <- path
  }
  files <- rbind(do.call(rbind, data_rows), files[keep, , drop = FALSE])
  rownames(files) <- NULL
  fix <- function(cat) {
    cat$files <- rbind(files, cat$files[cat$files$study == "qes_demo", , drop = FALSE])
    if (!any(cat$studies$demo)) {
      cat$files <- files
    }
    cat$studies$label_file_id <- NA_character_
    cat$text_fixes <- cat$text_fixes[0, , drop = FALSE]
    cat$type_fixes <- cat$type_fixes[0, , drop = FALSE]
    cat
  }
  list(catalog = fix(catalog), demo_catalog = fix(demo_catalog), paths = paths)
}

# Replace the catalog and the transport for the calling test.
#   data: named list of data frames served instead of fake_study_data(<code>)
#   fail: study codes whose data file cannot be downloaded (a simulated outage)
# Returns an environment whose `urls` field logs every request made, and
# whose `catalog` field is the fixture catalog.
local_fake_dataverse <- function(data = list(), fail = character(0), .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  fake <- fake_catalog(data, dir = dir)
  files <- fake$catalog$files

  log <- new.env(parent = emptyenv())
  log$urls <- character(0)
  log$catalog <- fake$catalog

  transport <- function(url, dest = NULL, handle = NULL) {
    log$urls <- c(log$urls, url)
    m <- regmatches(url, regexec("/api/access/datafile/([0-9]+)(/metadata/ddi|\\?format=original)?$", url))[[1]]
    if (length(m) == 0L) {
      stop(sprintf("fake Dataverse: unexpected request '%s'", url), call. = FALSE)
    }
    row <- files[files$file_id == m[2], , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("fake Dataverse: unknown file id in '%s'", url), call. = FALSE)
    }
    is_data <- identical(row$role, "data")
    if (is_data && row$study %in% fail) {
      stop(sprintf("fake Dataverse: '%s' is unavailable", row$study), call. = FALSE)
    }
    if (identical(m[3], "/metadata/ddi")) {
      if (!is_data) {
        stop("fake Dataverse: no DDI for a document", call. = FALSE)
      }
      d <- haven::read_sav(fake$paths[[row$file_id]], user_na = TRUE)
      return(fake_response(url, dest, write = function(path) writeLines(.fake_ddi(d), path, useBytes = TRUE)))
    }
    if (is_data) {
      if (!identical(m[3], "?format=original")) {
        stop("fake Dataverse: a data file must be requested as its original", call. = FALSE)
      }
      return(fake_response(url, dest, write = function(path) file.copy(fake$paths[[row$file_id]], path, overwrite = TRUE)))
    }
    fake_response(url, dest, write = function(path) writeLines(c("Synthetic document", row$study), path))
  }

  local_clear_codebook_cache(.env = .env)
  withr::local_options(qesR.cache = "none", qesR.memo = FALSE, .local_envir = .env)
  testthat::local_mocked_bindings(
    .qes_transport = transport,
    .qes_catalog = function(demo = FALSE) if (isTRUE(demo)) fake$demo_catalog else fake$catalog,
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR",
    .env = .env
  )
  invisible(log)
}

# A transport response, shaped like the value of qesR:::.qes_transport():
# list(url, status, headers, content). `write(path)` writes the body; `body`
# (raw or character) is the alternative. With `dest` the body goes to that
# file and `content` is `dest`; without it `content` is the raw body.
fake_response <- function(url, dest = NULL, status = 200L, headers = list(),
                          body = NULL, write = NULL) {
  target <- dest %||% tempfile()
  if (!is.null(write)) {
    write(target)
  } else {
    bytes <- if (is.raw(body)) body else charToRaw(paste(body %||% "", collapse = "\n"))
    writeBin(bytes, target)
  }
  content <- if (is.null(dest)) {
    on.exit(unlink(target), add = TRUE)
    readBin(target, "raw", file.size(target))
  } else {
    dest
  }
  list(url = url, status = as.integer(status), headers = headers, content = content)
}

# A curl-like transport error of class `curl_class` (for example
# "curl_error_operation_timedout").
fake_curl_error <- function(curl_class, message = "fake transport failure") {
  structure(
    class = c(curl_class, "curl_error", "error", "condition"),
    list(message = message, call = NULL)
  )
}

# The session codebook cache is package state; clear it before and after a test
# so results never depend on test order.
local_clear_codebook_cache <- function(.env = parent.frame()) {
  cache <- qesR:::.qes_codebook_cache
  rm(list = ls(cache, all.names = TRUE), envir = cache)
  withr::defer(rm(list = ls(cache, all.names = TRUE), envir = cache), envir = .env)
}

# Once-per-session message state (.qes_once, slice S0c). Reset it before and
# after the calling test so message tests never depend on test order
# (design.md section 8.1).
local_qes_once <- function(.env = parent.frame()) {
  reset <- getFromNamespace(".qes_reset_once", "qesR")
  reset()
  withr::defer(reset(), envir = .env)
  invisible()
}

# Treat every once-per-session notice as already shown, for the calling test
# only, so tests that are not about the notices print nothing and do not
# depend on test order.
local_qes_notices_shown <- function(.env = parent.frame()) {
  testthat::local_mocked_bindings(
    .qes_once_first = function(key) FALSE,
    .package = "qesR",
    .env = .env
  )
  invisible()
}

# Evaluate `expr`, muffle every message, and return how many had `class`.
count_class <- function(expr, class) {
  n <- 0L
  withCallingHandlers(
    expr,
    message = function(m) {
      if (inherits(m, class)) {
        n <<- n + 1L
      }
      invokeRestart("muffleMessage")
    }
  )
  n
}

# The session memo of dataset-version answers (qes_studies(check_updates =
# TRUE)); cleared before and after the calling test.
local_clear_latest_memo <- function(.env = parent.frame()) {
  memo <- qesR:::.qes_latest_memo
  rm(list = ls(memo, all.names = TRUE), envir = memo)
  withr::defer(rm(list = ls(memo, all.names = TRUE), envir = memo), envir = .env)
}
