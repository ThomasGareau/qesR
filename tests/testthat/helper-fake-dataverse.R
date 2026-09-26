# Offline stand-in for Dataverse, used by the contract tests (slice S0b).
#
# Until slice S2a introduces the `.qes_transport()` seam (design.md section 8.1),
# every request qesR makes goes through the internal
# `.download_file_with_fallback(url, destfile, quiet, allow_insecure_retry)`.
# `local_fake_dataverse()` replaces that one function for the calling test with
# a fake server that answers the three kinds of request the current code makes:
#
#   <server>/api/datasets/:persistentId/?persistentId=doi:<doi>   dataset JSON
#   <server>/api/access/datafile/<id>                             file bytes
#   <server>/api/access/datafile/<id>/metadata/ddi                DDI XML
#
# Every study in the catalog gets one data file (`<code>.sav`) and one
# questionnaire (`<code>_questionnaire.txt`). All data is synthetic: a few rows
# built below, never derived from real respondents. Any other URL is an error,
# so a test that reaches the network by mistake fails instead of downloading.

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

# Replace the transport for the calling test.
#   data: named list of data frames served instead of fake_study_data(<code>)
#   fail: study codes whose metadata request fails (a simulated outage)
# Returns an environment whose `urls` field logs every request made.
local_fake_dataverse <- function(data = list(), fail = character(0), .env = parent.frame()) {
  # Built from the exported catalog, not the internal `.qes_catalog` table,
  # which becomes a function in slice S1. The option keeps the (future)
  # deprecation notice of get_qescodes() out of the calling test.
  catalog <- withr::with_options(
    list(qesR.quiet_deprecated = TRUE),
    get_qescodes(detailed = TRUE)
  )
  ids <- data.frame(
    code = rep(catalog$qes_survey_code, each = 2L),
    id = as.character(rep(9000L + 10L * catalog$index, each = 2L) + c(1L, 2L)),
    kind = rep(c("data", "doc"), times = nrow(catalog)),
    stringsAsFactors = FALSE
  )
  ids$filename <- ifelse(
    ids$kind == "data",
    paste0(ids$code, ".sav"),
    paste0(ids$code, "_questionnaire.txt")
  )

  log <- new.env(parent = emptyenv())
  log$urls <- character(0)

  study_data <- function(code) data[[code]] %||% fake_study_data(code)

  transport <- function(url, destfile, quiet = TRUE, allow_insecure_retry = FALSE) {
    log$urls <- c(log$urls, url)

    if (grepl("/api/datasets/:persistentId/", url, fixed = TRUE)) {
      doi <- sub("^doi:", "", utils::URLdecode(sub("^.*persistentId=", "", url)))
      code <- catalog$qes_survey_code[match(doi, catalog$doi)]
      if (is.na(code)) {
        stop(sprintf("fake Dataverse: unknown DOI in '%s'", url), call. = FALSE)
      }
      if (code %in% fail) {
        stop(sprintf("fake Dataverse: '%s' is unavailable", code), call. = FALSE)
      }
      rows <- ids[ids$code == code, , drop = FALSE]
      files <- lapply(seq_len(nrow(rows)), function(i) {
        list(dataFile = list(id = as.integer(rows$id[i]), filename = rows$filename[i], filesize = 1000 * i))
      })
      json <- list(status = "OK", data = list(latestVersion = list(files = files)))
      jsonlite::write_json(json, destfile, auto_unbox = TRUE)
      return(invisible(destfile))
    }

    m <- regmatches(url, regexec("/api/(access/datafile|files)/([0-9]+)(/metadata/ddi)?$", url))[[1]]
    if (length(m) == 0L) {
      stop(sprintf("fake Dataverse: unexpected request '%s'", url), call. = FALSE)
    }
    row <- ids[ids$id == m[3], , drop = FALSE]
    if (nrow(row) != 1L) {
      stop(sprintf("fake Dataverse: unknown file id in '%s'", url), call. = FALSE)
    }

    if (nzchar(m[4])) {
      if (row$kind != "data") {
        stop("fake Dataverse: no DDI for a document", call. = FALSE)
      }
      writeLines(.fake_ddi(study_data(row$code)), destfile, useBytes = TRUE)
    } else if (row$kind == "data") {
      haven::write_sav(study_data(row$code), destfile)
    } else {
      writeLines(c("Synthetic questionnaire", row$code), destfile)
    }
    invisible(destfile)
  }

  local_clear_codebook_cache(.env = .env)
  testthat::local_mocked_bindings(
    .download_file_with_fallback = transport,
    .package = "qesR",
    .env = .env
  )
  invisible(log)
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
# (design.md section 8.1). Only the S0c-gated tests call this until then.
local_qes_once <- function(.env = parent.frame()) {
  reset <- getFromNamespace(".qes_reset_once", "qesR")
  reset()
  withr::defer(reset(), envir = .env)
  invisible()
}
