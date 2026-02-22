#' Download and load a Quebec election study
#'
#' Downloads a study file from Dataverse, applies variable/value labels when
#' metadata is available, and attaches a codebook.
#'
#' @param srvy A qesR survey code (see [get_qescodes()]).
#' @param file Optional regular expression to select a specific file when a
#'   Dataverse dataset has multiple files.
#' @param assign_global If `TRUE`, assign the returned data object into the
#'   global environment using `srvy` as object name (cesR-style behavior).
#' @param with_codebook If `TRUE`, attach codebook metadata as
#'   `attr(data, "qes_codebook")` and assign `<srvy>_codebook` in the global
#'   environment when `assign_global = TRUE`.
#' @param quiet If `TRUE`, suppress informational output and download progress.
#'
#' @return A labelled data frame/tibble for the selected survey.
#' @export
get_qes <- function(srvy, file = NULL, assign_global = TRUE, with_codebook = TRUE, quiet = FALSE) {
  study <- .get_qes_study(srvy)

  if (!quiet) {
    message(sprintf("%s: %s", study$qes_survey_code, study$name_en))
    message(sprintf("DOI: %s", study$doi_url))
    message(sprintf("Documentation: %s", study$documentation))
  }

  payload <- .download_and_read_qes(study, file = file, quiet = quiet, read_data = TRUE)
  data <- payload$data
  attr(data, "qes_survey_code") <- srvy

  if (isTRUE(with_codebook)) {
    attr(data, "qes_codebook") <- payload$codebook
    if (exists(".qes_codebook_cache", mode = "environment")) {
      cache_key <- .codebook_cache_key(srvy, file = file)
      assign(cache_key, payload$codebook, envir = .qes_codebook_cache)
    }

    if (isTRUE(assign_global)) {
      assign(paste0(srvy, "_codebook"), payload$codebook, envir = .GlobalEnv)
    }

    if (!quiet) {
      codebook_files <- attr(payload$codebook, "codebook_files", exact = TRUE)
      n_files <- if (is.null(codebook_files)) 0L else nrow(codebook_files)
      message(sprintf("Codebook variables available: %s", nrow(payload$codebook)))
      message(sprintf("Codebook/support files available: %s", n_files))
    }
  }

  if (isTRUE(assign_global)) {
    assign(srvy, data, envir = .GlobalEnv)
  }

  data
}

#' Preview a Quebec election study
#'
#' @param srvy A qesR survey code (see [get_qescodes()]).
#' @param obs Number of observations to preview. Default is `6`.
#' @param file Optional regular expression to select a specific file when a
#'   Dataverse dataset has multiple files.
#'
#' @return A data frame/tibble preview.
#' @export
get_preview <- function(srvy, obs = 6L, file = NULL) {
  if (!is.numeric(obs) || length(obs) != 1L || is.na(obs) || obs < 1L) {
    stop("`obs` must be a single number greater than or equal to 1.", call. = FALSE)
  }

  data <- get_qes(srvy, file = file, assign_global = FALSE, with_codebook = TRUE, quiet = TRUE)
  utils::head(data, n = as.integer(obs))
}

.get_codebook_row <- function(codebook, q) {
  if (!is.data.frame(codebook) || nrow(codebook) == 0L || !("variable" %in% names(codebook))) {
    return(NULL)
  }

  idx <- match(q, codebook$variable)
  if (is.na(idx)) {
    return(NULL)
  }

  codebook[idx, , drop = FALSE]
}

.regex_escape <- function(x) {
  gsub("([][{}()+*^$|\\\\?.-])", "\\\\\\1", x)
}

.is_likely_truncated_question <- function(x) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }

  txt <- .squish_ws(x)
  if (!nzchar(txt) || nchar(txt) < 20L) {
    return(FALSE)
  }

  if (grepl("[?.!]$", txt)) {
    return(FALSE)
  }

  tail_word <- tolower(sub(".*\\b([[:alpha:]']+)$", "\\1", txt))
  dangling <- c("a", "an", "the", "to", "of", "for", "in", "on", "at", "with", "from", "and", "or")
  tail_word %in% dangling
}

.read_pdf_lines_with_pdftotext <- function(pdf_file, quiet = TRUE) {
  if (!nzchar(Sys.which("pdftotext"))) {
    return(character(0))
  }

  txt_file <- tempfile(fileext = ".txt")
  on.exit(unlink(txt_file), add = TRUE)

  status <- suppressWarnings(system2(
    "pdftotext",
    args = c("-layout", pdf_file, txt_file),
    stdout = if (isTRUE(quiet)) FALSE else "",
    stderr = if (isTRUE(quiet)) FALSE else ""
  ))

  if (!identical(status, 0L) || !file.exists(txt_file)) {
    return(character(0))
  }

  tryCatch(
    readLines(txt_file, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
}

.read_pdf_lines_with_gs <- function(pdf_file, quiet = TRUE) {
  if (!nzchar(Sys.which("gs"))) {
    return(character(0))
  }

  txt_file <- tempfile(fileext = ".txt")
  on.exit(unlink(txt_file), add = TRUE)

  status <- suppressWarnings(system2(
    "gs",
    args = c(
      "-q",
      "-dNOPAUSE",
      "-dBATCH",
      "-sDEVICE=txtwrite",
      paste0("-sOutputFile=", txt_file),
      pdf_file
    ),
    stdout = if (isTRUE(quiet)) FALSE else "",
    stderr = if (isTRUE(quiet)) FALSE else ""
  ))

  if (!identical(status, 0L) || !file.exists(txt_file)) {
    return(character(0))
  }

  tryCatch(
    readLines(txt_file, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
}

.extract_pdf_text_lines <- function(pdf_file, quiet = TRUE) {
  candidates <- list(
    .read_pdf_lines_with_pdftotext(pdf_file, quiet = quiet),
    .read_pdf_lines_with_gs(pdf_file, quiet = quiet)
  )

  for (lines in candidates) {
    lines <- .squish_ws(lines)
    lines <- lines[!is.na(lines) & nzchar(lines)]
    if (length(lines) > 0L) {
      return(lines)
    }
  }

  character(0)
}

.clean_expanded_question <- function(candidate, q, partial = NA_character_) {
  candidate <- .squish_ws(candidate)
  if (!nzchar(candidate)) {
    return(NA_character_)
  }

  candidate <- sub(
    paste0("^", .regex_escape(q), "[:\\-\\s]*"),
    "",
    candidate,
    ignore.case = TRUE,
    perl = TRUE
  )
  candidate <- sub("\\s*\\u25BC.*$", "", candidate)
  candidate <- sub("\\s+If\\s+.*$", "", candidate)

  if (grepl("\\?", candidate, perl = TRUE)) {
    candidate <- sub("^(.*?\\?)\\s.*$", "\\1", candidate, perl = TRUE)
  }

  candidate <- .squish_ws(candidate)
  if (!nzchar(candidate)) {
    return(NA_character_)
  }

  if (!is.na(partial) && nzchar(partial) && nchar(candidate) <= nchar(partial) + 5L) {
    return(NA_character_)
  }

  candidate
}

.expand_question_from_pdf <- function(codebook, q, partial = NA_character_, quiet = TRUE) {
  files <- attr(codebook, "codebook_files", exact = TRUE)
  if (is.null(files) || nrow(files) == 0L) {
    return(NA_character_)
  }

  if (!("extension" %in% names(files))) {
    return(NA_character_)
  }

  pdf_idx <- which(tolower(files$extension) == "pdf")
  if (length(pdf_idx) == 0L) {
    return(NA_character_)
  }

  survey_code <- attr(codebook, "survey_code", exact = TRUE)
  if (is.null(survey_code) || !(survey_code %in% .qes_catalog$qes_survey_code)) {
    return(NA_character_)
  }

  study <- .get_qes_study(survey_code)
  file_row <- files[pdf_idx[1], , drop = FALSE]
  if (!("download_url" %in% names(file_row)) || is.na(file_row$download_url[1])) {
    return(NA_character_)
  }

  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)

  downloaded <- tryCatch(
    {
      .download_file_with_fallback(
        file_row$download_url[1],
        pdf_file,
        quiet = quiet,
        allow_insecure_retry = isTRUE(study$allow_insecure_retry)
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!downloaded) {
    return(NA_character_)
  }

  lines <- .extract_pdf_text_lines(pdf_file, quiet = quiet)
  if (length(lines) == 0L) {
    return(NA_character_)
  }

  idx <- grep(
    paste0("\\b", .regex_escape(q), "\\b"),
    lines,
    ignore.case = TRUE,
    perl = TRUE
  )

  if (length(idx) == 0L && !is.na(partial) && nzchar(partial)) {
    key <- substr(.squish_ws(partial), 1L, min(25L, nchar(.squish_ws(partial))))
    idx <- grep(key, lines, ignore.case = TRUE, fixed = TRUE)
  }

  if (length(idx) == 0L) {
    return(NA_character_)
  }

  window <- lines[idx[1]:min(length(lines), idx[1] + 15L)]
  candidate <- .squish_ws(paste(window, collapse = " "))
  candidate <- sub(
    paste0("^.*?\\b", .regex_escape(q), "\\b[:\\-\\s]*"),
    "",
    candidate,
    ignore.case = TRUE,
    perl = TRUE
  )
  candidate <- .squish_ws(candidate)

  if (!nzchar(candidate)) {
    return(NA_character_)
  }

  if (!is.na(partial) && nzchar(partial)) {
    pos <- regexpr(.squish_ws(partial), candidate, fixed = TRUE)
    if (!is.na(pos[1]) && pos[1] > 1L) {
      candidate <- substr(candidate, pos[1], nchar(candidate))
      candidate <- .squish_ws(candidate)
    }
  }

  .clean_expanded_question(candidate, q = q, partial = partial)
}

.get_question_from_codebook <- function(codebook, q, full = TRUE, quiet = TRUE) {
  row <- .get_codebook_row(codebook, q)
  if (is.null(row)) {
    return(NA_character_)
  }

  question <- if ("question" %in% names(row)) as.character(row$question[1]) else NA_character_
  label <- if ("label" %in% names(row)) as.character(row$label[1]) else NA_character_

  base <- NA_character_
  if (!is.na(question) && nzchar(question)) {
    base <- question
  } else if (!is.na(label) && nzchar(label)) {
    base <- label
  }

  if (!isTRUE(full) || is.na(base) || !.is_likely_truncated_question(base)) {
    return(base)
  }

  expanded <- .expand_question_from_pdf(codebook, q, partial = base, quiet = quiet)
  if (!is.na(expanded) && nchar(expanded) > nchar(base)) {
    return(expanded)
  }

  base
}

.maybe_fetch_codebook <- function(srvy, quiet = TRUE) {
  if (!is.character(srvy) || length(srvy) != 1L || is.na(srvy) || !nzchar(srvy)) {
    return(NULL)
  }

  if (!(srvy %in% .qes_catalog$qes_survey_code)) {
    return(NULL)
  }

  tryCatch(
    get_codebook(srvy, assign_global = FALSE, quiet = quiet),
    error = function(e) NULL
  )
}

.resolve_question_column <- function(data, q) {
  if (q %in% names(data)) {
    return(q)
  }

  lower_names <- tolower(names(data))
  exact_ci <- names(data)[lower_names == tolower(q)]
  if (length(exact_ci) == 1L) {
    return(exact_ci)
  }

  starts_with <- names(data)[startsWith(lower_names, tolower(q))]
  if (length(starts_with) == 1L) {
    return(starts_with)
  }

  contains <- names(data)[grepl(tolower(q), lower_names, fixed = TRUE)]
  if (length(contains) == 1L) {
    return(contains)
  }

  suggestion_pool <- unique(c(exact_ci, starts_with, contains))
  suggestion <- if (length(suggestion_pool) > 0L) {
    paste(utils::head(suggestion_pool, 5), collapse = ", ")
  } else {
    ""
  }

  if (nzchar(suggestion)) {
    stop(
      sprintf(
        "Column '%s' was not found in the data object. Close matches: %s",
        q,
        suggestion
      ),
      call. = FALSE
    )
  }

  stop(sprintf("Column '%s' was not found in the data object.", q), call. = FALSE)
}

#' Get survey question text from labels or the codebook
#'
#' @param do A data object or the character name of a data object in the
#'   global environment.
#' @param q Column name whose question label should be returned.
#' @param full If `TRUE`, try to recover full question text when the codebook
#'   metadata appears truncated.
#'
#' @return A character string containing the question text, or `NA_character_`
#'   when no question text is found.
#' @export
get_question <- function(do, q, full = TRUE) {
  .assert_single_string(q, "q")

  object_name <- NULL
  if (is.character(do) && length(do) == 1L) {
    object_name <- do
    if (!exists(do, envir = .GlobalEnv, inherits = FALSE)) {
      stop(sprintf("Object '%s' was not found in the global environment.", do), call. = FALSE)
    }
    data <- get(do, envir = .GlobalEnv, inherits = FALSE)
  } else if (is.data.frame(do)) {
    data <- do
  } else {
    stop("`do` must be a data.frame or the name of one in the global environment.", call. = FALSE)
  }

  q <- .resolve_question_column(data, q)

  question <- attr(data[[q]], "qes_question", exact = TRUE)
  question_hint <- NA_character_
  if (!is.null(question) && nzchar(as.character(question))) {
    question_hint <- as.character(question)
    if (!isTRUE(full) || !.is_likely_truncated_question(question_hint)) {
      return(question_hint)
    }
  }

  label <- attr(data[[q]], "label", exact = TRUE)
  label_hint <- if (!is.null(label) && nzchar(as.character(label))) as.character(label) else NA_character_

  variable_labels <- attr(data, "variable.labels", exact = TRUE)
  if (!is.null(variable_labels) && q %in% names(variable_labels)) {
    label <- variable_labels[[q]]
    if (!is.null(label) && nzchar(as.character(label))) {
      label_hint <- as.character(label)
    }
  }

  from_attr <- .get_question_from_codebook(
    attr(data, "qes_codebook", exact = TRUE),
    q,
    full = full,
    quiet = TRUE
  )
  if (!is.na(from_attr)) {
    return(from_attr)
  }

  if (!is.null(object_name)) {
    codebook_name <- paste0(object_name, "_codebook")
    if (exists(codebook_name, envir = .GlobalEnv, inherits = FALSE)) {
      from_obj <- .get_question_from_codebook(
        get(codebook_name, envir = .GlobalEnv, inherits = FALSE),
        q,
        full = full,
        quiet = TRUE
      )
      if (!is.na(from_obj)) {
        return(from_obj)
      }
    }
  }

  srvy_hint <- attr(data, "qes_survey_code", exact = TRUE)
  if (is.null(srvy_hint) && !is.null(object_name) && (object_name %in% .qes_catalog$qes_survey_code)) {
    srvy_hint <- object_name
  }

  fetched_codebook <- .maybe_fetch_codebook(srvy_hint, quiet = TRUE)
  from_fetched <- .get_question_from_codebook(fetched_codebook, q, full = full, quiet = TRUE)
  if (!is.na(from_fetched)) {
    if (!is.null(object_name) && exists(object_name, envir = .GlobalEnv, inherits = FALSE)) {
      obj <- get(object_name, envir = .GlobalEnv, inherits = FALSE)
      attr(obj, "qes_codebook") <- fetched_codebook
      if (!is.null(srvy_hint)) {
        attr(obj, "qes_survey_code") <- srvy_hint
      }
      assign(object_name, obj, envir = .GlobalEnv)
    }
    return(from_fetched)
  }

  if (!is.na(label_hint) && nzchar(label_hint)) {
    return(label_hint)
  }

  if (!is.na(question_hint) && nzchar(question_hint)) {
    return(question_hint)
  }

  warning(sprintf("No question label was found for '%s'.", q), call. = FALSE)
  NA_character_
}
