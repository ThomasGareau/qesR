#' Download and Load a Quebec Election Study
#'
#' Downloads a study file from Dataverse, applies labels when available, and attaches a codebook.
#'
#' `get_qes()` returns the data. It does not write anything into your
#' workspace unless you ask for it with `assign_global = TRUE`: write
#' `qes2018 <- get_qes("qes2018")`. The first call in a session that leaves
#' `assign_global` unset prints a one-time note about this change from qesR
#' 0.4.4; passing `assign_global` explicitly (TRUE or FALSE) avoids it.
#'
#' @param srvy A qesR survey code from `get_qescodes()`. Codes are trimmed
#'   and case-insensitive (`" QES2018 "` is `"qes2018"`); an unknown code is an
#'   error of class `qesR_error_unknown_study` that suggests near matches.
#' @param file Optional regular expression for choosing one file in multi-file datasets.
#' @param assign_global If TRUE, also assign the data as `<code>` into the
#'   environment `get_qes()` was called from (the global environment only when
#'   called at top level), where `<code>` is the canonical study code. With
#'   `with_codebook = TRUE`, the codebook is assigned as `<code>_codebook` too.
#'   Defaults to FALSE. The data is returned either way.
#' @param with_codebook If TRUE, attach codebook metadata as the `qes_codebook`
#'   attribute (and assign \code{<code>_codebook} when `assign_global = TRUE`).
#' @param quiet If TRUE, suppress informational output.
#'
#' @return A labelled data frame/tibble for the selected survey, returned
#'   visibly. `attr(, "qes_survey_code")` holds the canonical study code.
#' @examples
#' \donttest{
#'   qes2022 <- get_qes("qes2022")
#'   names(qes2022)[1:10]
#' }
#' @export
get_qes <- function(srvy, file = NULL, assign_global = FALSE, with_codebook = TRUE, quiet = FALSE) {
  .get_qes_impl(
    srvy, file, assign_global, with_codebook, quiet,
    envir = parent.frame(),
    assign_missing = missing(assign_global)
  )
}

# `envir`: where opt-in assignment lands (the caller of the exported name).
# `assign_missing`: TRUE only when a user called an exported entry point
# without assign_global; internal callers leave it FALSE.
.get_qes_impl <- function(srvy, file = NULL, assign_global = FALSE, with_codebook = TRUE,
                          quiet = FALSE, envir = NULL, assign_missing = FALSE) {
  study <- .get_qes_study(srvy)
  code <- study$qes_survey_code

  .qes_inform(
    "get_qes_banner",
    class = "qesR_message_download",
    args = list(
      code,
      if (identical(.qes_lang(), "fr")) study$name_fr else study$name_en,
      study$doi_url,
      study$documentation
    ),
    data = list(study = code),
    quiet = quiet
  )

  payload <- .download_and_read_qes(study, file = file, quiet = quiet, read_data = TRUE)
  data <- payload$data
  attr(data, "qes_survey_code") <- code

  if (isTRUE(with_codebook)) {
    attr(data, "qes_codebook") <- payload$codebook
    .qes_codebook_cache[[.codebook_cache_key(code, file = file)]] <- payload$codebook

    codebook_files <- attr(payload$codebook, "codebook_files", exact = TRUE)
    .qes_inform(
      "codebook_counts",
      class = "qesR_message_download",
      args = list(
        nrow(payload$codebook),
        if (is.null(codebook_files)) 0L else nrow(codebook_files)
      ),
      data = list(study = code),
      quiet = quiet
    )
  }

  if (isTRUE(assign_global)) {
    if (isTRUE(with_codebook)) {
      .qes_assign(paste0(code, "_codebook"), payload$codebook, envir)
    }
    .qes_assign(code, data, envir)
  } else if (isTRUE(assign_missing)) {
    .qes_assign_default_notice("get_qes", code)
  }

  data
}

#' Preview a Quebec Election Study
#'
#' Loads a study and returns the first observations.
#'
#' Soft-deprecated: use `head(get_qes(srvy), obs)`. `get_preview()` keeps
#' working and will not be removed; it prints a one-time notice (see
#' [qesR-deprecated]).
#'
#' @param srvy A qesR survey code from `get_qescodes()`.
#' @param obs Number of observations to return.
#' @param file Optional regular expression for choosing one file in multi-file datasets.
#'
#' @return A data frame/tibble preview (with attached codebook metadata).
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   get_preview("qes2022", obs = 3)
#' }
#' @export
get_preview <- function(srvy, obs = 6L, file = NULL) {
  .qes_deprecate("get_preview")
  .get_preview_impl(srvy, obs = obs, file = file)
}

.get_preview_impl <- function(srvy, obs = 6L, file = NULL) {
  if (!is.numeric(obs) || length(obs) != 1L || !is.finite(obs) || obs < 1 || obs != floor(obs)) {
    .qes_abort(
      "input_obs",
      class = "qesR_error_input",
      data = list(arg = "obs", value = obs)
    )
  }

  data <- .get_qes_impl(srvy, file = file, assign_global = FALSE, with_codebook = TRUE, quiet = TRUE)
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

  cmd <- paste(
    shQuote(Sys.which("pdftotext")),
    "-layout",
    shQuote(pdf_file),
    shQuote(txt_file)
  )

  status <- suppressWarnings(system(
    cmd,
    intern = FALSE,
    ignore.stdout = isTRUE(quiet),
    ignore.stderr = isTRUE(quiet)
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

  cmd <- paste(
    shQuote(Sys.which("gs")),
    "-q -dNOPAUSE -dBATCH -sDEVICE=txtwrite",
    paste0("-sOutputFile=", shQuote(txt_file)),
    shQuote(pdf_file)
  )

  status <- suppressWarnings(system(
    cmd,
    intern = FALSE,
    ignore.stdout = isTRUE(quiet),
    ignore.stderr = isTRUE(quiet)
  ))

  if (!identical(status, 0L) || !file.exists(txt_file)) {
    return(character(0))
  }

  tryCatch(
    readLines(txt_file, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
}

.read_pdf_lines_with_python <- function(pdf_file, quiet = TRUE) {
  py <- Sys.which("python3")
  if (!nzchar(py)) {
    return(character(0))
  }

  script_file <- tempfile(fileext = ".py")
  out_file <- tempfile(fileext = ".txt")
  on.exit(unlink(c(script_file, out_file)), add = TRUE)

  writeLines(
    c(
      "import sys",
      "try:",
      "    from PyPDF2 import PdfReader",
      "except Exception:",
      "    sys.exit(1)",
      "pdf_path = sys.argv[1]",
      "try:",
      "    reader = PdfReader(pdf_path)",
      "except Exception:",
      "    sys.exit(1)",
      "with open(sys.argv[2], 'w', encoding='utf-8') as f:",
      "    for page in reader.pages:",
      "        txt = page.extract_text() or ''",
      "        if txt:",
      "            f.write(txt)",
      "            f.write('\\n')"
    ),
    con = script_file,
    useBytes = TRUE
  )

  cmd <- paste(
    shQuote(py),
    shQuote(script_file),
    shQuote(pdf_file),
    shQuote(out_file)
  )

  status <- tryCatch(
    suppressWarnings(system(
      cmd,
      intern = FALSE,
      ignore.stdout = isTRUE(quiet),
      ignore.stderr = isTRUE(quiet)
    )),
    error = function(e) 1L
  )

  if (!identical(status, 0L) || !file.exists(out_file)) {
    return(character(0))
  }

  tryCatch(
    readLines(out_file, warn = FALSE, encoding = "UTF-8"),
    error = function(e) character(0)
  )
}

.extract_pdf_text_lines <- function(pdf_file, quiet = TRUE) {
  candidates <- list(
    .read_pdf_lines_with_pdftotext(pdf_file, quiet = quiet),
    .read_pdf_lines_with_gs(pdf_file, quiet = quiet),
    .read_pdf_lines_with_python(pdf_file, quiet = quiet)
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

  file_row <- files[pdf_idx[1], , drop = FALSE]
  if (!("download_url" %in% names(file_row)) || is.na(file_row$download_url[1])) {
    return(NA_character_)
  }

  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)

  downloaded <- tryCatch(
    {
      .qes_fetch_file(
        file_row$download_url[1],
        pdf_file,
        quiet = quiet
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
    .qes_codebook_impl(srvy, assign_global = FALSE, quiet = quiet),
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

  suggestions <- utils::head(unique(c(exact_ci, starts_with, contains)), 5L)

  if (length(suggestions) > 0L) {
    .qes_abort(
      "unknown_variable_suggest",
      class = "qesR_error_unknown_variable",
      args = list(.qes_q(q), .qes_q(suggestions)),
      data = list(study = NA_character_, variables = q, suggestions = suggestions)
    )
  }

  .qes_abort(
    "unknown_variable",
    class = "qesR_error_unknown_variable",
    args = list(.qes_q(q)),
    data = list(study = NA_character_, variables = q, suggestions = character(0))
  )
}

#' Get Survey Question Text
#'
#' Returns question text from variable labels or an attached/paired codebook, with optional full-question recovery.
#'
#' @param do A data.frame or the name of one in the calling environment.
#' @param q Column name whose question text should be returned.
#' @param full If TRUE, try to recover full question text when metadata appears truncated.
#'
#' @return A character scalar with question text, or `NA_character_` (with a
#'   warning of class `qesR_warning`) when none exists.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   d <- get_qes("qes2022")
#'   get_question(d, "cps_age_in_years")
#' }
#' @export
get_question <- function(do, q, full = TRUE) {
  .qes_deprecate("get_question")
  .get_question_impl(do, q, full = full, envir = parent.frame())
}

# `envir`: the caller's frame, where a character `do` is looked up (read only,
# never inherited from enclosing frames).
.get_question_impl <- function(do, q, full = TRUE, envir) {
  .assert_single_string(q, "q")

  object_name <- NULL
  object_env <- NULL
  if (is.character(do) && length(do) == 1L) {
    object_name <- do
    object_env <- envir
    if (!exists(do, envir = object_env, inherits = FALSE)) {
      .qes_abort(
        "input_object_missing",
        class = "qesR_error_input",
        args = list(.qes_q(do)),
        data = list(arg = "do", value = do)
      )
    }
    data <- get(do, envir = object_env, inherits = FALSE)
  } else if (is.data.frame(do)) {
    data <- do
  } else {
    .qes_abort(
      "input_do",
      class = "qesR_error_input",
      data = list(arg = "do", value = do)
    )
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
    if (exists(codebook_name, envir = object_env, inherits = FALSE)) {
      from_obj <- .get_question_from_codebook(
        get(codebook_name, envir = object_env, inherits = FALSE),
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
    return(from_fetched)
  }

  if (!is.na(label_hint) && nzchar(label_hint)) {
    return(label_hint)
  }

  if (!is.na(question_hint) && nzchar(question_hint)) {
    return(question_hint)
  }

  .qes_warn(
    "question_missing",
    args = list(.qes_q(q)),
    data = list(variable = q)
  )
  NA_character_
}
