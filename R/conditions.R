# Conditions, message language and once-per-session state (design.md
# section 7).
#
# Every condition qesR signals is built here, from a key of the message table
# (R/messages.R). A condition carries:
#   id     the message key;
#   lang   the language its message was rendered in ("en" or "fr");
#   args   the arguments used to render the message (so it can be rendered
#          again in another language, see .qes_condition_text());
#   parent the root cause, when there is one;
# plus data fields named by the design (study, url, arg, value, ...).
# Code and tests branch on the class and these fields, never on message text
# (design rule P7).

# ---- class tree ----------------------------------------------------------

# Parent of every non-root class. Roots are qesR_error, qesR_warning and
# qesR_message; a class missing here hangs directly under its root.
.qes_condition_parent <- c(
  qesR_error_http = "qesR_error_network",
  qesR_error_http_refused = "qesR_error_http",
  qesR_error_tls = "qesR_error_network",
  qesR_error_offline = "qesR_error_network",
  qesR_error_checksum = "qesR_error_source",
  qesR_error_rowcount = "qesR_error_source"
)

.qes_class_chain <- function(class, root) {
  chain <- character(0)
  cls <- class
  while (!is.null(cls) && !is.na(cls) && !identical(cls, root)) {
    chain <- c(chain, cls)
    cls <- if (cls %in% names(.qes_condition_parent)) {
      .qes_condition_parent[[cls]]
    } else {
      root
    }
  }
  c(chain, root)
}

# ---- language --------------------------------------------------------------

# Language of messages only (never of returned values, rule P4):
# option qesR.lang, then QESR_LANG, then LANGUAGE, then the messages locale.
# The first one that is set decides; a value starting with "fr" means French.
.qes_lang <- function() {
  locale_cat <- if (identical(.Platform$OS.type, "windows")) "LC_COLLATE" else "LC_MESSAGES"
  sources <- list(
    function() getOption("qesR.lang"),
    function() Sys.getenv("QESR_LANG"),
    function() Sys.getenv("LANGUAGE"),
    function() tryCatch(Sys.getlocale(locale_cat), error = function(e) "")
  )
  for (get_value in sources) {
    value <- get_value()
    if (is.character(value) && length(value) >= 1L && !is.na(value[1]) && nzchar(value[1])) {
      return(if (grepl("^fr", value[1], ignore.case = TRUE)) "fr" else "en")
    }
  }
  "en"
}

# ---- rendering -----------------------------------------------------------

# Mark values to be quoted in a message. Quoting follows the message
# language: 'x' in English, guillemets in French; vectors are comma-joined.
.qes_q <- function(x) {
  structure(list(as.character(x)), class = "qesR_quoted")
}

.qes_render_arg <- function(arg, lang) {
  if (inherits(arg, "qesR_quoted")) {
    x <- arg[[1]]
    if (length(x) == 0L) {
      return("")
    }
    quoted <- if (identical(lang, "fr")) {
      paste0("\u00ab\u00a0", x, "\u00a0\u00bb")
    } else {
      paste0("'", x, "'")
    }
    return(paste(quoted, collapse = ", "))
  }
  if (length(arg) == 0L) {
    return("")
  }
  paste(as.character(arg), collapse = ", ")
}

# Render message `id` in `lang` with the positional arguments `args`.
.qes_msg <- function(id, args = list(), lang = .qes_lang()) {
  entry <- .qes_messages[[id]]
  if (is.null(entry)) {
    stop(sprintf("qesR internal error: unknown message key '%s'.", id), call. = FALSE)
  }
  lang <- if (identical(lang, "fr")) "fr" else "en"
  template <- entry[[lang]]
  if (length(args) == 0L) {
    return(template)
  }
  rendered <- lapply(args, .qes_render_arg, lang = lang)
  do.call(sprintf, c(list(template), rendered))
}

.qes_is_condition <- function(cnd) {
  inherits(cnd, c("qesR_error", "qesR_warning", "qesR_message")) && !is.null(cnd$id)
}

# The message of a condition in a fixed language. qesR conditions are rendered
# again from their key and arguments; any other condition keeps its own text.
# Used where text is *returned* (not printed), so that returned values never
# depend on the session language (P4).
.qes_condition_text <- function(cnd, lang = "en") {
  if (!.qes_is_condition(cnd)) {
    return(conditionMessage(cnd))
  }
  .qes_full_message(cnd$id, cnd$args, lang, cnd$parent, cnd$details)
}

.qes_full_message <- function(id, args, lang, parent = NULL, details = NULL) {
  msg <- .qes_msg(id, args, lang)
  if (length(details) > 0L) {
    msg <- paste0(msg, "\n", .qes_msg("details", list(paste(details, collapse = "; ")), lang))
  }
  if (!is.null(parent)) {
    parent_text <- if (.qes_is_condition(parent)) {
      .qes_condition_text(parent, lang)
    } else {
      conditionMessage(parent)
    }
    parent_text <- sub("\n$", "", parent_text)
    msg <- paste0(msg, "\n", .qes_msg("caused_by", list(parent_text), lang))
  }
  msg
}

# ---- constructors ----------------------------------------------------------

.qes_condition <- function(id, class, root, args, data, parent, details, call) {
  lang <- .qes_lang()
  fields <- c(
    list(
      message = .qes_full_message(id, args, lang, parent, details),
      call = call,
      id = id,
      lang = lang,
      args = args,
      parent = parent,
      details = details
    ),
    data
  )
  structure(fields, class = c(.qes_class_chain(class %||% root, root), .qes_root_base(root)))
}

.qes_root_base <- function(root) {
  switch(
    root,
    qesR_error = c("error", "condition"),
    qesR_warning = c("warning", "condition"),
    qesR_message = c("message", "condition")
  )
}

# Signal an error of class `class` (a qesR_error_* name) with message `id`.
.qes_abort <- function(id, class = NULL, args = list(), data = list(),
                       parent = NULL, details = NULL, call = NULL) {
  cnd <- .qes_condition(id, class, "qesR_error", args, data, parent, details, call)
  stop(cnd)
}

.qes_warn <- function(id, class = NULL, args = list(), data = list(),
                      parent = NULL, details = NULL, call = NULL) {
  cnd <- .qes_condition(id, class, "qesR_warning", args, data, parent, details, call)
  warning(cnd)
  invisible(cnd)
}

# Informational message; `quiet = TRUE` silences it. Notices that `quiet` must
# not silence (deprecation, assignment default) pass quiet = FALSE.
.qes_inform <- function(id, class = NULL, args = list(), data = list(), quiet = FALSE) {
  if (isTRUE(quiet)) {
    return(invisible(NULL))
  }
  cnd <- .qes_condition(id, class, "qesR_message", args, data, NULL, NULL, NULL)
  cnd$message <- paste0(cnd$message, "\n")
  message(cnd)
  invisible(cnd)
}

# ---- once-per-session state --------------------------------------------------

# Flags for notices shown once per session (deprecation, assignment default,
# and later licence, values changed, disk-cache tip, ignored argument).
.qes_once <- new.env(parent = emptyenv())

# TRUE the first time `key` is seen in this session, FALSE afterwards.
.qes_once_first <- function(key) {
  if (isTRUE(.qes_once[[key]])) {
    return(FALSE)
  }
  .qes_once[[key]] <- TRUE
  TRUE
}

# Forget every once-per-session flag (for tests; see local_qes_once()).
.qes_reset_once <- function() {
  rm(list = ls(.qes_once, all.names = TRUE), envir = .qes_once)
  invisible()
}
