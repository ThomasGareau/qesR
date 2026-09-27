.qes_codebook_cache <- new.env(parent = emptyenv())

.codebook_cache_key <- function(srvy, file = NULL) {
  paste0(srvy, "::", ifelse(is.null(file), "", file))
}

.normalize_codebook_class <- function(codebook) {
  if (!inherits(codebook, "qes_codebook")) {
    class(codebook) <- unique(c("qes_codebook", class(codebook)))
  }
  codebook
}

.codebook_attr_names <- function(codebook) {
  setdiff(names(attributes(codebook)), c("names", "row.names", "class"))
}

.copy_codebook_attrs <- function(from, to) {
  for (nm in .codebook_attr_names(from)) {
    attr(to, nm) <- attr(from, nm, exact = TRUE)
  }
  to
}

.codebook_value_map <- function(codebook) {
  map <- attr(codebook, "value_labels_map", exact = TRUE)
  if (is.null(map)) {
    return(list())
  }
  map
}

.layout_codebook_compact <- function(codebook) {
  keep <- intersect(c("variable", "label", "question", "n_value_labels"), names(codebook))
  out <- as.data.frame(codebook[, keep, drop = FALSE], stringsAsFactors = FALSE)
  class(out) <- class(codebook)
  .copy_codebook_attrs(codebook, out)
}

.layout_codebook_wide <- function(codebook) {
  out <- .layout_codebook_compact(codebook)
  map <- .codebook_value_map(codebook)
  out$value_labels <- I(lapply(out$variable, function(v) map[[v]] %||% character(0)))
  class(out) <- class(codebook)
  .copy_codebook_attrs(codebook, out)
}

.layout_codebook_long <- function(codebook) {
  compact <- .layout_codebook_compact(codebook)
  map <- .codebook_value_map(codebook)

  rows <- vector("list", length(compact$variable))
  n_out <- 0L

  for (i in seq_len(nrow(compact))) {
    var <- compact$variable[i]
    vals <- map[[var]]

    if (is.null(vals) || length(vals) == 0L) {
      next
    }

    n_out <- n_out + 1L
    rows[[n_out]] <- data.frame(
      variable = var,
      value = names(vals),
      value_label = unname(vals),
      label = compact$label[i],
      question = compact$question[i],
      stringsAsFactors = FALSE
    )
  }

  if (n_out == 0L) {
    out <- data.frame(
      variable = character(0),
      value = character(0),
      value_label = character(0),
      label = character(0),
      question = character(0),
      stringsAsFactors = FALSE
    )
  } else {
    out <- do.call(rbind, rows[seq_len(n_out)])
  }

  class(out) <- class(codebook)
  .copy_codebook_attrs(codebook, out)
}

#' Reformat a qesR Codebook
#'
#' Reformats a `qes_codebook` object into compact, wide, or long layout.
#'
#' @param codebook A `qes_codebook` object from `qes_codebook()`.
#' @param layout One of `"compact"`, `"wide"`, or `"long"`.
#'
#' @return A reformatted codebook data frame.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   cb <- qes_codebook("qes2022")
#'   format_codebook(cb, layout = "long")
#' }
#' @export
format_codebook <- function(codebook, layout = c("compact", "wide", "long")) {
  .qes_deprecate("format_codebook")
  layout <- match.arg(layout)
  .format_codebook_impl(codebook, layout = layout)
}

.qes_abort_not_codebook <- function(codebook) {
  .qes_abort(
    "input_codebook",
    class = "qesR_error_input",
    data = list(arg = "codebook", value = class(codebook))
  )
}

.format_codebook_impl <- function(codebook, layout = c("compact", "wide", "long")) {
  layout <- match.arg(layout)

  if (!is.data.frame(codebook)) {
    .qes_abort_not_codebook(codebook)
  }

  codebook <- .normalize_codebook_class(codebook)

  switch(
    layout,
    compact = .layout_codebook_compact(codebook),
    wide = .layout_codebook_wide(codebook),
    long = .layout_codebook_long(codebook)
  )
}

#' Get Value Labels from a Codebook
#'
#' Extracts value-label mappings from a `qes_codebook` object.
#'
#' @param codebook A `qes_codebook` object from `qes_codebook()`.
#' @param variable Optional variable name. If `NULL`, returns mappings for all variables.
#' @param long If TRUE, return a long data frame with `variable`, `value`, and `value_label` columns.
#'
#' @return A named list of value-label vectors, or a long data frame when `long = TRUE`.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   cb <- qes_codebook("qes2022")
#'   get_value_labels(cb)
#' }
#' @export
get_value_labels <- function(codebook, variable = NULL, long = FALSE) {
  .qes_deprecate("get_value_labels")
  .get_value_labels_impl(codebook, variable = variable, long = long)
}

.get_value_labels_impl <- function(codebook, variable = NULL, long = FALSE) {
  if (!is.data.frame(codebook)) {
    .qes_abort_not_codebook(codebook)
  }

  map <- .codebook_value_map(codebook)

  if (!is.null(variable)) {
    .assert_single_string(variable, "variable")
    map <- map[intersect(variable, names(map))]
  }

  if (!isTRUE(long)) {
    return(map)
  }

  map <- map[vapply(map, function(x) !is.null(x) && length(x) > 0L, logical(1))]

  if (length(map) == 0L) {
    return(data.frame(
      variable = character(0),
      value = character(0),
      value_label = character(0),
      stringsAsFactors = FALSE
    ))
  }

  rows <- lapply(names(map), function(v) {
    vals <- map[[v]]
    data.frame(
      variable = v,
      value = names(vals),
      value_label = unname(vals),
      stringsAsFactors = FALSE
    )
  })

  do.call(rbind, rows)
}

#' Get a Quebec Election Study Codebook
#'
#' Downloads and returns study codebook metadata; results are cached per session.
#'
#' Soft-deprecated: use [qes_codebook()], which takes the same arguments.
#' `get_codebook()` keeps working and will not be removed; it prints a
#' one-time notice (see [qesR-deprecated]).
#'
#' @param srvy A qesR survey code from `qes_studies()`. Codes are trimmed and
#'   case-insensitive.
#' @param file Optional regular expression for choosing one file in multi-file datasets.
#' @param assign_global If TRUE, also assign the returned codebook as
#'   \code{<code>_codebook} into the environment the function was called from
#'   (the global environment only when called at top level), where `<code>` is
#'   the canonical study code. Defaults to FALSE.
#' @param quiet If TRUE, suppress informational output.
#' @param refresh If TRUE, force a fresh download instead of using cache.
#' @param layout One of `"compact"`, `"wide"`, or `"long"`.
#'
#' @return A `qes_codebook` data frame with variable metadata and codebook file
#'   manifest attributes, returned visibly.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   cb <- qes_codebook("qes2022")
#'   head(cb)
#' }
#' @export
get_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  .qes_deprecate("get_codebook")
  .qes_codebook_impl(
    srvy, file = file, assign_global = assign_global, quiet = quiet,
    refresh = refresh, layout = layout, envir = parent.frame()
  )
}

# The codebook pipeline behind qes_codebook() and its legacy aliases.
# `envir`: where opt-in assignment lands (the caller of the exported name).
.qes_codebook_impl <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long"),
  envir = NULL
) {
  layout <- match.arg(layout)
  study <- .get_qes_study(srvy, demo = TRUE)
  # resolve `file` first, as get_qes() does: it may name the data file of
  # another study of the same deposit, whose code then names the codebook
  selection <- .qes_select_data_file(study$qes_survey_code, file = file, quiet = quiet)
  if (!identical(selection$study, study$qes_survey_code)) {
    study <- .get_qes_study(selection$study, demo = TRUE)
  }
  code <- study$qes_survey_code
  key <- .codebook_cache_key(code, file = file)

  if (!isTRUE(refresh) && exists(key, envir = .qes_codebook_cache, inherits = FALSE)) {
    codebook <- get(key, envir = .qes_codebook_cache, inherits = FALSE)
  } else {
    payload <- .download_and_read_qes(study, file = file, quiet = quiet, read_data = TRUE,
      selection = selection)
    codebook <- payload$codebook
    codebook <- .normalize_codebook_class(codebook)
    attr(codebook, "cached_at") <- Sys.time()
    .qes_codebook_cache[[key]] <- codebook
  }

  out <- .format_codebook_impl(codebook, layout = layout)

  if (isTRUE(assign_global)) {
    .qes_assign(paste0(code, "_codebook"), out, envir)
  }

  out
}

#' Alias for get_codebook
#'
#' Backward-compatible alias for `get_codebook()`.
#'
#' Soft-deprecated: use [qes_codebook()], which takes the same arguments.
#' `get_qes_codebook()` keeps working and will not be removed; it prints a
#' one-time notice (see [qesR-deprecated]).
#'
#' @inheritParams get_codebook
#'
#' @return A `qes_codebook` data frame.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   cb <- qes_codebook("qes2022")
#'   head(cb)
#' }
#' @export
get_qes_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  .qes_deprecate("get_qes_codebook")
  .qes_codebook_impl(
    srvy, file = file, assign_global = assign_global, quiet = quiet,
    refresh = refresh, layout = layout, envir = parent.frame()
  )
}

#' Get a Quebec Election Study Codebook
#'
#' Returns the codebook of a study: one row per variable, with its label,
#' question text and value labels, plus a manifest of the study's
#' documentation files. Results are cached per session.
#'
#' @inheritParams get_codebook
#'
#' @return A `qes_codebook` data frame, returned visibly.
#' @examples
#' \donttest{
#'   cb <- qes_codebook("qes2022")
#'   head(cb)
#' }
#' @export
qes_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  .qes_codebook_impl(
    srvy, file = file, assign_global = assign_global, quiet = quiet,
    refresh = refresh, layout = layout, envir = parent.frame()
  )
}

#' Download Codebook Files
#'
#' Downloads codebook/support files (PDFs, questionnaires, metadata files) into a local directory.
#'
#' @param srvy A qesR survey code.
#' @param dest_dir Directory where files should be downloaded.
#' @param file Optional file selector passed to `qes_codebook()`.
#' @param quiet If TRUE, suppress informational output.
#' @param refresh If TRUE, force a fresh codebook metadata download first.
#' @param overwrite If TRUE, overwrite existing files in `dest_dir`.
#'
#' @return A data frame containing source file metadata and local download paths.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   download_codebook("qes2022", dest_dir = tempdir())
#' }
#' @export
download_codebook <- function(
  srvy,
  dest_dir = tempdir(),
  file = NULL,
  quiet = FALSE,
  refresh = FALSE,
  overwrite = FALSE
) {
  .qes_deprecate("download_codebook")
  .download_codebook_impl(
    srvy, dest_dir = dest_dir, file = file, quiet = quiet,
    refresh = refresh, overwrite = overwrite
  )
}

.download_codebook_impl <- function(
  srvy,
  dest_dir = tempdir(),
  file = NULL,
  quiet = FALSE,
  refresh = FALSE,
  overwrite = FALSE
) {
  .assert_single_string(dest_dir, "dest_dir")
  if (!dir.exists(dest_dir)) {
    dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  }

  study <- .get_qes_study(srvy)
  code <- study$qes_survey_code
  codebook <- .qes_codebook_impl(code, file = file, quiet = quiet, refresh = refresh, layout = "compact")
  files <- .qes_codebook_attr_files(codebook)

  if (nrow(files) == 0L) {
    .qes_inform("no_codebook_files", class = "qesR_message_download", args = list(.qes_q(code)), data = list(study = code), quiet = quiet)
    files$local_path <- character(0)
    files$downloaded <- logical(0)
    return(files)
  }

  local_paths <- character(nrow(files))
  downloaded <- logical(nrow(files))

  for (i in seq_len(nrow(files))) {
    src_name <- basename(files$filename[i])
    out_path <- file.path(dest_dir, src_name)
    local_paths[i] <- out_path

    if (file.exists(out_path) && !isTRUE(overwrite)) {
      downloaded[i] <- FALSE
      next
    }

    .qes_fetch(files$download_url[i], out_path, quiet = quiet, what = src_name)
    downloaded[i] <- TRUE
  }

  files$local_path <- local_paths
  files$downloaded <- downloaded
  files
}

#' @export
print.qes_codebook <- function(x, ...) {
  survey_code <- attr(x, "survey_code", exact = TRUE)
  doi <- attr(x, "doi", exact = TRUE)
  selected <- attr(x, "selected_data_file", exact = TRUE)
  files <- attr(x, "codebook_files", exact = TRUE)

  cat("<qes_codebook>")
  if (!is.null(survey_code)) cat(" survey:", survey_code)
  cat("\n")

  if (!is.null(doi)) cat("DOI:", doi, "\n")
  if (!is.null(selected)) cat("Data file:", selected, "\n")
  cat("Variables:", nrow(x), "\n")
  cat("Codebook/support files:", if (is.null(files)) 0L else nrow(files), "\n")

  preview <- as.data.frame(x)
  if ("value_labels" %in% names(preview)) {
    preview$value_labels <- vapply(preview$value_labels, function(v) {
      if (length(v) == 0L) "" else paste0("[", length(v), " labels]")
    }, character(1))
  }

  print(utils::head(preview, 10), ...)
  invisible(x)
}
