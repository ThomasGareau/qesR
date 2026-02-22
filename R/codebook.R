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

#' Reformat a qesR codebook layout
#'
#' @param codebook A `qes_codebook` object.
#' @param layout One of `"compact"`, `"wide"`, or `"long"`.
#'
#' @return A reformatted codebook data frame.
#' @export
format_codebook <- function(codebook, layout = c("compact", "wide", "long")) {
  layout <- match.arg(layout)

  if (!is.data.frame(codebook)) {
    stop("`codebook` must be a data.frame returned by get_codebook().", call. = FALSE)
  }

  codebook <- .normalize_codebook_class(codebook)

  switch(
    layout,
    compact = .layout_codebook_compact(codebook),
    wide = .layout_codebook_wide(codebook),
    long = .layout_codebook_long(codebook)
  )
}

#' Get value labels from a codebook
#'
#' @param codebook A `qes_codebook` object.
#' @param variable Optional variable name. If `NULL`, returns all variables.
#' @param long If `TRUE`, return a long data frame with `variable`, `value`, and
#'   `value_label` columns.
#'
#' @return A named list or a data frame of value labels.
#' @export
get_value_labels <- function(codebook, variable = NULL, long = FALSE) {
  if (!is.data.frame(codebook)) {
    stop("`codebook` must be a data.frame returned by get_codebook().", call. = FALSE)
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

#' Get a Quebec election study codebook
#'
#' Downloads and returns the codebook (variable metadata + value-label metadata)
#' for a study. Results are cached per session.
#'
#' @param srvy A qesR survey code (see [get_qescodes()]).
#' @param file Optional regular expression to select a specific file when a
#'   Dataverse dataset has multiple files.
#' @param assign_global If `TRUE`, assign the codebook into the global
#'   environment as `<srvy>_codebook`.
#' @param quiet If `TRUE`, suppress informational output and download progress.
#' @param refresh If `TRUE`, force a fresh download instead of using session
#'   cache.
#' @param layout One of `"compact"`, `"wide"`, or `"long"`.
#'
#' @return A `qes_codebook` data frame.
#' @export
get_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  layout <- match.arg(layout)
  study <- .get_qes_study(srvy)
  key <- .codebook_cache_key(study$qes_survey_code, file = file)

  if (!isTRUE(refresh) && exists(key, envir = .qes_codebook_cache, inherits = FALSE)) {
    codebook <- get(key, envir = .qes_codebook_cache, inherits = FALSE)
  } else {
    payload <- .download_and_read_qes(study, file = file, quiet = quiet, read_data = FALSE)
    codebook <- payload$codebook
    codebook <- .normalize_codebook_class(codebook)
    attr(codebook, "cached_at") <- Sys.time()
    assign(key, codebook, envir = .qes_codebook_cache)
  }

  if (isTRUE(assign_global)) {
    assign(paste0(srvy, "_codebook"), codebook, envir = .GlobalEnv)
  }

  format_codebook(codebook, layout = layout)
}

#' Alias for get_codebook
#'
#' Backward-compatible alias for [get_codebook()].
#'
#' @inheritParams get_codebook
#' @return A `qes_codebook` data frame.
#' @export
get_qes_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  get_codebook(
    srvy = srvy,
    file = file,
    assign_global = assign_global,
    quiet = quiet,
    refresh = refresh,
    layout = layout
  )
}

#' Alias for get_codebook
#'
#' Convenience alias for [get_codebook()].
#'
#' @inheritParams get_codebook
#' @return A `qes_codebook` data frame.
#' @export
qes_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  get_codebook(
    srvy = srvy,
    file = file,
    assign_global = assign_global,
    quiet = quiet,
    refresh = refresh,
    layout = layout
  )
}

#' Get codebook files
#'
#' Returns codebook/support files (e.g., PDFs, questionnaires, metadata files)
#' associated with a study.
#'
#' @param srvy A qesR survey code. Required if `codebook` is `NULL`.
#' @param codebook A `qes_codebook` object. If supplied, `srvy` is optional.
#' @param file Optional file selector passed to [get_codebook()] when `codebook`
#'   is not provided.
#' @param quiet If `TRUE`, suppress informational output when downloading.
#' @param refresh If `TRUE`, force a fresh codebook download when `codebook` is
#'   not provided.
#'
#' @return A data frame of codebook/support files.
#' @export
get_codebook_files <- function(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE) {
  if (is.null(codebook)) {
    if (is.null(srvy)) {
      stop("Provide `srvy` or `codebook`.", call. = FALSE)
    }
    codebook <- get_codebook(srvy, file = file, quiet = quiet, refresh = refresh, layout = "compact")
  }

  files <- attr(codebook, "codebook_files", exact = TRUE)
  if (is.null(files)) {
    files <- data.frame(
      file_id = character(0),
      filename = character(0),
      extension = character(0),
      size = numeric(0),
      download_url = character(0),
      stringsAsFactors = FALSE
    )
  }

  files
}

#' Download codebook/support files to disk
#'
#' Downloads codebook/support files (for example, PDF codebooks or questionnaires)
#' into a local directory.
#'
#' @param srvy A qesR survey code.
#' @param dest_dir Directory where files should be downloaded.
#' @param file Optional file selector passed to [get_codebook()].
#' @param quiet If `TRUE`, suppress informational output and download progress.
#' @param refresh If `TRUE`, force a fresh codebook metadata download first.
#' @param overwrite If `TRUE`, overwrite existing files in `dest_dir`.
#'
#' @return A data frame with source filename and local download path.
#' @export
download_codebook <- function(
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
  codebook <- get_codebook(srvy, file = file, quiet = quiet, refresh = refresh, layout = "compact")
  files <- get_codebook_files(codebook = codebook)

  if (nrow(files) == 0L) {
    if (!quiet) {
      message(sprintf("No codebook/support files found for '%s'.", srvy))
    }
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

    .download_file_with_fallback(
      files$download_url[i],
      out_path,
      quiet = quiet,
      allow_insecure_retry = isTRUE(study$allow_insecure_retry)
    )
    downloaded[i] <- TRUE
  }

  files$local_path <- local_paths
  files$downloaded <- downloaded
  files
}

#' Alias for get_codebook_files
#'
#' Backward-compatible alias for [get_codebook_files()].
#'
#' @inheritParams get_codebook_files
#' @return A data frame of codebook/support files.
#' @export
get_qes_codebook_files <- function(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE) {
  get_codebook_files(
    srvy = srvy,
    codebook = codebook,
    file = file,
    quiet = quiet,
    refresh = refresh
  )
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
