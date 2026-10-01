# Download cache (design.md section 4.2, slice S2a).
#
# Modes, from option qesR.cache or the environment variable QESR_CACHE:
#   "session" (default)  file.path(tempdir(), "qesR"), gone when R exits;
#   "disk"               tools::R_user_dir("qesR", "cache"), created only on
#                        this opt-in, with a message naming it;
#   "none"               a new directory under tempdir() for every download.
# Option qesR.cache_dir (or QESR_CACHE_DIR) names an existing directory of the
# user's choice and implies "disk" (unless qesR.cache is set to "session" or
# "none"). qesR never uses that directory itself: it creates, marks and uses a
# "qesR" subdirectory inside it, so the user's own files are never touched.
#
# Every root qesR creates holds a ".qesR-cache" marker, and qesR deletes files
# only under a marked root. The layout is content-addressed, with no index:
#   <root>/v1/<host>/<file_id>-<md5>.<ext>
# so a re-pinned catalog (a new md5) can never be served stale bytes. A
# disk cache used by qesR 0.5.0 to 0.7.0 may also hold the qes2022 metadata
# those versions built at runtime, in <root>/v1/shards/ (<study>-<md5>-s<n>.
# <variables|values>.csv): qesR no longer reads or lists it, and
# qes_cache_clear() deletes it with the files of its study. Files
# are written to a ".part" file, md5-checked, then renamed (.qes_request()).
# A cached file is checked for size on every use and for md5 once per session.

.qes_cache_marker <- ".qesR-cache"
.qes_cache_layout <- "v1"

# Session state: md5 checks already done, the number of network downloads
# (for the disk-cache tip), and memos of parsed data and dataset metadata.
.qes_cache_state <- new.env(parent = emptyenv())
.qes_memo <- new.env(parent = emptyenv())
.qes_latest_memo <- new.env(parent = emptyenv())

# ---- settings --------------------------------------------------------------------

# An option, else an environment variable, else NULL.
.qes_setting <- function(option, envvar) {
  value <- getOption(option)
  if (is.null(value)) {
    value <- Sys.getenv(envvar, "")
    if (!nzchar(value)) {
      return(NULL)
    }
  }
  value
}

.qes_cache_dir_setting <- function() {
  dir <- .qes_setting("qesR.cache_dir", "QESR_CACHE_DIR")
  if (is.null(dir)) {
    return(NULL)
  }
  if (!is.character(dir) || length(dir) != 1L || is.na(dir) || !nzchar(dir)) {
    .qes_abort(
      "input_option_dir",
      class = "qesR_error_input",
      args = list("qesR.cache_dir"),
      data = list(arg = "qesR.cache_dir", value = dir)
    )
  }
  path.expand(dir)
}

.qes_cache_mode <- function() {
  mode <- .qes_setting("qesR.cache", "QESR_CACHE")
  if (is.null(mode)) {
    return(if (is.null(.qes_cache_dir_setting())) "session" else "disk")
  }
  choices <- c("session", "disk", "none")
  if (!is.character(mode) || length(mode) != 1L || !(mode %in% choices)) {
    .qes_abort(
      "input_option_choice",
      class = "qesR_error_input",
      args = list("qesR.cache", .qes_q(choices)),
      data = list(arg = "qesR.cache", value = mode)
    )
  }
  mode
}

# ---- roots ---------------------------------------------------------------------------

# The cache root of `mode`. Nothing is created here. For "none" there is no
# lasting root: NA.
.qes_cache_root <- function(mode = .qes_cache_mode()) {
  switch(
    mode,
    session = file.path(tempdir(), "qesR"),
    none = NA_character_,
    disk = {
      dir <- .qes_cache_dir_setting()
      if (is.null(dir)) {
        tools::R_user_dir("qesR", which = "cache")
      } else {
        if (!dir.exists(dir)) {
          .qes_abort(
            "cache_dir_missing",
            class = "qesR_error_cache",
            args = list(.qes_q(dir)),
            data = list(path = dir, reason = "missing")
          )
        }
        file.path(normalizePath(dir, winslash = "/"), "qesR")
      }
    }
  )
}

.qes_cache_is_marked <- function(root) {
  file.exists(file.path(root, .qes_cache_marker))
}

# Create (if needed) and mark a cache root. An existing directory is used only
# if it already carries the marker or is empty, so qesR never writes into a
# directory of the user's that happens to have the same name.
.qes_cache_prepare <- function(root, mode, quiet = FALSE) {
  if (dir.exists(root)) {
    if (.qes_cache_is_marked(root)) {
      return(invisible(root))
    }
    if (length(list.files(root, all.files = TRUE, no.. = TRUE)) > 0L &&
      !.qes_cache_hand_made(root)) {
      .qes_abort(
        "cache_not_ours",
        class = "qesR_error_cache",
        args = list(.qes_q(root)),
        data = list(path = root, reason = "unmarked")
      )
    }
  } else if (!dir.create(root, recursive = TRUE, showWarnings = FALSE)) {
    .qes_abort(
      "cache_write",
      class = "qesR_error_cache",
      args = list(.qes_q(root)),
      data = list(path = root, reason = "write")
    )
  }
  writeLines(
    c(
      "qesR download cache.",
      "qes_cache_clear() may delete any file below this directory."
    ),
    file.path(root, .qes_cache_marker)
  )
  if (identical(mode, "disk")) {
    .qes_inform(
      "cache_created",
      class = "qesR_message_cached",
      args = list(.qes_q(root)),
      data = list(path = root),
      quiet = quiet
    )
  }
  invisible(root)
}

# Is `root` (unmarked) a cache laid out by hand, as the refused-download
# message explains: nothing but a v1/<host>/ tree of <file_id>-<md5>.<ext>
# files (Finder's .DS_Store and "._" files aside), with no symbolic link?
.qes_cache_hand_made <- function(root) {
  top <- list.files(root, all.files = TRUE, no.. = TRUE)
  top <- top[!.qes_cache_os_litter(top)]
  if (!identical(top, .qes_cache_layout)) {
    return(FALSE)
  }
  base <- file.path(root, .qes_cache_layout)
  entries <- list.files(base, recursive = TRUE, all.files = TRUE, include.dirs = TRUE)
  if (any(nzchar(Sys.readlink(file.path(base, entries))))) {
    return(FALSE)
  }
  entries <- entries[!.qes_cache_os_litter(basename(entries))]
  depth <- lengths(regmatches(entries, gregexpr("/", entries)))
  is_dir <- dir.exists(file.path(base, entries))
  # "shards" is the metadata folder of qesR 0.5.0 to 0.7.0, not a host
  host_ok <- grepl("^[a-z0-9.-]+$", sub("/.*$", "", entries)) & sub("/.*$", "", entries) != "shards"
  ok <- host_ok & ifelse(
    is_dir,
    depth == 0L,
    depth == 1L & grepl("^[0-9]+-[0-9a-f]{32}\\.[a-z0-9]+$", basename(entries))
  )
  length(entries) > 0L && all(ok)
}

.qes_cache_os_litter <- function(names) {
  names == ".DS_Store" | startsWith(names, "._")
}

# ---- paths -----------------------------------------------------------------------------

.qes_cache_host <- function(server) {
  gsub("[^a-z0-9.-]", "_", .qes_url_host(server))
}

# Extension of a catalog file: that of its original name, else its format.
.qes_cache_ext <- function(file_row) {
  ext <- tolower(tools::file_ext(file_row$original_file_name %||% ""))
  if (length(ext) != 1L || is.na(ext) || !nzchar(ext)) {
    ext <- tolower(file_row$format)
  }
  ext
}

.qes_cache_path <- function(root, server, file_id, md5, ext) {
  ok <- grepl("^[0-9]+$", file_id) && grepl("^[0-9a-f]{32}$", md5) && grepl("^[a-z0-9]+$", ext)
  if (!isTRUE(ok)) {
    stop("qesR internal error: invalid file id, md5 or extension for the cache.", call. = FALSE)
  }
  file.path(root, .qes_cache_layout, .qes_cache_host(server), sprintf("%s-%s.%s", file_id, md5, ext))
}

# `path` relative to the folder that contains the cache root, e.g.
# "qesR/v1/borealisdata.ca/102-<md5>.pdf": where a file goes in a directory
# named by qesR.cache_dir.
.qes_cache_relative <- function(path, root) {
  rel <- substring(path, nchar(root) + 2L)
  paste(basename(root), rel, sep = "/")
}

# ---- fetching a catalog file ------------------------------------------------------------

.qes_md5 <- function(path) {
  unname(tools::md5sum(path))
}

# Is the cached copy at `path` the pinned file? Size on every use; md5 once per
# session per path.
.qes_cache_valid <- function(path, file_row) {
  bytes <- suppressWarnings(as.numeric(file_row$bytes))
  if (length(bytes) == 1L && !is.na(bytes) && !identical(as.numeric(file.size(path)), bytes)) {
    return(FALSE)
  }
  key <- paste0("verified:", path)
  if (identical(.qes_cache_state[[key]], file_row$md5)) {
    return(TRUE)
  }
  ok <- identical(.qes_md5(path), file_row$md5)
  if (ok) {
    .qes_cache_state[[key]] <- file_row$md5
  }
  ok
}

.qes_interactive <- function() {
  interactive()
}

# After the second network download of a session, in session mode and in an
# interactive session, suggest the disk cache once.
.qes_cache_count_download <- function(mode, quiet) {
  n <- (.qes_cache_state$downloads %||% 0L) + 1L
  .qes_cache_state$downloads <- n
  if (n >= 2L && identical(mode, "session") && .qes_interactive() &&
    !isTRUE(quiet) && .qes_once_first("disk_cache_tip")) {
    .qes_inform("disk_cache_tip", class = "qesR_message_disk_cache_tip", quiet = quiet)
  }
  invisible(n)
}

# The local path of a pinned catalog file (a row of files.csv, served by
# `server`), downloading it into the cache when needed. The file is checked
# against the catalog md5 before it gets its final name; a mismatch is a
# qesR_error_checksum and nothing is kept. In mode "none" the file is put in a
# new directory under tempdir() and the path has attribute transient = TRUE:
# the caller deletes it once read (attribute transient_root: the directory to
# delete). .qes_cache_via(path) then says where the bytes came from.
.qes_cache_fetch <- function(file_row, server, quiet = FALSE) {
  mode <- .qes_cache_mode()
  root <- .qes_cache_root(mode)
  transient <- is.na(root)
  if (transient) {
    root <- tempfile("qesR-nocache-")
  }
  .qes_cache_prepare(root, mode, quiet = quiet)
  ext <- .qes_cache_ext(file_row)
  path <- .qes_cache_path(root, server, file_row$file_id, file_row$md5, ext)

  rejected <- NULL
  if (!transient && file.exists(path)) {
    if (.qes_cache_valid(path, file_row)) {
      .qes_inform(
        "cached_file",
        class = "qesR_message_cached",
        args = list(.qes_q(file_row$original_file_name %||% basename(path))),
        data = list(study = file_row$study, file_id = file_row$file_id, path = path),
        quiet = quiet
      )
      .qes_cache_state[[paste0("via:", path)]] <- paste0(mode, "_cache")
      return(path)
    }
    # a damaged copy, or a file saved here by hand that is not the pinned one
    # (typically the ingested .tab a browser gives by default): say so, then
    # discard it and download the file again
    rejected <- list(path = path, actual = .qes_md5(path))
    .qes_inform(
      "cache_rejected",
      class = "qesR_message_cached",
      args = list(.qes_q(path), file_row$md5, rejected$actual),
      data = list(study = file_row$study, file_id = file_row$file_id, path = path,
        expected = file_row$md5, actual = rejected$actual),
      quiet = quiet
    )
    unlink(path)
  }

  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  # any ingested file (data or label donor) is requested as its original: the
  # catalog md5 is that of the original, never of the ingested .tab
  kind <- if (isTRUE(as.logical(file_row$ingested))) "original" else "file"
  url <- .qes_url(server, kind, file_id = file_row$file_id)
  verify <- function(part) {
    actual <- .qes_md5(part)
    if (!identical(actual, file_row$md5)) {
      .qes_abort(
        "checksum",
        class = "qesR_error_checksum",
        args = list(.qes_q(file_row$study), .qes_q(file_row$file_id), file_row$md5, actual),
        data = list(study = file_row$study, file_id = file_row$file_id, expected = file_row$md5, actual = actual)
      )
    }
  }
  manual_path <- if (transient) NULL else structure(path, relative = .qes_cache_relative(path, root))
  tryCatch(
    .qes_fetch(
      url, path,
      quiet = quiet,
      what = file_row$original_file_name %||% basename(path),
      verify = verify,
      manual_path = manual_path
    ),
    qesR_error_http_refused = function(e) {
      if (is.null(rejected)) {
        stop(e)
      }
      # the server refuses and the copy found in the cache was not the pinned
      # file: report that copy, not only the refusal
      .qes_abort(
        "checksum_cached",
        class = "qesR_error_checksum",
        args = list(.qes_q(rejected$path), .qes_q(file_row$study), .qes_q(file_row$file_id),
          file_row$md5, rejected$actual),
        data = list(study = file_row$study, file_id = file_row$file_id, expected = file_row$md5,
          actual = rejected$actual, path = rejected$path, manual_path = path),
        parent = e
      )
    }
  )
  .qes_cache_state[[paste0("verified:", path)]] <- file_row$md5
  .qes_cache_count_download(mode, quiet)
  if (transient) {
    attr(path, "transient") <- TRUE
    attr(path, "transient_root") <- root
  }
  .qes_cache_state[[paste0("via:", path)]] <- "network"
  path
}

# Where the last .qes_cache_fetch() of `path` got its bytes: "network",
# "session_cache" or "disk_cache" (for the provenance of get_qes() data).
.qes_cache_via <- function(path) {
  .qes_cache_state[[paste0("via:", as.character(path))]] %||% NA_character_
}

# ---- in-session memo --------------------------------------------------------------------

# Parsed data kept in memory for the session, keyed by the md5 of its source
# file (option qesR.memo, default TRUE). Never written to disk. A key may join
# several parts with "+": "<md5>+<donor md5>" for data read with a label
# donor. Clearing by md5 drops every entry that has that md5 as one of its
# parts. (Metadata built in the session from data has its own memo in
# R/metadata.R; qes_cache_clear() empties both.)
.qes_memo_get <- function(md5) {
  if (!isTRUE(getOption("qesR.memo", TRUE))) {
    return(NULL)
  }
  .qes_memo[[md5]]$value
}

.qes_memo_set <- function(md5, value, study = NA_character_) {
  if (isTRUE(getOption("qesR.memo", TRUE))) {
    .qes_memo[[md5]] <- list(value = value, study = study)
  }
  invisible(value)
}

.qes_memo_clear <- function(studies = NULL, md5 = NULL) {
  keys <- ls(.qes_memo, all.names = TRUE)
  if (!is.null(studies) || !is.null(md5)) {
    keep <- vapply(keys, function(k) {
      !any(strsplit(k, "+", fixed = TRUE)[[1]] %in% md5) && !(.qes_memo[[k]]$study %in% studies)
    }, logical(1))
    keys <- keys[!keep]
  }
  rm(list = keys, envir = .qes_memo)
  invisible(keys)
}

# ---- listing ---------------------------------------------------------------------------

.qes_cache_empty_info <- function() {
  data.frame(
    study = character(0), file_id = character(0), md5 = character(0),
    bytes = numeric(0), retrieved = as.POSIXct(character(0)),
    kind = character(0), path = character(0),
    stringsAsFactors = FALSE
  )
}

# The files under a cache root, as the qes_cache_info() table.
.qes_cache_list <- function(root) {
  out <- .qes_cache_empty_info()
  if (is.na(root)) {
    return(out)
  }
  base <- file.path(root, .qes_cache_layout)
  if (!dir.exists(base)) {
    return(out)
  }
  paths <- .qes_cache_walk(root)
  if (length(paths) == 0L) {
    return(out)
  }
  name <- basename(paths)
  parent <- basename(dirname(paths))
  file_re <- "^([0-9]+)-([0-9a-f]{32})\\.([a-z0-9]+)$"
  keep <- parent != "shards" & grepl(file_re, name)
  if (!any(keep)) {
    return(out)
  }
  paths <- paths[keep]
  name <- name[keep]

  file_id <- sub(file_re, "\\1", name)
  md5 <- sub(file_re, "\\2", name)

  files <- .qes_catalog(demo = TRUE)$files
  hit <- match(paste(file_id, md5), paste(files$file_id, files$md5))
  hit_id <- match(file_id, files$file_id)
  hit[is.na(hit)] <- hit_id[is.na(hit)]

  info <- file.info(paths)
  out <- data.frame(
    study = files$study[hit],
    file_id = file_id,
    md5 = md5,
    bytes = as.numeric(info$size),
    retrieved = info$mtime,
    kind = rep("file", length(paths)),
    path = paths,
    stringsAsFactors = FALSE
  )
  out <- out[order(out$study, out$kind, out$file_id, out$path, na.last = TRUE), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The metadata files that qesR 0.5.0 to 0.7.0 built for qes2022 and kept in
# <root>/v1/shards/ (no longer read or listed), with their study and time,
# for qes_cache_clear().
.qes_cache_legacy_shards <- function(root) {
  paths <- .qes_cache_walk(root)
  re <- "^(.+)-[0-9a-f]{32}-s[0-9]+\\.(variables|values)\\.csv$"
  paths <- paths[basename(dirname(paths)) == "shards" & grepl(re, basename(paths))]
  data.frame(study = sub(re, "\\1", basename(paths)), retrieved = file.mtime(paths),
             path = paths, stringsAsFactors = FALSE)
}

# ---- qes_cache_info() ----------------------------------------------------------------

#' List the files in the download cache
#'
#' `qes_cache_info()` lists the data files and documents that qesR has kept
#' in its download cache, with the study each belongs to. It
#' only reads the cache directory: it makes no network request and creates
#' nothing.
#'
#' @section Where files are kept:
#' qesR keeps each downloaded file under the name
#' `<file_id>-<md5>.<ext>`, taken from its catalog entry, after checking it
#' against the catalog md5. Where depends on the option `qesR.cache` (or the
#' environment variable `QESR_CACHE`):
#' \describe{
#'   \item{`"session"` (default)}{a `qesR` folder in [tempdir()], deleted when
#'     R exits. Nothing is written outside the session's temporary directory.}
#'   \item{`"disk"`}{`tools::R_user_dir("qesR", "cache")`, kept between
#'     sessions. qesR creates it only when you choose this mode, and says so.}
#'   \item{`"none"`}{nothing is kept: each download goes to a new temporary
#'     folder.}
#' }
#' Setting `qesR.cache_dir` (or `QESR_CACHE_DIR`) to an existing directory
#' keeps files there instead, and implies `"disk"` unless `qesR.cache` says
#' otherwise. qesR then works in a `qesR` subfolder that it creates and marks,
#' and never touches the other files of that directory.
#'
#' A file you download yourself in a browser (for example when a server
#' refuses automated requests; see [qesR-package]) can be put in the cache
#' under the name the error message gives: qesR uses it once its md5 matches
#' the catalog.
#'
#' @return A data frame with one row per cached file: `study`, `file_id`,
#'   `md5`, `bytes`, `retrieved` (modification time), `kind` (always
#'   `"file"`) and `path`.
#'   Attributes `mode` (the cache mode)
#'   and `dir` (the cache directory, `NA` in mode `"none"`). The data frame
#'   has class `c("qes_cache_info", "data.frame")` and prints compactly: the
#'   mode, directory and total size once, then each file with its size and
#'   its path relative to the cache directory; the `path` column itself is
#'   absolute, and `as.data.frame()` gives every column. A subset of its
#'   columns is a plain data frame.
#'
#'   *En français* : une ligne par fichier en cache, de classe
#'   `c("qes_cache_info", "data.frame")`. L'affichage donne une fois le mode,
#'   le dossier et la taille totale, puis chaque fichier avec sa taille et
#'   son chemin relatif au dossier du cache ; la colonne `path` reste
#'   absolue, et `as.data.frame()` donne toutes les colonnes.
#'
#' @family cache
#' @seealso [qes_cache_clear()] to delete cached files.
#' @examples
#' info <- qes_cache_info()
#' attr(info, "mode")
#' attr(info, "dir")
#' @export
qes_cache_info <- function() {
  mode <- .qes_cache_mode()
  root <- .qes_cache_root(mode)
  out <- if (is.na(root)) .qes_cache_empty_info() else .qes_cache_list(root)
  attr(out, "mode") <- mode
  attr(out, "dir") <- root
  class(out) <- c("qes_cache_info", "data.frame")
  out
}

# Subsetting keeps the class, and the attributes mode and dir, only when
# every column is kept (a row subset); a column subset is a plain data frame.
#' @export
`[.qes_cache_info` <- function(x, ...) {
  out <- NextMethod()
  if (!is.data.frame(out)) {
    return(out)
  }
  if (identical(names(out), names(x))) {
    attr(out, "mode") <- attr(x, "mode", exact = TRUE)
    attr(out, "dir") <- attr(x, "dir", exact = TRUE)
  } else {
    class(out) <- setdiff(class(out), "qes_cache_info")
  }
  out
}

# Bytes as a short size ("12.3 MB"), base 1024 as file managers show.
.qes_format_bytes <- function(bytes) {
  bytes <- as.numeric(bytes)
  units <- c("B", "KB", "MB", "GB", "TB")
  out <- rep(NA_character_, length(bytes))
  ok <- !is.na(bytes)
  k <- pmin(pmax(floor(log(pmax(bytes[ok], 1), 1024)), 0), length(units) - 1L)
  v <- bytes[ok] / 1024^k
  out[ok] <- ifelse(k == 0, sprintf("%.0f %s", v, units[k + 1L]), sprintf("%.1f %s", v, units[k + 1L]))
  out
}

#' @export
print.qes_cache_info <- function(x, ...) {
  needed <- c("study", "kind", "file_id", "bytes", "retrieved", "path")
  if (!all(needed %in% names(x))) {
    print(as.data.frame(x), ...)
    return(invisible(x))
  }
  lang <- .qes_lang()
  mode <- attr(x, "mode", exact = TRUE) %||% NA_character_
  dir <- attr(x, "dir", exact = TRUE) %||% NA_character_
  total <- sum(as.numeric(x$bytes), na.rm = TRUE)
  cat(.qes_msg("cache_print_header",
               list(mode, if (is.na(dir)) "-" else dir, nrow(x), .qes_format_bytes(total)),
               lang = lang), "\n", sep = "")
  if (nrow(x) > 0L) {
    # paths relative to the cache directory (the column keeps them absolute)
    rel <- x$path
    if (!is.na(dir)) {
      prefix <- paste0(sub("[/\\\\]+$", "", dir), "/")
      at <- !is.na(rel) & startsWith(rel, prefix)
      rel[at] <- substring(rel[at], nchar(prefix) + 1L)
    }
    view <- data.frame(
      study = x$study,
      kind = x$kind,
      file_id = x$file_id,
      size = .qes_format_bytes(x$bytes),
      retrieved = format(x$retrieved, "%Y-%m-%d %H:%M"),
      file = rel,
      stringsAsFactors = FALSE
    )
    print(view, ...)
  }
  cat(.qes_msg("cache_print_footer", lang = lang), "\n", sep = "")
  invisible(x)
}

# ---- qes_cache_clear() ---------------------------------------------------------------

#' Delete files from the download cache
#'
#' `qes_cache_clear()` deletes cached files: all of them, those of some
#' studies, or those older than a given age. It also forgets the data and
#' metadata kept in memory for this session. It deletes files only in a
#' directory that qesR created and marked as its cache (it holds a
#' `.qesR-cache` file) and refuses any other directory.
#'
#' @param studies Optional character vector of study codes (see
#'   [qes_studies()]). `NULL` (default) clears every study.
#' @param older_than Optional age: a number of days, or a [difftime]. Only
#'   files retrieved longer ago than this are deleted.
#'
#' @return The paths of the deleted files, invisibly.
#'
#' @family cache
#' @seealso [qes_cache_info()], which also describes where files are kept.
#' @examples
#' # these examples work on the session cache only, so that running them
#' # never deletes a disk cache you have opted in to
#' op <- options(qesR.cache = "session")
#'
#' # delete files kept more than 30 days
#' qes_cache_clear(older_than = 30)
#'
#' # delete everything in the cache
#' qes_cache_clear()
#'
#' options(op)
#' @export
qes_cache_clear <- function(studies = NULL, older_than = NULL) {
  codes <- if (is.null(studies)) NULL else .qes_resolve_codes(studies, "studies", demo = TRUE)
  cutoff <- .qes_cache_cutoff(older_than)
  mode <- .qes_cache_mode()
  root <- .qes_cache_root(mode)
  removed <- character(0)
  removed_studies <- character(0)

  if (!is.na(root) && dir.exists(root)) {
    if (!.qes_cache_is_marked(root)) {
      .qes_abort(
        "cache_unmarked",
        class = "qesR_error_cache",
        args = list(.qes_q(root)),
        data = list(path = root, reason = "unmarked")
      )
    }
    listed <- .qes_cache_list(root)
    # the metadata of qes2022 that qesR 0.5.0 to 0.7.0 kept in the cache
    old <- .qes_cache_legacy_shards(root)
    listed <- rbind(listed, data.frame(
      study = old$study, file_id = rep(NA_character_, nrow(old)), md5 = rep(NA_character_, nrow(old)),
      bytes = rep(NA_real_, nrow(old)), retrieved = old$retrieved, kind = rep("shard", nrow(old)),
      path = old$path, stringsAsFactors = FALSE
    ))
    sel <- rep(TRUE, nrow(listed))
    if (!is.null(codes)) {
      sel <- sel & listed$study %in% codes
    }
    if (!is.null(cutoff)) {
      sel <- sel & listed$retrieved < cutoff
    }
    targets <- listed$path[sel]
    if (!is.null(cutoff) && is.null(codes)) {
      # also ".part" files left by a download interrupted long ago
      walked <- .qes_cache_walk(root)
      parts <- walked[grepl("\\.part$", walked)]
      targets <- c(targets, parts[file.mtime(parts) < cutoff])
    }
    if (is.null(codes) && is.null(cutoff)) {
      # everything under v1/, including interrupted ".part" downloads
      targets <- .qes_cache_walk(root)
    }
    unlink(targets[.qes_cache_inside(targets, root)])
    removed <- targets[!file.exists(targets)]
    removed_studies <- unique(stats::na.omit(listed$study[listed$path %in% removed]))
    .qes_cache_prune(file.path(root, .qes_cache_layout), root)
    .qes_cache_forget(removed)
  }

  if (is.null(codes) && is.null(cutoff)) {
    .qes_memo_clear()
    rm(list = ls(.qes_latest_memo, all.names = TRUE), envir = .qes_latest_memo)
    .qes_dict_forget()
  } else {
    removed_md5 <- sub("^[0-9]+-([0-9a-f]{32})\\..*$", "\\1", basename(removed))
    .qes_memo_clear(studies = codes, md5 = removed_md5)
    # the metadata held in memory for every study that lost a file, not only
    # the studies named: what is still on disk is read again when it is
    # needed
    forget <- unique(c(codes, removed_studies))
    if (length(forget) > 0L) {
      .qes_dict_forget(forget)
    }
  }
  invisible(removed)
}

# `older_than` as a cut-off time (NULL for none).
.qes_cache_cutoff <- function(older_than) {
  if (is.null(older_than)) {
    return(NULL)
  }
  seconds <- if (inherits(older_than, "difftime")) {
    as.numeric(older_than, units = "secs")
  } else if (is.numeric(older_than)) {
    as.numeric(older_than) * 86400
  } else {
    NA_real_
  }
  if (length(seconds) != 1L || is.na(seconds) || seconds < 0) {
    .qes_abort(
      "input_older_than",
      class = "qesR_error_input",
      data = list(arg = "older_than", value = older_than)
    )
  }
  .qes_now() - seconds
}

# Remove empty directories below `base` (deepest first), keeping `base`; never
# a symbolic link or a directory outside `root`.
.qes_cache_prune <- function(base, root) {
  if (!dir.exists(base)) {
    return(invisible())
  }
  dirs <- list.dirs(base, recursive = TRUE, full.names = TRUE)
  dirs <- setdiff(dirs, base)
  dirs <- dirs[!nzchar(Sys.readlink(dirs)) & .qes_cache_inside(dirs, root)]
  dirs <- dirs[order(nchar(dirs), decreasing = TRUE)]
  for (d in dirs) {
    if (length(list.files(d, all.files = TRUE, no.. = TRUE)) == 0L) {
      unlink(d, recursive = TRUE)
    }
  }
  invisible()
}

# The files below <root>/v1, without following symbolic links: the contents of
# a linked directory, or the target of a linked file, may be the user's.
.qes_cache_walk <- function(root) {
  base <- file.path(root, .qes_cache_layout)
  if (!dir.exists(base) || nzchar(Sys.readlink(base))) {
    return(character(0))
  }
  out <- character(0)
  todo <- base
  while (length(todo) > 0L) {
    d <- todo[[1L]]
    todo <- todo[-1L]
    entries <- list.files(d, all.files = TRUE, no.. = TRUE, full.names = TRUE)
    linked <- nzchar(Sys.readlink(entries))
    entries <- entries[!linked]
    is_dir <- dir.exists(entries)
    todo <- c(todo, entries[is_dir])
    out <- c(out, entries[!is_dir])
  }
  out[.qes_cache_inside(out, root)]
}

# Do `paths`, once resolved, stay inside the resolved `root`?
.qes_cache_inside <- function(paths, root) {
  if (length(paths) == 0L) {
    return(logical(0))
  }
  top <- paste0(normalizePath(root, winslash = "/", mustWork = FALSE), "/")
  startsWith(normalizePath(paths, winslash = "/", mustWork = FALSE), top)
}

# Forget the session's md5 checks of deleted files.
.qes_cache_forget <- function(paths) {
  keys <- intersect(paste0("verified:", paths), ls(.qes_cache_state, all.names = TRUE))
  rm(list = keys, envir = .qes_cache_state)
  invisible()
}
