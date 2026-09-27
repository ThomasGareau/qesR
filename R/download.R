# qes_download() (design.md sections 2.2, 4.2 and 4.3, slice S2c).
#
# qes_download() copies the original bytes of catalog files (data files and
# documents) into a directory the user names. The rules:
#   - `path` must already exist; qes_download() never creates it, and
#     writes nothing at all unless at least one file matches the request;
#   - before anything is written, every destination is checked: a file that
#     is already there with the expected md5 is kept (not downloaded again);
#     one that differs is an error unless overwrite = TRUE, and then nothing
#     is written;
#   - each file comes through the download cache (R/cache.R), which checks
#     it against its md5 before keeping it; it is then copied into `path` as
#     a ".part" file, checked against the md5 again, and only then renamed to
#     its final name, so a damaged or partial copy never gets that name;
#   - version = "pinned" (default) fetches the files pinned by the catalog;
#     version = "latest" is the only unpinned route: it asks Dataverse for the
#     latest published version of each deposit, checks each file against the
#     md5 Dataverse gives, raises qesR_warning_unpinned and records
#     pinned = FALSE.

.qes_data_roles <- c("data", "label_donor")

# Roles qes_download() may select for `what`.
.qes_download_roles <- function(what) {
  c(if ("data" %in% what) .qes_data_roles, if ("docs" %in% what) .qes_doc_roles)
}

# The catalog rows (files.csv) that qes_download() fetches, in the order of
# `codes`, then catalog order. With `role = NULL`, "data" means each study's
# pinned data file (the one get_qes() reads) and "docs" every document.
# Naming roles selects every file of those roles: role "data" also brings the
# other-format twins of a data file, "label_donor" the label donors. `lang`
# filters documents only (data files have no language).
.qes_download_select <- function(codes, what, role = NULL, lang = NULL) {
  files <- .qes_catalog(demo = TRUE)$files
  files <- files[files$study %in% codes, , drop = FALSE]
  keep <- rep(FALSE, nrow(files))
  if ("data" %in% what) {
    keep <- keep | if (is.null(role)) {
      files$role == "data" & files$is_default %in% TRUE
    } else {
      files$role %in% intersect(role, .qes_data_roles)
    }
  }
  if ("docs" %in% what) {
    doc <- files$role %in% (if (is.null(role)) .qes_doc_roles else intersect(role, .qes_doc_roles))
    if (!is.null(lang)) {
      doc <- doc & files$lang %in% lang
    }
    keep <- keep | doc
  }
  out <- files[keep, , drop = FALSE]
  out <- out[order(match(out$study, codes)), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# The deposited name of a catalog file: the original upload's name for an
# ingested (tabular) file, else the file name.
.qes_deposit_name <- function(file_row) {
  ifelse(as.logical(file_row$ingested) %in% TRUE, file_row$original_file_name, file_row$file_name)
}

# TRUE when `x` can be written in the native encoding, so that it can be
# used as a file name. Always TRUE in a UTF-8 locale; in another locale (the
# C locale, say) a name with characters it lacks cannot be used by file.*().
# Only a check: the name is never transliterated.
.qes_native_name_ok <- function(x) {
  if (isTRUE(l10n_info()[["UTF-8"]])) {
    return(TRUE)
  }
  !is.na(iconv(enc2utf8(x), "UTF-8", "", sub = NA))
}

# The name a file gets in `path`: its deposited name, made safe as a file
# name, with the catalog format as extension when the deposited name lacks it
# (the qes2018 methodological report is deposited with no extension). A name
# the native encoding cannot hold (a French accented name in the C locale)
# becomes the file id, with the deposited name's extension when it has a
# plain one.
.qes_download_name <- function(file_row) {
  name <- .qes_deposit_name(file_row)
  if (length(name) != 1L || is.na(name)) {
    name <- ""
  }
  name <- gsub("[/\\\\:*?\"<>|[:cntrl:]]", "_", name)
  name <- trimws(name)
  if (nzchar(name) && !.qes_native_name_ok(name)) {
    ext <- tools::file_ext(name)
    name <- as.character(file_row$file_id)
    if (grepl("^[A-Za-z0-9]+$", ext)) {
      name <- paste0(name, ".", ext)
    }
  }
  if (!nzchar(name) || name %in% c(".", "..")) {
    name <- as.character(file_row$file_id)
  }
  fmt <- tolower(file_row$format %||% "")
  if (length(fmt) == 1L && !is.na(fmt) && grepl("^[a-z0-9]+$", fmt) &&
    !identical(tolower(tools::file_ext(name)), fmt)) {
    name <- paste0(name, ".", fmt)
  }
  name
}

# ---- argument checks -------------------------------------------------------------

.qes_check_flag <- function(x, arg) {
  if (!is.logical(x) || length(x) != 1L || is.na(x)) {
    .qes_abort(
      "input_flag",
      class = "qesR_error_input",
      args = list(arg),
      data = list(arg = arg, value = x)
    )
  }
  invisible(x)
}

# One value of `choices`, the match.arg() way: the whole `choices` vector
# (the default, left as it is) means its first value, and an unambiguous
# abbreviation means the value it starts.
.qes_check_one <- function(x, arg, choices) {
  if (identical(x, choices)) {
    return(choices[1L])
  }
  if (is.character(x) && length(x) == 1L && !is.na(x) && nzchar(x)) {
    hit <- pmatch(x, choices)
    if (!is.na(hit)) {
      x <- choices[hit]
    }
  }
  if (!is.character(x) || length(x) != 1L || is.na(x) || !(x %in% choices)) {
    .qes_abort(
      "input_choice_one",
      class = "qesR_error_input",
      args = list(arg, .qes_q(choices)),
      data = list(arg = arg, value = x)
    )
  }
  x
}

# `path` must be an existing directory: qes_download() never creates one.
.qes_check_download_path <- function(path) {
  ok <- is.character(path) && length(path) == 1L && !is.na(path) && nzchar(path) &&
    dir.exists(path)
  if (!ok) {
    .qes_abort(
      "input_path_dir",
      class = "qesR_error_input",
      data = list(arg = "path", value = path)
    )
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

# ---- version = "latest" ----------------------------------------------------------

# For each selected catalog row, the matching file of the latest published
# version of its deposit, as a files.csv-shaped row: the same file id when
# it is still there, else the file that replaced it (Dataverse's
# previousDataFileId / rootDataFileId), else the only file with the same
# deposited name. Every deposit is asked once (the answer is kept for the
# session, like qes_studies(check_updates = TRUE)). Anything that prevents
# an md5-checked download is an error, before any file is written.
.qes_download_latest <- function(rows, quiet = FALSE) {
  catalog <- .qes_catalog(demo = TRUE)
  studies <- catalog$studies
  asked <- character(0)
  out <- rows
  for (i in seq_len(nrow(rows))) {
    row <- rows[i, , drop = FALSE]
    st <- studies[match(row$study, studies$study), , drop = FALSE]
    if (isTRUE(st$demo)) {
      .qes_abort(
        "download_latest_demo",
        class = "qesR_error_input",
        args = list(.qes_q(row$study)),
        data = list(arg = "version", value = "latest")
      )
    }
    key <- paste(st$server, st$doi)
    if (!(key %in% asked)) {
      .qes_inform(
        "check_updates",
        class = "qesR_message_download",
        args = list(st$doi),
        data = list(study = st$study),
        quiet = quiet
      )
      asked <- c(asked, key)
    }
    res <- .qes_fetch_latest(st$server, st$doi)
    if (inherits(res, "condition")) {
      .qes_abort(
        "latest_unreadable",
        class = "qesR_error_source",
        args = list(.qes_q(row$study), st$doi),
        data = list(study = row$study, file_id = row$file_id),
        parent = res
      )
    }
    if (identical(res$state, "DEACCESSIONED")) {
      .qes_abort(
        "latest_deaccessioned",
        class = "qesR_error_source",
        args = list(.qes_q(row$study), st$doi, res$version),
        data = list(study = row$study, file_id = row$file_id)
      )
    }
    files <- res$files
    if (is.null(files)) {
      files <- .qes_latest_files(list())
    }
    files <- files[!files$restricted, , drop = FALSE]
    hit <- which(files$id == row$file_id)
    if (length(hit) == 0L) {
      hit <- which(files$previous %in% row$file_id | files$root %in% row$file_id)
    }
    if (length(hit) == 0L) {
      hit <- which(files$name == .qes_deposit_name(row))
    }
    if (length(hit) != 1L || is.na(files$md5[hit])) {
      .qes_abort(
        "latest_missing",
        class = "qesR_error_source",
        args = list(.qes_q(row$study), .qes_q(row$file_id), res$version),
        data = list(study = row$study, file_id = row$file_id)
      )
    }
    f <- files[hit, , drop = FALSE]
    same <- identical(f$id, row$file_id) && identical(f$md5, row$md5)
    out$file_id[i] <- f$id
    out$md5[i] <- f$md5
    out$bytes[i] <- f$bytes
    out$ingested[i] <- f$ingested
    if (!is.na(f$name)) {
      out$original_file_name[i] <- f$name
      if (!isTRUE(f$ingested)) {
        out$file_name[i] <- f$name
      }
      # the replacing file may be of another format (.sav replaced by .dta,
      # .doc by .pdf): its name gives the format, except Dataverse's archival
      # ".tab" name for an ingested file whose original name is unknown
      ext <- tolower(tools::file_ext(f$name))
      if (grepl("^[a-z0-9]+$", ext) && !(isTRUE(f$ingested) && identical(ext, "tab"))) {
        out$format[i] <- ext
      }
    }
    out$dataset_version[i] <- res$version
    if (!same) {
      out$unf[i] <- NA_character_
      out$n_rows[i] <- NA_integer_
      out$n_cols[i] <- NA_integer_
    }
  }
  out
}

# ---- writing ---------------------------------------------------------------------

# Copy the md5-checked local file `src` into `dest`: first as a ".part" file
# next to it, checked against `file_row$md5`, then renamed. The part file
# never survives: it is renamed, or deleted when anything fails.
.qes_download_place <- function(src, dest, file_row, dir = dirname(dest)) {
  part <- tempfile(pattern = "qesR-", tmpdir = dir, fileext = ".part")
  on.exit(unlink(part), add = TRUE)
  copied <- suppressWarnings(file.copy(src, part, overwrite = FALSE))
  if (!isTRUE(copied)) {
    .qes_abort(
      "download_write",
      class = "qesR_error_input",
      args = list(.qes_q(dir)),
      data = list(arg = "path", value = dir)
    )
  }
  verify <- function(p) {
    actual <- .qes_md5(p)
    if (!identical(actual, file_row$md5)) {
      .qes_abort(
        "checksum",
        class = "qesR_error_checksum",
        args = list(.qes_q(file_row$study), .qes_q(file_row$file_id), file_row$md5, actual),
        data = list(study = file_row$study, file_id = file_row$file_id,
          expected = file_row$md5, actual = actual)
      )
    }
  }
  # a failure to rename or copy into `path` is about the user's directory,
  # not the download cache
  tryCatch(
    .qes_finish_part(part, dest, verify = verify),
    qesR_error_cache = function(e) {
      .qes_abort(
        "download_write",
        class = "qesR_error_input",
        args = list(.qes_q(dir)),
        data = list(arg = "path", value = dir),
        parent = e
      )
    }
  )
  invisible(dest)
}

# One md5-checked local copy of a catalog file (shipped demo file, or the
# download cache), placed at `dest`. Returns where the bytes came from.
.qes_download_one <- function(study_row, file_row, dest, dir, quiet = FALSE) {
  src <- .qes_local_file(study_row, file_row, quiet = quiet)
  on.exit(.qes_release_local_file(src), add = TRUE)
  .qes_download_place(src, dest, file_row, dir = dir)
  attr(src, "retrieved_via", exact = TRUE) %||% NA_character_
}

# Fetch the catalog rows `rows` into the existing directory `path`.
#   legacy = FALSE (qes_download): a file already at its destination is kept
#     when its md5 is the expected one; one that differs is an error unless
#     `overwrite`, checked for every file before anything is written;
#   legacy = TRUE (download_codebook, as in qesR 0.4.4): a file already there
#     is kept as it is unless `overwrite`, which replaces it.
# Returns the qes_download() table, with attribute qes_provenance.
.qes_download_files <- function(rows, path, overwrite = FALSE, quiet = FALSE,
                                pinned = TRUE, legacy = FALSE) {
  n <- nrow(rows)
  studies <- .qes_catalog(demo = TRUE)$studies
  local <- vapply(seq_len(n), function(i) .qes_download_name(rows[i, , drop = FALSE]), character(1))
  twin <- duplicated(local) | duplicated(local, fromLast = TRUE)
  local[twin] <- paste0(rows$file_id[twin], "-", local[twin])
  dest <- file.path(path, local)

  # before anything is written: what is already there
  action <- rep("fetch", n)
  observed <- rep(NA_character_, n)
  conflicts <- character(0)
  for (i in seq_len(n)) {
    if (!file.exists(dest[i])) {
      next
    }
    if (dir.exists(dest[i])) {
      conflicts <- c(conflicts, dest[i])
      next
    }
    if (legacy) {
      if (!isTRUE(overwrite)) {
        action[i] <- "keep_unchecked"
      }
      next
    }
    actual <- .qes_md5(dest[i])
    if (identical(actual, rows$md5[i])) {
      action[i] <- "keep"
      observed[i] <- actual
    } else if (!isTRUE(overwrite)) {
      conflicts <- c(conflicts, dest[i])
    }
  }
  if (length(conflicts) > 0L) {
    .qes_abort(
      "download_exists",
      class = "qesR_error_input",
      args = list(.qes_q(conflicts)),
      data = list(arg = "overwrite", value = overwrite, paths = conflicts)
    )
  }

  via <- rep(NA_character_, n)
  when <- rep(as.POSIXct(NA, tz = "UTC"), n)
  downloaded <- rep(FALSE, n)
  for (i in seq_len(n)) {
    if (identical(action[i], "keep")) {
      via[i] <- "user_data"
      when[i] <- Sys.time()
      .qes_inform(
        "download_kept",
        class = "qesR_message_cached",
        args = list(.qes_q(dest[i])),
        data = list(study = rows$study[i], file_id = rows$file_id[i], path = dest[i]),
        quiet = quiet
      )
      next
    }
    if (identical(action[i], "keep_unchecked")) {
      next
    }
    row <- rows[i, , drop = FALSE]
    st <- studies[match(row$study, studies$study), , drop = FALSE]
    via[i] <- .qes_download_one(st, row, dest[i], dir = path, quiet = quiet)
    when[i] <- Sys.time()
    observed[i] <- row$md5
    downloaded[i] <- TRUE
  }

  out <- data.frame(
    study = rows$study,
    file_id = rows$file_id,
    file_name = .qes_deposit_name(rows),
    role = rows$role,
    lang = rows$lang,
    md5 = rows$md5,
    local_path = normalizePath(dest, winslash = "/", mustWork = FALSE),
    from_cache = via %in% c("session_cache", "disk_cache"),
    downloaded = downloaded,
    pinned = rep(isTRUE(pinned), n),
    stringsAsFactors = FALSE
  )
  prov <- lapply(which(action != "keep_unchecked"), function(i) {
    st <- studies[match(rows$study[i], studies$study), , drop = FALSE]
    .qes_provenance_row(
      st, rows[i, , drop = FALSE],
      md5_observed = observed[i],
      md5_verified = !is.na(observed[i]),
      pinned = pinned,
      retrieved_via = via[i],
      retrieved_at = when[i]
    )
  })
  prov <- if (length(prov) > 0L) do.call(rbind, prov) else .qes_provenance_empty()
  rownames(prov) <- NULL
  attr(out, "qes_provenance") <- prov
  .qes_inform(
    "download_done",
    class = "qesR_message_download",
    args = list(sum(downloaded), sum(action == "keep"), .qes_q(path)),
    data = list(path = path),
    quiet = quiet
  )
  out
}

# A provenance record with no rows (the columns of .qes_provenance_row()),
# for a result with no files: qes_provenance() and qes_cite() then work on it.
.qes_provenance_empty <- function() {
  catalog <- .qes_catalog(demo = TRUE)
  prov <- .qes_provenance_row(catalog$studies[1L, , drop = FALSE], catalog$files[1L, , drop = FALSE])
  prov[0L, , drop = FALSE]
}

.qes_download_empty <- function() {
  out <- data.frame(
    study = character(0), file_id = character(0), file_name = character(0),
    role = character(0), lang = character(0), md5 = character(0),
    local_path = character(0), from_cache = logical(0), downloaded = logical(0),
    pinned = logical(0), stringsAsFactors = FALSE
  )
  attr(out, "qes_provenance") <- .qes_provenance_empty()
  out
}

# ---- qes_download() --------------------------------------------------------------

#' Download the original files of a study
#'
#' `qes_download()` saves the original files of one or more studies in a
#' directory of your choice: the data file as its authors deposited it (SPSS
#' or Stata), and, with `what = "docs"`, the codebooks, questionnaires and
#' reports. Every file is checked against the md5 checksum recorded in the
#' qesR catalog before it gets its final name. Use it to keep a copy of the
#' exact files behind an analysis; to load a study into R, use [get_qes()].
#' For example, `qes_download("qes2018", path = dir, what = c("data", "docs"), lang = "fr")`
#' saves the 2018 data file and its French documents into `dir`, a folder
#' you have created.
#'
#' @section What is written:
#' Apart from the download cache (see [qes_cache_info()]; by default in
#' [tempdir()]), nothing is written outside `path`, and nothing at all unless
#' at least one file matches the request. `path` must already exist: `qes_download()`
#' never creates a directory. Each file keeps its deposited name (with its
#' extension added when the deposit omits it). When the locale cannot write
#' that name (an accented name in the C locale, for example), the file is
#' named by its Dataverse file id instead, with the same extension.
#'
#' Before writing anything, `qes_download()` looks at what is already in
#' `path`. A file that is already there with the expected md5 is kept and not
#' downloaded again (`downloaded = FALSE`). A file of the same name with
#' other content is an error of class `qesR_error_input`, and then nothing is
#' written, unless `overwrite = TRUE`.
#'
#' Each file is fetched through the download cache (see [qes_cache_info()]),
#' so a file already downloaded in the session (for example by [get_qes()])
#' is not requested again. It is then written into `path` under a temporary
#' `.part` name, checked against its md5, and only then renamed: a damaged
#' or partial copy never gets the final name. A file that fails the check is
#' an error of class `qesR_error_checksum`. When several files are requested
#' and one fails, the files completed before it stay in `path`.
#'
#' @section Pinned and latest versions:
#' By default (`version = "pinned"`), the files are those of the dataset
#' version pinned by the qesR catalog, the files [get_qes()] reads.
#' `version = "latest"` asks Dataverse (one metadata request per deposit)
#' for its latest published version and downloads the matching files, each
#' checked against the md5 Dataverse gives. This is the only way qesR reaches
#' files it has not pinned: it raises a warning of class
#' `qesR_warning_unpinned`, and the result records `pinned = FALSE`.
#' [get_qes()] keeps reading the pinned files. A file that is not in the
#' latest version, or a deaccessioned deposit, is an error of class
#' `qesR_error_source`, and then nothing is written.
#'
#' @section En français:
#' `qes_download()` enregistre les fichiers originaux d'une ou de plusieurs
#' études dans un dossier de votre choix : le fichier de données tel que
#' déposé (SPSS ou Stata) et, avec `what = "docs"`, les livres de codes,
#' questionnaires et rapports. Chaque fichier est vérifié par sa somme md5
#' avant de recevoir son nom définitif. Hormis le cache de téléchargement
#' (voir [qes_cache_info()] ; par défaut dans [tempdir()]), rien n'est écrit
#' hors de `path`, qui doit déjà exister, et rien du tout si aucun fichier ne
#' correspond à la demande. Chaque fichier garde son nom de dépôt (avec son
#' extension si le dépôt l'omet), ou prend son identifiant Dataverse, avec la
#' même extension, si la locale ne peut pas écrire ce nom (un nom accentué
#' dans la locale C, par exemple). Un fichier déjà présent avec la bonne somme md5
#' est conservé ; un fichier du même nom au contenu différent est une erreur
#' (rien n'est écrit), sauf avec `overwrite = TRUE`. Si plusieurs fichiers
#' sont demandés et que l'un échoue, les fichiers déjà terminés restent dans
#' `path`. Avec `role = NULL`, `"data"` désigne le fichier de données retenu
#' par qesR (celui que lit [get_qes()]) et `"docs"` tous les documents ;
#' nommer des rôles retient tous les fichiers de ces rôles (`role = "data"`
#' inclut aussi une copie des mêmes données dans un autre format, et
#' `"label_donor"` le fichier dont [get_qes()] tire les étiquettes de
#' `qes2012`). `version = "latest"` télécharge la
#' dernière version publiée du dépôt, vérifiée par la somme md5 que donne
#' Dataverse, avec un avertissement de classe `qesR_warning_unpinned` ; le
#' résultat indique alors `pinned = FALSE`.
#'
#' @param studies A character vector of study codes (see [qes_studies()]),
#'   or `"all"`. Codes are trimmed and case-insensitive. `"qes_demo"`, the
#'   synthetic study shipped with qesR, copies its data file with no
#'   download.
#' @param path An existing directory to save the files in. There is no
#'   default.
#' @param what `"data"` (default) for the data file, `"docs"` for the
#'   documents, or `c("data", "docs")` for both.
#' @param role Optional character vector of file roles. With `role = NULL`,
#'   `"data"` means each study's pinned data file (the one [get_qes()]
#'   reads) and `"docs"` every document. Naming roles keeps only files of
#'   those roles: `"codebook"`, `"questionnaire"`, `"technical_report"` and
#'   `"methodology"` for documents; `"data"` for every data file of the
#'   study, including a copy of the same data in another format, and
#'   `"label_donor"` for the file whose labels [get_qes()] uses for `qes2012`.
#' @param version `"pinned"` (default) or `"latest"` (see *Pinned and latest
#'   versions*).
#' @param overwrite If `TRUE`, replace files of the same name whose content
#'   differs. Default `FALSE`.
#' @param lang Optional character vector of document languages to keep
#'   (`"en"`, `"fr"`). `NULL` keeps every language. Data files have no
#'   language and are not filtered.
#' @param quiet If `TRUE`, do not print progress messages.
#'
#' @return Invisibly, a data frame with one row per file: `study`,
#'   `file_id`, `file_name` (as deposited), `role`, `lang`, `md5` (the
#'   checksum the file was checked against), `local_path`, `from_cache`
#'   (`TRUE` when the bytes came from the download cache rather than the
#'   network), `downloaded` (`FALSE` for a file already in `path`) and
#'   `pinned`. Its attribute `qes_provenance` records the study-level
#'   provenance of each file (see [qes_provenance()]). With no matching file,
#'   a data frame with no rows (and an empty provenance record).
#'
#' @family studies and documents
#' @seealso [qes_docs()] to list the documents, [get_qes()] to load a study,
#'   [qes_provenance()] and [qes_cite()] to record and cite the files.
#' @examples
#' # the synthetic demonstration study ships with qesR: no download
#' dir <- file.path(tempdir(), "qes-files")
#' dir.create(dir)
#' files <- qes_download("qes_demo", path = dir)
#' files[, c("study", "file_name", "md5", "downloaded")]
#' qes_provenance(files)
#' unlink(dir, recursive = TRUE)
#' @export
qes_download <- function(studies, path, what = c("data", "docs"), role = NULL,
                         version = c("pinned", "latest"), overwrite = FALSE,
                         lang = NULL, quiet = FALSE) {
  codes <- .qes_resolve_codes(studies, "studies", demo = TRUE)
  if (missing(path)) {
    path <- NULL
  }
  path <- .qes_check_download_path(path)
  what <- if (missing(what)) "data" else what
  if (!is.character(what) || length(what) == 0L || anyNA(what) ||
    !all(what %in% c("data", "docs"))) {
    .qes_abort(
      "input_choice",
      class = "qesR_error_input",
      args = list("what", .qes_q(c("data", "docs"))),
      data = list(arg = "what", value = what)
    )
  }
  .qes_check_choice(role, "role", .qes_download_roles(what))
  version <- .qes_check_one(version, "version", c("pinned", "latest"))
  .qes_check_flag(overwrite, "overwrite")
  .qes_check_flag(quiet, "quiet")
  .qes_check_choice(lang, "lang", .qes_enum("lang")$value)

  rows <- .qes_download_select(codes, what, role = role, lang = lang)
  if (nrow(rows) == 0L) {
    .qes_inform(
      "download_none",
      class = "qesR_message_download",
      args = list(.qes_q(codes)),
      data = list(study = codes),
      quiet = quiet
    )
    return(invisible(.qes_download_empty()))
  }
  pinned <- identical(version, "pinned")
  if (!pinned) {
    rows <- .qes_download_latest(rows, quiet = quiet)
    # raised once the unpinned files are resolved and before any is written,
    # so a later failure (files completed before it stay in `path`) still
    # comes with the warning
    .qes_warn(
      "unpinned",
      class = "qesR_warning_unpinned",
      args = list(.qes_q(unique(rows$study))),
      data = list(study = unique(rows$study), file_id = rows$file_id)
    )
  }
  out <- .qes_download_files(rows, path, overwrite = overwrite, quiet = quiet, pinned = pinned)
  invisible(out)
}
