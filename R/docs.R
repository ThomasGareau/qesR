# Study documents (design.md section 2.2, qes_docs), offline from the catalog.

.qes_doc_roles <- c("codebook", "questionnaire", "technical_report", "methodology")

.qes_check_choice <- function(x, arg, choices) {
  if (is.null(x)) {
    return(invisible(NULL))
  }
  if (!is.character(x) || length(x) == 0L || anyNA(x) || !all(x %in% choices)) {
    .qes_abort(
      "input_choice",
      class = "qesR_error_input",
      args = list(arg, .qes_q(choices)),
      data = list(arg = arg, value = x)
    )
  }
  invisible(x)
}

#' List the documents of each study
#'
#' `qes_docs()` lists the codebooks, questionnaires and technical or
#' methodological reports deposited with each study, from the catalog shipped
#' with qesR. It makes no network request. Each row gives the Dataverse file
#' id, the deposited file name, a curated role and language, the size and md5
#' checksum, and a download URL.
#'
#' @param studies Optional character vector of study codes (see
#'   [qes_studies()]); `NULL` lists every study, `"all"` too. Codes are
#'   trimmed and case-insensitive.
#' @param role Optional character vector of roles to keep: `"codebook"`,
#'   `"questionnaire"`, `"technical_report"` or `"methodology"`.
#' @param lang Optional character vector of document languages to keep
#'   (`"en"`, `"fr"`). `NULL` keeps documents in every language.
#'
#' @return A data frame with one row per document and the columns `study`,
#'   `file_id`, `file_name`, `role`, `lang`, `format` (`pdf`, `doc` or
#'   `docx`), `bytes`, `md5` and `url`. Documents are listed in catalog order.
#'
#' @family studies and documents
#' @seealso [qes_studies()] for the studies.
#' @examples
#' qes_docs("qes2018")
#'
#' # French questionnaires of the Quebec Election Studies
#' qes_docs(qes_studies(family = "qes")$study, role = "questionnaire", lang = "fr")
#' @export
qes_docs <- function(studies = NULL, role = NULL, lang = NULL) {
  codes <- if (is.null(studies)) .qes_study_codes() else .qes_resolve_codes(studies, "studies")
  .qes_check_choice(role, "role", .qes_doc_roles)
  .qes_check_choice(lang, "lang", .qes_enum("lang")$value)

  catalog <- .qes_catalog()
  files <- catalog$files
  keep <- files$study %in% codes & files$role %in% (role %||% .qes_doc_roles)
  if (!is.null(lang)) {
    keep <- keep & files$lang %in% lang
  }
  docs <- files[keep, , drop = FALSE]
  docs <- docs[order(match(docs$study, codes)), , drop = FALSE]
  servers <- catalog$studies$server[match(docs$study, catalog$studies$study)]
  url <- vapply(seq_len(nrow(docs)), function(i) {
    .qes_url(servers[i], "file", file_id = docs$file_id[i])
  }, character(1))
  out <- data.frame(
    study = docs$study,
    file_id = docs$file_id,
    file_name = docs$file_name,
    role = docs$role,
    lang = docs$lang,
    format = docs$format,
    bytes = docs$bytes,
    md5 = docs$md5,
    url = url,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# ---- get_codebook_files() / get_qes_codebook_files() (legacy) ----------------

#' Get Codebook Files
#'
#' Returns codebook/support files (PDFs, questionnaires, metadata files) associated with a study.
#'
#' Soft-deprecated: use [qes_docs()]. `get_codebook_files()` keeps working and
#' will not be removed; it prints a one-time notice (see [qesR-deprecated]).
#' It now returns every document deposited with the study (codebooks,
#' questionnaires and reports) from the offline catalog, with the qesR 0.4.4
#' columns, and makes no network request. `file` and `refresh` no longer
#' change the result.
#'
#' @param srvy A qesR survey code. Required if `codebook` is NULL.
#' @param codebook A `qes_codebook` object. Its study is used when it records
#'   one; otherwise the file manifest attached to it is returned.
#' @param file Ignored: documents do not depend on which data file is read.
#' @param quiet If TRUE, suppress informational output.
#' @param refresh Ignored: the list comes from the catalog shipped with qesR.
#'
#' @return A data frame of codebook/support files with the columns `file_id`,
#'   `filename`, `extension`, `size` and `download_url`.
#' @seealso [qes_docs()], and [qesR-deprecated] for the legacy functions and
#'   their replacements.
#' @examples
#' get_codebook_files(srvy = "qes2022")
#' @export
get_codebook_files <- function(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE) {
  .qes_deprecate("get_codebook_files")
  .get_codebook_files_impl(
    srvy, codebook = codebook, file = file, quiet = quiet, refresh = refresh,
    fn = "get_codebook_files"
  )
}

#' Alias for get_codebook_files
#'
#' Backward-compatible alias for `get_codebook_files()`.
#'
#' Soft-deprecated: use [qes_docs()]. It keeps working and will not be
#' removed; it prints a one-time notice (see [qesR-deprecated]).
#'
#' @inheritParams get_codebook_files
#'
#' @return A data frame of codebook/support files.
#' @seealso [qes_docs()], and [qesR-deprecated] for the legacy functions and
#'   their replacements.
#' @examples
#' get_qes_codebook_files(srvy = "qes2022")
#' @export
get_qes_codebook_files <- function(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE) {
  .qes_deprecate("get_qes_codebook_files")
  .get_codebook_files_impl(
    srvy = srvy,
    codebook = codebook,
    file = file,
    quiet = quiet,
    refresh = refresh,
    fn = "get_qes_codebook_files"
  )
}

.qes_legacy_files_frame <- function(n = 0L) {
  data.frame(
    file_id = character(n),
    filename = character(n),
    extension = character(n),
    size = numeric(n),
    download_url = character(n),
    stringsAsFactors = FALSE
  )
}

.get_codebook_files_impl <- function(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE,
                                     refresh = FALSE, fn = "get_codebook_files") {
  if (!is.null(file)) {
    .qes_arg_ignored(fn, "file")
  }
  if (isTRUE(refresh)) {
    .qes_arg_ignored(fn, "refresh")
  }
  code <- NULL
  if (is.null(codebook)) {
    if (is.null(srvy)) {
      .qes_abort(
        "input_srvy_or_codebook",
        class = "qesR_error_input",
        data = list(arg = "srvy", value = NULL)
      )
    }
    code <- .get_qes_study(srvy)$qes_survey_code
  } else {
    recorded <- attr(codebook, "survey_code", exact = TRUE)
    if (is.character(recorded) && length(recorded) == 1L && !is.na(recorded)) {
      code <- .qes_resolve_codes(recorded, "codebook")
    } else {
      # A codebook that does not record its study: return its own manifest.
      return(.qes_codebook_attr_files(codebook))
    }
  }

  docs <- qes_docs(.qes_legacy_deposit_codes(code))
  out <- .qes_legacy_files_frame(nrow(docs))
  out$file_id <- docs$file_id
  out$filename <- docs$file_name
  out$extension <- docs$format
  out$size <- docs$bytes
  out$download_url <- docs$url
  out
}

# qesR 0.4.4 listed every document of a study's deposit. For a legacy code,
# the documents of the other catalog studies that share its deposit belong to
# it too (qes1998 lists the CROP and CREATEC codebooks as well as its own).
.qes_legacy_deposit_codes <- function(code) {
  if (!code %in% .qes_legacy_codes) {
    return(code)
  }
  st <- .qes_catalog()$studies
  i <- match(code, st$study)
  shared <- st$study[!st$demo & st$server == st$server[i] & st$doi == st$doi[i]]
  unique(c(code, shared))
}

# The file manifest attached to a codebook built by the DDI path
# (attr "codebook_files"), for get_codebook_files(codebook = ) on a codebook
# that does not record its study.
.qes_codebook_attr_files <- function(codebook) {
  files <- attr(codebook, "codebook_files", exact = TRUE)
  if (is.null(files)) {
    files <- .qes_legacy_files_frame()
  }
  files
}

# The once-per-session note for a legacy argument that no longer changes the
# result (class qesR_message_arg_ignored; `quiet` does not silence it).
.qes_arg_ignored <- function(fn, arg) {
  if (!.qes_once_first(paste0("arg_ignored:", fn, ":", arg))) {
    return(invisible(FALSE))
  }
  .qes_inform(
    "arg_ignored",
    class = "qesR_message_arg_ignored",
    args = list(fn, arg),
    data = list(fn = fn, arg = arg)
  )
  invisible(TRUE)
}
