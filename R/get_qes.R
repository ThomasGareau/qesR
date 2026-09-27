#' Download and Load a Quebec Election Study
#'
#' Reads a study's data file, as its authors deposited it, and returns it
#' with its labels.
#'
#' `get_qes()` returns the data. It does not write anything into your
#' workspace unless you ask for it with `assign_global = TRUE`: write
#' `qes2018 <- get_qes("qes2018")`. The first call in a session that leaves
#' `assign_global` unset prints a one-time note about this change from qesR
#' 0.4.4; passing `assign_global` explicitly (TRUE or FALSE) avoids it.
#'
#' @section Which file is read:
#' Each study is pinned to one data file of one Dataverse dataset version
#' (see [qes_studies()]). `get_qes()` reads the **original** upload of that
#' file (SPSS `.sav` or Stata `.dta`), not the tab-delimited copy Dataverse
#' makes of it, after checking it against the md5 checksum recorded in the
#' package catalog. A file that fails the check is an error (class
#' `qesR_error_checksum`) and is never used; so is a file whose number of
#' rows or columns differs from the catalog (`qesR_error_rowcount`). The
#' file is downloaded once and kept in the download cache (see
#' [qes_cache_info()]); within a session the parsed data is also kept in
#' memory, so a second call makes no request (`options(qesR.memo = FALSE)`
#' turns this off).
#'
#' The data is returned as deposited: column names, codes and missing values
#' (`NA`) are those of the file, and no row is dropped or recoded. For qesR
#' 0.4.4 users, names, codes and `is.na()` counts are unchanged; three
#' columns of `qes2007_panel` keep their 0.4.4 names (`AFFGÉN`, `PROPRIÉ`,
#' `PROPGÉN`). Compared with 0.4.4, accented text is no longer damaged,
#' `qes2022` dates are date-times, and labels are those of the file (below).
#'
#' @section Labels and missing values:
#' Labelled columns are [haven::labelled()] vectors; convert one with
#' [haven::as_factor()]. Variable and value labels come from the data file
#' itself, never from Dataverse's metadata or from a variable name. For
#' `qes2012`, whose Stata file (the one qesR 0.4.4 read, with its lowercase
#' names) has labels that Stata lowercased and cut at 80 characters, the
#' complete labels of the SPSS twin of the same data are used. A few labels
#' of the CROP files typed in another character set are corrected (for
#' example "RESTE DU QUÉBEC"). `qes2018`'s data file has value labels for
#' only a few variables; the codebook (see [qes_codebook()]) gives the others,
#' from the study's questionnaire, without changing the data.
#'
#' Codes that an SPSS file declares as user-missing (such as 8 or 9 for "Don't
#' know") are kept as values, as in qesR 0.4.4; the declaration is kept in
#' the column attributes `qes_na_values` and `qes_na_range`. [qes_missing()]
#' sets these codes, and the "don't know" and "refused" codes the codebook
#' types, to `NA`.
#'
#' The `qes_codebook` attribute is built offline from the metadata shipped
#' with qesR. `qes2022`'s metadata is not shipped (CC BY-NC 4.0): it is
#' built from the file just read and kept in the download cache, so that
#' [qes_codebook()] and [qes_search()] can use it later.
#'
#' @param srvy A qesR survey code from `qes_studies()`, or `"qes_demo"` for
#'   the small synthetic study shipped with the package. Codes are trimmed
#'   and case-insensitive (`" QES2018 "` is `"qes2018"`); an unknown code is an
#'   error of class `qesR_error_unknown_study` that suggests near matches.
#' @param file Optional regular expression that chooses one of the study's
#'   data files by name, when its deposit holds several (for example
#'   `get_qes("qes2014", file = "dta")` reads the Stata version instead of
#'   the default SPSS file). It is matched, ignoring case, against data files
#'   only (`get_qes("qes2012", file = "SPSS")` reads the SPSS twin whose
#'   labels complete the default Stata file's). A pattern that is not a valid
#'   regular expression is an error of class `qesR_error_input`. A pattern
#'   that matches no data file of the study but the data
#'   file of another study of the same deposit reads that study, with a
#'   message: `get_qes("qes1998", file = "CROP")` reads `qes1998_crop`. A
#'   pattern that matches no file, or several, is an error of class
#'   `qesR_error_ambiguous_file`.
#' @param assign_global If TRUE, also assign the data as `<code>` into the
#'   environment `get_qes()` was called from (the global environment only when
#'   called at top level), where `<code>` is the canonical study code. With
#'   `with_codebook = TRUE`, the codebook is assigned as `<code>_codebook` too.
#'   Defaults to FALSE. The data is returned either way.
#' @param with_codebook If TRUE, attach the study's codebook for the columns
#'   read (see [qes_codebook()]; built offline, with no request) as the
#'   `qes_codebook` attribute (and assign \code{<code>_codebook} when
#'   `assign_global = TRUE`).
#' @param quiet If TRUE, suppress informational output.
#'
#' @return A base data frame, returned visibly, with attributes
#'   `qes_survey_code` (the canonical study code), `qes_provenance` (a
#'   one-row data frame recording the DOI, dataset version, file id, file
#'   name, md5, UNF, dimensions, where the file came from and when, the
#'   licence, the source of the labels and the reader used; see
#'   [qes_provenance()]) and, with `with_codebook = TRUE`, `qes_codebook`.
#' @seealso [qes_studies()] for the study codes and their pinned files,
#'   [qes_provenance()] for the record of the file read, [qes_download()] to
#'   save the original files, [qes_cache_info()] for the download cache.
#' @examples
#' # the synthetic demonstration study ships with qesR: no download
#' demo <- get_qes("qes_demo", quiet = TRUE)
#' dim(demo)
#' attr(demo, "qes_provenance")[, c("study", "file_name", "md5_verified")]
#'
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
  study <- .get_qes_study(srvy, demo = TRUE)
  # `file` may name the data file of another study of the same deposit
  # (get_qes("qes1998", file = "CROP")): resolve it first, so the banner, the
  # assigned names and the attributes are those of the study actually read
  selection <- .qes_select_data_file(study$qes_survey_code, file = file, quiet = quiet)
  if (!identical(selection$study, study$qes_survey_code)) {
    study <- .get_qes_study(selection$study, demo = TRUE)
  }
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

  data <- .qes_read(code, selection$file$file_id, quiet = quiet)
  codebook <- NULL
  if (isTRUE(with_codebook)) {
    codebook <- .qes_data_codebook(data, code, selection$file, quiet = quiet)
    attr(data, "qes_codebook") <- codebook
    .qes_inform(
      "codebook_counts",
      class = "qesR_message_download",
      args = list(
        nrow(codebook),
        nrow(attr(codebook, "codebook_files", exact = TRUE) %||% data.frame())
      ),
      data = list(study = code),
      quiet = quiet
    )
  }
  attr(data, "qes_label_source") <- NULL
  attr(data, "qes_survey_code") <- code

  if (isTRUE(assign_global)) {
    if (isTRUE(with_codebook)) {
      .qes_assign(paste0(code, "_codebook"), codebook, envir)
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
#' [qesR-deprecated]). It is exactly `head()` of what [get_qes()] returns,
#' so the data comes from the same pinned file, is served from memory or the
#' download cache when the study was read before in the session, and keeps
#' the attributes `qes_survey_code`, `qes_provenance` and `qes_codebook`.
#'
#' @param srvy A qesR survey code from `qes_studies()`.
#' @param obs Number of observations to return: a whole number of at least
#'   1 (otherwise an error of class `qesR_error_input`).
#' @param file Optional regular expression for choosing one file in multi-file datasets.
#'
#' @return A base data frame with the first `obs` rows, and the attributes
#'   of [get_qes()] data (`qes_survey_code`, `qes_provenance`,
#'   `qes_codebook`).
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

# The codebook attached by get_qes(): the study's metadata for the columns of
# `data` (read from `file_row`), laid out compact, with the data's provenance.
# When `data` is the pinned file, it also serves to build the metadata of a
# study whose metadata is not shipped (a qes2022 shard), with no second read.
.qes_data_codebook <- function(data, code, file_row, quiet = TRUE) {
  pinned <- .qes_default_data_file(code, demo = .qes_is_demo_code(code))
  dict <- .qes_dict_study(
    code,
    data = if (identical(file_row$file_id, pinned$file_id)) data else NULL,
    quiet = quiet
  )
  dict <- .qes_dict_for_data(data, code, dict = dict)
  attrs <- .qes_codebook_attrs(code, file_row, names_row = file_row)
  prov <- attr(data, "qes_provenance", exact = TRUE)
  if (is.data.frame(prov)) {
    attrs$qes_provenance <- prov
  }
  .qes_codebook_make(dict, layout = "compact", lang = NULL, attrs = attrs)
}
