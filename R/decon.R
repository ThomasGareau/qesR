# A labelled column as get_decon() returned it in qesR 0.4.4: a factor of its
# labels. A column whose labels only repeat the codes (qes2022
# cps_age_in_years labels each age 15..115 with itself) stays a plain number,
# as in 0.4.4: as_factor() would turn ages into levels, and as.numeric() would
# then return level positions instead of ages.
.to_display_column <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    labels <- attr(x, "labels", exact = TRUE)
    identity_only <- length(labels) == 0L ||
      identical(trimws(names(labels)), trimws(as.character(unclass(labels))))
    if (identity_only && is.numeric(unclass(x))) {
      out <- .qes_plain(x)
      label <- attr(x, "label", exact = TRUE)
      if (!is.null(label)) {
        attr(out, "label") <- label
      }
      return(out)
    }
    return(haven::as_factor(x))
  }
  x
}

# Set the cells of `mask` to NA in a get_decon() column, keeping its class;
# the factor levels that only blanked cells had are dropped.
.qes_decon_blank <- function(x, mask) {
  if (!any(mask)) {
    return(x)
  }
  if (is.factor(x)) {
    gone <- unique(as.character(x[mask]))
    x[mask] <- NA
    unused <- setdiff(gone, as.character(x[!is.na(x)]))
    if (length(unused) > 0L) {
      x <- factor(x, levels = setdiff(levels(x), unused))
    }
    return(x)
  }
  x[mask] <- NA
  x
}

# get_decon() of one study: the frozen qesR 0.4.4 source of every column
# (inst/extdata/legacy/sources.csv, profile "decon"), shown as 0.4.4 showed
# it, with the verified-invalid cells set to NA (blanks.csv). Attributes:
# source_map, legacy_na_columns and timing (what turnout, votechoice and
# votechoice_text hold: "pre" for the campaign-period qes2022 items, NA where
# the column has no valid source).
.build_decon <- function(data, srvy) {
  n <- nrow(data)
  data <- .qes_legacy_source_labels(data, srvy, consumer = "decon")
  sources <- .qes_legacy_sources("decon", srvy)
  masks <- .qes_legacy_blank_masks(data, srvy, "decon", sources)

  out <- data.frame(qes_code = rep(srvy, n), stringsAsFactors = FALSE)
  counts <- integer(0)
  for (target in names(sources)) {
    source_col <- sources[[target]]
    if (is.na(source_col) || !(source_col %in% names(data))) {
      # a pinned file always has its frozen sources; only synthetic test
      # data can lack one
      out[[target]] <- rep(NA, n)
      next
    }
    x <- .to_display_column(data[[source_col]])
    if (!is.null(masks[[target]])) {
      counts[[target]] <- .qes_legacy_blank_count(x, masks[[target]])
      x <- .qes_decon_blank(x, masks[[target]])
    }
    out[[target]] <- x
  }

  timing <- .qes_legacy_constants(srvy)$decon_vote_timing
  attr(out, "timing") <- c(
    turnout = if (is.na(sources[["turnout"]]) || !is.null(masks[["turnout"]]) && all(masks[["turnout"]])) NA_character_ else timing,
    votechoice = if (is.na(sources[["votechoice"]]) || !is.null(masks[["votechoice"]]) && all(masks[["votechoice"]])) NA_character_ else timing,
    votechoice_text = if (is.na(sources[["votechoice_text"]])) NA_character_ else timing
  )
  attr(out, "source_map") <- data.frame(
    qes_code = srvy,
    column = names(sources),
    source_variable = unname(sources),
    stringsAsFactors = FALSE
  )
  attr(out, "legacy_na_columns") <- .qes_legacy_na_rows(srvy, "decon", sources, masks, n, counts)
  out
}

#' Create a Prepared Non-Exhaustive qesR Dataset
#'
#' Builds a small teaching dataset with 19 standardized columns from one
#' Quebec Election Study of qesR 0.4.4.
#'
#' `get_decon()` returns the data and assigns nothing unless
#' `assign_global = TRUE`: write `decon <- get_decon("qes2022")`. The first
#' call in a session that leaves `assign_global` unset prints a one-time note
#' about this change from qesR 0.4.4.
#'
#' Each column reads the variable qesR 0.4.4 read for that study (the
#' `source_map` attribute; nothing is chosen by name at run time) and shows
#' it as 0.4.4 did: a labelled column becomes a factor of its labels, other
#' columns keep their values. Every respondent of the study is kept.
#'
#' @section Columns:
#' `qes_code`, then `citizenship`, `yob` (year of birth), `age`, `gender`,
#' `province_territory`, `education`, `political_interest`, `turnout`,
#' `votechoice`, `votechoice_text`, `party_best`, `partylean`, `fed_pid`,
#' `prov_pid`, `ideology`, `income`, `religion` and `born_canada`. Most of
#' them are filled for `qes2022` only; a column with no source in a study is
#' all `NA`.
#'
#' @section What changed in 0.5.0:
#' Values verified to be wrong are `NA`; other differences come from reading
#' the original files (label case, repaired characters, doubles instead of
#' integers; see NEWS). The values set to `NA`:
#' * `turnout` and `votechoice` of every study but `qes2022`, whose sources in
#'   qesR 0.4.4 were other questions (`q1` and `q2`: satisfaction with
#'   democracy and the most important issue in `qes2018`, for example);
#' * `party_best` and `partylean` everywhere (a different construct in each
#'   study);
#' * the `-99` item nonresponse codes of `qes2022` in every column (the
#'   `ideology` mean is about 4.96, not 2.64).
#'
#' `qes2022` `turnout` and `votechoice` are the campaign-period likelihood of
#' voting and vote intention, as in 0.4.4; `attr(, "timing")` says so
#' (`"pre"`). The attributes `source_map` and `legacy_na_columns` give the
#' source of every column and the reason for every `NA` column (see
#' [get_qes_master()]). A message says so once per session (classes
#' `qesR_message_values_changed` and `qesR_message_legacy_columns`).
#'
#' @section En français:
#' `get_decon()` construit un petit jeu de données d'enseignement de 19
#' colonnes à partir d'une étude de qesR 0.4.4, en lisant les mêmes
#' variables que qesR 0.4.4. Depuis qesR 0.5.0, les valeurs vérifiées comme
#' fausses sont mises à `NA` : `turnout` et `votechoice` sauf pour `qes2022`,
#' `party_best` et `partylean` partout, et les codes `-99` de `qes2022`.
#' Les autres différences viennent de la lecture des fichiers originaux
#' (casse des étiquettes, caractères réparés, nombres réels au lieu
#' d'entiers : voir NEWS). L'attribut `timing` indique que `turnout` et `votechoice` de `qes2022`
#' sont mesurés pendant la campagne (`"pre"`).
#'
#' @param srvy A qesR survey code. Defaults to `"qes2022"`. Codes are trimmed
#'   and case-insensitive. The 11 studies of qesR 0.4.4 are available, and
#'   `"qes_demo"`, the synthetic demonstration study; the studies added
#'   since (`qes1998_crop`, `qes1998_createc`) raise an error of class
#'   `qesR_error_input`.
#' @param assign_global If TRUE, also assign the result as `decon` into the
#'   environment `get_decon()` was called from (the global environment only
#'   when called at top level). Defaults to FALSE.
#' @param quiet If TRUE, suppress informational output while downloading.
#'
#' @return A data frame with the 19 columns above, returned visibly, with
#'   the attributes `timing`, `source_map`, `legacy_na_columns` and
#'   `qes_provenance` (the file read; see [qes_provenance()]).
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' # the synthetic demonstration study, offline
#' decon <- get_decon("qes_demo", quiet = TRUE)
#' head(decon)
#'
#' \donttest{
#'   decon <- get_decon("qes2022")
#'   attr(decon, "timing")
#' }
#' @export
get_decon <- function(srvy = "qes2022", assign_global = FALSE, quiet = FALSE) {
  .qes_deprecate("get_decon")
  .get_decon_impl(
    srvy, assign_global = assign_global, quiet = quiet,
    envir = parent.frame(),
    assign_missing = missing(assign_global)
  )
}

.get_decon_impl <- function(srvy = "qes2022", assign_global = FALSE, quiet = FALSE,
                            envir = NULL, assign_missing = FALSE) {
  code <- .get_qes_study(srvy, demo = TRUE)$qes_survey_code
  if (length(code) != 1L || !(code %in% c(.qes_legacy_codes, "qes_demo"))) {
    .qes_abort(
      "input_decon_study",
      class = "qesR_error_input",
      args = list(.qes_q(srvy)),
      data = list(arg = "srvy", value = srvy)
    )
  }
  .qes_legacy_notice("get_decon")
  data <- .get_qes_impl(
    srvy = code,
    assign_global = FALSE,
    with_codebook = FALSE,
    quiet = quiet
  )

  decon <- .build_decon(data, srvy = code)
  attr(decon, "qes_provenance") <- attr(data, "qes_provenance", exact = TRUE)

  if (isTRUE(assign_global)) {
    .qes_assign("decon", decon, envir)
  } else if (isTRUE(assign_missing)) {
    .qes_assign_default_notice("get_decon", "decon")
  }

  decon
}
