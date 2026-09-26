.qes_catalog <- local({
  catalog <- data.frame(
    index = 1:11,
    qes_survey_code = c(
      "qes2022",
      "qes2018",
      "qes2018_panel",
      "qes2014",
      "qes2012",
      "qes2012_panel",
      "qes_crop_2007_2010",
      "qes2008",
      "qes2007",
      "qes2007_panel",
      "qes1998"
    ),
    year = c(
      "2022",
      "2018",
      "2018",
      "2014",
      "2012",
      "2012",
      "2007-2010",
      "2008",
      "2007",
      "2007",
      "1998"
    ),
    name_en = c(
      "Quebec Election Study 2022",
      "Quebec Election Study 2018",
      "Quebec Election Study 2018 Panel",
      "Quebec Election Study 2014",
      "Quebec Election Study 2012",
      "Quebec Election Study 2012 Panel",
      "CROP Quebec Opinion Polls (2007-2010)",
      "Quebec Election Study 2008",
      "Quebec Election Study 2007",
      "Quebec Election Study 2007 Panel",
      "Quebec Elections 1998"
    ),
    name_fr = c(
      "Etude electorale quebecoise 2022",
      "Etude electorale quebecoise 2018",
      "Panel de l'etude electorale quebecoise 2018",
      "Etude electorale quebecoise 2014",
      "Etude electorale quebecoise 2012",
      "Panel de l'etude electorale quebecoise 2012",
      "Sondages CROP (2007-2010)",
      "Etude electorale quebecoise 2008",
      "Etude electorale quebecoise 2007",
      "Panel de l'etude electorale quebecoise 2007",
      "Elections quebecoises de 1998"
    ),
    doi = c(
      "10.7910/DVN/PAQBDR",
      "10.5683/SP3/NWTGWS",
      "10.5683/SP3/XDDMMR",
      "10.5683/SP3/64F7WR",
      "10.5683/SP2/WXUPXT",
      "10.5683/SP3/RKHPVL",
      "10.5683/SP3/IRZ1PF",
      "10.5683/SP2/8KEYU3",
      "10.5683/SP2/6XGOKA",
      "10.5683/SP3/NDS6VT",
      "10.5683/SP2/QFUAWG"
    ),
    server = c(
      "https://dataverse.harvard.edu",
      rep("https://borealisdata.ca", 10)
    ),
    stringsAsFactors = FALSE
  )

  catalog$doi_url <- paste0("https://doi.org/", catalog$doi)

  catalog$documentation <- ifelse(
    catalog$server == "https://dataverse.harvard.edu",
    paste0("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:", catalog$doi),
    paste0("https://borealisdata.ca/dataset.xhtml?persistentId=doi:", catalog$doi)
  )

  catalog
})

# ---- study codes -----------------------------------------------------------
#
# Study codes are trimmed and case-folded to the canonical catalog code, never
# matched fuzzily: " QES2018 " is qes2018, "2018" is an error that suggests
# qes2018 (design.md section 2.1).

.qes_canon_code <- function(x) {
  tolower(trimws(x))
}

# Near matches for an unknown code: catalog codes that contain it, then the
# codes at the smallest edit distance (utils::adist), if it is small.
.qes_suggest_codes <- function(x, codes = .qes_catalog$qes_survey_code, n = 3L) {
  key <- .qes_canon_code(x)
  if (is.na(key) || !nzchar(key)) {
    return(character(0))
  }
  contains <- codes[grepl(key, codes, fixed = TRUE)]
  d <- as.vector(utils::adist(key, codes))
  limit <- max(2L, ceiling(nchar(key) / 3))
  near <- if (min(d) <= limit) codes[d == min(d)] else character(0)
  utils::head(unique(c(contains, near)), n)
}

.qes_unknown_study <- function(study) {
  suggestions <- unique(unlist(lapply(study, .qes_suggest_codes), use.names = FALSE))
  suggestions <- suggestions %||% character(0)
  if (length(suggestions) > 0L) {
    .qes_abort(
      "unknown_study_suggest",
      class = "qesR_error_unknown_study",
      args = list(.qes_q(study), .qes_q(suggestions)),
      data = list(study = study, suggestions = suggestions)
    )
  }
  .qes_abort(
    "unknown_study",
    class = "qesR_error_unknown_study",
    args = list(.qes_q(study)),
    data = list(study = study, suggestions = character(0))
  )
}

# One catalog row for a single study code (canonicalized).
.get_qes_study <- function(srvy, arg = "srvy") {
  .assert_single_string(srvy, arg)
  if (!nzchar(trimws(srvy))) {
    .qes_abort(
      "input_string",
      class = "qesR_error_input",
      args = list(arg),
      data = list(arg = arg, value = srvy)
    )
  }
  idx <- match(.qes_canon_code(srvy), .qes_catalog$qes_survey_code)
  if (is.na(idx)) {
    .qes_unknown_study(srvy)
  }
  .qes_catalog[idx, , drop = FALSE]
}

# Canonical codes for a vector of study codes. "all" is valid only on its own.
.qes_resolve_codes <- function(x, arg) {
  if (!is.character(x) || length(x) == 0L || anyNA(x) || !all(nzchar(trimws(x)))) {
    .qes_abort(
      "input_codes",
      class = "qesR_error_input",
      args = list(arg),
      data = list(arg = arg, value = x)
    )
  }
  canon <- .qes_canon_code(x)
  if ("all" %in% canon) {
    if (length(canon) > 1L) {
      .qes_abort(
        "input_all_mixed",
        class = "qesR_error_input",
        args = list(arg),
        data = list(arg = arg, value = x)
      )
    }
    return(.qes_catalog$qes_survey_code)
  }
  unknown <- x[!(canon %in% .qes_catalog$qes_survey_code)]
  if (length(unknown) > 0L) {
    .qes_unknown_study(unknown)
  }
  unique(canon)
}

#' List Quebec Election Study Survey Codes
#'
#' Returns a data frame of qesR survey call codes, with optional detailed metadata.
#'
#' @param detailed If TRUE, include year, names, DOI, and documentation columns.
#'
#' @return A data frame of qesR survey codes (cesR-style by default).
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' get_qescodes()
#' get_qescodes(detailed = TRUE)
#' @export
get_qescodes <- function(detailed = FALSE) {
  .qes_deprecate("get_qescodes")
  .get_qescodes_impl(detailed = detailed)
}

.get_qescodes_impl <- function(detailed = FALSE) {
  out <- .qes_catalog[, c(
    "index",
    "qes_survey_code"
  )]
  out$get_qes_call_char <- sprintf('"%s"', out$qes_survey_code)
  out <- out[, c("index", "qes_survey_code", "get_qes_call_char")]

  if (isTRUE(detailed)) {
    extra <- .qes_catalog[, c("year", "name_en", "name_fr", "doi", "doi_url", "documentation")]
    out <- cbind(out, extra, stringsAsFactors = FALSE)
  }

  rownames(out) <- NULL
  out
}
