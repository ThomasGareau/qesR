.pick_first_column <- function(data, candidates) {
  if (length(candidates) == 0L || length(names(data)) == 0L) {
    return(NA_character_)
  }

  candidates <- as.character(candidates)
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) == 0L) {
    return(NA_character_)
  }

  # Prefer exact hits first.
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) > 0L) {
    return(hit[1])
  }

  # Fall back to case-insensitive matching while keeping candidate priority.
  data_names <- names(data)
  data_names_lower <- tolower(data_names)
  for (cand in candidates) {
    idx <- match(tolower(cand), data_names_lower)
    if (!is.na(idx)) {
      return(data_names[idx])
    }
  }

  NA_character_
}

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

.build_decon <- function(data, srvy) {
  n <- nrow(data)
  # until the interim legacy builder freezes its sources (slice S4), the
  # labels qesR 0.4.4 attached to columns the file leaves unlabelled
  data <- .qes_legacy_source_labels(data, srvy, consumer = "decon")

  lookup <- list(
    citizenship = c("cps_citizen", "cps_citizenship", "citizenship"),
    yob = c("cps_yob", "yob", "ageyear_1"),
    age = c("cps_age_in_years", "age", "agecalc", "agenum"),
    gender = c("cps_genderid", "qsexe", "gender", "cps_gender"),
    province_territory = c("cps_province", "regio", "province", "province_territory"),
    education = c("cps_edu", "qscol", "education"),
    political_interest = c("cps_interest_1", "cps_intelection_1", "qinterest"),
    turnout = c("cps_turnout", "q1", "turnout"),
    votechoice = c("cps_votechoice1", "qes_votechoice", "qvote", "q2"),
    votechoice_text = c("cps_votechoice1_8_TEXT", "votechoice_text"),
    party_best = c("cps_partybest", "partybest", "qpartybest"),
    partylean = c("cps_votelean", "votelean", "partylean"),
    fed_pid = c("cps_fedpid", "fed_pid", "fpid"),
    prov_pid = c("cps_provpid", "prov_pid", "ppid"),
    ideology = c("cps_ideoself_1", "lr", "ideology"),
    income = c("cps_income", "income"),
    religion = c("cps_religion", "religion"),
    born_canada = c("cps_borncda", "born_canada")
  )

  out <- data.frame(qes_code = rep(srvy, n), stringsAsFactors = FALSE)

  for (target in names(lookup)) {
    source_col <- .pick_first_column(data, lookup[[target]])

    if (is.na(source_col)) {
      out[[target]] <- rep(NA, n)
    } else {
      out[[target]] <- .to_display_column(data[[source_col]])
    }
  }

  out
}

#' Create a Prepared Non-Exhaustive qesR Dataset
#'
#' Builds a deconstructed teaching/testing dataset with standardized columns from a selected Quebec election study.
#'
#' `get_decon()` returns the data and assigns nothing unless
#' `assign_global = TRUE`: write `decon <- get_decon("qes2022")`. The first
#' call in a session that leaves `assign_global` unset prints a one-time note
#' about this change from qesR 0.4.4.
#'
#' @param srvy A qesR survey code. Defaults to `"qes2022"`. Codes are trimmed
#'   and case-insensitive.
#' @param assign_global If TRUE, also assign the result as `decon` into the
#'   environment `get_decon()` was called from (the global environment only
#'   when called at top level). Defaults to FALSE.
#' @param quiet If TRUE, suppress informational output while downloading.
#'
#' @return A data frame with standardized columns, returned visibly.
#' @seealso [qesR-deprecated] for the legacy functions and their replacements.
#' @examples
#' \donttest{
#'   decon <- get_decon("qes2022")
#'   head(decon)
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
  code <- .get_qes_study(srvy)$qes_survey_code
  data <- .get_qes_impl(
    srvy = code,
    assign_global = FALSE,
    with_codebook = TRUE,
    quiet = quiet
  )

  decon <- .build_decon(data, srvy = code)

  if (isTRUE(assign_global)) {
    .qes_assign("decon", decon, envir)
  } else if (isTRUE(assign_missing)) {
    .qes_assign_default_notice("get_decon", "decon")
  }

  decon
}
