.pick_first_column <- function(data, candidates) {
  hit <- candidates[candidates %in% names(data)]
  if (length(hit) == 0L) {
    return(NA_character_)
  }
  hit[1]
}

.to_display_column <- function(x) {
  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    return(haven::as_factor(x))
  }
  x
}

.build_decon <- function(data, srvy) {
  n <- nrow(data)

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

#' Create a prepared non-exhaustive qesR dataset
#'
#' Builds a deconstructed (teaching/testing) dataset with standardized columns
#' from a selected Quebec election study.
#'
#' @param srvy A qesR survey code. Defaults to `"qes2022"`.
#' @param assign_global If `TRUE`, assign the result as `decon` in the global
#'   environment.
#' @param quiet If `TRUE`, suppress informational output while downloading.
#'
#' @return A data frame named `decon` when assigned globally.
#' @export
get_decon <- function(srvy = "qes2022", assign_global = TRUE, quiet = FALSE) {
  data <- get_qes(
    srvy = srvy,
    assign_global = FALSE,
    with_codebook = TRUE,
    quiet = quiet
  )

  decon <- .build_decon(data, srvy = srvy)

  if (isTRUE(assign_global)) {
    assign("decon", decon, envir = .GlobalEnv)
  }

  decon
}
