.master_harmonization_lookup <- function() {
  list(
    respondent_id = c("ResponseId", "responseid", "respid", "quetr", "quest2_crop", "quest", "QUEST", "questpost", "questpst", "questbv", "questionnaire7701", "seq"),
    interview_start = c("cps_StartDate", "sdat", "SDAT", "startdate", "StartDate"),
    interview_end = c("cps_EndDate", "enddate", "EndDate"),
    interview_recorded = c("cps_RecordedDate", "recordeddate", "RecordedDate", "date"),
    language = c("cps_UserLanguage", "LANG", "lang", "qlang", "QLANG", "langu", "lmat"),
    citizenship = c("cps_citizen", "cps_citizenship", "citizenship"),
    year_of_birth = c("cps_yob", "QAGE", "qage", "agex", "ageyear_1", "yob", "q75", "Q75"),
    age = c("cps_age_in_years", "SMAGE", "smage", "age", "agecalc", "agenum", "QAGE", "qage", "q0age"),
    age_group = c("CLAGE", "clage", "age_3gr", "agegrp", "age3", "age45", "age"),
    gender = c("cps_genderid", "QSEXE", "qsexe", "sexe", "sexe_post", "gender"),
    province_territory = c("cps_province", "q0qc", "Q0QC", "QREGION", "qregion", "regio", "reg", "reg_3gr"),
    education = c("cps_edu", "QSCOL", "qscol", "scol", "scolU", "scolu", "education", "educ", "edu", "scolu"),
    income = c("cps_income", "income"),
    religion = c("cps_religion", "religion"),
    born_canada = c("cps_borncda", "born_canada"),
    political_interest = c("cps_interest_1", "cps_intelection_1", "qinterest", "interest", "interet"),
    ideology = c("cps_ideoself_1", "lr", "ideology", "q70a"),
    turnout = c("cps_turnout", "Q2", "q21", "q11", "allervot", "allervo2", "avote", "participation", "votebin", "voteoui", "turnout", "q1"),
    vote_choice = c("cps_votechoice1", "Q3", "q25", "q12a", "q12", "voteprov", "intvoteprov", "intvote", "intvote2", "vote94", "vote", "qes_votechoice", "qvote", "q2", "vote_choice", "votechoice"),
    vote_choice_text = c("cps_votechoice1_8_TEXT", "q2_96_other", "votechoice_text"),
    party_best = c("cps_partybest", "partybest", "qpartybest", "Q8"),
    party_lean = c("cps_votelean", "Q5", "q13", "q12b", "intvoteref", "voteref", "intref", "votelean", "partylean"),
    sovereignty_support = c("cps_qc_referendum", "cps_qc_independent", "independance", "Q20", "q52", "q19", "souv_rec", "intvoteref", "intref"),
    federal_pid = c("cps_fedpid", "fed_pid", "fpid"),
    provincial_pid = c("cps_provpid", "prov_pid", "ppid"),
    survey_weight = c("cps_wts", "poids", "pond", "pondx", "ponder2", "ponder3", "ponderc", "pondvote", "weight", "weights", "wgt", "pdspart", "pdspart2")
  )
}

.master_study_overrides <- function() {
  list(
    qes2022 = c(sovereignty_support = "cps_qc_referendum"),
    qes2018 = c(turnout = "q1", vote_choice = "q2", sovereignty_support = NA_character_),
    qes2018_panel = c(vote_choice = "vote", party_lean = "intvote", sovereignty_support = "independance"),
    qes2014 = c(turnout = "Q2", vote_choice = "Q3", party_lean = "Q5", sovereignty_support = "Q20"),
    qes2012 = c(turnout = "q21", vote_choice = "q25", sovereignty_support = "q52"),
    qes2012_panel = c(turnout = "participation", vote_choice = "voteprov", party_lean = "intvoteprov", sovereignty_support = "souv_rec"),
    qes_crop_2007_2010 = c(age = "QAGE", sovereignty_support = NA_character_),
    qes2008 = c(turnout = "q11", vote_choice = "q12a", party_lean = "q12b", sovereignty_support = "q19"),
    qes2007 = c(turnout = "q11", vote_choice = "q12", party_lean = "q13", sovereignty_support = "q19"),
    qes2007_panel = c(turnout = "avote", vote_choice = "vote", party_lean = "intvote", sovereignty_support = "intref"),
    qes1998 = c(turnout = "allervot", vote_choice = "intvote")
  )
}

.resolve_master_source_column <- function(data, srvy, target, candidates) {
  overrides <- .master_study_overrides()
  study_map <- overrides[[srvy]]

  if (!is.null(study_map) && target %in% names(study_map)) {
    preferred <- unname(study_map[[target]])
    if (length(preferred) == 1L && is.na(preferred)) {
      return(NA_character_)
    }
    if (!is.na(preferred) && preferred %in% names(data)) {
      return(preferred)
    }
  }

  .pick_first_column(data, candidates)
}

.normalize_master_text <- function(x) {
  out <- as.character(x)
  out <- trimws(out)
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out <- tolower(out)
  out <- gsub("['`’]", "", out, perl = TRUE)
  out <- gsub("[^a-z0-9]+", " ", out, perl = TRUE)
  trimws(out)
}

.coerce_turnout_binary <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }

  raw <- as.character(x)
  norm <- .normalize_master_text(raw)
  out <- rep(NA_real_, length(norm))

  parsed_num <- suppressWarnings(as.numeric(raw))
  finite_num <- is.finite(parsed_num)
  if (any(finite_num)) {
    uniq <- unique(parsed_num[finite_num])
    if (all(uniq %in% c(0, 1))) {
      out[finite_num] <- parsed_num[finite_num]
    }
  }

  no_hit <- grepl(
    "^(0|2|non|no)$|\\bnon\\b|\\bno\\b|n[' ]?a pas vot|did not vot|didn[' ]?t vot|ne votera pas|nvp|annule|not vote|not voted|certain not to vote",
    norm,
    perl = TRUE
  )
  yes_hit <- grepl(
    "^(1|oui|yes)$|\\boui\\b|\\byes\\b|a vote|alle voter|already voted|par anticipation|le jour meme|certain to vote",
    norm,
    perl = TRUE
  )

  out[no_hit] <- 0
  out[yes_hit & !no_hit] <- 1
  out[.master_missing_vector(raw)] <- NA_real_
  out
}

.coerce_sovereignty_binary <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }

  raw <- as.character(x)
  norm <- .normalize_master_text(raw)
  out <- rep(NA_real_, length(norm))

  parsed_num <- suppressWarnings(as.numeric(raw))
  finite_num <- is.finite(parsed_num)
  if (any(finite_num)) {
    uniq <- unique(parsed_num[finite_num])
    if (all(uniq %in% c(0, 1))) {
      out[finite_num] <- parsed_num[finite_num]
    }
  }

  no_hit <- grepl(
    "^(0|2|non|no)$|\\bnon\\b|\\bno\\b|federalist|federaliste|signer la constitution|no change",
    norm,
    perl = TRUE
  )
  yes_hit <- grepl(
    "^(1|oui|yes)$|\\boui\\b|\\byes\\b|independ|souverain",
    norm,
    perl = TRUE
  )

  out[no_hit] <- 0
  out[yes_hit & !no_hit] <- 1
  out[.master_missing_vector(raw)] <- NA_real_
  out
}

.master_missing_vector <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.character(x)) {
    out <- is.na(x) | !nzchar(trimws(x))
    lowered <- tolower(trimws(x))
    out <- out | lowered %in% c("na", "n/a", "nan", "null")
    return(out)
  }

  is.na(x)
}

.parse_reference_year <- function(x) {
  if (is.na(x)) {
    return(NA_real_)
  }

  txt <- as.character(x)
  hits <- gregexpr("[0-9]{4}", txt, perl = TRUE)[[1]]
  if (identical(hits[1], -1L)) {
    return(NA_real_)
  }

  lens <- attr(hits, "match.length")
  start <- hits[length(hits)]
  width <- lens[length(lens)]
  suppressWarnings(as.numeric(substr(txt, start, start + width - 1L)))
}

.derive_age_numeric <- function(age, yob, ref_year) {
  age_num <- suppressWarnings(as.numeric(age))
  age_num[!is.finite(age_num)] <- NA_real_
  age_num[age_num < 13 | age_num > 120] <- NA_real_
  yob_num <- suppressWarnings(as.numeric(yob))

  calc <- ref_year - yob_num
  calc[!is.finite(calc)] <- NA_real_
  calc[calc < 0 | calc > 120] <- NA_real_

  fill <- is.na(age_num) & !is.na(calc)
  age_num[fill] <- calc[fill]
  age_num
}

.age_to_group <- function(age_num) {
  out <- rep(NA_character_, length(age_num))

  out[!is.na(age_num) & age_num < 18] <- "<18"
  out[!is.na(age_num) & age_num >= 18 & age_num <= 24] <- "18-24"
  out[!is.na(age_num) & age_num >= 25 & age_num <= 34] <- "25-34"
  out[!is.na(age_num) & age_num >= 35 & age_num <= 44] <- "35-44"
  out[!is.na(age_num) & age_num >= 45 & age_num <= 54] <- "45-54"
  out[!is.na(age_num) & age_num >= 55 & age_num <= 64] <- "55-64"
  out[!is.na(age_num) & age_num >= 65] <- "65+"

  out
}

.postprocess_master_dataset <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L) {
    return(master)
  }

  if (!("year_of_birth" %in% names(master))) {
    master$year_of_birth <- NA_real_
  }
  if (!("age" %in% names(master))) {
    master$age <- NA_real_
  }
  if (!("age_group" %in% names(master))) {
    master$age_group <- NA_character_
  }

  yob_num <- suppressWarnings(as.numeric(master$year_of_birth))
  valid_yob <- !is.na(yob_num) & yob_num >= 1850 & yob_num <= 2100
  yob_num[!valid_yob] <- NA_real_
  master$year_of_birth <- yob_num

  ref_year <- vapply(master$qes_year, .parse_reference_year, numeric(1))
  age_num <- .derive_age_numeric(master$age, master$year_of_birth, ref_year)
  master$age <- age_num

  existing_age_group <- as.character(master$age_group)
  existing_age_group[.master_missing_vector(existing_age_group)] <- NA_character_
  derived_age_group <- .age_to_group(age_num)

  # Prefer unified age groups derived from numeric age; fall back to existing text.
  final_age_group <- derived_age_group
  use_existing <- is.na(final_age_group) & !is.na(existing_age_group)
  final_age_group[use_existing] <- existing_age_group[use_existing]
  master$age_group <- final_age_group

  if ("survey_weight" %in% names(master)) {
    weight_num <- suppressWarnings(as.numeric(master$survey_weight))
    weight_missing <- .master_missing_vector(master$survey_weight)
    converted <- !is.na(weight_num) & !weight_missing
    if (sum(converted) >= 0.8 * max(1L, sum(!weight_missing))) {
      master$survey_weight <- weight_num
    }
  }

  if ("turnout" %in% names(master)) {
    master$turnout <- .coerce_turnout_binary(master$turnout)
  }

  if ("sovereignty_support" %in% names(master)) {
    master$sovereignty_support <- .coerce_sovereignty_binary(master$sovereignty_support)
  }

  master
}

.remove_master_study_duplicates <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L || !all(c("qes_code", "respondent_id") %in% names(master))) {
    return(list(data = master, removed = 0L))
  }

  ids <- trimws(as.character(master$respondent_id))
  codes <- as.character(master$qes_code)
  valid_id <- !is.na(ids) & nzchar(ids)
  valid_code <- !is.na(codes) & nzchar(codes)
  valid <- valid_id & valid_code

  key <- paste0(codes, "||", tolower(ids))
  keep <- !valid | !duplicated(key)

  list(
    data = master[keep, , drop = FALSE],
    removed = as.integer(sum(!keep))
  )
}

.remove_empty_master_rows <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L) {
    return(list(data = master, removed = 0L))
  }

  harm_cols <- intersect(names(.master_harmonization_lookup()), names(master))
  harm_cols <- setdiff(harm_cols, "respondent_id")
  if (length(harm_cols) == 0L) {
    return(list(data = master, removed = 0L))
  }

  missing_mat <- sapply(harm_cols, function(col) .master_missing_vector(master[[col]]))
  if (is.null(dim(missing_mat))) {
    missing_mat <- matrix(missing_mat, ncol = 1L)
  }

  all_missing <- rowSums(missing_mat) == ncol(missing_mat)
  keep <- !all_missing

  list(
    data = master[keep, , drop = FALSE],
    removed = as.integer(sum(all_missing))
  )
}

.coerce_master_value <- function(x, target) {
  numeric_targets <- c("year_of_birth", "age", "survey_weight")

  if (target %in% numeric_targets) {
    if (is.factor(x)) {
      x <- as.character(x)
    }

    numeric_x <- suppressWarnings(as.numeric(x))
    missing_numeric <- sum(is.na(numeric_x))
    missing_original <- sum(is.na(x))

    if (missing_numeric <= missing_original) {
      return(numeric_x)
    }
  }

  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    x <- haven::as_factor(x)
  }

  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.character(x))
  }

  if (is.factor(x)) {
    return(as.character(x))
  }

  x
}

.build_qes_master_study <- function(data, srvy, year = NA_character_, name_en = NA_character_) {
  lookup <- .master_harmonization_lookup()
  n <- nrow(data)

  out <- data.frame(
    qes_code = rep(srvy, n),
    qes_year = rep(year, n),
    qes_name_en = rep(name_en, n),
    stringsAsFactors = FALSE
  )

  source_map <- data.frame(
    qes_code = srvy,
    qes_year = year,
    qes_name_en = name_en,
    harmonized_variable = names(lookup),
    source_variable = NA_character_,
    stringsAsFactors = FALSE
  )

  for (target in names(lookup)) {
    source_col <- .resolve_master_source_column(data, srvy = srvy, target = target, candidates = lookup[[target]])
    source_map$source_variable[source_map$harmonized_variable == target] <- source_col

    if (is.na(source_col)) {
      out[[target]] <- rep(NA, n)
    } else {
      out[[target]] <- .coerce_master_value(data[[source_col]], target = target)
    }
  }

  # Some legacy files expose questionnaire identifiers (e.g., QUEST = 0) rather
  # than respondent IDs. In those cases, generate stable row IDs to avoid
  # collapsing almost all rows during within-study de-duplication.
  rid <- trimws(as.character(out$respondent_id))
  valid_rid <- !is.na(rid) & nzchar(rid)
  uniq_ratio <- if (sum(valid_rid) > 0L) {
    length(unique(tolower(rid[valid_rid]))) / sum(valid_rid)
  } else {
    0
  }

  if (sum(valid_rid) == 0L || uniq_ratio < 0.5) {
    out$respondent_id <- sprintf("%s_%s", srvy, seq_len(n))
    source_map$source_variable[source_map$harmonized_variable == "respondent_id"] <- "(synthetic_rowid)"
  }

  list(data = out, source_map = source_map)
}

.validate_master_surveys <- function(surveys) {
  if (is.null(surveys)) {
    return(.qes_catalog$qes_survey_code)
  }

  if (!is.character(surveys) || length(surveys) == 0L) {
    stop("`surveys` must be a non-empty character vector.", call. = FALSE)
  }

  surveys <- unique(surveys)
  unknown <- setdiff(surveys, .qes_catalog$qes_survey_code)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown survey code(s): %s. Use get_qescodes() to see valid codes.",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  surveys
}

#' Build a harmonized stacked master QES dataset
#'
#' Downloads multiple Quebec Election Study datasets, harmonizes a shared set of
#' variables, and stacks all rows into one master data frame.
#'
#' @param surveys Character vector of qesR survey codes. Defaults to all
#'   available studies in [get_qescodes()].
#' @param assign_global If `TRUE`, assign the result to `.GlobalEnv` using
#'   `object_name`.
#' @param object_name Name used when `assign_global = TRUE`. Defaults to
#'   `"qes_master"`.
#' @param quiet If `TRUE`, suppress informational output while downloading.
#' @param strict If `TRUE`, stop when any study fails to download. If `FALSE`,
#'   return partial results and record failures in attributes.
#' @param save_path Optional file path to persist the master dataset. If the
#'   path ends with `.rds`, it is saved with [saveRDS()]. Otherwise, a CSV is
#'   written with [utils::write.csv()].
#'
#' @return A harmonized stacked data frame. Attributes include:
#'   `source_map`, `loaded_surveys`, `failed_surveys`,
#'   `duplicates_removed`, and `empty_rows_removed`.
#' @export
get_qes_master <- function(
  surveys = NULL,
  assign_global = TRUE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
) {
  surveys <- .validate_master_surveys(surveys)
  .assert_single_string(object_name, "object_name")
  if (!is.null(save_path)) {
    .assert_single_string(save_path, "save_path")
  }

  stacked <- list()
  source_maps <- list()
  failed <- character(0)

  for (srvy in surveys) {
    study <- .qes_catalog[.qes_catalog$qes_survey_code == srvy, , drop = FALSE]

    dat <- tryCatch(
      get_qes(
        srvy = srvy,
        assign_global = FALSE,
        with_codebook = FALSE,
        quiet = quiet
      ),
      error = function(e) e
    )

    if (inherits(dat, "error")) {
      failed <- c(failed, sprintf("%s: %s", srvy, conditionMessage(dat)))
      if (!quiet) {
        message(sprintf("Skipping '%s' due to download/read error.", srvy))
      }
      next
    }

    built <- .build_qes_master_study(
      data = dat,
      srvy = srvy,
      year = study$year,
      name_en = study$name_en
    )

    stacked[[srvy]] <- built$data
    source_maps[[srvy]] <- built$source_map

    if (!quiet) {
      message(sprintf("[%s] rows loaded: %s", srvy, nrow(dat)))
    }
  }

  if (length(stacked) == 0L) {
    stop("No studies could be loaded. Check network access and survey availability.", call. = FALSE)
  }

  if (isTRUE(strict) && length(failed) > 0L) {
    stop(
      sprintf("Master build failed for %s study(ies): %s", length(failed), paste(failed, collapse = " | ")),
      call. = FALSE
    )
  }

  master <- do.call(rbind, stacked)
  rownames(master) <- NULL
  master <- .postprocess_master_dataset(master)

  dedup <- .remove_master_study_duplicates(master)
  master <- dedup$data

  no_empty <- .remove_empty_master_rows(master)
  master <- no_empty$data

  source_map <- do.call(rbind, source_maps)
  rownames(source_map) <- NULL

  attr(master, "source_map") <- source_map
  attr(master, "loaded_surveys") <- names(stacked)
  attr(master, "failed_surveys") <- failed
  attr(master, "duplicates_removed") <- dedup$removed
  attr(master, "empty_rows_removed") <- no_empty$removed
  attr(master, "harmonized_variables") <- names(.master_harmonization_lookup())
  attr(master, "saved_to") <- NULL

  if (isTRUE(assign_global)) {
    assign(object_name, master, envir = .GlobalEnv)
  }

  if (!is.null(save_path)) {
    out_dir <- dirname(save_path)
    if (!dir.exists(out_dir)) {
      stop(sprintf("Directory does not exist: '%s'.", out_dir), call. = FALSE)
    }

    ext <- tolower(tools::file_ext(save_path))
    if (ext == "rds") {
      saveRDS(master, save_path)
    } else {
      utils::write.csv(master, save_path, row.names = FALSE, na = "")
    }
    attr(master, "saved_to") <- save_path
  }

  if (!quiet) {
    message(sprintf("Master dataset rows: %s", nrow(master)))
    message(sprintf("Master dataset studies loaded: %s", length(stacked)))
    if (length(failed) > 0L) {
      message(sprintf("Master dataset studies skipped: %s", length(failed)))
    }
    if (dedup$removed > 0L) {
      message(sprintf("Master dataset within-study duplicate respondents removed: %s", dedup$removed))
    }
    if (no_empty$removed > 0L) {
      message(sprintf("Master dataset all-empty rows removed: %s", no_empty$removed))
    }
    if (!is.null(save_path)) {
      message(sprintf("Master dataset saved to: %s", save_path))
    }
  }

  master
}
