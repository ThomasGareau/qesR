# Question wording (design.md sections 2.2 and 6.2, slice S3): qes_question()
# and the legacy get_question().

#' The exact wording of survey questions
#'
#' `qes_question()` returns the question asked for each of the variables you
#' name, as the study's questionnaire gives it, with where it comes from.
#' Variable names must match exactly; there is no partial or fuzzy matching.
#'
#' The wording comes from the metadata of [qes_codebook()]: offline for the
#' studies shipped with qesR, from your cached copy of the data file for
#' `qes2022`. `qes2022`'s question text is its data file's variable label,
#' which Stata cuts at 80 characters: such rows have `truncated = TRUE` and
#' `doc_ref` names the codebook document that has the full wording (see
#' [qes_docs()]). qesR never guesses the end of a cut question.
#'
#' @section En français:
#' `qes_question()` renvoie le libellé exact de la question posée pour
#' chaque variable nommée, tel que le donne le questionnaire de l'étude,
#' avec sa source. Les noms de variables doivent correspondre exactement.
#' `lang = "fr"` donne le libellé français, `lang = "en"` l'anglais ; par
#' défaut, la langue de l'étude. Un libellé coupé à 80 caractères dans le
#' fichier (`qes2022`) est signalé par `truncated = TRUE`. `doc_ref` donne
#' une ou plusieurs références `<file_id>:<question>` séparées par `;` (par
#' exemple `"352010:Q19;352009:Q19"`, questionnaires anglais et français).
#'
#' @param x A study code from [qes_studies()], or a data frame returned by
#'   [get_qes()] (its study is read from its attributes).
#' @param variables Character vector of variable names (exact match). An
#'   unknown name is an error of class `qesR_error_unknown_variable` that
#'   suggests near matches.
#' @param lang `NULL` (default): the study's own language, or the other
#'   language when only that one is known; `"en"` or `"fr"`: that language
#'   only (`NA` where it is unknown). There is no machine translation.
#'
#' @return A data frame with one row per variable: `study`, `variable`,
#'   `question` (`NA` when unknown), `question_lang`, `truncated` (`TRUE`
#'   when the source cut the text), `source` (`"questionnaire"`, or `"file"`
#'   when the text is the data file's own label), `doc_ref` (the documents
#'   that give the text: one or more `<file_id>:<item>` references, the
#'   Dataverse file id and the question number, separated by `;`, e.g.
#'   `"352010:Q19;352009:Q19"` for the English and French questionnaires; a
#'   bare file id when the item is not known) and `universe` (who was asked,
#'   when the questionnaire says so).
#'
#' @family codebooks and search
#' @seealso [qes_codebook()] for every variable of a study, [qes_search()] to
#'   find variables by topic.
#' @examples
#' qes_question("qes2014", c("Q2", "Q19"))
#' qes_question("qes2014", "Q19", lang = "en")
#' @export
qes_question <- function(x, variables, lang = NULL) {
  lang <- .qes_check_lang(lang)
  if (is.data.frame(x)) {
    cb <- .qes_codebook_impl(x, quiet = TRUE, fn = "qes_question")
    src <- .qes_codebook_source(cb)
    dict <- src$dict
    study <- src$attrs$survey_code %||% NA_character_
  } else {
    .assert_single_string(x, "x")
    study <- .qes_resolve_codes(x, "x", demo = TRUE)
    if (length(study) != 1L) {
      .qes_abort("input_string", class = "qesR_error_input", args = list("x"), data = list(arg = "x", value = x))
    }
    dict <- .qes_dict_study(study)
  }
  vars <- .qes_match_variables(variables, dict$variables$variable, study = study,
    scope = if (is.data.frame(x)) "data" else "study")
  v <- dict$variables[match(vars, dict$variables$variable), , drop = FALSE]
  source_lang <- if (!is.na(study)) .qes_study_row(study, demo = TRUE)$source_lang else NA_character_
  q <- .qes_pick_question(v, lang, source_lang)
  universe <- ifelse(q$lang %in% "en", v$universe_en, ifelse(q$lang %in% "fr", v$universe_fr, NA_character_))
  out <- data.frame(
    study = v$study,
    variable = v$variable,
    question = q$text,
    question_lang = q$lang,
    truncated = ifelse(is.na(q$text), NA, v$question_truncated %in% TRUE),
    source = ifelse(is.na(q$text), NA_character_, v$question_source),
    doc_ref = ifelse(is.na(q$text), NA_character_, v$doc_ref),
    universe = universe,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

#' Get survey question text (legacy)
#'
#' Soft-deprecated: use [qes_question()]. `get_question()` keeps working and
#' will not be removed; it prints a one-time notice (see [qesR-deprecated]).
#'
#' `q` must name a column of the data exactly (ignoring case only when that
#' names a single column): `get_question(d, "q1")` never answers for `q10`.
#' An unknown name is an error of class `qesR_error_unknown_variable` that
#' suggests near matches. The text returned is the question of
#' [qes_question()] when the study is known, else the column's label; when
#' the source cut the question (the 80-character labels of `qes2022`), a
#' warning of class `qesR_warning_truncated` says so. `full` no longer
#' changes the result: the text is always the most complete one qesR has.
#'
#' @param do A data.frame or the name of one in the calling environment.
#' @param q Column name whose question text should be returned.
#' @param full Ignored: the full question text is always returned. Setting
#'   it to `FALSE` prints a one-time note.
#'
#' @return A character scalar with question text, or `NA_character_` (with a
#'   warning of class `qesR_warning`) when none exists.
#' @family legacy
#' @seealso [qes_question()], and [qesR-deprecated] for the legacy functions
#'   and their replacements.
#' @examples
#' demo <- get_qes("qes_demo", quiet = TRUE)
#' get_question(demo, "Q19")
#' @export
get_question <- function(do, q, full = TRUE) {
  .qes_deprecate("get_question")
  .get_question_impl(do, q, full = full, envir = parent.frame())
}

# Exact column match; the only tolerance is case, and only when it leaves a
# single column ([A:A6]: "q1" never resolves to "q10").
.resolve_question_column <- function(data, q) {
  if (q %in% names(data)) {
    return(q)
  }
  exact_ci <- names(data)[tolower(names(data)) == tolower(q)]
  if (length(exact_ci) == 1L) {
    return(exact_ci)
  }
  .qes_abort_unknown_variables(q, names(data))
}

# `envir`: the caller's frame, where a character `do` is looked up (read only,
# never inherited from enclosing frames).
.get_question_impl <- function(do, q, full = TRUE, envir) {
  .assert_single_string(q, "q")
  if (!isTRUE(full)) {
    .qes_arg_ignored("get_question", "full")
  }

  object_name <- NULL
  if (is.character(do) && length(do) == 1L) {
    object_name <- do
    if (!exists(do, envir = envir, inherits = FALSE)) {
      .qes_abort(
        "input_object_missing",
        class = "qesR_error_input",
        args = list(.qes_q(do)),
        data = list(arg = "do", value = do)
      )
    }
    data <- get(do, envir = envir, inherits = FALSE)
    if (!is.data.frame(data)) {
      .qes_abort("input_do", class = "qesR_error_input", data = list(arg = "do", value = do))
    }
  } else if (is.data.frame(do)) {
    data <- do
  } else {
    .qes_abort(
      "input_do",
      class = "qesR_error_input",
      data = list(arg = "do", value = do)
    )
  }

  q <- .resolve_question_column(data, q)

  # 1. the codebook attached to the data, or 2. the study's metadata
  row <- NULL
  cb <- attr(data, "qes_codebook", exact = TRUE)
  if (is.data.frame(cb) && "variable" %in% names(cb)) {
    src <- tryCatch(.qes_codebook_source(cb), error = function(e) NULL)
    if (!is.null(src)) {
      row <- src$dict$variables[src$dict$variables$variable == q, , drop = FALSE]
      study <- src$attrs$survey_code %||% NA_character_
    }
  }
  if (is.null(row) || nrow(row) == 0L || (is.na(row$question_en[1]) && is.na(row$question_fr[1]))) {
    study <- .qes_data_study(data)
    if (is.null(study) && !is.null(object_name) && object_name %in% .qes_study_codes()) {
      study <- object_name
    }
    if (!is.null(study)) {
      dict <- tryCatch(.qes_dict_for_data(data[, q, drop = FALSE], study), error = function(e) NULL)
      if (!is.null(dict)) {
        row <- dict$variables
      }
    }
  }
  if (!is.null(row) && nrow(row) == 1L) {
    source_lang <- if (!is.null(study) && !is.na(study) && study %in% .qes_study_codes(demo = TRUE)) {
      .qes_study_row(study, demo = TRUE)$source_lang
    } else {
      NA_character_
    }
    picked <- .qes_pick_question(row, NULL, source_lang)
    if (!is.na(picked$text)) {
      if (isTRUE(row$question_truncated)) {
        .qes_warn(
          "question_truncated",
          class = "qesR_warning_truncated",
          args = list(.qes_q(q), row$doc_ref %||% NA_character_),
          data = list(variable = q, doc_ref = row$doc_ref)
        )
      }
      return(picked$text)
    }
  }

  # 3. the variable label: the dictionary's (already cleaned), else the
  # column's own attributes, cleaned the same way as .qes_dict_build(): white
  # space squished, and a label that is empty or only repeats the variable's
  # name is no label ([A:K4]: 253 of 254 in qes2018)
  clean <- function(l) {
    if (!is.character(l) && !is.factor(l)) {
      return(NA_character_)
    }
    l <- as.character(l)
    if (length(l) < 1L || is.na(l[1])) {
      return(NA_character_)
    }
    l <- .squish_ws(l[1])
    if (!nzchar(l) || identical(tolower(l), tolower(q))) NA_character_ else l
  }
  candidates <- list(
    if (!is.null(row) && nrow(row) == 1L) row$label else NULL,
    attr(data[[q]], "qes_question", exact = TRUE),
    attr(data[[q]], "label", exact = TRUE)
  )
  variable_labels <- attr(data, "variable.labels", exact = TRUE)
  if (!is.null(variable_labels) && q %in% names(variable_labels)) {
    candidates[[length(candidates) + 1L]] <- variable_labels[[q]]
  }
  for (l in candidates) {
    l <- clean(l)
    if (!is.na(l)) {
      return(l)
    }
  }

  .qes_warn(
    "question_missing",
    args = list(.qes_q(q)),
    data = list(variable = q)
  )
  NA_character_
}
