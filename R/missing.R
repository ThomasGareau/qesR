# Missing codes in raw data (design.md sections 2.2 and 5.2, slice S3).

# Types left alone unless asked for: they are answers ("spoiled" ballot,
# option "not selected", "did not vote", "not registered"), not missing
# answers in most analyses.
.qes_missing_kept <- c("spoiled", "not_selected", "not_voted", "not_registered")

.qes_missing_types <- function() {
  rows <- .qes_na_reasons()
  rows$value[grepl("dictionary", rows$scope)]
}

#' Set "don't know", "refused" and other missing codes to NA
#'
#' In the data [get_qes()] returns, the codes that stand for a missing answer
#' ("don't know", "refused", SPSS user-missing codes such as 8 or 9, `-99` in
#' `qes2022`) are kept as values, as the files deposit them.
#' `qes_missing()` turns them into `NA`, or into tagged `NA` values that
#' remember the reason, using the missing type the codebook records for each
#' code (see [qes_codebook()], column `missing_codes`).
#'
#' Missing types, and their tag for `action = "tagged"`: `dk` (d),
#' `refused` (r), `dk_refused` (b, one code for both), `no_answer` (o),
#' `not_selected` (s, an option not ticked in a multiple-choice question),
#' `inapplicable` (i), `not_voted` (v), `spoiled` (p), `ineligible` (e),
#' `not_registered` (g), `not_in_wave` (w), `not_mappable` (m) and `user_na`
#' (u, a code the SPSS file declares missing without saying why). A code
#' has one type. The codes of each study were typed from their labels, by
#' hand; a variable whose codebook types none of its codes, and whose file
#' declares none, is left unchanged, and a message counts those variables.
#' Some files declare missing a code that is an answer ("Un autre parti",
#' or "ne voterait pas/annulerait" in a voting-intention item): such a code
#' has no missing type and is never changed. A declared code for "did not
#' vote" or a spoiled ballot has the type `not_voted` or `spoiled`, so the
#' default leaves it alone.
#'
#' `qes_missing()` uses the codebook [get_qes()] attached to `x`, else the
#' study's metadata shipped with qesR. It never downloads anything.
#'
#' @section En français:
#' `qes_missing()` remplace par `NA` les codes qui représentent une
#' non-réponse (« ne sait pas », « refus », codes manquants déclarés dans le
#' fichier SPSS, `-99` dans `qes2022`), selon le type que le codebook
#' attribue à chaque code. Avec `action = "tagged"`, les colonnes numériques
#' reçoivent des `NA` étiquetés ([haven::tagged_na()]) qui conservent la
#' raison. Par défaut, les types `spoiled`, `not_selected`, `not_voted` et
#' `not_registered`, qui sont des réponses, ne sont pas touchés. Un code
#' déclaré manquant dans le fichier mais qui est une réponse (« Un autre
#' parti », « ne voterait pas/annulerait » dans une intention de vote) n'a
#' pas de type et n'est jamais modifié. La fonction ne télécharge rien.
#'
#' @param x A data frame returned by [get_qes()] (its study is read from its
#'   attributes; a data frame that has lost them is an error of class
#'   `qesR_error_no_provenance`).
#' @param variables Optional character vector of column names (exact match)
#'   to recode; `NULL` recodes every column.
#' @param action `"na"` (default) sets the codes to `NA`; `"tagged"` sets
#'   them, in numeric columns, to [haven::tagged_na()] values whose tag is
#'   the letter of their type (other columns get a plain `NA`).
#' @param types Optional character vector of missing types to recode.
#'   `NULL` (default) means every type except `spoiled`, `not_selected`,
#'   `not_voted` and `not_registered`.
#' @param quiet If TRUE, suppress the message that counts the variables
#'   with no typed codes.
#'
#' @return `x`, returned visibly, with the codes recoded and the attribute
#'   `qes_missing_log`: a data frame with one row per recoded code and
#'   variable (`variable`, `value`, `missing_type`, `n_set`, the number of
#'   values set to `NA`). Labels and other attributes are kept.
#'
#' @family codebooks and search
#' @seealso [qes_codebook()] for the codes of each variable.
#' @examples
#' demo <- get_qes("qes_demo", quiet = TRUE)
#' table(demo$Q19, useNA = "ifany")
#' clean <- qes_missing(demo, variables = c("Q19", "Q28"))
#' table(clean$Q19, useNA = "ifany")
#' attr(clean, "qes_missing_log")
#'
#' # keep the reason: "don't know" and "refused" become different NAs
#' tagged <- qes_missing(demo, variables = "Q19", action = "tagged")
#' table(haven::na_tag(tagged$Q19), useNA = "ifany")
#' @export
qes_missing <- function(x, variables = NULL, action = c("na", "tagged"), types = NULL, quiet = FALSE) {
  if (!is.data.frame(x)) {
    .qes_abort(
      "input_missing_data",
      class = "qesR_error_input",
      args = list("x"),
      data = list(arg = "x", value = class(x))
    )
  }
  action <- .qes_check_one(action, "action", c("na", "tagged"))
  .qes_check_flag(quiet, "quiet")
  allowed <- .qes_missing_types()
  if (is.null(types)) {
    types <- setdiff(allowed, .qes_missing_kept)
  } else {
    .qes_check_choice(types, "types", allowed)
  }
  study <- .qes_data_study(x)
  if (is.null(study)) {
    .qes_abort(
      "no_provenance",
      class = "qesR_error_no_provenance",
      args = list("x"),
      data = list(arg = "x")
    )
  }
  vars <- if (is.null(variables)) names(x) else .qes_match_variables(variables, names(x), study = study)

  # the codebook get_qes() attached, else the study's metadata; never a
  # download (the labels needed are on the columns)
  cb_dict <- NULL
  cb <- attr(x, "qes_codebook", exact = TRUE)
  if (inherits(cb, "qes_codebook") && is.list(attr(cb, "qes_dict", exact = TRUE)) &&
      identical(attr(cb, "survey_code", exact = TRUE), study)) {
    cb_dict <- tryCatch(.qes_codebook_source(cb)$dict, error = function(e) NULL)
  }
  dict <- .qes_dict_for_data(x[, vars, drop = FALSE], study, dict = cb_dict, quiet = quiet)
  values <- dict$values
  tags <- .qes_na_reasons()
  tags <- stats::setNames(tags$code, tags$value)

  log <- list()
  untyped <- 0L
  for (v in vars) {
    col <- x[[v]]
    rows <- values[values$variable == v & !is.na(values$missing_type), c("value", "missing_type"), drop = FALSE]
    declared <- .qes_na_string(col)
    if (!is.na(declared)) {
      # a declared code the dictionary marks as an answer stays
      extra <- values$value[values$variable == v & is.na(values$missing_type) &
        !(values$label_flag %in% "declared_substantive")]
      extra <- extra[.qes_is_declared(extra, declared)]
      if (length(extra) > 0L) {
        rows <- rbind(rows, data.frame(value = extra, missing_type = "user_na", stringsAsFactors = FALSE))
      }
    }
    if (nrow(rows) == 0L) {
      untyped <- untyped + 1L
      next
    }
    rows <- rows[rows$missing_type %in% types, , drop = FALSE]
    if (nrow(rows) == 0L) {
      next
    }
    base <- .qes_plain(col)
    key <- if (is.numeric(base)) .qes_code_chr(base) else as.character(base)
    n_set <- integer(nrow(rows))
    for (k in seq_len(nrow(rows))) {
      hit <- which(key == rows$value[k])
      n_set[k] <- length(hit)
      if (length(hit) == 0L) {
        next
      }
      if (identical(action, "tagged") && is.double(base)) {
        base[hit] <- haven::tagged_na(tags[[rows$missing_type[k]]])
      } else {
        base[hit] <- NA
      }
    }
    attributes(base) <- attributes(col)
    x[[v]] <- base
    log[[v]] <- data.frame(
      variable = v, value = rows$value, missing_type = rows$missing_type, n_set = n_set,
      stringsAsFactors = FALSE
    )
  }
  out_log <- do.call(rbind, log)
  if (is.null(out_log)) {
    out_log <- data.frame(variable = character(0), value = character(0),
      missing_type = character(0), n_set = integer(0), stringsAsFactors = FALSE)
  }
  rownames(out_log) <- NULL
  if (untyped > 0L) {
    .qes_inform(
      "missing_untyped",
      class = "qesR_message_missing_untyped",
      args = list(untyped, length(vars)),
      data = list(study = study, n = untyped),
      quiet = quiet
    )
  }
  attr(x, "qes_missing_log") <- out_log
  x
}
