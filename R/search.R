# Bilingual search (design.md section 6.3, slice S3).
#
# qes_search() looks through variable names, variable labels, question text
# (English and French) and value labels of every study whose metadata is at
# hand: the shipped dictionary, plus any qes2022 shard already in the cache.
# Text is folded with .qes_fold() (accents, then case), which uses a table
# of code points and so gives the same answer in any locale.

.qes_search_fields <- c("variable", "label", "question", "values", "target")

# The folded text of one study's variables, kept for the session.
.qes_search_index <- function(study, dict) {
  key <- paste0("index:", study, ":", nrow(dict$variables), ":", nrow(dict$values))
  if (!is.null(.qes_dict_cache[[key]])) {
    return(.qes_dict_cache[[key]])
  }
  v <- dict$variables
  lab <- .qes_labelled_rows(dict$values)
  text_en <- ifelse(!is.na(lab$label_en), lab$label_en, ifelse(lab$label_lang %in% "en", lab$label, NA_character_))
  text_fr <- ifelse(!is.na(lab$label_fr), lab$label_fr, ifelse(lab$label_lang %in% "fr", lab$label, NA_character_))
  collapse <- function(text) {
    vapply(v$variable, function(x) {
      t <- text[lab$variable == x]
      t <- t[!is.na(t)]
      if (length(t) == 0L) NA_character_ else paste(t, collapse = " | ")
    }, character(1), USE.NAMES = FALSE)
  }
  values_text <- vapply(v$variable, function(x) {
    rows <- lab[lab$variable == x, , drop = FALSE]
    .qes_join_codes(rows$value, rows$label)
  }, character(1), USE.NAMES = FALSE)
  index <- list(
    variables = v,
    values_text = values_text,
    f_variable = .qes_fold(v$variable),
    f_label = .qes_fold(v$label),
    f_question_en = .qes_fold(v$question_en),
    f_question_fr = .qes_fold(v$question_fr),
    f_values_en = .qes_fold(collapse(text_en)),
    f_values_fr = .qes_fold(collapse(text_fr))
  )
  .qes_dict_cache[[key]] <- index
  index
}

#' Search variables across studies, in English and French
#'
#' `qes_search()` finds the variables whose name, label, question text or
#' value labels contain `pattern`, in every study whose description is at
#' hand, with no network request. The search ignores case and accents:
#' `"souverain"` finds "Souveraineté", `"quebec"` finds "Québec". Several
#' terms separated by `|` find any of them (`"souverain|sovereign"`).
#'
#' The studies released under CC0 are always searchable: their description
#' ships with qesR. `qes2022` (CC BY-NC 4.0) becomes searchable once its
#' codebook has been built from your own copy of the data, by
#' [qes_codebook()] or [get_qes()]; until then the printed result names it
#' as not searchable. Question text is known for the variables whose
#' questionnaire was matched to the data (see the `coverage` attribute).
#'
#' @section En français:
#' `qes_search()` trouve les variables dont le nom, l'étiquette, le texte de
#' la question ou les étiquettes de valeurs contiennent `pattern`, sans
#' requête réseau, en ignorant la casse et les accents. Par défaut
#' (`lang = "both"`), la recherche porte sur le français et l'anglais ;
#' `lang = "fr"` la limite aux textes français. Plusieurs termes séparés par
#' `|` trouvent l'un ou l'autre.
#'
#' @param pattern A single string. Unless `regex = TRUE` it is matched as
#'   plain text; `|` separates alternative terms.
#' @param studies Optional character vector of study codes (see
#'   [qes_studies()]); `NULL` or `"all"` searches every study.
#' @param fields Which fields to search: any of `"variable"` (names),
#'   `"label"` (variable labels), `"question"` (question text),
#'   `"values"` (value labels) and `"target"` (harmonized targets, not
#'   available yet).
#' @param regex If TRUE, `pattern` is a regular expression (Perl syntax),
#'   matched ignoring case and accents.
#' @param lang `"both"` (default) searches English and French text; `"en"`
#'   or `"fr"` only the text in that language (variable names are always
#'   searched). It also chooses the language of the `question` column.
#'
#' @return A data frame of class `qes_search`, one row per matching variable:
#'   `study`, `year`, `variable`, `label`, `question`, `question_lang`,
#'   `values` (`"1=Oui | 2=Non"`), `targets` (`NA` for now) and `matched_in`
#'   (the fields that matched, separated by `;`). Attributes:
#'   `not_searchable` (the requested studies whose description is not at
#'   hand) and `coverage` (per study: `n_variables`, `n_label`,
#'   `n_question` and `n_reviewed`, the variables whose wording was checked
#'   by hand against the questionnaire).
#'
#' @family codebooks and search
#' @seealso [qes_codebook()] and [qes_question()] for the variables found.
#' @examples
#' hits <- qes_search("souverain|sovereign")
#' head(hits[, c("study", "variable", "question")])
#'
#' # French question text only, in two studies; accents are optional
#' # ("interet" also finds the accented spelling)
#' qes_search("interet pour la politique", studies = c("qes2014", "qes2018"),
#'            fields = "question", lang = "fr")
#' @export
qes_search <- function(pattern, studies = NULL,
                       fields = c("variable", "label", "question", "values", "target"),
                       regex = FALSE, lang = c("both", "en", "fr")) {
  .assert_single_string(pattern, "pattern")
  pattern <- .qes_as_utf8(pattern)
  .qes_check_flag(regex, "regex")
  lang <- .qes_check_one(lang, "lang", c("both", "en", "fr"))
  if (!is.character(fields) || length(fields) == 0L || anyNA(fields) || !all(fields %in% .qes_search_fields)) {
    .qes_abort(
      "input_choice",
      class = "qesR_error_input",
      args = list("fields", .qes_q(.qes_search_fields)),
      data = list(arg = "fields", value = fields)
    )
  }
  codes <- if (is.null(studies)) .qes_study_codes() else .qes_resolve_codes(studies, "studies", demo = TRUE)

  if (isTRUE(regex)) {
    .qes_assert_regex(pattern, "pattern")
    pat <- .qes_unaccent(pattern)
    hit_fn <- function(text) !is.na(text) & grepl(pat, text, ignore.case = TRUE, perl = TRUE)
  } else {
    terms <- .qes_fold(strsplit(pattern, "|", fixed = TRUE)[[1]])
    terms <- unique(terms[!is.na(terms) & nzchar(terms)])
    if (length(terms) == 0L) {
      .qes_abort("input_string", class = "qesR_error_input", args = list("pattern"), data = list(arg = "pattern", value = pattern))
    }
    hit_fn <- function(text) {
      out <- rep(FALSE, length(text))
      for (t in terms) {
        out <- out | (!is.na(text) & grepl(t, text, fixed = TRUE))
      }
      out
    }
  }

  shards <- .qes_cached_shards()
  catalog <- .qes_catalog(demo = TRUE)$studies
  rows <- list()
  coverage <- list()
  not_searchable <- character(0)
  for (study in codes) {
    row <- catalog[match(study, catalog$study), , drop = FALSE]
    dict <- NULL
    if (isTRUE(row$metadata_shipped)) {
      shipped <- .qes_dict_shipped(demo = isTRUE(row$demo))
      if (study %in% shipped$variables$study) {
        dict <- .qes_dict_subset(shipped, study)
      }
    } else if (!is.null(shards[[study]])) {
      dict <- shards[[study]]
    }
    if (is.null(dict)) {
      not_searchable <- c(not_searchable, study)
      next
    }
    ix <- .qes_search_index(study, dict)
    v <- ix$variables
    coverage[[study]] <- data.frame(
      study = study,
      n_variables = nrow(v),
      n_label = sum(!is.na(v$label)),
      n_question = sum(!is.na(v$question_en) | !is.na(v$question_fr)),
      n_reviewed = sum(v$reviewed %in% TRUE),
      stringsAsFactors = FALSE
    )
    label_lang <- row$source_lang
    use_label <- identical(lang, "both") || identical(lang, label_lang)
    m_variable <- if ("variable" %in% fields) hit_fn(ix$f_variable) else rep(FALSE, nrow(v))
    m_label <- if ("label" %in% fields && use_label) hit_fn(ix$f_label) else rep(FALSE, nrow(v))
    m_q_en <- if ("question" %in% fields && lang != "fr") hit_fn(ix$f_question_en) else rep(FALSE, nrow(v))
    m_q_fr <- if ("question" %in% fields && lang != "en") hit_fn(ix$f_question_fr) else rep(FALSE, nrow(v))
    m_val <- rep(FALSE, nrow(v))
    if ("values" %in% fields) {
      if (lang != "fr") m_val <- m_val | hit_fn(ix$f_values_en)
      if (lang != "en") m_val <- m_val | hit_fn(ix$f_values_fr)
    }
    any_hit <- m_variable | m_label | m_q_en | m_q_fr | m_val
    if (!any(any_hit)) {
      next
    }
    i <- which(any_hit)
    pick_lang <- if (identical(lang, "both")) NULL else lang
    q <- .qes_pick_question(v[i, , drop = FALSE], pick_lang, label_lang)
    # a match in one language's question shows that language
    only_en <- m_q_en[i] & !m_q_fr[i]
    only_fr <- m_q_fr[i] & !m_q_en[i]
    q$text[only_en] <- v$question_en[i][only_en]
    q$lang[only_en] <- "en"
    q$text[only_fr] <- v$question_fr[i][only_fr]
    q$lang[only_fr] <- "fr"
    matched <- vapply(seq_along(i), function(k) {
      j <- i[k]
      paste(c("variable", "label", "question", "values")[c(
        m_variable[j], m_label[j], m_q_en[j] || m_q_fr[j], m_val[j]
      )], collapse = ";")
    }, character(1))
    rows[[study]] <- data.frame(
      study = study,
      year = row$year,
      variable = v$variable[i],
      label = v$label[i],
      question = q$text,
      question_lang = q$lang,
      values = ix$values_text[i],
      targets = NA_character_,
      matched_in = matched,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  if (is.null(out)) {
    out <- data.frame(
      study = character(0), year = integer(0), variable = character(0),
      label = character(0), question = character(0), question_lang = character(0),
      values = character(0), targets = character(0), matched_in = character(0),
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  cov <- do.call(rbind, coverage)
  if (is.null(cov)) {
    cov <- data.frame(study = character(0), n_variables = integer(0), n_label = integer(0),
      n_question = integer(0), n_reviewed = integer(0), stringsAsFactors = FALSE)
  }
  rownames(cov) <- NULL
  attr(out, "not_searchable") <- not_searchable
  attr(out, "coverage") <- cov
  class(out) <- c("qes_search", "data.frame")
  out
}

#' @export
print.qes_search <- function(x, n = 20L, ...) {
  df <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  attr(df, "not_searchable") <- NULL
  attr(df, "coverage") <- NULL
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 0L) {
    n <- 20L
  }
  if (nrow(df) == 0L) {
    cat(.qes_msg("search_none"), "\n", sep = "")
  } else {
    print(.qes_preview(utils::head(df, n)), ...)
    if (nrow(df) > n) {
      cat(.qes_msg("search_more", list(nrow(df) - n)), "\n", sep = "")
    }
  }
  missing <- attr(x, "not_searchable", exact = TRUE)
  if (length(missing) > 0L) {
    cat(.qes_msg("search_not_searchable", list(.qes_q(missing))), "\n", sep = "")
  }
  invisible(x)
}
