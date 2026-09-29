# Codebooks (design.md sections 2.3 and 6.2, slice S3).
#
# A codebook is built from the dictionary (R/metadata.R), offline for every
# study of the catalog. It is a data frame of class "qes_codebook" in one of
# three layouts:
#   compact  one row per variable: the columns of qesR 0.4.4 (variable, label,
#            question, n_value_labels), then study, position, type,
#            question_lang, question_truncated, value_labels ("1=Oui | 2=Non"),
#            missing_codes ("8=dk | 9=refused"), targets (the harmonized
#            targets the variable feeds in the shipped spec, as in
#            qes_search()), label_source, question_source, doc_ref;
#   wide     the same, with value_labels as a list of named character vectors
#            (the 0.4.4 wide layout);
#   long     one row per value (and one row with value NA for a variable
#            without value labels, [A:K7]): the 0.4.4 columns variable, value,
#            value_label, label, question, then study, missing_type,
#            is_declared_na.
# Every layout keeps the attributes survey_code, doi, doi_url,
# selected_data_file, files, codebook_files and qes_provenance ([A:K1]), the
# legacy value_labels_map, and an internal qes_dict attribute (the tables it
# was built from) so it can be laid out again.

.qes_codebook_layouts <- c("compact", "wide", "long")

# Attributes of a codebook of `code` describing the data file `file_row`.
# `names_row`: the file whose variable names the codebook uses (the pinned
# file, unless the codebook was built from data read from another file).
.qes_codebook_attrs <- function(code, file_row = NULL, names_row = NULL) {
  demo <- .qes_is_demo_code(code)
  study <- .qes_legacy_view(.qes_study_row(code, demo = demo))
  pinned <- .qes_default_data_file(code, demo = demo)
  file_row <- file_row %||% pinned
  names_row <- names_row %||% pinned
  files <- .qes_catalog(demo = demo)$files
  files <- files[files$study == code, , drop = FALSE]
  server <- study$server
  url <- function(ids) {
    if (is.na(server)) {
      return(rep(NA_character_, length(ids)))
    }
    vapply(ids, function(id) .qes_url(server, "file", file_id = id), character(1), USE.NAMES = FALSE)
  }
  manifest <- data.frame(
    file_id = files$file_id,
    filename = files$file_name,
    extension = tolower(tools::file_ext(files$file_name)),
    size = files$bytes,
    download_url = url(files$file_id),
    stringsAsFactors = FALSE
  )
  docs <- .get_codebook_files_impl(code, fn = NULL)
  list(
    survey_code = code,
    doi = study$doi,
    doi_url = study$doi_url,
    selected_data_file = file_row$file_name,
    variable_names_file = names_row$file_name,
    files = manifest,
    codebook_files = docs,
    qes_provenance = .qes_provenance_codes(code)
  )
}

# ---- layouts -------------------------------------------------------------------------

.qes_labelled_rows <- function(values) {
  values[values$label_source != "none" & !is.na(values$label), , drop = FALSE]
}

.qes_value_map <- function(variables, values) {
  lab <- .qes_labelled_rows(values)
  map <- lapply(variables$variable, function(v) {
    rows <- lab[lab$variable == v, , drop = FALSE]
    stats::setNames(rows$label, rows$value)
  })
  names(map) <- variables$variable
  map
}

.qes_join_codes <- function(codes, text) {
  if (length(codes) == 0L) NA_character_ else paste(paste0(codes, "=", text), collapse = " | ")
}

.qes_codebook_frame <- function(dict, layout, lang, source_lang) {
  v <- dict$variables
  values <- dict$values
  q <- .qes_pick_question(v, lang, source_lang)
  map <- .qes_value_map(v, values)
  n_labels <- vapply(map, length, integer(1), USE.NAMES = FALSE)
  if (identical(layout, "long")) {
    declared <- .qes_is_declared_rows(values, v)
    keep <- (values$label_source != "none" & !is.na(values$label)) | !is.na(values$missing_type) | declared
    rows <- values[keep, , drop = FALSE]
    declared <- declared[keep]
    idx <- match(rows$variable, v$variable)
    out <- data.frame(
      variable = rows$variable,
      value = rows$value,
      value_label = rows$label,
      label = v$label[idx],
      question = q$text[idx],
      study = rows$study,
      missing_type = rows$missing_type,
      is_declared_na = declared,
      stringsAsFactors = FALSE
    )
    bare <- setdiff(v$variable, out$variable)
    if (length(bare) > 0L) {
      idx <- match(bare, v$variable)
      out <- rbind(out, data.frame(
        variable = bare, value = NA_character_, value_label = NA_character_,
        label = v$label[idx], question = q$text[idx], study = v$study[idx],
        missing_type = NA_character_, is_declared_na = FALSE,
        stringsAsFactors = FALSE
      ))
    }
    ord <- order(match(out$variable, v$variable), seq_len(nrow(out)))
    out <- out[ord, , drop = FALSE]
    rownames(out) <- NULL
    return(out)
  }
  typed <- values[!is.na(values$missing_type), , drop = FALSE]
  missing_codes <- vapply(v$variable, function(x) {
    rows <- typed[typed$variable == x, , drop = FALSE]
    .qes_join_codes(rows$value, rows$missing_type)
  }, character(1), USE.NAMES = FALSE)
  value_text <- vapply(map, function(m) .qes_join_codes(names(m), unname(m)), character(1), USE.NAMES = FALSE)
  out <- data.frame(
    variable = v$variable,
    label = v$label,
    question = q$text,
    n_value_labels = n_labels,
    stringsAsFactors = FALSE
  )
  if (identical(layout, "wide")) {
    out$value_labels <- I(unname(map))
  }
  out$study <- v$study
  out$position <- v$position
  out$type <- v$type
  out$question_lang <- q$lang
  out$question_truncated <- v$question_truncated
  if (identical(layout, "compact")) {
    out$value_labels <- value_text
  }
  out$missing_codes <- missing_codes
  out$targets <- .qes_search_targets(v$study[1], v$variable)$targets
  out$label_source <- v$label_source
  out$question_source <- v$question_source
  out$doc_ref <- v$doc_ref
  rownames(out) <- NULL
  out
}

# Which rows of a values table are codes declared missing in their file.
.qes_is_declared_rows <- function(values, variables) {
  if (nrow(values) == 0L) {
    return(logical(0))
  }
  na <- variables$na_values[match(values$variable, variables$variable)]
  out <- logical(nrow(values))
  for (s in unique(na[!is.na(na)])) {
    rows <- which(na %in% s)
    out[rows] <- .qes_is_declared(values$value[rows], s)
  }
  out
}

# A codebook object from dictionary tables. `attrs`: .qes_codebook_attrs().
.qes_codebook_make <- function(dict, layout = "compact", lang = NULL, attrs = list()) {
  code <- attrs$survey_code %||% (dict$variables$study[1] %||% NA_character_)
  source_lang <- NA_character_
  if (!is.na(code) && code %in% .qes_study_codes(demo = TRUE)) {
    source_lang <- .qes_study_row(code, demo = TRUE)$source_lang
  }
  out <- .qes_codebook_frame(dict, layout, lang, source_lang)
  for (nm in names(attrs)) {
    attr(out, nm) <- attrs[[nm]]
  }
  attr(out, "value_labels_map") <- .qes_value_map(dict$variables, dict$values)
  attr(out, "qes_dict") <- list(variables = dict$variables, values = dict$values, lang = lang, layout = layout)
  # the attribution of metadata that is not CC0 (qes2022), kept when the
  # codebook is saved
  out <- .qes_with_licence_notice(out, code)
  class(out) <- c("qes_codebook", "data.frame")
  out
}

.qes_codebook_attr_names <- c(
  "survey_code", "doi", "doi_url", "selected_data_file", "variable_names_file",
  "files", "codebook_files", "qes_provenance"
)

# The tables and attributes behind an existing codebook: its qes_dict
# attribute, or, for a codebook made by hand or by qesR 0.4.4, its columns
# and value_labels_map (or its long rows).
.qes_codebook_source <- function(cb) {
  attrs <- list()
  for (nm in .qes_codebook_attr_names) {
    a <- attr(cb, nm, exact = TRUE)
    if (!is.null(a)) {
      attrs[[nm]] <- a
    }
  }
  inner <- attr(cb, "qes_dict", exact = TRUE)
  if (is.list(inner) && is.data.frame(inner$variables)) {
    return(list(dict = inner[c("variables", "values")], attrs = attrs, lang = inner$lang))
  }
  df <- as.data.frame(cb, stringsAsFactors = FALSE)
  if (!("variable" %in% names(df))) {
    .qes_abort_not_codebook(cb)
  }
  code <- attrs$survey_code %||% NA_character_
  vars <- unique(as.character(df$variable))
  first <- match(vars, df$variable)
  col <- function(nm) if (nm %in% names(df)) as.character(df[[nm]][first]) else rep(NA_character_, length(vars))
  variables <- data.frame(
    study = code, variable = vars, position = seq_along(vars), source_name = vars,
    type = NA_character_, measure = NA_character_, var_timing = NA_character_,
    label = col("label"), question_en = col("question"), question_fr = NA_character_,
    question_truncated = NA, universe_en = NA_character_, universe_fr = NA_character_,
    na_values = NA_character_, derived_from = NA_character_,
    label_source = NA_character_, question_source = NA_character_,
    doc_ref = NA_character_, reviewed = FALSE, stringsAsFactors = FALSE
  )
  if (length(vars) == 0L) {
    variables <- .qes_dict_empty("dict_variables")
  }
  map <- attr(cb, "value_labels_map", exact = TRUE)
  rows <- list()
  if (is.list(map) && length(map) > 0L) {
    for (v in intersect(names(map), vars)) {
      m <- map[[v]]
      if (length(m) > 0L) {
        rows[[v]] <- data.frame(variable = v, value = names(m), label = unname(as.character(m)), stringsAsFactors = FALSE)
      }
    }
  } else if (all(c("value", "value_label") %in% names(df))) {
    lab <- df[!is.na(df$value), c("variable", "value", "value_label"), drop = FALSE]
    names(lab)[3] <- "label"
    rows <- list(lab)
  }
  lab <- do.call(rbind, rows)
  values <- if (is.null(lab) || nrow(lab) == 0L) {
    .qes_dict_empty("dict_values")
  } else {
    lab <- lab[!duplicated(lab[c("variable", "value")]), , drop = FALSE]
    data.frame(
      study = code, variable = lab$variable, value = as.character(lab$value), label = lab$label,
      label_source = "file", label_lang = NA_character_, label_en = NA_character_,
      label_fr = NA_character_, missing_type = NA_character_, n = NA_integer_,
      label_flag = NA_character_, stringsAsFactors = FALSE
    )
  }
  variables$label_source <- ifelse(!is.na(variables$label) | variables$variable %in% values$variable, "file", "none")
  list(dict = list(variables = variables, values = values), attrs = attrs, lang = NULL)
}

.qes_dict_keep <- function(dict, vars) {
  keep <- dict$variables$variable %in% vars
  list(
    variables = dict$variables[keep, , drop = FALSE],
    values = dict$values[dict$values$variable %in% dict$variables$variable[keep], , drop = FALSE]
  )
}

.qes_abort_not_codebook <- function(codebook, arg = "codebook") {
  .qes_abort(
    "input_codebook",
    class = "qesR_error_input",
    data = list(arg = arg, value = class(codebook))
  )
}

.qes_abort_plain_codebook <- function(x, arg) {
  .qes_abort(
    "input_codebook_plain",
    class = "qesR_error_input",
    args = list(arg),
    data = list(arg = arg, value = class(x))
  )
}

# ---- the pipeline ------------------------------------------------------------------------

# qes_codebook() and its legacy aliases. `envir`: where opt-in assignment
# lands (the caller of the exported name). `fn`: the exported name, for the
# once-per-session note on `refresh`.
.qes_codebook_impl <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long"),
  variables = NULL,
  lang = NULL,
  envir = NULL,
  fn = "qes_codebook"
) {
  layout <- .qes_check_one(layout, "layout", .qes_codebook_layouts)
  lang <- .qes_check_lang(lang)
  .qes_check_flag(assign_global, "assign_global")
  if (isTRUE(refresh)) {
    .qes_arg_ignored(fn, "refresh")
  }

  if (is.character(srvy)) {
    study <- .get_qes_study(srvy, demo = TRUE)
    selection <- .qes_select_data_file(study$qes_survey_code, file = file, quiet = quiet)
    code <- selection$study
    dict <- .qes_dict_study(code, quiet = quiet)
    attrs <- .qes_codebook_attrs(code, selection$file)
  } else if (inherits(srvy, "qes_codebook")) {
    src <- .qes_codebook_source(srvy)
    dict <- src$dict
    attrs <- src$attrs
    if (missing(lang) || is.null(lang)) {
      lang <- src$lang
    }
    code <- attrs$survey_code %||% NA_character_
  } else if (is.data.frame(srvy)) {
    cb <- attr(srvy, "qes_codebook", exact = TRUE)
    study <- .qes_data_study(srvy)
    if (inherits(cb, "qes_codebook")) {
      src <- .qes_codebook_source(cb)
      dict <- .qes_dict_keep(src$dict, names(srvy))
      attrs <- src$attrs
      code <- attrs$survey_code %||% study %||% NA_character_
    } else if (!is.null(study)) {
      dict <- .qes_dict_for_data(srvy, study, quiet = quiet)
      prov <- attr(srvy, "qes_provenance", exact = TRUE)
      # the file the data was read from: its names are the codebook's
      data_file <- NULL
      if (is.data.frame(prov) && "file_id" %in% names(prov) && nrow(prov) >= 1L) {
        files <- .qes_catalog(demo = .qes_is_demo_code(study))$files
        data_file <- files[files$study == study & files$file_id == as.character(prov$file_id[1]), , drop = FALSE]
        if (nrow(data_file) != 1L) {
          data_file <- NULL
        }
      }
      attrs <- .qes_codebook_attrs(study, data_file, names_row = data_file)
      if (is.data.frame(prov)) {
        attrs$qes_provenance <- prov
      }
      code <- study
    } else {
      .qes_abort_plain_codebook(srvy, "srvy")
    }
  } else {
    .qes_assert_single_string_or_frame(srvy)
  }

  if (!is.null(variables)) {
    keep <- .qes_match_variables(variables, dict$variables$variable, study = code,
      scope = if (is.data.frame(srvy) && !inherits(srvy, "qes_codebook")) "data" else "study")
    dict <- .qes_dict_keep(dict, keep)
    dict$variables <- dict$variables[match(keep, dict$variables$variable), , drop = FALSE]
  }

  out <- .qes_codebook_make(dict, layout = layout, lang = lang, attrs = attrs)

  if (isTRUE(assign_global)) {
    if (is.na(code)) {
      .qes_abort_plain_codebook(srvy, "srvy")
    }
    .qes_assign(paste0(code, "_codebook"), out, envir)
  }
  out
}

.qes_assert_single_string_or_frame <- function(srvy) {
  .qes_abort(
    "input_codebook_srvy",
    class = "qesR_error_input",
    args = list("srvy"),
    data = list(arg = "srvy", value = class(srvy))
  )
}

# ---- exported: qes_codebook() --------------------------------------------------------------

#' The codebook of a study
#'
#' `qes_codebook()` describes every variable of a study: its label, the
#' question asked (in English or French), its value labels and which codes
#' mean "don't know", "refused" or another reason for a missing answer. The
#' description of every study ships with qesR, so the codebook needs no
#' download and no network.
#'
#' @section Where the description comes from:
#' Variable and value labels are those of the study's pinned data file, as
#' [get_qes()] reads it (the complete labels of the SPSS twin for `qes2012`).
#' A label that only repeats the variable's name is not a label: `label` is
#' then `NA`. Question text comes from the questionnaires deposited with the
#' study (`question_source = "questionnaire"`; `doc_ref` holds one or more
#' `<file_id>:<question>` references separated by `;`, e.g.
#' `"352010:Q19;352009:Q19"`), and for `qes2022` from its bilingual codebook
#' (`question_source = "codebook"`, `doc_ref` `"7449514:<question>"`; the
#' item of a grid is its stem followed by the item in brackets); `question`
#' is `NA` when the document does not give it, never a copy of the variable
#' name or of `label`, and never a machine translation. `qes2018`'s data file
#' has no value labels for most variables: they come from the study's French
#' questionnaire with its programmed answer codes
#' (`label_source = "supplement"`).
#'
#' The CC0 studies' description is in the public domain. The description of
#' `qes2022` (its labels, question text and answer counts) is derived from
#' the 2022 Quebec Election Study and carries its licence, CC BY-NC 4.0:
#' cite the study (`qes_cite("qes2022")`) and do not use it commercially
#' (see the file `COPYRIGHTS` of the installed package). A printed codebook
#' of `qes2022` ends its header with this licence and the attribution it
#' requires, which its attribute `licence_notice` also holds, so that a
#' saved codebook keeps it. Its variable labels are those of its Stata file, which cuts them
#' at 80 characters; its question text, from the codebook, is not cut.
#'
#' @section En français:
#' `qes_codebook()` décrit chaque variable d'une étude : son étiquette, la
#' question posée (en anglais ou en français), ses étiquettes de valeurs et
#' les codes qui signifient « ne sait pas », « refus » ou une autre raison de
#' non-réponse. La description de chaque étude est livrée avec qesR : aucun
#' téléchargement n'est nécessaire. Le texte des questions vient des
#' questionnaires déposés et, pour `qes2022`, de son livre de codes bilingue
#' (`doc_ref` : une ou plusieurs références `<file_id>:<question>` séparées
#' par `;`) ; il vaut `NA` lorsqu'il est inconnu, sans traduction
#' automatique. `lang = "fr"` donne le texte français. La description de
#' `qes2022` (étiquettes, texte des questions, effectifs) est tirée de
#' l'Étude électorale québécoise 2022 et reste sous sa licence, CC BY-NC
#' 4.0 : citez l'étude (`qes_cite("qes2022")`) et n'en faites pas d'usage
#' commercial (voir le fichier `COPYRIGHTS` du package installé) ; le
#' codebook imprimé de `qes2022` rappelle cette licence et l'attribution
#' qu'elle exige, que garde aussi son attribut `licence_notice` (à
#' conserver avec le codebook enregistré).
#' L'attribut `selected_data_file` garde le nom Dataverse du fichier décrit
#' (la copie `.tab` d'un fichier ingéré, comme dans qesR 0.4.4) ; l'en-tête
#' affiché nomme aussi le fichier original (`.sav` ou `.dta`) que
#' [get_qes()] lit et que [qes_provenance()] indique.
#'
#' @param srvy A study code from [qes_studies()] (trimmed and
#'   case-insensitive), or an object to describe again: a codebook returned
#'   by `qes_codebook()` (laid out again with `layout`), or a data frame
#'   returned by [get_qes()], whose codebook is limited to the columns it
#'   still has (base R's `[` drops the attributes that record the study; a
#'   subset made with it is a plain data frame). A plain data frame (for example a codebook read back from a
#'   CSV file, which has lost its class) is an error of class
#'   `qesR_error_input`: rebuild it with `qes_codebook("<code>")`.
#' @param file Optional regular expression choosing which data file of the
#'   study the codebook describes, as in [get_qes()]. The description is
#'   that of the pinned file, with the pinned file's variable names (the
#'   attribute `variable_names_file` names that file): the twin files of a
#'   deposit hold the same variables, but their names can differ in case
#'   (`qes2012`'s SPSS twin has `Q0QC` where the pinned Stata file has
#'   `q0qc`). For a codebook whose names match data read with
#'   `get_qes(file = )`, describe that data: `qes_codebook(<data>)`, or use
#'   the codebook [get_qes()] attaches.
#' @param assign_global If TRUE, also assign the returned codebook as
#'   \code{<code>_codebook} into the environment the function was called from
#'   (the global environment only when called at top level), where `<code>` is
#'   the canonical study code. Defaults to FALSE.
#' @param quiet If TRUE, suppress informational output.
#' @param refresh Ignored: the description is shipped with qesR. Setting it
#'   prints a one-time note.
#' @param layout `"compact"` (default: one row per variable), `"wide"` (the
#'   same, with the value labels as a list column) or `"long"` (one row per
#'   value).
#' @param variables Optional character vector of variable names (exact
#'   match) to describe; an unknown name is an error of class
#'   `qesR_error_unknown_variable` that suggests near matches.
#' @param lang Language of the question text: `NULL` (default) gives the
#'   study's own language (French for most studies, English for `qes2012` and
#'   `qes2022`), or the other language when only that one is known; `"en"` or
#'   `"fr"` gives that language only (`NA` where it is unknown). Labels are
#'   always those of the file.
#'
#' @return A data frame of class `qes_codebook`, returned visibly.
#'
#'   With `layout = "compact"`, one row per variable: `variable`, `label`
#'   (the file's variable label), `question`, `n_value_labels` (the columns
#'   of qesR 0.4.4, first and in that order), then `study`, `position` (in
#'   the data), `type` (`numeric`, `character`, `date`, `datetime` or
#'   `logical`), `question_lang`, `question_truncated`, `value_labels`
#'   (`"1=Oui | 2=Non"`), `missing_codes` (`"8=dk | 9=refused"`), `targets`
#'   (the harmonized targets the variable feeds in [qes_harmonize()],
#'   `;`-separated, `NA` for none, as in [qes_search()]), `label_source` (`file`, `label_donor`, `supplement`,
#'   `file_malformed` or `none`), `question_source` and `doc_ref`.
#'   `layout = "wide"` has the same columns with `value_labels` as a list of
#'   named character vectors. `layout = "long"` has one row per value:
#'   `variable`, `value`, `value_label`, `label`, `question`, then `study`,
#'   `missing_type` and `is_declared_na` (the code is declared missing in the
#'   SPSS file); a variable with no value labels keeps one row whose `value`
#'   is `NA`.
#'
#'   Every layout has the attributes `survey_code`, `doi`, `doi_url`,
#'   `selected_data_file` (the Dataverse name of the data file described:
#'   for an ingested file, the `.tab` copy, as in qesR 0.4.4; the header
#'   printed by the codebook also names the original `.sav` or `.dta` that
#'   [get_qes()] reads and [qes_provenance()] reports),
#'   `variable_names_file` (the data file whose
#'   variable names the codebook uses), `files` (the study's files),
#'   `codebook_files` (its documents, as [qes_docs()] lists them) and
#'   `qes_provenance`. The codebook of `qes2022` also has the attribute
#'   `licence_notice`: the attribution and licence (CC BY-NC 4.0) of its
#'   description, named by the study; keep it with any copy you share.
#'
#' @family codebooks and search
#' @seealso [qes_question()] for the exact wording of some variables,
#'   [qes_search()] to find variables across studies, [qes_missing()] to set
#'   the missing codes to `NA`.
#' @examples
#' cb <- qes_codebook("qes2014")
#' head(cb[, c("variable", "label", "question", "value_labels")])
#'
#' # one row per value, for two variables, with the English question text
#' qes_codebook("qes2014", layout = "long", variables = c("Q2", "Q19"), lang = "en")
#'
#' # the codebook of data you have read
#' demo <- get_qes("qes_demo", quiet = TRUE)
#' qes_codebook(demo, variables = c("Q19", "Q28"))
#' @export
qes_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long"),
  variables = NULL,
  lang = NULL
) {
  .qes_codebook_impl(
    srvy, file = file, assign_global = assign_global, quiet = quiet,
    refresh = refresh, layout = layout, variables = variables, lang = lang,
    envir = parent.frame(), fn = "qes_codebook"
  )
}

# ---- legacy wrappers ------------------------------------------------------------------------

#' Get a Quebec Election Study codebook (legacy)
#'
#' Soft-deprecated: use [qes_codebook()], which takes the same arguments.
#' `get_codebook()` and `get_qes_codebook()` keep working and will not be
#' removed; each prints a one-time notice (see [qesR-deprecated]). They
#' return what `qes_codebook()` returns: the codebook is built offline from
#' the metadata shipped with qesR (see [qes_codebook()]), the columns of
#' qesR 0.4.4 come first and new columns follow, and the attributes
#' (`survey_code`, `doi`, `files`, `codebook_files`, ...) are kept in every
#' layout. `refresh` no longer changes the result.
#'
#' @param srvy A qesR survey code from `qes_studies()`. Codes are trimmed and
#'   case-insensitive.
#' @param file Optional regular expression for choosing one file in multi-file datasets.
#' @param assign_global If TRUE, also assign the returned codebook as
#'   \code{<code>_codebook} into the environment the function was called from
#'   (the global environment only when called at top level), where `<code>` is
#'   the canonical study code. Defaults to FALSE.
#' @param quiet If TRUE, suppress informational output.
#' @param refresh Ignored (the codebook is built offline); setting it prints
#'   a one-time note.
#' @param layout One of `"compact"`, `"wide"`, or `"long"`.
#'
#' @return A `qes_codebook` data frame, returned visibly (see
#'   [qes_codebook()]).
#' @family legacy
#' @seealso [qes_codebook()], and [qesR-deprecated] for the legacy functions
#'   and their replacements.
#' @examples
#' cb <- get_codebook("qes2014")
#' head(cb[, 1:4])
#' @export
get_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  .qes_deprecate("get_codebook")
  .qes_codebook_impl(
    srvy, file = file, assign_global = assign_global, quiet = quiet,
    refresh = refresh, layout = layout, envir = parent.frame(), fn = "get_codebook"
  )
}

#' @rdname get_codebook
#' @export
get_qes_codebook <- function(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
) {
  .qes_deprecate("get_qes_codebook")
  .qes_codebook_impl(
    srvy, file = file, assign_global = assign_global, quiet = quiet,
    refresh = refresh, layout = layout, envir = parent.frame(), fn = "get_qes_codebook"
  )
}

#' Reformat a qesR codebook (legacy)
#'
#' Soft-deprecated: use `qes_codebook(codebook, layout = )`, which lays a
#' codebook out again. `format_codebook()` keeps working and will not be
#' removed; it prints a one-time notice (see [qesR-deprecated]).
#'
#' The long layout keeps the variables that have no value labels, with one
#' row whose `value` is `NA`. A plain data frame, for example a codebook
#' read back from a CSV file (which loses its class and attributes), is an
#' error of class `qesR_error_input`: rebuild it with
#' `qes_codebook("<code>")`.
#'
#' @param codebook A `qes_codebook` object from `qes_codebook()`.
#' @param layout One of `"compact"`, `"wide"`, or `"long"`.
#'
#' @return The codebook in the requested layout (see [qes_codebook()]).
#' @family legacy
#' @seealso [qes_codebook()], and [qesR-deprecated] for the legacy functions
#'   and their replacements.
#' @examples
#' cb <- qes_codebook("qes2014", variables = c("Q2", "Q19"))
#' format_codebook(cb, layout = "long")
#' @export
format_codebook <- function(codebook, layout = c("compact", "wide", "long")) {
  .qes_deprecate("format_codebook")
  if (!inherits(codebook, "qes_codebook")) {
    if (is.data.frame(codebook)) {
      if (!is.null(.qes_data_study(codebook)) ||
          inherits(attr(codebook, "qes_codebook", exact = TRUE), "qes_codebook")) {
        .qes_abort(
          "input_codebook_data",
          class = "qesR_error_input",
          args = list("codebook"),
          data = list(arg = "codebook", value = class(codebook))
        )
      }
      .qes_abort_plain_codebook(codebook, "codebook")
    }
    .qes_abort_not_codebook(codebook)
  }
  layout <- .qes_check_one(layout, "layout", .qes_codebook_layouts)
  .qes_codebook_impl(codebook, layout = layout, fn = "format_codebook")
}

#' Get value labels from a codebook (legacy)
#'
#' Soft-deprecated: use `qes_codebook(layout = "long")`, which has one row per
#' value with its label and missing type. `get_value_labels()` keeps working
#' and will not be removed; it prints a one-time notice (see
#' [qesR-deprecated]).
#'
#' A variable that is not in the codebook is an error of class
#' `qesR_error_unknown_variable` that suggests near matches (qesR 0.4.4
#' returned an empty list). A label that is an empty string in the file is
#' kept as `""`.
#'
#' @param codebook A `qes_codebook` object from `qes_codebook()`.
#' @param variable Optional variable name. If `NULL`, returns mappings for all
#'   variables that have value labels.
#' @param long If TRUE, return a long data frame with `variable`, `value`, and
#'   `value_label` columns.
#'
#' @return A named list of value-label vectors (named by code), or a long
#'   data frame when `long = TRUE`.
#' @family legacy
#' @seealso [qes_codebook()], and [qesR-deprecated] for the legacy functions
#'   and their replacements.
#' @examples
#' cb <- qes_codebook("qes2014", variables = c("Q2", "Q19"))
#' get_value_labels(cb, "Q19")
#' @export
get_value_labels <- function(codebook, variable = NULL, long = FALSE) {
  .qes_deprecate("get_value_labels")
  .get_value_labels_impl(codebook, variable = variable, long = long)
}

.get_value_labels_impl <- function(codebook, variable = NULL, long = FALSE) {
  if (!is.data.frame(codebook)) {
    .qes_abort_not_codebook(codebook)
  }
  src <- .qes_codebook_source(codebook)
  dict <- src$dict
  if (!is.null(variable)) {
    .assert_single_string(variable, "variable")
    .qes_match_variables(variable, dict$variables$variable,
      study = src$attrs$survey_code %||% NA_character_, arg = "variable", scope = "study")
    dict <- .qes_dict_keep(dict, variable)
  }
  lab <- .qes_labelled_rows(dict$values)
  lab <- lab[order(match(lab$variable, dict$variables$variable)), , drop = FALSE]
  if (isTRUE(long)) {
    out <- data.frame(
      variable = lab$variable,
      value = lab$value,
      value_label = lab$label,
      stringsAsFactors = FALSE
    )
    rownames(out) <- NULL
    return(out)
  }
  vars <- unique(lab$variable)
  map <- lapply(vars, function(v) {
    rows <- lab[lab$variable == v, , drop = FALSE]
    stats::setNames(rows$label, rows$value)
  })
  names(map) <- vars
  map
}

#' Download codebook files (legacy)
#'
#' Downloads a study's documentation files (codebooks, questionnaires and
#' reports) into a local directory.
#'
#' Soft-deprecated: use [qes_download()] with `what = "docs"`.
#' `download_codebook()` keeps working and will not be removed; it prints a
#' one-time notice (see [qesR-deprecated]). It now downloads the documents
#' listed in the offline catalog (the same files as [get_codebook_files()]),
#' checks each one against its md5 checksum before giving it its final name,
#' and makes no metadata request. `file` now selects documents by name; in
#' qesR 0.4.4 it selected a data file. `refresh` no longer changes the result.
#' `dest_dir` is created only when there is at least one file to download. A
#' file already in `dest_dir` is kept as it is unless `overwrite = TRUE`.
#'
#' @param srvy A qesR survey code.
#' @param dest_dir Directory where files should be downloaded. Created if
#'   needed, only when there is a file to download.
#' @param file Optional regular expression matched (ignoring case) against
#'   the document file names; only matching documents are downloaded.
#' @param quiet If TRUE, suppress informational output.
#' @param refresh Ignored: the list comes from the catalog shipped with qesR.
#' @param overwrite If TRUE, overwrite existing files in `dest_dir`.
#'
#' @return A data frame with the columns `file_id`, `filename`, `extension`,
#'   `size`, `download_url`, `local_path` and `downloaded`.
#' @family legacy
#' @seealso [qes_download()], [qes_docs()], and [qesR-deprecated] for the
#'   legacy functions and their replacements.
#' @examples
#' # the documents it would download for a study, offline
#' get_codebook_files("qes2018")[, c("filename", "size")]
#'
#' # a `file` pattern that matches no document: nothing is downloaded and
#' # no folder is created
#' download_codebook("qes2018", dest_dir = file.path(tempdir(), "qes_docs"),
#'                   file = "^no such document$")
#' @export
download_codebook <- function(
  srvy,
  dest_dir = tempdir(),
  file = NULL,
  quiet = FALSE,
  refresh = FALSE,
  overwrite = FALSE
) {
  .qes_deprecate("download_codebook")
  .download_codebook_impl(
    srvy, dest_dir = dest_dir, file = file, quiet = quiet,
    refresh = refresh, overwrite = overwrite
  )
}

# The legacy adapter over qes_download(what = "docs") (design.md section 2.3).
.download_codebook_impl <- function(
  srvy,
  dest_dir = tempdir(),
  file = NULL,
  quiet = FALSE,
  refresh = FALSE,
  overwrite = FALSE
) {
  .assert_single_string(dest_dir, "dest_dir")
  if (!is.null(file)) {
    .assert_single_string(file, "file")
    .qes_assert_regex(file, "file")
  }
  .qes_check_flag(overwrite, "overwrite")
  if (isTRUE(refresh)) {
    .qes_arg_ignored("download_codebook", "refresh")
  }
  code <- .get_qes_study(srvy, demo = TRUE)$qes_survey_code
  # the synthetic study has no documents: the zero-row branch below
  rows <- .qes_download_select(.qes_legacy_deposit_codes(code), what = "docs")
  if (!is.null(file)) {
    rows <- rows[grepl(file, rows$file_name, ignore.case = TRUE), , drop = FALSE]
  }

  out <- .qes_legacy_files_frame(nrow(rows))
  if (nrow(rows) == 0L) {
    .qes_inform("no_codebook_files", class = "qesR_message_download", args = list(.qes_q(code)), data = list(study = code), quiet = quiet)
    out$local_path <- character(0)
    out$downloaded <- logical(0)
    return(out)
  }
  if (!dir.exists(dest_dir) && !dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)) {
    .qes_abort(
      "download_write",
      class = "qesR_error_input",
      args = list(.qes_q(dest_dir)),
      data = list(arg = "dest_dir", value = dest_dir)
    )
  }
  done <- .qes_download_files(rows, normalizePath(dest_dir, winslash = "/", mustWork = TRUE),
    overwrite = overwrite, quiet = quiet, legacy = TRUE)

  servers <- .qes_catalog()$studies$server[match(rows$study, .qes_catalog()$studies$study)]
  out$file_id <- rows$file_id
  out$filename <- rows$file_name
  out$extension <- rows$format
  out$size <- rows$bytes
  out$download_url <- vapply(seq_len(nrow(rows)), function(i) {
    .qes_url(servers[i], "file", file_id = rows$file_id[i])
  }, character(1))
  out$local_path <- done$local_path
  out$downloaded <- done$downloaded
  out
}

# The "Data file:" line of print.qes_codebook(): the deposited file that
# get_qes() reads and qes_provenance() reports (the original .sav or .dta of
# an ingested file), then the Dataverse name the attribute
# selected_data_file keeps (qesR 0.4.4), when they differ. The attribute
# alone when the file is not in the catalog.
.qes_codebook_read_name <- function(survey_code, selected) {
  if (!is.character(selected) || length(selected) != 1L || is.na(selected)) {
    return(selected)
  }
  row <- tryCatch({
    if (!is.character(survey_code) || length(survey_code) != 1L || is.na(survey_code)) {
      NULL
    } else {
      files <- .qes_catalog(demo = .qes_is_demo_code(survey_code))$files
      files[files$study == survey_code & files$file_name == selected, , drop = FALSE]
    }
  }, error = function(e) NULL)
  if (is.null(row) || nrow(row) != 1L) {
    return(selected)
  }
  read_name <- .qes_deposit_name(row)
  if (is.na(read_name) || identical(read_name, selected)) {
    return(selected)
  }
  sprintf("%s (Dataverse: %s)", read_name, selected)
}

#' @export
print.qes_codebook <- function(x, n = 10L, ...) {
  survey_code <- attr(x, "survey_code", exact = TRUE)
  doi <- attr(x, "doi", exact = TRUE)
  selected <- attr(x, "selected_data_file", exact = TRUE)
  files <- attr(x, "codebook_files", exact = TRUE)

  cat("<qes_codebook>")
  if (!is.null(survey_code)) cat(" survey:", survey_code)
  cat("\n")

  if (!is.null(doi) && !is.na(doi)) cat("DOI:", doi, "\n")
  if (!is.null(selected)) cat("Data file:", .qes_codebook_read_name(survey_code, selected), "\n")
  n_var <- if ("variable" %in% names(x)) length(unique(x$variable)) else nrow(x)
  cat("Variables:", n_var, "\n")
  if (nrow(x) != n_var) cat("Rows:", nrow(x), "\n")
  cat("Codebook/support files:", if (is.null(files)) 0L else nrow(files), "\n")
  # the attribution and licence of metadata that is not CC0 (qes2022)
  notice <- .qes_licence_notice(if (is.character(survey_code)) survey_code[1] else NA_character_)
  if (!is.null(notice)) {
    cat(strwrap(notice, width = max(40L, getOption("width", 80L) - 2L)), sep = "\n")
  }

  preview <- as.data.frame(unclass(x), stringsAsFactors = FALSE)
  if ("value_labels" %in% names(preview) && is.list(preview$value_labels)) {
    preview$value_labels <- vapply(preview$value_labels, function(v) {
      if (length(v) == 0L) "" else paste0("[", length(v), " labels]")
    }, character(1))
  }
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 0L) {
    n <- 10L
  }
  print(.qes_preview(utils::head(preview, n)), ...)
  invisible(x)
}

# A data frame for printing: long text cut to `width` characters. The
# object itself is never changed.
.qes_preview <- function(df, width = 40L) {
  for (nm in names(df)) {
    v <- df[[nm]]
    if (is.character(v)) {
      long <- !is.na(v) & nchar(v) > width
      v[long] <- paste0(substr(v[long], 1L, width - 3L), "...")
      df[[nm]] <- v
    }
  }
  df
}
