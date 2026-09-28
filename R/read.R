# The reader (design.md sections 4.4 and 4.5, slice S2b).
#
# .qes_read() is the only function that reads a study's data. It reads the
# pinned ORIGINAL upload of a catalog data file (.sav or .dta), never the
# tab-delimited file Dataverse makes from it, and never an RDS, CSV or text
# file. The steps:
#
#   1. the file: files.csv gives (file_id, md5); the bytes come from the
#      download cache (R/cache.R), which checks them against that md5 before
#      use. qes_demo is read from the package itself, after the same check;
#   2. parse, dispatching on the catalog `format` only:
#      haven::read_sav(user_na = TRUE) or haven::read_dta(), with the
#      catalog `encoding` when it is set (else the file's own);
#   3. check n_rows and n_cols against the catalog (qesR_error_rowcount);
#   4. labels (precedence below), then the catalog's text fixes;
#   5. .qes_unspss(): SPSS user-missing codes stay as values, as in qesR
#      0.4.4, and their declarations move to attributes; then the catalog's
#      type fixes (whole-number codes stored as text become numbers);
#   6. name_map.csv renames that restore the names of qesR 0.4.4;
#   7. a tripwire: any U+FFFD or C1 control character left in a label or a
#      text value raises qesR_warning_encoding.
#
# Label precedence (variable and value labels):
#   * a label donor, when the study declares one (studies.label_file_id): a
#     twin upload of the same data (same UNF, same rows, the same names up to
#     case) whose labels are the complete originals. qes2012 is the only
#     case: its Stata file, whose lowercase names v0.4.4 returned, has labels
#     that Stata lowercased and cut at 80 characters, while the SPSS twin
#     keeps them intact (up to 256 characters). The donor supplies a label
#     wherever it has one;
#   * the pinned file's own labels otherwise. A malformed variable label (a
#     vector, e.g. qes2007_panel `ininum`) keeps its first element and is
#     recorded as "file_malformed";
#   * a reviewed supplement for files that have no labels at all (qes2018
#     value labels, from its questionnaire) is part of the shipped
#     dictionary (R/metadata.R): it describes the data in the codebook and
#     is never written onto the data;
#   * else none. Labels never come from DDI metadata or a variable name.
# The label source of each column is kept in the internal attribute
# qes_label_source (get_qes() drops it).
#
# Parsed data is kept in memory for the session (qesR.memo), keyed by md5.

.qes_read_formats <- c("sav", "zsav", "dta")

# ---- catalog lookups -------------------------------------------------------------

.qes_is_demo_code <- function(study) {
  identical(study, "qes_demo")
}

# The files.csv row of `file_id` in `study`.
.qes_file_row <- function(study, file_id, demo = .qes_is_demo_code(study)) {
  files <- .qes_catalog(demo = demo)$files
  row <- files[files$study == study & files$file_id == file_id, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop(sprintf("qesR internal error: file %s of %s is not in the catalog.", file_id, study), call. = FALSE)
  }
  rownames(row) <- NULL
  row
}

# The default data file of `study` (the lint guarantees exactly one).
.qes_default_data_file <- function(study, demo = .qes_is_demo_code(study)) {
  files <- .qes_catalog(demo = demo)$files
  row <- files[files$study == study & files$role == "data" & files$is_default %in% TRUE, , drop = FALSE]
  if (nrow(row) != 1L) {
    stop(sprintf("qesR internal error: %s has no single default data file.", study), call. = FALSE)
  }
  rownames(row) <- NULL
  row
}

# Which data file get_qes(srvy, file = ) reads. `file` is a regular
# expression matched (case-insensitively) against the names of the study's
# data files only ([A:D6]): documents are never data. A label donor is a
# complete data file too (the qes2012 SPSS twin, which get_qes(file = "SPSS")
# read in qesR 0.4.4), so it can be chosen; it is never the default. A
# pattern that matches no data file of the study but exactly one of another
# study of the same deposit (get_qes("qes1998", file = "CROP")) reads that
# study, with a message. No match, or more than one, is an error.
# Returns list(study = <code>, file = <files.csv row>).
.qes_select_data_file <- function(study, file = NULL, quiet = FALSE) {
  demo <- .qes_is_demo_code(study)
  if (is.null(file)) {
    return(list(study = study, file = .qes_default_data_file(study, demo = demo)))
  }
  .assert_single_string(file, "file")
  .qes_assert_regex(file, "file")
  cat <- .qes_catalog(demo = demo)
  data_files <- cat$files[cat$files$role %in% c("data", "label_donor"), , drop = FALSE]
  matches <- function(rows) {
    hit <- grepl(file, rows$file_name, ignore.case = TRUE) |
      grepl(file, rows$original_file_name, ignore.case = TRUE)
    rows[hit %in% TRUE, , drop = FALSE]
  }
  names_of <- function(rows) rows$original_file_name

  own <- data_files[data_files$study == study, , drop = FALSE]
  hit <- matches(own)
  if (nrow(hit) == 1L) {
    rownames(hit) <- NULL
    return(list(study = study, file = hit))
  }
  if (nrow(hit) > 1L) {
    .qes_abort_ambiguous_file(study, file, names_of(hit))
  }

  studies <- cat$studies
  me <- studies[studies$study == study, , drop = FALSE]
  siblings <- studies$study[studies$study != study & !studies$demo &
    studies$server %in% me$server & studies$doi %in% me$doi]
  hit <- matches(data_files[data_files$study %in% siblings, , drop = FALSE])
  if (nrow(hit) == 1L) {
    rownames(hit) <- NULL
    .qes_inform(
      "file_redirect",
      class = "qesR_message_download",
      args = list(.qes_q(study), .qes_q(file), .qes_q(hit$study)),
      data = list(study = hit$study, from = study, pattern = file),
      quiet = quiet
    )
    return(list(study = hit$study, file = hit))
  }
  if (nrow(hit) > 1L) {
    .qes_abort_ambiguous_file(study, file, names_of(hit))
  }
  .qes_abort(
    "file_no_match",
    class = "qesR_error_ambiguous_file",
    args = list(.qes_q(study), .qes_q(file), .qes_q(names_of(own))),
    data = list(study = study, pattern = file, candidates = names_of(own))
  )
}

# A user-supplied regular expression that R cannot compile is an input
# error, not a bare regex error with a locale-dependent message. `perl`
# selects the engine the caller matches with (TRE or PCRE), so the check
# accepts and refuses the same patterns as the match.
.qes_assert_regex <- function(pattern, arg, perl = FALSE, value = pattern) {
  ok <- tryCatch(
    {
      grepl(pattern, "", ignore.case = TRUE, perl = perl)
      TRUE
    },
    error = function(e) FALSE,
    warning = function(w) FALSE
  )
  if (!ok) {
    .qes_abort(
      "input_regex",
      class = "qesR_error_input",
      args = list(arg, .qes_q(value)),
      data = list(arg = arg, value = value)
    )
  }
  invisible(pattern)
}

.qes_abort_ambiguous_file <- function(study, pattern, candidates) {
  .qes_abort(
    "file_ambiguous",
    class = "qesR_error_ambiguous_file",
    args = list(.qes_q(study), .qes_q(pattern), .qes_q(candidates)),
    data = list(study = study, pattern = pattern, candidates = candidates)
  )
}

# The label donor of a data file: the studies.label_file_id row, used only
# for that study's data files of the same UNF.
.qes_label_donor <- function(study_row, file_row, demo = FALSE) {
  donor_id <- study_row$label_file_id
  if (length(donor_id) != 1L || is.na(donor_id) || identical(donor_id, file_row$file_id)) {
    return(NULL)
  }
  donor <- .qes_file_row(study_row$study, donor_id, demo = demo)
  if (is.na(donor$unf) || !identical(donor$unf, file_row$unf)) {
    return(NULL)
  }
  donor
}

# ---- local copies ----------------------------------------------------------------

# A local, md5-verified copy of a catalog file. qes_demo ships with the
# package; every other file comes through the download cache. Returns the
# path with attribute retrieved_via (and transient_root when the cache mode
# is "none": the caller deletes that directory once the file is read).
.qes_local_file <- function(study_row, file_row, quiet = TRUE) {
  if (isTRUE(study_row$demo)) {
    path <- .qes_extdata("demo", "data", file_row$original_file_name)
    actual <- .qes_md5(path)
    if (!identical(actual, file_row$md5)) {
      .qes_abort(
        "checksum",
        class = "qesR_error_checksum",
        args = list(.qes_q(file_row$study), .qes_q(file_row$file_id), file_row$md5, actual),
        data = list(study = file_row$study, file_id = file_row$file_id, expected = file_row$md5, actual = actual)
      )
    }
    attr(path, "retrieved_via") <- "local_demo"
    return(path)
  }
  path <- .qes_cache_fetch(file_row, study_row$server, quiet = quiet)
  attr(path, "retrieved_via") <- .qes_cache_via(path)
  path
}

.qes_release_local_file <- function(path) {
  root <- attr(path, "transient_root", exact = TRUE)
  if (!is.null(root)) {
    unlink(root, recursive = TRUE)
  }
  invisible()
}

# ---- parsing ---------------------------------------------------------------------

# Parse one original file. Dispatch is on the catalog format only; anything
# else (RDS, CSV, text, zip) is refused.
.qes_read_file <- function(path, format, encoding = NA_character_,
                           study = NA_character_, file_id = NA_character_) {
  format <- tolower(format %||% "")
  if (length(format) != 1L || !(format %in% .qes_read_formats)) {
    .qes_abort(
      "source_format",
      class = "qesR_error_source",
      args = list(.qes_q(format)),
      data = list(study = study, file_id = file_id, format = format)
    )
  }
  enc <- if (length(encoding) == 1L && !is.na(encoding) && nzchar(encoding)) encoding else NULL
  out <- if (format %in% c("sav", "zsav")) {
    haven::read_sav(path, user_na = TRUE, encoding = enc)
  } else {
    haven::read_dta(path, encoding = enc)
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

.qes_reader_call <- function(format) {
  if (format %in% c("sav", "zsav")) "haven::read_sav(user_na = TRUE)" else "haven::read_dta()"
}

.qes_check_dims <- function(data, file_row) {
  expected <- c(file_row$n_rows, file_row$n_cols)
  actual <- c(nrow(data), ncol(data))
  if (anyNA(expected) || !identical(as.integer(expected), as.integer(actual))) {
    .qes_abort(
      "rowcount",
      class = "qesR_error_rowcount",
      args = list(
        .qes_q(file_row$study), .qes_q(file_row$file_id),
        sprintf("%s x %s", expected[1], expected[2]),
        sprintf("%s x %s", actual[1], actual[2])
      ),
      data = list(study = file_row$study, file_id = file_row$file_id, expected = expected, actual = actual)
    )
  }
  invisible(data)
}

# ---- labels ----------------------------------------------------------------------

# Variable labels as found: a malformed label (a vector of several strings)
# keeps its first element. Returns the data and each column's label source.
.qes_file_labels <- function(data) {
  source <- stats::setNames(rep("none", ncol(data)), names(data))
  for (j in seq_along(data)) {
    lab <- attr(data[[j]], "label", exact = TRUE)
    if (is.null(lab)) {
      next
    }
    if (!is.character(lab) || length(lab) != 1L) {
      first <- as.character(lab)[1]
      attr(data[[j]], "label") <- if (is.na(first)) NULL else first
      source[j] <- if (is.na(first)) "none" else "file_malformed"
    } else {
      source[j] <- "file"
    }
  }
  if (any(source == "none")) {
    has_value_labels <- vapply(data, function(x) length(attr(x, "labels", exact = TRUE)) > 0L, logical(1))
    source[source == "none" & has_value_labels] <- "file"
  }
  list(data = data, source = source)
}

# The bare values of a column, with no attribute.
.qes_plain <- function(x) {
  attributes(x) <- NULL
  x
}

# Value labels `labels` cast to the type of `x`, or NULL when they cannot be
# (a numeric donor label set for a text column, say).
.qes_cast_labels <- function(labels, x) {
  if (is.null(labels) || length(labels) == 0L) {
    return(NULL)
  }
  base <- .qes_plain(x)
  if (is.numeric(base) && is.numeric(labels)) {
    return(stats::setNames(as.double(unclass(labels)), names(labels)))
  }
  if (is.character(base) && is.character(labels)) {
    return(stats::setNames(as.character(unclass(labels)), names(labels)))
  }
  NULL
}

# Labels from a donor twin, matched by name up to case. The donor supplies
# the variable label and the value labels wherever it has them.
.qes_apply_donor <- function(data, donor, source) {
  idx <- match(tolower(names(data)), tolower(names(donor)))
  for (j in which(!is.na(idx))) {
    d <- donor[[idx[j]]]
    x <- data[[j]]
    used <- FALSE
    lab <- attr(d, "label", exact = TRUE)
    if (is.character(lab) && length(lab) >= 1L && !is.na(lab[1]) && nzchar(lab[1])) {
      attr(x, "label") <- lab[1]
      used <- TRUE
    }
    labels <- .qes_cast_labels(attr(d, "labels", exact = TRUE), x)
    if (!is.null(labels)) {
      if (inherits(x, "haven_labelled")) {
        attr(x, "labels") <- labels
      } else {
        keep <- attributes(x)[setdiff(names(attributes(x)), c("class", "labels", "label"))]
        x <- haven::labelled(.qes_plain(x), labels = labels, label = attr(x, "label", exact = TRUE))
        attributes(x) <- c(attributes(x), keep[setdiff(names(keep), names(attributes(x)))])
      }
      used <- TRUE
    }
    if (used) {
      data[[j]] <- x
      source[j] <- "label_donor"
    }
  }
  list(data = data, source = source)
}

# The catalog's text fixes for one file (text_fixes.csv): single characters
# that the file's own encoding decodes wrongly in its labels, e.g. the
# CP850 letters typed into the Windows-1252 labels of the CROP files (byte
# 0x85 is "a grave" in CP850 but an ellipsis in Windows-1252). Characters are
# written as code points ("U+2026") so the catalog stays plain.
.qes_text_fixes <- function(file_id, demo = FALSE) {
  fixes <- .qes_catalog(demo = demo)$text_fixes
  fixes <- fixes[fixes$file_id == file_id, , drop = FALSE]
  if (nrow(fixes) == 0L) {
    return(NULL)
  }
  cp <- function(x) intToUtf8(strtoi(sub("^U\\+", "", x), 16L))
  list(from = vapply(fixes$from, cp, character(1), USE.NAMES = FALSE),
       to = vapply(fixes$to, cp, character(1), USE.NAMES = FALSE))
}

.qes_fix_text <- function(x, fixes) {
  if (is.null(x) || length(x) == 0L) {
    return(x)
  }
  for (i in seq_along(fixes$from)) {
    x <- gsub(fixes$from[i], fixes$to[i], x, fixed = TRUE)
  }
  x
}

# Apply text fixes to variable labels and value-label names (never to data).
.qes_apply_text_fixes <- function(data, fixes) {
  if (is.null(fixes)) {
    return(data)
  }
  for (j in seq_along(data)) {
    lab <- attr(data[[j]], "label", exact = TRUE)
    if (is.character(lab)) {
      attr(data[[j]], "label") <- .qes_fix_text(lab, fixes)
    }
    labels <- attr(data[[j]], "labels", exact = TRUE)
    if (length(labels) > 0L) {
      names(labels) <- .qes_fix_text(names(labels), fixes)
      attr(data[[j]], "labels") <- labels
    }
  }
  data
}

# ---- SPSS user-missing values -----------------------------------------------------

# Convert haven_labelled_spss columns to haven_labelled. The codes are kept
# as values (what qesR 0.4.4 returned, and what is.na() counts rely on); the
# declared user-missing values move to attributes qes_na_values and
# qes_na_range, where qes_missing() finds them.
.qes_unspss <- function(data) {
  for (j in seq_along(data)) {
    x <- data[[j]]
    if (!inherits(x, "haven_labelled_spss")) {
      next
    }
    na_values <- attr(x, "na_values", exact = TRUE)
    na_range <- attr(x, "na_range", exact = TRUE)
    attr(x, "na_values") <- NULL
    attr(x, "na_range") <- NULL
    class(x) <- setdiff(class(x), "haven_labelled_spss")
    if (!is.null(na_values)) {
      attr(x, "qes_na_values") <- na_values
    }
    if (!is.null(na_range)) {
      attr(x, "qes_na_range") <- na_range
    }
    data[[j]] <- x
  }
  data
}

# ---- whole-number codes stored as text -----------------------------------------------

.qes_whole_number <- "^-?[0-9]+$"

# Is every non-blank value of text column `x` a whole number?
.qes_is_number_text <- function(x) {
  v <- trimws(.qes_plain(x))
  v <- v[!is.na(v) & nzchar(v)]
  all(grepl(.qes_whole_number, v))
}

# A text column of whole-number codes as a numeric column, blanks as NA:
# qesR 0.4.4 read them as numbers (its text reader converted the quoted codes
# of Dataverse's .tab file, whose DDI declares them as text). Value labels
# and declared user-missing values follow.
.qes_text_to_number <- function(x) {
  v <- trimws(.qes_plain(x))
  v[!is.na(v) & !nzchar(v)] <- NA_character_
  y <- as.numeric(v)
  number_of <- function(z) {
    z <- trimws(as.character(z))
    stats::setNames(ifelse(grepl(.qes_whole_number, z), z, NA_character_), names(z))
  }
  labels <- attr(x, "labels", exact = TRUE)
  label <- attr(x, "label", exact = TRUE)
  if (length(labels) > 0L) {
    codes <- number_of(labels)
    keep <- !is.na(codes)
    y <- haven::labelled(y, labels = stats::setNames(as.numeric(codes[keep]), names(labels)[keep]), label = label)
  } else if (!is.null(label)) {
    attr(y, "label") <- label
  }
  for (nm in c("qes_na_values", "qes_na_range")) {
    na <- attr(x, nm, exact = TRUE)
    if (!is.null(na)) {
      na <- as.numeric(number_of(na))
      if (!anyNA(na)) {
        attr(y, nm) <- na
      }
    }
  }
  y
}

# The catalog's type fixes for one file (type_fixes.csv). `variables` is a
# ";"-list of source names, or "*" for every text column whose values are
# all whole numbers.
.qes_apply_type_fixes <- function(data, file_row, demo = FALSE) {
  fixes <- .qes_catalog(demo = demo)$type_fixes
  fixes <- fixes[fixes$file_id == file_row$file_id & fixes$to == "numeric", , drop = FALSE]
  for (i in seq_len(nrow(fixes))) {
    is_text <- vapply(data, function(x) is.character(.qes_plain(x)), logical(1))
    if (identical(fixes$variables[i], "*")) {
      vars <- names(data)[is_text]
      vars <- vars[vapply(data[vars], .qes_is_number_text, logical(1))]
    } else {
      vars <- .qes_split_list(fixes$variables[i])
      ok <- vars %in% names(data)[is_text]
      ok[ok] <- vapply(data[vars[ok]], .qes_is_number_text, logical(1))
      if (!all(ok)) {
        stop(sprintf(
          "qesR internal error: type fix of file %s does not apply to %s.",
          file_row$file_id, paste(vars[!ok], collapse = ", ")
        ), call. = FALSE)
      }
    }
    for (v in vars) {
      data[[v]] <- .qes_text_to_number(data[[v]])
    }
  }
  data
}

# ---- names -----------------------------------------------------------------------

# Renames of name_map.csv for one file. Returns the data with attribute
# name_map_applied (the number of renamed columns).
.qes_apply_name_map <- function(data, file_id, demo = FALSE) {
  map <- .qes_catalog(demo = demo)$name_map
  map <- map[map$file_id == file_id, , drop = FALSE]
  idx <- match(map$source_name, names(data))
  hit <- !is.na(idx)
  names(data)[idx[hit]] <- map$name[hit]
  attr(data, "name_map_applied") <- sum(hit)
  data
}

# ---- encoding tripwire -----------------------------------------------------------

.qes_bad_text <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0L) {
    return(0L)
  }
  valid <- validUTF8(x)
  pattern <- paste0("[", intToUtf8(0xFFFD), intToUtf8(0x80), "-", intToUtf8(0x9F), "]")
  sum(!valid) + sum(grepl(pattern, enc2utf8(x[valid]), perl = TRUE))
}

# Count labels and text values that still hold U+FFFD or a C1 control
# character (or are not valid UTF-8), and warn once for the file.
.qes_encoding_tripwire <- function(data, file_row) {
  counts <- vapply(data, function(x) {
    n <- .qes_bad_text(attr(x, "label", exact = TRUE))
    n <- n + .qes_bad_text(names(attr(x, "labels", exact = TRUE)))
    if (is.character(x)) {
      n <- n + .qes_bad_text(unique(x))
    }
    n
  }, integer(1))
  n <- sum(counts) + .qes_bad_text(names(data))
  if (n > 0L) {
    variables <- names(data)[counts > 0L]
    .qes_warn(
      "encoding",
      class = "qesR_warning_encoding",
      args = list(.qes_q(file_row$study), .qes_q(file_row$file_id), n, .qes_q(utils::head(variables, 5L))),
      data = list(study = file_row$study, file_id = file_row$file_id, n = n, variables = variables)
    )
  }
  invisible(n)
}

# ---- provenance ------------------------------------------------------------------

.qes_versions <- function() {
  v <- tryCatch(read.dcf(.qes_extdata("VERSIONS")), error = function(e) NULL)
  get <- function(field) {
    if (is.null(v) || !(field %in% colnames(v))) NA_character_ else unname(v[1, field])
  }
  list(catalog_version = get("catalog_version"), dict_version = get("dict_version"))
}

# One row of study-level provenance (design.md section 5.9), shared by the
# reader, qes_download() and qes_provenance(<codes>). `file_row` is the
# files.csv row of the file (or, for qes_download(version = "latest"), the
# row describing the latest file); fields that do not apply are NA.
.qes_provenance_row <- function(study_row, file_row,
                                md5_observed = NA_character_, md5_verified = NA,
                                pinned = TRUE, retrieved_via = NA_character_,
                                retrieved_at = NA, label_source = NA_character_,
                                label_file_id = NA_character_,
                                name_map_applied = NA_integer_,
                                reader = NA_character_, haven_version = NA_character_) {
  versions <- .qes_versions()
  when <- if (length(retrieved_at) == 1L && !is.na(retrieved_at)) {
    as.POSIXct(format(retrieved_at, tz = "UTC"), tz = "UTC")
  } else {
    as.POSIXct(NA, tz = "UTC")
  }
  data.frame(
    study = study_row$study,
    doi = study_row$doi,
    dataset_version = file_row$dataset_version %||% study_row$dataset_version,
    file_id = file_row$file_id,
    file_name = file_row$original_file_name,
    format = file_row$format,
    md5_expected = file_row$md5,
    md5_observed = as.character(md5_observed),
    md5_verified = as.logical(md5_verified),
    unf = file_row$unf,
    n_rows = file_row$n_rows,
    n_cols = file_row$n_cols,
    pinned = as.logical(pinned),
    retrieved_via = as.character(retrieved_via),
    retrieved_at = when,
    licence = study_row$licence,
    label_source = as.character(label_source),
    label_file_id = as.character(label_file_id),
    name_map_applied = as.integer(name_map_applied),
    reader = as.character(reader),
    haven_version = as.character(haven_version),
    catalog_version = versions$catalog_version,
    dict_version = versions$dict_version,
    stringsAsFactors = FALSE
  )
}

# The study-level provenance of data read from `file_row`; qes_provenance()
# returns it.
.qes_read_provenance <- function(study_row, file_row, donor, path, label_source, name_map_applied) {
  sources <- unique(label_source[label_source != "none"])
  sources <- sources[order(match(sources, c("label_donor", "file", "file_malformed")))]
  .qes_provenance_row(
    study_row, file_row,
    md5_observed = file_row$md5,
    md5_verified = TRUE,
    pinned = TRUE,
    retrieved_via = attr(path, "retrieved_via", exact = TRUE) %||% NA_character_,
    retrieved_at = Sys.time(),
    label_source = if (length(sources) == 0L) "none" else paste(sources, collapse = ";"),
    label_file_id = if (is.null(donor)) NA_character_ else donor$file_id,
    name_map_applied = name_map_applied,
    reader = .qes_reader_call(file_row$format),
    haven_version = as.character(utils::packageVersion("haven"))
  )
}

# ---- the reader ------------------------------------------------------------------

# Read the data file `file_id` (default: the study's pinned data file) of
# `study`, a canonical code. `cols` keeps only those columns (after renames).
# Returns a base data.frame with attributes qes_survey_code, qes_provenance
# and qes_label_source.
.qes_read <- function(study, file_id = NULL, cols = NULL, quiet = TRUE) {
  demo <- .qes_is_demo_code(study)
  cat <- .qes_catalog(demo = demo)
  study_row <- cat$studies[match(study, cat$studies$study), , drop = FALSE]
  if (nrow(study_row) != 1L || is.na(study_row$study)) {
    stop(sprintf("qesR internal error: unknown study '%s'.", study), call. = FALSE)
  }
  file_row <- if (is.null(file_id)) {
    .qes_default_data_file(study, demo = demo)
  } else {
    .qes_file_row(study, file_id, demo = demo)
  }
  if (!(tolower(file_row$format) %in% .qes_read_formats)) {
    .qes_abort(
      "source_format",
      class = "qesR_error_source",
      args = list(.qes_q(file_row$format)),
      data = list(study = study, file_id = file_row$file_id, format = file_row$format)
    )
  }
  donor <- .qes_label_donor(study_row, file_row, demo = demo)
  key <- paste0(file_row$md5, if (!is.null(donor)) paste0("+", donor$md5))

  data <- .qes_memo_get(key)
  if (is.null(data)) {
    data <- .qes_read_uncached(study_row, file_row, donor, demo = demo, quiet = quiet)
    .qes_memo_set(key, data, study = study)
  }

  if (!is.null(cols)) {
    missing_cols <- setdiff(cols, names(data))
    if (length(missing_cols) > 0L) {
      .qes_abort(
        "unknown_variable",
        class = "qesR_error_unknown_variable",
        args = list(.qes_q(missing_cols)),
        data = list(study = study, variables = missing_cols, suggestions = character(0))
      )
    }
    keep <- attributes(data)[c("qes_survey_code", "qes_provenance", "qes_label_source")]
    data <- data[, cols, drop = FALSE]
    keep$qes_label_source <- keep$qes_label_source[cols]
    for (nm in names(keep)) {
      attr(data, nm) <- keep[[nm]]
    }
  }
  data
}

.qes_read_uncached <- function(study_row, file_row, donor, demo = FALSE, quiet = TRUE) {
  path <- .qes_local_file(study_row, file_row, quiet = quiet)
  on.exit(.qes_release_local_file(path), add = TRUE)
  data <- .qes_read_file(path, file_row$format, file_row$encoding,
    study = file_row$study, file_id = file_row$file_id)
  .qes_check_dims(data, file_row)

  labelled <- .qes_file_labels(data)
  data <- labelled$data
  source <- labelled$source
  data <- .qes_apply_text_fixes(data, .qes_text_fixes(file_row$file_id, demo = demo))

  if (!is.null(donor)) {
    donor_path <- .qes_local_file(study_row, donor, quiet = quiet)
    on.exit(.qes_release_local_file(donor_path), add = TRUE)
    donor_data <- .qes_read_file(donor_path, donor$format, donor$encoding,
      study = donor$study, file_id = donor$file_id)
    .qes_check_dims(donor_data, donor)
    donor_data <- .qes_file_labels(donor_data)$data
    donor_data <- .qes_apply_text_fixes(donor_data, .qes_text_fixes(donor$file_id, demo = demo))
    donated <- .qes_apply_donor(data, donor_data, source)
    data <- donated$data
    source <- donated$source
  }

  data <- .qes_unspss(data)
  data <- .qes_apply_type_fixes(data, file_row, demo = demo)
  data <- .qes_apply_name_map(data, file_row$file_id, demo = demo)
  renamed <- attr(data, "name_map_applied", exact = TRUE)
  attr(data, "name_map_applied") <- NULL
  names(source) <- names(data)
  .qes_encoding_tripwire(data, file_row)

  attr(data, "qes_survey_code") <- study_row$study
  attr(data, "qes_provenance") <- .qes_read_provenance(study_row, file_row, donor, path, source, renamed)
  attr(data, "qes_label_source") <- source
  data
}
