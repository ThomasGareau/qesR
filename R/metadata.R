# Variable metadata: the dictionary (design.md section 6, slice S3).
#
# Codebooks, question text, bilingual search and missing-value codes all read
# one dictionary of two tables:
#   variables  one row per (study, variable): position, source name, type,
#              measure, timing, the file's variable label, the question text
#              in English and French with its source and document reference,
#              declared missing codes, label source, review flag;
#   values     one row per (study, variable, value): the value label, its
#              source and language, the missing type (the NA vocabulary of
#              enums.csv) and the unweighted count n.
#
# Where the tables come from:
#   * CC0 studies (studies.metadata_shipped = TRUE): shipped in
#     inst/extdata/dict/{variables,values}.csv.gz (qes_demo in
#     inst/extdata/demo/dict/), built by data-raw/build_dictionary.R from the
#     pinned original files and the curated data-raw/questions/*.csv. Offline.
#   * Other studies (qes2022, CC BY-NC 4.0, decision OD3): nothing is shipped.
#     The tables are built on first use from the user's own md5-verified copy
#     of the pinned data file and kept as a "shard" of two CSV files in the
#     download cache (never RDS). Missing codes come from the shipped
#     dict/shard_rules.csv, which holds variable names and codes only, no
#     label text.
#   * Anything else (a study of a test catalog): built from its data in
#     memory.
# Labels never come from DDI metadata or from a variable name: a variable
# label that only repeats the variable's name (253 of 254 in qes2018) is no
# label ([A:K4]).

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    y
  } else {
    x
  }
}

.squish_ws <- function(x) {
  gsub("\\s+", " ", trimws(x))
}

.assert_single_string <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    .qes_abort(
      "input_string",
      class = "qesR_error_input",
      args = list(arg_name),
      data = list(arg = arg_name, value = x)
    )
  }
}

# Schema version of a metadata shard; part of its cache file name, so a new
# schema never reads an old shard.
.qes_shard_schema <- "1"

# ---- codes and text --------------------------------------------------------------

# A code as text: whole numbers without decimals or exponent ("100000", "-99"),
# other numbers with up to 15 significant digits, text unchanged.
.qes_code_chr <- function(v) {
  if (is.character(v)) {
    return(v)
  }
  v <- as.numeric(v)
  out <- rep(NA_character_, length(v))
  ok <- !is.na(v)
  whole <- ok & v == round(v) & abs(v) < 1e15
  out[whole] <- sprintf("%.0f", v[whole])
  out[whole & out == "-0"] <- "0"
  other <- ok & !whole
  out[other] <- vapply(v[other], function(z) format(z, digits = 15, scientific = FALSE), character(1))
  out
}

# Accent and case folding for search (design.md section 6.3). The table maps
# code points, built here from integers, so it depends on no locale: accents
# are folded first, then A-Z is lower-cased, then whitespace is squished.
.qes_fold_from <- c(
  0xC0:0xC5, 0xC7, 0xC8:0xCB, 0xCC:0xCF, 0xD1, 0xD2:0xD6, 0xD8, 0xD9:0xDC, 0xDD,
  0xE0:0xE5, 0xE7, 0xE8:0xEB, 0xEC:0xEF, 0xF1, 0xF2:0xF6, 0xF8, 0xF9:0xFC, 0xFD, 0xFF,
  0x152, 0x153, 0xC6, 0xE6, 0xDF,
  0x2018, 0x2019, 0x201C, 0x201D, 0xAB, 0xBB, 0xA0, 0x202F, 0x2013, 0x2014, 0x2026
)
.qes_fold_to <- c(
  rep("a", 6), "c", rep("e", 4), rep("i", 4), "n", rep("o", 5), "o", rep("u", 4), "y",
  rep("a", 6), "c", rep("e", 4), rep("i", 4), "n", rep("o", 5), "o", rep("u", 4), "y", "y",
  "oe", "oe", "ae", "ae", "ss",
  "'", "'", "\"", "\"", "\"", "\"", " ", " ", "-", "-", "..."
)

# User input in UTF-8. In a UTF-8 locale this is enc2utf8(). In a C or other
# non-UTF-8, non-Latin-1 locale, a string typed or pasted at the console
# arrives as unmarked ("unknown") bytes that are usually UTF-8, and
# enc2utf8() would turn "\u00e9" into the text "<c3><a9>"; such bytes are
# marked as UTF-8 when validUTF8() accepts them. In a Latin-1 locale unmarked
# bytes are Latin-1 and are translated as usual.
.qes_as_utf8 <- function(x) {
  x <- as.character(x)
  unmarked <- !is.na(x) & Encoding(x) == "unknown"
  if (any(unmarked) && !isTRUE(l10n_info()[["UTF-8"]]) && !isTRUE(l10n_info()[["Latin-1"]])) {
    utf8 <- unmarked & validUTF8(x)
    Encoding(x[utf8]) <- "UTF-8"
  }
  enc2utf8(x)
}

.qes_fold <- function(x) {
  x <- .qes_as_utf8(x)
  out <- vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    cp <- utf8ToInt(s)
    if (length(cp) == 0L || anyNA(cp)) {
      return(if (length(cp) == 0L) "" else NA_character_)
    }
    hit <- match(cp, .qes_fold_from)
    upper <- cp >= 65L & cp <= 90L
    cp[upper] <- cp[upper] + 32L
    if (all(is.na(hit))) {
      return(intToUtf8(cp))
    }
    parts <- intToUtf8(cp, multiple = TRUE)
    parts[!is.na(hit)] <- .qes_fold_to[hit[!is.na(hit)]]
    paste(parts, collapse = "")
  }, character(1), USE.NAMES = FALSE)
  .squish_ws(out)
}

# Accents only (for regex patterns, whose escapes must keep their case).
.qes_unaccent <- function(x) {
  x <- .qes_as_utf8(x)
  vapply(x, function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    cp <- utf8ToInt(s)
    if (length(cp) == 0L || anyNA(cp)) {
      return(s)
    }
    hit <- match(cp, .qes_fold_from)
    if (all(is.na(hit))) {
      return(s)
    }
    parts <- intToUtf8(cp, multiple = TRUE)
    parts[!is.na(hit)] <- .qes_fold_to[hit[!is.na(hit)]]
    paste(parts, collapse = "")
  }, character(1), USE.NAMES = FALSE)
}

# ---- building the tables from data --------------------------------------------------

.qes_dict_empty <- function(table) {
  schema <- .qes_schemas[[table]]
  out <- lapply(schema, function(type) {
    switch(type, int = integer(0), num = numeric(0), lgl = logical(0), character(0))
  })
  as.data.frame(out, stringsAsFactors = FALSE)
}

.qes_storage <- function(x) {
  if (inherits(x, "POSIXt")) {
    return("datetime")
  }
  if (inherits(x, "Date")) {
    return("date")
  }
  base <- typeof(.qes_plain(x))
  if (base %in% c("double", "integer")) "numeric" else if (base == "logical") "logical" else "character"
}

.qes_na_string <- function(x) {
  vals <- attr(x, "qes_na_values", exact = TRUE) %||% attr(x, "na_values", exact = TRUE)
  range <- attr(x, "qes_na_range", exact = TRUE) %||% attr(x, "na_range", exact = TRUE)
  parts <- character(0)
  if (length(vals) > 0L) {
    parts <- c(parts, .qes_code_chr(unclass(vals)))
  }
  if (length(range) == 2L) {
    parts <- c(parts, paste0(.qes_code_chr(range[1]), "..", .qes_code_chr(range[2])))
  }
  if (length(parts) == 0L) NA_character_ else paste(parts, collapse = ";")
}

# Is `code` (text) declared missing by `na_values` (the string above)?
.qes_is_declared <- function(code, na_values) {
  if (length(na_values) != 1L || is.na(na_values)) {
    return(rep(FALSE, length(code)))
  }
  parts <- .qes_split_list(na_values)
  ranges <- parts[grepl("\\.\\.", parts)]
  out <- code %in% setdiff(parts, ranges)
  num <- suppressWarnings(as.numeric(code))
  for (r in ranges) {
    lim <- suppressWarnings(as.numeric(strsplit(r, "..", fixed = TRUE)[[1]]))
    out <- out | (!is.na(num) & num >= lim[1] & num <= lim[2])
  }
  out
}

# The dictionary tables of a data frame read by .qes_read() (or of any data
# frame with labelled columns). `study_row` and `file_row` are catalog rows;
# the labels, their sources and the missing declarations come from the data.
# Question text is left NA: it comes only from the curated files or, for a
# shard, from the file's own label (see .qes_shard_build()).
.qes_dict_build <- function(data, study_row, file_row = NULL) {
  study <- study_row$study
  vars <- names(data)
  n_var <- length(vars)
  src <- attr(data, "qes_label_source", exact = TRUE)
  src <- if (is.null(src)) rep(NA_character_, n_var) else unname(src[vars])
  lang <- study_row$source_lang %||% NA_character_

  source_name <- vars
  if (!is.null(file_row)) {
    map <- .qes_catalog(demo = isTRUE(study_row$demo))$name_map
    map <- map[map$file_id == file_row$file_id, , drop = FALSE]
    hit <- match(vars, map$name)
    source_name[!is.na(hit)] <- map$source_name[hit[!is.na(hit)]]
  }
  id_vars <- if (is.null(file_row)) character(0) else .qes_split_list(file_row$id_vars)
  single <- (study_row$study_design %||% NA_character_) %in% c("post", "pre")

  col_type <- character(n_var)
  col_measure <- character(n_var)
  col_label <- rep(NA_character_, n_var)
  col_na <- rep(NA_character_, n_var)
  col_source <- character(n_var)
  val_rows <- vector("list", n_var)
  for (j in seq_len(n_var)) {
    v <- vars[j]
    x <- data[[j]]
    type <- .qes_storage(x)
    label <- attr(x, "label", exact = TRUE)
    label <- if (is.character(label) && length(label) >= 1L && !is.na(label[1])) .squish_ws(label[1]) else NA_character_
    if (!is.na(label) && (!nzchar(label) || identical(tolower(label), tolower(v)))) {
      label <- NA_character_
    }
    labels <- attr(x, "labels", exact = TRUE)
    has_labels <- length(labels) > 0L
    source <- src[j]
    if (is.na(source)) {
      source <- if (!is.na(label) || has_labels) "file" else "none"
    }
    if (is.na(label) && !has_labels) {
      source <- "none"
    }
    if (identical(source, "file_malformed") && is.na(label)) {
      source <- if (has_labels) "file" else "none"
    }
    na_values <- .qes_na_string(x)
    col_type[j] <- type
    col_label[j] <- label
    col_na[j] <- na_values
    col_source[j] <- source
    col_measure[j] <- if (v %in% id_vars) {
      "id"
    } else if (grepl("(^|_)(pond|poids|ponder|weight|xpond)", tolower(v))) {
      "weight"
    } else if (type %in% c("date", "datetime")) {
      "admin"
    } else if (type == "character") {
      "text"
    } else if (has_labels) {
      "nominal"
    } else {
      "interval"
    }

    # values: every labelled code, plus every observed code of a numeric
    # column with at most 50 distinct codes; n is the unweighted count
    base <- .qes_plain(x)
    codes <- character(0)
    code_labels <- character(0)
    if (has_labels) {
      codes <- .qes_code_chr(unclass(unname(labels)))
      code_labels <- .squish_ws(names(labels))
      keep <- !is.na(codes) & !duplicated(codes)
      codes <- codes[keep]
      code_labels <- code_labels[keep]
    }
    observed <- character(0)
    counts <- NULL
    if (type %in% c("numeric", "character", "logical")) {
      key <- if (type == "numeric") .qes_code_chr(base) else as.character(base)
      key <- key[!is.na(key)]
      counts <- table(key)
      if (type == "numeric" && length(counts) <= 50L) {
        observed <- names(counts)
      }
    }
    all_codes <- unique(c(codes, observed))
    if (length(all_codes) == 0L) {
      next
    }
    if (type == "numeric") {
      all_codes <- all_codes[order(suppressWarnings(as.numeric(all_codes)))]
    }
    lab <- code_labels[match(all_codes, codes)]
    n <- if (is.null(counts)) rep(NA_integer_, length(all_codes)) else as.integer(counts[all_codes])
    n[is.na(n) & !is.null(counts)] <- 0L
    val_source <- ifelse(is.na(lab), "none", if (identical(source, "label_donor")) "label_donor" else "file")
    declared <- .qes_is_declared(all_codes, na_values)
    val_rows[[j]] <- data.frame(
      study = study, variable = v, value = all_codes, label = lab,
      label_source = val_source,
      label_lang = ifelse(is.na(lab), NA_character_, lang),
      label_en = if (identical(lang, "en")) lab else NA_character_,
      label_fr = if (identical(lang, "fr")) lab else NA_character_,
      missing_type = ifelse(declared, "user_na", NA_character_),
      n = n, label_flag = NA_character_,
      stringsAsFactors = FALSE
    )
  }
  variables <- if (n_var == 0L) .qes_dict_empty("dict_variables") else data.frame(
    study = rep(study, n_var), variable = vars, position = seq_len(n_var),
    source_name = source_name, type = col_type, measure = col_measure,
    var_timing = rep(if (single) "single" else NA_character_, n_var),
    label = col_label, question_en = NA_character_, question_fr = NA_character_,
    question_truncated = rep(NA, n_var), universe_en = NA_character_,
    universe_fr = NA_character_, na_values = col_na, derived_from = NA_character_,
    label_source = col_source, question_source = NA_character_,
    doc_ref = NA_character_, reviewed = rep(FALSE, n_var),
    stringsAsFactors = FALSE
  )
  values <- do.call(rbind, val_rows[!vapply(val_rows, is.null, logical(1))])
  if (is.null(values)) {
    values <- .qes_dict_empty("dict_values")
  }
  rownames(variables) <- NULL
  rownames(values) <- NULL
  list(variables = variables, values = values)
}

# ---- the shipped dictionary ------------------------------------------------------

.qes_dict_cache <- new.env(parent = emptyenv())

.qes_dict_dir <- function(demo = FALSE) {
  if (isTRUE(demo)) .qes_extdata("demo", "dict") else .qes_extdata("dict")
}

# Read the two dictionary tables of `dir` (shipped or a shard).
.qes_dict_read <- function(var_path, val_path) {
  variables <- .qes_read_csv(var_path, "dict_variables")
  values <- .qes_read_csv(val_path, "dict_values")
  values$label[values$label_source %in% "none"] <- NA_character_
  list(variables = variables, values = values)
}

# The shipped dictionary (main or demo tree), read once per session.
.qes_dict_shipped <- function(demo = FALSE) {
  key <- if (isTRUE(demo)) "demo" else "main"
  if (is.null(.qes_dict_cache[[key]])) {
    dir <- .qes_dict_dir(demo)
    .qes_dict_cache[[key]] <- .qes_dict_read(
      file.path(dir, "variables.csv.gz"), file.path(dir, "values.csv.gz")
    )
  }
  .qes_dict_cache[[key]]
}

.qes_shard_rules <- function() {
  if (is.null(.qes_dict_cache$rules)) {
    .qes_dict_cache$rules <- .qes_read_csv(file.path(.qes_dict_dir(), "shard_rules.csv"), "dict_shard_rules")
  }
  .qes_dict_cache$rules
}

.qes_dict_subset <- function(dict, study) {
  list(
    variables = dict$variables[dict$variables$study == study, , drop = FALSE],
    values = dict$values[dict$values$study == study, , drop = FALSE]
  )
}

# ---- shards (studies whose metadata is not shipped) ---------------------------------

# Is a variable label (as stored in the file) cut at the 80-character limit
# of Stata labels? Exactly 80 characters, or 79 when the cut fell on a space
# that Stata then dropped (the label then ends without closing punctuation):
# qes2022 `cps_age_in_years` ends "... we need to get a".
.qes_label_cut <- function(raw) {
  raw <- sub("\\s+$", "", raw)
  n <- nchar(raw, type = "chars")
  !is.na(raw) & (n >= 80L | (n == 79L & !grepl("[?.!:)]$", raw)))
}

# Shard tables built from the data: question text is the file's own variable
# label, flagged as truncated when the Stata limit cut it (.qes_label_cut(),
# 373 of 718 qes2022 labels have exactly 80 characters) with a reference to
# the study's codebook document; missing codes follow shard_rules.csv.
.qes_shard_build <- function(data, study_row, file_row) {
  dict <- .qes_dict_build(data, study_row, file_row)
  v <- dict$variables
  lang <- study_row$source_lang
  q_col <- if (identical(lang, "fr")) "question_fr" else "question_en"
  has <- !is.na(v$label)
  raw <- vapply(data, function(x) {
    l <- attr(x, "label", exact = TRUE)
    if (is.character(l) && length(l) >= 1L) l[1] else NA_character_
  }, character(1), USE.NAMES = FALSE)
  v[[q_col]][has] <- v$label[has]
  v$question_truncated[has] <- .qes_label_cut(raw[has])
  v$question_source[has] <- "file"
  docs <- .qes_catalog(demo = isTRUE(study_row$demo))$files
  docs <- docs[docs$study == study_row$study & docs$role == "codebook", , drop = FALSE]
  if (nrow(docs) > 0L) {
    v$doc_ref[has] <- docs$file_id[1]
  }
  dict$variables <- v
  dict$values <- .qes_apply_shard_rules(dict$values, study_row$study)
  dict
}

# Apply the shipped missing-code rules of `study` to its values table: a rule
# for a named variable wins over a rule for "*".
.qes_apply_shard_rules <- function(values, study) {
  rules <- tryCatch(.qes_shard_rules(), error = function(e) NULL)
  if (is.null(rules) || nrow(values) == 0L) {
    return(values)
  }
  rules <- rules[rules$study == study, , drop = FALSE]
  if (nrow(rules) == 0L) {
    return(values)
  }
  specific <- rules[rules$variable != "*", , drop = FALSE]
  generic <- rules[rules$variable == "*", , drop = FALSE]
  hit <- match(paste(values$variable, values$value), paste(specific$variable, specific$value))
  typed <- !is.na(hit)
  values$missing_type[typed] <- specific$missing_type[hit[typed]]
  hit_g <- match(values$value, generic$value)
  use_g <- !typed & !is.na(hit_g)
  values$missing_type[use_g] <- generic$missing_type[hit_g[use_g]]
  values
}

# Cache paths of the shard of `study` for its pinned file `file_row`, or NULL
# when no cache is active.
.qes_shard_paths <- function(study, file_row) {
  root <- .qes_cache_root(.qes_cache_mode())
  if (is.na(root)) {
    return(NULL)
  }
  c(
    variables = .qes_cache_shard_path(root, study, file_row$md5, .qes_shard_schema, "variables"),
    values = .qes_cache_shard_path(root, study, file_row$md5, .qes_shard_schema, "values")
  )
}

# The shard of `study` if it is in memory or in the cache; NULL otherwise.
# Never builds, reads data or downloads anything.
.qes_shard_cached <- function(study, file_row = NULL) {
  file_row <- file_row %||% .qes_default_data_file(study, demo = .qes_is_demo_code(study))
  memo_key <- paste0("shard:", study, ":", file_row$md5)
  if (!is.null(.qes_dict_cache[[memo_key]])) {
    return(.qes_dict_cache[[memo_key]])
  }
  paths <- .qes_shard_paths(study, file_row)
  if (is.null(paths) || !all(file.exists(paths))) {
    return(NULL)
  }
  dict <- tryCatch(.qes_dict_read(paths[["variables"]], paths[["values"]]), error = function(e) NULL)
  if (is.null(dict) || !all(dict$variables$study == study)) {
    return(NULL)
  }
  .qes_dict_cache[[memo_key]] <- dict
  dict
}

# The shard of `study` for its pinned data file: read from the cache, or
# built from the data (read through the cache) and written there. `data` may
# be passed when the caller has already read the whole pinned file.
.qes_shard <- function(study, data = NULL, quiet = TRUE) {
  demo <- .qes_is_demo_code(study)
  study_row <- .qes_study_row(study, demo = demo)
  file_row <- .qes_default_data_file(study, demo = demo)
  dict <- .qes_shard_cached(study, file_row)
  if (!is.null(dict)) {
    return(dict)
  }
  if (is.null(data)) {
    data <- .qes_read(study, file_row$file_id, quiet = quiet)
  }
  dict <- .qes_shard_build(data, study_row, file_row)
  paths <- .qes_shard_paths(study, file_row)
  if (!is.null(paths)) {
    mode <- .qes_cache_mode()
    .qes_cache_prepare(.qes_cache_root(mode), mode, quiet = quiet)
    dir.create(dirname(paths[["variables"]]), recursive = TRUE, showWarnings = FALSE)
    written <- tryCatch({
      .qes_write_csv(dict$variables, paths[["variables"]])
      .qes_write_csv(dict$values, paths[["values"]])
      TRUE
    }, error = function(e) FALSE)
    if (!isTRUE(written)) {
      unlink(paths)
    }
  }
  .qes_dict_cache[[paste0("shard:", study, ":", file_row$md5)]] <- dict
  dict
}

# The shards already in the cache for the pinned files, without building or
# downloading anything (for qes_search()).
.qes_cached_shards <- function() {
  out <- list()
  studies <- .qes_catalog()$studies
  for (study in studies$study[!studies$metadata_shipped %in% TRUE]) {
    file_row <- tryCatch(.qes_default_data_file(study), error = function(e) NULL)
    if (is.null(file_row)) {
      next
    }
    dict <- .qes_shard_cached(study, file_row)
    if (!is.null(dict)) {
      out[[study]] <- dict
    }
  }
  out
}

# Forget the metadata built in this session (shards, tables built from data
# and search indexes), for `studies` or for every study. The shipped
# dictionary stays: it is part of the package.
.qes_dict_forget <- function(studies = NULL) {
  keys <- ls(.qes_dict_cache, all.names = TRUE)
  runtime <- keys[grepl("^(shard|data|index):", keys)]
  if (!is.null(studies)) {
    runtime <- runtime[sub("^[a-z]+:([^:]+):.*$", "\\1", runtime) %in% studies]
  }
  rm(list = runtime, envir = .qes_dict_cache)
  invisible(runtime)
}

# ---- the tables of one study ----------------------------------------------------------

# Where the metadata of `study` comes from: "shipped", "shard" or "data".
.qes_dict_origin <- function(study) {
  demo <- .qes_is_demo_code(study)
  row <- .qes_study_row(study, demo = demo)
  if (isTRUE(row$metadata_shipped)) {
    shipped <- .qes_dict_shipped(demo = demo)
    if (study %in% shipped$variables$study) {
      return("shipped")
    }
    return("data")
  }
  "shard"
}

# The dictionary tables of one study's pinned data file. `data`: the pinned
# file's data, when the caller has read it already (it saves a read for a
# shard or a study of a test catalog).
.qes_dict_study <- function(study, data = NULL, quiet = TRUE) {
  demo <- .qes_is_demo_code(study)
  switch(
    .qes_dict_origin(study),
    shipped = .qes_dict_subset(.qes_dict_shipped(demo = demo), study),
    shard = .qes_shard(study, data = data, quiet = quiet),
    data = {
      file_row <- .qes_default_data_file(study, demo = demo)
      key <- paste0("data:", study, ":", file_row$md5)
      if (is.null(.qes_dict_cache[[key]])) {
        if (is.null(data)) {
          data <- .qes_read(study, file_row$file_id, quiet = quiet)
        }
        .qes_dict_cache[[key]] <- .qes_dict_build(data, .qes_study_row(study, demo = demo), file_row)
      }
      .qes_dict_cache[[key]]
    }
  )
}

# Is `data` the whole pinned file `file_row` (every row and column, as
# get_qes() returns it)? Only then may metadata built from it be cached.
.qes_is_whole_file <- function(data, file_row) {
  prov <- attr(data, "qes_provenance", exact = TRUE)
  if (is.data.frame(prov) && "file_id" %in% names(prov) && nrow(prov) >= 1L &&
      !identical(as.character(prov$file_id[1]), as.character(file_row$file_id))) {
    return(FALSE)
  }
  n_rows <- suppressWarnings(as.integer(file_row$n_rows))
  n_cols <- suppressWarnings(as.integer(file_row$n_cols))
  !is.na(n_rows) && !is.na(n_cols) && nrow(data) == n_rows && ncol(data) == n_cols
}

# The dictionary tables of `study` for describing `data`, without reading or
# downloading anything: the shipped tables, else the shard or tables already
# in memory or in the cache, else tables built from the columns at hand (a
# shard's rules are keyed by variable name and its labels are on the
# columns). Tables built from the whole pinned file are kept like any shard;
# tables built from part of it are not.
.qes_dict_at_hand <- function(data, study, quiet = TRUE) {
  origin <- .qes_dict_origin(study)
  if (identical(origin, "shipped")) {
    return(.qes_dict_study(study, quiet = quiet))
  }
  demo <- .qes_is_demo_code(study)
  file_row <- .qes_default_data_file(study, demo = demo)
  dict <- if (identical(origin, "shard")) {
    .qes_shard_cached(study, file_row)
  } else {
    .qes_dict_cache[[paste0("data:", study, ":", file_row$md5)]]
  }
  if (!is.null(dict)) {
    return(dict)
  }
  if (.qes_is_whole_file(data, file_row)) {
    return(.qes_dict_study(study, data = data, quiet = quiet))
  }
  study_row <- .qes_study_row(study, demo = demo)
  if (identical(origin, "shard")) {
    .qes_shard_build(data, study_row, file_row)
  } else {
    .qes_dict_build(data, study_row, file_row)
  }
}

# The tables describing the columns of `data` (read from a file of `study`):
# the study's dictionary rows for the columns it has (matched exactly, else
# ignoring case, as for the SPSS twin of qes2012 whose names are upper case),
# renamed and ordered as in `data`; columns the dictionary does not know are
# described from the data itself.
.qes_dict_for_data <- function(data, study, dict = NULL, quiet = TRUE) {
  demo <- .qes_is_demo_code(study)
  dict <- dict %||% .qes_dict_at_hand(data, study, quiet = quiet)
  vars <- names(data)
  dv <- dict$variables
  idx <- match(vars, dv$variable)
  ci <- is.na(idx)
  if (any(ci)) {
    lower <- match(tolower(vars[ci]), tolower(dv$variable))
    idx[ci] <- lower
  }
  known <- !is.na(idx)
  variables <- dv[idx[known], , drop = FALSE]
  old_names <- variables$variable
  variables$variable <- vars[known]
  variables$position <- which(known)
  values <- dict$values[dict$values$variable %in% old_names, , drop = FALSE]
  values$variable <- vars[known][match(values$variable, old_names)]
  if (any(!known)) {
    extra <- .qes_dict_build(data[, !known, drop = FALSE], .qes_study_row(study, demo = demo))
    extra$variables$position <- which(!known)
    variables <- rbind(variables, extra$variables)
    values <- rbind(values, extra$values)
  }
  variables <- variables[order(variables$position), , drop = FALSE]
  values <- values[order(match(values$variable, variables$variable)), , drop = FALSE]
  rownames(variables) <- NULL
  rownames(values) <- NULL
  list(variables = variables, values = values)
}

# The study code recorded on a data frame (qes_survey_code, else the study of
# its provenance), or NULL.
.qes_data_study <- function(x) {
  code <- attr(x, "qes_survey_code", exact = TRUE)
  if (!(is.character(code) && length(code) == 1L && !is.na(code))) {
    prov <- attr(x, "qes_provenance", exact = TRUE)
    code <- if (is.data.frame(prov) && "study" %in% names(prov) && nrow(prov) >= 1L) prov$study[1] else NULL
  }
  if (is.null(code) || !(code %in% .qes_study_codes(demo = TRUE))) {
    return(NULL)
  }
  code
}

# Suggestions for an unknown variable name: the names equal to it ignoring
# case, then names that contain it or that it contains (ignoring case), then
# the closest by edit distance.
.qes_suggest_variables <- function(x, choices, n = 5L) {
  if (length(choices) == 0L) {
    return(character(0))
  }
  low <- tolower(choices)
  key <- tolower(x)
  same <- choices[low == key]
  contains <- choices[grepl(key, low, fixed = TRUE) | vapply(low, function(ch) grepl(ch, key, fixed = TRUE), logical(1))]
  d <- utils::adist(key, low)[1, ]
  close <- choices[order(d)][sort(d) <= max(2L, nchar(key) %/% 3L)]
  utils::head(unique(c(same, contains, close)), n)
}

# `scope`: "data" when the names are columns of a data frame the user passed,
# "study" when they are looked up in a study's metadata (the message then
# names the study instead of "the data").
.qes_abort_unknown_variables <- function(missing, choices, study = NA_character_, scope = c("data", "study")) {
  scope <- match.arg(scope)
  suggestions <- unique(unlist(lapply(missing, .qes_suggest_variables, choices = choices)))
  in_study <- identical(scope, "study") && length(study) == 1L && !is.na(study)
  data <- list(study = study, variables = missing, suggestions = suggestions)
  if (in_study) {
    if (length(suggestions) > 0L) {
      .qes_abort(
        "unknown_variable_study_suggest",
        class = "qesR_error_unknown_variable",
        args = list(.qes_q(missing), .qes_q(study), .qes_q(suggestions)),
        data = data
      )
    }
    .qes_abort(
      "unknown_variable_study",
      class = "qesR_error_unknown_variable",
      args = list(.qes_q(missing), .qes_q(study)),
      data = data
    )
  }
  if (length(suggestions) > 0L) {
    .qes_abort(
      "unknown_variable_suggest",
      class = "qesR_error_unknown_variable",
      args = list(.qes_q(missing), .qes_q(suggestions)),
      data = data
    )
  }
  .qes_abort(
    "unknown_variable",
    class = "qesR_error_unknown_variable",
    args = list(.qes_q(missing)),
    data = data
  )
}

# Resolve `variables` (exact names) against `choices`; unknown names are an
# error that suggests near matches. `scope` as for
# .qes_abort_unknown_variables().
.qes_match_variables <- function(variables, choices, study = NA_character_, arg = "variables",
                                 scope = c("data", "study")) {
  scope <- match.arg(scope)
  if (!is.character(variables) || length(variables) == 0L || anyNA(variables) || !all(nzchar(variables))) {
    .qes_abort(
      "input_variables",
      class = "qesR_error_input",
      args = list(arg),
      data = list(arg = arg, value = variables)
    )
  }
  missing <- setdiff(variables, choices)
  if (length(missing) > 0L) {
    .qes_abort_unknown_variables(missing, choices, study, scope = scope)
  }
  unique(variables)
}

# `lang` for returned text: NULL (the study's language) or "en"/"fr".
.qes_check_lang <- function(lang, arg = "lang", allow_null = TRUE, choices = c("en", "fr")) {
  if (is.null(lang) && allow_null) {
    return(NULL)
  }
  .qes_check_one(lang, arg, choices)
}

# The question text of each row of a variables table in `lang` (NULL: the
# study's source language, else the other language when only that one is
# known). Returns list(text, lang).
.qes_pick_question <- function(variables, lang, source_lang) {
  en <- variables$question_en
  fr <- variables$question_fr
  if (identical(lang, "en")) {
    return(list(text = en, lang = ifelse(is.na(en), NA_character_, "en")))
  }
  if (identical(lang, "fr")) {
    return(list(text = fr, lang = ifelse(is.na(fr), NA_character_, "fr")))
  }
  first <- if (identical(source_lang, "en")) en else fr
  second <- if (identical(source_lang, "en")) fr else en
  first_lang <- if (identical(source_lang, "en")) "en" else "fr"
  second_lang <- if (identical(first_lang, "en")) "fr" else "en"
  text <- ifelse(is.na(first), second, first)
  qlang <- ifelse(!is.na(first), first_lang, ifelse(!is.na(second), second_lang, NA_character_))
  list(text = text, lang = qlang)
}
