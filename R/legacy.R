# The interim legacy builders (design.md section 5.12, slice S4).
#
# get_qes_master() and get_decon() keep the harmonization code of qesR 0.4.4
# (R/master.R, R/decon.R), run it on the pinned original files, and change
# its output only by deletion: nothing is recoded that 0.4.4 did not recode.
# Five tables in inst/extdata/legacy/ drive it:
#
#   sources.csv  (profile, column, study, source_variable)
#       The source variable qesR 0.4.4 chose for each column of each study,
#       frozen from a clean 0.4.4 build (data-raw/build_legacy.R, from the
#       R9 baseline). Nothing is discovered at run time, so a change in the
#       reader (labels, names) can never change which variable is read.
#       "(synthetic_rowid)" means <study>_<row>, as 0.4.4 did where a file
#       has no usable respondent id. qes_demo gets qes2014's sources where
#       its file (a subset of qes2014's names) has the variable.
#   blanks.csv   (profile, column, study, codes, cause, basis)
#       Cells verified to be invalid, set to NA: every cell of the column in
#       that study when `codes` is empty, else the cells whose source code is
#       one of `codes` (";"-separated). `study` or `column` "*" means all.
#       `cause` is the assessment finding ([A:H*]) or owner decision (OD*);
#       `basis` says why, in English.
#   studies.csv  (study, vote_choice_timing, sovereignty_item, decon_vote_timing)
#       Per-study constants that say what the timing-sensitive columns hold.
#   columns.csv  (column, target, definition, flag, note)
#       The legacy_column_map attribute: what each master column means in
#       this release and the harmonized target that will fill it in 0.7.0.
#   removed.csv  (column, studies, source_variables)
#       The 70 columns qesR 0.4.4 appended by stacking raw variables that
#       share a name across studies ([A:H1]); no longer built.
#
# qes_demo uses qes2014's blanks and constants.

.qes_legacy_env <- new.env(parent = emptyenv())

.qes_legacy_table <- function(name) {
  if (is.null(.qes_legacy_env[[name]])) {
    path <- system.file("extdata", "legacy", paste0(name, ".csv"), package = "qesR", mustWork = TRUE)
    .qes_legacy_env[[name]] <- .qes_read_csv(path, paste0("legacy_", name))
  }
  .qes_legacy_env[[name]]
}

# The study whose blanks and constants apply.
.qes_legacy_key <- function(study) {
  if (identical(study, "qes_demo")) "qes2014" else study
}

# Named character vector: column -> frozen source variable (NA: none).
.qes_legacy_sources <- function(profile, study) {
  src <- .qes_legacy_table("sources")
  rows <- src[src$profile == profile & src$study == study, , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop(sprintf("qesR internal error: no frozen %s sources for '%s'.", profile, study), call. = FALSE)
  }
  stats::setNames(rows$source_variable, rows$column)
}

# The blank rules of `profile` that apply to `study`, with "*" columns
# expanded to `columns`.
.qes_legacy_blank_rules <- function(profile, study, columns) {
  b <- .qes_legacy_table("blanks")
  key <- .qes_legacy_key(study)
  b <- b[b$profile == profile & b$study %in% c(key, "*"), , drop = FALSE]
  if (nrow(b) == 0L) {
    return(b)
  }
  expanded <- lapply(seq_len(nrow(b)), function(i) {
    if (identical(b$column[i], "*")) {
      out <- b[rep(i, length(columns)), , drop = FALSE]
      out$column <- columns
      out
    } else {
      b[i, , drop = FALSE]
    }
  })
  out <- do.call(rbind, expanded)
  rownames(out) <- NULL
  out
}

# Which cells of each column to blank, from the raw source values:
# a named list column -> logical vector (length nrow(data)), plus attribute
# `rules` (the rules applied).
.qes_legacy_blank_masks <- function(data, study, profile, sources) {
  n <- nrow(data)
  rules <- .qes_legacy_blank_rules(profile, study, names(sources)[!is.na(sources)])
  masks <- list()
  for (i in seq_len(nrow(rules))) {
    col <- rules$column[i]
    codes <- .qes_split_list(rules$codes[i])
    mask <- if (length(codes) == 0L) {
      rep(TRUE, n)
    } else {
      src <- sources[[col]]
      if (is.na(src) || !(src %in% names(data))) {
        rep(FALSE, n)
      } else {
        values <- suppressWarnings(as.numeric(.qes_plain(data[[src]])))
        !is.na(values) & values %in% as.numeric(codes)
      }
    }
    masks[[col]] <- if (is.null(masks[[col]])) mask else (masks[[col]] | mask)
  }
  attr(masks, "rules") <- rules
  masks
}

# The per-study constants (vote_choice_timing, sovereignty_item,
# decon_vote_timing) as a one-row data.frame.
.qes_legacy_constants <- function(study) {
  s <- .qes_legacy_table("studies")
  row <- s[s$study == .qes_legacy_key(study), , drop = FALSE]
  if (nrow(row) != 1L) {
    stop(sprintf("qesR internal error: no legacy constants for '%s'.", study), call. = FALSE)
  }
  row
}

# The number of values each mask sets to NA: the masked cells of the built
# column that were not already NA.
.qes_legacy_blank_count <- function(x, mask) {
  as.integer(sum(mask & !is.na(x)))
}

# legacy_na_columns: one row per (column, study) of the loaded studies whose
# cells are NA by design: `reason` "no_source" (qesR 0.4.4 found no source
# either) or "blanked" (verified invalid, set to NA in qesR 0.5.0), with the
# number of cells (the whole column for "no_source"; the values set to NA
# for "blanked"), the cause and its basis. `counts` is a named integer
# vector, column -> values set to NA. A column whose rules all name codes
# and that had none of them is not listed: nothing was blanked there.
.qes_legacy_na_rows <- function(study, profile, sources, masks, n, counts) {
  none <- names(sources)[is.na(sources)]
  rules <- attr(masks, "rules")
  whole <- unique(rules$column[vapply(rules$codes, function(x) length(.qes_split_list(x)) == 0L, logical(1))])
  blanked <- names(masks)
  counts <- stats::setNames(as.integer(counts[blanked]), blanked)
  counts[is.na(counts)] <- 0L
  blanked <- blanked[blanked %in% whole | counts > 0L]
  first <- function(col, field) {
    r <- rules[rules$column == col, , drop = FALSE]
    paste(unique(r[[field]]), collapse = "; ")
  }
  out <- rbind(
    data.frame(
      column = none, study = rep(study, length(none)), reason = rep("no_source", length(none)),
      n_cells = rep(as.integer(n), length(none)),
      cause = rep(NA_character_, length(none)), basis = rep(NA_character_, length(none)),
      stringsAsFactors = FALSE
    ),
    data.frame(
      column = blanked, study = rep(study, length(blanked)), reason = rep("blanked", length(blanked)),
      n_cells = as.integer(unname(counts[blanked])),
      cause = vapply(blanked, first, character(1), field = "cause", USE.NAMES = FALSE),
      basis = vapply(blanked, first, character(1), field = "basis", USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
  )
  out <- out[!(out$reason == "blanked" & out$column %in% none), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# legacy_column_map: columns.csv plus `studies_changed`, the studies whose
# cells in that column were blanked in qesR 0.5.0.
.qes_legacy_column_map <- function() {
  map <- .qes_legacy_table("columns")
  b <- .qes_legacy_table("blanks")
  b <- b[b$profile == "master", , drop = FALSE]
  changed <- vapply(map$column, function(col) {
    s <- b$study[b$column == col]
    if (length(s) == 0L) NA_character_ else if ("*" %in% s) "all" else paste(s, collapse = ";")
  }, character(1))
  out <- data.frame(
    column = map$column,
    target = map$target,
    definition = map$definition,
    studies_changed = unname(changed),
    flag = map$flag,
    note = map$note,
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

# The interim builders are not the harmonization engine: qes_spec records
# that no spec was used, and the md5 of the frozen legacy tables.
.qes_legacy_spec <- function() {
  paths <- system.file("extdata", "legacy", paste0(c("sources", "blanks", "studies", "columns", "removed"), ".csv"),
                       package = "qesR", mustWork = TRUE)
  list(
    version = NA_character_,
    hash = NA_character_,
    custom = FALSE,
    engine = "legacy-interim",
    legacy_tables = stats::setNames(unname(tools::md5sum(paths)), basename(paths))
  )
}

# Once-per-session notices of the legacy builders (design.md sections 5.12
# and 7). They are shown whatever `quiet` says, like the assignment notice,
# and only once per session.
.qes_legacy_notice <- function(fn) {
  if (.qes_once_first("values_changed")) {
    .qes_inform("legacy_values_changed", class = "qesR_message_values_changed", data = list(fn = fn))
  }
  if (.qes_once_first(paste0("legacy_columns:", fn))) {
    if (identical(fn, "get_qes_master")) {
      .qes_inform(
        "legacy_master_columns",
        class = "qesR_message_legacy_columns",
        args = list(nrow(.qes_legacy_table("removed"))),
        data = list(fn = fn)
      )
    } else {
      .qes_inform("legacy_decon_columns", class = "qesR_message_legacy_columns", data = list(fn = fn))
    }
  }
  invisible()
}
