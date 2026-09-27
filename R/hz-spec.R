# The harmonization spec (design.md section 5, slice HZ1).
#
# The spec is a directory of UTF-8 CSV files, inst/extdata/harmonize/ in the
# installed package:
#   SPEC           DCF: Spec-Version, Spec-Date, Schema-Version, Engine-Min,
#                  Hash (md5 of the content of every CSV), Licence;
#   targets.csv    one row per harmonized target (EN/FR label and definition);
#   levels.csv     level sets: codes stable forever, EN/FR labels, aliases;
#                  a row named "inherits=<levels_id>" includes another set;
#   crosswalk.csv  one reviewed decision per (study, wave, target, source
#                  variable): rule, gate, grade and its reasons, instrument,
#                  offered levels, wording and evidence;
#   valuemaps.csv  source code -> target level or NA reason, with the source
#                  label (or, for studies whose metadata cannot ship, the md5
#                  of its normalized text);
#   waves.csv      study x wave membership, timing, dates and mode;
#   weights.csv    the weights registry;
#   CHANGES.csv    the spec changelog (EN/FR);
#   gates.csv      joint counts of gate code and source code among a wave's
#                  members, for the rows the dictionary alone cannot project
#                  (built by data-raw/build_sources.R from the pinned files);
#   expected/marginals.csv
#                  the projected unweighted marginals of every projectable
#                  row (data-raw/project_marginals.R), checked by V-P1.
# gates.csv and expected/ describe the shipped studies; a spec directory
# without them loads with empty tables. Slice HZ6 adds legacy.csv; the content
# hash covers every CSV of the directory, so it is hashed without code
# changes.
#
# .qes_spec_load() reads a directory through the one CSV loader and types it;
# .qes_spec_check() (R/hz-validate.R) runs the validator V-S1 to V-S17 on the
# loaded tables and .qes_data_check() (R/hz-data.R) the data checks V-D* on
# the shipped dictionary. qes_spec() is the entry point; the loaded and
# validated spec is kept for the session, keyed by the directory and the md5
# of its files.

# Schema version this engine reads, and the files of a spec directory.
.qes_spec_schema_version <- "1"
.qes_spec_files <- c(
  targets = "targets.csv", levels = "levels.csv", crosswalk = "crosswalk.csv",
  valuemaps = "valuemaps.csv", waves = "waves.csv", weights = "weights.csv",
  changes = "CHANGES.csv"
)
# Optional files (a spec directory may lack them) and their schemas.
.qes_spec_optional_files <- c(gates = "gates.csv", expected = "expected/marginals.csv")
.qes_spec_table_schema <- c(
  targets = "spec_targets", levels = "spec_levels", crosswalk = "spec_crosswalk",
  valuemaps = "spec_valuemaps", waves = "spec_waves", weights = "spec_weights",
  changes = "spec_changes", gates = "spec_gates", expected = "spec_expected"
)
.qes_spec_fields <- c("Spec-Version", "Spec-Date", "Schema-Version", "Engine-Min", "Hash", "Licence")

.qes_spec_cache <- new.env(parent = emptyenv())

# ---- errors -----------------------------------------------------------------

# A problems table (one row per problem), the shape the validator returns.
.qes_spec_problems <- function(rule = character(0), severity = character(0),
                               table = character(0), row = integer(0),
                               key = character(0), detail = character(0)) {
  data.frame(
    rule = as.character(rule), severity = as.character(severity),
    table = as.character(table), row = as.integer(row),
    key = as.character(key), detail = as.character(detail),
    stringsAsFactors = FALSE
  )
}

# Raise qesR_error_spec for a spec that has problems of severity "error".
.qes_spec_abort <- function(problems, where) {
  errors <- problems[problems$severity == "error", , drop = FALSE]
  shown <- utils::head(errors, 5L)
  details <- sprintf("%s %s%s: %s", shown$rule, shown$table,
                     ifelse(is.na(shown$row), "", paste0(" row ", shown$row)), shown$detail)
  .qes_abort(
    "spec_invalid",
    class = "qesR_error_spec",
    args = list(where, nrow(errors)),
    data = list(problems = problems),
    details = details
  )
}

# ---- hash ---------------------------------------------------------------------

# The content hash of a spec directory: the md5 of every CSV under `dir`
# (recursively, in C-locale order of their relative paths), each read as text
# by the one loader, so that quoting style or a trailing newline does not
# change it. Cells are joined with U+001F, rows with U+001E and files with
# U+001D; each file starts with its relative path.
.qes_spec_hash <- function(dir) {
  rel <- list.files(dir, pattern = "\\.csv$", recursive = TRUE)
  rel <- rel[!grepl("(^|/)\\._", rel)]
  rel <- sort(rel, method = "radix")
  parts <- vapply(rel, function(f) {
    x <- .qes_read_csv(file.path(dir, f))
    rows <- if (nrow(x) > 0L) {
      do.call(paste, c(unname(as.list(x)), sep = "\x1f"))
    } else {
      character(0)
    }
    paste(c(paste0("== ", f), paste(names(x), collapse = "\x1f"), rows), collapse = "\x1e")
  }, character(1), USE.NAMES = FALSE)
  .qes_md5_text(paste(parts, collapse = "\x1d"))
}

# The content hash of a spec held in memory: its tables written back to
# canonical CSV text by the one writer (the same text the loader reads, so an
# unedited spec gets the hash of its directory), then hashed as a directory.
.qes_spec_tables_hash <- function(tables) {
  dir <- tempfile("qes_spec_")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  files <- c(.qes_spec_files, .qes_spec_optional_files)
  for (tab in names(tables)) {
    file <- files[tab]
    if (is.na(file) || !is.data.frame(tables[[tab]])) next
    # an optional table that the directory did not have stays absent
    if (tab %in% names(.qes_spec_optional_files) && isTRUE(attr(tables[[tab]], "absent"))) next
    dir.create(dirname(file.path(dir, file)), recursive = TRUE, showWarnings = FALSE)
    .qes_write_csv(tables[[tab]], file.path(dir, file))
  }
  .qes_spec_hash(dir)
}

# ---- loading ------------------------------------------------------------------

.qes_spec_shipped_dir <- function() {
  .qes_extdata("harmonize")
}

# Read and type a spec directory. Problems that stop the tables from being
# read at all (a missing file, a column that is not in the schema, a value of
# the wrong type) raise qesR_error_spec whatever `validate` says.
.qes_spec_load <- function(dir) {
  where <- dir
  missing <- c("SPEC", unname(.qes_spec_files))
  missing <- missing[!file.exists(file.path(dir, missing))]
  if (length(missing) > 0L) {
    .qes_spec_abort(.qes_spec_problems(
      "V-S1", "error", missing, NA_integer_, missing, "file missing from the spec directory"
    ), where)
  }
  meta <- tryCatch(read.dcf(file.path(dir, "SPEC"), all = TRUE), error = function(e) NULL)
  if (is.null(meta) || nrow(meta) != 1L || !all(.qes_spec_fields %in% names(meta))) {
    .qes_spec_abort(.qes_spec_problems(
      "V-S1", "error", "SPEC", NA_integer_, "SPEC",
      paste("SPEC must be one DCF record with the fields", paste(.qes_spec_fields, collapse = ", "))
    ), where)
  }
  meta <- vapply(.qes_spec_fields, function(f) enc2utf8(trimws(as.character(meta[[f]][1]))), character(1))
  if (!identical(meta[["Schema-Version"]], .qes_spec_schema_version)) {
    .qes_abort(
      "spec_schema",
      class = "qesR_error_spec",
      args = list(where, meta[["Schema-Version"]], .qes_spec_schema_version),
      data = list(problems = .qes_spec_problems(
        "V-S1", "error", "SPEC", NA_integer_, "Schema-Version",
        sprintf("schema version %s; this qesR reads %s", meta[["Schema-Version"]], .qes_spec_schema_version)
      ))
    )
  }
  engine_min <- tryCatch(package_version(meta[["Engine-Min"]]), error = function(e) NULL)
  if (!is.null(engine_min) && engine_min > .qes_engine_version()) {
    .qes_abort(
      "spec_engine",
      class = "qesR_error_spec",
      args = list(where, meta[["Engine-Min"]], as.character(.qes_engine_version())),
      data = list(problems = .qes_spec_problems(
        "V-S1", "error", "SPEC", NA_integer_, "Engine-Min",
        sprintf("needs qesR %s or later", meta[["Engine-Min"]])
      ))
    )
  }
  tables <- list()
  problems <- .qes_spec_problems()
  files <- c(.qes_spec_files, .qes_spec_optional_files)
  for (tab in names(files)) {
    path <- file.path(dir, files[[tab]])
    schema <- .qes_spec_table_schema[[tab]]
    if (!file.exists(path)) {
      # only optional files can be absent (required ones were checked above)
      tables[[tab]] <- structure(.qes_apply_schema(
        as.data.frame(stats::setNames(rep(list(character(0)), length(.qes_schemas[[schema]])),
                                      names(.qes_schemas[[schema]])), stringsAsFactors = FALSE),
        schema
      ), absent = TRUE)
      next
    }
    tables[[tab]] <- tryCatch(
      .qes_read_csv(path, schema),
      qesR_error_source = function(e) {
        problems <<- rbind(problems, .qes_spec_problems(
          "V-S1", "error", tab, NA_integer_, files[[tab]], e$reason %||% conditionMessage(e)
        ))
        NULL
      }
    )
  }
  if (nrow(problems) > 0L) {
    .qes_spec_abort(problems, where)
  }
  hash <- .qes_spec_hash(dir)
  structure(
    list(
      version = meta[["Spec-Version"]],
      date = meta[["Spec-Date"]],
      hash = hash,
      hash_recorded = meta[["Hash"]],
      custom = NA,
      schema_version = meta[["Schema-Version"]],
      engine_min = meta[["Engine-Min"]],
      licence = meta[["Licence"]],
      dir = dir,
      tables = tables
    ),
    class = "qes_spec"
  )
}

# The version of the running engine (the qesR package version).
.qes_engine_version <- function() {
  utils::packageVersion("qesR")
}

# md5 of every file of a directory, as one string: the session cache key.
.qes_spec_fingerprint <- function(dir) {
  files <- sort(list.files(dir, recursive = TRUE, all.files = FALSE), method = "radix")
  files <- files[!grepl("(^|/)\\._", files)]
  paste(files, unname(tools::md5sum(file.path(dir, files))), sep = ":", collapse = ";")
}

# The spec selected by `spec` (NULL, a directory or a qes_spec object),
# loaded once per session and checked unless `validate = "none"`. Returns the
# qes_spec object with attribute "check" (the problems table) when checked.
.qes_spec_get <- function(spec = NULL, validate = "error") {
  if (inherits(spec, "qes_spec")) {
    obj <- spec
    attr(obj, "check") <- NULL
    # the stored hash describes the tables as loaded; recompute it from the
    # tables so that an edited object is flagged custom (section 5.11)
    obj$hash <- .qes_spec_tables_hash(obj$tables)
    obj$custom <- !identical(obj$hash, .qes_spec_shipped_hash())
    if (!identical(validate, "none")) {
      attr(obj, "check") <- .qes_spec_check_safely(obj, obj$dir %||% "<qes_spec object>")
    }
  } else {
    dir <- .qes_spec_dir(spec)
    key <- paste(dir, .qes_spec_fingerprint(dir), sep = "|")
    hit <- .qes_spec_cache[[key]]
    if (is.null(hit)) {
      obj <- .qes_spec_load(dir)
      obj$custom <- !identical(dir, .qes_spec_dir(NULL)) &&
        !identical(obj$hash, .qes_spec_shipped_hash())
      hit <- list(spec = obj, check = NULL)
    }
    if (!identical(validate, "none") && is.null(hit$check)) {
      hit$check <- .qes_spec_check_safely(hit$spec, dir)
    }
    .qes_spec_cache[[key]] <- hit
    obj <- hit$spec
    if (!identical(validate, "none")) {
      attr(obj, "check") <- hit$check
    }
  }
  check <- attr(obj, "check")
  if (identical(validate, "error") && !is.null(check) && any(check$severity == "error")) {
    .qes_spec_abort(check, obj$dir %||% "<qes_spec object>")
  }
  obj
}

# Run the validator and the data checks on the shipped dictionary (R/hz-data.R);
# an unexpected R error inside them (a bug the rules did not foresee) becomes
# qesR_error_spec rather than a bare error. A custom spec whose gates.csv
# lacks the cells of a row cannot be checked offline for it: a warning, since
# its author checks it on the data with qes_spec(data = ).
.qes_spec_check_safely <- function(spec, where) {
  tryCatch(
    rbind(
      .qes_spec_check(spec),
      .qes_data_check(spec, .qes_hz_sources_shipped(spec),
                      offline_severity = if (isTRUE(spec$custom)) "warning" else "error")
    ),
    error = function(e) {
      if (inherits(e, "qesR_error")) stop(e)
      .qes_spec_abort(.qes_spec_problems(
        "internal", "error", NA_character_, NA_integer_, NA_character_,
        paste("the validator stopped:", conditionMessage(e))
      ), where)
    }
  )
}

# The directory named by `spec` (NULL is the shipped spec).
.qes_spec_dir <- function(spec) {
  if (is.null(spec)) {
    return(normalizePath(.qes_spec_shipped_dir(), winslash = "/", mustWork = TRUE))
  }
  if (!is.character(spec) || length(spec) != 1L || is.na(spec) || !dir.exists(spec)) {
    .qes_abort(
      "input_spec",
      class = "qesR_error_input",
      data = list(arg = "spec", value = spec)
    )
  }
  normalizePath(spec, winslash = "/", mustWork = TRUE)
}

# Content hash of the shipped spec (for the `custom` flag).
.qes_spec_shipped_hash <- function() {
  if (is.null(.qes_spec_cache[[".shipped_hash"]])) {
    .qes_spec_cache[[".shipped_hash"]] <- .qes_spec_hash(.qes_spec_dir(NULL))
  }
  .qes_spec_cache[[".shipped_hash"]]
}

# Forget the loaded specs (for tests).
.qes_spec_reset <- function() {
  rm(list = ls(.qes_spec_cache, all.names = TRUE), envir = .qes_spec_cache)
  invisible()
}

# ---- level sets ------------------------------------------------------------------

# The rows of level set `levels_id`, with "inherits=<id>" rows replaced by the
# rows of that set (recursively), relabelled with `levels_id`. NULL when the
# set does not exist or an inheritance cycles.
.qes_spec_levels <- function(levels, levels_id, seen = character(0)) {
  if (levels_id %in% seen) {
    return(NULL)
  }
  rows <- levels[!is.na(levels$levels_id) & levels$levels_id == levels_id, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(NULL)
  }
  out <- list()
  for (i in seq_len(nrow(rows))) {
    name <- rows$name[i]
    if (!is.na(name) && startsWith(name, "inherits=")) {
      parent <- .qes_spec_levels(levels, sub("^inherits=", "", name), c(seen, levels_id))
      if (is.null(parent)) {
        return(NULL)
      }
      parent$levels_id <- levels_id
      out[[length(out) + 1L]] <- parent
    } else {
      out[[length(out) + 1L]] <- rows[i, , drop = FALSE]
    }
  }
  out <- do.call(rbind, out)
  out <- out[order(out$order, out$code), , drop = FALSE]
  rownames(out) <- NULL
  out
}

# ---- qes_spec() -----------------------------------------------------------------

#' The harmonization spec
#'
#' `qes_spec()` is the one entry point to the harmonization specification: the
#' reviewed rules that say, study by study, which source question feeds each
#' harmonized variable ("target"), how its codes map to the target's levels,
#' why each missing value is missing and how comparable each study's question
#' is to the target's anchor question.
#'
#' Slices HZ1 and HZ2 implement `view = "spec"`: the spec as tables, checked
#' by the validator (rules V-S1 to V-S17) and by the data checks (V-D1 to
#' V-D4, V-D7 and V-D8) against the shipped dictionary and `gates.csv`, and,
#' with `data`, against data frames (V-D1 to V-D5, V-D7, V-D8). The
#' `"targets"` and `"crosswalk"` views and the export of this function arrive
#' with the harmonization engine.
#'
#' @param view `"targets"`, `"crosswalk"` or `"spec"`.
#' @param targets,studies Filters of the `"targets"` and `"crosswalk"` views.
#' @param level,format Options of the `"crosswalk"` view.
#' @param spec `NULL` (the spec shipped with qesR), the path of a spec
#'   directory (for example one copied from a release), or a `qes_spec` object.
#' @param validate What a problem found by the validator does: `"error"`
#'   raises `qesR_error_spec`, `"report"` returns the spec with the problems
#'   in `attr(, "check")`, `"none"` skips the check.
#' @param data A named list of data frames, one per study, named by study
#'   code, as [get_qes()] returns them: the data checks then also run on them
#'   (view `"spec"` only). Their problems are added to `attr(, "check")`.
#' @param lang Language of returned text, `"en"` or `"fr"`.
#' @return For `view = "spec"`, an object of class `qes_spec`: a list with the
#'   spec `version`, its content `hash`, `custom` (`TRUE` when it is not the
#'   shipped spec) and `tables` (targets, levels, crosswalk, valuemaps, waves,
#'   weights, changes, gates, expected); `attr(, "check")` holds the problems
#'   table (`rule`, `severity`, `table`, `row`, `key`, `detail`).
#' @noRd
qes_spec <- function(view = c("targets", "crosswalk", "spec"), targets = NULL, studies = NULL,
                     level = c("row", "code"), format = c("qesR", "retroharmonize"),
                     spec = NULL, validate = c("error", "report", "none"), data = NULL,
                     lang = c("en", "fr")) {
  level_given <- !missing(level)
  format_given <- !missing(format)
  view <- .qes_check_one(view, "view", c("targets", "crosswalk", "spec"))
  validate <- .qes_check_one(validate, "validate", c("error", "report", "none"))
  lang <- .qes_check_one(lang, "lang", c("en", "fr"))
  level <- .qes_check_one(level, "level", c("row", "code"))
  format <- .qes_check_one(format, "format", c("qesR", "retroharmonize"))
  wrong_view <- function(arg, views) {
    .qes_abort(
      "input_spec_view",
      class = "qesR_error_input",
      args = list(arg, .qes_q(views)),
      data = list(arg = arg, value = view)
    )
  }
  if (view != "crosswalk" && level_given) wrong_view("level", "crosswalk")
  if (view != "crosswalk" && format_given) wrong_view("format", "crosswalk")
  if (view == "spec" && !is.null(targets)) wrong_view("targets", c("targets", "crosswalk"))
  if (view == "spec" && !is.null(studies)) wrong_view("studies", c("targets", "crosswalk"))
  if (view != "spec" && !is.null(data)) wrong_view("data", "spec")
  if (view != "spec") {
    .qes_abort("spec_later", class = "qesR_error_input",
               args = list(sprintf("qes_spec(view = \"%s\")", view)),
               data = list(arg = "view", value = view))
  }
  if (is.null(data)) {
    return(.qes_spec_get(spec, validate))
  }
  data <- .qes_spec_data_arg(data)
  obj <- .qes_spec_get(spec, if (identical(validate, "none")) "none" else "report")
  if (identical(validate, "none")) {
    return(obj)
  }
  check <- rbind(attr(obj, "check"), tryCatch(
    .qes_data_check_frames(obj, data),
    error = function(e) {
      if (inherits(e, "qesR_error")) stop(e)
      .qes_spec_abort(.qes_spec_problems(
        "internal", "error", NA_character_, NA_integer_, NA_character_,
        paste("the data checks stopped:", conditionMessage(e))
      ), obj$dir %||% "<qes_spec object>")
    }
  ))
  rownames(check) <- NULL
  attr(obj, "check") <- check
  if (identical(validate, "error") && any(check$severity == "error")) {
    .qes_spec_abort(check, obj$dir %||% "<qes_spec object>")
  }
  obj
}

# The `data` argument of qes_spec(): a named list of data frames whose names
# are study codes (canonicalized; any case and spacing).
.qes_spec_data_arg <- function(data) {
  bad <- function() {
    .qes_abort("input_spec_data", class = "qesR_error_input", data = list(arg = "data", value = NULL))
  }
  if (is.data.frame(data) || !is.list(data) || length(data) == 0L || is.null(names(data)) ||
      anyNA(names(data)) || !all(nzchar(names(data))) ||
      !all(vapply(data, is.data.frame, logical(1)))) {
    bad()
  }
  matched <- .qes_match_codes(names(data), .qes_catalog()$studies)
  if (anyNA(matched)) {
    .qes_unknown_study(names(data)[is.na(matched)])
  }
  names(data) <- matched
  if (anyDuplicated(names(data)) > 0L) {
    bad()
  }
  data
}

#' @export
print.qes_spec <- function(x, ...) {
  lang <- .qes_lang()
  check <- attr(x, "check")
  n <- vapply(x$tables, NROW, integer(1))
  cat(.qes_msg("spec_print_head", list(x$version, x$date, x$hash), lang), "\n", sep = "")
  if (isTRUE(x$custom)) {
    cat(.qes_msg("spec_print_custom", list(x$dir), lang), "\n", sep = "")
  }
  cat(paste0("  ", names(n), ": ", n, collapse = "\n"), "\n", sep = "")
  if (is.null(check)) {
    cat(.qes_msg("spec_print_unchecked", list(), lang), "\n", sep = "")
  } else {
    counts <- table(factor(check$severity, levels = c("error", "warning", "note")))
    cat(.qes_msg("spec_print_check", list(counts[["error"]], counts[["warning"]], counts[["note"]]), lang),
        "\n", sep = "")
  }
  invisible(x)
}
