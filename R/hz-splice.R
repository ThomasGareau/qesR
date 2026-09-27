# Internal helpers on harmonized data (design.md section 2.2, slice HZ4;
# not exported, OD17): pooling the targets of one family into one column,
# and joining raw variables of the studies' files. Exporting either later
# keeps these signatures.

# Pool the targets of one family into one column, study by study (design.md
# section 2.2, OD17). For each study, the first target of `prefer` (default:
# the family's targets in spec order) whose cell was applied gives the
# values; `<into>__target` records which target that was and `<into>__grade`
# its grade (and `<into>__na` its NA reasons, when `x` has them). Targets
# with different level sets or types are never pooled (an error), and a
# warning lists the wording breaks (studies drawing on different targets).
# `spec` is the spec `x` was built with (NULL: the shipped spec).
.qes_splice <- function(x, family, into = family, prefer = NULL, spec = NULL) {
  if (!inherits(x, "qes_harmonized") || !is.data.frame(x)) {
    .qes_abort("input_design_x", class = "qesR_error_input", data = list(arg = "x", value = NULL))
  }
  if (!is.character(family) || length(family) != 1L || is.na(family)) {
    .qes_abort("input_string", class = "qesR_error_input", args = list("family"),
               data = list(arg = "family", value = family))
  }
  if (!is.character(into) || length(into) != 1L || is.na(into) || !grepl(.qes_name_pattern, into)) {
    .qes_abort("input_string", class = "qesR_error_input", args = list("into"),
               data = list(arg = "into", value = into))
  }
  if (into %in% names(x)) {
    .qes_abort("input_splice_into_exists", class = "qesR_error_input", args = list(.qes_q(into)),
               data = list(arg = "into", value = into))
  }
  sp <- .qes_spec_get(spec, "none")
  tg <- sp$tables$targets
  members <- tg$target[tg$family %in% family]
  candidates <- if (is.null(prefer)) members else prefer
  if (!is.character(candidates) || !all(candidates %in% members)) {
    .qes_abort("input_splice_family", class = "qesR_error_input", args = list(.qes_q(family)),
               data = list(arg = "prefer", value = prefer))
  }
  candidates <- intersect(candidates, names(x))
  if (length(candidates) == 0L) {
    .qes_abort("input_splice_family", class = "qesR_error_input", args = list(.qes_q(family)),
               data = list(arg = "family", value = family))
  }
  j <- match(candidates, tg$target)
  sets <- paste(tg$type[j], ifelse(is.na(tg$levels_id[j]), "-", tg$levels_id[j]), sep = ":")
  if (length(unique(sets)) > 1L) {
    .qes_abort("input_splice_levels", class = "qesR_error_input",
               args = list(.qes_q(candidates), .qes_q(unique(sets))),
               data = list(targets = candidates, level_sets = sets))
  }
  cell <- attr(attr(x, "qes_provenance", exact = TRUE), "cell", exact = TRUE)
  if (!is.data.frame(cell)) {
    .qes_abort("no_provenance", class = "qesR_error_no_provenance", args = list("x"), data = list(arg = "x"))
  }
  out_value <- x[[candidates[1]]]
  out_value[] <- NA
  out_target <- rep(NA_character_, nrow(x))
  out_grade <- rep(NA_character_, nrow(x))
  na_col <- paste0(candidates[1], "__na")
  out_na <- if (na_col %in% names(x)) x[[na_col]] else NULL
  used <- list()
  for (s in unique(as.character(x$study))) {
    rows <- which(x$study == s)
    ok <- cell[cell$study == s & cell$target %in% candidates & cell$included, , drop = FALSE]
    if (nrow(ok) == 0L) {
      if (!is.null(out_na)) out_na[rows] <- "not_asked"
      next
    }
    t <- candidates[candidates %in% ok$target][1]
    out_value[rows] <- x[[t]][rows]
    out_target[rows] <- t
    out_grade[rows] <- ok$grade[match(t, ok$target)]
    if (!is.null(out_na)) out_na[rows] <- x[[paste0(t, "__na")]][rows]
    used[[t]] <- c(used[[t]], s)
  }
  attr(out_value, "label") <- attr(x[[candidates[1]]], "label", exact = TRUE)
  x[[into]] <- out_value
  if (!is.null(out_na)) x[[paste0(into, "__na")]] <- out_na
  x[[paste0(into, "__target")]] <- out_target
  x[[paste0(into, "__grade")]] <- out_grade
  if (length(used) > 1L) {
    what <- vapply(names(used), function(t) sprintf("%s (%s)", t, paste(used[[t]], collapse = ", ")), character(1))
    .qes_warn("splice_wording", class = "qesR_warning_wording_break",
              args = list(.qes_q(into), paste(what, collapse = "; "), .qes_q(paste0(into, "__target"))),
              data = list(targets = names(used), studies = used))
  }
  x
}

# Join raw variables of the studies' files to harmonized data (design.md
# section 2.2, OD17): a column `<study>__<var>` per study and variable, in the
# file's own codes (labels kept), filled on that study's rows by source_row
# and NA on the others. `vars` is a character vector (each study that has a
# variable gets its column; a variable in none of them is an error) or a
# list named by study code. `data` is NULL (read the pinned files) or a
# named list of frames as get_qes() returns them.
.qes_join_raw <- function(x, vars, data = NULL) {
  if (!inherits(x, "qes_harmonized") || !is.data.frame(x) || !all(c("study", "source_row") %in% names(x))) {
    .qes_abort("input_design_x", class = "qesR_error_input", data = list(arg = "x", value = NULL))
  }
  by_study <- is.list(vars) && !is.null(names(vars))
  if (!(by_study || (is.character(vars) && length(vars) > 0L && !anyNA(vars)))) {
    .qes_abort("input_string", class = "qesR_error_input", args = list("vars"),
               data = list(arg = "vars", value = vars))
  }
  if (!is.null(data)) {
    data <- .qes_spec_data_arg(data, demo = TRUE)
  }
  studies <- unique(as.character(x$study))
  if (by_study) {
    names(vars) <- .qes_match_codes(names(vars), .qes_catalog(demo = TRUE)$studies)
    extra <- setdiff(names(vars), studies)
    if (length(extra) > 0L || anyNA(names(vars))) {
      .qes_abort("input_harmonize_data_names", class = "qesR_error_input", args = list(.qes_q(extra)),
                 data = list(arg = "vars", value = extra))
    }
    studies <- intersect(studies, names(vars))
  }
  found <- character(0)
  for (s in studies) {
    d <- if (!is.null(data[[s]])) data[[s]] else .qes_read(s, quiet = TRUE)
    want <- if (by_study) vars[[s]] else intersect(vars, names(d))
    miss <- setdiff(want, names(d))
    if (length(miss) > 0L) {
      .qes_abort("unknown_variable_study", class = "qesR_error_unknown_variable",
                 args = list(.qes_q(miss), .qes_q(s)),
                 data = list(study = s, variables = miss, suggestions = character(0)))
    }
    rows <- which(x$study == s)
    for (v in want) {
      col <- d[[v]][rep(NA_integer_, nrow(x))]
      col[rows] <- d[[v]][x$source_row[rows]]
      x[[paste0(s, "__", v)]] <- col
    }
    found <- c(found, want)
  }
  if (!by_study) {
    none <- setdiff(vars, found)
    if (length(none) > 0L) {
      .qes_abort("input_join_raw_vars", class = "qesR_error_input", args = list(.qes_q(none)),
                 data = list(arg = "vars", value = none))
    }
  }
  x
}
