# Data checks V-D1 to V-D8 and the projection check V-P1 (design.md sections
# 5.1, 5.10 and 5.11, slice HZ2).
#
# The checks never read respondent rows. They read a "sources" object, the
# aggregates of the pinned files:
#   variables  study, variable, type, na_values (the declared missing codes);
#   values     study, variable, value (code text), label, label_hash,
#              label_number (a label read as a number, for from_label rows),
#              missing_type, n (unweighted count; NA when not counted);
#   gates      the spec_gates table: joint counts of gate code and source code
#              among a wave's members (member_rule records the membership
#              rule they were counted under);
#   n_rows     rows of each study's pinned file.
# Three builders give one, and every check runs the same way on each:
#   .qes_hz_sources_shipped()  the shipped dictionary (CC0 studies) with the
#                              spec's gates.csv: tests and qes_spec();
#   .qes_hz_sources_read()     the same tables for a study whose metadata
#                              cannot ship (qes2022, OD3), from the
#                              build-ignored data-raw/nc/: CI only;
#   .qes_hz_sources_data()     data frames as get_qes() returns them:
#                              qes_spec(data = ), data-raw/build_sources.R
#                              and, from slice HZ3, the engine.
#
# A crosswalk row is "projected" from its cells, the joint counts (gate code,
# source code) among the wave's members, which gates.csv holds for every
# projectable row (the dictionary's per-code counts are not complete for
# every variable, so they are never used in their place). The
# projection applies the rule, na_codes and the gate exactly as the engine
# does (design.md section 5.6) and gives the unweighted marginals that
# expected/marginals.csv records (V-P1). Rows with other rules (weight, date,
# string, fn:) are not projected; the live column hashes cover them (V-L1).

# ---- sources ---------------------------------------------------------------------

.qes_hz_empty_sources <- function() {
  list(
    variables = data.frame(study = character(0), variable = character(0), type = character(0),
                           na_values = character(0), stringsAsFactors = FALSE),
    values = data.frame(study = character(0), variable = character(0), value = character(0),
                        label = character(0), label_hash = character(0), label_number = numeric(0),
                        missing_type = character(0), n = integer(0), stringsAsFactors = FALSE),
    gates = .qes_apply_schema(as.data.frame(
      stats::setNames(rep(list(character(0)), length(.qes_schemas$spec_gates)), names(.qes_schemas$spec_gates)),
      stringsAsFactors = FALSE
    ), "spec_gates"),
    n_rows = stats::setNames(integer(0), character(0))
  )
}

# Bind several sources objects (one study each, or disjoint studies).
.qes_hz_bind_sources <- function(...) {
  parts <- list(...)
  parts <- parts[!vapply(parts, is.null, logical(1))]
  out <- .qes_hz_empty_sources()
  for (p in parts) {
    out$variables <- rbind(out$variables, p$variables[, names(out$variables), drop = FALSE])
    out$values <- rbind(out$values, p$values[, names(out$values), drop = FALSE])
    out$gates <- rbind(out$gates, p$gates[, names(out$gates), drop = FALSE])
    out$n_rows <- c(out$n_rows, p$n_rows)
  }
  out
}

# A label read as a number ("10", "10 - a droite" is not one), for the rule
# option from_label = TRUE.
.qes_label_number <- function(label) {
  x <- trimws(label)
  ok <- !is.na(x) & grepl("^-?[0-9]+(\\.[0-9]+)?$", x)
  out <- rep(NA_real_, length(x))
  out[ok] <- as.numeric(x[ok])
  out
}

# The shipped dictionary of the studies whose metadata ships, with the gates
# table of `spec`.
.qes_hz_sources_shipped <- function(spec) {
  dict <- .qes_dict_shipped()
  cat_ <- .qes_catalog()
  studies <- cat_$studies$study[cat_$studies$metadata_shipped %in% TRUE]
  v <- dict$variables[dict$variables$study %in% studies, , drop = FALSE]
  val <- dict$values[dict$values$study %in% studies, , drop = FALSE]
  files <- cat_$files[cat_$files$role == "data" & cat_$files$is_default %in% TRUE, , drop = FALSE]
  n_rows <- stats::setNames(files$n_rows[match(studies, files$study)], studies)
  gates <- spec$tables$gates
  list(
    variables = data.frame(study = v$study, variable = v$variable, type = v$type,
                           na_values = v$na_values, stringsAsFactors = FALSE),
    values = data.frame(study = val$study, variable = val$variable, value = val$value,
                        label = val$label, label_hash = NA_character_,
                        label_number = .qes_label_number(val$label),
                        missing_type = val$missing_type, n = val$n, stringsAsFactors = FALSE),
    gates = if (is.null(gates)) .qes_hz_empty_sources()$gates else gates[gates$study %in% studies, , drop = FALSE],
    n_rows = n_rows[!is.na(n_rows)]
  )
}

# The aggregates of a study whose metadata cannot ship, from `dir`
# (data-raw/nc/ in the source tree): sources_<study>_variables.csv,
# sources_<study>_values.csv and gates_<study>.csv, written by
# data-raw/build_sources.R. NULL when the files are not there.
.qes_hz_sources_read <- function(dir, study) {
  paths <- file.path(dir, c(
    sprintf("sources_%s_variables.csv", study), sprintf("sources_%s_values.csv", study),
    sprintf("gates_%s.csv", study)
  ))
  if (!all(file.exists(paths))) {
    return(NULL)
  }
  v <- .qes_read_csv(paths[1], "hz_variables")
  val <- .qes_read_csv(paths[2], "hz_values")
  gates <- .qes_read_csv(paths[3], "spec_gates")
  files <- .qes_catalog()$files
  files <- files[files$study == study & files$role == "data" & files$is_default %in% TRUE, , drop = FALSE]
  list(
    variables = v,
    values = data.frame(study = val$study, variable = val$variable, value = val$value,
                        label = NA_character_, label_hash = val$label_hash,
                        label_number = val$label_number, missing_type = val$missing_type,
                        n = val$n, stringsAsFactors = FALSE),
    gates = gates,
    n_rows = stats::setNames(files$n_rows[1], study)
  )
}

# Variables of `study` that the spec names: sources and gates of crosswalk
# rows, membership, date, subsample, strata and mode variables of waves.
.qes_hz_spec_vars <- function(spec, study) {
  xw <- spec$tables$crosswalk[spec$tables$crosswalk$study == study, , drop = FALSE]
  wv <- spec$tables$waves[spec$tables$waves$study == study, , drop = FALSE]
  mode_var <- sub("^var:", "", wv$mode[!is.na(wv$mode) & startsWith(wv$mode, "var:")])
  vars <- c(xw$source_var, xw$gate_var, wv$member_var, wv$date_var, wv$subsample_var,
            wv$strata_var, mode_var)
  unique(vars[!is.na(vars) & nzchar(vars)])
}

# The sources of data frames: `data` is a named list, study code -> data frame
# as get_qes() returns it. Codes of every variable the spec names are counted
# (all of them, with no 50-code limit), labels come from the file, and the
# gates table has the cells of every projectable crosswalk row.
.qes_hz_sources_data <- function(spec, data) {
  out <- list()
  for (study in names(data)) {
    d <- data[[study]]
    # a factor (haven::as_factor()) holds label text, not the codes the
    # checks read: the checks would report false problems
    named <- intersect(.qes_hz_spec_vars(spec, study), names(d))
    fac <- named[vapply(d[named], is.factor, logical(1))]
    if (length(fac) > 0L) {
      .qes_abort("input_spec_data_factor", class = "qesR_error_input",
                 args = list(study, paste(fac, collapse = ", ")),
                 data = list(arg = "data", value = NULL))
    }
    vars <- names(d)
    types <- vapply(d, .qes_storage, character(1), USE.NAMES = FALSE)
    na_values <- vapply(d, .qes_na_string, character(1), USE.NAMES = FALSE)
    variables <- data.frame(study = rep(study, length(vars)), variable = vars, type = types,
                            na_values = na_values, stringsAsFactors = FALSE)
    rows <- list()
    for (v in intersect(.qes_hz_spec_vars(spec, study), vars)) {
      x <- d[[v]]
      if (!types[match(v, vars)] %in% c("numeric", "character", "logical")) next
      code <- .canon(x)
      tab <- table(code[!is.na(code)])
      labels <- attr(x, "labels", exact = TRUE)
      lab_code <- if (length(labels) > 0L) .canon(unname(unclass(labels))) else character(0)
      lab_text <- if (length(labels) > 0L) .squish_ws(names(labels)) else character(0)
      keep <- !is.na(lab_code) & !duplicated(lab_code)
      lab_code <- lab_code[keep]
      lab_text <- lab_text[keep]
      codes <- unique(c(names(tab), lab_code))
      if (length(codes) == 0L) next
      lab <- lab_text[match(codes, lab_code)]
      n <- as.integer(tab[codes])
      n[is.na(n)] <- 0L
      # a file declares missing codes (na_values, checked by V-D4) but does
      # not say what they mean: missing types come only from the curated
      # dictionary
      rows[[length(rows) + 1L]] <- data.frame(
        study = study, variable = v, value = codes, label = lab,
        label_hash = .qes_md5_text(.qes_norm_label(lab)), label_number = .qes_label_number(lab),
        missing_type = NA_character_, n = n,
        stringsAsFactors = FALSE
      )
    }
    values <- if (length(rows) > 0L) do.call(rbind, rows) else .qes_hz_empty_sources()$values
    gates <- .qes_hz_cells_data(spec, d, study)
    out[[study]] <- list(variables = variables, values = values, gates = gates,
                         n_rows = stats::setNames(nrow(d), study))
  }
  do.call(.qes_hz_bind_sources, unname(out))
}

# ---- waves and cells --------------------------------------------------------------

# The membership rule of wave row `w` as text ("" when every row is a
# member): "<member_var>=<codes>".
.qes_member_rule <- function(wv, w) {
  if (is.na(wv$member_var[w]) || !nzchar(wv$member_var[w])) "" else paste0(wv$member_var[w], "=", wv$member_codes[w])
}

# Which rows of `d` belong to wave row `w`; NULL when the member variable
# is missing from `d`.
.qes_wave_members <- function(wv, w, d) {
  if (.qes_member_rule(wv, w) == "") {
    return(rep(TRUE, nrow(d)))
  }
  var <- wv$member_var[w]
  if (!var %in% names(d)) {
    return(NULL)
  }
  code <- .canon(d[[var]])
  if (identical(wv$member_codes[w], "!NA")) !is.na(code) else code %in% .qes_split_list(wv$member_codes[w])
}

# Is crosswalk row `i` projected (a map or numeric rule)?
.qes_hz_projected <- function(xw, i) {
  xw$rule[i] %in% c("map", "numeric")
}

# The cells of every projectable crosswalk row of `study`, counted in data
# frame `d` (the spec_gates table).
.qes_hz_cells_data <- function(spec, d, study) {
  xw <- spec$tables$crosswalk
  wv <- spec$tables$waves
  out <- list()
  seen <- character(0)
  for (i in which(xw$study == study)) {
    if (!.qes_hz_projected(xw, i) || !xw$source_var[i] %in% names(d)) next
    gate <- xw$gate_var[i]
    if (!is.na(gate) && !gate %in% names(d)) next
    w <- which(wv$study == study & wv$wave == xw$wave[i])
    if (length(w) != 1L) next
    key <- paste(xw$wave[i], xw$source_var[i], gate, sep = "\x1f")
    if (key %in% seen) next
    seen <- c(seen, key)
    mem <- .qes_wave_members(wv, w, d)
    if (is.null(mem)) next
    src <- .canon(d[[xw$source_var[i]]])[mem]
    src[is.na(src)] <- "NA"
    g <- if (is.na(gate)) rep(NA_character_, length(src)) else {
      gv <- .canon(d[[gate]])[mem]
      gv[is.na(gv)] <- "NA"
      gv
    }
    if (length(src) == 0L) next
    k <- paste(ifelse(is.na(g), "", g), src, sep = "\x1f")
    tab <- table(k)
    parts <- strsplit(names(tab), "\x1f", fixed = TRUE)
    gc <- vapply(parts, `[`, character(1), 1L)
    out[[length(out) + 1L]] <- data.frame(
      study = study, wave = xw$wave[i], source_var = xw$source_var[i],
      member_rule = .qes_member_rule(wv, w), gate_var = gate,
      gate_code = ifelse(nzchar(gc), gc, NA_character_),
      source_code = vapply(parts, `[`, character(1), 2L), n = as.integer(tab),
      stringsAsFactors = FALSE
    )
  }
  if (length(out) == 0L) {
    return(.qes_hz_empty_sources()$gates)
  }
  res <- do.call(rbind, out)
  res$member_rule[res$member_rule == ""] <- NA_character_
  .qes_hz_sort_gates(res)
}

.qes_hz_sort_gates <- function(g) {
  num <- function(x) suppressWarnings(as.numeric(x))
  o <- order(g$study, g$wave, g$source_var, ifelse(is.na(g$gate_var), "", g$gate_var),
             is.na(num(g$gate_code)), num(g$gate_code), g$gate_code,
             is.na(num(g$source_code)), num(g$source_code), g$source_code, method = "radix")
  g <- g[o, , drop = FALSE]
  rownames(g) <- NULL
  g
}

# The cells of crosswalk row `i` (data frame gate_code, source_code, n; NA
# gate_code when the row has no gate) from `sources`, or a string saying why
# they cannot be had.
.qes_hz_cells <- function(spec, sources, i) {
  xw <- spec$tables$crosswalk
  wv <- spec$tables$waves
  study <- xw$study[i]
  w <- which(wv$study == study & wv$wave == xw$wave[i])
  rule <- if (length(w) == 1L) .qes_member_rule(wv, w) else ""
  g <- sources$gates
  same_gate <- if (is.na(xw$gate_var[i])) is.na(g$gate_var) else g$gate_var %in% xw$gate_var[i]
  rows <- g[g$study == study & g$wave == xw$wave[i] & g$source_var == xw$source_var[i] & same_gate, , drop = FALSE]
  if (nrow(rows) > 0L) {
    recorded <- unique(ifelse(is.na(rows$member_rule), "", rows$member_rule))
    if (!identical(recorded, rule)) {
      return(sprintf("gates.csv was counted under membership rule '%s', the wave now has '%s'; run data-raw/build_sources.R",
                     paste(recorded, collapse = "|"), rule))
    }
    return(data.frame(gate_code = rows$gate_code, source_code = rows$source_code, n = rows$n,
                      stringsAsFactors = FALSE))
  }
  # the dictionary's counts are not a substitute: it lists every observed
  # code only for numeric columns with at most 50 codes (R/metadata.R), so
  # an unlisted code would pass as system missing
  "gates.csv has no cells for this row; run data-raw/build_sources.R"
}

# ---- outcomes ---------------------------------------------------------------------

# The parsed rule of crosswalk row `i`: map, na_codes, numeric limits, affine,
# gate, level set.
.qes_hz_row_rule <- function(spec, i) {
  xw <- spec$tables$crosswalk
  args <- .qes_parse_kv(xw$args[i]) %||% character(0)
  tg <- spec$tables$targets
  j <- match(xw$target[i], tg$target)
  set <- if (!is.na(j) && !is.na(tg$levels_id[j])) .qes_spec_levels(spec$tables$levels, tg$levels_id[j]) else NULL
  vm <- spec$tables$valuemaps
  vm <- vm[!is.na(xw$map_id[i]) & vm$map_id %in% xw$map_id[i], , drop = FALSE]
  num <- function(key, default) {
    v <- suppressWarnings(as.numeric(unname(args[key])))
    if (length(v) == 0L || is.na(v)) default else v
  }
  aff <- if ("affine" %in% names(args)) .qes_affine(args[["affine"]]) else c(a = 1, b = 0)
  list(
    rule = xw$rule[i],
    source_var = xw$source_var[i],
    na_codes = .qes_parse_kv(xw$na_codes[i]) %||% character(0),
    map_code = vm$source_code,
    map_level = if (is.null(set)) rep(NA_character_, nrow(vm)) else set$name[match(vm$target_code, set$code)],
    map_reason = vm$na_reason,
    min = num("min", if (is.na(j)) -Inf else tg$valid_min[j] %|NA|% -Inf),
    max = num("max", if (is.na(j)) Inf else tg$valid_max[j] %|NA|% Inf),
    from_label = identical(unname(args["from_label"]), "TRUE"),
    affine = aff %||% c(a = 1, b = 0),
    gate_var = xw$gate_var[i],
    gate_to = .qes_parse_kv(xw$gate_to[i]) %||% character(0),
    set = set
  )
}

`%|NA|%` <- function(x, y) if (length(x) == 0L || is.na(x)) y else x

# The outcome of source codes `code` (text, "NA" for system missing) under
# rule `r`, before the gate: data frame value (level name or number text) and
# na_reason. `label_number` gives the numeric label of each code (from_label).
.qes_hz_outcome <- function(r, code, label_number = rep(NA_real_, length(code))) {
  value <- rep(NA_character_, length(code))
  reason <- rep(NA_character_, length(code))
  nac <- r$na_codes
  is_na <- code == "NA"
  in_nac <- code %in% names(nac)
  reason[in_nac] <- unname(nac[code[in_nac]])
  reason[is_na & !in_nac] <- "sysmis"
  rest <- !is_na & !in_nac
  if (identical(r$rule, "map")) {
    k <- match(code, r$map_code)
    hit <- rest & !is.na(k)
    value[hit] <- r$map_level[k[hit]]
    reason[hit] <- r$map_reason[k[hit]]
    reason[rest & is.na(k)] <- "unmapped"
  } else if (identical(r$rule, "numeric")) {
    x <- if (r$from_label) label_number else suppressWarnings(as.numeric(code))
    y <- r$affine[["a"]] * x + r$affine[["b"]]
    ok <- rest & !is.na(y) & y >= r$min & y <= r$max
    value[ok] <- .qes_code_chr(y[ok])
    reason[rest & !ok] <- "unmapped"
  } else {
    reason[rest] <- "unmapped"
  }
  data.frame(value = value, na_reason = reason, stringsAsFactors = FALSE)
}

# The outcome of cells: the source outcome, then the gate (a gate code listed
# in gate_to overrides it: an NA reason or a level name).
.qes_hz_cell_outcome <- function(r, cells, label_number) {
  out <- .qes_hz_outcome(r, cells$source_code, label_number)
  if (!is.na(r$gate_var) && length(r$gate_to) > 0L) {
    g <- cells$gate_code
    closed <- !is.na(g) & g %in% names(r$gate_to)
    to <- unname(r$gate_to[g[closed]])
    is_level <- !is.null(r$set) & to %in% (r$set$name %||% character(0))
    out$value[closed] <- ifelse(is_level, to, NA_character_)
    out$na_reason[closed] <- ifelse(is_level, NA_character_, to)
  }
  out
}

# ---- projection (V-P1) -----------------------------------------------------------

# The projected marginals of every projectable crosswalk row of the studies
# in `sources` (default: all of them): the spec_expected table. Rows whose
# cells cannot be had are listed in attr(, "unprojected") with the reason.
.qes_project_marginals <- function(spec, sources, studies = NULL) {
  xw <- spec$tables$crosswalk
  studies <- studies %||% unique(c(names(sources$n_rows), sources$gates$study))
  reasons <- .qes_enum("missing_type")$value
  out <- list()
  skipped <- list()
  for (i in which(xw$study %in% studies)) {
    if (!.qes_hz_projected(xw, i)) next
    cells <- .qes_hz_cells(spec, sources, i)
    if (is.character(cells)) {
      skipped[[length(skipped) + 1L]] <- data.frame(study = xw$study[i], wave = xw$wave[i], target = xw$target[i],
                                                     source_var = xw$source_var[i], reason = cells,
                                                     stringsAsFactors = FALSE)
      next
    }
    r <- .qes_hz_row_rule(spec, i)
    ln <- .qes_hz_label_numbers(sources, xw$study[i], xw$source_var[i], cells$source_code)
    oc <- .qes_hz_cell_outcome(r, cells, ln)
    key <- paste(ifelse(is.na(oc$value), "", oc$value), ifelse(is.na(oc$na_reason), "", oc$na_reason), sep = "\x1f")
    n <- tapply(cells$n, key, sum)
    parts <- strsplit(names(n), "\x1f", fixed = TRUE)
    value <- vapply(parts, function(p) if (nzchar(p[1])) p[1] else NA_character_, character(1))
    reason <- vapply(parts, function(p) if (length(p) > 1L && nzchar(p[2])) p[2] else NA_character_, character(1))
    m <- data.frame(study = xw$study[i], wave = xw$wave[i], target = xw$target[i],
                    source_var = xw$source_var[i], value = value, na_reason = reason,
                    n = as.integer(n), stringsAsFactors = FALSE)
    lvl_order <- if (!is.null(r$set)) match(m$value, r$set$name) else suppressWarnings(as.numeric(m$value))
    m <- m[order(is.na(m$value), lvl_order, match(m$na_reason, reasons), method = "radix"), , drop = FALSE]
    out[[length(out) + 1L]] <- m
  }
  res <- if (length(out) > 0L) do.call(rbind, out) else .qes_apply_schema(as.data.frame(
    stats::setNames(rep(list(character(0)), length(.qes_schemas$spec_expected)), names(.qes_schemas$spec_expected)),
    stringsAsFactors = FALSE
  ), "spec_expected")
  rownames(res) <- NULL
  attr(res, "unprojected") <- if (length(skipped) > 0L) do.call(rbind, skipped) else NULL
  res
}

# Numeric labels of `codes` of one variable (from_label rows).
.qes_hz_label_numbers <- function(sources, study, variable, codes) {
  val <- sources$values
  val <- val[val$study == study & val$variable == variable, , drop = FALSE]
  val$label_number[match(codes, val$value)]
}

# V-P1: the projection against the recorded marginals `expected` (default:
# the spec's expected/marginals.csv), for the studies in `sources`. Returns a
# problems table.
.qes_projection_check <- function(spec, sources, expected = spec$tables$expected, studies = NULL) {
  studies <- studies %||% unique(c(names(sources$n_rows), sources$gates$study))
  proj <- .qes_project_marginals(spec, sources, studies)
  prob <- list()
  add <- function(key, detail, severity = "error") {
    prob[[length(prob) + 1L]] <<- .qes_spec_problems("V-P1", severity, "expected", NA_integer_, key, detail)
  }
  un <- attr(proj, "unprojected")
  if (!is.null(un)) {
    for (k in seq_len(nrow(un))) {
      add(paste(un$study[k], un$wave[k], un$target[k], un$source_var[k], sep = "/"), un$reason[k])
    }
  }
  exp_ <- expected[expected$study %in% studies, , drop = FALSE]
  cell_key <- function(x) paste(x$study, x$wave, x$target, x$source_var, sep = "/")
  val_key <- function(x) paste(cell_key(x), ifelse(is.na(x$value), "", x$value), ifelse(is.na(x$na_reason), "", x$na_reason), sep = "|")
  for (k in unique(c(cell_key(proj), cell_key(exp_)))) {
    p <- proj[cell_key(proj) == k, , drop = FALSE]
    e <- exp_[cell_key(exp_) == k, , drop = FALSE]
    if (nrow(e) == 0L) {
      add(k, "projected but not in expected/marginals.csv; run data-raw/project_marginals.R")
      next
    }
    if (nrow(p) == 0L) {
      if (is.null(un) || !k %in% cell_key(un)) {
        add(k, "in expected/marginals.csv but not a projectable row of the spec")
      }
      next
    }
    pv <- stats::setNames(p$n, val_key(p))
    ev <- stats::setNames(e$n, val_key(e))
    keys <- union(names(pv), names(ev))
    diff_ <- keys[is.na(pv[keys]) | is.na(ev[keys]) | pv[keys] != ev[keys]]
    if (length(diff_) > 0L) {
      show <- utils::head(diff_, 4L)
      parts <- strsplit(sub("^[^|]*\\|", "", show), "|", fixed = TRUE)
      what <- vapply(parts, function(x) if (nzchar(x[1])) x[1] else x[2], character(1))
      add(k, paste0("projected marginals differ from expected/marginals.csv: ", paste(sprintf(
        "%s %s (expected %s)", what,
        ifelse(is.na(pv[show]), "0", pv[show]), ifelse(is.na(ev[show]), "0", ev[show])
      ), collapse = "; ")))
    }
  }
  if (length(prob) > 0L) do.call(rbind, prob) else .qes_spec_problems()
}

# The kind of change from marginals `old` to `new` (spec_expected tables):
# "major" when a recorded value changes or disappears (design.md section
# 5.11), "minor" when rows are only added, "none" otherwise. The keys that
# moved are in attr(, "keys"). A cell is (study, wave, target, source_var),
# the crosswalk key: a row that changes its source shows as a cell gone and
# a cell added, which is major.
.qes_expected_change <- function(old, new) {
  cell <- function(x) paste(x$study, x$wave, x$target, x$source_var, sep = "|")
  key <- function(x) paste(cell(x), ifelse(is.na(x$value), "", x$value),
                           ifelse(is.na(x$na_reason), "", x$na_reason), sep = "|")
  ov <- stats::setNames(old$n, key(old))
  nv <- stats::setNames(new$n, key(new))
  cells_old <- unique(cell(old))
  cells_new <- unique(cell(new))
  gone <- setdiff(cells_old, cells_new)
  # within a cell present in both, a value that appears or moves changes the
  # recorded marginals
  shared <- intersect(cells_old, cells_new)
  ko <- names(ov)[cell(old) %in% shared]
  kn <- names(nv)[cell(new) %in% shared]
  all_k <- union(ko, kn)
  moved <- all_k[is.na(ov[all_k]) | is.na(nv[all_k]) | ov[all_k] != nv[all_k]]
  keys <- unique(c(gone, moved))
  kind <- if (length(keys) > 0L) "major" else if (length(setdiff(cells_new, cells_old)) > 0L) "minor" else "none"
  structure(kind, keys = keys)
}

# ---- data checks V-D1 to V-D8 (V-D6, the md5 pin, is the reader's) -------------

# The data checks of `spec` against `sources` (studies with variables in
# `sources`; default all). With `data`, the named list of data frames that
# `sources` was built from, V-D5 runs too and V-D8 counts members in the
# data. `offline_severity` is the severity of a check that the aggregates
# cannot answer (a row without cells in gates.csv). Returns a problems table.
.qes_data_check <- function(spec, sources, studies = NULL, data = NULL, offline_severity = "error") {
  xw <- spec$tables$crosswalk
  vm <- spec$tables$valuemaps
  wv <- spec$tables$waves
  wt <- spec$tables$weights
  studies <- studies %||% unique(sources$variables$study)
  out <- list()
  add <- function(rule, table, row, key, detail, severity = "error") {
    if (length(row) == 0L || length(key) == 0L) return(invisible())
    n <- max(length(row), length(key))
    out[[length(out) + 1L]] <<- .qes_spec_problems(
      rep_len(rule, n), rep_len(severity, n), rep_len(table, n),
      rep_len(as.integer(row), n), rep_len(key, n), rep_len(detail, n)
    )
    invisible()
  }
  has <- function(x) !is.na(x) & nzchar(x)
  xkey <- function(i) paste(xw$study[i], xw$wave[i], xw$target[i], xw$source_var[i], sep = "/")
  vars_of <- function(study) sources$variables$variable[sources$variables$study == study]
  values_of <- function(study, var) {
    sources$values[sources$values$study == study & sources$values$variable == var, , drop = FALSE]
  }
  hint <- function(var, study) {
    s <- .qes_suggest_variables(var, vars_of(study), n = 3L)
    if (length(s) > 0L) sprintf(" (did you mean %s?)", paste(s, collapse = ", ")) else ""
  }
  exists_in <- function(var, study) var %in% vars_of(study)

  # ---- V-D1: variables exist, with exact case ------------------------------------
  for (i in which(xw$study %in% studies)) {
    for (col in c("source_var", "gate_var")) {
      v <- xw[[col]][i]
      if (has(v) && !exists_in(v, xw$study[i])) {
        add("V-D1", "crosswalk", i, xkey(i), sprintf("%s '%s' is not a variable of %s%s", col, v, xw$study[i], hint(v, xw$study[i])))
      }
    }
  }
  for (w in which(wv$study %in% studies)) {
    mode_var <- if (has(wv$mode[w]) && startsWith(wv$mode[w], "var:")) sub("^var:", "", wv$mode[w]) else NA_character_
    for (col in c("member_var", "date_var", "subsample_var", "strata_var", "mode")) {
      v <- if (col == "mode") mode_var else wv[[col]][w]
      if (has(v) && !exists_in(v, wv$study[w])) {
        add("V-D1", "waves", w, paste(wv$study[w], wv$wave[w], sep = "/"),
            sprintf("%s '%s' is not a variable of %s%s", col, v, wv$study[w], hint(v, wv$study[w])))
      }
    }
  }
  for (k in which(wt$study %in% studies)) {
    v <- wt$weight_var[k]
    if (has(v) && !exists_in(v, wt$study[k])) {
      add("V-D1", "weights", k, paste(wt$study[k], wt$wave[k], v, sep = "/"),
          sprintf("weight_var '%s' is not a variable of %s%s", v, wt$study[k], hint(v, wt$study[k])))
    }
  }

  # ---- V-D2, V-D4: observed codes are mapped; sentinels and declared codes -----------
  for (i in which(xw$study %in% studies)) {
    if (!.qes_hz_projected(xw, i) || !exists_in(xw$source_var[i], xw$study[i])) next
    r <- .qes_hz_row_rule(spec, i)
    val <- values_of(xw$study[i], xw$source_var[i])
    obs <- val[!is.na(val$n) & val$n > 0L, , drop = FALSE]
    # codes seen among the wave's members (gates.csv) count as observed too
    g <- sources$gates
    gc <- g$source_code[g$study == xw$study[i] & g$wave == xw$wave[i] & g$source_var == xw$source_var[i] &
                          g$n > 0L & g$source_code != "NA"]
    extra <- setdiff(unique(gc), obs$value)
    codes <- c(obs$value, extra)
    ln <- c(obs$label_number, val$label_number[match(extra, val$value)])
    if (length(codes) > 0L) {
      oc <- .qes_hz_outcome(r, codes, ln)
      bad <- codes[oc$na_reason %in% "unmapped"]
      if (length(bad) > 0L) {
        add("V-D2", "crosswalk", i, xkey(i), sprintf("observed code(s) %s have no outcome (not in the value map, the range or na_codes)", paste(utils::head(bad, 10L), collapse = ", ")))
      }
      # a code the dictionary types as missing must not become a value
      mt <- val$missing_type[match(codes, val$value)]
      leak <- codes[!is.na(mt) & !is.na(oc$value)]
      if (length(leak) > 0L) {
        add("V-D2", "crosswalk", i, xkey(i), sprintf("code(s) %s are missing codes in the dictionary (%s) but become values", paste(leak, collapse = ", "), paste(unique(mt[match(leak, codes)]), collapse = ", ")))
      }
      if (identical(r$rule, "numeric")) {
        sentinel <- c("-99", "8", "9", "97", "98", "99", "998", "999", "9999")
        lab <- val$label[match(codes, val$value)]
        s <- codes[codes %in% sentinel & !is.na(oc$value) & !is.na(lab) & is.na(.qes_label_number(lab)) &
                     !grepl("^-?[0-9]", trimws(lab))]
        if (length(s) > 0L) {
          add("V-D2", "crosswalk", i, xkey(i), sprintf("sentinel code(s) %s carry a text label but pass as values", paste(s, collapse = ", ")))
        }
      }
    }
    # V-D4: codes the file declares missing, where observed, are named explicitly
    na_decl <- sources$variables$na_values[sources$variables$study == xw$study[i] & sources$variables$variable == xw$source_var[i]]
    if (length(na_decl) == 1L && has(na_decl)) {
      decl <- codes[.qes_is_declared(codes, na_decl)]
      explicit <- c(names(r$na_codes), if (identical(r$rule, "map")) r$map_code)
      bad <- setdiff(decl, explicit)
      if (length(bad) > 0L) {
        add("V-D4", "crosswalk", i, xkey(i), sprintf("code(s) %s are declared missing in the file (%s) but not named in the value map or na_codes", paste(bad, collapse = ", "), na_decl))
      }
    }
  }

  # ---- V-D3: labels ---------------------------------------------------------------------
  for (i in which(xw$study %in% studies & xw$rule %in% "map" & has(xw$map_id))) {
    if (!exists_in(xw$source_var[i], xw$study[i])) next
    val <- values_of(xw$study[i], xw$source_var[i])
    rows <- which(vm$map_id == xw$map_id[i])
    for (k in rows) {
      j <- match(vm$source_code[k], val$value)
      file_label <- if (is.na(j)) NA_character_ else val$label[j]
      file_hash <- if (is.na(j)) NA_character_ else val$label_hash[j]
      if (is.na(file_hash) && !is.na(file_label)) file_hash <- .qes_md5_text(.qes_norm_label(file_label))
      origin <- vm$source_label_origin[k]
      vkey <- paste(vm$map_id[k], vm$source_code[k], sep = "/")
      if (has(vm$source_label[k])) {
        if (!is.na(file_label)) {
          if (!identical(.qes_norm_label(file_label), .qes_norm_label(vm$source_label[k]))) {
            add("V-D3", "valuemaps", k, vkey, sprintf("%s %s: the file labels code %s '%s', the value map '%s'", xw$study[i], xw$source_var[i], vm$source_code[k], file_label, vm$source_label[k]))
          }
        } else if (origin %in% c("file", "label_donor") && (is.na(j) || is.na(file_hash))) {
          add("V-D3", "valuemaps", k, vkey, sprintf("%s %s: the value map quotes a file label for code %s, but the file has none", xw$study[i], xw$source_var[i], vm$source_code[k]))
        }
      } else if (has(vm$source_label_hash[k])) {
        if (is.na(file_hash)) {
          add("V-D3", "valuemaps", k, vkey, sprintf("%s %s: the value map has a label hash for code %s, but the file has no label", xw$study[i], xw$source_var[i], vm$source_code[k]))
        } else if (!identical(file_hash, vm$source_label_hash[k])) {
          add("V-D3", "valuemaps", k, vkey, sprintf("%s %s: the label of code %s does not match the value map's label hash", xw$study[i], xw$source_var[i], vm$source_code[k]))
        }
      }
    }
  }

  # ---- V-D7: universe identity on gated rows ----------------------------------------------
  for (i in which(xw$study %in% studies & has(xw$gate_var))) {
    if (!.qes_hz_projected(xw, i)) next
    if (!exists_in(xw$source_var[i], xw$study[i]) || !exists_in(xw$gate_var[i], xw$study[i])) next
    cells <- .qes_hz_cells(spec, sources, i)
    if (is.character(cells)) {
      add("V-D7", "crosswalk", i, xkey(i), cells, severity = offline_severity)
      next
    }
    closed <- .qes_split_list(xw$gate_codes[i])
    answered <- tapply(cells$n * (cells$source_code != "NA"), cells$gate_code, sum)
    missing_ <- tapply(cells$n * (cells$source_code == "NA"), cells$gate_code, sum)
    for (gcode in names(answered)) {
      if (gcode %in% closed && answered[[gcode]] > 0L) {
        add("V-D7", "crosswalk", i, xkey(i), sprintf("%d respondents with %s = %s (gate closed) have a value of %s", answered[[gcode]], xw$gate_var[i], gcode, xw$source_var[i]))
      }
      if (!gcode %in% closed && missing_[[gcode]] > 0L) {
        add("V-D7", "crosswalk", i, xkey(i), sprintf("%d respondents with %s = %s have no value of %s and the gate gives them no outcome", missing_[[gcode]], xw$gate_var[i], gcode, xw$source_var[i]))
      }
    }
  }

  # ---- V-D8: wave membership counts -------------------------------------------------------
  for (w in which(wv$study %in% studies)) {
    study <- wv$study[w]
    rule <- .qes_member_rule(wv, w)
    wkey <- paste(study, wv$wave[w], sep = "/")
    n_rows <- unname(sources$n_rows[study])
    n <- NA_integer_
    if (!is.null(data) && !is.null(data[[study]])) {
      mem <- .qes_wave_members(wv, w, data[[study]])
      if (!is.null(mem)) n <- sum(mem)
    } else if (rule == "") {
      n <- n_rows
    } else {
      val <- values_of(study, wv$member_var[w])
      codes <- .qes_split_list(wv$member_codes[w])
      if (!identical(codes, "!NA") && nrow(val) > 0L && !anyNA(val$n) && all(codes %in% val$value)) {
        n <- sum(val$n[val$value %in% codes])
      } else {
        g <- sources$gates
        g <- g[g$study == study & g$wave == wv$wave[w] & g$member_rule %in% rule, , drop = FALSE]
        if (nrow(g) > 0L) {
          first <- g[g$source_var == g$source_var[1] & (g$gate_var %in% g$gate_var[1] | (is.na(g$gate_var) & is.na(g$gate_var[1]))), , drop = FALSE]
          n <- sum(first$n)
        }
      }
    }
    if (is.na(n)) {
      if (rule != "" && exists_in(wv$member_var[w], study)) {
        add("V-D8", "waves", w, wkey, "membership cannot be counted from the dictionary or gates.csv; run data-raw/build_sources.R",
            severity = offline_severity)
      }
    } else if (!identical(as.integer(n), wv$n_cases[w])) {
      add("V-D8", "waves", w, wkey, sprintf("%d rows are members of the wave, n_cases says %d", n, wv$n_cases[w]))
    }
  }

  # ---- V-D5: identifiers are unique (data only) -----------------------------------------------
  if (!is.null(data)) {
    files <- .qes_catalog()$files
    for (study in intersect(names(data), studies)) {
      f <- files[files$study == study & files$role == "data" & files$is_default %in% TRUE, , drop = FALSE]
      # ".row" is the position in the pinned file: unique by construction
      ids <- setdiff(.qes_split_list(f$id_vars[1]), ".row")
      d <- data[[study]]
      if (length(ids) == 0L) next
      if (!all(ids %in% names(d))) {
        add("V-D5", "catalog", NA, study, sprintf("id variable(s) %s are not in the data", paste(setdiff(ids, names(d)), collapse = ", ")))
        next
      }
      key <- do.call(paste, c(lapply(ids, function(v) .canon(d[[v]])), sep = "\x1f"))
      dup <- which(duplicated(key) | duplicated(key, fromLast = TRUE))
      if (length(dup) > 0L) {
        add("V-D5", "catalog", NA, study, sprintf("%d rows share an identifier (%s), e.g. rows %s", length(dup), paste(ids, collapse = ", "), paste(utils::head(dup, 5L), collapse = ", ")))
      }
    }
  }

  res <- if (length(out) > 0L) do.call(rbind, out) else .qes_spec_problems()
  rownames(res) <- NULL
  res
}

# The data checks of `spec` on data frames (named list, study code -> data
# frame as get_qes() returns it): V-D1 to V-D5, V-D7 and V-D8.
.qes_data_check_frames <- function(spec, data) {
  sources <- .qes_hz_sources_data(spec, data)
  .qes_data_check(spec, sources, studies = names(data), data = data)
}
