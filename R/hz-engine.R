# The harmonization engine (design.md sections 5.6, 5.8 and 5.9, slice HZ3).
#
# qes_harmonize() applies the reviewed crosswalk rows of the spec to the
# pinned data files (or to data frames the user gives) and returns one row
# per respondent. Its only internal representation is a cell table: for each
# study and target, a vector of values (a level name, a number or date as
# text) and a vector of NA reasons, aligned on the rows of the study's file.
# Value encodings (factor, labelled, code), the NA-reason companions and
# the provenance are pure functions of it.
#
# Per study:
#   1. read the pinned file through .qes_read() (md5 and dimensions checked),
#      or take the user's frame (recorded as unverified, with a warning);
#   2. check identifiers and run the data checks V-D1 to V-D4, V-D7 and V-D8
#      (R/hz-data.R) on the crosswalk rows to be applied;
#   3. apply wave membership: a respondent outside a row's wave gets reason
#      not_in_wave;
#   4. for each selected target, the study's primary crosswalk row (status
#      stable; review and draft only with include_draft = TRUE, else reason
#      not_reviewed; grade at least min_grade, else reason below_grade; no
#      row: reason not_asked). Cell provenance records why a cell was left
#      out in `excluded` (no_row, not_reviewed, not_in_data, below_grade):
#      .canon() of the source, then the rule, na_codes and the gate (per
#      code), then system missing -> sysmis; every NA gets a reason;
#   5. codes that no rule, map, range or na_code covers are "unmapped": an
#      error by default, NA with reason unmapped under unmapped = "warn" or
#      "na". A raw code never passes through.
# Waves, weights, eligibility, interview dates and modes (slice HZ4) are in
# R/hz-waves.R; the long layout is built here from the same per-study parts.
#
# The synthetic study qes_demo stands in for qes2014 (its variables are a
# subset of the qes2014 file): it is harmonized with the qes2014 rows whose
# variables it has; the others are not_asked.

.qes_hz_grades <- c("identical", "comparable", "approximate")

# The leading columns of harmonized data (both layouts): no target that is
# not itself a leading column (V-S15) and no pooled variable (V-F2) may take
# one of these names.
.qes_hz_leading_columns <- c("study", "year", "election_date", "family", "study_design", "target_population",
                             "waves", "wave", "wave_timing", "wave_design", "qes_id", "subsample", "stratum",
                             "source_row", "survey_mode", "interview_date", "days_to_election", "eligible_voter",
                             "weight_pre", "weight_post", "weight_pre_var", "weight_post_var", "weight",
                             "weight_var", "weight_auto")
.qes_hz_stand_ins <- c(qes_demo = "qes2014")

# The study whose spec rows `study` uses.
.qes_hz_spec_study <- function(study) {
  if (study %in% names(.qes_hz_stand_ins)) .qes_hz_stand_ins[[study]] else study
}

# Studies the spec covers: with a wave and at least one mapped crosswalk row,
# in catalog order.
.qes_hz_covered <- function(spec) {
  xw <- spec$tables$crosswalk
  mapped <- unique(xw$study[!is.na(xw$rule) & xw$rule != "none"])
  mapped <- intersect(mapped, spec$tables$waves$study)
  codes <- .qes_study_codes()
  c(intersect(codes, mapped), setdiff(mapped, codes))
}

# Crosswalk row statuses the engine applies: rows signed off by a reviewer
# ("stable") only, unless include_draft = TRUE, which also applies the rows
# not yet signed off ("review" and "draft"). Design rule P2: nothing is
# harmonized unless a reviewed spec row says so.
.qes_hz_statuses <- function(include_draft) {
  c("stable", if (isTRUE(include_draft)) c("review", "draft"))
}

# ---- arguments ------------------------------------------------------------------

# The studies to harmonize: NULL is the QES election studies the spec covers
# (catalog default_member), or the names of `data` when it is given; "all" is
# every study the spec covers.
.qes_hz_resolve_studies <- function(studies, data, spec) {
  covered <- .qes_hz_covered(spec)
  if (is.null(studies)) {
    if (!is.null(data)) {
      out <- names(data)
    } else {
      cat_ <- .qes_catalog()$studies
      out <- intersect(cat_$study[cat_$default_member %in% TRUE], covered)
    }
  } else if (is.character(studies) && length(studies) == 1L && identical(.qes_canon_code(studies), "all")) {
    out <- covered
  } else {
    out <- .qes_resolve_codes(studies, "studies", demo = TRUE)
  }
  uncovered <- out[!vapply(out, .qes_hz_spec_study, character(1)) %in% covered]
  if (length(uncovered) > 0L) {
    .qes_abort(
      "input_harmonize_study",
      class = "qesR_error_input",
      args = list(.qes_q(uncovered), .qes_q(covered)),
      data = list(arg = "studies", value = uncovered)
    )
  }
  extra <- setdiff(names(data), out)
  if (length(extra) > 0L) {
    .qes_abort(
      "input_harmonize_data_names",
      class = "qesR_error_input",
      args = list(.qes_q(extra)),
      data = list(arg = "data", value = extra)
    )
  }
  out
}

# Target, family, set and pooled variable names (disjoint, V-S15 and V-F2)
# -> target names, in the order of targets.csv (the pooled variables named
# are resolved apart, by .qes_pool_resolve()). A retired target is included
# only when named. The targets of the id and design blocks are leading
# columns of harmonized data: naming one is an error, unless `leading =
# TRUE` (the spec views).
.qes_hz_resolve_targets <- function(targets, spec, leading = FALSE) {
  tg <- spec$tables$targets
  if (!is.character(targets) || length(targets) == 0L || anyNA(targets) || !all(nzchar(targets))) {
    .qes_abort("input_targets", class = "qesR_error_input", data = list(arg = "targets", value = targets))
  }
  sets <- lapply(tg$sets, .qes_split_list)
  live <- !tg$status %in% "retired"
  hit <- rep(FALSE, nrow(tg))
  unknown <- character(0)
  # pooled variables (R/hz-pool.R), and the sets that name them, are known
  # names; .qes_pool_resolve() gives the pooled variables themselves
  pl <- .qes_pool_tables(spec)$pooled
  pool_names <- unique(c(pl$pooled, unlist(lapply(pl$sets[!pl$status %in% "retired"], .qes_split_list))))
  pools <- .qes_pool_resolve(targets, spec)
  for (t in unique(targets)) {
    by_target <- tg$target == t
    by_group <- live & (tg$family %in% t | vapply(sets, function(s) t %in% s, logical(1)))
    if (!any(by_target) && !any(by_group) && !t %in% pool_names) {
      unknown <- c(unknown, t)
    }
    hit <- hit | by_target | by_group
  }
  named_leading <- intersect(unique(targets), .qes_hz_leading_targets(spec))
  if (!leading && length(named_leading) > 0L) {
    .qes_abort(
      "input_targets_leading",
      class = "qesR_error_input",
      args = list(.qes_q(named_leading)),
      data = list(arg = "targets", value = named_leading)
    )
  }
  if (length(unknown) > 0L) {
    names_ <- unique(c(tg$target, tg$family[!is.na(tg$family)], unlist(sets), pool_names))
    suggestions <- unique(unlist(lapply(unknown, .qes_suggest_codes, codes = names_)))
    .qes_abort(
      if (length(suggestions) > 0L) "input_targets_unknown_suggest" else "input_targets_unknown",
      class = "qesR_error_input",
      args = if (length(suggestions) > 0L) list(.qes_q(unknown), .qes_q(suggestions)) else list(.qes_q(unknown)),
      data = list(arg = "targets", value = unknown, suggestions = suggestions %||% character(0))
    )
  }
  # targets of the id and design blocks are leading columns, never target
  # columns (a family or set that names one does not add it)
  if (leading) {
    return(tg$target[hit])
  }
  lead_hit <- hit & tg$target %in% .qes_hz_leading_targets(spec)
  if (!any(hit & !lead_hit) && length(pools) == 0L) {
    .qes_abort("input_targets_leading", class = "qesR_error_input", args = list(.qes_q(tg$target[lead_hit])),
               data = list(arg = "targets", value = targets))
  }
  tg$target[hit & !lead_hit]
}

# ---- study data -------------------------------------------------------------------

# Study-level provenance of a frame the user gave: nothing about its origin
# is verified.
.qes_hz_user_provenance <- function(study, d) {
  cat_ <- .qes_catalog(demo = TRUE)
  study_row <- cat_$studies[match(study, cat_$studies$study), , drop = FALSE]
  file_row <- .qes_default_data_file(study, demo = TRUE)
  file_row$n_rows <- nrow(d)
  file_row$n_cols <- ncol(d)
  .qes_provenance_row(
    study_row, file_row,
    md5_observed = NA_character_, md5_verified = FALSE, pinned = NA,
    retrieved_via = "user_data", retrieved_at = NA
  )
}

# A structural fingerprint of a user's frame: md5 of its names, row count,
# and the code set and label hashes of the variables the spec names.
.qes_hz_fingerprint <- function(d, vars) {
  parts <- c(paste(names(d), collapse = "\x1f"), as.character(nrow(d)))
  for (v in sort(intersect(vars, names(d)), method = "radix")) {
    x <- d[[v]]
    codes <- sort(unique(.canon(x)), method = "radix", na.last = TRUE)
    labs <- attr(x, "labels", exact = TRUE)
    lab <- if (length(labs) > 0L) paste(.canon(unname(unclass(labs))), .qes_norm_label(names(labs)), sep = "=") else character(0)
    parts <- c(parts, paste(v, paste(codes, collapse = ","), paste(lab, collapse = ","), sep = "\x1e"))
  }
  .qes_md5_text(paste(parts, collapse = "\x1d"))
}

# qes_id keys of the rows of `d`: the catalog id_vars of `study` joined with
# "-" (".row" is the position in the file). Missing or duplicated
# identifiers are errors. `user`: `d` is the user's frame (`data = `), whose
# missing identifier is named as a column dropped from it.
.qes_hz_ids <- function(study, d, user = FALSE) {
  file_row <- .qes_default_data_file(study, demo = TRUE)
  ids <- .qes_split_list(file_row$id_vars)
  if (length(ids) == 0L || identical(ids, ".row")) {
    return(list(key = as.character(seq_len(nrow(d))), id_vars = ".row"))
  }
  ids <- setdiff(ids, ".row")
  miss <- setdiff(ids, names(d))
  if (length(miss) > 0L) {
    if (isTRUE(user)) {
      .qes_abort(
        "hz_missing_id",
        class = "qesR_error_unknown_variable",
        args = list(paste0("data$", study), .qes_q(miss)),
        data = list(study = study, variables = miss, suggestions = character(0))
      )
    }
    .qes_abort(
      "unknown_variable_study",
      class = "qesR_error_unknown_variable",
      args = list(.qes_q(miss), .qes_q(study)),
      data = list(study = study, variables = miss, suggestions = character(0))
    )
  }
  parts <- lapply(ids, function(v) {
    x <- .canon(d[[v]])
    x[is.na(x)] <- "NA"
    x
  })
  key <- do.call(paste, c(parts, sep = "-"))
  dup <- which(duplicated(key) | duplicated(key, fromLast = TRUE))
  if (length(dup) > 0L) {
    .qes_abort(
      "duplicate_id",
      class = "qesR_error_duplicate_id",
      args = list(.qes_q(study), length(dup), .qes_q(ids), utils::head(dup, 5L)),
      data = list(study = study, id_vars = ids, rows = dup)
    )
  }
  list(key = key, id_vars = ids)
}

# The data checks of the rows the engine applies, on the study's frame.
# Pinned files must pass them all; for a user's frame, V-D3 (labels) and
# V-D7 (universe) are warnings and V-D8 (member counts) is skipped.
.qes_hz_check_study <- function(sub, d, study, verified, stand_in) {
  data <- stats::setNames(list(d), study)
  sources <- .qes_hz_sources_data(sub, data)
  p <- .qes_data_check(sub, sources, studies = study, data = data,
                       unmapped_codes = FALSE, member_counts = verified && !stand_in)
  err <- p[p$severity == "error", , drop = FALSE]
  if (!verified) {
    for (rule in c("V-D3", "V-D7")) {
      k <- err$rule == rule
      if (any(k)) {
        id <- if (rule == "V-D3") "label_mismatch" else "universe"
        .qes_warn(
          id,
          class = paste0("qesR_warning_", id),
          args = list(.qes_q(study), paste(utils::head(err$detail[k], 3L), collapse = "; ")),
          data = list(study = study, problems = err[k, , drop = FALSE])
        )
      }
    }
    err <- err[!err$rule %in% c("V-D3", "V-D7"), , drop = FALSE]
  }
  if (nrow(err) > 0L) {
    shown <- utils::head(err, 5L)
    .qes_abort(
      "harmonize_data_invalid",
      class = "qesR_error_spec",
      args = list(.qes_q(study), nrow(err)),
      data = list(study = study, problems = err),
      details = sprintf("%s %s", shown$rule, shown$detail)
    )
  }
  invisible(p)
}

# ---- one crosswalk row --------------------------------------------------------------

# Numeric labels of the codes of column `x` (rule option from_label).
.qes_hz_col_label_numbers <- function(x, code) {
  labs <- attr(x, "labels", exact = TRUE)
  if (length(labs) == 0L) {
    return(rep(NA_real_, length(code)))
  }
  lc <- .canon(unname(unclass(labs)))
  .qes_label_number(names(labs))[match(code, lc)]
}

# Value label text of the codes `code` of column `x` (rule string with
# from_label), trimmed at both ends and otherwise as in the file; NA for a
# code without a label or with a blank one.
.qes_hz_col_labels <- function(x, code) {
  labs <- attr(x, "labels", exact = TRUE)
  if (length(labs) == 0L) {
    return(rep(NA_character_, length(code)))
  }
  lc <- .canon(unname(unclass(labs)))
  txt <- trimws(enc2utf8(names(labs)))
  txt[!nzchar(txt)] <- NA_character_
  txt[match(code, lc)]
}

# Dates from source values in `format` (yyyymmdd, posixct, stata) as ISO
# text; NA where a value does not read as a date.
.qes_hz_dates <- function(x, format) {
  raw <- unclass(x)
  attributes(raw) <- NULL
  out <- switch(
    format %||% "yyyymmdd",
    yyyymmdd = as.Date(.qes_code_chr(suppressWarnings(as.numeric(raw))), format = "%Y%m%d"),
    stata = as.Date(suppressWarnings(as.numeric(raw)), origin = "1960-01-01"),
    posixct = if (inherits(x, "POSIXt")) as.Date(format(x, tz = "UTC", usetz = FALSE)) else
      as.Date(as.POSIXct(suppressWarnings(as.numeric(raw)), origin = "1970-01-01", tz = "UTC")),
    as.Date(rep(NA_character_, length(raw)))
  )
  format(out, "%Y-%m-%d")
}

# Apply crosswalk row `i` of `spec` to frame `d`: list(value, reason, src),
# each a character vector over the rows of `d`. Respondents outside the
# row's wave (`member` FALSE) get reason not_in_wave.
.qes_hz_apply_row <- function(spec, i, d, member) {
  xw <- spec$tables$crosswalk
  n <- nrow(d)
  rule <- xw$rule[i]
  base <- if (startsWith(rule, "fn:")) "fn" else rule
  x <- d[[xw$source_var[i]]]
  src <- .canon(x)
  code <- src
  code[is.na(code)] <- "NA"
  r <- .qes_hz_row_rule(spec, i)
  args <- .qes_parse_kv(xw$args[i]) %||% character(0)
  if (base %in% c("map", "numeric", "coalesce")) {
    ln <- if (r$from_label) .qes_hz_col_label_numbers(x, code) else rep(NA_real_, n)
    gate <- rep(NA_character_, n)
    if (!is.na(r$gate_var)) {
      gate <- .canon(d[[r$gate_var]])
      gate[is.na(gate)] <- "NA"
    }
    oc <- .qes_hz_cell_outcome(r, data.frame(gate_code = gate, source_code = code, stringsAsFactors = FALSE), ln)
    value <- oc$value
    reason <- oc$na_reason
    if (identical(base, "coalesce")) {
      # the next variables, each with its own map, for the rows whose
      # outcome so far is a fallthrough reason; a variable that did not ask
      # the respondent (inapplicable, system missing) leaves the outcome as
      # it was (see .qes_hz_coalesce_then() in R/hz-data.R). src records
      # "<variable>=<code>" of the variable that decided
      ft <- .qes_hz_coalesce_fallthrough(xw, i)
      src <- ifelse(is.na(src), NA_character_, paste0(xw$source_var[i], "=", src))
      then <- .qes_hz_coalesce_then(xw, i)
      for (j in seq_len(nrow(then))) {
        open <- which(is.na(value) & reason %in% ft)
        if (length(open) == 0L) break
        rj <- .qes_hz_part_rule(spec, i, then$var[j], then$map_id[j])
        sj <- .canon(d[[then$var[j]]])
        cj <- sj
        cj[is.na(cj)] <- "NA"
        oj <- .qes_hz_outcome(rj, cj[open])
        asked <- !is.na(oj$value) | !oj$na_reason %in% .qes_coalesce_default_fallthrough
        k <- open[asked]
        value[k] <- oj$value[asked]
        reason[k] <- oj$na_reason[asked]
        src[k] <- paste0(then$var[j], "=", sj[k])
      }
    }
  } else {
    value <- rep(NA_character_, n)
    reason <- rep(NA_character_, n)
    nac <- r$na_codes
    in_nac <- code %in% names(nac)
    reason[in_nac] <- unname(nac[code[in_nac]])
    rest <- !in_nac
    if (base == "weight") {
      w <- suppressWarnings(as.numeric(src))
      ok <- rest & !is.na(w) & w > 0
      value[ok] <- .qes_code_chr(w[ok])
      reason[rest & !ok] <- "sysmis"
    } else if (base == "date") {
      fmt <- unname(args["format"])
      if (length(fmt) == 0L || is.na(fmt)) fmt <- "yyyymmdd"
      dt <- .qes_hz_dates(x, fmt)
      ok <- rest & !is.na(dt)
      value[ok] <- dt[ok]
      reason[rest & code == "NA"] <- "sysmis"
      reason[rest & code != "NA" & !ok] <- "unmapped"
    } else if (base == "string") {
      ok <- rest & !is.na(src) & nzchar(src)
      if (identical(unname(args["from_label"]), "TRUE")) {
        # the text of each code's value label in the file (study-scoped
        # text targets such as income brackets); a code without a label is
        # unmapped, never passed through as a number
        lab <- .qes_hz_col_labels(x, src)
        value[ok & !is.na(lab)] <- lab[ok & !is.na(lab)]
        reason[ok & is.na(lab)] <- "unmapped"
      } else {
        value[ok] <- src[ok]
      }
      reason[rest & !ok] <- "sysmis"
    } else if (base == "constant") {
      value[] <- unname(args["value"])
    } else if (base == "fn") {
      f <- .qes_hz_fns[[sub("^fn:", "", rule)]]
      res <- f(x, list(data = d, row = xw[i, , drop = FALSE], spec = spec))
      value <- as.character(res$value)
      reason <- as.character(res$na_reason)
    }
    # the gate of a weight, date or string row, as for map and numeric rows
    # (.qes_hz_cell_outcome()): a gate code listed in gate_to overrides the
    # source's outcome, system missing included; these targets have no level
    # set, so the outcome is an NA reason (V-S3)
    if (base %in% c("weight", "date", "string") && !is.na(r$gate_var) && length(r$gate_to) > 0L) {
      gate <- .canon(d[[r$gate_var]])
      gate[is.na(gate)] <- "NA"
      closed <- gate %in% names(r$gate_to)
      value[closed] <- NA_character_
      reason[closed] <- unname(r$gate_to[gate[closed]])
    }
  }
  value[!member] <- NA_character_
  reason[!member] <- "not_in_wave"
  reason[!is.na(value)] <- NA_character_
  if (any(is.na(value) & is.na(reason))) {
    stop(sprintf("qesR internal error: %s %s left a missing value without a reason.",
                 xw$study[i], xw$target[i]), call. = FALSE)
  }
  list(value = value, reason = reason, src = src)
}

# ---- one study ------------------------------------------------------------------------

# The crosswalk row of target `t` in the study's rows `xw`, and why a target
# has none: list(row, reason, excluded, note). `grades = FALSE` ignores
# min_grade (the design inputs of the leading columns).
.qes_hz_pick <- function(t, xw, d, ctx, stand_in, grades = TRUE) {
  rank <- function(g) match(g, .qes_hz_grades)
  rows <- which(xw$target == t & xw$primary %in% TRUE & !is.na(xw$rule) & xw$rule != "none")
  # the legacy renderers (R/legacy.R) read only rows a reviewer has seen: a
  # row added after spec 4.2.0 and not yet reviewed (reviewed_by empty,
  # status review or draft) does not exist for them, so the legacy columns
  # stay as they were (V-S19)
  if (isTRUE(ctx$legacy)) {
    rows <- rows[!(is.na(xw$reviewed_by[rows]) & !xw$status[rows] %in% "stable")]
  }
  if (length(rows) == 0L) {
    return(list(row = NA_integer_, reason = "not_asked", excluded = "no_row",
                note = "no row in the spec for this study"))
  }
  i <- rows[1]
  vars <- stats::na.omit(c(.qes_hz_row_vars(xw, i), xw$gate_var[i]))
  if (!xw$status[i] %in% .qes_hz_statuses(ctx$include_draft)) {
    list(row = i, reason = "not_reviewed", excluded = "not_reviewed",
         note = sprintf("row with status %s, not signed off by a reviewer: not applied (include_draft = FALSE)", xw$status[i]))
  } else if (stand_in && !all(vars %in% names(d))) {
    list(row = i, reason = "not_asked", excluded = "not_in_data", note = "not in the demonstration data")
  } else if (grades && (is.na(rank(xw$grade[i])) || rank(xw$grade[i]) > rank(ctx$min_grade))) {
    list(row = i, reason = "below_grade", excluded = "below_grade",
         note = sprintf("grade %s is below min_grade = \"%s\"", xw$grade[i], ctx$min_grade))
  } else {
    list(row = i, reason = NA_character_, excluded = NA_character_, note = NA_character_)
  }
}

# Frames already read through .qes_read() in this call, by study code. Only
# the legacy builders (R/legacy.R) fill it, for the length of one build, so
# that each study's file is read once even with the cache and memo off.
.qes_hz_preread <- new.env(parent = emptyenv())

# Harmonize one study: its leading columns, the values, reasons and source
# codes of each target, its wave membership and weights, and its cell and
# study provenance.
.qes_hz_study <- function(study, frame, ctx) {
  spec <- ctx$spec
  spec_study <- .qes_hz_spec_study(study)
  stand_in <- !identical(spec_study, study)
  verified <- is.null(frame)
  if (verified) {
    # a file the legacy builders have just read through .qes_read() (the
    # same pinned, md5-checked read) is not requested a second time
    d <- .qes_hz_preread$frames[[study]] %||% .qes_read(study, quiet = ctx$quiet)
    prov <- attr(d, "qes_provenance", exact = TRUE)
  } else {
    d <- frame
    prov <- .qes_hz_user_provenance(study, d)
  }
  n <- nrow(d)
  ids <- .qes_hz_ids(study, d, user = !verified)

  # the spec rows of this study, relabelled with the output study code
  relabel <- function(x) {
    x <- x[x$study %in% spec_study, , drop = FALSE]
    x$study <- rep(study, nrow(x))
    rownames(x) <- NULL
    x
  }
  xw <- relabel(spec$tables$crosswalk)
  wv <- relabel(spec$tables$waves)
  wv <- wv[order(wv$wave_order), , drop = FALSE]
  rownames(wv) <- NULL
  wt <- relabel(spec$tables$weights)

  # the row of each requested target, and of each design input (age,
  # citizenship, interview mode) that the leading columns read; design
  # inputs follow the review status but not min_grade
  pick <- lapply(ctx$targets, .qes_hz_pick, xw = xw, d = d, ctx = ctx, stand_in = stand_in)
  names(pick) <- ctx$targets
  inputs <- intersect(.qes_hz_design_inputs, spec$tables$targets$target)
  pick_in <- lapply(inputs, .qes_hz_pick, xw = xw, d = d, ctx = ctx, stand_in = stand_in, grades = FALSE)
  names(pick_in) <- inputs
  # a frame given in `data` may leave out the variables of design inputs
  # that were not requested: they are then not read (eligibility or the
  # mode is NA), rather than failing the data checks
  for (t in setdiff(inputs, ctx$targets)) {
    p <- pick_in[[t]]
    if (!verified && is.na(p$reason) && !all(stats::na.omit(c(.qes_hz_row_vars(xw, p$row), xw$gate_var[p$row])) %in% names(d))) {
      pick_in[[t]]$reason <- "not_asked"
    }
  }
  applied <- function(p) unlist(lapply(p, function(x) if (is.na(x$reason)) x$row else NULL), use.names = FALSE)
  requested_rows <- applied(pick)
  apply_rows <- unique(c(requested_rows, applied(pick_in)))

  sub <- spec
  sub$tables$crosswalk <- xw[apply_rows, , drop = FALSE]
  rownames(sub$tables$crosswalk) <- NULL
  sub$tables$waves <- wv
  sub$tables$weights <- wt
  .qes_hz_check_study(sub, d, study, verified, stand_in)

  members <- .qes_hz_member_matrix(wv, d)

  # apply each row once
  results <- list()
  unmapped <- list()
  for (k in seq_along(apply_rows)) {
    i <- apply_rows[k]
    res <- .qes_hz_apply_row(sub, k, d, .qes_hz_row_member(members, wv, xw$wave[i]))
    results[[as.character(i)]] <- res
    # codes without a mapping are an error (or warning) for the requested
    # targets only; a design input nobody asked for keeps them as NA
    # (reason "unmapped"), so eligibility or the mode is NA for those rows
    um <- res$reason %in% "unmapped"
    if (any(um) && i %in% requested_rows) {
      tab <- table(res$src[um], useNA = "ifany")
      unmapped[[length(unmapped) + 1L]] <- data.frame(
        study = study, target = xw$target[i], source_var = xw$source_var[i],
        code = names(tab), n = as.integer(tab), stringsAsFactors = FALSE
      )
    }
  }
  result_of <- function(p) {
    if (is.na(p$reason)) {
      return(results[[as.character(p$row)]])
    }
    list(value = rep(NA_character_, n), reason = rep(p$reason, n), src = rep(NA_character_, n))
  }
  unmapped <- if (length(unmapped) > 0L) do.call(rbind, unmapped) else NULL
  if (!is.null(unmapped) && identical(ctx$unmapped, "error")) {
    first <- unmapped[unmapped$target == unmapped$target[1], , drop = FALSE]
    .qes_abort(
      "unmapped",
      class = "qesR_error_unmapped",
      args = list(.qes_q(study), .qes_q(first$target[1]), .qes_q(first$code), .qes_q(first$source_var[1]), sum(first$n)),
      data = list(study = study, target = first$target[1], codes = first$code, n = first$n, unmapped = unmapped)
    )
  }

  # the design inputs that were applied, with the timing of their wave
  cat_ <- .qes_catalog(demo = TRUE)
  s <- cat_$studies[match(study, cat_$studies$study), , drop = FALSE]
  e_date <- cat_$elections$election_date[match(s$election_id, cat_$elections$election_id)]
  design <- list()
  for (t in inputs) {
    p <- pick_in[[t]]
    if (!is.na(p$reason)) next
    r <- results[[as.character(p$row)]]
    w_idx <- .qes_wave_rows(wv, xw$wave[p$row])
    timing <- unique(wv$wave_timing[w_idx])
    if (length(timing) > 1L) {
      # a row of wave "*" over waves of different timings: each
      # respondent's answer has the timing of the first of those waves
      # they took part in
      first <- .qes_hz_first_wave(members, seq_len(ncol(members)) %in% w_idx)
      timing <- wv$wave_timing[first]
    }
    design[[t]] <- list(value = r$value, reason = r$reason, wave = xw$wave[p$row], timing = timing)
  }
  eligible <- .qes_hz_eligible(design, n, e_date)

  # per wave: dates, modes and weights over the rows
  dates <- lapply(seq_len(nrow(wv)), function(w) .qes_hz_wave_dates(wv, w, d))
  modes <- lapply(seq_len(nrow(wv)), function(w) .qes_hz_wave_modes(wv, w, n, design[["survey_mode"]]))
  weights <- .qes_hz_wave_weights(wv, wt, d, members, normalize = identical(ctx$weights, "normalized"))

  values <- list()
  reasons <- list()
  srcs <- list()
  cells <- list()
  tg <- spec$tables$targets
  derived_wave <- list()
  for (t in ctx$targets) {
    p <- pick[[t]]
    res <- result_of(p)
    rule <- tg$derive_rule[match(t, tg$target)]
    dv <- NULL
    if (p$excluded %in% "no_row" && !is.na(rule) && !isTRUE(ctx$legacy)) {
      # the derivation stage (design.md section 5.6 step 6): a target the
      # study has no row for, derived from other targets it has
      dv <- .qes_hz_derive(rule, t, pick_in, results, xw, wv, n, spec, ctx)
    }
    if (!is.null(dv)) {
      res <- dv$res
      cells[[t]] <- .qes_hz_cell_row(spec, study, t, xw, dv$pick, res, wv, weights, members, eligible)
      cells[[t]]$rule <- paste0("derive:", rule)
      cells[[t]]$map_id <- NA_character_
      cells[[t]]$grade <- dv$grade
      cells[[t]]$instrument <- NA_character_
      cells[[t]]$levels_not_offered <- NA_character_
      cells[[t]]$note <- dv$note
      if (!is.na(dv$pick$reason)) {
        cells[[t]]$included <- FALSE
        cells[[t]]$excluded <- dv$pick$excluded
      }
      pick[[t]] <- dv$pick
      derived_wave[[t]] <- xw$wave[dv$pick$row]
    } else {
      cells[[t]] <- .qes_hz_cell_row(spec, study, t, xw, p, res, wv, weights, members, eligible)
    }
    values[[t]] <- res$value
    reasons[[t]] <- res$reason
    srcs[[t]] <- res$src
  }
  # the item of each target's cell (pooled variables record it per value):
  # "<study>:<wave>:<source variables joined by +>"
  cell_item <- vapply(ctx$targets, function(t) {
    i <- pick[[t]]$row
    if (is.na(i) || pick[[t]]$excluded %in% "not_in_data") return(NA_character_)
    paste(study, xw$wave[i], paste(.qes_hz_row_vars(xw, i), collapse = "+"), sep = ":")
  }, character(1))

  list(
    study = study, n = n, s = s, e_date = e_date, wv = wv, members = members,
    wave_e_dates = .qes_hz_wave_election_dates(wv, cat_$elections),
    ids = ids, d_sub = .qes_hz_subsample(wv, d), d_strata = .qes_hz_stratum(wv, d, members, study),
    dates = dates, modes = modes, weights = weights, eligible = eligible,
    cell_wave = vapply(ctx$targets, function(t) if (is.na(pick[[t]]$row)) NA_character_ else xw$wave[pick[[t]]$row],
                       character(1)),
    cell_item = cell_item,
    values = values, reasons = reasons, srcs = srcs,
    cells = do.call(rbind, unname(cells)),
    provenance = prov,
    unmapped = unmapped,
    fingerprint = if (verified) NA_character_ else .qes_hz_fingerprint(d, .qes_hz_spec_vars(sub, study)),
    id_vars = ids$id_vars
  )
}

# The subsample of each row: the value of the waves' subsample variable, as
# text; NA when the study has none.
.qes_hz_subsample <- function(wv, d) {
  sub_var <- unique(stats::na.omit(wv$subsample_var))
  if (length(sub_var) > 0L && sub_var[1] %in% names(d)) .canon(d[[sub_var[1]]]) else rep(NA_character_, nrow(d))
}

# The sampling stratum of each row within its study: for pooled polls, the
# name of the poll's wave (as in `waves`); otherwise the value of the waves'
# strata variable, as text (the firm of the 1998 panel, firme_post: 1 =
# CREATEC, 2 = CROP); NA when the study has none.
.qes_hz_stratum <- function(wv, d, members, study) {
  var <- unique(stats::na.omit(wv$strata_var))
  if (length(var) == 0L || !var[1] %in% names(d)) {
    return(rep(NA_character_, nrow(d)))
  }
  if (.qes_poll_study(wv, study) && ncol(members) > 0L) {
    return(wv$wave[.qes_hz_first_wave(members)])
  }
  .canon(d[[var[1]]])
}

# Members of the wave (or, for "*", of any wave) that a crosswalk row names:
# a logical vector over the rows.
.qes_hz_row_member <- function(members, wv, wave) {
  idx <- .qes_wave_rows(wv, wave)
  if (length(idx) == 0L) {
    return(rep(FALSE, nrow(members)))
  }
  rowSums(members[, idx, drop = FALSE]) > 0L
}

# The date of the election each wave refers to (waves.csv election_ref),
# NA when it has none: the leading election_date of a study that has no
# single election in the catalog (pooled polls).
.qes_hz_wave_election_dates <- function(wv, elections) {
  elections$election_date[match(wv$election_ref, elections$election_id)]
}

# The leading columns of one study. Respondent layout: one row per row of
# the file; the interview date and mode are those of the first wave the
# respondent belongs to. Long layout: one row per respondent and wave the
# respondent belongs to (a respondent in no wave has no row); `rows` gives,
# for each output row, the row of the file and the wave
# (index into the waves, NA for none).
.qes_hz_lead <- function(part, layout, lang, rows) {
  s <- part$s
  n_out <- length(rows$row)
  wv <- part$wv
  w <- rows$wave
  pop_col <- paste0("target_population_", lang)
  pop <- if (nrow(wv) > 0L) wv[[pop_col]][w] else rep(NA_character_, n_out)
  pop[is.na(pop)] <- s[[pop_col]]
  pick_wave <- function(lst, empty) {
    out <- empty
    for (k in seq_along(lst)) {
      at <- which(w %in% k)
      out[at] <- lst[[k]][rows$row[at]]
    }
    out
  }
  interview <- pick_wave(part$dates, as.Date(rep(NA_character_, n_out)))
  mode <- pick_wave(part$modes, rep(NA_character_, n_out))
  # a study with one election in the catalog refers to it throughout; pooled
  # polls refer, poll by poll, to the election of their wave
  e_date <- rep(part$e_date, n_out)
  if (is.na(part$e_date) && length(part$wave_e_dates) > 0L) {
    e_date <- part$wave_e_dates[w]
  }
  # pooled polls span several years: each row takes the year its poll began
  year <- rep(as.integer(s$year), n_out)
  if (nrow(wv) > 0L && .qes_poll_study(wv, part$study)) {
    poll_year <- as.integer(substr(as.character(wv$fieldwork_start[w]), 1L, 4L))
    year[!is.na(poll_year)] <- poll_year[!is.na(poll_year)]
  }
  lead <- data.frame(
    study = rep(part$study, n_out),
    year = year,
    election_date = e_date,
    family = rep(s$family, n_out),
    study_design = rep(s$study_design, n_out),
    target_population = pop,
    stringsAsFactors = FALSE
  )
  if (identical(layout, "respondent")) {
    m <- part$members
    lead$waves <- if (ncol(m) > 0L) {
      apply(m, 1L, function(x) if (any(x)) paste(wv$wave[x], collapse = ";") else NA_character_)
    } else {
      rep(NA_character_, n_out)
    }
  } else {
    lead$wave <- wv$wave[w]
    lead$wave_timing <- wv$wave_timing[w]
    lead$wave_design <- wv$wave_design[w]
  }
  lead$qes_id <- if (n_out > 0L) paste0(part$study, ":", part$ids$key[rows$row]) else character(0)
  lead$subsample <- part$d_sub[rows$row]
  lead$stratum <- part$d_strata[rows$row]
  lead$source_row <- rows$row
  lead$survey_mode <- mode
  lead$interview_date <- interview
  lead$days_to_election <- as.integer(e_date - interview)
  lead$eligible_voter <- part$eligible[rows$row]
  lead
}

# The output rows of one study: row of the file and wave index.
.qes_hz_rows <- function(part, layout) {
  n <- part$n
  if (identical(layout, "respondent")) {
    return(list(row = seq_len(n), wave = .qes_hz_first_wave(part$members)))
  }
  m <- part$members
  if (ncol(m) == 0L) {
    return(list(row = seq_len(n), wave = rep(NA_integer_, n)))
  }
  # a respondent in no wave (an incomplete interview the waves' rules do
  # not count) has no row: .qes_hz_no_wave() counts them
  hit <- which(m, arr.ind = TRUE)
  row <- hit[, 1]
  wave <- hit[, 2]
  o <- order(row, wave)
  list(row = unname(row[o]), wave = unname(wave[o]))
}

# The respondents of each study that the long layout leaves out because they
# belong to no wave: data.frame(study, n_rows).
.qes_hz_no_wave <- function(parts) {
  n <- vapply(parts, function(part) {
    m <- part$members
    if (is.null(m) || ncol(m) == 0L || nrow(m) == 0L) 0L else sum(rowSums(m) == 0L)
  }, integer(1))
  keep <- n > 0L
  data.frame(study = as.character(vapply(parts, `[[`, character(1), "study"))[keep], n_rows = unname(n[keep]),
             stringsAsFactors = FALSE)
}

# The weight columns of one study: weight_pre, weight_post and their
# variables (respondent layout), or weight and weight_var (long layout).
.qes_hz_weight_cols <- function(part, layout, rows) {
  n_out <- length(rows$row)
  wv <- part$wv
  ws <- part$weights
  if (identical(layout, "long")) {
    value <- rep(NA_real_, n_out)
    var <- rep(NA_character_, n_out)
    for (k in seq_along(ws)) {
      at <- which(rows$wave %in% k)
      if (!ws[[k]]$used) next
      value[at] <- ws[[k]]$value[rows$row[at]]
      var[at] <- ws[[k]]$var
    }
    return(list(weight = value, weight_var = var))
  }
  out <- list()
  for (col in c("weight_pre", "weight_post")) {
    ok <- vapply(wv$wave_timing, function(tm) col %in% .qes_hz_weight_columns(tm), logical(1))
    first <- .qes_hz_first_wave(part$members, ok)
    value <- rep(NA_real_, n_out)
    var <- rep(NA_character_, n_out)
    for (k in which(ok)) {
      at <- which(first %in% k)
      if (!ws[[k]]$used) next
      value[at] <- ws[[k]]$value[rows$row[at]]
      var[at] <- ws[[k]]$var
    }
    out[[col]] <- value
    out[[paste0(col, "_var")]] <- var
  }
  out[c("weight_pre", "weight_post", "weight_pre_var", "weight_post_var")]
}

# The reasons a harmonized value can be missing, in vocabulary order (the
# levels of the __na companions and the n_* columns of cell provenance).
.qes_hz_reason_levels <- function() {
  r <- .qes_enum("missing_type")
  r$value[vapply(r$scope, function(s) any(c("spec", "engine") %in% .qes_split_list(s)), logical(1))]
}

# One row of cell provenance (design.md section 5.9) for target `t` of a
# study: the row applied (or why none was), its grade and instrument, the
# wave's recommended weight (variable, registry status, raw mean over the
# wave's members), the levels its question did not offer, the count of
# values and of each NA reason over the study's rows, and, for targets about
# an election, the number of the wave's members who could not vote in it.
.qes_hz_cell_row <- function(spec, study, t, xw, p, res, wv, weights, members, eligible) {
  i <- p$row
  has_row <- !is.na(i)
  get <- function(col) if (has_row) xw[[col]][i] else NA_character_
  tg <- spec$tables$targets
  j <- match(t, tg$target)
  not_offered <- NA_character_
  if (has_row && !is.na(tg$levels_id[j])) {
    offered <- .qes_split_list(xw$levels_offered[i])
    if (length(offered) > 0L) {
      set <- .qes_spec_levels(spec$tables$levels, tg$levels_id[j])
      not_offered <- paste(setdiff(set$name, offered), collapse = ";")
    }
  }
  w <- if (has_row) .qes_wave_rows(wv, xw$wave[i]) else integer(0)
  wgt <- .qes_hz_weights_of(weights, w)
  outside <- NA_integer_
  # NA when eligibility is unknown for every member of the wave (e.g. its
  # age rows are not signed off), not 0
  mem <- if (length(w) > 0L) rowSums(members[, w, drop = FALSE]) > 0L else logical(0)
  if (length(w) > 0L && !tg$election_ref_rule[j] %in% c("none", NA) && !all(is.na(eligible[mem]))) {
    outside <- sum(mem & eligible %in% FALSE)
  }
  levels <- .qes_hz_reason_levels()
  counts <- as.list(as.integer(table(factor(res$reason, levels = levels))))
  names(counts) <- paste0("n_", levels)
  out <- data.frame(
    study = study, wave = get("wave"), target = t, source_var = get("source_var"),
    rule = get("rule"), map_id = get("map_id"), grade = get("grade"),
    status = get("status"), instrument = get("instrument"),
    weight_var = wgt$var, weight_status = wgt$status, weight_mean_raw = wgt$mean_raw,
    levels_not_offered = not_offered,
    included = is.na(p$reason),
    excluded = p$excluded,
    n_valid = sum(!is.na(res$value)),
    stringsAsFactors = FALSE
  )
  out <- cbind(out, as.data.frame(counts, stringsAsFactors = FALSE))
  out$n_outside_universe <- outside
  out$note <- p$note
  out
}

# ---- output ------------------------------------------------------------------------

# Encode the values (level names or number text) of target `t`.
.qes_hz_encode <- function(value, t, spec, values, lang) {
  tg <- spec$tables$targets
  j <- match(t, tg$target)
  .qes_hz_encode_def(value, tg$type[j], tg$levels_id[j], tg[[paste0("label_", lang)]][j], spec, values, lang)
}

# Encode values of a column of type `type` (a target type) and level set
# `levels_id`, with variable label `label` (targets and pooled variables).
.qes_hz_encode_def <- function(value, type, levels_id, label, spec, values, lang) {
  out <- if (type %in% c("categorical", "ordinal")) {
    set <- .qes_spec_levels(spec$tables$levels, levels_id)
    labs <- set[[paste0("label_", lang)]]
    labs[is.na(labs) | duplicated(labs)] <- set$name[is.na(labs) | duplicated(labs)]
    k <- match(value, set$name)
    switch(
      values,
      factor = factor(labs[k], levels = labs, ordered = identical(type, "ordinal")),
      labelled = haven::labelled(as.integer(set$code[k]), labels = stats::setNames(as.integer(set$code), labs)),
      code = value
    )
  } else if (type %in% c("numeric", "weight")) {
    as.numeric(value)
  } else if (identical(type, "date")) {
    as.Date(value)
  } else {
    value
  }
  attr(out, "label") <- label
  out
}

# The value, reason and source of target `t` of one study on its output
# rows. In the long layout a value sits on the row of the wave that asked
# the question; the respondent's other waves have reason not_in_wave, except
# for time-invariant (static) targets such as the year of birth, which are
# repeated on every wave of the respondent.
.qes_hz_target_rows <- function(part, t, rows, layout, static) {
  v <- part$values[[t]][rows$row]
  r <- part$reasons[[t]][rows$row]
  s <- part$srcs[[t]][rows$row]
  if (identical(layout, "long") && !static) {
    cw <- part$cell_wave[[t]]
    if (!is.na(cw)) {
      # a row of wave "*" asked the question in each poll wave
      other <- !(rows$wave %in% .qes_wave_rows(part$wv, cw))
      v[other] <- NA_character_
      r[other] <- "not_in_wave"
      s[other] <- NA_character_
    }
  }
  list(value = v, reason = r, src = s)
}

# Combine the studies into the result data frame.
.qes_hz_assemble <- function(parts, ctx) {
  rows <- lapply(parts, .qes_hz_rows, layout = ctx$layout)
  lead <- do.call(rbind, Map(.qes_hz_lead, parts, rows, MoreArgs = list(layout = ctx$layout, lang = ctx$lang)))
  rownames(lead) <- NULL
  cols <- as.list(lead)
  tg <- ctx$spec$tables$targets
  static <- tg$target[tg$target_timing %in% "static"]
  per_target <- lapply(ctx$targets, function(t) {
    x <- Map(.qes_hz_target_rows, parts, rows,
             MoreArgs = list(t = t, layout = ctx$layout, static = t %in% static))
    list(value = unlist(lapply(x, `[[`, "value"), use.names = FALSE),
         reason = unlist(lapply(x, `[[`, "reason"), use.names = FALSE),
         src = unlist(lapply(x, `[[`, "src"), use.names = FALSE))
  })
  names(per_target) <- ctx$targets
  out_targets <- if (is.null(ctx$out_targets)) ctx$targets else ctx$out_targets
  for (t in out_targets) {
    cols[[t]] <- .qes_hz_encode(per_target[[t]]$value, t, ctx$spec, ctx$values, ctx$lang)
  }
  # pooled variables (R/hz-pool.R): from their members' cells, study by
  # study, on the same output rows
  pools <- ctx$pools %||% list()
  per_pool <- list()
  pool_prov <- list()
  for (p in names(pools)) {
    res <- Map(function(part, r, off) {
      member_rows <- function(t) {
        k <- off + seq_along(r$row)
        list(value = per_target[[t]]$value[k], reason = per_target[[t]]$reason[k], src = per_target[[t]]$src[k])
      }
      .qes_pool_rows(part, p, pools[[p]], r, member_rows, ctx)
    }, parts, rows, cumsum(c(0L, lengths(lapply(rows, `[[`, "row"))))[seq_along(parts)])
    per_pool[[p]] <- list(
      value = unlist(lapply(res, `[[`, "value"), use.names = FALSE),
      reason = unlist(lapply(res, `[[`, "reason"), use.names = FALSE),
      type = unlist(lapply(res, `[[`, "type"), use.names = FALSE),
      grade = unlist(lapply(res, `[[`, "grade"), use.names = FALSE),
      item = unlist(lapply(res, `[[`, "item"), use.names = FALSE),
      src = unlist(lapply(res, `[[`, "src"), use.names = FALSE)
    )
    for (k in seq_along(parts)) {
      if (is.null(parts[[k]]$cells)) next
      pr <- .qes_pool_provenance(parts[[k]]$study, p, res[[k]], parts[[k]], pools[[p]], ctx)
      if (!is.null(pr)) pr$.order <- k * 1000L + match(p, names(pools))
      pool_prov[[length(pool_prov) + 1L]] <- pr
    }
  }
  for (p in names(pools)) {
    cols[[p]] <- .qes_pool_encode(per_pool[[p]]$value, p, ctx$spec, ctx$values, ctx$lang)
  }
  for (p in names(pools)) {
    m <- .qes_pool_members(ctx$spec, p)
    cols[[paste0(p, "__type")]] <- factor(per_pool[[p]]$type, levels = m$type_name)
    cols[[paste0(p, "__grade")]] <- factor(per_pool[[p]]$grade, levels = .qes_hz_grades, ordered = TRUE)
    cols[[paste0(p, "__item")]] <- per_pool[[p]]$item
  }
  if (identical(ctx$missing, "reasons")) {
    levels <- .qes_hz_reason_levels()
    for (t in out_targets) {
      cols[[paste0(t, "__na")]] <- factor(per_target[[t]]$reason, levels = levels)
    }
    for (p in names(pools)) {
      cols[[paste0(p, "__na")]] <- factor(per_pool[[p]]$reason, levels = levels)
    }
  }
  if (isTRUE(ctx$keep_source)) {
    for (t in out_targets) {
      cols[[paste0(t, "__src")]] <- per_target[[t]]$src
    }
    for (p in names(pools)) {
      cols[[paste0(p, "__src")]] <- per_pool[[p]]$src
    }
  }
  wcols <- Map(.qes_hz_weight_cols, parts, rows, MoreArgs = list(layout = ctx$layout))
  for (nm in names(wcols[[1]])) {
    cols[[nm]] <- unlist(lapply(wcols, `[[`, nm), use.names = FALSE)
  }
  # study by study, then pooled variable by pooled variable (as rbind() of
  # per-study results gives them)
  pool_prov <- if (length(pool_prov) > 0L) do.call(rbind, pool_prov) else NULL
  if (!is.null(pool_prov)) {
    pool_prov <- pool_prov[order(pool_prov$.order), setdiff(names(pool_prov), ".order"), drop = FALSE]
    rownames(pool_prov) <- NULL
  }
  structure(cols, class = c("qes_harmonized", "data.frame"), row.names = c(NA_integer_, -nrow(lead)),
            qes_pooled_provenance = pool_prov)
}

# An empty result (every study failed under on_fail = "skip").
.qes_hz_empty <- function(ctx) {
  targets <- ctx$targets
  empty_chr <- stats::setNames(rep(list(character(0)), length(targets)), targets)
  part <- list(
    study = character(0), n = 0L,
    s = data.frame(year = NA_integer_, family = NA_character_, study_design = NA_character_,
                   target_population_en = NA_character_, target_population_fr = NA_character_,
                   stringsAsFactors = FALSE),
    e_date = as.Date(NA_character_),
    wv = data.frame(wave = character(0), wave_timing = character(0), wave_design = character(0),
                    target_population_en = character(0), target_population_fr = character(0),
                    stringsAsFactors = FALSE),
    members = matrix(FALSE, 0L, 0L), ids = list(key = character(0)), d_sub = character(0),
    d_strata = character(0), wave_e_dates = as.Date(character(0)),
    dates = list(), modes = list(), weights = list(), eligible = logical(0),
    cell_wave = stats::setNames(rep(NA_character_, length(targets)), targets),
    cell_item = stats::setNames(rep(NA_character_, length(targets)), targets),
    values = empty_chr, reasons = empty_chr, srcs = empty_chr, cells = NULL
  )
  .qes_hz_assemble(list(part), ctx)
}

# The call's arguments as text, for spec-level provenance.
.qes_hz_args_text <- function(args, fingerprints) {
  txt <- vapply(names(args), function(nm) {
    v <- args[[nm]]
    if (is.null(v)) return(paste0(nm, "=NULL"))
    if (inherits(v, "qes_spec")) return(paste0(nm, "=<qes_spec ", v$hash, ">"))
    paste0(nm, "=", paste(as.character(v), collapse = ","))
  }, character(1))
  if (length(fingerprints) > 0L) {
    txt <- c(txt, paste0("data=", paste(names(fingerprints), fingerprints, sep = ":", collapse = ",")))
  }
  paste(txt, collapse = "; ")
}

# ---- qes_harmonize() -------------------------------------------------------------

#' Harmonize variables across studies (experimental)
#'
#' `qes_harmonize()` builds one data frame from several studies, with one
#' column per harmonized variable ("target") and one row per respondent. It
#' does only what the reviewed harmonization spec says: for each study and
#' target, one question of that study, its codes mapped one by one to the
#' target's levels, and a reason for every missing value. Nothing is matched
#' by name or guessed, and a code the spec does not map is an error, never
#' passed through. [qes_spec()] shows the spec: which studies have which
#' target, how comparable each study's question is, and the exact mapping.
#'
#' @section Experimental:
#' The engine and its spec are experimental: targets, grades and mappings are
#' reviewed study by study and may still change. A crosswalk row is applied
#' only once a reviewer has signed it off (status `"stable"`); rows checked
#' against the files but not yet signed off (status `"review"` or `"draft"`)
#' are applied only with `include_draft = TRUE`, and a message says so. In
#' the shipped spec every row but three is signed off, after an automated
#' double review against the original files and documents (not a human
#' review; the crosswalk's `reviewed_by` says so, and `review_note` what the
#' review corrected). The three others (the strength of provincial party
#' identification of `qes2022` and the previous provincial vote of
#' `qes2008` and `qes2018`) are in review, and applied only with
#' `include_draft = TRUE`. A row's sign-off is about its content: the recommended
#' weights that still need review (those of `qes1998`, `qes2007_panel`,
#' `qes2012_panel` and the CROP polls) do not hold the rows of their waves,
#' but are themselves `NA` until they are reviewed (see *Weights*). A row
#' in review has its values missing by default (reason `not_reviewed`), and
#' `qes_spec("crosswalk")$review_note` says why it is held. When no cell of
#' the result is applied for that
#' reason, a warning of class `qesR_warning_all_unreviewed` gives the number
#' of values left `NA` and names `include_draft = TRUE`; otherwise a message
#' counts the cells (study and target) and values left out.
#'
#' Every result records the spec version and content hash
#' (`attr(, "qes_spec")`). The same spec version and hash, the same pinned
#' files and the same qesR version give the same data values and the same
#' study and cell provenance; only the `created` time of the spec-level
#' provenance (`qes_provenance(x, level = "spec")`) differs between calls. To keep a spec
#' fixed, copy `inst/extdata/harmonize/` of a qesR release and pass its path
#' as `spec`.
#'
#' @section Targets and comparability:
#' A target is one question stimulus: a different wording, scale, timing or
#' format makes another target (vote intention and reported vote, the
#' "independent country" and "sovereign country" referendum questions, the
#' four-point and 0-10 interest scales are all separate targets). A pooled
#' variable (see *Pooled variables*) combines such targets into one column,
#' and records row by row which one each value comes from. Each study's
#' question gets a grade against the target's anchor question:
#' * `identical`: the same question, options and format;
#' * `comparable`: the same stimulus, with differences (option order, whether
#'   "don't know" is offered, minor parties listed) not expected to move the
#'   shares of the common answers;
#' * `approximate`: the same construct, in a format or with a filter expected
#'   to move them.
#'
#' `min_grade` keeps only cells at or above a grade; the others become `NA`
#' with reason `below_grade`. By default approximate cells are included, and
#' a message lists them.
#'
#' Answer options that a study's question did not offer (the Conservative
#' party before 2022, say) are *structural zeros*: their share in that study
#' is 0 because nobody could choose them, not because nobody supported them.
#' The factor levels are the same in every study, so these levels are there
#' with no respondent; `qes_provenance(x, level = "cell")$levels_not_offered`
#' lists them, and printing the result names them.
#'
#' @eval .rd_na_reasons()
#'
#' @section Pooled variables:
#' A pooled variable is one column, for every study, that pools several
#' targets (its members) with a precedence among them. `vote_choice` is the
#' provincial vote choice: the reported vote (recall) where the study asked
#' it, else the vote intention with those who named no party pushed (the
#' undecided and, in some studies, those who would not vote or refused),
#' else the vote intention at the first question, whatever the wording of
#' each study.
#' `sov_support` pools the referendum wordings on sovereignty (an
#' independent country, a sovereign country, the 1995 question, and,
#' collapsed to yes or no, being favourable to independence),
#' `pol_interest` the interest scales on 0 to 1 (four-point items scored 1,
#' 0.7, 0.3 and 0; 0-10 items divided by 10) and `turnout` the reported
#' turnout (the likelihood of voting only when asked for). The first three
#' are in the default set `"core"`; `qes_spec("pooled")` lists their members.
#'
#' A row's value comes from the first member, by precedence, that asked the
#' respondent: a member that has a value, or a missing value that is an
#' answer (don't know, refused, did not vote), sets the row; a member that
#' did not ask the respondent, or whose answer cannot be used (not in the
#' wave, not asked in the study, not reviewed, below `min_grade`, routed
#' out, system missing, a code that straddles levels: `not_mappable`, such
#' as CROP's code 7 of the pushed intention), passes to the next. So a
#' respondent who did not vote is `NA` (reason `not_voted`) in
#' `vote_choice`, never given their earlier intention. When every member
#' passes, the row is `NA` with the reason, `__type`, `__grade` and
#' `__item` of the first usable member that has a row in the study and
#' wave, else of the first member that has a row: with
#' `min_grade = "comparable"`, a study whose only members are approximate
#' gets `NA` (reason `below_grade`) with that member's type and grade
#' (`approximate`), which say why the row is empty. With each pooled
#' column come `<pooled>__type` (the member's type name, such as `recall`
#' or `intention_push`), `<pooled>__grade` (the member row's grade, capped
#' at approximate for a transform that loses information, such as scoring a
#' four-point scale; never raised) and `<pooled>__item` (the source:
#' `<study>:<wave>:<variables>`), and `<pooled>__na` and `<pooled>__src`
#' with `missing = "reasons"` and `keep_source = TRUE`. `types` keeps some
#' members only, for example `types = list(vote_choice = "recall")`; the
#' precedence stays the spec's. The members are harmonized too, but are
#' columns of the result only when `targets` names them.
#'
#' In the respondent layout (one row per respondent), a study's values of a
#' pooled variable come from one wave: that of the first member the study
#' applies (the post-election recall of a pre/post study, say), so that one
#' weight column fits them; the respondents of its other waves are `NA`
#' (reason `not_in_wave`). The long layout keeps every wave, each row with
#' the members of its own wave. [qes_design()] then weights each study with
#' the column of that wave. A message (class `qesR_message_pooled`) and
#' `qes_provenance(x, level = "pooled")` say which member each study used,
#' and how many rows it gave.
#'
#' A target a study has no row for can be derived from other targets
#' (targets.csv `derive_rule`): the age groups `age_group3` and
#' `age_group6` from the age, else the year of birth (graded approximate:
#' an age at a band edge can be one year off), so that every study has an
#' age group. A direct row always wins, and cell provenance says
#' `rule = "derive:age_band"`.
#'
#' @section Data:
#' By default each study is read from its pinned original file, checked by
#' md5, as [get_qes()] reads it. `data` gives the frames instead, for
#' example ones you already read: `data = list(qes2018 = get_qes("qes2018"))`.
#' Their origin cannot be verified, so a warning says so and
#' `qes_provenance(x)$md5_verified` is `FALSE`. Give them as [get_qes()]
#' returns them (labelled codes, not factors). The synthetic study
#' `"qes_demo"` is harmonized with the rows of `qes2014`, whose variables it
#' copies; targets it has no question for are `not_asked`.
#'
#' @section Waves:
#' Each study has one or more waves (a post-election survey, the waves of a
#' panel, the campaign-period and post-election surveys of 2022), declared
#' in the spec with the rule that says who took part in each: a
#' disposition or date variable, never an answer. A question belongs to the
#' wave that asked it, so a respondent outside that wave is `NA` with reason
#' `not_in_wave`. The respondent layout has one row per respondent, and
#' `waves` lists the waves each respondent took part in. The long layout
#' (`layout = "long"`) has one row per respondent and wave, so `wave` is
#' never `NA`: a respondent who took part in no wave (in `qes2007_panel`, one
#' interview that neither wave's rule counts) has no row, a message (class
#' `qesR_message_no_wave`) says so, and `attr(x, "qes_no_wave")` counts them
#' study by study. A value sits on the row of the
#' wave that asked it and the respondent's other rows are `not_in_wave`,
#' except the time-invariant targets (year and month of birth), which are
#' repeated on each of the respondent's rows.
#'
#' Pooled polls (the monthly CROP polls of 2007-2010) have one wave per
#' poll: each respondent belongs to one poll, whose questions the spec
#' maps once for all the polls. In the same way, a question that does not
#' change over a panel (gender, education, mother tongue) is mapped once
#' for all its waves when each respondent answered it in whichever wave
#' they took part (the 2007 panel). Each poll refers to the next general
#' election, which `election_date` gives row by row, `year` is the year the
#' poll began, and its weight is normalized within the poll. `stratum`
#' gives the independent sample a respondent was drawn in, where a study
#' pools several: for pooled polls the poll's wave name (as in `waves`);
#' for the 1998 panel the polling firm's code in the file, `firme_post`
#' (`"1"` = CREATEC, `"2"` = CROP; each firm interviewed francophones
#' only). [qes_design()] uses it.
#'
#' `interview_date` is the wave's interview date where the file has one
#' (`NA` otherwise; the fieldwork dates of each wave are in
#' `qes_spec("spec")$tables$waves`), and `days_to_election` the number of
#' days from it to the election (negative after the election).
#' `survey_mode` is `"web"`, `"phone"` or `"mixed"`: the wave's mode, or,
#' where it varies by respondent (the 2018 panel's first wave), the
#' respondent's own.
#'
#' @section Weights:
#' Each wave has at most one recommended weight in the spec's registry, and
#' never one calibrated on the vote. The respondent layout has `weight_pre`
#' and `weight_post`, the recommended weights of the respondent's
#' pre-election and post-election waves (`NA` outside them), with the name
#' of the source variable in `weight_pre_var` and `weight_post_var`; the
#' long layout has `weight` and `weight_var`. `weights = "normalized"`
#' (default) divides each wave's weight by its mean over the wave's members
#' with a weight, so it has mean 1 in each study and wave; this fixes the
#' scale only (it does not give studies equal shares when pooled: see
#' [qes_design()]). `weights = "raw"` keeps the weights as deposited. A
#' weight that is registered but not accepted yet (registry status
#' `needs_review`) is `NA`, with a message, until it is reviewed; the raw
#' variables are still in the data read by [get_qes()], to join by
#' `source_row`.
#'
#' A pre-election question (vote intention) is weighted with `weight_pre`
#' and a post-election one (reported vote) with `weight_post`.
#' `attr(, "qes_weight_guide")` gives, for each target and study, the wave
#' that asked it and the weight column and variable that fit it, and a
#' message says when the targets of a study need different weights. For a
#' question that covers every wave of a panel (wave `"*"`, such as gender
#' in the 2007 panel), the guide names a weight only when those waves share
#' it; otherwise `weight_column` and `weight_var` are `NA`, since each
#' respondent's weight depends on the waves they took part in.
#'
#' @section Eligibility:
#' `eligible_voter` says whether the respondent could vote in the study's
#' election: 18 or older on election day and, where the study asked,
#' a Canadian citizen. It comes from the age targets of the spec (year and
#' month of birth, age, age group) and the citizenship target, read
#' whatever `targets` and `min_grade` ask for (but, like any target, only
#' from rows signed off by a reviewer, or with `include_draft = TRUE`).
#' It is `TRUE` or `FALSE` only when the answers settle it: someone born
#' 18 years before the election year whose month of birth is unknown, or
#' aged 17 when interviewed before the election, is `NA`, as is a
#' respondent whose study has no age question in the spec yet. The 2018
#' study sampled people aged 16 and over, so some of its respondents are
#' `FALSE`. Its weights target the population aged 16 and over; keeping
#' only eligible voters does not make them an 18-and-over calibration.
#'
#' @section Conditions:
#' Besides those of every qesR function (see [qesR-package]),
#' `qes_harmonize()` raises these classed conditions:
#' * errors: `qesR_error_unmapped` (a source code the spec does not map,
#'   with `unmapped = "error"`; fields `study`, `target`, `codes`, `n`,
#'   `unmapped`), `qesR_error_spec` (an invalid spec, or `data` that fail the
#'   spec's checks; field `problems`, a data frame of the failed rules) and
#'   `qesR_error_duplicate_id` (identifier variables that do not identify the
#'   rows of a study uniquely; fields `study`, `id_vars`, `rows`);
#' * warnings: `qesR_warning_all_unreviewed` (no cell applied, see
#'   `include_draft`), `qesR_warning_unmapped` (codes set to `NA` with
#'   `unmapped = "warn"`; field `unmapped`), `qesR_warning_partial` (studies
#'   left out with `on_fail = "skip"`; field `failed`) and
#'   `qesR_warning_unverified_source` (`data` not read by qesR from the
#'   pinned files; field `study`), and `qesR_warning_label_mismatch` and
#'   `qesR_warning_universe` (such `data` whose value labels differ from
#'   the pinned file's, or whose answers do not fit the question's
#'   universe; fields `study`, `problems`);
#' * messages, silenced by `quiet = TRUE`: `qesR_message_pooled` (which
#'   member of each pooled variable each study used; field `pooled`, the
#'   pooled provenance), `qesR_message_unreviewed_skipped`
#'   (cells left `NA` because their rows are not signed off),
#'   `qesR_message_unreviewed_cells` (cells that use rows not signed off,
#'   with `include_draft = TRUE`), `qesR_message_approximate_cells` (cells
#'   graded approximate are included), `qesR_message_structural_zeros`
#'   (levels a question did not offer), `qesR_message_weight_review`
#'   (recommended weights that need review, left `NA`),
#'   `qesR_message_weight_timing` (targets of one study that need different
#'   weights; not sent when all of the study's weights need review) and
#'   `qesR_message_no_wave` (respondents the long layout leaves out because
#'   they took part in no wave; field `no_wave`).
#'
#' [qes_design()] sends `qesR_message_design_dropped` (fields `study`, `n`)
#' when it leaves out rows without the chosen weight.
#'
#' @section En français:
#' `qes_harmonize()` (expérimental) construit un seul tableau à partir de
#' plusieurs études, une colonne par variable harmonisée (« cible ») et une
#' ligne par répondant, en appliquant uniquement la spécification révisée :
#' une question par étude et par cible, ses codes appariés un à un aux
#' niveaux de la cible, et un motif pour chaque valeur manquante. Un code non
#' apparié est une erreur. Chaque cellule (étude, cible) porte un niveau de
#' comparabilité (`identical`, `comparable`, `approximate`) ; `min_grade`
#' écarte les cellules sous un niveau donné. Les options qu'une question
#' n'offrait pas sont des zéros structurels, pas un appui nul. `lang = "fr"`
#' donne les étiquettes des niveaux en français ; les codes sont les mêmes.
#' `targets = "decon"` demande les cibles des colonnes de [get_decon()].
#' La disposition longue (`layout = "long"`) donne une ligne par personne et
#' par vague (`wave` n'y vaut jamais NA : une personne qui n'a participé à
#' aucune vague n'a pas de ligne, un message le dit et
#' `attr(x, "qes_no_wave")` les compte) ; une question invariante d'un panel (genre, scolarité) est
#' lue dans la vague à laquelle la personne a participé (panel de 2007) ;
#' les sondages CROP regroupés ont une vague par sondage, et
#' `stratum` donne l'échantillon indépendant d'où vient la personne : pour
#' les sondages regroupés, le nom de la vague du sondage (comme dans
#' `waves`) ; pour le panel de 1998, le code de la firme dans le fichier,
#' `firme_post` (`"1"` = CREATEC, `"2"` = CROP). Chaque sondage se rapporte
#' à l'élection générale suivante, que `election_date` donne ligne par
#' ligne, `year` est l'année où il a commencé, et sa pondération est
#' normalisée à l'intérieur du sondage. Les pondérations recommandées de chaque vague sont dans
#' `weight_pre` et `weight_post` (ou `weight` en disposition longue),
#' ramenées à une moyenne de 1 par étude et par vague ; une pondération
#' encore à réviser (statut `needs_review`) vaut NA. `attr(, "qes_weight_guide")` donne, pour
#' chaque cible et étude, la colonne de pondération qui convient ; pour une
#' question qui couvre toutes les vagues d'un panel (vague `"*"`), elle vaut
#' NA quand ces vagues n'ont pas la même pondération. `eligible_voter` indique si la personne
#' pouvait voter (18 ans le jour du scrutin et, là où l'étude l'a demandé,
#' citoyenneté canadienne) ; `interview_date`, `days_to_election` et
#' `survey_mode` décrivent l'entrevue. [qes_design()] en fait un plan de
#' sondage. Des résultats portant sur des études différentes se combinent
#' avec [rbind()] s'ils ont été construits avec la même spécification, les
#' mêmes `lang`, `layout`, `values`, `missing` et `weights` et les mêmes
#' colonnes (sinon, une erreur de classe `qesR_error_input`). Une ligne de
#' correspondance n'est appliquée qu'une fois approuvée par un réviseur
#' (statut `"stable"`) ; `include_draft = TRUE` applique aussi les lignes
#' vérifiées mais pas encore approuvées (statut `"review"` ou `"draft"`).
#' Dans la spécification livrée, toutes les lignes sauf trois sont
#' approuvées, après une double révision automatisée sur les fichiers et
#' documents originaux (et non une révision humaine ; la colonne
#' `reviewed_by` le dit) ; les trois autres (la force de l'identification
#' partisane provinciale de `qes2022` et le vote provincial précédent de
#' `qes2008` et de `qes2018`) sont en révision.
#' L'approbation d'une ligne porte sur son contenu : les pondérations
#' recommandées encore à réviser (celles de `qes1998`, `qes2007_panel`,
#' `qes2012_panel` et des sondages CROP) ne retiennent pas les lignes de
#' leurs vagues, mais valent elles-mêmes NA jusqu'à leur révision. Sans
#' `include_draft = TRUE`, les valeurs d'une ligne en révision sont NA
#' (motif `not_reviewed`), `qes_spec("crosswalk")$review_note` dit
#' pourquoi elle est retenue, et un avertissement de classe
#' `qesR_warning_all_unreviewed` le signale quand tout le résultat est NA. Les autres conditions ont des classes (section
#' *Conditions*) : erreurs `qesR_error_unmapped`, `qesR_error_spec` et
#' `qesR_error_duplicate_id` (variables d'identification qui n'identifient
#' pas les lignes de façon unique) ; avertissements `qesR_warning_unmapped`,
#' `qesR_warning_partial` et `qesR_warning_unverified_source` ; messages
#' `qesR_message_*`, masqués par `quiet = TRUE` ; [qes_design()] envoie
#' `qesR_message_design_dropped` quand il écarte des lignes sans la
#' pondération choisie. Une variable regroupée (section *Pooled
#' variables*) réunit plusieurs cibles en une seule colonne pour toutes les
#' études : `vote_choice` est le vote déclaré là où l'étude l'a demandé,
#' sinon l'intention de vote avec relance des personnes qui n'ont nommé
#' aucun parti (les indécis et, dans certaines études, celles qui ne
#' voteraient pas ou refusaient), sinon l'intention de vote ; `sov_support` réunit les libellés référendaires, `pol_interest`
#' les échelles d'intérêt ramenées de 0 à 1 et `turnout` la participation
#' déclarée. Le premier membre, par ordre de priorité, qui a interrogé la
#' personne donne la valeur ; `<variable>__type` indique ce membre,
#' `<variable>__grade` son niveau de comparabilité et `<variable>__item` sa
#' question ; `types = list(vote_choice = "recall")` ne garde que certains
#' membres. Les groupes d'âge sont dérivés de l'âge ou de l'année de
#' naissance là où l'étude n'a pas de question par tranches. Voir
#' `vignette("fr-reference-harmonisation", package = "qesR")`.
#'
#' @param studies Study codes (see [qes_studies()]). `NULL` (default) means
#'   the Quebec Election Studies the spec covers, or the studies named in
#'   `data` when it is given; `"all"` means every study the spec covers.
#' @param targets Target, family, set or pooled variable names (see
#'   [qes_spec()]); the default `"core"` is the core set (with the pooled
#'   variables `vote_choice`, `sov_support` and `pol_interest`), `"decon"`
#'   the targets of the columns of [get_decon()], and `"pooled"` every
#'   pooled variable.
#' @param layout `"respondent"` (default): one row per respondent of each
#'   study's file. `"long"`: one row per respondent and wave (see *Waves*).
#' @param values How categorical targets are returned: `"factor"` (default;
#'   ordered for ordinal targets, with the target's levels in every study),
#'   `"labelled"` ([haven::labelled()] integer codes that are stable across
#'   versions) or `"code"` (the ASCII level names). Numeric targets are
#'   numbers.
#' @param missing `"na"` (default) or `"reasons"`, which adds a factor column
#'   `<target>__na` giving the reason of each missing value.
#' @param min_grade The lowest comparability grade kept: `"approximate"`
#'   (default, every graded cell), `"comparable"` or `"identical"`.
#' @param weights `"normalized"` (default): each wave's recommended weight
#'   divided by its mean over the wave's members, so it has mean 1 in each
#'   study and wave. `"raw"`: as deposited. See *Weights*.
#' @param unmapped What a source code the spec does not map does: `"error"`
#'   (default) raises an error of class `qesR_error_unmapped`; `"warn"` and
#'   `"na"` set it to `NA` with reason `unmapped`, with or without a warning.
#' @param on_fail `"stop"` (default): a study that cannot be read or checked
#'   stops the call. `"skip"` leaves it out with a warning; the result's
#'   attribute `failed_studies` gives the reason.
#' @param keep_source If `TRUE`, adds `<target>__src`, the source code of
#'   each value as text.
#' @param include_draft If `TRUE`, also applies crosswalk rows not yet
#'   signed off by a reviewer (status `"review"` or `"draft"`); a message
#'   counts the cells that use them. `FALSE` (default) leaves them `NA`
#'   (reason `not_reviewed`), with a warning when that leaves the whole
#'   result `NA` (see *Experimental*).
#' @param data `NULL` (read the pinned files) or a named list of data frames
#'   as [get_qes()] returns them, named by study code.
#' @param spec `NULL` (the spec shipped with qesR), the path of a spec
#'   directory, or a `qes_spec` object (see [qes_spec()]).
#' @param lang Language of returned labels (factor levels, variable labels
#'   and target populations): `"en"` (default) or `"fr"`. Codes and reasons
#'   do not depend on it.
#' @param quiet If `TRUE`, no progress or informational messages.
#' @param types `NULL` (default: the default members of each pooled
#'   variable) or a named list, pooled variable -> the type names of the
#'   members to use (a member's target name is accepted too), for example
#'   `list(vote_choice = "recall")` or `list(turnout = c("recall",
#'   "intention"))`. The order given does not matter: the members are tried
#'   in the spec's order of precedence. See *Pooled variables*.
#'
#' @return A data frame of class `qes_harmonized`, returned visibly, one row
#'   per respondent of each study's file (no row is dropped), or per
#'   respondent and wave in the long layout (where a respondent in no wave
#'   has no row). Leading columns: `study`,
#'   `year` (the study's year; for pooled polls, the year the respondent's
#'   poll began), `election_date`, `family`, `study_design`, `target_population`,
#'   `waves` (the waves the respondent belongs to, `;`-separated; in the
#'   long layout `wave`, `wave_timing` and `wave_design` instead), `qes_id`
#'   (`<study>:<identifier>`, unique in the respondent layout), `subsample`,
#'   `stratum` (the independent sample within the study, see *Waves*: the
#'   poll's wave name for pooled polls, `"1"` = CREATEC and `"2"` = CROP
#'   for `qes1998`; `NA` for a study drawn as one sample), `source_row` (the row in the study's file, for joining raw variables
#'   with [merge()] on `study` and `source_row`), `survey_mode`,
#'   `interview_date` (of the respondent's first wave in the respondent
#'   layout), `days_to_election` and `eligible_voter`. Then one column per
#'   target, carrying its label in `attr(, "label")`, then one per pooled
#'   variable, then the `__type`, `__grade` and `__item` companions of each
#'   pooled variable, then the `__na` and `__src` companions (targets, then
#'   pooled variables), then the weight columns: `weight_pre`,
#'   `weight_post`, `weight_pre_var` and `weight_post_var` (respondent
#'   layout) or `weight` and `weight_var` (long layout). Attributes:
#'   `qes_spec` (spec `version`, `hash`, `custom`, `engine`),
#'   `qes_provenance` (see [qes_provenance()], with levels `"study"`,
#'   `"cell"`, `"spec"` and, with pooled variables, `"pooled"`),
#'   `qes_weight_guide` (`target` or pooled variable, `study`, `wave`,
#'   `target_timing`, `weight_column`, `weight_var`, `weight_status`) and
#'   `failed_studies` (`study`, `class`, `message`, `parent_message`) and,
#'   in the long layout, `qes_no_wave` (`study`, `n_rows`: the respondents
#'   left out because they took part in no wave; no row when there are none).
#'
#'   Results for different studies built with the same spec can be combined
#'   with [rbind()], which also combines their provenance. It is an error
#'   (class `qesR_error_input`) if the spec content hashes differ, the parts
#'   were built with different `lang`, `layout`, `values`, `missing`,
#'   `weights` or `types`, their columns differ (different `targets` or `keep_source`),
#'   or a study appears twice; harmonizing all the studies in one call is
#'   simpler.
#'
#' @family harmonization
#' @seealso [qes_design()] to use the weights in a survey design,
#'   [qes_spec()] for the targets and the mapping of each study,
#'   `vignette("harmonization-reference", package = "qesR")` for the
#'   reference generated from the spec, [qes_provenance()] for where each
#'   value came from.
#' @examples
#' # the synthetic demonstration study, harmonized with the signed-off
#' # qes2014 rows (its gender row, still in review, is left NA)
#' h <- qes_harmonize("qes_demo")
#' h
#' table(h$vote_prov_recall, useNA = "ifany")
#'
#' # why values are missing, and the grade of each cell
#' h <- qes_harmonize("qes_demo", targets = c("vote", "sov_indep"),
#'                    missing = "reasons", quiet = TRUE)
#' table(h$vote_prov_recall__na)
#' cells <- qes_provenance(h, level = "cell")
#' cells[, c("study", "target", "source_var", "grade", "n_valid")]
#'
#' # French labels, same codes
#' h_fr <- qes_harmonize("qes_demo", targets = "interest_4pt", lang = "fr", quiet = TRUE)
#' levels(h_fr$interest_4pt)
#'
#' # weights and eligibility: every question of the demonstration study was
#' # asked after the election, so weight_post is its weight (mean 1)
#' h <- qes_harmonize("qes_demo", targets = "sov_indep", quiet = TRUE)
#' h[1:3, c("study", "waves", "eligible_voter", "sov_indep", "weight_post", "weight_post_var")]
#' attr(h, "qes_weight_guide")
#'
#' # one row per respondent and wave
#' l <- qes_harmonize("qes_demo", targets = "sov_indep", layout = "long", quiet = TRUE)
#' table(l$wave, l$wave_timing)
#'
#' # one vote choice for every study: here the reported vote of the
#' # demonstration study (a qes2014 stand-in)
#' v <- qes_harmonize("qes_demo", targets = "vote_choice", missing = "reasons", quiet = TRUE)
#' table(v$vote_choice, v$vote_choice__type, useNA = "ifany")
#' qes_provenance(v, level = "pooled")[, c("study", "type", "member", "grade", "n_value")]
#' # recall only, or intentions only
#' v2 <- qes_harmonize("qes_demo", targets = "vote_choice",
#'                     types = list(vote_choice = "intention"), quiet = TRUE)
#'
#' # which rows are signed off (stable), and which are still in review
#' xw <- qes_spec("crosswalk")
#' table(xw$status)
#' xw[xw$status == "review", c("study", "target", "source_var", "grade")]
#' @export
qes_harmonize <- function(studies = NULL, targets = "core", layout = c("respondent", "long"),
                          values = c("factor", "labelled", "code"), missing = c("na", "reasons"),
                          min_grade = c("approximate", "comparable", "identical"),
                          weights = c("normalized", "raw"), unmapped = c("error", "warn", "na"),
                          on_fail = c("stop", "skip"), keep_source = FALSE, include_draft = FALSE,
                          data = NULL, spec = NULL, lang = c("en", "fr"), quiet = FALSE, types = NULL) {
  layout <- .qes_check_one(layout, "layout", c("respondent", "long"))
  values <- .qes_check_one(values, "values", c("factor", "labelled", "code"))
  missing <- .qes_check_one(missing, "missing", c("na", "reasons"))
  min_grade <- .qes_check_one(min_grade, "min_grade", c("approximate", "comparable", "identical"))
  weights <- .qes_check_one(weights, "weights", c("normalized", "raw"))
  unmapped <- .qes_check_one(unmapped, "unmapped", c("error", "warn", "na"))
  on_fail <- .qes_check_one(on_fail, "on_fail", c("stop", "skip"))
  lang <- .qes_check_one(lang, "lang", c("en", "fr"))
  .qes_check_flag(keep_source, "keep_source")
  .qes_check_flag(include_draft, "include_draft")
  .qes_check_flag(quiet, "quiet")
  sp <- .qes_spec_get(spec, "error")
  if (!is.null(data)) {
    data <- .qes_spec_data_arg(data, demo = TRUE)
  }
  study_codes <- .qes_hz_resolve_studies(studies, data, sp)
  target_names <- .qes_hz_resolve_targets(targets, sp)
  # pooled variables: their members are harmonized too, and shown only when
  # named in `targets` (R/hz-pool.R)
  pool_names <- .qes_pool_resolve(targets, sp)
  pools <- .qes_pool_resolve_types(types, pool_names, sp)
  tg_all <- sp$tables$targets$target
  computed <- tg_all[tg_all %in% c(target_names, unlist(pools, use.names = FALSE))]
  ctx <- list(spec = sp, targets = computed, out_targets = target_names, pools = pools, layout = layout,
              values = values, missing = missing, min_grade = min_grade, weights = weights,
              unmapped = unmapped, include_draft = include_draft, keep_source = keep_source,
              lang = lang, quiet = quiet, legacy = isTRUE(.qes_hz_preread$legacy))
  if (!is.null(data)) {
    unverified <- names(data)
    .qes_warn(
      "unverified_source",
      class = "qesR_warning_unverified_source",
      args = list(.qes_q(unverified)),
      data = list(study = unverified)
    )
  }

  parts <- list()
  failed <- list()
  for (s in study_codes) {
    res <- if (identical(on_fail, "stop")) {
      .qes_hz_study(s, data[[s]], ctx)
    } else {
      tryCatch(.qes_hz_study(s, data[[s]], ctx), error = function(e) e)
    }
    if (inherits(res, "error")) {
      parent <- res$parent
      failed[[s]] <- data.frame(
        study = s, class = class(res)[1],
        message = .qes_condition_text(res, "en"),
        parent_message = if (is.null(parent)) NA_character_ else .qes_condition_text(parent, "en"),
        stringsAsFactors = FALSE
      )
      next
    }
    parts[[s]] <- res
  }
  failed <- if (length(failed) > 0L) do.call(rbind, unname(failed)) else
    data.frame(study = character(0), class = character(0), message = character(0),
               parent_message = character(0), stringsAsFactors = FALSE)
  rownames(failed) <- NULL

  out <- if (length(parts) > 0L) .qes_hz_assemble(parts, ctx) else .qes_hz_empty(ctx)
  pooled_prov <- attr(out, "qes_pooled_provenance", exact = TRUE)
  attr(out, "qes_pooled_provenance") <- NULL
  cell <- if (length(parts) > 0L) do.call(rbind, lapply(unname(parts), `[[`, "cells")) else NULL
  # cell provenance describes the target columns; the members that only a
  # pooled variable reads are in the pooled provenance
  if (!is.null(cell)) {
    cell <- cell[cell$target %in% target_names, , drop = FALSE]
    rownames(cell) <- NULL
  }
  study_prov <- if (length(parts) > 0L) do.call(rbind, lapply(unname(parts), `[[`, "provenance")) else NULL
  if (!is.null(study_prov)) rownames(study_prov) <- NULL
  fingerprints <- unlist(lapply(parts, `[[`, "fingerprint"))
  fingerprints <- fingerprints[!is.na(fingerprints)]
  types_text <- if (length(pools) == 0L) NULL else
    paste(vapply(names(pools), function(p) {
      m <- .qes_pool_members(sp, p)
      paste0(p, ":", paste(m$type_name[m$member %in% pools[[p]]], collapse = "+"))
    }, character(1)), collapse = ",")
  call_args <- list(studies = study_codes, targets = targets, layout = layout, values = values,
                    missing = missing, min_grade = min_grade, weights = weights,
                    unmapped = unmapped, on_fail = on_fail, keep_source = keep_source,
                    include_draft = include_draft, spec = if (is.null(spec)) NULL else sp, lang = lang,
                    types = types_text)
  meta <- .qes_description_sha()
  spec_prov <- data.frame(
    spec_version = sp$version, spec_hash = sp$hash, spec_custom = isTRUE(sp$custom),
    qesR_version = as.character(.qes_engine_version()), qesR_sha = meta,
    args = .qes_hz_args_text(call_args, fingerprints),
    created = as.POSIXct(format(Sys.time(), tz = "UTC"), tz = "UTC"),
    stringsAsFactors = FALSE
  )
  if (!is.null(study_prov)) {
    attr(study_prov, "cell") <- cell
    attr(study_prov, "spec") <- spec_prov
    if (length(pools) > 0L) attr(study_prov, "pooled") <- pooled_prov %||% .qes_pool_provenance_empty()
  }
  # the options that change how values are encoded: rbind() refuses to mix
  # them (the pooled variables' types, when there are any, among them)
  opts <- list(layout = layout, values = values, missing = missing, weights = weights, lang = lang)
  if (!is.null(types_text)) opts$types <- types_text
  attr(out, "qes_spec") <- list(version = sp$version, hash = sp$hash, custom = isTRUE(sp$custom),
                                engine = as.character(.qes_engine_version()),
                                options = opts)
  attr(out, "qes_provenance") <- study_prov
  guide <- .qes_hz_weight_guide(parts, ctx)
  if (length(pools) > 0L && length(parts) > 0L) {
    # study by study, the targets then the pooled variables (as rbind() of
    # per-study results gives them)
    guide <- rbind(guide, .qes_pool_weight_guide(parts, ctx))
    guide <- guide[order(match(guide$study, names(parts)), !guide$target %in% names(pools)), , drop = FALSE]
    rownames(guide) <- NULL
  }
  attr(out, "qes_weight_guide") <- guide
  attr(out, "failed_studies") <- failed
  no_wave <- NULL
  if (identical(layout, "long")) {
    no_wave <- .qes_hz_no_wave(parts)
    attr(out, "qes_no_wave") <- no_wave
  }

  # notices
  um <- do.call(rbind, lapply(unname(parts), `[[`, "unmapped"))
  if (!is.null(um) && identical(unmapped, "warn")) {
    what <- sprintf("%s %s %s (%s)", um$study, um$target, ifelse(is.na(um$code), "NA", um$code), um$n)
    .qes_warn("unmapped_warn", class = "qesR_warning_unmapped",
              args = list(paste(utils::head(what, 10L), collapse = "; ")),
              data = list(unmapped = um))
  }
  if (nrow(failed) > 0L) {
    .qes_warn("harmonize_partial", class = "qesR_warning_partial",
              args = list(.qes_q(failed$study)), data = list(failed = failed))
  }
  if (!is.null(cell)) {
    approx <- cell[cell$included & cell$grade %in% "approximate", , drop = FALSE]
    if (nrow(approx) > 0L && identical(min_grade, "approximate")) {
      .qes_inform("approximate_cells", class = "qesR_message_approximate_cells",
                  args = list(paste(approx$study, approx$target, collapse = ", ")),
                  data = list(cells = approx[, c("study", "target")]), quiet = quiet)
    }
    zeros <- .qes_hz_zero_summary(cell)
    if (length(zeros) > 0L) {
      .qes_inform("structural_zeros", class = "qesR_message_structural_zeros",
                  args = list(paste(zeros, collapse = "; ")), quiet = quiet)
    }
    unsigned <- cell[cell$included & !cell$status %in% "stable", , drop = FALSE]
    if (nrow(unsigned) > 0L) {
      .qes_inform("unreviewed_cells", class = "qesR_message_unreviewed_cells",
                  args = list(nrow(unsigned)), data = list(cells = unsigned[, c("study", "target", "status")]),
                  quiet = quiet)
    }
    skipped <- cell[cell$excluded %in% "not_reviewed", , drop = FALSE]
    if (nrow(skipped) > 0L) {
      n_values <- sum(skipped$n_not_reviewed, na.rm = TRUE)
      skipped_data <- list(cells = skipped[, c("study", "target", "status")], n_values = n_values)
      if (!any(cell$included)) {
        # nothing was applied: every value of the result is NA
        .qes_warn("unreviewed_all", class = "qesR_warning_all_unreviewed",
                  args = list(n_values, .qes_q(unique(skipped$target))), data = skipped_data)
      } else {
        .qes_inform("unreviewed_skipped", class = "qesR_message_unreviewed_skipped",
                    args = list(nrow(skipped), n_values), data = skipped_data, quiet = quiet)
      }
    }
  }
  pool_lines <- .qes_pool_summary(pooled_prov)
  if (length(pool_lines) > 0L) {
    .qes_inform("pooled_types", class = "qesR_message_pooled",
                args = list(paste(pool_lines, collapse = "; ")),
                data = list(pooled = pooled_prov), quiet = quiet)
  }
  if (NROW(no_wave) > 0L) {
    .qes_inform("long_no_wave", class = "qesR_message_no_wave",
                args = list(sum(no_wave$n_rows), paste(sprintf("%s (%d)", no_wave$study, no_wave$n_rows), collapse = ", ")),
                data = list(no_wave = no_wave), quiet = quiet)
  }
  .qes_hz_weight_notices(parts, attr(out, "qes_weight_guide"), quiet, ctx$layout)
  out
}

# The weight guide (design.md section 5.3): for each requested target and
# study, the weight column that matches the timing of the wave that asked
# the question, and its variable. wave and weight_column are NA when no row
# was applied for the target in the study; weight_var and weight_status are
# NA when the wave has no recommended weight. A "*" row of a panel names a
# weight only when all its waves agree on it.
.qes_hz_weight_guide <- function(parts, ctx) {
  tg <- ctx$spec$tables$targets
  rows <- list()
  for (part in parts) {
    for (t in (if (is.null(ctx$out_targets)) ctx$targets else ctx$out_targets)) {
      cw <- part$cell_wave[[t]]
      included <- isTRUE(part$cells$included[match(t, part$cells$target)])
      # a row of wave "*" in pooled polls takes the first poll wave: the
      # polls share their timing (V-S9) and weight variable. In a panel, a
      # "*" row (a static target) covers waves with their own weights: the
      # guide names a weight only when those waves agree, else NA (each
      # respondent's weight is that of the waves they took part in)
      ws <- if (is.na(cw) || !included) integer(0) else .qes_wave_rows(part$wv, cw)
      if (length(ws) > 1L && .qes_poll_study(part$wv, part$study)) ws <- ws[1]
      timing <- tg$target_timing[match(t, tg$target)]
      column_of <- function(w) {
        if (identical(ctx$layout, "long")) return("weight")
        cols <- .qes_hz_weight_columns(part$wv$wave_timing[w])
        if (length(cols) == 2L) {
          if (timing %in% "pre") "weight_pre" else "weight_post"
        } else if (length(cols) == 1L) cols else NA_character_
      }
      one <- function(x) {
        x <- unique(x)
        if (length(x) == 1L) x else NA_character_
      }
      w <- if (length(ws) > 0L) ws[1] else NA_integer_
      column <- NA_character_
      var <- NA_character_
      status <- NA_character_
      if (length(ws) > 0L) {
        column <- one(vapply(ws, column_of, character(1)))
        var <- one(vapply(ws, function(k) part$weights[[k]]$var %||% NA_character_, character(1)))
        status <- one(vapply(ws, function(k) part$weights[[k]]$status %||% NA_character_, character(1)))
      }
      rows[[length(rows) + 1L]] <- data.frame(
        target = t, study = part$study, wave = if (is.na(w)) NA_character_ else cw, target_timing = timing,
        weight_column = column, weight_var = var, weight_status = status,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) {
    return(data.frame(target = character(0), study = character(0), wave = character(0),
                      target_timing = character(0), weight_column = character(0),
                      weight_var = character(0), weight_status = character(0), stringsAsFactors = FALSE))
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Notices about weights: waves whose recommended weight still needs review
# (their weights are NA), and, in the respondent layout, studies where the
# requested targets (static ones aside) come from waves with different
# weight columns (qesR_message_weight_timing), so that one weight column
# cannot serve them all. A study whose weights all need review gets no
# timing notice: both of its weight columns are NA.
.qes_hz_weight_notices <- function(parts, guide, quiet, layout = "respondent") {
  review <- character(0)
  for (part in parts) {
    ks <- which(vapply(seq_along(part$weights), function(k) {
      identical(part$weights[[k]]$status, "needs_review") && any(part$members[, k])
    }, logical(1)))
    vars <- vapply(part$weights[ks], `[[`, character(1), "var")
    polls <- .qes_poll_study(part$wv, part$study)
    for (v in unique(vars)) {
      k <- ks[vars == v]
      # the poll waves of pooled polls are given as a range, not listed
      waves <- if (polls && length(k) > 1L) paste0(part$wv$wave[k[1]], "..", part$wv$wave[k[length(k)]]) else part$wv$wave[k]
      review <- c(review, sprintf("%s %s (%s)", part$study, waves, v))
    }
  }
  if (length(review) > 0L) {
    .qes_inform("weight_review", class = "qesR_message_weight_review",
                args = list(paste(review, collapse = ", ")), data = list(weights = review), quiet = quiet)
  }
  # in the long layout each row already carries its wave's weight; static
  # targets do not count (as in qes_design())
  if (identical(layout, "long") || !is.data.frame(guide) || nrow(guide) == 0L) {
    return(invisible())
  }
  g <- guide[!is.na(guide$weight_column) & !guide$target_timing %in% "static", , drop = FALSE]
  mixed <- character(0)
  for (st in unique(g$study)) {
    x <- g[g$study == st, , drop = FALSE]
    if (length(unique(x$weight_column)) > 1L && !all(x$weight_status %in% "needs_review")) mixed <- c(mixed, st)
  }
  if (length(mixed) > 0L) {
    .qes_inform("weight_timing", class = "qesR_message_weight_timing",
                args = list(.qes_q(mixed)), data = list(study = mixed), quiet = quiet)
  }
  invisible()
}

# "RemoteSha" of an installed GitHub build, else NA.
.qes_description_sha <- function() {
  sha <- tryCatch(utils::packageDescription("qesR", fields = "RemoteSha"), error = function(e) NA)
  if (length(sha) != 1L || is.na(sha)) NA_character_ else as.character(sha)
}

# Structural zeros per target: "vote_prov_recall: PCQ, ADQ (qes2012), ...".
.qes_hz_zero_summary <- function(cell) {
  z <- cell[cell$included & !is.na(cell$levels_not_offered) & nzchar(cell$levels_not_offered), , drop = FALSE]
  if (nrow(z) == 0L) {
    return(character(0))
  }
  vapply(unique(z$target), function(t) {
    r <- z[z$target == t, , drop = FALSE]
    paste0(t, ": ", paste(sprintf("%s (%s)", r$study, gsub(";", ", ", r$levels_not_offered, fixed = TRUE)), collapse = ", "))
  }, character(1), USE.NAMES = FALSE)
}

#' @export
print.qes_harmonized <- function(x, n = 6L, ...) {
  lang <- .qes_lang()
  sp <- attr(x, "qes_spec", exact = TRUE)
  prov <- attr(x, "qes_provenance", exact = TRUE)
  cell <- if (is.null(prov)) NULL else attr(prov, "cell", exact = TRUE)
  df <- x
  class(df) <- "data.frame"
  if (!is.null(sp)) {
    studies <- unique(as.character(df$study))
    head <- if (nrow(df) == 0L) {
      .qes_msg("hz_print_head_empty", list(sp$version, sp$hash), lang)
    } else {
      .qes_msg("hz_print_head", list(nrow(df), .qes_q(studies), sp$version, sp$hash), lang)
    }
    cat(head, "\n", sep = "")
    if (isTRUE(sp$custom)) {
      cat(.qes_msg("hz_print_custom", list(), lang), "\n", sep = "")
    }
  }
  if (!is.null(cell)) {
    approx <- cell[cell$included & cell$grade %in% "approximate", , drop = FALSE]
    if (nrow(approx) > 0L) {
      cat(.qes_msg("hz_print_approx", list(paste(approx$study, approx$target, collapse = ", ")), lang), "\n", sep = "")
    }
    below <- cell[cell$n_below_grade > 0L, , drop = FALSE]
    if (nrow(below) > 0L) {
      cat(.qes_msg("hz_print_below", list(paste(below$study, below$target, collapse = ", ")), lang), "\n", sep = "")
    }
    zeros <- .qes_hz_zero_summary(cell)
    if (length(zeros) > 0L) {
      cat(.qes_msg("hz_print_zeros", list(paste(zeros, collapse = "; ")), lang), "\n", sep = "")
    }
    pool_lines <- .qes_pool_summary(attr(prov, "pooled", exact = TRUE))
    if (length(pool_lines) > 0L) {
      cat(.qes_msg("hz_print_pooled", list(paste(pool_lines, collapse = "; ")), lang), "\n", sep = "")
    }
    unsigned <- sum(cell$included & !cell$status %in% "stable")
    if (unsigned > 0L) {
      cat(.qes_msg("hz_print_unreviewed", list(unsigned), lang), "\n", sep = "")
    }
    skipped <- cell$excluded %in% "not_reviewed"
    if (any(skipped)) {
      n_values <- if ("n_not_reviewed" %in% names(cell)) sum(cell$n_not_reviewed[skipped], na.rm = TRUE) else NA_integer_
      cat(.qes_msg("hz_print_unreviewed_skipped", list(sum(skipped), n_values), lang), "\n", sep = "")
    }
  }
  guide <- attr(x, "qes_weight_guide", exact = TRUE)
  if (is.data.frame(guide) && "weight_status" %in% names(guide)) {
    g <- unique(guide[guide$weight_status %in% "needs_review", c("study", "wave", "weight_var"), drop = FALSE])
    if (nrow(g) > 0L) {
      cat(.qes_msg("hz_print_weight_review", list(paste(sprintf("%s %s (%s)", g$study, g$wave, g$weight_var),
                                                        collapse = ", ")), lang), "\n", sep = "")
    }
  }
  failed <- attr(x, "failed_studies", exact = TRUE)
  if (is.data.frame(failed) && nrow(failed) > 0L) {
    cat(.qes_msg("hz_print_failed", list(.qes_q(failed$study)), lang), "\n", sep = "")
  }
  if (!is.null(prov) && any(prov$licence %in% "CC BY-NC 4.0")) {
    nc <- prov$study[prov$licence %in% "CC BY-NC 4.0"]
    cat(.qes_msg("hz_print_licence", list(.qes_q(nc)), lang), "\n", sep = "")
  }
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || is.na(n) || n < 0L) n <- 6L
  print(utils::head(df, n), ...)
  if (nrow(df) > n) {
    cat(.qes_msg("search_more", list(nrow(df) - n), lang), "\n", sep = "")
  }
  invisible(x)
}

# rbind() of harmonized data: the rows, and the provenance of every part
# (study and cell records; one spec-level row per qes_harmonize() call), so
# printing and qes_provenance() describe all of them. The parts must come
# from the same spec (content hash) and cover different studies; if one
# argument is not harmonized data, the result is a plain data frame without
# the qes_* attributes.
#' @export
rbind.qes_harmonized <- function(..., deparse.level = 1) {
  args <- list(...)
  args <- args[!vapply(args, is.null, logical(1))]
  # the long layout has a `wave` column, the respondent layout `waves`
  layouts <- unique(unlist(lapply(args, function(a) {
    if (!inherits(a, "qes_harmonized") || !is.data.frame(a)) return(NULL)
    if ("wave" %in% names(a)) "long" else if ("waves" %in% names(a)) "respondent" else NULL
  })))
  if (length(layouts) > 1L) {
    .qes_abort("hz_rbind_layout", class = "qesR_error_input", data = list(layouts = layouts))
  }
  strip <- function(a) {
    if (!is.data.frame(a)) return(a)
    class(a) <- "data.frame"
    attr(a, "qes_spec") <- NULL
    attr(a, "qes_provenance") <- NULL
    attr(a, "qes_weight_guide") <- NULL
    attr(a, "failed_studies") <- NULL
    attr(a, "qes_no_wave") <- NULL
    a
  }
  harmonized <- vapply(args, function(a) {
    inherits(a, "qes_harmonized") && is.data.frame(a) && !is.null(attr(a, "qes_spec", exact = TRUE))
  }, logical(1))
  if (length(args) == 0L || !all(harmonized)) {
    out <- do.call(rbind, c(lapply(args, strip), list(deparse.level = deparse.level)))
    rownames(out) <- NULL
    return(out)
  }
  # every check runs before the bind, so a mismatch gets a classed message
  # rather than a bare base-R error
  specs <- lapply(args, attr, "qes_spec", exact = TRUE)
  hashes <- unique(vapply(specs, function(s) as.character(s$hash), character(1)))
  if (length(hashes) > 1L) {
    .qes_abort("hz_rbind_spec", class = "qesR_error_input", args = list(.qes_q(hashes)),
               data = list(hashes = hashes))
  }
  # the build options (objects made before they were recorded have none)
  opts <- lapply(specs, `[[`, "options")
  if (!any(vapply(opts, is.null, logical(1)))) {
    # the pooled variables' types are compared after the columns (a part
    # without pooled variables has other columns, reported as such)
    keys <- setdiff(unique(unlist(lapply(opts, names), use.names = FALSE)), "types")
    differ <- list()
    for (k in keys) {
      v <- vapply(opts, function(o) if (is.null(o[[k]])) NA_character_ else as.character(o[[k]])[1], character(1))
      if (length(unique(v)) > 1L) differ[[k]] <- v
    }
    if (length(differ) > 0L) {
      what <- vapply(names(differ), function(k) {
        sprintf("%s = %s", k, paste(sprintf("\"%s\"", unique(differ[[k]])), collapse = " / "))
      }, character(1))
      .qes_abort("hz_rbind_options", class = "qesR_error_input",
                 args = list(paste(what, collapse = "; ")),
                 data = list(options = differ))
    }
  }
  # the same columns: parts harmonized with different targets cannot be bound
  cols <- lapply(args, names)
  all_cols <- unique(unlist(cols, use.names = FALSE))
  lacking <- lapply(cols, function(cn) setdiff(all_cols, cn))
  if (any(lengths(lacking) > 0L)) {
    who <- vapply(seq_along(args), function(i) {
      st <- paste(unique(as.character(args[[i]]$study)), collapse = ", ")
      if (!nzchar(st)) st <- sprintf("#%d", i)
      if (length(lacking[[i]]) == 0L) NA_character_ else sprintf("%s: %s", st, paste(lacking[[i]], collapse = ", "))
    }, character(1))
    .qes_abort("hz_rbind_targets", class = "qesR_error_input",
               args = list(paste(who[!is.na(who)], collapse = "; ")),
               data = list(missing = lacking))
  }
  types <- vapply(opts, function(o) if (is.null(o$types)) NA_character_ else as.character(o$types)[1], character(1))
  if (length(unique(types)) > 1L) {
    .qes_abort("hz_rbind_options", class = "qesR_error_input",
               args = list(sprintf("types = %s", paste(sprintf("\"%s\"", unique(types)), collapse = " / "))),
               data = list(options = list(types = types)))
  }
  studies <- unlist(lapply(args, function(a) unique(as.character(a$study))), use.names = FALSE)
  dup <- unique(studies[duplicated(studies)])
  if (length(dup) > 0L) {
    .qes_abort("hz_rbind_study", class = "qesR_error_input", args = list(.qes_q(dup)),
               data = list(study = dup))
  }
  # columns in the order of the first part (base rbind matches them by name)
  out <- do.call(rbind, c(lapply(args, strip), list(deparse.level = deparse.level)))
  rownames(out) <- NULL
  provs <- lapply(args, attr, "qes_provenance", exact = TRUE)
  provs <- provs[vapply(provs, is.data.frame, logical(1))]
  bind <- function(x) {
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x) == 0L) return(NULL)
    x <- lapply(x, function(d) {
      d <- as.data.frame(d, stringsAsFactors = FALSE)
      attr(d, "cell") <- NULL
      attr(d, "spec") <- NULL
      attr(d, "pooled") <- NULL
      d
    })
    r <- do.call(rbind, unname(x))
    rownames(r) <- NULL
    r
  }
  study_prov <- bind(provs)
  if (!is.null(study_prov)) {
    attr(study_prov, "cell") <- bind(lapply(provs, attr, "cell", exact = TRUE))
    attr(study_prov, "spec") <- bind(lapply(provs, attr, "spec", exact = TRUE))
    pooled <- bind(lapply(provs, attr, "pooled", exact = TRUE))
    if (!is.null(pooled)) attr(study_prov, "pooled") <- pooled
  }
  failed <- bind(lapply(args, attr, "failed_studies", exact = TRUE))
  # rbind.data.frame drops the variable labels of factor columns
  for (nm in names(out)) {
    lab <- attr(args[[1]][[nm]], "label", exact = TRUE)
    if (!is.null(lab) && is.null(attr(out[[nm]], "label", exact = TRUE))) attr(out[[nm]], "label") <- lab
  }
  class(out) <- c("qes_harmonized", "data.frame")
  attr(out, "qes_spec") <- specs[[1]]
  attr(out, "qes_provenance") <- study_prov
  attr(out, "qes_weight_guide") <- bind(lapply(args, attr, "qes_weight_guide", exact = TRUE))
  attr(out, "failed_studies") <- failed
  if (identical(layouts, "long")) {
    nw <- lapply(args, attr, "qes_no_wave", exact = TRUE)
    nw <- nw[!vapply(nw, is.null, logical(1))]
    if (length(nw) > 0L) {
      nw <- do.call(rbind, unname(nw))
      rownames(nw) <- NULL
      attr(out, "qes_no_wave") <- nw
    }
  }
  out
}

# ---- column hashes (V-L1) ---------------------------------------------------------

# md5 of one harmonized column: each row's value (level name, or number as
# .qes_code_chr() writes it) or "NA:<reason>", one per line, in file order.
.qes_hz_column_md5 <- function(value, reason) {
  v <- if (is.numeric(value)) .qes_code_chr(value) else as.character(value)
  .qes_md5_text(paste(ifelse(is.na(v), paste0("NA:", reason), v), collapse = "\n"))
}

# The spec_hashes table of harmonized data `x`, built with values = "code"
# and missing = "reasons": one row per (study, target) cell that was applied.
.qes_hz_hashes <- function(x) {
  cell <- attr(attr(x, "qes_provenance", exact = TRUE), "cell", exact = TRUE)
  # derived cells (rule derive:<name>) are not crosswalk rows: the column
  # hashes of their sources cover them
  cell <- cell[cell$included & cell$target %in% names(x) & !startsWith(ifelse(is.na(cell$rule), "", cell$rule), "derive:"), , drop = FALSE]
  rows <- lapply(seq_len(nrow(cell)), function(k) {
    s <- cell$study[k]
    t <- cell$target[k]
    in_s <- x$study == s
    data.frame(study = s, wave = cell$wave[k], target = t, source_var = cell$source_var[k],
               n = sum(in_s),
               md5 = .qes_hz_column_md5(unclass(x[[t]])[in_s], as.character(x[[paste0(t, "__na")]])[in_s]),
               stringsAsFactors = FALSE)
  })
  # the pooled columns (V-F9): one row per study and pooled variable, wave
  # "*", source_var the member types applied, in order of precedence
  pooled <- attr(attr(x, "qes_provenance", exact = TRUE), "pooled", exact = TRUE)
  if (is.data.frame(pooled) && nrow(pooled) > 0L) {
    for (s in unique(pooled$study)) {
      for (p in unique(pooled$pooled[pooled$study == s])) {
        if (!p %in% names(x) || !paste0(p, "__na") %in% names(x)) next
        m <- pooled[pooled$study == s & pooled$pooled == p & pooled$included & pooled$used_in_layout, , drop = FALSE]
        if (nrow(m) == 0L) next
        in_s <- x$study == s
        rows[[length(rows) + 1L]] <- data.frame(
          study = s, wave = .qes_all_waves, target = p, source_var = paste(m$type[order(m$precedence)], collapse = "+"),
          n = sum(in_s), md5 = .qes_hz_column_md5(unclass(x[[p]])[in_s], as.character(x[[paste0(p, "__na")]])[in_s]),
          stringsAsFactors = FALSE)
      }
    }
  }
  out <- if (length(rows) > 0L) do.call(rbind, rows) else
    .qes_apply_schema(as.data.frame(stats::setNames(rep(list(character(0)), 6L), names(.qes_schemas$spec_hashes)),
                                    stringsAsFactors = FALSE), "spec_hashes")
  rownames(out) <- NULL
  out
}
