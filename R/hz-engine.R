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
# Waves and weights (the long layout, weight columns, eligibility, interview
# dates) come with slice HZ4.
#
# The synthetic study qes_demo stands in for qes2014 (its variables are a
# subset of the qes2014 file): it is harmonized with the qes2014 rows whose
# variables it has; the others are not_asked.

.qes_hz_grades <- c("identical", "comparable", "approximate")
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

# Target, family and set names (disjoint, V-S15) -> target names, in the
# order of targets.csv. A retired target is included only when named.
.qes_hz_resolve_targets <- function(targets, spec) {
  tg <- spec$tables$targets
  if (!is.character(targets) || length(targets) == 0L || anyNA(targets) || !all(nzchar(targets))) {
    .qes_abort("input_targets", class = "qesR_error_input", data = list(arg = "targets", value = targets))
  }
  sets <- lapply(tg$sets, .qes_split_list)
  live <- !tg$status %in% "retired"
  hit <- rep(FALSE, nrow(tg))
  unknown <- character(0)
  for (t in unique(targets)) {
    by_target <- tg$target == t
    by_group <- live & (tg$family %in% t | vapply(sets, function(s) t %in% s, logical(1)))
    if (!any(by_target) && !any(by_group)) {
      unknown <- c(unknown, t)
    }
    hit <- hit | by_target | by_group
  }
  if (length(unknown) > 0L) {
    names_ <- unique(c(tg$target, tg$family[!is.na(tg$family)], unlist(sets)))
    suggestions <- unique(unlist(lapply(unknown, .qes_suggest_codes, codes = names_)))
    .qes_abort(
      if (length(suggestions) > 0L) "input_targets_unknown_suggest" else "input_targets_unknown",
      class = "qesR_error_input",
      args = if (length(suggestions) > 0L) list(.qes_q(unknown), .qes_q(suggestions)) else list(.qes_q(unknown)),
      data = list(arg = "targets", value = unknown, suggestions = suggestions %||% character(0))
    )
  }
  tg$target[hit]
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
# identifiers are errors.
.qes_hz_ids <- function(study, d) {
  file_row <- .qes_default_data_file(study, demo = TRUE)
  ids <- .qes_split_list(file_row$id_vars)
  if (length(ids) == 0L || identical(ids, ".row")) {
    return(list(key = as.character(seq_len(nrow(d))), id_vars = ".row"))
  }
  ids <- setdiff(ids, ".row")
  miss <- setdiff(ids, names(d))
  if (length(miss) > 0L) {
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
  if (base %in% c("map", "numeric")) {
    ln <- if (r$from_label) .qes_hz_col_label_numbers(x, code) else rep(NA_real_, n)
    gate <- rep(NA_character_, n)
    if (!is.na(r$gate_var)) {
      gate <- .canon(d[[r$gate_var]])
      gate[is.na(gate)] <- "NA"
    }
    oc <- .qes_hz_cell_outcome(r, data.frame(gate_code = gate, source_code = code, stringsAsFactors = FALSE), ln)
    value <- oc$value
    reason <- oc$na_reason
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
      value[ok] <- src[ok]
      reason[rest & !ok] <- "sysmis"
    } else if (base == "constant") {
      value[] <- unname(args["value"])
    } else if (base == "fn") {
      f <- .qes_hz_fns[[sub("^fn:", "", rule)]]
      res <- f(x, list(data = d, row = xw[i, , drop = FALSE], spec = spec))
      value <- as.character(res$value)
      reason <- as.character(res$na_reason)
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

# Harmonize one study: its leading columns, the values, reasons and source
# codes of each target, and its cell and study provenance.
.qes_hz_study <- function(study, frame, ctx) {
  spec <- ctx$spec
  spec_study <- .qes_hz_spec_study(study)
  stand_in <- !identical(spec_study, study)
  verified <- is.null(frame)
  if (verified) {
    d <- .qes_read(study, quiet = ctx$quiet)
    prov <- attr(d, "qes_provenance", exact = TRUE)
  } else {
    d <- frame
    prov <- .qes_hz_user_provenance(study, d)
  }
  n <- nrow(d)
  ids <- .qes_hz_ids(study, d)

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
  wt <- relabel(spec$tables$weights)

  # the row of each target, and why a target has none
  rank <- function(g) match(g, .qes_hz_grades)
  pick <- list()
  for (t in ctx$targets) {
    rows <- which(xw$target == t & xw$primary %in% TRUE & !is.na(xw$rule) & xw$rule != "none")
    if (length(rows) == 0L) {
      pick[[t]] <- list(row = NA_integer_, reason = "not_asked", excluded = "no_row",
                        note = "no row in the spec for this study")
      next
    }
    i <- rows[1]
    vars <- stats::na.omit(c(xw$source_var[i], xw$gate_var[i]))
    if (!xw$status[i] %in% .qes_hz_statuses(ctx$include_draft)) {
      pick[[t]] <- list(row = i, reason = "not_reviewed", excluded = "not_reviewed",
                        note = sprintf("row with status %s, not signed off by a reviewer: not applied (include_draft = FALSE)", xw$status[i]))
    } else if (stand_in && !all(vars %in% names(d))) {
      pick[[t]] <- list(row = i, reason = "not_asked", excluded = "not_in_data",
                        note = "not in the demonstration data")
    } else if (is.na(rank(xw$grade[i])) || rank(xw$grade[i]) > rank(ctx$min_grade)) {
      pick[[t]] <- list(row = i, reason = "below_grade", excluded = "below_grade", note = sprintf("grade %s is below min_grade = \"%s\"", xw$grade[i], ctx$min_grade))
    } else {
      pick[[t]] <- list(row = i, reason = NA_character_, excluded = NA_character_, note = NA_character_)
    }
  }
  apply_rows <- unlist(lapply(pick, function(p) if (is.na(p$reason)) p$row else NULL), use.names = FALSE)

  sub <- spec
  sub$tables$crosswalk <- xw[apply_rows, , drop = FALSE]
  rownames(sub$tables$crosswalk) <- NULL
  sub$tables$waves <- wv
  sub$tables$weights <- wt
  .qes_hz_check_study(sub, d, study, verified, stand_in)

  members <- lapply(seq_len(nrow(wv)), function(w) .qes_wave_members(wv, w, d))
  names(members) <- wv$wave

  values <- list()
  reasons <- list()
  srcs <- list()
  cells <- list()
  unmapped <- list()
  for (t in ctx$targets) {
    p <- pick[[t]]
    if (is.na(p$reason)) {
      k <- match(p$row, apply_rows)
      res <- .qes_hz_apply_row(sub, k, d, members[[xw$wave[p$row]]])
      um <- res$reason %in% "unmapped"
      if (any(um)) {
        tab <- table(res$src[um], useNA = "ifany")
        unmapped[[t]] <- data.frame(study = study, target = t, source_var = xw$source_var[p$row],
                                    code = names(tab), n = as.integer(tab), stringsAsFactors = FALSE)
      }
    } else {
      res <- list(value = rep(NA_character_, n), reason = rep(p$reason, n), src = rep(NA_character_, n))
    }
    values[[t]] <- res$value
    reasons[[t]] <- res$reason
    srcs[[t]] <- res$src
    cells[[t]] <- .qes_hz_cell_row(spec, study, t, xw, p, res, wt)
  }
  unmapped <- if (length(unmapped) > 0L) do.call(rbind, unname(unmapped)) else NULL
  if (!is.null(unmapped) && identical(ctx$unmapped, "error")) {
    first <- unmapped[unmapped$target == unmapped$target[1], , drop = FALSE]
    .qes_abort(
      "unmapped",
      class = "qesR_error_unmapped",
      args = list(.qes_q(study), .qes_q(first$target[1]), .qes_q(first$code), .qes_q(first$source_var[1]), sum(first$n)),
      data = list(study = study, target = first$target[1], codes = first$code, n = first$n, unmapped = unmapped)
    )
  }

  list(
    lead = .qes_hz_lead(study, d, wv, members, ids, ctx$lang),
    values = values, reasons = reasons, srcs = srcs,
    cells = do.call(rbind, unname(cells)),
    provenance = prov,
    unmapped = unmapped,
    fingerprint = if (verified) NA_character_ else .qes_hz_fingerprint(d, .qes_hz_spec_vars(sub, study)),
    ids = ids$id_vars
  )
}

# The leading columns of one study (respondent layout).
.qes_hz_lead <- function(study, d, wv, members, ids, lang) {
  n <- nrow(d)
  cat_ <- .qes_catalog(demo = TRUE)
  s <- cat_$studies[match(study, cat_$studies$study), , drop = FALSE]
  el <- cat_$elections
  e_date <- el$election_date[match(s$election_id, el$election_id)]
  in_wave <- if (length(members) > 0L) do.call(cbind, members) else matrix(FALSE, n, 0L)
  waves <- if (ncol(in_wave) > 0L) {
    apply(in_wave, 1L, function(m) if (any(m)) paste(wv$wave[m], collapse = ";") else NA_character_)
  } else {
    rep(NA_character_, n)
  }
  pop_col <- paste0("target_population_", lang)
  default_pop <- s[[pop_col]]
  first_wave <- if (ncol(in_wave) > 0L) apply(in_wave, 1L, function(m) if (any(m)) which(m)[1] else NA_integer_) else rep(NA_integer_, n)
  pop <- wv[[pop_col]][first_wave]
  pop[is.na(pop)] <- default_pop
  sub_var <- unique(stats::na.omit(wv$subsample_var))
  subsample <- if (length(sub_var) > 0L && sub_var[1] %in% names(d)) .canon(d[[sub_var[1]]]) else rep(NA_character_, n)
  data.frame(
    study = rep(study, n),
    year = rep(as.integer(s$year), n),
    election_date = rep(e_date, n),
    family = rep(s$family, n),
    study_design = rep(s$study_design, n),
    target_population = pop,
    waves = waves,
    qes_id = paste0(study, ":", ids$key),
    subsample = subsample,
    source_row = seq_len(n),
    stringsAsFactors = FALSE
  )
}

# The reasons a harmonized value can be missing, in vocabulary order (the
# levels of the __na companions and the n_* columns of cell provenance).
.qes_hz_reason_levels <- function() {
  r <- .qes_enum("missing_type")
  r$value[vapply(r$scope, function(s) any(c("spec", "engine") %in% .qes_split_list(s)), logical(1))]
}

# One row of cell provenance (design.md section 5.9) for target `t` of a
# study: the row applied (or why none was), its grade and instrument, the
# levels its question did not offer, and the count of values and of each
# NA reason over the study's rows.
.qes_hz_cell_row <- function(spec, study, t, xw, p, res, wt) {
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
  weight_var <- NA_character_
  if (has_row) {
    k <- which(wt$wave == xw$wave[i] & wt$recommended %in% TRUE)
    if (length(k) > 0L) weight_var <- wt$weight_var[k[1]]
  }
  levels <- .qes_hz_reason_levels()
  counts <- as.list(as.integer(table(factor(res$reason, levels = levels))))
  names(counts) <- paste0("n_", levels)
  out <- data.frame(
    study = study, wave = get("wave"), target = t, source_var = get("source_var"),
    rule = get("rule"), map_id = get("map_id"), grade = get("grade"),
    status = get("status"), instrument = get("instrument"), weight_var = weight_var,
    levels_not_offered = not_offered,
    included = is.na(p$reason),
    excluded = p$excluded,
    n_valid = sum(!is.na(res$value)),
    stringsAsFactors = FALSE
  )
  out <- cbind(out, as.data.frame(counts, stringsAsFactors = FALSE))
  out$note <- p$note
  out
}

# ---- output ------------------------------------------------------------------------

# Encode the values (level names or number text) of target `t`.
.qes_hz_encode <- function(value, t, spec, values, lang) {
  tg <- spec$tables$targets
  j <- match(t, tg$target)
  type <- tg$type[j]
  label <- tg[[paste0("label_", lang)]][j]
  out <- if (type %in% c("categorical", "ordinal")) {
    set <- .qes_spec_levels(spec$tables$levels, tg$levels_id[j])
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

# Combine the studies into the respondent-layout data frame.
.qes_hz_assemble <- function(parts, ctx) {
  lead <- do.call(rbind, lapply(parts, `[[`, "lead"))
  cols <- as.list(lead)
  for (t in ctx$targets) {
    v <- unlist(lapply(parts, function(p) p$values[[t]]), use.names = FALSE)
    cols[[t]] <- .qes_hz_encode(v, t, ctx$spec, ctx$values, ctx$lang)
  }
  if (identical(ctx$missing, "reasons")) {
    levels <- .qes_hz_reason_levels()
    for (t in ctx$targets) {
      r <- unlist(lapply(parts, function(p) p$reasons[[t]]), use.names = FALSE)
      cols[[paste0(t, "__na")]] <- factor(r, levels = levels)
    }
  }
  if (isTRUE(ctx$keep_source)) {
    for (t in ctx$targets) {
      cols[[paste0(t, "__src")]] <- unlist(lapply(parts, function(p) p$srcs[[t]]), use.names = FALSE)
    }
  }
  structure(cols, class = c("qes_harmonized", "data.frame"), row.names = c(NA_integer_, -nrow(lead)))
}

# An empty result (every study failed under on_fail = "skip").
.qes_hz_empty <- function(ctx) {
  lead <- data.frame(
    study = character(0), year = integer(0), election_date = as.Date(character(0)),
    family = character(0), study_design = character(0), target_population = character(0),
    waves = character(0), qes_id = character(0), subsample = character(0),
    source_row = integer(0), stringsAsFactors = FALSE
  )
  parts <- list(list(
    lead = lead,
    values = stats::setNames(rep(list(character(0)), length(ctx$targets)), ctx$targets),
    reasons = stats::setNames(rep(list(character(0)), length(ctx$targets)), ctx$targets),
    srcs = stats::setNames(rep(list(character(0)), length(ctx$targets)), ctx$targets)
  ))
  .qes_hz_assemble(parts, ctx)
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
#' this version no row of the shipped spec is signed off yet, so the default
#' call gives missing values (reason `not_reviewed`) and the examples pass
#' `include_draft = TRUE`.
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
#' four-point and 0-10 interest scales are all separate targets, never
#' pooled). Each study's question gets a grade against the target's anchor
#' question:
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
#' Voir `vignette("fr-reference-harmonisation", package = "qesR")`.
#'
#' @param studies Study codes (see [qes_studies()]). `NULL` (default) means
#'   the Quebec Election Studies the spec covers, or the studies named in
#'   `data` when it is given; `"all"` means every study the spec covers.
#' @param targets Target, family or set names (see [qes_spec()]); the
#'   default `"core"` is the core set.
#' @param layout `"respondent"`: one row per respondent of each study's file.
#'   The long layout (one row per respondent and wave) is not available yet.
#' @param values How categorical targets are returned: `"factor"` (default;
#'   ordered for ordinal targets, with the target's levels in every study),
#'   `"labelled"` ([haven::labelled()] integer codes that are stable across
#'   versions) or `"code"` (the ASCII level names). Numeric targets are
#'   numbers.
#' @param missing `"na"` (default) or `"reasons"`, which adds a factor column
#'   `<target>__na` giving the reason of each missing value.
#' @param min_grade The lowest comparability grade kept: `"approximate"`
#'   (default, every graded cell), `"comparable"` or `"identical"`.
#' @param weights Reserved for the weight columns, which are not produced
#'   yet; the value is checked but has no effect.
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
#'   counts the cells that use them.
#' @param data `NULL` (read the pinned files) or a named list of data frames
#'   as [get_qes()] returns them, named by study code.
#' @param spec `NULL` (the spec shipped with qesR), the path of a spec
#'   directory, or a `qes_spec` object (see [qes_spec()]).
#' @param lang Language of returned labels (factor levels, variable labels
#'   and target populations): `"en"` (default) or `"fr"`. Codes and reasons
#'   do not depend on it.
#' @param quiet If `TRUE`, no progress or informational messages.
#'
#' @return A data frame of class `qes_harmonized`, returned visibly, one row
#'   per respondent of each study's file (no row is dropped). Leading
#'   columns: `study`, `year`, `election_date`, `family`, `study_design`,
#'   `target_population`, `waves` (the waves the respondent belongs to,
#'   `;`-separated), `qes_id` (`<study>:<identifier>`, unique), `subsample`
#'   and `source_row` (the row in the study's file, for joining raw
#'   variables with [merge()] on `study` and `source_row`). Then one column
#'   per target, carrying its label in `attr(, "label")`, then the
#'   `__na` and `__src` companions. Attributes: `qes_spec` (spec `version`,
#'   `hash`, `custom`, `engine`), `qes_provenance` (see [qes_provenance()],
#'   with levels `"study"`, `"cell"` and `"spec"`) and `failed_studies`
#'   (`study`, `class`, `message`, `parent_message`).
#'
#'   Results for different studies built with the same spec can be combined
#'   with [rbind()], which also combines their provenance (an error if the
#'   spec content hashes differ or a study appears twice); harmonizing all
#'   the studies in one call is simpler.
#'
#' @family harmonization
#' @seealso [qes_spec()] for the targets and the mapping of each study,
#'   `vignette("harmonization-reference", package = "qesR")` for the
#'   reference generated from the spec, [qes_provenance()] for where each
#'   value came from.
#' @examples
#' # the synthetic demonstration study, harmonized with the qes2014 rows
#' # (include_draft = TRUE: the shipped rows are not yet signed off)
#' h <- qes_harmonize("qes_demo", include_draft = TRUE)
#' h
#' table(h$vote_prov_recall, useNA = "ifany")
#'
#' # why values are missing, and the grade of each cell
#' h <- qes_harmonize("qes_demo", targets = c("vote", "sov_indep"),
#'                    missing = "reasons", include_draft = TRUE, quiet = TRUE)
#' table(h$vote_prov_recall__na)
#' cells <- qes_provenance(h, level = "cell")
#' cells[, c("study", "target", "source_var", "grade", "n_valid")]
#'
#' # French labels, same codes
#' h_fr <- qes_harmonize("qes_demo", targets = "interest_4pt", lang = "fr",
#'                       include_draft = TRUE, quiet = TRUE)
#' levels(h_fr$interest_4pt)
#' @export
qes_harmonize <- function(studies = NULL, targets = "core", layout = c("respondent", "long"),
                          values = c("factor", "labelled", "code"), missing = c("na", "reasons"),
                          min_grade = c("approximate", "comparable", "identical"),
                          weights = c("normalized", "raw"), unmapped = c("error", "warn", "na"),
                          on_fail = c("stop", "skip"), keep_source = FALSE, include_draft = FALSE,
                          data = NULL, spec = NULL, lang = c("en", "fr"), quiet = FALSE) {
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
  if (identical(layout, "long")) {
    .qes_abort("harmonize_later", class = "qesR_error_input",
               args = list("qes_harmonize(layout = \"long\")"),
               data = list(arg = "layout", value = layout))
  }
  sp <- .qes_spec_get(spec, "error")
  if (!is.null(data)) {
    data <- .qes_spec_data_arg(data, demo = TRUE)
  }
  study_codes <- .qes_hz_resolve_studies(studies, data, sp)
  target_names <- .qes_hz_resolve_targets(targets, sp)
  ctx <- list(spec = sp, targets = target_names, values = values, missing = missing,
              min_grade = min_grade, unmapped = unmapped, include_draft = include_draft,
              keep_source = keep_source, lang = lang, quiet = quiet)
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
  cell <- if (length(parts) > 0L) do.call(rbind, lapply(unname(parts), `[[`, "cells")) else NULL
  if (!is.null(cell)) rownames(cell) <- NULL
  study_prov <- if (length(parts) > 0L) do.call(rbind, lapply(unname(parts), `[[`, "provenance")) else NULL
  if (!is.null(study_prov)) rownames(study_prov) <- NULL
  fingerprints <- unlist(lapply(parts, `[[`, "fingerprint"))
  fingerprints <- fingerprints[!is.na(fingerprints)]
  call_args <- list(studies = study_codes, targets = targets, layout = layout, values = values,
                    missing = missing, min_grade = min_grade, weights = weights,
                    unmapped = unmapped, on_fail = on_fail, keep_source = keep_source,
                    include_draft = include_draft, spec = if (is.null(spec)) NULL else sp, lang = lang)
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
  }
  attr(out, "qes_spec") <- list(version = sp$version, hash = sp$hash, custom = isTRUE(sp$custom),
                                engine = as.character(.qes_engine_version()))
  attr(out, "qes_provenance") <- study_prov
  attr(out, "failed_studies") <- failed

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
      .qes_inform("unreviewed_skipped", class = "qesR_message_unreviewed_skipped",
                  args = list(nrow(skipped)), data = list(cells = skipped[, c("study", "target", "status")]),
                  quiet = quiet)
    }
  }
  out
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
    unsigned <- sum(cell$included & !cell$status %in% "stable")
    if (unsigned > 0L) {
      cat(.qes_msg("hz_print_unreviewed", list(unsigned), lang), "\n", sep = "")
    }
    skipped <- sum(cell$excluded %in% "not_reviewed")
    if (skipped > 0L) {
      cat(.qes_msg("hz_print_unreviewed_skipped", list(skipped), lang), "\n", sep = "")
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
  strip <- function(a) {
    if (!is.data.frame(a)) return(a)
    class(a) <- "data.frame"
    attr(a, "qes_spec") <- NULL
    attr(a, "qes_provenance") <- NULL
    attr(a, "failed_studies") <- NULL
    a
  }
  out <- do.call(rbind, c(lapply(args, strip), list(deparse.level = deparse.level)))
  rownames(out) <- NULL
  harmonized <- vapply(args, function(a) inherits(a, "qes_harmonized") && !is.null(attr(a, "qes_spec", exact = TRUE)),
                       logical(1))
  if (length(args) == 0L || !all(harmonized)) {
    return(out)
  }
  specs <- lapply(args, attr, "qes_spec", exact = TRUE)
  hashes <- unique(vapply(specs, function(s) as.character(s$hash), character(1)))
  if (length(hashes) > 1L) {
    .qes_abort("hz_rbind_spec", class = "qesR_error_input", args = list(.qes_q(hashes)),
               data = list(hashes = hashes))
  }
  studies <- unlist(lapply(args, function(a) unique(as.character(a$study))), use.names = FALSE)
  dup <- unique(studies[duplicated(studies)])
  if (length(dup) > 0L) {
    .qes_abort("hz_rbind_study", class = "qesR_error_input", args = list(.qes_q(dup)),
               data = list(study = dup))
  }
  provs <- lapply(args, attr, "qes_provenance", exact = TRUE)
  provs <- provs[vapply(provs, is.data.frame, logical(1))]
  bind <- function(x) {
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x) == 0L) return(NULL)
    x <- lapply(x, function(d) {
      d <- as.data.frame(d, stringsAsFactors = FALSE)
      attr(d, "cell") <- NULL
      attr(d, "spec") <- NULL
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
  attr(out, "failed_studies") <- failed
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
  cell <- cell[cell$included, , drop = FALSE]
  rows <- lapply(seq_len(nrow(cell)), function(k) {
    s <- cell$study[k]
    t <- cell$target[k]
    in_s <- x$study == s
    data.frame(study = s, wave = cell$wave[k], target = t, source_var = cell$source_var[k],
               n = sum(in_s),
               md5 = .qes_hz_column_md5(unclass(x[[t]])[in_s], as.character(x[[paste0(t, "__na")]])[in_s]),
               stringsAsFactors = FALSE)
  })
  out <- if (length(rows) > 0L) do.call(rbind, rows) else
    .qes_apply_schema(as.data.frame(stats::setNames(rep(list(character(0)), 6L), names(.qes_schemas$spec_hashes)),
                                    stringsAsFactors = FALSE), "spec_hashes")
  rownames(out) <- NULL
  out
}
