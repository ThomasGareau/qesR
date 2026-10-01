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
# Two builders give one, and every check runs the same way on each:
#   .qes_hz_sources_shipped()  the shipped dictionary with the spec's
#                              gates.csv: tests and qes_spec();
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
# gates.csv also holds the cells of a gated weight, date or string row, for
# the universe check V-D7 only.

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

# The shipped dictionary of the studies whose metadata ships (every study of
# the catalog), with the gates table of `spec`.
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

# Variables of `study` that the spec names: sources and gates of crosswalk
# rows, membership, date, subsample, strata and mode variables of waves.
.qes_hz_spec_vars <- function(spec, study) {
  xw <- spec$tables$crosswalk[spec$tables$crosswalk$study == study, , drop = FALSE]
  wv <- spec$tables$waves[spec$tables$waves$study == study, , drop = FALSE]
  mode_var <- sub("^var:", "", wv$mode[!is.na(wv$mode) & startsWith(wv$mode, "var:")])
  then <- unlist(lapply(which(xw$rule %in% c("coalesce", "fn:multiselect", "fn:amount_bands")),
                        function(i) .qes_hz_row_vars(xw, i)))
  vars <- c(xw$source_var, then, xw$gate_var, wv$member_var, wv$date_var, wv$subsample_var,
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

# Are the cells of crosswalk row `i` counted in gates.csv: a projected row,
# or a gated weight, date or string row (its cells are not projected, but
# they answer the universe check V-D7).
.qes_hz_counted <- function(xw, i) {
  .qes_hz_projected(xw, i) ||
    (xw$rule[i] %in% c("weight", "date", "string", "coalesce") && !is.na(xw$gate_var[i]) && nzchar(xw$gate_var[i]))
}

# The cells of every projectable or gated crosswalk row of `study`
# (.qes_hz_counted()), counted in data frame `d` (the spec_gates table).
.qes_hz_cells_data <- function(spec, d, study) {
  xw <- spec$tables$crosswalk
  wv <- spec$tables$waves
  out <- list()
  seen <- character(0)
  for (i in which(xw$study == study)) {
    if (!.qes_hz_counted(xw, i) || !xw$source_var[i] %in% names(d)) next
    gate <- xw$gate_var[i]
    if (!is.na(gate) && !gate %in% names(d)) next
    # one wave, or every poll wave of the study for wave "*"
    w <- .qes_wave_rows(wv, xw$wave[i], study)
    if (length(w) == 0L || (length(w) > 1L && !identical(xw$wave[i], .qes_all_waves))) next
    key <- paste(xw$wave[i], xw$source_var[i], gate, sep = "\x1f")
    if (key %in% seen) next
    seen <- c(seen, key)
    mem <- .qes_wave_members_rows(wv, w, d)
    if (is.null(mem)) next
    # an empty text is system missing, as in the engine (rule string)
    src <- .canon(d[[xw$source_var[i]]])[mem]
    src[is.na(src) | !nzchar(src)] <- "NA"
    # typed text (rule string without from_label) is counted as one token:
    # the checks need only answered or not, and no typed answer is kept
    args <- .qes_parse_kv(xw$args[i]) %||% character(0)
    if (identical(xw$rule[i], "string") && !identical(unname(args["from_label"]), "TRUE")) {
      na <- names(.qes_parse_kv(xw$na_codes[i]) %||% character(0))
      src[src != "NA" & !src %in% na] <- "<text>"
    }
    g <- if (is.na(gate)) rep(NA_character_, length(src)) else {
      gv <- .canon(d[[gate]])[mem]
      gv[is.na(gv) | !nzchar(gv)] <- "NA"
      gv
    }
    if (length(src) == 0L) next
    k <- paste(ifelse(is.na(g), "", g), src, sep = "\x1f")
    tab <- table(k)
    parts <- strsplit(names(tab), "\x1f", fixed = TRUE)
    gc <- vapply(parts, `[`, character(1), 1L)
    out[[length(out) + 1L]] <- data.frame(
      study = study, wave = xw$wave[i], source_var = xw$source_var[i],
      member_rule = .qes_member_rule_rows(wv, w), gate_var = gate,
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
  w <- .qes_wave_rows(wv, xw$wave[i], study)
  rule <- if (length(w) >= 1L) .qes_member_rule_rows(wv, w) else ""
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
  # code only for numeric columns with at most 50 whole-number codes
  # (R/metadata.R), so an unlisted code would pass as system missing
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

# ---- rule coalesce (schema 3) ------------------------------------------------------
#
# A coalesce row reads one question across several variables: a first
# question and the push asked of those who did not answer it (the pushed
# vote intention of 2022, the pushed 1995 referendum question of 2007 and
# 2008), or the two halves of a split ballot. source_var, map_id, na_codes
# and the gate are those of the first variable; args names the others, in
# order, with their own value maps ("then=<var>:<map_id>,<var>:<map_id>"),
# and the NA reasons that pass to the next variable ("fallthrough=dk,
# inapplicable,sysmis"; default inapplicable and sysmis). While a row's
# outcome is a value, or a reason not in fallthrough, it is final; else the
# next variable is read, and its outcome replaces the row's unless that
# variable did not ask the respondent (inapplicable or system missing). So
# a row keeps the reason of the last variable that asked it.

.qes_coalesce_default_fallthrough <- c("inapplicable", "sysmis")

# The variables and maps of coalesce row `i` after the first: data frame
# var, map_id (zero rows for another rule or an empty `then`); NULL when
# `then` is not in the grammar.
.qes_hz_coalesce_then <- function(xw, i) {
  empty <- data.frame(var = character(0), map_id = character(0), stringsAsFactors = FALSE)
  if (!identical(xw$rule[i], "coalesce")) return(empty)
  args <- .qes_parse_kv(xw$args[i]) %||% character(0)
  then <- unname(args["then"])
  if (length(then) == 0L || is.na(then) || !nzchar(then)) return(empty)
  parts <- strsplit(then, ",", fixed = TRUE)[[1]]
  if (!all(grepl("^[^:,]+:[^:,]+$", parts))) return(NULL)
  data.frame(var = sub(":.*$", "", parts), map_id = sub("^[^:]*:", "", parts), stringsAsFactors = FALSE)
}

# The NA reasons that pass to the next variable in coalesce row `i`.
.qes_hz_coalesce_fallthrough <- function(xw, i) {
  args <- .qes_parse_kv(xw$args[i]) %||% character(0)
  ft <- unname(args["fallthrough"])
  if (length(ft) == 0L || is.na(ft) || !nzchar(ft)) return(.qes_coalesce_default_fallthrough)
  strsplit(ft, ",", fixed = TRUE)[[1]]
}

# Every source variable of crosswalk row `i`, in order (one, except for a
# coalesce row, a fn:multiselect row and a fn:amount_bands row).
.qes_hz_row_vars <- function(xw, i) {
  then <- .qes_hz_coalesce_then(xw, i)
  sel <- if (identical(xw$rule[i], "fn:multiselect")) .qes_hz_multiselect_args(xw$args[i])$var else NULL
  amt <- if (identical(xw$rule[i], "fn:amount_bands")) .qes_hz_amount_args(xw$args[i])$then_var else NULL
  out <- unique(c(xw$source_var[i], if (!is.null(then)) then$var, sel, amt))
  out[!is.na(out)]
}

# ---- registered function fn:multiselect ------------------------------------------------
#
# A select-all-that-apply question stored as one 0/1 variable per option
# (the 2022 mother tongue: cps_lang_1 English, cps_lang_2 French,
# cps_lang_3 another language). args: "select=<var>:<level>,<var>:<level>,..."
# (the first variable is the row's source_var) and "selected=<code>" (the
# code of a ticked option, default 1). A respondent whose ticked options all
# give one level gets that level; options of two or more levels are
# not_mappable (never assigned to one of them, as a reported second mother
# tongue elsewhere); no option ticked is no_answer; every variable system
# missing is sysmis. With "first=TRUE" (relaxed rows, spec 4.4.0), the first
# ticked option in the order of `select` gives the level instead, so that
# options of two levels are assigned by that priority.

# The parsed args of a fn:multiselect row: list(var, level, selected, first).
.qes_hz_multiselect_args <- function(args) {
  kv <- .qes_parse_kv(args) %||% character(0)
  sel <- unname(kv["select"])
  first <- identical(unname(kv["first"]), "TRUE")
  if (length(sel) == 0L || is.na(sel)) {
    return(list(var = character(0), level = character(0), selected = "1", first = first))
  }
  parts <- strsplit(sel, ",", fixed = TRUE)[[1]]
  selected <- unname(kv["selected"])
  list(var = sub(":.*$", "", parts), level = sub("^[^:]*:", "", parts),
       selected = if (length(selected) == 0L || is.na(selected)) "1" else selected, first = first)
}

.qes_hz_fn_multiselect <- function(src, ctx) {
  a <- .qes_hz_multiselect_args(ctx$row$args)
  d <- ctx$data
  n <- nrow(d)
  ticked <- vapply(a$var, function(v) {
    x <- .canon(d[[v]])
    ifelse(is.na(x), NA, x == a$selected)
  }, logical(n))
  ticked <- matrix(ticked, nrow = n)
  all_na <- rowSums(!is.na(ticked)) == 0L
  n_levels <- apply(ticked, 1L, function(r) length(unique(a$level[r %in% TRUE])))
  first <- apply(ticked, 1L, function(r) {
    k <- which(r %in% TRUE)
    if (length(k) == 0L) NA_character_ else a$level[k[1]]
  })
  one <- if (isTRUE(a$first)) n_levels >= 1L else n_levels == 1L
  value <- ifelse(one, first, NA_character_)
  reason <- ifelse(one, NA_character_,
                   ifelse(all_na, "sysmis", ifelse(n_levels == 0L, "no_answer", "not_mappable")))
  list(value = value, na_reason = reason)
}

# ---- registered function fn:amount_bands (relaxed rows, spec 4.4.0) --------------------
#
# An amount asked as a number, with a bracket question for those who gave
# none (the 2022 household income: cps_income, then cps_income2). args:
# "breaks=<b1>,<b2>,..." (increasing), "levels=<l1>,<l2>,..." (one more than
# the breaks) and "then=<variable>:<map_id>" (the bracket question and its
# value map). An amount that is a number and not in the row's na_codes falls
# in level k when it is at least the (k-1)th break and below the kth; the
# codes of na_codes (no amount given) pass to the bracket question, whose
# map gives the level or the reason; a respondent the bracket question did
# not ask (system missing) keeps the amount's reason.

.qes_hz_fn_amount_bands <- function(src, ctx) {
  row <- ctx$row
  a <- .qes_hz_amount_args(row$args)
  d <- ctx$data
  n <- nrow(d)
  code <- .canon(src)
  code[is.na(code)] <- "NA"
  nac <- .qes_parse_kv(row$na_codes) %||% character(0)
  value <- rep(NA_character_, n)
  reason <- rep(NA_character_, n)
  in_nac <- code %in% names(nac)
  reason[in_nac] <- unname(nac[code[in_nac]])
  x <- suppressWarnings(as.numeric(code))
  ok <- !in_nac & !is.na(x) & x > 0
  k <- 1L + vapply(x[ok], function(z) sum(z >= a$breaks), integer(1))
  value[ok] <- a$levels[k]
  reason[!in_nac & code == "NA"] <- "sysmis"
  reason[!in_nac & code != "NA" & !ok] <- "unmapped"
  # the bracket question, for the amounts not given
  vm <- ctx$spec$tables$valuemaps
  vm <- vm[vm$map_id %in% a$then_map, , drop = FALSE]
  tg <- ctx$spec$tables$targets
  set <- .qes_spec_levels(ctx$spec$tables$levels, tg$levels_id[match(row$target, tg$target)])
  r <- list(rule = "map", na_codes = character(0), map_code = vm$source_code,
            map_level = set$name[match(vm$target_code, set$code)], map_reason = vm$na_reason)
  b <- .canon(d[[a$then_var]])
  b[is.na(b)] <- "NA"
  open <- which(in_nac)
  if (length(open) > 0L) {
    ob <- .qes_hz_outcome(r, b[open])
    asked <- !(ob$na_reason %in% .qes_coalesce_default_fallthrough)
    value[open[asked]] <- ob$value[asked]
    reason[open[asked]] <- ob$na_reason[asked]
  }
  reason[!is.na(value)] <- NA_character_
  list(value = value, na_reason = reason)
}

# Every value map of crosswalk row `i` (map_id, then the maps of a
# coalesce row's other variables and of a fn:amount_bands row's bracket
# question).
.qes_hz_row_maps <- function(xw, i) {
  then <- .qes_hz_coalesce_then(xw, i)
  amt <- if (identical(xw$rule[i], "fn:amount_bands")) .qes_hz_amount_args(xw$args[i])$then_map else NULL
  out <- c(xw$map_id[i], if (!is.null(then)) then$map_id, amt)
  out[!is.na(out) & nzchar(out)]
}

# The spec with each coalesce row replaced by one map row per variable (the
# first keeps the row's na_codes and gate; the others have none and are not
# primary): the form the data checks V-D1 to V-D4 and V-D7 read, and the
# code-level crosswalk view.
.qes_hz_expand_coalesce <- function(spec) {
  xw <- spec$tables$crosswalk
  k <- which(xw$rule %in% "coalesce")
  if (length(k) == 0L) return(spec)
  rows <- list()
  for (i in seq_len(nrow(xw))) {
    r <- xw[i, , drop = FALSE]
    if (!i %in% k) {
      rows[[length(rows) + 1L]] <- r
      next
    }
    first <- r
    first$rule <- "map"
    first$args <- NA_character_
    rows[[length(rows) + 1L]] <- first
    then <- .qes_hz_coalesce_then(xw, i)
    for (j in seq_len(nrow(then %||% data.frame()))) {
      o <- r
      o$rule <- "map"
      o$args <- NA_character_
      o$source_var <- then$var[j]
      o$map_id <- then$map_id[j]
      o$na_codes <- NA_character_
      o$gate_var <- NA_character_
      o$gate_codes <- NA_character_
      o$gate_to <- NA_character_
      o$primary <- FALSE
      rows[[length(rows) + 1L]] <- o
    }
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  spec$tables$crosswalk <- out
  spec
}

# The rule of variable `var` with value map `map_id` in coalesce row `i`:
# the row's rule (.qes_hz_row_rule()) with that map, no na_codes and no
# gate (the first variable keeps the row's own).
.qes_hz_part_rule <- function(spec, i, var, map_id) {
  r <- .qes_hz_row_rule(spec, i)
  vm <- spec$tables$valuemaps
  vm <- vm[vm$map_id %in% map_id, , drop = FALSE]
  r$rule <- "map"
  r$source_var <- var
  r$na_codes <- character(0)
  r$map_code <- vm$source_code
  r$map_level <- if (is.null(r$set)) rep(NA_character_, nrow(vm)) else r$set$name[match(vm$target_code, r$set$code)]
  r$map_reason <- vm$na_reason
  r$gate_var <- NA_character_
  r$gate_to <- character(0)
  r
}

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
  if (r$rule %in% c("map", "coalesce")) {
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
# cannot answer (a row without cells in gates.csv). The engine (slice HZ3)
# turns off two checks it does itself or cannot apply: `unmapped_codes =
# FALSE` skips the V-D2 report of observed codes with no outcome (the engine
# reports them under its `unmapped` argument), and `member_counts = FALSE`
# skips V-D8 (data given by the user may hold a subset of the rows). Returns
# a problems table.
.qes_data_check <- function(spec, sources, studies = NULL, data = NULL, offline_severity = "error",
                            unmapped_codes = TRUE, member_counts = TRUE) {
  # a coalesce row is checked variable by variable, as one map row each
  spec <- .qes_hz_expand_coalesce(spec)
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
  # the rows of each (study, variable) of the values table, indexed once
  value_rows <- split(seq_len(nrow(sources$values)), paste(sources$values$study, sources$values$variable, sep = "\x1f"))
  values_of <- function(study, var) {
    rows <- value_rows[[paste(study, var, sep = "\x1f")]] %||% integer(0)
    sources$values[rows, , drop = FALSE]
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
    # the other variables a registered function reads (fn:multiselect)
    for (v in setdiff(.qes_hz_row_vars(xw, i), xw$source_var[i])) {
      if (!exists_in(v, xw$study[i])) {
        add("V-D1", "crosswalk", i, xkey(i), sprintf("variable '%s' of args is not a variable of %s%s", v, xw$study[i], hint(v, xw$study[i])))
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
      if (length(bad) > 0L && isTRUE(unmapped_codes)) {
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
    # the normalized labels of the file and of the value map, once per map
    norm_file <- .qes_norm_label(val$label[match(vm$source_code[rows], val$value)])
    norm_map <- .qes_norm_label(vm$source_label[rows])
    for (k in rows) {
      j <- match(vm$source_code[k], val$value)
      file_label <- if (is.na(j)) NA_character_ else val$label[j]
      # the file has a label, or its hash (a study whose labels cannot ship)
      file_has_label <- !is.na(file_label) || (!is.na(j) && !is.na(val$label_hash[j]))
      # the md5 of the file's label, computed only when a hash is compared
      file_hash <- function() {
        h <- if (is.na(j)) NA_character_ else val$label_hash[j]
        if (is.na(h) && !is.na(file_label)) h <- .qes_md5_text(.qes_norm_label(file_label))
        h
      }
      origin <- vm$source_label_origin[k]
      vkey <- paste(vm$map_id[k], vm$source_code[k], sep = "/")
      if (has(vm$source_label[k])) {
        if (!is.na(file_label)) {
          if (!identical(norm_file[match(k, rows)], norm_map[match(k, rows)])) {
            add("V-D3", "valuemaps", k, vkey, sprintf("%s %s: the file labels code %s '%s', the value map '%s'", xw$study[i], xw$source_var[i], vm$source_code[k], file_label, vm$source_label[k]))
          }
        } else if (origin %in% c("file", "label_donor") && !file_has_label) {
          add("V-D3", "valuemaps", k, vkey, sprintf("%s %s: the value map quotes a file label for code %s, but the file has none", xw$study[i], xw$source_var[i], vm$source_code[k]))
        }
      } else if (has(vm$source_label_hash[k])) {
        file_hash <- file_hash()
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
    if (!.qes_hz_counted(xw, i)) next
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
  for (w in which(wv$study %in% studies & isTRUE(member_counts))) {
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

  # the poll waves of a study are disjoint samples: with data, no row is a
  # member of two of them (a row of wave "*" takes the respondent's poll)
  if (!is.null(data) && isTRUE(member_counts)) {
    for (study in intersect(names(data), studies)) {
      if (!.qes_poll_study(wv, study)) next
      idx <- which(wv$study == study)
      m <- vapply(idx, function(w) .qes_wave_members(wv, w, data[[study]]) %||% rep(FALSE, nrow(data[[study]])),
                  logical(nrow(data[[study]])))
      both <- which(rowSums(matrix(m, nrow = nrow(data[[study]]))) > 1L)
      if (length(both) > 0L) {
        add("V-D8", "waves", idx[1], study, sprintf("%d rows are members of two or more poll waves, e.g. rows %s",
                                                   length(both), paste(utils::head(both, 5L), collapse = ", ")))
      }
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
