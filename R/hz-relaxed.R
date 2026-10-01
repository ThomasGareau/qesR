# The relaxed layer of the spec (design.md section 5.14, spec 4.4.0,
# schema 4): the columns of qes_decon() (R/decon-relaxed.R).
#
# Relaxed harmonization puts one concept in one column for every study, even
# when the wording or the answer options differ, with coarse common
# categories (education in four groups, income in study-specific thirds,
# interest low, medium or high...). It is not the strict layer: a relaxed
# column is not a target (rule P3), has no crosswalk row and no grade, and
# never claims that two studies' questions are comparable. qes_harmonize()
# and its grades stay the strict layer.
#
# The spec declares the layer in two tables:
#   relaxed.csv       one row per output column of qes_decon(), in output
#                     order (position): its type and level set, its base, the
#                     transform of the base's values, its timing (static: one
#                     value per respondent, repeated on each of their waves;
#                     wave: on the wave that asked it), whether it is kept
#                     under 3 studies (essential), whether it shares the name
#                     of a target or pooled variable it extends (same_as), and
#                     its EN/FR label, relaxation rule and definition;
#   relaxed_maps.csv  one row per study and wave where the column needs a
#                     mapping of its own: the crosswalk row format without
#                     grades (rule, source variable, value map, na_codes,
#                     gate), with override (TRUE: the row replaces the base
#                     for that study and wave; FALSE: it fills a study the
#                     base does not cover), wording and review fields. Its
#                     value maps are in valuemaps.csv under the prefix rx_.
# Bases: target:<t> (the strict target's values), pooled:<p> (a pooled
# variable), pooled:<p>__type (the member each pooled value came from),
# column:<c> (a relaxed column placed earlier) or empty (the column has
# relaxed rows only; transform relaxed_only). Transforms: identity,
# recode:<from>=<to>,... (a <to> of NA:<reason> is a missing value with that
# reason), bands:<cut>,<cut>:<level>,<level>,<level> (a number in level k
# when it is at least the (k-1)th cut and below the kth), affine:<a*x+b>.
#
# The validator rules V-R1 to V-R12 are here (.qes_rx_check(), called by
# .qes_spec_check(); .qes_rx_data_check(), the V-R9 checks on the shipped
# dictionary, called with the data checks).

# The leading columns of qes_decon(): no relaxed column takes their names.
.qes_rx_id_columns <- c("study", "year", "wave", "qes_id", "weight", "weight_var")

# Relaxed columns that may share the name of a strict target without
# extending it (OD-R6): the relaxed religion (five groups) and the strict
# string target religion (each study's own categories, read by get_decon()).
.qes_rx_shadow_ok <- c("religion")

# Level sets whose value maps follow the thirds rule (V-R9): household income
# in thirds of each study's respondents.
.qes_rx_tercile_sets <- "rx_income3"

# Rules a relaxed row may use.
.qes_rx_rules <- c("map", "numeric", "coalesce", "constant", "fn:multiselect", "fn:amount_bands")

# Words that claim comparability, which no relaxed text may use (V-R7).
.qes_rx_claim_en <- "\\b(identical|strictly comparable|fully comparable)\\b"
.qes_rx_claim_fr <- "\\b(identiques?|strictement comparables?|pleinement comparables?)\\b"

# ---- tables ------------------------------------------------------------------------

# The relaxed tables of `spec` (empty data frames of the schema when the spec
# directory has none, as for a schema 2 or 3 spec).
.qes_rx_tables <- function(spec) {
  empty <- function(schema) {
    .qes_apply_schema(as.data.frame(
      stats::setNames(rep(list(character(0)), length(.qes_schemas[[schema]])), names(.qes_schemas[[schema]])),
      stringsAsFactors = FALSE
    ), schema)
  }
  list(
    relaxed = spec$tables$relaxed %||% empty("spec_relaxed"),
    maps = spec$tables$relaxed_maps %||% empty("spec_relaxed_maps"),
    expected = spec$tables$rx_expected %||% empty("spec_rx_expected"),
    hashes = spec$tables$rx_hashes %||% empty("spec_rx_hashes")
  )
}

# The relaxed columns that are not retired, in output order.
.qes_rx_columns <- function(spec) {
  rx <- .qes_rx_tables(spec)$relaxed
  rx <- rx[!rx$status %in% "retired", , drop = FALSE]
  rx <- rx[order(rx$position), , drop = FALSE]
  rownames(rx) <- NULL
  rx
}

# A base as list(kind, name): kind is target, pooled, type (the __type of a
# pooled variable), column or none; NULL when the text is not in the grammar.
.qes_rx_base <- function(text) {
  if (length(text) != 1L || is.na(text) || !nzchar(text)) return(list(kind = "none", name = NA_character_))
  m <- regmatches(text, regexec("^(target|pooled|column):([a-z][a-z0-9_]*)$", text))[[1]]
  if (length(m) == 0L) return(NULL)
  kind <- m[2]
  name <- m[3]
  if (identical(kind, "pooled") && grepl("__type$", name)) {
    return(list(kind = "type", name = sub("__type$", "", name)))
  }
  if (grepl("__", name, fixed = TRUE)) return(NULL)
  list(kind = kind, name = name)
}

# A transform as list(kind, map, cuts, levels, affine); NULL when the text is
# not in the grammar. recode maps level names to level names or to
# "NA:<reason>".
.qes_rx_transform <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) return(NULL)
  if (text %in% c("identity", "relaxed_only")) return(list(kind = text))
  kind <- sub(":.*$", "", text)
  arg <- if (grepl(":", text, fixed = TRUE)) sub("^[^:]*:", "", text) else ""
  if (identical(kind, "affine")) {
    a <- .qes_affine(arg)
    if (is.null(a)) return(NULL)
    return(list(kind = "affine", affine = a))
  }
  if (identical(kind, "recode")) {
    if (!nzchar(arg)) return(NULL)
    parts <- strsplit(arg, ",", fixed = TRUE)[[1]]
    if (!all(grepl("^[^=]+=[^=]+$", parts))) return(NULL)
    keys <- sub("=.*$", "", parts)
    if (anyDuplicated(keys) > 0L) return(NULL)
    return(list(kind = "recode", map = stats::setNames(sub("^[^=]+=", "", parts), keys)))
  }
  if (identical(kind, "bands")) {
    p <- strsplit(arg, ":", fixed = TRUE)[[1]]
    if (length(p) != 2L) return(NULL)
    cuts <- suppressWarnings(as.numeric(strsplit(p[1], ",", fixed = TRUE)[[1]]))
    levels <- strsplit(p[2], ",", fixed = TRUE)[[1]]
    if (length(cuts) == 0L || anyNA(cuts) || is.unsorted(cuts, strictly = TRUE) ||
        length(levels) != length(cuts) + 1L || anyDuplicated(levels) > 0L) {
      return(NULL)
    }
    return(list(kind = "bands", cuts = cuts, levels = levels))
  }
  NULL
}

# Apply transform `tr` to base values `value` (level names or number text)
# and reasons `reason`: list(value, reason).
.qes_rx_apply_transform <- function(value, reason, tr) {
  ok <- !is.na(value)
  if (is.null(tr) || tr$kind %in% c("identity", "relaxed_only") || !any(ok)) {
    return(list(value = value, reason = reason))
  }
  out <- rep(NA_character_, length(value))
  if (identical(tr$kind, "recode")) {
    to <- unname(tr$map[value[ok]])
    na_to <- !is.na(to) & startsWith(to, "NA:")
    v <- ifelse(na_to, NA_character_, to)
    out[ok] <- v
    r_ok <- ifelse(na_to, sub("^NA:", "", to), NA_character_)
    reason[ok] <- r_ok
    # a value with no image (a spec V-R4 rejects) is not left without a reason
    reason[ok][is.na(v) & is.na(r_ok)] <- "unmapped"
  } else if (identical(tr$kind, "bands")) {
    x <- suppressWarnings(as.numeric(value[ok]))
    k <- 1L + vapply(x, function(z) sum(z >= tr$cuts), integer(1))
    out[ok] <- tr$levels[k]
    reason[ok] <- NA_character_
  } else if (identical(tr$kind, "affine")) {
    x <- suppressWarnings(as.numeric(value[ok]))
    out[ok] <- .qes_code_chr(tr$affine[["a"]] * x + tr$affine[["b"]])
    reason[ok] <- NA_character_
  }
  list(value = out, reason = reason)
}

# Does transform `tr` collapse or reshape the base (a recode that merges
# levels or sets some to NA, bands, affine)? Identity does not.
.qes_rx_lossy <- function(tr) {
  if (is.null(tr)) return(FALSE)
  switch(tr$kind,
         recode = anyDuplicated(unname(tr$map)) > 0L || any(startsWith(unname(tr$map), "NA:")),
         bands = TRUE, affine = TRUE, FALSE)
}

# The parsed args of a fn:amount_bands row (R/hz-data.R): list(breaks,
# levels, then_var, then_map).
.qes_hz_amount_args <- function(args) {
  kv <- .qes_parse_kv(args) %||% character(0)
  breaks <- suppressWarnings(as.numeric(strsplit(unname(kv["breaks"]) %|NA|% "", ",", fixed = TRUE)[[1]]))
  levels <- strsplit(unname(kv["levels"]) %|NA|% "", ",", fixed = TRUE)[[1]]
  then <- unname(kv["then"]) %|NA|% ""
  list(breaks = breaks, levels = levels,
       then_var = if (grepl("^[^:,]+:[^:,]+$", then)) sub(":.*$", "", then) else NA_character_,
       then_map = if (grepl("^[^:,]+:[^:,]+$", then)) sub("^[^:]*:", "", then) else NA_character_)
}

# ---- the relaxed rows as a spec ------------------------------------------------------

# `spec` with its targets and crosswalk replaced by the relaxed columns (as
# targets) and the relaxed rows (as crosswalk rows, in the order of
# relaxed_maps.csv): the form .qes_hz_apply_row() and the data checks read.
# The grade fields are placeholders; nothing graded is ever read from them.
.qes_rx_pseudo_spec <- function(spec) {
  t <- .qes_rx_tables(spec)
  rx <- t$relaxed
  rm <- t$maps
  na <- function(n) rep(NA_character_, n)
  n <- nrow(rx)
  targets <- data.frame(
    target = rx$column, family = rx$column, block = rep("socio", n), type = rx$type,
    target_timing = ifelse(rx$timing %in% "static", "static", "any"), jurisdiction = na(n),
    election_ref_rule = rep("none", n), levels_id = rx$levels_id, valid_min = rx$valid_min,
    valid_max = rx$valid_max, anchor_row = na(n), derive_rule = na(n), derive_from = na(n),
    allow_constant = rep(TRUE, n), label_en = rx$label_en, label_fr = rx$label_fr,
    description_en = rx$description_en, description_fr = rx$description_fr, sets = na(n),
    status = rep("experimental", n), replaced_by = na(n), added_in = rx$added_in,
    stringsAsFactors = FALSE
  )
  k <- nrow(rm)
  xw <- data.frame(
    study = rm$study, wave = rm$wave, target = rm$column, rule = rm$rule, source_var = rm$source_var,
    map_id = rm$map_id, args = rm$args, na_codes = rm$na_codes, gate_var = rm$gate_var,
    gate_codes = rm$gate_codes, gate_to = rm$gate_to, primary = rep(TRUE, k),
    grade = rep("approximate", k), grade_reason_en = na(k), grade_reason_fr = na(k),
    instrument = na(k), election_ref = na(k), mode = na(k), dk_offered = na(k),
    levels_offered = na(k), wording_en = rm$wording_en, wording_fr = rm$wording_fr,
    wording_ref = rm$wording_ref, evidence = rm$evidence, notes_en = rm$notes_en,
    notes_fr = rm$notes_fr, reviewed_by = rm$reviewed_by, reviewed_on = rm$reviewed_on,
    review_note = rm$review_note, status = rm$status, stringsAsFactors = FALSE
  )
  out <- spec
  out$tables$targets <- targets
  out$tables$crosswalk <- xw
  out$tables$gates <- .qes_hz_empty_sources()$gates
  out
}

# ---- V-R1 to V-R8, V-R10 to V-R12 -------------------------------------------------------

# The studies a base covers (has a mapped primary row in, or derives from
# rows in): a character vector.
.qes_rx_base_studies <- function(spec, base, covered_columns) {
  xw <- spec$tables$crosswalk
  tg <- spec$tables$targets
  of_target <- function(t, seen = character(0)) {
    if (t %in% seen) return(character(0))
    s <- xw$study[xw$target %in% t & xw$primary %in% TRUE & !is.na(xw$rule) & xw$rule != "none"]
    j <- match(t, tg$target)
    if (!is.na(j) && !is.na(tg$derive_from[j])) {
      s <- c(s, unlist(lapply(.qes_split_list(tg$derive_from[j]), of_target, seen = c(seen, t))))
    }
    unique(s)
  }
  if (is.null(base)) return(character(0))
  switch(
    base$kind,
    target = of_target(base$name),
    pooled = , type = unique(unlist(lapply(.qes_pool_members(spec, base$name)$member, of_target))),
    column = covered_columns[[base$name]] %||% character(0),
    character(0)
  )
}

# The validator rules of the relaxed layer that read no data. `sets` holds
# the expanded level sets, `enum()` the catalog enums and `na_reasons` the
# spec NA vocabulary. Returns a problems table.
.qes_rx_check <- function(spec, sets, enum, study_names, na_reasons) {
  t <- .qes_rx_tables(spec)
  rx <- t$relaxed
  rm <- t$maps
  ex <- t$expected
  hs <- t$hashes
  vm <- spec$tables$valuemaps
  wv <- spec$tables$waves
  tg <- spec$tables$targets
  xw <- spec$tables$crosswalk
  out <- list()
  add <- function(rule, table, row, key, detail, severity = "error") {
    n <- max(length(row), length(key))
    if (length(row) == 0L || length(key) == 0L) return(invisible())
    out[[length(out) + 1L]] <<- .qes_spec_problems(
      rep_len(rule, n), rep_len(severity, n), rep_len(table, n),
      rep_len(as.integer(row), n), rep_len(key, n), rep_len(detail, n)
    )
    invisible()
  }
  has <- function(x) !is.na(x) & nzchar(x)
  if (nrow(rx) == 0L && nrow(rm) == 0L) return(.qes_spec_problems())
  mkey <- paste(rm$study, rm$wave, rm$column, rm$source_var, sep = "/")
  version_ok <- function(x) grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", x)
  pools <- .qes_pool_tables(spec)$pooled

  # ---- V-R1: form, types and keys -----------------------------------------------------
  for (col in c("column", "type", "timing", "label_en", "label_fr", "relax_en", "relax_fr",
                "description_en", "description_fr", "status", "added_in", "transform")) {
    bad <- which(!has(rx[[col]]))
    add("V-R1", "relaxed", bad, ifelse(is.na(rx$column[bad]), "", rx$column[bad]), sprintf("column '%s' is empty", col))
  }
  bad <- which(is.na(rx$position) | is.na(rx$essential) | is.na(rx$same_as))
  add("V-R1", "relaxed", bad, rx$column[bad], "position, essential and same_as are required")
  bad <- which(duplicated(rx$column))
  add("V-R1", "relaxed", bad, rx$column[bad], "duplicate column")
  pos <- rx$position[!is.na(rx$position)]
  if (length(pos) > 0L && !identical(sort(pos), seq_along(pos))) {
    add("V-R1", "relaxed", NA, "position", "positions must be 1, 2, ..., n (a total order)")
  }
  bad <- which(has(rx$type) & !rx$type %in% c("categorical", "ordinal", "numeric"))
  add("V-R1", "relaxed", bad, rx$column[bad], "type must be categorical, ordinal or numeric")
  bad <- which(has(rx$timing) & !rx$timing %in% c("static", "wave"))
  add("V-R1", "relaxed", bad, rx$column[bad], "timing must be static or wave")
  bad <- which(has(rx$status) & !rx$status %in% c(enum("row_status"), "retired"))
  add("V-R1", "relaxed", bad, rx$column[bad], "status must be draft, review, stable or retired")
  bad <- which(has(rx$added_in) & !version_ok(rx$added_in))
  add("V-R1", "relaxed", bad, rx$column[bad], "added_in must be a spec version")
  bad <- which(!is.na(rx$valid_min) & !is.na(rx$valid_max) & rx$valid_min > rx$valid_max)
  add("V-R1", "relaxed", bad, rx$column[bad], "valid_min is greater than valid_max")
  for (col in c("study", "wave", "column", "rule", "source_var", "status", "added_in")) {
    bad <- which(!has(rm[[col]]))
    add("V-R1", "relaxed_maps", bad, mkey[bad], sprintf("column '%s' is empty", col))
  }
  bad <- which(is.na(rm$override))
  add("V-R1", "relaxed_maps", bad, mkey[bad], "override is required (TRUE or FALSE)")
  bad <- which(duplicated(paste(rm$study, rm$wave, rm$column)))
  add("V-R1", "relaxed_maps", bad, mkey[bad], "more than one relaxed row for (study, wave, column)")
  bad <- which(has(rm$status) & !rm$status %in% enum("row_status"))
  add("V-R1", "relaxed_maps", bad, mkey[bad], "status must be draft, review or stable")
  bad <- which(has(rm$added_in) & !version_ok(rm$added_in))
  add("V-R1", "relaxed_maps", bad, mkey[bad], "added_in must be a spec version")
  bad <- which(rm$status %in% "stable" & (!has(rm$reviewed_by) | is.na(rm$reviewed_on)))
  add("V-R1", "relaxed_maps", bad, mkey[bad], "a stable relaxed row needs reviewed_by and reviewed_on")
  bad <- which(!rm$status %in% "draft" & !has(rm$evidence))
  add("V-R1", "relaxed_maps", bad, mkey[bad], "a relaxed row in review or stable needs evidence")

  # ---- V-R2: names ------------------------------------------------------------------------
  bad <- which(has(rx$column) & !grepl(.qes_name_pattern, rx$column))
  add("V-R2", "relaxed", bad, rx$column[bad], sprintf("column name does not match %s", .qes_name_pattern))
  bad <- which(rx$column %in% c(.qes_rx_id_columns, .qes_hz_leading_columns))
  add("V-R2", "relaxed", bad, rx$column[bad], "column name is the name of a leading column")
  bad <- which(rx$column %in% study_names)
  add("V-R2", "relaxed", bad, rx$column[bad], "column name equals a study code")
  bad <- which(grepl("__", rx$column, fixed = TRUE))
  add("V-R2", "relaxed", bad, rx$column[bad], "column name must not contain '__'")
  strict_names <- c(tg$target, pools$pooled)
  for (j in seq_len(nrow(rx))) {
    b <- .qes_rx_base(rx$base[j])
    shadow <- rx$column[j] %in% strict_names
    extends <- !is.null(b) && b$kind %in% c("target", "pooled") && identical(b$name, rx$column[j])
    if (shadow && !isTRUE(rx$same_as[j]) && !rx$column[j] %in% .qes_rx_shadow_ok) {
      add("V-R2", "relaxed", j, rx$column[j], "column name is a target or pooled variable: set same_as = TRUE and base it on that name, or rename it")
    }
    if (isTRUE(rx$same_as[j]) && !extends) {
      add("V-R2", "relaxed", j, rx$column[j], "same_as = TRUE needs base target:<column> or pooled:<column> of the same name")
    }
  }

  # ---- V-R3, V-R4: bases and transforms -------------------------------------------------------
  covered <- list()
  rx_ord <- rx[order(rx$position), , drop = FALSE]
  level_names <- function(id) if (has(id) && !is.null(sets[[id]])) sets[[id]]$name else NULL
  base_def <- function(b) {
    # list(type, levels) of a base
    switch(
      b$kind,
      target = {
        j <- match(b$name, tg$target)
        list(type = tg$type[j], levels = level_names(tg$levels_id[j]))
      },
      pooled = {
        j <- match(b$name, pools$pooled)
        list(type = pools$type[j], levels = level_names(pools$levels_id[j]))
      },
      type = list(type = "categorical", levels = .qes_pool_members(spec, b$name)$type_name),
      column = {
        j <- match(b$name, rx$column)
        list(type = rx$type[j], levels = level_names(rx$levels_id[j]))
      },
      list(type = NA_character_, levels = NULL)
    )
  }
  for (k in seq_len(nrow(rx_ord))) {
    j <- match(rx_ord$column[k], rx$column)
    key <- rx$column[j]
    b <- .qes_rx_base(rx$base[j])
    tr <- .qes_rx_transform(rx$transform[j])
    if (is.null(b)) {
      add("V-R3", "relaxed", j, key, "base must be target:<t>, pooled:<p>, pooled:<p>__type, column:<c> or empty")
      next
    }
    if (identical(b$kind, "target")) {
      i <- match(b$name, tg$target)
      if (is.na(i)) add("V-R3", "relaxed", j, key, sprintf("base target %s is not in targets.csv", b$name))
      else if (tg$status[i] %in% "retired") add("V-R3", "relaxed", j, key, sprintf("base target %s is retired", b$name))
      else if (tg$block[i] %in% c("id", "design")) add("V-R3", "relaxed", j, key, "base target is a leading-column target")
    } else if (b$kind %in% c("pooled", "type")) {
      i <- match(b$name, pools$pooled)
      if (is.na(i)) add("V-R3", "relaxed", j, key, sprintf("base pooled variable %s is not in pooled.csv", b$name))
      else if (pools$status[i] %in% "retired") add("V-R3", "relaxed", j, key, sprintf("base pooled variable %s is retired", b$name))
    } else if (identical(b$kind, "column")) {
      i <- match(b$name, rx$column)
      if (is.na(i) || is.na(rx$position[i]) || is.na(rx$position[j]) || rx$position[i] >= rx$position[j]) {
        add("V-R3", "relaxed", j, key, sprintf("base column %s must be a relaxed column placed earlier", b$name))
      }
    }
    if (is.null(tr)) {
      add("V-R4", "relaxed", j, key, "transform is not identity, recode:, bands:, affine: or relaxed_only")
      next
    }
    if (identical(b$kind, "none") != identical(tr$kind, "relaxed_only")) {
      add("V-R3", "relaxed", j, key, "an empty base goes with transform relaxed_only, and only it")
    }
    cat_col <- rx$type[j] %in% c("categorical", "ordinal")
    if (cat_col && (!has(rx$levels_id[j]) || is.null(sets[[rx$levels_id[j]]]))) {
      add("V-R4", "relaxed", j, key, "a categorical or ordinal column needs a levels_id of levels.csv")
    }
    if (!cat_col && has(rx$levels_id[j])) {
      add("V-R4", "relaxed", j, key, "only categorical and ordinal columns take levels_id")
    }
    if (identical(rx$type[j], "numeric") && (is.na(rx$valid_min[j]) || is.na(rx$valid_max[j]))) {
      add("V-R4", "relaxed", j, key, "a numeric column needs valid_min and valid_max")
    }
    mine <- level_names(rx$levels_id[j])
    bd <- base_def(b)
    base_cat <- bd$type %in% c("categorical", "ordinal")
    if (identical(tr$kind, "identity")) {
      if (base_cat != cat_col) {
        add("V-R4", "relaxed", j, key, "identity needs a base of the same kind (levels or numbers) as the column")
      } else if (cat_col && length(setdiff(bd$levels, mine)) > 0L) {
        add("V-R4", "relaxed", j, key, sprintf("identity: base level(s) %s are not levels of the column", paste(setdiff(bd$levels, mine), collapse = ", ")))
      }
    } else if (identical(tr$kind, "recode")) {
      if (!base_cat || !cat_col) {
        add("V-R4", "relaxed", j, key, "recode needs a categorical base and a categorical column")
      } else {
        miss <- setdiff(bd$levels, names(tr$map))
        if (length(miss) > 0L) add("V-R4", "relaxed", j, key, sprintf("recode is not total: base level(s) %s have no image", paste(miss, collapse = ", ")))
        extra <- setdiff(names(tr$map), bd$levels)
        if (length(extra) > 0L) add("V-R4", "relaxed", j, key, sprintf("recode names %s, not levels of the base", paste(extra, collapse = ", ")))
        to <- unname(tr$map)
        na_to <- sub("^NA:", "", to[startsWith(to, "NA:")])
        bad_to <- setdiff(to[!startsWith(to, "NA:")], mine)
        if (length(bad_to) > 0L) add("V-R4", "relaxed", j, key, sprintf("recode gives %s, not levels of the column", paste(bad_to, collapse = ", ")))
        bad_na <- setdiff(na_to, na_reasons)
        if (length(bad_na) > 0L) add("V-R4", "relaxed", j, key, sprintf("recode reason(s) %s not in the spec NA vocabulary", paste(bad_na, collapse = ", ")))
      }
    } else if (identical(tr$kind, "bands")) {
      if (base_cat || !cat_col) {
        add("V-R4", "relaxed", j, key, "bands needs a numeric base and a categorical column")
      } else if (length(setdiff(tr$levels, mine)) > 0L) {
        add("V-R4", "relaxed", j, key, sprintf("bands gives %s, not levels of the column", paste(setdiff(tr$levels, mine), collapse = ", ")))
      }
    } else if (identical(tr$kind, "affine")) {
      if (base_cat || cat_col) add("V-R4", "relaxed", j, key, "affine needs a numeric base and a numeric column")
    }
    covered[[key]] <- unique(c(.qes_rx_base_studies(spec, b, covered),
                               rm$study[rm$column %in% key]))
  }

  # ---- V-R5: English and French ------------------------------------------------------------------
  pair <- function(table, x, en, fr, keys) {
    bad <- which(has(x[[en]]) != has(x[[fr]]))
    add("V-R5", table, bad, keys[bad], sprintf("%s and %s must both be present or both empty", en, fr))
  }
  pair("relaxed_maps", rm, "notes_en", "notes_fr", mkey)
  bad <- which(!rm$status %in% "draft" & !has(rm$wording_en) & !has(rm$wording_fr) & !has(rm$wording_ref))
  add("V-R5", "relaxed_maps", bad, mkey[bad], "a relaxed row in review or stable needs a wording or wording_ref")
  files <- .qes_catalog()$files
  for (i in which(has(rm$wording_ref))) {
    refs <- .qes_split_list(rm$wording_ref[i])
    bad_form <- refs[!grepl("^[0-9]+:.+$", refs)]
    if (length(bad_form) > 0L) add("V-R5", "relaxed_maps", i, mkey[i], sprintf("wording_ref entry %s is not <file_id>:<ref>", paste(bad_form, collapse = ", ")))
    ids <- unique(sub(":.*$", "", refs[grepl("^[0-9]+:.+$", refs)]))
    unknown <- ids[!ids %in% as.character(files$file_id)]
    if (length(unknown) > 0L) add("V-R5", "relaxed_maps", i, mkey[i], sprintf("wording_ref file(s) %s are not in the catalog", paste(unknown, collapse = ", ")))
  }

  # ---- V-R6: coverage ------------------------------------------------------------------------------
  # a relaxed row whose every code is missing (a decision to leave a study
  # out, such as the education of qes1998) does not cover its study
  all_na_map <- function(id) {
    rows <- vm[vm$map_id %in% id, , drop = FALSE]
    nrow(rows) > 0L && all(is.na(rows$target_code))
  }
  for (j in seq_len(nrow(rx))) {
    if (isTRUE(rx$essential[j]) || rx$status[j] %in% "retired") next
    key <- rx$column[j]
    void <- rm$study[rm$column %in% key & rm$rule %in% "map" & vapply(rm$map_id, all_na_map, logical(1))]
    s <- setdiff(covered[[key]] %||% character(0), setdiff(void, .qes_rx_base_studies(spec, .qes_rx_base(rx$base[j]), covered)))
    if (length(s) < 3L) {
      add("V-R6", "relaxed", j, key, sprintf("covers %d stud%s (%s); a column that is not essential needs 3 or more",
                                              length(s), if (length(s) == 1L) "y" else "ies", paste(s, collapse = ", ")))
    }
  }

  # ---- V-R7: no grade, no claim of comparability -------------------------------------------------
  for (col in c("relax_en", "description_en", "label_en")) {
    bad <- which(has(rx[[col]]) & grepl(.qes_rx_claim_en, rx[[col]], ignore.case = TRUE, perl = TRUE))
    add("V-R7", "relaxed", bad, rx$column[bad], sprintf("%s claims comparability ('identical', 'strictly comparable'): relaxed columns carry no grade", col))
  }
  for (col in c("relax_fr", "description_fr", "label_fr")) {
    bad <- which(has(rx[[col]]) & grepl(.qes_rx_claim_fr, rx[[col]], ignore.case = TRUE, perl = TRUE))
    add("V-R7", "relaxed", bad, rx$column[bad], sprintf("%s claims comparability ('identique', 'strictement comparable'): relaxed columns carry no grade", col))
  }

  # ---- V-R8: relaxed rows ------------------------------------------------------------------------
  all_studies <- unique(c(wv$study, .qes_catalog()$studies$study))
  for (i in seq_len(nrow(rm))) {
    k <- mkey[i]
    j <- match(rm$column[i], rx$column)
    if (is.na(j)) {
      add("V-R8", "relaxed_maps", i, k, "column is not in relaxed.csv")
      next
    }
    if (rx$status[j] %in% "retired") add("V-R8", "relaxed_maps", i, k, "column is retired")
    if (!rm$study[i] %in% all_studies) add("V-R8", "relaxed_maps", i, k, "study is not in the catalog")
    star <- identical(rm$wave[i], .qes_all_waves)
    if (star) {
      if (!(.qes_poll_study(wv, rm$study[i]) || rx$timing[j] %in% "static") || !any(wv$study %in% rm$study[i])) {
        add("V-R8", "relaxed_maps", i, k, "wave * is allowed only in a study whose waves are all poll waves, or for a static column")
      }
    } else if (!paste(rm$study[i], rm$wave[i]) %in% paste(wv$study, wv$wave)) {
      add("V-R8", "relaxed_maps", i, k, "(study, wave) is not in waves.csv")
    }
    rule <- rm$rule[i]
    if (!rule %in% .qes_rx_rules) {
      add("V-R8", "relaxed_maps", i, k, sprintf("rule must be one of %s", paste(.qes_rx_rules, collapse = ", ")))
      next
    }
    base_rule <- if (startsWith(rule, "fn:")) "fn" else rule
    fits <- c(.qes_type_rules[[rx$type[j]]] %||% character(0), "fn")
    if (!base_rule %in% fits) add("V-R8", "relaxed_maps", i, k, sprintf("rule %s does not fit a %s column", rule, rx$type[j]))
    args <- .qes_parse_kv(rm$args[i])
    if (is.null(args)) add("V-R8", "relaxed_maps", i, k, "args must be key=value;key=value with unique keys")
    nac <- .qes_parse_kv(rm$na_codes[i])
    if (is.null(nac)) {
      add("V-R8", "relaxed_maps", i, k, "na_codes must be code=reason;code=reason with unique codes")
    } else {
      bad <- setdiff(unname(nac), na_reasons)
      if (length(bad) > 0L) add("V-R8", "relaxed_maps", i, k, sprintf("na_codes reason(s) %s not in the spec NA vocabulary", paste(bad, collapse = ", ")))
      if (!all(.qes_is_canon_code(names(nac)))) add("V-R8", "relaxed_maps", i, k, "na_codes has a code not in canonical form")
    }
    gate <- c(has(rm$gate_var[i]), has(rm$gate_codes[i]), has(rm$gate_to[i]))
    if (any(gate) && !all(gate)) add("V-R8", "relaxed_maps", i, k, "gate_var, gate_codes and gate_to go together")
    lv_names <- level_names(rx$levels_id[j]) %||% character(0)
    if (all(gate)) {
      to <- .qes_parse_kv(rm$gate_to[i])
      codes <- .qes_split_list(rm$gate_codes[i])
      if (is.null(to) || !setequal(names(to), codes)) add("V-R8", "relaxed_maps", i, k, "gate_to must give one outcome for each gate code")
      bad <- setdiff(unname(to %||% character(0)), c(na_reasons, lv_names))
      if (length(bad) > 0L) add("V-R8", "relaxed_maps", i, k, sprintf("gate_to outcome(s) %s are neither NA reasons nor levels of the column", paste(bad, collapse = ", ")))
    }
    maps <- c(rm$map_id[i])
    if (identical(rule, "coalesce")) {
      then <- .qes_hz_coalesce_then(rm[i, c("rule", "args"), drop = FALSE], 1L)
      if (is.null(then) || nrow(then) == 0L) add("V-R8", "relaxed_maps", i, k, "rule coalesce needs args then=<variable>:<map_id>,...")
      else maps <- c(maps, then$map_id)
    }
    if (identical(rule, "fn:amount_bands")) {
      a <- .qes_hz_amount_args(rm$args[i])
      if (length(a$breaks) == 0L || anyNA(a$breaks) || is.unsorted(a$breaks, strictly = TRUE) ||
          length(a$levels) != length(a$breaks) + 1L || length(setdiff(a$levels, lv_names)) > 0L) {
        add("V-R8", "relaxed_maps", i, k, "fn:amount_bands needs args breaks=<increasing numbers>;levels=<one more level of the column>")
      }
      if (is.na(a$then_var)) add("V-R8", "relaxed_maps", i, k, "fn:amount_bands needs args then=<variable>:<map_id>")
      else maps <- c(maps, a$then_map)
    }
    if (identical(rule, "fn:multiselect")) {
      ms <- .qes_hz_multiselect_args(rm$args[i])
      if (length(ms$var) == 0L || !identical(ms$var[1], rm$source_var[i])) {
        add("V-R8", "relaxed_maps", i, k, "fn:multiselect needs args select=<source_var>:<level>,... starting with the row's source_var")
      }
      bad <- setdiff(ms$level, lv_names)
      if (length(bad) > 0L) add("V-R8", "relaxed_maps", i, k, sprintf("fn:multiselect gives %s, not levels of the column", paste(bad, collapse = ", ")))
    }
    if (identical(rule, "constant")) {
      v <- unname((args %||% character(0))["value"])
      if (length(v) == 0L || is.na(v) || !v %in% lv_names) add("V-R8", "relaxed_maps", i, k, "rule constant needs args value=<level of the column>")
    }
    maps <- maps[has(maps)]
    if (rule %in% c("map", "coalesce") && !has(rm$map_id[i])) add("V-R8", "relaxed_maps", i, k, "rules map and coalesce need a map_id")
    if (!rule %in% c("map", "coalesce") && has(rm$map_id[i])) add("V-R8", "relaxed_maps", i, k, "only rules map and coalesce take a map_id")
    for (id in maps) {
      if (!startsWith(id, "rx_")) add("V-R8", "relaxed_maps", i, k, sprintf("map %s of a relaxed row must be named rx_...", id))
      rows <- which(vm$map_id == id)
      if (length(rows) == 0L) {
        add("V-R8", "relaxed_maps", i, k, sprintf("map %s is not in valuemaps.csv", id))
        next
      }
      set <- sets[[rx$levels_id[j] %|NA|% ""]]
      bad <- rows[!is.na(vm$target_code[rows]) & !vm$target_code[rows] %in% (set$code %||% integer(0))]
      add("V-R8", "valuemaps", bad, paste(vm$map_id[bad], vm$source_code[bad], sep = "/"),
          sprintf("target_code is not a code of the level set of column %s", rx$column[j]))
    }
  }
  # every rx_ map is read by a relaxed row, and no crosswalk row reads one
  rx_maps <- unique(vm$map_id[startsWith(vm$map_id, "rx_")])
  used <- unique(unlist(lapply(seq_len(nrow(rm)), function(i) {
    r <- rm[i, , drop = FALSE]
    then <- if (identical(r$rule, "coalesce")) .qes_hz_coalesce_then(r, 1L)$map_id else NULL
    amt <- if (identical(r$rule, "fn:amount_bands")) .qes_hz_amount_args(r$args)$then_map else NULL
    c(r$map_id, then, amt)
  })))
  add("V-R8", "valuemaps", NA, setdiff(rx_maps, used), "rx_ map is not used by any relaxed row")
  xw_maps <- unique(unlist(lapply(seq_len(nrow(xw)), function(i) .qes_hz_row_maps(xw, i))))
  add("V-R8", "crosswalk", NA, intersect(xw_maps, rx_maps), "a crosswalk row reads an rx_ map, which is for relaxed rows only")

  # ---- V-R10: override against the base --------------------------------------------------------
  for (i in seq_len(nrow(rm))) {
    j <- match(rm$column[i], rx$column)
    if (is.na(j) || is.na(rm$override[i])) next
    b <- .qes_rx_base(rx$base[j])
    base_studies <- .qes_rx_base_studies(spec, b, covered)
    if (identical(b$kind, "column")) {
      # a column base covers the studies of its own base and relaxed rows
      base_studies <- covered[[b$name]] %||% character(0)
    }
    in_base <- rm$study[i] %in% base_studies
    if (isTRUE(rm$override[i]) && !in_base) {
      add("V-R10", "relaxed_maps", i, mkey[i], "override = TRUE, but the base has no row for this study: set override = FALSE (the row fills the study)")
    }
    if (isFALSE(rm$override[i]) && in_base) {
      add("V-R10", "relaxed_maps", i, mkey[i], "override = FALSE, but the base has a row for this study: set override = TRUE (the row replaces it)")
    }
  }

  # ---- V-R11: recorded results (form; their content is checked live) ---------------------------
  if (nrow(ex) > 0L) {
    ekey <- paste(ex$column, ex$study, ex$wave, ex$value, ex$na_reason, sep = "/")
    bad <- which(!has(ex$column) | !has(ex$study) | !has(ex$wave) | is.na(ex$n) | ex$n < 0L | has(ex$value) == has(ex$na_reason))
    add("V-R11", "rx_expected", bad, ekey[bad], "column, study, wave, a count of 0 or more and exactly one of value and na_reason are required")
    bad <- which(!ex$column %in% rx$column)
    add("V-R11", "rx_expected", bad, ekey[bad], "column is not in relaxed.csv")
    bad <- which(duplicated(ekey))
    add("V-R11", "rx_expected", bad, ekey[bad], "duplicate (column, study, wave, value, na_reason)")
  }
  if (nrow(hs) > 0L) {
    hkey <- paste(hs$column, hs$study, sep = "/")
    bad <- which(!has(hs$column) | !has(hs$study) | is.na(hs$n) | !grepl("^[0-9a-f]{32}$", hs$md5))
    add("V-R11", "rx_hashes", bad, hkey[bad], "column, study, a count and an md5 are required")
    bad <- which(!hs$column %in% rx$column)
    add("V-R11", "rx_hashes", bad, hkey[bad], "column is not in relaxed.csv")
    bad <- which(duplicated(hkey))
    add("V-R11", "rx_hashes", bad, hkey[bad], "duplicate (column, study)")
  }

  # ---- V-R12: the legacy renderers never read the relaxed layer ----------------------------------
  lg <- spec$tables$legacy
  if (!is.null(lg) && nrow(lg) > 0L) {
    named <- unique(unlist(lapply(lg$target, .qes_split_list)))
    only_relaxed <- setdiff(intersect(named, rx$column), tg$target)
    add("V-R12", "legacy", NA, only_relaxed, "a legacy column names a relaxed column; the legacy renderers read targets only")
  }

  res <- if (length(out) > 0L) do.call(rbind, out) else .qes_spec_problems()
  rownames(res) <- NULL
  res
}

# ---- V-R9: the relaxed rows on the shipped dictionary ------------------------------------------

# The data checks V-D1 to V-D4 on the relaxed rows (as a pseudo-spec, on the
# shipped dictionary), reported as V-R9 on relaxed_maps.csv; then the checks
# the data checks do not make: the variables of a fn:amount_bands row's
# bracket question are mapped, and the income thirds follow the midpoint
# rule (a bracket goes to the first third when the share of respondents
# below it plus half its own share is under 1/3, to the last when it is over
# 2/3, else to the middle; brackets are never split). The universe check
# V-D7 of gated rows needs the joint counts, which are not shipped for the
# relaxed rows: it runs on the data, in qes_decon() and the live tests.
.qes_rx_data_check <- function(spec, sources) {
  t <- .qes_rx_tables(spec)
  rm <- t$maps
  if (nrow(rm) == 0L) return(.qes_spec_problems())
  ps <- .qes_rx_pseudo_spec(spec)
  src <- sources
  src$gates <- .qes_hz_empty_sources()$gates
  p <- .qes_data_check(ps, src, studies = unique(rm$study), offline_severity = "note", member_counts = FALSE)
  p <- p[p$table %in% "crosswalk" & p$rule %in% c("V-D1", "V-D2", "V-D3", "V-D4"), , drop = FALSE]
  mkey <- paste(rm$study, rm$wave, rm$column, rm$source_var, sep = "/")
  out <- list()
  if (nrow(p) > 0L) {
    out[[1]] <- .qes_spec_problems("V-R9", p$severity, "relaxed_maps", p$row, p$key,
                                   paste0(p$rule, ": ", p$detail))
  }
  add <- function(i, detail, table = "relaxed_maps", key = mkey[i]) {
    out[[length(out) + 1L]] <<- .qes_spec_problems("V-R9", "error", table, i, key, detail)
  }
  vm <- spec$tables$valuemaps
  val <- sources$values
  values_of <- function(study, var) val[val$study == study & val$variable == var, , drop = FALSE]
  rx <- t$relaxed
  for (i in seq_len(nrow(rm))) {
    if (identical(rm$rule[i], "fn:amount_bands")) {
      a <- .qes_hz_amount_args(rm$args[i])
      if (is.na(a$then_var)) next
      v <- values_of(rm$study[i], a$then_var)
      if (nrow(v) == 0L) {
        add(i, sprintf("variable '%s' of args then is not a variable of %s", a$then_var, rm$study[i]))
        next
      }
      obs <- v$value[!is.na(v$n) & v$n > 0L]
      bad <- setdiff(obs, vm$source_code[vm$map_id %in% a$then_map])
      if (length(bad) > 0L) add(i, sprintf("observed code(s) %s of %s are not in map %s", paste(bad, collapse = ", "), a$then_var, a$then_map))
    }
    j <- match(rm$column[i], rx$column)
    if (is.na(j) || !rx$levels_id[j] %in% .qes_rx_tercile_sets || !identical(rm$rule[i], "map")) next
    v <- values_of(rm$study[i], rm$source_var[i])
    m <- vm[vm$map_id %in% rm$map_id[i] & !is.na(vm$target_code), , drop = FALSE]
    if (nrow(m) == 0L || nrow(v) == 0L) next
    num <- suppressWarnings(as.numeric(m$source_code))
    m <- m[order(num), , drop = FALSE]
    n <- v$n[match(m$source_code, v$value)]
    if (anyNA(n)) {
      add(i, "the dictionary has no count for some brackets: the thirds rule cannot be checked")
      next
    }
    share <- n / sum(n)
    mid <- cumsum(share) - share / 2
    want <- ifelse(mid < 1 / 3, 1L, ifelse(mid > 2 / 3, 3L, 2L))
    bad <- m$source_code[want != m$target_code]
    if (length(bad) > 0L) {
      add(i, sprintf("bracket(s) %s do not follow the thirds rule on the dictionary counts (midpoints %s)",
                     paste(bad, collapse = ", "), paste(round(mid[want != m$target_code], 3), collapse = ", ")))
    }
  }
  res <- do.call(rbind, out)
  if (is.null(res)) return(.qes_spec_problems())
  rownames(res) <- NULL
  res
}
