# The legacy renderer (design.md section 5.12, slice HZ6).
#
# get_qes_master() and get_decon() are built by the harmonization engine:
# qes_harmonize() gives, study by study, the targets their columns need, and
# the table legacy.csv of the spec (inst/extdata/harmonize/legacy.csv, part
# of the spec's content hash, checked by the validator as V-S18) says how
# each legacy column is rendered from them. One row per (profile, column,
# studies):
#
#   profile     "master" (get_qes_master()) or "decon" (get_decon());
#   position    the column's position in the output (appended columns keep
#               theirs: they are never removed);
#   column      the legacy column name;
#   studies     empty for the column's default row, or a ";"-list of study
#               codes the row applies to instead (qes_demo takes the rows
#               of qes2014, whose variables it copies);
#   target      the target(s) the render reads, ";"-separated: for most
#               renders the first one the study has a row for, for
#               age_years and age_bands all of them;
#   render      how the column is made:
#                 catalog:<field>   study or name_en of the catalog;
#                 lead:<column>     a leading or weight column of the
#                                   harmonized data (year, family,
#                                   study_design, waves, subsample,
#                                   source_row, weight_pre, weight_post);
#                 id                qes_id without its "<study>:" prefix,
#                                   "<study>_<row>" where the id is the row;
#                 raw:<variable>    a variable of the study's file, as read
#                                   by get_qes() (weights, dates and ids that
#                                   are not harmonized): numbers, or text
#                                   (date-times as "YYYY-MM-DD HH:MM:SS");
#                 as_is             the target's value (number or text);
#                 level_en          the English label of the level;
#                 factor            a factor of the English labels, with the
#                                   target's levels (as qes_harmonize());
#                 recode:<level>=<text>;...  fixed text per level;
#                 legacy_party      the party labels of qesR 0.4.4;
#                 int01             yes = 1, no = 0, other levels NA;
#                 scale10           four-point interest levels as 10, 7, 3
#                                   and 0 (OD7), 0-10 values as they are;
#                 age_years         the age target, else the study's year
#                                   minus the year of birth (qesR 0.4.4);
#                 age_bands         six bands from age_years, else the
#                                   age_group6 levels, else age_group3;
#                 constant:<text>   the same text for every row;
#                 na_column         NA (no valid source);
#                 timing:<column>   the target_timing of the target that
#                                   filled <column> in the study;
#                 item:<column>     the name of that target;
#   type        character, numeric, integer or factor;
#   flag        "approximate" for columns that mix instruments;
#   cause       the finding or decision behind an NA column; a study's
#               row of cause legacy_frozen keeps the column as in qesR
#               0.7.1 although the study's question is harmonized since
#               (the legacy freeze, spec 4.3.0): its targets are read as
#               if the study had no row for them (NA, reason no_source);
#   definition, note  the legacy_column_map attribute (English).
#
# The engine runs with include_draft = FALSE (since spec 4.0.0): only the
# crosswalk rows signed off by a reviewer (status stable) are applied. A
# column whose question is in a row still in review is NA with reason
# not_reviewed in attr(, "legacy_na_columns"), its cause not_signed_off and
# its basis the row's review_note (why it is held); a render never falls
# through to a later target when an earlier one has a row in review. Every
# value keeps the engine's semantics (a code the spec does not map is NA,
# never passed through); unmapped codes warn (unmapped = "warn").

.qes_legacy_types <- c("character", "numeric", "integer", "factor")
.qes_legacy_lead <- c("year", "family", "study_design", "waves", "subsample", "source_row",
                      "weight_pre", "weight_post")
.qes_legacy_catalog <- c("study", "name_en")
.qes_legacy_target_renders <- c("as_is", "level_en", "factor", "recode", "legacy_party", "int01",
                                "scale10", "age_years", "age_bands")
.qes_legacy_plain_renders <- c("catalog", "lead", "id", "raw", "constant", "na_column", "timing", "item")
# renders that combine all their targets (the others use the first one the
# study has)
.qes_legacy_combining <- c("age_years", "age_bands")

# The party labels of qesR 0.4.4 (provincial and federal), by level name.
.qes_legacy_party_labels <- c(
  PLQ = "PLQ", PQ = "PQ", CAQ = "CAQ", QS = "QS", PVQ = "PVQ", PCQ = "PCQ", ON = "ON", ADQ = "ADQ",
  LPC = "Liberal", CPC = "Conservative", NDP = "NDP", BQ = "Bloc Quebecois", GPC = "Green", PPC = "PPC",
  other = "Other party", no_party = "Did not vote / None", none = "Did not vote / None"
)
# OD7: the four-point interest levels on the legacy 0-10 column.
.qes_legacy_scale10 <- c(very = 10, quite = 7, hardly = 3, not_at_all = 0)
# The legacy bands of age_group.
.qes_legacy_age6 <- c(a18_24 = "18-24", a25_34 = "25-34", a35_44 = "35-44", a45_54 = "45-54",
                      a55_64 = "55-64", a65_plus = "65+")
.qes_legacy_age3 <- c(a18_34 = "18-34", a35_54 = "35-54", a55_plus = "55+")

# ---- the table ------------------------------------------------------------------

# The render of a row as list(kind, arg): "recode:a=b" -> ("recode", "a=b").
.qes_legacy_parse_render <- function(render) {
  kind <- sub(":.*$", "", render)
  arg <- if (grepl(":", render, fixed = TRUE)) sub("^[^:]*:", "", render) else NA_character_
  list(kind = kind, arg = arg)
}

# The fixed texts of a recode render: named character vector level -> text,
# or NULL when the argument does not parse.
.qes_legacy_recode_map <- function(arg) {
  if (is.na(arg) || !nzchar(arg)) return(NULL)
  parts <- strsplit(arg, ";", fixed = TRUE)[[1]]
  kv <- strsplit(parts, "=", fixed = TRUE)
  if (!all(lengths(kv) == 2L)) return(NULL)
  keys <- vapply(kv, `[`, character(1), 1L)
  if (anyDuplicated(keys)) return(NULL)
  stats::setNames(vapply(kv, `[`, character(1), 2L), keys)
}

# V-S18: the legacy renderer table against the targets, level sets and
# catalog. Returns a problems table.
.qes_legacy_check <- function(lg, tg, sets, study_codes, target_set) {
  out <- list()
  add <- function(row, key, detail) {
    if (length(row) == 0L) return(invisible())
    out[[length(out) + 1L]] <<- .qes_spec_problems(
      rep("V-S18", length(row)), rep("error", length(row)), rep("legacy", length(row)),
      as.integer(row), rep_len(key, length(row)), rep_len(detail, length(row))
    )
    invisible()
  }
  has <- function(x) !is.na(x) & nzchar(x)
  key <- paste(lg$profile, lg$column, ifelse(has(lg$studies), lg$studies, "*"), sep = "/")
  bad <- which(!lg$profile %in% c("master", "decon"))
  add(bad, key[bad], "profile must be master or decon")
  bad <- which(!lg$type %in% .qes_legacy_types)
  add(bad, key[bad], sprintf("type must be one of %s", paste(.qes_legacy_types, collapse = ", ")))
  bad <- which(has(lg$flag) & !lg$flag %in% "approximate")
  add(bad, key[bad], "flag must be empty or approximate")
  for (i in seq_len(nrow(lg))) {
    r <- .qes_legacy_parse_render(lg$render[i])
    targets <- .qes_split_list(lg$target[i])
    unknown <- setdiff(targets, tg$target)
    if (length(unknown) > 0L) {
      add(i, key[i], sprintf("unknown target(s) %s", paste(unknown, collapse = ", ")))
      next
    }
    if (lg$cause[i] %in% "legacy_frozen" && (!has(lg$studies[i]) || !r$kind %in% .qes_legacy_target_renders ||
                                              r$kind %in% .qes_legacy_combining)) {
      add(i, key[i], "cause legacy_frozen needs a study's row with a single-target render")
    }
    if (r$kind %in% .qes_legacy_target_renders) {
      if (length(targets) == 0L) add(i, key[i], sprintf("render %s needs a target", r$kind))
    } else if (r$kind %in% .qes_legacy_plain_renders) {
      if (length(targets) > 0L) add(i, key[i], sprintf("render %s takes no target", r$kind))
    } else {
      add(i, key[i], sprintf("unknown render %s", lg$render[i]))
      next
    }
    types <- tg$type[match(targets, tg$target)]
    levels_of <- function(t) {
      s <- target_set(t)
      if (is.null(s)) character(0) else s$name
    }
    ok <- switch(
      r$kind,
      catalog = r$arg %in% .qes_legacy_catalog,
      lead = r$arg %in% .qes_legacy_lead,
      raw = has(r$arg),
      constant = has(r$arg),
      id = , na_column = is.na(r$arg),
      # the column it reads must come earlier: the renderer fills the
      # columns in position order
      timing = , item = {
        src <- lg$profile == lg$profile[i] & lg$column %in% r$arg
        any(src) && all(lg$position[src] < lg$position[i])
      },
      as_is = all(types %in% c("numeric", "string")),
      level_en = , factor = all(types %in% c("categorical", "ordinal")),
      recode = {
        m <- .qes_legacy_recode_map(r$arg)
        !is.null(m) && all(types %in% c("categorical", "ordinal")) &&
          all(unlist(lapply(targets, levels_of)) %in% names(m))
      },
      legacy_party = all(unlist(lapply(targets, levels_of)) %in% names(.qes_legacy_party_labels)),
      int01 = all(vapply(targets, function(t) all(c("yes", "no") %in% levels_of(t)), logical(1))),
      scale10 = all(vapply(seq_along(targets), function(k) {
        identical(types[k], "numeric") || setequal(levels_of(targets[k]), names(.qes_legacy_scale10))
      }, logical(1))),
      age_years = all(targets %in% c("age", "birth_year")),
      age_bands = all(targets %in% c("age", "birth_year", "age_group6", "age_group3")),
      FALSE
    )
    if (!isTRUE(ok)) {
      add(i, key[i], sprintf("render %s does not fit its argument or target(s) %s", lg$render[i],
                             paste(targets, collapse = ";")))
    }
    bad_s <- setdiff(.qes_split_list(lg$studies[i]), study_codes)
    if (length(bad_s) > 0L) add(i, key[i], sprintf("unknown study code(s) %s", paste(bad_s, collapse = ", ")))
  }
  for (p in unique(lg$profile)) {
    rows <- which(lg$profile == p)
    cols <- unique(lg$column[rows])
    for (col in cols) {
      k <- rows[lg$column[rows] == col]
      if (sum(!has(lg$studies[k])) != 1L) {
        add(k[1], key[k[1]], "a column needs exactly one default row (studies empty)")
      }
      # get_decon() builds one study at a time, so its columns may take a
      # type per study (as in qesR 0.4.4); the stacked master may not
      if (length(unique(lg$position[k])) != 1L || (p != "decon" && length(unique(lg$type[k])) != 1L)) {
        add(k[1], key[k[1]], "the rows of a column must share its position (and, in the master, its type)")
      }
      s <- unlist(lapply(lg$studies[k], .qes_split_list))
      if (anyDuplicated(s)) add(k[1], key[k[1]], "a study has two rows for the column")
    }
    pos <- tapply(lg$position[rows], lg$column[rows], function(x) x[1])
    if (anyDuplicated(pos) || !setequal(pos, seq_along(pos))) {
      add(rows[1], p, "positions must number the columns 1, 2, ... without gaps or repeats")
    }
  }
  if (length(out) == 0L) .qes_spec_problems() else do.call(rbind, out)
}

# The legacy table of the shipped spec for one profile.
.qes_legacy_table <- function(profile, spec = NULL) {
  sp <- spec %||% .qes_spec_get(NULL, "error")
  lg <- sp$tables$legacy
  lg <- lg[lg$profile == profile, , drop = FALSE]
  lg[order(lg$position), , drop = FALSE]
}

# The legacy columns of a profile, in order, with their types.
.qes_legacy_columns <- function(profile, spec = NULL) {
  lg <- .qes_legacy_table(profile, spec)
  lg <- lg[!duplicated(lg$column), , drop = FALSE]
  stats::setNames(lg$type, lg$column)
}

# The row of each column that applies to `study` (its own row, else the
# default one): a data frame in column order.
.qes_legacy_rows <- function(lg, study) {
  key <- .qes_hz_spec_study(study)
  cols <- unique(lg$column)
  rows <- lapply(cols, function(col) {
    k <- which(lg$column == col)
    own <- k[vapply(lg$studies[k], function(s) key %in% .qes_split_list(s), logical(1))]
    lg[if (length(own) > 0L) own[1] else k[is.na(lg$studies[k]) | !nzchar(lg$studies[k])][1], , drop = FALSE]
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# Every target a profile reads, in the order of the spec's targets.
.qes_legacy_targets <- function(lg, spec) {
  t <- unique(unlist(lapply(lg$target, .qes_split_list)))
  tg <- spec$tables$targets$target
  tg[tg %in% t]
}

# ---- rendering ------------------------------------------------------------------

# A column of `h` (harmonized data of one study, values = "code") as its
# plain values.
.qes_legacy_value <- function(h, t) {
  x <- h[[t]]
  attributes(x) <- NULL
  x
}

# The target a row uses in the study: the first of its targets whose cell
# was applied (NA when none was).
.qes_legacy_pick <- function(targets, cell) {
  # the first target with a signed-off row, or with a row in review (then
  # NA: a later target is not a stand-in for a question not signed off)
  held <- .qes_legacy_unreviewed(targets, cell)
  used <- c(cell$target[cell$included %in% TRUE], held)
  hit <- targets[targets %in% used]
  if (length(hit) == 0L) return(NA_character_)
  if (hit[1] %in% cell$target[cell$included %in% TRUE]) hit[1] else NA_character_
}

# The recommended weights of the waves of `study` that fill `col`
# (weight_pre or weight_post) whose registry status is needs_review: they
# are registered but not applied until they are reviewed. A study that
# stands in for another (qes_demo) has the other's weights.
.qes_legacy_pending_weight <- function(spec, study, col) {
  if (study %in% names(.qes_hz_stand_ins)) study <- .qes_hz_stand_ins[[study]]
  wv <- spec$tables$waves
  wt <- spec$tables$weights
  wv <- wv[wv$study %in% study, , drop = FALSE]
  waves <- wv$wave[vapply(wv$wave_timing, function(tm) col %in% .qes_hz_weight_columns(tm), logical(1))]
  if (length(waves) == 0L) return(character(0))
  k <- wt$study %in% study & wt$recommended %in% TRUE & wt$status %in% "needs_review" &
    (wt$wave %in% waves | wt$wave %in% .qes_all_waves)
  unique(wt$weight_var[k])
}

# The targets of `targets` whose row in the study is in review (not applied).
.qes_legacy_unreviewed <- function(targets, cell) {
  if (is.null(cell$excluded)) return(character(0))
  targets[targets %in% cell$target[cell$excluded %in% "not_reviewed"]]
}

# Age in years (render age_years): the age target, else year minus the
# year of birth, within 13 to 120 as in qesR 0.4.4.
.qes_legacy_age_years <- function(h, targets) {
  n <- nrow(h)
  age <- if ("age" %in% targets && "age" %in% names(h)) as.numeric(.qes_legacy_value(h, "age")) else rep(NA_real_, n)
  age[!is.finite(age) | age < 13 | age > 120] <- NA_real_
  if ("birth_year" %in% targets && "birth_year" %in% names(h)) {
    calc <- as.numeric(h$year) - as.numeric(.qes_legacy_value(h, "birth_year"))
    calc[!is.finite(calc) | calc < 0 | calc > 120] <- NA_real_
    fill <- is.na(age) & !is.na(calc)
    age[fill] <- calc[fill]
  }
  age
}

# Age bands (render age_bands): from the age where known (under 18: NA),
# else the study's six bands, else its three bands.
.qes_legacy_age_bands <- function(h, targets) {
  age <- .qes_legacy_age_years(h, targets)
  out <- as.character(cut(age, c(18, 25, 35, 45, 55, 65, Inf), right = FALSE, labels = unname(.qes_legacy_age6)))
  for (t in c("age_group6", "age_group3")) {
    if (!t %in% targets || !t %in% names(h)) next
    v <- .qes_legacy_value(h, t)
    lab <- if (t == "age_group6") .qes_legacy_age6 else .qes_legacy_age3
    fill <- is.na(out) & is.na(age) & !is.na(v)
    out[fill] <- unname(lab[v[fill]])
  }
  out
}

# One column of one study, rendered from its row `r` of legacy.csv.
.qes_legacy_render_column <- function(r, h, cell, raw, spec, filled) {
  n <- nrow(h)
  p <- .qes_legacy_parse_render(r$render)
  targets <- .qes_split_list(r$target)
  study <- as.character(h$study[1])
  combining <- p$kind %in% .qes_legacy_combining
  # the legacy freeze: a row of cause legacy_frozen reads no target
  frozen <- identical(r$cause, "legacy_frozen")
  t <- if (length(targets) > 0L && !combining && !frozen) .qes_legacy_pick(targets, cell) else NA_character_
  v <- if (!is.na(t)) .qes_legacy_value(h, t) else rep(NA_character_, n)
  value <- switch(
    p$kind,
    catalog = {
      v <- .qes_legacy_view(.qes_study_row(study, demo = TRUE))
      rep(if (identical(p$arg, "study")) v$qes_survey_code else v[[p$arg]], n)
    },
    lead = .qes_legacy_value(h, p$arg),
    id = {
      key <- sub("^[^:]*:", "", as.character(h$qes_id))
      ids <- .qes_split_list(.qes_default_data_file(study, demo = TRUE)$id_vars)
      if (length(ids) == 0L || identical(ids, ".row")) paste0(study, "_", key) else key
    },
    raw = {
      x <- raw[[p$arg]]
      if (is.null(x)) rep(NA, n) else if (inherits(x, "POSIXt")) {
        format(x, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      } else if (identical(r$type, "numeric")) {
        suppressWarnings(as.numeric(.qes_plain(x)))
      } else {
        .canon(x)
      }
    },
    constant = rep(p$arg, n),
    na_column = rep(NA, n),
    timing = , item = {
      src <- filled[[p$arg]]
      val <- if (is.null(src) || is.na(src)) NA_character_ else if (p$kind == "item") src else
        spec$tables$targets$target_timing[match(src, spec$tables$targets$target)]
      rep(val, n)
    },
    as_is = v,
    level_en = , factor = {
      if (is.na(t)) rep(NA_character_, n) else {
        f <- .qes_hz_encode(v, t, spec, "factor", "en")
        attr(f, "label") <- NULL
        if (p$kind == "factor") f else as.character(f)
      }
    },
    recode = unname(.qes_legacy_recode_map(p$arg)[v]),
    legacy_party = unname(.qes_legacy_party_labels[v]),
    int01 = ifelse(v %in% "yes", 1, ifelse(v %in% "no", 0, NA_real_)),
    scale10 = {
      if (is.na(t)) rep(NA_real_, n) else if (identical(spec$tables$targets$type[match(t, spec$tables$targets$target)], "numeric")) {
        as.numeric(v)
      } else {
        unname(.qes_legacy_scale10[v])
      }
    },
    age_years = .qes_legacy_age_years(h, targets),
    age_bands = .qes_legacy_age_bands(h, targets)
  )
  if (p$kind == "factor" && is.na(t)) {
    # no row in the study: an empty factor with the target's levels
    value <- .qes_hz_encode(rep(NA_character_, n), targets[1], spec, "factor", "en")
    attr(value, "label") <- NULL
  }
  value <- switch(
    r$type,
    character = as.character(value),
    numeric = as.numeric(value),
    integer = as.integer(value),
    factor = if (is.factor(value)) value else factor(value)
  )
  used <- if (combining) {
    hit <- targets[targets %in% cell$target[cell$included %in% TRUE]]
    if (length(hit) > 0L && any(!is.na(value))) paste(hit, collapse = ";") else NA_character_
  } else t
  list(value = value, target = if (any(!is.na(value))) used else NA_character_, kind = p$kind,
       raw = if (p$kind == "raw") p$arg else NA_character_)
}

# The legacy frame of one harmonized study: list(data, source_map, na_rows).
.qes_legacy_render_study <- function(h, raw, lg, spec) {
  study <- as.character(h$study[1])
  rows <- .qes_legacy_rows(lg, study)
  cell <- attr(attr(h, "qes_provenance", exact = TRUE), "cell")
  prov <- attr(h, "qes_provenance", exact = TRUE)
  cols <- list()
  filled <- list()
  map <- list()
  na_rows <- list()
  n <- nrow(h)
  srow <- .qes_legacy_view(.qes_study_row(study, demo = TRUE))
  for (k in seq_len(nrow(rows))) {
    r <- rows[k, , drop = FALSE]
    res <- .qes_legacy_render_column(r, h, cell, raw, spec, filled)
    cols[[r$column]] <- res$value
    filled[[r$column]] <- res$target
    targets <- .qes_split_list(res$target)
    ci <- cell[match(targets, cell$target), , drop = FALSE]
    # a raw variable the file lacks is no source (reason no_source below)
    src_var <- if (identical(res$kind, "raw")) {
      if (!is.na(res$raw) && res$raw %in% names(raw)) res$raw else NA_character_
    } else if (nrow(ci) > 0L) {
      paste(ci$source_var, collapse = ";")
    } else {
      NA_character_
    }
    map[[k]] <- data.frame(
      qes_code = study, qes_year = as.character(srow$year), qes_name_en = srow$name_en,
      harmonized_variable = r$column,
      source_variable = src_var,
      target = if (length(targets) > 0L) paste(targets, collapse = ";") else NA_character_,
      map_id = if (nrow(ci) > 0L && any(!is.na(ci$map_id))) paste(stats::na.omit(ci$map_id), collapse = ";") else NA_character_,
      grade = if (nrow(ci) > 0L) paste(ci$grade, collapse = ";") else NA_character_,
      status = if (nrow(ci) > 0L) paste(ci$status, collapse = ";") else {
        held <- cell[match(.qes_legacy_unreviewed(.qes_split_list(r$target), cell), cell$target), , drop = FALSE]
        if (nrow(held) > 0L) paste(held$status, collapse = ";") else NA_character_
      },
      render = r$render,
      file_md5 = if (is.null(prov)) NA_character_ else prov$md5_observed[1],
      spec_version = attr(h, "qes_spec", exact = TRUE)$version,
      stringsAsFactors = FALSE
    )
    if (all(is.na(res$value)) && n > 0L) {
      # no_source: no question applied, a raw variable the file lacks, or a
      # weight the spec has not registered for the study
      arg <- .qes_legacy_parse_render(r$render)$arg
      no_raw <- res$kind == "raw" && !arg %in% names(raw)
      var_col <- paste0(arg, "_var")
      no_weight <- res$kind == "lead" && grepl("^weight_", arg) && var_col %in% names(h) && all(is.na(h[[var_col]]))
      held <- .qes_legacy_unreviewed(.qes_split_list(r$target), cell)
      # a recommended weight that is registered but needs review is not
      # applied (qes_harmonize() leaves it NA) until it is accepted
      pending <- if (no_weight) .qes_legacy_pending_weight(spec, study, arg) else character(0)
      reason <- if (res$kind == "na_column") "na_column" else if (identical(r$cause, "legacy_frozen")) "no_source" else
        if (length(pending) > 0L) "not_reviewed" else
        if (no_raw || no_weight) "no_source" else
        if (is.na(res$target) && length(held) > 0L) "not_reviewed" else if (is.na(res$target) && !is.na(r$target) &&
        !any(.qes_split_list(r$target) %in% cell$target[cell$included %in% TRUE])) "no_source" else "all_missing"
      cause <- r$cause
      basis <- r$note %|NA|% r$definition
      if (length(pending) > 0L) {
        cause <- "weight_needs_review"
        basis <- sprintf(paste("The spec's recommended weight of this wave (%s) needs review: it is registered",
                               "but not applied until it is accepted (status needs_review in",
                               "qes_spec(\"spec\")$tables$weights, whose source_ref says what is known of it).",
                               "The study's answers do not depend on it."),
                         paste(pending, collapse = ", "))
      } else if (reason == "not_reviewed") {
        # why the row is held, from the crosswalk (review_note)
        cause <- "not_signed_off"
        xw <- spec$tables$crosswalk
        spec_study <- if (study %in% names(.qes_hz_stand_ins)) .qes_hz_stand_ins[[study]] else study
        k <- which(xw$study == spec_study & xw$target %in% held & xw$primary %in% TRUE & xw$status != "stable")
        notes <- unique(stats::na.omit(xw$review_note[k]))
        basis <- if (length(notes) > 0L) paste(notes, collapse = " ") else
          sprintf("The crosswalk row of %s is not signed off by a reviewer yet.", paste(held, collapse = ", "))
      }
      na_rows[[length(na_rows) + 1L]] <- data.frame(
        column = r$column, study = study, reason = reason, n_cells = as.integer(n),
        cause = cause, basis = basis, stringsAsFactors = FALSE
      )
    }
  }
  data <- structure(cols, class = "data.frame", row.names = c(NA_integer_, -n))
  list(
    data = data,
    source_map = do.call(rbind, map),
    na_rows = if (length(na_rows) > 0L) do.call(rbind, na_rows) else NULL,
    filled = filled
  )
}

# Harmonize one study for a legacy profile: the harmonized data (values
# "code", with every target the profile reads) and the study's file as
# get_qes() reads it (for the raw renders). The engine's own notices are
# muffled: the legacy builders have theirs.
.qes_legacy_harmonize <- function(study, lg, spec, quiet) {
  targets <- .qes_legacy_targets(lg, spec)
  # read the file once: the engine takes this frame, and the raw renders
  # (weights, dates) read their variables from it
  d <- .qes_read(study, quiet = quiet)
  .qes_hz_preread$frames <- stats::setNames(list(d), study)
  # legacy mode: rows added after spec 4.2.0 and not reviewed yet, and the
  # derived cells (spec 4.3.0), do not reach the legacy columns
  .qes_hz_preread$legacy <- TRUE
  on.exit({
    .qes_hz_preread$frames <- NULL
    .qes_hz_preread$legacy <- NULL
  }, add = TRUE)
  muffle <- c("qesR_message_approximate_cells", "qesR_message_structural_zeros", "qesR_message_unreviewed_cells",
              "qesR_message_unreviewed_skipped", "qesR_message_weight_review", "qesR_message_weight_timing")
  # the columns left NA by rows not signed off are listed, with the reason
  # not_reviewed, in legacy_na_columns, and announced by the builder
  h <- withCallingHandlers(
    qes_harmonize(study, targets = targets, layout = "respondent", values = "code", missing = "reasons",
                  min_grade = "approximate", weights = "raw", unmapped = "warn", on_fail = "stop",
                  include_draft = FALSE, lang = "en", quiet = quiet),
    message = function(m) if (inherits(m, muffle)) invokeRestart("muffleMessage"),
    qesR_warning_all_unreviewed = function(w) invokeRestart("muffleWarning")
  )
  raw_vars <- unique(stats::na.omit(vapply(lg$render, function(x) {
    p <- .qes_legacy_parse_render(x)
    if (p$kind == "raw") p$arg else NA_character_
  }, character(1))))
  raw <- NULL
  if (length(raw_vars) > 0L) raw <- d[intersect(raw_vars, names(d))]
  list(h = h, raw = raw)
}

# Build a legacy profile for `studies`: list(data, source_map, na_rows,
# provenance, failed, failed_conditions, loaded, filled). A study that
# cannot be read or harmonized is skipped (with a message) and recorded;
# with skip = FALSE (get_decon(), one study) its error is raised as is.
.qes_legacy_build <- function(profile, studies, quiet, skip = TRUE) {
  spec <- .qes_spec_get(NULL, "error")
  lg <- .qes_legacy_table(profile, spec)
  columns <- names(.qes_legacy_columns(profile, spec))
  parts <- list()
  failed <- character(0)
  failed_conditions <- list()
  for (study in studies) {
    res <- if (skip) {
      tryCatch(.qes_legacy_harmonize(study, lg, spec, quiet), error = function(e) e)
    } else {
      .qes_legacy_harmonize(study, lg, spec, quiet)
    }
    if (inherits(res, "error")) {
      # returned text, so rendered in English whatever the session language
      reason <- gsub("\n", " ", .qes_condition_text(res, lang = "en"))
      failed <- c(failed, sprintf("%s: %s", study, reason))
      failed_conditions[[study]] <- res
      .qes_inform("master_skip", class = "qesR_message_download", args = list(.qes_q(study)),
                  data = list(study = study, error = res), quiet = quiet)
      next
    }
    part <- .qes_legacy_render_study(res$h, res$raw, lg, spec)
    prov <- attr(res$h, "qes_provenance", exact = TRUE)
    # every row of the file, and only those (design rule P5)
    if (!is.null(prov) && !identical(as.integer(nrow(part$data)), as.integer(prov$n_rows[1]))) {
      stop(sprintf("qesR internal error: the legacy rows of '%s' differ from its file.", study), call. = FALSE)
    }
    part$provenance <- prov
    part$spec <- attr(res$h, "qes_spec", exact = TRUE)
    parts[[study]] <- part
    .qes_inform("master_rows_loaded", class = "qesR_message_download", args = list(study, nrow(part$data)),
                data = list(study = study), quiet = quiet)
    # the columns of rows not signed off (a weight that needs review is not
    # announced: legacy_na_columns gives it, with the cause weight_needs_review)
    held <- part$na_rows$column[part$na_rows$reason %in% "not_reviewed" & part$na_rows$cause %in% "not_signed_off"]
    if (length(held) > 0L) {
      .qes_inform("legacy_unreviewed", class = "qesR_message_values_changed",
                  args = list(study, length(held), paste(held, collapse = ", ")),
                  data = list(study = study, columns = held), quiet = quiet)
    }
  }
  bind <- function(field) {
    x <- lapply(unname(parts), `[[`, field)
    x <- x[!vapply(x, is.null, logical(1))]
    if (length(x) == 0L) return(NULL)
    out <- do.call(rbind, x)
    rownames(out) <- NULL
    out
  }
  data <- if (length(parts) > 0L) {
    # factor columns keep the target's levels in every study
    d <- lapply(columns, function(col) {
      vals <- lapply(parts, function(p) p$data[[col]])
      if (all(vapply(vals, is.factor, logical(1)))) {
        lev <- unique(unlist(lapply(vals, levels)))
        ord <- all(vapply(vals, is.ordered, logical(1)))
        factor(unlist(lapply(vals, as.character), use.names = FALSE), levels = lev, ordered = ord)
      } else {
        unlist(vals, use.names = FALSE)
      }
    })
    structure(stats::setNames(d, columns), class = "data.frame",
              row.names = c(NA_integer_, -sum(vapply(parts, function(p) nrow(p$data), integer(1)))))
  } else NULL
  provs <- lapply(unname(parts), `[[`, "provenance")
  provs <- provs[!vapply(provs, is.null, logical(1))]
  prov <- NULL
  if (length(provs) > 0L) {
    prov <- do.call(rbind, provs)
    rownames(prov) <- NULL
    cells <- do.call(rbind, lapply(provs, attr, "cell"))
    rownames(cells) <- NULL
    attr(prov, "cell") <- cells
    # one spec row per study built (one qes_harmonize() call each), as
    # rbind() of harmonized results gives
    specs <- lapply(provs, attr, "spec", exact = TRUE)
    specs <- specs[!vapply(specs, is.null, logical(1))]
    if (length(specs) > 0L) {
      spec_rows <- do.call(rbind, specs)
      rownames(spec_rows) <- NULL
      attr(prov, "spec") <- spec_rows
    }
  }
  list(
    data = data, source_map = bind("source_map"), na_rows = bind("na_rows"), provenance = prov,
    spec = if (length(parts) > 0L) parts[[1]]$spec else NULL,
    failed = failed, failed_conditions = failed_conditions, loaded = names(parts),
    filled = lapply(parts, `[[`, "filled")
  )
}

# ---- attributes -----------------------------------------------------------------

# legacy_column_map: one row per column of the profile: its target(s) and
# render, definition, the studies whose values differ from qesR 0.4.4
# (inst/extdata/legacy/changes.csv, written by data-raw/compare_legacy.R),
# flag and note.
.qes_legacy_column_map <- function(profile = "master") {
  lg <- .qes_legacy_table(profile)
  first <- lg[!duplicated(lg$column), , drop = FALSE]
  ch <- .qes_legacy_changes()
  ch <- ch[ch$profile == profile, , drop = FALSE]
  target <- vapply(first$column, function(col) {
    t <- unique(unlist(lapply(lg$target[lg$column == col], .qes_split_list)))
    if (length(t) == 0L) NA_character_ else paste(t, collapse = ";")
  }, character(1))
  changed <- vapply(first$column, function(col) {
    s <- unique(ch$study[ch$column == col])
    if (length(s) == 0L) NA_character_ else paste(s, collapse = ";")
  }, character(1))
  out <- data.frame(
    column = first$column,
    target = unname(target),
    definition = first$definition,
    studies_changed = unname(changed),
    flag = first$flag,
    note = first$note,
    render = vapply(first$column, function(col) paste(unique(lg$render[lg$column == col]), collapse = " | "), character(1),
                    USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

.qes_legacy_env <- new.env(parent = emptyenv())

# A table of inst/extdata/legacy/: removed.csv (the 70 columns qesR 0.4.4
# stacked by name, [A:H1]) or changes.csv (profile, column, study, cause:
# the studies whose values in a column differ from qesR 0.4.4).
.qes_legacy_file <- function(name) {
  if (is.null(.qes_legacy_env[[name]])) {
    path <- system.file("extdata", "legacy", paste0(name, ".csv"), package = "qesR", mustWork = TRUE)
    .qes_legacy_env[[name]] <- .qes_read_csv(path, paste0("legacy_", name))
  }
  .qes_legacy_env[[name]]
}
.qes_legacy_changes <- function() .qes_legacy_file("changes")
.qes_legacy_removed <- function() .qes_legacy_file("removed")

# The legacy builders' record of the spec: that of the harmonized data, plus
# the renderer.
.qes_legacy_spec <- function(spec_attr) {
  c(spec_attr %||% list(version = NA_character_, hash = NA_character_, custom = FALSE, engine = NA_character_),
    list(renderer = "legacy.csv"))
}

# Once-per-session notices of the legacy builders (design.md sections 5.12
# and 7). They are shown whatever `quiet` says, like the assignment notice,
# and only once per session.
.qes_legacy_notice <- function(fn) {
  # one key for both builders (the note is shown once per session), with the
  # text of the builder that shows it: its columns and its attributes
  if (.qes_once_first("values_changed")) {
    id <- if (identical(fn, "get_decon")) "legacy_values_changed_decon" else "legacy_values_changed"
    .qes_inform(id, class = "qesR_message_values_changed", data = list(fn = fn))
  }
  if (.qes_once_first(paste0("legacy_columns:", fn))) {
    if (identical(fn, "get_qes_master")) {
      .qes_inform(
        "legacy_master_columns",
        class = "qesR_message_legacy_columns",
        args = list(nrow(.qes_legacy_removed())),
        data = list(fn = fn)
      )
    } else {
      .qes_inform("legacy_decon_columns", class = "qesR_message_legacy_columns", data = list(fn = fn))
    }
  }
  invisible()
}
