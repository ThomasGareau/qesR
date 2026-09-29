# Pooled variables (design.md sections 5.2 and 5.6, spec 4.3.0).
#
# A pooled variable is one output column that pools several targets of the
# spec, its members: the provincial vote choice pools the reported vote, the
# pushed vote intention and the vote intention; support for sovereignty
# pools the referendum wordings; interest in politics pools the interest
# scales on 0-1. The members stay targets (rule P3: one target, one
# stimulus); the pooled column is a view of their cells, never a target:
# it has no crosswalk row of its own, and `<pooled>__type` says which member
# each value comes from.
#
# The spec declares them in two tables (schema 3):
#   pooled.csv          one row per pooled variable: its type and level set
#                       (or 0-1 range), anchor member, EN/FR label and
#                       definition, sets, status;
#   pooled_members.csv  one row per member: its type name (the value of
#                       <pooled>__type), precedence (1 wins), whether it is
#                       used by default, the transform of its values into
#                       the pooled variable's levels or scale, and a grade
#                       cap for lossy transforms.
# Transforms: "identity" (the member's levels are the pooled variable's, or
# a subset: party_qc of party_qc_intent), "recode:<level>=<level>,..." (a
# collapse into the pooled levels), "score:<level>=<number>,..." (an
# ordinal member scored on a numeric scale) and "affine:<a*x+b>" (a numeric
# member rescaled; the affine grammar of the crosswalk). A recode that
# merges levels and every score transform lose information: the validator
# (V-F6) requires grade_cap = approximate for them.
#
# How a row gets its value (.qes_pool_rows()): the members of the requested
# types are tried in order of precedence. The first member whose cell has a
# value, or an NA reason that is an answer (a terminal reason: the
# respondent was asked, and declined, did not know, did not vote...), sets
# the row. A member that did not ask the respondent (a fall-through reason:
# not in the wave, not asked, not reviewed, below the grade, system missing,
# not mappable, ...) passes to the next. When every member falls through,
# the row is NA with the reason of the first usable member that has a row
# in the study and wave, else of the first member that has a row (so a pool
# whose members are all unreviewed or below the grade still says
# not_reviewed or below_grade). Grades are never upgraded: <pooled>__grade is the member
# row's grade, capped by grade_cap.
#
# Layouts. In the long layout (one row per respondent and wave) the rule
# runs on each row, whose members' values sit on the wave that asked them.
# In the respondent layout (one row per respondent) the pooled value of a
# study comes from one wave: that of the first member (by precedence) the
# study applies; members of the study's other waves are left out, so that
# one weight column fits the study's values.

# NA reasons that pass to the next member: the member did not ask the
# respondent, or its answer cannot be used. Every other reason (dk,
# refused, not_voted, ...) is an answer and stops the search.
.qes_pool_fallthrough <- c("not_in_wave", "not_asked", "inapplicable", "below_grade", "not_reviewed",
                           "sysmis", "not_mappable", "unmapped")

# The suffixes of a pooled variable's companion columns.
.qes_pool_companions <- c("__type", "__grade", "__item")

# ---- spec tables ---------------------------------------------------------------------

# The pooled tables of `spec` (empty data frames of the schema when the spec
# directory has none, as for a schema 2 spec).
.qes_pool_tables <- function(spec) {
  empty <- function(schema) {
    .qes_apply_schema(as.data.frame(
      stats::setNames(rep(list(character(0)), length(.qes_schemas[[schema]])), names(.qes_schemas[[schema]])),
      stringsAsFactors = FALSE
    ), schema)
  }
  list(
    pooled = spec$tables$pooled %||% empty("spec_pooled"),
    members = spec$tables$pooled_members %||% empty("spec_pooled_members")
  )
}

# The pooled variables that are not retired, in the order of pooled.csv.
.qes_pool_names <- function(spec, live = TRUE) {
  p <- .qes_pool_tables(spec)$pooled
  if (isTRUE(live)) p$pooled[!p$status %in% "retired"] else p$pooled
}

# The members of pooled variable `pool`, in order of precedence.
.qes_pool_members <- function(spec, pool) {
  m <- .qes_pool_tables(spec)$members
  m <- m[m$pooled %in% pool, , drop = FALSE]
  m <- m[order(m$precedence), , drop = FALSE]
  rownames(m) <- NULL
  m
}

# A transform as list(kind, map, affine): kind is identity, recode, score
# or affine; `map` a named character vector (recode: level -> level, score:
# level -> number text); NULL when the text is not in the grammar.
.qes_pool_transform <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text) || !nzchar(text)) return(NULL)
  if (identical(text, "identity")) return(list(kind = "identity", map = NULL, affine = NULL))
  kind <- sub(":.*$", "", text)
  arg <- if (grepl(":", text, fixed = TRUE)) sub("^[^:]*:", "", text) else ""
  if (identical(kind, "affine")) {
    a <- .qes_affine(arg)
    if (is.null(a)) return(NULL)
    return(list(kind = "affine", map = NULL, affine = a))
  }
  if (!kind %in% c("recode", "score") || !nzchar(arg)) return(NULL)
  parts <- strsplit(arg, ",", fixed = TRUE)[[1]]
  if (!all(grepl("^[^=]+=[^=]+$", parts))) return(NULL)
  keys <- sub("=.*$", "", parts)
  vals <- sub("^[^=]+=", "", parts)
  if (anyDuplicated(keys) > 0L) return(NULL)
  if (identical(kind, "score") && anyNA(suppressWarnings(as.numeric(vals)))) return(NULL)
  list(kind = kind, map = stats::setNames(vals, keys), affine = NULL)
}

# Apply transform `tr` to member values `value` (level names or number text):
# the pooled variable's values as text (NA where the member value is NA or
# has no image, which V-F5 rules out in a valid spec).
.qes_pool_apply_transform <- function(value, tr) {
  out <- rep(NA_character_, length(value))
  ok <- !is.na(value)
  if (!any(ok)) return(out)
  out[ok] <- switch(
    tr$kind,
    identity = value[ok],
    recode = , score = unname(tr$map[value[ok]]),
    affine = {
      x <- suppressWarnings(as.numeric(value[ok]))
      .qes_code_chr(tr$affine[["a"]] * x + tr$affine[["b"]])
    }
  )
  out
}

# Is recode/score transform `tr` lossy (it merges levels, or scores an
# ordinal scale)? Identity and affine transforms are not.
.qes_pool_lossy <- function(tr) {
  if (is.null(tr)) return(FALSE)
  switch(tr$kind, identity = FALSE, affine = FALSE, score = TRUE, recode = anyDuplicated(unname(tr$map)) > 0L)
}

# The worse of two grades (NA when either is not a grade).
.qes_pool_cap <- function(grade, cap) {
  g <- match(grade, .qes_hz_grades)
  k <- match(cap, .qes_hz_grades)
  k[is.na(k)] <- 1L
  out <- .qes_hz_grades[pmax(g, k)]
  out[is.na(g)] <- NA_character_
  out
}

# ---- arguments ------------------------------------------------------------------------

# The pooled variables named by `targets` (directly or through a set), in
# the order of pooled.csv.
.qes_pool_resolve <- function(targets, spec) {
  p <- .qes_pool_tables(spec)$pooled
  if (nrow(p) == 0L) return(character(0))
  live <- !p$status %in% "retired"
  sets <- lapply(p$sets, .qes_split_list)
  hit <- rep(FALSE, nrow(p))
  for (t in unique(targets)) {
    hit <- hit | p$pooled == t | (live & vapply(sets, function(s) t %in% s, logical(1)))
  }
  p$pooled[hit]
}

# The members of each pooled variable of `pools` that `types` selects: a
# named list, pool -> member targets in order of precedence. `types` is
# NULL (the default members of each pool) or a named list, pool -> type
# names (a member's target name is accepted too); the order of the names
# given is ignored (precedence comes from the spec).
.qes_pool_resolve_types <- function(types, pools, spec) {
  if (!is.null(types)) {
    ok <- is.list(types) && length(types) > 0L && !is.null(names(types)) && all(nzchar(names(types))) &&
      !anyDuplicated(names(types)) &&
      all(vapply(types, function(v) is.character(v) && length(v) > 0L && !anyNA(v), logical(1)))
    if (!ok) {
      .qes_abort("input_types", class = "qesR_error_input", data = list(arg = "types", value = types))
    }
    unknown <- setdiff(names(types), pools)
    if (length(unknown) > 0L) {
      .qes_abort("input_types_pool", class = "qesR_error_input",
                 args = list(.qes_q(unknown), if (length(pools) > 0L) .qes_q(pools) else "-"),
                 data = list(arg = "types", value = unknown, suggestions = pools))
    }
  }
  out <- list()
  for (p in pools) {
    m <- .qes_pool_members(spec, p)
    want <- types[[p]]
    if (is.null(want)) {
      sel <- m$default %in% TRUE
    } else {
      bad <- setdiff(want, c(m$type_name, m$member))
      if (length(bad) > 0L) {
        .qes_abort("input_types_unknown", class = "qesR_error_input",
                   args = list(.qes_q(bad), .qes_q(p), .qes_q(m$type_name)),
                   data = list(arg = "types", value = bad, suggestions = m$type_name))
      }
      sel <- m$type_name %in% want | m$member %in% want
    }
    out[[p]] <- m$member[sel]
  }
  out
}

# ---- the pooled cells of one study ---------------------------------------------------------

# The members of pool `pool` (selected, in precedence) that study part
# `part` uses, with their capped grade, and whether each is usable (its
# cell was applied and its capped grade is at least min_grade). In the
# respondent layout, only the members of one wave (see the header).
.qes_pool_study_members <- function(part, pool, selected, ctx) {
  m <- .qes_pool_members(ctx$spec, pool)
  m <- m[m$member %in% selected, , drop = FALSE]
  cell <- part$cells
  k <- match(m$member, cell$target)
  m$wave <- cell$wave[k]
  m$grade <- .qes_pool_cap(cell$grade[k], m$grade_cap)
  m$included <- cell$included[k] %in% TRUE
  m$has_row <- !is.na(k) & !cell$excluded[k] %in% c("no_row", "not_in_data")
  rank <- match(m$grade, .qes_hz_grades)
  m$usable <- m$included & !is.na(rank) & rank <= match(ctx$min_grade, .qes_hz_grades)
  m$item <- unname(part$cell_item[m$member])
  m$chosen <- TRUE
  if (identical(ctx$layout, "respondent") && any(m$usable)) {
    w <- m$wave[which(m$usable)[1]]
    m$chosen <- m$wave %in% c(w, .qes_all_waves) | identical(w, .qes_all_waves) | !m$has_row
  }
  m
}

# The pooled variable `pool` over the output rows `rows` of study `part`:
# list(value, reason, type, grade, item, src), each a character vector.
# `member_rows(t)` gives member t's list(value, reason, src) on those rows.
.qes_pool_rows <- function(part, pool, selected, rows, member_rows, ctx) {
  n <- length(rows$row)
  value <- rep(NA_character_, n)
  reason <- rep(NA_character_, n)
  type <- rep(NA_character_, n)
  grade <- rep(NA_character_, n)
  item <- rep(NA_character_, n)
  src <- rep(NA_character_, n)
  if (n == 0L) {
    return(list(value = value, reason = reason, type = type, grade = grade, item = item, src = src))
  }
  m <- .qes_pool_study_members(part, pool, selected, ctx)
  m <- m[m$chosen, , drop = FALSE]
  done <- rep(FALSE, n)
  # the first usable member with a row in each output row's wave, else the
  # first member with a row (the reason of a row every member falls through)
  first_with_row <- rep(NA_integer_, n)
  first_reason <- rep(NA_character_, n)
  first_usable <- rep(NA_integer_, n)
  first_usable_reason <- rep(NA_character_, n)
  any_not_in_wave <- rep(FALSE, n)
  for (j in seq_len(nrow(m))) {
    x <- member_rows(m$member[j])
    r <- x$reason
    if (m$has_row[j] && !m$usable[j] && m$included[j]) {
      # applied, but its capped grade is below min_grade
      r[!is.na(x$value) | !r %in% "not_in_wave"] <- "below_grade"
      x$value[] <- NA_character_
    }
    has_value <- !is.na(x$value)
    terminal <- !has_value & !is.na(r) & !r %in% .qes_pool_fallthrough
    take <- !done & (has_value | terminal)
    if (any(take)) {
      tr <- .qes_pool_transform(m$transform[j])
      value[take] <- .qes_pool_apply_transform(x$value[take], tr)
      reason[take] <- ifelse(has_value[take], NA_character_, r[take])
      type[take] <- m$type_name[j]
      grade[take] <- m$grade[j]
      item[take] <- m$item[j]
      src[take] <- x$src[take]
      done <- done | take
    }
    in_wave <- m$has_row[j] & !r %in% c("not_in_wave", "not_asked")
    fill <- !done & is.na(first_with_row) & in_wave
    first_with_row[fill] <- j
    first_reason[fill] <- r[fill]
    fill_u <- !done & is.na(first_usable) & in_wave & m$usable[j]
    first_usable[fill_u] <- j
    first_usable_reason[fill_u] <- r[fill_u]
    any_not_in_wave <- any_not_in_wave | r %in% "not_in_wave"
  }
  rest <- !done
  if (any(rest)) {
    k <- ifelse(is.na(first_usable), first_with_row, first_usable)
    k_reason <- ifelse(is.na(first_usable), first_reason, first_usable_reason)
    hit <- rest & !is.na(k)
    reason[hit] <- k_reason[hit]
    type[hit] <- m$type_name[k[hit]]
    grade[hit] <- m$grade[k[hit]]
    item[hit] <- m$item[k[hit]]
    miss <- rest & is.na(k)
    reason[miss] <- ifelse(any_not_in_wave[miss], "not_in_wave", "not_asked")
  }
  # a value that is missing after the transform (a spec V-F5 rejects) is not
  # left without a reason
  bad <- is.na(value) & is.na(reason)
  if (any(bad)) {
    stop(sprintf("qesR internal error: the pooled variable %s left a missing value without a reason.", pool),
         call. = FALSE)
  }
  list(value = value, reason = reason, type = type, grade = grade, item = item, src = src)
}

# ---- output ---------------------------------------------------------------------------

# Encode the values of pooled variable `pool` (level names or number text),
# as .qes_hz_encode() does for a target.
.qes_pool_encode <- function(value, pool, spec, values, lang) {
  p <- .qes_pool_tables(spec)$pooled
  j <- match(pool, p$pooled)
  .qes_hz_encode_def(value, p$type[j], p$levels_id[j], p[[paste0("label_", lang)]][j], spec, values, lang)
}

# One row of pooled provenance per study, pool and member type used: the
# member, its precedence, wave and item, the counts of values, of answers
# that are NA (terminal reasons) and of rows it passed on (fall-through),
# and its capped grade.
.qes_pool_provenance <- function(study, pool, res, part, selected, ctx) {
  m <- .qes_pool_study_members(part, pool, selected, ctx)
  rows <- lapply(seq_len(nrow(m)), function(j) {
    used <- res$type %in% m$type_name[j]
    data.frame(
      study = study, pooled = pool, type = m$type_name[j], member = m$member[j],
      precedence = m$precedence[j], wave = m$wave[j], item = m$item[j],
      grade = m$grade[j], used_in_layout = m$chosen[j], included = m$included[j] & m$usable[j],
      n_value = sum(used & !is.na(res$value)),
      n_answer_na = sum(used & is.na(res$value) & !res$reason %in% .qes_pool_fallthrough),
      n_fallthrough = sum(used & is.na(res$value) & res$reason %in% .qes_pool_fallthrough),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

# An empty pooled provenance table.
.qes_pool_provenance_empty <- function() {
  data.frame(study = character(0), pooled = character(0), type = character(0), member = character(0),
             precedence = integer(0), wave = character(0), item = character(0), grade = character(0),
             used_in_layout = logical(0), included = logical(0), n_value = integer(0),
             n_answer_na = integer(0), n_fallthrough = integer(0), stringsAsFactors = FALSE)
}

# The weight guide rows of the pooled variables (the wave each study's
# pooled values come from in the respondent layout, and its weight column).
.qes_pool_weight_guide <- function(parts, ctx) {
  rows <- list()
  tg <- ctx$spec$tables$targets
  for (part in parts) {
    for (p in names(ctx$pools)) {
      m <- .qes_pool_study_members(part, p, ctx$pools[[p]], c(ctx[setdiff(names(ctx), "layout")], layout = "respondent"))
      u <- which(m$usable)
      wave <- NA_character_
      timing <- NA_character_
      column <- NA_character_
      var <- NA_character_
      status <- NA_character_
      if (length(u) > 0L) {
        wave <- m$wave[u[1]]
        timing <- tg$target_timing[match(m$member[u[1]], tg$target)]
        ws <- .qes_wave_rows(part$wv, wave)
        if (length(ws) > 1L && .qes_poll_study(part$wv, part$study)) ws <- ws[1]
        one <- function(x) {
          x <- unique(x)
          if (length(x) == 1L) x else NA_character_
        }
        if (length(ws) > 0L) {
          column <- if (identical(ctx$layout, "long")) "weight" else one(vapply(ws, function(w) {
            cols <- .qes_hz_weight_columns(part$wv$wave_timing[w])
            if (length(cols) == 2L) {
              if (timing %in% "pre") "weight_pre" else "weight_post"
            } else if (length(cols) == 1L) cols else NA_character_
          }, character(1)))
          var <- one(vapply(ws, function(k) part$weights[[k]]$var %||% NA_character_, character(1)))
          status <- one(vapply(ws, function(k) part$weights[[k]]$status %||% NA_character_, character(1)))
        }
      }
      rows[[length(rows) + 1L]] <- data.frame(
        target = p, study = part$study, wave = wave, target_timing = timing,
        weight_column = column, weight_var = var, weight_status = status, stringsAsFactors = FALSE
      )
    }
  }
  if (length(rows) == 0L) return(NULL)
  do.call(rbind, rows)
}

# The types each study used, per pool, as text for the message and the
# print header: "vote_choice: recall (qes2012, qes2014), intention_push
# (qes_crop_2007_2010)".
.qes_pool_summary <- function(prov) {
  if (!is.data.frame(prov) || nrow(prov) == 0L) return(character(0))
  used <- prov[prov$n_value > 0L, , drop = FALSE]
  if (nrow(used) == 0L) return(character(0))
  vapply(unique(used$pooled), function(p) {
    u <- used[used$pooled == p, , drop = FALSE]
    types <- unique(u$type[order(u$precedence)])
    paste0(p, ": ", paste(vapply(types, function(t) {
      sprintf("%s (%s)", t, paste(unique(u$study[u$type == t]), collapse = ", "))
    }, character(1)), collapse = ", "))
  }, character(1), USE.NAMES = FALSE)
}

# ---- lineage ----------------------------------------------------------------------------

# Party lineages (design.md section 5.4, OD11): the ADQ merged into the CAQ
# on 2012-01-21, Option nationale into Quebec solidaire in December 2017.
# The harmonized levels keep these parties apart; a lineage view joins them
# for time series.
.qes_lineages <- list(
  adq_caq = c(ADQ = "ADQ_CAQ", CAQ = "ADQ_CAQ"),
  on_qs = c(ON = "QS")
)
.qes_lineage_labels <- list(
  ADQ_CAQ = c(en = "ADQ/CAQ", fr = "ADQ/CAQ")
)

#' Join the parties of one lineage (ADQ and CAQ) in harmonized data
#'
#' `qes_party_lineage()` adds, for each party column of harmonized data, a
#' column that joins the parties of a lineage: by default the Action
#' democratique du Quebec (ADQ) and the Coalition avenir Quebec (CAQ), into
#' which the ADQ merged on 2012-01-21, as one level `"ADQ/CAQ"`. It is a
#' view for time series of the Quebec parties, such as vote choice from 1998
#' to 2022: the harmonized columns keep the ADQ and the CAQ apart (they are
#' different parties, and no study offers both), and so do the targets and
#' pooled variables of the spec.
#'
#' The lineage is a comparison across parties, not across questions, so the
#' new column's grade (`<col>_lineage__grade`) is `approximate` for every
#' row fielded before the merger (`year` before 2012: the ADQ era, the CROP
#' polls of 2007-2010 included); elsewhere it is the grade of the column's
#' own cell (`<col>__grade` for a pooled variable, else the grade of the
#' study's cell in `qes_provenance(x, level = "cell")`).
#'
#' @section En français:
#' `qes_party_lineage()` ajoute, pour chaque colonne de partis de données
#' harmonisées, une colonne qui réunit les partis d'une même filiation : par
#' défaut l'Action démocratique du Québec (ADQ) et la Coalition avenir
#' Québec (CAQ), avec laquelle l'ADQ a fusionné le 21 janvier 2012, en un
#' seul niveau « ADQ/CAQ ». C'est une vue pour les séries chronologiques ;
#' les colonnes harmonisées gardent l'ADQ et la CAQ distinctes. Le niveau de
#' comparabilité de la nouvelle colonne est `approximate` pour les lignes
#' recueillies avant la fusion (`year` avant 2012).
#'
#' @param x Harmonized data returned by [qes_harmonize()].
#' @param cols The party columns to join (targets or pooled variables whose
#'   levels are Quebec parties). `NULL` (default) means every such column of
#'   `x`.
#' @param lineage The lineages to apply: `"adq_caq"` (default, ADQ and CAQ
#'   as `"ADQ/CAQ"`) and, optionally, `"on_qs"` (Option nationale as Quebec
#'   solidaire, which it joined in December 2017).
#'
#' @return `x` with, for each column `<col>` of `cols`, the columns
#'   `<col>_lineage` (a factor, or the ASCII code `"ADQ_CAQ"` when `x` was
#'   built with `values = "code"`; `"labelled"` data get codes as text) and
#'   `<col>_lineage__grade`, placed at the end.
#' @family harmonization
#' @seealso [qes_harmonize()] and its `vote_choice` pooled variable.
#' @examples
#' h <- qes_harmonize("qes_demo", targets = "vote_choice", quiet = TRUE)
#' h <- qes_party_lineage(h)
#' table(h$vote_choice_lineage, useNA = "ifany")
#' @export
qes_party_lineage <- function(x, cols = NULL, lineage = "adq_caq") {
  if (!inherits(x, "qes_harmonized") || !is.data.frame(x)) {
    .qes_abort("input_design_x", class = "qesR_error_input", data = list(arg = "x", value = NULL))
  }
  if (!is.character(lineage) || length(lineage) == 0L || anyNA(lineage) || !all(lineage %in% names(.qes_lineages))) {
    .qes_abort("input_lineage", class = "qesR_error_input", args = list(.qes_q(names(.qes_lineages))),
               data = list(arg = "lineage", value = lineage))
  }
  meta <- attr(x, "qes_spec", exact = TRUE)
  lang <- meta$options$lang %||% "en"
  values <- meta$options$values %||% "factor"
  sp <- .qes_spec_get(NULL, "none")
  tg <- sp$tables$targets
  pl <- .qes_pool_tables(sp)$pooled
  party_sets <- c("party_qc", "party_qc_intent", "pid_qc")
  set_of <- function(col) {
    j <- match(col, tg$target)
    if (!is.na(j)) return(tg$levels_id[j])
    j <- match(col, pl$pooled)
    if (!is.na(j)) return(pl$levels_id[j])
    NA_character_
  }
  party_cols <- names(x)[vapply(names(x), function(col) set_of(col) %in% party_sets, logical(1))]
  if (is.null(cols)) {
    cols <- party_cols
  } else if (!is.character(cols) || anyNA(cols) || !all(cols %in% party_cols)) {
    .qes_abort("input_lineage_cols", class = "qesR_error_input", args = list(.qes_q(party_cols)),
               data = list(arg = "cols", value = cols))
  }
  map <- unlist(unname(.qes_lineages[lineage]))
  prov <- attr(x, "qes_provenance", exact = TRUE)
  cell <- if (is.null(prov)) NULL else attr(prov, "cell", exact = TRUE)
  # the ADQ era: studies (and polls) fielded before the merger of 2012-01-21
  before <- suppressWarnings(as.integer(as.character(x$year))) < 2012L
  for (col in cols) {
    set <- .qes_spec_levels(sp$tables$levels, set_of(col))
    labs <- set[[paste0("label_", lang)]]
    v <- x[[col]]
    code <- if (is.factor(v)) set$name[match(as.character(v), labs)] else if (identical(values, "labelled")) {
      set$name[match(as.integer(unclass(v)), set$code)]
    } else {
      as.character(v)
    }
    joined <- ifelse(code %in% names(map), unname(map[code]), code)
    out_levels <- unique(ifelse(set$name %in% names(map), unname(map[set$name]), set$name))
    out_labels <- vapply(out_levels, function(l) {
      if (!is.null(.qes_lineage_labels[[l]])) return(.qes_lineage_labels[[l]][[lang]])
      labs[match(l, set$name)]
    }, character(1))
    newv <- if (identical(values, "factor")) {
      factor(unname(out_labels[joined]), levels = unname(out_labels))
    } else {
      joined
    }
    attr(newv, "label") <- paste(attr(v, "label", exact = TRUE) %||% col, "(lineage)")
    g <- if (paste0(col, "__grade") %in% names(x)) as.character(x[[paste0(col, "__grade")]]) else if (!is.null(cell)) {
      cell$grade[match(paste(x$study, col), paste(cell$study, cell$target))]
    } else rep(NA_character_, nrow(x))
    g[before %in% TRUE & !is.na(g)] <- "approximate"
    x[[paste0(col, "_lineage")]] <- newv
    x[[paste0(col, "_lineage__grade")]] <- factor(g, levels = .qes_hz_grades, ordered = TRUE)
  }
  x
}
