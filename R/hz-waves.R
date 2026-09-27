# Waves, weights and eligibility of harmonized data (design.md sections 5.2,
# 5.3, 5.6 and 5.8, slice HZ4).
#
# Every study of the spec has one or more waves (waves.csv): a pre- or
# post-election cross-section, or the waves of a panel or of a pre/post
# design such as the 2022 campaign-period and post-election surveys. A
# respondent belongs to a wave by its membership rule, declared on a
# disposition or date variable; the engine never infers it from answers.
# For each respondent and wave this file gives
#   * the interview date (the wave's date variable) and the days from it to
#     the election;
#   * the interview mode (the wave's mode, or, when the mode varies by
#     respondent, the survey_mode crosswalk row of that wave);
#   * the wave's recommended weight (weights.csv), normalized to mean 1 over
#     the wave's members with a weight, or raw. A weight whose registry row
#     still needs review is NA until it is documented;
# and, for each respondent, whether they could vote in the study's election
# (eligible_voter), from the age and citizenship targets of the spec.

# Targets the engine reads for the leading columns, whether or not they are
# requested: the age and citizenship targets that decide eligibility, and the
# interview mode of waves whose mode varies by respondent.
.qes_hz_design_inputs <- c("birth_year", "birth_month", "age", "age_group3", "citizen", "survey_mode")

# Targets whose block is id or design are leading columns of the result, not
# target columns.
.qes_hz_leading_targets <- function(spec) {
  tg <- spec$tables$targets
  tg$target[tg$block %in% c("id", "design")]
}

# ---- membership ----------------------------------------------------------------

# Logical matrix: rows of `d` x waves of `wv` (in wave order), TRUE where the
# row is a member of the wave. A wave whose membership variable is missing
# from `d` has no member.
.qes_hz_member_matrix <- function(wv, d) {
  n <- nrow(d)
  m <- matrix(FALSE, n, nrow(wv), dimnames = list(NULL, wv$wave))
  for (w in seq_len(nrow(wv))) {
    mem <- .qes_wave_members(wv, w, d)
    if (!is.null(mem)) m[, w] <- mem
  }
  m
}

# The first wave (column of `members`) each row belongs to, among the waves
# where `ok` is TRUE; NA for a row in none of them.
.qes_hz_first_wave <- function(members, ok = rep(TRUE, ncol(members))) {
  n <- nrow(members)
  if (ncol(members) == 0L || !any(ok)) {
    return(rep(NA_integer_, n))
  }
  sub <- members[, ok, drop = FALSE]
  idx <- which(ok)
  out <- max.col(sub, ties.method = "first")
  out <- idx[out]
  out[rowSums(sub) == 0L] <- NA_integer_
  out
}

# ---- dates and modes -------------------------------------------------------------

# Interview dates (Date) of the rows of `d` in wave row `w`: its date
# variable read in its date format; NA when the wave has none.
.qes_hz_wave_dates <- function(wv, w, d) {
  var <- wv$date_var[w]
  if (is.na(var) || !var %in% names(d)) {
    return(as.Date(rep(NA_character_, nrow(d))))
  }
  as.Date(.qes_hz_dates(d[[var]], wv$date_format[w] %|NA|% "yyyymmdd"))
}

# Interview modes (web, phone, mixed; NA when unknown) of the rows of `d` in
# wave row `w`. `mode_cell` is the survey_mode cell of the study (values
# over the rows, and the wave of its crosswalk row), or NULL.
.qes_hz_wave_modes <- function(wv, w, n, mode_cell) {
  mode <- wv$mode[w]
  if (is.na(mode)) {
    return(rep(NA_character_, n))
  }
  if (!startsWith(mode, "var:")) {
    return(rep(mode, n))
  }
  if (is.null(mode_cell) || !identical(mode_cell$wave, wv$wave[w])) {
    return(rep(NA_character_, n))
  }
  mode_cell$value
}

# ---- weights -----------------------------------------------------------------------

# The recommended weight of each wave of a study, over the rows of `d`:
# a list, one element per wave row of `wv`, with `value` (numeric, NA outside
# the wave or where the weight is missing or not positive), `var`, `status`,
# `role`, `mean_raw` (mean of the raw weight over the wave's members with a
# weight) and `used` (FALSE when the wave has no recommended weight or its
# registry row needs review; `value` is then NA throughout).
.qes_hz_wave_weights <- function(wv, wt, d, members, normalize) {
  n <- nrow(d)
  lapply(seq_len(nrow(wv)), function(w) {
    k <- which(wt$wave == wv$wave[w] & wt$recommended %in% TRUE)
    empty <- list(value = rep(NA_real_, n), var = NA_character_, status = NA_character_,
                  role = NA_character_, mean_raw = NA_real_, used = FALSE)
    if (length(k) == 0L) {
      return(empty)
    }
    k <- k[1]
    out <- empty
    out$var <- wt$weight_var[k]
    out$status <- wt$status[k]
    out$role <- wt$role[k]
    if (!out$var %in% names(d)) {
      return(out)
    }
    raw <- unclass(d[[out$var]])
    attributes(raw) <- NULL
    x <- suppressWarnings(as.numeric(raw))
    mem <- members[, w]
    x[!mem | is.na(x) | x <= 0] <- NA_real_
    out$mean_raw <- if (any(!is.na(x))) mean(x, na.rm = TRUE) else NA_real_
    if (!identical(out$status, "reviewed")) {
      return(out)
    }
    out$used <- TRUE
    out$value <- if (isTRUE(normalize) && !is.na(out$mean_raw)) x / out$mean_raw else x
    out
  })
}

# The weight column a wave's weight goes to in the respondent layout: pre
# for a pre-election wave, post for a post-election wave, both for a wave
# that spans the election.
.qes_hz_weight_columns <- function(timing) {
  switch(timing %|NA|% "", pre = "weight_pre", post = "weight_post",
         between = c("weight_pre", "weight_post"), character(0))
}

# ---- eligibility -------------------------------------------------------------------

# The age bounds of the levels of an age-band level set: level names
# a<low>_<high> or a<low>_plus. NULL for a name of another form.
.qes_hz_band_bounds <- function(name) {
  m <- regmatches(name, regexec("^a([0-9]+)_([0-9]+|plus)$", name))[[1]]
  if (length(m) != 3L) {
    return(NULL)
  }
  c(low = as.numeric(m[2]), high = if (m[3] == "plus") Inf else as.numeric(m[3]))
}

# Whether each respondent could vote in the study's election: 18 or older on
# election day and, where the study asked, a Canadian citizen. `cells` holds
# the study's values of the design inputs (value: level name or number text,
# over the rows; reason; timing: the timing of the wave that asked). Each
# source gives TRUE, FALSE or nothing for a row:
#   * birth_year Y (with birth_month M where asked) against the election
#     date E: E's year minus Y of 19 or more is TRUE, of 17 or less FALSE;
#     of exactly 18, M before E's month is TRUE, after it FALSE, and the
#     same month or an unknown month says nothing;
#   * age A at an interview before the election: 18 or more is TRUE (the
#     respondent was at least as old on election day), 16 or less FALSE;
#     after the election: 19 or more TRUE, 17 or less FALSE;
#   * an age band [L, H] (level a<L>_<H>) in the same way, with L for the
#     TRUE rule and H for the FALSE rule.
# Sources that disagree give NA. Then citizen "no" gives FALSE, and a
# missing answer to a citizenship question that was asked gives NA unless
# the respondent is already FALSE. A respondent no source speaks for is NA.
.qes_hz_eligible <- function(cells, n, e_date) {
  says_true <- rep(FALSE, n)
  says_false <- rep(FALSE, n)
  add <- function(t, f) {
    says_true <<- says_true | (t %in% TRUE)
    says_false <<- says_false | (f %in% TRUE)
  }
  # (cells[["age"]], never cells$age, which would partially match age_group3)
  num <- function(t) suppressWarnings(as.numeric(cells[[t]]$value))
  if (!is.na(e_date)) {
    ey <- as.integer(format(e_date, "%Y"))
    em <- as.integer(format(e_date, "%m"))
    if (!is.null(cells[["birth_year"]])) {
      dy <- ey - num("birth_year")
      month <- if (is.null(cells[["birth_month"]])) rep(NA_real_, n) else num("birth_month")
      add(dy >= 19 | (dy == 18 & month < em), dy <= 17 | (dy == 18 & month > em))
    }
  }
  timing_rules <- function(timing) {
    switch(timing %|NA|% "", pre = c(true = 18, false = 16), post = c(true = 19, false = 17),
           c(true = 19, false = 16))
  }
  if (!is.null(cells[["age"]])) {
    a <- num("age")
    r <- timing_rules(cells[["age"]]$timing)
    add(a >= r[["true"]], a <= r[["false"]])
  }
  if (!is.null(cells[["age_group3"]])) {
    v <- cells[["age_group3"]]$value
    bounds <- lapply(v, function(x) if (is.na(x)) NULL else .qes_hz_band_bounds(x))
    low <- vapply(bounds, function(b) if (is.null(b)) NA_real_ else b[["low"]], numeric(1))
    high <- vapply(bounds, function(b) if (is.null(b)) NA_real_ else b[["high"]], numeric(1))
    r <- timing_rules(cells[["age_group3"]]$timing)
    add(low >= r[["true"]], high <= r[["false"]])
  }
  out <- rep(NA, n)
  out[says_true & !says_false] <- TRUE
  out[says_false & !says_true] <- FALSE
  if (!is.null(cells[["citizen"]])) {
    cz <- cells[["citizen"]]
    out[cz$value %in% "no"] <- FALSE
    asked_missing <- is.na(cz$value) & !cz$reason %in% c("not_asked", "not_reviewed", "not_in_wave")
    out[asked_missing & !(out %in% FALSE)] <- NA
  }
  out
}
