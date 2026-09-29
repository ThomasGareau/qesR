# Validation against official results and the census (design.md sections
# 5.10 and 8.3, slice HZ7). Internal: run by the live tests
# (tests/testthat/test-validation-live.R), by data-raw/build_validation.R,
# which records the baselines in inst/extdata/validation/validation_report.csv,
# and by the website's Validation article. Nothing here makes a request: it
# reads a harmonized data frame and the benchmarks in
# inst/extdata/validation/ (data-raw/build_benchmarks.R).
#
# The official results of Elections Quebec (official_results.csv,
# official_turnout.csv) and the recorded report, whose recall and turnout
# rows hold official shares, are in the source tree but not in the package
# build (.Rbuildignore): their terms of use need written permission for
# redistribution and adaptation (inst/COPYRIGHTS, section 3). An installed
# package has only the census margins, so the recall and turnout checks are
# skipped there and the recorded report is empty; the live tests,
# data-raw/build_validation.R and the website run on the source tree.
#
# Checks (one row each in the report, plus one row per level of each
# comparison):
#   V-L2     recall: dissimilarity index between the weighted reported vote
#            (vote_prov_recall) and the official shares of valid votes, in
#            points; gated at the recorded baseline + 2.0. Parties the study's
#            question did not list are counted as "other" on the official
#            side. Only weighted rows are gated; skipped when the weight is
#            calibrated on the vote or when the wave declares a population
#            other than the electorate (qes1998, francophones only).
#   turnout  reported turnout minus official turnout (ballots cast over
#            registered electors), in points; information only, but outside
#            0 to 35 points it fails (design.md section 8.3).
#   census   dissimilarity index between the survey's margins of gender,
#            age, mother tongue and education and those of the census that
#            precedes the study, each on the census table's age cut (see
#            data-raw/build_benchmarks.R); weighted rows gated like V-L2.
#   V-L4     construct validity: the directions of design.md section 8.3.
# A dissimilarity index is half the sum of the absolute differences between
# two distributions, in points: the share that would have to change category
# for the two to agree.

# The path of a table of inst/extdata/validation/, "" when the package does
# not have it (the build-ignored tables, in an installed package).
.qes_validation_file <- function(file) {
  system.file("extdata", "validation", file, package = "qesR")
}

.qes_validation_has <- function(file) nzchar(.qes_validation_file(file))

# The benchmark tables; results and turnout are NULL where they are not
# installed.
.qes_validation_benchmarks <- function() {
  read <- function(file, schema) {
    if (.qes_validation_has(file)) .qes_read_csv(.qes_validation_file(file), schema) else NULL
  }
  list(
    results = read("official_results.csv", "validation_results"),
    turnout = read("official_turnout.csv", "validation_turnout"),
    census = .qes_read_csv(system.file("extdata", "validation", "census_margins.csv", package = "qesR",
                                       mustWork = TRUE), "validation_census")
  )
}

# The recorded report (baselines); no rows where it is not installed.
.qes_validation_recorded <- function() {
  if (.qes_validation_has("validation_report.csv")) {
    .qes_read_csv(.qes_validation_file("validation_report.csv"), "validation_report")
  } else {
    .qes_val_empty()
  }
}

# The targets the report reads.
.qes_validation_targets <- c(
  "vote_prov_recall", "turnout_prov_recall", "gender", "age", "birth_year", "age_group6",
  "education4", "lang_mother", "interest_4pt", "lr_self", "sov_indep", "pid_prov"
)

# Harmonize `studies` (every study the spec covers by default) and build the
# report. `data` is passed to qes_harmonize() (tests).
.qes_validation_run <- function(studies = "all", data = NULL, spec = NULL) {
  h <- qes_harmonize(studies, targets = .qes_validation_targets, values = "code", missing = "reasons",
                     include_draft = TRUE, data = data, spec = spec, quiet = TRUE)
  .qes_validation_report(h, spec = spec)
}

.qes_val_columns <- c(
  "study", "check", "rule", "reference", "variable", "universe", "level", "weight", "n",
  "estimate", "benchmark", "value", "status", "note"
)

.qes_val_row <- function(study, check, rule, reference, variable, universe = NA_character_,
                         level = NA_character_, weight = "none", n = NA_integer_,
                         estimate = NA_real_, benchmark = NA_real_, value = NA_real_,
                         status = NA_character_, note = NA_character_) {
  data.frame(
    study = study, check = check, rule = rule, reference = reference, variable = variable,
    universe = universe, level = level, weight = weight, n = as.integer(n),
    estimate = round(estimate, 2), benchmark = round(benchmark, 2), value = round(value, 2),
    status = status, note = note, stringsAsFactors = FALSE
  )
}

# A report with no rows.
.qes_val_empty <- function() {
  cols <- names(.qes_schemas$validation_report)
  .qes_apply_schema(as.data.frame(stats::setNames(rep(list(character(0)), length(cols)), cols),
                                  stringsAsFactors = FALSE), "validation_report")[, .qes_val_columns]
}

# Weighted percentage distribution of `x` over `levels`.
.qes_val_dist <- function(x, w, levels) {
  tab <- vapply(levels, function(l) sum(w[x == l]), numeric(1))
  100 * tab / sum(tab)
}

.qes_val_dissimilarity <- function(a, b) 0.5 * sum(abs(a - b))

# The weight columns of a study for a target: the unweighted "none" always,
# and the harmonized weight column the weight guide names when its weight is
# reviewed. Returns a named list of weight vectors over the study's rows,
# with the attribute role (the registry role of the named weight).
.qes_val_weights <- function(h, study, target, spec) {
  in_s <- h$study == study
  out <- list(none = rep(1, sum(in_s)))
  guide <- attr(h, "qes_weight_guide")
  g <- guide[guide$study == study & guide$target == target & !is.na(guide$weight_column), , drop = FALSE]
  if (nrow(g) && identical(g$weight_status[1], "reviewed")) {
    w <- h[[g$weight_column[1]]][in_s]
    if (any(!is.na(w))) {
      wt <- spec$tables$weights
      role <- wt$role[wt$study == study & wt$wave == g$wave[1] & wt$weight_var == g$weight_var[1]]
      out[[g$weight_var[1]]] <- structure(w, role = if (length(role)) role[1] else NA_character_)
    }
  }
  out
}

# The election of each study (its rows' election date).
.qes_val_election <- function(h, study) {
  el <- .qes_catalog()$elections
  d <- unique(h$election_date[h$study == study])
  el$election_id[match(d, el$election_date)]
}

# A wave that declares a population other than the study's (waves.csv
# target_population_en, qes1998: francophones only).
.qes_val_subpopulation <- function(spec, study) {
  wv <- spec$tables$waves
  any(!is.na(wv$target_population_en[wv$study == study]))
}

.qes_validation_report <- function(h, benchmarks = .qes_validation_benchmarks(), spec = NULL) {
  spec <- .qes_spec_get(spec)
  studies <- unique(h$study)
  rows <- list()
  for (study in studies) {
    rows <- c(
      rows,
      list(.qes_val_recall(h, study, benchmarks, spec)),
      list(.qes_val_turnout(h, study, benchmarks, spec)),
      list(.qes_val_census(h, study, benchmarks, spec)),
      list(.qes_val_construct(h, study, spec))
    )
  }
  rows <- rows[!vapply(rows, is.null, logical(1))]
  if (!length(rows)) {
    out <- .qes_val_empty()
  } else {
    out <- do.call(rbind, rows)
  }
  rownames(out) <- NULL
  out[, .qes_val_columns]
}

# ---- V-L2: reported vote against the official results --------------------------------------
.qes_val_recall <- function(h, study, benchmarks, spec) {
  in_s <- h$study == study
  x <- h$vote_prov_recall[in_s]
  if (is.null(x) || all(is.na(x))) {
    return(NULL)
  }
  x <- as.character(x)
  election <- .qes_val_election(h, study)
  if (is.null(benchmarks$results)) {
    return(.qes_val_row(study, "recall", "V-L2", paste(election, collapse = ";"), "vote_prov_recall",
                        status = "skipped", note = "the official results are not installed (inst/COPYRIGHTS, section 3)"))
  }
  off <- benchmarks$results[benchmarks$results$election_id %in% election, , drop = FALSE]
  if (length(election) != 1L || !nrow(off)) {
    return(.qes_val_row(study, "recall", "V-L2", paste(election, collapse = ";"), "vote_prov_recall",
                        status = "skipped", note = "no official results for this election"))
  }
  xw <- spec$tables$crosswalk
  xw <- xw[xw$study == study & xw$target == "vote_prov_recall" & xw$primary %in% TRUE, , drop = FALSE]
  offered <- .qes_split_list(xw$levels_offered[1])
  if (!length(offered)) {
    offered <- unique(x[!is.na(x)])
  }
  # the official side in the study's categories: a party the question did not
  # list could only be answered as "other"
  party <- ifelse(off$party %in% setdiff(offered, "other"), off$party, "other")
  levels <- union(setdiff(offered, "other"), if ("other" %in% offered) "other")
  votes <- vapply(levels, function(l) sum(off$votes[party == l]), numeric(1))
  bench <- 100 * votes / sum(votes)
  note_off <- if (!("other" %in% offered)) "the question offered no other party: official shares among the listed parties" else NA_character_
  skip_pop <- .qes_val_subpopulation(spec, study)
  out <- list()
  for (wn in names(ws <- .qes_val_weights(h, study, "vote_prov_recall", spec))) {
    w <- ws[[wn]]
    ok <- !is.na(x) & x %in% levels & !is.na(w)
    if (!any(ok)) next
    est <- .qes_val_dist(x[ok], w[ok], levels)
    status <- if (wn == "none") "info" else "gate"
    note <- note_off
    if (skip_pop) {
      status <- "skipped"
      note <- "the study's population is not the electorate (a wave declares its own target population)"
    } else if (identical(attr(w, "role"), "vote_calibrated")) {
      status <- "skipped"
      note <- "the weight is calibrated on the vote"
    }
    out[[length(out) + 1L]] <- .qes_val_row(
      study, "recall", "V-L2", election, "vote_prov_recall", level = levels, weight = wn,
      n = vapply(levels, function(l) sum(ok & x == l), integer(1)),
      estimate = est, benchmark = bench, value = est - bench
    )
    out[[length(out) + 1L]] <- .qes_val_row(
      study, "recall", "V-L2", election, "vote_prov_recall", level = NA_character_, weight = wn,
      n = sum(ok), value = .qes_val_dissimilarity(est, bench), status = status, note = note
    )
  }
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}

# ---- reported turnout against the official turnout -----------------------------------------
.qes_val_turnout <- function(h, study, benchmarks, spec) {
  in_s <- h$study == study
  x <- h$turnout_prov_recall[in_s]
  if (is.null(x) || all(is.na(x))) {
    return(NULL)
  }
  x <- as.character(x)
  election <- .qes_val_election(h, study)
  if (is.null(benchmarks$turnout)) {
    return(.qes_val_row(study, "turnout", "turnout", paste(election, collapse = ";"), "turnout_prov_recall",
                        status = "skipped", note = "the official results are not installed (inst/COPYRIGHTS, section 3)"))
  }
  off <- benchmarks$turnout[benchmarks$turnout$election_id %in% election, , drop = FALSE]
  if (length(election) != 1L || nrow(off) != 1L) {
    return(NULL)
  }
  eligible <- !(h$eligible_voter[in_s] %in% FALSE)
  skip_pop <- .qes_val_subpopulation(spec, study)
  out <- list()
  for (wn in names(ws <- .qes_val_weights(h, study, "turnout_prov_recall", spec))) {
    w <- ws[[wn]]
    # yes or no only: not registered, ineligible, don't know and refusals
    # are left out of the denominator (OD12)
    ok <- x %in% c("yes", "no") & eligible & !is.na(w)
    if (!any(ok)) next
    est <- 100 * sum(w[ok & x == "yes"]) / sum(w[ok])
    value <- est - off$turnout
    status <- if (value < 0 || value > 35) "fail" else "info"
    note <- NA_character_
    if (skip_pop) {
      status <- "skipped"
      note <- "the study's population is not the electorate (a wave declares its own target population)"
    } else if (identical(attr(w, "role"), "turnout_calibrated")) {
      status <- "skipped"
      note <- "the weight is calibrated on turnout"
    }
    out[[length(out) + 1L]] <- .qes_val_row(
      study, "turnout", "turnout", election, "turnout_prov_recall", level = "yes", weight = wn,
      n = sum(ok), estimate = est, benchmark = off$turnout, value = value, status = status, note = note
    )
  }
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}

# ---- demographic margins against the census --------------------------------------------------
# The respondent's age in years: the age target where there is one, else the
# year of the election minus the year of birth (one year too high for those
# whose birthday falls after election day).
.qes_val_age_years <- function(h, in_s) {
  age <- if (!is.null(h$age)) as.numeric(h$age[in_s]) else rep(NA_real_, sum(in_s))
  if (!is.null(h$birth_year)) {
    yob <- as.numeric(h$birth_year[in_s])
    from_yob <- as.numeric(format(h$election_date[in_s], "%Y")) - yob
    age[is.na(age)] <- from_yob[is.na(age)]
  }
  age
}

.qes_val_age6 <- function(age) {
  as.character(cut(age, c(-Inf, 24, 34, 44, 54, 64, Inf),
                   labels = c("a18_24", "a25_34", "a35_44", "a45_54", "a55_64", "a65_plus")))
}

.qes_val_census <- function(h, study, benchmarks, spec) {
  if (.qes_val_subpopulation(spec, study)) {
    return(NULL)
  }
  in_s <- h$study == study
  cen <- benchmarks$census
  # the census on or before the study's latest year, for the whole study.
  # A pooled study whose rows fall on either side of a census year would
  # compare its early rows with the later census: stop rather than do so.
  years <- unique(as.integer(h$year[in_s]))
  years <- years[!is.na(years)]
  if (!length(years)) {
    return(NULL)
  }
  row_census <- vapply(years, function(y) {
    suppressWarnings(max(cen$census_year[cen$census_year <= y]))
  }, numeric(1))
  row_census <- unique(row_census[is.finite(row_census)])
  if (length(row_census) > 1L) {
    stop(sprintf("study %s spans censuses %s: compare each census period separately",
                 study, paste(sort(row_census), collapse = ", ")), call. = FALSE)
  }
  census_year <- suppressWarnings(max(cen$census_year[cen$census_year <= max(years)]))
  if (!is.finite(census_year)) {
    return(NULL)
  }
  cen <- cen[cen$census_year == census_year, , drop = FALSE]
  age <- .qes_val_age_years(h, in_s)
  band <- if (!is.null(h$age_group6)) as.character(h$age_group6[in_s]) else rep(NA_character_, sum(in_s))
  band[is.na(band)] <- .qes_val_age6(age)[is.na(band)]
  band[!is.na(band) & !is.na(age) & age < 18] <- NA_character_
  # a respondent is in the universe of a cut when their age (or band) says
  # so; with no age at all, adult samples are counted in
  in_universe <- function(cut) {
    min_age <- as.integer(sub("\\+$", "", cut))
    known <- !is.na(age)
    out <- ifelse(known, age >= min_age, TRUE)
    if (min_age >= 25) {
      out <- ifelse(known, out, !is.na(band) & band != "a18_24")
    }
    out & !(h$eligible_voter[in_s] %in% FALSE & (is.na(age) | age < 18))
  }
  edu <- if (!is.null(h$education4)) as.character(h$education4[in_s]) else NULL
  if (!is.null(edu)) {
    # the level reached, in the census's two groups (data-raw/build_benchmarks.R)
    edu <- c(primary = "below_university", secondary = "below_university", college = "below_university",
             university = "university")[edu]
  }
  vars <- list(
    gender = if (!is.null(h$gender)) as.character(h$gender[in_s]),
    age_group6 = band,
    lang_mother = if (!is.null(h$lang_mother)) as.character(h$lang_mother[in_s]),
    education = unname(edu)
  )
  source_target <- c(gender = "gender", age_group6 = "gender", lang_mother = "lang_mother", education = "education4")
  out <- list()
  for (v in names(vars)) {
    x <- vars[[v]]
    b <- cen[cen$variable == v, , drop = FALSE]
    if (is.null(x) || all(is.na(x)) || !nrow(b)) next
    universe <- b$universe[1]
    levels <- b$level
    if (v == "age_group6" && all(is.na(age)) && all(is.na(h$age_group6[in_s]))) next
    ok_u <- in_universe(universe)
    ws <- .qes_val_weights(h, study, if (v == "age_group6") "gender" else source_target[[v]], spec)
    for (wn in names(ws)) {
      w <- ws[[wn]]
      ok <- ok_u & !is.na(x) & x %in% levels & !is.na(w)
      if (sum(ok) < 30L) next
      est <- .qes_val_dist(x[ok], w[ok], levels)
      bench <- b$share / sum(b$share) * 100
      ref <- sprintf("Census %d", census_year)
      table <- paste("Statistics Canada", paste(unique(b$source_table), collapse = ";"))
      out[[length(out) + 1L]] <- .qes_val_row(
        study, "census", "census", ref, v, universe = universe, level = levels, weight = wn,
        n = vapply(levels, function(l) sum(ok & x == l), integer(1)),
        estimate = est, benchmark = bench, value = est - bench, note = table
      )
      out[[length(out) + 1L]] <- .qes_val_row(
        study, "census", "census", ref, v, universe = universe, weight = wn, n = sum(ok),
        value = .qes_val_dissimilarity(est, bench), status = if (wn == "none") "info" else "gate",
        note = table
      )
    }
  }
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}

# ---- V-L4: construct validity ----------------------------------------------------------------
.qes_val_construct <- function(h, study, spec) {
  in_s <- h$study == study
  get <- function(t) if (is.null(h[[t]])) rep(NA_character_, sum(in_s)) else as.character(h[[t]][in_s])
  vote <- get("vote_prov_recall")
  if (all(is.na(vote))) {
    return(NULL)
  }
  ws <- .qes_val_weights(h, study, "vote_prov_recall", spec)
  wn <- names(ws)[length(ws)]
  w <- ws[[wn]]
  w[is.na(w)] <- 0
  wmean <- function(v, ok) if (sum(w[ok]) > 0) sum(w[ok] * v[ok]) / sum(w[ok]) else NA_real_
  out <- list()
  add <- function(variable, level, n, estimate, benchmark, value, pass, note) {
    out[[length(out) + 1L]] <<- .qes_val_row(
      study, "construct", "V-L4", "direction", variable, level = level, weight = wn, n = n,
      estimate = estimate, benchmark = benchmark, value = value,
      status = if (is.na(pass)) "skipped" else if (pass) "pass" else "fail", note = note
    )
  }
  # interest (1 not at all to 4 very): voters above non-voters
  interest <- c(very = 4, quite = 3, hardly = 2, not_at_all = 1)[get("interest_4pt")]
  turnout <- get("turnout_prov_recall")
  if (any(!is.na(interest))) {
    v_ok <- !is.na(interest) & turnout %in% "yes"
    n_ok <- !is.na(interest) & turnout %in% "no"
    if (sum(v_ok) >= 10L && sum(n_ok) >= 10L) {
      a <- wmean(interest, v_ok)
      b <- wmean(interest, n_ok)
      add("interest_4pt", "voters - nonvoters", sum(v_ok | n_ok), a, b, a - b, a > b,
          "mean interest (1 = not at all, 4 = very) of voters (estimate) and nonvoters (benchmark)")
    }
  }
  # left-right: QS voters left of CAQ voters
  lr <- suppressWarnings(as.numeric(get("lr_self")))
  if (any(!is.na(lr))) {
    q_ok <- !is.na(lr) & vote %in% "QS"
    c_ok <- !is.na(lr) & vote %in% "CAQ"
    if (sum(q_ok) >= 10L && sum(c_ok) >= 10L) {
      a <- wmean(lr, q_ok)
      b <- wmean(lr, c_ok)
      add("lr_self", "QS - CAQ", sum(q_ok | c_ok), a, b, a - b, a < b,
          "mean left-right self-placement (0-10) of QS voters (estimate) and CAQ voters (benchmark)")
    }
  }
  # independence: yes among PQ voters more than 40 points above PLQ voters
  sov <- get("sov_indep")
  if (any(!is.na(sov))) {
    yes <- as.numeric(sov %in% "yes")
    p_ok <- sov %in% c("yes", "no") & vote %in% "PQ"
    l_ok <- sov %in% c("yes", "no") & vote %in% "PLQ"
    if (sum(p_ok) >= 10L && sum(l_ok) >= 10L) {
      a <- 100 * wmean(yes, p_ok)
      b <- 100 * wmean(yes, l_ok)
      add("sov_indep", "PQ - PLQ", sum(p_ok | l_ok), a, b, a - b, a - b > 40,
          "percent yes (among yes and no) of PQ voters (estimate) and PLQ voters (benchmark)")
    }
  }
  # party identification: the party voted for at least 55% of the time
  pid <- get("pid_prov")
  if (any(!is.na(pid))) {
    ok <- !is.na(pid) & pid != "none" & !is.na(vote)
    if (sum(ok) >= 30L) {
      a <- 100 * wmean(as.numeric(pid == vote), ok)
      add("pid_prov", "pid = vote", sum(ok), a, NA_real_, a, a >= 55,
          "percent of partisans (a party identification) who voted for that party")
    }
  }
  if (!length(out)) {
    return(NULL)
  }
  do.call(rbind, out)
}

# ---- gate --------------------------------------------------------------------------------------
# Compare a report with the recorded one: a gated row (status "gate") passes
# when its value is at most the recorded value + `tolerance` points. Returns
# the report with `baseline`, `max_allowed` and the final status (pass,
# fail, or new when nothing is recorded for the row).
.qes_validation_gate <- function(report, recorded, tolerance = 2.0) {
  key <- function(x) paste(x$study, x$check, x$variable, x$weight, sep = "\x1f")
  summary_rows <- function(x) is.na(x$level) & x$check %in% c("recall", "census")
  rec <- recorded[summary_rows(recorded), , drop = FALSE]
  report$baseline <- NA_real_
  report$max_allowed <- NA_real_
  s <- summary_rows(report)
  m <- match(key(report)[s], key(rec))
  report$baseline[s] <- rec$value[m]
  report$max_allowed[s] <- rec$value[m] + tolerance
  g <- which(report$status %in% "gate")
  base <- report$baseline[g]
  report$status[g] <- ifelse(is.na(base), "new", ifelse(report$value[g] <= base + tolerance + 1e-9, "pass", "fail"))
  report
}
