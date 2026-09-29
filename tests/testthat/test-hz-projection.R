# V-P1: the marginals projected from the spec, the dictionary and gates.csv
# equal expected/marginals.csv (design.md sections 5.10 and 5.11, slice
# HZ2), the MAJOR rule on expected/, and the semantics of the projection.

test_that("the projection of the shipped spec is exactly expected/marginals.csv (V-P1)", {
  s <- hz_spec()
  src <- .qes_hz_sources_shipped(s)
  proj <- .qes_project_marginals(s, src)
  expect_null(attr(proj, "unprojected"))
  attr(proj, "unprojected") <- NULL
  expect_identical(proj, s$tables$expected)
  expect_identical(nrow(.qes_projection_check(s, src)), 0L)
  # every projectable row of every study whose metadata ships is projected
  shipped <- .qes_catalog()$studies
  shipped <- shipped$study[shipped$metadata_shipped %in% TRUE]
  xw <- s$tables$crosswalk
  rows <- xw[xw$rule %in% c("map", "numeric") & xw$study %in% shipped, ]
  expect_setequal(unique(paste(proj$study, proj$wave, proj$target)), paste(rows$study, rows$wave, rows$target))
  # every study ships its aggregates, qes2022 included (OD3 lifted)
  expect_setequal(unique(s$tables$expected$study), unique(rows$study))
  expect_true(any(s$tables$gates$study == "qes2022"))
})

test_that("each projected row counts every member of its wave once", {
  s <- hz_spec()
  e <- s$tables$expected
  totals <- tapply(e$n, paste(e$study, e$wave, e$target, e$source_var), sum)
  wave_of <- sub(" [^ ]+ [^ ]+$", "", names(totals))
  wv <- s$tables$waves
  size <- stats::setNames(wv$n_cases, paste(wv$study, wv$wave))
  # a row of wave "*" counts the members of every poll wave of its study
  # (each respondent answered in one poll), or of any wave of a panel (for
  # a time-invariant item: the 2007 panel's 2,050 pre-wave and 391
  # post-only respondents)
  poll <- wv$wave_design == "poll_wave"
  polls <- tapply(wv$n_cases[poll], wv$study[poll], sum)
  size <- c(size, stats::setNames(as.integer(polls), paste(names(polls), "*")), `qes2007_panel *` = 2441L)
  expect_identical(as.vector(totals), unname(size[wave_of]))
})

test_that("a spec change that moves counts fails V-P1", {
  s <- hz_spec()
  vm <- s$tables$valuemaps
  i <- which(vm$map_id == "vote_qes2018_q6" & vm$source_code == "95")
  vm$target_code[i] <- 90L
  vm$na_reason[i] <- NA
  s$tables$valuemaps <- vm
  p <- .qes_projection_check(s, .qes_hz_sources_shipped(s))
  expect_identical(p$key, "qes2018/post/vote_prov_recall/q6")
  expect_match(p$detail, "other 123 (expected 92)", fixed = TRUE)
  # a row without cells cannot be projected
  s <- hz_spec()
  s$tables$gates <- s$tables$gates[s$tables$gates$study != "qes2014", ]
  p <- .qes_projection_check(s, .qes_hz_sources_shipped(s))
  # (every projectable row of qes2014 has its cells there)
  xw <- s$tables$crosswalk
  rows <- which(xw$study == "qes2014" & xw$rule %in% c("map", "numeric"))
  expect_setequal(p$key, paste(xw$study, xw$wave, xw$target, xw$source_var, sep = "/")[rows])
  expect_true(all(grepl("gates.csv has no cells", p$detail, fixed = TRUE)))
})

test_that("real-code marginals are what the source files give", {
  e <- hz_spec()$tables$expected
  n <- function(study, target, value = NA, reason = NA) {
    rows <- e$study == study & e$target == target &
      (if (anyNA(value)) is.na(e$value) else e$value %in% value) &
      (if (anyNA(reason)) is.na(e$na_reason) else e$na_reason %in% reason)
    sum(e$n[rows])
  }
  # [A:H3]: 2018 q27 = 1 is "very" (743); 2014 Q32 keeps its endpoints (0 and 10)
  expect_identical(n("qes2018", "interest_4pt", "very"), 743L)
  expect_identical(c(n("qes2014", "lr_self", "0"), n("qes2014", "lr_self", "10")), c(55L, 87L))
  expect_identical(n("qes2014", "lr_self", as.character(0:10)), 1195L)
  # [A:H4]: 2018 ideology is q36_1, 2,490 valid answers
  expect_identical(n("qes2018", "lr_self", as.character(0:10)), 2490L)
  # the 2018 recall: universe q5 = 4 (2,207), q6 95 spoiled (31), minors routed out
  expect_identical(n("qes2018", "vote_prov_recall", c("PLQ", "PQ", "CAQ", "QS", "other")), 2016L)
  expect_identical(n("qes2018", "vote_prov_recall", reason = "spoiled"), 31L)
  expect_identical(n("qes2018", "vote_prov_recall", reason = "ineligible"), 99L)
  expect_identical(n("qes2018", "vote_prov_recall", reason = "inapplicable"), 270L)
  expect_identical(n("qes2018", "turnout_prov_recall", "yes"), 2207L)
  # the 2012 recall: q25 answered exactly by the 1,369 who voted (q21 = 1)
  expect_identical(n("qes2012", "vote_prov_recall", reason = "not_voted"), 117L)
  expect_identical(n("qes2012", "vote_prov_recall", c("PLQ", "PQ", "CAQ", "QS", "PVQ", "ON", "other")), 1274L)
  # 2007 panel: post-wave members only (2,054); "non rejoint" never occurs among them [A:N1]
  expect_identical(n("qes2007_panel", "vote_prov_recall", c("PLQ", "PQ", "QS", "PVQ", "ADQ", "other")), 1494L)
  expect_identical(n("qes2007_panel", "vote_prov_recall", reason = "not_in_wave"), 0L)
  expect_identical(n("qes2007_panel", "vote_prov_recall", reason = "not_voted"), 302L)
  # the no-party answers of the intention are a level, not missing
  expect_identical(n("qes2007_panel", "vote_prov_intent", "no_party"), 81L)
  expect_identical(n("qes2007_panel", "vote_prov_intent", reason = "sysmis"), 1L)
  # 2018 panel: rts_q2 answered exactly by the 731 who voted; independance is never projected
  expect_identical(n("qes2018_panel", "vote_prov_recall", reason = "not_voted"), 111L)
  expect_false(any(e$source_var == "independance"))
  # 2012 panel: 97 would not vote is a level of the referendum set
  expect_true(all(e$value[e$study == "qes2012_panel" & e$target == "sov_sovereign_country"] %in%
                    c(NA, "yes", "no", "would_not_vote")))
  # age bands collapse exactly from six bands (slice HZ4): 2012 panel 34 + 93,
  # 155 + 184, 169 + 209; the producers' own recodes give the same counts
  expect_identical(n("qes2012_panel", "age_group3", "a18_34"), 127L)
  expect_identical(n("qes2012_panel", "age_group3", "a35_54"), 339L)
  expect_identical(n("qes2012_panel", "age_group3", "a55_plus"), 378L)
  # the 2007 panel's age bands, in whichever wave the respondent answered
  # (spec 1.0.0; the pre-election wave alone had 441 aged 18-34)
  expect_identical(n("qes2007_panel", "age_group3", "a18_34"), 528L)
  expect_identical(n("qes2007_panel", "age_group3", reason = "sysmis"), 1L)
  expect_identical(n("qes2007_panel", "age_group6", "a65_plus"), 418L)
  expect_identical(n("qes2018_panel", "age_group3", "a55_plus"), 551L)
  # 2018 year and month of birth: the 41 who chose not to answer are refused
  expect_identical(n("qes2018", "birth_year", reason = "refused"), 41L)
  expect_identical(n("qes2018", "birth_month", reason = "refused"), 41L)
  expect_identical(n("qes2018", "age", reason = "inapplicable"), 3031L)
  # the interview mode of the 2018 panel's first wave: 400 telephone, 850 web
  expect_identical(n("qes2018_panel", "survey_mode", "phone"), 400L)
  expect_identical(n("qes2018_panel", "survey_mode", "web"), 850L)
})

test_that("a row without cells in gates.csv is reported, never projected from the dictionary", {
  # the dictionary lists only the labelled codes of agex (more than 50
  # codes): its counts must not stand in for the cells
  s <- hz_spec()
  # (the birth_year row of qes2012 reads agex: drop it and its cells)
  s$tables$crosswalk <- s$tables$crosswalk[-hz_xw_row(s$tables, "qes2012", "birth_year"), , drop = FALSE]
  s$tables$gates <- s$tables$gates[!(s$tables$gates$study == "qes2012" & s$tables$gates$source_var == "agex"), , drop = FALSE]
  i <- hz_xw_row(s$tables, "qes2012", "lr_self")
  s$tables$crosswalk$source_var[i] <- "agex"
  s$tables$crosswalk$args[i] <- "min=18;max=120"
  src <- .qes_hz_sources_shipped(s)
  expect_true("agex" %in% src$values$variable[src$values$study == "qes2012"])
  proj <- expect_no_error(.qes_project_marginals(s, src))
  expect_false(any(proj$source_var == "agex"))
  un <- attr(proj, "unprojected")
  expect_identical(un$source_var, "agex")
  expect_match(un$reason, "gates.csv has no cells", fixed = TRUE)
  p <- .qes_projection_check(s, src)
  expect_true(any(p$key == "qes2012/post/lr_self/agex" & p$severity == "error"))
})

test_that("the projection applies the rule, na_codes and the gate in that order", {
  s <- hz_spec()
  i <- hz_xw_row(s$tables, "qes2018", "vote_prov_recall")
  r <- .qes_hz_row_rule(s, i)
  cells <- data.frame(
    gate_code = c("4", "4", "4", "4", "1", "5", "NA", "99", "4"),
    source_code = c("1", "95", "96", "99", "NA", "NA", "NA", "NA", "42"),
    n = c(10L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 1L),
    stringsAsFactors = FALSE
  )
  oc <- .qes_hz_cell_outcome(r, cells, rep(NA_real_, nrow(cells)))
  expect_identical(oc$value, c("PLQ", NA, "other", NA, NA, NA, NA, NA, NA))
  expect_identical(oc$na_reason, c(NA, "spoiled", NA, "refused", "not_voted", "ineligible", "inapplicable", "refused", "unmapped"))
  # a gate outcome that is a level; an open gate with a missing source is sysmis
  r$gate_to[["1"]] <- "other"
  oc <- .qes_hz_cell_outcome(r, cells[5, ], NA_real_)
  expect_identical(c(oc$value, oc$na_reason), c("other", NA))
  oc <- .qes_hz_cell_outcome(r, data.frame(gate_code = "4", source_code = "NA", n = 1L), NA_real_)
  expect_identical(oc$na_reason, "sysmis")
  # numeric rules: range, affine, from_label and the NA token
  r <- .qes_hz_row_rule(s, hz_xw_row(s$tables, "qes2014", "lr_self"))
  oc <- .qes_hz_outcome(r, c("0", "10", "11", "98", "NA", "x"))
  expect_identical(oc$value, c("0", "10", NA, NA, NA, NA))
  expect_identical(oc$na_reason, c(NA, NA, "unmapped", "dk", "sysmis", "unmapped"))
  r$affine <- c(a = 0.5, b = 1)
  expect_identical(.qes_hz_outcome(r, c("2", "18", "20"))$value, c("2", "10", NA))
  r <- .qes_hz_row_rule(s, hz_xw_row(s$tables, "qes2018_panel", "lr_self"))
  oc <- .qes_hz_outcome(r, c("1", "11", "12"), c(0, 10, NA))
  expect_identical(oc$value, c("0", "10", NA))
  expect_identical(oc$na_reason, c(NA, NA, "dk_refused"))
  r <- .qes_hz_row_rule(s, hz_xw_row(s$tables, "qes2018", "turnout_prov_recall"))
  expect_identical(.qes_hz_outcome(r, "NA")$na_reason, "inapplicable")
})

test_that("the MAJOR rule classifies changes to expected/marginals.csv", {
  e <- hz_spec()$tables$expected
  expect_identical(as.character(.qes_expected_change(e, e)), "none")
  # rows added only: minor
  add <- e[e$target == "lr_self" & e$study == "qes2012", ]
  add$study <- "qes2008"
  expect_identical(as.character(.qes_expected_change(e, rbind(e, add))), "minor")
  # a count that moves, a value that appears in a cell, a cell removed, a new source: major
  moved <- e
  moved$n[1] <- moved$n[1] + 1L
  ch <- .qes_expected_change(e, moved)
  expect_identical(as.character(ch), "major")
  expect_length(attr(ch, "keys"), 1L)
  extra <- e[1, ]
  extra$value <- "PCQ"
  extra$n <- 0L
  expect_identical(as.character(.qes_expected_change(e, rbind(e, extra))), "major")
  gone <- e[!(e$study == "qes2014" & e$target == "pid_prov"), ]
  expect_identical(as.character(.qes_expected_change(e, gone)), "major")
  src <- e
  src$source_var[src$study == "qes2014" & src$target == "pid_prov"] <- "Q56"
  expect_identical(as.character(.qes_expected_change(e, src)), "major")
  # a second row of one (study, wave, target) on another source: its cells
  # are keyed apart, so a count that moves in either is seen
  second <- e[e$study == "qes2014" & e$target == "lr_self", ]
  second$source_var <- "Q31A"
  two <- rbind(e, second)
  expect_identical(as.character(.qes_expected_change(e, two)), "minor")
  expect_identical(as.character(.qes_expected_change(two, two)), "none")
  bumped <- two
  k <- which(bumped$source_var == "Q31A")[1]
  bumped$n[k] <- bumped$n[k] + 5L
  ch <- .qes_expected_change(two, bumped)
  expect_identical(as.character(ch), "major")
  expect_match(attr(ch, "keys"), "^qes2014\\|post\\|lr_self\\|Q31A\\|")
  # and the validator does not see duplicates in it
  s <- hz_spec()
  s$tables$expected <- two
  p <- .qes_spec_check(s)
  expect_false(any(p$rule == "V-S2" & p$table == "expected"))
})

test_that("gates.csv and expected/ are checked for form by the validator", {
  s <- hz_spec()
  s$tables$gates$source_code[1] <- "01"
  s$tables$expected$na_reason[1] <- "dk"
  s$tables$expected <- rbind(s$tables$expected, s$tables$expected[2, ])
  s$tables$gates$source_var[2] <- "q999"
  p <- .qes_spec_check(s)
  expect_true(all(c("V-S1", "V-S2", "V-S4") %in% p$rule[p$table %in% c("gates", "expected")]))
  # cells counted under a gate the row no longer has, or under another
  # membership rule, are stale (V-S4)
  s <- hz_spec()
  i <- hz_xw_row(s$tables, "qes2012", "vote_prov_recall")
  s$tables$crosswalk$gate_var[i] <- NA
  s$tables$crosswalk$gate_codes[i] <- NA
  s$tables$crosswalk$gate_to[i] <- NA
  p <- .qes_spec_check(s)
  expect_true(any(p$rule == "V-S4" & p$table == "gates" & grepl("^qes2012/post/q25/q21/", p$key)))
  s <- hz_spec()
  w <- which(s$tables$waves$study == "qes2007_panel" & s$tables$waves$wave == "post")
  s$tables$waves$member_codes[w] <- "CO;X"
  p <- .qes_spec_check(s)
  p <- p[p$rule == "V-S4" & p$table == "gates", ]
  expect_true(nrow(p) > 0L && all(grepl("membership rule", p$detail)))
})

test_that("the projection does not depend on the locale", {
  s <- hz_spec()
  ref <- .qes_project_marginals(s, .qes_hz_sources_shipped(s))
  syn <- .qes_synthetic("qes2018_panel", spec = s)
  ref_cells <- .qes_hz_sources_data(s, syn)$gates
  withr::local_envvar(LANGUAGE = "fr")
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    expect_identical(.qes_project_marginals(s, .qes_hz_sources_shipped(s)), ref)
    expect_identical(.qes_hz_sources_data(s, syn)$gates, ref_cells)
  })
})
