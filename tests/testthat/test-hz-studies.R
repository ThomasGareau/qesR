# The remaining studies of the spec (design.md sections 5.3, 5.5 and 5.7,
# slice HZ5): qes2007, qes2008, qes2012_panel, the pooled CROP polls of
# 2007-2010 (one wave per poll) and the 1998 panel (two firms, francophones
# only). Offline, on the shipped spec and on synthetic data built from it;
# the counts of the originals are checked by test-hz-engine-live.R.

hz_run <- function(data, ..., include_draft = TRUE, quiet = TRUE) {
  withCallingHandlers(
    qes_harmonize(data = data, ..., include_draft = include_draft, quiet = quiet),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning")
  )
}
crop <- "qes_crop_2007_2010"

# A copy of the shipped spec whose weights of `study` are marked reviewed,
# so that the engine applies them.
reviewed_weights <- function(study, s = hz_spec()) {
  k <- s$tables$weights$study == study
  s$tables$weights$status[k] <- "reviewed"
  s
}

# The shipped spec with small CROP polls (20 to 43 respondents each, not
# about 1,000), so that synthetic CROP data stay small. The polls are coded
# 101 to 124 and their gates.csv cells dropped, so that the offline checks
# do not compare these sizes with the counts of the real file.
small_polls <- function(s = hz_spec()) {
  k <- which(s$tables$waves$study == crop)
  s$tables$waves$n_cases[k] <- 19L + seq_along(k)
  s$tables$waves$member_codes[k] <- as.character(100L + seq_along(k))
  s$tables$gates <- s$tables$gates[s$tables$gates$study != crop, , drop = FALSE]
  s
}

test_that("the spec covers the remaining studies, and not the firms' own 1998 files", {
  s <- hz_spec()
  covered <- .qes_hz_covered(s)
  expect_true(all(c("qes2007", "qes2008", "qes2012_panel", crop, "qes1998") %in% covered))
  expect_false(any(c("qes1998_crop", "qes1998_createc") %in% covered))
  expect_identical(hz_errors(.qes_spec_check(s)), character(0))
  expect_identical(hz_errors(.qes_data_check(s, .qes_hz_sources_shipped(s))), character(0))
  # the default studies are the Quebec Election Studies only
  expect_false(any(c(crop, "qes1998", "qes2012_panel") %in% .qes_hz_resolve_studies(NULL, NULL, s)))
})

test_that("the pooled CROP polls have one wave per poll, each with its election", {
  wv <- hz_spec()$tables$waves
  w <- wv[wv$study == crop, ]
  expect_identical(nrow(w), 24L)
  expect_true(all(w$wave_design == "poll_wave" & w$wave_timing == "pre" & w$mode == "phone"))
  expect_identical(w$member_var, rep("projet", 24L))
  expect_identical(w$member_codes, as.character(1:24))
  expect_identical(sum(w$n_cases), 24027L)
  expect_identical(w$wave[c(1, 15, 16, 24)], c("poll_2007_06", "poll_2008_11", "poll_2009_01", "poll_2010_01"))
  # the polls up to November 2008 refer to the election of 2008-12-08, the
  # later ones to the next, of 2012-09-04
  expect_identical(w$election_ref, rep(c("QC2008", "QC2012"), c(15L, 9L)))
  # each poll is an independent sample: a stratum of its own
  expect_identical(unique(w$strata_var), "projet")
  # the crosswalk and weight rows name every poll at once, with wave "*"
  xw <- hz_spec()$tables$crosswalk
  expect_true(all(xw$wave[xw$study == crop] == "*"))
  expect_true(all(is.na(xw$election_ref[xw$study == crop])))
  wt <- hz_spec()$tables$weights
  expect_identical(wt$wave[wt$study == crop], "*")
  expect_identical(wt$status[wt$study == crop], "needs_review")
})

test_that("wave * is allowed only in a study of poll waves or for a static or any-time target, and checked like the waves it names", {
  s <- hz_spec()
  # in a study whose waves are not poll waves, for a target tied to an
  # election period
  t <- s$tables
  i <- hz_xw_row(t, "qes2014", "vote_prov_recall")
  t$crosswalk$wave[i] <- "*"
  s1 <- s
  s1$tables <- t
  p <- .qes_spec_check(s1)
  expect_true(any(p$rule == "V-S4" & grepl("poll waves", p$detail)))
  # a time-invariant item asked in whichever wave (the 2007 panel's gender)
  expect_identical(t$crosswalk$wave[hz_xw_row(t, "qes2007_panel", "gender")], "*")
  expect_false(any(.qes_spec_check(s)$severity == "error"))
  # a row of wave * takes its election from each wave, not from election_ref
  s2 <- s
  i <- hz_xw_row(s2$tables, crop, "vote_prov_intent")
  s2$tables$crosswalk$election_ref[i] <- "QC2008"
  p <- .qes_spec_check(s2)
  expect_true(any(p$rule == "V-S9" & grepl("leave election_ref empty", p$detail)))
  s3 <- s
  s3$tables$waves$election_ref[s3$tables$waves$study == crop][3] <- NA
  p <- .qes_spec_check(s3)
  expect_true(any(p$rule == "V-S9" & grepl("every wave", p$detail)))
  # weights are registered for wave * or wave by wave, not both
  s4 <- s
  extra <- s4$tables$weights[s4$tables$weights$study == crop, ]
  extra$wave <- "poll_2007_06"
  extra$recommended <- FALSE
  s4$tables$weights <- rbind(s4$tables$weights, extra)
  p <- .qes_spec_check(s4)
  expect_true(any(p$rule == "V-S4" & grepl("not both", p$detail)))
  # the membership rule of wave * is the polls' rule, which gates.csv records
  wv <- s$tables$waves
  expect_identical(.qes_member_rule_rows(wv, .qes_wave_rows(wv, "*", crop)),
                   paste0("projet=", paste(1:24, collapse = ";")))
  expect_identical(.qes_wave_rows(wv, "pre", "qes1998"), which(wv$study == "qes1998" & wv$wave == "pre"))
  expect_true(.qes_poll_study(wv, crop))
  expect_false(.qes_poll_study(wv, "qes1998"))
  g <- s$tables$gates
  expect_identical(unique(g$member_rule[g$study == crop]), paste0("projet=", paste(1:24, collapse = ";")))
})

test_that("pooled polls: one row per respondent in their poll, strata and elections by poll", {
  s <- reviewed_weights(crop, small_polls())
  syn <- .qes_synthetic(crop, spec = s)
  d <- syn[[crop]]
  n <- sum(20:43)
  # synthetic polls are disjoint blocks of the polls' sizes
  expect_identical(nrow(d), n)
  expect_identical(as.vector(table(factor(unclass(d$projet), 101:124))), s$tables$waves$n_cases[s$tables$waves$study == crop])
  expect_identical(hz_errors(.qes_data_check_frames(s, syn)), character(0))
  h <- hz_run(syn, targets = c("vote_prov_intent", "vote_prov_intent_push", "age_group3"),
              missing = "reasons", spec = s)
  expect_identical(nrow(h), n)
  poll <- unclass(d$projet) - 100
  waves <- s$tables$waves[s$tables$waves$study == crop, ]
  expect_identical(h$waves, waves$wave[poll])
  # the stratum is the poll, by its wave name; the year is the poll's
  expect_identical(h$stratum, waves$wave[poll])
  expect_identical(h$year, as.integer(substr(waves$wave[poll], 6L, 9L)))
  expect_identical(sort(unique(h$year)), 2007:2010)
  expect_identical(h$election_date, as.Date(ifelse(poll <= 15, "2008-12-08", "2012-09-04")))
  expect_true(all(is.na(h$interview_date)))
  # a question of wave "*" is asked in every poll: no respondent is not_in_wave
  expect_false(any(h$vote_prov_intent__na %in% "not_in_wave"))
  expect_true(any(h$vote_prov_intent_push__na %in% "not_mappable"))
  # the weight is normalized within each poll, and the guide names it
  expect_equal(as.vector(tapply(h$weight_pre, poll, mean)), rep(1, 24))
  expect_identical(unique(h$weight_pre_var), "XPOND")
  expect_true(all(is.na(h$weight_post)))
  guide <- attr(h, "qes_weight_guide")
  expect_identical(unique(guide$weight_column), "weight_pre")
  expect_identical(unique(guide$wave), "*")
  cell <- qes_provenance(h, "cell")
  expect_identical(unique(cell$wave), "*")
  expect_identical(unique(cell$weight_var), "XPOND")
  expect_equal(unique(cell$weight_mean_raw), 1)
  # the long layout: one row per respondent (each is in one poll), the value
  # on it
  l <- hz_run(syn, targets = "vote_prov_intent", layout = "long", missing = "reasons", spec = s)
  expect_identical(nrow(l), n)
  expect_identical(l$wave, waves$wave[poll])
  expect_identical(as.character(l$vote_prov_intent__na), as.character(h$vote_prov_intent__na))
  expect_identical(l$election_date, h$election_date)
  expect_identical(l$stratum, h$stratum)
  expect_identical(l$year, h$year)
  expect_equal(as.vector(tapply(l$weight, l$wave, mean)), rep(1, 24))
  # the design: one stratum per poll
  skip_if_not_installed("survey")
  des <- qes_design(h, weight = "weight_pre")
  expect_identical(sort(unique(des$variables$qes_stratum)), sort(paste(crop, waves$wave, sep = ":")))
})

test_that("pool = \"equal\" counts the polls of pooled polls as one study in the long layout", {
  skip_if_not_installed("survey")
  s <- reviewed_weights("qes2007", reviewed_weights(crop, small_polls()))
  syn <- c(.qes_synthetic(crop, spec = s), .qes_synthetic("qes2007", spec = s))
  h <- hz_run(syn, studies = c(crop, "qes2007"), targets = c("age_group3", "vote_prov_recall"),
              layout = "long", spec = s)
  des <- qes_design(h, weight = "weight", pool = "equal")
  tot <- tapply(stats::weights(des), des$variables$study, sum)
  # one total per study, not one per poll: 24 polls would get 24 shares
  expect_equal(unname(tot[crop]), unname(tot["qes2007"]))
  expect_equal(sum(tot), nrow(des$variables))
})

test_that("a respondent in two poll waves fails V-D8", {
  s <- small_polls()
  syn <- .qes_synthetic(crop, spec = s)
  # move a respondent of the first poll to the second: two polls now have
  # the wrong size; the disjointness check needs a row in two polls, which a
  # single membership variable cannot give, so give the polls two variables
  s2 <- s
  w <- which(s2$tables$waves$study == crop)[2]
  s2$tables$waves$member_var[w] <- "projet2"
  d <- syn[[crop]]
  d$projet2 <- ifelse(unclass(d$projet) == 102, 102, 0)
  d$projet2[1] <- 102
  p <- .qes_data_check_frames(s2, stats::setNames(list(d), crop))
  expect_true(any(p$rule == "V-D8" & grepl("two or more poll waves", p$detail)))
})

test_that("the 1998 panel pools two firms, francophones only, with the firm as a stratum", {
  s <- hz_spec()
  wv <- s$tables$waves[s$tables$waves$study == "qes1998", ]
  expect_identical(wv$wave, c("pre", "post"))
  expect_identical(wv$n_cases, c(1483L, 1483L))
  expect_identical(unique(wv$strata_var), "firme_post")
  expect_identical(unique(wv$subsample_var), "firme_post")
  expect_true(all(grepl("francophone", wv$target_population_en, ignore.case = TRUE)))
  expect_true(all(grepl("francophones", wv$target_population_fr)))
  xw <- s$tables$crosswalk
  vm <- s$tables$valuemaps
  reason <- function(map, code) vm$na_reason[vm$map_id == map & vm$source_code == code]
  target <- function(map, code) vm$target_code[vm$map_id == map & vm$source_code == code]
  # intvote is the pushed intention (q7a + q7b, r3 + r4): never vote_prov_intent
  i <- which(xw$study == "qes1998" & xw$source_var == "intvote")
  expect_identical(xw$target[i], "vote_prov_intent_push")
  expect_false("vote_prov_intent" %in% xw$target[xw$study == "qes1998"])
  expect_identical(reason("vote_qes1998_intvote", "10"), "inapplicable")
  expect_identical(target("vote_qes1998_intvote", "6"), 95L)
  # intvote2 has shifted labels: documented, never mapped
  i <- which(xw$study == "qes1998" & xw$source_var == "intvote2")
  expect_identical(c(xw$rule[i], xw$grade[i]), c("none", "not_comparable"))
  expect_false(xw$primary[i])
  # the 1995 question was asked by CROP only
  i <- which(xw$study == "qes1998" & xw$target == "sov_partnership_1995")
  expect_identical(c(xw$source_var[i], xw$gate_var[i], xw$gate_to[i]), c("q16a_crop", "firme_post", "1=inapplicable"))
  # unlabelled age codes take the CREATEC file's labels, as a supplement
  k <- vm$map_id == "age3_qes1998_age" & vm$source_code %in% c("6", "9")
  expect_identical(vm$source_label_origin[k], c("supplement", "supplement"))
  # the weights are registered, and all need review
  wt <- s$tables$weights[s$tables$weights$study == "qes1998", ]
  expect_true(all(wt$status == "needs_review"))
  expect_identical(wt$weight_var[wt$recommended], c("ponder3", "ponder3"))
})

test_that("the 1998 firms are strata of the design", {
  skip_if_not_installed("survey")
  s <- reviewed_weights("qes1998")
  syn <- .qes_synthetic("qes1998", spec = s)
  syn$qes1998$firme_post <- rep_len(c(1, 2), nrow(syn$qes1998))
  syn$qes1998$q16a_crop[syn$qes1998$firme_post == 1] <- NA
  h <- hz_run(syn, targets = c("vote_prov_recall", "turnout_prov_recall"), spec = s)
  # the firm's code in the file: "1" = CREATEC, "2" = CROP
  expect_identical(h$stratum, as.character(syn$qes1998$firme_post))
  expect_identical(sort(unique(h$stratum)), c("1", "2"))
  expect_true(all(h$year == 1998L))
  expect_identical(h$subsample, h$stratum)
  des <- qes_design(h, weight = "weight_post")
  expect_identical(sort(unique(des$variables$qes_stratum)), c("qes1998:1", "qes1998:2"))
  # a study drawn as one sample has no stratum
  syn2 <- .qes_synthetic("qes2014", spec = hz_spec())
  h2 <- hz_run(syn2, targets = "vote_prov_recall")
  expect_true(all(is.na(h2$stratum)))
})

test_that("real-code decisions of the remaining studies hold in the shipped spec", {
  t <- hz_spec()$tables
  xw <- t$crosswalk
  vm <- t$valuemaps
  reason <- function(map, code) vm$na_reason[vm$map_id == map & vm$source_code == code]
  target <- function(map, code) vm$target_code[vm$map_id == map & vm$source_code == code]
  # 2007 and 2008: code 3 is the ADQ (party_qc 8), never the CAQ; "none"
  # among voters is a spoiled or blank ballot
  expect_identical(target("vote_qes2007_q12", "3"), 8L)
  expect_identical(target("vote_qes2008_q12a", "3"), 8L)
  expect_identical(reason("vote_qes2007_q12", "97"), "spoiled")
  expect_identical(reason("vote_qes2008_q12a", "97"), "spoiled")
  expect_identical(reason("vote_qes2007_q12", "95"), "not_voted")
  # the 2007 interview mode varies by respondent (type 1 telephone, 2 web)
  expect_identical(t$waves$mode[t$waves$study == "qes2007"], "var:type")
  expect_identical(c(target("mode_qes2007_type", "1"), target("mode_qes2007_type", "2")), c(2L, 1L))
  # 2008: seven age bands collapse exactly into three
  expect_identical(vapply(as.character(2:8), function(k) target("age3_qes2008_q0age", k), integer(1)),
                   c(1L, 1L, 2L, 2L, 3L, 3L, 3L), ignore_attr = TRUE)
  # 2012 panel: ON is code 1 of the intention items and 6 of the recall;
  # 9 of the recall is one code for don't know and refusal
  expect_identical(target("vote_qes2012p_intvoteprov1", "1"), 7L)
  expect_identical(target("vote_qes2012p_voteprov", "6"), 7L)
  expect_identical(reason("vote_qes2012p_voteprov", "9"), "dk_refused")
  expect_identical(reason("vote_qes2012p_voteprov", "0"), "not_voted")
  expect_identical(reason("vote_qes2012p_intvoteprov1", "98"), "refused")
  expect_identical(reason("vote_qes2012p_intvoteprov1", "99"), "dk")
  i <- which(xw$study == "qes2012_panel" & xw$source_var == "interetrec")
  expect_identical(c(xw$rule[i], xw$grade[i]), c("none", "not_comparable"))
  # the post wave of the 2012 panel is dated by its last call
  wv <- t$waves
  expect_identical(wv$date_var[wv$study == "qes2012_panel"], c(NA, "ResLastCallDate_last"))
  # CROP: would not vote is an answer in the first question; the combined
  # item's code 7 straddles no party and another party
  expect_identical(target("vote_crop_intvoteprova", "7"), 95L)
  expect_identical(reason("vote_crop_intvoteprova", "9"), "dk_refused")
  expect_identical(reason("vote_crop_intvoteprov", "7"), "not_mappable")
  # CROP's referendum item has no documented wording: not in the spec
  expect_false(any(c("intvoterefa", "intvoteref", "QP4", "voteprec") %in% xw$source_var[xw$study == crop]))
})

test_that("the 1995 sovereignty-partnership question is its own target", {
  t <- hz_spec()$tables
  tg <- t$targets[t$targets$target == "sov_partnership_1995", ]
  expect_identical(tg$family, "sovereignty")
  expect_identical(tg$levels_id, "referendum")
  expect_identical(tg$anchor_row, "qes2007:post:q19")
  xw <- t$crosswalk[t$crosswalk$target == "sov_partnership_1995", ]
  expect_setequal(xw$study, c("qes2007", "qes2008", "qes1998", "qes2007_panel"))
  # the push questions (q20, intref2, q16b_crop) and the combined items are
  # never rows of the target
  expect_false(any(c("q20", "intref2", "intref", "q16b_crop", "voteref") %in% xw$source_var))
  # never pooled with the independent-country or sovereign-country items
  expect_false(any(t$crosswalk$source_var[t$crosswalk$target %in% c("sov_indep", "sov_sovereign_country")] %in% xw$source_var))
})

test_that("synthetic data of the remaining studies pass the data checks and harmonize", {
  s <- hz_spec()
  studies <- c("qes2007", "qes2008", "qes2012_panel", "qes1998")
  syn <- .qes_synthetic(studies, spec = s)
  expect_identical(hz_errors(.qes_data_check_frames(s, syn)), character(0))
  h <- hz_run(syn, missing = "reasons")
  expect_identical(as.vector(table(h$study)[studies]), vapply(syn, nrow, integer(1), USE.NAMES = FALSE))
  expect_setequal(unique(h$survey_mode[h$study == "qes2007"]), c("phone", "web"))
  expect_identical(unique(h$election_date[h$study == "qes1998"]), as.Date("1998-11-30"))
  expect_identical(unique(h$waves[h$study == "qes1998"]), "pre;post")
  # every value of a gated row has a reason
  expect_false(anyNA(h$vote_prov_recall__na[is.na(h$vote_prov_recall)]))
})

test_that("the reference, the coverage page and the weight notice summarize the polls", {
  s <- hz_spec()
  expect_identical(.qes_wave_label(c("*", "post"), "en"), c("each poll", "post"))
  expect_identical(.qes_wave_label("*", "fr"), "chaque sondage")
  ref <- .spec_reference_md("en", spec = s, targets = "vote_prov_intent", header = FALSE)
  expect_match(ref, "`intvoteprova` (each poll)", fixed = TRUE)
  expect_match(.spec_reference_md("fr", spec = s, targets = "vote_prov_intent", header = FALSE),
               "`intvoteprova` (chaque sondage)", fixed = TRUE)
  cov <- .spec_coverage_md("en", spec = s)
  expect_match(cov, "24 poll waves, poll_2007_06 to poll_2010_01 (n = 1,000 to 1,004 each)", fixed = TRUE)
  expect_false(grepl("poll_2008_03 (n =", cov, fixed = TRUE))
  expect_match(.spec_coverage_md("fr", spec = s), "24 vagues de sondage", fixed = TRUE)
  # the weight notice counts the polls
  syn <- .qes_synthetic(crop, spec = small_polls())
  m <- character(0)
  withCallingHandlers(
    hz_run(syn, targets = "vote_prov_intent", quiet = FALSE, spec = small_polls()),
    qesR_message_weight_review = function(c) {
      m <<- conditionMessage(c)
      invokeRestart("muffleMessage")
    },
    message = function(c) invokeRestart("muffleMessage")
  )
  expect_match(m, "qes_crop_2007_2010 poll_2007_06..poll_2010_01 (XPOND)", fixed = TRUE)
})
