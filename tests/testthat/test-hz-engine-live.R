# Tier 1 (live) for the harmonization engine (design.md sections 5.10, 8.2
# and 11, slices HZ3 and HZ4): V-L1 and V-L3 on the pinned originals, and
# the waves, weights and eligibility they give. The engine's column
# hashes equal expected/hashes.csv for every study, and its marginals among
# each wave's members equal expected/marginals.csv for the studies whose
# metadata ships (qes2022's are checked in CI by data-raw/build_hashes.R).
# Never runs on CRAN; see helper-live.R.

test_that("V-L1: engine hashes and marginals on the originals equal expected/ (live)", {
  local_live_originals()
  s <- hz_spec()
  # every target but the leading-column one (survey_mode), checked below
  h <- qes_harmonize("all", targets = setdiff(s$tables$targets$target, .qes_hz_leading_targets(s)),
                     values = "code", missing = "reasons", include_draft = TRUE, quiet = TRUE)
  # every study the spec covers, every row kept
  cat_ <- .qes_catalog()$files
  cat_ <- cat_[cat_$role == "data" & cat_$is_default %in% TRUE, ]
  for (study in unique(h$study)) {
    expect_identical(sum(h$study == study), cat_$n_rows[cat_$study == study], label = study)
  }
  expect_false(anyDuplicated(h$qes_id) > 0L)
  expect_true(all(qes_provenance(h)$md5_verified))
  # column hashes
  got <- .qes_hz_hashes(h)
  expect_identical(got, s$tables$hashes)
  # marginals among the members of each row's wave
  cell <- qes_provenance(h, level = "cell")
  shipped <- .qes_catalog()$studies
  shipped <- shipped$study[shipped$metadata_shipped %in% TRUE]
  cell <- cell[cell$included & cell$rule %in% c("map", "numeric") & cell$study %in% shipped, ]
  for (k in seq_len(nrow(cell))) {
    in_s <- h$study == cell$study[k]
    member <- .qes_hz_waves_member(h$waves[in_s], cell$wave[k])
    v <- h[[cell$target[k]]][in_s][member]
    v <- if (is.numeric(v)) .qes_code_chr(v) else v
    r <- as.character(h[[paste0(cell$target[k], "__na")]][in_s][member])
    got_n <- table(ifelse(is.na(v), paste0("NA:", r), v))
    e <- s$tables$expected
    e <- e[e$study == cell$study[k] & e$target == cell$target[k] & e$wave == cell$wave[k], ]
    exp_n <- stats::setNames(e$n, ifelse(is.na(e$value), paste0("NA:", e$na_reason), e$value))
    expect_identical(as.integer(got_n[names(exp_n)]), unname(exp_n), label = paste(cell$study[k], cell$target[k]))
    expect_identical(sum(got_n), sum(exp_n), label = paste(cell$study[k], cell$target[k]))
  }
})

test_that("known counts on the originals (live)", {
  local_live_originals()
  h <- qes_harmonize(c("qes2018", "qes2014", "qes2022"), values = "code", missing = "reasons",
                     include_draft = TRUE, quiet = TRUE)
  q18 <- h[h$study == "qes2018", ]
  # 2018 q6: 490 PLQ, 392 PQ, 700 CAQ, 342 QS, 31 spoiled, 92 other, 160 refused
  expect_identical(as.vector(table(q18$vote_prov_recall)[c("PLQ", "PQ", "CAQ", "QS", "other")]),
                   c(490L, 392L, 700L, 342L, 92L))
  expect_identical(sum(q18$vote_prov_recall__na %in% "spoiled"), 31L)
  expect_identical(sum(!is.na(q18$lr_self)), 2490L)
  expect_identical(as.vector(table(q18$interest_4pt)[c("very", "quite", "hardly", "not_at_all")]),
                   c(743L, 1422L, 687L, 160L))
  q14 <- h[h$study == "qes2014", ]
  expect_identical(sum(!is.na(q14$lr_self)), 1195L)
  expect_identical(as.vector(table(q14$pid_prov)[c("PLQ", "PQ", "CAQ", "QS", "ON", "PVQ", "none")]),
                   c(480L, 378L, 192L, 113L, 10L, 29L, 181L))
  q22 <- h[h$study == "qes2022", ]
  expect_identical(sum(q22$turnout_prov_recall %in% "yes"), 1109L)
  expect_identical(sum(q22$turnout_prov_recall__na %in% "not_registered"), 3L)
  expect_identical(sum(q22$turnout_prov_recall__na %in% "dk"), 2L)
  expect_identical(sum(q22$turnout_prov_recall__na %in% "not_in_wave"), 301L)
  expect_identical(sum(q22$vote_prov_intent__na %in% "inapplicable"), 89L)
  expect_equal(mean(q22$lr_self, na.rm = TRUE), 4.96, tolerance = 0.005)
})

test_that("V-L3 and the waves, weights and eligibility of the originals (live)", {
  local_live_originals()
  h <- qes_harmonize("all", missing = "reasons", include_draft = TRUE, quiet = TRUE)
  by <- function(study, x) x[h$study == study]
  # V-L3: the 2018 weight reproduces the sex x French margins of the
  # methodological report (file 361045, Table 14: 0.376, 0.112, 0.395, 0.117)
  y <- .qes_join_raw(h[h$study == "qes2018", ], c("qsexe", "qlangue"))
  w <- y$weight_post
  share <- function(sex, french) {
    sum(w[unclass(y$qes2018__qsexe) == sex & (unclass(y$qes2018__qlangue) %in% 1) == french]) / sum(w)
  }
  expect_lt(abs(share(1, TRUE) - 0.376), 0.005)
  expect_lt(abs(share(1, FALSE) - 0.112), 0.005)
  expect_lt(abs(share(2, TRUE) - 0.395), 0.005)
  expect_lt(abs(share(2, FALSE) - 0.117), 0.005)
  # reviewed weights have mean 1 in each wave; those that need review are NA
  for (st in c("qes2012", "qes2014", "qes2018")) {
    expect_equal(mean(by(st, h$weight_post)), 1, label = st)
  }
  expect_equal(mean(by("qes2022", h$weight_pre)), 1)
  expect_identical(sum(!is.na(by("qes2022", h$weight_post))), 1220L)
  expect_equal(mean(by("qes2022", h$weight_post), na.rm = TRUE), 1)
  for (st in c("qes2007_panel", "qes2012_panel", "qes2018_panel")) {
    expect_true(all(is.na(by(st, h$weight_pre))) && all(is.na(by(st, h$weight_post))), label = st)
  }
  # eligibility: 2018 sampled ages 16 and over; the other samples are adults
  expect_identical(as.vector(table(factor(by("qes2018", h$eligible_voter), c(FALSE, TRUE)), useNA = "always")),
                   c(255L, 2799L, 18L))
  expect_true(all(by("qes2018", h$eligible_voter)[unclass(get_qes("qes2018", assign_global = FALSE, quiet = TRUE)$ageyear_1) %in% c(2001, 2002)] %in% FALSE))
  expect_true(all(by("qes2022", h$eligible_voter)))
  expect_true(all(by("qes2014", h$eligible_voter)))
  expect_identical(sum(is.na(by("qes2012", h$eligible_voter))), 21L)
  expect_true(all(by("qes2012_panel", h$eligible_voter)))
  expect_true(all(by("qes2018_panel", h$eligible_voter)))
  # the 2007 panel's age bands in whichever wave the respondent answered
  # (spec 1.0.0): 2,049 pre-wave respondents, and 304 of the 391 reached
  # only after the election (those aged 18-34 then may have been 17 on
  # election day)
  elig <- by("qes2007_panel", h$eligible_voter)
  expect_identical(sum(elig %in% TRUE), 2353L)
  expect_identical(sum(elig[!grepl("pre", by("qes2007_panel", h$waves))] %in% TRUE), 304L)
  # interview dates and modes
  expect_identical(range(by("qes2022", h$interview_date)), as.Date(c("2022-09-19", "2022-09-23")))
  expect_identical(range(by("qes2007_panel", h$interview_date), na.rm = TRUE), as.Date(c("2007-03-01", "2007-04-13")))
  expect_identical(as.vector(table(by("qes2018_panel", h$survey_mode))[c("phone", "web")]), c(400L, 850L))
  # the long layout: one row per respondent and wave
  l <- qes_harmonize(c("qes2022", "qes2007_panel", "qes2018_panel"), targets = "vote_prov_recall",
                     layout = "long", include_draft = TRUE, quiet = TRUE)
  expect_identical(as.vector(table(l$study, l$wave, useNA = "ifany")["qes2022", c("cps", "pes")]), c(1521L, 1220L))
  expect_identical(sum(l$study == "qes2007_panel"), 2050L + 2054L + 1L)
  expect_identical(sum(l$study == "qes2018_panel"), 1250L + 842L)
  expect_equal(mean(l$weight[l$study == "qes2022" & l$wave == "pes"]), 1)
})

test_that("the remaining studies on the originals: counts, waves, strata and eligibility (live)", {
  local_live_originals()
  studies <- c("qes2007", "qes2008", "qes2012_panel", "qes_crop_2007_2010", "qes1998", "qes2007_panel")
  h <- qes_harmonize(studies, values = "code", missing = "reasons", include_draft = TRUE, quiet = TRUE)
  by <- function(study, col) h[[col]][h$study == study]
  n_of <- function(study, col, levels) as.vector(table(factor(by(study, col), levels)))
  reasons <- function(study, col) table(as.character(by(study, paste0(col, "__na"))))
  # qes2007: q12 among the 1,990 voters; 97 (none) spoiled, 95 not voted
  expect_identical(n_of("qes2007", "vote_prov_recall", c("PLQ", "PQ", "ADQ", "QS", "PVQ", "other")),
                   c(439L, 521L, 583L, 75L, 101L, 8L))
  r <- reasons("qes2007", "vote_prov_recall")
  expect_identical(as.vector(r[c("not_voted", "spoiled", "dk", "refused")]), c(176L, 12L, 8L, 252L))
  expect_identical(n_of("qes2007", "sov_partnership_1995", c("yes", "no", "would_not_vote")), c(811L, 1122L, 31L))
  expect_identical(as.vector(table(by("qes2007", "survey_mode"))[c("phone", "web")]), c(1003L, 1172L))
  expect_identical(as.vector(table(by("qes2007", "eligible_voter"), useNA = "always")), c(2133L, 42L))
  # qes2008
  expect_identical(n_of("qes2008", "vote_prov_recall", c("PLQ", "PQ", "ADQ", "QS", "PVQ", "other")),
                   c(352L, 336L, 144L, 38L, 22L, 6L))
  expect_identical(n_of("qes2008", "age_group3", c("a18_34", "a35_54", "a55_plus")), c(266L, 484L, 401L))
  expect_identical(n_of("qes2008", "turnout_prov_recall", c("yes", "no")), c(986L, 145L))
  expect_identical(sum(is.na(by("qes2008", "eligible_voter"))), 9L)
  # qes2012_panel: recall and turnout of the post-election wave
  expect_identical(n_of("qes2012_panel", "turnout_prov_recall", c("yes", "no")), c(778L, 66L))
  expect_identical(n_of("qes2012_panel", "vote_prov_recall", c("CAQ", "PLQ", "PQ", "QS", "PVQ", "ON", "other")),
                   c(151L, 167L, 243L, 45L, 10L, 16L, 1L))
  expect_identical(as.vector(reasons("qes2012_panel", "vote_prov_recall")[c("not_voted", "spoiled", "dk_refused")]),
                   c(66L, 7L, 138L))
  # CROP: 24 polls, the first question and the age bands
  expect_identical(n_of("qes_crop_2007_2010", "vote_prov_intent", c("ADQ", "PLQ", "PQ", "QS", "PVQ", "other", "no_party")),
                   c(3295L, 6479L, 6609L, 1102L, 1136L, 70L, 1605L))
  expect_identical(sum(by("qes_crop_2007_2010", "vote_prov_intent_push__na") %in% "not_mappable"), 1179L)
  expect_identical(n_of("qes_crop_2007_2010", "age_group3", c("a18_34", "a35_54", "a55_plus")), c(5323L, 10188L, 8466L))
  expect_identical(length(unique(by("qes_crop_2007_2010", "waves"))), 24L)
  expect_identical(length(unique(by("qes_crop_2007_2010", "stratum"))), 24L)
  expect_identical(by("qes_crop_2007_2010", "stratum"), by("qes_crop_2007_2010", "waves"))
  expect_identical(sort(unique(by("qes_crop_2007_2010", "year"))), 2007:2010)
  expect_identical(as.vector(table(by("qes_crop_2007_2010", "election_date"))), c(15019L, 9008L))
  expect_identical(as.vector(table(by("qes_crop_2007_2010", "eligible_voter"), useNA = "always")), c(23977L, 50L))
  # qes1998: two firms, francophones only
  expect_identical(as.vector(table(by("qes1998", "stratum"))[c("1", "2")]), c(1057L, 426L))
  expect_identical(n_of("qes1998", "vote_prov_recall", c("ADQ", "PLQ", "PQ", "other")), c(204L, 384L, 517L, 21L))
  expect_identical(sum(by("qes1998", "vote_prov_intent_push__na") %in% "inapplicable"), 79L)
  expect_identical(n_of("qes1998", "sov_partnership_1995", c("yes", "no", "would_not_vote")), c(159L, 212L, 8L))
  expect_identical(sum(by("qes1998", "sov_partnership_1995__na") %in% "inapplicable"), 1057L)
  expect_true(all(grepl("francophone", by("qes1998", "target_population"), ignore.case = TRUE)))
  # qes2007_panel: the 1995 question in the pre-election wave
  expect_identical(n_of("qes2007_panel", "sov_partnership_1995", c("yes", "no", "would_not_vote")), c(821L, 1014L, 29L))
  # the post-election interviews of the 2012 panel are dated
  l <- qes_harmonize("qes2012_panel", targets = "vote_prov_recall", layout = "long", include_draft = TRUE, quiet = TRUE)
  expect_identical(range(l$interview_date[l$wave == "post"]), as.Date(c("2012-09-10", "2012-09-17")))
  expect_true(all(is.na(l$interview_date[l$wave == "pre"])))
  # every weight of these studies needs review: NA
  expect_true(all(is.na(h$weight_pre) & is.na(h$weight_post)))
})
