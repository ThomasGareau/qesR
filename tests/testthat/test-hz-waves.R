# Waves, weights, eligibility, interview dates and modes of harmonized data
# (design.md sections 5.2, 5.3, 5.6 and 5.8, slice HZ4), end to end on the
# synthetic data of .qes_synthetic() and, for eligibility, on the rule
# itself.

hz_run <- function(data, ..., include_draft = TRUE, quiet = TRUE) {
  withCallingHandlers(
    qes_harmonize(data = data, ..., include_draft = include_draft, quiet = quiet),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    # the synthetic qes2022 has no value labels (the spec holds their hashes)
    qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning")
  )
}
hz_syn <- function(studies) .qes_synthetic(studies, spec = hz_spec())

lead_respondent <- c("study", "year", "election_date", "family", "study_design", "target_population",
                     "waves", "qes_id", "subsample", "stratum", "source_row", "survey_mode", "interview_date",
                     "days_to_election", "eligible_voter")
lead_long <- c("study", "year", "election_date", "family", "study_design", "target_population",
               "wave", "wave_timing", "wave_design", "qes_id", "subsample", "stratum", "source_row", "survey_mode",
               "interview_date", "days_to_election", "eligible_voter")

test_that("the respondent layout has the wave, date, mode, eligibility and weight columns", {
  syn <- hz_syn(c("qes2022", "qes2012"))
  h <- hz_run(syn, targets = c("vote_prov_recall", "vote_prov_intent", "birth_year"))
  expect_identical(names(h), c(lead_respondent, "vote_prov_recall", "vote_prov_intent", "birth_year",
                               "weight_pre", "weight_post", "weight_pre_var", "weight_post_var"))
  expect_s3_class(h$interview_date, "Date")
  expect_type(h$days_to_election, "integer")
  expect_type(h$eligible_voter, "logical")
  expect_type(h$weight_pre, "double")
  p <- h[h$study == "qes2022", ]
  # every respondent is in the campaign wave, 1,220 in the post-election wave
  expect_identical(as.vector(table(p$waves)[c("cps", "cps;pes")]), c(301L, 1220L))
  # the interview date is the first wave's: the date variable, as a date
  expect_identical(p$interview_date, as.Date(format(syn$qes2022$cps_StartDate, "%Y-%m-%d")))
  expect_identical(unique(p$days_to_election), as.integer(as.Date("2022-10-03") - unique(p$interview_date)))
  expect_identical(unique(p$survey_mode), "web")
  # weights: the recommended weight of each wave, mean 1 over its members
  expect_equal(mean(p$weight_pre), 1)
  in_pes <- grepl("pes", p$waves)
  expect_equal(mean(p$weight_post[in_pes]), 1)
  expect_true(all(is.na(p$weight_post[!in_pes])))
  expect_identical(unique(p$weight_pre_var), "cps_weight_general")
  expect_identical(unique(p$weight_post_var[in_pes]), "pes_weight_general")
  # a post-election cross-section has no pre-election weight, and no date
  q <- h[h$study == "qes2012", ]
  expect_true(all(is.na(q$weight_pre)) && all(is.na(q$weight_pre_var)))
  expect_equal(mean(q$weight_post), 1)
  expect_identical(unique(q$weight_post_var), "pond")
  expect_true(all(is.na(q$interview_date)) && all(is.na(q$days_to_election)))
  # raw weights are the deposited values
  raw <- hz_run(syn["qes2022"], targets = "vote_prov_intent", weights = "raw")
  expect_identical(raw$weight_pre, unclass(syn$qes2022$cps_weight_general))
})

test_that("weights that need review are NA, with a message; the guide says which weight fits", {
  syn <- hz_syn(c("qes2018_panel", "qes2012"))
  classes <- character(0)
  h <- withCallingHandlers(
    hz_run(syn, targets = c("vote_prov_intent", "vote_prov_recall", "lr_self"), quiet = FALSE),
    message = function(m) {
      classes <<- c(classes, class(m)[1])
      invokeRestart("muffleMessage")
    }
  )
  expect_true("qesR_message_weight_review" %in% classes)
  p <- h[h$study == "qes2018_panel", ]
  expect_true(all(is.na(p$weight_pre)) && all(is.na(p$weight_post)))
  expect_false(all(is.na(h$weight_post[h$study == "qes2012"])))
  guide <- attr(h, "qes_weight_guide")
  expect_identical(names(guide), c("target", "study", "wave", "target_timing", "weight_column",
                                   "weight_var", "weight_status"))
  g <- guide[guide$study == "qes2018_panel", ]
  expect_identical(g$weight_column[g$target == "vote_prov_intent"], "weight_pre")
  expect_identical(g$weight_column[g$target == "vote_prov_recall"], "weight_post")
  expect_identical(g$weight_var[g$target == "vote_prov_intent"], "weight")
  expect_identical(g$weight_status[g$target == "vote_prov_intent"], "needs_review")
  # a target the study has no question for has no weight
  g <- guide[guide$study == "qes2012", ]
  expect_true(is.na(g$weight_column[g$target == "vote_prov_intent"]))
  expect_identical(g$weight_column[g$target == "lr_self"], "weight_post")
  # printing lists the weights awaiting review
  withr::local_options(qesR.lang = "en")
  expect_true(any(grepl("Weights awaiting review", capture.output(print(h)), fixed = TRUE)))
  # cell provenance records the weight's status and raw mean
  cell <- qes_provenance(h, level = "cell")
  k <- cell$study == "qes2018_panel" & cell$target == "vote_prov_intent"
  expect_identical(cell$weight_status[k], "needs_review")
  expect_equal(cell$weight_mean_raw[k], 1)
  expect_true(all(c("weight_status", "weight_mean_raw", "n_outside_universe") %in% names(cell)))
})

test_that("targets of waves with different weights are announced", {
  syn <- hz_syn("qes2022")
  classes <- character(0)
  withCallingHandlers(
    hz_run(syn, targets = c("vote_prov_intent", "vote_prov_recall"), quiet = FALSE),
    message = function(m) {
      classes <<- c(classes, class(m)[1])
      invokeRestart("muffleMessage")
    }
  )
  expect_true("qesR_message_weight_timing" %in% classes)
  classes <- character(0)
  withCallingHandlers(
    hz_run(syn, targets = "vote_prov_intent", quiet = FALSE),
    message = function(m) {
      classes <<- c(classes, class(m)[1])
      invokeRestart("muffleMessage")
    }
  )
  expect_false("qesR_message_weight_timing" %in% classes)
  # static targets do not count, and the long layout carries each wave's
  # weight on its rows: no notice
  for (args in list(list(targets = c("vote_prov_recall", "birth_year")),
                    list(targets = c("vote_prov_intent", "vote_prov_recall"), layout = "long"))) {
    classes <- character(0)
    withCallingHandlers(
      do.call(hz_run, c(list(syn), args, list(quiet = FALSE))),
      message = function(m) {
        classes <<- c(classes, class(m)[1])
        invokeRestart("muffleMessage")
      }
    )
    expect_false("qesR_message_weight_timing" %in% classes)
  }
})

test_that("the long layout has one row per respondent and wave", {
  syn <- hz_syn(c("qes2022", "qes2007_panel"))
  l <- hz_run(syn, targets = c("vote_prov_recall", "vote_prov_intent", "birth_year"),
              layout = "long", missing = "reasons", keep_source = TRUE)
  expect_s3_class(l, "qes_harmonized")
  expect_identical(names(l)[seq_along(lead_long)], lead_long)
  expect_identical(names(l)[(ncol(l) - 1L):ncol(l)], c("weight", "weight_var"))
  # rows: one per membership, and one for a respondent in no wave
  r <- hz_run(syn, targets = "vote_prov_recall")
  n_member <- vapply(strsplit(ifelse(is.na(r$waves), "", r$waves), ";", fixed = TRUE), length, integer(1))
  expect_identical(nrow(l), sum(pmax(n_member, 1L)))
  expect_false(anyDuplicated(paste(l$qes_id, l$wave)) > 0L)
  expect_true(all(is.na(l$wave[l$qes_id %in% r$qes_id[n_member == 0L]])))
  # the same respondents as the respondent layout, each with its source row
  expect_setequal(unique(l$qes_id), r$qes_id)
  # a value sits on the row of the wave that asked; other rows are not_in_wave
  p <- l[l$study == "qes2022", ]
  expect_true(all(p$vote_prov_recall__na[p$wave == "cps"] == "not_in_wave"))
  expect_true(all(p$vote_prov_intent__na[p$wave == "pes"] == "not_in_wave"))
  expect_true(all(is.na(p$vote_prov_intent__src[p$wave == "pes"])))
  # the year of birth does not change: it is on every row of the respondent
  cps <- p[p$wave == "cps", ]
  pes <- p[p$wave == "pes", ]
  expect_identical(pes$birth_year, cps$birth_year[match(pes$qes_id, cps$qes_id)])
  # each row has its wave's weight, date and timing
  expect_equal(mean(p$weight[p$wave == "cps"]), 1)
  expect_equal(mean(p$weight[p$wave == "pes"]), 1)
  expect_identical(unique(p$weight_var[p$wave == "pes"]), "pes_weight_general")
  expect_identical(unique(p$wave_timing[p$wave == "pes"]), "post")
  expect_true(all(p$days_to_election[p$wave == "pes"] < 0))
  expect_true(all(p$days_to_election[p$wave == "cps"] > 0))
  # eligibility is the respondent's, on each of their rows
  expect_identical(pes$eligible_voter, cps$eligible_voter[match(pes$qes_id, cps$qes_id)])
  # cell provenance is the same as in the respondent layout
  rr <- hz_run(syn, targets = c("vote_prov_recall", "vote_prov_intent", "birth_year"), missing = "reasons")
  expect_identical(qes_provenance(l, level = "cell"), qes_provenance(rr, level = "cell"))
  expect_identical(p$vote_prov_intent[p$wave == "cps"], rr$vote_prov_intent[rr$study == "qes2022"])
  # French labels change nothing but text
  fr <- hz_run(syn, targets = "vote_prov_recall", layout = "long", values = "code", lang = "fr")
  en <- hz_run(syn, targets = "vote_prov_recall", layout = "long", values = "code")
  expect_identical(as.character(fr$vote_prov_recall), as.character(en$vote_prov_recall))
  expect_identical(fr$wave, en$wave)
})

test_that("an empty long result keeps the long columns", {
  syn <- hz_syn("qes2014")
  bad <- syn
  bad$qes2014$Q19 <- NULL
  expect_warning(l <- hz_run(bad, layout = "long", on_fail = "skip"), class = "qesR_warning_partial")
  expect_identical(nrow(l), 0L)
  expect_identical(names(l), names(hz_run(syn, layout = "long")))
})

test_that("the interview mode varies by respondent where the wave says so", {
  syn <- hz_syn("qes2018_panel")
  h <- hz_run(syn, targets = "vote_prov_intent")
  method <- unclass(syn$qes2018_panel$method)
  expect_true(all(h$survey_mode[method %in% c(1, 2)] == "phone"))
  expect_true(all(h$survey_mode[method %in% 3] == "web"))
  l <- hz_run(syn, targets = "vote_prov_intent", layout = "long")
  expect_identical(unique(l$survey_mode[l$wave == "post"]), "mixed")
  # the mode row is a crosswalk row like any other: not applied until signed off
  h0 <- suppressMessages(withCallingHandlers(
    qes_harmonize(data = syn, targets = "vote_prov_intent", quiet = TRUE),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    qesR_warning_all_unreviewed = function(w) invokeRestart("muffleWarning")
  ))
  expect_true(all(is.na(h0$survey_mode)))
  # the spec requires a survey_mode row on the variable a var: mode names
  s <- hz_spec()
  s$tables$crosswalk <- s$tables$crosswalk[s$tables$crosswalk$target != "survey_mode", ]
  s$tables$valuemaps <- s$tables$valuemaps[s$tables$valuemaps$map_id != "mode_qes2018p_method", ]
  p <- .qes_spec_check(s)
  expect_true(any(p$rule == "V-S4" & p$table == "waves" & grepl("survey_mode", p$detail)))
})

test_that("eligibility follows age on election day and citizenship", {
  e <- as.Date("2018-10-01")
  cell <- function(value, timing = "post", reason = rep(NA_character_, length(value))) {
    list(value = as.character(value), reason = reason, timing = timing)
  }
  # year of birth, with the month when it is known
  by <- cell(c(1999, 2001, 2000, 2000, 2000, 2000, NA))
  bm <- cell(c(NA, NA, 5, 11, 10, NA, NA))
  expect_identical(.qes_hz_eligible(list(birth_year = by, birth_month = bm), 7L, e),
                   c(TRUE, FALSE, TRUE, FALSE, NA, NA, NA))
  expect_identical(.qes_hz_eligible(list(birth_year = by), 7L, e),
                   c(TRUE, FALSE, NA, NA, NA, NA, NA))
  # age at an interview before or after the election
  expect_identical(.qes_hz_eligible(list(age = cell(c(18, 17, 16), "pre")), 3L, e), c(TRUE, NA, FALSE))
  expect_identical(.qes_hz_eligible(list(age = cell(c(19, 18, 17), "post")), 3L, e), c(TRUE, NA, FALSE))
  # age bands
  bands <- cell(c("a18_34", "a35_54", "a55_plus", NA))
  expect_identical(.qes_hz_eligible(list(age_group3 = cell(bands$value, "pre")), 4L, e), c(TRUE, TRUE, TRUE, NA))
  expect_identical(.qes_hz_eligible(list(age_group3 = bands), 4L, e), c(NA, TRUE, TRUE, NA))
  # sources that disagree say nothing
  expect_identical(.qes_hz_eligible(list(birth_year = cell(2005), age = cell(40)), 1L, e), NA)
  # citizenship: "no" is not eligible; a missing answer to a question asked
  # leaves eligibility unknown
  cz <- cell(c("yes", "no", NA, NA), reason = c(NA, NA, "dk", "not_asked"))
  expect_identical(.qes_hz_eligible(list(age = cell(rep(40, 4)), citizen = cz), 4L, e), c(TRUE, FALSE, NA, TRUE))
  # no source: unknown
  expect_identical(.qes_hz_eligible(list(), 2L, e), c(NA, NA))
  expect_null(.qes_hz_band_bounds("young"))
})

test_that("eligibility reads the age targets whatever targets and min_grade ask for", {
  syn <- hz_syn(c("qes2018", "qes2018_panel"))
  h <- hz_run(syn, targets = "sov_indep")
  expect_false("birth_year" %in% names(h))
  full <- hz_run(syn, targets = c("sov_indep", "birth_year"))
  expect_identical(h$eligible_voter, full$eligible_voter)
  strict <- hz_run(syn, targets = c("sov_indep", "birth_year"), min_grade = "identical", missing = "reasons")
  expect_true(all(strict$birth_year__na[strict$study == "qes2018"] == "below_grade"))
  expect_identical(strict$eligible_voter, h$eligible_voter)
  # the 2018 panel's first wave came before the election: every age band is 18 or more
  p <- h[h$study == "qes2018_panel", ]
  expect_true(all(p$eligible_voter[unclass(syn$qes2018_panel$age) %in% 1:3]))
  expect_true(all(is.na(p$eligible_voter[unclass(syn$qes2018_panel$age) %in% 4])))
  # 2018 sampled people aged 16 and over: born 2001 or later is never
  # eligible (rows that gave a year of birth and no age)
  y <- h[h$study == "qes2018", ]
  born_late <- unclass(syn$qes2018$ageyear_1) %in% 2001:2010 & unclass(syn$qes2018$agensp) %in% 0
  expect_true(any(born_late))
  expect_true(all(y$eligible_voter[born_late] %in% FALSE))
  # election targets count the members who could not vote
  r <- hz_run(syn, targets = c("vote_prov_recall", "lr_self"))
  cell <- qes_provenance(r, level = "cell")
  k <- cell$study == "qes2018" & cell$target == "vote_prov_recall"
  expect_identical(cell$n_outside_universe[k], sum(r$eligible_voter[r$study == "qes2018"] %in% FALSE))
  expect_true(is.na(cell$n_outside_universe[cell$study == "qes2018" & cell$target == "lr_self"]))
})

test_that("frames given in data may leave out the variables of unrequested design inputs", {
  syn <- hz_syn("qes2018")
  d <- syn$qes2018
  d$ageyear_1 <- NULL
  d$agemonth_1 <- NULL
  h <- hz_run(list(qes2018 = d), targets = "vote_prov_recall")
  # eligibility then comes from the age asked of the others only
  expect_true(all(is.na(h$eligible_voter[unclass(d$agensp) %in% 0])))
  # a requested target still needs its variable
  expect_error(hz_run(list(qes2018 = d), targets = "birth_year"), class = "qesR_error_spec")
})

test_that("codes without a mapping in unrequested design inputs leave them NA", {
  d <- suppressMessages(get_qes("qes_demo", quiet = TRUE))
  before <- hz_run(list(qes_demo = d), targets = "sov_indep")
  d$QAGE[1] <- 1234
  h <- hz_run(list(qes_demo = d), targets = "sov_indep")
  expect_true(is.na(h$eligible_voter[1]))
  expect_identical(h$eligible_voter[-1], before$eligible_voter[-1])
  # a requested target still stops on them
  expect_error(hz_run(list(qes_demo = d), targets = c("sov_indep", "birth_year")), class = "qesR_error_unmapped")
})

test_that("n_outside_universe is NA when eligibility is not available", {
  syn <- hz_syn("qes2018")
  h <- hz_run(syn, targets = "vote_prov_recall")
  cell <- qes_provenance(h, "cell")
  expect_false(is.na(cell$n_outside_universe[cell$target == "vote_prov_recall"]))
  # the same cell when no member of the wave has an eligibility
  part <- list(eligible = rep(NA, nrow(syn$qes2018)))
  local_mocked_bindings(.qes_hz_eligible = function(...) part$eligible)
  cell0 <- qes_provenance(hz_run(syn, targets = "vote_prov_recall"), "cell")
  expect_true(is.na(cell0$n_outside_universe[cell0$target == "vote_prov_recall"]))
})

test_that("rbind() refuses results with different layouts", {
  syn <- hz_syn(c("qes2012", "qes2014"))
  long <- hz_run(syn["qes2012"], targets = "lr_self", layout = "long")
  resp <- hz_run(syn["qes2014"], targets = "lr_self")
  err <- expect_error(rbind(long, resp), class = "qesR_error_input")
  expect_identical(err$id, "hz_rbind_layout")
})

test_that("rbind() keeps the weight guide of every part", {
  syn <- hz_syn(c("qes2012", "qes2014"))
  one <- hz_run(syn["qes2012"], targets = "lr_self")
  two <- hz_run(syn["qes2014"], targets = "lr_self")
  both <- rbind(one, two)
  expect_identical(attr(both, "qes_weight_guide")$study, c("qes2012", "qes2014"))
  expect_identical(attr(both, "qes_weight_guide"), attr(hz_run(syn, targets = "lr_self"), "qes_weight_guide"))
})

test_that("qes_studies() lists each study's waves from the spec", {
  s <- qes_studies()
  expect_identical(s$waves[s$study == "qes2022"], "cps;pes")
  expect_identical(s$waves[s$study == "qes2018_panel"], "pre;post")
})

test_that("the weight guide names no single wave's weight for a panel's '*' row", {
  syn <- hz_syn("qes2007_panel")
  h <- hz_run(syn, targets = c("gender", "vote_prov_recall"))
  g <- attr(h, "qes_weight_guide")
  gen <- g[g$target == "gender", ]
  expect_identical(gen$wave, "*")
  # the pre and post waves have different weights: neither is named
  expect_true(is.na(gen$weight_column))
  expect_true(is.na(gen$weight_var))
  # a question of one wave keeps its wave's weight
  rec <- g[g$target == "vote_prov_recall", ]
  expect_identical(rec$weight_column, "weight_post")
})
