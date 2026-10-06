# Regressions of the harmonization engine, one per assessment ID or verified
# spec decision (design.md sections 5.5, 5.7 and 8.1, slice HZ3), with the
# real source codes, on synthetic frames (.qes_synthetic()) whose first rows
# are set to the codes under test. The counts on the real files are checked
# by the live tests (test-hz-engine-live.R).

hz_reg <- function(data, ...) {
  withCallingHandlers(
    qes_harmonize(data = data, ..., missing = "reasons", values = "code", include_draft = TRUE, quiet = TRUE),
    # qes2022's labels cannot ship, so its synthetic twin has none (V-D3)
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning")
  )
}

# The synthetic frame of `study` with the first rows of `vars` set to
# `values` (a named list of equal-length vectors); other rows keep codes
# consistent with the gates, so the data checks still pass.
hz_frame <- function(study, values) {
  d <- .qes_synthetic(study)[[study]]
  n <- length(values[[1]])
  for (v in names(values)) {
    x <- d[[v]]
    x[seq_len(n)] <- values[[v]]
    d[[v]] <- x
  }
  stats::setNames(list(d), study)
}

test_that("[A:H3] 2018 q27 = 1 is 'very' (same direction as 2012 and 2014), 2014 Q32 keeps 0 and 10", {
  h <- hz_reg(hz_frame("qes2018", list(q27 = c(1, 4, 98))))
  expect_identical(h$interest_4pt[1:3], c("very", "not_at_all", NA))
  expect_identical(as.character(h$interest_4pt__na[3]), "dk")
  h <- hz_reg(hz_frame("qes2014", list(Q32 = c(0, 10, 98))))
  expect_identical(h$lr_self[1:3], c(0, 10, NA))
})

test_that("[A:H4] 2018 ideology comes from q36_1; 2022 PCQ is code 7 in the PES and 5 in the CPS", {
  skip_on_cran()
  h <- hz_reg(hz_frame("qes2018", list(q36_1 = c(3, 99))))
  expect_identical(h$lr_self[1:2], c(3, NA))
  cell <- qes_provenance(h, level = "cell")
  expect_identical(cell$source_var[cell$target == "lr_self"], "q36_1")
  d <- .qes_synthetic("qes2022")$qes2022
  pes <- which(!is.na(d$pes_StartDate))[1:2]
  d$pes_turnout[pes] <- 1
  d$pes_votechoice[pes] <- c(7, 5)
  d$cps_turnout[1:2] <- 1
  d$cps_votechoice1[1:2] <- c(5, 8)
  h <- hz_reg(list(qes2022 = d))
  expect_identical(h$vote_prov_recall[pes], c("PCQ", "other"))
  expect_identical(h$vote_prov_intent[1:2], c("PCQ", "other"))
  # recall and intention are separate targets, from their own waves
  cell <- qes_provenance(h, level = "cell")
  expect_identical(cell$source_var[cell$target == "vote_prov_recall"], "pes_votechoice")
  expect_identical(cell$wave[cell$target == "vote_prov_recall"], "pes")
  expect_identical(cell$source_var[cell$target == "vote_prov_intent"], "cps_votechoice1")
})

test_that("2022 cps_turnout gates vote intention; pes_turnout 5 and 6 are not_registered and dk (OD12)", {
  d <- .qes_synthetic("qes2022")$qes2022
  d$cps_turnout[1:4] <- c(3, 4, 5, 6)
  d$cps_votechoice1[1:4] <- NA
  pes <- which(!is.na(d$pes_StartDate))[1:3]
  d$pes_turnout[pes] <- c(5, 6, 2)
  d$pes_votechoice[pes] <- NA
  h <- hz_reg(list(qes2022 = d))
  expect_identical(as.character(h$vote_prov_intent__na[1:4]), c("inapplicable", "inapplicable", "inapplicable", "ineligible"))
  expect_identical(as.character(h$turnout_prov_recall__na[pes[1:2]]), c("not_registered", "dk"))
  expect_identical(h$turnout_prov_recall[pes[3]], "no")
  expect_identical(as.character(h$vote_prov_recall__na[pes]), c("not_registered", "dk", "not_voted"))
  # outside the post-election wave, recall is not_in_wave
  out <- which(is.na(d$pes_StartDate))[1]
  expect_identical(as.character(h$vote_prov_recall__na[out]), "not_in_wave")
})

test_that("[A:H5] no respondent is dropped: every qes2007_panel row, keyed on (nompn, quest)", {
  d <- .qes_synthetic("qes2007_panel")$qes2007_panel
  # the same quest in two sampling projects is two respondents
  d$quest[1:2] <- 1
  d$nompn[1:2] <- c(1, 2)
  h <- hz_reg(list(qes2007_panel = d))
  expect_identical(nrow(h), nrow(d))
  expect_false(anyDuplicated(h$qes_id) > 0L)
  expect_identical(h$qes_id[1:2], c("qes2007_panel:1-1", "qes2007_panel:2-1"))
})

test_that("[A:N1] 2007p vote = 10 ('non rejoint') is not_in_wave, never a nonvoter", {
  h <- hz_reg(hz_frame("qes2007_panel", list(vote = c(10, 0, 7))))
  expect_identical(as.character(h$vote_prov_recall__na[1:3]), c("not_in_wave", "not_voted", "spoiled"))
})

test_that("2007p wave membership follows the dispositions (2,050 pre, 2,054 post)", {
  d <- .qes_synthetic("qes2007_panel")$qes2007_panel
  h <- hz_reg(list(qes2007_panel = d))
  waves <- strsplit(ifelse(is.na(h$waves), "", h$waves), ";", fixed = TRUE)
  expect_identical(sum(vapply(waves, function(w) "pre" %in% w, logical(1))), 2050L)
  expect_identical(sum(vapply(waves, function(w) "post" %in% w, logical(1))), 2054L)
})

test_that("[A:H6] turnout and vote recall come from the turnout and vote questions", {
  h <- hz_reg(.qes_synthetic("qes2018"))
  cell <- qes_provenance(h, level = "cell")
  expect_identical(cell$source_var[cell$target == "turnout_prov_recall"], "q5")
  expect_identical(cell$source_var[cell$target == "vote_prov_recall"], "q6")
  h <- hz_reg(hz_frame("qes2018", list(q5 = c(4, 1, 2, 3, 5), q6 = c(2, NA, NA, NA, NA))))
  expect_identical(h$turnout_prov_recall[1:4], c("yes", "no", "no", "no"))
  expect_identical(as.character(h$turnout_prov_recall__na[5]), "ineligible")
  expect_identical(as.character(h$vote_prov_recall__na[2:5]), c("not_voted", "not_voted", "not_voted", "ineligible"))
})

test_that("2018p independance is never mapped; sov_favour comes from rts_q7", {
  d <- .qes_synthetic("qes2018_panel")$qes2018_panel
  h1 <- hz_reg(list(qes2018_panel = d))
  d$independance <- 99
  h2 <- hz_reg(list(qes2018_panel = d))
  expect_identical(h1$sov_favour, h2$sov_favour)
  cell <- qes_provenance(h1, level = "cell")
  expect_identical(cell$source_var[cell$target == "sov_favour"], "rts_q7")
})

test_that("2012p intvoteref: 98 is refused and 99 don't know (reversed), 97 would not vote", {
  h <- hz_reg(hz_frame("qes2012_panel", list(intvoteref = c(98, 99, 97, 1))))
  expect_identical(h$sov_sovereign_country[1:4], c(NA, NA, "would_not_vote", "yes"))
  expect_identical(as.character(h$sov_sovereign_country__na[1:2]), c("refused", "dk"))
  # a different stimulus from sov_indep: never pooled into it
  expect_true(all(h$sov_indep__na == "not_asked"))
})
