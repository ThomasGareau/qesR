# qes_harmonize(), the harmonization engine (design.md sections 5.6, 5.8,
# 5.9 and 8.1, slice HZ3), end to end on the synthetic data of
# .qes_synthetic(): real variable names, every mapped code and gate outcome,
# the waves' n_cases, and no respondent.

# The rows of the shipped spec are not yet signed off by a reviewer (status
# "review"), so the tests apply them with include_draft = TRUE. Data given in
# `data` are unverified: that warning is tested once, and muffled here.
hz_run <- function(data, ..., include_draft = TRUE, quiet = TRUE) {
  withCallingHandlers(
    qes_harmonize(data = data, ..., include_draft = include_draft, quiet = quiet),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning")
  )
}

hz_syn <- function(studies) .qes_synthetic(studies, spec = hz_spec())

# the targets of the "core" set, in spec order (the leading-column target
# survey_mode and birth_month are not in it)
core_targets <- function() {
  tg <- hz_spec()$tables$targets
  tg$target[vapply(tg$sets, function(x) "core" %in% .qes_split_list(x), logical(1))]
}

# The pooled variables of the core set (spec 4.3.0), in the order of pooled.csv.
core_pools <- function() {
  pl <- hz_spec()$tables$pooled
  pl$pooled[vapply(pl$sets, function(x) "core" %in% .qes_split_list(x), logical(1))]
}

# The classes of the messages `expr` signals (muffled), and its value.
hz_messages <- function(expr) {
  classes <- character(0)
  value <- withCallingHandlers(expr, message = function(m) {
    classes <<- c(classes, class(m)[1])
    invokeRestart("muffleMessage")
  })
  list(classes = classes, value = value)
}

test_that("two panel studies with two waves each give one row per respondent", {
  syn <- hz_syn(c("qes2007_panel", "qes2018_panel"))
  expect_warning(
    h <- qes_harmonize(data = syn, include_draft = TRUE, quiet = TRUE),
    class = "qesR_warning_unverified_source"
  )
  expect_s3_class(h, "qes_harmonized")
  expect_visible(hz_run(syn))
  # every source row is kept, in file order, with a unique qes_id
  expect_identical(nrow(h), nrow(syn$qes2007_panel) + nrow(syn$qes2018_panel))
  expect_identical(as.vector(table(h$study)[c("qes2007_panel", "qes2018_panel")]),
                   c(nrow(syn$qes2007_panel), nrow(syn$qes2018_panel)))
  expect_false(anyDuplicated(h$qes_id) > 0L)
  expect_identical(h$source_row[h$study == "qes2007_panel"], seq_len(nrow(syn$qes2007_panel)))
  expect_identical(h$qes_id[1], paste0("qes2007_panel:", .canon(syn$qes2007_panel$nompn[1]), "-",
                                       .canon(syn$qes2007_panel$quest[1])))
  # leading columns, then the core targets
  expect_identical(names(h)[1:15], c("study", "year", "election_date", "family", "study_design",
                                     "target_population", "waves", "qes_id", "subsample", "stratum", "source_row",
                                     "survey_mode", "interview_date", "days_to_election", "eligible_voter"))
  core <- core_targets()
  expect_identical(names(h)[16:(15 + length(core))], core)
  # then the core pooled variables and their companions (spec 4.3.0)
  pools <- core_pools()
  companions <- as.vector(t(outer(pools, c("__type", "__grade", "__item"), paste0)))
  expect_identical(names(h)[(16 + length(core)):ncol(h)],
                   c(pools, companions, "weight_pre", "weight_post", "weight_pre_var", "weight_post_var"))
  expect_s3_class(h$election_date, "Date")
  expect_identical(unique(h$election_date[h$study == "qes2007_panel"]), as.Date("2007-03-26"))
  # wave membership: the waves column and not_in_wave for non-members
  p7 <- h[h$study == "qes2007_panel", ]
  in_post <- syn$qes2007_panel$resultat_pst == "CO"
  in_pre <- syn$qes2007_panel$resultat == "C"
  expect_identical(grepl("post", p7$waves) %in% TRUE, in_post)
  expect_identical(grepl("pre", p7$waves) %in% TRUE, in_pre)
  expect_true(all(is.na(p7$vote_prov_recall[!in_post])))
  hr <- hz_run(syn, missing = "reasons")
  r7 <- hr[hr$study == "qes2007_panel", ]
  expect_true(all(r7$vote_prov_recall__na[!in_post] == "not_in_wave"))
  expect_true(all(r7$vote_prov_intent__na[!in_pre] == "not_in_wave"))
  # among members, only code 10 ("non rejoint") says not_in_wave [A:N1]
  code <- unclass(syn$qes2007_panel$vote)
  expect_false(any(r7$vote_prov_recall__na[in_post & !code %in% 10] %in% "not_in_wave"))
  expect_true(all(r7$vote_prov_recall__na[code %in% 10] == "not_in_wave"))
  # the subsample column comes from the wave's subsample variable
  expect_identical(p7$subsample, .canon(syn$qes2007_panel$nom_proj2))
})

test_that("factor levels are the same in every study, and lang changes labels only", {
  syn <- hz_syn(c("qes2012", "qes2014", "qes2018"))
  h <- hz_run(syn)
  s <- hz_spec()
  set <- .qes_spec_levels(s$tables$levels, "party_qc")
  expect_identical(levels(h$vote_prov_recall), set$label_en)
  expect_false(is.ordered(h$vote_prov_recall))
  expect_true(is.ordered(h$interest_4pt))
  # per-study results have the same levels
  one <- hz_run(syn["qes2012"])
  two <- hz_run(syn["qes2018"])
  expect_identical(levels(one$vote_prov_recall), levels(two$vote_prov_recall))
  # French labels, identical codes
  fr <- hz_run(syn, lang = "fr")
  expect_identical(as.integer(fr$interest_4pt), as.integer(h$interest_4pt))
  expect_identical(levels(fr$interest_4pt)[1], "Tr\u00e8s int\u00e9ress\u00e9(e)")
  expect_identical(attr(fr$interest_4pt, "label"), s$tables$targets$label_fr[s$tables$targets$target == "interest_4pt"])
  strip <- function(x) {
    attr(x, "label") <- NULL
    x
  }
  code_en <- hz_run(syn, values = "code")
  code_fr <- hz_run(syn, values = "code", lang = "fr")
  for (t in core_targets()) {
    expect_identical(strip(code_fr[[t]]), strip(code_en[[t]]), label = t)
  }
  # labelled: the stable level codes, labels in lang
  lab <- hz_run(syn, values = "labelled")
  expect_s3_class(lab$vote_prov_recall, "haven_labelled")
  expect_identical(unclass(lab$vote_prov_recall)[h$vote_prov_recall %in% "Other party"][1], 90L)
  expect_identical(names(attr(lab$vote_prov_recall, "labels"))[attr(lab$vote_prov_recall, "labels") == 90L], "Other party")
  # code: ASCII level names; numeric targets are numbers in every encoding
  code <- hz_run(syn, values = "code")
  expect_type(code$vote_prov_recall, "character")
  expect_true(all(code$vote_prov_recall %in% c(set$name, NA)))
  expect_type(code$lr_self, "double")
  expect_type(h$lr_self, "double")
  expect_true(all(h$lr_self >= 0 & h$lr_self <= 10, na.rm = TRUE))
})

test_that("every missing value has a reason, and cell provenance counts sum to the rows", {
  syn <- hz_syn(c("qes2012", "qes2018", "qes2018_panel", "qes2007_panel"))
  h <- hz_run(syn, missing = "reasons", keep_source = TRUE)
  reasons <- .qes_hz_reason_levels()
  for (t in core_targets()) {
    na <- h[[paste0(t, "__na")]]
    expect_s3_class(na, "factor")
    expect_identical(levels(na), reasons)
    expect_identical(is.na(h[[t]]), !is.na(na), label = t)
  }
  cell <- qes_provenance(h, level = "cell")
  expect_s3_class(cell, "qes_provenance")
  n_cols <- c("n_valid", paste0("n_", reasons))
  totals <- rowSums(as.matrix(cell[, n_cols]))
  expect_identical(as.integer(totals), as.integer(table(h$study)[cell$study]))
  expect_identical(nrow(cell), length(unique(h$study)) * length(core_targets()))
  # a target a study has no question for is not_asked
  k <- cell$study == "qes2012" & cell$target == "vote_prov_intent"
  expect_false(cell$included[k])
  expect_identical(cell$n_not_asked[k], nrow(syn$qes2012))
  # source codes are kept as text with keep_source
  expect_identical(h$vote_prov_recall__src[h$study == "qes2018"], .canon(syn$qes2018$q6))
  # study and spec provenance
  study <- qes_provenance(h)
  expect_identical(study$study, unique(h$study))
  expect_true(all(study$md5_verified %in% FALSE))
  expect_true(all(study$retrieved_via == "user_data"))
  sp <- qes_provenance(h, level = "spec")
  expect_identical(sp$spec_version, hz_spec()$version)
  expect_identical(sp$spec_hash, attr(h, "qes_spec")$hash)
  expect_match(sp$args, "data=qes2012:[0-9a-f]{32}")
  expect_s3_class(sp$created, "POSIXct")
  expect_identical(attr(h, "qes_spec")$version, hz_spec()$version)
  expect_false(attr(h, "qes_spec")$custom)
})

test_that("structural zeros are listed and announced", {
  syn <- hz_syn("qes2018")
  m <- hz_messages(hz_run(syn, quiet = FALSE))
  expect_true("qesR_message_structural_zeros" %in% m$classes)
  expect_identical(hz_messages(hz_run(syn, quiet = TRUE))$classes, character(0))
  h <- hz_run(syn)
  cell <- qes_provenance(h, level = "cell")
  z <- cell$levels_not_offered[cell$target == "vote_prov_recall"]
  expect_identical(z, "PVQ;PCQ;ON;ADQ")
  # every level stays a factor level, with no respondent in a study that did not offer it
  expect_true("PCQ" %in% levels(h$vote_prov_recall))
  expect_identical(sum(h$vote_prov_recall %in% "PCQ"), 0L)
  out <- capture.output(print(h))
  expect_true(any(grepl("PCQ", out[1:6])))
})

test_that("min_grade sets lower cells to NA with reason below_grade", {
  syn <- hz_syn(c("qes2018", "qes2014"))
  # approximate cells are announced under the default min_grade
  expect_true("qesR_message_approximate_cells" %in% hz_messages(hz_run(syn, quiet = FALSE))$classes)
  h <- hz_run(syn, min_grade = "comparable", missing = "reasons")
  expect_true(all(is.na(h$turnout_prov_recall[h$study == "qes2018"])))
  expect_true(all(h$turnout_prov_recall__na[h$study == "qes2018"] == "below_grade"))
  expect_false(all(is.na(h$turnout_prov_recall[h$study == "qes2014"])))
  expect_false("qesR_message_approximate_cells" %in%
                 hz_messages(hz_run(syn, min_grade = "comparable", quiet = FALSE))$classes)
  ident <- hz_run(syn, min_grade = "identical", missing = "reasons")
  cell <- qes_provenance(ident, level = "cell")
  expect_true(all(cell$included == (cell$grade %in% "identical")))
  expect_true(all(ident$sov_indep__na[ident$study == "qes2018"] == "below_grade"))
  expect_false(all(is.na(ident$sov_indep[ident$study == "qes2014"])))
})

test_that("rows not signed off by a reviewer are applied only with include_draft", {
  syn <- hz_syn("qes2014")
  unsigned <- hz_spec_unsigned()
  # no row applied at all: a warning (qesR_warning_all_unreviewed), not the
  # message
  expect_warning(
    m <- hz_messages(hz_run(syn, include_draft = FALSE, quiet = FALSE, missing = "reasons", spec = unsigned)),
    class = "qesR_warning_all_unreviewed"
  )
  expect_false("qesR_message_unreviewed_skipped" %in% m$classes)
  expect_false("qesR_message_unreviewed_cells" %in% m$classes)
  h <- m$value
  expect_true(all(is.na(h$vote_prov_recall)))
  # a question that was asked, whose row is not signed off: not_reviewed, not not_asked
  expect_true(all(h$vote_prov_recall__na == "not_reviewed"))
  cell <- qes_provenance(h, level = "cell")
  expect_match(cell$note[cell$target == "vote_prov_recall"], "not signed off")
  expect_identical(cell$excluded[cell$target == "vote_prov_recall"], "not_reviewed")
  expect_identical(cell$n_not_reviewed[cell$target == "vote_prov_recall"], nrow(syn$qes2014))
  # a target without a row stays not_asked
  expect_identical(cell$excluded[cell$target == "vote_prov_intent"], "no_row")
  expect_true(all(h$vote_prov_intent__na == "not_asked"))
  withr::local_options(qesR.lang = "en")
  expect_true(any(grepl("include_draft = FALSE", capture.output(print(h)), fixed = TRUE)))
  # with include_draft = TRUE they are applied, with their own message
  m1 <- hz_messages(hz_run(syn, include_draft = TRUE, quiet = FALSE, spec = unsigned))
  expect_true("qesR_message_unreviewed_cells" %in% m1$classes)
  expect_false("qesR_message_unreviewed_skipped" %in% m1$classes)
  # a signed-off row is applied by default
  s <- unsigned
  i <- hz_xw_row(s$tables, "qes2014", "vote_prov_recall")
  s$tables$crosswalk$status[i] <- "stable"
  s$tables$crosswalk$reviewed_by[i] <- "Test Reviewer"
  s$tables$crosswalk$reviewed_on[i] <- as.Date("2026-09-27")
  h2 <- hz_run(syn, include_draft = FALSE, spec = s)
  expect_false(all(is.na(h2$vote_prov_recall)))
  expect_true(all(is.na(h2$sov_indep)))
  expect_true(attr(h2, "qes_spec")$custom)
  # the shipped spec: every row reviewed up to spec 4.2.0 is signed off
  # (spec 4.1.0) and applied by default; the rows added in spec 4.3.0 are
  # in review (not reviewed yet: reviewed_by is empty)
  h3 <- hz_run(syn, include_draft = FALSE, missing = "reasons")
  expect_false(all(is.na(h3$vote_prov_recall)))
  expect_false(all(is.na(h3$sov_indep)))
  expect_false(any(h3$gender__na %in% "not_reviewed"))
  xw <- hz_spec()$tables$crosswalk
  expect_true(all(xw$status[!is.na(xw$reviewed_by)] == "stable"))
  expect_true(all(xw$status[is.na(xw$reviewed_by)] == "review"))
})

test_that("cells left out for another reason are not counted as not signed off", {
  withr::local_options(qesR.lang = "en")
  # below min_grade, with include_draft = TRUE
  syn <- hz_syn(c("qes2022", "qes2014"))
  # (the synthetic qes2022 has no value labels, which the spec holds only as
  # hashes: the citizenship row that eligible_voter reads warns about them)
  m <- hz_messages(withCallingHandlers(
    hz_run(syn, include_draft = TRUE, min_grade = "identical", quiet = FALSE),
    qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning")
  ))
  expect_false("qesR_message_unreviewed_skipped" %in% m$classes)
  cell <- qes_provenance(m$value, level = "cell")
  expect_true(all(cell$excluded[!cell$included] %in% c("below_grade", "no_row")))
  expect_true(any(cell$excluded %in% "below_grade"))
  expect_true(all(is.na(cell$excluded[cell$included])))
  out <- capture.output(print(m$value))
  expect_false(any(grepl("include_draft = FALSE", out, fixed = TRUE)))
  expect_true(any(grepl("Below min_grade", out, fixed = TRUE)))
  # a variable missing from the demonstration data
  testthat::local_mocked_bindings(.qes_transport = function(...) stop("no request expected"), .package = "qesR")
  m <- hz_messages(qes_harmonize("qes_demo", targets = "pid_prov", include_draft = TRUE, quiet = FALSE))
  expect_false("qesR_message_unreviewed_skipped" %in% m$classes)
  expect_identical(qes_provenance(m$value, level = "cell")$excluded, "not_in_data")
  expect_false(any(grepl("include_draft = FALSE", capture.output(print(m$value)), fixed = TRUE)))
})

test_that("unmapped codes are an error, or NA with reason unmapped", {
  syn <- hz_syn("qes2014")
  d <- syn$qes2014
  d$Q19[1:3] <- 7
  err <- expect_error(hz_run(list(qes2014 = d)), class = "qesR_error_unmapped")
  expect_identical(err$study, "qes2014")
  expect_identical(err$target, "sov_indep")
  expect_identical(err$codes, "7")
  expect_identical(err$n, 3L)
  expect_warning(h <- hz_run(list(qes2014 = d), unmapped = "warn", missing = "reasons"),
                 class = "qesR_warning_unmapped")
  expect_true(all(h$sov_indep__na[1:3] == "unmapped"))
  expect_true(all(is.na(h$sov_indep[1:3])))
  expect_no_warning(h2 <- hz_run(list(qes2014 = d), unmapped = "na", missing = "reasons"))
  expect_identical(h2$sov_indep__na, h$sov_indep__na)
  # a numeric code outside the range is unmapped too, never passed through
  d <- syn$qes2014
  d$Q32[1] <- 11
  expect_error(hz_run(list(qes2014 = d)), class = "qesR_error_unmapped")
})

test_that("data that fail the spec's checks stop, or are skipped with on_fail = 'skip'", {
  syn <- hz_syn(c("qes2012", "qes2014"))
  bad <- syn
  bad$qes2014$Q19 <- NULL
  err <- expect_error(hz_run(bad), class = "qesR_error_spec")
  expect_true("V-D1" %in% err$problems$rule)
  expect_warning(h <- hz_run(bad, on_fail = "skip"), class = "qesR_warning_partial")
  expect_identical(unique(h$study), "qes2012")
  failed <- attr(h, "failed_studies")
  expect_identical(failed$study, "qes2014")
  expect_identical(failed$class, "qesR_error_spec")
  expect_output(print(h), "qes2014")
  # every study failing gives an empty result with the same columns
  expect_warning(all_bad <- hz_run(bad["qes2014"], on_fail = "skip"), class = "qesR_warning_partial")
  expect_identical(nrow(all_bad), 0L)
  expect_identical(names(all_bad), names(hz_run(syn["qes2014"])))
  withr::local_options(qesR.lang = "en")
  head <- capture.output(print(all_bad))[1]
  expect_match(head, "no rows, every study failed", fixed = TRUE)
  expect_false(grepl("from ;", head, fixed = TRUE))
  withr::local_options(qesR.lang = "fr")
  expect_match(capture.output(print(all_bad))[1], "aucune ligne", fixed = TRUE)
  # a label that differs from the spec's is a warning for data given by the user
  d <- syn$qes2012
  labs <- attr(d$q52, "labels")
  names(labs)[labs == 1] <- "Maybe"
  attr(d$q52, "labels") <- labs
  expect_warning(hz_run(list(qes2012 = d)), class = "qesR_warning_label_mismatch")
})

test_that("identifiers must exist and be unique", {
  syn <- hz_syn("qes2018_panel")
  d <- syn$qes2018_panel
  d$id[2] <- d$id[1]
  d$method[2] <- d$method[1]
  err <- expect_error(hz_run(list(qes2018_panel = d)), class = "qesR_error_duplicate_id")
  expect_identical(err$study, "qes2018_panel")
  expect_identical(err$id_vars, c("method", "id"))
  expect_identical(err$rows, 1:2)
  d$id <- NULL
  expect_error(hz_run(list(qes2018_panel = d)), class = "qesR_error_unknown_variable")
})

test_that("arguments are checked", {
  syn <- hz_syn("qes2014")
  expect_error(qes_harmonize(layout = "wide"), class = "qesR_error_input")
  expect_error(qes_harmonize(weights = "trimmed"), class = "qesR_error_input")
  # the interview mode is a leading column, not a target to request
  expect_error(qes_harmonize(targets = "survey_mode", data = syn), class = "qesR_error_input")
  expect_error(qes_harmonize(values = "text"), class = "qesR_error_input")
  expect_error(qes_harmonize(keep_source = NA), class = "qesR_error_input")
  err <- expect_error(qes_harmonize(targets = "vote_prov_recal", data = syn), class = "qesR_error_input")
  expect_true("vote_prov_recall" %in% err$suggestions)
  expect_error(qes_harmonize(targets = character(0), data = syn), class = "qesR_error_input")
  # the firms' own 1998 files are raw data of the qes1998 respondents, not in the spec
  expect_error(qes_harmonize("qes1998_crop"), class = "qesR_error_input")
  expect_error(qes_harmonize("qes2019"), class = "qesR_error_unknown_study")
  expect_error(qes_harmonize("qes2012", data = syn), class = "qesR_error_input")
  expect_error(qes_harmonize(data = list(syn$qes2014)), class = "qesR_error_input")
  expect_error(qes_harmonize(data = syn$qes2014), class = "qesR_error_input")
  fac <- syn
  fac$qes2014$Q19 <- factor(fac$qes2014$Q19)
  expect_error(hz_run(fac), class = "qesR_error_input")
})

test_that("targets accepts target, family and set names, in spec order", {
  syn <- hz_syn("qes2014")
  wcols <- c("weight_pre", "weight_post", "weight_pre_var", "weight_post_var")
  h <- hz_run(syn, targets = c("sov_indep", "vote_prov"))
  expect_identical(setdiff(names(h)[-(1:15)], wcols),
                   c("vote_prov_recall", "vote_prov_intent", "vote_prov_intent_push", "sov_indep",
                     "vote_prov_intent_other"))
  h <- hz_run(syn, targets = "vote")
  expect_identical(setdiff(names(h)[-(1:15)], wcols),
                   c("vote_prov_recall", "vote_prov_intent", "vote_prov_intent_push", "turnout_prov_recall",
                     "turnout_prov_likely", "vote_prov_prev", "vote_fed_recall", "vote_choice", "turnout",
                     "vote_choice__type", "vote_choice__grade", "vote_choice__item",
                     "turnout__type", "turnout__grade", "turnout__item"))
  # a family whose only target is a leading column adds nothing
  expect_error(hz_run(syn, targets = "interview_mode"), class = "qesR_error_input")
})

test_that("study selection: defaults, 'all' and the demonstration study", {
  s <- hz_spec()
  covered <- .qes_hz_covered(s)
  expect_true(all(c("qes2012", "qes2014", "qes2018", "qes2022", "qes2007_panel", "qes2018_panel",
                    "qes2007", "qes2008", "qes2012_panel", "qes_crop_2007_2010", "qes1998") %in% covered))
  # the default is the Quebec Election Studies, never the pooled polls or panels
  expect_identical(.qes_hz_resolve_studies(NULL, NULL, s),
                   c("qes2022", "qes2018", "qes2014", "qes2012", "qes2008", "qes2007"))
  expect_identical(.qes_hz_resolve_studies("all", NULL, s), covered)
  expect_identical(.qes_hz_resolve_studies(" QES2014 ", NULL, s), "qes2014")
  expect_identical(.qes_hz_resolve_studies(NULL, list(qes2018 = 1, qes2012 = 1), s), c("qes2018", "qes2012"))
  expect_error(.qes_hz_resolve_studies(c("all", "qes2014"), NULL, s), class = "qesR_error_input")
})

test_that("the demonstration study is harmonized offline with the qes2014 rows", {
  testthat::local_mocked_bindings(.qes_transport = function(...) stop("no request expected"), .package = "qesR")
  h <- qes_harmonize("qes_demo", missing = "reasons", include_draft = TRUE, quiet = TRUE)
  demo <- .qes_read("qes_demo")
  expect_identical(nrow(h), nrow(demo))
  expect_identical(unique(h$study), "qes_demo")
  # Q55 (pid_prov) is not in the demo: not_asked, with a note
  expect_true(all(h$pid_prov__na == "not_asked"))
  cell <- qes_provenance(h, level = "cell")
  expect_match(cell$note[cell$target == "pid_prov"], "demonstration")
  # Q3 codes follow the qes2014 map
  code <- unclass(demo$Q3)
  expect_true(all(h$vote_prov_recall[code %in% 1] == "PLQ"))
  expect_true(all(h$vote_prov_recall__na[code %in% 99] == "refused"))
  expect_true(all(h$vote_prov_recall__na[unclass(demo$Q2) %in% 2] == "not_voted"))
  # the file was read from the package and checked by md5
  expect_true(qes_provenance(h)$md5_verified)
  # default: the signed-off rows are applied (every row since spec 4.1.0)
  h0 <- qes_harmonize("qes_demo", missing = "reasons", quiet = TRUE)
  expect_identical(h0$vote_prov_recall, h$vote_prov_recall)
  expect_identical(h0$gender, h$gender)
  expect_false(any(h0$gender__na %in% "not_reviewed"))
})

test_that("printing shows the spec, the approximate and structural-zero cells, and the licence", {
  syn <- hz_syn(c("qes2018", "qes2014"))
  h <- hz_run(syn)
  withr::local_options(qesR.lang = "en")
  out <- capture.output(print(h))
  expect_match(out[1], "experimental")
  expect_match(out[1], hz_spec()$version, fixed = TRUE)
  expect_true(any(grepl("Approximate cells", out)))
  expect_true(any(grepl("Structural zeros", out)))
  expect_false(any(grepl("CC BY-NC", out)))
  # a subset keeps the class but not the attributes: plain printing
  expect_output(print(h[1:2, 1:3]), "study")
  withr::local_options(qesR.lang = "fr")
  expect_match(capture.output(print(h))[1], "exp\u00e9rimental")
})

test_that("rbind() of per-study results combines their rows and provenance", {
  syn <- hz_syn(c("qes2012", "qes2022"))
  # the synthetic qes2022 frame has no value labels (the file's are not shipped)
  run <- function(...) {
    withCallingHandlers(hz_run(...), qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning"))
  }
  one <- run(syn["qes2012"], missing = "reasons")
  two <- run(syn["qes2022"], missing = "reasons")
  both <- rbind(one, two)
  expect_s3_class(both, "qes_harmonized")
  expect_identical(nrow(both), nrow(one) + nrow(two))
  expect_identical(levels(both$vote_prov_recall), levels(one$vote_prov_recall))
  expect_identical(both$qes_id, c(one$qes_id, two$qes_id))
  expect_identical(qes_provenance(both)$study, c("qes2012", "qes2022"))
  cell <- qes_provenance(both, level = "cell")
  expect_identical(unique(cell$study), c("qes2012", "qes2022"))
  expect_identical(nrow(cell), 2L * length(core_targets()))
  expect_identical(nrow(qes_provenance(both, level = "spec")), 2L)
  # the same as harmonizing both studies at once, apart from the provenance
  ref <- run(syn[c("qes2012", "qes2022")], missing = "reasons")
  strip <- function(x) {
    attr(x, "qes_provenance") <- NULL
    x
  }
  expect_identical(strip(both), strip(ref))
  # the licence of qes2022 is printed even when it is not the first part
  withr::local_options(qesR.lang = "en")
  out <- capture.output(print(both))
  expect_true(any(grepl("CC BY-NC", out)))
  expect_match(out[1], "qes2012")
  expect_match(out[1], "qes2022")
  # a study twice, or two specs, is an error
  expect_error(rbind(one, one), class = "qesR_error_input")
  other <- two
  attr(other, "qes_spec")$hash <- "0123456789abcdef0123456789abcdef"
  expect_error(rbind(one, other), class = "qesR_error_input")
  # parts built with different options are refused, naming the option
  fr <- run(syn["qes2022"], missing = "reasons", lang = "fr")
  err <- expect_error(rbind(one, fr), class = "qesR_error_input")
  expect_identical(err$id, "hz_rbind_options")
  expect_identical(names(err$options), "lang")
  expect_match(conditionMessage(err), "lang", fixed = TRUE)
  code <- run(syn["qes2022"], missing = "reasons", values = "code")
  err <- expect_error(rbind(one, code), class = "qesR_error_input")
  expect_identical(names(err$options), "values")
  na_only <- run(syn["qes2022"])
  err <- expect_error(rbind(one, na_only), class = "qesR_error_input")
  expect_true("missing" %in% names(err$options))
  raw_w <- run(syn["qes2022"], missing = "reasons", weights = "raw")
  err <- expect_error(rbind(one, raw_w), class = "qesR_error_input")
  expect_identical(names(err$options), "weights")
  # objects made before the options were recorded are still bound
  old1 <- one
  old2 <- two
  attr(old1, "qes_spec")$options <- NULL
  attr(old2, "qes_spec")$options <- NULL
  expect_s3_class(rbind(old1, old2), "qes_harmonized")
  # different targets: a classed error listing what each part lacks, before
  # base rbind() can fail on the column count
  fewer <- run(syn["qes2022"], missing = "reasons", targets = "gender")
  err <- expect_error(rbind(one, fewer), class = "qesR_error_input")
  expect_identical(err$id, "hz_rbind_targets")
  expect_length(err$missing[[1]], 0L)
  expect_true(length(err$missing[[2]]) > 0L)
  expect_match(conditionMessage(err), "qes2022", fixed = TRUE)
  # with a plain data frame, a plain data frame without the qes attributes
  plain <- rbind.qes_harmonized(one, as.data.frame(strip(two)))
  expect_false(inherits(plain, "qes_harmonized"))
  expect_null(attr(plain, "qes_provenance"))
})

test_that("harmonized output does not depend on the locale", {
  syn <- hz_syn(c("qes2014", "qes2018_panel"))
  ref <- hz_run(syn, missing = "reasons", lang = "fr")
  attr(ref, "qes_provenance") <- NULL
  withr::with_locale(c(LC_COLLATE = "C", LC_CTYPE = "C"), {
    withr::with_envvar(c(LANGUAGE = "fr"), {
      got <- hz_run(syn, missing = "reasons", lang = "fr")
    })
  })
  attr(got, "qes_provenance") <- NULL
  expect_identical(got, ref)
})

test_that("a result left all NA by unreviewed rows is a warning that counts the values", {
  syn <- hz_syn("qes2014")
  unsigned <- hz_spec_unsigned()
  w <- expect_warning(
    h <- hz_run(syn, targets = c("vote_prov_recall", "gender"), include_draft = FALSE, quiet = TRUE, spec = unsigned),
    class = "qesR_warning_all_unreviewed"
  )
  expect_true(all(is.na(h$vote_prov_recall)))
  expect_true(all(is.na(h$gender)))
  expect_identical(as.integer(w$n_values), 2L * nrow(syn$qes2014))
  expect_setequal(w$cells$target, c("vote_prov_recall", "gender"))
  expect_identical(w$id, "unreviewed_all")
  # the warning is not silenced by quiet, and replaces the message
  m <- hz_messages(withCallingHandlers(
    hz_run(syn, targets = c("vote_prov_recall", "gender"), include_draft = FALSE, quiet = FALSE, spec = unsigned),
    qesR_warning_all_unreviewed = function(w) invokeRestart("muffleWarning")
  ))
  expect_false("qesR_message_unreviewed_skipped" %in% m$classes)
  # the print line counts values as well as cells
  withr::local_options(qesR.lang = "en")
  out <- capture.output(print(h))
  line <- out[grepl("include_draft = FALSE", out, fixed = TRUE)]
  expect_length(line, 1L)
  expect_match(line, sprintf("2 (%d value(s))", 2L * nrow(syn$qes2014)), fixed = TRUE)
  # a signed-off row applied next to unreviewed ones: a message, with the count
  s <- unsigned
  i <- hz_xw_row(s$tables, "qes2014", "vote_prov_recall")
  s$tables$crosswalk$status[i] <- "stable"
  s$tables$crosswalk$reviewed_by[i] <- "Test Reviewer"
  s$tables$crosswalk$reviewed_on[i] <- as.Date("2026-09-27")
  got <- NULL
  expect_no_warning(withCallingHandlers(
    hz_run(syn, targets = c("vote_prov_recall", "gender"), include_draft = FALSE, spec = s, quiet = FALSE),
    qesR_message_unreviewed_skipped = function(m) {
      got <<- m
      invokeRestart("muffleMessage")
    },
    message = function(m) invokeRestart("muffleMessage")
  ))
  expect_false(is.null(got))
  expect_identical(as.integer(got$n_values), nrow(syn$qes2014))
})

test_that("a data frame given in data without its identifier names the dropped column", {
  syn <- hz_syn("qes2018")
  ids <- qesR:::.qes_split_list(qesR:::.qes_default_data_file("qes2018", demo = TRUE)$id_vars)
  ids <- setdiff(ids, ".row")
  skip_if(length(ids) == 0L)
  d <- syn$qes2018
  d <- d[, setdiff(names(d), ids), drop = FALSE]
  withr::local_options(qesR.lang = "en")
  err <- expect_error(hz_run(list(qes2018 = d), targets = "gender"), class = "qesR_error_unknown_variable")
  expect_identical(err$id, "hz_missing_id")
  expect_identical(err$variables, ids)
  expect_match(conditionMessage(err), "data$qes2018", fixed = TRUE)
  expect_match(conditionMessage(err), "qes_id", fixed = TRUE)
})
