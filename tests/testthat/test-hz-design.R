# qes_design() and the internal .qes_splice() and .qes_join_raw()
# (design.md sections 2.2 and 5.3, slice HZ4; OD17).

hz_run <- function(data, ..., include_draft = TRUE, quiet = TRUE) {
  withCallingHandlers(
    qes_harmonize(data = data, ..., include_draft = include_draft, quiet = quiet),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning")
  )
}
hz_syn <- function(studies) .qes_synthetic(studies, spec = hz_spec())

test_that("qes_design() picks the weight that fits the targets, or asks", {
  skip_if_not_installed("survey")
  syn <- hz_syn(c("qes2012", "qes2022"))
  post <- hz_run(syn, targets = c("vote_prov_recall", "turnout_prov_recall", "birth_year"))
  d <- suppressMessages(qes_design(post))
  expect_s3_class(d, "survey.design2")
  # post-election targets: weight_post; the 301 qes2022 respondents outside
  # the post-election wave are left out, with a message
  expect_message(qes_design(post), class = "qesR_message_design_dropped")
  expect_identical(nrow(d$variables), sum(!is.na(post$weight_post)))
  expect_equal(sum(weights(d)), sum(post$weight_post, na.rm = TRUE))
  expect_identical(d$variables$qes_stratum, d$variables$study)
  expect_false(inherits(d$variables, "qes_harmonized"))
  # pre- and post-election targets: no one weight fits, the user chooses
  mixed <- hz_run(syn, targets = c("vote_prov_intent", "vote_prov_recall"))
  err <- expect_error(qes_design(mixed), class = "qesR_error_input")
  expect_identical(err$arg, "weight")
  d <- suppressMessages(qes_design(mixed, weight = "weight_pre"))
  expect_identical(unique(d$variables$study), "qes2022")
  # a weight column that x does not have
  expect_error(qes_design(mixed, weight = "weight"), class = "qesR_error_input")
  expect_error(qes_design(mixed, weight = c("weight_pre", "weight_post")), class = "qesR_error_input")
  # targets with no timing: the one weight column that has values
  any_t <- hz_run(syn["qes2012"], targets = c("lr_self", "sov_indep"))
  expect_identical(suppressMessages(qes_design(any_t))$variables$weight_post, any_t$weight_post)
})

test_that("qes_design() takes the weight of the wave each target came from", {
  skip_if_not_installed("survey")
  syn <- hz_syn("qes2022")
  # sov_indep (timing "any") was asked in the pre-election wave of qes2022,
  # reported vote in the post-election one: the guide calls for both weights
  mixed <- hz_run(syn, targets = c("vote_prov_recall", "sov_indep"))
  g <- attr(mixed, "qes_weight_guide")
  expect_identical(g$weight_column[g$target == "sov_indep"], "weight_pre")
  err <- expect_error(qes_design(mixed), class = "qesR_error_input")
  expect_identical(err$id, "input_design_weight_choose")
  # sov_indep alone: the pre-election weight, every respondent kept
  pre <- hz_run(syn, targets = "sov_indep")
  d <- qes_design(pre)
  expect_identical(nrow(d$variables), nrow(pre))
  expect_equal(sum(weights(d)), sum(pre$weight_pre))
  # static targets only, and both weight columns have values: the user
  # chooses, with a message that does not speak of timings
  static <- hz_run(syn, targets = "birth_year")
  err <- expect_error(qes_design(static), class = "qesR_error_input")
  expect_identical(err$id, "input_design_weight_untimed")
})

test_that("pool = 'equal' gives each study the same total", {
  skip_if_not_installed("survey")
  syn <- hz_syn(c("qes2012", "qes2014"))
  h <- hz_run(syn, targets = "lr_self")
  d <- qes_design(h, weight = "weight_post", pool = "equal")
  totals <- tapply(weights(d), d$variables$study, sum)
  expect_equal(unname(totals[1]), unname(totals[2]))
  expect_equal(sum(totals), nrow(h))
  as_is <- qes_design(h, weight = "weight_post")
  expect_equal(as.vector(tapply(weights(as_is), as_is$variables$study, sum)),
               as.numeric(table(h$study)[c("qes2012", "qes2014")]))
})

test_that("the long layout uses the respondent as the sampling unit", {
  skip_if_not_installed("survey")
  syn <- hz_syn("qes2022")
  l <- hz_run(syn, targets = c("vote_prov_intent", "vote_prov_recall"), layout = "long")
  d <- qes_design(l)
  expect_s3_class(d, "survey.design2")
  expect_identical(nrow(d$variables), nrow(l))
  # one sampling unit per respondent, whatever their number of waves
  expect_identical(length(unique(d$cluster[[1]])), length(unique(l$qes_id)))
  eq <- qes_design(l, pool = "equal")
  totals <- tapply(weights(eq), paste(eq$variables$study, eq$variables$wave), sum)
  expect_equal(unname(totals[1]), unname(totals[2]))
})

test_that("qes_design() checks its input and the suggested packages", {
  expect_error(qes_design(data.frame(a = 1)), class = "qesR_error_input")
  h <- hz_run(hz_syn("qes2012"), targets = "lr_self")
  expect_error(qes_design(h, engine = "stata"), class = "qesR_error_input")
  local_mocked_bindings(.qes_has_package = function(p) FALSE)
  err <- expect_error(qes_design(h, weight = "weight_post"), class = "qesR_error_dependency")
  expect_identical(err$package, "survey")
  all_na <- h
  all_na$weight_post <- NA_real_
  local_mocked_bindings(.qes_has_package = function(p) TRUE)
  expect_error(qes_design(all_na, weight = "weight_post"), class = "qesR_error_input")
})

test_that("engine = 'srvyr' returns a tbl_svy", {
  skip_if_not_installed("survey")
  skip_if_not_installed("srvyr")
  h <- hz_run(hz_syn("qes2012"), targets = "lr_self")
  d <- qes_design(h, weight = "weight_post", engine = "srvyr")
  expect_s3_class(d, "tbl_svy")
})

test_that(".qes_splice() pools one family study by study and flags wording breaks", {
  syn <- hz_syn(c("qes2012", "qes2012_panel"))
  h <- hz_run(syn, targets = c("sov_indep", "sov_sovereign_country"), missing = "reasons")
  expect_warning(
    s <- .qes_splice(h, "sovereignty", into = "sov_referendum", prefer = c("sov_indep", "sov_sovereign_country")),
    class = "qesR_warning_wording_break"
  )
  expect_identical(unique(s$sov_referendum__target[s$study == "qes2012"]), "sov_indep")
  expect_identical(unique(s$sov_referendum__target[s$study == "qes2012_panel"]), "sov_sovereign_country")
  expect_identical(s$sov_referendum[s$study == "qes2012"], h$sov_indep[h$study == "qes2012"])
  expect_identical(s$sov_referendum[s$study == "qes2012_panel"], h$sov_sovereign_country[h$study == "qes2012_panel"])
  expect_identical(levels(s$sov_referendum), levels(h$sov_indep))
  expect_identical(unique(s$sov_referendum__grade[s$study == "qes2012_panel"]), "identical")
  expect_identical(s$sov_referendum__na[s$study == "qes2012"], h$sov_indep__na[h$study == "qes2012"])
  # one target only: no wording break
  one <- hz_run(syn["qes2012"], targets = c("sov_indep", "sov_sovereign_country"))
  expect_no_warning(.qes_splice(one, "sovereignty", into = "x"))
  # targets with different level sets are never pooled
  h2 <- hz_run(hz_syn("qes2018_panel"), targets = c("sov_favour", "sov_indep"))
  expect_error(.qes_splice(h2, "sovereignty", into = "x"), class = "qesR_error_input")
  expect_error(.qes_splice(h, "vote_prov", into = "x"), class = "qesR_error_input")
  err <- expect_error(.qes_splice(h, "sovereignty", into = "sov_indep"), class = "qesR_error_input")
  expect_identical(err$id, "input_splice_into_exists")
  err <- expect_error(.qes_splice(h, "sovereignty", into = NA_character_), class = "qesR_error_input")
  expect_identical(err$id, "input_string")
  expect_false(exists("qes_splice", envir = asNamespace("qesR"), inherits = FALSE))
})

test_that(".qes_join_raw() adds raw columns by study and source row", {
  syn <- hz_syn(c("qes2012", "qes2014"))
  h <- hz_run(syn, targets = "lr_self")
  j <- .qes_join_raw(h, c("q71", "Q32"), data = syn)
  expect_identical(j$qes2012__q71[j$study == "qes2012"], syn$qes2012$q71)
  expect_true(all(is.na(j$qes2012__q71[j$study == "qes2014"])))
  expect_identical(j$qes2014__Q32[j$study == "qes2014"], syn$qes2014$Q32)
  expect_false("qes2014__q71" %in% names(j))
  expect_s3_class(j, "qes_harmonized")
  # per study, as a named list
  k <- .qes_join_raw(h, list(qes2014 = "Q2"), data = syn)
  expect_identical(setdiff(names(k), names(h)), "qes2014__Q2")
  # the long layout joins on source_row too
  l <- hz_run(syn, targets = "lr_self", layout = "long")
  jl <- .qes_join_raw(l, "q71", data = syn)
  expect_identical(jl$qes2012__q71[jl$study == "qes2012"], syn$qes2012$q71[l$source_row[l$study == "qes2012"]])
  expect_error(.qes_join_raw(h, "no_such_variable", data = syn), class = "qesR_error_input")
  expect_error(.qes_join_raw(h, list(qes2014 = "no_such"), data = syn), class = "qesR_error_unknown_variable")
  expect_false(exists("qes_join_raw", envir = asNamespace("qesR"), inherits = FALSE))
})
