# Pooled variables (R/hz-pool.R, spec 4.3.0): one column that pools several
# targets with a precedence, its companions, the validator rules V-F1 to
# V-F7, the rule coalesce, the derivation stage and the party lineage. End
# to end on the synthetic data of .qes_synthetic(): real variable names,
# every mapped code and gate outcome, no respondent.

hz_run <- function(data, ..., include_draft = TRUE, quiet = TRUE) {
  withCallingHandlers(
    qes_harmonize(data = data, ..., include_draft = include_draft, quiet = quiet),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning")
  )
}
hz_syn <- function(studies) .qes_synthetic(studies, spec = hz_spec())
pool_members <- function(p) .qes_pool_members(hz_spec(), p)

# The value and reason a pooled column must have in each row, from the
# member columns (rule 1 of R/hz-pool.R: the first member, by precedence,
# with a value or a terminal reason), or NA where every member falls through.
expected_pool <- function(h, p, types = NULL) {
  m <- pool_members(p)
  if (!is.null(types)) m <- m[m$type_name %in% types, , drop = FALSE] else m <- m[m$default, , drop = FALSE]
  value <- rep(NA_character_, nrow(h))
  type <- rep(NA_character_, nrow(h))
  for (j in rev(seq_len(nrow(m)))) {
    v <- as.character(h[[m$member[j]]])
    r <- as.character(h[[paste0(m$member[j], "__na")]])
    stop_here <- !is.na(v) | !r %in% .qes_pool_fallthrough
    tr <- .qes_pool_transform(m$transform[j])
    value[stop_here] <- .qes_pool_apply_transform(v, tr)[stop_here]
    type[stop_here] <- m$type_name[j]
  }
  list(value = value, type = type)
}

test_that("the shipped pooled variables pass V-F1 to V-F7 and name their members", {
  s <- hz_spec()
  p <- .qes_spec_check(s)
  expect_false(any(grepl("^V-F", p$rule)))
  expect_identical(.qes_pool_names(s), c("vote_choice", "sov_support", "pol_interest", "turnout"))
  vc <- pool_members("vote_choice")
  expect_identical(vc$type_name, c("recall", "intention_push", "intention"))
  expect_identical(vc$member, c("vote_prov_recall", "vote_prov_intent_push", "vote_prov_intent"))
  # the collapse of the favour scale and the scored interest items are capped
  expect_identical(pool_members("sov_support")$grade_cap[pool_members("sov_support")$type_name == "favour"], "approximate")
  expect_true(all(pool_members("pol_interest")$grade_cap[grepl("4pt", pool_members("pol_interest")$type_name)] == "approximate"))
  # an intention is not a turnout: not used by default
  expect_false(pool_members("turnout")$default[pool_members("turnout")$type_name == "intention"])
})

test_that("a pooled value is the first member, by precedence, that asked the respondent", {
  syn <- hz_syn(c("qes2022", "qes2007_panel", "qes2012"))
  members <- pool_members("vote_choice")$member
  h <- hz_run(syn, targets = c("vote_choice", members), layout = "long", values = "code", missing = "reasons")
  e <- expected_pool(h, "vote_choice")
  expect_identical(as.character(h$vote_choice), e$value)
  has <- !is.na(e$type)
  expect_identical(as.character(h$vote_choice__type)[has], e$type[has])
  # a row every member passes is NA with a reason that says no member asked
  # (or the reason of the first member that has a row in the wave)
  none <- is.na(e$type)
  expect_true(all(as.character(h$vote_choice__na)[none] %in% .qes_pool_fallthrough))
  # every NA has a reason, and no row is dropped
  expect_false(anyNA(h$vote_choice__na[is.na(h$vote_choice)]))
  expect_true(all(is.na(h$vote_choice__na[!is.na(h$vote_choice)])))
  expect_identical(nrow(h), nrow(hz_run(syn, targets = "vote_prov_recall", layout = "long")))
  # a respondent who did not vote is NA with that answer, never given an
  # intention: not_voted and refused are terminal
  expect_false("not_voted" %in% .qes_pool_fallthrough)
  voted_no <- h$vote_prov_recall__na %in% "not_voted"
  expect_true(all(h$vote_choice__na[voted_no] %in% "not_voted"))
  expect_true(all(h$vote_choice__type[voted_no] %in% "recall"))
})

test_that("types keeps some members only, in the spec's order of precedence", {
  syn <- hz_syn(c("qes2022", "qes2012"))
  members <- pool_members("vote_choice")$member
  h <- hz_run(syn, targets = c("vote_choice", members), values = "code", missing = "reasons",
              types = list(vote_choice = "recall"))
  # one type: the pooled column is its member, value for value and reason
  # for reason
  expect_identical(as.character(h$vote_choice), as.character(h$vote_prov_recall))
  expect_identical(as.character(h$vote_choice__na), as.character(h$vote_prov_recall__na))
  # the order given does not matter; a member's target name is accepted
  a <- hz_run(syn, targets = "vote_choice", values = "code", types = list(vote_choice = c("intention", "intention_push")))
  b <- hz_run(syn, targets = "vote_choice", values = "code",
              types = list(vote_choice = c("vote_prov_intent_push", "intention")))
  expect_identical(a$vote_choice, b$vote_choice)
  expect_identical(a$vote_choice__type, b$vote_choice__type)
  expect_true(all(a$vote_choice__type[a$study == "qes2012"] %in% NA))
  # errors
  expect_error(hz_run(syn, targets = "vote_choice", types = list(vote_choice = "recal")), class = "qesR_error_input")
  expect_error(hz_run(syn, targets = "vote_choice", types = list(sov_support = "independence")), class = "qesR_error_input")
  expect_error(hz_run(syn, targets = "vote_choice", types = "recall"), class = "qesR_error_input")
  expect_error(hz_run(syn, targets = "vote_choice", types = list(vote_choice = NA_character_)), class = "qesR_error_input")
})

test_that("the respondent layout takes a study's pooled values from one wave", {
  syn <- hz_syn(c("qes2022", "qes2012_panel"))
  h <- hz_run(syn, targets = "vote_choice", missing = "reasons", values = "code")
  # both studies ask the reported vote after the election: the post-election
  # wave gives every value, and the other respondents are not_in_wave
  expect_true(all(h$vote_choice__type %in% c("recall", NA)))
  s22 <- h$study == "qes2022"
  only_cps <- s22 & !grepl("pes", h$waves)
  expect_true(any(only_cps))
  expect_true(all(h$vote_choice__na[only_cps] %in% "not_in_wave"))
  prov <- qes_provenance(h, level = "pooled")
  expect_identical(prov$used_in_layout[prov$study == "qes2022"], c(TRUE, FALSE, FALSE))
  # the long layout keeps every wave: the campaign rows get the intentions
  l <- hz_run(syn, targets = "vote_choice", layout = "long", values = "code")
  expect_true(all(c("recall", "intention_push") %in% l$vote_choice__type[l$study == "qes2022"]))
  expect_true(all(l$vote_choice__type[l$study == "qes2022" & l$wave == "cps"] %in% c("intention_push", "intention", NA)))
})

test_that("grades are the member's, capped, never raised; min_grade applies to the capped grade", {
  syn <- hz_syn(c("qes2018_panel", "qes2012", "qes2007"))
  h <- hz_run(syn, targets = c("sov_support", "pol_interest"), missing = "reasons")
  prov <- qes_provenance(h, level = "pooled")
  cell_grade <- function(s, p, type) prov$grade[prov$study == s & prov$pooled == p & prov$type == type]
  expect_identical(cell_grade("qes2018_panel", "sov_support", "favour"), "approximate")
  expect_identical(cell_grade("qes2012", "pol_interest", "general_4pt"), "approximate")
  expect_identical(cell_grade("qes2007", "pol_interest", "general_0_10"), "identical")
  used <- !is.na(h$sov_support__type)
  expect_identical(as.character(h$sov_support__grade[used]),
                   prov$grade[match(paste(h$study[used], "sov_support", h$sov_support__type[used]),
                                    paste(prov$study, prov$pooled, prov$type))])
  expect_s3_class(h$sov_support__grade, "ordered")
  # the 2018 panel's favour scale is collapsed to yes or no
  s18 <- h$study == "qes2018_panel" & !is.na(h$sov_support)
  expect_true(all(as.character(h$sov_support[s18]) %in% c("Yes", "No")))
  # pol_interest is on 0-1: the four-point scores and the 0-10 items / 10
  expect_true(all(h$pol_interest >= 0 & h$pol_interest <= 1, na.rm = TRUE))
  expect_setequal(unique(stats::na.omit(h$pol_interest[h$study == "qes2012"])), c(1, 0.7, 0.3, 0))
  c2 <- hz_run(syn, targets = c("sov_support", "pol_interest"), missing = "reasons", min_grade = "comparable")
  expect_true(all(c2$sov_support__na[c2$study == "qes2018_panel" & grepl("post", c2$waves)] %in% "below_grade"))
  expect_true(all(is.na(c2$pol_interest[c2$study == "qes2012"])))
  expect_false(all(is.na(c2$pol_interest[c2$study == "qes2007"])))
})

test_that("a row every member passes takes the reason of the first usable member", {
  # the 2007 panel's pushed 1995 question put back in review: with the
  # default include_draft = FALSE it is not usable, and a respondent whose
  # intref1 is system missing gets the reason, type and item of the first
  # question (review of 2026-09-29), not not_reviewed from the push
  s <- hz_spec()
  xw <- s$tables$crosswalk
  push <- xw$study == "qes2007_panel" & xw$target == "sov_partnership_1995_push"
  xw$status[push] <- "review"
  xw$reviewed_by[push] <- NA_character_
  s$tables$crosswalk <- xw
  d <- .qes_synthetic("qes2007_panel", spec = s)$qes2007_panel
  x <- unclass(d$intref1)
  x[1] <- NA
  d$intref1[] <- x
  run <- function(draft) {
    h <- hz_run(list(qes2007_panel = d), targets = "sov_support", layout = "long", missing = "reasons",
                spec = s, include_draft = draft)
    h[h$source_row == 1L & h$wave == "pre", , drop = FALSE]
  }
  h <- run(FALSE)
  expect_identical(as.character(h$sov_support__na), "sysmis")
  expect_identical(as.character(h$sov_support__type), "partnership_1995")
  expect_identical(as.character(h$sov_support__item), "qes2007_panel:pre:intref1")
  h <- run(TRUE)
  expect_identical(as.character(h$sov_support__na), "sysmis")
  expect_identical(as.character(h$sov_support__type), "partnership_1995_push")
  # a pool whose every member is unusable still says why
  first <- s$tables$crosswalk$study == "qes2007_panel" & s$tables$crosswalk$target == "sov_partnership_1995"
  s$tables$crosswalk$status[first] <- "review"
  s$tables$crosswalk$review_note[first] <- "Held for a test."
  h <- run(FALSE)
  expect_identical(as.character(h$sov_support__na), "not_reviewed")
})

test_that("pol_interest times 10 is the legacy political_interest scale", {
  syn <- hz_syn(c("qes2012", "qes2007"))
  h <- hz_run(syn, targets = c("pol_interest", "interest_4pt", "interest_0_10"), values = "code")
  four <- !is.na(h$interest_4pt) & h$pol_interest__type %in% "general_4pt"
  expect_equal(10 * h$pol_interest[four], unname(.qes_legacy_scale10[h$interest_4pt[four]]))
  ten <- !is.na(h$interest_0_10) & h$pol_interest__type %in% "general_0_10"
  expect_equal(10 * h$pol_interest[ten], as.numeric(h$interest_0_10[ten]))
})

test_that("pooled columns, companions, printing, provenance and messages", {
  syn <- hz_syn("qes2014")
  withr::local_options(qesR.lang = "en")
  m <- NULL
  h <- withCallingHandlers(
    qes_harmonize(data = syn, targets = "vote_choice", include_draft = TRUE, quiet = FALSE),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    qesR_message_pooled = function(msg) {
      m <<- msg
      invokeRestart("muffleMessage")
    },
    message = function(msg) invokeRestart("muffleMessage")
  )
  expect_s3_class(m, "qesR_message_pooled")
  expect_match(conditionMessage(m), "vote_choice: recall (qes2014)", fixed = TRUE)
  expect_true(all(c("vote_choice", "vote_choice__type", "vote_choice__grade", "vote_choice__item") %in% names(h)))
  # the members are harmonized, but are columns only when named
  expect_false("vote_prov_recall" %in% names(h))
  expect_identical(attr(h$vote_choice, "label"), "Provincial vote choice (pooled)")
  expect_identical(levels(h$vote_choice), .qes_spec_levels(hz_spec()$tables$levels, "party_qc_intent")$label_en)
  expect_true(all(h$vote_choice__item[!is.na(h$vote_choice)] == "qes2014:post:Q3"))
  expect_output(print(h), "Pooled variables", fixed = TRUE)
  prov <- qes_provenance(h, level = "pooled")
  expect_identical(names(prov), c("study", "pooled", "type", "member", "precedence", "wave", "item", "grade",
                                  "used_in_layout", "included", "n_value", "n_answer_na", "n_fallthrough"))
  expect_identical(sum(prov$n_value), sum(!is.na(h$vote_choice)))
  expect_identical(sum(prov$n_value + prov$n_answer_na + prov$n_fallthrough), nrow(h))
  # cell provenance describes the target columns only
  expect_identical(nrow(qes_provenance(h, level = "cell")), 0L)
  # French labels, same codes
  fr <- hz_run(syn, targets = "vote_choice", lang = "fr")
  expect_identical(attr(fr$vote_choice, "label"), "Choix de vote provincial (regroupé)")
  expect_identical(as.character(fr$vote_choice__type), as.character(h$vote_choice__type))
  # "pooled" is a set; a pooled name is not a target of qes_spec()
  p <- hz_run(syn, targets = "pooled")
  expect_true(all(c("vote_choice", "sov_support", "pol_interest", "turnout") %in% names(p)))
  err <- expect_error(hz_run(syn, targets = "vote_choic"), class = "qesR_error_input")
  expect_true("vote_choice" %in% err$suggestions)
})

test_that("rbind() of per-study results with pooled variables equals one call", {
  syn <- hz_syn(c("qes2012", "qes2014"))
  both <- hz_run(syn, targets = c("vote_choice", "sov_support"))
  a <- hz_run(syn["qes2012"], targets = c("vote_choice", "sov_support"))
  b <- hz_run(syn["qes2014"], targets = c("vote_choice", "sov_support"))
  ab <- rbind(a, b)
  strip <- function(x) {
    attr(x, "qes_provenance") <- NULL
    attr(x, "qes_spec") <- NULL
    x
  }
  expect_identical(strip(ab), strip(both))
  expect_identical(qes_provenance(ab, level = "pooled"), qes_provenance(both, level = "pooled"))
  c1 <- hz_run(syn["qes2014"], targets = c("vote_choice", "sov_support"), types = list(vote_choice = "intention"))
  err <- expect_error(rbind(a, c1), class = "qesR_error_input")
  expect_identical(err$id, "hz_rbind_options")
})

test_that("qes_spec('pooled') gives each member's grade in each study", {
  v <- qes_spec("pooled")
  expect_identical(names(v)[1:15], c("pooled", "label", "definition", "type", "levels", "type_name", "type_label",
                                     "member", "precedence", "default", "transform", "grade_cap", "note", "status",
                                     "added_in"))
  vc <- qes_spec("pooled", targets = "vote_choice")
  expect_identical(vc$qes2012[vc$type_name == "recall"], "identical")
  expect_true(is.na(vc$qes2012[vc$type_name == "intention"]))
  expect_identical(qes_spec("pooled", targets = "sov_support")$qes2018_panel[5], "approximate")
  expect_match(qes_spec("pooled", targets = "pol_interest")$levels[1], "^0-1$")
  expect_identical(unique(qes_spec("pooled", targets = "vote_choice", lang = "fr")$label),
                   "Choix de vote provincial (regroupé)")
  expect_error(qes_spec("pooled", targets = "not_a_pool"), class = "qesR_error_input")
  # the reference has a chapter on them, in English and French alike
  en <- .spec_reference_md("en")
  expect_match(en, "## Pooled variables", fixed = TRUE)
  expect_match(.spec_reference_md("fr"), "## Variables regroupées", fixed = TRUE)
  expect_match(en, "{#pooled-vote_choice}", fixed = TRUE)
  expect_match(.spec_coverage_md("en"), "## Pooled variables by study", fixed = TRUE)
})

test_that("qes_design() weights each study with the column of its pooled values' wave", {
  skip_if_not_installed("survey")
  syn <- hz_syn(c("qes2012", "qes2022"))
  h <- hz_run(syn, targets = c("sov_support"), types = NULL)
  # 2012 asked sovereignty after the election, 2022 during the campaign
  g <- attr(h, "qes_weight_guide")
  expect_identical(g$weight_column[g$target == "sov_support"], c("weight_post", "weight_pre"))
  d <- suppressMessages(qes_design(h))
  expect_true("weight_auto" %in% names(d$variables))
  w <- d$variables
  expect_identical(w$weight_auto[w$study == "qes2012"], w$weight_post[w$study == "qes2012"])
  expect_identical(w$weight_auto[w$study == "qes2022"], w$weight_pre[w$study == "qes2022"])
  # a study whose targets call for both columns still asks to choose
  both <- hz_run(syn, targets = c("sov_support", "vote_prov_recall"))
  expect_error(suppressMessages(qes_design(both)), class = "qesR_error_input")
})

test_that("the validator rejects malformed pooled variables (V-F1 to V-F7)", {
  s <- hz_spec()
  bad <- function(edit) {
    t <- s
    t$tables <- edit(t$tables)
    p <- .qes_spec_check(t)
    unique(p$rule[p$severity == "error"])
  }
  expect_true("V-F1" %in% bad(function(t) {
    t$pooled_members$precedence[t$pooled_members$member == "vote_prov_intent"] <- 1L
    t
  }))
  expect_true("V-F2" %in% bad(function(t) {
    t$pooled$pooled[1] <- "wave"
    t$pooled_members$pooled[t$pooled_members$pooled == "vote_choice"] <- "wave"
    t
  }))
  expect_true("V-F3" %in% bad(function(t) {
    t$pooled_members$member[1] <- "no_such_target"
    t
  }))
  expect_true("V-F5" %in% bad(function(t) {
    t$pooled_members$member[t$pooled_members$type_name == "intention"] <- "pid_prov"
    t
  }))
  expect_true("V-F5" %in% bad(function(t) {
    k <- t$pooled_members$type_name == "general_4pt"
    t$pooled_members$transform[k] <- "score:very=1,quite=0.7"
    t
  }))
  expect_true("V-F6" %in% bad(function(t) {
    t$pooled_members$grade_cap[t$pooled_members$type_name == "favour"] <- NA
    t
  }))
  expect_true("V-F7" %in% bad(function(t) {
    t$pooled$label_fr[1] <- NA
    t
  }))
  expect_true("V-F4" %in% bad(function(t) {
    t$pooled_members$default[t$pooled_members$pooled == "turnout"] <- FALSE
    t
  }))
})

test_that("rule coalesce reads a question, then its push for those it passes", {
  syn <- hz_syn("qes2007")
  d <- syn$qes2007
  q19 <- unclass(d$q19)
  q20 <- unclass(d$q20)
  q19[1:4] <- c(8, 8, 8, 1)
  q20[1:4] <- c(1, 8, NA, NA)
  d$q19[] <- q19
  d$q20[] <- q20
  h <- hz_run(list(qes2007 = d), targets = c("sov_partnership_1995_push", "sov_partnership_1995"), values = "code",
              missing = "reasons", keep_source = TRUE)
  expect_identical(h$sov_partnership_1995_push[1:4], c("yes", NA, NA, "yes"))
  expect_identical(as.character(h$sov_partnership_1995_push__na[1:4]), c(NA, "dk", "dk", NA))
  expect_identical(h$sov_partnership_1995_push__src[1:4], c("q20=1", "q20=8", "q19=8", "q19=1"))
  # the first question alone is its own target
  expect_identical(as.character(h$sov_partnership_1995__na[1:3]), rep("dk", 3))
  # the code view lists every variable of the row
  codes <- qes_spec("crosswalk", targets = "sov_partnership_1995_push", studies = "qes2007", level = "code")
  expect_true(all(c("q19", "q20") %in% codes$variable))
  expect_identical(.qes_hz_row_vars(hz_spec()$tables$crosswalk[hz_spec()$tables$crosswalk$study == "qes2007" &
                                                                 hz_spec()$tables$crosswalk$target == "sov_partnership_1995_push", ], 1L),
                   c("q19", "q20"))
})

test_that("the age groups are derived from the year of birth where no row asks them", {
  syn <- hz_syn("qes2012")
  d <- syn$qes2012
  x <- unclass(d$agex)
  # fieldwork starts on 2012-09-12: the age reached in 2012
  x[1:5] <- c(1994, 1995, 1977, 1978, 9999)
  d$agex[] <- x
  h <- hz_run(list(qes2012 = d), targets = c("age_group3", "age_group6"), values = "code", missing = "reasons")
  expect_identical(h$age_group3[1:4], c("a18_34", NA, "a35_54", "a18_34"))
  expect_identical(h$age_group6[1:4], c("a18_24", NA, "a35_44", "a25_34"))
  # born in 1995: 17 in 2012, below the lowest band
  expect_identical(as.character(h$age_group3__na[c(2, 5)]), c("ineligible", "refused"))
  cell <- qes_provenance(h, level = "cell")
  expect_identical(cell$rule, c("derive:age_band", "derive:age_band"))
  expect_identical(cell$grade, c("approximate", "approximate"))
  expect_match(cell$note[1], "year of birth (agex)", fixed = TRUE)
  # below min_grade, and a direct row always wins
  h2 <- hz_run(list(qes2012 = d), targets = "age_group3", missing = "reasons", min_grade = "comparable")
  expect_true(all(h2$age_group3__na %in% "below_grade"))
  p <- hz_run(hz_syn("qes2012_panel"), targets = "age_group3")
  expect_identical(qes_provenance(p, level = "cell")$rule, "map")
})

test_that("qes_party_lineage() joins the ADQ and the CAQ, graded approximate before 2012", {
  syn <- hz_syn(c("qes2008", "qes2012"))
  h <- hz_run(syn, targets = c("vote_choice", "vote_prov_recall"))
  l <- qes_party_lineage(h)
  expect_true(all(c("vote_choice_lineage", "vote_choice_lineage__grade", "vote_prov_recall_lineage") %in% names(l)))
  expect_true("ADQ/CAQ" %in% levels(l$vote_choice_lineage))
  expect_false(any(c("ADQ", "CAQ") %in% levels(l$vote_choice_lineage)))
  adq <- h$vote_choice %in% "ADQ"
  expect_true(any(adq))
  expect_true(all(l$vote_choice_lineage[adq] == "ADQ/CAQ"))
  caq <- h$vote_choice %in% "CAQ"
  expect_true(all(l$vote_choice_lineage[caq] == "ADQ/CAQ"))
  expect_true(all(l$vote_choice_lineage__grade[h$study == "qes2008" & !is.na(h$vote_choice)] == "approximate"))
  expect_identical(as.character(l$vote_choice_lineage__grade[h$study == "qes2012"]),
                   as.character(h$vote_choice__grade[h$study == "qes2012"]))
  # codes, and Option nationale into QS on request
  hc <- hz_run(syn, targets = "vote_choice", values = "code")
  lc <- qes_party_lineage(hc, lineage = c("adq_caq", "on_qs"))
  expect_true(all(lc$vote_choice_lineage[hc$vote_choice %in% c("ADQ", "CAQ")] == "ADQ_CAQ"))
  expect_true(all(lc$vote_choice_lineage[hc$vote_choice %in% "ON"] == "QS"))
  expect_error(qes_party_lineage(h, cols = "study"), class = "qesR_error_input")
  expect_error(qes_party_lineage(h, lineage = "x"), class = "qesR_error_input")
  expect_error(qes_party_lineage(data.frame(a = 1)), class = "qesR_error_input")
})

test_that("rows nobody has reviewed and derived cells never reach the legacy columns (V-S19)", {
  local_qes_notices_shown()
  local_fake_legacy()
  now <- suppressMessages(get_qes_master(surveys = c("qes2012", "qes2022"), quiet = TRUE))
  dnow <- suppressWarnings(suppressMessages(get_decon("qes2022", quiet = TRUE)))
  d12now <- suppressWarnings(suppressMessages(get_decon("qes2012", quiet = TRUE)))
  # the legacy renderers with the spec frozen before the rows of 4.3.0 (all
  # reviewed on 2026-09-29 or not reviewed yet): no such row, no legacy
  # freeze row, no derivation, no pooled variable
  frozen <- legacy_signed_spec()
  xw <- frozen$tables$crosswalk
  frozen$tables$crosswalk <- xw[!is.na(xw$reviewed_on) & xw$reviewed_on < as.Date("2026-09-29"), , drop = FALSE]
  lg <- frozen$tables$legacy
  frozen$tables$legacy <- lg[!lg$cause %in% "legacy_frozen", , drop = FALSE]
  frozen$tables$targets$derive_rule <- NA_character_
  frozen$tables$targets$derive_from <- NA_character_
  frozen$tables$pooled <- frozen$tables$pooled[0, , drop = FALSE]
  frozen$tables$pooled_members <- frozen$tables$pooled_members[0, , drop = FALSE]
  get_spec <- getFromNamespace(".qes_spec_get", "qesR")
  testthat::local_mocked_bindings(
    .qes_spec_get = function(spec = NULL, validate = "error") if (is.null(spec)) frozen else get_spec(spec, validate),
    .package = "qesR"
  )
  then <- suppressMessages(get_qes_master(surveys = c("qes2012", "qes2022"), quiet = TRUE))
  dthen <- suppressWarnings(suppressMessages(get_decon("qes2022", quiet = TRUE)))
  d12then <- suppressWarnings(suppressMessages(get_decon("qes2012", quiet = TRUE)))
  # the values are those of the frozen spec, and so are the reasons of
  # legacy_na_columns; only the cause and basis of the frozen cells
  # (legacy.csv study rows of cause legacy_frozen) differ
  frozen_cells <- c("qes2012 federal_pid", "qes2012 fed_pid", "qes2022 language")
  cells <- function(x) {
    na <- attr(x, "legacy_na_columns")
    if (is.null(na)) return(NULL)
    hit <- paste(na$study, na$column) %in% frozen_cells
    na$cause[hit] <- NA_character_
    na$basis[hit] <- NA_character_
    na
  }
  values <- function(x) {
    attributes(x) <- attributes(x)[c("names", "row.names", "class")]
    x
  }
  expect_identical(values(now), values(then))
  expect_identical(values(dnow), values(dthen))
  expect_identical(cells(now), cells(then))
  expect_identical(cells(dnow), cells(dthen))
  expect_identical(values(d12now), values(d12then))
  expect_identical(cells(d12now), cells(d12then))
  expect_true(all(is.na(d12now$fed_pid)))
  expect_true(all(is.na(now$language[now$qes_code == "qes2022"])))
  expect_true(all(is.na(now$federal_pid[now$qes_code == "qes2012"])))
  na <- attr(now, "legacy_na_columns")
  hit <- na[paste(na$study, na$column) %in% frozen_cells, , drop = FALSE]
  expect_setequal(paste(hit$study, hit$column), c("qes2012 federal_pid", "qes2022 language"))
  expect_true(all(hit$reason == "no_source" & hit$cause == "legacy_frozen"))
  # the rows the frozen columns would read are signed off
  xw <- get_spec(NULL, "none")$tables$crosswalk
  expect_true(all(xw$status[xw$study == "qes2012" & xw$target == "pid_fed"] == "stable"))
  expect_true(all(xw$status[xw$study == "qes2022" & xw$target == "lang_mother"] == "stable"))
})
