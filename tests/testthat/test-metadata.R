# qes_question(), the shipped qes2022 metadata and locale identity (slice S3,
# design.md sections 2.2, 6.1 and 8.1).

test_that("qes_question() returns the exact wording, with its source", {
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  q <- qes_question("qes2014", c("Q19", "Q2"))
  expect_identical(
    names(q),
    c("study", "variable", "question", "question_lang", "truncated", "source", "doc_ref", "universe")
  )
  expect_identical(q$variable, c("Q19", "Q2"))
  expect_identical(q$question_lang, c("fr", "fr"))
  expect_identical(q$truncated, c(FALSE, FALSE))
  expect_identical(q$source, c("questionnaire", "questionnaire"))
  en <- qes_question("qes2014", "Q19", lang = "en")
  expect_match(en$question, "independent country", fixed = TRUE)
  expect_identical(en$question_lang, "en")
  # universe from the questionnaire's routing (qes2018 q6: asked of voters)
  q6 <- qes_question("qes2018", "q6")
  expect_false(is.na(q6$universe))
  # unknown wording is NA, with no source
  none <- qes_question("qes2007_panel", "interet", lang = "en")
  expect_true(is.na(none$question))
  expect_true(is.na(none$source))
  # exact names only, several studies are an error
  err <- expect_error(qes_question("qes2014", "q19"), class = "qesR_error_unknown_variable")
  expect_identical(err$variables, "q19")
  expect_true("Q19" %in% err$suggestions)
  expect_error(qes_question(c("qes2012", "qes2014"), "q1"), class = "qesR_error_input")
  expect_error(qes_question("qes2014", "Q19", lang = "de"), class = "qesR_error_input")
})

test_that("qes_question() accepts data read by get_qes()", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  expect_identical(qes_question(demo, "Q19"), qes_question("qes_demo", "Q19"))
})

# A synthetic qes2022 file with some of the study's variable names: a vote
# item whose codes 9 and 10 the shipped dictionary types, a select-all column
# (1/-99, not_selected) and a 0-10 item whose -99 is item nonresponse. Its
# labels are made up: the description comes from the shipped dictionary.
fake_2022 <- function() {
  d <- data.frame(
    ResponseId = c("R1", "R2", "R3", "R4"),
    cps_votechoice1 = haven::labelled(c(1, 9, 10, 3), labels = c("Parti A" = 1, "Parti C" = 3, "Refused" = 9, "DK" = 10)),
    cps_lang_2 = c(1, -99, 1, -99),
    cps_ideoself_1 = c(0, 5, -99, 10),
    stringsAsFactors = FALSE
  )
  attr(d$cps_votechoice1, "label") <- "Which party do you think you will vote for?"
  attr(d$cps_lang_2, "label") <- "Which language(s) did you learn as a child? - Selected Choice French"
  attr(d$cps_ideoself_1, "label") <- "Where would you place yourself?"
  d
}

test_that("qes2022 metadata ships: codebook, question and search work offline ([A:K3], OD3 lifted)", {
  local_qes_notices_shown()
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .qes_read = function(...) stop("no data file should be read"),
    .package = "qesR"
  )
  root <- withr::local_tempdir()
  withr::local_options(qesR.cache_dir = root, qesR.cache = "disk")

  cb <- qes_codebook("qes2022", quiet = TRUE)
  expect_identical(nrow(cb), 718L)
  v <- cb[cb$variable == "cps_votechoice1", ]
  expect_identical(v$question, "Which party do you think you will vote for?")
  expect_false(v$question_truncated)
  expect_identical(v$question_source, "codebook")
  expect_identical(v$doc_ref, "7449514:cps_votechoice1")
  expect_match(v$missing_codes, "9=refused | 10=dk", fixed = TRUE)
  expect_identical(cb$missing_codes[cb$variable == "cps_lang_2"], "-99=not_selected")
  expect_identical(cb$missing_codes[cb$variable == "cps_ideoself_1"], "-99=no_answer")
  # nothing is written to the cache
  expect_identical(nrow(qes_cache_info()), 0L)

  fr <- qes_question("qes2022", "cps_votechoice1", lang = "fr")
  expect_identical(fr$question, "Pour quel parti pr\u00e9voyez-vous voter?")
  expect_identical(fr$source, "codebook")
  long <- qes_codebook("qes2022", layout = "long", variables = "cps_turnout", lang = "fr")
  expect_identical(long$value_label[long$value %in% "1"], "Certain to vote")

  # the legacy codebook functions
  old <- suppressWarnings(suppressMessages(get_codebook("qes2022", quiet = TRUE)))
  expect_identical(nrow(old), 718L)
  expect_true("cps_votechoice1" %in% names(get_value_labels(old)))
})

test_that("qes_search() finds qes2022 items in English and French, offline", {
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  en <- qes_search("will vote for", studies = "qes2022", lang = "en")
  expect_true("cps_votechoice1" %in% en$variable)
  expect_identical(attr(en, "not_searchable"), character(0))
  # French question text, with or without accents
  fr <- qes_search("prevoyez-vous voter", studies = "qes2022", fields = "question", lang = "fr")
  expect_true("cps_votechoice1" %in% fr$variable)
  expect_identical(fr$question_lang[fr$variable == "cps_votechoice1"], "fr")
  # French value labels, from the codebook
  vals <- qes_search("Je ne sais pas", studies = "qes2022", fields = "values", lang = "fr")
  expect_true("cps_votechoice1" %in% vals$variable)
  # every study is searchable, and qes2022 with the others
  all <- qes_search("souverainet\u00e9|sovereignty")
  expect_true("qes2022" %in% all$study)
  expect_identical(attr(all, "not_searchable"), character(0))
  cov <- attr(all, "coverage")
  expect_gt(cov$n_question[cov$study == "qes2022"], 400L)
})

test_that("get_qes('qes2022') attaches the shipped codebook, and qes_missing() uses it", {
  local_qes_notices_shown()
  log <- local_fake_dataverse(data = list(qes2022 = fake_2022()))
  d <- get_qes("qes2022", quiet = TRUE)
  expect_length(log$urls, 1L)
  expect_identical(get_question(d, "cps_votechoice1"), "Which party do you think you will vote for?")
  out <- qes_missing(d, quiet = TRUE)
  plain <- function(x) {
    attributes(x) <- NULL
    x
  }
  expect_identical(plain(out$cps_votechoice1), c(1, NA, NA, 3))
  # -99 in a select-all item means "not selected": kept by default
  expect_identical(out$cps_lang_2, d$cps_lang_2)
  expect_identical(plain(out$cps_ideoself_1), c(0, 5, NA, 10))
  expect_length(log$urls, 1L)
})

# More than 50 distinct values: -99 in an unlabelled income or year of birth
# is still "no answer" (the shipped dictionary has a row for that code).
fake_2022_wide <- function() {
  n <- 60L
  d <- data.frame(
    ResponseId = sprintf("R%02d", seq_len(n)),
    cps_income = c(-99, -99, seq(1000, by = 1500, length.out = n - 2L)),
    cps_yob = c(-99, seq(1930, by = 1, length.out = n - 1L)),
    cps_lang_2 = rep(c(1, -99), n / 2L),
    stringsAsFactors = FALSE
  )
  attr(d$cps_income, "label") <- "Household income"
  attr(d$cps_yob, "label") <- "Year of birth"
  d
}

test_that("the qes2022 code rule (-99 = no answer) covers unlabelled numeric columns", {
  local_qes_notices_shown()
  local_fake_dataverse(data = list(qes2022 = fake_2022_wide()))
  d <- get_qes("qes2022", quiet = TRUE)
  cb <- qes_codebook(d, quiet = TRUE)
  expect_identical(cb$missing_codes[cb$variable == "cps_income"], "-99=no_answer")
  expect_identical(cb$missing_codes[cb$variable == "cps_yob"], "-99=no_answer")
  expect_identical(cb$missing_codes[cb$variable == "cps_lang_2"], "-99=not_selected")
  out <- qes_missing(d, quiet = TRUE)
  expect_identical(sum(is.na(out$cps_income)), 2L)
  expect_false(any(unclass(out$cps_income) %in% -99, na.rm = TRUE))
  expect_identical(sum(is.na(out$cps_yob)), 1L)
  # a select-all item keeps its -99 by default
  expect_identical(out$cps_lang_2, d$cps_lang_2)
  tagged <- qes_missing(d, variables = "cps_income", action = "tagged", quiet = TRUE)
  expect_identical(sum(haven::na_tag(tagged$cps_income) %in% "o"), 2L)
  # a subset of the columns, with no codebook attached
  getFromNamespace(".qes_dict_forget", "qesR")()
  sub <- d[, c("ResponseId", "cps_income")]
  attr(sub, "qes_survey_code") <- "qes2022"
  expect_identical(sum(is.na(qes_missing(sub, quiet = TRUE)$cps_income)), 2L)
})

test_that("functions given qes2022 data never download it again to describe it", {
  local_qes_notices_shown()
  log <- local_fake_dataverse(data = list(qes2022 = fake_2022()))
  d <- get_qes("qes2022", quiet = TRUE)
  expect_length(log$urls, 1L)
  forget <- getFromNamespace(".qes_dict_forget", "qesR")
  memo_clear <- getFromNamespace(".qes_memo_clear", "qesR")
  # a new session: nothing in memory
  forget()
  memo_clear()
  out <- qes_missing(d, quiet = TRUE)
  expect_identical(as.numeric(unclass(out$cps_ideoself_1)), c(0, 5, NA, 10))
  forget()
  q <- qes_question(d, "cps_ideoself_1")
  expect_match(q$question, "left and right", fixed = TRUE)
  forget()
  bare <- d
  attr(bare, "qes_codebook") <- NULL
  cb <- qes_codebook(bare, quiet = TRUE)
  expect_identical(cb$missing_codes[cb$variable == "cps_ideoself_1"], "-99=no_answer")
  forget()
  sub <- d[, c("ResponseId", "cps_lang_2")]
  attr(sub, "qes_survey_code") <- "qes2022"
  expect_identical(
    unclass(qes_missing(sub, quiet = TRUE)$cps_lang_2),
    unclass(sub$cps_lang_2)
  )
  forget()
  expect_no_warning(get_question(bare, "cps_votechoice1"))
  expect_length(log$urls, 1L)
})

test_that("qes_question() reports unknown names against the study, exact match first", {
  err <- expect_error(qes_question("qes2014", "q19"), class = "qesR_error_unknown_variable")
  expect_identical(err$suggestions[1], "Q19")
  expect_identical(err$id, "unknown_variable_study_suggest")
  expect_match(conditionMessage(err), "qes2014", fixed = TRUE)
  expect_no_match(conditionMessage(err), "in the data", fixed = TRUE)
  err <- expect_error(qes_question("qes2014", character(0)), class = "qesR_error_input")
  expect_identical(err$id, "input_variables")
  expect_error(qes_question("qes2014", NA_character_), class = "qesR_error_input")
  err <- expect_error(qes_codebook("qes2014", variables = "zzzzzzzz"), class = "qesR_error_unknown_variable")
  expect_identical(err$id, "unknown_variable_study")
})

test_that("qes_codebook(file =) names the file whose variable names it uses", {
  local_qes_notices_shown()
  cb <- qes_codebook("qes2012", quiet = TRUE)
  expect_identical(attr(cb, "variable_names_file"), attr(cb, "selected_data_file"))
  spss <- suppressMessages(qes_codebook("qes2012", file = "SPSS", quiet = TRUE))
  expect_false(identical(attr(spss, "selected_data_file"), attr(cb, "selected_data_file")))
  expect_identical(attr(spss, "variable_names_file"), attr(cb, "selected_data_file"))
  expect_identical(spss$variable, cb$variable)
  # the attribute survives a new layout
  expect_identical(attr(qes_codebook(spss, layout = "long"), "variable_names_file"), attr(cb, "selected_data_file"))
})

test_that("codebook, question and search output do not depend on the locale", {
  local_qes_notices_shown()
  cb <- qes_codebook("qes2007_panel", layout = "long")
  q <- qes_question("qes2014", c("Q19", "Q28"), lang = "fr")
  s <- qes_search(paste0("r", intToUtf8(0xE9), "f", intToUtf8(0xE9), "rendum"), studies = "qes2014")
  env <- getFromNamespace(".qes_dict_cache", "qesR")
  withr::local_envvar(LANGUAGE = "fr")
  withr::local_locale(c(LC_COLLATE = "C", LC_CTYPE = "C"))
  rm(list = ls(env, all.names = TRUE), envir = env)
  withr::defer(rm(list = ls(env, all.names = TRUE), envir = env))
  expect_identical(qes_codebook("qes2007_panel", layout = "long"), cb)
  expect_identical(qes_question("qes2014", c("Q19", "Q28"), lang = "fr"), q)
  expect_identical(
    qes_search(paste0("r", intToUtf8(0xE9), "f", intToUtf8(0xE9), "rendum"), studies = "qes2014"),
    s
  )
})

test_that("qes_question('qes2022') downloads nothing: its wording ships", {
  local_qes_notices_shown()
  log <- local_fake_dataverse(data = list(qes2022 = fake_2022()))
  expect_no_message(q <- qes_question("qes2022", "cps_ideoself_1"))
  expect_length(log$urls, 0L)
  expect_match(q$question, "left and right", fixed = TRUE)
  expect_identical(q$doc_ref, "7449514:cps_ideoself_1")
})

test_that("codebooks, qes_question() and qes_search() results of qes2022 keep the licence notice", {
  local_qes_notices_shown()
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  withr::local_options(qesR.lang = "en")
  cb <- qes_codebook("qes2022", quiet = TRUE, variables = "cps_turnout")
  ln <- attr(cb, "licence_notice", exact = TRUE)
  expect_identical(names(ln), "qes2022")
  expect_match(ln, "CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)", fixed = TRUE)
  # kept by every layout, and absent for a CC0 study
  expect_identical(attr(qes_codebook("qes2022", quiet = TRUE, layout = "long", variables = "cps_turnout"),
                        "licence_notice", exact = TRUE), ln)
  expect_null(attr(qes_codebook("qes2014", quiet = TRUE, variables = "Q19"), "licence_notice", exact = TRUE))
  q <- qes_question("qes2022", "cps_qc_referendum")
  expect_identical(attr(q, "licence_notice", exact = TRUE), ln)
  expect_null(attr(qes_question("qes2014", "Q19"), "licence_notice", exact = TRUE))
  hits <- qes_search("referendum", studies = c("qes2022", "qes2014"), fields = "variable")
  expect_true("qes2022" %in% hits$study)
  expect_identical(attr(hits, "licence_notice", exact = TRUE), ln)
  out <- paste(utils::capture.output(print(hits)), collapse = " ")
  expect_match(out, "CC BY-NC 4.0", fixed = TRUE)
  # no qes2022 row, no notice
  expect_null(attr(qes_search("Q19", studies = "qes2014", fields = "variable"), "licence_notice", exact = TRUE))
  withr::local_options(qesR.lang = "fr")
  expect_match(attr(qes_question("qes2022", "cps_qc_referendum"), "licence_notice", exact = TRUE),
               "licence MIT de qesR", fixed = TRUE)
})

test_that("a printed qes2022 codebook and the harmonization reference carry the licence notice", {
  local_qes_notices_shown()
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  withr::local_options(qesR.lang = "en", width = 200)
  out <- paste(utils::capture.output(print(qes_codebook("qes2022", quiet = TRUE), n = 1)), collapse = " ")
  expect_match(out, "CC BY-NC 4.0", fixed = TRUE)
  expect_match(out, "https://creativecommons.org/licenses/by-nc/4.0/", fixed = TRUE)
  expect_match(out, "https://doi.org/10.7910/DVN/PAQBDR", fixed = TRUE)
  expect_match(out, "Mahéo, Bélanger, Stephenson and Harell", fixed = TRUE)
  expect_match(out, "not covered by qesR's MIT licence", fixed = TRUE)
  # a CC0 study has no notice
  cc0 <- paste(utils::capture.output(print(qes_codebook("qes2014", quiet = TRUE), n = 1)), collapse = " ")
  expect_false(grepl("CC BY-NC", cc0, fixed = TRUE))
  notice <- getFromNamespace(".qes_licence_notice", "qesR")
  expect_null(notice("qes2014"))
  expect_null(notice("qes_demo"))
  expect_match(notice("qes2022", "fr"), "Mahéo, Bélanger, Stephenson et Harell", fixed = TRUE)
  expect_match(notice("qes2022", "fr"), "licence MIT de qesR", fixed = TRUE)
  # the notice says the material was adapted and where the changes are listed
  expect_match(notice("qes2022", "en"), "Adapted by qesR", fixed = TRUE)
  expect_match(notice("qes2022", "en"), "system.file(\"COPYRIGHTS\", package = \"qesR\")", fixed = TRUE)
  expect_match(notice("qes2022", "fr"), "Adapt\u00e9e par qesR", fixed = TRUE)
  # the harmonization reference quotes the spec's qes2022 wording: it says so
  ref <- getFromNamespace(".spec_reference_md", "qesR")
  expect_true(any(grepl("CC BY-NC 4.0", ref(lang = "en"), fixed = TRUE)))
  expect_true(any(grepl("CC BY-NC 4.0", ref(lang = "fr"), fixed = TRUE)))
})
