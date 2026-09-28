# qes_question(), the qes2022 metadata shard and locale identity (slice S3,
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

# A synthetic qes2022 file: an 80-character label (truncated), a -99 item
# nonresponse, a select-all column (1/-99, typed not_selected by the shipped
# rules) and a vote item whose codes 9 and 10 the rules type.
fake_2022 <- function() {
  long <- substr(paste0("Which party do you think you will vote for? ", strrep("And more words ", 5)), 1, 80)
  stopifnot(nchar(long) == 80L)
  d <- data.frame(
    ResponseId = c("R1", "R2", "R3", "R4"),
    cps_votechoice1 = haven::labelled(c(1, 9, 10, 3), labels = c("Parti A" = 1, "Parti C" = 3, "Refused" = 9, "DK" = 10)),
    cps_lang_2 = c(1, -99, 1, -99),
    cps_ideoself_1 = c(0, 5, -99, 10),
    stringsAsFactors = FALSE
  )
  attr(d$cps_votechoice1, "label") <- long
  attr(d$cps_lang_2, "label") <- "Which language(s) did you learn as a child? - Selected Choice French"
  attr(d$cps_ideoself_1, "label") <- "Where would you place yourself?"
  d
}

test_that("qes2022 metadata is built from the user's copy, as a CSV shard in the cache ([A:K3], OD3)", {
  local_qes_notices_shown()
  local_fake_dataverse(data = list(qes2022 = fake_2022()))
  root <- withr::local_tempdir()
  withr::local_options(qesR.cache_dir = root, qesR.cache = "disk")

  cb <- qes_codebook("qes2022", quiet = TRUE)
  expect_identical(cb$variable, names(fake_2022()))
  v <- cb[cb$variable == "cps_votechoice1", ]
  expect_true(v$question_truncated)
  expect_identical(v$question_source, "file")
  expect_identical(v$doc_ref, "7449514")
  expect_identical(v$missing_codes, "9=refused | 10=dk")
  expect_identical(cb$missing_codes[cb$variable == "cps_lang_2"], "-99=not_selected")
  expect_identical(cb$missing_codes[cb$variable == "cps_ideoself_1"], "-99=no_answer")

  info <- qes_cache_info()
  shards <- info[info$kind == "shard", ]
  expect_identical(nrow(shards), 2L)
  expect_true(all(grepl("^qes2022-[0-9a-f]{32}-s2\\.(variables|values)\\.csv$", basename(shards$path))))
  expect_identical(unique(shards$study), "qes2022")
  # a second session reads the shard, not the data
  getFromNamespace(".qes_dict_forget", "qesR")()
  testthat::local_mocked_bindings(
    .qes_read = function(...) stop("the shard should be read, not the data"),
    .package = "qesR"
  )
  expect_identical(qes_codebook("qes2022", quiet = TRUE), cb)
  # qes_search() picks the shard up
  hits <- qes_search("will vote for")
  expect_true("cps_votechoice1" %in% hits$variable[hits$study == "qes2022"])
  expect_false("qes2022" %in% attr(hits, "not_searchable"))
  # the shard is cleared with the cache
  suppressMessages(qes_cache_clear(studies = "qes2022"))
  expect_identical(nrow(qes_cache_info()[qes_cache_info()$kind == "shard", ]), 0L)
})

test_that("get_qes('qes2022') builds the shard from the file it reads, and qes_missing() uses it", {
  local_qes_notices_shown()
  log <- local_fake_dataverse(data = list(qes2022 = fake_2022()))
  d <- get_qes("qes2022", quiet = TRUE)
  expect_length(log$urls, 1L)
  w <- expect_warning(q <- get_question(d, "cps_votechoice1"), class = "qesR_warning_truncated")
  expect_identical(nchar(q), 80L)
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

# More than 50 distinct values, so the values table lists no observed code:
# -99 in an unlabelled income or year of birth is still "no answer".
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

test_that("the generic qes2022 rule (-99 = no answer) covers unlabelled numeric columns", {
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
  # the same without the shard: tables built from the columns at hand
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
  # a new session: no shard in memory, no cache, no data in memory
  forget()
  memo_clear()
  out <- qes_missing(d, quiet = TRUE)
  expect_identical(as.numeric(unclass(out$cps_ideoself_1)), c(0, 5, NA, 10))
  forget()
  q <- qes_question(d, "cps_ideoself_1")
  expect_identical(q$question, "Where would you place yourself?")
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
  expect_warning(get_question(bare, "cps_votechoice1"), class = "qesR_warning_truncated")
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

test_that("qes_question('qes2022') announces the download of an uncached file", {
  local_qes_notices_shown()
  log <- local_fake_dataverse(data = list(qes2022 = fake_2022()))
  expect_message(q <- qes_question("qes2022", "cps_ideoself_1"), class = "qesR_message_download")
  expect_length(log$urls, 1L)
  expect_identical(q$question, "Where would you place yourself?")
  # in memory now: no second download, no message
  expect_no_message(qes_question("qes2022", "cps_ideoself_1"))
  expect_length(log$urls, 1L)
})
