# Conditions and the message table (design.md section 7, slice S0c).
# Tests assert classes and fields; message text is compared only between the
# two languages of the table, never against a literal.

messages <- function() qesR:::.qes_messages

placeholders <- function(x) {
  sort(regmatches(x, gregexpr("%[0-9]+\\$[-0-9.]*[sdif]|%[-0-9.]*[sdif]", x))[[1]])
}

non_positional <- function(x) {
  x <- gsub("%%", "", x, fixed = TRUE)
  grepl("%", gsub("%[0-9]+\\$[-0-9.]*[sdif]", "", x))
}

test_that("every message key has distinct, non-empty English and French text", {
  for (key in names(messages())) {
    entry <- messages()[[key]]
    expect_identical(sort(names(entry)), c("en", "fr"), info = key)
    expect_true(all(nzchar(entry)), info = key)
    expect_true(all(validUTF8(entry)), info = key)
    expect_false(identical(entry[["en"]], entry[["fr"]]), info = key)
  }
})

test_that("both languages of a key use the same positional placeholders", {
  for (key in names(messages())) {
    entry <- messages()[[key]]
    expect_identical(placeholders(entry[["en"]]), placeholders(entry[["fr"]]), info = key)
    # positional placeholders only (%1$s), so a translation may reorder them
    expect_false(non_positional(entry[["en"]]), info = key)
    expect_false(non_positional(entry[["fr"]]), info = key)
  }
})

test_that("every key renders in both languages", {
  for (key in names(messages())) {
    n <- length(unique(placeholders(messages()[[key]][["en"]])))
    args <- as.list(rep("x", n))
    expect_type(qesR:::.qes_msg(key, args, "en"), "character")
    expect_type(qesR:::.qes_msg(key, args, "fr"), "character")
  }
})

test_that("message language follows option, QESR_LANG, LANGUAGE, then locale", {
  lang <- qesR:::.qes_lang
  withr::local_options(qesR.lang = NULL)
  withr::local_envvar(QESR_LANG = NA, LANGUAGE = "fr_CA:fr")
  expect_identical(lang(), "fr")
  withr::local_envvar(LANGUAGE = "en")
  expect_identical(lang(), "en")
  withr::local_envvar(QESR_LANG = "FR")
  expect_identical(lang(), "fr")
  withr::local_options(qesR.lang = "en")
  expect_identical(lang(), "en")
  withr::local_options(qesR.lang = "fr")
  expect_identical(lang(), "fr")
})

test_that("errors carry the class chain, id, lang and data fields", {
  local_fake_dataverse()
  withr::local_options(qesR.lang = "en")
  err <- expect_error(get_qes("QES 2022", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_identical(
    class(err),
    c("qesR_error_unknown_study", "qesR_error", "error", "condition")
  )
  expect_identical(err$id, "unknown_study_suggest")
  expect_identical(err$lang, "en")
  expect_identical(err$study, "QES 2022")
  expect_null(err$call)
})

test_that("nested classes inherit their parents", {
  chain <- qesR:::.qes_class_chain
  expect_identical(
    chain("qesR_error_http_refused", "qesR_error"),
    c("qesR_error_http_refused", "qesR_error_http", "qesR_error_network", "qesR_error")
  )
  expect_identical(
    chain("qesR_error_checksum", "qesR_error"),
    c("qesR_error_checksum", "qesR_error_source", "qesR_error")
  )
  expect_identical(chain("qesR_error_input", "qesR_error"), c("qesR_error_input", "qesR_error"))
})

test_that("the message language changes the text, never the class or fields", {
  local_fake_dataverse()
  withr::local_options(qesR.lang = "en")
  en <- expect_error(get_qes("QES 2022", quiet = TRUE), class = "qesR_error_unknown_study")
  withr::local_options(qesR.lang = "fr")
  fr <- expect_error(get_qes("QES 2022", quiet = TRUE), class = "qesR_error_unknown_study")
  expect_identical(fr$lang, "fr")
  expect_identical(class(en), class(fr))
  expect_identical(en[c("id", "study", "suggestions")], fr[c("id", "study", "suggestions")])
  expect_false(identical(conditionMessage(en), conditionMessage(fr)))
  # a condition can be rendered again in a fixed language
  expect_identical(qesR:::.qes_condition_text(fr, "en"), conditionMessage(en))
})

test_that("a parent condition is kept and its message appended", {
  parent <- simpleError("root cause text")
  err <- tryCatch(
    qesR:::.qes_abort(
      "network",
      class = "qesR_error_network",
      args = list(qesR:::.qes_q("https://example.org/x")),
      data = list(url = "https://example.org/x", attempts = 1L),
      parent = parent
    ),
    error = function(e) e
  )
  expect_s3_class(err, "qesR_error_network")
  expect_identical(err$parent, parent)
  expect_true(grepl("root cause text", conditionMessage(err), fixed = TRUE))
  expect_identical(err$attempts, 1L)
})

test_that("informational messages are classed and silenced by quiet", {
  local_fake_dataverse()
  expect_gt(count_class(get_qes("qes2018", assign_global = FALSE), "qesR_message_download"), 0L)
  expect_no_message(get_qes("qes2018", assign_global = FALSE, quiet = TRUE))
})

test_that("every message get_qes() and get_qes_master() emit has a documented subclass", {
  local_fake_dataverse()
  local_qes_once()
  documented <- c(
    "qesR_message_download", "qesR_message_assign_default", "qesR_message_deprecated",
    "qesR_message_values_changed", "qesR_message_legacy_columns"
  )
  seen <- list()
  collect <- function(expr) {
    withCallingHandlers(expr, message = function(m) {
      seen[[length(seen) + 1L]] <<- m
      invokeRestart("muffleMessage")
    })
  }
  collect(get_qes("qes2018"))
  collect(get_qes_master(surveys = "qes2018", save_path = withr::local_tempfile(fileext = ".csv")))
  expect_gt(length(seen), 0L)
  for (m in seen) {
    expect_true(inherits(m, "qesR_message"), info = class(m)[1])
    expect_true(inherits(m, documented), info = class(m)[1])
  }
})

test_that("get_question() warns with a qesR_warning and its variable", {
  local_qes_notices_shown()
  dat <- data.frame(x = 1:3)
  w <- expect_warning(get_question(dat, "x"), class = "qesR_warning")
  expect_identical(w$variable, "x")
  expect_identical(w$id, "question_missing")
})

test_that("get_qes_master() records failures in English whatever the language", {
  local_fake_dataverse(fail = "qes2008")
  withr::local_options(qesR.lang = "fr")
  master <- get_qes_master(surveys = c("qes2018", "qes2008"), assign_global = FALSE, quiet = TRUE)
  withr::local_options(qesR.lang = "en")
  again <- get_qes_master(surveys = c("qes2018", "qes2008"), assign_global = FALSE, quiet = TRUE)
  expect_identical(attr(master, "failed_surveys"), attr(again, "failed_surveys"))
  expect_match(attr(master, "failed_surveys"), "^qes2008: ")

  err <- expect_error(
    get_qes_master(surveys = c("qes2018", "qes2008"), assign_global = FALSE, quiet = TRUE, strict = TRUE),
    class = "qesR_error_source"
  )
  expect_identical(err$study, "qes2008")
  expect_s3_class(err$parent, "error")
  expect_named(err$failures, "qes2008")
  # the per-study reasons are not pre-rendered into the arguments (they would
  # stay in English under qesR.lang = "fr"); only the codes are passed
  expect_s3_class(err$args[[2]], "qesR_quoted")

  local_fake_dataverse(fail = c("qes2018", "qes2008"))
  err <- expect_error(
    get_qes_master(surveys = c("qes2018", "qes2008"), assign_global = FALSE, quiet = TRUE),
    class = "qesR_error_source"
  )
  expect_setequal(err$study, c("qes2018", "qes2008"))
})

test_that("unsupported and RDS files are never read", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1), path)
  err <- expect_error(qesR:::.qes_read_file(path, "rds"), class = "qesR_error_source")
  expect_identical(err$format, "rds")
})
