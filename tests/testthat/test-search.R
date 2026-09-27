# Bilingual search (slice S3, design.md section 6.3).

fold <- function(x) getFromNamespace(".qes_fold", "qesR")(x)

# Forget everything read from the dictionary, so the calling test reads the
# shipped files again (e.g. under another locale); forgotten again at the end.
local_fresh_dict <- function(.env = parent.frame()) {
  env <- getFromNamespace(".qes_dict_cache", "qesR")
  rm(list = ls(env, all.names = TRUE), envir = env)
  withr::defer(rm(list = ls(env, all.names = TRUE), envir = env), envir = .env)
}

test_that("the fold table is built from code points, and folds accents then case", {
  from <- getFromNamespace(".qes_fold_from", "qesR")
  to <- getFromNamespace(".qes_fold_to", "qesR")
  expect_true(is.numeric(from))
  expect_identical(length(from), length(to))
  expect_true(all(Encoding(to) == "unknown"))
  expect_true(all(!grepl("[^ -~]", to)))
  s <- c(
    paste0("Souverainet", intToUtf8(0xE9)),
    paste0(intToUtf8(0xC9), "TUDE ", intToUtf8(0x152), "UVRE"),
    paste0("l", intToUtf8(0x2019), "ind", intToUtf8(0xE9), "pendance"),
    NA
  )
  expect_identical(fold(s), c("souverainete", "etude oeuvre", "l'independance", NA))
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    expect_identical(fold(s), c("souverainete", "etude oeuvre", "l'independance", NA))
  })
})

test_that("qes_search() is offline, case- and accent-insensitive, in both languages", {
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  hits <- qes_search("souverain|sovereign")
  expect_s3_class(hits, "qes_search")
  expect_identical(
    names(hits),
    c("study", "year", "variable", "label", "question", "question_lang", "values", "targets", "matched_in")
  )
  expect_gt(nrow(hits), 10L)
  expect_true(all(is.na(hits$targets)))
  # accents and case do not matter
  a <- qes_search(paste0("IND", intToUtf8(0xC9), "PENDANT"), studies = "qes2014")
  b <- qes_search("independant", studies = "qes2014")
  expect_identical(a, b)
  expect_true("Q19" %in% a$variable)
  # both languages by default; "en" finds the English question only
  en <- qes_search("independent country", studies = "qes2014", fields = "question", lang = "en")
  expect_true("Q19" %in% en$variable)
  expect_identical(unique(en$question_lang), "en")
  fr_only <- qes_search("independent country", studies = "qes2014", fields = "question", lang = "fr")
  expect_identical(nrow(fr_only), 0L)
})

test_that("fields, regex and studies restrict the search", {
  v <- qes_search("^q2[67]$", studies = "qes2018", fields = "variable", regex = TRUE)
  expect_identical(v$variable, c("q26", "q27"))
  expect_true(all(v$matched_in == "variable"))
  vals <- qes_search("Ailleurs au Canada", studies = "qes2018", fields = "values")
  expect_true("q69" %in% vals$variable)
  expect_match(vals$values[vals$variable == "q69"], "2=Ailleurs au Canada", fixed = TRUE)
  expect_error(qes_search("x", fields = "nope"), class = "qesR_error_input")
  expect_error(qes_search("(", regex = TRUE), class = "qesR_error_input")
  expect_error(qes_search(""), class = "qesR_error_input")
  expect_error(qes_search("x", studies = "2018"), class = "qesR_error_unknown_study")
})

test_that("studies without metadata at hand are named, and coverage is reported", {
  local_clear_dict_memo()
  withr::local_options(qesR.cache = "none")
  hits <- qes_search("vote")
  expect_identical(attr(hits, "not_searchable"), "qes2022")
  cov <- attr(hits, "coverage")
  expect_true(all(c("study", "n_variables", "n_label", "n_question", "n_reviewed") %in% names(cov)))
  expect_identical(cov$n_variables[cov$study == "qes2014"], 140L)
  out <- utils::capture.output(print(hits, n = 2))
  expect_true(any(grepl("qes2022", out, fixed = TRUE)))
  none <- qes_search("zzzqqqxxx")
  expect_identical(nrow(none), 0L)
  expect_true(length(utils::capture.output(print(none))) >= 1L)
})

test_that("search results do not depend on the locale or the message language", {
  pattern <- paste0("int", intToUtf8(0xE9), "r", intToUtf8(0xEA), "t")
  default <- qes_search(pattern, studies = c("qes2014", "qes2018"))
  expect_gt(nrow(default), 0L)
  withr::local_envvar(LANGUAGE = "fr")
  withr::local_locale(c(LC_COLLATE = "C", LC_CTYPE = "C"))
  local_fresh_dict()
  expect_identical(qes_search(pattern, studies = c("qes2014", "qes2018")), default)
})
