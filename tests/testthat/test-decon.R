# get_decon() rendered from the harmonization engine (design.md sections 2.3
# and 5.12, slice HZ6): the profile "decon" of the spec's legacy.csv.

test_that("get_decon() runs on the demo offline and refuses the studies added since 0.4.4", {
  local_qes_notices_shown()
  load <- getFromNamespace(".qes_load_catalog", "qesR")
  main <- load(catalog_dir())
  with_demo <- load(catalog_dir(), demo_dir = system.file("extdata", "demo", "catalog", package = "qesR"))
  testthat::local_mocked_bindings(
    .qes_catalog = function(demo = FALSE) if (isTRUE(demo)) with_demo else main,
    .package = "qesR"
  )
  decon <- get_decon("qes_demo", quiet = TRUE)
  expect_identical(names(decon), v044_decon_cols)
  expect_identical(nrow(decon), 60L)
  expect_true(all(decon$qes_code == "qes_demo"))
  # categorical columns are factors with the target's English levels
  expect_s3_class(decon$gender, "factor")
  expect_identical(levels(decon$gender), c("Man", "Woman", "Non-binary", "Another gender"))
  expect_identical(levels(decon$turnout), c("Yes", "No"))
  expect_true(any(!is.na(decon$votechoice)))
  expect_identical(attr(decon, "timing"), c(turnout = "post", votechoice = "post", votechoice_text = NA_character_))
  sm <- attr(decon, "source_map")
  expect_identical(names(sm), c("qes_code", "column", "source_variable", "target", "grade"))
  expect_identical(sm$target[sm$column == "votechoice"], "vote_prov_recall")
  expect_identical(attr(decon, "qes_provenance")$study, "qes_demo")
  # party_best and partylean have no valid source anywhere
  na <- attr(decon, "legacy_na_columns")
  expect_identical(na$reason[na$column == "party_best"], "na_column")
  expect_error(get_decon("qes1998_crop", quiet = TRUE), class = "qesR_error_input")
  expect_error(get_decon("all", quiet = TRUE), class = "qesR_error_input")
})

test_that("qes2022 keeps the campaign-period turnout and vote (OD9), with its types", {
  local_qes_notices_shown()
  local_fake_legacy()
  d <- get_decon("qes2022", quiet = TRUE)
  expect_identical(attr(d, "timing"), c(turnout = "pre", votechoice = "pre", votechoice_text = "pre"))
  sm <- attr(d, "source_map")
  expect_identical(sm$target[sm$column == "turnout"], "turnout_prov_likely")
  expect_identical(sm$target[sm$column == "votechoice"], "vote_prov_intent")
  expect_s3_class(d$turnout, "ordered")
  expect_s3_class(d$votechoice, "factor")
  # numbers stay numbers (the qes2022 income is an amount); text stays text
  for (col in c("yob", "age", "political_interest", "ideology", "income")) {
    expect_true(is.numeric(d[[col]]), info = col)
  }
  expect_type(d$religion, "character")
  expect_type(d$votechoice_text, "character")
  expect_true(all(d$province_territory == "Quebec"))
})

test_that("the reported vote and turnout fill the other studies; the panels give age bands", {
  local_qes_notices_shown()
  local_fake_legacy()
  d <- get_decon("qes2018", quiet = TRUE)
  expect_identical(attr(d, "timing")[c("turnout", "votechoice")], c(turnout = "post", votechoice = "post"))
  expect_true(any(!is.na(d$turnout)))
  expect_true(is.numeric(d$age))
  expect_type(d$income, "character")
  p <- get_decon("qes2018_panel", quiet = TRUE)
  # the panel asked age bands, not the age: a factor of bands, as in 0.4.4
  expect_s3_class(p$age, "factor")
  expect_true(all(levels(p$age) %in% c("18-34", "35-54", "55+")))
  expect_identical(attr(p, "source_map")$target[attr(p, "source_map")$column == "age"], "age_group3")
  # a study without a question for a column: NA, with its levels
  expect_true(all(is.na(p$fed_pid)))
  expect_true(length(levels(p$fed_pid)) > 0L)
})

test_that("the replacement that the get_decon() notice names returns values for the demo", {
  reg <- getFromNamespace(".qes_deprecated", "qesR")
  call_text <- reg$replacement[reg$name == "get_decon"]
  expect_match(call_text, "include_draft = TRUE", fixed = TRUE)
  # the documented call, with the demo as `srvy`
  expr <- str2lang(sub("qes_harmonize(", "qes_harmonize(srvy, ", call_text, fixed = TRUE))
  expr$quiet <- TRUE
  h <- eval(expr, list(srvy = "qes_demo", qes_harmonize = qesR::qes_harmonize))
  expect_identical(nrow(h), 60L)
  for (col in c("gender", "vote_prov_recall", "turnout_prov_recall", "lr_self", "birth_year")) {
    expect_true(any(!is.na(h[[col]])), info = col)
  }
})

test_that("get_decon() raises the error of its one study, without a 'skipping' message", {
  local_qes_notices_shown()
  local_fake_legacy(fail = "qes2018")
  ids <- character(0)
  err <- withCallingHandlers(
    tryCatch(get_decon("qes2018", quiet = FALSE), error = function(e) e),
    message = function(m) {
      ids <<- c(ids, m$id %||% NA_character_)
      invokeRestart("muffleMessage")
    }
  )
  expect_s3_class(err, "error")
  expect_false("master_skip" %in% ids)
  # the master, which builds several studies, still skips with the message
  ids <- character(0)
  withCallingHandlers(
    get_qes_master(surveys = c("qes2018", "qes2014"), quiet = FALSE),
    message = function(m) {
      ids <<- c(ids, m$id %||% NA_character_)
      invokeRestart("muffleMessage")
    }
  )
  expect_true("master_skip" %in% ids)
})
