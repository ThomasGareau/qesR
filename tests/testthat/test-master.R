# get_qes_master() rendered from the harmonization engine (design.md section
# 5.12, slice HZ6). The demonstration study runs offline against the
# shipped catalog; other studies run on the synthetic data of
# helper-legacy.R.

with_shipped <- function(code) {
  load <- getFromNamespace(".qes_load_catalog", "qesR")
  main <- load(catalog_dir())
  with_demo <- load(catalog_dir(), demo_dir = system.file("extdata", "demo", "catalog", package = "qesR"))
  testthat::with_mocked_bindings(
    code,
    .qes_catalog = function(demo = FALSE) if (isTRUE(demo)) with_demo else main,
    .package = "qesR"
  )
}

test_that("the demonstration master is rendered from the engine, offline", {
  local_qes_notices_shown()
  m <- with_shipped(get_qes_master(surveys = "qes_demo", quiet = TRUE))
  demo <- with_shipped(get_qes("qes_demo", quiet = TRUE))
  expect_identical(nrow(m), nrow(demo))
  expect_identical(names(m)[seq_along(v044_master_cols)], names(v044_master_cols))
  expect_true(all(m$qes_code == "qes_demo"))
  expect_true(all(m$qes_year == "2014"))
  expect_identical(m$respondent_id, paste0("qes_demo_", seq_len(nrow(m))))
  # the qes2014 rows the demo stands in for: reported vote with the 0.4.4
  # party labels, turnout and sovereignty as 1/0, the 0.4.4 weight
  expect_true(all(m$vote_choice %in% c(NA, "PLQ", "PQ", "CAQ", "QS", "PVQ", "ON", "Other party")))
  expect_true(all(m$turnout %in% c(NA, 0, 1)))
  expect_true(all(m$sovereignty_support %in% c(NA, 0, 1)))
  expect_identical(m$sovereignty, m$sovereignty_support)
  expect_equal(m$survey_weight, as.numeric(unclass(demo$POND)), ignore_attr = TRUE)
  expect_true(all(m$political_interest %in% c(NA, 0, 3, 7, 10)))
  expect_identical(m$age, 2014 - m$year_of_birth)
  # the qes2014 gender row is held in review (spec 4.0.0): NA, not_reviewed
  expect_true(all(is.na(m$gender)))
  expect_true(all(m$province_territory == "Quebec"))
  expect_true(all(m$vote_choice_timing == "post"))
  expect_true(all(m$sovereignty_item == "sov_indep"))
  expect_true(all(m$waves == "post"))
  expect_identical(m$source_row, seq_len(nrow(m)))
  # the provenance of the file read, with the cell and spec levels
  prov <- attr(m, "qes_provenance")
  expect_identical(prov$study, "qes_demo")
  expect_s3_class(attr(prov, "cell"), "data.frame")
  expect_identical(attr(m, "qes_spec")$version, hz_spec()$version)
  expect_identical(attr(m, "qes_spec")$hash, hz_spec()$hash)
})

test_that("source_map, legacy_na_columns and legacy_column_map describe every column", {
  local_qes_notices_shown()
  m <- with_shipped(get_qes_master(surveys = "qes_demo", quiet = TRUE))
  sm <- attr(m, "source_map")
  expect_identical(names(sm), c("qes_code", "qes_year", "qes_name_en", "harmonized_variable", "source_variable",
                                "target", "map_id", "grade", "status", "render", "file_md5", "spec_version"))
  expect_identical(sm$harmonized_variable, names(m))
  expect_identical(sm$source_variable[sm$harmonized_variable == "vote_choice"], "Q3")
  expect_identical(sm$target[sm$harmonized_variable == "vote_choice"], "vote_prov_recall")
  expect_identical(sm$grade[sm$harmonized_variable == "vote_choice"], "comparable")
  expect_identical(sm$source_variable[sm$harmonized_variable == "survey_weight"], "POND")
  expect_identical(sm$target[sm$harmonized_variable == "age"], "birth_year")
  na <- attr(m, "legacy_na_columns")
  expect_identical(names(na), c("column", "study", "reason", "n_cells", "cause", "basis"))
  expect_true(all(na$reason %in% c("no_source", "na_column", "all_missing", "not_reviewed")))
  # a column whose row is held in review: not_reviewed, with the row's
  # review_note as the basis, and the row's status in source_map
  expect_identical(na$reason[na$column == "gender"], "not_reviewed")
  expect_identical(na$cause[na$column == "gender"], "not_signed_off")
  expect_match(na$basis[na$column == "gender"], "second reviewer", fixed = TRUE)
  expect_identical(sm$status[sm$harmonized_variable == "gender"], "review")
  expect_identical(sm$status[sm$harmonized_variable == "vote_choice"], "stable")
  # every column that is NA throughout is listed, and only those
  all_na <- names(m)[vapply(m, function(x) all(is.na(x)), logical(1))]
  expect_setequal(na$column, all_na)
  expect_identical(na$reason[na$column == "party_best"], "na_column")
  expect_identical(na$cause[na$column == "party_best"], "no_valid_source")
  expect_identical(na$reason[na$column == "education"], "no_source")
  # a raw variable the file lacks is no_source, not all_missing
  expect_identical(na$reason[na$column == "interview_start"], "no_source")
  # ... and source_map names no source for it
  expect_true(is.na(sm$source_variable[sm$harmonized_variable == "interview_start"]))
  no_src <- na[na$reason == "no_source", , drop = FALSE]
  hit <- sm[match(paste(no_src$column, no_src$study), paste(sm$harmonized_variable, sm$qes_code)), , drop = FALSE]
  expect_true(all(is.na(hit$source_variable)))
  # as is a weight the spec registers for no wave of the study
  expect_identical(na$reason[na$column == "weight_pre"], "no_source")
  map <- attr(m, "legacy_column_map")
  expect_identical(map$column, names(m))
  expect_identical(map$target[map$column == "vote_choice"], "vote_prov_recall")
})

test_that("get_qes_master saves UTF-8 CSV or RDS, with the provenance next to it", {
  local_qes_notices_shown()
  dir <- withr::local_tempdir()
  csv_path <- file.path(dir, "master.csv")
  master_csv <- with_shipped(get_qes_master(surveys = "qes_demo", quiet = TRUE, save_path = csv_path))
  expect_true(file.exists(csv_path))
  expect_identical(attr(master_csv, "saved_to", exact = TRUE), csv_path)
  expect_null(attr(master_csv, "variable_name_map_path", exact = TRUE))
  back <- utils::read.csv(csv_path, colClasses = "character", encoding = "UTF-8")
  expect_identical(names(back), names(master_csv))
  expect_true(all(validUTF8(readLines(csv_path, encoding = "UTF-8"))))
  prov_path <- file.path(dir, "master_provenance.csv")
  expect_true(file.exists(prov_path))
  prov <- utils::read.csv(prov_path, colClasses = "character", encoding = "UTF-8")
  expect_identical(prov$study, "qes_demo")
  expect_false(file.exists(file.path(dir, "master_variable_name_map.csv")))

  rds_path <- file.path(dir, "master.rds")
  master_rds <- with_shipped(get_qes_master(surveys = "qes_demo", quiet = TRUE, save_path = rds_path))
  expect_true(file.exists(rds_path))
  expect_identical(nrow(readRDS(rds_path)), nrow(master_rds))

  # a dot in a directory name is not an extension
  dotted <- file.path(dir, "out.d")
  dir.create(dotted)
  with_shipped(get_qes_master(surveys = "qes_demo", quiet = TRUE, save_path = file.path(dotted, "master")))
  expect_true(file.exists(file.path(dotted, "master")))
  expect_true(file.exists(file.path(dotted, "master_provenance.csv")))
  expect_false(file.exists(file.path(dir, "out_provenance.csv")))
})

test_that("get_qes_master no longer stacks raw variables shared across studies", {
  local_qes_notices_shown()
  local_fake_legacy()
  master <- get_qes_master(surveys = c("qes2022", "qes2018"), quiet = TRUE)
  expect_identical(names(master), names(qesR:::.qes_legacy_columns("master")))
  expect_false(any(c("q10", "q16", "issue_family_support", "info_source_primary") %in% names(master)))
  expect_identical(attr(master, "crossstudy_variables_added"), character(0))
  expect_identical(nrow(attr(master, "variable_name_map")), 0L)
  expect_identical(names(attr(master, "variable_name_map")), c("legacy_variable", "master_variable", "label_hint"))
  removed <- attr(master, "removed_columns")
  expect_length(removed, 70L)
  expect_true(all(c("issue_family_support", "vote_federal_2006", "pondam1") %in% removed))
})

test_that("a study's rows do not depend on the studies loaded with it, and no row is dropped", {
  local_qes_notices_shown()
  local_fake_legacy()
  both <- get_qes_master(surveys = c("qes2008", "qes2012_panel"), quiet = TRUE)
  syn <- legacy_synthetic()
  expect_identical(as.vector(table(both$qes_code)[c("qes2008", "qes2012_panel")]),
                   c(nrow(syn$qes2008), nrow(syn$qes2012_panel)))
  for (s in c("qes2008", "qes2012_panel")) {
    one <- get_qes_master(surveys = s, quiet = TRUE)
    part <- both[both$qes_code == s, , drop = FALSE]
    rownames(part) <- NULL
    expect_equal(as.data.frame(one), as.data.frame(part), ignore_attr = TRUE, info = s)
  }
})

test_that("the columns of rows held in review are NA, with the reason, and announced", {
  local_qes_notices_shown()
  local_fake_legacy(signed = FALSE)
  msgs <- list()
  m <- withCallingHandlers(
    get_qes_master(surveys = c("qes1998", "qes2014")),
    message = function(cnd) {
      msgs[[length(msgs) + 1L]] <<- cnd
      invokeRestart("muffleMessage")
    }
  )
  by <- function(s, col) m[[col]][m$qes_code == s]
  # qes1998: every row is held (its recommended weight needs review)
  expect_true(all(is.na(by("qes1998", "vote_choice"))))
  expect_true(all(is.na(by("qes1998", "gender"))))
  # qes2014: its rows are signed off but gender
  expect_true(any(!is.na(by("qes2014", "vote_choice"))))
  expect_true(all(is.na(by("qes2014", "gender"))))
  na <- attr(m, "legacy_na_columns")
  hit <- na[na$study == "qes1998" & na$column == "vote_choice", , drop = FALSE]
  expect_identical(hit$reason, "not_reviewed")
  expect_identical(hit$cause, "not_signed_off")
  expect_match(hit$basis, "V-S13", fixed = TRUE)
  expect_identical(na$reason[na$study == "qes2014" & na$column == "gender"], "not_reviewed")
  # no later target stands in for one whose row is held
  expect_true(all(is.na(by("qes1998", "vote_choice_timing"))))
  held <- Filter(function(cnd) identical(cnd$study, "qes1998") && !is.null(cnd$columns), msgs)
  expect_length(held, 1L)
  expect_s3_class(held[[1]], "qesR_message_values_changed")
  expect_true("vote_choice" %in% held[[1]]$columns)
})

test_that("the recall targets fill vote_choice and turnout; the 1998 recall is used", {
  local_qes_notices_shown()
  local_fake_legacy()
  m <- get_qes_master(surveys = c("qes1998", "qes2022", "qes_crop_2007_2010"), quiet = TRUE)
  by <- function(s, col) m[[col]][m$qes_code == s]
  # qes1998 and qes2022: the reported vote of their post-election waves
  expect_true(any(!is.na(by("qes1998", "vote_choice"))))
  expect_true(all(by("qes1998", "vote_choice_timing") == "post"))
  expect_true(any(!is.na(by("qes2022", "turnout"))))
  # the CROP polls asked only intentions: vote_choice is NA, vote_intent is not
  expect_true(all(is.na(by("qes_crop_2007_2010", "vote_choice"))))
  expect_true(all(is.na(by("qes_crop_2007_2010", "vote_choice_timing"))))
  expect_true(any(!is.na(by("qes_crop_2007_2010", "vote_intent"))))
  na <- attr(m, "legacy_na_columns")
  expect_identical(na$reason[na$column == "vote_choice" & na$study == "qes_crop_2007_2010"], "no_source")
  # with the rule behind it (OD4; OD5 for the sovereignty columns)
  expect_identical(na$cause[na$column == "vote_choice" & na$study == "qes_crop_2007_2010"], "reported_vote_only")
  expect_identical(na$cause[na$column == "turnout" & na$study == "qes_crop_2007_2010"], "reported_vote_only")
  expect_identical(na$cause[na$column == "sovereignty_support" & na$study == "qes_crop_2007_2010"],
                   "independence_question_only")
  expect_identical(na$cause[na$column == "sovereignty_support" & na$study == "qes1998"], "independence_question_only")
  # 1998's language is its sample restriction, qes2022's is not harmonized yet
  expect_true(all(by("qes1998", "language") == "French"))
  expect_true(all(is.na(by("qes2022", "language"))))
  # the qes2022 weight of 0.4.4 (OD6) and its interview dates as text
  expect_true(all(!is.na(by("qes2022", "survey_weight"))))
  expect_match(by("qes2022", "interview_start")[1], "^2022-09-[0-9]{2} [0-9]{2}:[0-9]{2}:[0-9]{2}$")
})

test_that("a code the spec does not map is NA with a warning, never passed through", {
  local_qes_notices_shown()
  syn <- legacy_synthetic()$qes2018
  x <- unclass(syn$qsexe)
  x[1] <- 7
  syn$qsexe <- x
  local_fake_legacy(data = list(qes2018 = syn))
  expect_warning(m <- get_qes_master(surveys = "qes2018", quiet = TRUE), class = "qesR_warning_unmapped")
  expect_true(is.na(m$gender[1]))
  expect_true(all(m$gender[-1] %in% c("Man", "Woman")))
})

test_that("each study's file is requested once, and the spec provenance covers every study built", {
  local_qes_notices_shown()
  log <- local_fake_legacy()
  studies <- c("qes2008", "qes2012_panel")
  m <- get_qes_master(surveys = studies, quiet = TRUE)
  # cache and memo are off in the fake Dataverse: one request per study
  expect_length(log$urls, length(studies))
  expect_false(anyDuplicated(log$urls) > 0L)
  sp <- qes_provenance(m, level = "spec")
  expect_identical(nrow(sp), length(studies))
  for (s in studies) expect_true(any(grepl(s, sp$args, fixed = TRUE)), info = s)
  d <- get_decon("qes2018", quiet = TRUE)
  expect_length(log$urls, length(studies) + 1L)
})
