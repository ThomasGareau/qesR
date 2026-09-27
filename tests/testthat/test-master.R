# Restored from 397aa0c^ in slice S0b. Triage:
# * Tests that mocked get_qes() now serve their data through the offline fake
#   Dataverse (helper-fake-dataverse.R), so they run the real read path and
#   survive slice S0c, where get_qes_master() stops calling the exported
#   get_qes().
# * "drops rows empty" and "saves variable name sidecar" pin v0.4.4 behaviour
#   that slice S4 removes on purpose (no row is ever dropped, P5; the sidecar
#   becomes <stem>_provenance.csv). S4 replaces them with the contract tests
#   in test-contract-legacy.R.

test_that(".build_qes_master_study harmonizes mapped columns", {
  dat <- data.frame(
    ResponseId = c("r1", "r2"),
    cps_age_in_years = c(30, 45),
    cps_genderid = c(1, 2),
    cps_votechoice1 = c(3, 4),
    qscol = c(5, 6),
    stringsAsFactors = FALSE
  )

  out <- qesR:::.build_qes_master_study(
    data = dat,
    srvy = "qes2022",
    year = "2022",
    name_en = "Quebec Election Study 2022"
  )

  expect_true(is.list(out))
  expect_true(all(c("data", "source_map") %in% names(out)))
  expect_true(identical(nrow(out$data), 2L))
  expect_true(all(c("qes_code", "qes_year", "respondent_id", "age", "gender", "vote_choice", "education") %in% names(out$data)))
  expect_true(all(out$data$qes_code == "qes2022"))
  expect_true(all(out$data$qes_year == "2022"))
  expect_true(identical(out$data$respondent_id, c("r1", "r2")))
  expect_true(identical(out$data$age, c(30, 45)))
  expect_true(identical(out$data$education, c(5, 6)))

  src_age <- out$source_map$source_variable[out$source_map$harmonized_variable == "age"]
  expect_true(identical(src_age, "cps_age_in_years"))
  src_edu <- out$source_map$source_variable[out$source_map$harmonized_variable == "education"]
  expect_true(identical(src_edu, "qscol"))
})

test_that("get_qes_master stacks studies and records failures", {
  local_fake_dataverse(
    data = list(
      qes2022 = data.frame(ResponseId = c("a1", "a2"), cps_age_in_years = c(20, 21), stringsAsFactors = FALSE),
      qes2018 = data.frame(responseid = c("b1"), agecalc = 40, stringsAsFactors = FALSE)
    ),
    fail = "qes2008"
  )

  master <- get_qes_master(
    surveys = c("qes2022", "qes2018", "qes2008"),
    assign_global = FALSE,
    quiet = TRUE,
    strict = FALSE
  )

  expect_s3_class(master, "data.frame")
  expect_true(identical(nrow(master), 3L))
  expect_true(all(c("qes_code", "respondent_id", "age") %in% names(master)))
  expect_true(identical(sort(unique(master$qes_code)), c("qes2018", "qes2022")))
  expect_true(length(attr(master, "failed_surveys", exact = TRUE)) == 1L)
  expect_true(grepl("^qes2008:", attr(master, "failed_surveys", exact = TRUE)[1]))
  expect_true(all(c("qes2018", "qes2022") %in% attr(master, "loaded_surveys", exact = TRUE)))
})

test_that("get_qes_master validates survey codes", {
  log <- local_fake_dataverse()
  err <- expect_error(
    get_qes_master(surveys = c("qes2022", "not_real"), assign_global = FALSE, quiet = TRUE),
    class = "qesR_error_unknown_study"
  )
  expect_identical(err$study, "not_real")
  expect_length(log$urls, 0L)
})

test_that(".build_qes_master_study supports qes1998 naming variants", {
  dat <- data.frame(
    quetr = c(101, 102),
    sexe_post = c(1, 2),
    poids = c(1.2, 0.8),
    intvote = c(3, 4),
    allervot = c(1, 1),
    age = haven::labelled(
      c(2L, 6L),
      labels = c("DE 25 A 34 ANS" = 2L, "65 ANS ET PLUS" = 6L)
    ),
    stringsAsFactors = FALSE
  )

  out <- qesR:::.build_qes_master_study(
    data = dat,
    srvy = "qes1998",
    year = "1998",
    name_en = "Quebec Elections 1998"
  )$data

  out <- qesR:::.postprocess_master_dataset(out)
  expect_true(identical(out$respondent_id, c(101, 102)))
  expect_true(identical(out$gender, c(1, 2)))
  expect_true(identical(out$survey_weight, c(1.2, 0.8)))
  expect_true(all(out$vote_choice == c(3, 4)))
  expect_true(identical(out$turnout, c(1, 1)))
  expect_true(identical(out$age, c(NA_real_, NA_real_)))
  expect_true(identical(out$age_group, c("25-34", "65+")))
})

test_that("get_qes_master derives age_group from age and year_of_birth", {
  local_fake_dataverse(
    data = list(
      qes2022 = data.frame(ResponseId = c("x1", "x2"), cps_age_in_years = c(23, 67), stringsAsFactors = FALSE),
      qes2012 = data.frame(quest = "x3", agex = 1980, scol = 4, stringsAsFactors = FALSE)
    )
  )

  master <- get_qes_master(
    surveys = c("qes2022", "qes2012"),
    assign_global = FALSE,
    quiet = TRUE
  )

  expect_true(all(c("age", "age_group", "education") %in% names(master)))

  row_x1 <- master[master$respondent_id == "x1", , drop = FALSE]
  row_x2 <- master[master$respondent_id == "x2", , drop = FALSE]
  row_x3 <- master[master$respondent_id == "x3", , drop = FALSE]

  expect_true(identical(row_x1$age_group, "18-24"))
  expect_true(identical(row_x2$age_group, "65+"))
  expect_true(identical(row_x3$age_group, "25-34"))
  expect_true(identical(row_x3$education, 4))
})

test_that("get_qes_master drops rows empty across harmonized variables (v0.4.4; removed in S4)", {
  local_fake_dataverse(
    data = list(qes2022 = data.frame(unused_col = c(1, 2), stringsAsFactors = FALSE))
  )

  master <- get_qes_master(
    surveys = "qes2022",
    assign_global = FALSE,
    quiet = TRUE
  )

  expect_true(identical(nrow(master), 0L))
  expect_true(identical(attr(master, "empty_rows_removed", exact = TRUE), 2L))
})

test_that("get_qes_master can save master file to disk", {
  local_fake_dataverse(
    data = list(qes2022 = data.frame(ResponseId = "x1", cps_age_in_years = 33, stringsAsFactors = FALSE))
  )

  csv_path <- withr::local_tempfile(fileext = ".csv")
  master_csv <- get_qes_master(
    surveys = "qes2022",
    assign_global = FALSE,
    quiet = TRUE,
    save_path = csv_path
  )
  expect_true(file.exists(csv_path))
  expect_true(identical(attr(master_csv, "saved_to", exact = TRUE), csv_path))

  rds_path <- withr::local_tempfile(fileext = ".rds")
  master_rds <- get_qes_master(
    surveys = "qes2022",
    assign_global = FALSE,
    quiet = TRUE,
    save_path = rds_path
  )
  expect_true(file.exists(rds_path))
  roundtrip <- readRDS(rds_path)
  expect_true(identical(nrow(roundtrip), nrow(master_rds)))
})

test_that("get_qes_master renames opaque legacy variables and records mapping", {
  q2022 <- data.frame(ResponseId = "x1", q10 = 1, q16 = 2, stringsAsFactors = FALSE)
  attr(q2022$q10, "label") <- "Important issue: cost of living"
  attr(q2022$q16, "label") <- "Main source of political information"
  q2018 <- data.frame(responseid = "x2", q10 = 3, q16 = 4, stringsAsFactors = FALSE)
  attr(q2018$q10, "label") <- "Important issue: cost of living"
  attr(q2018$q16, "label") <- "Main source of political information"
  local_fake_dataverse(data = list(qes2022 = q2022, qes2018 = q2018))

  master <- get_qes_master(
    surveys = c("qes2022", "qes2018"),
    assign_global = FALSE,
    quiet = TRUE
  )

  var_map <- attr(master, "variable_name_map", exact = TRUE)
  expect_true(is.data.frame(var_map))
  expect_true(all(c("legacy_variable", "master_variable", "label_hint") %in% names(var_map)))
  expect_true(all(c("q10", "q16") %in% var_map$legacy_variable))
  expect_false(any(c("q10", "q16") %in% names(master)))
  expect_true(all(var_map$master_variable %in% names(master)))
})

test_that("get_qes_master saves variable name sidecar map when renaming occurs (v0.4.4; replaced in S4)", {
  q2022 <- data.frame(ResponseId = "x1", q10 = 1, stringsAsFactors = FALSE)
  attr(q2022$q10, "label") <- "Important issue: cost of living"
  q2018 <- data.frame(responseid = "x2", q10 = 2, stringsAsFactors = FALSE)
  attr(q2018$q10, "label") <- "Important issue: cost of living"
  local_fake_dataverse(data = list(qes2022 = q2022, qes2018 = q2018))

  out <- withr::local_tempfile(fileext = ".csv")
  master <- get_qes_master(
    surveys = c("qes2022", "qes2018"),
    assign_global = FALSE,
    quiet = TRUE,
    save_path = out
  )

  map_path <- attr(master, "variable_name_map_path", exact = TRUE)
  expect_true(is.character(map_path) && length(map_path) == 1L && nzchar(map_path))
  expect_true(file.exists(map_path))
  unlink(map_path)
})

# Slice S2b: the master reads the original files; until slice S4 freezes its
# sources it gets the labels of 0.4.4 back where the files have none.

test_that("qes2018 education maps from the 0.4.4 labels of qscol, not raw codes", {
  local_fake_dataverse(data = list(
    qes2018 = data.frame(responseid = c("b1", "b2", "b3", "b4", "b5", "b6"), qscol = c(3, 5, 8, 11, 13, 99))
  ))
  master <- get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE)
  expect_identical(master$education, c(
    "Primary or less", "Secondary", "Secondary",
    "College/CEGEP/Technical", "University", NA
  ))
})

test_that("qes1998 education and age group map from the panel file's labels", {
  local_fake_dataverse(data = list(qes1998 = data.frame(
    quetr = 1:5,
    scol = haven::labelled(c(1, 2, 3, 9, NA), labels = c("1-9 ans" = 1, "10-15 ans" = 2, "univ. +" = 3)),
    age = haven::labelled(c(1, 5, 6, 9, 2), labels = c("18-24" = 1, "25-34" = 2, "55-64" = 5))
  )))
  master <- get_qes_master(surveys = "qes1998", assign_global = FALSE, quiet = TRUE)
  expect_identical(master$education, c("Primary or less", "College/CEGEP/Technical", "University", NA, NA))
  expect_identical(master$age_group, c("18-24", "55-64", "65+", NA, "25-34"))
})

test_that("legacy labels: trimmed, blanks dropped, issue text kept out of vote_choice_text", {
  dat <- data.frame(
    q1 = haven::labelled(c(1, 96), labels = c(" Refus" = 1, " " = 96)),
    q2_96_other = haven::labelled(c(1, 2), labels = c("Cannabis" = 1, "Culture" = 2), label = "Other issue")
  )
  out <- qesR:::.qes_legacy_source_labels(dat, "qes2018", consumer = "master")
  expect_identical(attr(out$q1, "labels"), c(Refus = 1))
  expect_identical(as.character(haven::as_factor(out$q1)), c("Refus", "96"))
  expect_false(inherits(out$q2_96_other, "haven_labelled"))
  expect_identical(attr(out$q2_96_other, "label"), "Other issue")
  # a code the file labels keeps the file's label
  dat <- data.frame(qscol = haven::labelled(c(8, 9), labels = c("DES (fichier)" = 8)))
  out <- qesR:::.qes_legacy_source_labels(dat, "qes2018", consumer = "master")
  expect_identical(as.character(haven::as_factor(out$qscol)), c("DES (fichier)", "Secondaire 5 (DEP)"))
})
