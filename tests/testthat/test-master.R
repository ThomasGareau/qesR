# The interim legacy master (design.md section 5.12, slice S4): the qesR 0.4.4
# conversions on the frozen 0.4.4 sources, with deletions only. Data comes
# from the offline fake Dataverse (helper-fake-dataverse.R); every value is
# synthetic. The frozen tables themselves are checked in test-legacy.R.

test_that(".build_qes_master_study reads only the frozen 0.4.4 sources", {
  dat <- data.frame(
    ResponseId = c("r1", "r2"),
    cps_age_in_years = c(30, 45),
    cps_genderid = c(1, 2),
    cps_votechoice1 = c(3, 4),
    cps_edu = c(7, 9),
    # names 0.4.4 could have matched at run time, never read now
    qscol = c(5, 6),
    lr = c(1, 2),
    stringsAsFactors = FALSE
  )

  out <- qesR:::.build_qes_master_study(
    data = dat,
    srvy = "qes2022",
    year = "2022",
    name_en = "Quebec Election Study 2022"
  )

  expect_true(all(c("data", "source_map", "masks") %in% names(out)))
  expect_identical(nrow(out$data), 2L)
  expect_true(all(out$data$qes_code == "qes2022"))
  expect_identical(out$data$respondent_id, c("r1", "r2"))
  expect_identical(out$data$age, c(30, 45))
  expect_identical(out$data$education, c(7, 9))
  expect_true(all(is.na(out$data$ideology)))

  src <- out$source_map
  expect_identical(src$source_variable[src$harmonized_variable == "age"], "cps_age_in_years")
  expect_identical(src$source_variable[src$harmonized_variable == "education"], "cps_edu")
  expect_false(any(c("qscol", "lr") %in% src$source_variable))
  # the vote intention of qes2022 is blanked (OD4)
  expect_true(all(out$masks$vote_choice))
})

test_that("get_qes_master stacks studies and records failures", {
  local_qes_notices_shown()
  local_fake_dataverse(
    data = list(
      qes2022 = data.frame(ResponseId = c("a1", "a2"), cps_age_in_years = c(20, 21), stringsAsFactors = FALSE),
      qes2018 = data.frame(responseid = c("b1"), age = 40, stringsAsFactors = FALSE)
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
  expect_identical(nrow(master), 3L)
  expect_identical(sort(unique(master$qes_code)), c("qes2018", "qes2022"))
  expect_identical(master$age, c(20, 21, 40))
  expect_length(attr(master, "failed_surveys", exact = TRUE), 1L)
  expect_match(attr(master, "failed_surveys", exact = TRUE)[1], "^qes2008:")
  expect_setequal(attr(master, "loaded_surveys", exact = TRUE), c("qes2018", "qes2022"))
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

test_that("qes1998 reads its frozen sources, and its vote intention is blanked", {
  local_qes_notices_shown()
  local_fake_dataverse(data = list(qes1998 = data.frame(
    quetr = c(101, 102),
    sexe_post = c(1, 2),
    ponderc = c(1.2, 0.8),
    # poids is not the 0.4.4 source of the weight
    poids = c(9, 9),
    intvote = c(3, 4),
    q1post = c(1, 2),
    age = haven::labelled(c(2, 6), labels = c("DE 25 A 34 ANS" = 2, "65 ANS ET PLUS" = 6)),
    stringsAsFactors = FALSE
  )))

  out <- get_qes_master(surveys = "qes1998", quiet = TRUE)
  expect_identical(out$respondent_id, c("101", "102"))
  expect_identical(out$gender, c("Man", "Woman"))
  expect_identical(out$survey_weight, c(1.2, 0.8))
  expect_identical(out$turnout, c(1, 0))
  expect_identical(out$vote_choice, c(NA_character_, NA_character_))
  expect_identical(out$age, c(NA_real_, NA_real_))
  expect_identical(out$age_group, c("25-34", "65+"))
  expect_identical(out$language, c("French", "French"))
})

test_that("get_qes_master derives age_group from age and year_of_birth", {
  local_qes_notices_shown()
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

  row_x1 <- master[master$respondent_id == "x1", , drop = FALSE]
  row_x2 <- master[master$respondent_id == "x2", , drop = FALSE]
  row_x3 <- master[master$respondent_id == "x3", , drop = FALSE]

  expect_identical(row_x1$age_group, "18-24")
  expect_identical(row_x2$age_group, "65+")
  expect_identical(row_x3$age_group, "25-34")
  # an unlabelled code is text in the master, as in the full 0.4.4 build
  expect_identical(row_x3$education, "Secondary")
})

test_that("get_qes_master keeps rows that are empty across the harmonized columns", {
  local_qes_notices_shown()
  local_fake_dataverse(
    data = list(qes2022 = data.frame(unused_col = c(1, 2), stringsAsFactors = FALSE))
  )

  master <- get_qes_master(surveys = "qes2022", assign_global = FALSE, quiet = TRUE)

  expect_identical(nrow(master), 2L)
  expect_identical(attr(master, "empty_rows_removed", exact = TRUE), 0L)
  expect_identical(attr(master, "duplicates_removed", exact = TRUE), 0L)
})

test_that("get_qes_master saves UTF-8 CSV or RDS, with the provenance next to it", {
  local_qes_notices_shown()
  local_fake_dataverse(
    data = list(qes2022 = data.frame(
      ResponseId = "x1", cps_age_in_years = 33,
      cps_votechoice1_8_TEXT = "Bloc Montréal",
      cps_province = haven::labelled(11, labels = stats::setNames(11, "Québec")),
      stringsAsFactors = FALSE
    ))
  )

  dir <- withr::local_tempdir()
  csv_path <- file.path(dir, "master.csv")
  master_csv <- get_qes_master(surveys = "qes2022", quiet = TRUE, save_path = csv_path)
  expect_true(file.exists(csv_path))
  expect_identical(attr(master_csv, "saved_to", exact = TRUE), csv_path)
  expect_null(attr(master_csv, "variable_name_map_path", exact = TRUE))
  back <- utils::read.csv(csv_path, colClasses = "character", encoding = "UTF-8")
  expect_identical(names(back), names(master_csv))
  expect_true(all(validUTF8(readLines(csv_path, encoding = "UTF-8"))))
  prov_path <- file.path(dir, "master_provenance.csv")
  expect_true(file.exists(prov_path))
  prov <- utils::read.csv(prov_path, colClasses = "character", encoding = "UTF-8")
  expect_identical(prov$study, "qes2022")
  expect_false(file.exists(file.path(dir, "master_variable_name_map.csv")))

  rds_path <- file.path(dir, "master.rds")
  master_rds <- get_qes_master(surveys = "qes2022", quiet = TRUE, save_path = rds_path)
  expect_true(file.exists(rds_path))
  expect_true(file.exists(file.path(dir, "master_provenance.csv")))
  expect_identical(nrow(readRDS(rds_path)), nrow(master_rds))

  # a dot in a directory name is not an extension
  dotted <- file.path(dir, "out.d")
  dir.create(dotted)
  get_qes_master(surveys = "qes2022", quiet = TRUE, save_path = file.path(dotted, "master"))
  expect_true(file.exists(file.path(dotted, "master")))
  expect_true(file.exists(file.path(dotted, "master_provenance.csv")))
  expect_false(file.exists(file.path(dir, "out_provenance.csv")))
})

test_that("get_qes_master no longer stacks raw variables shared across studies", {
  local_qes_notices_shown()
  q2022 <- data.frame(ResponseId = "x1", q10 = 1, q16 = 2, stringsAsFactors = FALSE)
  q2018 <- data.frame(responseid = "x2", q10 = 3, q16 = 4, stringsAsFactors = FALSE)
  local_fake_dataverse(data = list(qes2022 = q2022, qes2018 = q2018))

  master <- get_qes_master(surveys = c("qes2022", "qes2018"), quiet = TRUE)

  expect_identical(names(master), c(names(qesR:::.qes_master_types), "vote_choice_timing", "sovereignty_item"))
  expect_false(any(c("q10", "q16", "issue_family_support", "info_source_primary") %in% names(master)))
  expect_identical(attr(master, "crossstudy_variables_added"), character(0))
  expect_identical(nrow(attr(master, "variable_name_map")), 0L)
  expect_identical(names(attr(master, "variable_name_map")), c("legacy_variable", "master_variable", "label_hint"))
  removed <- attr(master, "removed_columns")
  expect_length(removed, 70L)
  expect_true(all(c("issue_family_support", "vote_federal_2006", "pondam1") %in% removed))
})

test_that("a study's rows do not depend on the studies loaded with it", {
  local_qes_notices_shown()
  # qes2018's gender and region codes are unlabelled in its file; the full
  # 0.4.4 build read them as text ("1" is a man)
  local_fake_dataverse(data = list(
    qes2018 = data.frame(responseid = c("b1", "b2"), qsexe = c(1, 2), q0qc = c(6, 3), q5 = c(4, 1)),
    qes2012 = data.frame(quest = "c1", sexe = haven::labelled(2, labels = c(Male = 1, Female = 2)))
  ))
  alone <- get_qes_master(surveys = "qes2018", quiet = TRUE)
  both <- get_qes_master(surveys = c("qes2012", "qes2018"), quiet = TRUE)
  expect_identical(alone$gender, c("Man", "Woman"))
  expect_identical(alone$province_territory, c("Quebec", "Quebec"))
  expect_identical(alone$turnout, c(1, 0))
  rows <- both[both$qes_code == "qes2018", , drop = FALSE]
  rownames(rows) <- NULL
  strip <- function(x) {
    attributes(x) <- attributes(x)[c("names", "row.names", "class")]
    x
  }
  expect_identical(strip(rows), strip(alone))
})

# The labels of 0.4.4 are put back where the files have none (R/master.R).

test_that("qes2018 education maps from the 0.4.4 labels of qscol, not raw codes", {
  local_qes_notices_shown()
  local_fake_dataverse(data = list(
    qes2018 = data.frame(responseid = c("b1", "b2", "b3", "b4", "b5", "b6"), qscol = c(3, 5, 8, 11, 13, 99))
  ))
  master <- get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE)
  expect_identical(master$education, c(
    "Primary or less", "Secondary", "Secondary",
    "College/CEGEP/Technical", "University", NA
  ))
})

test_that("qes1998 age group maps from the panel file's labels; its education is blanked", {
  local_qes_notices_shown()
  local_fake_dataverse(data = list(qes1998 = data.frame(
    quetr = 1:5,
    scol = haven::labelled(c(1, 2, 3, 9, NA), labels = c("1-9 ans" = 1, "10-15 ans" = 2, "univ. +" = 3)),
    age = haven::labelled(c(1, 5, 6, 9, 2), labels = c("18-24" = 1, "25-34" = 2, "55-64" = 5))
  )))
  master <- get_qes_master(surveys = "qes1998", assign_global = FALSE, quiet = TRUE)
  expect_identical(master$age_group, c("18-24", "55-64", "65+", NA, "25-34"))
  # scol 2 (10-15 years) mixes secondary and college (design.md 5.7)
  expect_identical(master$education, rep(NA_character_, 5))
  na <- attr(master, "legacy_na_columns")
  expect_identical(na$reason[na$column == "education"], "blanked")
  # the values set to NA: scol 9 and the missing answer were NA already
  expect_identical(na$n_cells[na$column == "education"], 3L)
})

test_that("legacy labels: trimmed and blank labels dropped", {
  dat <- data.frame(q1 = haven::labelled(c(1, 96), labels = c(" Refus" = 1, " " = 96)))
  out <- qesR:::.qes_legacy_source_labels(dat, "qes2018", consumer = "master")
  expect_identical(attr(out$q1, "labels"), c(Refus = 1))
  expect_identical(as.character(haven::as_factor(out$q1)), c("Refus", "96"))
  # a code the file labels keeps the file's label
  dat <- data.frame(qscol = haven::labelled(c(8, 9), labels = c("DES (fichier)" = 8)))
  out <- qesR:::.qes_legacy_source_labels(dat, "qes2018", consumer = "master")
  expect_identical(as.character(haven::as_factor(out$qscol)), c("DES (fichier)", "Secondaire 5 (DEP)"))
})
