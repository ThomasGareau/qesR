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
  local_mocked_bindings(
    get_qes = function(srvy, assign_global, with_codebook, quiet) {
      if (identical(srvy, "qes2022")) {
        return(data.frame(ResponseId = c("a1", "a2"), cps_age_in_years = c(20, 21), stringsAsFactors = FALSE))
      }
      if (identical(srvy, "qes2018")) {
        return(data.frame(responseid = c("b1"), agecalc = 40, stringsAsFactors = FALSE))
      }
      stop("mocked failure")
    },
    .env = asNamespace("qesR")
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
  expect_error(
    get_qes_master(surveys = c("qes2022", "not_real"), assign_global = FALSE, quiet = TRUE),
    "Unknown survey code"
  )
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
  expect_true(identical(out$vote_choice, c(3, 4)))
  expect_true(identical(out$turnout, c(1, 1)))
  expect_true(identical(out$age, c(NA_real_, NA_real_)))
  expect_true(identical(out$age_group, c("DE 25 A 34 ANS", "65 ANS ET PLUS")))
})

test_that("get_qes_master derives age_group from age and year_of_birth", {
  local_mocked_bindings(
    get_qes = function(srvy, assign_global, with_codebook, quiet) {
      if (identical(srvy, "qes2022")) {
        return(data.frame(ResponseId = c("x1", "x2"), cps_age_in_years = c(23, 67), stringsAsFactors = FALSE))
      }
      if (identical(srvy, "qes2012")) {
        return(data.frame(quest = "x3", agex = 1980, scol = 4, stringsAsFactors = FALSE))
      }
      stop("mocked failure")
    },
    .env = asNamespace("qesR")
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

test_that("get_qes_master does not deduplicate respondents across different studies", {
  local_mocked_bindings(
    get_qes = function(srvy, assign_global, with_codebook, quiet) {
      if (identical(srvy, "qes2007")) {
        return(data.frame(quest = c("11", "12"), age = c(30, 40), stringsAsFactors = FALSE))
      }
      if (identical(srvy, "qes2007_panel")) {
        return(data.frame(quest = c("12", "13"), age = c(41, 50), stringsAsFactors = FALSE))
      }
      stop("mocked failure")
    },
    .env = asNamespace("qesR")
  )

  master <- get_qes_master(
    surveys = c("qes2007", "qes2007_panel"),
    assign_global = FALSE,
    quiet = TRUE
  )

  expect_true(identical(nrow(master), 4L))
  expect_true(identical(attr(master, "duplicates_removed", exact = TRUE), 0L))
  expect_true(identical(sum(master$respondent_id == "12"), 2L))
})

test_that("get_qes_master removes duplicate respondents within the same study", {
  local_mocked_bindings(
    get_qes = function(srvy, assign_global, with_codebook, quiet) {
      if (identical(srvy, "qes2007")) {
        return(data.frame(quest = c("11", "11", "12"), age = c(30, 31, 40), stringsAsFactors = FALSE))
      }
      stop("mocked failure")
    },
    .env = asNamespace("qesR")
  )

  master <- get_qes_master(
    surveys = "qes2007",
    assign_global = FALSE,
    quiet = TRUE
  )

  expect_true(identical(nrow(master), 2L))
  expect_true(identical(attr(master, "duplicates_removed", exact = TRUE), 1L))
  expect_true(identical(sum(master$respondent_id == "11"), 1L))
  expect_true(identical(master$age[master$respondent_id == "11"], 30))
})

test_that("get_qes_master drops rows empty across harmonized variables", {
  local_mocked_bindings(
    get_qes = function(srvy, assign_global, with_codebook, quiet) {
      data.frame(unused_col = c(1, 2), stringsAsFactors = FALSE)
    },
    .env = asNamespace("qesR")
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
  local_mocked_bindings(
    get_qes = function(srvy, assign_global, with_codebook, quiet) {
      data.frame(ResponseId = "x1", cps_age_in_years = 33, stringsAsFactors = FALSE)
    },
    .env = asNamespace("qesR")
  )

  csv_path <- tempfile(fileext = ".csv")
  master_csv <- get_qes_master(
    surveys = "qes2022",
    assign_global = FALSE,
    quiet = TRUE,
    save_path = csv_path
  )
  expect_true(file.exists(csv_path))
  expect_true(identical(attr(master_csv, "saved_to", exact = TRUE), csv_path))

  rds_path <- tempfile(fileext = ".rds")
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
