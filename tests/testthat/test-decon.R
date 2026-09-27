# Restored from 397aa0c^ in slice S0b. Triage: kept unchanged.

test_that(".build_decon maps and preserves row count", {
  dat <- data.frame(
    cps_citizen = c(1, 2),
    cps_age_in_years = c(30, 45),
    cps_genderid = c(1, 2),
    cps_province = c(11, 11),
    cps_edu = c(3, 4),
    stringsAsFactors = FALSE
  )

  out <- qesR:::.build_decon(dat, srvy = "qes2022")

  expect_s3_class(out, "data.frame")
  expect_true(identical(nrow(out), 2L))
  expect_true(all(c("qes_code", "citizenship", "age", "gender") %in% names(out)))
  expect_true(all(out$qes_code == "qes2022"))
})

# Slice S2b: get_decon() reads the original files, and changes from 0.4.4
# only by deletion or blanking (design.md section 2.3).

test_that("a column whose labels only repeat its codes stays a number", {
  # qes2022 cps_age_in_years: each age 15..115 is labelled with itself
  ages <- c(53, 30, 76)
  dat <- data.frame(
    cps_age_in_years = haven::labelled(ages, labels = stats::setNames(15:115 + 0, as.character(15:115)), label = "Age"),
    cps_genderid = haven::labelled(c(1, 2, 1), labels = c("A man" = 1, "A woman" = 2)),
    cps_interest_1 = c(7, 8, 8),
    stringsAsFactors = FALSE
  )
  out <- qesR:::.build_decon(dat, srvy = "qes2022")
  expect_true(is.numeric(out$age))
  expect_false(is.factor(out$age))
  expect_identical(as.numeric(out$age), ages)
  expect_identical(mean(out$age), mean(ages))
  expect_identical(attr(out$age, "label"), "Age")
  # a column with real labels is still a factor of them
  expect_identical(as.character(out$gender), c("A man", "A woman", "A man"))
  expect_true(is.numeric(out$political_interest))
})

test_that("qes2018 gender and education get the labels of 0.4.4 back", {
  # the .dta has no value labels for qsexe and qscol
  dat <- data.frame(qsexe = c(1, 2), qscol = c(8, 13), age = c(40, 50))
  out <- qesR:::.build_decon(dat, srvy = "qes2018")
  expect_identical(as.character(out$gender), c("Masculin", "Feminin"))
  expect_identical(as.character(out$education), c("Secondaire 5 (DES)", "Universite non completee"))
  expect_s3_class(out$education, "factor")
  expect_true(is.numeric(out$age))
})

test_that("qes1998 age code 6, unlabelled in the panel file, is 65+", {
  dat <- data.frame(age = haven::labelled(c(1, 6, 9), labels = c("18-24" = 1, "55-64" = 5)))
  out <- qesR:::.build_decon(dat, srvy = "qes1998")
  expect_identical(as.character(out$age), c("18-24", "65+", "Refus/pas de reponse"))
})

test_that("get_decon() column classes are those of 0.4.4 (numbers stay numbers)", {
  # v0.4.4 classes of the qes2022 decon (R9 baseline); integer and double
  # are both numbers (get_qes() now stores codes as doubles)
  v044 <- c(
    qes_code = "character", citizenship = "factor", yob = "factor", age = "numeric",
    gender = "factor", education = "factor", political_interest = "numeric",
    ideology = "numeric", income = "numeric", votechoice_text = "character"
  )
  dat <- data.frame(
    cps_citizen = haven::labelled(c(1, 2), labels = c("Canadian citizen" = 1, "Permanent resident" = 2)),
    # years are coded 1..91 and labelled with the year
    cps_yob = haven::labelled(c(51, -99), labels = c("-99" = -99, "1920" = 1, "1970" = 51)),
    cps_age_in_years = haven::labelled(c(52, 30), labels = stats::setNames(c(30, 52), c("30", "52"))),
    cps_genderid = haven::labelled(c(1, 2), labels = c("A man" = 1, "A woman" = 2)),
    cps_edu = haven::labelled(c(3, 4), labels = c("Some" = 3, "More" = 4)),
    cps_interest_1 = c(7, 8),
    cps_ideoself_1 = c(5, 6),
    cps_income = c(77000, 0),
    cps_votechoice1_8_TEXT = c("", "Bloc Montréal"),
    stringsAsFactors = FALSE
  )
  out <- qesR:::.build_decon(dat, srvy = "qes2022")
  family <- function(x) if (is.factor(x)) "factor" else if (is.numeric(x)) "numeric" else class(x)[1]
  expect_identical(vapply(out[names(v044)], family, character(1)), v044)
})
