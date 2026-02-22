test_that("PDF line parser extracts question text by variable", {
  lines <- c(
    "cps_age_in_years To make sure we are talking to a cross section of Canadians, we need to",
    "get a little information about your background. First, how old are you?",
    "\u25BC 15 (15) ... 115 (115)",
    "cps_age_in_years Afin d'etre certains que nous nous adressons a un echantillon representatif",
    "cps_genderid Are you...?",
    "\u25BC A man"
  )

  parsed <- qesR:::.extract_codebook_questions_from_lines(
    lines,
    variables = c("cps_age_in_years", "cps_genderid")
  )

  expect_true("cps_age_in_years" %in% names(parsed))
  expect_true("cps_genderid" %in% names(parsed))
  expect_true(grepl("how old are you\\?$", tolower(parsed[["cps_age_in_years"]])))
  expect_true(identical(parsed[["cps_genderid"]], "Are you...?"))
})

test_that("codebook enrichment fills missing question from label", {
  cb <- data.frame(
    variable = c("var_a", "var_b"),
    label = c("Label A", NA_character_),
    question = c(NA_character_, NA_character_),
    n_value_labels = c(0L, 0L),
    stringsAsFactors = FALSE
  )

  manifest <- data.frame(
    file_id = character(0),
    filename = character(0),
    extension = character(0),
    size = numeric(0),
    download_url = character(0),
    stringsAsFactors = FALSE
  )

  study <- qesR:::.get_qes_study("qes2022")
  out <- qesR:::.enrich_codebook_questions(cb, study = study, file_manifest = manifest, quiet = TRUE)

  expect_true(identical(out$question[1], "Label A"))
  expect_true(is.na(out$question[2]))
})
