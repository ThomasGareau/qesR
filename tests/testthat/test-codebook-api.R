# Restored from 397aa0c^ in slice S0b. Triage: the alias test now checks that
# get_codebook()'s formals are a prefix of qes_codebook()'s, because slice S3
# appends `variables` and `lang` to qes_codebook() only (design.md section 2.3).
# Slice S3: codebooks made by hand (or by qesR 0.4.4), with value_labels_map,
# still work with the legacy helpers; the compact layout now also has the
# value labels as text, and the long layout keeps unlabelled variables.

test_that("qes_codebook alias mirrors get_codebook formals", {
  legacy <- names(formals(get_codebook))
  current <- names(formals(qes_codebook))
  expect_identical(current[seq_along(legacy)], legacy)
})

test_that("get_codebook_files reads attached file manifest", {
  local_qes_notices_shown()
  cb <- data.frame(
    variable = "x",
    label = "X",
    question = NA_character_,
    n_value_labels = 0L,
    stringsAsFactors = FALSE
  )
  class(cb) <- c("qes_codebook", class(cb))
  attr(cb, "codebook_files") <- data.frame(
    file_id = "1",
    filename = "Codebook.pdf",
    extension = "pdf",
    size = 100,
    download_url = "https://example.org/codebook.pdf",
    stringsAsFactors = FALSE
  )

  files <- get_codebook_files(codebook = cb)
  expect_s3_class(files, "data.frame")
  expect_true(identical(nrow(files), 1L))
  expect_true("filename" %in% names(files))
})

test_that("format_codebook supports compact, wide, and long layouts", {
  cb <- data.frame(
    variable = c("vote_choice", "interest"),
    label = c("Vote choice", "Political interest"),
    question = c("Which party did you vote for?", "How interested are you in politics?"),
    n_value_labels = c(2L, 0L),
    stringsAsFactors = FALSE
  )
  class(cb) <- c("qes_codebook", class(cb))
  attr(cb, "value_labels_map") <- list(
    vote_choice = c(`1` = "Party A", `2` = "Party B")
  )

  local_qes_notices_shown()
  compact <- format_codebook(cb, layout = "compact")
  expect_identical(names(compact)[1:4], c("variable", "label", "question", "n_value_labels"))
  expect_identical(compact$value_labels, c("1=Party A | 2=Party B", NA))
  expect_identical(compact$n_value_labels, c(2L, 0L))

  wide <- format_codebook(cb, layout = "wide")
  expect_true("value_labels" %in% names(wide))
  expect_true(is.list(wide$value_labels))
  expect_identical(wide$value_labels[[1]], c(`1` = "Party A", `2` = "Party B"))

  long <- format_codebook(cb, layout = "long")
  expect_true(all(c("variable", "value", "value_label") %in% names(long)))
  expect_identical(nrow(long), 3L)
  expect_identical(long$variable, c("vote_choice", "vote_choice", "interest"))
  expect_true(is.na(long$value[3]))
})

test_that("get_value_labels returns list and long table", {
  cb <- data.frame(
    variable = c("vote_choice"),
    label = c("Vote choice"),
    question = c("Which party did you vote for?"),
    n_value_labels = c(2L),
    stringsAsFactors = FALSE
  )
  class(cb) <- c("qes_codebook", class(cb))
  attr(cb, "value_labels_map") <- list(
    vote_choice = c(`1` = "Party A", `2` = "Party B")
  )

  local_qes_notices_shown()
  as_list <- get_value_labels(cb)
  expect_true(is.list(as_list))
  expect_true("vote_choice" %in% names(as_list))

  as_long <- get_value_labels(cb, long = TRUE)
  expect_true(all(c("variable", "value", "value_label") %in% names(as_long)))
  expect_true(nrow(as_long) == 2L)
})

test_that("get_value_labels() errors on an unknown variable and keeps '' labels ([A:K6])", {
  local_qes_notices_shown()
  cb <- qes_codebook("qes2012", variables = c("q52", "q64"))
  err <- expect_error(get_value_labels(cb, "q5"), class = "qesR_error_unknown_variable")
  expect_true("q52" %in% err$suggestions)
  labs <- get_value_labels(cb, "q64")
  # q64 = 96 has an empty label in the file (design.md section 6.1)
  expect_true("96" %in% names(labs$q64))
  expect_identical(unname(labs$q64["96"]), "")
  expect_identical(names(get_value_labels(cb, "q52")), "q52")
  expect_error(get_value_labels(42), class = "qesR_error_input")
})
