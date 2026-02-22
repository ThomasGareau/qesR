test_that("qes_codebook alias mirrors get_codebook formals", {
  expect_true(identical(names(formals(qes_codebook)), names(formals(get_codebook))))
})

test_that("get_codebook_files reads attached file manifest", {
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

  compact <- format_codebook(cb, layout = "compact")
  expect_false("value_labels" %in% names(compact))

  wide <- format_codebook(cb, layout = "wide")
  expect_true("value_labels" %in% names(wide))
  expect_true(is.list(wide$value_labels))

  long <- format_codebook(cb, layout = "long")
  expect_true(all(c("variable", "value", "value_label") %in% names(long)))
  expect_true(nrow(long) == 2L)
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

  as_list <- get_value_labels(cb)
  expect_true(is.list(as_list))
  expect_true("vote_choice" %in% names(as_list))

  as_long <- get_value_labels(cb, long = TRUE)
  expect_true(all(c("variable", "value", "value_label") %in% names(as_long)))
  expect_true(nrow(as_long) == 2L)
})
