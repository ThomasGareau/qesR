# Codebooks (slice S3, design.md sections 2.3 and 6.2): built offline from
# the shipped dictionary. The DDI path and its tests are gone with xml2.

test_that("get_question uses attached codebook metadata", {
  local_qes_notices_shown()
  dat <- data.frame(vote_choice = c(1, 2, 1))
  codebook <- data.frame(
    variable = "vote_choice",
    label = "Vote choice",
    question = "Which party did you vote for?",
    n_value_labels = 2L,
    stringsAsFactors = FALSE
  )

  attr(dat, "qes_codebook") <- codebook
  expect_identical(get_question(dat, "vote_choice"), "Which party did you vote for?")
})

test_that("qes_codebook() is offline for a shipped study", {
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  cb <- qes_codebook("qes2014")
  expect_s3_class(cb, "qes_codebook")
  expect_identical(nrow(cb), shipped_catalog()$files$n_cols[shipped_catalog()$files$file_id == "425916"])
  expect_identical(
    names(cb),
    c("variable", "label", "question", "n_value_labels", "study", "position", "type",
      "question_lang", "question_truncated", "value_labels", "missing_codes", "targets",
      "label_source", "question_source", "doc_ref")
  )
  q19 <- cb[cb$variable == "Q19", ]
  expect_identical(q19$value_labels, "1=Oui | 2=Non | 8=Je ne sais pas | 9=Je préfère ne pas répondre")
  expect_identical(q19$missing_codes, "8=dk | 9=refused")
  expect_identical(q19$question_lang, "fr")
  expect_identical(q19$question_source, "questionnaire")
  expect_match(q19$doc_ref, "352009:Q19", fixed = TRUE)
})

test_that("codebook attributes survive every layout ([A:K1])", {
  for (layout in c("compact", "wide", "long")) {
    cb <- qes_codebook("qes2018", layout = layout)
    for (a in c("survey_code", "doi", "doi_url", "selected_data_file", "files", "codebook_files", "qes_provenance")) {
      expect_false(is.null(attr(cb, a, exact = TRUE)), info = paste(layout, a))
    }
    expect_identical(attr(cb, "survey_code"), "qes2018")
    # the documents of the deposit are listed ([A:K2]): EN/FR questionnaires,
    # the programmed codebook and the methodological report
    expect_identical(nrow(attr(cb, "codebook_files")), 4L, info = layout)
    expect_true(all(attr(cb, "codebook_files")$file_id %in% qes_docs("qes2018")$file_id))
  }
})

test_that("a label that repeats the variable name is no label, and question is never a copy ([A:K4], [A:K5])", {
  cb <- qes_codebook("qes2018")
  # 253 of 254 qes2018 file labels are the variable's own name
  expect_false(any(tolower(cb$label) == tolower(cb$variable), na.rm = TRUE))
  expect_false(any(cb$question == cb$variable, na.rm = TRUE))
  both <- !is.na(cb$label) & !is.na(cb$question)
  expect_false(any(cb$label[both] == cb$question[both]))
  q1 <- cb[cb$variable == "responseid", ]
  expect_true(is.na(q1$label))
  expect_true(is.na(q1$question))
  # labels and questions have no raw newlines
  expect_false(any(grepl("\n", c(cb$label, cb$question)), na.rm = TRUE))
})

test_that("qes2018 value labels come from its questionnaire, with verified counts", {
  cb <- qes_codebook("qes2018", layout = "long", variables = c("q26", "q6"))
  q26 <- cb[cb$variable == "q26", ]
  expect_identical(q26$value, c("1", "2", "8", "9"))
  expect_identical(q26$value_label[q26$value == "8"], "Je ne sais pas")
  expect_identical(q26$missing_type[q26$value == "8"], "dk")
  values <- getFromNamespace(".qes_dict_study", "qesR")("qes2018")$values
  q6 <- values[values$variable == "q6", ]
  expect_identical(q6$n, c(490L, 392L, 700L, 342L, 31L, 92L, 160L))
  expect_identical(q6$missing_type[q6$value == "95"], "spoiled")
  expect_identical(unique(q6$label_source), "supplement")
})

test_that("the long layout keeps variables without value labels ([A:K7])", {
  cb <- qes_codebook("qes2014", layout = "long", variables = c("QUEST", "Q19"))
  expect_identical(
    names(cb),
    c("variable", "value", "value_label", "label", "question", "study", "missing_type", "is_declared_na")
  )
  quest <- cb[cb$variable == "QUEST", ]
  expect_identical(nrow(quest), 1L)
  expect_true(is.na(quest$value))
  expect_identical(sum(cb$variable == "Q19"), 4L)
})

test_that("declared SPSS missing codes are typed user_na unless a label types them", {
  cb <- qes_codebook("qes2007_panel", layout = "long", variables = "interet")
  expect_identical(cb$missing_type[cb$value == "9"], "dk_refused")
  expect_true(cb$is_declared_na[cb$value == "9"])
  v <- getFromNamespace(".qes_dict_study", "qesR")("qes2007_panel")$values
  expect_true(any(v$missing_type %in% "user_na"))
})

test_that("lang chooses the question language, NULL the study's own", {
  en <- qes_codebook("qes2014", variables = "Q19", lang = "en")
  fr <- qes_codebook("qes2014", variables = "Q19", lang = "fr")
  own <- qes_codebook("qes2014", variables = "Q19")
  expect_identical(en$question_lang, "en")
  expect_match(en$question, "^If there were a referendum")
  expect_identical(fr$question_lang, "fr")
  expect_identical(own$question, fr$question)
  # no machine translation: unknown text stays NA
  en_panel <- qes_codebook("qes2007_panel", variables = "interet", lang = "en")
  expect_true(is.na(en_panel$question))
  expect_error(qes_codebook("qes2014", lang = "de"), class = "qesR_error_input")
})

test_that("variables must match exactly, with suggestions", {
  err <- expect_error(qes_codebook("qes2014", variables = "q19"), class = "qesR_error_unknown_variable")
  expect_true("Q19" %in% err$suggestions)
  expect_identical(err$variables, "q19")
})

test_that("refresh is a no-op with a once-per-session note", {
  local_qes_once()
  expect_message(
    a <- qes_codebook("qes2014", refresh = TRUE),
    class = "qesR_message_arg_ignored"
  )
  expect_identical(a, qes_codebook("qes2014"))
  expect_no_message(qes_codebook("qes2014", refresh = TRUE), class = "qesR_message_arg_ignored")
})

test_that("a codebook can be laid out again; a plain data frame is an error ([A:A5])", {
  local_qes_notices_shown()
  cb <- qes_codebook("qes2014", variables = c("Q2", "Q19"))
  long <- qes_codebook(cb, layout = "long")
  expect_identical(long, qes_codebook("qes2014", variables = c("Q2", "Q19"), layout = "long"))
  expect_identical(format_codebook(cb, layout = "long"), long)
  # round trip back to compact
  expect_identical(qes_codebook(long, layout = "compact"), cb)

  plain <- as.data.frame(unclass(cb))
  attributes(plain) <- attributes(plain)[c("names", "row.names", "class")]
  class(plain) <- "data.frame"
  expect_error(format_codebook(plain), class = "qesR_error_input")
  expect_error(qes_codebook(plain), class = "qesR_error_input")
  expect_error(qes_codebook(42), class = "qesR_error_input")
})

test_that("format_codebook() on get_qes() data says it is data, not a codebook", {
  local_qes_notices_shown()
  withr::local_options(qesR.lang = "en")
  demo <- get_qes("qes_demo", quiet = TRUE)
  err <- expect_error(format_codebook(demo), class = "qesR_error_input")
  expect_match(conditionMessage(err), "is data read by get_qes()", fixed = TRUE)
  expect_no_match(conditionMessage(err), "plain data frame", fixed = TRUE)
})

test_that("the codebook of get_qes() data describes its columns", {
  local_qes_notices_shown()
  demo <- get_qes("qes_demo", quiet = TRUE)
  cb <- attr(demo, "qes_codebook")
  expect_identical(cb$variable, names(demo))
  expect_identical(attr(cb, "qes_provenance"), attr(demo, "qes_provenance"))
  sub <- qes_codebook(demo, variables = c("Q19", "Q28"))
  expect_identical(sub$variable, c("Q19", "Q28"))
  # the demo shares qes2014's names and wording
  expect_identical(
    qes_question("qes_demo", "Q19")$question,
    qes_question("qes2014", "Q19")$question
  )
  # a frame that kept its attributes but lost columns gets a smaller codebook
  kept <- demo
  kept$Q3 <- NULL
  expect_false("Q3" %in% qes_codebook(kept)$variable)
})

test_that("print() shows the study and counts variables, not rows", {
  cb <- qes_codebook("qes2014", layout = "long", variables = c("Q2", "Q19"))
  out <- utils::capture.output(print(cb, n = 3))
  expect_true(any(grepl("survey: qes2014", out, fixed = TRUE)))
  expect_true(any(grepl("Variables: 2", out, fixed = TRUE)))
  expect_true(any(grepl("Rows: 7", out, fixed = TRUE)))
})

test_that("print() names the data file get_qes() reads, and the Dataverse copy", {
  cb <- qes_codebook("qes2014", variables = "Q2")
  row <- qesR:::.qes_default_data_file("qes2014")
  # the attribute keeps the qesR 0.4.4 value, the Dataverse name
  expect_identical(attr(cb, "selected_data_file"), row$file_name)
  out <- utils::capture.output(print(cb, n = 1))
  line <- out[startsWith(out, "Data file:")]
  expect_length(line, 1L)
  read_name <- qesR:::.qes_deposit_name(row)
  expect_true(startsWith(line, paste("Data file:", read_name)))
  if (!identical(read_name, row$file_name)) {
    expect_match(line, paste0("(Dataverse: ", row$file_name, ")"), fixed = TRUE)
  }
  # a codebook naming a file the catalog does not have prints it as is
  expect_identical(qesR:::.qes_codebook_read_name("qes2014", "other.tab"), "other.tab")
  expect_identical(qesR:::.qes_codebook_read_name(NULL, "other.tab"), "other.tab")
})
