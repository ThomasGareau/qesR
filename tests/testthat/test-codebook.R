# Restored from 397aa0c^ in slice S0b. The DDI tests go with the DDI path and
# xml2 in slice S3; the get_question() test stays. Since slice S2b the DDI
# only feeds the interim codebook: labels on the data come from the data file
# (test-readers.R), so the old check that DDI labels are applied to the data
# is gone.

test_that("DDI parsing works", {
  ddi <- xml2::read_xml(
    '<codeBook><dataDscr>
      <var ID="V1" name="vote_choice">
        <labl>Vote choice</labl>
        <qstn><qstnLit>Which party did you vote for?</qstnLit></qstn>
        <catgry><catValu>1</catValu><labl>Party A</labl></catgry>
        <catgry><catValu>2</catValu><labl>Party B</labl></catgry>
      </var>
      <var ID="V2" name="interest">
        <labl>Political interest</labl>
        <qstn><qstnLit>How interested are you in politics?</qstnLit></qstn>
      </var>
    </dataDscr></codeBook>'
  )

  parsed <- qesR:::.parse_qes_ddi(ddi)
  expect_true(is.list(parsed))
  expect_true(all(c("data", "value_labels") %in% names(parsed)))
  expect_true("vote_choice" %in% parsed$data$variable)

  row <- parsed$data[parsed$data$variable == "vote_choice", , drop = FALSE]
  expect_identical(row$label, "Vote choice")
  expect_identical(row$question, "Which party did you vote for?")
  expect_identical(unname(parsed$value_labels$vote_choice), c("Party A", "Party B"))
})

test_that("DDI parser ignores trivial value labels", {
  ddi <- xml2::read_xml(
    '<codeBook><dataDscr>
      <var ID="V1" name="age">
        <labl>Age</labl>
        <catgry><catValu>1</catValu><labl>1</labl></catgry>
        <catgry><catValu>2</catValu><labl>2.0</labl></catgry>
        <catgry><catValu>99</catValu><labl>Prefer not to say</labl></catgry>
      </var>
    </dataDscr></codeBook>'
  )

  parsed <- qesR:::.parse_qes_ddi(ddi)
  expect_true(is.list(parsed))
  expect_true(identical(parsed$data$n_value_labels[1], 1L))
  expect_true(identical(unname(parsed$value_labels$age), "Prefer not to say"))
  expect_true(identical(names(parsed$value_labels$age), "99"))
})

test_that("DDI parser drops open-text style value maps for *_other variables", {
  cats <- paste0(
    "<catgry><catValu>", 1:16, "</catValu><labl>",
    "Very long open text response option ", 1:16,
    "</labl></catgry>",
    collapse = ""
  )

  xml <- paste0(
    "<codeBook><dataDscr>",
    "<var ID='V1' name='q2_96_other'><labl>q2_96_other</labl>", cats, "</var>",
    "</dataDscr></codeBook>"
  )

  parsed <- qesR:::.parse_qes_ddi(xml2::read_xml(xml))
  expect_true(is.list(parsed))
  expect_true(identical(parsed$data$n_value_labels[1], 0L))
  expect_true(identical(length(parsed$value_labels$q2_96_other), 0L))
})

test_that("get_question uses attached codebook metadata", {
  dat <- data.frame(vote_choice = c(1, 2, 1))
  codebook <- data.frame(
    variable = "vote_choice",
    label = "Vote choice",
    question = "Which party did you vote for?",
    n_value_labels = 2L,
    stringsAsFactors = FALSE
  )

  attr(dat, "qes_codebook") <- codebook
  expect_true(identical(get_question(dat, "vote_choice"), "Which party did you vote for?"))
})
