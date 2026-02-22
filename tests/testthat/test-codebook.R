test_that("DDI parsing and label application works", {
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

  dat <- data.frame(vote_choice = c(1, 2, 1), interest = c(3, 2, 1))
  out <- qesR:::.apply_ddi_labels(dat, parsed)

  expect_true(inherits(out$vote_choice, "haven_labelled"))
  expect_true(identical(attr(out$vote_choice, "label"), "Vote choice"))
  expect_true(identical(attr(out$vote_choice, "qes_question"), "Which party did you vote for?"))
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
