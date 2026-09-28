# qes_cite() and inst/CITATION (slice S1, design.md sections 2.2, 8.1, 9).

test_that("qes_cite() with no study cites qesR", {
  txt <- qes_cite()
  expect_length(txt, 1L)
  expect_match(txt, "Gareau-Paquette, Thomas", fixed = TRUE)
  expect_match(txt, as.character(utils::packageVersion("qesR")), fixed = TRUE)
  bib <- qes_cite(style = "bibentry")
  expect_s3_class(bib, "bibentry")
  expect_length(bib, 1L)
})

test_that("dataset citations follow the Dataverse citation", {
  txt <- qes_cite(c("qes2012", "qes2022"))
  expect_length(txt, 3L)
  expect_identical(
    txt[2],
    paste0(
      "B\u00e9langer, \u00c9ric; Nadeau, Richard; Henderson, Ailsa; Hepburn, Eve, 2023, ",
      "\"\u00c9tude \u00e9lectorale qu\u00e9b\u00e9coise 2012\", https://doi.org/10.5683/SP2/WXUPXT, ",
      "Borealis, V1, UNF:6:nG192rAWV0IlYSpRg4WBaQ=="
    )
  )
  expect_match(txt[3], "Harvard Dataverse, V1.1, UNF:6:I/DFDdqJv7wNEoyyRdxaIw==", fixed = TRUE)
})

test_that("the three 1998 surveys cite the shared deposit and their file", {
  txt <- qes_cite(c("qes1998", "qes1998_crop", "qes1998_createc"))
  expect_length(txt, 4L)
  expect_true(all(grepl("https://doi.org/10.5683/SP2/QFUAWG", txt[-1], fixed = TRUE)))
  expect_match(txt[2], "[file: Total_panel_election_QC1998.sav]", fixed = TRUE)
  expect_match(txt[3], "[file: Total_sondages_election_CROP1998.sav]", fixed = TRUE)
  expect_match(txt[4], "[file: Total_sondages_election_CREATEC1998.sav]", fixed = TRUE)
  # a study that is a whole deposit names no file
  expect_false(grepl("[file:", qes_cite("qes2018")[2], fixed = TRUE))
})

test_that("lang changes only qesR's own words, never the session language", {
  en <- qes_cite("qes1998_crop")
  fr <- qes_cite("qes1998_crop", lang = "fr")
  expect_false(identical(en, fr))
  expect_match(fr[2], "fichier\u00a0: ", fixed = TRUE)
  expect_identical(withr::with_options(list(qesR.lang = "fr"), qes_cite("qes1998_crop")), en)
  expect_error(qes_cite(lang = "de"), class = "qesR_error_input")
})

test_that("bibtex and bibentry styles carry the same entries", {
  bib <- qes_cite(c("qes2014", "qes2007_panel"), style = "bibentry")
  expect_s3_class(bib, "bibentry")
  expect_length(bib, 3L)
  keys <- vapply(unclass(bib), function(e) attr(e, "key"), character(1))
  expect_identical(keys, c("qesR", "qes2014", "qes2007_panel"))
  expect_identical(bib[[2]]$doi, "10.5683/SP3/64F7WR")
  expect_identical(bib[[2]]$version, "V1")
  expect_identical(format(bib[[3]]$author), c("Claire Durand", "John Goyder"))

  tex <- qes_cite(c("qes2014", "qes2007_panel"), style = "bibtex")
  expect_length(tex, 3L)
  expect_match(tex[2], "^@Misc\\{qes2014,")
  expect_match(tex[1], "^@Manual\\{qesR,")
})

test_that("qes_cite() reads the study of a data frame, or refuses one without it", {
  dat <- structure(data.frame(x = 1), qes_survey_code = "qes2018")
  expect_identical(qes_cite(dat), qes_cite("qes2018"))
  prov <- structure(data.frame(x = 1), qes_provenance = data.frame(study = c("qes2014", "qes2012")))
  expect_identical(qes_cite(prov), qes_cite(c("qes2014", "qes2012")))
  err <- expect_error(qes_cite(data.frame(x = 1)), class = "qesR_error_no_provenance")
  expect_identical(err$arg, "x")
  expect_error(qes_cite("qes2019"), class = "qesR_error_unknown_study")
  # the synthetic demo has no deposit to cite
  expect_identical(qes_cite("qes_demo"), qes_cite())
})

test_that("qes_cite() is served by the .qes_catalog() seam", {
  local_fixture_catalog()
  txt <- qes_cite("qes_fixture_b")
  expect_match(txt[2], "^Doe, Jane, 2024, \"Fixture study B\", https://doi.org/10.9999/FIX/BBBBBB, Example Dataverse, V2.1, UNF:6:fixtureB==$")
})

test_that("qes_cite() of harmonized data gives the spec version and content hash", {
  testthat::local_mocked_bindings(.qes_transport = function(...) stop("no request expected"), .package = "qesR")
  h <- suppressWarnings(qes_harmonize("qes2014", data = list(qes2014 = .qes_synthetic("qes2014")$qes2014),
                                      include_draft = TRUE, quiet = TRUE))
  sp <- attr(h, "qes_spec")
  txt <- qes_cite(h)
  expect_length(txt, 2L)
  expect_match(txt[1], sprintf("harmonization spec %s (content hash %s)", sp$version, sp$hash), fixed = TRUE)
  expect_identical(txt[2], qes_cite("qes2014")[2])
  fr <- qes_cite(h, lang = "fr")
  expect_match(fr[1], sprintf("spécification d'harmonisation %s", sp$version), fixed = TRUE)
  bib <- qes_cite(h, style = "bibentry")
  expect_match(bib[[1]]$note, sp$hash, fixed = TRUE)
  expect_match(qes_cite(h, style = "bibtex")[1], sp$hash, fixed = TRUE)
  # without harmonized data, qesR alone, as citation("qesR")
  expect_false(grepl("spec", qes_cite()[1], fixed = TRUE))
})

test_that("a bad style is an input error", {
  expect_error(qes_cite("qes2014", style = NA), class = "qesR_error_input")
  expect_error(qes_cite("qes2014", style = "bad"), class = "qesR_error_input")
  expect_identical(qes_cite("qes2014", style = "bibt"), qes_cite("qes2014", style = "bibtex"))
})
