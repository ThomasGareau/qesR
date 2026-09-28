# qes_studies(), the study-code rules and get_qescodes() on the catalog
# (slice S1, design.md sections 2.1-2.3 and 3).

study_cols <- c(
  "study", "aliases", "family", "title_deposit", "title_en", "title_fr", "authors",
  "year", "year_end", "election_id", "study_design", "default_member",
  "target_population_en", "target_population_fr", "server", "doi",
  "dataset_version", "data_file_id", "label_file_id", "source_lang", "licence",
  "licence_url", "metadata_shipped", "publisher", "citation_year", "dataset_unf",
  "notes_en", "notes_fr", "doi_url", "waves"
)

test_that("qes_studies() returns the offline catalog with doi_url and waves", {
  s <- qes_studies()
  expect_s3_class(s, "data.frame")
  expect_identical(names(s), study_cols)
  expect_identical(nrow(s), 13L)
  expect_false("qes_demo" %in% s$study)
  expect_identical(s$doi_url, paste0("https://doi.org/", s$doi))
  # waves come from the harmonization spec (slice HZ4), in field order; NA
  # for a study the spec does not cover yet
  waves <- stats::setNames(s$waves, s$study)
  expect_identical(unname(waves[c("qes2022", "qes2018", "qes2014", "qes2012", "qes2018_panel",
                                  "qes2007_panel", "qes2012_panel")]),
                   c("cps;pes", "post", "post", "post", "pre;post", "pre;post", "pre;post"))
  expect_identical(unname(waves[c("qes2007", "qes2008", "qes1998")]), c("post", "post", "pre;post"))
  # the pooled CROP polls: one wave per monthly poll, in field order
  crop <- strsplit(waves[["qes_crop_2007_2010"]], ";", fixed = TRUE)[[1]]
  expect_length(crop, 24L)
  expect_identical(crop[c(1, 24)], c("poll_2007_06", "poll_2010_01"))
  # the firms' own 1998 files are not harmonized
  expect_true(all(is.na(waves[c("qes1998_crop", "qes1998_createc")])))
  expect_type(s$year, "integer")
  expect_type(s$default_member, "logical")
  expect_identical(rownames(s), as.character(seq_len(nrow(s))))
})

test_that("qes_studies() is visible, offline and language-independent", {
  expect_visible(qes_studies())
  en <- withr::with_options(list(qesR.lang = "en"), qes_studies())
  fr <- withr::with_options(list(qesR.lang = "fr"), qes_studies())
  expect_identical(en, fr)
})

test_that("qes_studies(family =) filters, and validates its input", {
  expect_identical(
    qes_studies(family = "qes")$study,
    c("qes2022", "qes2018", "qes2014", "qes2012", "qes2008", "qes2007")
  )
  expect_identical(
    qes_studies(family = c("durand_panel", "crop_polls"))$study,
    c("qes2018_panel", "qes2012_panel", "qes_crop_2007_2010", "qes2007_panel")
  )
  err <- expect_error(qes_studies(family = "QES"), class = "qesR_error_input")
  expect_identical(err$arg, "family")
  expect_error(qes_studies(family = "demo"), class = "qesR_error_input")
  expect_error(qes_studies(check_updates = NA), class = "qesR_error_input")
})

test_that("the default members are the QES election studies", {
  s <- qes_studies()
  expect_identical(s$study[s$default_member], s$study[s$family == "qes"])
})

test_that("non-QES studies carry their own titles and authors", {
  s <- qes_studies()
  panels <- s[s$family == "durand_panel", , drop = FALSE]
  expect_false(any(grepl("Quebec Election Study", panels$title_en, fixed = TRUE)))
  expect_true(all(grepl("^Durand, Claire", panels$authors)))
  expect_true(all(grepl("\u00e9", s$title_fr[s$family == "qes"])))
})

test_that("qes_studies() is served by the .qes_catalog() seam", {
  local_fixture_catalog()
  s <- qes_studies()
  expect_identical(s$study, c("qes2018", "qes_fixture_b"))
  expect_identical(s$doi_url[1], "https://doi.org/10.9999/FIX/AAAAAA")
  expect_identical(qes_studies(family = "durand_panel")$study, "qes_fixture_b")
})

test_that("study codes are trimmed, case-insensitive and matched on aliases, never fuzzily", {
  resolve <- getFromNamespace(".qes_resolve_codes", "qesR")
  expect_identical(resolve(" QES2018 ", "x"), "qes2018")
  expect_identical(resolve(c("qes2018", "QES2018"), "x"), "qes2018")
  expect_identical(resolve("all", "x"), qes_studies()$study)
  expect_error(resolve(c("all", "qes2018"), "x"), class = "qesR_error_input")
  err <- expect_error(resolve("2018", "x"), class = "qesR_error_unknown_study")
  expect_true("qes2018" %in% err$suggestions)
  expect_error(resolve("qes_demo", "x"), class = "qesR_error_unknown_study")
  expect_identical(resolve("qes_demo", "x", demo = TRUE), "qes_demo")

  local_fixture_catalog()
  expect_identical(resolve("FIXB", "x"), "qes_fixture_b")
})

test_that("get_qescodes() is a legacy adapter over the catalog", {
  local_qes_notices_shown()
  codes <- get_qescodes(detailed = TRUE)
  expect_identical(names(codes), v044_qescodes_detailed_cols)
  expect_identical(codes$qes_survey_code, qes_studies()$study)
  expect_identical(codes$index, seq_len(nrow(codes)))
  expect_identical(codes$qes_survey_code[12:13], c("qes1998_crop", "qes1998_createc"))
  expect_identical(codes$year[codes$qes_survey_code == "qes_crop_2007_2010"], "2007-2010")
  expect_type(codes$year, "character")
  expect_identical(codes$documentation, codes$doi_url)
  expect_true(all(startsWith(codes$doi_url, "https://doi.org/")))
  expect_identical(
    codes$name_fr[codes$qes_survey_code == "qes2018"],
    "\u00c9tude \u00e9lectorale qu\u00e9b\u00e9coise 2018"
  )
})

test_that("get_qescodes() announces qes_studies() once per session", {
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = NULL)
  expect_identical(count_class(get_qescodes(), "qesR_message_deprecated"), 1L)
  expect_identical(count_class(get_qescodes(), "qesR_message_deprecated"), 0L)
})

test_that("the legacy master keeps the 11 v0.4.4 studies for NULL and \"all\"", {
  validate <- getFromNamespace(".validate_master_surveys", "qesR")
  expect_identical(validate(NULL), v044_qescodes)
  expect_identical(validate("all"), v044_qescodes)
  expect_identical(validate(" ALL "), v044_qescodes)
  expect_identical(validate(c("qes2018", "qes2022")), c("qes2018", "qes2022"))
})

test_that("the legacy master refuses the 1998 firm codes until the engine", {
  validate <- getFromNamespace(".validate_master_surveys", "qesR")
  err <- expect_error(validate("qes1998_crop"), class = "qesR_error_input")
  expect_identical(err$value, "qes1998_crop")
  # qes1998 already holds the CROP and CREATEC respondents: no double count
  err <- expect_error(
    validate(c("qes1998", "qes1998_crop", "qes1998_createc")),
    class = "qesR_error_input"
  )
  expect_identical(err$value, c("qes1998_crop", "qes1998_createc"))
  expect_error(get_qes_master("qes1998_createc", quiet = TRUE), class = "qesR_error_input")
})

test_that("get_qes() reads each study's pinned data file, never one chosen by size", {
  select <- getFromNamespace(".qes_select_data_file", "qesR")
  files <- shipped_catalog()$files
  for (code in v044_qescodes) {
    pinned <- files$file_id[files$study == code & files$role == "data" & files$is_default]
    expect_identical(select(code)$file$file_id, pinned, info = code)
  }
  # the three 1998 studies of one deposit each read their own file
  expect_identical(select("qes1998")$file$file_id, "329987")
  expect_identical(select("qes1998_crop")$file$file_id, "286331")
  expect_identical(select("qes1998_createc")$file$file_id, "316121")
  # twins: the SPSS file is pinned for qes2014, the Stata file for qes2012
  expect_identical(select("qes2014")$file$format, "sav")
  expect_identical(select("qes2012")$file$format, "dta")
})

test_that("`file` chooses among a study's data files only ([A:D6])", {
  select <- getFromNamespace(".qes_select_data_file", "qesR")
  expect_identical(select("qes2014", file = "STATA")$file$file_id, "425915")
  expect_identical(select("qes2014", file = "\\.dta$")$file$file_id, "425915")
  expect_identical(select("qes2014", file = "spss")$file$file_id, "425916")

  err <- expect_error(select("qes2014", file = "Quebec Election"), class = "qesR_error_ambiguous_file")
  expect_setequal(err$candidates, c("Quebec Election Study 2014.sav", "Quebec Election Study 2014.dta"))

  err <- expect_error(select("qes2018", file = "questionnaire"), class = "qesR_error_ambiguous_file")
  expect_identical(err$study, "qes2018")
  expect_identical(err$candidates, "Quebec Election Study 2018.dta")

  # the qes2012 SPSS twin is the label donor, and a complete data file: it
  # can be chosen (get_qes("qes2012", file = "SPSS") read it in 0.4.4), but is
  # never the default
  expect_identical(select("qes2012", file = "SPSS")$file$file_id, "425917")
  expect_identical(select("qes2012", file = "STATA")$file$file_id, "425918")
  expect_identical(select("qes2012")$file$file_id, "425918")
  expect_error(select("qes2012", file = "Quebec Election Study 2012"), class = "qesR_error_ambiguous_file")
  expect_error(select("qes2014", file = c("a", "b")), class = "qesR_error_input")

  # a pattern R cannot compile is an input error, not a bare regex error
  err <- expect_error(select("qes2014", file = "("), class = "qesR_error_input")
  expect_identical(err$arg, "file")
  expect_identical(err$value, "(")
})

test_that("`file` naming another study of the same deposit reads that study, with a message", {
  select <- getFromNamespace(".qes_select_data_file", "qesR")
  msg <- NULL
  out <- withCallingHandlers(
    select("qes1998", file = "CROP"),
    qesR_message_download = function(m) {
      msg <<- m
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(out$study, "qes1998_crop")
  expect_identical(out$file$file_id, "286331")
  expect_identical(msg$id, "file_redirect")
  expect_silent(select("qes1998", file = "CREATEC", quiet = TRUE))
  # never across deposits
  expect_error(select("qes2012", file = "2014"), class = "qesR_error_ambiguous_file")
})

test_that("a codebook describes the pinned data file of its own study only ([A:D2])", {
  # The shared 1998 deposit: each of its three studies has its own metadata,
  # built from its own data file, offline.
  local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  crop <- qes_codebook("qes1998_crop")
  createc <- qes_codebook("qes1998_createc")
  files <- shipped_catalog()$files
  expect_identical(nrow(crop), files$n_cols[files$file_id == "286331"])
  expect_identical(nrow(createc), files$n_cols[files$file_id == "316121"])
  expect_identical(attr(crop, "survey_code"), "qes1998_crop")
  expect_identical(attr(crop, "selected_data_file"), files$file_name[files$file_id == "286331"])
  # the file list comes from the catalog: no dataset listing request
  expect_true(all(attr(crop, "files")$file_id %in% files$file_id))
  # `file` naming a sibling's data file describes that sibling
  expect_identical(
    attr(suppressMessages(qes_codebook("qes1998", file = "CROP")), "survey_code"),
    "qes1998_crop"
  )
})

test_that("qes_studies() prints a compact view in the session language", {
  s <- qes_studies()
  expect_s3_class(s, c("qes_studies", "data.frame"), exact = TRUE)
  withr::local_options(qesR.lang = "en", width = 200)
  out <- capture.output(res <- withVisible(print(s)))
  expect_false(res$visible)
  header <- out[1]
  for (col in c("study", "year", "family", "study_design", "licence", "title")) {
    expect_match(header, col, fixed = TRUE)
  }
  expect_false(grepl("notes_en", paste(out, collapse = "\n"), fixed = TRUE))
  expect_match(out[length(out)], "13 studies", fixed = TRUE)
  expect_match(out[length(out)], "all 30 columns", fixed = TRUE)
  # the title follows the language; the data do not
  withr::local_options(qesR.lang = "fr")
  fr <- capture.output(print(s))
  expect_true(any(grepl(substr(s$title_fr[s$study == "qes2018"], 1L, 20L), fr, fixed = TRUE)))
  expect_match(fr[length(fr)], "13 études", fixed = TRUE)
  # subsets: rows keep the compact print, columns are a plain data frame
  rows <- s[s$family == "qes", ]
  expect_s3_class(rows, "qes_studies")
  expect_no_error(capture.output(print(rows)))
  cols <- s[, c("study", "doi")]
  expect_identical(class(cols), "data.frame")
  expect_identical(s[["study"]], s$study)
  expect_type(s[, "study"], "character")
  expect_identical(class(as.data.frame(s)), "data.frame")
  empty <- s[0, ]
  expect_no_error(capture.output(print(empty)))
  # the legacy table is unchanged
  local_qes_notices_shown()
  expect_identical(class(get_qescodes()), "data.frame")
})
