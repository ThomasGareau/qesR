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
  # waves arrive with the harmonization spec (slice HZ4)
  expect_true(all(is.na(s$waves)))
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
  expect_identical(validate("qes1998_crop"), "qes1998_crop")
})

test_that("studies added after 0.4.4 read their pinned data file", {
  choose <- getFromNamespace(".choose_remote_file", "qesR")
  files <- data.frame(
    id = c("329987", "316121", "286331"),
    filename = c("Total_panel.tab", "Total_CREATEC.tab", "Total_CROP.tab"),
    extension = "tab",
    size = c(3, 2, 1),
    stringsAsFactors = FALSE
  )
  expect_identical(choose(files, study_code = "qes1998")$id, "329987")
  expect_identical(choose(files, study_code = "qes1998_crop", pinned_id = "286331")$id, "286331")
  err <- expect_error(
    choose(files[1:2, ], study_code = "qes1998_crop", pinned_id = "286331"),
    class = "qesR_error_source"
  )
  expect_identical(err$file_id, "286331")
})
