# The frozen tables of the interim legacy builders (inst/extdata/legacy/,
# R/legacy.R; design.md section 5.12, slice S4) and what they guarantee.

legacy <- function(name) qesR:::.qes_legacy_table(name)

master_source_columns <- setdiff(names(qesR:::.qes_master_types), c("qes_code", "qes_year", "qes_name_en"))
decon_source_columns <- setdiff(v044_decon_cols, "qes_code")
legacy_studies <- c(v044_qescodes, "qes_demo")

test_that("the legacy tables are UTF-8 CSV with LF endings, and match their schemas", {
  dir <- system.file("extdata", "legacy", package = "qesR", mustWork = TRUE)
  files <- list.files(dir, pattern = "\\.csv$", full.names = TRUE)
  expect_setequal(basename(files), c("sources.csv", "blanks.csv", "studies.csv", "columns.csv", "removed.csv"))
  for (f in files) {
    bytes <- file_bytes(f)
    expect_false(any(bytes == as.raw(0x0d)), info = basename(f))
    expect_true(validUTF8(rawToChar(bytes)), info = basename(f))
    name <- sub("\\.csv$", "", basename(f))
    expect_identical(names(legacy(name)), names(qesR:::.qes_schemas[[paste0("legacy_", name)]]), info = name)
  }
})

test_that("every column of every legacy study has exactly one frozen source", {
  src <- legacy("sources")
  expect_setequal(unique(src$profile), c("master", "decon"))
  for (p in c("master", "decon")) {
    cols <- if (p == "master") master_source_columns else decon_source_columns
    rows <- src[src$profile == p, , drop = FALSE]
    expect_setequal(unique(rows$study), legacy_studies)
    for (s in legacy_studies) {
      expect_identical(rows$column[rows$study == s], cols, info = paste(p, s))
    }
  }
  expect_false(anyDuplicated(paste(src$profile, src$column, src$study)) > 0L)
  # only respondent_id may be synthetic
  expect_true(all(src$column[src$source_variable %in% "(synthetic_rowid)"] == "respondent_id"))
  # the demo reads only variables its file has, with qes2014's choices
  demo <- src[src$study == "qes_demo" & !is.na(src$source_variable) & src$source_variable != "(synthetic_rowid)", ]
  demo_names <- names(get_qes("qes_demo", quiet = TRUE, with_codebook = FALSE))
  expect_true(all(demo$source_variable %in% demo_names))
  q14 <- src[src$study == "qes2014", ]
  expect_identical(
    demo$source_variable,
    q14$source_variable[match(paste(demo$profile, demo$column), paste(q14$profile, q14$column))]
  )
})

test_that("the frozen sources are those qesR 0.4.4 chose", {
  src <- legacy("sources")
  pick <- function(p, col, s) src$source_variable[src$profile == p & src$column == col & src$study == s]
  # spot checks against the source_map of the R9 baseline (design.md 13.3)
  expect_identical(pick("master", "turnout", "qes2018"), "q5")
  expect_identical(pick("master", "vote_choice", "qes2022"), "cps_votechoice1")
  expect_identical(pick("master", "ideology", "qes2018"), NA_character_)
  expect_identical(pick("master", "party_best", "qes2018"), "q8")
  expect_identical(pick("master", "survey_weight", "qes_crop_2007_2010"), "XPOND")
  expect_identical(pick("master", "respondent_id", "qes2007_panel"), "quest")
  expect_identical(pick("decon", "turnout", "qes2014"), "Q1")
  expect_identical(pick("decon", "votechoice", "qes2018"), "q2")
})

test_that("blank rules name real columns, studies and causes", {
  b <- legacy("blanks")
  for (i in seq_len(nrow(b))) {
    cols <- if (b$profile[i] == "master") master_source_columns else decon_source_columns
    expect_true(b$column[i] %in% c(cols, "*"), info = b$column[i])
    expect_true(b$study[i] %in% c(v044_qescodes, "*"), info = b$study[i])
    expect_true(nzchar(b$basis[i]))
    codes <- qesR:::.qes_split_list(b$codes[i])
    expect_false(anyNA(suppressWarnings(as.numeric(codes))), info = paste(b$column[i], b$study[i]))
  }
  expect_true(all(b$cause %in% c("A:H2", "A:H3", "A:H4", "A:H6", "A:H7", "OD4", "OD5", "OD8")))
  expect_false(anyDuplicated(paste(b$profile, b$column, b$study)) > 0L)
  # the blanks of design.md 5.12 (and OD4, OD5, OD8) are all there
  has <- function(col, s, p = "master") any(b$profile == p & b$column == col & b$study %in% c(s, "*"))
  expect_true(has("party_best", "qes2012") && has("party_lean", "qes2014"))
  expect_true(has("political_interest", "qes2018") && has("ideology", "qes2014"))
  expect_true(has("born_canada", "qes2018") && has("language", "qes2014") && has("language", "qes2022"))
  expect_true(has("income", "qes2018") && has("religion", "qes2018"))
  for (s in c("qes2022", "qes_crop_2007_2010", "qes1998")) expect_true(has("vote_choice", s), info = s)
  expect_true(has("turnout", "qes2022"))
  for (s in c("qes2007", "qes2008", "qes1998", "qes2012_panel")) {
    expect_true(has("sovereignty_support", s) && has("sovereignty", s), info = s)
  }
  expect_true(has("vote_choice_text", "qes2018"))
  expect_true(has("turnout", "qes2018", "decon") && has("votechoice", "qes2018", "decon"))
  expect_true(has("party_best", "qes2022", "decon") && has("partylean", "qes2022", "decon"))
})

test_that("the per-study constants and the column map cover the master", {
  s <- legacy("studies")
  expect_identical(s$study, v044_qescodes)
  timing <- qesR:::.qes_catalog()$enums
  timing <- timing$value[timing$enum == "target_timing"]
  expect_true(all(is.na(s$vote_choice_timing) | s$vote_choice_timing %in% timing))
  expect_true(all(is.na(s$decon_vote_timing) | s$decon_vote_timing %in% timing))
  expect_true(all(is.na(s$sovereignty_item) | s$sovereignty_item == "sov_indep"))
  # a timing is given only where vote_choice keeps a source
  b <- legacy("blanks")
  vote_blanked <- b$study[b$profile == "master" & b$column == "vote_choice" & is.na(b$codes)]
  expect_true(all(is.na(s$vote_choice_timing[s$study %in% vote_blanked])))
  sov_blanked <- b$study[b$profile == "master" & b$column == "sovereignty_support" & is.na(b$codes)]
  expect_true(all(is.na(s$sovereignty_item[s$study %in% sov_blanked])))
  expect_true(all(!is.na(s$sovereignty_item[!s$study %in% sov_blanked])))

  map <- legacy("columns")
  expect_identical(map$column, c(names(qesR:::.qes_master_types), names(qesR:::.qes_master_appended)))
  expect_true(all(nzchar(map$definition)))
  removed <- legacy("removed")
  expect_identical(nrow(removed), 70L)
  expect_false(anyDuplicated(removed$column) > 0L)
  expect_length(intersect(removed$column, map$column), 0L)
})

test_that("the interim master runs on qes_demo through the frozen source map", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  m <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
  expect_identical(nrow(m), 60L)
  expect_length(log$urls, 0L)
  expect_identical(names(m)[seq_along(v044_master_cols)], names(v044_master_cols))
  expect_identical(names(m)[-seq_along(v044_master_cols)], c("vote_choice_timing", "sovereignty_item"))
  types <- vapply(m[seq_along(v044_master_cols)], function(x) class(x)[1], character(1))
  expect_identical(types, v044_master_cols)
  expect_identical(m$respondent_id, sprintf("qes_demo_%d", 1:60))
  # qes2014's blanks apply: interview language (OD8) and truncated ideology
  expect_true(all(is.na(m$language)))
  expect_true(all(is.na(m$ideology)))
  expect_true(all(m$turnout %in% c(0, 1, NA)))
  expect_true(any(!is.na(m$vote_choice)))
  expect_true(all(m$vote_choice_timing == "post"))
  expect_true(all(m$sovereignty_item == "sov_indep"))
  src <- attr(m, "source_map")
  expect_identical(src$source_variable[src$harmonized_variable == "vote_choice"], "Q3")
  expect_identical(attr(m, "qes_provenance")$study, "qes_demo")
  na <- attr(m, "legacy_na_columns")
  expect_identical(names(na), c("column", "study", "reason", "n_cells", "cause", "basis"))
  expect_setequal(na$reason, c("no_source", "blanked"))
  expect_identical(na$cause[na$column == "language"], "OD8")
  expect_identical(attr(m, "qes_spec")$engine, "legacy-interim")
  map <- attr(m, "legacy_column_map")
  expect_identical(names(map), c("column", "target", "definition", "studies_changed", "flag", "note"))
  expect_identical(map$studies_changed[map$column == "party_best"], "all")
  expect_identical(map$flag[map$column == "political_interest"], "approximate")
})

test_that("blanks by code set only the listed codes to NA", {
  local_qes_notices_shown()
  local_fake_dataverse(data = list(
    qes2022 = data.frame(
      ResponseId = c("a", "b", "c"),
      cps_income = c(52000, -99, 0),
      cps_religion = haven::labelled(c(1, -99, 2), labels = c("-99" = -99, "None" = 1, "Catholic" = 2)),
      cps_UserLanguage = c("FR-CA", "EN", "FR-CA"),
      cps_turnout = haven::labelled(c(1, 1, 2), labels = c("Certain to vote" = 1, "Likely" = 2)),
      cps_qc_referendum = haven::labelled(c(1, 2, 1), labels = c("Yes" = 1, "No" = 2))
    ),
    qes2007_panel = data.frame(
      quest = 1:3,
      vote = haven::labelled(c(1, 0, 10), labels = c("n'a pas voté" = 0, "L'ADQ" = 1, "non rejoint" = 10))
    )
  ))
  m <- get_qes_master(surveys = c("qes2022", "qes2007_panel"), quiet = TRUE)
  m22 <- m[m$qes_code == "qes2022", ]
  expect_identical(m22$income, c("52000", NA, "0"))
  expect_identical(m22$religion, c("None", NA, "Catholic"))
  expect_true(all(is.na(m22$language)))
  expect_true(all(is.na(m22$turnout)))
  expect_identical(m22$sovereignty_support, c(1, 0, 1))
  expect_identical(m22$sovereignty, c(1, 0, 1))
  expect_true(all(is.na(m22$vote_choice_timing)))
  m07 <- m[m$qes_code == "qes2007_panel", ]
  expect_identical(m07$vote_choice, c("ADQ", "Did not vote / None", NA))
  expect_true(all(is.na(m07$sovereignty_support)))
  na <- attr(m, "legacy_na_columns")
  pick <- function(col, s) na[na$column == col & na$study == s, , drop = FALSE]
  expect_identical(pick("income", "qes2022")$n_cells, 1L)
  expect_identical(pick("vote_choice", "qes2007_panel")$n_cells, 1L)
  expect_identical(pick("vote_choice", "qes2007_panel")$reason, "blanked")
  expect_identical(pick("citizenship", "qes2007_panel")$reason, "no_source")
  # n_cells counts the values set to NA, and a code rule that blanked
  # nothing is not listed
  expect_identical(pick("religion", "qes2022")$n_cells, 1L)
  expect_identical(nrow(pick("vote_choice_text", "qes2022")), 1L)
  d <- get_decon("qes2022", quiet = TRUE)
  dna <- attr(d, "legacy_na_columns")
  expect_identical(dna$n_cells[dna$column == "income"], 1L)
  expect_false("citizenship" %in% dna$column[dna$reason == "blanked"])
  expect_true(all(dna$n_cells[dna$reason == "blanked" & !dna$column %in% c("party_best", "partylean")] > 0L))
})

test_that("no row is dropped, and every study's row count is its file's", {
  local_qes_notices_shown()
  dup <- data.frame(quest = c(1, 1, 2, 3), nompn = c("674A", "674B", "674A", "674A"), age = c(30, 40, NA, 50))
  local_fake_dataverse(data = list(qes2007_panel = dup))
  m <- get_qes_master(surveys = "qes2007_panel", quiet = TRUE)
  expect_identical(nrow(m), 4L)
  expect_identical(m$respondent_id, c("1", "1", "2", "3"))
  expect_identical(attr(m, "duplicates_removed"), 0L)
  expect_identical(attr(m, "empty_rows_removed"), 0L)
  expect_identical(attr(m, "qes_provenance")$n_rows, 4L)
})

test_that("the notices of the legacy builders are classed and shown once per session", {
  local_fake_dataverse()
  local_qes_once()
  n_values <- count_class(get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_values_changed")
  expect_identical(n_values, 1L)
  expect_identical(
    count_class(get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_values_changed"),
    0L
  )
  expect_identical(
    count_class(get_decon("qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_values_changed"),
    0L
  )
  local_qes_once()
  m <- NULL
  withCallingHandlers(
    get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE),
    message = function(cnd) {
      if (inherits(cnd, "qesR_message_legacy_columns")) m <<- cnd
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(m$id, "legacy_master_columns")
  expect_identical(m$fn, "get_qes_master")
  expect_identical(
    count_class(get_decon("qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_legacy_columns"),
    1L
  )
})
