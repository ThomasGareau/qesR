# Tier 1 (live) for the legacy switch (design.md section 5.12, slice HZ6):
# get_qes_master() and get_decon() rendered from the engine on the pinned
# originals. The full comparison with qesR 0.4.4 and 0.5.0, cell by cell,
# is data-raw/compare_legacy.R; these are its headline counts (CC0 studies).
# Never runs on CRAN; see helper-live.R.

test_that("the engine-rendered master keeps every row and the 0.4.4 schema (live)", {
  local_live_originals()
  local_qes_notices_shown()
  m <- get_qes_master(quiet = TRUE)
  expect_identical(nrow(m), 40987L)
  expect_identical(attr(m, "failed_surveys"), character(0))
  expect_setequal(attr(m, "loaded_surveys"), v044_qescodes)
  n <- length(v044_master_cols)
  expect_identical(names(m)[seq_len(n)], names(v044_master_cols))
  expect_identical(vapply(m[seq_len(n)], function(x) class(x)[1], character(1)), v044_master_cols)
  prov <- attr(m, "qes_provenance")
  expect_true(all(prov$md5_verified))
  expect_identical(as.vector(table(m$qes_code)[prov$study]), prov$n_rows)
  by <- function(s, col) m[[col]][m$qes_code == s]
  # the reported vote (OD4): qes2012 as in 0.5.0 without the don't-know category
  expect_identical(
    as.vector(table(factor(by("qes2012", "vote_choice"), c("PLQ", "PQ", "CAQ", "QS", "PVQ", "ON", "Other party")))),
    c(278L, 509L, 324L, 96L, 13L, 39L, 15L)
  )
  # qes1998's rows are signed off (spec 4.1.0): the reported vote of its
  # recontact; its recommended weight still needs review, so weight_pre and
  # weight_post are NA, with the reason, and survey_weight is ponderc
  expect_true(any(!is.na(by("qes1998", "vote_choice"))))
  na <- attr(m, "legacy_na_columns")
  expect_false(any(na$study == "qes1998" & na$cause %in% "not_signed_off"))
  for (col in c("weight_pre", "weight_post")) {
    expect_true(all(is.na(by("qes1998", col))), info = col)
    expect_identical(na$reason[na$column == col & na$study == "qes1998"], "not_reviewed", info = col)
    expect_identical(na$cause[na$column == col & na$study == "qes1998"], "weight_needs_review", info = col)
    expect_match(na$basis[na$column == col & na$study == "qes1998"], "ponder3", fixed = TRUE)
  }
  expect_false(all(is.na(by("qes1998", "survey_weight"))))
  expect_true(all(is.na(by("qes_crop_2007_2010", "vote_choice"))))
  # columns filled from the spec
  expect_identical(sum(!is.na(by("qes2018", "ideology"))), 2490L)
  expect_identical(sum(!is.na(by("qes2014", "ideology"))), 1195L)
  expect_identical(sum(!is.na(by("qes2018", "political_interest"))), 3012L)
  expect_true(all(by("qes2018", "political_interest") %in% c(NA, 0, 3, 7, 10)))
  expect_identical(sum(!is.na(by("qes2014", "language"))), 1402L)
  expect_identical(sum(!is.na(by("qes2018", "born_canada"))), 3047L)
  # identifiers joined from the files' id variables, unique in the 2007 panel
  expect_false(anyDuplicated(by("qes2007_panel", "respondent_id")) > 0L)
  expect_true(all(m$province_territory == "Quebec"))
})

test_that("get_decon() of the originals: reported vote, and the qes2022 campaign items (live)", {
  local_live_originals()
  local_qes_notices_shown()
  d <- get_decon("qes2018", quiet = TRUE)
  expect_identical(sum(!is.na(d$turnout)), 2639L)
  expect_identical(attr(d, "timing")[["votechoice"]], "post")
  d22 <- get_decon("qes2022", quiet = TRUE)
  expect_identical(attr(d22, "timing")[["votechoice"]], "pre")
  expect_true(is.numeric(d22$income))
  # -99 (not answered) and 0 (a blank amount, sent to the follow-up) are NA
  expect_false(any(d22$income %in% c(-99, 0)))
  d98 <- get_decon("qes1998", quiet = TRUE)
  expect_true(any(!is.na(d98$votechoice)))
  expect_false("not_reviewed" %in% attr(d98, "legacy_na_columns")$reason)
})
