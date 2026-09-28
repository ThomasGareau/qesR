# One crosswalk row at a time (design.md sections 5.6 and 8.1, slice HZ3):
# .qes_hz_apply_row() on small frames, for each rule of the closed
# vocabulary, per-code gates, na_codes, wave membership and user-missing
# codes.

# The shipped spec with crosswalk row `i` of (study, target) edited by `...`
# (column = value), as the only row: .qes_hz_apply_row(spec, 1, ...).
hz_one_row <- function(study, target, ...) {
  s <- hz_spec()
  xw <- s$tables$crosswalk
  i <- which(xw$study == study & xw$target == target & xw$primary %in% TRUE)
  row <- xw[i, , drop = FALSE]
  edits <- list(...)
  for (nm in names(edits)) row[[nm]] <- edits[[nm]]
  s$tables$crosswalk <- row
  s
}

apply1 <- function(spec, d, member = rep(TRUE, nrow(d))) .qes_hz_apply_row(spec, 1L, d, member)

# A one-column frame holding `x` as it is (labelled vectors keep their class).
df1 <- function(name, x) {
  d <- data.frame(.r = seq_along(x))
  d[[name]] <- x
  d
}

test_that("map: codes, per-code gate outcomes and system missing", {
  s <- hz_one_row("qes2018", "vote_prov_recall")
  d <- data.frame(q5 = c(4, 4, 1, 5, 99, NA, 4, 4), q6 = c(1, 95, NA, NA, NA, NA, 96, NA))
  r <- apply1(s, d)
  expect_identical(r$value, c("PLQ", NA, NA, NA, NA, NA, "other", NA))
  expect_identical(r$reason, c(NA, "spoiled", "not_voted", "ineligible", "refused", "inapplicable", NA, "sysmis"))
  expect_identical(r$src, c("1", "95", NA, NA, NA, NA, "96", NA))
  # respondents outside the wave
  r <- apply1(s, d, member = c(TRUE, FALSE, rep(TRUE, 6)))
  expect_identical(r$reason[2], "not_in_wave")
  expect_identical(r$value[1], "PLQ")
})

test_that("map: a gate can give a level, and an unlisted code is unmapped", {
  s <- hz_one_row("qes2014", "sov_indep", gate_var = "G", gate_codes = "0", gate_to = "0=no")
  d <- data.frame(G = c(0, 1, 1, 1), Q19 = c(NA, 1, 8, 5))
  r <- apply1(s, d)
  expect_identical(r$value, c("no", "yes", NA, NA))
  expect_identical(r$reason, c(NA, NA, "dk", "unmapped"))
  # text codes are exact strings
  s <- hz_one_row("qes2014", "sov_indep")
  s$tables$valuemaps <- rbind(s$tables$valuemaps, transform(
    s$tables$valuemaps[s$tables$valuemaps$map_id == "sov_qes2014_q19", ][1, ],
    source_code = "OUI", target_code = 1L
  ))
  r <- apply1(s, data.frame(Q19 = c("OUI", " OUI ", "oui"), stringsAsFactors = FALSE))
  expect_identical(r$value, c("yes", "yes", NA))
  expect_identical(r$reason[3], "unmapped")
})

test_that("numeric: range, na_codes, affine and from_label", {
  s <- hz_one_row("qes2014", "lr_self")
  r <- apply1(s, data.frame(Q32 = c(0, 10, 5, 98, 99, 11, NA)))
  expect_identical(r$value, c("0", "10", "5", NA, NA, NA, NA))
  expect_identical(r$reason, c(NA, NA, NA, "dk", "refused", "unmapped", "sysmis"))
  # affine: codes 1..11 on a 0..10 scale
  s <- hz_one_row("qes2014", "lr_self", args = "min=0;max=10;affine=x-1")
  r <- apply1(s, data.frame(Q32 = c(1, 11, 12, 98)))
  expect_identical(r$value, c("0", "10", NA, NA))
  expect_identical(r$reason, c(NA, NA, "unmapped", "dk"))
  # from_label: the value is the number its label reads as
  s <- hz_one_row("qes2018_panel", "lr_self")
  x <- haven::labelled(c(1, 11, 6, 12, NA), labels = stats::setNames(c(1:11, 12), c(as.character(0:10), "Ne sais pas")))
  r <- apply1(s, df1("rts_q8", x))
  expect_identical(r$value, c("0", "10", "5", NA, NA))
  expect_identical(r$reason, c(NA, NA, NA, "dk_refused", "sysmis"))
  # large whole numbers are never written in scientific notation
  s <- hz_one_row("qes2022", "birth_year", args = "min=0;max=200000")
  r <- apply1(s, data.frame(cps_yob = 1e5))
  expect_identical(r$value, "100000")
})

test_that("SPSS user-missing codes are read as codes, never lost [A:N3]", {
  s <- hz_one_row("qes2007_panel", "vote_prov_recall")
  x <- haven::labelled_spss(c(1, 6, 7, 9, 10, 0), labels = c(ADQ = 1, Autre = 6), na_range = c(6, 10))
  expect_true(is.na(x[2]))
  r <- apply1(s, df1("vote", x))
  expect_identical(r$value, c("ADQ", "other", NA, NA, NA, NA))
  expect_identical(r$reason, c(NA, NA, "spoiled", "refused", "not_in_wave", "not_voted"))
})

test_that("weight, date, string and constant rules", {
  s <- hz_one_row("qes2014", "lr_self", rule = "weight", source_var = "w", args = NA_character_,
                  na_codes = NA_character_)
  r <- apply1(s, data.frame(w = c(0.5, 0, -1, NA, 2)))
  expect_identical(r$value, c("0.5", NA, NA, NA, "2"))
  expect_identical(r$reason, c(NA, "sysmis", "sysmis", "sysmis", NA))

  s <- hz_one_row("qes2014", "lr_self", rule = "date", source_var = "d", args = "format=yyyymmdd",
                  na_codes = NA_character_)
  r <- apply1(s, data.frame(d = c(20070301, 20071399, NA)))
  expect_identical(r$value, c("2007-03-01", NA, NA))
  expect_identical(r$reason, c(NA, "unmapped", "sysmis"))
  s <- hz_one_row("qes2014", "lr_self", rule = "date", source_var = "d", args = "format=stata",
                  na_codes = NA_character_)
  expect_identical(apply1(s, data.frame(d = 17226))$value, format(as.Date(17226, origin = "1960-01-01")))
  s <- hz_one_row("qes2014", "lr_self", rule = "date", source_var = "d", args = "format=posixct",
                  na_codes = NA_character_)
  d <- data.frame(d = as.POSIXct("2022-09-19 23:30:00", tz = "UTC"))
  expect_identical(apply1(s, d)$value, "2022-09-19")

  s <- hz_one_row("qes2014", "lr_self", rule = "string", source_var = "t", args = NA_character_,
                  na_codes = NA_character_)
  r <- apply1(s, data.frame(t = c(" abc ", "", NA), stringsAsFactors = FALSE))
  expect_identical(r$value, c("abc", NA, NA))
  expect_identical(r$reason, c(NA, "sysmis", "sysmis"))

  # a gate on a string row: a closed gate code gives its NA reason in place
  # of system missing, and overrides na_codes (the religion rows of qes2012
  # and qes2014, asked only of those who belong to a religion)
  s <- hz_one_row("qes2012", "religion")
  x <- haven::labelled(c(1, 2, 9, NA, NA, NA), labels = c(Catholic = 1, Protestant = 2, `Prefers not to answer` = 9))
  d <- df1("q103", x)
  d$q102 <- c(1, 1, 1, 2, 3, NA)
  r <- apply1(s, d)
  expect_identical(r$value, c("Catholic", "Protestant", NA, NA, NA, NA))
  expect_identical(r$reason, c(NA, NA, "refused", "inapplicable", "refused", "sysmis"))
  s <- hz_one_row("qes2014", "lr_self", rule = "weight", source_var = "w", args = NA_character_,
                  na_codes = NA_character_, gate_var = "G", gate_codes = "0", gate_to = "0=inapplicable")
  r <- apply1(s, data.frame(w = c(0.5, 0.5, NA), G = c(1, 0, 0)))
  expect_identical(r$value, c("0.5", NA, NA))
  expect_identical(r$reason, c(NA, "inapplicable", "inapplicable"))

  s <- hz_one_row("qes2014", "lr_self", rule = "constant", source_var = "t", args = "value=French",
                  na_codes = NA_character_)
  r <- apply1(s, data.frame(t = 1:3), member = c(TRUE, TRUE, FALSE))
  expect_identical(r$value, c("French", "French", NA))
  expect_identical(r$reason, c(NA, NA, "not_in_wave"))
})

test_that("fn: rules call a registered function, which must give every NA a reason", {
  testthat::local_mocked_bindings(
    .qes_hz_fns = list(
      halve = function(src, ctx) {
        x <- as.numeric(src)
        list(value = ifelse(is.na(x), NA, .qes_code_chr(x / 2)), na_reason = ifelse(is.na(x), "sysmis", NA))
      },
      broken = function(src, ctx) list(value = rep(NA_character_, length(src)), na_reason = rep(NA_character_, length(src)))
    ),
    .package = "qesR"
  )
  s <- hz_one_row("qes2014", "lr_self", rule = "fn:halve", args = NA_character_, na_codes = NA_character_)
  r <- apply1(s, data.frame(Q32 = c(4, NA)))
  expect_identical(r$value, c("2", NA))
  expect_identical(r$reason, c(NA, "sysmis"))
  s <- hz_one_row("qes2014", "lr_self", rule = "fn:broken", args = NA_character_, na_codes = NA_character_)
  expect_error(apply1(s, data.frame(Q32 = 1)), "without a reason")
})

test_that("column hashes depend on every value and reason, in order", {
  a <- .qes_hz_column_md5(c("PLQ", NA), c(NA, "dk"))
  expect_match(a, "^[0-9a-f]{32}$")
  expect_false(identical(a, .qes_hz_column_md5(c("PLQ", NA), c(NA, "refused"))))
  expect_false(identical(a, .qes_hz_column_md5(c(NA, "PLQ"), c("dk", NA))))
  expect_identical(.qes_hz_column_md5(c(1e5, NA), c(NA, "dk")), .qes_hz_column_md5(c("100000", NA), c(NA, "dk")))
})
