# The validation benchmarks and the recorded validation report (design.md
# sections 5.10 and 8.3, slice HZ7; R/validation.R), offline: the shipped
# tables are consistent, the recorded report ships no qes2022 aggregate
# (OD3), and the report and its gate work on the synthetic qes_demo. The
# checks on the pinned files are in test-validation-live.R.
#
# The official results of Elections Quebec and the recorded report are in the
# source tree but not in the package build (inst/COPYRIGHTS, section 3): the
# tests that read them run on the source tree only, and the report skips the
# recall and turnout checks without them.

skip_if_no_official <- function() {
  testthat::skip_if_not(
    .qes_validation_has("official_results.csv") && .qes_validation_has("official_turnout.csv"),
    "the official results are not installed (build-ignored)"
  )
}

test_that("the official results add up and name their source", {
  skip_if_no_official()
  b <- .qes_validation_benchmarks()
  r <- b$results
  expect_false(anyDuplicated(paste(r$election_id, r$party)) > 0L)
  expect_true(all(r$party %in% c(.qes_spec_levels(.qes_spec_get()$tables$levels, "party_qc")$name)))
  el <- .qes_catalog()$elections
  expect_true(all(r$election_id %in% el$election_id))
  for (e in unique(r$election_id)) {
    x <- r[r$election_id == e, ]
    expect_identical(sum(x$votes), unique(x$votes_valid), label = e)
    expect_equal(sum(x$share_valid), 100, tolerance = 1e-5, label = e)
    expect_equal(x$share_valid, round(100 * x$votes / x$votes_valid, 4), label = e)
    expect_identical(unique(x$source_url), el$source_url[el$election_id == e], label = e)
  }
  t <- b$turnout
  expect_setequal(t$election_id, unique(r$election_id))
  expect_equal(t$turnout, round(100 * t$ballots_cast / t$registered, 4))
  expect_equal(t$ballots_valid + t$ballots_rejected, t$ballots_cast)
  expect_identical(t$ballots_valid, as.numeric(tapply(r$votes, r$election_id, sum)[t$election_id]))
  # every election a study reports the vote of has results
  xw <- .qes_spec_get()$tables$crosswalk
  refs <- unique(xw$election_ref[xw$target %in% c("vote_prov_recall", "turnout_prov_recall")])
  expect_true(all(refs[!is.na(refs)] %in% t$election_id))
  # figures checked against the published results pages (2018: CAQ 37.42,
  # turnout 66.45; 2022: CAQ 40.98, turnout 66.15)
  expect_identical(r$share_valid[r$election_id == "QC2018" & r$party == "CAQ"], 37.4226)
  expect_identical(t$turnout[t$election_id == "QC2022"], 66.1475)
})

test_that("the census margins are distributions with a documented age cut", {
  cen <- .qes_validation_benchmarks()$census
  expect_true(all(cen$count > 0))
  key <- paste(cen$census_year, cen$variable)
  for (k in unique(key)) {
    x <- cen[key == k, ]
    expect_equal(sum(x$share), 100, tolerance = 1e-3, label = k)
    expect_equal(x$share, round(100 * x$count / sum(x$count), 4), label = k)
    expect_length(unique(x$universe), 1L)
  }
  expect_setequal(unique(cen$variable), c("gender", "age_group6", "lang_mother", "education"))
  expect_true(all(cen$universe %in% c("18+", "20+", "25+")))
  expect_true(all(grepl("^https://www150\\.statcan\\.gc\\.ca/t1/tbl1/en/tv\\.action\\?pid=[0-9]{10}$", cen$source_url)))
  # levels are the harmonized levels (education: the census's two groups)
  lv <- .qes_spec_get()$tables$levels
  expect_true(all(cen$level[cen$variable == "gender"] %in% c("man", "woman")))
  expect_true(all(cen$level[cen$variable == "lang_mother"] %in% .qes_spec_levels(lv, "lang3")$name))
  expect_true(all(cen$level[cen$variable == "age_group6"] %in% .qes_spec_levels(lv, "age6")$name))
  expect_setequal(cen$level[cen$variable == "education"], c("below_university", "university"))
  # 18+ where single years are published (2016, 2021), the nearest cut before
  expect_identical(unique(cen$universe[cen$variable == "gender" & cen$census_year >= 2016]), "18+")
  expect_identical(unique(cen$universe[cen$variable == "age_group6" & cen$census_year < 2016]), "25+")
  expect_identical(sum(cen$count[cen$census_year == 2021 & cen$variable == "gender"]), 6850675)
})

test_that("the recorded report ships no qes2022 aggregate and matches the benchmarks", {
  skip_if_no_official()
  testthat::skip_if_not(.qes_validation_has("validation_report.csv"), "the recorded report is not installed (build-ignored)")
  rec <- .qes_validation_recorded()
  expect_identical(names(rec), .qes_val_columns)
  shipped <- .qes_catalog()$studies
  shipped <- shipped$study[shipped$metadata_shipped %in% TRUE]
  expect_true(all(rec$study %in% shipped))
  expect_false("qes2022" %in% rec$study)
  expect_true(all(rec$status %in% c(NA, "info", "gate", "pass", "fail", "skipped")))
  expect_true(all(rec$check %in% c("recall", "turnout", "census", "construct")))
  # one V-L2 index per study with a reviewed weight, and the baselines of
  # design.md section 8.3 confirmed on the official figures (R3)
  v <- rec[rec$check == "recall" & is.na(rec$level) & rec$status %in% "gate", ]
  expect_setequal(v$study, c("qes2012", "qes2014", "qes2018"))
  expect_equal(v$value[match(c("qes2012", "qes2014", "qes2018"), v$study)], c(8.0, 5.6, 3.2), tolerance = 0.06)
  # 1998 covers francophones only: skipped
  expect_identical(unique(rec$status[rec$study == "qes1998" & is.na(rec$level)]), "skipped")
  # the benchmark column is the official share in the study's categories
  res <- .qes_validation_benchmarks()$results
  lv <- rec[rec$check == "recall" & !is.na(rec$level) & rec$level != "other", ]
  off <- res$share_valid[match(paste(lv$reference, lv$level), paste(res$election_id, res$party))]
  expect_equal(lv$benchmark, round(off, 2), tolerance = 0.006)
  # each index is half the sum of its level differences
  for (k in which(rec$check %in% c("recall", "census") & is.na(rec$level) & !is.na(rec$value))) {
    parts <- rec[rec$study == rec$study[k] & rec$check == rec$check[k] & rec$variable == rec$variable[k] &
                   rec$weight == rec$weight[k] & !is.na(rec$level), ]
    expect_equal(rec$value[k], 0.5 * sum(abs(parts$estimate - parts$benchmark)), tolerance = 0.03,
                 label = paste(rec$study[k], rec$variable[k]))
  }
  # no gated row fails its own record; turnout over-reporting within 0-35
  expect_false(any(rec$status %in% "fail"))
  t <- rec[rec$check == "turnout" & !(rec$status %in% "skipped"), ]
  expect_true(all(t$value > 0 & t$value < 35))
})

test_that("without the official results, recall and turnout are skipped and the record is empty", {
  local_mocked_bindings(.qes_validation_has = function(file) file == "census_margins.csv")
  b <- .qes_validation_benchmarks()
  expect_null(b$results)
  expect_null(b$turnout)
  expect_gt(nrow(b$census), 0L)
  rec <- .qes_validation_recorded()
  expect_identical(names(rec), .qes_val_columns)
  expect_identical(nrow(rec), 0L)
  r <- .qes_validation_run("qes_demo")
  expect_identical(names(r), .qes_val_columns)
  off <- r[r$check %in% c("recall", "turnout"), ]
  expect_identical(nrow(off), 2L)
  expect_identical(off$status, c("skipped", "skipped"))
  expect_true(all(is.na(off$benchmark)))
  expect_gt(sum(r$check == "census"), 0L)
})

test_that("the report runs offline on qes_demo and the gate compares with the record", {
  skip_if_no_official()
  r <- .qes_validation_run("qes_demo")
  expect_identical(names(r), .qes_val_columns)
  expect_setequal(unique(r$check), c("recall", "turnout", "census", "construct"))
  rc <- r[r$check == "recall", ]
  expect_identical(unique(rc$reference), "QC2014")
  # the parties no respondent could name are counted as "other" on the
  # official side, and the official shares still sum to 100
  expect_equal(sum(rc$benchmark[rc$weight == "none" & !is.na(rc$level)]), 100, tolerance = 0.02)
  idx <- rc[is.na(rc$level), ]
  expect_identical(idx$status, c("info", "gate"))
  lv <- rc[!is.na(rc$level) & rc$weight == "none", ]
  expect_equal(idx$value[1], 0.5 * sum(abs(lv$estimate - lv$benchmark)), tolerance = 0.02)
  expect_identical(sum(lv$n), idx$n[1])
  # the census before the 2014 election is 2011: gender 20+, ages 25+
  cz <- r[r$check == "census", ]
  expect_identical(unique(cz$reference), "Census 2011")
  expect_identical(unique(cz$universe[cz$variable == "age_group6"]), "25+")
  expect_false("a18_24" %in% cz$level)

  # the gate: a recorded value + 2.0 points passes, more fails, none is new
  g <- r[r$status %in% "gate", ]
  rec <- g
  rec$value <- rec$value - c(1, 3)[seq_len(nrow(rec)) %% 2 + 1]
  out <- .qes_validation_gate(r, rec)
  s <- out[r$status %in% "gate", ]
  expect_identical(nrow(s), nrow(g))
  expect_identical(s$status, ifelse(s$value - s$baseline <= 2, "pass", "fail"))
  expect_equal(s$max_allowed, s$baseline + 2)
  expect_identical(unique(.qes_validation_gate(r, rec[0, ])$status[r$status %in% "gate"]), "new")
})

test_that("validation reads only shipped tables and makes no request", {
  local_mocked_bindings(.qes_transport = function(...) stop("no request expected"))
  expect_no_error(.qes_validation_run("qes_demo"))
})
