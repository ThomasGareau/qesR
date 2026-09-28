# Tier 1 and 2 (live) validation against official results and the census
# (design.md sections 5.10, 8.2 and 8.3, slice HZ7): V-L2, the turnout
# over-report, the census margins and V-L4 on the pinned originals, and V-L5
# against Dataverse. V-L1 and V-L3 are in test-hz-engine-live.R. Never runs on
# CRAN; see helper-live.R. The weekly job is .github/workflows/live.yml.
#
# The recorded values (baselines) are inst/extdata/validation/
# validation_report.csv, written by data-raw/build_validation.R, and, for
# qes2022, whose aggregates do not ship (OD3), the build-ignored
# data-raw/nc/validation_qes2022.csv: its rows are gated only when the tests
# run from the source tree. The gate is one-sided: a gated row fails only
# when it rises more than 2.0 points above its recorded value. The official
# results and the recorded report are not in the package build
# (inst/COPYRIGHTS, section 3): the first test needs the source tree.

recorded_with_nc <- function() {
  nc <- testthat::test_path("..", "..", "data-raw", "nc", "validation_qes2022.csv")
  extra <- if (file.exists(nc)) .qes_read_csv(nc, "validation_report") else NULL
  .qes_validation_recorded(extra)
}

describe_rows <- function(x) {
  paste(sprintf("%s %s %s (%s): %.2f, recorded %.2f", x$study, x$check, x$variable, x$weight,
                x$value, x$baseline), collapse = "; ")
}

test_that("V-L2, turnout and census margins stay within 2.0 points of the record (live)", {
  local_live_originals()
  skip_if_not(.qes_validation_has("official_results.csv") && .qes_validation_has("validation_report.csv"),
              "the official results and the recorded report are not installed (build-ignored)")
  r <- .qes_validation_run("all")
  rec <- recorded_with_nc()
  g <- .qes_validation_gate(r, rec)
  gated <- r$status %in% "gate"
  # studies whose record is not available here (qes2022 outside the source
  # tree) are not gated
  missing_record <- gated & g$status %in% "new" & !(g$study %in% rec$study)
  fails <- g[gated & !missing_record & !(g$status %in% "pass"), ]
  expect(nrow(fails) == 0L, paste("above the recorded value + 2.0 points, or not recorded (run data-raw/build_validation.R):",
                                  describe_rows(fails)))
  # V-L2 runs on every study with a reviewed weight; 1998 (francophones
  # only) is skipped
  v <- g[g$check == "recall" & is.na(g$level), ]
  expect_true(all(c("qes2012", "qes2014", "qes2018", "qes2022") %in% v$study[v$weight != "none"]))
  expect_identical(unique(v$status[v$study == "qes1998"]), "skipped")
  # turnout is over-reported, by less than 35 points (information)
  t <- g[g$check == "turnout" & !(g$status %in% "skipped"), ]
  expect(all(t$value > 0 & t$value < 35), paste("turnout over-report outside 0-35 points:",
                                                paste(t$study[!(t$value > 0 & t$value < 35)], collapse = ", ")))
  # the census rows with weights calibrated on that margin come close to it
  # (qes2022: gender, age and education, 2021 Census; qes2018 gender)
  cz <- g[g$check == "census" & is.na(g$level) & g$weight != "none", ]
  expect_lt(cz$value[cz$study == "qes2022" & cz$variable == "gender"], 0.5)
  expect_lt(cz$value[cz$study == "qes2022" & cz$variable == "age_group6"], 2)
  expect_lt(cz$value[cz$study == "qes2018" & cz$variable == "gender"], 0.5)
  # A value that moves within the gate (or falls) passes here; a stale
  # baseline is reported, not failed, by data-raw/build_validation.R --check
  # in the weekly job, and recording it again is gated by that script.
})

test_that("V-L4: construct validity directions (live)", {
  local_live_originals()
  r <- .qes_validation_run(c("qes2012", "qes2014", "qes2018", "qes2022"))
  k <- r[r$check == "construct", ]
  bad <- k[k$status %in% "fail", ]
  expect(nrow(bad) == 0L, paste("construct directions fail:", paste(bad$study, bad$variable, collapse = "; ")))
  # every direction is checked in the studies that have its questions
  has <- function(variable) sort(k$study[k$variable == variable & k$status %in% "pass"])
  expect_identical(has("interest_4pt"), c("qes2012", "qes2014", "qes2018"))
  expect_identical(has("lr_self"), c("qes2012", "qes2014", "qes2018", "qes2022"))
  expect_identical(has("sov_indep"), c("qes2012", "qes2014", "qes2018", "qes2022"))
  expect_identical(has("pid_prov"), c("qes2012", "qes2014", "qes2018", "qes2022"))
})

test_that("V-L5: the pinned deposits have not changed on Dataverse (live, network)", {
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("QESR_LIVE"), "true"), "set QESR_LIVE=true to ask Dataverse")
  skip_if_offline()
  st <- qes_studies(check_updates = TRUE, quiet = TRUE)
  drift <- st[!(st$status %in% "current"), c("study", "dataset_version", "latest_version", "status")]
  expect(nrow(drift) == 0L, paste(
    "deposits differ from the catalog pins:",
    paste(sprintf("%s (pinned %s, latest %s: %s)", drift$study, drift$dataset_version,
                  drift$latest_version, drift$status), collapse = "; ")
  ))
})
