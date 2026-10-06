# The legacy harmonization of qesR 0.4.4, ported into the spec format
# (fixtures/spec_legacy/), must fail the checks: each bug that the
# assessment found is caught by a named rule (design.md section 8.1, slice
# HZ2). The fixture replaces the shipped rows of the same study and target,
# adds its value maps and two legacy targets (party_best, born_canada).

legacy_spec <- function() {
  fx <- test_path("fixtures", "spec_legacy")
  s <- hz_spec()
  lx <- .qes_read_csv(file.path(fx, "crosswalk.csv"), "spec_crosswalk")
  lv <- .qes_read_csv(file.path(fx, "valuemaps.csv"), "spec_valuemaps")
  lt <- .qes_read_csv(file.path(fx, "targets.csv"), "spec_targets")
  xw <- s$tables$crosswalk
  xw <- xw[!paste(xw$study, xw$target) %in% paste(lx$study, lx$target), , drop = FALSE]
  s$tables$crosswalk <- rbind(xw, lx)
  vm <- s$tables$valuemaps
  s$tables$valuemaps <- rbind(vm[vm$map_id %in% s$tables$crosswalk$map_id, , drop = FALSE], lv)
  s$tables$targets <- rbind(s$tables$targets, lt)
  s$custom <- TRUE
  s
}

legacy_problems <- function() {
  s <- legacy_spec()
  p <- rbind(.qes_spec_check(s), .qes_data_check(s, .qes_hz_sources_shipped(s)))
  p[p$severity == "error", , drop = FALSE]
}

test_that("the ported 0.4.4 harmonization raises V-S8, V-S9, V-D1, V-D2 and V-D3", {
  skip_on_cran()
  p <- legacy_problems()
  expect_true(all(c("V-S8", "V-S9", "V-D1", "V-D2", "V-D3") %in% p$rule))
  caught <- function(rule, key) any(p$rule == rule & startsWith(p$key, key))
  # [A:H2] [A:H7]: party_best read from "Q8" through a case-insensitive fallback
  expect_true(caught("V-D1", "qes2012/post/party_best/Q8"))
  # [A:H2]: bare party codes of 2018 q8 read as don't know
  expect_true(caught("V-S8", "legacy_party_qes2018_q8/1"))
  expect_true(caught("V-S8", "legacy_party_qes2018_q8/96"))
  # [A:H4]: 2018 born_canada from a yes/no reading of q69 (1 Quebec, 2 rest of
  # Canada, 3 abroad): 3, 98 and 99 left raw, labels that are not the file's
  expect_true(caught("V-D2", "qes2018/post/born_canada/q69"))
  expect_match(p$detail[p$rule == "V-D2" & p$key == "qes2018/post/born_canada/q69"], "3, 98, 99", fixed = TRUE)
  expect_true(caught("V-D3", "legacy_born_qes2018_q69/2"))
  # [A:H4]: 2018 ideology read from q36; the file has q36_1
  expect_true(caught("V-D1", "qes2018/post/lr_self/q36"))
  expect_match(p$detail[p$rule == "V-D1" & p$key == "qes2018/post/lr_self/q36"], "q36_1", fixed = TRUE)
  # [A:H3]: 2014 ideology lost its endpoints 0 and 10
  expect_match(p$detail[p$rule == "V-D2" & p$key == "qes2014/post/lr_self/Q32"], "0, 10", fixed = TRUE)
  # [A:H4]: 2022 vote_choice from the campaign-period intention
  expect_true(caught("V-S9", "qes2022/cps/vote_prov_recall/cps_votechoice1"))
  # [A:N3]: the 2007 panel's declared missing codes 6-10 never mapped
  expect_true(caught("V-D2", "qes2007_panel/post/vote_prov_recall/vote"))
  expect_true(caught("V-D4", "qes2007_panel/post/vote_prov_recall/vote"))
})

test_that("qes_spec() refuses the ported 0.4.4 harmonization", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  s <- legacy_spec()
  for (tab in c("targets", "levels", "crosswalk", "valuemaps", "waves", "weights", "changes")) {
    .qes_write_csv(s$tables[[tab]], file.path(dir, .qes_spec_files[[tab]]))
  }
  file.copy(file.path(hz_spec_dir(), "SPEC"), dir)
  err <- expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
  expect_true(all(c("V-S8", "V-S9", "V-D1", "V-D2", "V-D3") %in% err$problems$rule))
})
