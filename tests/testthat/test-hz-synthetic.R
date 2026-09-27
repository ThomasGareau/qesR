# .qes_synthetic(): data built from the spec, shaped like get_qes() output,
# with every code and gate outcome and no respondent (design.md section 8.1,
# slice HZ2).

test_that("synthetic data have the spec's variables, wave sizes and codes", {
  s <- hz_spec()
  syn <- .qes_synthetic(c("qes2007_panel", "qes2018_panel"), spec = s)
  expect_named(syn, c("qes2007_panel", "qes2018_panel"))
  p7 <- syn$qes2007_panel
  expect_s3_class(p7, "data.frame")
  expect_identical(attr(p7, "qes_survey_code"), "qes2007_panel")
  expect_true(isTRUE(attr(p7, "qes_synthetic")))
  # wave sizes are n_cases: 2,050 pre-wave and 2,054 post-wave members, with
  # members of one wave only on both sides
  expect_identical(sum(p7$resultat == "C"), 2050L)
  expect_identical(sum(p7$resultat_pst == "CO"), 2054L)
  expect_true(any(p7$resultat == "C" & p7$resultat_pst != "CO"))
  expect_true(any(p7$resultat != "C" & p7$resultat_pst == "CO"))
  # identifiers are unique; every source and gate of the spec is a column
  expect_false(anyDuplicated(paste(p7$nompn, p7$quest)) > 0L)
  xw <- s$tables$crosswalk
  for (study in names(syn)) {
    vars <- unique(stats::na.omit(c(xw$source_var[xw$study == study], xw$gate_var[xw$study == study])))
    expect_true(all(vars %in% names(syn[[study]])), label = study)
  }
  # every code of every value map occurs among the wave's members
  vm <- s$tables$valuemaps
  for (i in which(xw$study %in% names(syn) & xw$rule %in% "map")) {
    d <- syn[[xw$study[i]]]
    codes <- .canon(d[[xw$source_var[i]]])
    expect_true(all(vm$source_code[vm$map_id == xw$map_id[i]] %in% codes), label = xw$source_var[i])
  }
  # labels are the ones the value map quotes from the file
  labs <- attr(p7$intvote, "labels")
  expect_identical(names(labs)[labs == 9], "NSP/refus")
  # weights have mean 1 on the wave's members
  w <- syn$qes2018_panel$weight_rts
  expect_equal(mean(w[syn$qes2018_panel$repondant_post == 1]), 1)
  expect_true(all(is.na(w[syn$qes2018_panel$repondant_post != 1])))
})

test_that("a gated synthetic source is missing exactly where its gate is closed", {
  syn <- .qes_synthetic(c("qes2018", "qes2018_panel"))
  q <- syn$qes2018
  expect_identical(is.na(q$q6), !(q$q5 %in% 4))
  expect_true(all(c(1, 2, 3, 5, 99) %in% q$q5) && anyNA(q$q5))
  p <- syn$qes2018_panel
  post <- p$repondant_post == 1
  expect_identical(is.na(p$rts_q2[post]), unclass(p$rts_q1)[post] %in% c(1, 2, 4))
  # the qes2018 file has no value labels; its synthetic twin has none either
  expect_null(attr(q$q6, "labels"))
  # from_label rows get labels that read as numbers in the target's range
  labs <- attr(p$rts_q8, "labels")
  expect_true(all(as.numeric(names(labs)) >= 0 & as.numeric(names(labs)) <= 10))
})

test_that("synthetic data project and check like real data", {
  s <- hz_spec()
  syn <- .qes_synthetic(c("qes2012", "qes2014", "qes2012_panel"), spec = s)
  src <- .qes_hz_sources_data(s, syn)
  proj <- .qes_project_marginals(s, src)
  expect_null(attr(proj, "unprojected"))
  # no code is left unmapped, every NA has a reason, and each row counts the wave
  expect_false(any(proj$na_reason %in% "unmapped"))
  expect_true(all(!is.na(proj$value) | !is.na(proj$na_reason)))
  tot <- tapply(proj$n, paste(proj$study, proj$target), sum)
  expect_true(all(tot == c(qes2012 = 1505L, qes2014 = 1517L, qes2012_panel = 844L)[sub(" .*$", "", names(tot))]))
  expect_identical(hz_errors(.qes_data_check_frames(s, syn)), character(0))
})

test_that("synthetic weights have mean 1 when a wave has an odd number of members", {
  s <- hz_spec()
  syn <- .qes_synthetic(c("qes2012", "qes2014"), spec = s)
  expect_identical(nrow(syn$qes2012) %% 2L, 1L)
  expect_equal(mean(syn$qes2012$pond), 1)
  expect_equal(mean(syn$qes2014$POND), 1)
})

test_that("synthetic codes of an affine numeric row stay within its range", {
  s <- hz_spec()
  i <- hz_xw_row(s$tables, "qes2014", "lr_self")
  # 3 * x within 0..11: codes 0..3 only (4 would give 12)
  s$tables$crosswalk$args[i] <- "min=0;max=11;affine=3*x"
  syn <- .qes_synthetic("qes2014", spec = s)
  x <- unclass(syn$qes2014$Q32)
  codes <- setdiff(unique(x[!is.na(x)]), c(98, 99))
  expect_true(all(3 * codes >= 0 & 3 * codes <= 11))
  expect_true(all(c(0, 3) %in% codes))
  # a negative slope: -2 * x + 10 within 0..10 needs codes 0..5
  s$tables$crosswalk$args[i] <- "min=0;max=10;affine=-2*x+10"
  x <- unclass(.qes_synthetic("qes2014", spec = s)$qes2014$Q32)
  codes <- setdiff(unique(x[!is.na(x)]), c(98, 99))
  expect_true(all(-2 * codes + 10 >= 0 & -2 * codes + 10 <= 10))
  expect_true(all(c(0, 5) %in% codes))
  # and the data pass their own checks
  expect_false("V-D2" %in% hz_errors(.qes_data_check_frames(s, list(qes2014 = .qes_synthetic("qes2014", spec = s)$qes2014))))
})

test_that("synthetic data do not depend on the locale", {
  ref <- .qes_synthetic("qes2014")
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    expect_identical(.qes_synthetic("qes2014"), ref)
  })
})
