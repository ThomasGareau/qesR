# The data checks V-D1 to V-D5, V-D7 and V-D8 (design.md section 5.10,
# slice HZ2): on the shipped dictionary with gates.csv (offline), and on data
# frames (qes_spec(data = )), here the synthetic data of .qes_synthetic().

test_that("the shipped spec passes the data checks on the dictionary", {
  s <- hz_spec()
  p <- .qes_data_check(s, .qes_hz_sources_shipped(s))
  expect_identical(p$rule[p$severity %in% c("error", "warning")], character(0))
  # qes_spec() runs them too
  chk <- attr(qes_spec("spec"), "check")
  expect_false(any(grepl("^V-D", chk$rule)))
})

test_that("V-D1 finds variables that do not exist, with a hint", {
  skip_on_cran()
  p <- hz_data_problems(function(t) {
    i <- hz_xw_row(t, "qes2012", "vote_prov_recall")
    t$crosswalk$source_var[i] <- "Q25"
    t$crosswalk$gate_var[i] <- "Q21"
    t
  })
  d1 <- p[p$rule == "V-D1", , drop = FALSE]
  expect_identical(nrow(d1), 2L)
  expect_match(d1$detail[1], "'Q25' is not a variable of qes2012 (did you mean q25", fixed = TRUE)
  # the weight lint: the SPSS donor spells POND, the pinned Stata file pond
  p <- hz_data_problems(function(t) {
    t$weights$weight_var[t$weights$study == "qes2012"] <- "POND"
    t
  })
  expect_identical(p$table[p$rule == "V-D1"], "weights")
  p <- hz_data_problems(function(t) {
    t$waves$member_var[t$waves$study == "qes2018_panel" & t$waves$wave == "post"] <- "repondant"
    t
  })
  expect_true("V-D1" %in% hz_errors(p))
})

test_that("V-D2 finds unmapped codes and missing codes that become values", {
  skip_on_cran()
  p <- hz_data_problems(function(t) {
    vm <- t$valuemaps
    t$valuemaps <- vm[!(vm$map_id == "vote_qes2012_q25" & vm$source_code == "99"), , drop = FALSE]
    t
  })
  expect_match(p$detail[p$rule == "V-D2"], "observed code(s) 99", fixed = TRUE)
  # a numeric row without its na_codes lets 98 and 99 through as unmapped
  p <- hz_data_problems(function(t) {
    t$crosswalk$na_codes[hz_xw_row(t, "qes2012", "lr_self")] <- NA
    t
  })
  expect_match(p$detail[p$rule == "V-D2"], "98, 99", fixed = TRUE)
  # a refusal mapped to a level, and a range wide enough to take 98 and 99
  p <- hz_data_problems(function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2012_q25" & t$valuemaps$source_code == "99")
    t$valuemaps$target_code[i] <- 90L
    t$valuemaps$na_reason[i] <- NA
    t
  })
  expect_match(p$detail[p$rule == "V-D2"], "code(s) 99 are missing codes in the dictionary (refused)", fixed = TRUE)
  p <- hz_data_problems(function(t) {
    i <- hz_xw_row(t, "qes2018", "lr_self")
    t$crosswalk$args[i] <- "min=0;max=99"
    t$crosswalk$na_codes[i] <- NA
    t
  })
  expect_true(any(grepl("sentinel code(s) 98, 99", p$detail[p$rule == "V-D2"], fixed = TRUE)))
})

test_that("V-D3 compares value-map labels and label hashes with the file", {
  skip_on_cran()
  p <- hz_data_problems(function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2014_q3" & t$valuemaps$source_code == "1")
    t$valuemaps$source_label[i] <- "Parti québécois"
    t
  })
  expect_identical(p$key[p$rule == "V-D3"], "vote_qes2014_q3/1")
  # labels compare after normalization (case, accents, apostrophes)
  p <- hz_data_problems(function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2014_q3" & t$valuemaps$source_code == "99")
    t$valuemaps$source_label[i] <- "JE PREFERE NE PAS REPONDRE"
    t
  })
  expect_false("V-D3" %in% p$rule)
  # a hash in place of the label: right, then wrong
  hash_edit <- function(text) function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2014_q3" & t$valuemaps$source_code == "2")
    t$valuemaps$source_label[i] <- NA
    t$valuemaps$source_label_hash[i] <- .qes_md5_text(.qes_norm_label(text))
    t
  }
  expect_false("V-D3" %in% hz_data_problems(hash_edit("Parti québécois"))$rule)
  expect_true("V-D3" %in% hz_data_problems(hash_edit("Parti libéral"))$rule)
  # a label said to come from the file, for a code the file does not label
  p <- hz_data_problems(function(t) {
    vm <- t$valuemaps
    extra <- vm[vm$map_id == "vote_qes2014_q3" & vm$source_code == "99", ]
    extra$source_code <- "97"
    t$valuemaps <- rbind(vm, extra)
    t
  })
  expect_match(p$detail[p$rule == "V-D3"], "the file has none", fixed = TRUE)
})

test_that("V-D4 requires the file's declared missing codes to be named", {
  skip_on_cran()
  p <- hz_data_problems(function(t) {
    t$crosswalk$na_codes[hz_xw_row(t, "qes2018_panel", "lr_self")] <- NA
    t
  })
  expect_match(p$detail[p$rule == "V-D4"], "code(s) 12 are declared missing in the file (12)", fixed = TRUE)
  p <- hz_data_problems(function(t) {
    vm <- t$valuemaps
    t$valuemaps <- vm[!(vm$map_id == "vote_qes2007p_vote" & vm$source_code == "6"), , drop = FALSE]
    t
  })
  expect_true(all(c("V-D2", "V-D4") %in% hz_errors(p)))
})

test_that("V-D7 checks the universe of gated rows against gates.csv", {
  skip_on_cran()
  # a closed gate code with answers
  p <- hz_data_problems(sources_edit = function(src) {
    g <- src$gates
    i <- which(g$study == "qes2012" & g$gate_var %in% "q21" & g$gate_code == "2")
    g$source_code[i] <- "1"
    src$gates <- g
    src
  })
  expect_match(p$detail[p$rule == "V-D7"], "with q21 = 2 (gate closed) have a value of q25", fixed = TRUE)
  # a gated string row (religion, gated on its filter question) is checked too
  p <- hz_data_problems(sources_edit = function(src) {
    g <- src$gates
    i <- which(g$study == "qes2012" & g$gate_var %in% "q102" & g$gate_code == "2")
    g$source_code[i] <- "1"
    src$gates <- g
    src
  })
  expect_identical(p$detail[p$rule == "V-D7"], "842 respondents with q102 = 2 (gate closed) have a value of q103")
  # a gate code the spec leaves open, with no answer and no outcome
  p <- hz_data_problems(function(t) {
    i <- hz_xw_row(t, "qes2012", "vote_prov_recall")
    t$crosswalk$gate_codes[i] <- "2;8"
    t$crosswalk$gate_to[i] <- "2=not_voted;8=dk"
    t
  })
  expect_match(p$detail[p$rule == "V-D7"], "19 respondents with q21 = 9 have no value of q25", fixed = TRUE)
  # no cells, and cells counted under another membership rule
  p <- hz_data_problems(sources_edit = function(src) {
    src$gates <- src$gates[src$gates$study != "qes2018", , drop = FALSE]
    src
  })
  expect_match(p$detail[p$rule == "V-D7"], "gates.csv has no cells", fixed = TRUE)
  p <- hz_data_problems(function(t) {
    t$waves$member_codes[t$waves$study == "qes2018_panel" & t$waves$wave == "post"] <- "1;2"
    t
  })
  expect_match(p$detail[p$rule == "V-D7"], "counted under membership rule 'repondant_post=1'", fixed = TRUE)
})

test_that("V-D8 checks wave sizes against the dictionary and gates.csv", {
  skip_on_cran()
  for (study in c("qes2012", "qes2018_panel", "qes2007_panel")) {
    p <- hz_data_problems(function(t) {
      i <- which(t$waves$study == study)[length(which(t$waves$study == study))]
      t$waves$n_cases[i] <- t$waves$n_cases[i] + 1L
      t
    })
    expect_true("V-D8" %in% hz_errors(p), label = study)
  }
})

test_that("a custom spec without gates.csv loads with warnings, not errors", {
  skip_on_cran()
  dir <- withr::local_tempdir()
  file.copy(list.files(hz_spec_dir(), full.names = TRUE), dir, recursive = TRUE)
  unlink(file.path(dir, "gates.csv"))
  s <- expect_no_error(qes_spec("spec", spec = dir))
  expect_true(s$custom)
  expect_identical(nrow(s$tables$gates), 0L)
  chk <- attr(s, "check")
  expect_true(all(chk$severity[chk$rule %in% c("V-D7", "V-D8")] == "warning"))
  expect_true("V-D7" %in% chk$rule)
})

test_that("synthetic data pass the data checks of the spec", {
  skip_on_cran()
  syn <- .qes_synthetic(c("qes2007_panel", "qes2018_panel", "qes2012"))
  s <- hz_spec()
  p <- .qes_data_check_frames(s, syn)
  expect_identical(p$rule[p$severity == "error"], character(0))
  s2 <- qes_spec("spec", data = syn)
  expect_s3_class(s2, "qes_spec")
})

test_that("the data checks on data frames find each problem (V-D1 to V-D8)", {
  skip_on_cran()
  s <- hz_spec()
  syn <- .qes_synthetic(c("qes2007_panel", "qes2018_panel"))
  rules_on <- function(edit) {
    d <- syn
    d <- edit(d)
    hz_errors(.qes_data_check_frames(s, d))
  }
  # V-D1: a renamed column
  expect_true("V-D1" %in% rules_on(function(d) {
    names(d$qes2018_panel)[names(d$qes2018_panel) == "rts_q2"] <- "RTS_Q2"
    d
  }))
  # V-D2: a code no rule maps
  expect_true("V-D2" %in% rules_on(function(d) {
    x <- d$qes2018_panel$rv1a
    x[1] <- 42
    d$qes2018_panel$rv1a <- x
    d
  }))
  # V-D3: a value label that is not the one quoted
  expect_true("V-D3" %in% rules_on(function(d) {
    x <- d$qes2007_panel$intvote
    labs <- attr(x, "labels")
    names(labs)[labs == 2] <- "PQ"
    attr(x, "labels") <- labs
    d$qes2007_panel$intvote <- x
    d
  }))
  # V-D4: a declared missing code, observed and not named
  expect_true("V-D4" %in% rules_on(function(d) {
    x <- d$qes2018_panel$rts_q7
    x[which(!is.na(x))[1]] <- 9
    attr(x, "qes_na_values") <- 9
    d$qes2018_panel$rts_q7 <- x
    d
  }))
  # V-D5: two rows with one identifier
  expect_true("V-D5" %in% rules_on(function(d) {
    d$qes2007_panel$nompn[2] <- d$qes2007_panel$nompn[1]
    d
  }))
  # V-D7: an answer where the gate is closed
  expect_true("V-D7" %in% rules_on(function(d) {
    closed <- which(unclass(d$qes2018_panel$rts_q1) %in% c(1, 2))[1]
    x <- d$qes2018_panel$rts_q2
    x[closed] <- 1
    d$qes2018_panel$rts_q2 <- x
    d
  }))
  # V-D8: one member too few
  expect_true("V-D8" %in% rules_on(function(d) {
    d$qes2018_panel$repondant_post[which(d$qes2018_panel$repondant_post == 1)[1]] <- 2
    d
  }))
})

test_that("qes_spec(data = ) adds the data problems and checks its argument", {
  skip_on_cran()
  syn <- .qes_synthetic("qes2018_panel")
  names(syn) <- " QES2018_PANEL "
  expect_no_error(qes_spec("spec", data = syn))
  bad <- syn
  names(bad[[1]])[names(bad[[1]]) == "rts_q8"] <- "rts_q8x"
  err <- expect_error(qes_spec("spec", data = bad), class = "qesR_error_spec")
  expect_true("V-D1" %in% err$problems$rule)
  rep <- qes_spec("spec", data = bad, validate = "report")
  expect_true("V-D1" %in% attr(rep, "check")$rule)
  expect_null(attr(qes_spec("spec", data = bad, validate = "none"), "check"))
  expect_error(qes_spec("spec", data = data.frame(a = 1)), class = "qesR_error_input")
  expect_error(qes_spec("spec", data = list(data.frame(a = 1))), class = "qesR_error_input")
  expect_error(qes_spec("spec", data = list(qes2018 = 1)), class = "qesR_error_input")
  expect_error(qes_spec("spec", data = list(qes2019 = data.frame())), class = "qesR_error_unknown_study")
  expect_error(qes_spec("spec", data = list(qes2018 = data.frame(), QES2018 = data.frame())),
               class = "qesR_error_input")
  expect_error(qes_spec("targets", data = syn), class = "qesR_error_input")
  # factors (haven::as_factor()) hold label text, not codes: an input error,
  # not false V-D2 and V-D3 problems
  fac <- .qes_synthetic("qes2007_panel")
  fac$qes2007_panel$intvote <- haven::as_factor(fac$qes2007_panel$intvote)
  err <- expect_error(qes_spec("spec", data = fac), class = "qesR_error_input")
  expect_match(conditionMessage(err), "intvote", fixed = TRUE)
  expect_error(.qes_data_check_frames(hz_spec(), fac), class = "qesR_error_input")
  # a factor column the spec does not name is left alone
  other <- .qes_synthetic("qes2007_panel")
  other$qes2007_panel$unrelated <- factor("a")
  expect_no_error(qes_spec("spec", data = other))
})

test_that("the data checks give the same result under a C locale and French messages", {
  skip_on_cran()
  s <- hz_spec()
  ref <- .qes_data_check(s, .qes_hz_sources_shipped(s))
  syn <- .qes_synthetic("qes2007_panel")
  ref_frames <- .qes_data_check_frames(s, syn)
  withr::local_envvar(LANGUAGE = "fr")
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    expect_identical(.qes_data_check(s, .qes_hz_sources_shipped(s)), ref)
    expect_identical(.qes_data_check_frames(s, syn), ref_frames)
  })
})
