# The harmonization spec and its validator (design.md sections 5 and 5.10,
# slice HZ1): V-S1 to V-S17, the SPEC version and content hash (V-P2), and
# regressions on real codes.

spec_dir <- function() system.file("extdata", "harmonize", package = "qesR", mustWork = TRUE)

shipped_spec <- function() {
  s <- .qes_spec_load(normalizePath(spec_dir(), winslash = "/"))
  s$custom <- FALSE
  s
}

# The rules raised by the validator after `edit` changes the shipped tables.
rules_after <- function(edit, release = FALSE) {
  s <- shipped_spec()
  s$tables <- edit(s$tables)
  p <- .qes_spec_check(s, release = release)
  unique(p$rule[p$severity == "error"])
}

# A copy of the shipped spec directory in a temporary folder.
local_spec_copy <- function(env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = env)
  file.copy(list.files(spec_dir(), full.names = TRUE), dir)
  dir
}

xw_row <- function(t, study, target, source_var = NULL) {
  i <- which(t$crosswalk$study == study & t$crosswalk$target == target)
  if (!is.null(source_var)) i <- i[t$crosswalk$source_var[i] == source_var]
  i
}

test_that("the shipped spec passes V-S1 to V-S17 and the hash check", {
  s <- shipped_spec()
  p <- .qes_spec_check(s)
  expect_identical(p$rule[p$severity == "error"], character(0))
  expect_identical(p$rule[p$severity == "warning"], character(0))
  # release rules: no draft rows, no stable row on a weight that needs review
  p <- .qes_spec_check(s, release = TRUE)
  expect_identical(p$rule[p$severity == "error"], character(0))
})

test_that("SPEC records the version, the schema and the content hash (V-P2)", {
  meta <- read.dcf(file.path(spec_dir(), "SPEC"))
  expect_setequal(colnames(meta), c("Spec-Version", "Spec-Date", "Schema-Version", "Engine-Min", "Hash", "Licence"))
  expect_match(meta[1, "Spec-Version"], "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  expect_identical(unname(meta[1, "Schema-Version"]), .qes_spec_schema_version)
  expect_true(package_version(meta[1, "Engine-Min"]) <= utils::packageVersion("qesR"))
  # the recorded hash is the hash of the content: a spec edit without a new
  # hash (and so without a version bump) fails here
  expect_identical(unname(meta[1, "Hash"]), .qes_spec_hash(spec_dir()))
  changes <- .qes_read_csv(file.path(spec_dir(), "CHANGES.csv"), "spec_changes")
  expect_true(meta[1, "Spec-Version"] %in% changes$spec_version)
  expect_true(all(package_version(changes$spec_version) <= package_version(meta[1, "Spec-Version"])))
})

test_that("the content hash ignores quoting and follows content", {
  dir <- local_spec_copy()
  h <- .qes_spec_hash(dir)
  expect_identical(h, .qes_spec_hash(spec_dir()))
  # quoting every field changes the bytes, not the content
  x <- .qes_read_csv(file.path(dir, "weights.csv"))
  .qes_write_csv(x, file.path(dir, "weights.csv"))
  expect_identical(.qes_spec_hash(dir), h)
  x$source_ref[1] <- paste(x$source_ref[1], "(edited)")
  .qes_write_csv(x, file.path(dir, "weights.csv"))
  expect_false(identical(.qes_spec_hash(dir), h))
})

test_that("qes_spec(view = 'spec') returns the checked spec", {
  s <- qes_spec("spec")
  expect_s3_class(s, "qes_spec")
  expect_identical(s$version, unname(read.dcf(file.path(spec_dir(), "SPEC"))[1, "Spec-Version"]))
  expect_false(s$custom)
  expect_setequal(names(s$tables), c("targets", "levels", "crosswalk", "valuemaps", "waves", "weights", "changes"))
  chk <- attr(s, "check")
  expect_s3_class(chk, "data.frame")
  expect_identical(names(chk), c("rule", "severity", "table", "row", "key", "detail"))
  expect_false(any(chk$severity == "error"))
  expect_null(attr(qes_spec("spec", validate = "none"), "check"))
  expect_output(print(s), "qesR harmonization spec")
  # a qes_spec object is accepted as `spec` and checked again
  expect_identical(attr(qes_spec("spec", spec = s), "check"), chk)
})

test_that("an unchanged copy of the spec is not custom; an edited one is", {
  dir <- local_spec_copy()
  expect_false(qes_spec("spec", spec = dir)$custom)
  x <- .qes_read_csv(file.path(dir, "CHANGES.csv"))
  x$change_en[1] <- paste(x$change_en[1], "Edited.")
  .qes_write_csv(x, file.path(dir, "CHANGES.csv"))
  s <- qes_spec("spec", spec = dir, validate = "report")
  expect_true(s$custom)
  # a stale Hash in an edited copy is a warning, not an error
  chk <- attr(s, "check")
  expect_identical(chk$severity[chk$rule == "V-P2"], "warning")
  expect_no_error(qes_spec("spec", spec = dir))
})

test_that("a broken spec raises qesR_error_spec with the problems table", {
  dir <- local_spec_copy()
  x <- .qes_read_csv(file.path(dir, "crosswalk.csv"))
  x$grade[1] <- "same"
  .qes_write_csv(x, file.path(dir, "crosswalk.csv"))
  err <- expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
  expect_true("V-S3" %in% err$problems$rule)
  expect_identical(err$id, "spec_invalid")
  rep <- qes_spec("spec", spec = dir, validate = "report")
  expect_true("V-S3" %in% attr(rep, "check")$rule)
})

test_that("files, schema version, engine version and column types are enforced", {
  dir <- local_spec_copy()
  unlink(file.path(dir, "levels.csv"))
  expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")

  dir <- local_spec_copy()
  spec_file <- file.path(dir, "SPEC")
  writeLines(sub("^Schema-Version: .*$", "Schema-Version: 99", readLines(spec_file)), spec_file)
  err <- expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
  expect_identical(err$id, "spec_schema")

  dir <- local_spec_copy()
  spec_file <- file.path(dir, "SPEC")
  writeLines(sub("^Engine-Min: .*$", "Engine-Min: 99.0.0", readLines(spec_file)), spec_file)
  err <- expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
  expect_identical(err$id, "spec_engine")

  dir <- local_spec_copy()
  x <- .qes_read_csv(file.path(dir, "waves.csv"))
  x$n_cases[1] <- "many"
  .qes_write_csv(x, file.path(dir, "waves.csv"))
  err <- expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
  expect_identical(err$problems$rule, "V-S1")

  dir <- local_spec_copy()
  x <- .qes_read_csv(file.path(dir, "targets.csv"))
  x$extra <- ""
  .qes_write_csv(x, file.path(dir, "targets.csv"))
  expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
})

test_that("qes_spec() checks its arguments", {
  expect_error(qes_spec("spec", spec = tempfile()), class = "qesR_error_input")
  expect_error(qes_spec("spec", spec = 1), class = "qesR_error_input")
  expect_error(qes_spec("tables"), class = "qesR_error_input")
  expect_error(qes_spec("spec", level = "code"), class = "qesR_error_input")
  expect_error(qes_spec("spec", format = "retroharmonize"), class = "qesR_error_input")
  expect_error(qes_spec("spec", targets = "lr_self"), class = "qesR_error_input")
  expect_error(qes_spec("spec", studies = "qes2018"), class = "qesR_error_input")
  expect_error(qes_spec("targets", data = list()), class = "qesR_error_input")
  expect_error(qes_spec("spec", validate = "maybe"), class = "qesR_error_input")
  expect_error(qes_spec("spec", lang = "de"), class = "qesR_error_input")
  # the targets and crosswalk views and the data checks come with the engine
  expect_error(qes_spec(), class = "qesR_error_input")
  expect_error(qes_spec("crosswalk"), class = "qesR_error_input")
  expect_error(qes_spec("spec", data = list(qes2018 = data.frame())), class = "qesR_error_input")
})

test_that("V-S1 catches malformed cells", {
  expect_true("V-S1" %in% rules_after(function(t) {
    t$crosswalk$args[xw_row(t, "qes2012", "lr_self")] <- "min=0;max"
    t
  }))
  expect_true("V-S1" %in% rules_after(function(t) {
    t$crosswalk$args[xw_row(t, "qes2012", "lr_self")] <- "min=0;max=10;affine=exp(x)"
    t
  }))
  expect_true("V-S1" %in% rules_after(function(t) {
    t$crosswalk$gate_to[xw_row(t, "qes2012", "vote_prov_recall")] <- "2=not_voted;9=refused"
    t
  }))
  expect_true("V-S1" %in% rules_after(function(t) {
    t$valuemaps$source_code[1] <- "01"
    t
  }))
  expect_true("V-S1" %in% rules_after(function(t) {
    t$valuemaps$na_reason[1] <- "dk"
    t
  }))
  expect_true("V-S1" %in% rules_after(function(t) {
    t$waves$member_codes[t$waves$study == "qes2007_panel"][1] <- NA
    t
  }))
})

test_that("V-S2 catches duplicate keys and codes with two outcomes", {
  expect_true("V-S2" %in% rules_after(function(t) {
    t$crosswalk <- rbind(t$crosswalk, t$crosswalk[1, ])
    t
  }))
  expect_true("V-S2" %in% rules_after(function(t) {
    t$valuemaps <- rbind(t$valuemaps, t$valuemaps[1, ])
    t
  }))
  expect_true("V-S2" %in% rules_after(function(t) {
    t$crosswalk$na_codes[xw_row(t, "qes2012", "vote_prov_recall")] <- "99=refused"
    t
  }))
})

test_that("V-S3 checks every enum column and the NA vocabulary", {
  expect_true("V-S3" %in% rules_after(function(t) {
    t$targets$type[1] <- "nominal"
    t
  }))
  expect_true("V-S3" %in% rules_after(function(t) {
    t$crosswalk$na_codes[xw_row(t, "qes2012", "lr_self")] <- "98=dunno;99=refused"
    t
  }))
  expect_true("V-S3" %in% rules_after(function(t) {
    t$crosswalk$rule[xw_row(t, "qes2012", "lr_self")] <- "fn:Bad Name"
    t
  }))
  # engine-only reasons are not spec reasons
  expect_true("V-S3" %in% rules_after(function(t) {
    t$valuemaps$na_reason[which(t$valuemaps$na_reason == "dk")[1]] <- "sysmis"
    t
  }))
  expect_true("V-S3" %in% rules_after(function(t) {
    t$waves$mode[1] <- "telephone"
    t
  }))
})

test_that("V-S4 checks references in both directions", {
  expect_true("V-S4" %in% rules_after(function(t) {
    t$crosswalk$study[1] <- "qes2019"
    t
  }))
  expect_true("V-S4" %in% rules_after(function(t) {
    t$valuemaps$map_id[1] <- "unused_map"
    t
  }))
  expect_true("V-S4" %in% rules_after(function(t) {
    t$valuemaps$target_code[which(!is.na(t$valuemaps$target_code))[1]] <- 555L
    t
  }))
  expect_true("V-S4" %in% rules_after(function(t) {
    t$targets$anchor_row[t$targets$target == "lr_self"] <- "qes2012:post:q70"
    t
  }))
  expect_true("V-S4" %in% rules_after(function(t) {
    t$levels$name[t$levels$levels_id == "pid_qc" & grepl("^inherits=", t$levels$name)] <- "inherits=pid_qc"
    t
  }))
  expect_true("V-S4" %in% rules_after(function(t) {
    t$weights$wave[1] <- "mid"
    t
  }))
})

test_that("V-S5 allows one primary row per study and target", {
  expect_true("V-S5" %in% rules_after(function(t) {
    extra <- t$crosswalk[xw_row(t, "qes2012", "lr_self"), ]
    extra$source_var <- "q70_a"
    extra$wave <- "post"
    extra$rule <- "none"
    extra$grade <- "not_comparable"
    t$crosswalk <- rbind(t$crosswalk, extra)
    t
  }))
})

test_that("V-S6 requires English and French", {
  expect_true("V-S6" %in% rules_after(function(t) {
    t$targets$label_fr[1] <- NA
    t
  }))
  expect_true("V-S6" %in% rules_after(function(t) {
    t$crosswalk$grade_reason_en[1] <- NA
    t
  }))
  expect_true("V-S6" %in% rules_after(function(t) {
    t$crosswalk$notes_en[1] <- "A note in English only."
    t
  }))
  expect_true("V-S6" %in% rules_after(function(t) {
    t$levels$label_en[1] <- NA
    t
  }))
})

test_that("V-S7 refuses replacement, control and decomposed characters", {
  for (bad in c(intToUtf8(0xFFFD), intToUtf8(0x85), "\t", paste0("e", intToUtf8(0x301)))) {
    expect_true("V-S7" %in% rules_after(function(t) {
      t$targets$description_en[1] <- paste0("Text ", bad)
      t
    }), label = utf8ToInt(bad)[1])
  }
})

test_that("V-S8 catches a label that names another level", {
  expect_true("V-S8" %in% rules_after(function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2014_q3" & t$valuemaps$source_code == "1")
    t$valuemaps$target_code[i] <- 2L
    t
  }))
  # a refusal mapped to a party label, and a label hash that matches an alias
  expect_true("V-S8" %in% rules_after(function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2012_q25" & t$valuemaps$source_code == "99")
    t$valuemaps$source_label[i] <- "Parti Qu\u00e9b\u00e9cois"
    t
  }))
  expect_true("V-S8" %in% rules_after(function(t) {
    i <- which(t$valuemaps$map_id == "vote_qes2022_pes" & t$valuemaps$source_code == "7")
    t$valuemaps$target_code[i] <- 1L
    t
  }))
  # the documented exception is allowed; removing it is not
  expect_true("V-S8" %in% rules_after(function(t) {
    t$valuemaps$alias_exception[t$valuemaps$map_id == "vote_qes2007p_intvote"] <- NA
    t
  }))
})

test_that("V-S9 refuses timing and election mismatches", {
  # [A:H4]: a pre-election target from the 2022 post-election wave
  expect_true("V-S9" %in% rules_after(function(t) {
    t$crosswalk$wave[xw_row(t, "qes2022", "vote_prov_intent")] <- "pes"
    t
  }))
  expect_true("V-S9" %in% rules_after(function(t) {
    t$crosswalk$election_ref[xw_row(t, "qes2014", "vote_prov_recall")] <- "QC2012"
    t
  }))
  expect_true("V-S9" %in% rules_after(function(t) {
    t$crosswalk$election_ref[xw_row(t, "qes2014", "sov_indep")] <- "QC2014"
    t
  }))
})

test_that("V-S10 requires a registered function and caps fn: rules", {
  expect_true("V-S10" %in% rules_after(function(t) {
    t$crosswalk$rule[xw_row(t, "qes2012", "lr_self")] <- "fn:unknown_function"
    t
  }))
})

test_that("V-S11 sets the review requirements and the licence limits", {
  expect_true("V-S11" %in% rules_after(function(t) {
    t$crosswalk$status[1] <- "stable"
    t
  }))
  expect_true("V-S11" %in% rules_after(function(t) {
    t$crosswalk$status[1] <- "draft"
    t
  }, release = TRUE))
  expect_false("V-S11" %in% rules_after(function(t) {
    t$crosswalk$status[1] <- "draft"
    t
  }))
  expect_true("V-S11" %in% rules_after(function(t) {
    t$valuemaps$source_label_origin[1] <- "ddi"
    t
  }))
  # OD3: no qes2022 wording or label text
  expect_true("V-S11" %in% rules_after(function(t) {
    t$crosswalk$wording_en[xw_row(t, "qes2022", "lr_self")] <- "Some wording"
    t
  }))
  expect_true("V-S11" %in% rules_after(function(t) {
    t$valuemaps$source_label[t$valuemaps$map_id == "vote_qes2022_pes"][1] <- "Some label"
    t
  }))
  expect_true("V-S11" %in% rules_after(function(t) {
    t$crosswalk$evidence[1] <- NA
    t
  }))
})

test_that("V-S12 ties rules to target types and not_comparable to rule none", {
  expect_true("V-S12" %in% rules_after(function(t) {
    t$crosswalk$grade[xw_row(t, "qes2018_panel", "sov_favour", "independance")] <- "comparable"
    t
  }))
  expect_true("V-S12" %in% rules_after(function(t) {
    i <- xw_row(t, "qes2012", "lr_self")
    t$crosswalk$rule[i] <- "constant"
    t$crosswalk$args[i] <- NA
    t$crosswalk$na_codes[i] <- NA
    t
  }))
  expect_true("V-S12" %in% rules_after(function(t) {
    t$crosswalk$args[xw_row(t, "qes2022", "birth_year")] <- "min=1850;max=2010;from_label=TRUE"
    t
  }))
})

test_that("V-S13 checks weight roles and one recommended weight per wave", {
  expect_true("V-S13" %in% rules_after(function(t) {
    t$weights$recommended[t$weights$weight_var == "cps_weight_general_trimmed"] <- TRUE
    t
  }))
  expect_true("V-S13" %in% rules_after(function(t) {
    t$weights$recommended[t$weights$weight_var == "pdspart2"] <- TRUE
    t$weights$recommended[t$weights$study == "qes2007_panel" & t$weights$weight_var == "pondam1"] <- FALSE
    t
  }))
  # released stable rows may not use a weight that needs review
  expect_true("V-S13" %in% rules_after(function(t) {
    i <- xw_row(t, "qes2018_panel", "vote_prov_intent")
    t$crosswalk$status[i] <- "stable"
    t$crosswalk$reviewed_by[i] <- "reviewer"
    t$crosswalk$reviewed_on[i] <- as.Date("2026-09-27")
    t
  }, release = TRUE))
})

test_that("V-S14 requires ordinal maps to be monotone", {
  expect_true("V-S14" %in% rules_after(function(t) {
    m <- t$valuemaps$map_id == "interest4_qes2018_q27"
    t$valuemaps$target_code[m & t$valuemaps$source_code == "2"] <- 3L
    t$valuemaps$target_code[m & t$valuemaps$source_code == "3"] <- 2L
    t
  }))
})

test_that("V-S15 keeps target, family and set names apart", {
  expect_true("V-S15" %in% rules_after(function(t) {
    t$targets$family[t$targets$target == "lr_self"] <- "lr_self"
    t
  }))
  expect_true("V-S15" %in% rules_after(function(t) {
    t$targets$sets[1] <- "core;qes2018"
    t
  }))
  expect_true("V-S15" %in% rules_after(function(t) {
    t$targets$family[1] <- "Vote__prov"
    t
  }))
})

test_that("V-S16 keeps mapped levels within the offered levels", {
  expect_true("V-S16" %in% rules_after(function(t) {
    t$crosswalk$levels_offered[xw_row(t, "qes2018", "vote_prov_recall")] <- "PLQ;PQ;CAQ"
    t
  }))
  expect_true("V-S16" %in% rules_after(function(t) {
    t$crosswalk$levels_offered[xw_row(t, "qes2018", "vote_prov_recall")] <- "PLQ;PQ;CAQ;QS;BQ"
    t
  }))
})

test_that("V-S17 holds identical rows to their anchor", {
  expect_true("V-S17" %in% rules_after(function(t) {
    t$crosswalk$grade[xw_row(t, "qes2018", "pid_prov")] <- "identical"
    t
  }))
  expect_true("V-S17" %in% rules_after(function(t) {
    t$crosswalk$dk_offered[xw_row(t, "qes2014", "sov_indep")] <- "none"
    t
  }))
  expect_true("V-S17" %in% rules_after(function(t) {
    t$crosswalk$grade[xw_row(t, "qes2012", "lr_self")] <- "comparable"
    t
  }))
})

test_that("level codes are frozen", {
  lv <- .qes_spec_levels(shipped_spec()$tables$levels, "party_qc")
  expect_identical(
    stats::setNames(lv$code, lv$name),
    c(PLQ = 1L, PQ = 2L, CAQ = 3L, QS = 4L, PVQ = 5L, PCQ = 6L, ON = 7L, ADQ = 8L, other = 90L)
  )
  intent <- .qes_spec_levels(shipped_spec()$tables$levels, "party_qc_intent")
  expect_identical(intent$name, c(lv$name, "no_party"))
  expect_identical(intent$code[intent$name == "no_party"], 95L)
  pid <- .qes_spec_levels(shipped_spec()$tables$levels, "pid_qc")
  expect_identical(pid$code[pid$name == "none"], 97L)
})

test_that("real-code regressions hold in the shipped spec", {
  t <- shipped_spec()$tables
  vm <- t$valuemaps
  code <- function(map, src) vm$target_code[vm$map_id == map & vm$source_code == src]
  reason <- function(map, src) vm$na_reason[vm$map_id == map & vm$source_code == src]
  set <- function(id) .qes_spec_levels(t$levels, id)
  lvl <- function(id, name) set(id)$code[set(id)$name == name]
  xw <- t$crosswalk
  # [A:H3]: 2018 q27 = 1 is "very", the direction of 2012 q67 and 2014 Q28
  expect_identical(code("interest4_qes2018_q27", "1"), lvl("interest4", "very"))
  expect_identical(code("interest4_qes2012_q67", "1"), lvl("interest4", "very"))
  expect_identical(code("interest4_qes2014_q28", "1"), lvl("interest4", "very"))
  # [A:H3]: 2014 Q32 keeps 0 and 10
  expect_identical(xw$args[xw$study == "qes2014" & xw$source_var == "Q32"], "min=0;max=10")
  # [A:H4]: 2022 PES 7 and CPS 5 are both PCQ; PES 5 is another party
  expect_identical(code("vote_qes2022_pes", "7"), lvl("party_qc", "PCQ"))
  expect_identical(code("vote_qes2022_cps", "5"), lvl("party_qc", "PCQ"))
  expect_identical(code("vote_qes2022_pes", "5"), lvl("party_qc", "other"))
  # OD12: pes_turnout 5 not registered, 6 don't know; gated recall uses them
  expect_identical(reason("turnout_qes2022_pes", "5"), "not_registered")
  expect_identical(reason("turnout_qes2022_pes", "6"), "dk")
  i <- which(xw$study == "qes2022" & xw$target == "vote_prov_recall")
  expect_identical(xw$gate_to[i], "2=not_voted;3=not_voted;4=not_voted;5=not_registered;6=dk")
  # the 2022 intention is gated on cps_turnout, 9 = refused and 10 = don't know
  i <- which(xw$study == "qes2022" & xw$target == "vote_prov_intent")
  expect_identical(xw$gate_var[i], "cps_turnout")
  expect_identical(c(reason("vote_qes2022_cps", "9"), reason("vote_qes2022_cps", "10")), c("refused", "dk"))
  # 2018 q6 95 is a spoiled ballot; q5 = 5 ineligible; minors routed out of q5
  expect_identical(reason("vote_qes2018_q6", "95"), "spoiled")
  expect_identical(reason("turnout_qes2018_q5", "5"), "ineligible")
  expect_identical(xw$na_codes[xw$study == "qes2018" & xw$source_var == "q5"], "NA=inapplicable")
  # 2007 panel: membership by disposition; 10 "non rejoint" is not a nonvoter [A:N1]
  wv <- t$waves
  expect_identical(wv$n_cases[wv$study == "qes2007_panel"], c(2050L, 2054L))
  expect_identical(wv$member_codes[wv$study == "qes2007_panel"], c("C", "CO"))
  expect_identical(reason("vote_qes2007p_vote", "10"), "not_in_wave")
  expect_identical(reason("vote_qes2007p_vote", "0"), "not_voted")
  # 2018 panel: independance is a recode of rts_q7, documented and never mapped
  i <- which(xw$study == "qes2018_panel" & xw$source_var == "independance")
  expect_identical(c(xw$rule[i], xw$grade[i]), c("none", "not_comparable"))
  expect_false(xw$primary[i])
  # 2012 panel: 98 and 99 reversed; "pays souverain" is not sov_indep
  expect_identical(c(reason("sov_qes2012p_intvoteref", "98"), reason("sov_qes2012p_intvoteref", "99")), c("refused", "dk"))
  expect_identical(xw$target[xw$study == "qes2012_panel"], "sov_sovereign_country")
  # a recommended weight is never vote-calibrated (OD6/V-S13), and qes2022 is untrimmed (OD10)
  wt <- t$weights
  expect_identical(wt$weight_var[wt$study == "qes2022" & wt$recommended], c("cps_weight_general", "pes_weight_general"))
  expect_false(any(wt$recommended & wt$role %in% c("vote_calibrated", "turnout_calibrated")))
})

test_that("no qes2022 label text or wording ships (OD3)", {
  t <- shipped_spec()$tables
  xw <- t$crosswalk
  rows <- xw$study == "qes2022"
  expect_true(any(rows))
  expect_true(all(is.na(xw$wording_en[rows]) & is.na(xw$wording_fr[rows])))
  expect_true(all(!is.na(xw$wording_ref[rows])))
  maps <- t$valuemaps$map_id %in% xw$map_id[rows]
  expect_true(all(is.na(t$valuemaps$source_label[maps])))
  expect_true(all(grepl("^[0-9a-f]{32}$", t$valuemaps$source_label_hash[maps])))
  # no counts in the text of qes2022 rows (numbers with a thousands separator
  # or a percentage)
  text <- unlist(xw[rows, c("evidence", "notes_en", "notes_fr", "grade_reason_en", "grade_reason_fr")])
  expect_false(any(grepl("[0-9],[0-9]{3}|[0-9]+ ?%", text[!is.na(text)])))
  # waves and weights carry only what the codebook publishes (wave sizes in
  # n_cases, dates, mode, weighting method): no counts in their text either
  wv <- t$waves[t$waves$study == "qes2022", , drop = FALSE]
  wt <- t$weights[t$weights$study == "qes2022", , drop = FALSE]
  expect_true(nrow(wv) > 0L && nrow(wt) > 0L)
  text <- c(unlist(wv[, c("notes", "target_population_en", "target_population_fr")]),
            unlist(wt[, c("source_ref", "population")]))
  expect_false(any(grepl("[0-9],[0-9]{3}|[0-9]+ ?%", text[!is.na(text)])))
})

test_that("crosswalk wording of CC0 studies is the dictionary's question text", {
  xw <- shipped_spec()$tables$crosswalk
  vars <- .qes_dict_shipped()$variables
  for (i in seq_len(nrow(xw))) {
    d <- vars[vars$study == xw$study[i] & vars$variable == xw$source_var[i], , drop = FALSE]
    if (nrow(d) != 1L) next
    for (lang in c("en", "fr")) {
      q <- d[[paste0("question_", lang)]]
      w <- xw[[paste0("wording_", lang)]][i]
      if (!is.na(q) && !is.na(w)) {
        expect_identical(w, q, info = paste(xw$study[i], xw$source_var[i], lang))
      }
    }
  }
})

test_that("the spec reads the same under a C locale and French messages", {
  ref <- .qes_spec_check(shipped_spec())
  ref_spec <- shipped_spec()
  withr::local_envvar(LANGUAGE = "fr")
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    s <- shipped_spec()
    expect_identical(s, ref_spec)
    expect_identical(.qes_spec_hash(spec_dir()), ref_spec$hash)
    expect_identical(.qes_spec_check(s), ref)
  })
})

test_that("a value-map code outside an ordinal set is reported by V-S4, not an R error", {
  dir <- local_spec_copy()
  x <- .qes_read_csv(file.path(dir, "valuemaps.csv"))
  x$target_code[x$map_id == "interest4_qes2018_q27" & x$source_code == "2"] <- "50"
  .qes_write_csv(x, file.path(dir, "valuemaps.csv"))
  s <- expect_no_error(qes_spec("spec", spec = dir, validate = "report"))
  chk <- attr(s, "check")
  expect_true("V-S4" %in% chk$rule[chk$severity == "error"])
  expect_error(qes_spec("spec", spec = dir), class = "qesR_error_spec")
})

test_that("an unexpected error inside the validator becomes qesR_error_spec", {
  s <- shipped_spec()
  s$tables$targets$type <- list(1)
  expect_error(qes_spec("spec", spec = s, validate = "report"), class = "qesR_error_spec")
})

test_that("V-S2 checks level sets as expanded by inherits=", {
  expect_true("V-S2" %in% rules_after(function(t) {
    t$levels$code[t$levels$levels_id == "pid_qc" & t$levels$name %in% "none"] <- 90L
    t$levels$order[t$levels$levels_id == "pid_qc" & t$levels$name %in% "none"] <- 90L
    t
  }))
})

test_that("V-S4 checks the files of wording_ref", {
  expect_true("V-S4" %in% rules_after(function(t) {
    t$crosswalk$wording_ref[xw_row(t, "qes2012", "vote_prov_recall")] <- "999999:Q25"
    t
  }))
  expect_true("V-S4" %in% rules_after(function(t) {
    t$crosswalk$wording_ref[xw_row(t, "qes2012", "vote_prov_recall")] <- "352010:Q3"
    t
  }))
})

test_that("V-S1 refuses args with min greater than max", {
  expect_true("V-S1" %in% rules_after(function(t) {
    t$crosswalk$args[xw_row(t, "qes2014", "lr_self")] <- "min=10;max=0"
    t
  }))
})

test_that("levels problems give the data row of levels.csv", {
  s <- shipped_spec()
  i <- which(s$tables$levels$levels_id == "interest4" & s$tables$levels$name == "not_at_all")
  expect_identical(i, 26L)
  s$tables$levels$name[i] <- "hardly"
  s$tables$levels$label_fr[i] <- NA
  p <- .qes_spec_check(s)
  expect_identical(p$row[p$rule == "V-S2" & p$table == "levels"], i)
  expect_identical(p$row[p$rule == "V-S6" & p$table == "levels"], i)
})

test_that("an edited qes_spec object gets a new hash and is custom", {
  s <- qes_spec("spec")
  expect_false(qes_spec("spec", spec = s)$custom)
  w <- s$tables$weights
  i <- which(w$study == "qes2022" & w$wave == "cps")
  w$recommended[i] <- !w$recommended[i]
  s$tables$weights <- w
  e <- qes_spec("spec", spec = s, validate = "report")
  expect_true(e$custom)
  expect_false(identical(e$hash, s$hash))
  chk <- attr(e, "check")
  expect_true("warning" %in% chk$severity[chk$rule == "V-P2"])
})
