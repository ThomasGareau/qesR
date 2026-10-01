# The relaxed layer (R/hz-relaxed.R, spec 4.4.0; signed off in 4.5.0) and qes_decon()
# (R/decon-relaxed.R): the spec tables and their validator rules V-R1 to
# V-R12, the views, and qes_decon() end to end, offline, on the
# demonstration study and on synthetic data built from the strict and the
# relaxed rows (real variable names, every mapped code and gate outcome, no
# respondent).

# Synthetic frames that also hold the variables of the relaxed rows: the
# relaxed rows (renamed so that they do not meet the strict targets) are
# added to the crosswalk the generator reads; the bracket question of a
# fn:amount_bands row is added as a map row, and the 2022 amount gets
# amounts in each third.
rx_syn <- function(studies) {
  s <- hz_spec()
  ps <- .qes_rx_pseudo_spec(s)
  xr <- ps$tables$crosswalk
  tr <- ps$tables$targets
  xr$target <- paste0("rx__", xr$target)
  tr$target <- paste0("rx__", tr$target)
  amt <- xr[xr$rule %in% "fn:amount_bands", , drop = FALSE]
  if (nrow(amt) > 0L) {
    a <- lapply(amt$args, .qes_hz_amount_args)
    amt$rule <- "map"
    amt$source_var <- vapply(a, `[[`, "", "then_var")
    amt$map_id <- vapply(a, `[[`, "", "then_map")
    amt$args <- NA_character_
    amt$na_codes <- NA_character_
  }
  m <- s
  m$tables$crosswalk <- rbind(s$tables$crosswalk, xr, amt)
  m$tables$targets <- rbind(s$tables$targets, tr)
  syn <- .qes_synthetic(studies, spec = m)
  if (!is.null(syn$qes2022)) {
    syn$qes2022$cps_income <- rep_len(c(10000, 60000, 120000, 0, -99), nrow(syn$qes2022))
  }
  syn
}

# qes_decon() on frames given as data (unverified: the warning is expected).
rx_run <- function(data, ..., include_review = TRUE, quiet = TRUE) {
  withCallingHandlers(
    .qes_decon_build(names(data), data = data, include_review = include_review, quiet = quiet, ...),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning"),
    qesR_warning_label_mismatch = function(w) invokeRestart("muffleWarning")
  )
}

rx_cols <- function() .qes_rx_columns(hz_spec())

test_that("the shipped relaxed layer passes V-R1 to V-R12 and covers 28 concepts", {
  s <- hz_spec()
  p <- .qes_spec_check(s)
  expect_false(any(grepl("^V-R", p$rule)), info = paste(p$detail[grepl("^V-R", p$rule)], collapse = "; "))
  p9 <- .qes_rx_data_check(s, .qes_hz_sources_shipped(s))
  expect_identical(nrow(p9[p9$severity == "error", ]), 0L, info = paste(p9$detail, collapse = "; "))
  rx <- rx_cols()
  expect_identical(nrow(rx), 32L)
  expect_identical(rx$position, seq_len(32L))
  expect_true(all(c("education", "income_cat", "vote_choice", "sovereignty", "lr", "religion") %in% rx$column))
  # every relaxed row is signed off (spec 4.5.0), by a review that says it
  # was automated, not human
  rm <- .qes_rx_tables(s)$maps
  expect_true(all(rm$status %in% "stable"))
  expect_true(all(grepl("not a human review", rm$reviewed_by, fixed = TRUE)))
  expect_true(all(grepl("not a human review", rm$review_note, fixed = TRUE)))
  expect_true(all(rm$reviewed_on == as.Date("2026-10-01")))
  expect_true(all(startsWith(stats::na.omit(rm$map_id), "rx_")))
  # schema 4, spec 4.5.0
  expect_identical(s$schema_version, "4")
  expect_identical(s$version, "4.5.0")
})

test_that("a relaxed column is never a target, and carries no grade", {
  s <- hz_spec()
  expect_false(any(c("grade", "grade_reason_en") %in% names(s$tables$relaxed)))
  expect_false(any(c("grade", "grade_reason_en") %in% names(s$tables$relaxed_maps)))
  rx <- rx_cols()
  shadow <- rx$column[rx$column %in% s$tables$targets$target]
  # same name as a target: the column extends it, except religion (OD-R6)
  expect_setequal(shadow, c("gender", "religion", "born_canada", "gov_satisfaction"))
  expect_true(all(rx$same_as[rx$column %in% setdiff(shadow, "religion")]))
  expect_false(any(grepl(.qes_rx_claim_en, rx$relax_en, ignore.case = TRUE, perl = TRUE)))
})

test_that("the validator rejects malformed relaxed tables (V-R1 to V-R12)", {
  s <- hz_spec()
  bad <- function(edit) {
    t <- s
    t$tables <- edit(t$tables)
    p <- rbind(.qes_spec_check(t), .qes_rx_data_check(t, .qes_hz_sources_shipped(t)))
    unique(p$rule[p$severity == "error"])
  }
  expect_true("V-R1" %in% bad(function(t) {
    t$relaxed$position[2] <- 1L
    t
  }))
  expect_true("V-R1" %in% bad(function(t) {
    t$relaxed_maps <- rbind(t$relaxed_maps, t$relaxed_maps[1, ])
    t
  }))
  expect_true("V-R2" %in% bad(function(t) {
    t$relaxed$column[t$relaxed$column == "lr"] <- "weight"
    t
  }))
  expect_true("V-R2" %in% bad(function(t) {
    t$relaxed$same_as[t$relaxed$column == "gender"] <- FALSE
    t
  }))
  expect_true("V-R3" %in% bad(function(t) {
    t$relaxed$base[t$relaxed$column == "yob"] <- "target:no_such_target"
    t
  }))
  expect_true("V-R3" %in% bad(function(t) {
    t$relaxed$base[t$relaxed$column == "language_fr"] <- "column:interest"
    t
  }))
  expect_true("V-R4" %in% bad(function(t) {
    t$relaxed$transform[t$relaxed$column == "born_quebec"] <- "recode:quebec=yes,abroad=no"
    t
  }))
  expect_true("V-R4" %in% bad(function(t) {
    t$relaxed$transform[t$relaxed$column == "interest"] <- "bands:0.75,0.35:low,medium,high"
    t
  }))
  expect_true("V-R5" %in% bad(function(t) {
    t$relaxed_maps$notes_fr[which(!is.na(t$relaxed_maps$notes_en))[1]] <- NA
    t
  }))
  expect_true("V-R6" %in% bad(function(t) {
    t$relaxed$essential[t$relaxed$column == "citizenship"] <- FALSE
    t
  }))
  expect_true("V-R7" %in% bad(function(t) {
    t$relaxed$relax_en[1] <- "The questions are identical in every study."
    t
  }))
  expect_true("V-R8" %in% bad(function(t) {
    t$relaxed_maps$wave[t$relaxed_maps$column == "religion"][1] <- "pre"
    t
  }))
  expect_true("V-R8" %in% bad(function(t) {
    k <- which(t$valuemaps$map_id == "rx_education_qes2007_q77")[1]
    t$valuemaps$target_code[k] <- 9L
    t
  }))
  expect_true("V-R9" %in% bad(function(t) {
    t$valuemaps <- t$valuemaps[!(t$valuemaps$map_id == "rx_education_qes2014_qscol" & t$valuemaps$source_code == "5"), ]
    t
  }))
  expect_true("V-R9" %in% bad(function(t) {
    # a bracket out of its third
    k <- t$valuemaps$map_id == "rx_income_cat_qes2007_q78" & t$valuemaps$source_code == "4"
    t$valuemaps$target_code[k] <- 1L
    t
  }))
  expect_true("V-R10" %in% bad(function(t) {
    t$relaxed_maps$override[t$relaxed_maps$column == "education"][1] <- TRUE
    t
  }))
  expect_true("V-R10" %in% bad(function(t) {
    t$relaxed_maps$override[t$relaxed_maps$column == "language" & t$relaxed_maps$study == "qes2007"] <- FALSE
    t
  }))
  expect_true("V-R11" %in% bad(function(t) {
    t$rx_hashes$md5[1] <- "not an md5"
    t
  }))
  expect_true("V-R12" %in% bad(function(t) {
    t$legacy$target[which(!is.na(t$legacy$target))[1]] <- "income_cat"
    t
  }))
})

test_that("the relaxed views list the columns and each study's relaxed rows", {
  v <- qes_spec("relaxed")
  expect_identical(v$column, rx_cols()$column)
  expect_true(all(c("label", "base", "transform", "timing", "relaxed", "qes2022", "qes1998") %in% names(v)))
  expect_identical(v$qes2022[v$column == "education"], "relaxed")
  expect_identical(v$qes2014[v$column == "gender"], "strict")
  expect_true(is.na(v$qes1998[v$column == "religion"]))
  # a column built on another column takes its source study by study: the
  # 1998 mother tongue comes from a relaxed row, and so do its flags
  expect_identical(v$qes1998[v$column == "language"], "relaxed")
  expect_identical(v$qes1998[v$column == "language_fr"], "relaxed")
  expect_identical(v$qes2018[v$column == "language_eng"], v$qes2018[v$column == "language"])
  fr <- qes_spec("relaxed", lang = "fr", targets = "education")
  expect_identical(fr$label, "Scolarit\u00e9")
  m <- qes_spec("relaxed_maps", targets = "education")
  expect_true(all(m$column == "education"))
  expect_match(m$recode[m$study == "qes2007"], "1-4 = No high school diploma; 5 = High school diploma", fixed = TRUE)
  m_fr <- qes_spec("relaxed_maps", targets = "education", studies = "qes2007", lang = "fr")
  expect_match(m_fr$recode, "Sans dipl\u00f4me", fixed = TRUE)
  expect_error(qes_spec("relaxed", targets = "no_such_column"), class = "qesR_error_input")
  expect_error(qes_spec("spec", targets = "education"), class = "qesR_error_input")
})

test_that("qes_decon() returns the documented columns, in order, offline", {
  d <- qes_decon("qes_demo", quiet = TRUE)
  expect_false(inherits(d, "qes_harmonized"))
  expect_identical(class(d), "data.frame")
  expect_identical(names(d), c("study", "year", "wave", "qes_id", "weight", "weight_var", rx_cols()$column))
  expect_identical(nrow(d), nrow(qes_harmonize("qes_demo", targets = "gender", layout = "long", quiet = TRUE)))
  d0 <- qes_decon("qes_demo", weights = FALSE, quiet = TRUE)
  expect_identical(names(d0), c("study", "year", "wave", "qes_id", rx_cols()$column))
  expect_null(attr(d0, "weights"))
  # factors with every level, ordered where the column is ordinal; numbers
  expect_s3_class(d$age_group, "ordered")
  expect_identical(levels(d$education), c("No high school diploma", "High school diploma",
                                          "College, CEGEP or trade school", "University"))
  expect_type(d$lr, "double")
  expect_type(d$yob, "double")
  # attributes
  for (col in rx_cols()$column) {
    expect_true(is.character(attr(d[[col]], "label")) && nzchar(attr(d[[col]], "label")), info = col)
    expect_true(is.character(attr(d[[col]], "relaxed")) && nzchar(attr(d[[col]], "relaxed")), info = col)
    expect_true(is.data.frame(attr(d[[col]], "sources")), info = col)
  }
  src <- attr(d, "decon_sources")
  expect_identical(names(src), c("column", "study", "wave", "base", "source_var", "wording_ref", "recode", "relaxed",
                                 "status", "applied", "n_value", "na_reasons"))
  expect_identical(attr(d, "spec_version"), "4.5.0")
  expect_identical(attr(d, "lang"), "en")
  expect_s3_class(qes_provenance(d), "qes_provenance")
})

test_that("identity columns equal their strict target or pooled variable", {
  d <- qes_decon("qes_demo", quiet = TRUE)
  h <- qes_harmonize("qes_demo", targets = c("gender", "vote_choice", "lr_self", "birth_year", "turnout"),
                     layout = "long", quiet = TRUE)
  expect_identical(as.character(d$gender), as.character(h$gender))
  expect_identical(as.character(d$vote_choice), as.character(h$vote_choice))
  expect_identical(as.numeric(d$lr), as.numeric(h$lr_self))
  expect_identical(as.numeric(d$yob), as.numeric(h$birth_year))
  expect_identical(as.character(d$turnout), as.character(h$turnout))
  expect_identical(as.character(d$vote_type), as.character(.qes_rx_level_text(as.character(h$vote_choice__type),
                                                                                .qes_spec_levels(hz_spec()$tables$levels, "rx_vote_type"), "en")))
})

test_that("the collapses follow relaxed.csv (sovereignty, interest)", {
  d <- qes_decon("qes_demo", quiet = TRUE)
  h <- qes_harmonize("qes_demo", targets = c("sov_support", "pol_interest"), layout = "long", values = "code",
                     missing = "reasons", quiet = TRUE)
  sv <- as.character(h$sov_support)
  expect_identical(as.character(d$sovereignty), ifelse(sv %in% "yes", "Yes", ifelse(sv %in% "no", "No", NA)))
  x <- h$pol_interest
  want <- ifelse(is.na(x), NA, ifelse(x < 0.35, "Low", ifelse(x < 0.75, "Medium", "High")))
  expect_identical(as.character(d$interest), want)
  expect_identical(as.numeric(d$interest_01), as.numeric(x))
})

test_that("labels follow lang; column names do not", {
  en <- qes_decon("qes_demo", quiet = TRUE)
  fr <- qes_decon("qes_demo", lang = "fr", quiet = TRUE)
  expect_identical(names(fr), names(en))
  expect_identical(levels(fr$interest), c("Faible", "Moyen", "\u00c9lev\u00e9"))
  expect_identical(attr(fr$education, "label"), "Scolarit\u00e9")
  expect_match(attr(fr$sovereignty, "relaxed"), "^Oui ou non")
  expect_identical(attr(fr, "lang"), "fr")
  expect_identical(is.na(fr$gender), is.na(en$gender))
})

test_that("signed-off relaxed rows are applied; rows in review are not, and say so", {
  d <- qes_decon("qes_demo", quiet = TRUE)
  # the demonstration study has QREGION (qes2014's administrative region),
  # whose relaxed row is signed off
  expect_false(anyNA(d$region_admin))
  src <- attr(d, "decon_sources")
  r <- src[src$column == "region_admin", ]
  expect_identical(r$status, "stable")
  expect_true(r$applied)
  # the demonstration study does not hold the other relaxed variables
  expect_false(any(src$column == "education"))
  expect_message(qes_decon("qes_demo"), class = "qesR_message_decon")
  expect_silent(qes_decon("qes_demo", quiet = TRUE))
  # the same row put back in review is not applied: NA, reason not_reviewed
  s <- hz_spec()
  s$tables$relaxed_maps$status <- "review"
  d2 <- .qes_decon_build("qes_demo", quiet = TRUE, spec = s)
  expect_true(all(is.na(d2$region_admin)))
  r2 <- attr(d2, "decon_sources")
  r2 <- r2[r2$column == "region_admin", ]
  expect_identical(r2$status, "review")
  expect_false(r2$applied)
  expect_match(r2$na_reasons, "not_reviewed")
  # include_review applies it again
  d3 <- .qes_decon_build("qes_demo", quiet = TRUE, spec = s, include_review = TRUE)
  expect_identical(as.character(d3$region_admin), as.character(d$region_admin))
})

test_that("weights have mean 1 in each study and wave, or NA with a reason", {
  d <- qes_decon("qes_demo", quiet = TRUE)
  expect_equal(mean(d$weight), 1)
  expect_identical(unique(d$weight_var), "POND")
  w <- attr(d, "weights")
  expect_identical(names(w), c("study", "wave", "weight_var", "status", "reason", "n_rows", "n_weight", "mean"))
  expect_true(is.na(w$reason))
})

test_that("qes_decon() rejects bad arguments with qesR_error_input", {
  expect_error(qes_decon("qes2099"), class = "qesR_error_unknown_study")
  expect_error(qes_decon("qes1998_crop"), class = "qesR_error_input")
  expect_error(qes_decon("qes_demo", lang = "de"), class = "qesR_error_input")
  expect_error(qes_decon("qes_demo", weights = NA), class = "qesR_error_input")
  expect_error(qes_decon("qes_demo", quiet = "yes"), class = "qesR_error_input")
})

test_that("end to end on synthetic data: relaxed rows, static columns, sources", {
  syn <- rx_syn(c("qes2022", "qes2014", "qes2007_panel", "qes_crop_2007_2010", "qes1998"))
  d <- rx_run(syn)
  rx <- rx_cols()
  # no row is dropped
  h <- withCallingHandlers(
    qes_harmonize(names(syn), targets = "gender", layout = "long", data = syn, quiet = TRUE),
    qesR_warning_unverified_source = function(w) invokeRestart("muffleWarning")
  )
  expect_identical(nrow(d), nrow(h))
  expect_identical(d$qes_id, h$qes_id)
  # every relaxed row of these studies is applied, with values
  src <- attr(d, "decon_sources")
  rel <- src[src$base == "relaxed", ]
  expect_true(all(rel$applied))
  rm <- .qes_rx_tables(hz_spec())$maps
  rm <- rm[rm$study %in% names(syn), ]
  expect_setequal(paste(rel$column, rel$study), paste(rm$column, rm$study))
  # static columns are the same on every row of a respondent
  for (col in rx$column[rx$timing == "static"]) {
    by_id <- tapply(as.character(d[[col]]), d$qes_id, function(x) length(unique(x)))
    expect_true(all(by_id == 1L), info = col)
  }
  # the 2022 marital status, asked after the election, is on both rows of
  # a respondent of the two waves, and missing for the campaign-only ones
  k <- d$study == "qes2022"
  both <- d$qes_id[k][duplicated(d$qes_id[k])]
  expect_true(sum(!is.na(d$marital[k & d$qes_id %in% both])) > 0L)
  expect_identical(is.na(d$marital[k & d$wave == "cps" & d$qes_id %in% both]),
                   is.na(d$marital[k & d$wave == "pes"]))
  expect_true(all(is.na(d$marital[k & !d$qes_id %in% both])))
  # wave columns sit on the wave that asked them: vote_type recall on pes
  expect_true(all(as.character(d$vote_type[k & d$wave == "pes"]) == "Reported vote (after the election)"))
  # the income amount of 2022 in thirds, then the brackets
  inc <- syn$qes2022$cps_income
  first <- !duplicated(d$qes_id[k])
  got <- as.character(d$income_cat[k][first])
  expect_identical(got[inc == 10000], rep("Low (bottom third)", sum(inc == 10000)))
  expect_identical(got[inc == 60000], rep("Middle", sum(inc == 60000)))
  expect_identical(got[inc == 120000], rep("High (top third)", sum(inc == 120000)))
  # the CROP sovereignty question and its type
  c_ <- d$study == "qes_crop_2007_2010"
  expect_true(all(as.character(d$sovereignty_type[c_]) == "CROP referendum question (wording not deposited)"))
  expect_true(any(!is.na(d$sovereignty[c_])))
  # language and its flags: French first, both flags yes for two tongues
  m22 <- syn$qes2022
  two <- m22$cps_lang_1 %in% 1 & m22$cps_lang_2 %in% 1
  if (any(two)) {
    ids <- paste0("qes2022:", .qes_hz_ids("qes2022", m22)$key[two])
    x <- d[d$qes_id %in% ids, ]
    expect_true(all(as.character(x$language) == "French"))
    expect_true(all(as.character(x$language_fr) == "Yes") && all(as.character(x$language_eng) == "Yes"))
  }
  # sources count the values they gave
  for (col in c("education", "religion", "vote_choice")) {
    s <- src[src$column == col, ]
    static <- rx$timing[rx$column == col] == "static"
    n_all <- if (static) sum(!is.na(d[[col]][!duplicated(d$qes_id)])) else sum(!is.na(d[[col]]))
    expect_identical(sum(s$n_value), n_all, info = col)
  }
  # the party lineage applies to the relaxed party columns
  l <- qes_party_lineage(d)
  expect_true(all(c("vote_choice_lineage", "vote_prev_lineage", "pid_lineage") %in% names(l)))
  expect_false(any(grepl("__grade", names(l))))
  expect_true(all(as.character(l$vote_choice_lineage[as.character(d$vote_choice) %in% c("ADQ", "CAQ")]) == "ADQ/CAQ"))
})

test_that("a relaxed row whose codes the data do not map is a spec error", {
  syn <- rx_syn("qes2014")
  x <- unclass(syn$qes2014$QSCOL)
  x[1] <- 77
  syn$qes2014$QSCOL <- haven::labelled(x, labels = attr(syn$qes2014$QSCOL, "labels"))
  expect_error(rx_run(syn), class = "qesR_error_spec")
})

test_that("the Columns section of ?qes_decon is generated from relaxed.csv", {
  rd <- .rd_decon_columns()
  expect_identical(rd[1], "@section Columns:")
  for (col in rx_cols()$column) expect_true(any(grepl(sprintf("\\code{%s}", col), rd, fixed = TRUE)), info = col)
})

test_that("the generated reference has a chapter on the relaxed layer, in both languages", {
  en <- .spec_reference_md("en")
  fr <- .spec_reference_md("fr")
  expect_match(en, "## Relaxed harmonization: qes_decon()", fixed = TRUE)
  expect_match(fr, "## Harmonisation souple\u00a0: qes_decon()", fixed = TRUE)
  expect_match(en, "{#relaxed-education}", fixed = TRUE)
})
