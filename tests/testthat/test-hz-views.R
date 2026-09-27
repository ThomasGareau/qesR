# The "targets" and "crosswalk" views of qes_spec(), and the reference
# generated from the spec (design.md sections 2.2 and 9, slice HZ3).

test_that("view 'targets' gives one row per target and each study's best grade", {
  v <- qes_spec()
  s <- hz_spec()
  expect_identical(v$target, s$tables$targets$target)
  expect_identical(names(v)[1:9], c("target", "family", "type", "target_timing", "label", "definition",
                                   "levels", "status", "added_in"))
  expect_true(all(c("qes2012", "qes2014", "qes2018", "qes2022", "qes2007_panel", "qes2018_panel") %in% names(v)))
  expect_identical(v$qes2012[v$target == "vote_prov_recall"], "identical")
  expect_identical(v$qes2018[v$target == "turnout_prov_recall"], "approximate")
  expect_true(is.na(v$qes2012[v$target == "vote_prov_intent"]))
  # 2018p sov_favour: identical (rts_q7); the not_comparable independance row does not count
  expect_identical(v$qes2018_panel[v$target == "sov_favour"], "identical")
  expect_match(v$levels[v$target == "interest_4pt"], "^1=Very interested; 2=")
  # filters and language
  f <- qes_spec(targets = "vote", studies = c("qes2014", "qes_demo"), lang = "fr")
  expect_identical(f$target, c("vote_prov_recall", "vote_prov_intent", "vote_prov_intent_push", "turnout_prov_recall"))
  expect_identical(names(f)[-(1:9)], "qes2014")
  expect_identical(f$label[1], "Vote provincial (rappel)")
  expect_identical(attr(v, "qes_spec")$version, s$version)
})

test_that("view 'crosswalk' gives the rows, their grade reasons and structural zeros", {
  xw <- qes_spec("crosswalk", targets = "vote_prov_recall")
  expect_s3_class(xw, "qes_crosswalk")
  expect_true(all(xw$target == "vote_prov_recall"))
  expect_identical(xw$levels_not_offered[xw$study == "qes2018"], "PVQ;PCQ;ON;ADQ")
  expect_identical(xw$gate[xw$study == "qes2012"], "q21: 2 = not_voted, 8 = dk, 9 = refused")
  expect_identical(xw$weight_var[xw$study == "qes2014"], "POND")
  expect_identical(xw$grade_reason[xw$study == "qes2012"], "Anchor row of the target.")
  fr <- qes_spec("crosswalk", targets = "vote_prov_recall", lang = "fr")
  expect_identical(fr$grade_reason[fr$study == "qes2012"], "Ligne d'ancrage de la cible.")
  # the wording of qes2022 cannot ship: its document reference is given
  expect_true(is.na(xw$wording[xw$study == "qes2022"]))
  expect_identical(xw$wording_ref[xw$study == "qes2022"], "7449514:p.68")
  # the documentation-only row is listed, and is never primary
  all_rows <- qes_spec("crosswalk", studies = "qes2018_panel")
  k <- all_rows$source_var == "independance"
  expect_identical(all_rows$grade[k], "not_comparable")
  expect_false(all_rows$primary[k])
})

test_that("view 'crosswalk' at code level, and in the retroharmonize format", {
  cw <- qes_spec("crosswalk", targets = "vote_prov_recall", studies = "qes2018", level = "code")
  expect_identical(names(cw), c("study", "wave", "target", "variable", "source_code", "source_label", "origin",
                                "target_code", "target_level", "target_label", "na_reason", "note"))
  expect_identical(cw$na_reason[cw$variable == "q6" & cw$source_code == "95"], "spoiled")
  expect_identical(cw$target_level[cw$variable == "q6" & cw$source_code == "96"], "other")
  expect_identical(cw$na_reason[cw$variable == "q5" & cw$source_code == "5"], "ineligible")
  expect_identical(unique(cw$origin), c("map", "gate"))
  num <- qes_spec("crosswalk", targets = "lr_self", studies = "qes2014", level = "code")
  expect_identical(num$origin, c("range", "na_codes", "na_codes"))
  expect_identical(num$source_code[1], "0-10")
  rh <- qes_spec("crosswalk", targets = "sov_indep", studies = "qes2014", format = "retroharmonize")
  expect_identical(names(rh), c("id", "filename", "var_name_orig", "var_name_target", "val_numeric_orig",
                                "val_numeric_target", "val_label_orig", "val_label_target", "na_label_orig",
                                "na_label_target", "class_orig", "class_target"))
  expect_identical(rh$val_numeric_orig, c(1, 2, 8, 9))
  expect_identical(rh$na_label_target, c(NA, NA, "dk", "refused"))
  expect_identical(rh$filename[1], "Quebec Election Study 2014.sav")
})

test_that("qes_spec() checks its arguments against the view", {
  expect_error(qes_spec("targets", level = "code"), class = "qesR_error_input")
  expect_error(qes_spec("spec", format = "retroharmonize"), class = "qesR_error_input")
  expect_error(qes_spec("spec", targets = "core"), class = "qesR_error_input")
  expect_error(qes_spec("crosswalk", format = "retroharmonize", level = "row"), class = "qesR_error_input")
  expect_error(qes_spec(targets = "no_such_target"), class = "qesR_error_input")
  expect_error(qes_spec(studies = "qes2019"), class = "qesR_error_unknown_study")
  expect_error(qes_spec("crosswalk", data = list()), class = "qesR_error_input")
})

test_that("printing a single-target crosswalk view shows its reference section", {
  withr::local_options(qesR.lang = "en")
  out <- capture.output(print(qes_spec("crosswalk", targets = "sov_indep")))
  expect_match(out[1], "^## `sov_indep`: Referendum vote: independent country")
  expect_true(any(grepl("| qes2012 | `q52` (post) | `identical` (anchor", out, fixed = TRUE)))
  fr <- capture.output(print(qes_spec("crosswalk", targets = "sov_indep", lang = "fr")))
  expect_match(fr[1], "pays ind\u00e9pendant")
  # several targets print as a table
  expect_false(any(grepl("^## ", capture.output(print(qes_spec("crosswalk", targets = "sovereignty"))))))
})

test_that("the generated reference covers every target, in English and French alike", {
  en <- .spec_reference_md("en")
  fr <- .spec_reference_md("fr")
  s <- hz_spec()
  for (t in s$tables$targets$target) {
    expect_true(grepl(sprintf("#### `%s`:", t), en, fixed = TRUE), label = t)
    expect_true(grepl(sprintf("#### `%s`\u00a0:", t), fr, fixed = TRUE), label = t)
  }
  lines_en <- strsplit(en, "\n", fixed = TRUE)[[1]]
  lines_fr <- strsplit(fr, "\n", fixed = TRUE)[[1]]
  # same structure: headings and table rows line up
  expect_identical(length(lines_en), length(lines_fr))
  expect_identical(grepl("^#", lines_en), grepl("^#", lines_fr))
  expect_identical(grepl("^\\|", lines_en), grepl("^\\|", lines_fr))
  expect_true(grepl(s$version, en, fixed = TRUE))
  expect_true(grepl(s$hash, fr, fixed = TRUE))
  # every mapped row appears in a coverage table; the licence keeps qes2022 wording out
  xw <- s$tables$crosswalk
  expect_identical(sum(grepl("^\\| qes", lines_en)), sum(xw$rule != "none"))
  expect_true(grepl("document 7449514, p.68", en, fixed = TRUE))
  # structural zeros and review status are marked
  expect_true(grepl("not offered: PVQ, PCQ, ON, ADQ", en, fixed = TRUE))
  expect_true(grepl("(in review)", en, fixed = TRUE))
  expect_true(grepl("(en r\u00e9vision)", fr, fixed = TRUE))
  # French typography: a no-break space before each colon, none before in English
  expect_false(grepl("\u00a0:", en, fixed = TRUE))
  expect_true(grepl("**Plage valide**\u00a0: 0-10", fr, fixed = TRUE))
  # the reference does not depend on the locale
  withr::with_locale(c(LC_COLLATE = "C", LC_CTYPE = "C"), {
    expect_identical(.spec_reference_md("fr"), fr)
  })
})

test_that("the target list of ?qes_spec is generated from the spec", {
  rd <- .rd_targets()
  expect_match(rd[1], "^@section Targets in the shipped spec")
  for (t in hz_spec()$tables$targets$target) {
    expect_true(any(grepl(sprintf("\\code{%s}}", t), rd, fixed = TRUE)), label = t)
  }
  page <- paste(as.character(qesR_rd_db()[["qes_spec.Rd"]]), collapse = "")
  for (t in hz_spec()$tables$targets$target) {
    expect_true(grepl(t, page, fixed = TRUE), label = t)
  }
  expect_true(grepl(hz_spec()$version, page, fixed = TRUE))
})

test_that("qes_search() gives the targets a variable feeds, and searches them", {
  hits <- qes_search("^Q19$", studies = "qes2014", fields = "variable", regex = TRUE)
  expect_identical(hits$targets, "sov_indep")
  t <- qes_search("independent country", studies = "qes2014", fields = "target")
  expect_identical(t$variable, "Q19")
  expect_identical(t$matched_in, "target")
  # French target labels: "Vote provincial (rappel)", "A vot\u00e9 ... (rappel)"
  fr <- qes_search("rappel", studies = "qes2012", fields = "target")
  expect_identical(fr$variable, c("q21", "q25"))
  # the target labels are searched in the language(s) lang asks for
  expect_identical(qes_search("rappel", studies = "qes2012", fields = "target", lang = "fr")$variable,
                   c("q21", "q25"))
  expect_identical(nrow(qes_search("rappel", studies = "qes2012", fields = "target", lang = "en")), 0L)
  expect_identical(qes_search("independent country", studies = "qes2014", fields = "target", lang = "en")$variable,
                   "Q19")
  expect_identical(nrow(qes_search("independent country", studies = "qes2014", fields = "target", lang = "fr")), 0L)
  # target names are searched in either language
  expect_identical(qes_search("sov_indep", studies = "qes2014", fields = "target", lang = "fr")$variable, "Q19")
  none <- qes_search("^Q1$", studies = "qes2014", fields = "variable", regex = TRUE)
  expect_true(all(is.na(none$targets)))
  demo <- qes_search("^Q3$", studies = "qes_demo", fields = "variable", regex = TRUE)
  expect_identical(demo$targets, "vote_prov_recall")
  # qes_codebook() gives the same targets as qes_search()
  cb <- qes_codebook("qes_demo")
  expect_identical(cb$targets[cb$variable == "Q3"], "vote_prov_recall")
  expect_true(any(is.na(cb$targets)))
  hits <- qes_search(".", studies = "qes_demo", fields = "variable", regex = TRUE)
  expect_identical(cb$targets[match(hits$variable, cb$variable)], hits$targets)
  cb14 <- qes_codebook("qes2014", layout = "wide")
  expect_identical(cb14$targets[cb14$variable == "Q19"], "sov_indep")
})
