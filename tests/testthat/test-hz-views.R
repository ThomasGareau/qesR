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
  expect_identical(f$target, c("vote_prov_recall", "vote_prov_intent", "vote_prov_intent_push", "turnout_prov_recall",
                               "turnout_prov_likely"))
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

# ---- the coverage grid of the website (design.md section 9.1, slice W.1) ------------

# The rows of the markdown table that follows heading `title` in `md`, as a
# list of character vectors (header and separator rows dropped).
md_table_rows <- function(md, title) {
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  start <- match(title, lines)
  expect_false(is.na(start), label = title)
  lines <- lines[-seq_len(start)]
  lines <- lines[cumsum(!grepl("^\\|", lines) & cumsum(grepl("^\\|", lines)) > 0) == 0]
  lines <- lines[grepl("^\\|", lines)][-(1:2)]
  lapply(strsplit(sub("^\\| (.*) \\|$", "\\1", lines), " | ", fixed = TRUE), trimws)
}

test_that("the reference gives every target a fixed anchor, in English and French", {
  s <- hz_spec()
  for (lang in c("en", "fr")) {
    ref <- .spec_reference_md(lang)
    for (t in s$tables$targets$target) {
      expect_true(any(grepl(sprintf("^#### `%s`.*\\{#target-%s\\}$", t, t),
                            strsplit(ref, "\n", fixed = TRUE)[[1]], perl = TRUE)), label = t)
    }
  }
  # printing one target (no header) gives no anchor, so the console shows none
  withr::local_options(qesR.lang = "en")
  out <- capture.output(print(qes_spec("crosswalk", targets = "sov_indep")))
  expect_false(any(grepl("{#target-", out, fixed = TRUE)))
})

test_that("the coverage grid gives each target's grade in each study, as qes_spec() does", {
  v <- qes_spec()
  s <- hz_spec()
  md <- .spec_coverage_md("en")
  rows <- md_table_rows(md, "## Targets by study")
  # a row per block, with the block's name only, before its targets
  is_block <- vapply(rows, function(r) grepl("^\\*\\*", r[1]), logical(1))
  blocks <- unique(s$tables$targets$block)
  expect_identical(sum(is_block), length(blocks))
  expect_true(is_block[1])
  expect_true(all(vapply(rows[is_block], function(r) all(r[-1] == ""), logical(1))))
  rows <- rows[!is_block]
  expect_length(rows, nrow(s$tables$targets))
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  header <- strsplit(lines[grep("^\\| Target \\|", lines)], " | ", fixed = TRUE)[[1]]
  studies <- sub(" \\|$", "", header[-1])
  expect_setequal(studies, names(v)[-(1:9)])
  xw <- s$tables$crosswalk
  multi <- names(which(table(s$tables$waves$study) > 1L))
  for (r in rows) {
    t <- sub("^\\[`([a-z0-9_]+)`\\].*$", "\\1", r[1])
    expect_identical(r[1], sprintf("[`%s`](harmonization-reference.html#target-%s)<br>%s", t, t,
                                   s$tables$targets$label_en[s$tables$targets$target == t]))
    for (k in seq_along(studies)) {
      cell <- r[1 + k]
      g <- v[[studies[k]]][v$target == t]
      if (is.na(g)) {
        expect_identical(cell, "\u2014", label = paste(t, studies[k]))
        next
      }
      expect_true(startsWith(cell, .qes_enum_label("grade", g, "en")), label = paste(t, studies[k]))
      row <- xw[xw$study == studies[k] & xw$target == t & xw$rule != "none", , drop = FALSE]
      if (studies[k] %in% multi) {
        expect_true(grepl(sprintf("(%s)", .qes_wave_label(row$wave, "en", row$study, s)), cell, fixed = TRUE))
      }
      zero <- .qes_not_offered(s, row)
      expect_identical(endsWith(cell, "\\*"), !is.na(zero) && nzchar(zero), label = paste(t, studies[k]))
    }
  }
  # a few cells, by hand
  cell <- function(t, study) rows[[match(t, vapply(rows, function(r) sub("^\\[`([a-z0-9_]+)`\\].*$", "\\1", r[1]), ""))]][1 + match(study, studies)]
  expect_identical(cell("vote_prov_recall", "qes2018"), "Comparable \\*")
  expect_identical(cell("vote_prov_recall", "qes2022"), "Comparable (pes) \\*")
  expect_identical(cell("turnout_prov_recall", "qes2014"), "Comparable")
  expect_identical(cell("sov_indep", "qes2018_panel"), "\u2014")
  # every mapped row is a cell; none is signed off yet, and the page says so
  expect_identical(sum(vapply(rows, function(r) sum(r[-1] != "\u2014"), 0L)), sum(xw$rule != "none"))
  if (all(xw$status[xw$rule != "none"] != "stable")) {
    expect_true(grepl(sprintf("All %d cells use crosswalk rows", sum(xw$rule != "none")), md, fixed = TRUE))
  }
  expect_true(grepl(s$version, md, fixed = TRUE))
  expect_true(grepl(s$hash, md, fixed = TRUE))
})

test_that("the coverage grid lists each study's waves, weights and grades", {
  s <- hz_spec()
  md <- .spec_coverage_md("en")
  rows <- md_table_rows(md, "## Studies")
  studies <- gsub("`", "", vapply(rows, `[`, "", 1))
  expect_setequal(studies, unique(s$tables$waves$study))
  xw <- s$tables$crosswalk
  xw <- xw[xw$rule != "none" & xw$primary %in% TRUE, , drop = FALSE]
  for (r in rows) {
    st <- gsub("`", "", r[1])
    g <- xw$grade[xw$study == st]
    # the targets of the study (its primary rows, one per target)
    expect_identical(as.integer(r[3]), length(unique(xw$target[xw$study == st])))
    expect_identical(as.integer(r[4:6]), vapply(c("identical", "comparable", "approximate"), function(x) sum(g == x), 0L,
                                                USE.NAMES = FALSE))
  }
  waves <- setNames(vapply(rows, `[`, "", 2), studies)
  expect_identical(waves[["qes2022"]], "cps (n = 1,521): `cps_weight_general`; pes (n = 1,220): `pes_weight_general`")
  expect_identical(waves[["qes2018_panel"]], "pre (n = 1,250): `weight`; post (n = 842): `weight_rts`")
  expect_match(waves[["qes2012_panel"]], "`pondam1` (needs review, not applied)", fixed = TRUE)
  expect_match(waves[["qes2007_panel"]], "post (n = 2,054): `pond_tot_am1` (needs review, not applied)", fixed = TRUE)
  # both qes2008 weights are calibrated on vote or turnout: none is recommended
  expect_match(waves[["qes2008"]], "post (n = 1,151): no recommended weight", fixed = TRUE)
  # the catalog's studies without a spec row are named
  others <- setdiff(.qes_study_codes(), studies)
  expect_true(length(others) > 0L)
  expect_true(grepl(paste0("`", others, "`", collapse = ", "), md, fixed = TRUE))
})

test_that("the reference marks the recommended weights that need review, as the coverage grid does", {
  s <- hz_spec()
  en <- paste(.spec_reference_md("en", spec = s, targets = "vote_prov_recall", header = FALSE), collapse = "\n")
  fr <- paste(.spec_reference_md("fr", spec = s, targets = "vote_prov_recall", header = FALSE), collapse = "\n")
  # qes2012's weight is reviewed; the 2012 panel's is not, and is not applied
  expect_match(en, "| `pond` |", fixed = TRUE)
  expect_match(en, "`pond_post` (needs review, not applied)", fixed = TRUE)
  expect_match(fr, "`pond_post` (\u00e0 r\u00e9viser, non appliqu\u00e9e)", fixed = TRUE)
  expect_false(grepl("| pond_post |", en, fixed = TRUE))
})

test_that("the coverage grid has the same shape in English and French, whatever the locale", {
  en <- .spec_coverage_md("en")
  fr <- .spec_coverage_md("fr")
  lines_en <- strsplit(en, "\n", fixed = TRUE)[[1]]
  lines_fr <- strsplit(fr, "\n", fixed = TRUE)[[1]]
  expect_identical(length(lines_en), length(lines_fr))
  expect_identical(grepl("^#", lines_en), grepl("^#", lines_fr))
  expect_identical(grepl("^\\|", lines_en), grepl("^\\|", lines_fr))
  # the dashes (no question) are in the same cells
  dashes <- function(rows) lapply(rows, function(r) r == "\u2014")
  expect_identical(dashes(md_table_rows(en, "## Targets by study")),
                   dashes(md_table_rows(fr, "## Cibles par \u00e9tude")))
  # French links go to the French reference, with the same anchors
  expect_true(grepl("(fr-reference-harmonisation.html#target-sov_indep)", fr, fixed = TRUE))
  expect_false(grepl("(harmonization-reference.html", fr, fixed = TRUE))
  expect_true(grepl("Identique (cps)", fr, fixed = TRUE))
  expect_true(grepl("(n = 1\u00a0521)\u00a0: `cps_weight_general`\u00a0; pes", fr, fixed = TRUE))
  expect_false(grepl("\u00a0:", en, fixed = TRUE))
  withr::with_locale(c(LC_COLLATE = "C", LC_CTYPE = "C"), {
    expect_identical(.spec_coverage_md("fr"), fr)
    expect_identical(.spec_coverage_md("en"), en)
  })
  # another reference page can be named
  expect_true(grepl("(ref.html#target-age)", .spec_coverage_md("en", reference = "ref.html"), fixed = TRUE))
})

test_that("the coverage grid counts the rows qes_harmonize() applies, and says which are drafts", {
  s <- hz_spec()
  xw <- s$tables$crosswalk
  mapped <- which(xw$rule != "none" & xw$primary %in% TRUE)
  # one draft row, and a second, non-primary row for the same study and target
  i <- mapped[1]
  xw$status[i] <- "draft"
  extra <- xw[i, , drop = FALSE]
  extra$primary <- FALSE
  extra$status <- "review"
  s$tables$crosswalk <- rbind(xw, extra)
  n <- length(mapped)
  for (lang in c("en", "fr")) {
    md <- .spec_coverage_md(lang, spec = s)
    key <- function(k) .qes_rt(k, lang)
    n_review <- sum(xw$status[mapped] == "review")
    n_stable <- sum(xw$status[mapped] == "stable")
    expect_true(grepl(sprintf(key("cov_status_note"), n_review, n), md, fixed = TRUE), label = lang)
    expect_true(grepl(sprintf(key("cov_stable_note"), n_stable, n), md, fixed = TRUE), label = lang)
    expect_true(grepl(sprintf(key("cov_draft_note"), 1L, n), md, fixed = TRUE), label = lang)
    expect_false(grepl(sprintf(key("cov_status_all"), n), md, fixed = TRUE), label = lang)
  }
  md <- .spec_coverage_md("en", spec = s)
  rows <- md_table_rows(md, "## Studies")
  st <- xw$study[i]
  r <- rows[[match(paste0("`", st, "`"), vapply(rows, `[`, "", 1))]]
  expect_identical(as.integer(r[3]), sum(xw$study[mapped] == st))
})

test_that("the home page grid links each target to its section of the reference", {
  plain <- strsplit(.spec_readme_md(hz_spec()), "\n", fixed = TRUE)[[1]]
  linked <- strsplit(.spec_readme_md(hz_spec(), reference = "articles/ref.html"), "\n", fixed = TRUE)[[1]]
  expect_identical(length(linked), length(plain))
  expect_identical(linked[2], plain[2])
  # the same study codes, which may wrap only after "qes" and at "_"
  expect_identical(gsub("<wbr>|</?code>", "", linked[1]), gsub("`", "", plain[1]))
  expect_false(grepl("<wbr>[^_q]*<wbr>[0-9]", linked[1]))
  targets <- sub("^\\| `([a-z0-9_]+)` \\|.*$", "\\1", plain[-(1:2)])
  expect_identical(linked[-(1:2)],
                   sprintf("| [`%s`](articles/ref.html#target-%s) |%s", targets, targets,
                           sub("^\\| `[a-z0-9_]+` \\|", "", plain[-(1:2)])))
  # the anchors are those of the reference sections
  ref <- .spec_reference_md("en", spec = hz_spec())
  for (t in targets) expect_match(ref, sprintf("{#target-%s}", t), fixed = TRUE)
})

test_that("the README grid gives the first letter of each grade qes_spec() gives", {
  v <- qes_spec()
  md <- .spec_readme_md(hz_spec())
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  header <- strsplit(sub("^\\| (.*) \\|$", "\\1", lines[1]), " | ", fixed = TRUE)[[1]]
  studies <- gsub("`", "", trimws(header[-1]))
  expect_setequal(studies, names(v)[-(1:9)])
  rows <- lapply(strsplit(sub("^\\| (.*) \\|$", "\\1", lines[-(1:2)]), " | ", fixed = TRUE), trimws)
  expect_setequal(vapply(rows, function(r) gsub("`", "", r[1]), ""), v$target)
  letter <- c(identical = "I", comparable = "C", approximate = "A")
  for (r in rows) {
    t <- gsub("`", "", r[1])
    g <- unlist(v[v$target == t, studies], use.names = FALSE)
    expect_identical(r[-1], ifelse(is.na(g), "—", letter[g]), label = t)
  }
})
