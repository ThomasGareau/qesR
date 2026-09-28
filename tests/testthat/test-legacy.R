# The legacy renderer (R/legacy.R, the spec's legacy.csv; design.md section
# 5.12, slice HZ6): the table, its validator rule V-S18, and each render.

legacy_tab <- function(profile) qesR:::.qes_legacy_table(profile, hz_spec())

test_that("legacy.csv is part of the spec, UTF-8 with LF endings, and passes V-S18", {
  path <- file.path(hz_spec_dir(), "legacy.csv")
  bytes <- file_bytes(path)
  expect_false(any(bytes == as.raw(0x0d)))
  expect_true(validUTF8(rawToChar(bytes)))
  s <- hz_spec()
  expect_identical(names(s$tables$legacy), names(qesR:::.qes_schemas$spec_legacy))
  p <- .qes_spec_check(s)
  expect_false(any(p$rule == "V-S18"))
  # the content hash covers it: an edit changes the hash
  t <- s$tables
  t$legacy$definition[1] <- "edited"
  expect_false(identical(qesR:::.qes_spec_tables_hash(t), s$hash))
})

test_that("the master profile has the 30 columns of 0.4.4 in order and type, then the appended ones", {
  cols <- qesR:::.qes_legacy_columns("master", hz_spec())
  n <- length(v044_master_cols)
  expect_identical(names(cols)[seq_len(n)], names(v044_master_cols))
  expect_identical(unname(cols[seq_len(n)]), unname(v044_master_cols))
  expect_identical(names(cols)[n + 1:2], c("vote_choice_timing", "sovereignty_item"))
  expect_identical(names(qesR:::.qes_legacy_columns("decon", hz_spec())), v044_decon_cols)
})

test_that("every legacy target is a target of the spec, and every study row names catalog studies", {
  s <- hz_spec()
  lg <- s$tables$legacy
  t <- unique(unlist(lapply(lg$target, qesR:::.qes_split_list)))
  expect_true(all(t %in% s$tables$targets$target))
  st <- unique(unlist(lapply(lg$studies, qesR:::.qes_split_list)))
  expect_true(all(st %in% v044_qescodes))
  # the survey weight of 0.4.4 (OD6), study by study
  w <- lg[lg$profile == "master" & lg$column == "survey_weight" & !is.na(lg$studies), , drop = FALSE]
  expect_setequal(w$studies, v044_qescodes)
  expect_true(all(startsWith(w$render, "raw:")))
  # the qes2022 turnout and vote of get_decon() are the campaign-period items (OD9)
  d <- lg[lg$profile == "decon" & lg$studies %in% "qes2022", , drop = FALSE]
  expect_identical(d$target[d$column == "turnout"], "turnout_prov_likely")
  expect_identical(d$target[d$column == "votechoice"], "vote_prov_intent")
  # the master's turnout and vote are the reported ones (OD4), in every study
  m <- lg[lg$profile == "master", , drop = FALSE]
  expect_identical(unique(m$target[m$column == "vote_choice"]), "vote_prov_recall")
  expect_identical(unique(m$target[m$column == "turnout"]), "turnout_prov_recall")
  expect_identical(unique(m$target[m$column == "sovereignty_support"]), "sov_indep")
  # the study rows of those columns only carry the decision (OD4, OD5), by a
  # descriptive name
  k <- m$column %in% c("vote_choice", "turnout") & !is.na(m$studies)
  expect_identical(unique(m$cause[k]), "reported_vote_only")
  expect_identical(unique(m$studies[k]), "qes_crop_2007_2010")
  k <- m$column %in% c("sovereignty_support", "sovereignty") & !is.na(m$studies)
  expect_identical(unique(m$cause[k]), "independence_question_only")
  # qes2007_panel asked the 1995 question (sov_partnership_1995), like qes2007
  k <- m$column == "sovereignty_support" & grepl("qes2007_panel", m$studies)
  expect_match(m$note[k], "1995 question", fixed = TRUE)
  expect_true(grepl("qes2007", m$studies[k], fixed = TRUE))
  # no shipped text cites the design's internal decision ids
  for (col in c("cause", "definition", "note")) {
    expect_false(any(grepl("\\bOD[0-9]+\\b|A:H[0-9]", lg[[col]])), info = col)
  }
})

test_that("V-S18 finds a broken renderer table", {
  s <- hz_spec()
  broken <- function(edit) {
    s2 <- s
    s2$tables$legacy <- edit(s2$tables$legacy)
    p <- .qes_spec_check(s2)
    p[p$rule == "V-S18", , drop = FALSE]
  }
  row <- function(lg, col, profile = "master") which(lg$profile == profile & lg$column == col & is.na(lg$studies))
  expect_match(broken(function(lg) { lg$target[row(lg, "gender")] <- "gendr"; lg })$detail, "unknown target")
  expect_match(broken(function(lg) { lg$render[row(lg, "gender")] <- "bogus"; lg })$detail, "unknown render")
  # a recode must name every level of the target
  expect_match(broken(function(lg) { lg$render[row(lg, "gender")] <- "recode:man=Man;woman=Woman"; lg })$detail,
               "does not fit")
  # int01 needs a yes/no target; legacy_party a party set
  expect_match(broken(function(lg) { lg$target[row(lg, "turnout")] <- "gender"; lg })$detail, "does not fit")
  expect_match(broken(function(lg) { lg$target[row(lg, "vote_choice")] <- "gender"; lg })$detail, "does not fit")
  # a render without a target, a target on a render that takes none
  expect_match(broken(function(lg) { lg$target[row(lg, "gender")] <- NA; lg })$detail, "needs a target")
  expect_match(broken(function(lg) { lg$target[row(lg, "qes_code")] <- "gender"; lg })$detail, "takes no target")
  # positions number the columns without gaps; each column has one default row
  expect_match(broken(function(lg) { lg$position[lg$column == "survey_weight"] <- 99L; lg })$detail, "positions")
  expect_match(broken(function(lg) { lg$studies[row(lg, "gender")] <- "qes2018"; lg })$detail, "one default row")
  expect_match(broken(function(lg) {
    lg$studies[lg$profile == "master" & lg$column == "survey_weight" & lg$studies %in% "qes2018"] <- "qes2019"
    lg
  })$detail, "unknown study")
  # the stacked master has one type per column; get_decon() may take one per study
  expect_match(broken(function(lg) {
    k <- which(lg$profile == "master" & lg$column == "survey_weight" & lg$studies %in% "qes2018")
    lg$type[k] <- "character"
    lg
  })$detail, "type")
  expect_identical(nrow(broken(identity)), 0L)
  expect_match(broken(function(lg) { lg$render[row(lg, "vote_choice_timing")] <- "timing:no_such_column"; lg })$detail,
               "does not fit")
  # a timing or item column must read a column placed before it
  expect_match(broken(function(lg) { lg$render[row(lg, "vote_choice_timing")] <- "timing:vote_intent"; lg })$detail,
               "does not fit")
  expect_match(broken(function(lg) {
    lg$render[row(lg, "sovereignty_item")] <- "item:sov_partnership_1995"
    lg
  })$detail, "does not fit")
})

test_that("each study takes its own row of a column, qes_demo those of qes2014", {
  lg <- legacy_tab("master")
  r <- qesR:::.qes_legacy_rows(lg, "qes2022")
  expect_identical(r$column, unique(lg$column))
  expect_identical(r$render[r$column == "interview_start"], "raw:cps_StartDate")
  expect_identical(r$render[r$column == "survey_weight"], "raw:cps_weight_general")
  expect_identical(qesR:::.qes_legacy_rows(lg, "qes1998")$render[r$column == "language"], "constant:French")
  expect_identical(qesR:::.qes_legacy_rows(lg, "qes2018")$render[r$column == "language"], "level_en")
  demo <- qesR:::.qes_legacy_rows(lg, "qes_demo")
  expect_identical(demo$render[demo$column == "survey_weight"], "raw:POND")
  expect_identical(demo$render[demo$column == "interview_start"], "raw:SDAT")
})

# A harmonized study as the renderer sees it (values = "code"), with its
# cell provenance: `included` lists the targets the study has.
fake_h <- function(study = "qes2014", included = character(0), ...) {
  cols <- list(...)
  n <- if (length(cols) > 0L) length(cols[[1]]) else 3L
  h <- data.frame(study = rep(study, n), year = rep(2014L, n), qes_id = paste0(study, ":", seq_len(n)),
                  family = rep("qes", n), stringsAsFactors = FALSE)
  for (nm in names(cols)) h[[nm]] <- cols[[nm]]
  k <- length(included)
  cell <- data.frame(study = rep(study, k), target = included, included = rep(TRUE, k),
                     source_var = sprintf("v_%s", included), map_id = rep(NA_character_, k), grade = rep("comparable", k),
                     stringsAsFactors = FALSE)
  list(h = h, cell = cell)
}
render <- function(render, x, target = NA_character_, type = "character", raw = NULL, filled = list()) {
  r <- data.frame(profile = "master", column = "col", studies = NA_character_, target = target,
                  render = render, type = type, stringsAsFactors = FALSE)
  qesR:::.qes_legacy_render_column(r, x$h, x$cell, raw, hz_spec(), filled)
}

test_that("party labels are those of qesR 0.4.4, and missing values stay NA", {
  x <- fake_h(included = "vote_prov_recall", vote_prov_recall = c("PLQ", "other", NA, "ADQ"))
  expect_identical(render("legacy_party", x, "vote_prov_recall")$value, c("PLQ", "Other party", NA, "ADQ"))
  x <- fake_h(included = "vote_prov_intent", vote_prov_intent = c("no_party", "QS", "CAQ"))
  expect_identical(render("legacy_party", x, "vote_prov_intent")$value, c("Did not vote / None", "QS", "CAQ"))
  x <- fake_h(included = "pid_fed", pid_fed = c("LPC", "BQ", "none", "PPC"))
  expect_identical(render("legacy_party", x, "pid_fed")$value, c("Liberal", "Bloc Quebecois", "Did not vote / None", "PPC"))
})

test_that("int01, scale10 and recode renders", {
  x <- fake_h(included = "sov_indep", sov_indep = c("yes", "no", "would_not_vote", NA))
  expect_identical(render("int01", x, "sov_indep", "numeric")$value, c(1, 0, NA, NA))
  # OD7: four-point interest on 0-10; 0-10 items as answered
  x <- fake_h(included = "interest_4pt", interest_4pt = c("very", "quite", "hardly", "not_at_all", NA))
  expect_identical(render("scale10", x, "interest_4pt;interest_0_10", "numeric")$value, c(10, 7, 3, 0, NA))
  x <- fake_h(included = "interest_0_10", interest_0_10 = c(4, 9, NA))
  expect_identical(render("scale10", x, "interest_4pt;interest_0_10", "numeric")$value, c(4, 9, NA))
  x <- fake_h(included = "gender", gender = c("man", "woman", "nonbinary", "other"))
  expect_identical(render("recode:man=Man;woman=Woman;nonbinary=Non-binary;other=Other", x, "gender")$value,
                   c("Man", "Woman", "Non-binary", "Other"))
  expect_identical(render("level_en", x, "gender")$value, c("Man", "Woman", "Non-binary", "Another gender"))
  f <- render("factor", x, "gender", "factor")$value
  expect_s3_class(f, "factor")
  expect_identical(levels(f), c("Man", "Woman", "Non-binary", "Another gender"))
  # a study without the target: an empty factor with the target's levels
  y <- fake_h(included = character(0), gender = rep(NA_character_, 3))
  f <- render("factor", y, "gender", "factor")$value
  expect_true(all(is.na(f)))
  expect_identical(levels(f), c("Man", "Woman", "Non-binary", "Another gender"))
})

test_that("age in years and age bands follow qesR 0.4.4, from the targets", {
  x <- fake_h(included = c("age", "birth_year"), age = c(40, NA, NA, NA, 200),
              birth_year = c(1960, 1990, NA, 1999, NA))
  # the age item, else the study's year minus the year of birth
  expect_identical(render("age_years", x, "age;birth_year", "numeric")$value, c(40, 24, NA, 15, NA))
  bands <- render("age_bands", x, "age;birth_year;age_group6;age_group3")$value
  expect_identical(bands, c("35-44", "18-24", NA, NA, NA))
  # the study's own bands where no age is known: six, else three
  y <- fake_h(included = c("age_group6", "age_group3"), age_group6 = c("a65_plus", NA, NA),
              age_group3 = c("a55_plus", "a18_34", NA))
  expect_identical(render("age_bands", y, "age;birth_year;age_group6;age_group3")$value, c("65+", "18-34", NA))
  z <- fake_h(included = "age_group3", age_group3 = c("a35_54", "a55_plus", NA))
  expect_identical(render("age_bands", z, "age;birth_year;age_group6;age_group3")$value, c("35-54", "55+", NA))
})

test_that("catalog, lead, id, raw, constant, timing and item renders", {
  x <- fake_h("qes2018", included = "vote_prov_recall", vote_prov_recall = c("PQ", "CAQ", NA))
  expect_identical(render("catalog:study", x)$value, rep("qes2018", 3))
  expect_identical(render("catalog:name_en", x)$value, rep("Quebec Election Study 2018", 3))
  expect_identical(render("lead:year", x)$value, rep("2014", 3))
  expect_identical(render("lead:family", x)$value, rep("qes", 3))
  expect_identical(render("id", x)$value, c("1", "2", "3"))
  # a study identified by its row (qes2014): <study>_<row>, as in 0.4.4
  expect_identical(render("id", fake_h("qes2014"))$value, c("qes2014_1", "qes2014_2", "qes2014_3"))
  raw <- data.frame(w = c(1.5, 0.5, 1), d = as.POSIXct(c("2022-09-19 14:17:02", NA, "2022-09-20 01:00:00"), tz = "UTC"),
                    n = c(20140409, 20140410, NA))
  expect_identical(render("raw:w", x, type = "numeric", raw = raw)$value, c(1.5, 0.5, 1))
  expect_identical(render("raw:d", x, raw = raw)$value, c("2022-09-19 14:17:02", NA, "2022-09-20 01:00:00"))
  expect_identical(render("raw:n", x, raw = raw)$value, c("20140409", "20140410", NA))
  expect_identical(render("raw:absent", x, raw = raw)$value, rep(NA_character_, 3))
  expect_identical(render("constant:Quebec", x)$value, rep("Quebec", 3))
  expect_identical(render("na_column", x, type = "numeric")$value, rep(NA_real_, 3))
  filled <- list(vote_choice = "vote_prov_recall", sovereignty_support = NA_character_)
  expect_identical(render("timing:vote_choice", x, filled = filled)$value, rep("post", 3))
  expect_identical(render("item:vote_choice", x, filled = filled)$value, rep("vote_prov_recall", 3))
  expect_identical(render("item:sovereignty_support", x, filled = filled)$value, rep(NA_character_, 3))
  # the first target the study has is the one used, and recorded
  out <- render("legacy_party", x, "vote_prov_recall")
  expect_identical(out$target, "vote_prov_recall")
  expect_identical(render("legacy_party", fake_h("qes2018"), "vote_prov_recall")$target, NA_character_)
})

test_that("changes.csv and removed.csv match their schemas; the column map reads them", {
  dir <- system.file("extdata", "legacy", package = "qesR", mustWork = TRUE)
  expect_setequal(basename(list.files(dir, pattern = "\\.csv$")), c("changes.csv", "removed.csv"))
  for (name in c("changes", "removed")) {
    f <- file.path(dir, paste0(name, ".csv"))
    bytes <- file_bytes(f)
    expect_false(any(bytes == as.raw(0x0d)), info = name)
    expect_true(validUTF8(rawToChar(bytes)), info = name)
    expect_identical(names(qesR:::.qes_legacy_file(name)), names(qesR:::.qes_schemas[[paste0("legacy_", name)]]))
  }
  ch <- qesR:::.qes_legacy_changes()
  expect_true(all(ch$study %in% v044_qescodes))
  expect_setequal(unique(ch$profile), c("master", "decon"))
  # causes are descriptive names (never the design's internal ids), or 0.5.0
  causes <- unique(unlist(strsplit(ch$cause, ";", fixed = TRUE)))
  expect_true(all(causes %in% c("0.5.0", "harmonization_engine", "all_rows_kept", "reported_vote_only",
                                "interest_on_0_10", "two_first_languages", "campaign_period_vote",
                                "scale_corrected", "coding_error_fixed", "other_question_fixed",
                                "not_signed_off", "review_correction")))
  expect_length(qesR:::.qes_legacy_removed()$column, 70L)
  map <- qesR:::.qes_legacy_column_map("master")
  expect_identical(names(map), c("column", "target", "definition", "studies_changed", "flag", "note", "render"))
  expect_identical(map$column, names(qesR:::.qes_legacy_columns("master")))
  expect_true(all(!is.na(map$definition)))
  expect_match(map$studies_changed[map$column == "vote_choice"], "qes1998")
  expect_true(is.na(map$studies_changed[map$column == "qes_code"]))
  expect_identical(map$flag[map$column == "political_interest"], "approximate")
})

test_that("the notices of the legacy builders are classed and shown once per session", {
  local_fake_legacy()
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  n_values <- count_class(get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_values_changed")
  expect_identical(n_values, 1L)
  expect_identical(
    count_class(get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_values_changed"),
    0L
  )
  expect_identical(
    count_class(get_decon("qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_values_changed"),
    0L
  )
  local_qes_once()
  m <- NULL
  withCallingHandlers(
    get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE),
    message = function(cnd) {
      if (inherits(cnd, "qesR_message_legacy_columns")) m <<- cnd
      invokeRestart("muffleMessage")
    }
  )
  expect_identical(m$id, "legacy_master_columns")
  expect_identical(m$fn, "get_qes_master")
  expect_identical(
    count_class(get_decon("qes2018", assign_global = FALSE, quiet = TRUE), "qesR_message_legacy_columns"),
    1L
  )
})

test_that("the values-changed note names the columns and attributes of its builder", {
  local_fake_legacy()
  local_qes_once()
  withr::local_options(qesR.quiet_deprecated = TRUE, qesR.lang = "en")
  grab <- function(expr) {
    m <- NULL
    withCallingHandlers(expr, message = function(cnd) {
      if (inherits(cnd, "qesR_message_values_changed")) m <<- cnd
      invokeRestart("muffleMessage")
    })
    m
  }
  # get_decon() first in the session: its own attributes and columns
  d <- NULL
  m <- grab(d <- get_decon("qes2018", assign_global = FALSE, quiet = TRUE))
  expect_identical(m$id, "legacy_values_changed_decon")
  expect_identical(m$fn, "get_decon")
  msg <- conditionMessage(m)
  expect_match(msg, "source_map", fixed = TRUE)
  expect_match(msg, "votechoice", fixed = TRUE)
  expect_false(grepl("legacy_column_map", msg, fixed = TRUE))
  expect_false(grepl("vote_choice", msg, fixed = TRUE))
  expect_false(grepl("vote_intent", msg, fixed = TRUE))
  named <- regmatches(msg, gregexpr('attr\\(, "[a-z_]+"\\)', msg))[[1]]
  named <- sub('^attr\\(, "([a-z_]+)"\\)$', "\\1", named)
  expect_true(length(named) > 0L)
  expect_true(all(named %in% names(attributes(d))))
  # shared once key: get_qes_master() shows nothing more this session
  expect_null(grab(get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE)))
  # get_qes_master() first: the master's text
  local_qes_once()
  m <- grab(get_qes_master(surveys = "qes2018", assign_global = FALSE, quiet = TRUE))
  expect_identical(m$id, "legacy_values_changed")
  expect_match(conditionMessage(m), "legacy_column_map", fixed = TRUE)
})
