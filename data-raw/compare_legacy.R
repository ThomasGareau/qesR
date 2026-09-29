# Compare the legacy builders with qesR 0.4.4 and 0.5.0 (design.md sections
# 5.11 and 5.12; the gate of slice HZ6, the legacy switch of qesR 0.7.0).
#
# Usage (from the package root):
#   QESR_LEGACY_BASELINE=<dir044> QESR_LEGACY_BASELINE_050=<dir050> \
#     [QESR_TEST_DATA_DIR=<cache>] Rscript data-raw/compare_legacy.R [--check]
#
# <dir044> is the output directory of the R9 baseline run (qesR 0.4.4 at
# v0.4.4, fresh session, LANGUAGE=en), holding legacy_master_v044.rds,
# legacy_decon_qes2022.rds and legacy_decon_all_other_studies.rds.
# <dir050> holds legacy_master_050.rds and legacy_decon_050.rds (a named
# list, one get_decon() per study): the interim builders of qesR 0.5.0,
# whose values 0.6.0 kept, run on the same pinned files (for example from
# the redesign branch at commit aa1d3bd). These are files a maintainer's
# own runs wrote; they are read here, in a development script, never by the
# package.
# QESR_TEST_DATA_DIR, when set, is a qesR download cache holding the pinned
# originals (see ?qes_cache_info), used with no request; otherwise the files
# are downloaded politely into the session cache.
#
# It builds get_qes_master() (the 11 studies) and get_decon() (each study)
# with the package in the working directory and compares every cell with
# 0.5.0 (the same rows, in the same order) and with 0.4.4 (after matching
# out the rows 0.4.4 dropped by de-duplication, with the 0.5.0 identifiers).
# A cell that differs from 0.5.0 is to_na (a value became NA), from_na (NA
# became a value), changed or precision (numbers equal within 1e-6); every
# (profile, column, study, kind) with such cells must be listed in
# `explained` below, with its cause, or the gate fails. The differences
# between 0.4.4 and 0.5.0 were the gate of slice S4 and are counted, not
# re-explained.
#
# Writes
#   dev/legacy-diff.md                 the full report (build-ignored);
#   inst/extdata/legacy/changes.csv    per (profile, column, study) whose
#                                      values differ from qesR 0.4.4 (beyond
#                                      precision), the causes; no counts.
#                                      It feeds the studies_changed column
#                                      of attr(, "legacy_column_map");
#   NEWS.md                            the "What changed" table, between the
#                                      lines <!-- legacy-table: start ... -->
#                                      and <!-- legacy-table: end -->: counts
#                                      of values of every study whose
#                                      metadata ships (all of them; the
#                                      counts of qes2022 are CC BY-NC 4.0,
#                                      inst/COPYRIGHTS, section 2).
# With --check it writes nothing and fails when a file differs from what it
# would write. It exits with status 1 if any difference is unexplained.

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args

base <- Sys.getenv("QESR_LEGACY_BASELINE")
base050 <- Sys.getenv("QESR_LEGACY_BASELINE_050")
if (!nzchar(base) || !dir.exists(base)) {
  stop("Set QESR_LEGACY_BASELINE to the output directory of the R9 baseline run.", call. = FALSE)
}
if (!nzchar(base050) || !dir.exists(base050)) {
  stop("Set QESR_LEGACY_BASELINE_050 to the directory of the qesR 0.5.0 legacy builds.", call. = FALSE)
}
if (requireNamespace("pkgload", quietly = TRUE)) {
  pkgload::load_all(".", quiet = TRUE, export_all = FALSE)
} else {
  library(qesR)
}
cache <- Sys.getenv("QESR_TEST_DATA_DIR")
if (nzchar(cache)) {
  options(qesR.cache_dir = cache, qesR.cache = "disk")
}
options(qesR.lang = "en", qesR.quiet_deprecated = TRUE)

ns <- asNamespace("qesR")
legacy_codes <- get(".qes_legacy_codes", envir = ns)
catalog <- get(".qes_catalog", envir = ns)()
# the studies whose counts and means may be published (their metadata
# ships: every study since OD3 was lifted)
shipped <- catalog$studies$study[catalog$studies$metadata_shipped %in% TRUE]

# Differences from qesR 0.5.0, with their cause. `study` "*" means every
# study. Every other difference fails the gate.
explained <- read.csv(text = '
profile,column,study,kind,cause,explanation
master,income,qes2022,to_na,REV,"An amount of 0 is a blank that the survey sent to the bracket follow-up cps_income2: a missing value (spec 4.0.0)"
decon,income,qes2022,to_na,REV,"An amount of 0 is a blank that the survey sent to the bracket follow-up cps_income2: a missing value (spec 4.0.0)"
master,respondent_id,qes2018_panel,changed,HZ6,"The file\'s identifier (interview mode and id, qes_id) instead of a made-up <study>_<row>"
master,respondent_id,qes2007_panel,changed,HZ6,"The project and questionnaire number (nompn-quest, qes_id), unique, instead of quest, which repeats across the two subsamples"
master,*,qes2007_panel,to_na,P5,"The one row in no wave (neither interview completed, by its disposition codes) is not_in_wave"
master,language,qes2014,from_na,OD8,"The mother tongue (QLANG); 0.5.0 blanked the interview language (LANG)"
master,language,qes2007,to_na,OD8,"Two first languages including French or English (codes 4, 5 and 7) are not mapped to one of them"
master,year_of_birth,qes2022,from_na,HZ6,"cps_yob read as the year its value label gives (codes 1-91 are labelled with years)"
master,age,qes2018,from_na,HZ6,"The age item (agenum) of the respondents who gave no year of birth"
master,age_group,qes2018,from_na,HZ6,"Age bands of the respondents who gave their age (agenum) but no year of birth"
master,age_group,qes2008,from_na,HZ6,"The study\'s own age bands (q0age) where the year of birth is missing"
master,age_group,qes2012_panel,changed,HZ6,"Six age bands, which the question has, instead of the producer\'s three-band recode"
master,age_group,qes2007_panel,changed,HZ6,"Six age bands, which the question has, instead of the producer\'s three-band recode"
master,province_territory,qes2018_panel,changed,A:H4,"Quebec for every respondent; 0.4.4 put the region (Couronne, ROQ) in the province column"
master,education,qes2018_panel,changed,A:H4,"The trade certificate (d3 code 4), left as a label by 0.4.4, is College/CEGEP/Technical"
master,education,qes2012,to_na,HZ6,"scol code 10 (Certificate and diploma) is in neither questionnaire: not mappable"
master,education,qes2012,changed,HZ6,"scol code 8 (the French questionnaire\'s cours technique) is College, not University"
master,education,qes2008,changed,A:H4,"maîtrise (q77 code 10), left as a label by 0.4.4, is University"
master,education,qes2007,changed,A:H4,"maîtrise (q77 code 10), left as a label by 0.4.4, is University"
master,income,qes2022,changed,HZ6,"Amounts written in full (100000, not 1e+05)"
master,income,qes2012,from_na,HZ6,"The income brackets of reven (0.4.4 found no source)"
master,born_canada,qes2018,from_na,A:H4,"q69 with Canadian-born people outside Quebec as Yes (0.5.0 blanked the 0.4.4 coding)"
master,political_interest,qes2018,from_na,OD7,"q27 on 0-10 with the corrected conversion (10, 7, 3, 0)"
master,ideology,qes2018,from_na,A:H3,"q36_1 (0.4.4 found no source)"
master,ideology,qes2014,from_na,A:H3,"Q32 with its 0 and 10 answers (0.5.0 blanked the truncated 0.4.4 column)"
master,turnout,qes2022,from_na,OD4,"Reported turnout from the post-election wave (pes_turnout)"
master,turnout,qes2018_panel,from_na,HZ6,"The turnout question (rts_q1) instead of the vote question: 14 who went to vote but did not know for whom are 1"
master,turnout,qes2018_panel,changed,HZ6,"The turnout question (rts_q1) instead of the vote question: 13 who went to vote and spoiled their ballot are 1, not 0"
master,vote_choice,*,to_na,OD4,"Nonvoters, spoiled ballots, don\'t know and refusals are missing values of the reported vote (0.4.4 categories Did not vote / None and Don\'t know / Refused)"
master,vote_choice,qes2022,from_na,OD4,"The reported vote of the post-election wave (pes_votechoice)"
master,vote_choice,qes1998,from_na,OD4,"The reported vote of the post-election recontact (q3post)"
master,federal_pid,qes2022,to_na,A:H4,"-99 (item not answered) is a missing value (0.4.4 category Don\'t know / Refused)"
master,provincial_pid,*,from_na,HZ6,"Provincial party identification of the spec (0.4.4 found a source only in qes2022)"
master,vote_choice_timing,*,from_na,OD4,"vote_choice now holds the reported vote in this study"
decon,*,qes2007_panel,to_na,P5,"The one row in no wave is not_in_wave"
decon,gender,qes2014;qes2018;qes2022,changed,HZ6,"Factor levels are the target\'s English labels (Man, Woman rather than the study\'s own labels, for example Masculin, F\u00e9minin)"
decon,education,qes2014;qes2018;qes2022,changed,HZ6,"The four groups of education4 (the master\'s groups, with the target\'s English labels) instead of the study\'s own categories"
decon,citizenship,qes2022,changed,HZ6,"Factor levels are the target\'s levels (Yes, No) instead of the study\'s labels (Canadian citizen, Permanent resident, Other)"
decon,province_territory,qes2008;qes2012;qes2014;qes2018,changed,HZ6,"Quebec for every respondent, as in the master (0.4.4 put the study\'s region there: MTL RMR, QC RMR, other regions, or its codes)"
decon,age,qes2018,changed,HZ6,"Age in years from the age item or the year of birth (0.4.4 left the codes 1-3 of an age-band item)"
decon,age,qes2007_panel,changed,HZ6,"The six age bands with the master\'s labels (0.4.4 kept the study\'s French band labels)"
decon,prov_pid,qes2022,changed,HZ6,"Factor levels are the target\'s English labels (None of these for no party)"
decon,fed_pid,qes2022,changed,HZ6,"Factor levels are the target\'s English labels (Bloc Québécois, Another party, None of these)"
decon,turnout,qes2022,changed,OD9,"The campaign-period likelihood of voting with the target\'s ordered levels instead of the study\'s labels (I already voted and not eligible are not levels of the target)"
decon,votechoice,qes2022,changed,OD9,"The campaign-period vote intention with the target\'s party labels"
decon,gender,qes1998;qes2007;qes2007_panel;qes2008;qes2012;qes2012_panel;qes2018_panel;qes_crop_2007_2010,from_na,HZ6,"Filled from the target gender (0.5.0 had no valid source); equals the master\'s gender"
decon,province_territory,qes1998;qes2007;qes2007_panel;qes2012_panel;qes2018_panel;qes_crop_2007_2010,from_na,HZ6,"Quebec for every respondent, as in the master"
decon,education,qes2007;qes2007_panel;qes2008;qes2012;qes2018_panel;qes_crop_2007_2010,from_na,HZ6,"Filled from education4; equals the master\'s education"
decon,income,qes2007;qes2007_panel;qes2008;qes2012;qes2014;qes2018_panel;qes_crop_2007_2010,from_na,HZ6,"Filled from income_native; equals the master\'s income"
decon,religion,qes2012;qes2014,from_na,HZ6,"Filled from religion; equals the master\'s religion"
decon,yob,qes2007;qes2008;qes2012;qes2014,from_na,HZ6,"Filled from birth_year; equals the master\'s year_of_birth"
decon,age,qes2007;qes2008;qes2014,from_na,HZ6,"Age from the year of birth, as the master\'s age"
decon,political_interest,qes2007;qes2007_panel;qes2008;qes2012;qes2014;qes2018,from_na,OD7,"Interest on 0-10 as the master\'s political_interest"
decon,ideology,qes2012;qes2014;qes2018;qes2018_panel,from_na,A:H3,"Filled from lr_self; equals the master\'s ideology"
decon,prov_pid,qes2007;qes2008;qes2012;qes2014;qes2018,from_na,HZ6,"Filled from pid_prov; equals the master\'s provincial_pid"
decon,born_canada,qes2012;qes2014;qes2018,from_na,A:H4,"Filled from born_canada; equals the master\'s born_canada"
decon,turnout,qes1998;qes2007;qes2007_panel;qes2008;qes2012;qes2012_panel;qes2014;qes2018;qes2018_panel,from_na,A:H6,"The reported turnout (0.5.0 blanked the 0.4.4 column, which read another question); equals the master\'s turnout"
decon,votechoice,qes1998;qes2007;qes2007_panel;qes2008;qes2012;qes2012_panel;qes2014;qes2018;qes2018_panel,from_na,A:H6,"The reported vote (0.5.0 blanked the 0.4.4 column, which read another question); equals the master\'s vote_choice"
decon,age,qes1998,to_na,HZ6,"age code 9 (refused) is a missing value, not a band"
decon,votechoice,qes2022,to_na,OD9,"Don\'t know and prefer not to answer are missing values of the vote intention"
decon,votechoice_text,qes2022,to_na,OD9,"Empty text of respondents never asked the vote intention is a missing value"
decon,fed_pid,qes2022,to_na,A:H4,"-99 (item not answered) is a missing value"
decon,education,qes2012,to_na,HZ6,"scol code 10 is not mappable"
decon,education,*,to_na,A:H4,"Don\'t know and refusal answers are missing values, not education categories"
', colClasses = "character", strip.white = TRUE)
# The ids above are the design's (dev/assessment.md, dev/open-questions.md);
# changes.csv, which ships, gives each cause a descriptive name instead
# (section 5 of dev/legacy-diff.md maps the two).
cause_names <- c(
  "0.5.0" = "0.5.0", HZ6 = "harmonization_engine", P5 = "all_rows_kept",
  OD4 = "reported_vote_only", OD7 = "interest_on_0_10", OD8 = "two_first_languages",
  OD9 = "campaign_period_vote", "A:H3" = "scale_corrected", "A:H4" = "coding_error_fixed",
  "A:H6" = "other_question_fixed", SIGNOFF = "not_signed_off", REV = "review_correction"
)
stopifnot(all(explained$cause %in% names(cause_names)))
explained$cause <- unname(cause_names[explained$cause])

explain <- function(profile, column, study, kind) {
  # `study` may list several studies, separated by ";"
  in_study <- vapply(strsplit(explained$study, ";", fixed = TRUE), function(x) any(x %in% c(study, "*")), logical(1))
  hit <- explained$profile == profile & explained$column %in% c(column, "*") &
    in_study & explained$kind == kind
  # every cause that applies (a row for the column and a row for every
  # column, "*", may both match)
  if (any(hit)) unique(explained$cause[hit]) else NA_character_
}

# Cell classification of two aligned columns.
classify <- function(a, b) {
  if (is.factor(a)) a <- as.character(a)
  if (is.factor(b)) b <- as.character(b)
  na_a <- is.na(a)
  na_b <- is.na(b)
  both <- !na_a & !na_b
  num <- is.numeric(a) && is.numeric(b)
  exact <- if (num) both & a == b else both & as.character(a) == as.character(b)
  close <- if (num) both & abs(a - b) <= 1e-6 * pmax(1, abs(a)) else exact
  c(to_na = sum(!na_a & na_b), from_na = sum(na_a & !na_b), changed = sum(both & !close),
    precision = sum(close & !exact))
}
fmt_mean <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(x))) "" else formatC(mean(x, na.rm = TRUE), format = "f", digits = 2)
}

problems <- character(0)
rows_out <- list()
changes <- list()

# One column of one study: compare with 0.5.0 (gated) and 0.4.4 (counted).
# `kept` marks the rows 0.4.4 kept (o44 has those rows only).
compare_cell <- function(profile, s, v, o44, o50, n, kept = rep(TRUE, length(n))) {
  k50 <- classify(o50, n)
  k44 <- if (is.null(o44)) c(to_na = NA, from_na = NA, changed = NA, precision = NA) else classify(o44, n[kept])
  cause50 <- character(0)
  for (kind in c("to_na", "from_na", "changed")) {
    if (k50[[kind]] > 0) {
      c_ <- explain(profile, v, s, kind)
      if (all(is.na(c_))) {
        problems <<- c(problems, sprintf("%s %s %s: UNEXPLAINED %s (%d cells) against 0.5.0", profile, s, v, kind, k50[[kind]]))
      } else {
        cause50 <- c(cause50, c_)
      }
    }
  }
  diff44 <- !is.null(o44) && any(k44[c("to_na", "from_na", "changed")] > 0)
  if (all(k50[c("to_na", "from_na", "changed")] == 0) && !diff44) return(invisible())
  numeric_col <- is.numeric(n) && s %in% shipped
  before <- diff44 && any(classify(o44, o50[kept])[c("to_na", "from_na", "changed")] > 0)
  cause <- unique(c(if (before) "0.5.0", cause50))
  rows_out[[length(rows_out) + 1L]] <<- data.frame(
    profile = profile, study = s, column = v,
    valid_044 = if (is.null(o44)) NA_integer_ else sum(!is.na(o44)), valid_050 = sum(!is.na(o50)), valid_070 = sum(!is.na(n)),
    to_na = k50[["to_na"]], from_na = k50[["from_na"]], changed = k50[["changed"]],
    mean_044 = if (numeric_col && !is.null(o44)) fmt_mean(o44) else "",
    mean_050 = if (numeric_col) fmt_mean(o50) else "",
    mean_070 = if (numeric_col) fmt_mean(n) else "",
    cause = paste(cause, collapse = ", "),
    stringsAsFactors = FALSE
  )
  if (diff44) {
    changes[[length(changes) + 1L]] <<- data.frame(profile = profile, column = v, study = s,
                                                   cause = paste(cause, collapse = ";"), stringsAsFactors = FALSE)
  }
  invisible()
}

# ---- master -------------------------------------------------------------------

old44 <- readRDS(file.path(base, "legacy_master_v044.rds"))
old50 <- readRDS(file.path(base050, "legacy_master_050.rds"))
new <- suppressWarnings(suppressMessages(get_qes_master(quiet = TRUE)))
documented <- names(old44)[1:30]
if (!identical(names(new)[1:30], documented)) {
  problems <- c(problems, "master: the 30 documented columns are not first, in order")
}
if (!identical(names(new)[seq_along(old50)], names(old50))) {
  problems <- c(problems, "master: the columns of 0.5.0 are not kept first, in order (appended columns are never removed)")
}
types_ok <- vapply(documented, function(v) identical(class(new[[v]])[1], class(old50[[v]])[1]), logical(1))
if (!all(types_ok)) {
  problems <- c(problems, sprintf("master: the type of %s changed", paste(documented[!types_ok], collapse = ", ")))
}
if (!identical(as.character(new$qes_code), as.character(old50$qes_code))) {
  problems <- c(problems, "master: the rows differ from 0.5.0 (every row of every file, in file order)")
}
row_table <- list()
for (s in legacy_codes) {
  o50 <- old50[old50$qes_code == s, , drop = FALSE]
  n <- new[new$qes_code == s, , drop = FALSE]
  o44 <- old44[old44$qes_code == s, , drop = FALSE]
  # 0.4.4 dropped duplicated respondent_id rows: match them out with the
  # 0.5.0 identifiers (the same as 0.4.4's)
  id <- tolower(trimws(as.character(o50$respondent_id)))
  kept <- is.na(id) | !nzchar(id) | !duplicated(id)
  aligned <- sum(kept) == nrow(o44) && all(as.character(o50$respondent_id[kept]) == as.character(o44$respondent_id))
  if (!aligned) {
    problems <- c(problems, sprintf("master %s: rows do not align with 0.4.4 after matching out its de-duplication", s))
  }
  prov <- attr(new, "qes_provenance")
  row_table[[s]] <- data.frame(study = s, rows_044 = nrow(o44), rows_050 = nrow(o50), rows_070 = nrow(n),
                               file_rows = prov$n_rows[prov$study == s], stringsAsFactors = FALSE)
  for (v in names(old50)) {
    compare_cell("master", s, v, if (aligned && v %in% documented) o44[[v]] else NULL, o50[[v]], n[[v]], kept)
  }
}

# Each study's rows do not depend on which other studies are loaded.
for (s in legacy_codes) {
  one <- suppressWarnings(suppressMessages(get_qes_master(surveys = s, quiet = TRUE)))
  full <- new[new$qes_code == s, , drop = FALSE]
  rownames(full) <- NULL
  attributes(one) <- attributes(one)[c("names", "row.names", "class")]
  attributes(full) <- attributes(full)[c("names", "row.names", "class")]
  if (!isTRUE(all.equal(one, full, check.attributes = FALSE))) {
    problems <- c(problems, sprintf("master %s: built alone, its rows differ from the full build", s))
  }
}

# ---- get_decon() -------------------------------------------------------------

old_decon44 <- readRDS(file.path(base, "legacy_decon_all_other_studies.rds"))
old_decon44$qes2022 <- readRDS(file.path(base, "legacy_decon_qes2022.rds"))
old_decon50 <- readRDS(file.path(base050, "legacy_decon_050.rds"))
for (s in legacy_codes) {
  o44 <- old_decon44[[s]]
  o50 <- old_decon50[[s]]
  n <- suppressWarnings(suppressMessages(get_decon(s, quiet = TRUE)))
  if (!identical(names(n), names(o50)) || nrow(n) != nrow(o50)) {
    problems <- c(problems, sprintf("decon %s: columns or rows differ from 0.5.0", s))
    next
  }
  same44 <- !is.null(o44) && nrow(o44) == nrow(n)
  for (v in names(o50)) {
    compare_cell("decon", s, v, if (same44) o44[[v]] else NULL, o50[[v]], n[[v]])
  }
}

# Each get_decon() column that reads the same target as a master column
# must hold the same values once labels are normalized: the master is gated
# cell by cell against 0.5.0 above, so this carries that gate over to the
# decon values (a wrong recode or target would show here).
decon_pairs <- c(
  gender = "gender", yob = "year_of_birth", ideology = "ideology", income = "income",
  religion = "religion", political_interest = "political_interest", prov_pid = "provincial_pid",
  fed_pid = "federal_pid", education = "education", born_canada = "born_canada",
  votechoice = "vote_choice", turnout = "turnout"
)
# decon labels (the targets' English levels) -> master labels (0.4.4's)
decon_labels <- c(
  "College (CEGEP, technical)" = "College/CEGEP/Technical", "Another gender" = "Other",
  "None of these" = "Did not vote / None", "Bloc Qu\u00e9b\u00e9cois" = "Bloc Quebecois",
  "Another party" = "Other party"
)
norm_decon <- function(x, column) {
  x <- if (is.numeric(x)) ifelse(is.na(x), NA_character_, sprintf("%.15g", x)) else as.character(x)
  x[x %in% "NA"] <- NA_character_
  # the master's turnout is 1/0 (int01), get_decon()'s Yes/No
  labels <- if (column == "turnout") c(decon_labels, "Yes" = "1", "No" = "0") else decon_labels
  hit <- x %in% names(labels)
  x[hit] <- unname(labels[x[hit]])
  x
}
norm_master <- function(x) {
  x <- if (is.numeric(x)) ifelse(is.na(x), NA_character_, sprintf("%.15g", x)) else as.character(x)
  x[x %in% "NA"] <- NA_character_
  x
}
decon_master_checked <- 0L
for (s in legacy_codes) {
  d <- suppressWarnings(suppressMessages(get_decon(s, quiet = TRUE)))
  mm <- new[new$qes_code == s, , drop = FALSE]
  for (dv in names(decon_pairs)) {
    # qes2022's get_decon() keeps the campaign-period items (OD9)
    if (s == "qes2022" && dv %in% c("votechoice", "turnout")) next
    a <- norm_decon(d[[dv]], dv)
    b <- norm_master(mm[[decon_pairs[[dv]]]])
    if (!identical(is.na(a), is.na(b)) || !all(a == b, na.rm = TRUE)) {
      problems <- c(problems, sprintf("decon %s: %s differs from the master's %s", s, dv, decon_pairs[[dv]]))
    }
    decon_master_checked <- decon_master_checked + 1L
  }
}

# ---- outputs --------------------------------------------------------------------

cells <- do.call(rbind, rows_out)
rows <- do.call(rbind, row_table)
changes <- do.call(rbind, changes)
changes <- changes[order(changes$profile != "master", match(changes$study, legacy_codes)), , drop = FALSE]
rownames(changes) <- NULL
md_table <- function(x) {
  x[] <- lapply(x, function(v) { v <- as.character(v); v[is.na(v)] <- ""; gsub("|", "\\|", v, fixed = TRUE) })
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|"),
    apply(x, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  )
}
causes <- data.frame(
  cause = unname(cause_names[c("0.5.0", "HZ6", "P5", "OD4", "OD7", "OD8", "OD9", "A:H3", "A:H4", "A:H6")]),
  id = c("0.5.0", "HZ6", "P5", "OD4", "OD7", "OD8", "OD9", "A:H3", "A:H4", "A:H6"),
  meaning = c(
    "Already different in qesR 0.5.0 (the interim builders: deletions and blanks, slice S4; see its NEWS)",
    "Rendered from the harmonization engine: the spec's question and value map for the study (slice HZ6)",
    "Every row of the file is kept, and a row outside every wave has no answers (design rule P5)",
    "vote_choice and turnout are the reported vote and turnout only (owner decision OD4)",
    "political_interest puts four-point items on 0-10 as 10, 7, 3 and 0 (OD7)",
    "language is the mother tongue; two first languages are NA (OD8)",
    "get_decon(\"qes2022\") keeps the campaign-period intention (OD9)",
    "Interest and ideology scales (dev/assessment.md 5.4)",
    "Other verified coding errors of qesR 0.4.4 (assessment 5.4)",
    "get_decon() turnout and votechoice read other questions in qesR 0.4.4 (q1/q2; assessment 5.4)"
  ),
  stringsAsFactors = FALSE
)
explain_table <- explained[, c("profile", "column", "study", "kind", "cause", "explanation")]

report <- c(
  "# Legacy builders: differences from qesR 0.4.4 and 0.5.0",
  "",
  "Generated by `data-raw/compare_legacy.R` (design.md section 5.12, slice HZ6 gate). Do not edit by hand.",
  "",
  sprintf("- Baselines: qesR 0.4.4 (R9 run at tag v0.4.4, `legacy_master_v044.rds`, md5 `%s`) and qesR 0.5.0 (`legacy_master_050.rds`, md5 `%s`).",
          unname(tools::md5sum(file.path(base, "legacy_master_v044.rds"))),
          unname(tools::md5sum(file.path(base050, "legacy_master_050.rds")))),
  sprintf("- New: qesR %s from the working tree, spec %s (content hash %s).",
          as.character(utils::packageVersion("qesR")), attr(new, "qes_spec")$version, attr(new, "qes_spec")$hash),
  "- Aggregates only: counts per column and study, and means of numeric columns (those of `qes2022` are CC BY-NC 4.0, inst/COPYRIGHTS, section 2).",
  "- `to_na`, `from_na`, `changed`: cells that differ from qesR 0.5.0 (same rows); every such (column, study, kind) is explained in section 4. `valid_044` counts the rows 0.4.4 kept.",
  "",
  sprintf("**Gate: %s.**", if (length(problems) == 0L) "every difference from qesR 0.5.0 is explained" else "FAILED"),
  sprintf("- `get_decon()` against the master: %d (study, column) pairs that read the same target hold the same values once labels are normalized (qes2022 `turnout` and `votechoice` aside, OD9).",
          decon_master_checked),
  if (length(problems) > 0L) c("", paste0("- ", problems)) else character(0),
  "",
  "## 1. Rows",
  "",
  md_table(rows),
  "",
  "## 2. Columns",
  "",
  sprintf("- The 30 documented columns are first, in the 0.4.4 order and types: %s.",
          if (identical(names(new)[1:30], documented) && all(types_ok)) "yes" else "NO"),
  sprintf("- Appended in 0.5.0 and kept: %s.", paste0("`", setdiff(names(old50), documented), "`", collapse = ", ")),
  sprintf("- Appended in 0.7.0: %s.", paste0("`", setdiff(names(new), names(old50)), "`", collapse = ", ")),
  "",
  "## 3. Cells, per column and study",
  "",
  "Columns and studies not listed are identical in qesR 0.4.4, 0.5.0 and 0.7.0.",
  "",
  md_table(cells),
  "",
  "## 4. Explained differences from qesR 0.5.0",
  "",
  md_table(explain_table),
  "",
  "## 5. Causes",
  "",
  md_table(causes),
  ""
)

# The notes of one (column, study), joined into one sentence: a lead clause
# ("...: ") that every note shares is given once, and each clause after the
# first starts in lower case (unless it opens with a proper noun).
lower_first <- function(x) {
  keep <- !grepl("^[A-Z][a-z]", x) | grepl("^(Quebec|Canadian|French|English|CEGEP)\\b", x)
  ifelse(keep, x, paste0(tolower(substr(x, 1L, 1L)), substring(x, 2L)))
}
join_notes <- function(x) {
  x <- unique(x)
  if (length(x) < 2L) return(paste(x, collapse = ""))
  if (grepl(": ", x[1], fixed = TRUE)) {
    lead <- sub("^(.*?: ).*$", "\\1", x[1], perl = TRUE)
    if (all(startsWith(x, lead))) {
      return(paste0(lead, paste(substring(x, nchar(lead) + 1L), collapse = " and ")))
    }
  }
  paste(c(x[1], lower_first(x[-1])), collapse = "; ")
}

# the explained rows whose study field (a study, a ";"-list or "*") covers `study`
covers <- function(field, study) vapply(strsplit(field, ";", fixed = TRUE), function(x) any(x %in% c(study, "*")), logical(1))

# NEWS: master columns whose values changed, of the studies whose metadata ships
fmt_n <- function(x) ifelse(is.na(x), "", formatC(as.numeric(x), format = "d", big.mark = ","))
news_rows <- cells[cells$profile == "master" & cells$study %in% shipped &
                     (cells$to_na + cells$from_na + cells$changed > 0), , drop = FALSE]
why <- vapply(seq_len(nrow(news_rows)), function(i) {
  e <- explained[explained$profile == "master" & explained$column %in% c(news_rows$column[i], "*") &
                   covers(explained$study, news_rows$study[i]), , drop = FALSE]
  e <- e[e$kind %in% c(if (news_rows$to_na[i] > 0) "to_na", if (news_rows$from_na[i] > 0) "from_na",
                       if (news_rows$changed[i] > 0) "changed"), , drop = FALSE]
  join_notes(e$explanation)
}, character(1))
news_tab <- data.frame(
  Column = paste0("`", news_rows$column, "`"), Study = paste0("`", news_rows$study, "`"),
  `0.4.4` = fmt_n(news_rows$valid_044), `0.5.0` = fmt_n(news_rows$valid_050),
  `0.7.0` = fmt_n(news_rows$valid_070), Why = why,
  check.names = FALSE, stringsAsFactors = FALSE
)
news_tab$`0.4.4`[is.na(news_rows$valid_044)] <- ""
nc <- cells[cells$profile == "master" & !cells$study %in% shipped & (cells$to_na + cells$from_na + cells$changed > 0), , drop = FALSE]
nc_why <- vapply(seq_len(nrow(nc)), function(i) {
  e <- explained[explained$profile == "master" & explained$column %in% c(nc$column[i], "*") &
                   covers(explained$study, nc$study[i]), , drop = FALSE]
  e <- e[e$kind %in% c(if (nc$to_na[i] > 0) "to_na", if (nc$from_na[i] > 0) "from_na",
                       if (nc$changed[i] > 0) "changed"), , drop = FALSE]
  sprintf("`%s`, %s", nc$column[i], lower_first(join_notes(e$explanation)))
}, character(1))
news_block <- c(
  "<!-- legacy-table: start (generated by data-raw/compare_legacy.R) -->",
  "",
  "Values (non-missing cells) of the `get_qes_master()` columns that changed in 0.7.0, by study, in qesR 0.4.4, 0.5.0 and 0.7.0 (the counts of `qes2022`, published since 0.7.1, are CC BY-NC 4.0 like its other metadata). The 0.4.4 counts are over the rows 0.4.4 kept (it dropped 380 `qes2007_panel` rows and 1 `qes_crop_2007_2010` row). Columns and studies not listed are unchanged since 0.5.0.",
  "",
  md_table(news_tab),
  "",
  if (nrow(nc) > 0L) c(sprintf("In %s: %s.", paste0("`", unique(nc$study), "`", collapse = ", "), paste(nc_why, collapse = "; ")), ""),
  "<!-- legacy-table: end -->"
)

write_text <- function(lines, path) {
  txt <- paste0(paste(lines, collapse = "\n"), "\n")
  if (check_only) {
    cur <- if (file.exists(path)) paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n") else ""
    if (!identical(paste0(cur, "\n"), txt)) problems <<- c(problems, sprintf("%s is not current", path))
    return(invisible())
  }
  con <- file(path, open = "wb")
  writeBin(charToRaw(enc2utf8(txt)), con)
  close(con)
}
write_text(report, file.path("dev", "legacy-diff.md"))
csv_lines <- function(x) {
  q <- function(v) { v <- as.character(v); v[is.na(v)] <- ""; need <- grepl('[",;\n]', v); v[need] <- paste0('"', gsub('"', '""', v[need]), '"'); v }
  c(paste(names(x), collapse = ","), if (nrow(x) > 0L) do.call(paste, c(lapply(x, q), sep = ",")))
}
write_text(csv_lines(changes), file.path("inst", "extdata", "legacy", "changes.csv"))
news <- readLines("NEWS.md", encoding = "UTF-8")
a <- grep("^<!-- legacy-table: start", news)
b <- grep("^<!-- legacy-table: end", news)
if (length(a) != 1L || length(b) != 1L || b < a) {
  problems <- c(problems, "NEWS.md has no legacy-table markers")
} else {
  write_text(c(news[seq_len(a - 1L)], news_block, news[-seq_len(b)]), "NEWS.md")
}

cat(sprintf("%s dev/legacy-diff.md: %d cell rows; changes.csv: %d rows; %d problem(s).\n",
            if (check_only) "Checked" else "Wrote", nrow(cells), nrow(changes), length(problems)))
if (length(problems) > 0L) {
  cat(paste0("  ", problems, "\n"), sep = "")
  quit(status = 1L)
}
