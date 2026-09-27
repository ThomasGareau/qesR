# Compare the legacy builders with qesR 0.4.4 (design.md sections 5.11 and
# 5.12; the gate of slice S4).
#
# Usage (from the package root):
#   QESR_LEGACY_BASELINE=<dir> [QESR_TEST_DATA_DIR=<cache>] \
#     Rscript data-raw/compare_legacy.R [--out dev/legacy-diff.md]
#
# <dir> is the output directory of the R9 baseline run (qesR 0.4.4 at
# d1faad6, fresh session, LANGUAGE=en), holding legacy_master_v044.rds,
# legacy_decon_qes2022.rds and legacy_decon_all_other_studies.rds. These are
# files the owner's own run of 0.4.4 wrote; they are read here, in a
# development script, never by the package.
# QESR_TEST_DATA_DIR, when set, is a qesR download cache holding the pinned
# originals (see ?qes_cache_info), used with no request; otherwise the files
# are downloaded politely into the session cache.
#
# It builds get_qes_master() (all 11 studies) and get_decon() (each study)
# with the package in the working directory, aligns their rows with the
# baseline, and classifies every cell that differs:
#   * to_na     a value became NA. Every such cell must be one a blank rule
#               of inst/extdata/legacy/blanks.csv set to NA: the same build
#               with no blank rule (the cached blanks table emptied) must
#               hold a value there, and must equal the build wherever the
#               build has a value;
#   * from_na, changed, precision
#               any other difference. It must be listed in `explained`
#               below, with its cause.
# qesR 0.4.4 dropped rows (de-duplication); those rows are matched out first,
# and counted as restored.
#
# Writes an aggregates-only table (counts per column and study, and means of
# numeric columns for the CC0 studies; nothing for a single respondent) to
# --out (default dev/legacy-diff.md), and exits with status 1 if any
# difference is unexplained.

args <- commandArgs(trailingOnly = TRUE)
out_path <- if ("--out" %in% args) args[which(args == "--out") + 1L] else file.path("dev", "legacy-diff.md")

base <- Sys.getenv("QESR_LEGACY_BASELINE")
if (!nzchar(base) || !dir.exists(base)) {
  stop("Set QESR_LEGACY_BASELINE to the output directory of the R9 baseline run.", call. = FALSE)
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
options(qesR.lang = "en")

ns <- asNamespace("qesR")
legacy_codes <- get(".qes_legacy_codes", envir = ns)
blanks <- get(".qes_legacy_table", envir = ns)("blanks")
catalog <- get(".qes_catalog", envir = ns)()
cc0 <- catalog$studies$study[catalog$studies$metadata_shipped %in% TRUE]

# Differences that are not blanks, with their cause. Every other difference
# fails the gate.
explained <- read.csv(text = '
profile,column,study,kind,cause,explanation
master,qes_name_en,qes2018_panel,changed,S1,Study name from the catalog (the Durand panel title)
master,qes_name_en,qes2012_panel,changed,S1,Study name from the catalog (the Durand panel title)
master,qes_name_en,qes2007_panel,changed,S1,Study name from the catalog (the Durand panel title)
master,qes_name_en,qes_crop_2007_2010,changed,S1,Study name from the catalog (the CROP deposit title)
master,qes_name_en,qes1998,changed,S1,Study name from the catalog (the 1998 deposit title)
master,interview_start,qes2022,changed,S2b,Date-times read from the original file: no trailing .000
master,interview_end,qes2022,changed,S2b,Date-times read from the original file: no trailing .000
master,interview_recorded,qes2022,changed,S2b,Date-times read from the original file: no trailing .000
master,province_territory,qes_crop_2007_2010,changed,S2b,Text typed in the DOS character set repaired: RESTE DU QUEBEC is now recognised as Quebec
master,income,qes_crop_2007_2010,changed,S2b,Text typed in the DOS character set repaired (a instead of an ellipsis in the brackets)
master,religion,qes2012,changed,S2b,Labels in their original case (from the SPSS twin)
master,turnout,qes2018,from_na,S2b,"q5 codes 1 and 3 (did not vote) are 0, as the 0.4.4 q5 map intends; 0.4.4 missed them because its hand-typed labels did not match its own regex"
master,survey_weight,*,precision,S2b,Weights at the precision stored in the original file (0.4.4 read 7 printed digits)
decon,province_territory,qes2012,changed,S2b,Labels in their original case (from the SPSS twin)
decon,age,qes1998,changed,S2b,"The panel file\'s own age labels (18-24 ... 65+) instead of another file\'s"
decon,votechoice_text,qes2022,changed,S2b,Replacement characters repaired
', colClasses = "character", strip.white = TRUE)

is_explained <- function(profile, column, study, kind) {
  any(explained$profile == profile & explained$column == column &
        explained$study %in% c(study, "*") & explained$kind == kind)
}
cause_of <- function(profile, column, study, kind) {
  hit <- explained$profile == profile & explained$column == column &
    explained$study %in% c(study, "*") & explained$kind == kind
  if (any(hit)) explained$cause[which(hit)[1]] else NA_character_
}
blank_rule <- function(profile, column, study) {
  b <- blanks[blanks$profile == profile & blanks$column %in% c(column, "*") &
                blanks$study %in% c(study, "*"), , drop = FALSE]
  if (nrow(b) == 0L) NULL else b
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
  c(
    to_na = sum(!na_a & na_b),
    from_na = sum(na_a & !na_b),
    changed = sum(both & !close),
    precision = sum(close & !exact)
  )
}

fmt_mean <- function(x) {
  x <- suppressWarnings(as.numeric(as.character(x)))
  if (all(is.na(x))) "" else formatC(mean(x, na.rm = TRUE), format = "f", digits = 2)
}

problems <- character(0)
rows_out <- list()

# The same builds with no blank rule: the cells a rule set to NA are those
# that hold a value here and are NA in the real build.
legacy_env <- get(".qes_legacy_env", envir = ns)
without_blanks <- function(expr) {
  legacy_env$blanks <- blanks[0, , drop = FALSE]
  on.exit(legacy_env$blanks <- blanks, add = TRUE)
  force(expr)
}

# The to_na cells of `old` -> `new` that no blank rule explains, and the
# cells where the build and the unblanked build differ other than by a blank.
blank_check <- function(old, new, raw) {
  if (is.factor(new)) new <- as.character(new)
  if (is.factor(raw)) raw <- as.character(raw)
  to_na <- !is.na(old) & is.na(new)
  c(
    hidden = sum(to_na & is.na(raw)),
    other = sum(!is.na(new) & (is.na(raw) | as.character(new) != as.character(raw)))
  )
}

# ---- master -------------------------------------------------------------------

old <- readRDS(file.path(base, "legacy_master_v044.rds"))
new <- suppressMessages(get_qes_master(quiet = TRUE))
raw_new <- without_blanks(suppressMessages(get_qes_master(quiet = TRUE)))
documented <- names(old)[1:30]
removed <- setdiff(names(old), documented)
appended <- setdiff(names(new), documented)
if (!identical(names(new)[1:30], documented)) {
  problems <- c(problems, "master: the 30 documented columns are not first, in order")
}
if (!setequal(removed, attr(new, "removed_columns"))) {
  problems <- c(problems, "master: removed_columns does not list the columns 0.4.4 appended")
}

row_table <- list()
for (s in legacy_codes) {
  o <- old[old$qes_code == s, , drop = FALSE]
  n <- new[new$qes_code == s, , drop = FALSE]
  id <- tolower(trimws(as.character(n$respondent_id)))
  valid <- !is.na(id) & nzchar(id)
  kept <- !valid | !duplicated(id)
  n2 <- n[kept, , drop = FALSE]
  r2 <- raw_new[raw_new$qes_code == s, , drop = FALSE][kept, , drop = FALSE]
  aligned <- nrow(n2) == nrow(o) && all(as.character(n2$respondent_id) == as.character(o$respondent_id))
  if (!aligned) {
    problems <- c(problems, sprintf("master %s: rows do not align with 0.4.4 after matching out its de-duplication", s))
    next
  }
  prov <- attr(new, "qes_provenance")
  file_rows <- prov$n_rows[prov$study == s]
  row_table[[s]] <- data.frame(
    study = s, rows_v044 = nrow(o), rows_new = nrow(n), file_rows = file_rows,
    restored = nrow(n) - nrow(o), stringsAsFactors = FALSE
  )
  if (!identical(as.integer(nrow(n)), as.integer(file_rows))) {
    problems <- c(problems, sprintf("master %s: %d rows, the file has %d", s, nrow(n), file_rows))
  }
  for (v in documented) {
    bc <- blank_check(o[[v]], n2[[v]], r2[[v]])
    if (bc[["other"]] > 0) {
      problems <- c(problems, sprintf("master %s %s: %d cells differ from the unblanked build other than by a blank", s, v, bc[["other"]]))
    }
    k <- classify(o[[v]], n2[[v]])
    if (all(k == 0)) next
    rule <- blank_rule("master", v, s)
    status <- character(0)
    cause <- character(0)
    if (k[["to_na"]] > 0) {
      if (is.null(rule) || bc[["hidden"]] > 0) {
        status <- c(status, sprintf("UNEXPLAINED to_na (%d cells not set by a blank rule)", if (is.null(rule)) k[["to_na"]] else bc[["hidden"]]))
      } else {
        cause <- c(cause, unique(rule$cause))
      }
    }
    for (kind in c("from_na", "changed", "precision")) {
      if (k[[kind]] > 0) {
        if (is_explained("master", v, s, kind)) {
          cause <- c(cause, cause_of("master", v, s, kind))
        } else {
          status <- c(status, paste("UNEXPLAINED", kind))
        }
      }
    }
    if (length(status) > 0L) {
      problems <- c(problems, sprintf("master %s %s: %s", s, v, paste(status, collapse = ", ")))
    }
    numeric_col <- is.numeric(o[[v]]) && s %in% cc0
    rows_out[[length(rows_out) + 1L]] <- data.frame(
      profile = "master", study = s, column = v,
      valid_v044 = sum(!is.na(o[[v]])), valid_new = sum(!is.na(n2[[v]])),
      to_na = k[["to_na"]], from_na = k[["from_na"]], changed = k[["changed"]], precision = k[["precision"]],
      mean_v044 = if (numeric_col) fmt_mean(o[[v]]) else "",
      mean_new = if (numeric_col) fmt_mean(n2[[v]]) else "",
      cause = paste(unique(cause), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
}

# Each study's rows do not depend on which other studies are loaded.
for (s in legacy_codes) {
  one <- suppressMessages(get_qes_master(surveys = s, quiet = TRUE))
  full <- new[new$qes_code == s, , drop = FALSE]
  rownames(full) <- NULL
  attributes(one) <- attributes(one)[c("names", "row.names", "class")]
  attributes(full) <- attributes(full)[c("names", "row.names", "class")]
  if (!identical(one, full)) {
    problems <- c(problems, sprintf("master %s: built alone, its rows differ from the full build", s))
  }
}

# ---- get_decon() -------------------------------------------------------------

old_decon <- readRDS(file.path(base, "legacy_decon_all_other_studies.rds"))
old_decon$qes2022 <- readRDS(file.path(base, "legacy_decon_qes2022.rds"))
for (s in legacy_codes) {
  o <- old_decon[[s]]
  n <- suppressMessages(get_decon(s, quiet = TRUE))
  r <- without_blanks(suppressMessages(get_decon(s, quiet = TRUE)))
  if (!identical(names(n), names(o)) || nrow(n) != nrow(o)) {
    problems <- c(problems, sprintf("decon %s: columns or rows differ from 0.4.4", s))
    next
  }
  for (v in names(o)) {
    bc <- blank_check(o[[v]], n[[v]], r[[v]])
    if (bc[["other"]] > 0) {
      problems <- c(problems, sprintf("decon %s %s: %d cells differ from the unblanked build other than by a blank", s, v, bc[["other"]]))
    }
    k <- classify(o[[v]], n[[v]])
    if (all(k == 0)) next
    rule <- blank_rule("decon", v, s)
    status <- character(0)
    cause <- character(0)
    if (k[["to_na"]] > 0) {
      if (is.null(rule) || bc[["hidden"]] > 0) {
        status <- c(status, sprintf("UNEXPLAINED to_na (%d cells not set by a blank rule)", if (is.null(rule)) k[["to_na"]] else bc[["hidden"]]))
      } else {
        cause <- c(cause, unique(rule$cause))
      }
    }
    for (kind in c("from_na", "changed", "precision")) {
      if (k[[kind]] > 0) {
        if (is_explained("decon", v, s, kind)) {
          cause <- c(cause, cause_of("decon", v, s, kind))
        } else {
          status <- c(status, paste("UNEXPLAINED", kind))
        }
      }
    }
    if (length(status) > 0L) {
      problems <- c(problems, sprintf("decon %s %s: %s", s, v, paste(status, collapse = ", ")))
    }
    numeric_col <- is.numeric(o[[v]]) && is.numeric(n[[v]]) && s %in% cc0
    rows_out[[length(rows_out) + 1L]] <- data.frame(
      profile = "decon", study = s, column = v,
      valid_v044 = sum(!is.na(o[[v]])), valid_new = sum(!is.na(n[[v]])),
      to_na = k[["to_na"]], from_na = k[["from_na"]], changed = k[["changed"]], precision = k[["precision"]],
      mean_v044 = if (numeric_col) fmt_mean(o[[v]]) else "",
      mean_new = if (numeric_col) fmt_mean(n[[v]]) else "",
      cause = paste(unique(cause), collapse = ", "),
      stringsAsFactors = FALSE
    )
  }
  cls_old <- vapply(o, function(x) class(x)[1], character(1))
  cls_new <- vapply(n, function(x) class(x)[1], character(1))
  fam <- function(x) ifelse(x %in% c("integer", "numeric"), "numeric", x)
  bad <- names(o)[fam(cls_old) != fam(cls_new)]
  if (length(bad) > 0L) {
    problems <- c(problems, sprintf("decon %s: column class changed for %s", s, paste(bad, collapse = ", ")))
  }
}

# ---- report --------------------------------------------------------------------

cells <- do.call(rbind, rows_out)
rows <- do.call(rbind, row_table)
md_table <- function(x) {
  x[] <- lapply(x, function(v) { v <- as.character(v); v[is.na(v)] <- ""; gsub("|", "\\|", v, fixed = TRUE) })
  c(
    paste0("| ", paste(names(x), collapse = " | "), " |"),
    paste0("|", paste(rep("---", ncol(x)), collapse = "|"), "|"),
    apply(x, 1, function(r) paste0("| ", paste(r, collapse = " | "), " |"))
  )
}
rule_table <- blanks[, c("profile", "column", "study", "codes", "cause", "basis")]
rule_table$codes[is.na(rule_table$codes)] <- "all"
extra_rules <- rule_table[!grepl("design 5\\.12|design 2\\.3", rule_table$basis), , drop = FALSE]
causes <- data.frame(
  cause = c("A:H1", "A:H2", "A:H3", "A:H4", "A:H6", "A:H7", "OD4", "OD5", "OD8", "S1", "S2b"),
  meaning = c(
    "Columns stacked by raw variable name across studies (dev/assessment.md 5.4)",
    "party_best filled from unrelated items (assessment 5.4)",
    "Interest and ideology scales: raw 1-4 codes, lost endpoints, a derived index (assessment 5.4)",
    "Other verified coding errors in core columns (assessment 5.4)",
    "get_decon() sources that are other questions (assessment 5.4)",
    "vote_choice_text sourced from the issue item (assessment 5.4)",
    "vote_choice and turnout are the reported vote only (owner decision OD4)",
    "sovereignty_support is the independent-country referendum item only (OD5)",
    "language is the mother tongue only (OD8)",
    "Study names from the offline catalog (slice S1)",
    "Reading the pinned original files (slice S2b): labels, character sets, precision"
  ),
  stringsAsFactors = FALSE
)

lines <- c(
  "# Legacy builders: differences from qesR 0.4.4",
  "",
  "Generated by `data-raw/compare_legacy.R` (design.md section 5.12, slice S4 gate). Do not edit by hand.",
  "",
  sprintf("- Baseline: R9 run of qesR 0.4.4 at d1faad6 (`legacy_master_v044.rds`, md5 `%s`).",
          unname(tools::md5sum(file.path(base, "legacy_master_v044.rds")))),
  sprintf("- New: qesR %s from the working tree; legacy tables %s.",
          as.character(utils::packageVersion("qesR")),
          paste(sprintf("`%s` %s", names(attr(new, "qes_spec")$legacy_tables),
                        substr(attr(new, "qes_spec")$legacy_tables, 1, 8)), collapse = ", ")),
  "- Aggregates only: counts per column and study, and means of numeric columns for the CC0 studies (none for `qes2022`, CC BY-NC).",
  "- `to_na`: a value that became NA; each cell is one a blank rule (table 4) set to NA, checked cell by cell against the same build with no blank rule. `from_na`, `changed`, `precision` (equal within 1e-6): other differences, each explained by its cause.",
  "",
  sprintf("**Gate: %s.**", if (length(problems) == 0L) "every difference is an intended deletion, blank or listed reader change" else "FAILED"),
  if (length(problems) > 0L) c("", paste0("- ", problems)) else character(0),
  "",
  "## 1. Rows",
  "",
  "qesR 0.4.4 removed rows by de-duplicating `respondent_id` within a study; 0.5.0 keeps every row of every file.",
  "",
  md_table(rows),
  "",
  sprintf("Total: %d rows in 0.4.4, %d now (%d restored).", sum(rows$rows_v044), sum(rows$rows_new), sum(rows$restored)),
  "",
  "## 2. Columns",
  "",
  sprintf("- The 30 documented columns are first, in the 0.4.4 order: %s.", if (identical(names(new)[1:30], documented)) "yes" else "NO"),
  sprintf("- Removed ([A:H1]): the %d columns 0.4.4 appended by stacking raw variables that share a name across studies (`attr(, \"removed_columns\")`): %s.",
          length(removed), paste0("`", removed, "`", collapse = ", ")),
  sprintf("- Appended: %s.", paste0("`", appended, "`", collapse = ", ")),
  "",
  "## 3. Cells that differ, per column and study",
  "",
  "Rows aligned on the rows 0.4.4 kept. Columns and studies not listed are identical to 0.4.4.",
  "",
  md_table(cells),
  "",
  "## 4. Blank rules (`inst/extdata/legacy/blanks.csv`)",
  "",
  md_table(rule_table),
  "",
  "## 5. Causes",
  "",
  md_table(causes),
  "",
  "## 6. Blanks beyond the list enumerated in design.md 5.12 and 2.3",
  "",
  "These rules apply the same decisions (OD4, OD5, OD8, [A:H3], [A:H4], [A:H6]) to cells the design did not list by name; each was verified on the original files (its basis says how). They are listed here for the owner's review.",
  "",
  md_table(extra_rules),
  ""
)
con <- file(out_path, open = "wb")
writeBin(charToRaw(enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))), con)
close(con)
cat(sprintf("Wrote %s: %d cell rows, %d problem(s).\n", out_path, nrow(cells), length(problems)))
if (length(problems) > 0L) {
  cat(paste0("  ", problems, "\n"), sep = "")
  quit(status = 1L)
}
