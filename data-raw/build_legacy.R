# The record of qesR 0.4.4's legacy sources, from the clean qesR 0.4.4
# baseline (R9; design.md section 5.12, slices S4 and HZ6).
#
# Usage (from the package root):
#   QESR_LEGACY_BASELINE=<dir> Rscript data-raw/build_legacy.R [--check]
#
# <dir> is the output directory of the R9 baseline run (qesR 0.4.4 at
# commit v0.4.4, fresh session, LANGUAGE=en). It must hold:
#   legacy_master_source_map.csv            the `source_map` attribute of
#                                           get_qes_master() (1,067 rows);
#   legacy_get_qes_names_all_studies.csv    the column names get_qes()
#                                           returned for the 11 studies.
# This script makes no network request.
#
# Writes
#   data-raw/legacy_source_map.csv          the frozen record: every
#       (profile, column, study) source variable qesR 0.4.4 chose, including
#       the 70 auto-stacked master columns that 0.5.0 removed. `origin` says
#       where each row comes from:
#         * master rows: the `source_map` attribute of the baseline master;
#         * decon rows: qesR 0.4.4's get_decon() lookup (first exact, then
#           case-insensitive match over its candidate lists, R/decon.R at
#           tag v0.4.4) applied to the 0.4.4 get_qes() column names, since 0.4.4
#           get_decon() recorded no source map.
#       The interim builders of qesR 0.5.0 read their sources from it; since
#       0.7.0 the legacy columns are rendered from the harmonization engine
#       (the spec's legacy.csv), and the record documents what 0.4.4 read
#       (data-raw/compare_legacy.R compares the results);
#   inst/extdata/legacy/removed.csv         the 70 auto-stacked master columns
#       ([A:H1]) and the raw variables they stacked (attr(, "removed_columns")
#       of get_qes_master()).
# With --check it writes nothing and fails if the shipped files differ.

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args

base <- Sys.getenv("QESR_LEGACY_BASELINE")
if (!nzchar(base) || !dir.exists(base)) {
  stop("Set QESR_LEGACY_BASELINE to the output directory of the R9 baseline run.", call. = FALSE)
}

source_map <- utils::read.csv(file.path(base, "legacy_master_source_map.csv"),
                              colClasses = "character", encoding = "UTF-8")
names_v044 <- utils::read.csv(file.path(base, "legacy_get_qes_names_all_studies.csv"),
                              colClasses = "character", encoding = "UTF-8")
names(names_v044)[1] <- "study"

studies <- c(
  "qes2022", "qes2018", "qes2018_panel", "qes2014", "qes2012", "qes2012_panel",
  "qes_crop_2007_2010", "qes2008", "qes2007", "qes2007_panel", "qes1998"
)
stopifnot(setequal(unique(source_map$qes_code), studies))

master_columns <- c(
  "respondent_id", "interview_start", "interview_end", "interview_recorded",
  "language", "citizenship", "year_of_birth", "age", "age_group", "gender",
  "province_territory", "education", "income", "religion", "born_canada",
  "political_interest", "ideology", "turnout", "vote_choice",
  "vote_choice_text", "party_best", "party_lean", "sovereignty_support",
  "sovereignty", "federal_pid", "provincial_pid", "survey_weight"
)

# qesR 0.4.4 get_decon() lookup, verbatim from R/decon.R at tag v0.4.4.
decon_lookup <- list(
  citizenship = c("cps_citizen", "cps_citizenship", "citizenship"),
  yob = c("cps_yob", "yob", "ageyear_1"),
  age = c("cps_age_in_years", "age", "agecalc", "agenum"),
  gender = c("cps_genderid", "qsexe", "gender", "cps_gender"),
  province_territory = c("cps_province", "regio", "province", "province_territory"),
  education = c("cps_edu", "qscol", "education"),
  political_interest = c("cps_interest_1", "cps_intelection_1", "qinterest"),
  turnout = c("cps_turnout", "q1", "turnout"),
  votechoice = c("cps_votechoice1", "qes_votechoice", "qvote", "q2"),
  votechoice_text = c("cps_votechoice1_8_TEXT", "votechoice_text"),
  party_best = c("cps_partybest", "partybest", "qpartybest"),
  partylean = c("cps_votelean", "votelean", "partylean"),
  fed_pid = c("cps_fedpid", "fed_pid", "fpid"),
  prov_pid = c("cps_provpid", "prov_pid", "ppid"),
  ideology = c("cps_ideoself_1", "lr", "ideology"),
  income = c("cps_income", "income"),
  religion = c("cps_religion", "religion"),
  born_canada = c("cps_borncda", "born_canada")
)
pick_v044 <- function(names, candidates) {
  hit <- candidates[candidates %in% names]
  if (length(hit) > 0L) {
    return(hit[1])
  }
  for (cand in candidates) {
    i <- match(tolower(cand), tolower(names))
    if (!is.na(i)) {
      return(names[i])
    }
  }
  NA_character_
}

master_rows <- data.frame(
  profile = "master",
  column = source_map$harmonized_variable,
  study = source_map$qes_code,
  source_variable = source_map$source_variable,
  kind = ifelse(source_map$harmonized_variable %in% master_columns, "core", "stacked"),
  origin = "source_map attribute of get_qes_master(), qesR 0.4.4 (tag v0.4.4)",
  stringsAsFactors = FALSE
)
decon_rows <- do.call(rbind, lapply(studies, function(s) {
  nm <- names_v044$name[names_v044$study == s]
  data.frame(
    profile = "decon",
    column = names(decon_lookup),
    study = s,
    source_variable = vapply(decon_lookup, function(c) pick_v044(nm, c), character(1)),
    kind = "core",
    origin = "get_decon() lookup of qesR 0.4.4 (tag v0.4.4) over its get_qes() names",
    stringsAsFactors = FALSE
  )
}))
frozen <- rbind(master_rows, decon_rows)
rownames(frozen) <- NULL

stacked <- frozen[frozen$profile == "master" & frozen$kind == "stacked" & !is.na(frozen$source_variable), ]
removed <- do.call(rbind, lapply(unique(frozen$column[frozen$kind == "stacked"]), function(col) {
  rows <- stacked[stacked$column == col, ]
  data.frame(
    column = col,
    studies = paste(rows$study, collapse = ";"),
    source_variables = paste(unique(rows$source_variable), collapse = ";"),
    stringsAsFactors = FALSE
  )
}))

write_utf8_csv <- function(x, path) {
  quote <- function(v) {
    out <- paste0("\"", gsub("\"", "\"\"", enc2utf8(as.character(v)), fixed = TRUE), "\"")
    out[is.na(v)] <- ""
    out
  }
  lines <- c(
    paste(quote(names(x)), collapse = ","),
    if (nrow(x) > 0L) do.call(paste, c(lapply(x, quote), sep = ","))
  )
  text <- paste0(paste(lines, collapse = "\n"), "\n")
  if (check_only) {
    if (!file.exists(path) || !identical(readBin(path, "raw", file.size(path)), charToRaw(text))) {
      stop(sprintf("%s is not what data-raw/build_legacy.R writes.", path), call. = FALSE)
    }
    return(invisible())
  }
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(text), con)
}

dir.create(file.path("inst", "extdata", "legacy"), showWarnings = FALSE)
write_utf8_csv(frozen, file.path("data-raw", "legacy_source_map.csv"))
write_utf8_csv(removed, file.path("inst", "extdata", "legacy", "removed.csv"))
cat(sprintf("frozen rows: %d; removed columns: %d\n", nrow(frozen), nrow(removed)))
