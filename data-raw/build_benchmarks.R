#!/usr/bin/env Rscript

# Benchmarks for the live validation (design.md sections 5.10 and 8.3, slice
# HZ7; data needs R3 and R5 of section 13.3).
#
# Usage (from the package root):
#   QESR_BENCH_SRC=<dir> Rscript data-raw/build_benchmarks.R [--check]
#
# <dir> holds the source files, downloaded verbatim (one plain request each,
# no personal information, at least 1 s apart):
#   dgeq_json/gen<date>_resultats.json   Élections Québec's archived official
#       results of each general election, as its results pages load them:
#       https://donnees.electionsquebec.qc.ca/production/provincial/resultats/archives/gen<date>/resultats.json
#   zips/98100020-eng.zip, zips/98100218-eng.zip, zips/98100384-eng.zip
#       Statistics Canada's full-table CSVs of the census tables 98-10-0020-01
#       (age in single years by gender, 2016 and 2021), 98-10-0218-01 (mother
#       tongue by age, 2011, 2016 and 2021) and 98-10-0384-01 (highest
#       certificate by age and gender, 2006, 2011 (National Household
#       Survey), 2016 and 2021): https://www150.statcan.gc.ca/n1/tbl/csv/<pid>-eng.zip
#
# It writes three tables to inst/extdata/validation/ (read by
# R/validation.R):
#   official_results.csv  province-wide votes and share of valid votes of each
#       party, in the party levels of the spec (party_qc); every other party
#       and the independents are one row "other";
#   official_turnout.csv  registered electors, ballots cast, valid and rejected
#       ballots; turnout is ballots cast over registered electors (for 2014
#       the JSON's own rate, 71.4326, disagrees with its counts, 71.4361, and
#       with the turnout history page, 71.44: the counts are used);
#   census_margins.csv    Quebec margins of gender, age, mother tongue and
#       education at the census that precedes each study.
#
# Age cuts. The electorate is 18 and over, but only table 98-10-0020-01 (2016
# and 2021) has single years of age. Elsewhere the nearest published cut is
# used, and the survey side of each comparison is cut the same way where the
# respondent's age is known (R/validation.R):
#   gender       18+ in 2016 and 2021 (98-10-0020-01, whole population);
#                20+ in 2006 and 2011 (98-10-0384-01, private households;
#                the 2011 count is from the National Household Survey);
#   age_group6   18+ in 2016 and 2021 (six bands 18-24 to 65+); 25+ in 2006
#                and 2011 (98-10-0384-01 starts its bands at 15-19 and
#                20-24, so the 18-24 band cannot be formed: five bands
#                25-34 to 65+);
#   lang_mother  20+ (98-10-0218-01 has 15-19 and 20-24 bands), persons
#                outside institutions, single answers only (French, English,
#                a non-official language); multiple mother tongues are left
#                out, as the surveys' multiple answers are (not_mappable);
#                not published for 2006 in the collected tables;
#   education    25+ (25-64 and 65+ of 98-10-0384-01), private households,
#                highest certificate in two groups: university (any
#                university certificate, diploma or degree) and below
#                university. The census counts the certificates completed,
#                the surveys the level reached (a year of CEGEP or of
#                university counts), and a trades certificate (in Quebec
#                mostly the secondary-level DEP) is secondary in some
#                questionnaires and technical (college) in others, so the
#                college level cannot be compared; the university share is
#                still expected to be higher in the surveys.
#
# With --check it writes nothing and fails when a table differs.

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
src <- Sys.getenv("QESR_BENCH_SRC")
if (!nzchar(src) || !dir.exists(src)) {
  stop("Set QESR_BENCH_SRC to the directory holding dgeq_json/ and zips/.", call. = FALSE)
}
pkgload::load_all(".", quiet = TRUE, export_all = TRUE)
out_dir <- file.path("inst", "extdata", "validation")
dir.create(out_dir, showWarnings = FALSE)

# ---- Élections Québec ------------------------------------------------------------------
elections <- .qes_catalog()$elections
elections <- elections[elections$election_id %in% c("QC1998", "QC2007", "QC2008", "QC2012", "QC2014", "QC2018", "QC2022"), ]

# DGEQ party names to the levels of party_qc. Abbreviations are not stable
# keys (P.C.Q. is the Parti communiste in 1994 and 1998, and in 2022
# "P.C.Q./C.P.Q" is the Parti canadien du Québec), so the full name is used.
party_level <- function(name) {
  n <- tolower(name)
  out <- rep("other", length(n))
  out[grepl("^parti libéral du québec", n)] <- "PLQ"
  out[grepl("^parti québécois$", n)] <- "PQ"
  out[grepl("^action démocratique du québec", n)] <- "ADQ"
  out[grepl("^coalition avenir québec", n)] <- "CAQ"
  out[grepl("^québec solidaire$", n)] <- "QS"
  out[grepl("^parti vert du québec", n)] <- "PVQ"
  out[grepl("^option nationale", n)] <- "ON"
  out[grepl("parti conservateur du québec", n)] <- "PCQ"
  out
}

res <- list()
turn <- list()
for (k in seq_len(nrow(elections))) {
  e <- elections[k, ]
  url <- e$source_url
  j <- jsonlite::fromJSON(file.path(src, "dgeq_json", sprintf("gen%s_resultats.json", format(e$election_date))))
  s <- j$statistiques
  p <- s$partisPolitiques
  stopifnot(sum(p$nbVoteTotal) == s$nbVoteValide)
  lvl <- party_level(p$nomPartiPolitique)
  # each named level is one party at one election
  stopifnot(!anyDuplicated(lvl[lvl != "other"]))
  levels <- c("PLQ", "PQ", "ADQ", "CAQ", "QS", "PVQ", "ON", "PCQ", "other")
  rows <- lapply(levels, function(l) {
    in_l <- lvl == l
    if (!any(in_l)) {
      return(NULL)
    }
    names_l <- if (l == "other") {
      sprintf("%d other parties and independent candidates", sum(in_l))
    } else {
      p$nomPartiPolitique[in_l]
    }
    data.frame(
      election_id = e$election_id, party = l, name_official = names_l,
      votes = sum(p$nbVoteTotal[in_l]), votes_valid = s$nbVoteValide,
      share_valid = round(100 * sum(p$nbVoteTotal[in_l]) / s$nbVoteValide, 4),
      source_url = url, stringsAsFactors = FALSE
    )
  })
  res[[k]] <- do.call(rbind, rows)
  turn[[k]] <- data.frame(
    election_id = e$election_id, registered = s$nbElecteurInscrit, ballots_cast = s$nbVoteExerce,
    ballots_valid = s$nbVoteValide, ballots_rejected = s$nbVoteRejete,
    turnout = round(100 * s$nbVoteExerce / s$nbElecteurInscrit, 4),
    source_url = url, stringsAsFactors = FALSE
  )
}
results <- do.call(rbind, res)
turnout <- do.call(rbind, turn)
rownames(results) <- rownames(turnout) <- NULL

# ---- Statistics Canada -----------------------------------------------------------------
read_zip <- function(pid) {
  zip <- file.path(src, "zips", paste0(pid, "-eng.zip"))
  x <- utils::read.csv(unz(zip, paste0(pid, ".csv")), check.names = FALSE, encoding = "UTF-8",
                       stringsAsFactors = FALSE)
  names(x)[1] <- sub("^﻿", "", names(x)[1])
  names(x) <- sub("^ï»¿", "", names(x))
  x[x$GEO == "Quebec", , drop = FALSE]
}
table_url <- function(pid) sprintf("https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=%s01", pid)
census <- list()
add <- function(year, variable, universe, counts, table, note) {
  census[[length(census) + 1L]] <<- data.frame(
    census_year = as.integer(year), variable = variable, level = names(counts), universe = universe,
    count = as.numeric(counts), share = round(100 * as.numeric(counts) / sum(counts), 4),
    source_table = sprintf("%s-%s-%s-01", substr(table, 1, 2), substr(table, 3, 4), substr(table, 5, 8)),
    source_url = table_url(table), note = note, stringsAsFactors = FALSE
  )
}

# 98-10-0020-01: single years of age by gender, 2016 and 2021
a <- read_zip("98100020")
age_col <- grep("^Age \\(in single years\\)", names(a), value = TRUE)
lab <- trimws(a[[age_col]])
age <- ifelse(grepl("^[0-9]+( years?)?$", lab), suppressWarnings(as.integer(sub(" .*$", "", lab))), NA_integer_)
age[lab == "100 years and over"] <- 100L
age[lab == "Under 1 year"] <- 0L
a$age <- age
men <- grep("^Gender \\(3a\\):Men\\+", names(a), value = TRUE)
women <- grep("^Gender \\(3a\\):Women\\+", names(a), value = TRUE)
total <- grep("^Gender \\(3a\\):Total - Gender", names(a), value = TRUE)
for (yr in c(2016L, 2021L)) {
  y <- a[a[["Census year (2)"]] == yr & !is.na(a$age), , drop = FALSE]
  stopifnot(identical(sort(y$age), 0:100))
  ad <- y[y$age >= 18, ]
  add(yr, "gender", "18+", c(man = sum(ad[[men]]), woman = sum(ad[[women]])), "98100020",
      "Whole population, men+ and women+ (non-binary persons are distributed into the two categories by Statistics Canada).")
  band <- cut(ad$age, c(17, 24, 34, 44, 54, 64, Inf), labels = c("a18_24", "a25_34", "a35_44", "a45_54", "a55_64", "a65_plus"))
  add(yr, "age_group6", "18+", tapply(ad[[total]], band, sum), "98100020", "Whole population.")
}

# 98-10-0384-01: highest certificate by age and gender, 2006, 2011 (NHS),
# 2016 and 2021, private households
ed <- read_zip("98100384")
ed <- ed[ed[["Statistics (2A)"]] == "Count", , drop = FALSE]
cert <- "Highest certificate, diploma or degree (15)"
year_col <- function(yr) grep(sprintf("^Census year \\(4\\):%d", yr), names(ed), value = TRUE)
ed_count <- function(yr, age, gender = "Total - Gender", certificate = "Total - Highest certificate, diploma or degree") {
  r <- ed[ed[["Age (15A)"]] %in% age & ed[["Gender (3a)"]] == gender & ed[[cert]] %in% certificate, , drop = FALSE]
  stopifnot(nrow(r) == length(age) * length(certificate))
  sum(as.numeric(r[[year_col(yr)]]))
}
for (yr in c(2006L, 2011L)) {
  src_note <- if (yr == 2011L) "National Household Survey 2011 (voluntary), " else ""
  a20 <- c("20 to 24 years", "25 to 64 years", "65 years and over")
  add(yr, "gender", "20+", c(man = ed_count(yr, a20, "Men+"), woman = ed_count(yr, a20, "Women+")), "98100384",
      paste0(src_note, "private households; the nearest cut to 18+ in the published tables."))
  bands <- c(a25_34 = "25 to 34 years", a35_44 = "35 to 44 years", a45_54 = "45 to 54 years",
             a55_64 = "55 to 64 years", a65_plus = "65 years and over")
  add(yr, "age_group6", "25+", vapply(bands, function(b) ed_count(yr, b), numeric(1)), "98100384",
      paste0(src_note, "private households; five bands from 25, as the tables cannot form 18-24."))
}
groups <- list(
  below_university = c("No certificate, diploma or degree",
                       "High (secondary) school diploma or equivalency certificate",
                       "Apprenticeship or trades certificate or diploma",
                       "College, CEGEP or other non-university certificate or diploma"),
  university = c("University certificate or diploma below bachelor level", "Bachelor’s degree or higher")
)
for (yr in c(2006L, 2011L, 2016L, 2021L)) {
  a25 <- c("25 to 64 years", "65 years and over")
  counts <- vapply(groups, function(g) ed_count(yr, a25, certificate = g), numeric(1))
  # Statistics Canada rounds counts at random to a multiple of 5
  stopifnot(abs(sum(counts) - ed_count(yr, a25)) <= 50)
  add(yr, "education", "25+", counts, "98100384",
      paste0(if (yr == 2011L) "National Household Survey 2011 (voluntary), " else "",
             "private households; highest certificate completed: university or below."))
}

# 98-10-0218-01: mother tongue by age, 2011, 2016 and 2021, persons outside
# institutions
mt <- read_zip("98100218")
mt_col <- function(yr) grep(sprintf("^Statistics \\(6A\\):%d Counts", yr), names(mt), value = TRUE)
for (yr in c(2011L, 2016L, 2021L)) {
  a20 <- c("20 to 24 years", "25 to 44 years", "45 to 64 years", "65 years and over")
  lv <- c(french = "French", english = "English", other = "Non-official language")
  counts <- vapply(lv, function(l) {
    r <- mt[mt[["Age (13B)"]] %in% a20 & mt[["Mother tongue (8)"]] == l, , drop = FALSE]
    stopifnot(nrow(r) == length(a20))
    sum(as.numeric(r[[mt_col(yr)]]))
  }, numeric(1))
  add(yr, "lang_mother", "20+", counts, "98100218",
      "Persons outside institutions; single mother tongues only (multiple answers left out); the nearest cut to 18+ in the published tables.")
}
census <- do.call(rbind, census)
rownames(census) <- NULL

# ---- write or check --------------------------------------------------------------------
tables <- list(official_results.csv = results, official_turnout.csv = turnout, census_margins.csv = census)
failed <- FALSE
for (f in names(tables)) {
  path <- file.path(out_dir, f)
  if (check_only) {
    tmp <- tempfile(fileext = ".csv")
    .qes_write_csv(tables[[f]], tmp)
    same <- file.exists(path) && identical(unname(tools::md5sum(tmp)), unname(tools::md5sum(path)))
    if (!same) {
      failed <- TRUE
      cat(f, ": differs from the sources\n")
    }
  } else {
    .qes_write_csv(tables[[f]], path)
    cat("wrote", path, "(", nrow(tables[[f]]), "rows )\n")
  }
}
if (failed) quit(status = 1L)
if (check_only) cat("Benchmarks: the shipped tables equal the sources.\n")
