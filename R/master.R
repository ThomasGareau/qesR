.recode_turnout_by_study <- function(out, raw, qes_code) {
  if (length(out) == 0L) {
    return(out)
  }

  code <- as.character(qes_code)
  raw_chr <- .normalize_master_text(raw)
  raw_num <- suppressWarnings(as.numeric(as.character(raw)))

  # qes2018 Q5: 4 = voted, 1/2/3 = did not vote, 5 = not eligible.
  idx_2018 <- code == "qes2018"
  if (any(idx_2018, na.rm = TRUE)) {
    out[idx_2018] <- NA_real_
    out[idx_2018 & raw_num %in% c(4)] <- 1
    out[idx_2018 & raw_num %in% c(1, 2, 3)] <- 0
    out[idx_2018 & grepl("\\ba vote\\b|certain davoir vote", raw_chr, perl = TRUE)] <- 1
    out[idx_2018 & grepl("n a pas vote|voulait voter|vote generalement mais n a pas vote", raw_chr, perl = TRUE)] <- 0
  }

  # qes2018_panel: use vote behavior coding when available; fall back to RTS wording.
  idx_2018_panel <- code == "qes2018_panel"
  if (any(idx_2018_panel, na.rm = TRUE)) {
    out[idx_2018_panel] <- NA_real_

    # vote labels: party choices imply voted; explicit no-vote/spoiled imply no.
    vote_no_hit <- idx_2018_panel & grepl(
      "n a pas vote|n'a pas vote|n a pas vot|n'a pas vot|annul",
      raw_chr,
      perl = TRUE
    )
    vote_yes_hit <- idx_2018_panel & grepl(
      "parti liberal|parti quebec|coalition avenir|quebec solidaire|parti vert|option nationale|autre parti",
      raw_chr,
      perl = TRUE
    )
    out[vote_no_hit] <- 0
    out[vote_yes_hit & !vote_no_hit] <- 1

    # rts_q1 style fallback.
    no_hit <- idx_2018_panel & (
      raw_num %in% c(1, 2) |
      grepl("navez pas pu|decide de ne pas|ne pas aller voter", raw_chr, perl = TRUE)
    )
    yes_hit <- idx_2018_panel & (
      raw_num %in% c(3) |
      grepl("etes alle voter|est alle voter|alle voter", raw_chr, perl = TRUE)
    )

    out[no_hit & is.na(out)] <- 0
    out[yes_hit & !no_hit & is.na(out)] <- 1
  }

  # qes1998 q1post: 1 = yes voted, 2 = no.
  idx_1998 <- code == "qes1998"
  if (any(idx_1998, na.rm = TRUE)) {
    out[idx_1998] <- NA_real_
    out[idx_1998 & (raw_num %in% c(1) | raw_chr %in% c("oui", "yes"))] <- 1
    out[idx_1998 & (raw_num %in% c(2) | raw_chr %in% c("non", "no"))] <- 0

    # allervot fallback when q1post is not available.
    out[idx_1998 & is.na(out) & raw_num %in% c(1, 2)] <- 1
    out[idx_1998 & is.na(out) & raw_num %in% c(3, 4)] <- 0
    out[idx_1998 & is.na(out) & grepl("certain|tres prob|assez prob", raw_chr, perl = TRUE)] <- 1
    out[idx_1998 & is.na(out) & grepl("peu prob|pas prob", raw_chr, perl = TRUE)] <- 0
  }

  # qes2012_panel participation: 1/2 = yes (different voting modes), 3 = no.
  idx_2012_panel <- code == "qes2012_panel"
  if (any(idx_2012_panel, na.rm = TRUE)) {
    out[idx_2012_panel & raw_num %in% c(1, 2)] <- 1
    out[idx_2012_panel & raw_num %in% c(3)] <- 0
  }

  out
}

.recode_sovereignty_by_study <- function(out, raw, qes_code) {
  if (length(out) == 0L) {
    return(out)
  }

  code <- as.character(qes_code)
  raw_chr <- .normalize_master_text(raw)
  raw_num <- suppressWarnings(as.numeric(as.character(raw)))

  # qes2018 Q26: 1 = Yes, 2 = No, 8/9 = DK/PNTS.
  idx_2018 <- code == "qes2018"
  if (any(idx_2018, na.rm = TRUE)) {
    out[idx_2018] <- NA_real_
    out[idx_2018 & (raw_num %in% c(1) | raw_chr %in% c("oui", "yes"))] <- 1
    out[idx_2018 & (raw_num %in% c(2) | raw_chr %in% c("non", "no"))] <- 0
  }

  # qes1998 voteref: keep explicit Yes/No only.
  idx_1998 <- code == "qes1998"
  if (any(idx_1998, na.rm = TRUE)) {
    out[idx_1998] <- NA_real_
    out[idx_1998 & (raw_num %in% c(1) | raw_chr %in% c("oui", "yes"))] <- 1
    out[idx_1998 & (raw_num %in% c(2) | raw_chr %in% c("non", "no"))] <- 0
  }

  # qes2018_panel independance is already binary: 1 yes / 0 no.
  idx_2018_panel <- code == "qes2018_panel"
  if (any(idx_2018_panel, na.rm = TRUE)) {
    out[idx_2018_panel & (raw_num %in% c(1) | raw_chr %in% c("oui", "yes"))] <- 1
    out[idx_2018_panel & (raw_num %in% c(0) | raw_chr %in% c("non", "no"))] <- 0
  }

  # Legacy studies with 1=yes and 2=no coding.
  legacy <- code %in% c("qes2007", "qes2008", "qes2012", "qes2012_panel", "qes2014", "qes2007_panel", "qes_crop_2007_2010")
  if (any(legacy, na.rm = TRUE)) {
    out[legacy & (raw_num %in% c(1) | raw_chr %in% c("oui", "yes"))] <- 1
    out[legacy & (raw_num %in% c(2) | raw_chr %in% c("non", "no"))] <- 0
  }

  out
}

.recode_language_by_study <- function(out, raw, qes_code) {
  if (length(out) == 0L) {
    return(out)
  }

  code <- as.character(qes_code)
  raw_chr <- .normalize_master_text(raw)
  raw_num <- suppressWarnings(as.numeric(as.character(raw)))
  out <- as.character(out)

  # qes2018 QLANGUE programmed values:
  # 1 French, 2 English, 96 Other, 98 DK, 99 PNR.
  idx_2018 <- code == "qes2018"
  if (any(idx_2018, na.rm = TRUE)) {
    out[idx_2018] <- NA_character_
    out[idx_2018 & (raw_num %in% c(1) | grepl("francais|french", raw_chr, perl = TRUE))] <- "French"
    out[idx_2018 & (raw_num %in% c(2) | grepl("anglais|english", raw_chr, perl = TRUE))] <- "English"
    out[idx_2018 & (
      raw_num %in% c(96, 3) |
      grepl("^autre$|\\bother\\b", raw_chr, perl = TRUE)
    )] <- "Other"
  }

  # qes2007 can include unlabelled non-1/2 codes for other languages.
  idx_2007 <- code == "qes2007"
  if (any(idx_2007, na.rm = TRUE)) {
    out[idx_2007 & raw_num %in% c(1)] <- "French"
    out[idx_2007 & raw_num %in% c(2)] <- "English"
    out[idx_2007 & !is.na(raw_num) & !(raw_num %in% c(1, 2, 9, 98, 99))] <- "Other"
  }

  out
}

.fill_study_constants <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L || !("qes_code" %in% names(master))) {
    return(master)
  }

  code <- as.character(master$qes_code)

  if ("language" %in% names(master)) {
    # 1998 files retained in this package are francophone-only.
    idx <- code == "qes1998" & is.na(master$language)
    master$language[idx] <- "French"
  }

  if ("province_territory" %in% names(master)) {
    idx <- code != "qes2022" & is.na(master$province_territory)
    master$province_territory[idx] <- "Quebec"
  }

  master
}

.normalize_master_text <- function(x) {
  out <- as.character(x)
  out <- trimws(out)
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out <- tolower(out)
  out <- gsub("['`]", "", out, perl = TRUE)
  out <- gsub("[^a-z0-9]+", " ", out, perl = TRUE)
  trimws(out)
}

.coerce_turnout_binary <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }

  raw <- as.character(x)
  norm <- .normalize_master_text(raw)
  out <- rep(NA_real_, length(norm))

  parsed_num <- suppressWarnings(as.numeric(raw))
  finite_num <- is.finite(parsed_num)
  if (any(finite_num)) {
    uniq <- unique(parsed_num[finite_num])
    if (all(uniq %in% c(0, 1))) {
      out[finite_num] <- parsed_num[finite_num]
    }
  }

  no_hit <- grepl(
    "^(0|2|non|no)$|\\bnon\\b|\\bno\\b|n[' ]?a pas vot|did not vot|didn[' ]?t vot|ne votera pas|nvp|annule|not vote|not voted|certain not to vote",
    norm,
    perl = TRUE
  )
  yes_hit <- grepl(
    "^(1|oui|yes)$|\\boui\\b|\\byes\\b|a vote|alle voter|already voted|par anticipation|le jour meme|certain to vote",
    norm,
    perl = TRUE
  )

  out[no_hit] <- 0
  out[yes_hit & !no_hit] <- 1
  out[.master_missing_vector(raw)] <- NA_real_
  out
}

.coerce_sovereignty_binary <- function(x) {
  if (length(x) == 0L) {
    return(x)
  }

  raw <- as.character(x)
  norm <- .normalize_master_text(raw)
  out <- rep(NA_real_, length(norm))

  parsed_num <- suppressWarnings(as.numeric(raw))
  finite_num <- is.finite(parsed_num)
  if (any(finite_num)) {
    uniq <- unique(parsed_num[finite_num])
    if (all(uniq %in% c(0, 1))) {
      out[finite_num] <- parsed_num[finite_num]
    }
  }

  no_hit <- grepl(
    "^(0|2|non|no)$|\\bnon\\b|\\bno\\b|federalist|federaliste|signer la constitution|no change",
    norm,
    perl = TRUE
  )
  yes_hit <- grepl(
    "^(1|oui|yes)$|\\boui\\b|\\byes\\b|independ|souverain",
    norm,
    perl = TRUE
  )

  out[no_hit] <- 0
  out[yes_hit & !no_hit] <- 1
  out[.master_missing_vector(raw)] <- NA_real_
  out
}

.master_missing_vector <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }

  if (is.character(x)) {
    out <- is.na(x) | !nzchar(trimws(x))
    lowered <- tolower(trimws(x))
    out <- out | lowered %in% c("na", "n/a", "nan", "null")
    return(out)
  }

  is.na(x)
}

.derive_crop_collection_year <- function(x) {
  n <- length(x)
  if (n == 0L) {
    return(character(0))
  }

  out <- rep(NA_character_, n)
  x_num <- suppressWarnings(as.numeric(as.character(x)))

  labs <- attr(x, "labels", exact = TRUE)
  if (!is.null(labs) && length(labs) > 0L) {
    lab_vals <- suppressWarnings(as.numeric(unname(labs)))
    lab_txt <- names(labs)
    lab_year <- sub(".*([12][0-9]{3}).*$", "\\1", lab_txt, perl = TRUE)
    bad <- !grepl("^[12][0-9]{3}$", lab_year)
    lab_year[bad] <- NA_character_

    key <- as.character(lab_vals)
    year_map <- stats::setNames(lab_year, key)
    hit <- as.character(x_num)
    mapped <- unname(year_map[hit])
    out[!is.na(mapped)] <- mapped[!is.na(mapped)]
  }

  # Defensive fallback if labels are missing or malformed.
  idx <- is.na(out) & is.finite(x_num)
  out[idx & x_num >= 1 & x_num <= 5] <- "2007"
  out[idx & x_num >= 6 & x_num <= 15] <- "2008"
  out[idx & x_num >= 16 & x_num <= 23] <- "2009"
  out[idx & x_num == 24] <- "2010"

  out
}

.parse_reference_year <- function(x) {
  if (is.na(x)) {
    return(NA_real_)
  }

  txt <- as.character(x)
  hits <- gregexpr("[0-9]{4}", txt, perl = TRUE)[[1]]
  if (identical(hits[1], -1L)) {
    return(NA_real_)
  }

  lens <- attr(hits, "match.length")
  start <- hits[length(hits)]
  width <- lens[length(lens)]
  suppressWarnings(as.numeric(substr(txt, start, start + width - 1L)))
}

.derive_age_numeric <- function(age, yob, ref_year) {
  age_num <- suppressWarnings(as.numeric(age))
  age_num[!is.finite(age_num)] <- NA_real_
  age_num[age_num < 13 | age_num > 120] <- NA_real_
  yob_num <- suppressWarnings(as.numeric(yob))

  calc <- ref_year - yob_num
  calc[!is.finite(calc)] <- NA_real_
  calc[calc < 0 | calc > 120] <- NA_real_

  fill <- is.na(age_num) & !is.na(calc)
  age_num[fill] <- calc[fill]
  age_num
}

.age_to_group <- function(age_num) {
  out <- rep(NA_character_, length(age_num))

  out[!is.na(age_num) & age_num < 18] <- "<18"
  out[!is.na(age_num) & age_num >= 18 & age_num <= 24] <- "18-24"
  out[!is.na(age_num) & age_num >= 25 & age_num <= 34] <- "25-34"
  out[!is.na(age_num) & age_num >= 35 & age_num <= 44] <- "35-44"
  out[!is.na(age_num) & age_num >= 45 & age_num <= 54] <- "45-54"
  out[!is.na(age_num) & age_num >= 55 & age_num <= 64] <- "55-64"
  out[!is.na(age_num) & age_num >= 65] <- "65+"

  out
}

.recode_age_group_by_study <- function(age_group, qes_code) {
  out <- as.character(age_group)
  code <- as.character(qes_code)

  # qes_crop_2007_2010 stores grouped ages as numeric category codes.
  idx <- code == "qes_crop_2007_2010"
  if (any(idx, na.rm = TRUE)) {
    map <- c(
      "1" = "18-24",
      "2" = "25-34",
      "3" = "35-44",
      "4" = "45-54",
      "5" = "55-64",
      "6" = "65+"
    )
    vals <- trimws(out[idx])
    rec <- unname(map[vals])
    keep_old <- is.na(rec) | !nzchar(rec)
    rec[keep_old] <- vals[keep_old]
    out[idx] <- rec
  }

  out
}

.standardize_age_group_labels <- function(x) {
  out <- as.character(x)
  if (length(out) == 0L) {
    return(out)
  }

  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  # Some files store age values in the age-group field.
  num <- suppressWarnings(as.numeric(out))
  valid_num_age <- is.finite(num) & num >= 13 & num <= 120
  rec[valid_num_age] <- .age_to_group(num[valid_num_age])

  exact_map <- list(
    "<18" = c("<18", "under 18", "moins de 18", "moins de 18 ans"),
    "18-24" = c("18 24", "18 to 24", "de 18 a 24 ans"),
    "25-34" = c("25 34", "25 to 34", "de 25 a 34 ans"),
    "35-44" = c("35 44", "35 to 44", "de 35 a 44 ans"),
    "45-54" = c("45 54", "45 to 54", "de 45 a 54 ans"),
    "55-64" = c("55 64", "55 to 64", "de 55 a 64 ans"),
    "65+" = c("65 plus", "65 ans et plus", "65 and over"),
    "18-34" = c("18 34", "18 34 ans", "de 18 a 34 ans"),
    "35-54" = c("35 54", "35 54 ans", "de 35 a 54 ans"),
    "55+" = c("55 plus", "55 ans et plus", "55 and over", "55")
  )

  for (label in names(exact_map)) {
    vals <- exact_map[[label]]
    hit <- is.na(rec) & !is.na(norm) & norm %in% vals
    rec[hit] <- label
  }

  # Fallback regex in case labels vary slightly across files.
  rec[is.na(rec) & grepl("\\b18\\b.*\\b24\\b", norm, perl = TRUE)] <- "18-24"
  rec[is.na(rec) & grepl("\\b25\\b.*\\b34\\b", norm, perl = TRUE)] <- "25-34"
  rec[is.na(rec) & grepl("\\b35\\b.*\\b44\\b", norm, perl = TRUE)] <- "35-44"
  rec[is.na(rec) & grepl("\\b45\\b.*\\b54\\b", norm, perl = TRUE)] <- "45-54"
  rec[is.na(rec) & grepl("\\b55\\b.*\\b64\\b", norm, perl = TRUE)] <- "55-64"
  rec[is.na(rec) & grepl("\\b18\\b.*\\b34\\b", norm, perl = TRUE)] <- "18-34"
  rec[is.na(rec) & grepl("\\b35\\b.*\\b54\\b", norm, perl = TRUE)] <- "35-54"
  rec[is.na(rec) & grepl("\\b65\\b", norm, perl = TRUE)] <- "65+"
  rec[is.na(rec) & grepl("\\b55\\b.*\\b(plus|et plus|and over)\\b", norm, perl = TRUE)] <- "55+"

  # Keep unknown strings only if they are not obvious non-substantive values.
  non_substantive <- is.na(rec) & grepl("refus|pas de reponse|dont know|don t know|refused|prefer not", norm, perl = TRUE)
  rec[non_substantive] <- NA_character_

  rec
}

.standardize_language_labels <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  french_hit <- grepl("\\b(francais|french|fr ca|frca|fr)\\b", norm, perl = TRUE)
  english_hit <- grepl("\\b(anglais|english|en)\\b", norm, perl = TRUE)
  other_hit <- grepl("\\b(autre|other)\\b", norm, perl = TRUE)
  dk_hit <- grepl("nsp|refus|dont know|don t know|prefer not", norm, perl = TRUE)

  rec[french_hit] <- "French"
  rec[english_hit & !french_hit] <- "English"
  rec[other_hit & !french_hit & !english_hit] <- "Other"
  rec[dk_hit] <- NA_character_

  numeric_code <- is.na(rec) & grepl("^[0-9]+$", norm)
  rec[numeric_code] <- NA_character_

  keep_original <- is.na(rec) & !is.na(out) & !numeric_code & !dk_hit
  rec[keep_original] <- out[keep_original]
  rec
}

.standardize_citizenship_labels <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  rec[grepl("canadian citizen|citoyen canad", norm, perl = TRUE)] <- "Canadian citizen"
  rec[grepl("permanent resident|resident permanent", norm, perl = TRUE)] <- "Permanent resident"
  rec[grepl("\\bother\\b|\\bautre\\b", norm, perl = TRUE)] <- "Other"

  keep_original <- is.na(rec) & !is.na(out)
  rec[keep_original] <- out[keep_original]
  rec
}

.standardize_born_canada <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  yes_hit <- grepl("^(1|yes|oui)$|\\byes\\b|\\boui\\b", norm, perl = TRUE)
  no_hit <- grepl("^(0|2|no|non)$|\\bno\\b|\\bnon\\b", norm, perl = TRUE)
  born_here_hit <- grepl(
    "in quebec|au quebec|another part of canada|ailleurs au canada|reste du canada|outside quebec but in canada",
    norm,
    perl = TRUE
  )
  born_elsewhere_hit <- grepl("somewhere else|ailleurs|outside canada|hors du canada", norm, perl = TRUE)
  dk_hit <- grepl("dont know|don t know|refus|prefer not", norm, perl = TRUE)

  rec[yes_hit] <- "Yes"
  rec[no_hit] <- "No"
  rec[born_here_hit] <- "Yes"
  rec[born_elsewhere_hit & !born_here_hit] <- "No"
  rec[dk_hit] <- NA_character_

  keep_original <- is.na(rec) & !is.na(out)
  rec[keep_original] <- out[keep_original]
  rec
}

.standardize_gender_labels <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  nonbinary_hit <- grepl("non binary|nonbinary", norm, perl = TRUE)
  other_hit <- grepl("another gender|autre genre", norm, perl = TRUE)
  woman_hit <- grepl("^(2)$|\\bwoman\\b|\\bfemale\\b|\\bfemme\\b|\\bfeminin\\b", norm, perl = TRUE)
  man_hit <- grepl("^(1)$|\\bman\\b|\\bmale\\b|\\bhomme\\b|\\bmasculin\\b", norm, perl = TRUE)
  dk_hit <- grepl("dont know|don t know|refus|prefer not", norm, perl = TRUE)

  rec[man_hit] <- "Man"
  rec[woman_hit & !man_hit] <- "Woman"
  rec[nonbinary_hit] <- "Non-binary"
  rec[other_hit] <- "Other"
  rec[dk_hit] <- NA_character_

  keep_original <- is.na(rec) & !is.na(out)
  rec[keep_original] <- out[keep_original]
  rec
}

.standardize_education_labels <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  # Explicit numeric coding used in some recent studies.
  code_map <- c(
    "0" = "Non-university",
    "1" = "Primary or less",
    "2" = "Primary or less",
    "3" = "Primary or less",
    "4" = "Secondary",
    "5" = "Secondary",
    "6" = "College/CEGEP/Technical",
    "7" = "College/CEGEP/Technical",
    "8" = "University",
    "9" = "University",
    "10" = "University",
    "11" = "University",
    "12" = "University",
    "13" = "University",
    "14" = "University",
    "15" = "University"
  )
  rec <- unname(code_map[norm])

  # qes1998 panel `scol` labels (years of schooling), mapped to the
  # categories qesR 0.4.4 gave these codes
  years_map <- c(
    "1 9 ans" = "Primary or less",
    "10 15 ans" = "College/CEGEP/Technical",
    "univ" = "University"
  )
  years_hit <- is.na(rec) & !is.na(norm) & norm %in% names(years_map)
  rec[years_hit] <- unname(years_map[norm[years_hit]])

  university_hit <- grepl(
    "universit|university|undergraduate|bachelor|baccalaureat|master|maitrise|doctor|postgraduate|higher education|professional degree|16 annees ou plus|etudes uni",
    norm,
    perl = TRUE
  )
  college_hit <- grepl(
    "cegep|college|technical|technique|certificate and diploma|post secondary|13 a 15 annees",
    norm,
    perl = TRUE
  )
  secondary_hit <- grepl(
    "secondaire|secondary|high school|8 a 12 annees|cours secondaire",
    norm,
    perl = TRUE
  )
  primary_hit <- grepl(
    "primaire|elementaire|elementary|no schooling|aucune scolarite|7 annees ou moins|cours primaire",
    norm,
    perl = TRUE
  )
  dk_hit <- grepl("refus|pas de reponse|dont know|don t know|prefer not|je prefere", norm, perl = TRUE) | norm %in% c("99", "-99")

  rec[is.na(rec) & university_hit] <- "University"
  rec[is.na(rec) & college_hit] <- "College/CEGEP/Technical"
  rec[is.na(rec) & secondary_hit] <- "Secondary"
  rec[is.na(rec) & primary_hit] <- "Primary or less"
  rec[dk_hit] <- NA_character_

  keep_original <- is.na(rec) & !is.na(out) & !dk_hit
  rec[keep_original] <- out[keep_original]
  rec
}

.standardize_province_territory <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  rec[grepl("alberta", norm, perl = TRUE)] <- "Alberta"
  rec[grepl("british columbia|colombie britannique", norm, perl = TRUE)] <- "British Columbia"
  rec[grepl("manitoba", norm, perl = TRUE)] <- "Manitoba"
  rec[grepl("new brunswick|nouveau brunswick", norm, perl = TRUE)] <- "New Brunswick"
  rec[grepl("newfoundland|labrador|terre neuve", norm, perl = TRUE)] <- "Newfoundland and Labrador"
  rec[grepl("northwest territories|territoires du nord ouest", norm, perl = TRUE)] <- "Northwest Territories"
  rec[grepl("nunavut", norm, perl = TRUE)] <- "Nunavut"
  rec[grepl("ontario", norm, perl = TRUE)] <- "Ontario"
  rec[grepl("prince edward island|ile du prince", norm, perl = TRUE)] <- "Prince Edward Island"
  rec[grepl("saskatchewan", norm, perl = TRUE)] <- "Saskatchewan"
  rec[grepl("yukon", norm, perl = TRUE)] <- "Yukon"

  quebec_region_hit <- grepl(
    "quebec|montreal|mtl rmr|qc rmr|mont er egie|monteregie|capitale nationale|chaudiere|laval|lanaudiere|estrie|laurentides|mauricie|outaouais|centre du quebec|saguenay|bas saint laurent|abitibi|cote nord|c ote nord|gaspesie|nord du quebec|rest of quebec|quebec cma|autres regions|reste du quebec",
    norm,
    perl = TRUE
  )
  rec[is.na(rec) & quebec_region_hit] <- "Quebec"

  # Many files use 1..17 for Quebec administrative regions.
  numeric_region <- is.na(rec) & grepl("^[0-9]+$", norm)
  num <- suppressWarnings(as.integer(norm))
  rec[numeric_region & !is.na(num) & num >= 1L & num <= 17L] <- "Quebec"

  keep_original <- is.na(rec) & !is.na(out)
  rec[keep_original] <- out[keep_original]
  rec
}

.coerce_scale_0_10 <- function(x, kind = c("generic", "interest", "ideology")) {
  kind <- match.arg(kind)
  raw <- as.character(x)
  norm <- .normalize_master_text(raw)
  out <- rep(NA_real_, length(raw))

  num <- suppressWarnings(as.numeric(raw))
  valid_num <- is.finite(num) & num >= 0 & num <= 10
  out[valid_num] <- num[valid_num]

  if (kind == "interest") {
    # Many legacy waves use 1-4 ordinal interest levels rather than 0-10.
    out[is.na(out) & num %in% c(1)] <- 10
    out[is.na(out) & num %in% c(2)] <- 7
    out[is.na(out) & num %in% c(3)] <- 3
    out[is.na(out) & num %in% c(4)] <- 0

    none_hit <- grepl("pas du tout|not at all|none", norm, perl = TRUE)
    low_hit <- grepl("^peu$|\\ba little\\b|little|hardly interested|pas tres interess", norm, perl = TRUE)
    mid_hit <- grepl("assez|fairly|somewhat|quite interested|plut\\s*ot interess", norm, perl = TRUE)
    high_hit <- grepl("beaucoup|very interested|a lot|tres interess", norm, perl = TRUE) &
      !grepl("pas tres interess", norm, perl = TRUE)

    out[is.na(out) & none_hit] <- 0
    out[is.na(out) & low_hit] <- 3
    out[is.na(out) & mid_hit] <- 7
    out[is.na(out) & high_hit] <- 10
  }

  if (kind == "ideology") {
    out[is.na(out) & grepl("0 most left|most left", norm, perl = TRUE)] <- 0
    out[is.na(out) & grepl("10 most right|most right", norm, perl = TRUE)] <- 10
  }

  dk_hit <- grepl("dont know|don t know|refus|prefer not", norm, perl = TRUE) | norm %in% c("-99", "99")
  out[dk_hit] <- NA_real_
  out
}

.standardize_party_label <- function(x, domain = c("provincial", "federal")) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  domain <- match.arg(domain)
  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)
  rec <- rep(NA_character_, length(out))

  dk_hit <- grepl(
    "nsp|refus|dont know|don t know|ne sais pas|ne sait pas|pas certain|prefer not|je prefere|pas de reponse|^[-+]?[0-9]+$",
    norm,
    perl = TRUE
  )
  none_hit <- grepl(
    "n a pas vote|na pas vote|n a pas vot|na pas vot|ne votera pas|annul|a annule son vote|annulerait|aucun|none of these|non rejoint|did not vote",
    norm,
    perl = TRUE
  )
  other_hit <- grepl("autre parti|another party|other party|un autre parti|^other$|egalite", norm, perl = TRUE)

  rec[dk_hit] <- "Don't know / Refused"
  rec[none_hit] <- "Did not vote / None"
  rec[other_hit] <- "Other party"

  if (domain == "federal") {
    rec[is.na(rec) & grepl("bloc quebec", norm, perl = TRUE)] <- "Bloc Quebecois"
    rec[is.na(rec) & grepl("\\bliberal\\b", norm, perl = TRUE)] <- "Liberal"
    rec[is.na(rec) & grepl("conserv", norm, perl = TRUE)] <- "Conservative"
    rec[is.na(rec) & grepl("\\bndp\\b|new democratic", norm, perl = TRUE)] <- "NDP"
    rec[is.na(rec) & grepl("\\bgreen\\b|parti vert", norm, perl = TRUE)] <- "Green"
    rec[is.na(rec) & grepl("\\bppc\\b|peoples party", norm, perl = TRUE)] <- "PPC"
  } else {
    rec[is.na(rec) & grepl("coalition avenir quebec|\\bcaq\\b|caquiste|francois legault", norm, perl = TRUE)] <- "CAQ"
    rec[is.na(rec) & grepl("parti liberal du quebec|quebec liberal party|\\bplq\\b|\\bliberal\\b", norm, perl = TRUE)] <- "PLQ"
    rec[is.na(rec) & grepl("parti quebecois|\\bpq\\b|pequiste", norm, perl = TRUE)] <- "PQ"
    rec[is.na(rec) & grepl("quebec solidaire|\\bqs\\b|\\bsolidaire\\b", norm, perl = TRUE)] <- "QS"
    rec[is.na(rec) & grepl("action democratique|\\badq\\b|^ladq$|\\bl adq\\b", norm, perl = TRUE)] <- "ADQ"
    rec[is.na(rec) & grepl("parti conservateur du quebec|\\bpcq\\b|\\bconservateur\\b", norm, perl = TRUE)] <- "PCQ"
    rec[is.na(rec) & grepl("parti vert du quebec|\\bpv\\b|parti vert|green party", norm, perl = TRUE)] <- "PVQ"
    rec[is.na(rec) & grepl("option nationale", norm, perl = TRUE)] <- "ON"
  }

  keep_original <- is.na(rec) & !is.na(out)
  rec[keep_original] <- out[keep_original]
  rec
}

.clean_vote_choice_text <- function(x) {
  if (!(is.character(x) || is.factor(x))) {
    return(x)
  }

  out <- as.character(x)
  out[.master_missing_vector(out)] <- NA_character_
  norm <- .normalize_master_text(out)

  code_like <- grepl("^[-+]?[0-9]+$", norm) | norm %in% c("-99", "95", "96", "97", "98", "99")
  dk_like <- grepl("dont know|don t know|refus|prefer not", norm, perl = TRUE)
  out[code_like | dk_like] <- NA_character_
  out
}

.recode_party_fields_by_study <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L || !("qes_code" %in% names(master))) {
    return(master)
  }

  code <- as.character(master$qes_code)
  map_qes2018 <- c(
    "1" = "PLQ",
    "2" = "PQ",
    "3" = "CAQ",
    "4" = "QS",
    "95" = "Did not vote / None",
    "96" = "Other party",
    "99" = "Don't know / Refused"
  )

  if ("vote_choice" %in% names(master)) {
    idx <- code == "qes2018"
    if (any(idx, na.rm = TRUE)) {
      vote <- as.character(master$vote_choice)
      rec <- unname(map_qes2018[trimws(vote)])
      hit <- idx & !is.na(rec)
      vote[hit] <- rec[hit]
      master$vote_choice <- vote
    }
  }

  if ("party_lean" %in% names(master)) {
    idx <- code == "qes2018"
    if (any(idx, na.rm = TRUE)) {
      lean <- as.character(master$party_lean)
      rec <- unname(map_qes2018[trimws(lean)])
      hit <- idx & !is.na(rec)
      lean[hit] <- rec[hit]
      master$party_lean <- lean
    }
  }

  master
}

.postprocess_master_dataset <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L) {
    return(master)
  }

  if (!("year_of_birth" %in% names(master))) {
    master$year_of_birth <- NA_real_
  }
  if (!("age" %in% names(master))) {
    master$age <- NA_real_
  }
  if (!("age_group" %in% names(master))) {
    master$age_group <- NA_character_
  }

  yob_num <- suppressWarnings(as.numeric(master$year_of_birth))
  valid_yob <- !is.na(yob_num) & yob_num >= 1850 & yob_num <= 2100
  yob_num[!valid_yob] <- NA_real_
  master$year_of_birth <- yob_num

  ref_year <- vapply(master$qes_year, .parse_reference_year, numeric(1))
  age_num <- .derive_age_numeric(master$age, master$year_of_birth, ref_year)
  master$age <- age_num

  existing_age_group <- as.character(master$age_group)
  existing_age_group[.master_missing_vector(existing_age_group)] <- NA_character_
  derived_age_group <- .age_to_group(age_num)

  # Prefer unified age groups derived from numeric age; fall back to existing text.
  final_age_group <- derived_age_group
  use_existing <- is.na(final_age_group) & !is.na(existing_age_group)
  final_age_group[use_existing] <- existing_age_group[use_existing]
  final_age_group <- .recode_age_group_by_study(
    age_group = final_age_group,
    qes_code = master$qes_code
  )
  final_age_group <- .standardize_age_group_labels(final_age_group)
  master$age_group <- final_age_group

  if ("survey_weight" %in% names(master)) {
    weight_num <- suppressWarnings(as.numeric(master$survey_weight))
    weight_missing <- .master_missing_vector(master$survey_weight)
    converted <- !is.na(weight_num) & !weight_missing
    if (sum(converted) >= 0.8 * max(1L, sum(!weight_missing))) {
      master$survey_weight <- weight_num
    }
  }

  if ("turnout" %in% names(master)) {
    turnout_raw <- master$turnout
    master$turnout <- .coerce_turnout_binary(turnout_raw)
    master$turnout <- .recode_turnout_by_study(
      out = master$turnout,
      raw = turnout_raw,
      qes_code = master$qes_code
    )
  }

  if ("sovereignty_support" %in% names(master)) {
    sov_raw <- master$sovereignty_support
    master$sovereignty_support <- .coerce_sovereignty_binary(sov_raw)
    master$sovereignty_support <- .recode_sovereignty_by_study(
      out = master$sovereignty_support,
      raw = sov_raw,
      qes_code = master$qes_code
    )
  }

  if ("sovereignty" %in% names(master)) {
    sov_raw <- master$sovereignty
    master$sovereignty <- .coerce_sovereignty_binary(sov_raw)
    master$sovereignty <- .recode_sovereignty_by_study(
      out = master$sovereignty,
      raw = sov_raw,
      qes_code = master$qes_code
    )
  }

  if (all(c("sovereignty_support", "sovereignty") %in% names(master))) {
    fill_from_support <- is.na(master$sovereignty) & !is.na(master$sovereignty_support)
    fill_from_sovereignty <- is.na(master$sovereignty_support) & !is.na(master$sovereignty)
    master$sovereignty[fill_from_support] <- master$sovereignty_support[fill_from_support]
    master$sovereignty_support[fill_from_sovereignty] <- master$sovereignty[fill_from_sovereignty]
  }

  if ("language" %in% names(master)) {
    lang_raw <- master$language
    lang_recoded <- .recode_language_by_study(
      out = as.character(lang_raw),
      raw = lang_raw,
      qes_code = master$qes_code
    )
    master$language <- .standardize_language_labels(lang_recoded)
  }

  if ("citizenship" %in% names(master)) {
    master$citizenship <- .standardize_citizenship_labels(master$citizenship)
  }

  if ("born_canada" %in% names(master)) {
    master$born_canada <- .standardize_born_canada(master$born_canada)
  }

  if ("gender" %in% names(master)) {
    master$gender <- .standardize_gender_labels(master$gender)
  }

  if ("education" %in% names(master)) {
    master$education <- .standardize_education_labels(master$education)
  }

  if ("province_territory" %in% names(master)) {
    master$province_territory <- .standardize_province_territory(master$province_territory)
  }

  if ("political_interest" %in% names(master)) {
    master$political_interest <- .coerce_scale_0_10(master$political_interest, kind = "interest")
  }

  if ("ideology" %in% names(master)) {
    master$ideology <- .coerce_scale_0_10(master$ideology, kind = "ideology")
  }

  master <- .recode_party_fields_by_study(master)

  if ("vote_choice" %in% names(master)) {
    master$vote_choice <- .standardize_party_label(master$vote_choice, domain = "provincial")
  }
  if ("party_best" %in% names(master)) {
    master$party_best <- .standardize_party_label(master$party_best, domain = "provincial")
  }
  if ("party_lean" %in% names(master)) {
    master$party_lean <- .standardize_party_label(master$party_lean, domain = "provincial")
  }
  if ("provincial_pid" %in% names(master)) {
    master$provincial_pid <- .standardize_party_label(master$provincial_pid, domain = "provincial")
  }
  if ("federal_pid" %in% names(master)) {
    master$federal_pid <- .standardize_party_label(master$federal_pid, domain = "federal")
  }

  if ("vote_choice_text" %in% names(master)) {
    master$vote_choice_text <- .clean_vote_choice_text(master$vote_choice_text)
  }

  master <- .fill_study_constants(master)

  master
}

.coerce_master_value <- function(x, target) {
  numeric_targets <- c("year_of_birth", "age", "survey_weight")

  if (target %in% numeric_targets) {
    if (is.factor(x)) {
      x <- as.character(x)
    }

    numeric_x <- suppressWarnings(as.numeric(x))
    missing_numeric <- sum(is.na(numeric_x))
    missing_original <- sum(is.na(x))

    if (missing_numeric <= missing_original) {
      return(numeric_x)
    }
  }

  if (inherits(x, "haven_labelled") || inherits(x, "labelled")) {
    x <- haven::as_factor(x)
  }

  if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXlt")) {
    return(as.character(x))
  }

  if (is.factor(x)) {
    return(as.character(x))
  }

  x
}

# ---- labels of qesR 0.4.4 for the legacy builders -------------------------------
#
# get_qes_master() and get_decon() turn labelled columns into their labels.
# qesR 0.4.4 labelled a few source columns that the original files leave
# unlabelled, from hand-typed maps (.qes_legacy_label_maps below); the
# reader no longer does (labels come from the files, R/read.R), so these
# builders put the 0.4.4 labels back on exactly the columns whose values
# depend on them, and only for codes the file leaves unlabelled. Like the
# frozen sources (R/legacy.R), they are part of the interim builders and go
# with them when the engine renders the legacy columns (0.7.0):
#   * qes2018 `qscol` (education) in both builders, and `qsexe` (gender) in
#     get_decon(), which 0.4.4 returned as factors of those labels;
#   * qes1998 `scol` code 9 ("Refus / pas de reponse", declared missing in the
#     panel codebook) and `age` codes 6 and 9, in both builders. The panel
#     file labels age codes 1-5 only ("18-24" ... "55-64"); its codebook shows
#     code 6 (234 respondents) as an unlabelled value and code 9 (1) as
#     missing, and the CREATEC codebook of the same deposit (file 332050), for
#     the same question, labels 6 "65 ANS ET PLUS" and 9 "REFUS/PAS DE
#     REPONSE", as 0.4.4 had them.
# Every labelled column also has its label text trimmed ("  Refus" in the
# qes2007 SPSS labels) and blank labels dropped (qes2012 code 96), since 0.4.4
# showed those codes as numbers.
.qes_legacy_label_vars <- list(
  master = list(qes2018 = "qscol", qes1998 = c("scol", "age")),
  decon = list(qes2018 = c("qsexe", "qscol"), qes1998 = "age")
)

# The 0.4.4 label maps of those columns, frozen as qesR 0.4.4 typed them
# (without accents) in its hand-made codebook overrides, which slice S3
# replaced by the dictionary.
.qes_legacy_label_maps <- list(
  qes2018 = list(
    qsexe = c("1" = "Masculin", "2" = "Feminin"),
    qscol = c(
      "1" = "Aucune scolarite",
      "2" = "Cours primaire (pas fini)",
      "3" = "Cours primaire (complete)",
      "4" = "Secondaire 1",
      "5" = "Secondaire 2",
      "6" = "Secondaire 3",
      "7" = "Secondaire 4",
      "8" = "Secondaire 5 (DES)",
      "9" = "Secondaire 5 (DEP)",
      "10" = "CEGEP (pas fini)",
      "11" = "CEGEP (avec DEC)",
      "12" = "CEGEP (programme technique)",
      "13" = "Universite non completee",
      "14" = "Baccalaureat",
      "15" = "Maitrise ou doctorat",
      "99" = "Je prefere ne pas repondre"
    )
  ),
  qes1998 = list(
    scol = c(
      "1" = "1-9 ans",
      "2" = "10-15 ans",
      "3" = "Universite+",
      "9" = "Refus / pas de reponse"
    )
  )
)

.qes_legacy_label_fills <- function(srvy, consumer = c("master", "decon")) {
  consumer <- match.arg(consumer)
  vars <- .qes_legacy_label_vars[[consumer]][[srvy]]
  if (length(vars) == 0L) {
    return(list())
  }
  maps <- .qes_legacy_label_maps[[srvy]] %||% list()
  if (identical(srvy, "qes1998")) {
    maps$age <- c("6" = "65+", "9" = "Refus/pas de reponse")
  }
  maps[intersect(vars, names(maps))]
}

.qes_legacy_source_labels <- function(data, srvy, consumer = c("master", "decon")) {
  consumer <- match.arg(consumer)
  for (j in seq_along(data)) {
    labels <- attr(data[[j]], "labels", exact = TRUE)
    if (length(labels) == 0L) {
      next
    }
    names(labels) <- trimws(names(labels))
    labels <- labels[!is.na(names(labels)) & nzchar(names(labels))]
    attr(data[[j]], "labels") <- if (length(labels) > 0L) labels else NULL
  }

  fills <- .qes_legacy_label_fills(srvy, consumer)
  for (v in intersect(names(fills), names(data))) {
    x <- data[[v]]
    values <- .qes_plain(x)
    if (!is.numeric(values)) {
      next
    }
    existing <- attr(x, "labels", exact = TRUE)
    fill <- fills[[v]]
    codes <- as.numeric(names(fill))
    add <- !(codes %in% as.numeric(unclass(existing)))
    labels <- c(
      if (length(existing) > 0L) stats::setNames(as.numeric(unclass(existing)), names(existing)),
      stats::setNames(codes[add], unname(fill[add]))
    )
    storage.mode(labels) <- storage.mode(values)
    data[[v]] <- haven::labelled(values, labels = labels, label = attr(x, "label", exact = TRUE))
  }
  data
}

# The 30 documented columns of the legacy master, in the order and with the
# types of qesR 0.4.4; the columns appended since follow them.
.qes_master_types <- c(
  qes_code = "character", qes_year = "character", qes_name_en = "character",
  respondent_id = "character", interview_start = "character",
  interview_end = "character", interview_recorded = "character",
  language = "character", citizenship = "character", year_of_birth = "numeric",
  age = "numeric", age_group = "character", gender = "character",
  province_territory = "character", education = "character",
  income = "character", religion = "character", born_canada = "character",
  political_interest = "numeric", ideology = "numeric", turnout = "numeric",
  vote_choice = "character", vote_choice_text = "character",
  party_best = "character", party_lean = "character",
  sovereignty_support = "numeric", sovereignty = "numeric",
  federal_pid = "character", provincial_pid = "character",
  survey_weight = "numeric"
)
# Appended in qesR 0.5.0; appended columns are never removed.
.qes_master_appended <- c(vote_choice_timing = "character", sovereignty_item = "character")

.qes_master_cast <- function(x, type) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  switch(type,
    character = as.character(x),
    numeric = if (is.numeric(x)) as.numeric(x) else suppressWarnings(as.numeric(as.character(x)))
  )
}

# One study of the legacy master: the frozen v0.4.4 source of every column
# (inst/extdata/legacy/sources.csv), converted as 0.4.4 converted it. No
# source is looked up by name. Returns list(data, source_map, masks), where
# `masks` marks the cells to blank (see .qes_legacy_blank_masks()).
.build_qes_master_study <- function(data, srvy, year = NA_character_, name_en = NA_character_) {
  sources <- .qes_legacy_sources("master", srvy)
  n <- nrow(data)

  out <- data.frame(
    qes_code = rep(srvy, n),
    qes_year = rep(year, n),
    qes_name_en = rep(name_en, n),
    stringsAsFactors = FALSE
  )
  for (target in names(sources)) {
    source_col <- sources[[target]]
    if (identical(source_col, "(synthetic_rowid)")) {
      out[[target]] <- sprintf("%s_%s", srvy, seq_len(n))
    } else if (is.na(source_col) || !(source_col %in% names(data))) {
      # a pinned file always has its frozen sources (its md5 fixes its
      # columns); only synthetic test data can lack one
      out[[target]] <- rep(NA, n)
    } else {
      out[[target]] <- .coerce_master_value(data[[source_col]], target = target)
    }
  }

  if (identical(srvy, "qes_crop_2007_2010") && ("projet" %in% names(data))) {
    derived_year <- .derive_crop_collection_year(data$projet)
    use_derived <- !is.na(derived_year) & nzchar(derived_year)
    out$qes_year[use_derived] <- derived_year[use_derived]
  }

  source_map <- data.frame(
    qes_code = srvy,
    qes_year = year,
    qes_name_en = name_en,
    harmonized_variable = names(sources),
    source_variable = unname(sources),
    stringsAsFactors = FALSE
  )

  list(
    data = out,
    source_map = source_map,
    masks = .qes_legacy_blank_masks(data, srvy, "master", sources),
    sources = sources
  )
}

# The finished master rows of one study: the 0.4.4 recodes run on this
# study alone (so a study's rows never depend on which other studies are
# loaded), then the verified-invalid cells blanked, the documented types set
# and the per-study constants appended. Returns list(data, counts), where
# `counts` is the number of values each mask set to NA.
#
# qesR 0.4.4 ran its recodes on the stacked columns of all 11 studies, which
# rbind() had made text wherever one study's source was text: every column
# but year_of_birth, age and survey_weight. The recodes treat text and
# numbers differently (a numeric gender code is left as is, the text "1" is
# a man), so each study's columns are made text first, as in that full
# build: every study then gets the values the full 0.4.4 build gave it,
# whichever studies are loaded with it.
.qes_master_numeric_sources <- c("year_of_birth", "age", "survey_weight")

.finish_qes_master_study <- function(built) {
  out <- built$data
  for (col in setdiff(names(out), .qes_master_numeric_sources)) {
    if (is.factor(out[[col]]) || !is.character(out[[col]])) {
      out[[col]] <- as.character(out[[col]])
    }
  }
  out <- .postprocess_master_dataset(out)
  counts <- integer(0)
  for (col in names(built$masks)) {
    counts[[col]] <- .qes_legacy_blank_count(out[[col]], built$masks[[col]])
    out[[col]][built$masks[[col]]] <- NA
  }
  for (col in names(.qes_master_types)) {
    out[[col]] <- .qes_master_cast(out[[col]], .qes_master_types[[col]])
  }
  out <- out[names(.qes_master_types)]
  constants <- .qes_legacy_constants(built$source_map$qes_code[1])
  n <- nrow(out)
  out$vote_choice_timing <- rep(constants$vote_choice_timing, n)
  out$sovereignty_item <- rep(constants$sovereignty_item, n)
  list(data = out, counts = counts)
}

# The legacy master builds only the 11 qesR 0.4.4 studies, by default and
# with "all", plus the synthetic `qes_demo` when it is named. The studies
# added to the catalog since (the 1998 CROP and CREATEC surveys) are refused
# until the engine-based master (0.7.0): the qes1998 panel file already
# holds their respondents, so naming them with qes1998 would count the same
# people twice, and the frozen sources cover the 0.4.4 studies only.
.validate_master_surveys <- function(surveys) {
  if (is.null(surveys)) {
    return(.qes_legacy_codes)
  }
  if (is.character(surveys) && length(surveys) == 1L && identical(.qes_canon_code(surveys), "all")) {
    return(.qes_legacy_codes)
  }
  codes <- .qes_resolve_codes(surveys, "surveys", demo = TRUE)
  new_codes <- setdiff(codes, c(.qes_legacy_codes, "qes_demo"))
  if (length(new_codes) > 0L) {
    .qes_abort(
      "input_master_study",
      class = "qesR_error_input",
      args = list(.qes_q(new_codes)),
      data = list(arg = "surveys", value = new_codes)
    )
  }
  codes
}

#' Build the Legacy Stacked Master QES Dataset
#'
#' Reads the Quebec Election Studies of qesR 0.4.4 and stacks them in one
#' data frame with the 0.4.4 columns: one row per respondent of each study,
#' and the same 30 harmonized columns for every study.
#'
#' `get_qes_master()` is the fixed legacy schema of qesR 0.4.4. It is kept
#' stable, with the same arguments, columns and column types, so that code
#' written for 0.4.4 keeps working; new work should use the study files
#' themselves ([get_qes()]) and, from qesR 0.6.0, the harmonization engine.
#' Its values are those of 0.4.4 except where they were wrong: in qesR 0.5.0
#' the master changes by deletion, apart from the reader changes listed
#' (see *What changed in 0.5.0*).
#'
#' `get_qes_master()` returns the data and assigns nothing unless
#' `assign_global = TRUE`: write `master <- get_qes_master()`. The first call
#' in a session that leaves `assign_global` unset prints a one-time note about
#' this change from qesR 0.4.4.
#'
#' @section How it is built:
#' Each study is read with [get_qes()] from its pinned original file. For
#' each column, the master reads the variable qesR 0.4.4 read (the frozen
#' `source_map` attribute: nothing is chosen by name at run time) and
#' converts it as 0.4.4 did. Every row of every file is kept: there is no
#' de-duplication and no removal of empty rows, so each study contributes
#' exactly its number of respondents (`qes2007_panel`: 2,442 rows).
#'
#' @section What changed in 0.5.0:
#' Results from qesR 0.4.4 can be reproduced only by installing that version
#' (`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`).
#' Compared with 0.4.4:
#' * **Rows.** No respondent is dropped. qesR 0.4.4 removed 380
#'   `qes2007_panel` respondents (its `quest` number repeats across the two
#'   subsamples) and one `qes_crop_2007_2010` respondent as "duplicates".
#' * **Columns removed.** The 70 columns 0.4.4 appended after the 30
#'   documented ones, by stacking raw variables that share a name across
#'   studies (for example `vote_federal_2006`, which held satisfaction with
#'   democracy for `qes2012`), are gone; `attr(, "removed_columns")` lists
#'   them. Read those items from each study with [get_qes()].
#' * **Cells blanked.** Values verified to be wrong are `NA`:
#'   `party_best` and `party_lean` everywhere; `vote_choice` and `turnout`
#'   where the source is a vote intention (`qes2022`, `qes_crop_2007_2010`,
#'   `qes1998`) and `qes2007_panel` respondents not reached after the
#'   election; `sovereignty_support` and `sovereignty` where the question is
#'   not the referendum on an independent country; `language` where the
#'   source is the interview language (`qes2014`, `qes2022`); raw codes and
#'   `-99` codes left in `income` and `religion`; `political_interest` of
#'   `qes2018` (raw 1-4 codes) and `qes2012_panel`; `ideology` of `qes2014`
#'   (its 0 and 10 answers were lost); `born_canada` of `qes2018`;
#'   `vote_choice_text` of `qes2018` and `qes2022`; `education` of `qes1998`;
#'   and "don't know" and refusal labels left in `born_canada`, `language`,
#'   `education`, `income` and `religion`. `vote_choice` keeps the 0.4.4
#'   category "Don't know / Refused" until 0.7.0. `attr(, "legacy_na_columns")` lists every blanked column
#'   of every study, with the number of cells and the reason.
#' * **Columns appended.** `vote_choice_timing` (`"post"`: the vote reported
#'   after the election) and `sovereignty_item` (`"sov_indep"`: the
#'   referendum on an independent country) say what `vote_choice` and
#'   `sovereignty_support` hold in each study, one value per study; they
#'   are `NA` in the studies where those columns are blanked.
#' * **Other changes** come from reading the original files (accents
#'   repaired, weights at full precision, `qes2022` interview dates without a
#'   trailing `.000`) and from the catalog's study names. `qes2018`
#'   `turnout` is 0 instead of `NA` for the 336 respondents who said they
#'   did not vote (`q5` codes 1 and 3), as the 0.4.4 coding intended.
#'
#' A message says so once per session (class `qesR_message_values_changed`,
#' and `qesR_message_legacy_columns` for the removed columns).
#'
#' @section En français:
#' `get_qes_master()` est le fichier fusionné hérité de qesR 0.4.4 : mêmes
#' arguments, mêmes 30 colonnes, mêmes types. Chaque colonne lit la variable
#' que qesR 0.4.4 lisait (attribut `source_map`, figé), convertie de la même
#' façon. Aucune ligne n'est retirée (`qes2007_panel` : 2 442 lignes). Les
#' 70 colonnes empilées par nom de variable sont retirées
#' (`attr(, "removed_columns")`) et les valeurs vérifiées comme fausses sont
#' mises à `NA` (`attr(, "legacy_na_columns")` en donne la liste et la
#' raison). `vote_choice_timing` et `sovereignty_item` précisent ce que
#' contiennent `vote_choice` et `sovereignty_support` dans chaque étude.
#' Les résultats de qesR 0.4.4 ne se reproduisent qu'en installant cette
#' version (`remotes::install_github("ThomasGareau/qesR", ref = "v0.4.4")`).
#' La lecture des fichiers originaux change aussi quelques valeurs (accents,
#' pondérations, dates de `qes2022`, `turnout` de `qes2018` : voir NEWS).
#'
#' @param surveys Character vector of qesR survey codes (see [qes_studies()]).
#'   Defaults to the 11 studies of qesR 0.4.4; `"all"` on its own means the
#'   same 11. Studies added to the catalog since (`qes1998_crop`,
#'   `qes1998_createc`) are not in the master yet and raise an error: their
#'   respondents are already in `qes1998`. Read them with [get_qes()]. Codes
#'   are trimmed and case-insensitive. `"qes_demo"` builds the master of the
#'   synthetic demonstration study, offline.
#' @param assign_global If TRUE, also assign the result as `object_name` into
#'   the environment `get_qes_master()` was called from (the global environment
#'   only when called at top level), after `saved_to` is set. Defaults to FALSE.
#' @param object_name Object name used when `assign_global = TRUE`. Defaults to
#'   `"qes_master"`.
#' @param quiet If TRUE, suppress informational output while downloading.
#' @param strict If TRUE, stop when any study fails. If FALSE, return partial results and
#'   record failures in attributes.
#' @param save_path Optional output path for writing the master file: `.rds`
#'   writes an RDS file, any other extension a UTF-8 CSV file. The
#'   provenance of every study read is written next to it, as
#'   `<stem>_provenance.csv`.
#'
#' @return A data frame, returned visibly: the 30 documented columns of
#'   qesR 0.4.4 in their order and type, then `vote_choice_timing` and
#'   `sovereignty_item`. Attributes:
#'   * `source_map`: the source variable of every column of every study
#'     (`qes_code`, `qes_year`, `qes_name_en`, `harmonized_variable`,
#'     `source_variable`, `file_md5`);
#'   * `loaded_surveys`, `failed_surveys`: the studies read, and one line per
#'     failed study, `"<code>: <reason>"`. qesR's own part of the reason is
#'     always in English, whatever the message language; a root cause raised
#'     by R itself (for example a download error) keeps the text R reported.
#'     The full conditions are in the `failures` field of the
#'     `strict = TRUE` error;
#'   * `duplicates_removed` and `empty_rows_removed`: always `0L`;
#'   * `harmonized_variables`: the columns after `qes_code`, `qes_year` and
#'     `qes_name_en`;
#'   * `crossstudy_variables_added` (always empty) and `variable_name_map`
#'     (no rows): kept for code written for 0.4.4;
#'   * `legacy_na_columns`: one row per column and study whose cells are `NA`
#'     by design (`column`, `study`, `reason` `"no_source"` or `"blanked"`,
#'     `n_cells`, `cause`, `basis`); `n_cells` is the whole column for
#'     `"no_source"` and the number of values set to `NA` for `"blanked"`;
#'   * `legacy_column_map`: what each column means (`column`, `target`,
#'     `definition`, `studies_changed`, `flag`, `note`); `flag` is
#'     `"approximate"` for columns that mix instruments;
#'   * `removed_columns`: the names of the 70 columns no longer built;
#'   * `qes_provenance`: the file read for each study (see
#'     [qes_provenance()]);
#'   * `qes_spec`: records that the frozen legacy tables, not a
#'     harmonization spec, built the data;
#'   * `saved_to`: the output path when `save_path` is given.
#' @seealso [get_qes()] for the study files, [qes_provenance()] and
#'   [qes_cite()] to record and cite the files read.
#' @examples
#' # the synthetic demonstration study, offline
#' demo_master <- get_qes_master(surveys = "qes_demo", quiet = TRUE)
#' head(demo_master[, c("qes_code", "gender", "turnout", "vote_choice")])
#' attr(demo_master, "legacy_na_columns")[, c("column", "reason", "cause")]
#' @export
get_qes_master <- function(
  surveys = NULL,
  assign_global = FALSE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
) {
  .get_qes_master_impl(
    surveys = surveys, assign_global = assign_global, object_name = object_name,
    quiet = quiet, strict = strict, save_path = save_path,
    envir = parent.frame(),
    assign_missing = missing(assign_global)
  )
}

.get_qes_master_impl <- function(
  surveys = NULL,
  assign_global = FALSE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL,
  envir = NULL,
  assign_missing = FALSE
) {
  surveys <- .validate_master_surveys(surveys)
  .assert_single_string(object_name, "object_name")
  if (!is.null(save_path)) {
    .assert_single_string(save_path, "save_path")
    out_dir <- dirname(save_path)
    if (!dir.exists(out_dir)) {
      .qes_abort(
        "input_save_dir",
        class = "qesR_error_input",
        args = list(.qes_q(out_dir)),
        data = list(arg = "save_path", value = save_path)
      )
    }
  }
  .qes_legacy_notice("get_qes_master")

  stacked <- list()
  source_maps <- list()
  na_rows <- list()
  provenance <- list()
  failed <- character(0)
  failed_conditions <- list()

  for (srvy in surveys) {
    study <- .qes_legacy_view(.qes_study_row(srvy, demo = TRUE))

    dat <- tryCatch(
      .get_qes_impl(
        srvy = srvy,
        assign_global = FALSE,
        with_codebook = FALSE,
        quiet = quiet
      ),
      error = function(e) e
    )

    if (inherits(dat, "error")) {
      # returned text, so rendered in English whatever the session language
      reason <- gsub("\n", " ", .qes_condition_text(dat, lang = "en"))
      failed <- c(failed, sprintf("%s: %s", srvy, reason))
      failed_conditions[[srvy]] <- dat
      .qes_inform(
        "master_skip",
        class = "qesR_message_download",
        args = list(.qes_q(srvy)),
        data = list(study = srvy, error = dat),
        quiet = quiet
      )
      next
    }

    prov <- attr(dat, "qes_provenance", exact = TRUE)
    dat <- .qes_legacy_source_labels(dat, srvy, consumer = "master")
    built <- .build_qes_master_study(
      data = dat,
      srvy = srvy,
      year = study$year,
      name_en = study$name_en
    )
    finished <- .finish_qes_master_study(built)
    rows <- finished$data
    # every row of the file, and only those (design rule P5)
    if (nrow(rows) != nrow(dat) || (!is.null(prov) && !identical(as.integer(nrow(rows)), as.integer(prov$n_rows[1])))) {
      stop(sprintf("qesR internal error: the master rows of '%s' differ from its file.", srvy), call. = FALSE)
    }

    built$source_map$file_md5 <- if (is.null(prov)) NA_character_ else prov$md5_observed[1]
    stacked[[srvy]] <- rows
    source_maps[[srvy]] <- built$source_map
    na_rows[[srvy]] <- .qes_legacy_na_rows(srvy, "master", built$sources, built$masks, nrow(rows), finished$counts)
    if (!is.null(prov)) {
      provenance[[srvy]] <- prov
    }

    .qes_inform(
      "master_rows_loaded",
      class = "qesR_message_download",
      args = list(srvy, nrow(dat)),
      data = list(study = srvy),
      quiet = quiet
    )
  }

  if (length(stacked) == 0L) {
    .qes_abort(
      "master_none",
      class = "qesR_error_source",
      data = list(
        study = names(failed_conditions),
        file_id = NA_character_,
        failures = failed_conditions
      ),
      parent = if (length(failed_conditions) > 0L) failed_conditions[[1]] else NULL
    )
  }

  if (isTRUE(strict) && length(failed) > 0L) {
    .qes_abort(
      "master_strict",
      class = "qesR_error_source",
      args = list(length(failed), .qes_q(names(failed_conditions))),
      data = list(
        study = names(failed_conditions),
        file_id = NA_character_,
        failures = failed_conditions
      ),
      parent = failed_conditions[[1]]
    )
  }

  master <- do.call(rbind, stacked)
  rownames(master) <- NULL
  source_map <- do.call(rbind, source_maps)
  rownames(source_map) <- NULL
  legacy_na <- do.call(rbind, na_rows)
  rownames(legacy_na) <- NULL
  prov <- if (length(provenance) > 0L) do.call(rbind, provenance) else NULL
  if (!is.null(prov)) {
    rownames(prov) <- NULL
  }

  attr(master, "source_map") <- source_map
  attr(master, "loaded_surveys") <- names(stacked)
  attr(master, "failed_surveys") <- failed
  attr(master, "duplicates_removed") <- 0L
  attr(master, "empty_rows_removed") <- 0L
  attr(master, "harmonized_variables") <- setdiff(names(master), c("qes_code", "qes_year", "qes_name_en"))
  attr(master, "crossstudy_variables_added") <- character(0)
  attr(master, "variable_name_map") <- data.frame(
    legacy_variable = character(0),
    master_variable = character(0),
    label_hint = character(0),
    stringsAsFactors = FALSE
  )
  attr(master, "legacy_na_columns") <- legacy_na
  attr(master, "legacy_column_map") <- .qes_legacy_column_map()
  attr(master, "removed_columns") <- .qes_legacy_table("removed")$column
  attr(master, "qes_provenance") <- prov
  attr(master, "qes_spec") <- .qes_legacy_spec()

  if (!is.null(save_path)) {
    ext <- tolower(tools::file_ext(save_path))
    if (ext == "rds") {
      saveRDS(master, save_path)
    } else {
      .qes_write_csv(master, save_path)
    }
    if (!is.null(prov)) {
      stem <- tools::file_path_sans_ext(save_path)
      .qes_write_csv(as.data.frame(prov), paste0(stem, "_provenance.csv"))
    }
    attr(master, "saved_to") <- save_path
  }

  # Attributes are final before opt-in assignment, so the assigned object is
  # identical to the returned one.
  if (isTRUE(assign_global)) {
    .qes_assign(object_name, master, envir)
  } else if (isTRUE(assign_missing)) {
    .qes_assign_default_notice("get_qes_master", object_name)
  }

  if (!quiet) {
    .qes_inform("master_n_rows", class = "qesR_message_download", args = list(nrow(master)))
    .qes_inform("master_n_loaded", class = "qesR_message_download", args = list(length(stacked)))
    if (length(failed) > 0L) {
      .qes_inform("master_n_skipped", class = "qesR_message_download", args = list(length(failed)))
    }
    if (!is.null(save_path)) {
      .qes_inform("master_saved", class = "qesR_message_download", args = list(save_path))
    }
  }

  master
}
