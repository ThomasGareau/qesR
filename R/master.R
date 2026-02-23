.master_harmonization_lookup <- function() {
  list(
    respondent_id = c("ResponseId", "responseid", "respid", "quetr", "quest2_crop", "quest", "QUEST", "questpost", "questpst", "questbv", "questionnaire7701", "seq"),
    interview_start = c("cps_StartDate", "sdat", "SDAT", "startdate", "StartDate"),
    interview_end = c("cps_EndDate", "enddate", "EndDate"),
    interview_recorded = c("cps_RecordedDate", "recordeddate", "RecordedDate", "date"),
    language = c("cps_UserLanguage", "LANG", "lang", "qlang", "QLANG", "qlangue", "s1", "langfix", "langu", "lmat", "lmat2", "lusag", "lusage", "lang_pre", "s_lang_pst"),
    citizenship = c("cps_citizen", "cps_citizenship", "citizenship"),
    year_of_birth = c("cps_yob", "QAGE", "qage", "agex", "ageyear_1", "yob", "q75", "Q75"),
    age = c("cps_age_in_years", "SMAGE", "smage", "age", "agecalc", "agenum", "QAGE", "qage", "q0age"),
    age_group = c("CLAGE", "clage", "age_3gr", "agegrp", "age3", "age45", "age"),
    gender = c("cps_genderid", "QSEXE", "qsexe", "sexfix", "sexe", "sexe_post", "gender", "q76", "SEXE"),
    province_territory = c("cps_province", "q0qc", "Q0QC", "QREGION", "qregion", "regio", "reg", "reg_3gr", "region"),
    education = c("cps_edu", "QSCOL", "qscol", "q77", "d3", "scol", "scolU", "scolu", "education", "educ", "edu"),
    income = c("cps_income", "q78", "Q78", "q100", "Q100", "q57", "Q57", "q61", "Q61", "d5", "D5", "revenu", "income"),
    religion = c("cps_religion", "q103", "Q103", "q63", "Q63", "religion"),
    born_canada = c("cps_borncda", "q105", "Q105", "born_canada"),
    political_interest = c("cps_interest_1", "cps_intelection_1", "qinterest", "interest", "interet", "interetrec", "interet2", "interetbin", "interetrev", "interetrev2", "q14", "Q14", "q15", "Q15", "q27", "Q27", "q28", "Q28"),
    ideology = c("cps_ideoself_1", "lr", "ideology", "q71", "Q71", "q32", "Q32", "q36", "Q36", "rts_q8", "q70a"),
    turnout = c("cps_turnout", "Q2", "q21", "q11", "q5", "rts_q1", "q1post", "allervot", "allervo2", "avote", "participation", "votebin", "voteoui", "turnout", "q1"),
    vote_choice = c("cps_votechoice1", "Q3", "q25", "q12a", "q12", "q6", "rts_q2", "voteprov", "intvoteprov", "intvote", "intvote2", "vote94", "vote", "qes_votechoice", "qvote", "q2", "vote_choice", "votechoice"),
    vote_choice_text = c("cps_votechoice1_8_TEXT", "q2_96_other", "votechoice_text"),
    party_best = c("cps_partybest", "partybest", "qpartybest", "Q8"),
    party_lean = c("cps_votelean", "Q5", "q13", "q12b", "q5a", "rv1b", "intvote", "intvoteprov", "votelean", "partylean"),
    sovereignty_support = c("cps_qc_referendum", "cps_qc_independent", "independance", "q26", "q19", "Q19", "q52", "Q20", "souv_rec", "voteref", "intvoteref", "intref"),
    sovereignty = c("cps_qc_referendum", "cps_qc_independent", "independance", "q26", "q19", "Q19", "q52", "Q20", "souv_rec", "voteref", "intvoteref", "intref"),
    federal_pid = c("cps_fedpid", "fed_pid", "fpid"),
    provincial_pid = c("cps_provpid", "prov_pid", "ppid"),
    survey_weight = c(
      "cps_weight_general",
      "cps_weight_general_trimmed",
      "pes_weight_general",
      "pes_weight_general_trimmed",
      "cps_wts",
      "ponderc",
      "poids",
      "POND",
      "pond",
      "pondx",
      "xpond",
      "XPOND",
      "pond_post",
      "pond_postam1",
      "ponder2",
      "ponder3",
      "pondvote",
      "weight",
      "weights",
      "wgt",
      "pdspart",
      "pdspart2"
    )
  )
}

.master_study_overrides <- function() {
  list(
    qes2022 = c(
      sovereignty_support = "cps_qc_referendum",
      sovereignty = "cps_qc_referendum",
      survey_weight = "cps_weight_general"
    ),
    qes2018 = c(
      language = "qlangue",
      turnout = "q5",
      vote_choice = "q6",
      party_lean = "q5a",
      political_interest = "q27",
      ideology = "q36",
      income = "q61",
      religion = "q67",
      born_canada = "q69",
      sovereignty_support = "q26",
      sovereignty = "q26"
    ),
    qes2018_panel = c(
      language = "s1",
      gender = "sexfix",
      province_territory = "region",
      education = "d3",
      turnout = "vote",
      vote_choice = "vote",
      party_lean = "rv1b",
      ideology = "rts_q8",
      income = "d5",
      sovereignty_support = "independance",
      sovereignty = "independance",
      survey_weight = "weight"
    ),
    qes2014 = c(
      turnout = "Q2",
      vote_choice = "Q3",
      party_lean = "Q5",
      political_interest = "Q28",
      ideology = "Q32",
      income = "Q57",
      religion = "Q63",
      born_canada = "Q65",
      sovereignty_support = "Q19",
      sovereignty = "Q19",
      survey_weight = "POND"
    ),
    qes2012 = c(
      gender = "sexe",
      turnout = "q21",
      vote_choice = "q25",
      political_interest = "q67",
      ideology = "q71",
      income = NA_character_,
      religion = "q103",
      born_canada = "q105",
      sovereignty_support = "q52",
      sovereignty = "q52"
    ),
    qes2012_panel = c(
      turnout = "participation",
      vote_choice = "voteprov",
      party_lean = "intvoteprov",
      political_interest = "interetrec",
      sovereignty_support = "souv_rec",
      sovereignty = "souv_rec"
    ),
    qes_crop_2007_2010 = c(
      age = NA_character_,
      age_group = "QAGE",
      gender = "SEXE",
      income = "revenu",
      sovereignty_support = "intvoteref",
      sovereignty = "intvoteref",
      survey_weight = "XPOND"
    ),
    qes2008 = c(
      gender = "q76",
      education = "q77",
      turnout = "q11",
      vote_choice = "q12a",
      party_lean = "q12b",
      political_interest = "q14",
      income = "q78",
      ideology = NA_character_,
      religion = NA_character_,
      born_canada = NA_character_,
      sovereignty_support = "q19",
      sovereignty = "q19"
    ),
    qes2007 = c(
      gender = "q76",
      education = "q77",
      turnout = "q11",
      vote_choice = "q12",
      party_lean = "q13",
      political_interest = "q15",
      income = "q78",
      ideology = NA_character_,
      religion = NA_character_,
      born_canada = NA_character_,
      sovereignty_support = "q19",
      sovereignty = "q19"
    ),
    qes2007_panel = c(
      turnout = "avote",
      vote_choice = "vote",
      party_lean = "intvote",
      political_interest = "interet",
      sovereignty_support = "intref",
      sovereignty = "intref"
    ),
    qes1998 = c(
      turnout = "q1post",
      vote_choice = "intvote",
      party_lean = NA_character_,
      sovereignty_support = "voteref",
      sovereignty = "voteref",
      survey_weight = "ponderc"
    )
  )
}

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

.resolve_master_source_column <- function(data, srvy, target, candidates) {
  overrides <- .master_study_overrides()
  study_map <- overrides[[srvy]]

  if (!is.null(study_map) && target %in% names(study_map)) {
    preferred <- unname(study_map[[target]])
    if (length(preferred) == 1L && is.na(preferred)) {
      return(NA_character_)
    }
    if (!is.na(preferred)) {
      preferred_hit <- .pick_first_column(data, preferred)
      if (!is.na(preferred_hit)) {
        return(preferred_hit)
      }
    }
  }

  .pick_first_column(data, candidates)
}

.normalize_master_text <- function(x) {
  out <- as.character(x)
  out <- trimws(out)
  out <- iconv(out, from = "", to = "ASCII//TRANSLIT")
  out <- tolower(out)
  out <- gsub("['`’]", "", out, perl = TRUE)
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
    high_hit <- grepl("beaucoup|very interested|a lot|tres interess|tr[eè]s interess", norm, perl = TRUE) &
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

.remove_master_study_duplicates <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L || !all(c("qes_code", "respondent_id") %in% names(master))) {
    return(list(data = master, removed = 0L))
  }

  ids <- trimws(as.character(master$respondent_id))
  codes <- as.character(master$qes_code)
  valid_id <- !is.na(ids) & nzchar(ids)
  valid_code <- !is.na(codes) & nzchar(codes)
  valid <- valid_id & valid_code

  key <- paste0(codes, "||", tolower(ids))
  keep <- !valid | !duplicated(key)

  list(
    data = master[keep, , drop = FALSE],
    removed = as.integer(sum(!keep))
  )
}

.remove_empty_master_rows <- function(master) {
  if (!is.data.frame(master) || nrow(master) == 0L) {
    return(list(data = master, removed = 0L))
  }

  harm_cols <- intersect(names(.master_harmonization_lookup()), names(master))
  harm_cols <- setdiff(harm_cols, "respondent_id")
  if (length(harm_cols) == 0L) {
    return(list(data = master, removed = 0L))
  }

  missing_mat <- sapply(harm_cols, function(col) .master_missing_vector(master[[col]]))
  if (is.null(dim(missing_mat))) {
    missing_mat <- matrix(missing_mat, ncol = 1L)
  }

  all_missing <- rowSums(missing_mat) == ncol(missing_mat)
  keep <- !all_missing

  list(
    data = master[keep, , drop = FALSE],
    removed = as.integer(sum(all_missing))
  )
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

.build_qes_master_study <- function(data, srvy, year = NA_character_, name_en = NA_character_) {
  lookup <- .master_harmonization_lookup()
  n <- nrow(data)

  out <- data.frame(
    qes_code = rep(srvy, n),
    qes_year = rep(year, n),
    qes_name_en = rep(name_en, n),
    stringsAsFactors = FALSE
  )

  source_map <- data.frame(
    qes_code = srvy,
    qes_year = year,
    qes_name_en = name_en,
    harmonized_variable = names(lookup),
    source_variable = NA_character_,
    stringsAsFactors = FALSE
  )

  for (target in names(lookup)) {
    source_col <- .resolve_master_source_column(data, srvy = srvy, target = target, candidates = lookup[[target]])
    source_map$source_variable[source_map$harmonized_variable == target] <- source_col

    if (is.na(source_col)) {
      out[[target]] <- rep(NA, n)
    } else {
      out[[target]] <- .coerce_master_value(data[[source_col]], target = target)
    }
  }

  # Some legacy files expose questionnaire identifiers (e.g., QUEST = 0) rather
  # than respondent IDs. In those cases, generate stable row IDs to avoid
  # collapsing almost all rows during within-study de-duplication.
  rid <- trimws(as.character(out$respondent_id))
  valid_rid <- !is.na(rid) & nzchar(rid)
  uniq_ratio <- if (sum(valid_rid) > 0L) {
    length(unique(tolower(rid[valid_rid]))) / sum(valid_rid)
  } else {
    0
  }

  if (sum(valid_rid) == 0L || uniq_ratio < 0.5) {
    out$respondent_id <- sprintf("%s_%s", srvy, seq_len(n))
    source_map$source_variable[source_map$harmonized_variable == "respondent_id"] <- "(synthetic_rowid)"
  }

  list(data = out, source_map = source_map)
}

.validate_master_surveys <- function(surveys) {
  if (is.null(surveys)) {
    return(.qes_catalog$qes_survey_code)
  }

  if (!is.character(surveys) || length(surveys) == 0L) {
    stop("`surveys` must be a non-empty character vector.", call. = FALSE)
  }

  surveys <- unique(surveys)
  unknown <- setdiff(surveys, .qes_catalog$qes_survey_code)
  if (length(unknown) > 0L) {
    stop(
      sprintf(
        "Unknown survey code(s): %s. Use get_qescodes() to see valid codes.",
        paste(unknown, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  surveys
}

#' Build a harmonized stacked master QES dataset
#'
#' Downloads multiple Quebec Election Study datasets, harmonizes a shared set of
#' variables, and stacks all rows into one master data frame.
#'
#' @param surveys Character vector of qesR survey codes. Defaults to all
#'   available studies in [get_qescodes()].
#' @param assign_global If `TRUE`, assign the result to `.GlobalEnv` using
#'   `object_name`.
#' @param object_name Name used when `assign_global = TRUE`. Defaults to
#'   `"qes_master"`.
#' @param quiet If `TRUE`, suppress informational output while downloading.
#' @param strict If `TRUE`, stop when any study fails to download. If `FALSE`,
#'   return partial results and record failures in attributes.
#' @param save_path Optional file path to persist the master dataset. If the
#'   path ends with `.rds`, it is saved with [saveRDS()]. Otherwise, a CSV is
#'   written with [utils::write.csv()].
#'
#' @return A harmonized stacked data frame. Attributes include:
#'   `source_map`, `loaded_surveys`, `failed_surveys`,
#'   `duplicates_removed`, and `empty_rows_removed`.
#' @export
get_qes_master <- function(
  surveys = NULL,
  assign_global = TRUE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
) {
  surveys <- .validate_master_surveys(surveys)
  .assert_single_string(object_name, "object_name")
  if (!is.null(save_path)) {
    .assert_single_string(save_path, "save_path")
  }

  stacked <- list()
  source_maps <- list()
  failed <- character(0)

  for (srvy in surveys) {
    study <- .qes_catalog[.qes_catalog$qes_survey_code == srvy, , drop = FALSE]

    dat <- tryCatch(
      get_qes(
        srvy = srvy,
        assign_global = FALSE,
        with_codebook = FALSE,
        quiet = quiet
      ),
      error = function(e) e
    )

    if (inherits(dat, "error")) {
      failed <- c(failed, sprintf("%s: %s", srvy, conditionMessage(dat)))
      if (!quiet) {
        message(sprintf("Skipping '%s' due to download/read error.", srvy))
      }
      next
    }

    built <- .build_qes_master_study(
      data = dat,
      srvy = srvy,
      year = study$year,
      name_en = study$name_en
    )

    stacked[[srvy]] <- built$data
    source_maps[[srvy]] <- built$source_map

    if (!quiet) {
      message(sprintf("[%s] rows loaded: %s", srvy, nrow(dat)))
    }
  }

  if (length(stacked) == 0L) {
    stop("No studies could be loaded. Check network access and survey availability.", call. = FALSE)
  }

  if (isTRUE(strict) && length(failed) > 0L) {
    stop(
      sprintf("Master build failed for %s study(ies): %s", length(failed), paste(failed, collapse = " | ")),
      call. = FALSE
    )
  }

  master <- do.call(rbind, stacked)
  rownames(master) <- NULL
  master <- .postprocess_master_dataset(master)

  dedup <- .remove_master_study_duplicates(master)
  master <- dedup$data

  no_empty <- .remove_empty_master_rows(master)
  master <- no_empty$data

  source_map <- do.call(rbind, source_maps)
  rownames(source_map) <- NULL

  attr(master, "source_map") <- source_map
  attr(master, "loaded_surveys") <- names(stacked)
  attr(master, "failed_surveys") <- failed
  attr(master, "duplicates_removed") <- dedup$removed
  attr(master, "empty_rows_removed") <- no_empty$removed
  attr(master, "harmonized_variables") <- names(.master_harmonization_lookup())
  attr(master, "saved_to") <- NULL

  if (isTRUE(assign_global)) {
    assign(object_name, master, envir = .GlobalEnv)
  }

  if (!is.null(save_path)) {
    out_dir <- dirname(save_path)
    if (!dir.exists(out_dir)) {
      stop(sprintf("Directory does not exist: '%s'.", out_dir), call. = FALSE)
    }

    ext <- tolower(tools::file_ext(save_path))
    if (ext == "rds") {
      saveRDS(master, save_path)
    } else {
      utils::write.csv(master, save_path, row.names = FALSE, na = "")
    }
    attr(master, "saved_to") <- save_path
  }

  if (!quiet) {
    message(sprintf("Master dataset rows: %s", nrow(master)))
    message(sprintf("Master dataset studies loaded: %s", length(stacked)))
    if (length(failed) > 0L) {
      message(sprintf("Master dataset studies skipped: %s", length(failed)))
    }
    if (dedup$removed > 0L) {
      message(sprintf("Master dataset within-study duplicate respondents removed: %s", dedup$removed))
    }
    if (no_empty$removed > 0L) {
      message(sprintf("Master dataset all-empty rows removed: %s", no_empty$removed))
    }
    if (!is.null(save_path)) {
      message(sprintf("Master dataset saved to: %s", save_path))
    }
  }

  master
}
