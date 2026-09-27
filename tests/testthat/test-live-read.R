# Tier 1 (live): get_qes() on the pinned originals of every study (design.md
# sections 2.4, 4.4 and 8.2; the S2b merge and release gates). Never runs on
# CRAN.
#
# Two ways to run it:
#   * QESR_TEST_DATA_DIR=<dir>: <dir> is a download cache laid out by hand
#     (<dir>/qesR/v1/<host>/<file_id>-<md5>.<ext>, see ?qes_cache_info),
#     holding the md5-verified originals. No request is made.
#   * QESR_LIVE=true: the files are downloaded once, politely, into the
#     session cache.
#
# For every study: the md5 and the rows and columns of the catalog; the
# column names, storage type and is.na() counts of qesR 0.4.4 (the S2b merge
# gate, from fixtures/v044-get-qes-names.csv), with the deviations NEWS lists;
# the values and NA pattern of haven reading the same original (as.numeric()
# identity); unique respondent keys; the known marginals and universe
# identities of design.md section 8.2. Every other data file of a study (a
# twin or the label donor) must not hold whole-number codes as text where
# 0.4.4 had numbers, and get_decon() keeps the 0.4.4 column types.

local_live_originals <- function(.env = parent.frame()) {
  skip_on_cran()
  dir <- Sys.getenv("QESR_TEST_DATA_DIR")
  if (nzchar(dir)) {
    withr::local_options(qesR.cache_dir = dir, qesR.cache = "disk", .local_envir = .env)
    testthat::local_mocked_bindings(
      .qes_transport = function(...) stop("QESR_TEST_DATA_DIR is set: no request expected"),
      .package = "qesR",
      .env = .env
    )
  } else {
    skip_if_not(identical(Sys.getenv("QESR_LIVE"), "true"), "set QESR_LIVE=true or QESR_TEST_DATA_DIR to run live tests")
    skip_if_offline()
  }
  invisible()
}

# haven's own reading of an original, as the reference.
haven_original <- function(file_row) {
  path <- qesR:::.qes_cache_fetch(file_row, qesR:::.qes_study_row(file_row$study)$server, quiet = TRUE)
  if (file_row$format %in% c("sav", "zsav")) {
    as.data.frame(haven::read_sav(path, user_na = TRUE))
  } else {
    as.data.frame(haven::read_dta(path))
  }
}

plain_values <- function(x) {
  attributes(x) <- NULL
  x
}

# The storage of a column as the manifest records it.
storage_type <- function(x) {
  t <- typeof(plain_values(x))
  if (t %in% c("integer", "double")) "numeric" else t
}

# Deviations from 0.4.4 in storage or is.na() that NEWS lists (qes2022): the
# six date columns are date-times (post-election dates of non-respondents are
# NA, not ""), and two _TEXT columns are text as in the file (blanks are "",
# not NA).
v044_deviations <- list(
  qes2022 = c(
    "cps_StartDate", "cps_EndDate", "cps_RecordedDate",
    "pes_StartDate", "pes_EndDate", "pes_RecordedDate",
    "cps_votechoice3_5_TEXT", "pes_maildifficult_11_TEXT"
  )
)

live_data <- function(study, ...) {
  get_qes(study, with_codebook = FALSE, quiet = TRUE, ...)
}

test_that("get_qes() reads every pinned original: names, values and keys (live)", {
  local_live_originals()
  local_qes_notices_shown()
  cat <- shipped_catalog()
  manifest <- v044_get_qes_names()
  for (s in cat$studies$study) {
    dat <- get_qes(s, with_codebook = FALSE, quiet = TRUE)
    row <- cat$files[cat$files$study == s & cat$files$role == "data" & cat$files$is_default, , drop = FALSE]
    prov <- attr(dat, "qes_provenance")
    expect_identical(prov$md5_observed, row$md5, info = s)
    expect_identical(dim(dat), c(row$n_rows, row$n_cols), info = s)
    if (s %in% manifest$study) {
      m <- manifest[manifest$study == s, , drop = FALSE]
      expect_identical(names(dat), as_utf8(m$name), info = s)
      # storage and is.na() counts of 0.4.4, except the listed deviations
      same <- !(names(dat) %in% v044_deviations[[s]])
      types <- vapply(dat, storage_type, character(1))
      n_na <- vapply(dat, function(x) sum(is.na(x)), integer(1))
      expect_identical(unname(types[same]), m$type[same], info = s)
      expect_identical(unname(n_na[same]), as.integer(m$n_na[same]), info = s)
      # the deviations are real (a stale list would hide nothing)
      if (any(!same)) {
        expect_true(all(types[!same] != m$type[!same] | n_na[!same] != m$n_na[!same]), info = s)
      }
    }

    ref <- haven_original(row)
    expect_identical(nrow(ref), nrow(dat), info = s)
    for (j in seq_along(ref)) {
      a <- plain_values(ref[[j]])
      b <- plain_values(dat[[j]])
      if (is.character(a) && is.numeric(b)) {
        # a catalog type fix: whole-number codes stored as text
        a <- trimws(a)
        a[!nzchar(a)] <- NA_character_
        a <- as.numeric(a)
      }
      expect_identical(b, a, info = paste(s, names(dat)[j]))
    }

    ids <- qesR:::.qes_split_list(row$id_vars)
    if (length(ids) > 0L && !identical(ids, ".row")) {
      expect_false(anyDuplicated(dat[ids]) > 0L, info = s)
    }
  }
})

test_that("labels come from the files, with the declared repairs (live)", {
  local_live_originals()
  local_qes_notices_shown()
  q12 <- get_qes("qes2012", with_codebook = FALSE, quiet = TRUE)
  expect_identical(attr(q12$q1, "label", exact = TRUE), "How attached do you feel to Quebec?")
  expect_identical(attr(q12, "qes_provenance")$label_file_id, "425917")
  crop <- get_qes("qes_crop_2007_2010", with_codebook = FALSE, quiet = TRUE)
  expect_true(as_utf8("RESTE DU QU\u00c9BEC") %in% names(attr(crop$REG, "labels")))
  q22 <- get_qes("qes2022", with_codebook = FALSE, quiet = TRUE)
  expect_s3_class(q22$cps_StartDate, "POSIXct")
  p07 <- get_qes("qes2007_panel", with_codebook = FALSE, quiet = TRUE)
  expect_true(all(as_utf8(c("AFFG\u00c9N", "PROPRI\u00c9", "PROPG\u00c9N")) %in% names(p07)))
  expect_gt(sum(vapply(p07, function(x) !is.null(attr(x, "qes_na_values")), logical(1))), 50L)
})

test_that("known marginals and universe identities hold (live, design.md section 8.2)", {
  local_live_originals()
  local_qes_notices_shown()
  counts <- function(x) as.vector(table(plain_values(x)))

  q18 <- live_data("qes2018")
  # "valid" is not "don't know" (98) or "prefer not to answer" (99)
  expect_identical(sum(!(plain_values(q18$q36_1) %in% c(98, 99))), 2490L)
  expect_identical(counts(q18$q69), c(2605L, 138L, 304L, 8L, 17L))
  expect_identical(counts(q18$q27), c(743L, 1422L, 687L, 160L, 40L, 20L))
  expect_identical(counts(q18$q6), c(490L, 392L, 700L, 342L, 31L, 92L, 160L))
  expect_identical(sum(!is.na(q18$q6)), sum(plain_values(q18$q5) == 4, na.rm = TRUE))
  expect_identical(sum(!is.na(q18$q6)), 2207L)
  expect_identical(sum(!is.na(q18$q67)), 1245L)

  q14 <- live_data("qes2014")
  expect_identical(sum(!(plain_values(q14$Q32) %in% c(98, 99))), 1195L)
  expect_identical(counts(q14$Q55), c(480L, 378L, 192L, 113L, 10L, 29L, 181L, 92L, 42L))
  expect_identical(sum(!is.na(q14$Q3)), 1352L)

  q12 <- live_data("qes2012")
  expect_identical(sum(!is.na(q12$q25)), 1369L)

  q22 <- live_data("qes2022")
  expect_identical(counts(q22$pes_turnout), c(1109L, 47L, 47L, 12L, 3L, 2L))
  expect_identical(sum(is.na(q22$pes_turnout)), 301L)
  expect_identical(sum(!is.na(q22$pes_votechoice)), 1109L)
  no_vote <- is.na(q22$cps_votechoice1)
  expect_identical(sum(no_vote), 89L)
  expect_true(all(plain_values(q22$cps_turnout)[no_vote] %in% 3:5))
  expect_identical(sum(!is.na(q22$pes_StartDate)), 1220L)
  expect_identical(!is.na(q22$pes_StartDate), !is.na(q22$pes_weight_general))

  p07 <- live_data("qes2007_panel")
  c07 <- plain_values(p07$resultat) %in% "C"
  co07 <- plain_values(p07$resultat_pst) %in% "CO"
  expect_identical(c(sum(c07), sum(co07), sum(c07 & co07)), c(2050L, 2054L, 1663L))
  expect_identical(nrow(unique(p07[c("nompn", "quest")])), 2442L)

  p18 <- live_data("qes2018_panel")
  expect_identical(nrow(unique(p18[c("method", "id")])), 1250L)
  expect_identical(sum(!is.na(p18$rts_q2)), 731L)
})

test_that("no data file of a study holds whole-number codes as text where 0.4.4 had numbers (live)", {
  local_live_originals()
  local_qes_notices_shown()
  cat <- shipped_catalog()
  manifest <- v044_get_qes_names()
  others <- cat$files[cat$files$role %in% c("data", "label_donor") & !cat$files$is_default, , drop = FALSE]
  expect_gt(nrow(others), 0L)
  for (i in seq_len(nrow(others))) {
    row <- others[i, , drop = FALSE]
    dat <- qesR:::.qes_read(row$study, row$file_id)
    m <- manifest[manifest$study == row$study, , drop = FALSE]
    numeric_in_v044 <- tolower(m$name[m$type == "numeric"])
    for (nm in names(dat)[tolower(names(dat)) %in% numeric_in_v044]) {
      x <- plain_values(dat[[nm]])
      if (is.character(x)) {
        v <- trimws(x[!is.na(x) & nzchar(trimws(x))])
        expect_false(length(v) > 0L && all(grepl("^-?[0-9]+$", v)), info = paste(row$study, row$file_id, nm))
      }
    }
  }
})

test_that("get_decon() keeps the column types of 0.4.4 (live)", {
  local_live_originals()
  local_qes_notices_shown()
  withr::local_options(qesR.quiet_deprecated = TRUE)
  v044 <- utils::read.csv(testthat::test_path("fixtures", "v044-get-decon-classes.csv"), stringsAsFactors = FALSE)
  family <- function(x) {
    if (is.factor(x)) "factor" else if (is.numeric(x)) "numeric" else class(x)[1]
  }
  for (s in unique(v044$study)) {
    d <- suppressMessages(get_decon(s, assign_global = FALSE, quiet = TRUE))
    m <- v044[v044$study == s, , drop = FALSE]
    expected <- ifelse(m$class %in% c("integer", "numeric"), "numeric", m$class)
    expect_identical(names(d), m$column, info = s)
    expect_identical(unname(vapply(d, family, character(1))), expected, info = s)
  }
  # qes2022 age is a number: its mean is the mean age, not of level positions
  d22 <- suppressMessages(get_decon("qes2022", assign_global = FALSE, quiet = TRUE))
  q22 <- live_data("qes2022")
  expect_identical(mean(d22$age), mean(plain_values(q22$cps_age_in_years)))
})

test_that("the shipped dictionary describes the pinned files as the reader reads them (live, S3)", {
  local_live_originals()
  local_qes_notices_shown()
  build <- getFromNamespace(".qes_dict_build", "qesR")
  shipped <- getFromNamespace(".qes_dict_shipped", "qesR")()
  st <- shipped_catalog()$studies
  for (s in st$study[st$metadata_shipped %in% TRUE]) {
    file_row <- qesR:::.qes_default_data_file(s)
    data <- qesR:::.qes_read(s, file_row$file_id)
    fresh <- build(data, qesR:::.qes_study_row(s), file_row)
    v <- shipped$variables[shipped$variables$study == s, ]
    expect_identical(v$variable, fresh$variables$variable, info = s)
    from_file <- v$label_source != "supplement"
    expect_identical(v$label[from_file], fresh$variables$label[from_file], info = s)
    expect_identical(v$na_values, fresh$variables$na_values, info = s)
    x <- shipped$values[shipped$values$study == s & shipped$values$label_source != "supplement", ]
    key <- paste(fresh$values$variable, fresh$values$value)
    i <- match(paste(x$variable, x$value), key)
    expect_false(anyNA(i), info = s)
    expect_identical(x$label, fresh$values$label[i], info = s)
    expect_identical(x$n, fresh$values$n[i], info = s)
  }
})

test_that("the qes2022 shard describes the pinned file, and flags cut labels (live, S3)", {
  local_live_originals()
  local_qes_notices_shown()
  cb <- qes_codebook("qes2022", quiet = TRUE)
  expect_identical(nrow(cb), 718L)
  expect_identical(sum(cb$label_source == "file"), 716L)
  expect_gt(sum(cb$question_truncated, na.rm = TRUE), 360L)
  age <- cb[cb$variable == "cps_age_in_years", ]
  expect_true(age$question_truncated)
  expect_identical(age$doc_ref, "7449514")
  expect_identical(cb$missing_codes[cb$variable == "cps_lang_2"], "-99=not_selected")
  expect_match(cb$missing_codes[cb$variable == "cps_votechoice1"], "10=dk", fixed = TRUE)
  expect_true("cps_qc_referendum" %in% qes_search("referendum", studies = "qes2022")$variable)
})
