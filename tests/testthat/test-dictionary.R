# The shipped dictionary (slice S3, design.md sections 6.1 and 3.4): lints on
# inst/extdata/dict/ and the demo tree. Every study ships, qes2022 included
# (its rows are CC BY-NC 4.0; decision OD3 was lifted on 2026-09-28).

dict_dir <- function(demo = FALSE) {
  if (demo) {
    system.file("extdata", "demo", "dict", package = "qesR", mustWork = TRUE)
  } else {
    system.file("extdata", "dict", package = "qesR", mustWork = TRUE)
  }
}

dict_tables <- function(demo = FALSE) {
  getFromNamespace(".qes_dict_shipped", "qesR")(demo = demo)
}

test_that("the dictionary files are UTF-8, LF, no BOM, and small", {
  files <- c(
    list.files(dict_dir(), full.names = TRUE),
    list.files(dict_dir(TRUE), full.names = TRUE)
  )
  files <- files[!grepl("^\\._", basename(files))]
  expect_setequal(
    basename(files),
    c("variables.csv.gz", "values.csv.gz", "variables.csv.gz", "values.csv.gz")
  )
  for (f in files) {
    con <- if (grepl("\\.gz$", f)) gzfile(f, "rb") else file(f, "rb")
    b <- readBin(con, "raw", 50e6)
    close(con)
    expect_false(any(b == as.raw(0x0d)), info = f)
    expect_false(identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf))), info = f)
    text <- rawToChar(b)
    Encoding(text) <- "UTF-8"
    expect_true(validUTF8(text), info = f)
    cps <- utf8ToInt(text)
    expect_false(any(cps == 0xFFFD), info = f)
    expect_false(any(cps >= 0x80 & cps <= 0x9F), info = f)
  }
  # design.md section 11 (S3 exit gate): the dictionary is under 1 MB,
  # gz-compressed, with qes2022 (its 718 variables add about 70 KB)
  expect_true(all(grepl("\\.csv\\.gz$", files)))
  expect_lt(sum(file.size(files)), 1e6)
})

test_that("the tables match their schemas, keys are unique and values are closed", {
  schemas <- getFromNamespace(".qes_schemas", "qesR")
  enums <- shipped_catalog()$enums
  for (demo in c(FALSE, TRUE)) {
    d <- dict_tables(demo)
    v <- d$variables
    x <- d$values
    expect_identical(names(v), names(schemas$dict_variables))
    expect_identical(names(x), names(schemas$dict_values))
    expect_false(anyDuplicated(paste(v$study, v$variable)) > 0L)
    expect_false(anyDuplicated(paste(x$study, x$variable, x$value)) > 0L)
    expect_true(all(paste(x$study, x$variable) %in% paste(v$study, v$variable)))
    expect_true(all(v$label_source %in% enums$value[enums$enum == "label_source"]))
    expect_true(all(x$label_source %in% enums$value[enums$enum == "label_source"]))
    expect_false(any(v$label_source == "ddi"))
    dict_types <- enums$value[enums$enum == "missing_type" & grepl("dictionary", enums$scope)]
    expect_true(all(x$missing_type[!is.na(x$missing_type)] %in% dict_types))
    expect_true(all(v$measure %in% enums$value[enums$enum == "measure"]))
    expect_true(all(v$type %in% c("numeric", "character", "date", "datetime", "logical")))
    expect_true(all(v$var_timing[!is.na(v$var_timing)] == "single" |
      grepl("^(pre|post|wave:[a-z0-9_]+)$", v$var_timing[!is.na(v$var_timing)])))
    expect_true(all(x$n[!is.na(x$n)] >= 0L))
    # an unlabelled code has no label; a labelled one keeps "" when that is its label
    expect_true(all(is.na(x$label[x$label_source == "none"])))
    expect_true(all(!is.na(x$label[x$label_source != "none"])))
    # a label is never the variable's own name ([A:K4]) and never a copy of
    # the question text ([A:K5])
    expect_false(any(tolower(v$label) == tolower(v$variable), na.rm = TRUE))
    # positions run 1..n within each study
    for (s in unique(v$study)) {
      expect_identical(v$position[v$study == s], seq_len(sum(v$study == s)), info = s)
    }
  }
})

test_that("every study ships, qes2022 included (OD3 lifted)", {
  st <- shipped_catalog()$studies
  d <- dict_tables()
  expect_true(all(st$metadata_shipped))
  expect_setequal(unique(d$variables$study), st$study)
  expect_identical(unique(dict_tables(TRUE)$variables$study), "qes_demo")
})

test_that("qes2022 ships its labels, its codebook wording in both languages and its missing codes", {
  d <- dict_tables()
  v <- d$variables[d$variables$study == "qes2022", ]
  x <- d$values[d$values$study == "qes2022", ]
  expect_identical(nrow(v), 718L)
  expect_identical(sum(!is.na(v$label)), 716L)
  # the question text comes from the bilingual codebook (file 7449514), never
  # from the variable labels, which Stata cuts at 80 characters
  q <- v[!is.na(v$question_en) | !is.na(v$question_fr), ]
  expect_gt(nrow(q), 400L)
  expect_gt(sum(!is.na(q$question_fr)), 400L)
  expect_true(all(q$question_source == "codebook"))
  expect_true(all(startsWith(q$doc_ref, "7449514:")))
  expect_false(any(q$question_truncated))
  turnout <- v[v$variable == "cps_turnout", ]
  expect_identical(turnout$question_en, "The Quebec election is scheduled for October 3, 2022. In this election, are you\u2026")
  expect_identical(turnout$question_fr, "L'\u00e9lection au Qu\u00e9bec est pr\u00e9vue pour le 3 octobre 2022. Dans le cadre de cette \u00e9lection, \u00eates-vous...")
  # a grid item: the stem, then the item in brackets
  expect_match(v$question_en[v$variable == "cps_leadertherm_1"], "\\[Dominique Anglade\\]$")
  # value labels of the file (English), with the codebook's French
  dk <- x[x$variable == "cps_votechoice1" & x$value == "10", ]
  expect_identical(dk$label, "Don't know")
  expect_identical(dk$label_fr, "Je ne sais pas")
  expect_identical(dk$missing_type, "dk")
  expect_gt(sum(!is.na(x$label_fr)), 1000L)
  # -99 is item nonresponse (the codebook, p. 7), except in the 102
  # select-all items, where it is an option not ticked
  expect_identical(sum(x$missing_type %in% "not_selected"), 102L)
  expect_true(all(x$missing_type[x$value == "-99"] %in% c("no_answer", "not_selected", "dk", "refused", "dk_refused")))
  expect_identical(x$missing_type[x$variable == "cps_lang_2" & x$value == "-99"], "not_selected")
  expect_identical(x$missing_type[x$variable == "cps_yob" & x$value == "-99"], "no_answer")
})

test_that("qes2022: a variable with French labels has one for every labelled answer", {
  # the codebook's options wrap over lines ("... qui n'est pas adjacente à
  # une" / "grande ville (3)"), lose their ")" ("Peu (3") or carry a
  # footnote marker ("(1)1"); none of these may drop a French label
  d <- dict_tables()
  x <- d$values[d$values$study == "qes2022", ]
  with_fr <- unique(x$variable[!is.na(x$label_fr)])
  # labelled answers the codebook gives no French for (none today)
  exceptions <- character(0)
  gap <- x[x$variable %in% with_fr & !is.na(x$label) & is.na(x$missing_type) & is.na(x$label_fr), ]
  gap <- gap[!paste(gap$variable, gap$value) %in% exceptions, ]
  expect_identical(nrow(gap), 0L, info = paste(gap$variable, gap$value, collapse = ", "))
  fr <- function(v, code) x$label_fr[x$variable == v & x$value == code]
  expect_identical(fr("pes_confidence_1", "2"), "Assez")
  expect_identical(fr("pes_confidence_1", "3"), "Peu")
  expect_identical(fr("pes_rural", "3"), "Dans une ville de taille moyenne (15k-50k personnes) qui n'est pas adjacente à une grande ville")
  expect_identical(fr("pes_q8", "1"), "Parti libéral du Québec")
  expect_identical(fr("pes_maileasy", "5"), "Je ne suis pas certain(e)")
})

test_that("qes2022: typed-text boxes are worded like grid items, and a misspelt name is read", {
  d <- dict_tables()
  v <- d$variables[d$variables$study == "qes2022", ]
  q <- function(var, col = "question_en") v[[col]][v$variable == var]
  # the question, then the option that opens the box, as in the spec's crosswalk
  expect_identical(q("cps_votechoice1_8_TEXT"), "Which party do you think you will vote for? [Another party (please specify)]")
  expect_identical(q("cps_votechoice1_8_TEXT", "question_fr"), "Pour quel parti prévoyez-vous voter? [Autre parti (veuillez spécifier)]")
  xw <- utils::read.csv(system.file("extdata", "harmonize", "crosswalk.csv", package = "qesR"),
                        colClasses = "character", encoding = "UTF-8")
  row <- xw[xw$study == "qes2022" & xw$source_var == "cps_votechoice1_8_TEXT", ]
  expect_identical(row$wording_en, q("cps_votechoice1_8_TEXT"))
  expect_identical(row$wording_fr, q("cps_votechoice1_8_TEXT", "question_fr"))
  text_vars <- v$variable[grepl("_TEXT$", v$variable)]
  expect_true(all(!is.na(v$question_en[v$variable %in% text_vars])))
  # the codebook spells it "cps__Duration_in_seconds_"
  expect_match(q("cps_Duration__in_seconds_"), "^How long the respondent spent in the Campaign Period Survey, in seconds")
})

test_that("the values table lists codes, never the values of a continuous column", {
  # a code of a numeric variable is a whole number written as "%.0f" (the
  # same text on every platform); the values of a weight, an id or any
  # column holding a fraction are not listed (0.7.0 listed the weights of
  # qes2007_panel as 15-digit fractions, whose text differs across
  # platforms, which failed the live check on Linux)
  for (demo in c(FALSE, TRUE)) {
    d <- dict_tables(demo)
    v <- d$variables
    x <- d$values
    type <- v$type[match(paste(x$study, x$variable), paste(v$study, v$variable))]
    measure <- v$measure[match(paste(x$study, x$variable), paste(v$study, v$variable))]
    num <- x[type == "numeric", ]
    expect_true(all(grepl("^-?[0-9]+$", num$value)), info = paste(utils::head(num$value[!grepl("^-?[0-9]+$", num$value)]), collapse = " "))
    expect_false(any(measure %in% c("weight", "id") & x$label_source == "none"))
  }
  x <- dict_tables()$values
  expect_false(any(x$study == "qes2007_panel" & x$variable %in% c("pondam1", "pond", "prop_bv", "s_res_ML")))
})

test_that("each study has as many variables as its pinned file has columns", {
  d <- dict_tables()
  files <- shipped_catalog()$files
  st <- shipped_catalog()$studies
  for (s in unique(d$variables$study)) {
    pinned <- files[files$study == s & files$file_id == st$data_file_id[st$study == s], ]
    expect_identical(sum(d$variables$study == s), pinned$n_cols, info = s)
  }
})

test_that("reviewed rows have question text, with a document of their study", {
  d <- dict_tables()
  v <- d$variables
  rev <- v[v$reviewed, ]
  expect_gt(nrow(rev), 40L)
  expect_true(all(!is.na(rev$question_en) | !is.na(rev$question_fr)))
  files <- shipped_catalog()$files
  q <- v[!is.na(v$doc_ref), ]
  ids <- lapply(strsplit(q$doc_ref, ";", fixed = TRUE), function(r) sub(":.*$", "", r))
  for (i in seq_len(nrow(q))) {
    expect_true(all(ids[[i]] %in% files$file_id[files$study == q$study[i]]), info = paste(q$study[i], q$variable[i]))
  }
  expect_true(all(q$question_source %in% c("questionnaire", "codebook", "file")))
})

test_that("verified rows of design.md section 6.1 are in the dictionary", {
  x <- dict_tables()$values
  row <- function(s, v, val) x[x$study == s & x$variable == v & x$value == val, ]
  r <- row("qes2012", "q52", "1")
  expect_identical(r$label, "Yes")
  expect_identical(r$label_source, "label_donor")
  expect_identical(r$n, 567L)
  r <- row("qes2012", "q52", "8")
  expect_identical(r$missing_type, "dk")
  expect_identical(r$n, 156L)
  r <- row("qes2018", "q26", "8")
  expect_identical(r$label_source, "supplement")
  expect_identical(r$label_lang, "fr")
  expect_identical(r$missing_type, "dk")
  expect_identical(r$n, 463L)
  r <- row("qes2018", "q69", "2")
  expect_identical(r$label, "Ailleurs au Canada")
  expect_identical(r$n, 138L)
  r <- row("qes2007_panel", "interet", "9")
  expect_identical(r$label, "* NSP/Refus")
  expect_identical(r$missing_type, "dk_refused")
  expect_identical(r$n, 3L)
  # a genuine "" label survives (keep_empty)
  expect_identical(row("qes2012", "q64", "96")$label, "")
})

test_that("derivations and label flags of design.md section 6.1 are recorded", {
  d <- dict_tables()
  v <- d$variables
  x <- d$values
  var <- function(s, name) v[v$study == s & v$variable == name, ]
  expect_identical(var("qes2018_panel", "independance")$derived_from, "rts_q7")
  expect_identical(var("qes2012", "age")$derived_from, "agex")
  # every derived_from names variables of the same study
  for (i in which(!is.na(v$derived_from))) {
    from <- strsplit(v$derived_from[i], ";", fixed = TRUE)[[1]]
    expect_true(all(from %in% v$variable[v$study == v$study[i]]), info = paste(v$study[i], v$variable[i]))
  }
  i2 <- x[x$study == "qes1998" & x$variable == "intvote2", ]
  expect_true(nrow(i2) > 0L)
  expect_true(all(i2$label_flag == "shifted_in_source"))
  expect_true(all(x$label_flag[!is.na(x$label_flag)] %in% c("declared_substantive", "shifted_in_source")))
  # a declared missing code that is an answer has no missing type
  sub <- x[x$label_flag %in% "declared_substantive", ]
  expect_true(nrow(sub) > 0L)
  expect_true(all(is.na(sub$missing_type)))
  expect_identical(x$label_flag[x$study == "qes2007_panel" & x$variable == "vote" & x$value == "6"], "declared_substantive")
  # a declared code with a known meaning is typed by it, not user_na
  mt <- function(s, name, val) x$missing_type[x$study == s & x$variable == name & x$value == val]
  expect_identical(mt("qes1998", "q2post", "8"), "not_voted")
  expect_identical(mt("qes2012_panel", "voteprov", "8"), "spoiled")
  expect_identical(mt("qes2007_panel", "vote", "10"), "not_in_wave")
  expect_identical(mt("qes2018_panel", "rts_q2", "6"), "spoiled")
})
