# The shipped dictionary (slice S3, design.md sections 6.1 and 3.4): lints on
# inst/extdata/dict/ and the demo tree, and the licence rule of OD3 (nothing
# of qes2022 ships but variable names and codes in shard_rules.csv).

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
    c("variables.csv.gz", "values.csv.gz", "shard_rules.csv", "variables.csv.gz", "values.csv.gz")
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
  # design.md section 11 (S3 exit gate): the dictionary is under 1 MB
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

test_that("every CC0 study ships; qes2022 ships no rows and no label text (OD3)", {
  st <- shipped_catalog()$studies
  d <- dict_tables()
  expect_setequal(unique(d$variables$study), st$study[st$metadata_shipped %in% TRUE])
  expect_false("qes2022" %in% d$variables$study)
  expect_false("qes2022" %in% d$values$study)
  expect_identical(unique(dict_tables(TRUE)$variables$study), "qes_demo")
  # shard rules: variable names and codes only
  rules <- getFromNamespace(".qes_shard_rules", "qesR")()
  expect_identical(names(rules), c("study", "variable", "value", "missing_type", "evidence"))
  expect_identical(unique(rules$study), "qes2022")
  expect_true(all(grepl("^(\\*|[A-Za-z][A-Za-z0-9_]*)$", rules$variable)))
  expect_true(all(grepl("^-?[0-9]+$", rules$value)))
  expect_true("*" %in% rules$variable)
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
