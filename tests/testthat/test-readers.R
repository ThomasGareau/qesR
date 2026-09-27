# The reader (slice S2b, design.md sections 4.4, 4.5 and 8.1): the pinned
# original file, md5-verified; SPSS user-missing values; label precedence;
# text and type fixes; the name map; the encoding tripwire; provenance. All
# fixtures are small synthetic files written at test time
# (helper-reader.R); none holds a real respondent.

spss_missing_data <- function() {
  data.frame(
    id = 1:6,
    interet = haven::labelled_spss(
      c(1, 2, 8, 9, NA, 4),
      labels = c("Beaucoup" = 1, "Assez" = 2, "Peu" = 3, "Pas du tout" = 4, "NSP" = 8, "Refus" = 9),
      na_values = c(8, 9),
      label = "Int\u00e9r\u00eat pour la politique"
    ),
    echelle = haven::labelled_spss(c(0, 10, 97, 98, 5, NA), na_range = c(97, 99), label = "Scale"),
    stringsAsFactors = FALSE
  )
}

test_that("SPSS user-missing codes stay as values and their declarations move to attributes", {
  raw <- spss_missing_data()
  local_reader_catalog(list(reader_file("qes_fx", "901", raw)))
  d <- read_fixture("qes_fx")

  expect_identical(class(d), "data.frame")
  expect_false(inherits(d$interet, "haven_labelled_spss"))
  expect_s3_class(d$interet, "haven_labelled")
  # codes kept as values: is.na() is the file's system-missing only
  expect_identical(as.numeric(d$interet), c(1, 2, 8, 9, NA, 4))
  expect_identical(sum(is.na(d$interet)), 1L)
  expect_identical(sum(is.na(d$echelle)), 1L)
  expect_identical(attr(d$interet, "qes_na_values"), c(8, 9))
  expect_identical(attr(d$echelle, "qes_na_range"), c(97, 99))
  expect_null(attr(d$interet, "na_values", exact = TRUE))
  expect_identical(attr(d$interet, "label", exact = TRUE), "Int\u00e9r\u00eat pour la politique")
  expect_identical(names(attr(d$interet, "labels"))[5], "NSP")
})

test_that(".qes_unspss() leaves every other column untouched", {
  x <- data.frame(a = 1:3, b = c("x", "y", "z"), stringsAsFactors = FALSE)
  x$c <- haven::labelled(c(1, 2, 1), labels = c(Oui = 1, Non = 2))
  expect_identical(qesR:::.qes_unspss(x), x)
})

test_that("long labels and empty value labels are kept as written", {
  long <- paste(rep("abcdefghij", 8), collapse = "") # 80 characters
  d <- data.frame(q64 = haven::labelled(c(1, 96), labels = c("Autre" = 1, "Aucun" = 96)))
  attr(d$q64, "labels") <- c("Autre" = 1, " " = 96)
  attr(d$q64, "label") <- long
  local_reader_catalog(list(reader_file("qes_fx", "902", d)))
  out <- read_fixture("qes_fx")
  expect_identical(attr(out$q64, "label", exact = TRUE), long)
  expect_identical(unname(attr(out$q64, "labels")), c(1, 96))
})

test_that("a malformed variable label keeps its first element", {
  d <- data.frame(ininum = c(1, 2), autre = c(3, 4))
  attr(d$ininum, "label") <- c("first", "second", "third")
  attr(d$autre, "label") <- "fine"
  out <- qesR:::.qes_file_labels(d)
  expect_identical(attr(out$data$ininum, "label"), "first")
  expect_identical(unname(out$source), c("file_malformed", "file"))
})

test_that("a label donor twin supplies complete labels; the data is the pinned file's", {
  stata <- data.frame(
    quest = c(10, 11, 12),
    q0qc = haven::labelled(c(1, 2, 1), labels = c("bas-saint-laurent" = 1, "saguenay" = 2)),
    pond = c(0.5, 1, 1.5)
  )
  attr(stata$q0qc, "label") <- "in what region in quebec do you live?"
  attr(stata$pond, "label") <- "pond"
  spss <- data.frame(
    QUEST = c(10, 11, 12),
    Q0QC = haven::labelled(c(1, 2, 1), labels = c("Bas-Saint-Laurent" = 1, "Saguenay" = 2)),
    POND = c(0.5, 1, 1.5)
  )
  attr(spss$Q0QC, "label") <- "In what region in Quebec do you live? It is a long label that Stata would cut at eighty characters"
  local_reader_catalog(list(
    reader_file("qes_fx", "903", stata, format = "dta", unf = "UNF:6:same"),
    reader_file("qes_fx", "904", spss, role = "label_donor", unf = "UNF:6:same")
  ))
  out <- read_fixture("qes_fx")

  # names and values of the pinned (Stata) file
  expect_identical(names(out), c("quest", "q0qc", "pond"))
  expect_identical(as.numeric(out$q0qc), c(1, 2, 1))
  # labels of the donor
  expect_identical(attr(out$q0qc, "label", exact = TRUE), attr(spss$Q0QC, "label"))
  expect_identical(names(attr(out$q0qc, "labels")), c("Bas-Saint-Laurent", "Saguenay"))
  # the donor has no label for pond: the file's own stays
  expect_identical(attr(out$pond, "label", exact = TRUE), "pond")
  src <- attr(out, "qes_label_source")
  expect_identical(unname(src[c("q0qc", "pond")]), c("label_donor", "file"))
  prov <- attr(out, "qes_provenance")
  expect_identical(prov$label_file_id, "904")
  expect_identical(prov$label_source, "label_donor;file")
})

test_that("a donor of another UNF is never used", {
  a <- data.frame(q1 = haven::labelled(c(1, 2), labels = c(oui = 1, non = 2)))
  b <- data.frame(Q1 = haven::labelled(c(1, 2), labels = c(Oui = 1, Non = 2)))
  local_reader_catalog(list(
    reader_file("qes_fx", "905", a, unf = "UNF:6:a"),
    reader_file("qes_fx", "906", b, role = "label_donor", unf = "UNF:6:b")
  ))
  out <- read_fixture("qes_fx")
  expect_identical(names(attr(out$q1, "labels")), c("oui", "non"))
  expect_true(is.na(attr(out, "qes_provenance")$label_file_id))
})

test_that("text fixes repair CP850 letters in labels, and never touch data", {
  d <- data.frame(
    REG = haven::labelled(c(1, 4), labels = stats::setNames(c(1, 4), c("QU\u00c9BEC RMR", "RESTE DU QU\u0090BEC"))),
    Occup = haven::labelled(c(1, 2), labels = stats::setNames(c(1, 2), c("trav. \u2026 temps plein", "autre"))),
    note = c("attendez\u2026", "ok"),
    stringsAsFactors = FALSE
  )
  attr(d$Occup, "label") <- "correspond le mieux \u2026"
  fixes <- catalog_rows(
    file_id = c("907", "907"), from = c("U+0090", "U+2026"), to = c("U+00C9", "U+00E0"),
    evidence = c("test", "test")
  )
  local_reader_catalog(list(reader_file("qes_fx", "907", d)), text_fixes = fixes)
  out <- expect_silent(read_fixture("qes_fx"))
  expect_identical(names(attr(out$REG, "labels"))[2], "RESTE DU QU\u00c9BEC")
  expect_identical(names(attr(out$Occup, "labels"))[1], "trav. \u00e0 temps plein")
  expect_identical(attr(out$Occup, "label", exact = TRUE), "correspond le mieux \u00e0")
  expect_identical(as.vector(out$note), c("attendez\u2026", "ok"))
})

test_that("a replacement or C1 control character left after decoding raises qesR_warning_encoding", {
  d <- data.frame(
    REG = haven::labelled(c(1, 4), labels = stats::setNames(c(1, 4), c("ok", "RESTE DU QU\u0090BEC"))),
    txt = c("L\ufffdvis", "Laval"),
    stringsAsFactors = FALSE
  )
  local_reader_catalog(list(reader_file("qes_fx", "908", d)))
  w <- expect_warning(out <- read_fixture("qes_fx"), class = "qesR_warning_encoding")
  expect_identical(w$study, "qes_fx")
  expect_identical(w$n, 2L)
  expect_setequal(w$variables, c("REG", "txt"))
  # the data is still returned, unchanged
  expect_identical(as.vector(out$txt), c("L\ufffdvis", "Laval"))
})

test_that("whole-number codes stored as text become numbers where the catalog says so", {
  d <- data.frame(
    quest = c("0001", "0002", "0003"),
    q1 = haven::labelled(c("01", "98", " "), labels = c("Oui" = "01", "NSP" = "98"), label = "Question 1"),
    codep = c("H2X 1Y4", "G1R", ""),
    ids = c("126", "292", "178"),
    stringsAsFactors = FALSE
  )
  fixes <- catalog_rows(file_id = "909", variables = "*", to = "numeric", evidence = "test")
  local_reader_catalog(list(reader_file("qes_fx", "909", d)), type_fixes = fixes)
  out <- read_fixture("qes_fx")
  expect_identical(as.vector(out$quest), c(1, 2, 3))
  expect_identical(as.numeric(out$q1), c(1, 98, NA))
  expect_identical(attr(out$q1, "labels"), c("Oui" = 1, "NSP" = 98))
  expect_identical(attr(out$q1, "label", exact = TRUE), "Question 1")
  # not whole numbers: stays text
  expect_identical(as.vector(out$codep), c("H2X 1Y4", "G1R", ""))
  expect_identical(as.vector(out$ids), c(126, 292, 178))
})

test_that("a type fix listing variables converts only those, and refuses a wrong list", {
  d <- data.frame(a = c("1", "2"), b = c("3", "4"), stringsAsFactors = FALSE)
  fixes <- catalog_rows(file_id = "910", variables = "b", to = "numeric", evidence = "test")
  local_reader_catalog(list(reader_file("qes_fx", "910", d)), type_fixes = fixes)
  out <- read_fixture("qes_fx")
  expect_identical(as.vector(out$a), c("1", "2"))
  expect_identical(as.vector(out$b), c(3, 4))

  fixes$variables <- "b;missing_var"
  local_reader_catalog(list(reader_file("qes_fx", "910", d)), type_fixes = fixes)
  expect_error(read_fixture("qes_fx"), "type fix")
})

test_that("the name map restores v0.4.4 names, accented and long", {
  d <- data.frame(x = 1:2, y = 3:4, z = 5:6)
  names(d) <- c("AffG\u00e9n\u00e9rale", "propri\u00e9t\u00e9", "quest")
  nm <- catalog_rows(
    file_id = c("911", "911"),
    source_name = c("AffG\u00e9n\u00e9rale", "propri\u00e9t\u00e9"),
    name = c("AFFG\u00c9N", "PROPRI\u00c9"),
    evidence = c("test", "test")
  )
  local_reader_catalog(list(reader_file("qes_fx", "911", d)), name_map = nm)
  out <- read_fixture("qes_fx")
  expect_identical(names(out), c("AFFG\u00c9N", "PROPRI\u00c9", "quest"))
  expect_identical(attr(out, "qes_provenance")$name_map_applied, 2L)
  expect_identical(names(attr(out, "qes_label_source")), names(out))
})

test_that("Stata dates stay dates and text stays text", {
  d <- data.frame(
    start = as.POSIXct(c("2022-09-01 10:00:00", NA), tz = "UTC"),
    jour = as.Date(c("2022-09-01", "2022-10-03")),
    texte = c("-99", ""),
    n = c(1, -99),
    stringsAsFactors = FALSE
  )
  local_reader_catalog(list(reader_file("qes_fx", "912", d, format = "dta")))
  out <- read_fixture("qes_fx")
  expect_s3_class(out$start, "POSIXct")
  expect_identical(sum(is.na(out$start)), 1L)
  expect_s3_class(out$jour, "Date")
  expect_identical(as.vector(out$texte), c("-99", ""))
  expect_identical(as.numeric(out$n), c(1, -99))
})

test_that("rows are never dropped, even with duplicate ids across subsamples", {
  d <- data.frame(nompn = c("a", "a", "b", "b"), quest = c(1, 2, 1, 2), vide = NA_real_)
  local_reader_catalog(list(reader_file("qes_fx", "913", d)))
  out <- read_fixture("qes_fx")
  expect_identical(nrow(out), 4L)
  expect_identical(as.vector(out$quest), c(1, 2, 1, 2))
})

test_that("a file that does not match its catalog md5 is qesR_error_checksum", {
  d <- data.frame(a = 1:3)
  log <- local_reader_catalog(
    list(reader_file("qes_fx", "914", d)),
    serve = function(id) "not the pinned bytes"
  )
  err <- expect_error(read_fixture("qes_fx"), class = "qesR_error_checksum")
  expect_s3_class(err, "qesR_error_source")
  expect_identical(err$file_id, "914")
})

test_that("a file of the wrong size is qesR_error_rowcount", {
  d <- data.frame(a = 1:3, b = 4:6)
  local_reader_catalog(list(reader_file("qes_fx", "915", d, n_rows = 4L)))
  err <- expect_error(read_fixture("qes_fx"), class = "qesR_error_rowcount")
  expect_s3_class(err, "qesR_error_source")
  expect_identical(err$expected, c(4L, 2L))
  expect_identical(err$actual, c(3L, 2L))
})

test_that("only .sav and .dta originals are read; RDS, CSV and text are refused", {
  path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(data.frame(x = 1), path)
  for (fmt in c("rds", "csv", "tab", "por", "zip")) {
    err <- expect_error(qesR:::.qes_read_file(path, fmt), class = "qesR_error_source")
    expect_identical(err$format, fmt)
  }
})

test_that("`cols` keeps columns and the reader's attributes", {
  local_reader_catalog(list(reader_file("qes_fx", "916", spss_missing_data())))
  out <- read_fixture("qes_fx", cols = c("interet", "id"))
  expect_identical(names(out), c("interet", "id"))
  expect_identical(attr(out, "qes_survey_code"), "qes_fx")
  expect_s3_class(attr(out, "qes_provenance"), "data.frame")
  err <- expect_error(read_fixture("qes_fx", cols = "nope"), class = "qesR_error_unknown_variable")
  expect_identical(err$variables, "nope")
})

test_that("parsed data is kept in memory for the session: a second read makes no request", {
  log <- local_reader_catalog(list(reader_file("qes_fx", "917", spss_missing_data())), memo = TRUE)
  a <- read_fixture("qes_fx")
  expect_length(log$urls, 1L)
  b <- read_fixture("qes_fx")
  expect_length(log$urls, 1L)
  expect_identical(a, b)
})

test_that("provenance records the pinned file, its check and the reader", {
  local_reader_catalog(list(reader_file("qes_fx", "918", spss_missing_data())))
  prov <- attr(read_fixture("qes_fx"), "qes_provenance")
  expect_identical(nrow(prov), 1L)
  expect_true(all(c(
    "study", "doi", "dataset_version", "file_id", "file_name", "format",
    "md5_expected", "md5_observed", "md5_verified", "unf", "n_rows", "n_cols",
    "pinned", "retrieved_via", "retrieved_at", "licence", "label_source",
    "name_map_applied", "reader", "haven_version", "catalog_version", "dict_version"
  ) %in% names(prov)))
  expect_identical(prov$file_id, "918")
  expect_true(prov$md5_verified)
  expect_identical(prov$retrieved_via, "network")
  expect_identical(prov$reader, "haven::read_sav(user_na = TRUE)")
  expect_identical(attr(prov$retrieved_at, "tzone"), "UTC")
})

test_that("the reader's output does not depend on the locale or the message language", {
  d <- spss_missing_data()
  names(d)[2] <- "int\u00e9r\u00eat"
  attr(d[[2]], "labels") <- stats::setNames(c(1, 8), c("Beaucoup", "Ne sait pas \u00e0 cette question"))
  local_reader_catalog(list(reader_file("qes_fx", "919", d)))
  strip <- function(x) {
    attr(x, "qes_provenance")$retrieved_at <- NULL
    x
  }
  default <- strip(read_fixture("qes_fx"))
  withr::local_envvar(LANGUAGE = "fr")
  withr::local_locale(c(LC_COLLATE = "C", LC_CTYPE = "C"))
  expect_identical(strip(read_fixture("qes_fx")), default)
})

# ---- get_qes() on the reader ------------------------------------------------------

test_that("get_qes() returns the reader's data as a base data frame, with an offline codebook", {
  local_qes_notices_shown()
  d <- data.frame(q1 = haven::labelled(c(1, 2), labels = c("Oui" = 1, "Non" = 2), label = "Question du fichier"))
  log <- local_reader_catalog(list(reader_file("qes_fx", "920", d)))
  out <- get_qes("qes_fx", quiet = TRUE)
  expect_identical(class(out), "data.frame")
  expect_identical(attr(out$q1, "label", exact = TRUE), "Question du fichier")
  expect_identical(names(attr(out$q1, "labels")), c("Oui", "Non"))
  expect_identical(attr(out, "qes_survey_code"), "qes_fx")
  expect_s3_class(attr(out, "qes_provenance"), "data.frame")
  expect_null(attr(out, "qes_label_source", exact = TRUE))
  # the codebook takes the data's labels; no metadata request is made
  cb <- attr(out, "qes_codebook")
  expect_identical(cb$label[cb$variable == "q1"], "Question du fichier")
  expect_identical(cb$value_labels[cb$variable == "q1"], "1=Oui | 2=Non")
  expect_false(any(grepl("metadata|ddi", log$urls)))
})

test_that("get_qes('qes_demo') reads the shipped demo file offline, md5-checked", {
  local_qes_notices_shown()
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  withr::local_options(qesR.memo = FALSE)
  demo <- get_qes("qes_demo", quiet = TRUE)
  row <- shipped_catalog(demo = TRUE)$files
  row <- row[row$study == "qes_demo", , drop = FALSE]
  expect_identical(dim(demo), c(row$n_rows, row$n_cols))
  prov <- attr(demo, "qes_provenance")
  expect_identical(prov$retrieved_via, "local_demo")
  expect_identical(prov$md5_observed, row$md5)
  expect_identical(attr(demo, "qes_survey_code"), "qes_demo")
  expect_s3_class(attr(demo, "qes_codebook"), "data.frame")
})

test_that("get_qes() on the offline fake reads the pinned file as an original", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  out <- get_qes("qes2018", with_codebook = FALSE, quiet = TRUE)
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/425914?format=original")
  expect_identical(names(out), names(fake_study_data("qes2018")))
  expect_identical(attr(out, "qes_provenance")$file_id, "425914")
})

test_that("get_qes(file =) naming another study of the deposit reads and assigns that study", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  env <- new.env()
  n <- count_class(
    out <- getFromNamespace(".get_qes_impl", "qesR")(
      "qes1998", file = "CROP", assign_global = TRUE, with_codebook = FALSE, envir = env
    ),
    "qesR_message_download"
  )
  expect_gte(n, 2L) # the banner and the redirect
  expect_identical(attr(out, "qes_survey_code"), "qes1998_crop")
  expect_identical(ls(env), "qes1998_crop")
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/286331?format=original")
})

test_that("twin data files that both carry a type fix both read as numbers", {
  # qes2007: the pinned SPSS file and its Stata twin (the file of 0.4.4)
  # store the same codes as zero-padded text
  d <- data.frame(quest = c("1", "2"), q1 = c("01", " "), codep = c("H2X", "G1R"), stringsAsFactors = FALSE)
  fixes <- catalog_rows(
    file_id = c("931", "932"), variables = c("*", "*"),
    to = c("numeric", "numeric"), evidence = c("test", "test")
  )
  local_reader_catalog(list(
    reader_file("qes_fx", "931", d, unf = "UNF:6:a"),
    reader_file("qes_fx", "932", d, format = "dta", is_default = FALSE, unf = "UNF:6:b")
  ), type_fixes = fixes)
  local_qes_notices_shown()
  pinned <- get_qes("qes_fx", with_codebook = FALSE, quiet = TRUE)
  twin <- get_qes("qes_fx", file = "\\.dta$", with_codebook = FALSE, quiet = TRUE)
  expect_identical(attr(twin, "qes_provenance")$file_id, "932")
  for (out in list(pinned, twin)) {
    expect_identical(as.vector(out$q1), c(1, NA))
    expect_identical(as.vector(out$quest), c(1, 2))
    expect_identical(as.vector(out$codep), c("H2X", "G1R"))
  }
})

test_that("a label donor can be read as data when `file` names it", {
  stata <- data.frame(quest = c(1, 2), q1 = haven::labelled(c(1, 2), labels = c("oui" = 1, "non" = 2)))
  spss <- data.frame(QUEST = c(1, 2), Q1 = haven::labelled(c(1, 2), labels = c("Oui" = 1, "Non" = 2)))
  local_reader_catalog(list(
    reader_file("qes_fx", "933", stata, format = "dta", unf = "UNF:6:same"),
    reader_file("qes_fx", "934", spss, role = "label_donor", unf = "UNF:6:same")
  ))
  local_qes_notices_shown()
  out <- get_qes("qes_fx", file = "\\.sav$", with_codebook = FALSE, quiet = TRUE)
  expect_identical(names(out), c("QUEST", "Q1"))
  prov <- attr(out, "qes_provenance")
  expect_identical(prov$file_id, "934")
  # the donor is not its own donor
  expect_identical(prov$label_file_id, NA_character_)
  expect_identical(names(get_qes("qes_fx", with_codebook = FALSE, quiet = TRUE)), c("quest", "q1"))
})

test_that("clearing the memo by md5 drops data read with a label donor", {
  stata <- data.frame(quest = c(1, 2), q1 = haven::labelled(c(1, 2), labels = c("oui" = 1, "non" = 2)))
  spss <- data.frame(QUEST = c(1, 2), Q1 = haven::labelled(c(1, 2), labels = c("Oui" = 1, "Non" = 2)))
  log <- local_reader_catalog(list(
    reader_file("qes_fx", "935", stata, format = "dta", unf = "UNF:6:same"),
    reader_file("qes_fx", "936", spss, role = "label_donor", unf = "UNF:6:same")
  ), memo = TRUE)
  read_fixture("qes_fx")
  expect_length(log$urls, 2L)
  read_fixture("qes_fx")
  expect_length(log$urls, 2L)
  files <- log$catalog$files
  # removing either file (qes_cache_clear(older_than =) clears by md5) drops
  # the entry keyed "<md5>+<donor md5>"
  for (id in c("936", "935")) {
    dropped <- qesR:::.qes_memo_clear(md5 = files$md5[files$file_id == id])
    expect_length(dropped, 1L)
    n <- length(log$urls)
    read_fixture("qes_fx")
    expect_length(log$urls, n + 2L)
  }
  expect_length(qesR:::.qes_memo_clear(md5 = "0123456789abcdef0123456789abcdef"), 0L)
})

test_that("a repeated get_qes() with its codebook makes no request (data memo)", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  withr::local_options(qesR.memo = TRUE)
  local_clean_memo()
  get_qes("qes2018", quiet = TRUE)
  # the codebook is built offline: the data file is the only request
  expect_identical(log$urls, "https://borealisdata.ca/api/access/datafile/425914?format=original")
  get_qes("qes2018", quiet = TRUE)
  getFromNamespace(".get_preview_impl", "qesR")("qes2018", obs = 2L)
  expect_length(log$urls, 1L)
  # qes_cache_clear() forgets the parsed data
  suppressMessages(qes_cache_clear())
  get_qes("qes2018", quiet = TRUE)
  expect_length(log$urls, 2L)
})

test_that("the banner and the codebook follow a `file` that names another study", {
  local_qes_notices_shown()
  log <- local_fake_dataverse()
  msgs <- character(0)
  withCallingHandlers(
    get_qes("qes1998", file = "CROP", with_codebook = FALSE),
    qesR_message_download = function(m) {
      msgs <<- c(msgs, m$id)
      if (identical(m$id, "get_qes_banner")) {
        expect_identical(m$study, "qes1998_crop")
      }
      invokeRestart("muffleMessage")
    },
    message = function(m) invokeRestart("muffleMessage")
  )
  expect_true(all(c("file_redirect", "get_qes_banner") %in% msgs))
  expect_lt(match("file_redirect", msgs), match("get_qes_banner", msgs))

  env <- new.env()
  cb <- getFromNamespace(".qes_codebook_impl", "qesR")(
    "qes1998", file = "CROP", assign_global = TRUE, quiet = TRUE, envir = env
  )
  expect_identical(ls(env), "qes1998_crop_codebook")
  expect_identical(env$qes1998_crop_codebook, cb)
})

test_that("qes_codebook() reads the demo study, like get_qes()", {
  local_qes_notices_shown()
  testthat::local_mocked_bindings(
    .qes_transport = function(...) stop("no request expected"),
    .package = "qesR"
  )
  local_clear_dict_memo()
  cb <- qes_codebook("qes_demo", quiet = TRUE)
  expect_s3_class(cb, "data.frame")
  expect_gt(nrow(cb), 0L)
})
