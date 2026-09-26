# The one CSV loader, .qes_read_csv() (design.md section 5.1, slice S1).

read_csv <- function(...) getFromNamespace(".qes_read_csv", "qesR")(...)

write_bytes <- function(text, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
}

test_that("every column is read as text, with the literal NA kept", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_bytes("code,value\n007,NA\n1e5,\n", path)
  x <- read_csv(path)
  expect_identical(x$code, c("007", "1e5"))
  expect_identical(x$value, c("NA", ""))
})

test_that("a schema types columns and turns empty cells into NA", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_bytes(paste0(
    "election_id,jurisdiction,election_type,election_date,label_en,label_fr,source_url\n",
    "QC2018,QC,general,2018-10-01,A,B,\n"
  ), path)
  x <- read_csv(path, "elections")
  expect_s3_class(x$election_date, "Date")
  expect_identical(x$source_url, NA_character_)
})

test_that("an invalid typed value or a wrong header is a classed error", {
  path <- withr::local_tempfile(fileext = ".csv")
  write_bytes(paste0(
    "enum,value,order,code,scope,label_en,label_fr\n",
    "lang,en,first,,,English,Anglais\n"
  ), path)
  err <- expect_error(read_csv(path, "enums"), class = "qesR_error_source")
  expect_identical(err$id, "catalog_invalid")

  write_bytes("enum,value\nlang,en\n", path)
  expect_error(read_csv(path, "enums"), class = "qesR_error_source")
})

test_that("invalid UTF-8 is refused", {
  path <- withr::local_tempfile(fileext = ".csv")
  con <- file(path, open = "wb")
  writeBin(c(charToRaw("a,b\nx,"), as.raw(0xe9), charToRaw("\n")), con)
  close(con)
  expect_error(read_csv(path), class = "qesR_error_source")
})

test_that("accented text survives a C locale set inside the session", {
  e_acute <- intToUtf8(0xE9)
  text <- paste0("a,b\n", "Tr", e_acute, "s,Qu", e_acute, "bec\n")
  path <- withr::local_tempfile(fileext = ".csv")
  write_bytes(text, path)
  expected <- read_csv(path)
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    x <- read_csv(path)
  })
  expect_identical(x, expected)
  expect_identical(x$a, enc2utf8(paste0("Tr", e_acute, "s")))
  expect_identical(Encoding(x$b), "UTF-8")
})

test_that("the shipped catalog reads identically under a C locale", {
  load <- getFromNamespace(".qes_load_catalog", "qesR")
  expected <- load(catalog_dir())
  withr::with_locale(c(LC_CTYPE = "C", LC_COLLATE = "C"), {
    x <- load(catalog_dir())
  })
  expect_identical(x, expected)
})

test_that("lists split on semicolons", {
  split <- getFromNamespace(".qes_split_list", "qesR")
  expect_identical(split("nompn;quest"), c("nompn", "quest"))
  expect_identical(split(NA_character_), character(0))
  expect_identical(split(""), character(0))
})
