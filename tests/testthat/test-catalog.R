# Catalog lints (slice S1, design.md sections 3.1-3.4 and 8.1): schemas, keys,
# foreign keys, enum columns, pins, encodings, VERSIONS and the qes_demo tree.

enum_values <- function(cat, name) cat$enums$value[cat$enums$enum == name]

test_that("every catalog CSV is UTF-8 with LF line endings and no BOM", {
  csvs <- c(
    list.files(catalog_dir(), pattern = "\\.csv$", full.names = TRUE),
    list.files(system.file("extdata", "demo", "catalog", package = "qesR"), pattern = "\\.csv$", full.names = TRUE)
  )
  expect_gte(length(csvs), 7L)
  for (f in csvs) {
    b <- file_bytes(f)
    expect_false(any(b == as.raw(0x0d)), info = basename(f))
    expect_false(identical(b[1:3], as.raw(c(0xef, 0xbb, 0xbf))), info = basename(f))
    text <- rawToChar(b)
    Encoding(text) <- "UTF-8"
    expect_true(validUTF8(text), info = basename(f))
    # no U+FFFD replacement character and no C1 control character
    cps <- utf8ToInt(text)
    expect_false(any(cps == 0xFFFD), info = basename(f))
    expect_false(any(cps >= 0x80 & cps <= 0x9F), info = basename(f))
  }
})

test_that("the catalog tables match their schemas and keys are unique", {
  cat <- shipped_catalog()
  schemas <- getFromNamespace(".qes_schemas", "qesR")
  for (tab in names(schemas)) {
    expect_identical(setdiff(names(cat[[tab]]), "demo"), names(schemas[[tab]]), info = tab)
  }
  expect_false(anyDuplicated(cat$studies$study) > 0L)
  expect_false(anyDuplicated(paste(cat$files$study, cat$files$file_id)) > 0L)
  expect_false(anyDuplicated(cat$elections$election_id) > 0L)
  expect_false(anyDuplicated(paste(cat$name_map$file_id, cat$name_map$source_name)) > 0L)
  expect_false(anyDuplicated(paste(cat$enums$enum, cat$enums$value)) > 0L)
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", cat$studies$study)))
})

test_that("required study fields are filled", {
  s <- shipped_catalog()$studies
  required <- c(
    "study", "family", "title_deposit", "title_en", "title_fr", "authors", "year",
    "study_design", "default_member", "target_population_en", "target_population_fr",
    "server", "doi", "dataset_version", "data_file_id", "source_lang", "licence",
    "licence_url", "metadata_shipped", "publisher", "citation_year", "dataset_unf"
  )
  for (col in required) {
    expect_false(anyNA(s[[col]]), info = col)
  }
})

test_that("foreign keys hold between the catalog tables", {
  cat <- shipped_catalog()
  s <- cat$studies
  f <- cat$files
  expect_true(all(f$study %in% s$study))
  expect_true(all(s$study %in% f$study))
  ok_election <- is.na(s$election_id) | s$election_id %in% cat$elections$election_id
  expect_true(all(ok_election), info = paste(s$study[!ok_election], collapse = ", "))
  expect_true(all(cat$name_map$file_id %in% f$file_id))
  # the pinned data file is that study's one default data file
  for (i in seq_len(nrow(s))) {
    rows <- f[f$study == s$study[i] & f$role == "data" & f$is_default, , drop = FALSE]
    expect_identical(nrow(rows), 1L, info = s$study[i])
    expect_identical(rows$file_id, s$data_file_id[i], info = s$study[i])
    expect_identical(rows$dataset_version, s$dataset_version[i], info = s$study[i])
    if (!is.na(s$label_file_id[i])) {
      donor <- f[f$study == s$study[i] & f$file_id == s$label_file_id[i], , drop = FALSE]
      expect_identical(donor$role, "label_donor", info = s$study[i])
      pinned <- f[f$study == s$study[i] & f$file_id == s$data_file_id[i], , drop = FALSE]
      expect_identical(donor$unf, pinned$unf, info = s$study[i])
    }
  }
  # only data files are defaults, and every file carries its study's version
  expect_true(all(f$role[f$is_default] == "data"))
  expect_identical(f$dataset_version, s$dataset_version[match(f$study, s$study)])
})

test_that("enum columns use the values of enums.csv", {
  cat <- shipped_catalog()
  s <- cat$studies
  f <- cat$files
  checks <- list(
    list(s$family, "family"), list(s$study_design, "study_design"),
    list(s$source_lang, "lang"), list(s$licence, "licence"),
    list(f$role, "role"), list(f$format, "format"),
    list(f$lang[!is.na(f$lang)], "lang"),
    list(cat$elections$jurisdiction, "jurisdiction"),
    list(cat$elections$election_type, "election_type")
  )
  for (chk in checks) {
    expect_true(all(chk[[1]] %in% enum_values(cat, chk[[2]])), info = chk[[2]])
  }
  # documents have a language, data files do not
  docs <- !(f$role %in% c("data", "label_donor"))
  expect_false(anyNA(f$lang[docs]))
  expect_true(all(is.na(f$lang[!docs])))
})

test_that("every enum value has an English and a French label", {
  e <- shipped_catalog()$enums
  expect_false(anyNA(e$label_en))
  expect_false(anyNA(e$label_fr))
  expect_true(all(nzchar(e$label_en) & nzchar(e$label_fr)))
  expect_true(all(grepl("^[a-z][a-z0-9_]*$", unique(e$enum))))
  for (nm in unique(e$enum)) {
    expect_identical(sort(e$order[e$enum == nm]), seq_len(sum(e$enum == nm)), info = nm)
  }
})

test_that("checksums, sizes and dimensions are well formed", {
  f <- shipped_catalog()$files
  expect_true(all(grepl("^[0-9a-f]{32}$", f$md5)))
  expect_true(all(f$checksum_type == "MD5"))
  expect_true(all(grepl("^[0-9]+$", f$file_id)))
  expect_true(all(f$bytes > 0))
  data <- f$role %in% c("data", "label_donor")
  expect_false(anyNA(f$n_rows[data]))
  expect_false(anyNA(f$n_cols[data]))
  expect_false(anyNA(f$id_vars[data]))
  expect_true(all(f$ingested[data]))
  expect_true(all(grepl("^UNF:6:", f$unf[data])))
  expect_true(all(f$format[data] %in% c("sav", "zsav", "por", "dta")))
  expect_true(all(f$format[!data] %in% c("pdf", "doc", "docx")))
})

test_that("metadata_shipped follows the licence", {
  s <- shipped_catalog()$studies
  expect_identical(s$metadata_shipped, grepl("^CC0", s$licence))
  expect_identical(s$study[!s$metadata_shipped], "qes2022")
})

test_that("the first 11 studies are the qesR 0.4.4 codes, in order", {
  s <- shipped_catalog()$studies
  expect_identical(s$study[seq_along(v044_qescodes)], v044_qescodes)
  expect_identical(getFromNamespace(".qes_legacy_codes", "qesR"), v044_qescodes)
})

test_that("pins match the Dataverse records they were built from", {
  # Spot checks against the cached dataset JSON (design.md section 3.3).
  f <- shipped_catalog()$files
  s <- shipped_catalog()$studies
  pin <- function(study, id) f[f$study == study & f$file_id == id, , drop = FALSE]
  expect_identical(pin("qes2022", "7449513")$md5, "c51bafed57776ffa8d4c6f301f5c945b")
  expect_identical(pin("qes2022", "7449513")$n_cols, 718L)
  expect_identical(pin("qes2018", "425914")$md5, "d24f5b0be727d688ad305b8eb61f0d30")
  expect_identical(pin("qes2014", "425916")$format, "sav")
  expect_identical(pin("qes2012", "425917")$role, "label_donor")
  expect_identical(pin("qes2007_panel", "352415")$id_vars, "nompn;quest")
  expect_identical(pin("qes_crop_2007_2010", "329990")$n_rows, 24027L)
  expect_identical(s$dataset_version[s$study == "qes2022"], "1.1")
  expect_identical(s$server[s$study == "qes2022"], "https://dataverse.harvard.edu")
  expect_true(all(s$server[s$study != "qes2022"] == "https://borealisdata.ca"))
})

test_that("the 1998 deposit is split into its three surveys", {
  s <- shipped_catalog()$studies
  f <- shipped_catalog()$files
  s98 <- s[s$family == "polls_1998", , drop = FALSE]
  expect_identical(s98$study, c("qes1998", "qes1998_crop", "qes1998_createc"))
  expect_identical(unique(s98$doi), "10.5683/SP2/QFUAWG")
  expect_identical(s98$data_file_id, c("329987", "286331", "316121"))
  n <- f$n_rows[match(s98$data_file_id, f$file_id)]
  expect_identical(n, c(1483L, 450L, 1057L))
  # qes1998 keeps the file qesR 0.4.4 loaded
  expect_identical(s98$data_file_id[1], "329987")
})

test_that("id_vars and name_map targets exist in the v0.4.4 name manifest", {
  manifest <- v044_get_qes_names()
  cat <- shipped_catalog()
  f <- cat$files
  for (code in v044_qescodes) {
    row <- f[f$study == code & f$is_default, , drop = FALSE]
    ids <- setdiff(strsplit(row$id_vars, ";", fixed = TRUE)[[1]], ".row")
    expect_true(all(ids %in% manifest$name[manifest$study == code]), info = code)
  }
  nm <- cat$name_map
  expect_true(all(as_utf8(nm$name) %in% manifest$name[manifest$study == "qes2007_panel"]))
  expect_false(any(as_utf8(nm$source_name) %in% manifest$name[manifest$study == "qes2007_panel"]))
})

test_that("VERSIONS records the catalog version and the md5 of every CSV", {
  path <- system.file("extdata", "VERSIONS", package = "qesR", mustWork = TRUE)
  v <- read.dcf(path)
  expect_true(all(c("catalog_version", "schema_version", "built_from", "csv_md5") %in% colnames(v)))
  expect_match(v[1, "catalog_version"], "^[0-9]+\\.[0-9]+\\.[0-9]+$")
  split <- function(x) trimws(strsplit(x, ",", fixed = TRUE)[[1]])

  recorded <- split(v[1, "csv_md5"])
  csvs <- sort(list.files(catalog_dir(), pattern = "\\.csv$"))
  expected <- sprintf(
    "catalog/%s:%s", csvs, unname(tools::md5sum(file.path(catalog_dir(), csvs)))
  )
  expect_identical(recorded, expected)

  f <- shipped_catalog()$files
  expect_setequal(split(v[1, "built_from"]), unique(sprintf("%s:%s", f$file_id, f$md5)))
})

test_that("elections are the verified DGEQ general elections", {
  e <- shipped_catalog()$elections
  expect_s3_class(e$election_date, "Date")
  expect_identical(format(e$election_date[e$election_id == "QC2018"]), "2018-10-01")
  expect_identical(format(e$election_date[e$election_id == "QC1998"]), "1998-11-30")
  expect_true(all(grepl("^https://donnees\\.electionsquebec\\.qc\\.ca/", e$source_url)))
})

test_that("the NA vocabulary is closed, tagged and bilingual", {
  na <- getFromNamespace(".qes_na_reasons", "qesR")()
  expect_identical(names(na), c("value", "code", "scope", "label_en", "label_fr"))
  expect_true(all(c(
    "dk", "refused", "dk_refused", "no_answer", "not_selected", "inapplicable",
    "not_voted", "spoiled", "ineligible", "not_registered", "not_in_wave",
    "not_mappable", "user_na", "sysmis", "not_asked", "below_grade", "unmapped"
  ) %in% na$value))
  tagged <- na$code[!is.na(na$code)]
  expect_true(all(grepl("^[a-z]$", tagged)))
  expect_false(anyDuplicated(tagged) > 0L)
  # reasons set only by the engine have no qes_missing() tag
  expect_true(all(is.na(na$code[na$scope == "engine"])))
  expect_identical(na$code[na$value == "not_registered"], "g")
})

test_that("the qes_demo tree is synthetic, separate and checksummed", {
  main <- shipped_catalog()
  demo <- shipped_catalog(demo = TRUE)
  expect_false("qes_demo" %in% main$studies$study)
  expect_true("qes_demo" %in% demo$studies$study)
  expect_identical(nrow(demo$studies), nrow(main$studies) + 1L)
  row <- demo$files[demo$files$study == "qes_demo", , drop = FALSE]
  expect_identical(nrow(row), 1L)
  path <- system.file("extdata", "demo", "data", row$file_name, package = "qesR", mustWork = TRUE)
  expect_identical(unname(tools::md5sum(path)), row$md5)
  expect_identical(file.info(path)$size, row$bytes)
  expect_lt(file.info(path)$size, 20000)
  dat <- haven::read_sav(path)
  expect_identical(c(nrow(dat), ncol(dat)), c(row$n_rows, row$n_cols))
  # variable names are a subset of real qes2014 names
  manifest <- v044_get_qes_names()
  expect_true(all(names(dat) %in% manifest$name[manifest$study == "qes2014"]))
  expect_identical(demo$studies$family[demo$studies$study == "qes_demo"], "demo")
  # like qes2014: QUEST is constant 0 without a label, and rows are the key
  expect_true(all(dat$QUEST == 0))
  expect_null(attr(dat$QUEST, "label", exact = TRUE))
  expect_identical(row$id_vars, ".row")
})

test_that(".qes_catalog() reads the catalog once per session", {
  a <- shipped_catalog()
  b <- shipped_catalog()
  expect_identical(a, b)
  cache <- getFromNamespace(".qes_catalog_cache", "qesR")
  expect_true(exists("main", envir = cache, inherits = FALSE))
})
