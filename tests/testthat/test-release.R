# Release contract for 0.5.0 (design.md sections 1.2, 5.13, 9 and 10; OD13):
# licence and copyright, no shipped respondent data, offline examples, the
# French entry point and NEWS.

pkg_file <- function(...) system.file(..., package = "qesR")

test_that("Authors@R names one person, the maintainer, as the copyright holder (OD13)", {
  desc <- utils::packageDescription("qesR")
  authors <- eval(parse(text = desc[["Authors@R"]]))
  holders <- authors[vapply(authors, function(p) "cph" %in% p$role, logical(1))]
  expect_length(holders, 1L)
  expect_identical(format(holders, include = c("given", "family")), "Thomas Gareau-Paquette")
  expect_true(all(c("aut", "cre") %in% holders[[1]]$role))
  expect_false(any(grepl("Quebec Election Study", format(authors), fixed = TRUE)))
  expect_false("LazyData" %in% names(desc))
})

test_that("LICENSE names the copyright holder of Authors@R", {
  path <- pkg_file("LICENSE")
  skip_if(!nzchar(path), "LICENSE is not installed")
  lines <- readLines(path, warn = FALSE)
  expect_true("COPYRIGHT HOLDER: Thomas Gareau-Paquette" %in% lines)
  expect_false(any(grepl("rightful owners|Quebec Election Study", lines)))
})

test_that("no respondent data ship: the only data file is the synthetic demo", {
  expect_identical(pkg_file("extdata", "qes_master.csv"), "")
  files <- list.files(pkg_file("extdata"), recursive = TRUE)
  data_files <- files[grepl("\\.(sav|zsav|dta|por|rds|rda|RData|tab|xlsx?)$", files, ignore.case = TRUE)]
  expect_identical(data_files, "demo/data/qes_demo.sav")
  # every CSV is metadata (catalog, dictionary, legacy tables, harmonization
  # spec) or a validation table: public benchmarks and the aggregates of the
  # validation report
  csv <- files[grepl("\\.csv(\\.gz)?$", files)]
  expect_true(all(grepl("^(catalog|dict|legacy|harmonize|validation|demo/catalog|demo/dict)/", csv)), info = paste(csv, collapse = ", "))
})

test_that("COPYRIGHTS attributes every shipped study by DOI and gives the licence of qes2022", {
  path <- pkg_file("COPYRIGHTS")
  expect_true(nzchar(path))
  text <- paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = "\n")
  studies <- qesR:::.qes_catalog()$studies
  studies <- studies[!studies$demo, , drop = FALSE]
  for (i in seq_len(nrow(studies))) {
    expect_true(grepl(studies$study[i], text, fixed = TRUE), info = studies$study[i])
    expect_true(grepl(studies$doi[i], text, fixed = TRUE), info = studies$doi[i])
  }
  expect_true(grepl("CC BY-NC 4.0", text, fixed = TRUE))
  # OD3 lifted: the qes2022 metadata ships under its licence, with attribution
  expect_true(grepl("https://creativecommons.org/licenses/by-nc/4.0/", text, fixed = TRUE))
  expect_true(grepl("NOT covered by the MIT\\s+licence of qesR", text))
  expect_true(grepl("https://doi.org/10.7910/DVN/PAQBDR", text, fixed = TRUE))
  # OD16: the 1998 population is quoted from the codebook; the definition,
  # pending until spec 3.0.0, is confirmed by linking the files
  expect_true(grepl("retenir uniquement les\\s+francophones", text))
  expect_true(grepl("confirms the definition", text, fixed = TRUE))
  # Élections Québec's open-data licence and its required notice
  expect_true(grepl("https://www.dgeq.org/licence.html", text, fixed = TRUE))
  expect_true(grepl("Comprend des donn\u00e9es ouvertes octroy\u00e9es", text, fixed = TRUE))
  # the benchmark sources of inst/extdata/validation/ and their terms
  expect_true(grepl("Statistics Canada Open Licence", text, fixed = TRUE))
  expect_true(grepl("Adapted from Statistics Canada", text, fixed = TRUE))
  expect_true(grepl("does not constitute an endorsement\\s+by Statistics Canada", text))
  expect_true(grepl("\u00c9lections Qu\u00e9bec's terms\\s+of use", text))
})

test_that("the MIT licence is stated to cover the code only", {
  desc <- utils::packageDescription("qesR")
  expect_identical(desc[["License"]], "MIT + file LICENSE")
  copyright <- gsub("\\s+", " ", desc[["Copyright"]])
  expect_match(copyright, "covers the package code only", fixed = TRUE)
  expect_match(copyright, "inst/COPYRIGHTS", fixed = TRUE)
  expect_match(copyright, "CC BY-NC 4.0", fixed = TRUE)
  expect_match(gsub("\\s+", " ", desc[["Description"]]), "MIT licence covers the package code only", fixed = TRUE)
  path <- pkg_file("COPYRIGHTS")
  text <- gsub("\\s+", " ", paste(readLines(path, encoding = "UTF-8", warn = FALSE), collapse = " "))
  expect_match(text, "covers the qesR code only", fixed = TRUE)
  # the attribution the licence requires: authors, title, DOI, licence, URL,
  # changes made
  for (x in c("Mah\u00e9o, Val\u00e9rie-Anne", "B\u00e9langer, \u00c9ric", "Stephenson, Laura B",
              "Harell, Allison", "\"2022 Quebec Election Study\"", "Changes made by qesR",
              "Licensed under CC BY-NC 4.0 (https://creativecommons.org/licenses/by-nc/4.0/)")) {
    expect_match(text, x, fixed = TRUE)
  }
})

test_that("the qes2022 licence notice lists exactly the shipped files with its metadata", {
  path <- pkg_file("COPYRIGHTS")
  expect_true(nzchar(path))
  text <- readLines(path, encoding = "UTF-8", warn = FALSE)
  # the list of section 2: from "... licence of qesR:" to "Attribution"
  from <- grep("licence of qesR:$", text)
  to <- grep("^Attribution", text)
  expect_length(from, 1L)
  expect_length(to, 1L)
  block <- text[(from + 1L):(to - 1L)]
  listed <- unique(unlist(regmatches(block, gregexpr("(inst|tests)/[A-Za-z0-9_./-]+\\.csv(\\.gz)?", block))))
  expect_gt(length(listed), 5L)
  read_table <- function(file) {
    con <- if (grepl("\\.gz$", file)) gzfile(file) else file
    utils::read.csv(con, colClasses = "character", encoding = "UTF-8")
  }
  has_2022 <- function(x) {
    any(x$study %in% "qes2022") || any(grepl("qes2022", x$map_id %||% character(0), fixed = TRUE))
  }
  for (p in listed) {
    file <- if (startsWith(p, "inst/")) {
      system.file(sub("^inst/", "", p), package = "qesR")
    } else {
      testthat::test_path(sub("^tests/testthat/", "", p))
    }
    expect_true(nzchar(file) && file.exists(file), info = p)
    if (nzchar(file) && file.exists(file)) {
      expect_true(has_2022(read_table(file)), info = p)
    }
  }
  # every installed table with rows of qes2022 is listed, except those that
  # only name the study (the catalog's files, the render rules, the legacy
  # changes) and the build-ignored validation report; the catalog's
  # studies.csv is listed, for the qes2022 notes that quote the codebook
  ext <- system.file("extdata", package = "qesR")
  files <- list.files(ext, pattern = "\\.csv(\\.gz)?$", recursive = TRUE)
  files <- files[!startsWith(files, "demo/") & !grepl("(^|/)\\._", files)]
  own <- c("catalog/files.csv", "harmonize/legacy.csv",
           "legacy/changes.csv", "validation/validation_report.csv")
  with_2022 <- files[vapply(file.path(ext, files), function(f) has_2022(read_table(f)), logical(1))]
  expect_setequal(setdiff(with_2022, own), sub("^inst/extdata/", "", listed[startsWith(listed, "inst/")]))
})

test_that("the only network example is qes_studies(check_updates = TRUE), guarded", {
  db <- qesR_rd_db()
  text <- vapply(db, function(rd) paste(as.character(rd), collapse = ""), character(1))
  with_donttest <- names(text)[grepl("\\donttest", text, fixed = TRUE)]
  expect_identical(with_donttest, "qes_studies.Rd")
  expect_false(any(grepl("\\dontrun", text, fixed = TRUE)))
  ex <- text[["qes_studies.Rd"]]
  expect_true(grepl("check_updates = TRUE", ex, fixed = TRUE))
  expect_true(grepl("curl::has_internet()", ex, fixed = TRUE))
  expect_true(grepl("qesR_error_network", ex, fixed = TRUE))
})

test_that("no example has a commented-out line of code", {
  db <- qesR_rd_db()
  for (name in names(db)) {
    ex <- tempfile(fileext = ".R")
    tools::Rd2ex(db[[name]], ex, commentDonttest = FALSE, commentDontrun = FALSE)
    if (!file.exists(ex)) next
    lines <- readLines(ex, encoding = "UTF-8", warn = FALSE)
    # "# x <- ..." or a whole call "# f(...)"; prose such as
    # "# merge() drops the record" does not match
    code <- grep("^# *[A-Za-z_.][A-Za-z0-9_.:]* *(<-.*|\\(.*\\))[[:space:]]*$", lines, value = TRUE)
    expect_identical(code, character(0), info = name)
  }
})

test_that("every example outside \\donttest runs offline", {
  db <- qesR_rd_db()
  local_mocked_bindings(
    .qes_transport = function(...) stop("an example made a network request"),
    .package = "qesR"
  )
  withr::local_options(qesR.lang = "en", qesR.quiet_deprecated = TRUE)
  withr::local_dir(withr::local_tempdir())
  for (name in names(db)) {
    ex <- tempfile(fileext = ".R")
    tools::Rd2ex(db[[name]], ex, commentDonttest = TRUE, commentDontrun = TRUE)
    if (!file.exists(ex)) next
    env <- new.env(parent = globalenv())
    err <- tryCatch(
      {
        # (an example may show an error with try(), which prints to stderr)
        utils::capture.output(
          invisible(suppressMessages(utils::capture.output(source(ex, local = env, echo = FALSE)))),
          type = "message"
        )
        NULL
      },
      error = function(e) conditionMessage(e)
    )
    expect_null(err, info = name)
  }
})

test_that("NEWS has a 0.5.0 section with the soft-deprecation table", {
  path <- pkg_file("NEWS.md")
  skip_if(!nzchar(path), "NEWS.md is not installed")
  news <- readLines(path, encoding = "UTF-8", warn = FALSE)
  version <- utils::packageDescription("qesR")$Version
  expect_identical(news[grepl("^# ", news)][1], paste("# qesR", version))
  section <- news[seq_len(which(news == "# qesR 0.4.4") - 1L)]
  for (f in qesR:::.qes_deprecated$name) {
    expect_true(any(grepl(paste0("`", f, "()`"), section, fixed = TRUE)), info = f)
  }
  expect_true(any(grepl("legacy-diff.md", section, fixed = TRUE)))
})

test_that("the catalog version NEWS quotes is the one in VERSIONS", {
  news_path <- pkg_file("NEWS.md")
  versions_path <- pkg_file("extdata", "VERSIONS")
  skip_if(!nzchar(news_path) || !nzchar(versions_path), "NEWS.md or VERSIONS is not installed")
  news <- readLines(news_path, encoding = "UTF-8", warn = FALSE)
  section <- news[seq_len(which(news == "# qesR 0.4.4") - 1L)]
  # the newest release that changed the catalog quotes the current version
  quoted <- regmatches(section, regexpr("catalog version is now [0-9]+\\.[0-9]+\\.[0-9]+", section))
  expect_gte(length(quoted), 1L)
  recorded <- unname(trimws(read.dcf(versions_path, fields = "catalog_version")[1, 1]))
  expect_identical(sub("^.* ", "", quoted[1]), recorded)
})
