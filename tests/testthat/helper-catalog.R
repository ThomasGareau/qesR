# Catalog helpers (slice S1, design.md sections 3 and 8.1).
#
# .qes_catalog() is the catalog test seam. local_fixture_catalog() replaces it
# for the calling test with a small synthetic catalog: the studies and files
# of tests/testthat/fixtures/catalog/, plus the shipped elections, name map
# and enums. Nothing in the fixture describes a real deposit.

catalog_dir <- function() {
  system.file("extdata", "catalog", package = "qesR", mustWork = TRUE)
}

shipped_catalog <- function(demo = FALSE) {
  getFromNamespace(".qes_catalog", "qesR")(demo = demo)
}

fixture_catalog <- function() {
  read_csv <- getFromNamespace(".qes_read_csv", "qesR")
  out <- getFromNamespace(".qes_load_catalog", "qesR")(catalog_dir())
  dir <- testthat::test_path("fixtures", "catalog")
  out$studies <- read_csv(file.path(dir, "studies.csv"), "studies")
  out$studies$demo <- FALSE
  out$files <- read_csv(file.path(dir, "files.csv"), "files")
  out
}

local_fixture_catalog <- function(.env = parent.frame()) {
  fixture <- fixture_catalog()
  testthat::local_mocked_bindings(
    .qes_catalog = function(demo = FALSE) fixture,
    .package = "qesR",
    .env = .env
  )
  invisible(fixture)
}

# Raw bytes of a shipped file (for encoding and line-ending lints).
file_bytes <- function(path) {
  readBin(path, "raw", file.info(path)$size)
}
