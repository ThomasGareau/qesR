# Contract: the export surface (design.md sections 2.1, 2.2 and 8.1).

test_that("every export is one of the 27 planned names", {
  exports <- getNamespaceExports("qesR")
  expect_length(setdiff(exports, final_exports), 0L)
  expect_length(intersect(exports, internal_only), 0L)
})

test_that("exports equal the 27 names of design.md section 2.2", {
  skip("complete when the 13 new exports have shipped (S1-HZ4)")
  expect_setequal(getNamespaceExports("qesR"), final_exports)
})

test_that("every export outside the 14 frozen v0.4.4 names is named qes_<noun|verb>", {
  exports <- setdiff(getNamespaceExports("qesR"), v044_exports)
  expect_true(all(grepl("^qes_[a-z_]+$", exports)), info = paste(exports, collapse = ", "))
})

test_that("legacy exports are exactly the 11 frozen names", {
  exports <- getNamespaceExports("qesR")
  expect_true(all(legacy_exports %in% exports))
  expect_setequal(setdiff(v044_exports, canonical_existing_exports), legacy_exports)
})

test_that("print.qes_codebook is registered", {
  expect_true(is.function(utils::getS3method("print", "qes_codebook", optional = TRUE)))
})

test_that("every canonical export is mentioned on ?qesR-fr", {
  skip("?qesR-fr is written with the 0.5.0 docs (S5)")
  rd <- tools::Rd_db("qesR")[["qesR-fr.Rd"]]
  text <- paste(as.character(rd), collapse = "")
  canonical <- intersect(c(canonical_existing_exports, new_exports), getNamespaceExports("qesR"))
  for (f in canonical) {
    expect_true(grepl(f, text, fixed = TRUE), info = f)
  }
})

test_that("inst/CITATION equals qes_cite(NULL, 'bibentry')", {
  cite <- getExportedValue("qesR", "qes_cite")
  expect_equal(
    cite(NULL, style = "bibentry"),
    utils::readCitationFile(
      system.file("CITATION", package = "qesR"),
      meta = utils::packageDescription("qesR")
    )
  )
  # utils::citation() needs an installed package (Meta/); from a source tree
  # loaded by pkgload (devtools::test()) it cannot read the version
  installed <- file.exists(file.path(getNamespaceInfo("qesR", "path"), "Meta", "package.rds"))
  if (installed) {
    expect_equal(cite(NULL, style = "bibentry"), utils::citation("qesR"))
  }
})
