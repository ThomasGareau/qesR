# Contract: the 14 v0.4.4 exports keep their signatures (design.md section 1.2,
# constraint 1; section 8.1 "formals() snapshot").

test_that("all 14 v0.4.4 exports still exist and are functions", {
  exports <- getNamespaceExports("qesR")
  for (f in v044_exports) {
    expect_true(f %in% exports, info = f)
    expect_true(is.function(getExportedValue("qesR", f)), info = f)
  }
})

test_that("formals() match d1faad6 up to the allowed differences", {
  expected <- allowed_formals()
  for (f in v044_exports) {
    current <- as.list(formals(getExportedValue("qesR", f)))
    want <- as.list(expected[[f]])
    n <- length(want)

    expect_identical(names(current)[seq_len(n)], names(want), info = f)
    expect_identical(current[seq_len(n)], want, info = f)

    extra <- current[-seq_len(n)]
    allowed <- as.list(allowed_appended_formals[[f]] %||% list())
    expect_true(length(extra) <= length(allowed), info = f)
    if (length(extra) > 0L) {
      expect_identical(extra, allowed[seq_along(extra)], info = f)
    }
  }
})

test_that("assign_global defaults to FALSE on every export that has it", {
  for (f in v044_exports) {
    fm <- formals(getExportedValue("qesR", f))
    if ("assign_global" %in% names(fm)) {
      expect_false(fm$assign_global, info = f)
    }
  }
})

test_that("get_qes() and get_qes_master() have no `...` and no trailing arguments", {
  expect_identical(
    names(formals(qesR::get_qes)),
    c("srvy", "file", "assign_global", "with_codebook", "quiet")
  )
  expect_identical(
    names(formals(qesR::get_qes_master)),
    c("surveys", "assign_global", "object_name", "quiet", "strict", "save_path")
  )
})
