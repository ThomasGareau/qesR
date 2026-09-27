# Tier 1 (live) tests: the pinned originals, from a local cache or the
# network (design.md section 8.2). Never on CRAN.
#
#   * QESR_TEST_DATA_DIR=<dir>: <dir> is a download cache laid out by hand
#     (<dir>/qesR/v1/<host>/<file_id>-<md5>.<ext>, see ?qes_cache_info),
#     holding the md5-verified originals. No request is made.
#   * QESR_LIVE=true: the files are downloaded once, politely, into the
#     session cache.
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
