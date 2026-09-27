# Run the whole suite against the session cache in tempdir(): a developer's
# own cache settings (options qesR.cache and qesR.cache_dir, or the variables
# QESR_CACHE and QESR_CACHE_DIR, e.g. from .Rprofile or .Renviron) must never
# point qes_cache_clear() or a download at their persistent cache.
withr::local_options(
  qesR.cache = NULL, qesR.cache_dir = NULL,
  .local_envir = testthat::teardown_env()
)
withr::local_envvar(
  QESR_CACHE = NA, QESR_CACHE_DIR = NA,
  .local_envir = testthat::teardown_env()
)
