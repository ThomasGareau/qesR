can_run_tests <- requireNamespace("testthat", quietly = TRUE) &&
  requireNamespace("waldo", quietly = TRUE) &&
  requireNamespace("vctrs", quietly = TRUE) &&
  utils::packageVersion("vctrs") >= "0.6.0"

if (can_run_tests) {
  library(testthat)
  library(qesR)
  test_check("qesR")
} else {
  message("Skipping testthat checks: incompatible or missing test dependencies.")
}
