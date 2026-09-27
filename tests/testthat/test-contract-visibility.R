# Contract: every export returns visibly and has no side effects outside
# tempdir() (design.md section 1.2, constraints 2 and 3; section 8.1).
# All requests go to the offline fake Dataverse (helper-fake-dataverse.R).

export_calls <- function() {
  list(
    get_qes = quote(get_qes("qes2018")),
    get_qes_master = quote(get_qes_master()),
    qes_codebook = quote(qes_codebook("qes2018")),
    get_codebook = quote(get_codebook("qes2018")),
    get_qes_codebook = quote(get_qes_codebook("qes2018")),
    format_codebook = quote(format_codebook(get_codebook("qes2018", quiet = TRUE))),
    get_value_labels = quote(get_value_labels(get_codebook("qes2018", quiet = TRUE))),
    get_question = quote(get_question(get_qes("qes2018", quiet = TRUE), "q1")),
    get_codebook_files = quote(get_codebook_files("qes2018")),
    get_qes_codebook_files = quote(get_qes_codebook_files("qes2018")),
    download_codebook = quote(download_codebook("qes2018")),
    get_preview = quote(get_preview("qes2018")),
    get_decon = quote(get_decon()),
    get_qescodes = quote(get_qescodes()),
    qes_studies = quote(qes_studies()),
    qes_docs = quote(qes_docs()),
    qes_cite = quote(qes_cite("qes2018")),
    qes_download = quote(qes_download("qes2018", path = tempdir(), what = c("data", "docs"))),
    qes_provenance = quote(qes_provenance(get_qes("qes2018", quiet = TRUE))),
    qes_cache_info = quote(qes_cache_info()),
    qes_cache_clear = quote(qes_cache_clear()),
    qes_question = quote(qes_question("qes2018", "q26")),
    qes_search = quote(qes_search("souverain")),
    qes_missing = quote(qes_missing(get_qes("qes2018", quiet = TRUE))),
    # the harmonization spec describes the real files: its checks run
    # against the shipped catalog, not the fake one (the demo study is read
    # from the package, with no request)
    qes_spec = quote(with_shipped_catalog(qes_spec())),
    qes_harmonize = quote(with_shipped_catalog(qes_harmonize("qes_demo", include_draft = TRUE)))
  )
}

with_shipped_catalog <- function(code) {
  load <- getFromNamespace(".qes_load_catalog", "qesR")
  main <- load(catalog_dir())
  with_demo <- load(catalog_dir(), demo_dir = system.file("extdata", "demo", "catalog", package = "qesR"))
  testthat::with_mocked_bindings(
    code,
    .qes_catalog = function(demo = FALSE) if (isTRUE(demo)) with_demo else main,
    .package = "qesR"
  )
}

# Exports that return invisibly by design (design.md section 2.2): they act on
# files and return what they did, not data.
invisible_by_design <- c("qes_cache_clear", "qes_download")

# Point the R_user_dir() roots at empty temporary directories for the calling
# test; tools::R_user_dir() reads these variables on every call.
local_user_dirs <- function(.env = parent.frame()) {
  withr::local_envvar(
    R_USER_DATA_DIR = withr::local_tempdir(.local_envir = .env),
    R_USER_CONFIG_DIR = withr::local_tempdir(.local_envir = .env),
    R_USER_CACHE_DIR = withr::local_tempdir(.local_envir = .env),
    .local_envir = .env
  )
}

side_effect_state <- function() {
  user_files <- lapply(c(data = "data", config = "config", cache = "cache"), function(w) {
    sort(list.files(tools::R_user_dir("qesR", which = w), recursive = TRUE, all.files = TRUE))
  })
  list(
    global = setdiff(ls(globalenv(), all.names = TRUE), ".Random.seed"),
    wd = getwd(),
    wd_files = sort(list.files(getwd(), all.files = TRUE, no.. = TRUE)),
    # only paths qesR could plausibly write in the home directory, so that
    # other processes sharing ~ (CRAN, CI) cannot make the test flaky
    home_qesr = sort(Sys.glob(path.expand(c("~/qesR*", "~/.qesR*", "~/qes_*", "~/qes20*")))),
    user_files = user_files
  )
}

test_that("the call table covers every export", {
  expect_true(all(v044_exports %in% names(export_calls())))
  expect_setequal(names(export_calls()), getNamespaceExports("qesR"))
})

test_that("every export returns its value visibly", {
  local_fake_dataverse()
  local_tempdir_cleanup()
  for (f in names(export_calls())) {
    res <- suppressMessages(withVisible(eval(export_calls()[[f]])))
    expect_identical(res$visible, !(f %in% invisible_by_design), info = f)
    expect_false(is.null(res$value), info = f)
  }
})

test_that("default calls leave globalenv, the working directory, ~ and R_user_dir untouched", {
  local_user_dirs()
  local_fake_dataverse()
  local_tempdir_cleanup()
  before <- side_effect_state()
  expect_identical(unname(lengths(before$user_files)), c(0L, 0L, 0L))
  for (f in names(export_calls())) {
    suppressMessages(eval(export_calls()[[f]]))
    expect_identical(side_effect_state(), before, info = f)
  }
})

test_that("the offline fake answers every request the exports make", {
  log <- local_fake_dataverse()
  local_tempdir_cleanup()
  for (f in names(export_calls())) {
    suppressMessages(eval(export_calls()[[f]]))
  }
  expect_true(length(log$urls) > 0L)
  expect_true(all(grepl("^https://(borealisdata\\.ca|dataverse\\.harvard\\.edu)/api/", log$urls)))
})
