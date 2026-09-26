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
    get_qescodes = quote(get_qescodes())
  )
}

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

test_that("the call table covers all 14 v0.4.4 exports", {
  expect_setequal(names(export_calls()), v044_exports)
})

test_that("every export returns its value visibly", {
  local_fake_dataverse()
  withr::defer(unlink(file.path(tempdir(), "qes2018_questionnaire.txt")))
  for (f in names(export_calls())) {
    res <- suppressMessages(withVisible(eval(export_calls()[[f]])))
    expect_true(res$visible, info = f)
    expect_false(is.null(res$value), info = f)
  }
})

test_that("default calls leave globalenv, the working directory, ~ and R_user_dir untouched", {
  local_user_dirs()
  local_fake_dataverse()
  withr::defer(unlink(file.path(tempdir(), "qes2018_questionnaire.txt")))
  before <- side_effect_state()
  expect_identical(unname(lengths(before$user_files)), c(0L, 0L, 0L))
  for (f in names(export_calls())) {
    suppressMessages(eval(export_calls()[[f]]))
    expect_identical(side_effect_state(), before, info = f)
  }
})

test_that("the offline fake answers every request the exports make", {
  log <- local_fake_dataverse()
  withr::defer(unlink(file.path(tempdir(), "qes2018_questionnaire.txt")))
  for (f in names(export_calls())) {
    suppressMessages(eval(export_calls()[[f]]))
  }
  expect_true(length(log$urls) > 0L)
  expect_true(all(grepl("^https://(borealisdata\\.ca|dataverse\\.harvard\\.edu)/api/", log$urls)))
})
