# Offline fixture of the legacy builders (slice HZ6): get_qes_master() and
# get_decon() are rendered from the harmonization engine, which checks the
# spec against the catalog (wave sizes, V-D8) and each study's data against
# the spec. The fake Dataverse therefore serves, for every study of the
# spec, the synthetic data of .qes_synthetic() (the real variable names,
# every mapped code, each wave's n_cases members; no respondent), built
# once per session.
#
# The synthetic qes2022 has no value labels: the spec holds only their
# hashes (OD3), so its label check (V-D3) cannot pass on made-up data. The
# fixture skips the data checks of qes2022 only; they run on the real file
# in the live tests, and on synthetic CC0 studies here.

legacy_synthetic_env <- new.env(parent = emptyenv())

legacy_synthetic <- function() {
  if (is.null(legacy_synthetic_env$data)) {
    spec <- getFromNamespace(".qes_spec_get", "qesR")(NULL, "none")
    studies <- getFromNamespace(".qes_hz_covered", "qesR")(spec)
    syn <- getFromNamespace(".qes_synthetic", "qesR")(studies, spec = spec)
    legacy_synthetic_env$data <- lapply(syn, function(d) {
      attr(d, "qes_survey_code") <- NULL
      attr(d, "qes_synthetic") <- NULL
      d
    })
  }
  legacy_synthetic_env$data
}

# The shipped spec with every crosswalk row in review signed off, as if the
# held rows were stable: the renders of every study can then be tested. The
# legacy builders apply signed-off rows only (spec 4.0.0). Since spec 4.1.0
# no shipped row is in review, so this is the shipped spec; it stays so that
# a row put back in review does not hide a render from the tests.
legacy_signed_spec <- function() {
  if (is.null(legacy_synthetic_env$signed)) {
    s <- getFromNamespace(".qes_spec_get", "qesR")(NULL, "none")
    xw <- s$tables$crosswalk
    held <- xw$status %in% "review"
    xw$status[held] <- "stable"
    xw$reviewed_by[held & is.na(xw$reviewed_by)] <- "Test Reviewer"
    xw$reviewed_on[held & is.na(xw$reviewed_on)] <- as.Date("2026-09-27")
    s$tables$crosswalk <- xw
    legacy_synthetic_env$signed <- s
  }
  legacy_synthetic_env$signed
}

# The shipped spec with rows put back in review, to test what the legacy
# builders do with a row that is not signed off: every row of qes1998 and
# the gender row of qes2014, each with a review_note that says why.
legacy_held_note <- "Held for a test: not signed off."
legacy_held_spec <- function() {
  if (is.null(legacy_synthetic_env$held)) {
    s <- getFromNamespace(".qes_spec_get", "qesR")(NULL, "none")
    xw <- s$tables$crosswalk
    held <- xw$study %in% "qes1998" | (xw$study %in% "qes2014" & xw$target %in% "gender")
    xw$status[held] <- "review"
    xw$review_note[held] <- legacy_held_note
    s$tables$crosswalk <- xw
    legacy_synthetic_env$held <- s
  }
  legacy_synthetic_env$held
}

# The fake Dataverse with synthetic study data (see above); `data` replaces
# the frames of some studies, `fail` makes some unavailable. The shipped
# spec's rows in review are applied as if signed off (legacy_signed_spec()),
# to test the renders; with `held = TRUE` the rows of legacy_held_spec() are
# in review instead. Returns the request log of local_fake_dataverse().
local_fake_legacy <- function(data = list(), fail = character(0), held = FALSE, .env = parent.frame()) {
  syn <- legacy_synthetic()
  syn[names(data)] <- data
  log <- local_fake_dataverse(data = syn, fail = fail, .env = .env)
  check <- getFromNamespace(".qes_hz_check_study", "qesR")
  get_spec <- getFromNamespace(".qes_spec_get", "qesR")
  test_spec <- if (isTRUE(held)) legacy_held_spec() else legacy_signed_spec()
  testthat::local_mocked_bindings(
    .qes_hz_check_study = function(sub, d, study, verified, stand_in) {
      if (identical(study, "qes2022")) invisible(NULL) else check(sub, d, study, verified, stand_in)
    },
    .qes_spec_get = function(spec = NULL, validate = "error") {
      if (is.null(spec)) test_spec else get_spec(spec, validate)
    },
    .package = "qesR",
    .env = .env
  )
  invisible(log)
}
