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

# The fake Dataverse with synthetic study data (see above); `data` replaces
# the frames of some studies, `fail` makes some unavailable. Returns the
# request log of local_fake_dataverse().
local_fake_legacy <- function(data = list(), fail = character(0), .env = parent.frame()) {
  syn <- legacy_synthetic()
  syn[names(data)] <- data
  log <- local_fake_dataverse(data = syn, fail = fail, .env = .env)
  check <- getFromNamespace(".qes_hz_check_study", "qesR")
  testthat::local_mocked_bindings(
    .qes_hz_check_study = function(sub, d, study, verified, stand_in) {
      if (identical(study, "qes2022")) invisible(NULL) else check(sub, d, study, verified, stand_in)
    },
    .package = "qesR",
    .env = .env
  )
  invisible(log)
}
