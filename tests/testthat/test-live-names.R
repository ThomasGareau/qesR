# Tier 1 (live): get_qes() column names equal the v0.4.4 manifest for every
# study (design.md sections 2.4 and 8.2). Never runs on CRAN or offline; set
# QESR_LIVE=true to run it.
#
# Skipped until slice S2b. The interim transport (slice S0c) is paced but has
# no cache, so this loop would send about 40 requests and download every full
# data file on each run. That breaks the Dataverse politeness rule (cache, few
# requests). S2b enables it through the cached transport (live.yml); never
# compare against the baseline without that cache.

test_that("get_qes() column names equal the v0.4.4 manifest (live)", {
  skip("enabled in S2b: live checks need the paced, cached transport (live.yml)")
  skip_on_cran()
  skip_if_not(identical(Sys.getenv("QESR_LIVE"), "true"), "set QESR_LIVE=true to run live tests")
  skip_if_offline()

  manifest <- v044_get_qes_names()
  for (s in unique(manifest$study)) {
    # column names do not depend on the codebook, so skip its requests
    dat <- get_qes(s, with_codebook = FALSE, quiet = TRUE)
    expect_identical(names(dat), manifest$name[manifest$study == s], info = s)
  }
})
