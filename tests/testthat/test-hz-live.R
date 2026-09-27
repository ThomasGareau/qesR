# Tier 1 (live) for the harmonization spec (design.md sections 5.10 and 8.2,
# slice HZ2): on the pinned originals, the data checks pass and the
# projection from the file equals expected/marginals.csv, so the offline
# projection from the dictionary and gates.csv (test-hz-projection.R) is
# exact. Never runs on CRAN; see helper-live.R.

test_that("the spec passes the data checks on the originals and V-P1 is exact (live)", {
  local_live_originals()
  s <- hz_spec()
  cat_ <- .qes_catalog()
  shipped <- cat_$studies$study[cat_$studies$metadata_shipped %in% TRUE]
  for (study in unique(s$tables$crosswalk$study)) {
    data <- stats::setNames(list(.qes_read(study)), study)
    p <- .qes_data_check_frames(s, data)
    expect_identical(hz_errors(p), character(0), label = study)
    if (study %in% shipped) {
      proj <- .qes_project_marginals(s, .qes_hz_sources_data(s, data), study)
      expect_null(attr(proj, "unprojected"))
      attr(proj, "unprojected") <- NULL
      e <- s$tables$expected[s$tables$expected$study == study, , drop = FALSE]
      rownames(e) <- NULL
      expect_identical(proj, e, label = study)
    }
  }
})
