# Shared by the harmonization tests (test-hz-*.R).

hz_spec_dir <- function() system.file("extdata", "harmonize", package = "qesR", mustWork = TRUE)

# The shipped spec, loaded afresh (not the session copy) and marked shipped.
hz_spec <- function() {
  s <- .qes_spec_load(normalizePath(hz_spec_dir(), winslash = "/"))
  s$custom <- FALSE
  s
}

# The problems of the data checks on the shipped dictionary after `edit`
# changes the spec tables (and, with `sources_edit`, the sources).
hz_data_problems <- function(edit = identity, sources_edit = identity) {
  s <- hz_spec()
  s$tables <- edit(s$tables)
  .qes_data_check(s, sources_edit(.qes_hz_sources_shipped(s)))
}

# The shipped spec with every signed-off row put back in review (the state
# of every row before spec 4.0.0), to test what happens to rows not signed off.
hz_spec_unsigned <- function() {
  s <- hz_spec()
  xw <- s$tables$crosswalk
  xw$status[xw$status %in% "stable"] <- "review"
  xw$reviewed_by <- NA_character_
  xw$reviewed_on <- as.Date(NA)
  xw$review_note <- NA_character_
  s$tables$crosswalk <- xw
  s
}

hz_errors <- function(p) unique(p$rule[p$severity == "error"])

hz_xw_row <- function(t, study, target) which(t$crosswalk$study == study & t$crosswalk$target == target)
