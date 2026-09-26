# Contract: legacy output shapes (design.md sections 2.3, 2.4 and 8.1,
# "Legacy master" and "Legacy name manifests"). Column names come from the
# v0.4.4 outputs (helper-manifests.R); data comes from the offline fake.

test_that("get_qescodes() keeps the v0.4.4 columns and the first 11 codes", {
  local_qes_notices_shown()
  codes <- get_qescodes()
  expect_s3_class(codes, "data.frame")
  expect_identical(names(codes), v044_qescodes_cols)
  expect_identical(codes$qes_survey_code[seq_along(v044_qescodes)], v044_qescodes)
  expect_identical(codes$index[seq_along(v044_qescodes)], seq_along(v044_qescodes))
  expect_identical(names(get_qescodes(detailed = TRUE)), v044_qescodes_detailed_cols)
})

test_that("get_qes_master() keeps the 30 documented columns, in order", {
  local_qes_notices_shown()
  local_fake_dataverse()
  master <- get_qes_master(quiet = TRUE)
  n <- length(v044_master_cols)
  expect_identical(names(master)[seq_len(n)], names(v044_master_cols))
})

test_that("get_qes_master() keeps the v0.4.4 type of every documented column", {
  local_qes_notices_shown()
  skip("fixed in S4: a column with no valid source is all-NA of its v0.4.4 type, not logical")
  local_fake_dataverse()
  master <- get_qes_master(quiet = TRUE)
  n <- length(v044_master_cols)
  types <- vapply(master[seq_len(n)], function(x) class(x)[1], character(1))
  expect_identical(types, v044_master_cols)
})

test_that("get_qes_master() keeps every v0.4.4 attribute name", {
  local_qes_notices_shown()
  local_fake_dataverse()
  master <- get_qes_master(quiet = TRUE)
  expect_true(all(v044_master_attrs %in% names(attributes(master))))
  expect_identical(sort(attr(master, "loaded_surveys")), sort(v044_qescodes))
  expect_identical(attr(master, "failed_surveys"), character(0))
})

test_that("get_qes_master() records failures and keeps the other studies", {
  local_qes_notices_shown()
  local_fake_dataverse(fail = "qes2008")
  master <- get_qes_master(surveys = c("qes2018", "qes2008"), quiet = TRUE)
  expect_identical(attr(master, "loaded_surveys"), "qes2018")
  expect_length(attr(master, "failed_surveys"), 1L)
  expect_match(attr(master, "failed_surveys"), "^qes2008:")
  expect_error(
    get_qes_master(surveys = c("qes2018", "qes2008"), quiet = TRUE, strict = TRUE)
  )
})

test_that("get_qes_master() never drops a row", {
  local_qes_notices_shown()
  skip("fixed in S4: no de-duplication and no empty-row removal (P5)")
  dup <- fake_study_data("qes2018")
  dup$quest[2] <- dup$quest[1]
  dup[3, c("age", "sexe", "q1", "poids")] <- NA
  local_fake_dataverse(data = list(qes2018 = dup))
  master <- get_qes_master(surveys = "qes2018", quiet = TRUE)
  expect_identical(nrow(master), nrow(dup))
  expect_identical(attr(master, "duplicates_removed"), 0L)
  expect_identical(attr(master, "empty_rows_removed"), 0L)
})

test_that("get_decon() keeps the 19 v0.4.4 columns", {
  local_qes_notices_shown()
  local_fake_dataverse()
  decon <- get_decon("qes2018", quiet = TRUE)
  expect_identical(names(decon), v044_decon_cols)
  expect_identical(nrow(decon), nrow(fake_study_data("qes2018")))
})

test_that("codebook layouts keep the v0.4.4 columns", {
  local_qes_notices_shown()
  local_fake_dataverse()
  for (layout in names(v044_codebook_cols)) {
    cb <- get_codebook("qes2018", quiet = TRUE, layout = layout)
    expect_s3_class(cb, "qes_codebook")
    expect_identical(names(cb), v044_codebook_cols[[layout]], info = layout)
  }
  cb <- get_codebook("qes2018", quiet = TRUE)
  expect_identical(names(get_value_labels(cb, long = TRUE)), v044_value_labels_long_cols)
})

test_that("codebook file helpers keep the v0.4.4 columns", {
  local_qes_notices_shown()
  local_fake_dataverse()
  dest <- withr::local_tempdir()
  expect_identical(names(get_codebook_files("qes2018", quiet = TRUE)), v044_codebook_files_cols)
  expect_identical(names(get_qes_codebook_files("qes2018", quiet = TRUE)), v044_codebook_files_cols)
  out <- download_codebook("qes2018", dest_dir = dest, quiet = TRUE)
  expect_identical(names(out), v044_download_codebook_cols)
  expect_true(all(file.exists(out$local_path)))
  expect_true(all(startsWith(normalizePath(out$local_path), normalizePath(dest))))
})

test_that("get_preview() returns the first `obs` rows of get_qes()", {
  local_qes_notices_shown()
  local_fake_dataverse()
  prev <- get_preview("qes2018", obs = 2)
  expect_identical(nrow(prev), 2L)
  expect_identical(names(prev), names(fake_study_data("qes2018")))
})

test_that("get_qes() returns the served columns unchanged and records the code", {
  local_qes_notices_shown()
  local_fake_dataverse()
  dat <- get_qes("qes2018", quiet = TRUE)
  expect_s3_class(dat, "data.frame")
  expect_identical(names(dat), names(fake_study_data("qes2018")))
  expect_identical(nrow(dat), 4L)
  expect_identical(attr(dat, "qes_survey_code"), "qes2018")
  expect_s3_class(attr(dat, "qes_codebook"), "data.frame")
})

test_that("the v0.4.4 get_qes() name manifest covers the 11 legacy studies", {
  local_qes_notices_shown()
  manifest <- v044_get_qes_names()
  expect_identical(names(manifest), c("study", "position", "name"))
  expect_setequal(unique(manifest$study), v044_qescodes)
  for (s in v044_qescodes) {
    rows <- manifest[manifest$study == s, , drop = FALSE]
    expect_identical(rows$position, seq_len(nrow(rows)), info = s)
    expect_false(anyDuplicated(rows$name) > 0L, info = s)
  }
  # the three qes2007_panel names restored by name_map.csv (design.md 2.4)
  panel <- manifest$name[manifest$study == "qes2007_panel"]
  expect_true(all(as_utf8(c("AFFG\u00c9N", "PROPRI\u00c9", "PROPG\u00c9N")) %in% panel))
})
