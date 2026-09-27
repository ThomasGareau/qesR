# Tier 1 (live) for the harmonization engine (design.md sections 5.10, 8.2
# and 11, slice HZ3): V-L1 on the pinned originals. The engine's column
# hashes equal expected/hashes.csv for every study, and its marginals among
# each wave's members equal expected/marginals.csv for the studies whose
# metadata ships (qes2022's are checked in CI by data-raw/build_hashes.R).
# Never runs on CRAN; see helper-live.R.

test_that("V-L1: engine hashes and marginals on the originals equal expected/ (live)", {
  local_live_originals()
  s <- hz_spec()
  h <- qes_harmonize("all", targets = s$tables$targets$target, values = "code", missing = "reasons",
                     include_draft = TRUE, quiet = TRUE)
  # every study the spec covers, every row kept
  cat_ <- .qes_catalog()$files
  cat_ <- cat_[cat_$role == "data" & cat_$is_default %in% TRUE, ]
  for (study in unique(h$study)) {
    expect_identical(sum(h$study == study), cat_$n_rows[cat_$study == study], label = study)
  }
  expect_false(anyDuplicated(h$qes_id) > 0L)
  expect_true(all(qes_provenance(h)$md5_verified))
  # column hashes
  got <- .qes_hz_hashes(h)
  expect_identical(got, s$tables$hashes)
  # marginals among the members of each row's wave
  cell <- qes_provenance(h, level = "cell")
  shipped <- .qes_catalog()$studies
  shipped <- shipped$study[shipped$metadata_shipped %in% TRUE]
  cell <- cell[cell$included & cell$rule %in% c("map", "numeric") & cell$study %in% shipped, ]
  for (k in seq_len(nrow(cell))) {
    in_s <- h$study == cell$study[k]
    member <- vapply(strsplit(ifelse(is.na(h$waves[in_s]), "", h$waves[in_s]), ";", fixed = TRUE),
                     function(w) cell$wave[k] %in% w, logical(1))
    v <- h[[cell$target[k]]][in_s][member]
    v <- if (is.numeric(v)) .qes_code_chr(v) else v
    r <- as.character(h[[paste0(cell$target[k], "__na")]][in_s][member])
    got_n <- table(ifelse(is.na(v), paste0("NA:", r), v))
    e <- s$tables$expected
    e <- e[e$study == cell$study[k] & e$target == cell$target[k] & e$wave == cell$wave[k], ]
    exp_n <- stats::setNames(e$n, ifelse(is.na(e$value), paste0("NA:", e$na_reason), e$value))
    expect_identical(as.integer(got_n[names(exp_n)]), unname(exp_n), label = paste(cell$study[k], cell$target[k]))
    expect_identical(sum(got_n), sum(exp_n), label = paste(cell$study[k], cell$target[k]))
  }
})

test_that("known counts on the originals (live)", {
  local_live_originals()
  h <- qes_harmonize(c("qes2018", "qes2014", "qes2022"), values = "code", missing = "reasons",
                     include_draft = TRUE, quiet = TRUE)
  q18 <- h[h$study == "qes2018", ]
  # 2018 q6: 490 PLQ, 392 PQ, 700 CAQ, 342 QS, 31 spoiled, 92 other, 160 refused
  expect_identical(as.vector(table(q18$vote_prov_recall)[c("PLQ", "PQ", "CAQ", "QS", "other")]),
                   c(490L, 392L, 700L, 342L, 92L))
  expect_identical(sum(q18$vote_prov_recall__na %in% "spoiled"), 31L)
  expect_identical(sum(!is.na(q18$lr_self)), 2490L)
  expect_identical(as.vector(table(q18$interest_4pt)[c("very", "quite", "hardly", "not_at_all")]),
                   c(743L, 1422L, 687L, 160L))
  q14 <- h[h$study == "qes2014", ]
  expect_identical(sum(!is.na(q14$lr_self)), 1195L)
  expect_identical(as.vector(table(q14$pid_prov)[c("PLQ", "PQ", "CAQ", "QS", "ON", "PVQ", "none")]),
                   c(480L, 378L, 192L, 113L, 10L, 29L, 181L))
  q22 <- h[h$study == "qes2022", ]
  expect_identical(sum(q22$turnout_prov_recall %in% "yes"), 1109L)
  expect_identical(sum(q22$turnout_prov_recall__na %in% "not_registered"), 3L)
  expect_identical(sum(q22$turnout_prov_recall__na %in% "dk"), 2L)
  expect_identical(sum(q22$turnout_prov_recall__na %in% "not_in_wave"), 301L)
  expect_identical(sum(q22$vote_prov_intent__na %in% "inapplicable"), 89L)
  expect_equal(mean(q22$lr_self, na.rm = TRUE), 4.96, tolerance = 0.005)
})
