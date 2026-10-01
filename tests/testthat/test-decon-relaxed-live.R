# Tier 1 (live) for the relaxed layer and qes_decon() (design.md section
# 5.14, V-R9 and V-R11): on the pinned originals, every relaxed row passes
# the data checks, the columns equal expected/relaxed_marginals.csv and
# expected/relaxed_hashes.csv (every relaxed row applied, as
# data-raw/build_relaxed.R --expected records them), the counts of the
# design record hold, and the income thirds of qes2022 are the terciles of
# the file. Never runs on CRAN; see helper-live.R.

rx_live <- function() .qes_decon_compute(NULL, lang = "en", quiet = TRUE, include_review = TRUE)

test_that("V-R11: qes_decon() on the originals equals the recorded results (live)", {
  local_live_originals()
  s <- hz_spec()
  res <- rx_live()
  lead <- res$lead
  rx <- res$rx
  ex <- s$tables$rx_expected
  hs <- s$tables$rx_hashes
  for (col in rx$column) {
    static <- rx$timing[rx$column == col] == "static"
    for (st in unique(lead$study)) {
      k <- which(lead$study == st)
      if (static) k <- k[!duplicated(lead$qes_id[k])]
      v <- res$value[[col]][k]
      r <- res$reason[[col]][k]
      cell <- ifelse(is.na(v), paste0("NA:", r), v)
      h <- hs[hs$column == col & hs$study == st, ]
      expect_identical(.qes_md5_text(paste(cell, collapse = "\n")), h$md5, label = paste(col, st))
      e <- ex[ex$column == col & ex$study == st, ]
      w <- if (static) rep("*", length(k)) else ifelse(is.na(lead$wave[k]), "NA", lead$wave[k])
      got <- table(paste(w, cell))
      want <- stats::setNames(e$n, paste(e$wave, ifelse(is.na(e$value), paste0("NA:", e$na_reason), e$value)))
      expect_identical(as.integer(got[names(want)]), unname(want), label = paste(col, st))
      expect_identical(sum(got), sum(want), label = paste(col, st))
    }
  }
})

test_that("the counts of the design record hold on the originals (live)", {
  local_live_originals()
  res <- rx_live()
  lead <- res$lead
  first <- !duplicated(lead$qes_id)
  count <- function(col, study) {
    k <- lead$study == study & first
    as.vector(table(factor(res$value[[col]][k], levels = .qes_spec_levels(hz_spec()$tables$levels,
                                                                           res$rx$levels_id[res$rx$column == col])$name)))
  }
  expect_identical(count("education", "qes2007"), c(195L, 355L, 633L, 975L))
  expect_identical(count("education", "qes2018"), c(322L, 457L, 1089L, 1181L))
  expect_identical(count("education", "qes2022"), c(63L, 219L, 493L, 746L))
  expect_identical(count("income_cat", "qes2022"), c(521L, 493L, 497L))
  expect_identical(count("income_cat", "qes2012"), c(391L, 555L, 411L))
  expect_identical(count("religion", "qes2018"), c(941L, 108L, 59L, 115L, 1737L))
  expect_identical(count("marital", "qes2018"), c(1614L, 265L, 127L, 1037L))
  expect_identical(count("employment", "qes_crop_2007_2010"), c(14836L, 702L, 5795L, 1160L, 1475L))
  expect_identical(count("language", "qes2022"), c(1352L, 121L, 48L))
  expect_identical(count("language", "qes2014"), c(1224L, 226L, 67L))
  expect_identical(count("region", "qes2007_panel"), c(1251L, 213L, 977L))
  k <- lead$study == "qes_crop_2007_2010"
  expect_identical(as.vector(table(factor(res$value$sovereignty[k], levels = c("yes", "no")))), c(8737L, 13617L))
  k <- lead$study == "qes1998" & lead$wave %in% "pre"
  expect_identical(as.vector(table(factor(res$value$gov_satisfaction[k], levels = c("very", "fairly", "not_very", "not_at_all")))),
                   c(134L, 653L, 387L, 170L))
})

test_that("the income breaks of qes2022 are the terciles of the amounts given (live)", {
  local_live_originals()
  d <- get_qes("qes2022", assign_global = FALSE, quiet = TRUE)
  a <- as.numeric(unclass(d$cps_income))
  a <- a[!is.na(a) & a > 0]
  rm <- .qes_rx_tables(hz_spec())$maps
  args <- .qes_hz_amount_args(rm$args[rm$study == "qes2022" & rm$column == "income_cat"])
  expect_identical(length(a), 1444L)
  expect_equal(unname(stats::quantile(a, c(1, 2) / 3)), args$breaks)
})

test_that("qes_decon() runs on every study by default, and the lineage on it (live)", {
  local_live_originals()
  d <- qes_decon(quiet = TRUE)
  expect_identical(sort(unique(d$study)), sort(.qes_hz_covered(hz_spec())))
  # the weights have mean 1 in each study and wave that has one
  w <- attr(d, "weights")
  ok <- is.na(w$reason)
  expect_true(all(abs(w$mean[ok] - 1) < 1e-8))
  expect_true(all(w$reason[w$study == "qes2008"] %in% "no_recommended_weight"))
  expect_true(all(is.na(d$weight[d$study == "qes2008"])))
  l <- qes_party_lineage(d)
  expect_true("vote_choice_lineage" %in% names(l))
})
