# Data helpers of the example pages (website only; see _theme.R).
#
# Every estimate is made within one study and one wave, with the survey
# package, through qes_design(): the study's reviewed weight where it has
# one, else a unit weight (the study is then drawn hollow and marked
# "unweighted"). Shares get a logit 95% confidence interval, means a Wald
# one; a cell whose denominator has fewer than 30 respondents is not drawn.

# the 11 studies the harmonization specification covers
qz_studies <- c("qes1998", "qes2007", "qes2007_panel", "qes2008", "qes_crop_2007_2010",
                "qes2012", "qes2012_panel", "qes2014", "qes2018", "qes2018_panel", "qes2022")

# One Quebec Election Study per election: the series of the line charts.
# The Durand panels, which interviewed at the same elections, are in the
# table views.
qz_primary <- c(`1998` = "qes1998", `2007` = "qes2007", `2008` = "qes2008", `2012` = "qes2012",
                `2014` = "qes2014", `2018` = "qes2018", `2022` = "qes2022")

qz_study_label <- function(s) {
  lab <- c(qes1998 = tr("1998 polls", "Sondages de 1998"),
           qes2007 = tr("QES 2007", "EEQ 2007"), qes2007_panel = tr("2007 panel", "Panel 2007"),
           qes2008 = tr("QES 2008", "EEQ 2008"), qes_crop_2007_2010 = tr("CROP polls", "Sondages CROP"),
           qes2012 = tr("QES 2012", "EEQ 2012"), qes2012_panel = tr("2012 panel", "Panel 2012"),
           qes2014 = tr("QES 2014", "EEQ 2014"), qes2018 = tr("QES 2018", "EEQ 2018"),
           qes2018_panel = tr("2018 panel", "Panel 2018"), qes2022 = tr("QES 2022", "EEQ 2022"))
  unname(lab[as.character(s)])
}

qz_year <- function(s) {
  st <- qes_studies()
  # the first year of a study that spans several ("2007-2010")
  as.integer(substr(as.character(st$year[match(s, st$study)]), 1, 4))
}

qz_min_n <- 30

# What a page downloads the first time it runs: the number of studies and
# the size of their data files (the byte counts, in the shipped catalog, of
# the data file and label donor get_qes() reads), for the note at the top
# of each example page.
qz_download <- function(studies) {
  st <- as.data.frame(qes_studies())
  st <- st[st$study %in% studies, , drop = FALSE]
  files <- utils::read.csv(system.file("extdata", "catalog", "files.csv", package = "qesR", mustWork = TRUE),
                           colClasses = "character")
  ids <- paste(st$study, c(st$data_file_id, st$label_file_id))
  bytes <- as.numeric(files$bytes[paste(files$study, files$file_id) %in% ids])
  list(n = nrow(st), mb = sum(bytes) / 1e6)
}

# The official results of Elections Quebec (share of valid votes, turnout),
# kept in the qesR source repository (not in the package build). The pages
# are built from the source tree, two folders up.
qz_official_file <- function(name) {
  path <- system.file("extdata", "validation", name, package = "qesR")
  if (!nzchar(path)) path <- file.path("..", "..", "inst", "extdata", "validation", name)
  path
}
qz_official <- function() {
  o <- utils::read.csv(qz_official_file("official_results.csv"), encoding = "UTF-8")
  o$year <- as.integer(sub("^QC", "", o$election_id))
  o$party <- ifelse(o$party %in% c("PVQ", "ON"), "other", o$party)
  o <- stats::aggregate(share_valid ~ year + party, o, sum)
  o[order(o$year, match(o$party, qz_parties)), ]
}
qz_official_turnout <- function() {
  o <- utils::read.csv(qz_official_file("official_turnout.csv"), encoding = "UTF-8")
  o$year <- as.integer(sub("^QC", "", o$election_id))
  o[, c("year", "turnout", "registered")]
}

# ---- designs ---------------------------------------------------------------

# Does the study have a reviewed weight in `x` (the weight column `col`)?
qz_weighted <- function(x, study, col) {
  any(!is.na(x[[col]][x$study == study]))
}

# The weight column of respondent-layout data for pooled variable `var` in
# `study`: the column of the wave its values come from (the wave is the
# second field of `<var>__item`, "qes2022:cps:cps_qc_referendum").
qz_weight_col <- function(x, study, var) {
  item <- x[[paste0(var, "__item")]][x$study == study]
  item <- item[!is.na(item)][1]
  wave <- if (is.na(item)) NA_character_ else strsplit(item, ":", fixed = TRUE)[[1]][2]
  w <- qesR::qes_spec("spec")$tables$waves
  timing <- w$wave_timing[match(paste(study, wave), paste(w$study, w$wave))]
  if (identical(timing, "pre")) "weight_pre" else "weight_post"
}

# A survey design for one study: qes_design() with the study's reviewed
# weight, or with a unit weight where it has no validated weight (or where
# `unweighted` asks for it, to show what the weight changes).
qz_design <- function(x, study, col = "weight_post", unweighted = FALSE) {
  sub <- x[x$study %in% study, , drop = FALSE]
  weighted <- any(!is.na(sub[[col]])) && !unweighted
  if (!weighted) sub[[col]] <- 1
  d <- suppressMessages(qes_design(sub, weight = col))
  attr(d, "qz_weighted") <- weighted
  d
}

# ---- estimates -------------------------------------------------------------

qz_logit_ci <- function(p, se) {
  s <- se / (p * (1 - p))
  lo <- stats::plogis(stats::qlogis(p) - 1.96 * s)
  hi <- stats::plogis(stats::qlogis(p) + 1.96 * s)
  lo[p <= 0 | p >= 1] <- NA
  hi[p <= 0 | p >= 1] <- NA
  cbind(lo = lo, hi = hi)
}

# Shares of the levels of factor `var` among the rows of design `d` with a
# value (the denominator), in each group of `by` (a column; NULL = all),
# in percent, with logit 95% intervals and the unweighted denominator n.
qz_share <- function(d, var, by = NULL, levels = NULL) {
  dat <- d$variables
  groups <- if (is.null(by)) "all" else sort(unique(stats::na.omit(as.character(dat[[by]]))))
  out <- lapply(groups, function(g) {
    keep <- !is.na(dat[[var]])
    if (!is.null(by)) keep <- keep & as.character(dat[[by]]) %in% g
    n <- sum(keep)
    lev <- levels(dat[[var]])
    if (n < 2L) {
      return(data.frame(group = g, level = lev, pct = NA_real_, lo = NA_real_, hi = NA_real_, n = n))
    }
    dg <- subset(d, keep)
    m <- survey::svymean(stats::as.formula(paste0("~", var)), dg, na.rm = TRUE)
    p <- as.numeric(stats::coef(m))
    se <- sqrt(diag(stats::vcov(m)))
    ci <- qz_logit_ci(p, se)
    data.frame(group = g, level = lev, pct = 100 * p, lo = 100 * ci[, "lo"], hi = 100 * ci[, "hi"], n = n)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) {
    out <- data.frame(group = character(), level = character(), pct = numeric(), lo = numeric(),
                      hi = numeric(), n = integer())
  }
  if (!is.null(levels)) out <- out[out$level %in% levels, , drop = FALSE]
  out$weighted <- rep(isTRUE(attr(d, "qz_weighted")), nrow(out))
  rownames(out) <- NULL
  out
}

# Mean of numeric `var`, by group, with a Wald 95% interval.
qz_mean <- function(d, var, by = NULL) {
  dat <- d$variables
  groups <- if (is.null(by)) "all" else sort(unique(stats::na.omit(as.character(dat[[by]]))))
  out <- lapply(groups, function(g) {
    keep <- !is.na(dat[[var]])
    if (!is.null(by)) keep <- keep & as.character(dat[[by]]) %in% g
    n <- sum(keep)
    if (n < 2L) return(data.frame(group = g, mean = NA_real_, lo = NA_real_, hi = NA_real_, n = n))
    m <- survey::svymean(stats::as.formula(paste0("~", var)), subset(d, keep), na.rm = TRUE)
    se <- sqrt(as.numeric(stats::vcov(m)))
    data.frame(group = g, mean = as.numeric(stats::coef(m)), lo = as.numeric(stats::coef(m)) - 1.96 * se,
               hi = as.numeric(stats::coef(m)) + 1.96 * se, n = n)
  })
  out <- do.call(rbind, out)
  if (is.null(out)) out <- data.frame(group = character(), mean = numeric(), lo = numeric(), hi = numeric(), n = integer())
  out$weighted <- rep(isTRUE(attr(d, "qz_weighted")), nrow(out))
  out
}

# The difference, in points, between two groups of `by` (g1 minus g2) in
# the share of rows of `var` equal to `level`, among the rows with a value,
# with a 95% interval from the covariance of the two domain estimates
# (svyby(covmat = TRUE) and svycontrast()). NA when either group has fewer
# than 30 respondents.
qz_gap <- function(d, var, level, by, g1, g2) {
  dat <- d$variables
  g <- as.character(dat[[by]])
  keep <- !is.na(dat[[var]]) & g %in% c(g1, g2)
  n1 <- sum(keep & g %in% g1)
  n2 <- sum(keep & g %in% g2)
  out <- data.frame(gap = NA_real_, lo = NA_real_, hi = NA_real_, n1 = n1, n2 = n2)
  if (min(n1, n2) < qz_min_n) return(out)
  d$variables$.qz_y <- as.numeric(as.character(dat[[var]]) %in% level)
  d$variables$.qz_g <- factor(g, levels = c(g1, g2))
  ds <- subset(d, keep)
  b <- survey::svyby(~.qz_y, ~.qz_g, ds, survey::svymean, covmat = TRUE)
  cf <- stats::setNames(c(1, -1), c(g1, g2))[names(stats::coef(b))]
  ct <- survey::svycontrast(b, cf)
  est <- 100 * as.numeric(stats::coef(ct))
  se <- 100 * sqrt(as.numeric(stats::vcov(ct)))
  out$gap <- est
  out$lo <- est - 1.96 * se
  out$hi <- est + 1.96 * se
  out
}

# Effective number of parties, 1 / sum(p^2), from the shares of the levels
# of `var`, with a delta-method 95% interval.
qz_enp <- function(d, var, keep_rows = NULL) {
  if (!is.null(keep_rows)) d <- subset(d, keep_rows)
  dat <- d$variables
  n <- sum(!is.na(dat[[var]]))
  if (n < qz_min_n) return(data.frame(enp = NA_real_, lo = NA_real_, hi = NA_real_, n = n))
  m <- survey::svymean(stats::as.formula(paste0("~", var)), d, na.rm = TRUE)
  p <- as.numeric(stats::coef(m))
  v <- stats::vcov(m)
  s <- sum(p^2)
  g <- -2 * p / s^2
  se <- sqrt(as.numeric(t(g) %*% v %*% g))
  data.frame(enp = 1 / s, lo = 1 / s - 1.96 * se, hi = 1 / s + 1.96 * se, n = n)
}

qz_enp_official <- function(o) {
  stats::aggregate(share_valid ~ year, o, function(s) 1 / sum((s / sum(s))^2))
}

# a cell that is too small is kept in the table as "n < 30" but not drawn
qz_small <- function(x) x$n < qz_min_n

# the text of the "weighting" column of the table views
qz_weight_text <- function(weighted) {
  ifelse(weighted, tr("weighted", "pondéré"), tr("unweighted: no validated weight", "non pondéré : aucune pondération validée"))
}

# a formatted estimate and interval for the table views
qz_cell <- function(pct, lo, hi, n, digits = 1) {
  ifelse(n < qz_min_n, tr("n < 30", "n < 30"),
         ifelse(is.na(lo), qz_num(pct, digits),
                paste0(qz_num(pct, digits), " [", qz_num(lo, digits), tr(", ", " ; "), qz_num(hi, digits), "]")))
}

# Was a party a possible answer in a study? It must have run in that
# election (official results) and be listed by the study's question (the
# harmonization spec's levels_offered for `target`). Other cells are
# structural zeros: never drawn, "—" (did not run) or "n.l." (not listed)
# in the tables. PVQ, ON and "Other party" fold into "other", always shown.
qz_party_status <- function(study, party, target = "vote_prov_recall") {
  xw <- qesR::qes_spec("crosswalk", targets = target)
  offered <- stats::setNames(strsplit(xw$levels_offered, ";", fixed = TRUE), xw$study)
  o <- qz_official()
  year <- qz_year(study)
  ran <- mapply(function(s, p, y) p == "other" || p %in% o$party[o$year == y], study, party, year)
  listed <- mapply(function(s, p) p == "other" || p %in% offered[[s]], study, party)
  unname(ifelse(!ran, "not_run", ifelse(!listed, "not_listed", "offered")))
}

# the first non-missing value of `col` in each study (a study's grade, type
# or source item of a pooled variable)
qz_first <- function(x, col, study) {
  vapply(study, function(s) {
    v <- x[[col]][x$study == s]
    v <- as.character(v[!is.na(v)])
    if (length(v)) v[1] else ""
  }, character(1), USE.NAMES = FALSE)
}

qz_grade_text <- function(g) {
  lab <- c(identical = tr("identical", "identique"), comparable = tr("comparable", "comparable"),
           approximate = tr("approximate", "approximatif"))
  out <- unname(lab[g])
  out[is.na(out)] <- ""
  out
}

# add constant columns (study, group, ...) in front of an estimate table,
# also when it has no rows
qz_tag <- function(x, ...) {
  v <- list(...)
  for (n in names(v)) x[[n]] <- rep(v[[n]], nrow(x))
  x[c(names(v), setdiff(names(x), names(v)))]
}
