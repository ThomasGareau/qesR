# Synthetic study data built from the spec (design.md section 8.1, slice
# HZ2): the fixture of the offline end-to-end tests.
#
# .qes_synthetic() gives, for each study, a data frame shaped like what
# get_qes() returns (the real variable names, labelled numeric columns, the
# study's identifier columns) whose rows are no one: every code of every
# crosswalk row appears, with every gate outcome and wave membership, and the
# data passes the data checks V-D1 to V-D5, V-D7 and V-D8 of its spec:
#   * the number of members of each wave is its n_cases: waves without a
#     membership rule hold every row; the waves with one hold consecutive
#     blocks shifted by one row each, so every such wave has members and
#     non-members;
#   * a gated source is missing exactly where its gate is closed, and cycles
#     through its codes where the gate is open;
#   * value labels are those the value map quotes from the file (origins
#     "file" and "label_donor"); a study whose file has no labels (qes2018)
#     or whose labels cannot ship (qes2022, hashes only) gets none, and
#     from_label rows get labels made up from their range (numeric rows) or
#     "Category 1" and "Category 2" (string rows, the text of the value);
#   * weights are 0.5 and 1.5 in turn on the wave's members, the last one 1
#     when their number is odd, so the mean is exactly 1.
# Nothing comes from respondent data; counts are the spec's n_cases only.

.qes_synthetic <- function(studies, spec = NULL) {
  spec <- if (inherits(spec, "qes_spec")) spec else .qes_spec_get(spec, "none")
  # every variable of a coalesce row gets the codes of its own map
  spec <- .qes_hz_expand_coalesce(spec)
  out <- lapply(studies, .qes_synthetic_study, spec = spec)
  names(out) <- studies
  out
}

# A code that is not in `codes` (text): "1" if free, else one more than the
# largest number, else "X".
.qes_synth_other <- function(codes) {
  codes <- codes[codes != "NA"]
  if (!"1" %in% codes) {
    return("1")
  }
  num <- suppressWarnings(as.numeric(codes))
  if (all(!is.na(num))) .qes_code_chr(max(num) + 1) else "X"
}

.qes_synthetic_study <- function(study, spec) {
  xw_all <- spec$tables$crosswalk
  xw_rows <- which(xw_all$study == study)
  xw <- xw_all[xw_rows, , drop = FALSE]
  wv <- spec$tables$waves
  wv <- wv[wv$study == study, , drop = FALSE]
  wt <- spec$tables$weights
  wt <- wt[wt$study == study, , drop = FALSE]
  vm <- spec$tables$valuemaps
  if (nrow(wv) == 0L) {
    stop(sprintf("qesR internal error: no waves for study '%s' in the spec.", study), call. = FALSE)
  }

  # rows and wave membership
  has_rule <- !is.na(wv$member_var) & nzchar(wv$member_var)
  n_all <- unique(wv$n_cases[!has_rule])
  if (length(n_all) > 1L) {
    stop(sprintf("qesR internal error: waves of '%s' without a membership rule differ in n_cases.", study), call. = FALSE)
  }
  ruled <- which(has_rule)
  # the poll waves of pooled polls are disjoint: consecutive blocks of
  # their sizes; other ruled waves overlap, each shifted by one row
  polls <- .qes_poll_study(wv, study)
  shift <- if (polls) c(0L, cumsum(wv$n_cases[ruled])[-length(ruled)]) else seq_along(ruled) - 1L
  N <- max(c(n_all, shift + wv$n_cases[ruled], 1L))
  members <- lapply(seq_len(nrow(wv)), function(w) {
    if (!has_rule[w]) return(rep(TRUE, N))
    k <- match(w, ruled)
    seq_len(N) %in% (shift[k] + seq_len(wv$n_cases[w]))
  })
  names(members) <- wv$wave
  # wave "*" names every poll wave
  members[[.qes_all_waves]] <- Reduce(`|`, members, rep(FALSE, N))
  d <- list()
  labels <- list()

  # identifiers (".row" is the row position)
  files <- .qes_catalog()$files
  f <- files[files$study == study & files$role == "data" & files$is_default %in% TRUE, , drop = FALSE]
  ids <- setdiff(.qes_split_list(if (nrow(f) > 0L) f$id_vars[1] else NA_character_), ".row")
  # an identifier that is also a crosswalk source (qes2018_panel method, the
  # interview mode) is filled with the source's codes below; the first other
  # identifier makes the key unique
  ids <- setdiff(ids, xw$source_var)
  for (k in seq_along(ids)) {
    d[[ids[k]]] <- if (k == 1L) as.numeric(seq_len(N)) else rep(1, N)
  }

  # dates of the waves (also membership when the rule is "!NA" on a date)
  as_date_value <- function(w, fmt) {
    day <- wv$fieldwork_start[w] %|NA|% wv$fieldwork_end[w] %|NA|% as.Date("2000-01-01")
    switch(fmt,
      yyyymmdd = as.numeric(format(day, "%Y%m%d")),
      posixct = as.POSIXct(format(day), tz = "UTC"),
      stata = as.numeric(day - as.Date("1960-01-01")),
      as.numeric(format(day, "%Y%m%d"))
    )
  }
  for (w in seq_len(nrow(wv))) {
    v <- wv$date_var[w]
    if (is.na(v)) next
    val <- as_date_value(w, wv$date_format[w])
    x <- d[[v]] %||% rep(val, N)[rep(NA_integer_, N)]
    x[members[[w]]] <- val
    d[[v]] <- x
  }
  # membership variables
  for (w in ruled) {
    v <- wv$member_var[w]
    codes <- .qes_split_list(wv$member_codes[w])
    if (identical(codes, "!NA")) {
      if (is.null(d[[v]])) {
        x <- rep(NA_real_, N)
        x[members[[w]]] <- 1
        d[[v]] <- x
      }
      next
    }
    num <- suppressWarnings(as.numeric(codes))
    # a code no wave on this variable uses marks the rows in none of them
    all_codes <- unique(unlist(lapply(wv$member_codes[ruled][wv$member_var[ruled] == v], .qes_split_list)))
    other <- .qes_synth_other(all_codes)
    x <- d[[v]] %||% rep(other, N)
    x[members[[w]]] <- codes[1]
    d[[v]] <- x
    if (all(!is.na(num)) && !is.na(suppressWarnings(as.numeric(other)))) {
      d[[v]] <- as.numeric(d[[v]])
    }
  }

  # the codes of each source variable, its gate, its members and its labels
  rows <- which(!is.na(xw$source_var))
  info <- list()
  # a select-all item read by fn:multiselect: each option's variable is
  # ticked (1) or not (-99) in turn
  for (i in rows[xw$rule[rows] %in% "fn:multiselect"]) {
    a <- .qes_hz_multiselect_args(xw$args[i])
    for (k in seq_along(a$var)) {
      info[[a$var[k]]] <- list(codes = if (k %% 2L == 1L) c(a$selected, "-99") else c("-99", a$selected),
                               na_token = FALSE, gate = NA_character_, closed = character(0),
                               mask = members[[xw$wave[i]]], labels = NULL)
    }
  }
  rows <- rows[!xw$rule[rows] %in% "fn:multiselect"]
  for (i in rows) {
    v <- xw$source_var[i]
    it <- info[[v]] %||% list(codes = character(0), na_token = FALSE, gate = NA_character_,
                              closed = character(0), mask = rep(FALSE, N), labels = NULL)
    it$mask <- it$mask | members[[xw$wave[i]]]
    nac <- .qes_parse_kv(xw$na_codes[i]) %||% character(0)
    it$na_token <- it$na_token || "NA" %in% names(nac)
    codes <- setdiff(names(nac), "NA")
    if (xw$rule[i] %in% "map") {
      m <- vm[vm$map_id %in% xw$map_id[i], , drop = FALSE]
      codes <- c(codes, m$source_code)
      quoted <- !is.na(m$source_label) & m$source_label_origin %in% c("file", "label_donor")
      if (any(quoted)) {
        it$labels <- c(it$labels, stats::setNames(m$source_code[quoted], m$source_label[quoted]))
      }
    } else if (xw$rule[i] %in% "numeric") {
      r <- .qes_hz_row_rule(spec, xw_rows[i])
      lim <- c(r$min, (r$min + r$max) / 2, r$max)
      lim <- lim[is.finite(lim)]
      if (length(lim) == 0L) lim <- c(0, 1)
      if (r$from_label) {
        lab <- .qes_code_chr(lim)
        num_codes <- .qes_code_chr(seq_along(lim))
        codes <- c(codes, num_codes)
        it$labels <- c(it$labels, stats::setNames(num_codes, lab))
      } else {
        # whole codes whose image under the affine transform stays in the
        # range: the inverse images of the limits, the outer ones rounded
        # toward the inside, then only codes that map into [min, max]
        a <- r$affine[["a"]]
        b <- r$affine[["b"]]
        x <- sort((lim - b) / a)
        n_x <- length(x)
        x <- unique(c(ceiling(x[1] - 1e-9), round(x[-c(1L, n_x)]), floor(x[n_x] + 1e-9)))
        y <- a * x + b
        x <- x[y >= r$min - 1e-9 & y <= r$max + 1e-9]
        codes <- c(codes, .qes_code_chr(x))
      }
    } else if (xw$rule[i] %in% "string") {
      # open text: two made-up codes, labelled "Category 1" and "Category 2"
      # for from_label rows (the value is the label)
      args <- .qes_parse_kv(xw$args[i]) %||% character(0)
      num_codes <- c("1", "2")
      codes <- c(codes, num_codes)
      if (identical(unname(args["from_label"]), "TRUE")) {
        it$labels <- c(it$labels, stats::setNames(num_codes, paste("Category", num_codes)))
      }
    }
    it$codes <- unique(c(it$codes, codes))
    if (!is.na(xw$gate_var[i]) && is.na(it$gate)) {
      it$gate <- xw$gate_var[i]
      it$closed <- .qes_split_list(xw$gate_codes[i])
    }
    info[[v]] <- it
  }
  # a gate variable that is no row's source: the closed codes of every row
  # it filters (qes2018 agensp closes birth_year on 1 and age on 0) and one
  # code open for all of them
  gated <- names(info)[vapply(names(info), function(v) !is.na(info[[v]]$gate), logical(1))]
  for (g in unique(vapply(gated, function(v) info[[v]]$gate, character(1)))) {
    if (!is.null(info[[g]])) next
    users <- gated[vapply(gated, function(v) identical(info[[v]]$gate, g), logical(1))]
    closed <- unique(unlist(lapply(users, function(v) info[[v]]$closed)))
    mask <- Reduce(`|`, lapply(users, function(v) info[[v]]$mask))
    info[[g]] <- list(codes = c(setdiff(closed, "NA"), .qes_synth_other(closed)),
                      na_token = "NA" %in% closed, gate = NA_character_, closed = character(0),
                      mask = mask, labels = NULL)
  }

  # fill the variables, gates first
  done <- names(d)
  todo <- setdiff(names(info), done)
  while (length(todo) > 0L) {
    ready <- todo[vapply(todo, function(v) is.na(info[[v]]$gate) || info[[v]]$gate %in% done, logical(1))]
    if (length(ready) == 0L) {
      stop(sprintf("qesR internal error: the gates of '%s' form a cycle.", study), call. = FALSE)
    }
    for (v in ready) {
      it <- info[[v]]
      codes <- it$codes
      text <- any(is.na(suppressWarnings(as.numeric(codes))))
      x <- rep(NA_character_, N)
      mask <- it$mask
      if (!is.na(it$gate)) {
        gv <- .canon(d[[it$gate]])
        gv[is.na(gv)] <- "NA"
        open <- mask & !(gv %in% it$closed)
        x[open] <- rep_len(codes, sum(open))
      } else {
        pool <- c(codes, if (it$na_token) "NA")
        x[mask] <- rep_len(pool, sum(mask))
        x[x %in% "NA"] <- NA_character_
      }
      if (!text) {
        x <- as.numeric(x)
        if (length(it$labels) > 0L) {
          lab <- it$labels[!duplicated(it$labels)]
          x <- haven::labelled(x, labels = stats::setNames(as.numeric(unname(lab)), names(lab)))
        }
      }
      d[[v]] <- x
    }
    done <- c(done, ready)
    todo <- setdiff(todo, ready)
  }
  # documentation-only rows (rule none) name variables that must exist
  for (v in setdiff(xw$source_var[xw$rule %in% "none"], names(d))) {
    d[[v]] <- rep(NA_real_, N)
  }
  # weights, subsample and strata variables, mode variables
  for (k in seq_len(nrow(wt))) {
    v <- wt$weight_var[k]
    x <- d[[v]] %||% rep(NA_real_, N)
    # a weight of wave "*" has mean 1 in each poll wave
    waves <- if (identical(wt$wave[k], .qes_all_waves)) wv$wave else wt$wave[k]
    for (w in waves) {
      mem <- members[[w]]
      x[mem] <- rep_len(c(0.5, 1.5), sum(mem))
      if (sum(mem) %% 2L == 1L) x[max(which(mem))] <- 1
    }
    d[[v]] <- x
  }
  for (v in unique(stats::na.omit(c(wv$subsample_var, wv$strata_var)))) {
    if (is.null(d[[v]])) d[[v]] <- rep("A", N)
  }
  for (m in unique(wv$mode[!is.na(wv$mode) & startsWith(wv$mode, "var:")])) {
    v <- sub("^var:", "", m)
    if (is.null(d[[v]])) d[[v]] <- rep_len(c(1, 2, 3), N)
  }

  out <- structure(d, class = "data.frame", row.names = c(NA_integer_, -N))
  attr(out, "qes_survey_code") <- study
  attr(out, "qes_synthetic") <- TRUE
  out
}
