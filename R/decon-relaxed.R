# qes_decon(): relaxed harmonization across studies (design.md section 5.14,
# spec 4.4.0; the spec tables are read in R/hz-relaxed.R).
#
# One flat data frame for the harmonizable studies, one row per respondent
# and wave, with plain cesR-style column names: one concept per column even
# when the questions differ, in coarse common categories. Each column is
# built, in the order of relaxed.csv, from
#   1. its base: a strict target or a pooled variable of qes_harmonize(),
#      run once in the long layout (values = "code", missing = "reasons",
#      signed-off rows only), or a relaxed column placed earlier;
#   2. the transform of the base's values (identity, recode, bands, affine);
#   3. the relaxed rows of the column (relaxed_maps.csv), applied by the
#      engine's own row function (.qes_hz_apply_row()) to each study's file:
#      a row with override = TRUE replaces the base for its study (a static
#      column) or its study and wave (a wave column); with override = FALSE
#      it fills a study the base does not cover. Only signed-off rows
#      (status stable) are applied: the cells of a row still in review are
#      NA, reason not_reviewed (a row that would replace the base leaves the
#      base in place);
#   4. for a static column, the value is then repeated on every row of the
#      respondent (the first wave with a value wins).
# Every value has a source, recorded in attr(, "decon_sources"); relaxed
# columns carry no grade.

# The messages of qes_harmonize() that qes_decon() replaces with its own.
.qes_decon_muffled <- c(
  "qesR_message_approximate_cells", "qesR_message_structural_zeros", "qesR_message_pooled",
  "qesR_message_unreviewed_cells", "qesR_message_unreviewed_skipped", "qesR_message_weight_review",
  "qesR_message_weight_timing"
)

# Words of the recode texts (ASCII; French with \u escapes).
.qes_rx_words <- list(
  en = c(then = "then", scored = "scored", times = "times", plus = "plus", under = "under",
         to_under = "to under", or_more = "or more", every = "every respondent", first = "the first option ticked, in this order:",
         only = "the one level of the options ticked:", derived = "derived from", age = "the age at the start of fieldwork",
         numbers = "numbers from %s to %s", pass = "pass to", gate = "where", other = "else", amount = "amount",
         from_label = "read from the value labels"),
  fr = c(then = "puis", scored = "not\u00e9", times = "fois", plus = "plus", under = "moins de",
         to_under = "\u00e0 moins de", or_more = "ou plus", every = "chaque personne",
         first = "la premi\u00e8re option coch\u00e9e, dans cet ordre\u00a0:",
         only = "le seul niveau des options coch\u00e9es\u00a0:", derived = "d\u00e9riv\u00e9 de",
         age = "l'\u00e2ge au d\u00e9but du terrain", numbers = "nombres de %s \u00e0 %s", pass = "passent \u00e0",
         gate = "si", other = "sinon", amount = "montant", from_label = "lus dans les \u00e9tiquettes de valeur")
)

# ---- recode texts -------------------------------------------------------------------

# Codes as compact text: consecutive whole numbers as ranges ("1-4, 6").
.qes_rx_codes_text <- function(codes) {
  num <- suppressWarnings(as.numeric(codes))
  if (anyNA(num) || any(num != round(num))) return(paste(codes, collapse = ", "))
  num <- sort(unique(num))
  runs <- split(num, cumsum(c(1, diff(num) != 1)))
  paste(vapply(runs, function(r) {
    if (length(r) >= 3L) paste0(.qes_code_chr(r[1]), "-", .qes_code_chr(r[length(r)]))
    else paste(.qes_code_chr(r), collapse = ", ")
  }, character(1)), collapse = ", ")
}

# The label of NA reason `r` in `lang`, marked as missing.
.qes_rx_reason_text <- function(r, lang) {
  if (length(r) == 0L) return(character(0))
  e <- .qes_enum("missing_type")
  lab <- e[[paste0("label_", lang)]][match(r, e$value)]
  lab[is.na(lab)] <- r[is.na(lab)]
  paste0(lab, " (NA)")
}

# The label of level names `x` of level set `set` in `lang`.
.qes_rx_level_text <- function(x, set, lang) {
  if (is.null(set)) return(x)
  lab <- set[[paste0("label_", lang)]][match(x, set$name)]
  ifelse(is.na(lab), x, lab)
}

# Text of value map `map_id` (and na_codes `nac`) into level set `set`:
# "1-4 = No high school diploma; 5 = High school diploma; 99 = Refused (NA)".
.qes_rx_map_text <- function(vm, map_id, set, nac, lang) {
  m <- vm[vm$map_id %in% map_id, , drop = FALSE]
  code <- c(m$source_code, names(nac))
  out <- c(
    ifelse(is.na(m$target_code),
           .qes_rx_reason_text(m$na_reason, lang),
           .qes_rx_level_text(set$name[match(m$target_code, set$code)] %||% as.character(m$target_code), set, lang)),
    .qes_rx_reason_text(unname(nac), lang)
  )
  if (length(code) == 0L) return("")
  groups <- unique(out)
  first <- vapply(groups, function(g) {
    x <- suppressWarnings(as.numeric(code[out == g]))
    if (all(is.na(x))) Inf else min(x, na.rm = TRUE)
  }, numeric(1))
  groups <- groups[order(is.na(match(groups, out[!grepl("\\(NA\\)$", out)])), first)]
  paste(vapply(groups, function(g) paste0(.qes_rx_codes_text(code[out == g]), " = ", g), character(1)),
        collapse = "; ")
}

# Text of crosswalk-like row `i` of `xw` (a strict crosswalk row, or a
# relaxed row of the pseudo-spec): how its codes become levels or reasons.
.qes_rx_row_text <- function(spec, xw, i, set, lang) {
  w <- .qes_rx_words[[lang]]
  vm <- spec$tables$valuemaps
  nac <- .qes_parse_kv(xw$na_codes[i]) %||% character(0)
  rule <- xw$rule[i]
  args <- .qes_parse_kv(xw$args[i]) %||% character(0)
  txt <- switch(
    if (startsWith(rule %|NA|% "", "fn:")) rule else rule %|NA|% "",
    map = .qes_rx_map_text(vm, xw$map_id[i], set, nac, lang),
    coalesce = {
      then <- .qes_hz_coalesce_then(xw, i)
      ft <- .qes_hz_coalesce_fallthrough(xw, i)
      parts <- .qes_rx_map_text(vm, xw$map_id[i], set, nac, lang)
      for (j in seq_len(nrow(then))) {
        parts <- c(parts, sprintf("%s %s %s: %s", paste(.qes_rx_reason_text(ft, lang), collapse = ", "), w[["pass"]],
                                  then$var[j], .qes_rx_map_text(vm, then$map_id[j], set, character(0), lang)))
      }
      paste(parts, collapse = "; ")
    },
    numeric = {
      lo <- unname(args["min"]) %|NA|% ""
      hi <- unname(args["max"]) %|NA|% ""
      t0 <- if (nzchar(lo) && nzchar(hi)) sprintf(w[["numbers"]], lo, hi) else ""
      if (identical(unname(args["from_label"]), "TRUE")) t0 <- paste(t0, w[["from_label"]])
      if ("affine" %in% names(args)) t0 <- paste0(t0, ", ", args[["affine"]])
      paste(c(t0, if (length(nac) > 0L) .qes_rx_map_text(vm, NA, set, nac, lang)), collapse = "; ")
    },
    constant = paste0(w[["every"]], " = ", .qes_rx_level_text(unname(args["value"]), set, lang)),
    `fn:multiselect` = {
      a <- .qes_hz_multiselect_args(xw$args[i])
      paste(if (isTRUE(a$first)) w[["first"]] else w[["only"]],
            paste(sprintf("%s (%s)", a$var, .qes_rx_level_text(a$level, set, lang)), collapse = ", "))
    },
    `fn:amount_bands` = {
      a <- .qes_hz_amount_args(xw$args[i])
      b <- .qes_code_chr(a$breaks)
      lv <- .qes_rx_level_text(a$levels, set, lang)
      bands <- c(sprintf("%s %s %s = %s", w[["amount"]], w[["under"]], b[1], lv[1]))
      for (k in seq_along(b)[-1]) bands <- c(bands, sprintf("%s %s %s = %s", b[k - 1L], w[["to_under"]], b[k], lv[k]))
      bands <- c(bands, sprintf("%s %s = %s", b[length(b)], w[["or_more"]], lv[length(lv)]))
      paste(c(bands, sprintf("%s %s %s: %s", .qes_rx_codes_text(names(nac)), w[["pass"]], a$then_var,
                             .qes_rx_map_text(vm, a$then_map, set, character(0), lang))), collapse = "; ")
    },
    rule %|NA|% ""
  )
  if (!is.na(xw$gate_var[i]) && nzchar(xw$gate_var[i])) {
    to <- .qes_parse_kv(xw$gate_to[i]) %||% character(0)
    is_level <- !is.null(set) & unname(to) %in% (set$name %||% character(0))
    lab <- ifelse(is_level, .qes_rx_level_text(unname(to), set, lang), .qes_rx_reason_text(unname(to), lang))
    g <- unique(lab)
    gtxt <- vapply(g, function(x) paste0(.qes_rx_codes_text(names(to)[lab == x]), " = ", x), character(1))
    txt <- sprintf("%s %s: %s; %s %s: %s", w[["gate"]], xw$gate_var[i], paste(gtxt, collapse = "; "),
                   w[["other"]], xw$source_var[i], txt)
  }
  txt
}

# Text of a transform (of a pooled member or of a relaxed column) from base
# levels `from_set` to levels `to_set`, prefixed with "then".
.qes_rx_transform_text <- function(text, from_set, to_set, lang) {
  w <- .qes_rx_words[[lang]]
  if (is.null(text) || is.na(text) || text %in% c("identity", "relaxed_only")) return(NA_character_)
  tr <- .qes_rx_transform(text)
  ptr <- .qes_pool_transform(text)
  body <- if (!is.null(tr) && identical(tr$kind, "recode")) {
    to <- unname(tr$map)
    to_lab <- ifelse(startsWith(to, "NA:"), .qes_rx_reason_text(sub("^NA:", "", to), lang), .qes_rx_level_text(to, to_set, lang))
    paste(paste0(.qes_rx_level_text(names(tr$map), from_set, lang), " = ", to_lab), collapse = "; ")
  } else if (!is.null(tr) && identical(tr$kind, "bands")) {
    lv <- .qes_rx_level_text(tr$levels, to_set, lang)
    cuts <- .qes_code_chr(tr$cuts)
    parts <- sprintf("%s %s = %s", w[["under"]], cuts[1], lv[1])
    for (k in seq_along(cuts)[-1]) parts <- c(parts, sprintf("%s %s %s = %s", cuts[k - 1L], w[["to_under"]], cuts[k], lv[k]))
    paste(c(parts, sprintf("%s %s = %s", cuts[length(cuts)], w[["or_more"]], lv[length(lv)])), collapse = "; ")
  } else if (!is.null(ptr) && identical(ptr$kind, "score")) {
    paste(w[["scored"]], paste(paste0(.qes_rx_level_text(names(ptr$map), from_set, lang), " = ", ptr$map), collapse = ", "))
  } else if (!is.null(tr) && identical(tr$kind, "affine")) {
    a <- tr$affine
    paste0("x ", w[["times"]], " ", .qes_code_chr(a[["a"]]), if (a[["b"]] != 0) paste0(" ", w[["plus"]], " ", .qes_code_chr(a[["b"]])) else "")
  } else {
    text
  }
  paste0(w[["then"]], " ", body)
}

# ---- arguments ------------------------------------------------------------------------

# The studies of qes_decon(): NULL (or "all") is every harmonizable study,
# in the order of waves.csv; the 1998 firms' own files are an error that
# points to qes1998.
.qes_decon_studies <- function(studies, sp, data) {
  covered <- .qes_hz_covered(sp)
  order_ <- unique(sp$tables$waves$study)
  all_ <- c(intersect(order_, covered), setdiff(covered, order_))
  if (is.null(studies)) return(if (is.null(data)) all_ else names(data))
  if (is.character(studies) && length(studies) == 1L && identical(.qes_canon_code(studies), "all")) return(all_)
  codes <- .qes_resolve_codes(studies, "studies", demo = TRUE)
  firms <- intersect(codes, c("qes1998_crop", "qes1998_createc"))
  if (length(firms) > 0L) {
    .qes_abort("input_decon_firm", class = "qesR_error_input", args = list(.qes_q(firms)),
               data = list(arg = "studies", value = firms))
  }
  uncovered <- codes[!vapply(codes, .qes_hz_spec_study, character(1)) %in% covered]
  if (length(uncovered) > 0L) {
    .qes_abort("input_harmonize_study", class = "qesR_error_input",
               args = list(.qes_q(uncovered), .qes_q(covered)), data = list(arg = "studies", value = uncovered))
  }
  codes
}

# ---- the columns ------------------------------------------------------------------------

# Build the relaxed columns: list(lead, rx, value, reason, key, sources,
# weights, provenance, spec). `value`, `reason` and `key` are lists by
# column over the rows of `lead` (level names or numbers as text, NA
# reasons, and the row of `sources` each value came from).
# `include_review = TRUE` also applies the relaxed rows in review (the
# recorded results of data-raw/build_relaxed.R and the live tests).
.qes_decon_compute <- function(studies = NULL, lang = "en", quiet = FALSE, data = NULL, spec = NULL,
                               include_review = FALSE) {
  sp <- .qes_spec_get(spec, "error")
  if (!is.null(data)) data <- .qes_spec_data_arg(data, demo = TRUE)
  codes <- .qes_decon_studies(studies, sp, data)
  rx <- .qes_rx_columns(sp)
  rm <- .qes_rx_tables(sp)$maps
  tg <- sp$tables$targets
  pl <- .qes_pool_tables(sp)$pooled
  bases <- lapply(rx$base, .qes_rx_base)
  b_kind <- vapply(bases, `[[`, "", "kind")
  b_name <- vapply(bases, function(b) b$name %|NA|% NA_character_, "")
  targets <- unique(b_name[b_kind == "target"])
  pools <- unique(b_name[b_kind %in% c("pooled", "type")])
  applied_status <- c("stable", if (isTRUE(include_review)) c("review", "draft"))

  h <- withCallingHandlers(
    qes_harmonize(studies = codes, targets = c(targets, pools), layout = "long", values = "code",
                  missing = "reasons", min_grade = "approximate", weights = "normalized",
                  include_draft = FALSE, data = data, spec = spec, lang = lang, quiet = quiet),
    message = function(m) if (inherits(m, .qes_decon_muffled)) invokeRestart("muffleMessage")
  )
  prov <- attr(h, "qes_provenance", exact = TRUE)
  cell <- attr(prov, "cell", exact = TRUE)
  pprov <- attr(prov, "pooled", exact = TRUE)
  n <- nrow(h)
  lead <- data.frame(study = h$study, year = h$year, wave = h$wave, qes_id = h$qes_id,
                     weight = h$weight, weight_var = h$weight_var, stringsAsFactors = FALSE)

  # each study's frame (read once more through the session memo, or given)
  frames <- list()
  for (s in codes) {
    frames[[s]] <- if (!is.null(data)) data[[s]] else (.qes_hz_preread$frames[[s]] %||% .qes_read(s, quiet = TRUE))
  }

  # the relaxed rows as a spec, and their results per study
  ps <- .qes_rx_pseudo_spec(sp)
  rx_res <- list()
  for (s in codes) {
    spec_s <- .qes_hz_spec_study(s)
    d <- frames[[s]]
    k <- which(rm$study == spec_s & rm$column %in% rx$column)
    if (length(k) == 0L) next
    wv <- sp$tables$waves[sp$tables$waves$study == spec_s, , drop = FALSE]
    wv <- wv[order(wv$wave_order), , drop = FALSE]
    members <- .qes_hz_member_matrix(wv, d)
    ok <- vapply(k, function(i) all(.qes_hz_row_vars(ps$tables$crosswalk, i) %in% names(d)) &&
                   (is.na(rm$gate_var[i]) || rm$gate_var[i] %in% names(d)), logical(1))
    run <- k[ok & rm$status[k] %in% applied_status]
    if (length(run) > 0L) {
      sub <- ps
      sub$tables$crosswalk <- ps$tables$crosswalk[run, , drop = FALSE]
      sub$tables$crosswalk$study <- s
      sub$tables$waves <- transform(wv, study = s)
      rownames(sub$tables$crosswalk) <- NULL
      dl <- stats::setNames(list(d), s)
      p <- .qes_data_check(sub, .qes_hz_sources_data(sub, dl), studies = s, data = dl,
                           unmapped_codes = TRUE, member_counts = FALSE)
      err <- p[p$severity == "error" & p$table %in% c("crosswalk", "valuemaps"), , drop = FALSE]
      # as in the engine, the labels and the universe of a frame given as
      # data (or of the demonstration study, which keeps only some labels)
      # are not held against the spec
      if (!is.null(data) || !identical(s, spec_s)) err <- err[!err$rule %in% c("V-D3", "V-D7"), , drop = FALSE]
      if (nrow(err) > 0L) {
        shown <- utils::head(err, 5L)
        .qes_abort("decon_data_invalid", class = "qesR_error_spec", args = list(.qes_q(s), nrow(err)),
                   data = list(study = s, problems = err), details = sprintf("%s %s", shown$rule, shown$detail))
      }
    }
    for (i in k) {
      member <- rowSums(members[, .qes_wave_rows(wv, rm$wave[i]), drop = FALSE]) > 0L
      res <- if (i %in% run) {
        j <- match(i, run)
        r <- .qes_hz_apply_row(sub, j, d, member)
        if (any(r$reason %in% "unmapped")) {
          bad <- unique(r$src[r$reason %in% "unmapped"])
          .qes_abort("decon_data_invalid", class = "qesR_error_spec", args = list(.qes_q(s), length(bad)),
                     data = list(study = s), details = sprintf("V-D2 %s %s: code(s) %s have no outcome", rm$column[i],
                                                              rm$source_var[i], paste(utils::head(bad, 10L), collapse = ", ")))
        }
        r
      } else NULL
      rx_res[[paste(s, i)]] <- list(row = i, study = s, wv = wv, res = res,
                                    applied = i %in% run, in_data = ok[match(i, k)])
    }
  }

  # sources
  src <- list()
  new_src <- function(column, study, wave, base, source_var, wording_ref, recode, relaxed, status, applied) {
    src[[length(src) + 1L]] <<- data.frame(
      column = column, study = study, wave = wave, base = base, source_var = source_var,
      wording_ref = wording_ref, recode = recode, relaxed = relaxed, status = status, applied = applied,
      stringsAsFactors = FALSE
    )
    length(src)
  }
  xw <- sp$tables$crosswalk
  levels_of <- function(id) if (is.na(id) || !nzchar(id)) NULL else .qes_spec_levels(sp$tables$levels, id)
  # the source text of a strict crosswalk row of target t in study s and wave w
  strict_row <- function(s, t, w, var) {
    i <- which(xw$study == .qes_hz_spec_study(s) & xw$target == t & xw$wave %in% w & xw$source_var %in% var &
                 xw$primary %in% TRUE)
    if (length(i) == 0L) NA_integer_ else i[1]
  }
  vars_text <- function(xw_, i) {
    v <- .qes_hz_row_vars(xw_, i)
    out <- paste(v, collapse = " > ")
    if (!is.na(xw_$gate_var[i]) && nzchar(xw_$gate_var[i])) out <- paste(xw_$gate_var[i], "/", out)
    out
  }
  wave_rows <- function(s) which(lead$study == s)

  values <- list()
  reasons <- list()
  keys <- list()
  for (jx in seq_len(nrow(rx))) {
    col <- rx$column[jx]
    b <- bases[[jx]]
    tr <- .qes_rx_transform(rx$transform[jx])
    col_set <- levels_of(rx$levels_id[jx])
    lossy <- .qes_rx_lossy(tr)
    v <- rep(NA_character_, n)
    r <- rep("not_asked", n)
    key <- rep(NA_integer_, n)
    tr_text_of <- function(from_set) .qes_rx_transform_text(rx$transform[jx], from_set, col_set, lang)
    if (identical(b$kind, "target")) {
      t <- b$name
      v <- as.character(h[[t]])
      r <- as.character(h[[paste0(t, "__na")]])
      t_set <- levels_of(tg$levels_id[match(t, tg$target)])
      tt <- tr_text_of(t_set)
      cl <- cell[cell$target == t & !cell$excluded %in% "no_row", , drop = FALSE]
      for (q in seq_len(nrow(cl))) {
        s <- cl$study[q]
        derived <- startsWith(cl$rule[q] %|NA|% "", "derive:")
        i <- if (derived) {
          # the row of the target it derives from (the year of birth)
          which(xw$study == .qes_hz_spec_study(s) & xw$wave %in% cl$wave[q] & xw$source_var %in% cl$source_var[q] &
                  xw$primary %in% TRUE)[1]
        } else {
          strict_row(s, t, cl$wave[q], cl$source_var[q])
        }
        w <- .qes_rx_words[[lang]]
        rec <- if (derived) {
          paste(w[["derived"]], paste0(cl$source_var[q], ":"), w[["age"]])
        } else if (!is.na(i)) .qes_rx_row_text(sp, xw, i, t_set, lang) else NA_character_
        if (!is.na(tt)) rec <- paste(c(rec, tt), collapse = "; ")
        kk <- new_src(col, s, cl$wave[q], paste0("target:", t),
                      if (!is.na(i)) vars_text(xw, i) else cl$source_var[q],
                      if (!is.na(i)) xw$wording_ref[i] else NA_character_, rec, lossy,
                      cl$status[q] %|NA|% NA_character_, isTRUE(cl$included[q]))
        rows <- wave_rows(s)
        rows <- rows[!is.na(v[rows]) | !r[rows] %in% c("not_asked", "not_in_wave")]
        key[rows] <- kk
      }
    } else if (b$kind %in% c("pooled", "type")) {
      p <- b$name
      ty <- as.character(h[[paste0(p, "__type")]])
      pr <- as.character(h[[paste0(p, "__na")]])
      if (identical(b$kind, "pooled")) {
        v <- as.character(h[[p]])
        r <- pr
      } else {
        v <- ty
        r <- ifelse(is.na(ty), pr, NA_character_)
      }
      m <- .qes_pool_members(sp, p)
      pv <- pprov[pprov$pooled == p & !is.na(pprov$wave), , drop = FALSE]
      p_set <- levels_of(pl$levels_id[match(p, pl$pooled)])
      for (q in seq_len(nrow(pv))) {
        s <- pv$study[q]
        mem <- pv$member[q]
        i <- strict_row(s, mem, pv$wave[q], strsplit(sub("^[^:]*:[^:]*:", "", pv$item[q] %|NA|% ""), "+", fixed = TRUE)[[1]][1])
        m_set <- levels_of(tg$levels_id[match(mem, tg$target)])
        if (identical(b$kind, "type")) {
          # the member's type, and the column's level it becomes
          to <- if (!is.null(tr) && identical(tr$kind, "recode")) unname(tr$map[pv$type[q]]) else pv$type[q]
          rec <- paste0(mem, " = ", .qes_rx_level_text(to, col_set, lang))
        } else {
          rec <- if (!is.na(i)) .qes_rx_row_text(sp, xw, i, m_set, lang) else NA_character_
          mt <- .qes_rx_transform_text(m$transform[match(mem, m$member)], m_set, p_set, lang)
          rec <- paste(stats::na.omit(c(rec, mt, tr_text_of(p_set))), collapse = "; ")
        }
        m_lossy <- .qes_pool_lossy(.qes_pool_transform(m$transform[match(mem, m$member)]))
        kk <- new_src(col, s, pv$wave[q], paste0("pooled:", p, if (identical(b$kind, "type")) "__type"),
                      if (!is.na(i)) vars_text(xw, i) else NA_character_,
                      if (!is.na(i)) xw$wording_ref[i] else NA_character_, rec,
                      lossy || (identical(b$kind, "pooled") && m_lossy),
                      if (!is.na(i)) xw$status[i] else NA_character_, isTRUE(pv$included[q]))
        key[h$study == s & ty %in% pv$type[q]] <- kk
      }
    } else if (identical(b$kind, "column")) {
      v <- values[[b$name]]
      r <- reasons[[b$name]]
      bk <- keys[[b$name]]
      b_set <- levels_of(rx$levels_id[match(b$name, rx$column)])
      tt <- tr_text_of(b_set)
      bs <- do.call(rbind, src[unique(stats::na.omit(bk))])
      map_k <- integer(0)
      for (q in seq_len(NROW(bs))) {
        kk <- new_src(col, bs$study[q], bs$wave[q], paste0("column:", b$name), bs$source_var[q], bs$wording_ref[q],
                      paste(stats::na.omit(c(bs$recode[q], tt)), collapse = "; "), bs$relaxed[q] || lossy,
                      bs$status[q], bs$applied[q])
        map_k[as.character(unique(stats::na.omit(bk))[q])] <- kk
      }
      key <- unname(map_k[as.character(bk)])
    }
    tv <- .qes_rx_apply_transform(v, r, tr)
    v <- tv$value
    r <- tv$reason

    # the relaxed rows of this column
    for (e in rx_res) {
      i <- e$row
      # a row whose variables the data lack (the demonstration study holds
      # a subset of qes2014) is not a source of the study
      if (!identical(rm$column[i], col) || !isTRUE(e$in_data)) next
      s <- e$study
      rows <- wave_rows(s)
      wv <- e$wv
      in_wave <- match(lead$wave[rows], wv$wave) %in% .qes_wave_rows(wv, rm$wave[i])
      pseudo_row <- ps$tables$crosswalk
      rec <- .qes_rx_row_text(ps, pseudo_row, i, col_set, lang)
      kk <- new_src(col, s, rm$wave[i], "relaxed", vars_text(pseudo_row, i), rm$wording_ref[i], rec, TRUE,
                    rm$status[i], e$applied)
      override <- isTRUE(rm$override[i])
      if (e$applied) {
        fr <- h$source_row[rows]
        rv <- e$res$value[fr]
        rr <- e$res$reason[fr]
        rv[!in_wave] <- NA_character_
        rr[!in_wave] <- "not_in_wave"
        take <- if (override) {
          if (rx$timing[jx] %in% "static") rep(TRUE, length(rows)) else in_wave
        } else {
          in_wave & is.na(v[rows]) & r[rows] %in% c("not_asked", "not_in_wave")
        }
        v[rows[take]] <- rv[take]
        r[rows[take]] <- rr[take]
        key[rows[take]] <- kk
      } else if (!override) {
        # a row in review (or whose variables the data lack) fills nothing
        take <- in_wave & is.na(v[rows]) & r[rows] %in% c("not_asked", "not_in_wave")
        r[rows[take]] <- "not_reviewed"
        key[rows[take]] <- kk
      }
    }

    # a static column: one value per respondent, on each of their rows
    if (rx$timing[jx] %in% "static") {
      g <- lead$qes_id
      iv <- which(!is.na(v))
      pick <- iv[match(g, g[iv])]
      it <- which(is.na(v) & !r %in% c("not_asked", "not_in_wave"))
      pick2 <- it[match(g, g[it])]
      pick[is.na(pick)] <- pick2[is.na(pick)]
      hit <- !is.na(pick)
      v[hit] <- v[pick[hit]]
      r[hit] <- r[pick[hit]]
      key[hit] <- key[pick[hit]]
    }
    r[!is.na(v)] <- NA_character_
    r[is.na(v) & is.na(r)] <- "not_asked"
    values[[col]] <- v
    reasons[[col]] <- r
    keys[[col]] <- key
  }

  sources <- if (length(src) > 0L) do.call(rbind, src) else NULL
  if (!is.null(sources)) {
    sources$key <- seq_len(nrow(sources))
    # a strict or pooled source that gave no row (a pooled member another
    # member always preceded, a base a relaxed row replaced) is left out
    used <- unique(unlist(lapply(names(keys), function(col) paste(col, unique(stats::na.omit(keys[[col]]))))))
    keep <- sources$base %in% "relaxed" | paste(sources$column, sources$key) %in% used
    old_key <- sources$key[keep]
    sources <- sources[keep, , drop = FALSE]
    sources$key <- seq_len(nrow(sources))
    for (col in names(keys)) keys[[col]] <- match(keys[[col]], old_key)
    rownames(sources) <- NULL
  }

  # weights: per study and wave, the recommended weight and why it is NA
  wt <- sp$tables$weights
  wv_all <- sp$tables$waves
  winfo <- list()
  for (s in codes) {
    spec_s <- .qes_hz_spec_study(s)
    ws <- wv_all[wv_all$study == spec_s, , drop = FALSE]
    for (w in ws$wave[order(ws$wave_order)]) {
      kr <- which(wt$study == spec_s & (wt$wave == w | wt$wave == .qes_all_waves) & wt$recommended %in% TRUE)
      rows <- which(lead$study == s & lead$wave %in% w)
      wvar <- if (length(kr) > 0L) wt$weight_var[kr[1]] else NA_character_
      status <- if (length(kr) > 0L) wt$status[kr[1]] else NA_character_
      reason <- if (length(kr) == 0L) "no_recommended_weight" else if (!identical(status, "reviewed")) "needs_review" else
        if (all(is.na(lead$weight[rows]))) "not_in_data" else NA_character_
      winfo[[length(winfo) + 1L]] <- data.frame(
        study = s, wave = w, weight_var = wvar, status = status, reason = reason, n_rows = length(rows),
        n_weight = sum(!is.na(lead$weight[rows])),
        mean = if (any(!is.na(lead$weight[rows]))) mean(lead$weight[rows], na.rm = TRUE) else NA_real_,
        stringsAsFactors = FALSE
      )
    }
  }
  winfo <- do.call(rbind, winfo)
  rownames(winfo) <- NULL

  list(lead = lead, rx = rx, value = values, reason = reasons, key = keys, sources = sources,
       weights = winfo, provenance = prov, spec = sp)
}

# ---- qes_decon() -------------------------------------------------------------------------

#' One flat data frame of relaxed harmonized variables (experimental)
#'
#' `qes_decon()` returns one data frame for every harmonized Quebec Election
#' Study, with one row per respondent and wave and one column per concept,
#' under plain names in the style of the cesR package (`education`,
#' `income_cat`, `vote_choice`, `sovereignty`, `lr`...). It is a relaxed
#' harmonization: one concept goes in one column for every study, even when
#' the wording or the answer options differ, with coarse common categories
#' (education in four groups, income in thirds of each study's respondents,
#' interest low, medium or high, a referendum vote of yes or no whatever the
#' question). It trades exactness for coverage: use [qes_harmonize()] and
#' its grades when the difference between two questions matters.
#'
#' Each column says how it was relaxed (`attr(, "relaxed")`, one sentence)
#' and where each study's values come from (`attr(, "sources")`: the source
#' variable, the document that gives its wording and the rule that turns its
#' codes into the column's categories). `attr(x, "decon_sources")` gathers
#' them for every column, with the count of values and of each reason for a
#' missing value. Relaxed columns carry no comparability grade, and none of
#' them claims that two studies asked the same question.
#'
#' A column is built from a strict target or a pooled variable of the
#' harmonization spec where one exists (`gender`, `vote_choice`,
#' `satis_democracy`...), recoded into the column's categories where needed
#' (`born_quebec` from the birthplace, `interest` from the 0-1 interest
#' scale), and from relaxed mappings of the studies' own questions where the
#' strict layer has none or keeps a study out (`education`, `income_cat`,
#' `religion`, `employment`...). The relaxed mappings are reviewed like the
#' strict ones: a mapping not yet signed off (status `review`) is not
#' applied, and its cells are `NA` with reason `not_reviewed` (see
#' `qes_spec("relaxed_maps")`); a message says how many. The 60 mappings of
#' spec 4.5.0 are signed off (status `stable`) by an automated double review
#' against the original files and documents, not a human review, as their
#' `reviewed_by` and `review_note` say.
#'
#' @section Rows, waves and weights:
#' The rows are those of `qes_harmonize(layout = "long")`: one per respondent
#' and wave they took part in, so a panel respondent has one row per wave,
#' and no row is dropped. Socio-demographic columns (education, income,
#' language, religion, region...) are the same on every row of a respondent;
#' the vote, turnout and attitudes sit on the row of the wave that asked them
#' (the vote intention on the campaign wave, the reported vote on the
#' post-election wave). With `weights = TRUE`, `weight` is the wave's
#' recommended weight rescaled to mean 1 in each study and wave, and
#' `weight_var` its source variable. Both are `NA` where the study has no
#' recommended weight (`qes2008`: its weights are calibrated on the reported
#' vote) or where the weight still needs review; `attr(x, "weights")` says
#' which, study by study and wave by wave. Estimate within one study and
#' wave, or with [qes_design()] on [qes_harmonize()] output.
#'
#' @section Correspondence with cesR:
#' cesR's `get_decon()` returns 21 columns of the 2019 Canadian Election
#' Study. Their counterparts here: `citizenship`, `yob`, `gender`,
#' `education`, `lr` (cesR's `lr`, `lr_bef` and `lr_aft`: no study asks the
#' scale both before and after the election), `religion`, `language`,
#' `language_eng` and `language_fr` (no study asks about Indigenous
#' languages), `employment`, `income_cat` (the income amount is not
#' comparable across studies), `marital` and `econ_retro` (Quebec's economy).
#' `province_territory` is dropped (every respondent lives in Quebec), and so
#' are sexual orientation, the federal economy and personal finances (one
#' study at most).
#'
#' @eval .rd_decon_columns()
#'
#' @section Experimental:
#' The relaxed layer is new in qesR 0.9.0 (spec 4.4.0; its mappings signed
#' off in spec 4.5.0). Its columns, groups and mappings may change, and a new
#' relaxed mapping starts in review.
#'
#' @section En français:
#' `qes_decon()` renvoie un seul tableau pour toutes les études électorales
#' québécoises harmonisées, une ligne par personne et par vague, une colonne
#' par concept, sous des noms simples à la manière du package cesR. C'est une
#' harmonisation souple : un concept va dans une seule colonne pour chaque
#' étude, même quand le libellé ou les choix de réponse diffèrent, avec des
#' catégories communes larges (la scolarité en quatre groupes, le revenu en
#' tiers des répondants de chaque étude, l'intérêt faible, moyen ou élevé, un
#' vote référendaire oui ou non quelle que soit la question). Elle échange
#' l'exactitude contre la couverture : utilisez [qes_harmonize()] et ses
#' niveaux de comparabilité quand la différence entre deux questions compte.
#' Chaque colonne dit comment elle a été assouplie (`attr(, "relaxed")`) et
#' d'où viennent les valeurs de chaque étude (`attr(, "sources")`) ;
#' `attr(x, "decon_sources")` les réunit toutes. Les colonnes souples n'ont
#' aucun niveau de comparabilité. Les appariements souples sont révisés comme
#' les autres : un appariement pas encore approuvé (statut `review`) n'est pas
#' appliqué, et ses cellules valent `NA` (motif `not_reviewed`). Les 60
#' appariements de la spécification 4.5.0 sont approuvés (statut `stable`)
#' par une double révision automatisée sur les fichiers et les documents
#' originaux, et non par une révision humaine, comme le disent leurs champs
#' `reviewed_by` et `review_note`. Les noms de
#' colonnes restent en anglais ; `lang = "fr"` donne les étiquettes, les
#' règles et les sources en français.
#'
#' @param studies `NULL` (default) or `"all"`: every harmonized study (the 11
#'   Quebec studies; the 1998 firms' own files are in `qes1998`). Otherwise
#'   study codes, as [qes_studies()] lists them; `"qes_demo"`, the synthetic
#'   demonstration study, is accepted and needs no download.
#' @param lang Language of the factor levels, of the `label`, `relaxed` and
#'   `sources` attributes and of `decon_sources`: `"en"` (default) or `"fr"`.
#'   Column names do not change.
#' @param weights If `TRUE` (default), adds `weight` and `weight_var`.
#' @param quiet If `TRUE`, no download, cache or summary messages.
#'
#' @return A data frame (no class of its own), returned visibly, with the
#'   columns `study`, `year` (the study's year; for the CROP polls, the year
#'   of the respondent's poll), `wave`, `qes_id` (`<study>:<identifier>`),
#'   then `weight` and `weight_var` (with `weights = TRUE`), then the relaxed
#'   columns listed under *Columns*: factors (ordered for ordinal ones) with
#'   every category as a level, or numbers. Each relaxed column carries
#'   `attr(, "label")`, `attr(, "relaxed")` and `attr(, "sources")` (a data
#'   frame: `study`, `wave`, `source_var`, `wording_ref`, `recode`). The data
#'   frame carries `decon_sources` (`column`, `study`, `wave`, `base`,
#'   `source_var`, `wording_ref`, `recode`, `relaxed` (`TRUE` where a relaxed
#'   mapping or a collapse was used), `status` and `applied` of the source
#'   row, `n_value` (respondents of a static column, rows of a wave column)
#'   and `na_reasons` (`"dk=12; refused=3"`)), `weights` (`study`, `wave`,
#'   `weight_var`, `status`, `reason`: `NA`, `"needs_review"`,
#'   `"no_recommended_weight"` or `"not_in_data"`, `n_rows`, `n_weight`,
#'   `mean`), `qes_provenance` (the files read; see [qes_provenance()]),
#'   `spec_version`, `qesR_version` and `lang`.
#' @family harmonization
#' @seealso [qes_harmonize()] for the strict, graded harmonization;
#'   [qes_spec()] with `view = "relaxed"` for the columns and
#'   `view = "relaxed_maps"` for each study's relaxed mapping;
#'   [qes_party_lineage()], which also joins the ADQ and the CAQ in
#'   `vote_choice`, `vote_prev` and `pid`.
#' @examples
#' # the synthetic demonstration study, offline
#' d <- qes_decon("qes_demo", quiet = TRUE)
#' head(d[, c("study", "wave", "gender", "age_group", "vote_choice", "sovereignty")])
#' attr(d$sovereignty, "relaxed")
#' attr(d$vote_choice, "sources")
#'
#' # where every column comes from, study by study
#' src <- attr(d, "decon_sources")
#' src[, c("column", "study", "source_var", "relaxed", "n_value")]
#'
#' # French labels, same column names
#' d_fr <- qes_decon("qes_demo", lang = "fr", quiet = TRUE)
#' levels(d_fr$interest)
#'
#' # every study: qes_decon() reads the 11 files (downloaded once, then from
#' # the cache), e.g. d <- qes_decon(); table(d$study, d$sovereignty)
#' @export
qes_decon <- function(studies = NULL, lang = c("en", "fr"), weights = TRUE, quiet = FALSE) {
  lang <- .qes_check_one(lang, "lang", c("en", "fr"))
  .qes_check_flag(weights, "weights")
  .qes_check_flag(quiet, "quiet")
  .qes_decon_build(studies, lang = lang, weights = weights, quiet = quiet)
}

# The data frame of qes_decon() from .qes_decon_compute().
.qes_decon_build <- function(studies = NULL, lang = "en", weights = TRUE, quiet = FALSE, data = NULL,
                             spec = NULL, include_review = FALSE) {
  x <- .qes_decon_compute(studies, lang = lang, quiet = quiet, data = data, spec = spec,
                          include_review = include_review)
  sp <- x$spec
  rx <- x$rx
  lead <- x$lead
  cols <- list(study = lead$study, year = lead$year, wave = lead$wave, qes_id = lead$qes_id)
  if (isTRUE(weights)) {
    cols$weight <- lead$weight
    cols$weight_var <- lead$weight_var
  }
  src <- x$sources
  first_row <- !duplicated(lead$qes_id)
  counts <- lapply(seq_len(NROW(src)), function(k) {
    col <- src$column[k]
    static <- rx$timing[match(col, rx$column)] %in% "static"
    hit <- x$key[[col]] %in% k & (if (static) first_row else TRUE)
    rr <- x$reason[[col]][hit & is.na(x$value[[col]])]
    tab <- table(factor(rr, levels = .qes_hz_reason_levels()))
    tab <- tab[tab > 0L]
    list(n = sum(hit & !is.na(x$value[[col]])),
         na = if (length(tab) > 0L) paste(paste0(names(tab), "=", as.integer(tab)), collapse = "; ") else NA_character_)
  })
  if (!is.null(src)) {
    src$n_value <- vapply(counts, `[[`, integer(1), "n")
    src$na_reasons <- vapply(counts, `[[`, character(1), "na")
  }
  for (j in seq_len(nrow(rx))) {
    col <- rx$column[j]
    v <- x$value[[col]]
    out <- if (rx$type[j] %in% c("categorical", "ordinal")) {
      set <- .qes_spec_levels(sp$tables$levels, rx$levels_id[j])
      labs <- set[[paste0("label_", lang)]]
      labs[is.na(labs) | duplicated(labs)] <- set$name[is.na(labs) | duplicated(labs)]
      factor(labs[match(v, set$name)], levels = labs, ordered = identical(rx$type[j], "ordinal"))
    } else {
      as.numeric(v)
    }
    attr(out, "label") <- rx[[paste0("label_", lang)]][j]
    attr(out, "relaxed") <- rx[[paste0("relax_", lang)]][j]
    s <- if (is.null(src)) NULL else src[src$column == col, c("study", "wave", "source_var", "wording_ref", "recode"), drop = FALSE]
    if (!is.null(s)) rownames(s) <- NULL
    attr(out, "sources") <- s
    cols[[col]] <- out
  }
  res <- structure(cols, class = "data.frame", row.names = c(NA_integer_, -nrow(lead)))
  if (!is.null(src)) {
    src <- src[, c("column", "study", "wave", "base", "source_var", "wording_ref", "recode", "relaxed",
                   "status", "applied", "n_value", "na_reasons")]
    rownames(src) <- NULL
  }
  attr(res, "decon_sources") <- src
  if (isTRUE(weights)) attr(res, "weights") <- x$weights
  study_prov <- x$provenance
  if (!is.null(study_prov)) {
    attr(study_prov, "cell") <- NULL
    attr(study_prov, "pooled") <- NULL
  }
  attr(res, "qes_provenance") <- study_prov
  attr(res, "spec_version") <- sp$version
  attr(res, "qesR_version") <- as.character(.qes_engine_version())
  attr(res, "lang") <- lang

  # the one summary message
  if (!isTRUE(quiet) && !is.null(src)) {
    used <- src[src$applied & src$n_value > 0L, , drop = FALSE]
    relaxed_cols <- unique(used$column[used$base == "relaxed"])
    rel_txt <- vapply(relaxed_cols, function(col) {
      sprintf("%s (%s)", col, paste(unique(used$study[used$column == col & used$base == "relaxed"]), collapse = ", "))
    }, character(1))
    held <- src[src$base == "relaxed" & !src$applied, , drop = FALSE]
    args <- list(nrow(rx), .qes_q(unique(lead$study)), if (length(rel_txt) > 0L) paste(rel_txt, collapse = "; ") else "-")
    if (nrow(held) > 0L) {
      .qes_inform("decon_summary_held", class = "qesR_message_decon",
                  args = c(args, list(nrow(held), paste(unique(held$column), collapse = ", "))),
                  data = list(sources = src), quiet = quiet)
    } else {
      .qes_inform("decon_summary", class = "qesR_message_decon", args = args, data = list(sources = src),
                  quiet = quiet)
    }
  }
  res
}

# ---- documentation ----------------------------------------------------------------------

# The Columns section of ?qes_decon, generated from relaxed.csv.
.rd_decon_columns <- function() {
  sp <- tryCatch(.qes_spec_get(NULL, "none"), error = function(e) NULL)
  if (is.null(sp)) return(character(0))
  rx <- .qes_rx_columns(sp)
  if (nrow(rx) == 0L) return(character(0))
  esc <- function(x) gsub("([%{}\\\\])", "\\\\\\1", x)
  items <- vapply(seq_len(nrow(rx)), function(j) {
    lv <- if (rx$type[j] %in% c("categorical", "ordinal")) {
      set <- .qes_spec_levels(sp$tables$levels, rx$levels_id[j])
      paste0(" Levels: ", paste(set$label_en, collapse = "; "), ".")
    } else {
      sprintf(" A number from %s to %s.", .qes_code_chr(rx$valid_min[j]), .qes_code_chr(rx$valid_max[j]))
    }
    sprintf("\\item{\\code{%s}}{%s (%s, %s). %s%s}", rx$column[j], esc(rx$label_en[j]),
            if (rx$timing[j] %in% "static") "one value per respondent" else "on the wave that asked it",
            esc(rx$base[j] %|NA|% "relaxed mappings only"), esc(rx$relax_en[j]), esc(lv))
  }, character(1))
  c("@section Columns:",
    sprintf("The relaxed columns of spec %s, in order (\\code{qes_spec(\"relaxed\")} lists them with their French labels):", sp$version),
    "\\describe{", items, "}")
}
