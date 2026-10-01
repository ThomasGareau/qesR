# The "targets" and "crosswalk" views of qes_spec() and the reference
# generated from the spec (design.md sections 2.2, 5.8 and 9, slice HZ3).
#
# Everything here is built from the spec tables only, so the reference, the
# views and the target list on ?qes_spec cannot drift from the rows the
# engine applies. Returned text follows `lang` (rule P4); the fixed phrases
# of the reference are in .qes_ref_text, English and French.

# ---- helpers ---------------------------------------------------------------------

# The value of an enum in `lang` ("en" or "fr"), the value itself when the
# enum has no label for it.
.qes_enum_label <- function(enum, value, lang) {
  rows <- .qes_enum(enum)
  lab <- rows[[paste0("label_", lang)]][match(value, rows$value)]
  ifelse(is.na(lab), value, lab)
}

# Text in `lang`, the other language when it is missing.
.qes_pick_lang <- function(x, col, lang) {
  own <- x[[paste0(col, "_", lang)]]
  other <- x[[paste0(col, "_", if (identical(lang, "fr")) "en" else "fr")]]
  ifelse(is.na(own), other, own)
}

# The gate of crosswalk rows as text: "q21: 2 = not_voted, 8 = dk".
.qes_gate_text <- function(xw) {
  vapply(seq_len(nrow(xw)), function(i) {
    if (is.na(xw$gate_var[i])) return(NA_character_)
    to <- .qes_parse_kv(xw$gate_to[i]) %||% character(0)
    paste0(xw$gate_var[i], ": ", paste(names(to), to, sep = " = ", collapse = ", "))
  }, character(1))
}

# Levels of the target set a crosswalk row did not offer (";"-list), NA when
# the target has no levels or the row lists none.
.qes_not_offered <- function(spec, xw) {
  tg <- spec$tables$targets
  vapply(seq_len(nrow(xw)), function(i) {
    id <- tg$levels_id[match(xw$target[i], tg$target)]
    offered <- .qes_split_list(xw$levels_offered[i])
    if (is.na(id) || length(offered) == 0L) return(NA_character_)
    set <- .qes_spec_levels(spec$tables$levels, id)
    paste(setdiff(set$name, offered), collapse = ";")
  }, character(1))
}

# A crosswalk row's wave for the reference: its name, or, for the wave "*",
# "each poll" (pooled polls) or "any wave" (a question asked in whichever
# wave the respondent took part).
.qes_wave_label <- function(wave, lang, study = NULL, spec = NULL) {
  polls <- if (is.null(study) || is.null(spec)) rep(TRUE, length(wave)) else
    vapply(study, function(s) .qes_poll_study(spec$tables$waves, s), logical(1))
  ifelse(wave %in% .qes_all_waves, ifelse(polls, .qes_rt("each_poll", lang), .qes_rt("any_wave", lang)), wave)
}

# The recommended weight of each crosswalk row's study-wave: its registry
# rows (weight_var and status; NA where there is none).
.qes_row_weight_rec <- function(spec, xw) {
  wt <- spec$tables$weights
  rec <- wt[wt$recommended %in% TRUE, , drop = FALSE]
  i <- match(paste(xw$study, xw$wave), paste(rec$study, rec$wave))
  # the poll waves of a row of wave "*" (or of a weight of wave "*")
  star <- match(paste(xw$study, .qes_all_waves), paste(rec$study, rec$wave))
  i[is.na(i)] <- star[is.na(i)]
  for (k in which(is.na(i) & xw$wave %in% .qes_all_waves)) {
    hit <- which(rec$study == xw$study[k])
    if (length(unique(rec$weight_var[hit])) == 1L) i[k] <- hit[1]
  }
  out <- rec[i, c("weight_var", "status"), drop = FALSE]
  rownames(out) <- NULL
  out
}
.qes_row_weight <- function(spec, xw) .qes_row_weight_rec(spec, xw)$weight_var

# A recommended weight as the reference and coverage pages show it: the
# variable, and whether it is applied (a weight that needs review is not).
# `site = TRUE` (the website) says so in plain words.
.qes_weight_cell <- function(var, status, lang, site = FALSE) {
  mark <- .qes_rt(if (isTRUE(site)) "site_weight_review" else "cov_weight_review", lang)
  ifelse(is.na(var), NA_character_,
         ifelse(status %in% "reviewed", paste0("`", var, "`"),
                paste0("`", var, "` (", mark, ")")))
}

# Spec text as the website shows it: without the names of the spec's own
# files (the reasons and descriptions of a few rows cite waves.csv).
.qes_site_text <- function(x) {
  x <- gsub("\\s*\\((see |voir )?waves\\.csv\\)", "", x, perl = TRUE)
  x <- gsub(",\\s*(see|voir) waves\\.csv\\)", ")", x, perl = TRUE)
  x <- gsub("\\((see|voir) waves\\.csv\\s*;\\s*", "(", x, perl = TRUE)
  x
}

# The `studies` filter of the views: canonical codes (the demo study means
# the rows it stands in for).
.qes_view_studies <- function(studies) {
  if (is.null(studies)) return(NULL)
  codes <- .qes_resolve_codes(studies, "studies", demo = TRUE)
  unique(vapply(codes, .qes_hz_spec_study, character(1), USE.NAMES = FALSE))
}

# ---- view "targets" -------------------------------------------------------------------

.qes_spec_targets_view <- function(spec, targets, studies, lang) {
  tg <- spec$tables$targets
  tg <- tg[if (is.null(targets)) rep(TRUE, nrow(tg)) else tg$target %in% targets, , drop = FALSE]
  xw <- spec$tables$crosswalk
  xw <- xw[!is.na(xw$rule) & xw$rule != "none", , drop = FALSE]
  cols <- intersect(c(.qes_study_codes(), unique(xw$study)), unique(xw$study))
  if (!is.null(studies)) cols <- intersect(cols, studies)
  levels_text <- vapply(tg$levels_id, function(id) {
    if (is.na(id)) return(NA_character_)
    set <- .qes_spec_levels(spec$tables$levels, id)
    paste(set$code, set[[paste0("label_", lang)]], sep = "=", collapse = "; ")
  }, character(1), USE.NAMES = FALSE)
  out <- data.frame(
    target = tg$target, family = tg$family, type = tg$type, target_timing = tg$target_timing,
    label = .qes_pick_lang(tg, "label", lang),
    definition = .qes_pick_lang(tg, "description", lang),
    levels = levels_text, status = tg$status, added_in = tg$added_in,
    stringsAsFactors = FALSE
  )
  for (s in cols) {
    out[[s]] <- vapply(tg$target, function(t) {
      g <- xw$grade[xw$study == s & xw$target == t]
      g <- g[g %in% .qes_hz_grades]
      if (length(g) == 0L) NA_character_ else .qes_hz_grades[min(match(g, .qes_hz_grades))]
    }, character(1), USE.NAMES = FALSE)
  }
  rownames(out) <- NULL
  attr(out, "qes_spec") <- list(version = spec$version, hash = spec$hash, custom = isTRUE(spec$custom))
  out
}

# ---- view "pooled" ------------------------------------------------------------------------

# One row per member of each pooled variable (R/hz-pool.R), with the grade
# of the member's row in each study, capped by its grade_cap.
.qes_spec_pooled_view <- function(spec, pools, studies, lang) {
  pt <- .qes_pool_tables(spec)
  pl <- pt$pooled
  pm <- pt$members
  if (!is.null(pools)) pl <- pl[pl$pooled %in% pools, , drop = FALSE]
  pm <- pm[pm$pooled %in% pl$pooled, , drop = FALSE]
  pm <- pm[order(match(pm$pooled, pl$pooled), pm$precedence), , drop = FALSE]
  xw <- spec$tables$crosswalk
  xw <- xw[!is.na(xw$rule) & xw$rule != "none" & xw$primary %in% TRUE, , drop = FALSE]
  cols <- intersect(c(.qes_study_codes(), unique(xw$study)), unique(xw$study))
  if (!is.null(studies)) cols <- intersect(cols, studies)
  j <- match(pm$pooled, pl$pooled)
  levels_text <- vapply(seq_len(nrow(pm)), function(k) {
    id <- pl$levels_id[j[k]]
    if (is.na(id)) return(sprintf("%s-%s", .qes_code_chr(pl$valid_min[j[k]]), .qes_code_chr(pl$valid_max[j[k]])))
    set <- .qes_spec_levels(spec$tables$levels, id)
    paste(set$code, set[[paste0("label_", lang)]], sep = "=", collapse = "; ")
  }, character(1))
  out <- data.frame(
    pooled = pm$pooled, label = .qes_pick_lang(pl, "label", lang)[j],
    definition = .qes_pick_lang(pl, "description", lang)[j], type = pl$type[j], levels = levels_text,
    type_name = pm$type_name, type_label = .qes_pick_lang(pm, "type_label", lang), member = pm$member,
    precedence = pm$precedence, default = pm$default, transform = pm$transform, grade_cap = pm$grade_cap,
    note = .qes_pick_lang(pm, "note", lang), status = pl$status[j], added_in = pm$added_in,
    stringsAsFactors = FALSE
  )
  for (s in cols) {
    g <- xw$grade[match(paste(s, pm$member), paste(xw$study, xw$target))]
    out[[s]] <- .qes_pool_cap(g, pm$grade_cap)
  }
  rownames(out) <- NULL
  attr(out, "qes_spec") <- list(version = spec$version, hash = spec$hash, custom = isTRUE(spec$custom))
  out
}

# ---- view "crosswalk" ------------------------------------------------------------------

# The relaxed columns of qes_decon() (view "relaxed"): one row per column,
# then one column per study: "strict" where the base gives the study's
# values, "relaxed" where a relaxed row of the study does (with its status
# when it is not signed off), NA where neither. A column built on another
# column (base "column:<name>") takes that column's source in each study,
# so that a study whose values of the base column come from a relaxed row
# is "relaxed" there too.
.qes_spec_relaxed_view <- function(spec, columns, studies, lang) {
  t <- .qes_rx_tables(spec)
  rx <- t$relaxed[order(t$relaxed$position), , drop = FALSE]
  rm <- t$maps
  if (!is.null(columns)) rx <- rx[rx$column %in% columns, , drop = FALSE]
  all_studies <- unique(c(.qes_study_codes(), spec$tables$waves$study, rm$study))
  source_of <- list()
  covered <- list()
  for (k in order(t$relaxed$position)) {
    col <- t$relaxed$column[k]
    base <- .qes_rx_base(t$relaxed$base[k])
    base_studies <- .qes_rx_base_studies(spec, base, covered)
    parent <- if (!is.null(base) && identical(base$kind, "column")) source_of[[base$name]] else NULL
    source_of[[col]] <- vapply(all_studies, function(st) {
      i <- which(rm$column == col & rm$study == st)
      if (length(i) > 0L) {
        stt <- unique(rm$status[i])
        return(if (all(stt %in% "stable")) "relaxed" else paste0("relaxed (", paste(stt, collapse = ", "), ")"))
      }
      if (!is.null(parent)) return(unname(parent[st]))
      if (st %in% base_studies) "strict" else NA_character_
    }, character(1))
    covered[[col]] <- all_studies[!is.na(source_of[[col]])]
  }
  levels_text <- vapply(seq_len(nrow(rx)), function(j) {
    if (is.na(rx$levels_id[j])) return(sprintf("%s-%s", .qes_code_chr(rx$valid_min[j]), .qes_code_chr(rx$valid_max[j])))
    set <- .qes_spec_levels(spec$tables$levels, rx$levels_id[j])
    paste(paste0(set$name, "=", set[[paste0("label_", lang)]]), collapse = "; ")
  }, character(1))
  out <- data.frame(
    column = rx$column, position = rx$position, label = rx[[paste0("label_", lang)]], type = rx$type,
    levels = levels_text, base = rx$base, transform = rx$transform, timing = rx$timing,
    relaxed = rx[[paste0("relax_", lang)]], definition = rx[[paste0("description_", lang)]],
    essential = rx$essential, status = rx$status, added_in = rx$added_in, stringsAsFactors = FALSE
  )
  study_cols <- intersect(.qes_study_codes(), unique(c(spec$tables$waves$study, rm$study)))
  if (!is.null(studies)) study_cols <- intersect(study_cols, studies)
  for (st in study_cols) {
    out[[st]] <- vapply(rx$column, function(col) unname(source_of[[col]][st]), character(1), USE.NAMES = FALSE)
  }
  rownames(out) <- NULL
  out
}

# The relaxed rows (view "relaxed_maps"), with the recode in words.
.qes_spec_relaxed_maps_view <- function(spec, columns, studies, lang) {
  t <- .qes_rx_tables(spec)
  rm <- t$maps
  ps <- .qes_rx_pseudo_spec(spec)
  keep <- rep(TRUE, nrow(rm))
  if (!is.null(columns)) keep <- keep & rm$column %in% columns
  if (!is.null(studies)) keep <- keep & rm$study %in% studies
  idx <- which(keep)
  rx <- t$relaxed
  recode <- vapply(idx, function(i) {
    j <- match(rm$column[i], rx$column)
    set <- if (is.na(j) || is.na(rx$levels_id[j])) NULL else .qes_spec_levels(spec$tables$levels, rx$levels_id[j])
    .qes_rx_row_text(ps, ps$tables$crosswalk, i, set, lang)
  }, character(1))
  gate <- ifelse(is.na(rm$gate_var[idx]), NA_character_, paste0(rm$gate_var[idx], ": ", rm$gate_to[idx]))
  out <- data.frame(
    study = rm$study[idx], wave = rm$wave[idx], column = rm$column[idx], source_var = rm$source_var[idx],
    rule = rm$rule[idx], map_id = rm$map_id[idx], args = rm$args[idx], na_codes = rm$na_codes[idx],
    gate = gate, override = rm$override[idx], recode = recode,
    wording = .qes_pick_lang(rm[idx, , drop = FALSE], "wording", lang), wording_ref = rm$wording_ref[idx],
    notes = .qes_pick_lang(rm[idx, , drop = FALSE], "notes", lang), evidence = rm$evidence[idx],
    status = rm$status[idx], reviewed_by = rm$reviewed_by[idx], reviewed_on = rm$reviewed_on[idx],
    review_note = rm$review_note[idx], added_in = rm$added_in[idx], stringsAsFactors = FALSE
  )
  rownames(out) <- NULL
  out
}

.qes_spec_crosswalk_view <- function(spec, targets, studies, level, format, lang) {
  all_rows <- spec$tables$crosswalk
  idx <- which((is.null(targets) | all_rows$target %in% targets) & (is.null(studies) | all_rows$study %in% studies))
  xw <- all_rows[idx, , drop = FALSE]
  rownames(xw) <- NULL
  if (identical(format, "retroharmonize")) {
    out <- .qes_crosswalk_retroharmonize(spec, idx, lang)
  } else if (identical(level, "code")) {
    out <- .qes_crosswalk_codes(spec, idx, lang)
  } else {
    wording <- ifelse(is.na(xw[[paste0("wording_", lang)]]),
                      xw[[paste0("wording_", if (identical(lang, "fr")) "en" else "fr")]],
                      xw[[paste0("wording_", lang)]])
    out <- data.frame(
      study = xw$study, wave = xw$wave, target = xw$target, source_var = xw$source_var,
      rule = xw$rule, map_id = xw$map_id, args = xw$args, na_codes = xw$na_codes,
      gate = .qes_gate_text(xw), primary = xw$primary, grade = xw$grade,
      grade_reason = .qes_pick_lang(xw, "grade_reason", lang),
      instrument = xw$instrument, election_ref = xw$election_ref, mode = xw$mode,
      dk_offered = xw$dk_offered, levels_offered = xw$levels_offered,
      levels_not_offered = .qes_not_offered(spec, xw),
      wording = wording, wording_ref = xw$wording_ref,
      weight_var = .qes_row_weight(spec, xw), status = xw$status, reviewed_by = xw$reviewed_by,
      reviewed_on = xw$reviewed_on, review_note = xw$review_note, evidence = xw$evidence,
      notes = .qes_pick_lang(xw, "notes", lang),
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  structure(out, class = c("qes_crosswalk", "data.frame"),
            qes_spec = list(version = spec$version, hash = spec$hash, custom = isTRUE(spec$custom)),
            targets = unique(xw$target), lang = lang, spec_object = spec, view_level = level)
}

# One row per code a crosswalk row names: value map codes, na_codes and gate
# codes, with the outcome of each.
.qes_crosswalk_codes <- function(spec, idx, lang) {
  xw <- spec$tables$crosswalk[idx, , drop = FALSE]
  vm <- spec$tables$valuemaps
  tg <- spec$tables$targets
  out <- list()
  for (i in seq_len(nrow(xw))) {
    j <- match(xw$target[i], tg$target)
    set <- if (!is.na(tg$levels_id[j])) .qes_spec_levels(spec$tables$levels, tg$levels_id[j]) else NULL
    base <- data.frame(study = xw$study[i], wave = xw$wave[i], target = xw$target[i],
                       stringsAsFactors = FALSE)
    add <- function(variable, code, label, origin, target_code, reason, note) {
      if (length(code) == 0L) return(invisible())
      level <- if (is.null(set)) rep(NA_character_, length(code)) else set$name[match(target_code, set$code)]
      tlab <- if (is.null(set)) rep(NA_character_, length(code)) else set[[paste0("label_", lang)]][match(target_code, set$code)]
      out[[length(out) + 1L]] <<- cbind(base[rep(1L, length(code)), , drop = FALSE], data.frame(
        variable = variable, source_code = code, source_label = label, origin = origin,
        target_code = as.integer(target_code), target_level = level, target_label = tlab,
        na_reason = reason, note = note, stringsAsFactors = FALSE
      ))
    }
    if (xw$rule[i] %in% c("map", "coalesce")) {
      m <- vm[vm$map_id %in% xw$map_id[i], , drop = FALSE]
      add(xw$source_var[i], m$source_code, m$source_label, rep("map", nrow(m)), m$target_code, m$na_reason, m$note)
      # the other variables of a coalesce row, each with its own map
      then <- .qes_hz_coalesce_then(xw, i) %||% data.frame(var = character(0), map_id = character(0))
      for (k in seq_len(nrow(then))) {
        m <- vm[vm$map_id %in% then$map_id[k], , drop = FALSE]
        add(rep(then$var[k], nrow(m)), m$source_code, m$source_label, rep("map", nrow(m)), m$target_code, m$na_reason, m$note)
      }
    }
    if (xw$rule[i] %in% "numeric") {
      r <- .qes_hz_row_rule(spec, idx[i])
      range <- sprintf("%s-%s", .qes_code_chr(r$min), .qes_code_chr(r$max))
      add(xw$source_var[i], range, NA_character_, "range", NA_integer_, NA_character_,
          if (r$from_label) "values read from the value labels" else NA_character_)
    }
    nac <- .qes_parse_kv(xw$na_codes[i]) %||% character(0)
    add(xw$source_var[i], names(nac), rep(NA_character_, length(nac)), rep("na_codes", length(nac)),
        rep(NA_integer_, length(nac)), unname(nac), rep(NA_character_, length(nac)))
    to <- .qes_parse_kv(xw$gate_to[i]) %||% character(0)
    if (length(to) > 0L) {
      is_level <- !is.null(set) & unname(to) %in% (set$name %||% character(0))
      add(rep(xw$gate_var[i], length(to)), names(to), rep(NA_character_, length(to)), rep("gate", length(to)),
          ifelse(is_level, set$code[match(unname(to), set$name)], NA_integer_),
          ifelse(is_level, NA_character_, unname(to)), rep(NA_character_, length(to)))
    }
  }
  if (length(out) == 0L) {
    return(data.frame(study = character(0), wave = character(0), target = character(0),
                      variable = character(0), source_code = character(0), source_label = character(0),
                      origin = character(0), target_code = integer(0), target_level = character(0),
                      target_label = character(0), na_reason = character(0), note = character(0),
                      stringsAsFactors = FALSE))
  }
  do.call(rbind, out)
}

# The code-level crosswalk under the column names of the retroharmonize
# package's crosswalk tables (no dependency): value maps and na_codes of the
# source variables (gate outcomes belong to another variable).
.qes_crosswalk_retroharmonize <- function(spec, idx, lang) {
  codes <- .qes_crosswalk_codes(spec, idx, lang)
  codes <- codes[codes$origin %in% c("map", "na_codes"), , drop = FALSE]
  tg <- spec$tables$targets
  files <- .qes_catalog()$files
  data_file <- files[files$role == "data" & files$is_default %in% TRUE, , drop = FALSE]
  reason_label <- .qes_enum_label("missing_type", codes$na_reason, lang)
  type <- tg$type[match(codes$target, tg$target)]
  data.frame(
    id = codes$study,
    filename = data_file$original_file_name[match(codes$study, data_file$study)],
    var_name_orig = codes$variable,
    var_name_target = codes$target,
    val_numeric_orig = suppressWarnings(as.numeric(codes$source_code)),
    val_numeric_target = codes$target_code,
    val_label_orig = codes$source_label,
    val_label_target = ifelse(is.na(codes$na_reason), codes$target_label, reason_label),
    na_label_orig = NA_character_,
    na_label_target = codes$na_reason,
    class_orig = "haven_labelled",
    class_target = ifelse(type %in% c("categorical", "ordinal"), "factor", "numeric"),
    stringsAsFactors = FALSE
  )
}

#' @export
print.qes_crosswalk <- function(x, ...) {
  targets <- attr(x, "targets", exact = TRUE)
  spec <- attr(x, "spec_object", exact = TRUE)
  if (length(targets) == 1L && !is.null(spec) && identical(attr(x, "view_level", exact = TRUE), "row") &&
      "grade_reason" %in% names(x)) {
    cat(.spec_reference_md(lang = attr(x, "lang", exact = TRUE) %||% "en", spec = spec,
                           targets = targets, header = FALSE, studies = unique(x$study)))
    return(invisible(x))
  }
  df <- x
  class(df) <- "data.frame"
  attr(df, "spec_object") <- NULL
  attr(df, "targets") <- NULL
  attr(df, "qes_spec") <- NULL
  attr(df, "lang") <- NULL
  attr(df, "view_level") <- NULL
  print(df, ...)
  invisible(x)
}

# ---- the generated reference -----------------------------------------------------------

# Fixed phrases of the reference, English and French (\u escapes).
.qes_ref_text <- list(
  # the colon of "label: text" (French typography: a no-break space before)
  colon = c(en = ": ", fr = "\u00a0: "),
  title_note = c(
    en = "This reference is generated from the harmonization spec shipped with qesR: version %s of %s, content hash %s. It is **experimental**: targets, grades and mappings are reviewed study by study and may change. Nothing on this page is written by hand; `qes_spec()` returns the same information as data frames.",
    fr = "Cette r\u00e9f\u00e9rence est g\u00e9n\u00e9r\u00e9e \u00e0 partir de la sp\u00e9cification d'harmonisation fournie avec qesR\u00a0: version %s du %s, empreinte du contenu %s. Elle est **exp\u00e9rimentale**\u00a0: les cibles, les niveaux de comparabilit\u00e9 et les appariements sont r\u00e9vis\u00e9s \u00e9tude par \u00e9tude et peuvent changer. Rien sur cette page n'est \u00e9crit \u00e0 la main\u00a0; `qes_spec()` renvoie les m\u00eames informations sous forme de tableaux."
  ),
  how_title = c(en = "How to read this reference", fr = "Comment lire cette r\u00e9f\u00e9rence"),
  how = c(
    en = "Each target is one question stimulus: a different wording, scale, timing or format makes another target; a pooled variable (last chapter) combines targets into one column and records which one each value comes from. For each study, the coverage table gives the source variable and its wave, the comparability grade of the study's question against the target's anchor question and the reason for it, the instrument (the item format), the levels the question offered, the question wording (or, when the wording cannot be shipped, the document and page that give it), the filter question and what each of its codes means, the study's recommended weight (marked when it needs review: qes_harmonize() does not apply it, and its weight columns are NA) and whether \"don't know\" was offered.",
    fr = "Chaque cible correspond \u00e0 un seul stimulus de question\u00a0: une formulation, une \u00e9chelle, un moment ou un format diff\u00e9rent donne une autre cible\u00a0; une variable regroup\u00e9e (dernier chapitre) r\u00e9unit des cibles en une seule colonne et indique de laquelle vient chaque valeur. Pour chaque \u00e9tude, le tableau de couverture donne la variable source et sa vague, le niveau de comparabilit\u00e9 de la question de l'\u00e9tude par rapport \u00e0 la question d'ancrage de la cible et sa raison, l'instrument (le format de la question), les niveaux offerts, le libell\u00e9 de la question (ou, quand il ne peut pas \u00eatre fourni, le document et la page qui le donnent), la question filtre et le sens de chacun de ses codes, la pond\u00e9ration recommand\u00e9e de l'\u00e9tude (signal\u00e9e quand elle est \u00e0 r\u00e9viser\u00a0: qes_harmonize() ne l'applique pas, et ses colonnes de pond\u00e9ration valent NA) et si \u00ab\u00a0je ne sais pas\u00a0\u00bb \u00e9tait offert."
  ),
  grades_title = c(en = "Comparability grades", fr = "Niveaux de comparabilit\u00e9"),
  grade_identical = c(
    en = "the same stem in every language fielded, the same options, the same don't-know option, universe and mode family as the anchor question;",
    fr = "le m\u00eame \u00e9nonc\u00e9 dans chaque langue, les m\u00eames options, la m\u00eame option \u00ab\u00a0je ne sais pas\u00a0\u00bb, le m\u00eame univers et le m\u00eame mode que la question d'ancrage\u00a0;"
  ),
  grade_comparable = c(
    en = "the same construct and stimulus; differences (a temporal adverb, option order, whether don't know is offered, which minor parties are listed) are not expected to move the shares of the common levels;",
    fr = "le m\u00eame concept et le m\u00eame stimulus\u00a0; les diff\u00e9rences (un adverbe de temps, l'ordre des options, l'offre de \u00ab\u00a0je ne sais pas\u00a0\u00bb, les petits partis nomm\u00e9s) ne devraient pas modifier les proportions des niveaux communs\u00a0;"
  ),
  grade_approximate = c(
    en = "the same construct, but a format, filter or mode expected to move the shares; `qes_harmonize(min_grade = \"comparable\")` sets these cells to `NA` (reason `below_grade`);",
    fr = "le m\u00eame concept, mais un format, un filtre ou un mode qui devrait modifier les proportions\u00a0; `qes_harmonize(min_grade = \"comparable\")` met ces cellules \u00e0 `NA` (motif `below_grade`)\u00a0;"
  ),
  grade_not_comparable = c(
    en = "another construct, or a source that must not be used: recorded here, never mapped.",
    fr = "un autre concept, ou une source \u00e0 ne pas utiliser\u00a0: consign\u00e9e ici, jamais appari\u00e9e."
  ),
  zeros_title = c(en = "Structural zeros", fr = "Z\u00e9ros structurels"),
  zeros = c(
    en = "A level that a study's question did not offer (a party missing from its list, say) is a *structural zero*: its share in that study is 0 because nobody could choose it, not because nobody supported it. In the coverage tables such levels are listed after \"not offered\". `qes_provenance(x, level = \"cell\")$levels_not_offered` gives them for harmonized data.",
    fr = "Un niveau que la question d'une \u00e9tude n'offrait pas (un parti absent de sa liste, par exemple) est un *z\u00e9ro structurel*\u00a0: sa part dans cette \u00e9tude est nulle parce que personne ne pouvait le choisir, et non parce que personne ne l'appuyait. Dans les tableaux de couverture, ces niveaux suivent la mention \u00ab\u00a0non offerts\u00a0\u00bb. `qes_provenance(x, level = \"cell\")$levels_not_offered` les donne pour les donn\u00e9es harmonis\u00e9es."
  ),
  missing_title = c(en = "Why a value is missing", fr = "Pourquoi une valeur manque"),
  missing = c(
    en = "Every missing value of a target carries one of these reasons (`qes_harmonize(missing = \"reasons\")` adds them as `<target>__na` columns):",
    fr = "Chaque valeur manquante d'une cible porte l'un de ces motifs (`qes_harmonize(missing = \"reasons\")` les ajoute dans des colonnes `<cible>__na`)\u00a0:"
  ),
  targets_title = c(en = "Targets", fr = "Cibles"),
  family = c(en = "Family", fr = "Famille"),
  type = c(en = "type", fr = "type"),
  timing = c(en = "timing", fr = "moment"),
  status = c(en = "status", fr = "statut"),
  added_in = c(en = "added in spec", fr = "ajout\u00e9e dans la sp\u00e9cification"),
  range = c(en = "Valid range", fr = "Plage valide"),
  levels = c(en = "Levels", fr = "Niveaux"),
  code = c(en = "Code", fr = "Code"),
  name = c(en = "Name", fr = "Nom"),
  label = c(en = "Label", fr = "\u00c9tiquette"),
  coverage = c(en = "Coverage", fr = "Couverture"),
  study = c(en = "Study", fr = "\u00c9tude"),
  source = c(en = "Source", fr = "Source"),
  grade = c(en = "Grade", fr = "Niveau"),
  reason = c(en = "Reason", fr = "Raison"),
  instrument = c(en = "Instrument", fr = "Instrument"),
  offered = c(en = "Levels offered", fr = "Niveaux offerts"),
  wording = c(en = "Wording", fr = "Libell\u00e9"),
  gate = c(en = "Filter", fr = "Filtre"),
  weight = c(en = "Weight", fr = "Pond\u00e9ration"),
  dk = c(en = "Don't know", fr = "Ne sait pas"),
  not_offered = c(en = "not offered", fr = "non offerts"),
  anchor = c(en = "anchor", fr = "ancrage"),
  draft = c(en = "draft", fr = "provisoire"),
  review = c(en = "in review", fr = "en r\u00e9vision"),
  status_note = c(
    en = "A row with no mark is signed off by a reviewer (status stable; to date, by an automated double review against the original files and documents, not a human review), and `qes_harmonize()` applies it by default. A row marked \"in review\" was checked against the original files and documents but is not signed off (the `review_note` column of `qes_spec(\"crosswalk\")` says why it is held); a row marked \"draft\" is not yet checked. `qes_harmonize()` applies these only with `include_draft = TRUE`.",
    fr = "Une ligne sans mention est approuv\u00e9e par un r\u00e9viseur (statut stable\u00a0; jusqu'ici, par une double r\u00e9vision automatis\u00e9e sur les fichiers et documents originaux, et non par une r\u00e9vision humaine), et `qes_harmonize()` l'applique par d\u00e9faut. Une ligne marqu\u00e9e \u00ab\u00a0en r\u00e9vision\u00a0\u00bb a \u00e9t\u00e9 v\u00e9rifi\u00e9e sur les fichiers et documents originaux mais n'est pas approuv\u00e9e (la colonne `review_note` de `qes_spec(\"crosswalk\")` dit pourquoi elle est retenue)\u00a0; une ligne marqu\u00e9e \u00ab\u00a0provisoire\u00a0\u00bb n'est pas encore v\u00e9rifi\u00e9e. `qes_harmonize()` n'applique celles-ci qu'avec `include_draft = TRUE`."
  ),
  document = c(en = "document %s, %s", fr = "document %s, %s"),
  none = c(en = "No study has a question for this target yet.", fr = "Aucune \u00e9tude n'a encore de question pour cette cible."),
  not_used = c(en = "Not used", fr = "Non utilis\u00e9"),
  history = c(en = "History", fr = "Historique"),
  all_targets = c(en = "all targets", fr = "toutes les cibles"),
  # the website's versions of the reference and the coverage grid
  # (site = TRUE): no spec version, hash, review process or history
  site_title_note = c(
    en = "`qes_spec()` returns the same information as data frames, and `qes_provenance(x, level = \"spec\")` the version of the rules that produced harmonized data.",
    fr = "`qes_spec()` renvoie les m\u00eames informations sous forme de tableaux, et `qes_provenance(x, level = \"spec\")` la version des r\u00e8gles qui a produit des donn\u00e9es harmonis\u00e9es."
  ),
  site_how = c(
    en = "Each target is one question stimulus: a different wording, scale, timing or format makes another target; a pooled variable (last chapter) combines targets into one column and records which one each value comes from. For each study, the coverage table of a target gives:",
    fr = "Chaque cible correspond \u00e0 un seul stimulus de question\u00a0: une formulation, une \u00e9chelle, un moment ou un format diff\u00e9rent donne une autre cible\u00a0; une variable regroup\u00e9e (dernier chapitre) r\u00e9unit des cibles en une seule colonne et indique de laquelle vient chaque valeur. Pour chaque \u00e9tude, le tableau de couverture d'une cible donne\u00a0:"
  ),
  site_how_items = list(
    en = c("**Source**: the variable and the wave that asked it;",
           "**Grade** and **Reason**: how comparable the study's question is to the target's anchor question, and why;",
           "**Instrument** and **Levels offered**: the item format and the answers it offered (the others are structural zeros);",
           "**Wording**: the question text, or the document and page that give it;",
           "**Filter**: the filter question, and what each of its codes means;",
           "**Weight**: the study's recommended weight for that wave (marked when it is not usable yet: its weight columns are then `NA`);",
           "**Don't know**: whether \"don't know\" was offered."),
    fr = c("**Source**\u00a0: la variable et la vague qui l'a pos\u00e9e\u00a0;",
           "**Niveau** et **Raison**\u00a0: \u00e0 quel point la question de l'\u00e9tude est comparable \u00e0 la question d'ancrage de la cible, et pourquoi\u00a0;",
           "**Instrument** et **Niveaux offerts**\u00a0: le format de la question et les r\u00e9ponses offertes (les autres sont des z\u00e9ros structurels)\u00a0;",
           "**Libell\u00e9**\u00a0: le texte de la question, ou le document et la page qui le donnent\u00a0;",
           "**Filtre**\u00a0: la question filtre, et le sens de chacun de ses codes\u00a0;",
           "**Pond\u00e9ration**\u00a0: la pond\u00e9ration recommand\u00e9e de l'\u00e9tude pour cette vague (signal\u00e9e quand elle n'est pas encore utilisable\u00a0: ses colonnes de pond\u00e9ration valent alors `NA`)\u00a0;",
           "**Ne sait pas**\u00a0: si \u00ab\u00a0je ne sais pas\u00a0\u00bb \u00e9tait offert.")
  ),
  site_status_note = c(
    en = "A row marked *awaiting sign-off* is applied only if you ask for it (`include_draft = TRUE`).",
    fr = "Une ligne marqu\u00e9e *en attente d'approbation* n'est appliqu\u00e9e que si vous la demandez (`include_draft = TRUE`)."
  ),
  site_pending = c(en = "awaiting sign-off", fr = "en attente d'approbation"),
  site_weight_review = c(en = "not usable yet", fr = "pas encore utilisable"),
  site_licence = c(
    en = "The wording and labels of `%s` quoted on this page come from *%s* (%s, %s, <%s>) and keep its licence, [%s](%s). See [Citing qesR and the studies](%s) for the attribution.",
    fr = "Les libell\u00e9s et les \u00e9tiquettes de `%s` cit\u00e9s sur cette page viennent de *%s* (%s, %s, <%s>) et gardent sa licence, [%s](%s). Voir [Citer qesR et les \u00e9tudes](%s) pour l'attribution."
  ),
  site_citations = c(en = "citations.html#licences-and-attribution", fr = "fr-citations.html#licences-et-attribution"),
  site_cov_note = c(
    en = "Each cell gives the comparability grade of the study's question for the target, against the target's anchor question; a dash means the study has no question for the target. A target's name links to its section of the [variable reference](%s), which gives the question, its wording, the levels it offered and the reason for its grade. `qes_spec()` returns the same grid as a data frame.",
    fr = "Chaque cellule donne le niveau de comparabilit\u00e9 de la question de l'\u00e9tude pour la cible, par rapport \u00e0 la question d'ancrage de la cible\u00a0; un tiret signifie que l'\u00e9tude n'a pas de question pour la cible. Le nom d'une cible m\u00e8ne \u00e0 sa section de la [r\u00e9f\u00e9rence des variables](%s), qui donne la question, son libell\u00e9, les niveaux offerts et la raison de son niveau. `qes_spec()` renvoie la m\u00eame grille sous forme de tableau."
  ),
  site_cov_pending = c(
    en = "\u2020 Awaiting sign-off: applied only if you ask for it (`qes_harmonize(include_draft = TRUE)`).",
    fr = "\u2020 En attente d'approbation\u00a0: appliqu\u00e9e seulement si vous la demandez (`qes_harmonize(include_draft = TRUE)`)."
  ),
  site_cov_studies_note = c(
    en = "`n` is the number of respondents of each wave. A weight marked *not usable yet* is not documented well enough to use: its weight columns are `NA`. `qes_design()` uses the weight of the wave each target came from.",
    fr = "`n` est le nombre de r\u00e9pondants de chaque vague. Une pond\u00e9ration marqu\u00e9e *pas encore utilisable* n'est pas assez document\u00e9e pour \u00eatre utilis\u00e9e\u00a0: ses colonnes de pond\u00e9ration valent `NA`. `qes_design()` utilise la pond\u00e9ration de la vague d'o\u00f9 vient chaque cible."
  ),
  site_not_in_spec = c(
    en = "Not harmonized: %s. `get_qes()` reads them.",
    fr = "Non harmonis\u00e9es\u00a0: %s. `get_qes()` les lit."
  ),
  site_not_in_spec_in = c(
    en = "Not harmonized: %s (their respondents are in %s). `get_qes()` reads them.",
    fr = "Non harmonis\u00e9es\u00a0: %s (leurs r\u00e9pondants sont dans %s). `get_qes()` les lit."
  ),
  # the relaxed layer (.qes_relaxed_reference_md)
  relaxed_title = c(en = "Relaxed harmonization: qes_decon()", fr = "Harmonisation souple\u00a0: qes_decon()"),
  relaxed_intro = c(
    en = "`qes_decon()` puts one concept in one column for every study, even when the wording or the answer options differ, with coarse common categories, under plain column names. It trades exactness for coverage: a relaxed column has no grade and does not claim that two studies asked the same question; the targets above keep the strict, graded versions. A column is built from a strict target or a pooled variable where one exists, recoded into the column's categories where needed, and from relaxed mappings of the studies' own questions where the strict layer has none. A relaxed mapping is applied once a reviewer has signed it off (status `stable`).",
    fr = "`qes_decon()` met un concept dans une seule colonne pour chaque \u00e9tude, m\u00eame quand le libell\u00e9 ou les choix de r\u00e9ponse diff\u00e8rent, avec des cat\u00e9gories communes larges, sous des noms de colonnes simples. Elle \u00e9change l'exactitude contre la couverture\u00a0: une colonne souple n'a pas de niveau de comparabilit\u00e9 et ne pr\u00e9tend pas que deux \u00e9tudes ont pos\u00e9 la m\u00eame question\u00a0; les cibles ci-dessus gardent les versions strictes, avec leurs niveaux. Une colonne est construite \u00e0 partir d'une cible stricte ou d'une variable regroup\u00e9e quand il en existe une, recod\u00e9e au besoin dans les cat\u00e9gories de la colonne, et d'appariements souples des questions propres aux \u00e9tudes l\u00e0 o\u00f9 la couche stricte n'en a pas. Un appariement souple est appliqu\u00e9 une fois approuv\u00e9 par un r\u00e9viseur (statut `stable`)."
  ),
  relaxed_base = c(en = "Base", fr = "Base"),
  relaxed_only = c(en = "relaxed mappings only", fr = "appariements souples seulement"),
  relaxed_how = c(en = "How it is relaxed", fr = "Comment elle est assouplie"),
  relaxed_rows = c(en = "Relaxed mappings", fr = "Appariements souples"),
  relaxed_none = c(en = "None: every study's values come from the base.", fr = "Aucun\u00a0: les valeurs de chaque \u00e9tude viennent de la base."),
  relaxed_recode = c(en = "Recode", fr = "Recodage"),
  relaxed_use = c(en = "Use", fr = "Usage"),
  relaxed_notes = c(en = "Notes", fr = "Notes"),
  relaxed_replaces = c(en = "replaces the base", fr = "remplace la base"),
  relaxed_fills = c(en = "fills the study", fr = "compl\u00e8te l'\u00e9tude"),
  relaxed_static = c(en = "one value per respondent", fr = "une valeur par personne"),
  relaxed_wave = c(en = "on the wave that asked it", fr = "sur la vague qui l'a pos\u00e9e"),
  # the coverage grid (.spec_coverage_md)
  cov_note = c(
    en = "This grid is generated from the harmonization spec shipped with qesR: version %s of %s, content hash %s. It is **experimental**. Each cell gives the comparability grade of the study's question for the target, against the target's anchor question; a dash means the study has no question for the target in the spec. A target's name links to its section of the [harmonization reference](%s), which gives the question, its wording, the levels it offered and the reason for its grade. `qes_spec()` returns the same grid as a data frame.",
    fr = "Cette grille est g\u00e9n\u00e9r\u00e9e \u00e0 partir de la sp\u00e9cification d'harmonisation fournie avec qesR\u00a0: version %s du %s, empreinte du contenu %s. Elle est **exp\u00e9rimentale**. Chaque cellule donne le niveau de comparabilit\u00e9 de la question de l'\u00e9tude pour la cible, par rapport \u00e0 la question d'ancrage de la cible\u00a0; un tiret signifie que l'\u00e9tude n'a pas de question pour la cible dans la sp\u00e9cification. Le nom d'une cible m\u00e8ne \u00e0 sa section de la [r\u00e9f\u00e9rence de l'harmonisation](%s), qui donne la question, son libell\u00e9, les niveaux offerts et la raison de son niveau. `qes_spec()` renvoie la m\u00eame grille sous forme de tableau."
  ),
  cov_grid_title = c(en = "Targets by study", fr = "Cibles par \u00e9tude"),
  cov_studies_title = c(en = "Studies", fr = "\u00c9tudes"),
  cov_target = c(en = "Target", fr = "Cible"),
  cov_wave_note = c(
    en = "For a study with more than one wave, the wave that asked the question is in parentheses.",
    fr = "Pour une \u00e9tude de plusieurs vagues, la vague qui a pos\u00e9 la question est entre parenth\u00e8ses."
  ),
  cov_zero_note = c(
    en = "\\* The study's question did not offer every level of the target: the levels it did not offer are structural zeros, listed in the reference.",
    fr = "\\* La question de l'\u00e9tude n'offrait pas tous les niveaux de la cible\u00a0: les niveaux non offerts sont des z\u00e9ros structurels, list\u00e9s dans la r\u00e9f\u00e9rence."
  ),
  cov_status_all = c(
    en = "All %d cells use crosswalk rows that are checked against the original files and documents but not yet signed off by a reviewer; `qes_harmonize()` applies them only with `include_draft = TRUE`.",
    fr = "Les %d cellules utilisent toutes des lignes de correspondance v\u00e9rifi\u00e9es sur les fichiers et documents originaux, mais pas encore approuv\u00e9es par un r\u00e9viseur\u00a0; `qes_harmonize()` ne les applique qu'avec `include_draft = TRUE`."
  ),
  cov_draft_all = c(
    en = "All %d cells use draft crosswalk rows, not yet checked against the original files and documents; `qes_harmonize()` applies them only with `include_draft = TRUE`.",
    fr = "Les %d cellules utilisent toutes des lignes de correspondance provisoires, pas encore v\u00e9rifi\u00e9es sur les fichiers et documents originaux\u00a0; `qes_harmonize()` ne les applique qu'avec `include_draft = TRUE`."
  ),
  cov_draft_note = c(
    en = "%d of the %d cells use draft crosswalk rows, not yet checked against the original files and documents; `qes_harmonize()` applies them only with `include_draft = TRUE`.",
    fr = "%d des %d cellules utilisent des lignes de correspondance provisoires, pas encore v\u00e9rifi\u00e9es sur les fichiers et documents originaux\u00a0; `qes_harmonize()` ne les applique qu'avec `include_draft = TRUE`."
  ),
  semi = c(en = "; ", fr = "\u00a0; "),
  cov_status_note = c(
    en = "%d of the %d cells use crosswalk rows that are checked against the original files and documents but not yet signed off by a reviewer; `qes_harmonize()` applies them only with `include_draft = TRUE`.",
    fr = "%d des %d cellules utilisent des lignes de correspondance v\u00e9rifi\u00e9es sur les fichiers et documents originaux, mais pas encore approuv\u00e9es par un r\u00e9viseur\u00a0; `qes_harmonize()` ne les applique qu'avec `include_draft = TRUE`."
  ),
  cov_stable_all = c(
    en = "All %d cells use crosswalk rows signed off by a reviewer (status stable), which `qes_harmonize()` applies by default; the crosswalk's `reviewed_by` says who or what reviewed each row (to date, an automated double review against the original files and documents, not a human review).",
    fr = "Les %d cellules utilisent toutes des lignes de correspondance approuv\u00e9es par un r\u00e9viseur (statut stable), que `qes_harmonize()` applique par d\u00e9faut\u00a0; la colonne `reviewed_by` de la table de correspondance dit qui ou quoi a r\u00e9vis\u00e9 chaque ligne (jusqu'ici, une double r\u00e9vision automatis\u00e9e sur les fichiers et documents originaux, et non une r\u00e9vision humaine)."
  ),
  cov_stable_note = c(
    en = "%d of the %d cells use crosswalk rows signed off by a reviewer (status stable), which `qes_harmonize()` applies by default; the crosswalk's `reviewed_by` says who or what reviewed each row (to date, an automated double review against the original files and documents, not a human review).",
    fr = "%d des %d cellules utilisent des lignes de correspondance approuv\u00e9es par un r\u00e9viseur (statut stable), que `qes_harmonize()` applique par d\u00e9faut\u00a0; la colonne `reviewed_by` de la table de correspondance dit qui ou quoi a r\u00e9vis\u00e9 chaque ligne (jusqu'ici, une double r\u00e9vision automatis\u00e9e sur les fichiers et documents originaux, et non une r\u00e9vision humaine)."
  ),
  cov_waves = c(en = "Waves and recommended weights", fr = "Vagues et pond\u00e9rations recommand\u00e9es"),
  cov_n_targets = c(en = "Targets", fr = "Cibles"),
  cov_no_weight = c(en = "no recommended weight", fr = "aucune pond\u00e9ration recommand\u00e9e"),
  # the wave "*" of pooled polls, and their waves in the studies table
  each_poll = c(en = "each poll", fr = "chaque sondage"),
  any_wave = c(en = "any wave", fr = "toute vague"),
  cov_polls = c(en = "%d poll waves, %s to %s (n = %s to %s each)%s%s",
                fr = "%d vagues de sondage, de %s \u00e0 %s (n = %s \u00e0 %s chacune)%s%s"),
  cov_weight_review = c(en = "needs review, not applied", fr = "\u00e0 r\u00e9viser, non appliqu\u00e9e"),
  cov_studies_note = c(
    en = "`n` is the number of respondents of each wave. A weight that needs review is not applied: `qes_harmonize()` returns `NA` for it until its documentation is checked. `qes_design()` uses the weight of the wave each target came from.",
    fr = "`n` est le nombre de r\u00e9pondants de chaque vague. Une pond\u00e9ration \u00e0 r\u00e9viser n'est pas appliqu\u00e9e\u00a0: `qes_harmonize()` renvoie `NA` pour cette pond\u00e9ration tant que sa documentation n'est pas v\u00e9rifi\u00e9e. `qes_design()` utilise la pond\u00e9ration de la vague d'o\u00f9 vient chaque cible."
  ),
  # the pooled variables (R/hz-pool.R)
  pooled_title = c(en = "Pooled variables", fr = "Variables regroup\u00e9es"),
  pooled_intro = c(
    en = "A pooled variable is one column for every study that pools several targets, its members: `vote_choice` pools the reported vote and the vote intentions, `sov_support` the referendum wordings, `pol_interest` the interest scales. The members stay targets, each one stimulus; the pooled column records, row by row, which member its value comes from (`<pooled>__type`), that member's grade (`<pooled>__grade`, never raised; a lossy transform caps it at approximate) and its item (`<pooled>__item`, `study:wave:source variables`). `qes_harmonize(targets = \"vote_choice\")` returns the pooled column and its companions; `types = list(vote_choice = \"recall\")` keeps some members only.",
    fr = "Une variable regroup\u00e9e est une seule colonne pour toutes les \u00e9tudes qui regroupe plusieurs cibles, ses membres\u00a0: `vote_choice` regroupe le vote d\u00e9clar\u00e9 et les intentions de vote, `sov_support` les libell\u00e9s r\u00e9f\u00e9rendaires, `pol_interest` les \u00e9chelles d'int\u00e9r\u00eat. Les membres restent des cibles, chacune un seul stimulus\u00a0; la colonne regroup\u00e9e indique, ligne par ligne, de quel membre vient sa valeur (`<variable>__type`), le niveau de comparabilit\u00e9 de ce membre (`<variable>__grade`, jamais relev\u00e9\u00a0; une transformation avec perte le plafonne \u00e0 approximate) et sa question (`<variable>__item`, `\u00e9tude:vague:variables sources`). `qes_harmonize(targets = \"vote_choice\")` renvoie la colonne regroup\u00e9e et ses compagnes\u00a0; `types = list(vote_choice = \"recall\")` ne garde que certains membres."
  ),
  pooled_rule = c(
    en = "How a row gets its value: the members of the requested types are tried in order of precedence. The first member whose cell has a value, or a missing value that is an answer (don't know, refused, did not vote, ...), sets the row. A member that did not ask the respondent (not in the wave, not asked, not reviewed, below the grade, routed out, system missing, a code that straddles levels) passes to the next. When every member passes, the row is `NA` with the reason of the first usable member that has a row in the study and wave, else of the first member that has a row. In the respondent layout (one row per respondent), a study's values come from one wave: that of the first member it applies, so that one weight column fits them; the long layout (`layout = \"long\"`, one row per respondent and wave) keeps every wave.",
    fr = "Comment une ligne re\u00e7oit sa valeur\u00a0: les membres des types demand\u00e9s sont essay\u00e9s par ordre de priorit\u00e9. Le premier membre dont la cellule a une valeur, ou une valeur manquante qui est une r\u00e9ponse (ne sait pas, refus, n'a pas vot\u00e9, ...), d\u00e9termine la ligne. Un membre qui n'a pas interrog\u00e9 la personne (absente de la vague, question non pos\u00e9e, ligne non approuv\u00e9e, sous le niveau demand\u00e9, \u00e9cart\u00e9e par un filtre, valeur manquante syst\u00e8me, code \u00e0 cheval sur plusieurs niveaux) passe au suivant. Quand tous les membres passent, la ligne est `NA` avec le motif du premier membre utilisable qui a une ligne dans l'\u00e9tude et la vague, sinon du premier membre qui a une ligne. En disposition par r\u00e9pondant (une ligne par personne), les valeurs d'une \u00e9tude viennent d'une seule vague\u00a0: celle du premier membre qu'elle applique, pour qu'une seule colonne de pond\u00e9ration leur convienne\u00a0; la disposition longue (`layout = \"long\"`, une ligne par personne et par vague) garde toutes les vagues."
  ),
  members = c(en = "Members", fr = "Membres"),
  type_name = c(en = "Type", fr = "Type"),
  member = c(en = "Member (target)", fr = "Membre (cible)"),
  precedence = c(en = "Precedence", fr = "Priorit\u00e9"),
  default = c(en = "Default", fr = "Par d\u00e9faut"),
  transform = c(en = "Transform", fr = "Transformation"),
  cap = c(en = "Grade cap", fr = "Plafond"),
  yes = c(en = "yes", fr = "oui"),
  no = c(en = "no", fr = "non"),
  pooled_used = c(en = "Respondent layout uses", fr = "Disposition par r\u00e9pondant"),
  pooled_coverage_note = c(
    en = "Each cell gives the member's grade in the study (capped by its grade cap) and the wave that asked it; a dash means the study has no question for the member. The last column is the member the respondent layout takes the study's values from, among the default members (the long layout uses every wave).",
    fr = "Chaque cellule donne le niveau du membre dans l'\u00e9tude (plafonn\u00e9) et la vague qui a pos\u00e9 la question\u00a0; un tiret signifie que l'\u00e9tude n'a pas de question pour le membre. La derni\u00e8re colonne donne le membre dont la disposition par r\u00e9pondant tire les valeurs de l'\u00e9tude, parmi les membres par d\u00e9faut (la disposition longue utilise toutes les vagues)."
  ),
  cov_pooled_title = c(en = "Pooled variables by study", fr = "Variables regroup\u00e9es par \u00e9tude"),
  cov_pooled_note = c(
    en = "Each cell gives the member type a pooled variable takes a study's values from in the respondent layout, and its grade; a dash means no member of its default types has a question in the study.",
    fr = "Chaque cellule donne le type de membre dont une variable regroup\u00e9e tire les valeurs d'une \u00e9tude en disposition par r\u00e9pondant, et son niveau\u00a0; un tiret signifie qu'aucun membre de ses types par d\u00e9faut n'a de question dans l'\u00e9tude."
  ),
  cov_not_in_spec = c(
    en = "Studies of the catalog that are not in the spec yet: %s. `get_qes()` reads them; `qes_harmonize()` does not cover them yet.",
    fr = "\u00c9tudes du catalogue qui ne sont pas encore dans la sp\u00e9cification\u00a0: %s. `get_qes()` les lit\u00a0; `qes_harmonize()` ne les couvre pas encore."
  )
)

.qes_rt <- function(key, lang) {
  .qes_ref_text[[key]][[if (identical(lang, "fr")) "fr" else "en"]]
}

# A markdown table cell: no pipes or line breaks.
.qes_md_cell <- function(x) {
  x <- enc2utf8(ifelse(is.na(x), "", as.character(x)))
  # byte-wise on UTF-8 text, then marked again: under a C locale gsub() would
  # otherwise return the text unmarked (returned text never depends on the
  # locale, rule P4)
  x <- gsub("|", "\\|", x, fixed = TRUE, useBytes = TRUE)
  x <- gsub("[\r\n]+", " ", x, useBytes = TRUE)
  Encoding(x) <- "UTF-8"
  x
}

.qes_md_table <- function(header, rows) {
  if (length(rows) == 0L) return(character(0))
  c(
    paste0("| ", paste(header, collapse = " | "), " |"),
    paste0("|", paste(rep("---", length(header)), collapse = "|"), "|"),
    vapply(rows, function(r) paste0("| ", paste(.qes_md_cell(r), collapse = " | "), " |"), character(1))
  )
}

# The reference of the spec as markdown text, in `lang`: an introduction
# (grades, structural zeros, NA reasons) with `header = TRUE`, then one
# section per target, grouped by block: definition, levels, coverage table
# and history. `targets` and `studies` restrict it (print() of a
# single-target crosswalk view uses it). `site = TRUE` gives the website's
# version (the reference vignettes): the same targets, tables and anchors,
# without the spec's version, hash and review process, the history of each
# target or the names of the spec's files, and with a short licence notice
# that links to the citations page.
.spec_reference_md <- function(lang = "en", spec = NULL, targets = NULL, header = TRUE, studies = NULL,
                               site = FALSE) {
  lang <- if (identical(lang, "fr")) "fr" else "en"
  spec <- if (inherits(spec, "qes_spec")) spec else .qes_spec_get(spec, "none")
  site <- isTRUE(site)
  t_ <- function(key) .qes_rt(key, lang)
  txt <- if (site) .qes_site_text else identity
  cl <- t_("colon")
  tg <- spec$tables$targets
  xw <- spec$tables$crosswalk
  ch <- spec$tables$changes
  out <- character(0)
  if (isTRUE(header) && site) {
    mapped <- xw[!is.na(xw$rule) & xw$rule != "none", , drop = FALSE]
    out <- c(out, t_("site_title_note"), "", paste("##", t_("how_title")), "", t_("site_how"), "",
             paste("-", t_("site_how_items")), "",
             if (any(mapped$status %in% c("draft", "review"))) c(t_("site_status_note"), ""))
    for (st in intersect(.qes_study_codes(), unique(xw$study))) {
      notice <- .qes_licence_notice_site(st, lang)
      if (!is.null(notice)) out <- c(out, notice, "")
    }
  } else if (isTRUE(header)) {
    out <- c(out, sprintf(t_("title_note"), spec$version, format(as.Date(spec$date)), paste0("`", spec$hash, "`")), "",
             paste("##", t_("how_title")), "", t_("how"), "", t_("status_note"), "")
    # the wording and labels quoted from a study whose metadata is not CC0
    # carry its licence and attribution (qes2022, CC BY-NC 4.0)
    for (st in intersect(.qes_study_codes(), unique(xw$study))) {
      notice <- .qes_licence_notice(st, lang)
      if (!is.null(notice)) out <- c(out, notice, "")
    }
  }
  if (isTRUE(header)) {
    out <- c(out, paste("###", t_("grades_title")), "")
    for (g in c("identical", "comparable", "approximate", "not_comparable")) {
      out <- c(out, sprintf("- **`%s`** (%s)%s%s", g, .qes_enum_label("grade", g, lang), cl, t_(paste0("grade_", g))))
    }
    out <- c(out, "", paste("###", t_("zeros_title")), "", t_("zeros"), "",
             paste("###", t_("missing_title")), "", t_("missing"), "")
    reasons <- .qes_hz_reason_levels()
    out <- c(out, .qes_md_table(c(t_("code"), t_("label")),
                                lapply(reasons, function(r) c(paste0("`", r, "`"), .qes_enum_label("missing_type", r, lang)))), "")
    out <- c(out, paste("##", t_("targets_title")), "")
  }
  sel <- if (is.null(targets)) tg$target else intersect(tg$target, targets)
  blocks <- .qes_enum("block")$value
  sel <- sel[order(match(tg$block[match(sel, tg$target)], blocks), match(sel, tg$target))]
  current_block <- NA_character_
  studies_order <- .qes_study_codes()
  for (t in sel) {
    j <- match(t, tg$target)
    if (isTRUE(header) && !identical(tg$block[j], current_block)) {
      current_block <- tg$block[j]
      out <- c(out, paste("###", .qes_enum_label("block", current_block, lang)), "")
    }
    h <- if (isTRUE(header)) "####" else "##"
    # with the header (the reference page), each target's section has a
    # fixed id, "target-<name>", so other pages (the coverage grid) can
    # link to it
    id <- if (isTRUE(header)) sprintf(" {#target-%s}", t) else ""
    out <- c(out, sprintf("%s `%s`%s%s%s", h, t, cl, .qes_pick_lang(tg[j, , drop = FALSE], "label", lang), id), "",
             txt(.qes_pick_lang(tg[j, , drop = FALSE], "description", lang)), "")
    facts <- if (site) {
      sprintf("%s `%s` \u00b7 %s %s \u00b7 %s %s",
              t_("family"), tg$family[j], t_("type"), .qes_enum_label("target_type", tg$type[j], lang),
              t_("timing"), .qes_enum_label("target_timing", tg$target_timing[j], lang))
    } else {
      sprintf("%s `%s` \u00b7 %s %s \u00b7 %s %s \u00b7 %s %s \u00b7 %s %s",
              t_("family"), tg$family[j], t_("type"), .qes_enum_label("target_type", tg$type[j], lang),
              t_("timing"), .qes_enum_label("target_timing", tg$target_timing[j], lang),
              t_("status"), .qes_enum_label("target_status", tg$status[j], lang),
              t_("added_in"), tg$added_in[j])
    }
    out <- c(out, facts, "")
    if (!is.na(tg$levels_id[j])) {
      set <- .qes_spec_levels(spec$tables$levels, tg$levels_id[j])
      out <- c(out, paste0("**", t_("levels"), "**"), "",
               .qes_md_table(c(t_("code"), t_("name"), t_("label")),
                             lapply(seq_len(nrow(set)), function(k) c(set$code[k], paste0("`", set$name[k], "`"),
                                                                      set[[paste0("label_", lang)]][k]))), "")
    } else if (!is.na(tg$valid_min[j]) || !is.na(tg$valid_max[j])) {
      out <- c(out, sprintf("**%s**%s%s-%s", t_("range"), cl, .qes_code_chr(tg$valid_min[j]), .qes_code_chr(tg$valid_max[j])), "")
    }
    rows <- xw[xw$target == t, , drop = FALSE]
    if (!is.null(studies)) rows <- rows[rows$study %in% studies, , drop = FALSE]
    rows <- rows[order(match(rows$study, studies_order), rows$wave), , drop = FALSE]
    used <- rows[!is.na(rows$rule) & rows$rule != "none", , drop = FALSE]
    unused <- rows[is.na(rows$rule) | rows$rule == "none", , drop = FALSE]
    out <- c(out, paste0("**", t_("coverage"), "**"), "")
    if (nrow(used) == 0L) {
      out <- c(out, t_("none"), "")
    } else {
      not_off <- .qes_not_offered(spec, used)
      wrec <- .qes_row_weight_rec(spec, used)
      weight <- .qes_weight_cell(wrec$weight_var, wrec$status, lang, site = site)
      gate <- .qes_gate_text(used)
      anchor <- paste(used$study, used$wave, used$source_var, sep = ":") %in% tg$anchor_row[j]
      cells <- lapply(seq_len(nrow(used)), function(k) {
        u <- used[k, , drop = FALSE]
        src <- paste0(paste0("`", .qes_hz_row_vars(u, 1L), "`", collapse = " + "), " (", .qes_wave_label(u$wave, lang, u$study, spec), ")")
        grade <- paste0("`", u$grade, "`", if (anchor[k]) paste0(" (", t_("anchor"), ")") else "",
                        if (u$status %in% c("draft", "review")) {
                          paste0(" (", t_(if (site) "site_pending" else u$status), ")")
                        } else "")
        offered <- gsub(";", ", ", u$levels_offered %|NA|% "", fixed = TRUE)
        if (!is.na(not_off[k]) && nzchar(not_off[k])) {
          offered <- paste0(offered, "; ", t_("not_offered"), cl, gsub(";", ", ", not_off[k], fixed = TRUE))
        }
        wording <- .qes_pick_lang(u, "wording", lang)
        if (is.na(wording) && !is.na(u$wording_ref)) {
          refs <- .qes_split_list(u$wording_ref)
          wording <- paste(vapply(refs, function(r) sprintf(t_("document"), sub(":.*$", "", r), sub("^[^:]*:", "", r)),
                                  character(1)), collapse = "; ")
        }
        c(u$study, src, grade, txt(.qes_pick_lang(u, "grade_reason", lang)), u$instrument, offered,
          wording, gate[k], weight[k], .qes_enum_label("dk_offered", u$dk_offered, lang))
      })
      out <- c(out, .qes_md_table(c(t_("study"), t_("source"), t_("grade"), t_("reason"), t_("instrument"),
                                    t_("offered"), t_("wording"), t_("gate"), t_("weight"), t_("dk")), cells), "")
    }
    if (nrow(unused) > 0L) {
      out <- c(out, paste0("**", t_("not_used"), "**"), "")
      for (k in seq_len(nrow(unused))) {
        u <- unused[k, , drop = FALSE]
        out <- c(out, sprintf("- %s `%s` (%s)%s`%s`. %s", u$study, u$source_var, .qes_wave_label(u$wave, lang, u$study, spec), cl, u$grade,
                              txt(.qes_pick_lang(u, "grade_reason", lang))))
      }
      out <- c(out, "")
    }
    hist <- ch[is.na(ch$targets) | vapply(ch$targets, function(x) t %in% .qes_split_list(x), logical(1)), , drop = FALSE]
    if (nrow(hist) > 0L && !site) {
      out <- c(out, paste0("**", t_("history"), "**"), "")
      for (k in seq_len(nrow(hist))) {
        out <- c(out, sprintf("- %s (%s)%s%s", hist$spec_version[k], format(hist$date[k]), cl,
                              .qes_pick_lang(hist[k, , drop = FALSE], "change", lang)))
      }
      out <- c(out, "")
    }
  }
  if (isTRUE(header) && is.null(targets)) {
    out <- c(out, .qes_pooled_reference_md(spec, lang, studies, site = site))
    out <- c(out, .qes_relaxed_reference_md(spec, lang, studies, site = site))
  }
  paste0(paste(out, collapse = "\n"), "\n")
}

# The short licence notice of the website's reference: where the quoted
# wording of `study` comes from, its licence, and a link to the citations
# page, which gives the attribution in full; NULL when its metadata is CC0
# or does not ship (as .qes_licence_notice()).
.qes_licence_notice_site <- function(study, lang) {
  if (is.null(.qes_licence_notice(study, lang))) return(NULL)
  s <- .qes_catalog()$studies
  s <- s[s$study == study, , drop = FALSE]
  family <- sub(",.*$", "", .qes_split_list(s$authors))
  and <- if (identical(lang, "fr")) " et " else " and "
  authors <- if (length(family) > 1L) {
    paste0(paste(family[-length(family)], collapse = ", "), and, family[length(family)])
  } else {
    family
  }
  sprintf(.qes_rt("site_licence", lang), study, s$title_deposit, authors, s$citation_year,
          paste0("https://doi.org/", s$doi), s$licence, .qes_metadata_licences[[s$licence]],
          .qes_rt("site_citations", lang))
}

# The chapter of the reference on pooled variables (R/hz-pool.R), in
# `lang`: the rule, then one section per pooled variable with its
# definition, levels, members and coverage by study, and its history
# (`site = TRUE`: no status, spec version or history, as .spec_reference_md()).
.qes_pooled_reference_md <- function(spec, lang, studies = NULL, pools = NULL, site = FALSE) {
  site <- isTRUE(site)
  t_ <- function(key) .qes_rt(key, lang)
  cl <- t_("colon")
  pt <- .qes_pool_tables(spec)
  pl <- pt$pooled
  if (!is.null(pools)) pl <- pl[pl$pooled %in% pools, , drop = FALSE]
  if (nrow(pl) == 0L) return(character(0))
  ch <- spec$tables$changes
  xw <- spec$tables$crosswalk
  xw <- xw[!is.na(xw$rule) & xw$rule != "none" & xw$primary %in% TRUE, , drop = FALSE]
  study_cols <- intersect(.qes_study_codes(), unique(xw$study))
  if (!is.null(studies)) study_cols <- intersect(study_cols, studies)
  out <- c(paste("##", t_("pooled_title")), "", t_("pooled_intro"), "", t_("pooled_rule"), "")
  for (j in seq_len(nrow(pl))) {
    p <- pl$pooled[j]
    m <- .qes_pool_members(spec, p)
    out <- c(out, sprintf("### `%s`%s%s {#pooled-%s}", p, cl, .qes_pick_lang(pl[j, , drop = FALSE], "label", lang), p), "",
             .qes_pick_lang(pl[j, , drop = FALSE], "description", lang), "",
             if (site) {
               sprintf("%s %s", t_("type"), .qes_enum_label("target_type", pl$type[j], lang))
             } else {
               sprintf("%s %s \u00b7 %s %s \u00b7 %s %s", t_("type"), .qes_enum_label("target_type", pl$type[j], lang),
                       t_("status"), .qes_enum_label("target_status", pl$status[j], lang), t_("added_in"), pl$added_in[j])
             }, "")
    if (!is.na(pl$levels_id[j])) {
      set <- .qes_spec_levels(spec$tables$levels, pl$levels_id[j])
      out <- c(out, paste0("**", t_("levels"), "**"), "",
               .qes_md_table(c(t_("code"), t_("name"), t_("label")),
                             lapply(seq_len(nrow(set)), function(k) c(set$code[k], paste0("`", set$name[k], "`"),
                                                                      set[[paste0("label_", lang)]][k]))), "")
    } else {
      out <- c(out, sprintf("**%s**%s%s-%s", t_("range"), cl, .qes_code_chr(pl$valid_min[j]), .qes_code_chr(pl$valid_max[j])), "")
    }
    out <- c(out, paste0("**", t_("members"), "**"), "",
             .qes_md_table(c(t_("precedence"), t_("type_name"), t_("member"), t_("default"), t_("transform"), t_("cap")),
                           lapply(seq_len(nrow(m)), function(k) {
                             lab <- .qes_pick_lang(m[k, , drop = FALSE], "type_label", lang)
                             note <- .qes_pick_lang(m[k, , drop = FALSE], "note", lang)
                             c(m$precedence[k], sprintf("`%s`: %s%s", m$type_name[k], lab, if (is.na(note)) "" else paste0(". ", note)),
                               sprintf("[`%s`](#target-%s)", m$member[k], m$member[k]),
                               t_(if (isTRUE(m$default[k])) "yes" else "no"), paste0("`", m$transform[k], "`"),
                               if (is.na(m$grade_cap[k])) "" else paste0("`", m$grade_cap[k], "`"))
                           })), "")
    rows <- lapply(study_cols, function(st) {
      cells <- vapply(seq_len(nrow(m)), function(k) {
        i <- which(xw$study == st & xw$target == m$member[k])
        if (length(i) == 0L) return("\u2014")
        i <- i[1]
        g <- .qes_pool_cap(xw$grade[i], m$grade_cap[k])
        paste0(.qes_enum_label("grade", g, lang), " (", .qes_wave_label(xw$wave[i], lang, st, spec), ")",
               if (xw$status[i] %in% c("draft", "review")) {
                 paste0(", ", t_(if (site) "site_pending" else xw$status[i]))
               } else "")
      }, character(1))
      has_row <- vapply(m$member, function(t) any(xw$study == st & xw$target == t), logical(1))
      used <- which(has_row & m$default %in% TRUE)
      c(paste0("`", st, "`"), cells, if (length(used) == 0L) "\u2014" else paste0("`", m$type_name[used[1]], "`"))
    })
    out <- c(out, paste0("**", t_("coverage"), "**"), "",
             .qes_md_table(c(t_("study"), paste0("`", m$type_name, "`"), t_("pooled_used")), rows), "",
             t_("pooled_coverage_note"), "")
    hist <- ch[!is.na(ch$targets) & vapply(ch$targets, function(x) p %in% .qes_split_list(x), logical(1)), , drop = FALSE]
    if (nrow(hist) > 0L && !site) {
      out <- c(out, paste0("**", t_("history"), "**"), "")
      for (k in seq_len(nrow(hist))) {
        out <- c(out, sprintf("- %s (%s)%s%s", hist$spec_version[k], format(hist$date[k]), cl,
                              .qes_pick_lang(hist[k, , drop = FALSE], "change", lang)))
      }
      out <- c(out, "")
    }
  }
  out
}

# The chapter of the reference on the relaxed layer of qes_decon()
# (R/hz-relaxed.R), in `lang`: one section per column with its definition,
# base, transform, relaxation rule, levels and relaxed mappings.
.qes_relaxed_reference_md <- function(spec, lang, studies = NULL, site = FALSE) {
  t_ <- function(key) .qes_rt(key, lang)
  cl <- t_("colon")
  t <- .qes_rx_tables(spec)
  rx <- .qes_rx_columns(spec)
  if (nrow(rx) == 0L) return(character(0))
  rm <- t$maps
  if (!is.null(studies)) rm <- rm[rm$study %in% studies, , drop = FALSE]
  ps <- .qes_rx_pseudo_spec(spec)
  all_rm <- t$maps
  out <- c(paste("##", t_("relaxed_title")), "", t_("relaxed_intro"), "")
  for (j in seq_len(nrow(rx))) {
    col <- rx$column[j]
    base <- if (is.na(rx$base[j])) t_("relaxed_only") else paste0("`", rx$base[j], "`")
    out <- c(out, sprintf("### `%s`%s%s {#relaxed-%s}", col, cl, rx[[paste0("label_", lang)]][j], col), "",
             rx[[paste0("description_", lang)]][j], "",
             sprintf("%s%s%s \u00b7 `%s` \u00b7 %s", t_("relaxed_base"), cl, base, rx$transform[j],
                     t_(if (rx$timing[j] %in% "static") "relaxed_static" else "relaxed_wave")), "",
             sprintf("**%s**%s%s", t_("relaxed_how"), cl, rx[[paste0("relax_", lang)]][j]), "")
    if (!is.na(rx$levels_id[j])) {
      set <- .qes_spec_levels(spec$tables$levels, rx$levels_id[j])
      out <- c(out, paste0("**", t_("levels"), "**"), "",
               .qes_md_table(c(t_("code"), t_("name"), t_("label")),
                             lapply(seq_len(nrow(set)), function(k) c(set$code[k], paste0("`", set$name[k], "`"),
                                                                      set[[paste0("label_", lang)]][k]))), "")
    } else {
      out <- c(out, sprintf("**%s**%s%s-%s", t_("range"), cl, .qes_code_chr(rx$valid_min[j]), .qes_code_chr(rx$valid_max[j])), "")
    }
    k <- which(rm$column == col)
    out <- c(out, paste0("**", t_("relaxed_rows"), "**"), "")
    if (length(k) == 0L) {
      out <- c(out, t_("relaxed_none"), "")
      next
    }
    set <- if (is.na(rx$levels_id[j])) NULL else .qes_spec_levels(spec$tables$levels, rx$levels_id[j])
    cells <- lapply(k, function(i) {
      ii <- match(paste(rm$study[i], rm$wave[i], rm$column[i]), paste(all_rm$study, all_rm$wave, all_rm$column))
      notes <- .qes_pick_lang(rm[i, , drop = FALSE], "notes", lang)
      c(rm$study[i], sprintf("`%s` (%s)", rm$source_var[i], .qes_wave_label(rm$wave[i], lang, rm$study[i], spec)),
        .qes_rx_row_text(ps, ps$tables$crosswalk, ii, set, lang),
        t_(if (isTRUE(rm$override[i])) "relaxed_replaces" else "relaxed_fills"),
        if (is.na(notes)) "" else notes,
        if (site && rm$status[i] %in% c("draft", "review")) t_("site_pending") else rm$status[i])
    })
    out <- c(out, .qes_md_table(c(t_("study"), t_("source"), t_("relaxed_recode"), t_("relaxed_use"), t_("relaxed_notes"), t_("status")), cells), "")
  }
  out
}

# The member type (and its grade) a pooled variable takes each study's
# values from in the respondent layout: the first default member with a
# primary mapped row. A named list, pool -> named character vector over
# studies (NA: none).
.qes_pooled_used <- function(spec, studies) {
  xw <- spec$tables$crosswalk
  xw <- xw[!is.na(xw$rule) & xw$rule != "none" & xw$primary %in% TRUE, , drop = FALSE]
  out <- list()
  for (p in .qes_pool_names(spec)) {
    m <- .qes_pool_members(spec, p)
    m <- m[m$default %in% TRUE, , drop = FALSE]
    out[[p]] <- vapply(studies, function(st) {
      for (k in seq_len(nrow(m))) {
        i <- which(xw$study == st & xw$target == m$member[k])
        if (length(i) > 0L) return(paste(m$type_name[k], .qes_pool_cap(xw$grade[i[1]], m$grade_cap[k]), sep = "|"))
      }
      NA_character_
    }, character(1))
  }
  out
}

# The coverage grid of the spec as markdown text, in `lang` (the website's
# coverage page, design.md section 9.1): one row per target, grouped by
# block, one column per study of the spec, each cell the grade of the
# study's mapped row (the wave in parentheses for studies of several waves,
# "\\*" when its question did not offer every level of the target); then
# one row per study with its waves, their size and recommended weight, and
# the number of targets of each grade. `reference` is the page the target
# names link to (anchors "#target-<name>" of .spec_reference_md()).
# `site = TRUE` gives the website's version: no spec version, hash or review
# process; a cell whose row awaits sign-off is marked with a dagger instead.
.spec_coverage_md <- function(lang = "en", spec = NULL, reference = NULL, site = FALSE) {
  lang <- if (identical(lang, "fr")) "fr" else "en"
  spec <- if (inherits(spec, "qes_spec")) spec else .qes_spec_get(spec, "none")
  site <- isTRUE(site)
  if (is.null(reference)) {
    reference <- if (identical(lang, "fr")) "fr-reference-harmonisation.html" else "harmonization-reference.html"
  }
  t_ <- function(key) .qes_rt(key, lang)
  number <- function(n) formatC(as.integer(n), format = "d", big.mark = if (identical(lang, "fr")) "\u00a0" else ",")
  dash <- "\u2014"
  tg <- spec$tables$targets
  xw <- spec$tables$crosswalk
  used <- xw[!is.na(xw$rule) & xw$rule != "none", , drop = FALSE]
  used_rows <- used
  # the counts are of the rows qes_harmonize() would apply: the primary row
  # of each study and target (at most one, rule V-S5)
  applied <- used[used$primary %in% TRUE, , drop = FALSE]
  wv <- spec$tables$waves
  wt <- spec$tables$weights
  studies <- intersect(.qes_study_codes(), unique(c(used$study, wv$study)))
  # studies of another catalog (a user spec) come after, in their own order
  studies <- c(studies, setdiff(unique(used$study), studies))
  multi_wave <- names(which(table(wv$study) > 1L))
  not_off <- .qes_not_offered(spec, used)
  blocks <- .qes_enum("block")$value
  ord <- order(match(tg$block, blocks), seq_len(nrow(tg)))
  tg <- tg[ord, , drop = FALSE]
  # one row per target, its label under its name; a row with the block's
  # name before the first target of each block (a narrow table: the study
  # columns must fit beside it)
  grid <- list()
  for (j in seq_len(nrow(tg))) {
    t <- tg$target[j]
    if (j == 1L || !identical(tg$block[j], tg$block[j - 1L])) {
      grid[[length(grid) + 1L]] <- c(paste0("**", .qes_enum_label("block", tg$block[j], lang), "**"),
                                     rep("", length(studies)))
    }
    cells <- vapply(studies, function(s) {
      k <- which(used$study == s & used$target == t)
      if (length(k) == 0L) return(dash)
      paste(vapply(k, function(i) {
        paste0(.qes_enum_label("grade", used$grade[i], lang),
               if (s %in% multi_wave) paste0(" (", .qes_wave_label(used$wave[i], lang, s, spec), ")") else "",
               if (!is.na(not_off[i]) && nzchar(not_off[i])) " \\*" else "",
               if (site && used$status[i] %in% c("draft", "review")) " \u2020" else "")
      }, character(1)), collapse = t_("semi"))
    }, character(1), USE.NAMES = FALSE)
    grid[[length(grid) + 1L]] <- c(sprintf("[`%s`](%s#target-%s)<br>%s", t, reference, t,
                                           .qes_pick_lang(tg[j, , drop = FALSE], "label", lang)), cells)
  }
  grades <- intersect(.qes_hz_grades, c("identical", "comparable", "approximate"))
  study_rows <- lapply(studies, function(s) {
    w <- wv[wv$study == s, , drop = FALSE]
    w <- w[order(w$wave_order), , drop = FALSE]
    weight_of <- function(wave) {
      rec <- wt[wt$study == s & wt$wave %in% c(wave, .qes_all_waves) & wt$recommended %in% TRUE, , drop = FALSE]
      if (nrow(rec) == 0L) t_("cov_no_weight") else .qes_weight_cell(rec$weight_var[1], rec$status[1], lang, site = site)
    }
    waves <- if (.qes_poll_study(wv, s) && nrow(w) > 1L) {
      # pooled polls: the number of polls, their first and last, their sizes
      sprintf(t_("cov_polls"), nrow(w), w$wave[1], w$wave[nrow(w)], number(min(w$n_cases)),
              number(max(w$n_cases)), t_("colon"), weight_of(w$wave[1]))
    } else {
      vapply(seq_len(nrow(w)), function(i) {
        sprintf("%s (n = %s)%s%s", w$wave[i], number(w$n_cases[i]), t_("colon"), weight_of(w$wave[i]))
      }, character(1))
    }
    g <- applied$grade[applied$study == s]
    c(paste0("`", s, "`"), paste(waves, collapse = t_("semi")),
      as.character(length(unique(applied$target[applied$study == s]))),
      vapply(grades, function(x) as.character(sum(g == x)), character(1), USE.NAMES = FALSE))
  })
  # rows in review were checked against the files but not signed off; draft
  # rows were not yet checked: each has its own sentence
  status_line <- function(n, all_key, some_key) {
    if (n == 0L) return(NULL)
    if (n == nrow(applied)) c(sprintf(t_(all_key), n), "") else c(sprintf(t_(some_key), n, nrow(applied)), "")
  }
  others <- setdiff(.qes_study_codes(), studies)
  # the pooled variables: the member type each study's values come from
  pooled_grid <- NULL
  used <- .qes_pooled_used(spec, studies)
  if (length(used) > 0L) {
    pl <- .qes_pool_tables(spec)$pooled
    prow <- lapply(names(used), function(p) {
      u <- used[[p]]
      cells <- ifelse(is.na(u), dash, sprintf("`%s` (%s)", sub("\\|.*$", "", u),
                                             .qes_enum_label("grade", sub("^.*\\|", "", u), lang)))
      c(sprintf("[`%s`](%s#pooled-%s)<br>%s", p, reference, p,
                .qes_pick_lang(pl[match(p, pl$pooled), , drop = FALSE], "label", lang)), cells)
    })
    pooled_grid <- c(paste("##", t_("cov_pooled_title")), "", .qes_md_table(c(t_("cov_target"), studies), prow), "",
                     t_("cov_pooled_note"), "")
  }
  status_lines <- if (site) {
    if (any(used_rows$status %in% c("draft", "review"))) c(t_("site_cov_pending"), "")
  } else {
    c(status_line(sum(applied$status == "stable"), "cov_stable_all", "cov_stable_note"),
      status_line(sum(applied$status == "review"), "cov_status_all", "cov_status_note"),
      status_line(sum(applied$status == "draft"), "cov_draft_all", "cov_draft_note"))
  }
  out <- c(
    if (site) sprintf(t_("site_cov_note"), reference) else
      sprintf(t_("cov_note"), spec$version, format(as.Date(spec$date)), paste0("`", spec$hash, "`"), reference), "",
    paste("##", t_("cov_grid_title")), "",
    .qes_md_table(c(t_("cov_target"), studies), grid), "",
    t_("cov_wave_note"), "", t_("cov_zero_note"), "",
    if (site) status_lines,
    pooled_grid,
    if (!site) status_lines,
    paste("##", t_("cov_studies_title")), "",
    .qes_md_table(c(t_("study"), t_("cov_waves"), t_("cov_n_targets"),
                    .qes_enum_label("grade", grades, lang)), study_rows), "",
    t_(if (site) "site_cov_studies_note" else "cov_studies_note"), ""
  )
  if (length(others) > 0L && site) {
    # the catalog's studies outside the spec: on the website, where their
    # respondents are when they share the deposit of a harmonized study
    cat_studies <- .qes_catalog()$studies
    doi_of <- function(x) cat_studies$doi[match(x, cat_studies$study)]
    within <- unique(studies[doi_of(studies) %in% doi_of(others)])
    out <- c(out, if (length(within) > 0L && all(doi_of(others) %in% doi_of(within))) {
      sprintf(t_("site_not_in_spec_in"), paste0("`", others, "`", collapse = ", "),
              paste0("`", within, "`", collapse = ", "))
    } else {
      sprintf(t_("site_not_in_spec"), paste0("`", others, "`", collapse = ", "))
    }, "")
  } else if (length(others) > 0L) {
    out <- c(out, sprintf(t_("cov_not_in_spec"), paste0("`", others, "`", collapse = ", ")), "")
  }
  paste0(paste(out, collapse = "\n"), "\n")
}

# The compact coverage grid of README.md (design.md section 9), one row per
# target in block order, one column per study of the spec, each cell the
# first letter of the grade of the row qes_harmonize() applies (I, C or A,
# the same in English and French) or a dash. data-raw/readme_coverage.R
# writes it between the "coverage" markers of README.md, and
# data-raw/spec_check.R (V-P3) checks that it is current. With `reference`
# (the path of the harmonization reference page), each target's name links
# to its section there, as on the website, and the study codes of the header
# may wrap after "qes" and at "_", so that the 11 studies fit a narrow
# column.
.spec_readme_md <- function(spec = NULL, reference = NULL) {
  spec <- if (inherits(spec, "qes_spec")) spec else .qes_spec_get(spec, "none")
  tg <- spec$tables$targets
  xw <- spec$tables$crosswalk
  applied <- xw[!is.na(xw$rule) & xw$rule != "none" & xw$primary %in% TRUE, , drop = FALSE]
  wv <- spec$tables$waves
  studies <- intersect(.qes_study_codes(), unique(c(applied$study, wv$study)))
  studies <- c(studies, setdiff(unique(applied$study), studies))
  tg <- tg[order(match(tg$block, .qes_enum("block")$value), seq_len(nrow(tg))), , drop = FALSE]
  letter <- c(identical = "I", comparable = "C", approximate = "A")
  rows <- lapply(tg$target, function(t) {
    name <- paste0("`", t, "`")
    if (!is.null(reference)) name <- sprintf("[%s](%s#target-%s)", name, reference, t)
    c(name, vapply(studies, function(s) {
      g <- applied$grade[applied$study == s & applied$target == t]
      if (length(g) == 0L || is.na(letter[g[1]])) "\u2014" else letter[[g[1]]]
    }, character(1), USE.NAMES = FALSE))
  })
  # the pooled variables, after the targets: the grade of the member each
  # study's values come from (R/hz-pool.R)
  used <- .qes_pooled_used(spec, studies)
  for (p in names(used)) {
    name <- paste0("`", p, "`")
    if (!is.null(reference)) name <- sprintf("[%s](%s#pooled-%s)", name, reference, p)
    g <- sub("^.*\\|", "", used[[p]])
    rows[[length(rows) + 1L]] <- c(paste(name, "(pooled)"),
                                   ifelse(is.na(used[[p]]), "\u2014", unname(letter[g])))
  }
  header <- paste0("`", studies, "`")
  if (!is.null(reference)) {
    # on the website the study codes may wrap, only after "qes" and at "_"
    header <- sprintf("<code>%s</code>", gsub("_", "_<wbr>", sub("^qes([0-9])", "qes<wbr>\\1", studies)))
  }
  paste0(paste(.qes_md_table(c("", header), rows), collapse = "\n"), "\n")
}

# The list of targets for the Rd page of qes_spec() (roxygen @eval), from
# the shipped spec.
.rd_targets <- function() {
  spec <- .qes_spec_load(normalizePath(.qes_spec_shipped_dir(), winslash = "/", mustWork = TRUE))
  tg <- spec$tables$targets
  xw <- spec$tables$crosswalk
  xw <- xw[!is.na(xw$rule) & xw$rule != "none", , drop = FALSE]
  items <- vapply(seq_len(nrow(tg)), function(j) {
    studies <- unique(xw$study[xw$target == tg$target[j]])
    sets <- .qes_split_list(tg$sets[j])
    sprintf("\\item{\\code{%s}}{%s (family \\code{%s}; %s; %s). Studies: %s.}",
            tg$target[j], tg$label_en[j], tg$family[j],
            if (length(sets) == 0L) "no set" else paste("sets", paste(sprintf("\\code{%s}", sets), collapse = ", ")),
            tg$type[j], if (length(studies) == 0L) "none yet" else paste(studies, collapse = ", "))
  }, character(1))
  pt <- .qes_pool_tables(spec)
  pooled <- vapply(seq_len(nrow(pt$pooled)), function(j) {
    m <- .qes_pool_members(spec, pt$pooled$pooled[j])
    sprintf("\\item{\\code{%s}}{%s (pooled, %s; sets %s). Members, in order of precedence: %s.}",
            pt$pooled$pooled[j], pt$pooled$label_en[j], pt$pooled$type[j],
            paste(sprintf("\\code{%s}", .qes_split_list(pt$pooled$sets[j])), collapse = ", "),
            paste(sprintf("\\code{%s} (%s%s)", m$member, m$type_name, ifelse(m$default %in% TRUE, "", ", not by default")),
                  collapse = ", "))
  }, character(1))
  c(sprintf("@section Targets in the shipped spec (version %s):", spec$version),
    "Generated from the spec by roxygen; `qes_spec()` gives the same list with each study's grade.",
    "\\describe{", items, "}",
    if (length(pooled) > 0L) c(
      "",
      "Pooled variables (one column that pools several targets; `qes_spec(\"pooled\")` gives each study's grade):",
      "\\describe{", pooled, "}"
    ))
}

# The "Missing values" section of ?qes_harmonize, generated from the NA
# reasons of the catalog vocabulary that the engine can set (roxygen @eval),
# so the documented list cannot drift from the `__na` levels.
.rd_na_reasons <- function() {
  r <- .qes_enum("missing_type")
  r <- r[r$value %in% .qes_hz_reason_levels(), , drop = FALSE]
  items <- sprintf("\\item{\\code{%s}}{%s.}", r$value, r$label_en)
  c("@section Missing values:",
    paste("Every missing value has a reason, in this order (the levels of the",
          "\\code{<target>__na} columns and the \\code{n_<reason>} columns of",
          "\\code{qes_provenance(x, level = \"cell\")}); generated from the catalog vocabulary:"),
    "\\describe{", items, "}",
    paste("\\code{not_asked} means the study has no question for the target;",
          "\\code{not_reviewed} that it has one, whose crosswalk row is not yet signed off by a",
          "reviewer (applied with \\code{include_draft = TRUE}).",
          "\\code{missing = \"reasons\"} adds a factor column \\code{<target>__na} with the",
          "reason of each missing value."))
}
