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

# The recommended weight of each crosswalk row's study-wave.
.qes_row_weight <- function(spec, xw) {
  wt <- spec$tables$weights
  rec <- wt[wt$recommended %in% TRUE, , drop = FALSE]
  rec$weight_var[match(paste(xw$study, xw$wave), paste(rec$study, rec$wave))]
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

# ---- view "crosswalk" ------------------------------------------------------------------

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
      weight_var = .qes_row_weight(spec, xw), status = xw$status, evidence = xw$evidence,
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
    if (xw$rule[i] %in% "map") {
      m <- vm[vm$map_id %in% xw$map_id[i], , drop = FALSE]
      add(xw$source_var[i], m$source_code, m$source_label, rep("map", nrow(m)), m$target_code, m$na_reason, m$note)
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
    en = "Each target is one question stimulus: a different wording, scale, timing or format makes another target, and targets are never pooled. For each study, the coverage table gives the source variable and its wave, the comparability grade of the study's question against the target's anchor question and the reason for it, the instrument (the item format), the levels the question offered, the question wording (or, when the wording cannot be shipped, the document and page that give it), the filter question and what each of its codes means, the study's recommended weight and whether \"don't know\" was offered.",
    fr = "Chaque cible correspond \u00e0 un seul stimulus de question\u00a0: une formulation, une \u00e9chelle, un moment ou un format diff\u00e9rent donne une autre cible, et les cibles ne sont jamais regroup\u00e9es. Pour chaque \u00e9tude, le tableau de couverture donne la variable source et sa vague, le niveau de comparabilit\u00e9 de la question de l'\u00e9tude par rapport \u00e0 la question d'ancrage de la cible et sa raison, l'instrument (le format de la question), les niveaux offerts, le libell\u00e9 de la question (ou, quand il ne peut pas \u00eatre fourni, le document et la page qui le donnent), la question filtre et le sens de chacun de ses codes, la pond\u00e9ration recommand\u00e9e de l'\u00e9tude et si \u00ab\u00a0je ne sais pas\u00a0\u00bb \u00e9tait offert."
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
    en = "A row marked \"in review\" was checked against the original files and documents but is not yet signed off by a reviewer; a row marked \"draft\" is not yet checked. `qes_harmonize()` applies such rows only with `include_draft = TRUE`.",
    fr = "Une ligne marqu\u00e9e \u00ab\u00a0en r\u00e9vision\u00a0\u00bb a \u00e9t\u00e9 v\u00e9rifi\u00e9e sur les fichiers et documents originaux mais n'est pas encore approuv\u00e9e par un r\u00e9viseur\u00a0; une ligne marqu\u00e9e \u00ab\u00a0provisoire\u00a0\u00bb n'est pas encore v\u00e9rifi\u00e9e. `qes_harmonize()` n'applique ces lignes qu'avec `include_draft = TRUE`."
  ),
  document = c(en = "document %s, %s", fr = "document %s, %s"),
  none = c(en = "No study has a question for this target yet.", fr = "Aucune \u00e9tude n'a encore de question pour cette cible."),
  not_used = c(en = "Not used", fr = "Non utilis\u00e9"),
  history = c(en = "History", fr = "Historique"),
  all_targets = c(en = "all targets", fr = "toutes les cibles")
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
# single-target crosswalk view uses it).
.spec_reference_md <- function(lang = "en", spec = NULL, targets = NULL, header = TRUE, studies = NULL) {
  lang <- if (identical(lang, "fr")) "fr" else "en"
  spec <- if (inherits(spec, "qes_spec")) spec else .qes_spec_get(spec, "none")
  t_ <- function(key) .qes_rt(key, lang)
  cl <- t_("colon")
  tg <- spec$tables$targets
  xw <- spec$tables$crosswalk
  ch <- spec$tables$changes
  out <- character(0)
  if (isTRUE(header)) {
    out <- c(out, sprintf(t_("title_note"), spec$version, format(as.Date(spec$date)), paste0("`", spec$hash, "`")), "",
             paste("##", t_("how_title")), "", t_("how"), "", t_("status_note"), "",
             paste("###", t_("grades_title")), "")
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
    out <- c(out, sprintf("%s `%s`%s%s", h, t, cl, .qes_pick_lang(tg[j, , drop = FALSE], "label", lang)), "",
             .qes_pick_lang(tg[j, , drop = FALSE], "description", lang), "")
    facts <- sprintf("%s `%s` \u00b7 %s %s \u00b7 %s %s \u00b7 %s %s \u00b7 %s %s",
                     t_("family"), tg$family[j], t_("type"), .qes_enum_label("target_type", tg$type[j], lang),
                     t_("timing"), .qes_enum_label("target_timing", tg$target_timing[j], lang),
                     t_("status"), .qes_enum_label("target_status", tg$status[j], lang),
                     t_("added_in"), tg$added_in[j])
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
      weight <- .qes_row_weight(spec, used)
      gate <- .qes_gate_text(used)
      anchor <- paste(used$study, used$wave, used$source_var, sep = ":") %in% tg$anchor_row[j]
      cells <- lapply(seq_len(nrow(used)), function(k) {
        u <- used[k, , drop = FALSE]
        src <- paste0("`", u$source_var, "` (", u$wave, ")")
        grade <- paste0("`", u$grade, "`", if (anchor[k]) paste0(" (", t_("anchor"), ")") else "",
                        if (u$status %in% c("draft", "review")) paste0(" (", t_(u$status), ")") else "")
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
        c(u$study, src, grade, .qes_pick_lang(u, "grade_reason", lang), u$instrument, offered,
          wording, gate[k], weight[k], .qes_enum_label("dk_offered", u$dk_offered, lang))
      })
      out <- c(out, .qes_md_table(c(t_("study"), t_("source"), t_("grade"), t_("reason"), t_("instrument"),
                                    t_("offered"), t_("wording"), t_("gate"), t_("weight"), t_("dk")), cells), "")
    }
    if (nrow(unused) > 0L) {
      out <- c(out, paste0("**", t_("not_used"), "**"), "")
      for (k in seq_len(nrow(unused))) {
        u <- unused[k, , drop = FALSE]
        out <- c(out, sprintf("- %s `%s` (%s)%s`%s`. %s", u$study, u$source_var, u$wave, cl, u$grade,
                              .qes_pick_lang(u, "grade_reason", lang)))
      }
      out <- c(out, "")
    }
    hist <- ch[is.na(ch$targets) | vapply(ch$targets, function(x) t %in% .qes_split_list(x), logical(1)), , drop = FALSE]
    if (nrow(hist) > 0L) {
      out <- c(out, paste0("**", t_("history"), "**"), "")
      for (k in seq_len(nrow(hist))) {
        out <- c(out, sprintf("- %s (%s)%s%s", hist$spec_version[k], format(hist$date[k]), cl,
                              .qes_pick_lang(hist[k, , drop = FALSE], "change", lang)))
      }
      out <- c(out, "")
    }
  }
  paste0(paste(out, collapse = "\n"), "\n")
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
  c(sprintf("@section Targets in the shipped spec (version %s):", spec$version),
    "Generated from the spec by roxygen; `qes_spec()` gives the same list with each study's grade.",
    "\\describe{", items, "}")
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
