# The spec validator, rules V-S1 to V-S17, V-S18 (the legacy renderer
# table, R/legacy.R), V-S19 (a note on the rows the legacy freeze holds),
# V-F1 to V-F7 (the pooled variables, R/hz-pool.R; spec 4.3.0), V-R1 to
# V-R12 (the relaxed layer, R/hz-relaxed.R; spec 4.4.0) and the hash half of
# V-P2 (design.md section 5.10, slice HZ1).
#
# One implementation, run in four places: at runtime by qes_spec() (once per
# session), in the offline tests, in CI (data-raw/spec_check.R, with
# `release = TRUE` on tags) and, with the data checks V-D* of R/hz-data.R,
# on the original files (data-raw/build_sources.R). It never reads data: the
# tables gates.csv, expected/marginals.csv and expected/hashes.csv are
# checked here for their form only (V-S1, V-S2, V-S4); their content is
# checked by V-D7, V-D8, V-P1 and, on the originals, V-L1.
#
# .qes_spec_check() returns a problems table: rule, severity ("error",
# "warning" or "note"), table, row (the data row of the CSV, 1-based), key and
# detail (fixed English text: returned text never depends on the session
# language, rule P4). qes_spec(validate = "error") raises qesR_error_spec when
# any problem has severity "error".

# Registered functions for rule "fn:<name>" (design.md section 5.6): each is
# function(src, ctx) returning list(value, na_reason), with a test file
# tests/testthat/test-hz-fn-<name>.R. multiselect (spec 4.3.0, R/hz-data.R):
# a select-all-that-apply question stored as one variable per option;
# amount_bands (spec 4.4.0, relaxed rows only): an amount in bands, with a
# bracket follow-up for those who gave no amount.
.qes_hz_fns <- list(
  multiselect = function(src, ctx) .qes_hz_fn_multiselect(src, ctx),
  amount_bands = function(src, ctx) .qes_hz_fn_amount_bands(src, ctx)
)

# Name grammar of targets, families, sets and level names (V-S15).
.qes_name_pattern <- "^[a-z][a-z0-9]*(_[a-z0-9]+)*$"
.qes_level_name_pattern <- "^[A-Za-z][A-Za-z0-9]*(_[A-Za-z0-9]+)*$"

# Rules each target type accepts (with "fn:" and "none" for every type).
.qes_type_rules <- list(
  categorical = c("map", "coalesce", "constant"),
  ordinal = c("map", "coalesce", "constant"),
  numeric = c("numeric", "constant"),
  date = "date",
  string = "string",
  weight = "weight"
)

.qes_spec_check <- function(spec, release = FALSE, source_dir = NULL) {
  tg <- spec$tables$targets
  lv <- spec$tables$levels
  xw <- spec$tables$crosswalk
  vm <- spec$tables$valuemaps
  wv <- spec$tables$waves
  wt <- spec$tables$weights
  ch <- spec$tables$changes
  cat_ <- .qes_catalog()
  enums <- cat_$enums
  studies <- cat_$studies
  elections <- cat_$elections

  out <- list()
  # one problem per element of `row` or `key` (the longer; the other and
  # `detail` are recycled); nothing when either is empty
  add <- function(rule, table, row, key, detail, severity = "error") {
    n <- max(length(row), length(key))
    if (length(row) == 0L || length(key) == 0L) {
      return(invisible())
    }
    out[[length(out) + 1L]] <<- .qes_spec_problems(
      rep_len(rule, n), rep_len(severity, n), rep_len(table, n),
      rep_len(as.integer(row), n), rep_len(key, n), rep_len(detail, n)
    )
    invisible()
  }
  enum <- function(name) enums$value[enums$enum == name]
  has <- function(x) !is.na(x) & nzchar(x)
  split <- function(x) if (length(x) == 1L && has(x)) strsplit(x, ";", fixed = TRUE)[[1]] else character(0)
  xkey <- function(i) paste(xw$study[i], xw$wave[i], xw$target[i], xw$source_var[i], sep = "/")
  na_reasons <- enums$value[enums$enum == "missing_type" &
                              vapply(enums$scope, function(s) "spec" %in% split(s), logical(1))]
  study_codes <- studies$study
  study_names <- unique(c(study_codes, unlist(lapply(studies$aliases, split))))

  # level sets, expanded
  set_ids <- unique(lv$levels_id[has(lv$levels_id)])
  sets <- stats::setNames(lapply(set_ids, function(id) .qes_spec_levels(lv, id)), set_ids)
  target_set <- function(target) {
    id <- tg$levels_id[match(target, tg$target)]
    if (length(id) == 0L || is.na(id)) NULL else sets[[id]]
  }

  # ---- V-S1: formats beyond column types -------------------------------------
  meta_ok <- c(
    `Spec-Version` = grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", spec$version),
    `Spec-Date` = grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", spec$date),
    Hash = grepl("^[0-9a-f]{32}$", spec$hash_recorded),
    `Engine-Min` = grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", spec$engine_min),
    Licence = has(spec$licence)
  )
  add("V-S1", "SPEC", NA, names(meta_ok)[!meta_ok], "invalid or empty field")
  required <- list(
    targets = c("target", "family", "block", "type", "target_timing", "status", "added_in", "allow_constant"),
    levels = c("levels_id", "name"),
    crosswalk = c("study", "wave", "target", "rule", "source_var", "primary", "grade", "status"),
    valuemaps = c("map_id", "source_code"),
    waves = c("study", "wave", "wave_order", "wave_timing", "wave_design", "n_cases", "mode"),
    weights = c("study", "wave", "weight_var", "recommended", "status"),
    changes = c("spec_version", "date", "kind", "change_en", "change_fr")
  )
  for (tab in names(required)) {
    x <- spec$tables[[tab]]
    for (col in required[[tab]]) {
      bad <- which(is.na(x[[col]]))
      add("V-S1", tab, bad, col, sprintf("column '%s' is empty", col))
    }
  }
  inh <- has(lv$name) & startsWith(lv$name, "inherits=")
  bad <- which(!inh & (is.na(lv$code) | is.na(lv$order) | is.na(lv$substantive)))
  add("V-S1", "levels", bad, paste(lv$levels_id[bad], lv$name[bad], sep = "/"), "code, order and substantive are required")
  bad <- which(!inh & has(lv$name) & !grepl(.qes_level_name_pattern, lv$name))
  add("V-S1", "levels", bad, lv$name[bad], "level names are ASCII keys (letters, digits, underscores)")
  bad <- which(has(tg$anchor_row) & !grepl("^[^:]+:[^:]+:[^:]+$", tg$anchor_row))
  add("V-S1", "targets", bad, tg$target[bad], "anchor_row must be study:wave:source_var")
  bad <- which(!is.na(tg$valid_min) & !is.na(tg$valid_max) & tg$valid_min > tg$valid_max)
  add("V-S1", "targets", bad, tg$target[bad], "valid_min is greater than valid_max")
  bad <- which(!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", tg$added_in))
  add("V-S1", "targets", bad, tg$target[bad], "added_in must be a spec version")
  bad <- which(!grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", ch$spec_version))
  add("V-S1", "changes", bad, ch$spec_version[bad], "spec_version must be major.minor.patch")

  for (i in seq_len(nrow(xw))) {
    k <- xkey(i)
    rule <- xw$rule[i]
    base <- if (has(rule) && startsWith(rule, "fn:")) "fn" else rule
    args <- .qes_parse_kv(xw$args[i])
    if (is.null(args)) {
      add("V-S1", "crosswalk", i, k, "args must be key=value;key=value with unique keys")
    } else if (length(args) > 0L && !identical(base, "fn")) {
      allowed <- .qes_arg_keys[[base]] %||% character(0)
      extra <- setdiff(names(args), allowed)
      if (length(extra) > 0L) {
        add("V-S1", "crosswalk", i, k, sprintf("args key(s) %s not allowed for rule %s", paste(extra, collapse = ", "), rule))
      }
      for (nm in intersect(names(args), c("min", "max"))) {
        if (is.na(suppressWarnings(as.numeric(args[[nm]])))) {
          add("V-S1", "crosswalk", i, k, sprintf("args %s must be a number", nm))
        }
      }
      if (all(c("min", "max") %in% names(args))) {
        lim <- suppressWarnings(as.numeric(args[c("min", "max")]))
        if (!anyNA(lim) && lim[1] > lim[2]) {
          add("V-S1", "crosswalk", i, k, "args min is greater than max")
        }
      }
      for (nm in intersect(names(args), c("from_label", "nonmonotone"))) {
        if (!args[[nm]] %in% c("TRUE", "FALSE")) {
          add("V-S1", "crosswalk", i, k, sprintf("args %s must be TRUE or FALSE", nm))
        }
      }
      if ("affine" %in% names(args) && is.null(.qes_affine(args[["affine"]]))) {
        add("V-S1", "crosswalk", i, k, "args affine is not in the grammar a*x+b")
      }
      if ("format" %in% names(args) && !args[["format"]] %in% enum("date_format")) {
        add("V-S1", "crosswalk", i, k, "args format is not a date format of enums.csv")
      }
    }
    if (identical(base, "coalesce")) {
      then <- .qes_hz_coalesce_then(xw, i)
      if (is.null(then) || nrow(then) == 0L) {
        add("V-S1", "crosswalk", i, k, "rule coalesce needs args then=<variable>:<map_id>,... (at least one)")
      } else if (anyDuplicated(c(xw$source_var[i], then$var)) > 0L) {
        add("V-S1", "crosswalk", i, k, "rule coalesce names a variable twice")
      }
    }
    nac <- .qes_parse_kv(xw$na_codes[i])
    if (is.null(nac)) {
      add("V-S1", "crosswalk", i, k, "na_codes must be code=reason;code=reason with unique codes")
    } else if (!all(.qes_is_canon_code(names(nac)))) {
      add("V-S1", "crosswalk", i, k, "na_codes has a code not in canonical form")
    }
    gate <- c(has(xw$gate_var[i]), has(xw$gate_codes[i]), has(xw$gate_to[i]))
    if (any(gate) && !all(gate)) {
      add("V-S1", "crosswalk", i, k, "gate_var, gate_codes and gate_to go together")
    } else if (all(gate)) {
      codes <- split(xw$gate_codes[i])
      to <- .qes_parse_kv(xw$gate_to[i])
      if (!all(.qes_is_canon_code(codes))) {
        add("V-S1", "crosswalk", i, k, "gate_codes has a code not in canonical form")
      }
      if (is.null(to) || !setequal(names(to), codes)) {
        add("V-S1", "crosswalk", i, k, "gate_to must give one outcome for each gate code (code=outcome)")
      }
      if (!base %in% c("map", "coalesce", "numeric", "weight", "date", "string")) {
        add("V-S1", "crosswalk", i, k, "a gate applies only to rules map, coalesce, numeric, weight, date and string")
      }
    }
    lo <- split(xw$levels_offered[i])
    if (anyDuplicated(lo) > 0L) {
      add("V-S1", "crosswalk", i, k, "levels_offered repeats a level")
    }
  }
  bad <- which(!.qes_is_canon_code(vm$source_code))
  add("V-S1", "valuemaps", bad, paste(vm$map_id[bad], vm$source_code[bad], sep = "/"), "source_code is not in canonical form")
  both <- !is.na(vm$target_code) & has(vm$na_reason)
  neither <- is.na(vm$target_code) & !has(vm$na_reason)
  bad <- which(both | neither)
  add("V-S1", "valuemaps", bad, paste(vm$map_id[bad], vm$source_code[bad], sep = "/"), "exactly one of target_code and na_reason is set")
  bad <- which(has(vm$source_label_hash) & !grepl("^[0-9a-f]{32}$", vm$source_label_hash))
  add("V-S1", "valuemaps", bad, paste(vm$map_id[bad], vm$source_code[bad], sep = "/"), "source_label_hash must be an md5")
  lab_hash <- which(has(vm$source_label) & has(vm$source_label_hash))
  if (length(lab_hash) > 0L) {
    bad <- lab_hash[.qes_md5_text(.qes_norm_label(vm$source_label[lab_hash])) != vm$source_label_hash[lab_hash]]
    add("V-S1", "valuemaps", bad, paste(vm$map_id[bad], vm$source_code[bad], sep = "/"), "source_label_hash is not the md5 of the normalized source_label")
  }
  member <- cbind(has(wv$member_var), has(wv$member_codes))
  bad <- which(member[, 1] != member[, 2])
  add("V-S1", "waves", bad, paste(wv$study[bad], wv$wave[bad], sep = "/"), "member_var and member_codes go together")
  for (i in which(has(wv$member_codes))) {
    codes <- split(wv$member_codes[i])
    if (!(identical(codes, "!NA") || all(.qes_is_canon_code(codes)))) {
      add("V-S1", "waves", i, paste(wv$study[i], wv$wave[i], sep = "/"), "member_codes must be canonical codes or !NA")
    }
  }
  bad <- which(has(wv$date_var) != has(wv$date_format))
  add("V-S1", "waves", bad, paste(wv$study[bad], wv$wave[bad], sep = "/"), "date_var and date_format go together")
  bad <- which(!is.na(wv$fieldwork_start) & !is.na(wv$fieldwork_end) & wv$fieldwork_start > wv$fieldwork_end)
  add("V-S1", "waves", bad, paste(wv$study[bad], wv$wave[bad], sep = "/"), "fieldwork_start is after fieldwork_end")
  for (i in which(has(wt$trim))) {
    lim <- suppressWarnings(as.numeric(split(wt$trim[i])))
    if (length(lim) != 2L || anyNA(lim) || lim[1] >= lim[2]) {
      add("V-S1", "weights", i, paste(wt$study[i], wt$wave[i], wt$weight_var[i], sep = "/"), "trim must be low;high")
    }
  }

  gt <- spec$tables$gates
  ex <- spec$tables$expected
  if (!is.null(gt) && nrow(gt) > 0L) {
    gkeys <- paste(gt$study, gt$wave, gt$source_var, gt$gate_var, gt$gate_code, gt$source_code, sep = "/")
    bad <- which(is.na(gt$study) | is.na(gt$wave) | is.na(gt$source_var) | is.na(gt$source_code) | is.na(gt$n) | gt$n < 0L)
    add("V-S1", "gates", bad, gkeys[bad], "study, wave, source_var, source_code and a count of 0 or more are required")
    bad <- which(!is.na(gt$source_code) & !.qes_is_canon_code(gt$source_code))
    add("V-S1", "gates", bad, gkeys[bad], "source_code is not in canonical form")
    bad <- which(has(gt$gate_code) & !.qes_is_canon_code(gt$gate_code))
    add("V-S1", "gates", bad, gkeys[bad], "gate_code is not in canonical form")
    bad <- which(has(gt$gate_var) != has(gt$gate_code))
    add("V-S1", "gates", bad, gkeys[bad], "gate_var and gate_code go together")
  }
  if (!is.null(ex) && nrow(ex) > 0L) {
    ekeys <- paste(ex$study, ex$wave, ex$target, ex$source_var, ex$value, ex$na_reason, sep = "/")
    bad <- which(is.na(ex$study) | is.na(ex$wave) | is.na(ex$target) | is.na(ex$source_var) | is.na(ex$n) | ex$n < 0L)
    add("V-S1", "expected", bad, ekeys[bad], "study, wave, target, source_var and a count of 0 or more are required")
    bad <- which(has(ex$value) == has(ex$na_reason))
    add("V-S1", "expected", bad, ekeys[bad], "exactly one of value and na_reason is set")
  }
  hs <- spec$tables$hashes
  if (!is.null(hs) && nrow(hs) > 0L) {
    hkeys <- paste(hs$study, hs$wave, hs$target, hs$source_var, sep = "/")
    bad <- which(is.na(hs$study) | is.na(hs$wave) | is.na(hs$target) | is.na(hs$source_var) |
                   is.na(hs$n) | hs$n < 0L | !grepl("^[0-9a-f]{32}$", hs$md5))
    add("V-S1", "hashes", bad, hkeys[bad], "study, wave, target, source_var, a count of 0 or more and an md5 are required")
  }

  # ---- V-S2: unique keys ------------------------------------------------------------
  # `rows`: the table rows of `keys` (when the keys are a subset of the table)
  dup <- function(table, keys, label, rows = seq_along(keys[[1]])) {
    k <- do.call(paste, c(keys, sep = "/"))
    bad <- which(duplicated(k))
    add("V-S2", table, rows[bad], k[bad], sprintf("duplicate %s", label))
  }
  dup("targets", list(tg$target), "target")
  lv_rows <- which(!inh)
  dup("levels", list(lv$levels_id[!inh], lv$code[!inh]), "(levels_id, code)", lv_rows)
  dup("levels", list(lv$levels_id[!inh], lv$name[!inh]), "(levels_id, name)", lv_rows)
  # the sets as expanded by inherits=: a set must not reuse a code or a name
  # of a set it includes (a duplicate within the rows of one set is reported
  # above, so only duplicates across sets are reported here)
  within <- function(col) {
    raw <- lv[!inh & has(lv$levels_id), , drop = FALSE]
    k <- paste(raw$levels_id, raw[[col]], sep = "\x1f")
    unique(raw[[col]][duplicated(k)])
  }
  for (col in c("code", "name")) {
    seen_dup <- within(col)
    for (id in unique(lv$levels_id[inh])) {
      s <- sets[[id]]
      if (is.null(s)) next
      v <- s[[col]][!is.na(s[[col]])]
      d <- unique(v[duplicated(v)])
      d <- d[!d %in% seen_dup]
      if (length(d) > 0L) {
        add("V-S2", "levels", NA, paste(id, d, sep = "/"), sprintf("duplicate %s after inherits=", col))
      }
    }
  }
  dup("crosswalk", list(xw$study, xw$wave, xw$target, xw$source_var), "(study, wave, target, source_var)")
  active <- has(xw$rule) & xw$rule != "none"
  k_active <- paste(xw$study, xw$wave, xw$target, sep = "/")
  bad <- which(active & duplicated(ifelse(active, k_active, NA_character_), incomparables = NA))
  add("V-S2", "crosswalk", bad, k_active[bad], "more than one mapped row for (study, wave, target)")
  dup("valuemaps", list(vm$map_id, vm$source_code), "(map_id, source_code)")
  dup("waves", list(wv$study, wv$wave), "(study, wave)")
  dup("waves", list(wv$study, wv$wave_order), "(study, wave_order)")
  dup("weights", list(wt$study, wt$wave, wt$weight_var), "(study, wave, weight_var)")
  if (!is.null(gt) && nrow(gt) > 0L) {
    dup("gates", list(gt$study, gt$wave, gt$source_var, gt$gate_var, gt$gate_code, gt$source_code),
        "(study, wave, source_var, gate_var, gate_code, source_code)")
  }
  if (!is.null(ex) && nrow(ex) > 0L) {
    dup("expected", list(ex$study, ex$wave, ex$target, ex$source_var, ex$value, ex$na_reason),
        "(study, wave, target, source_var, value, na_reason)")
  }
  if (!is.null(hs) && nrow(hs) > 0L) {
    dup("hashes", list(hs$study, hs$target), "(study, target)")
  }
  for (i in seq_len(nrow(xw))) {
    nac <- names(.qes_parse_kv(xw$na_codes[i]) %||% character(0))
    mapped <- if (has(xw$map_id[i])) vm$source_code[vm$map_id == xw$map_id[i]] else character(0)
    both <- intersect(nac, mapped)
    if (length(both) > 0L) {
      add("V-S2", "crosswalk", i, xkey(i), sprintf("code(s) %s both in na_codes and in the value map", paste(both, collapse = ", ")))
    }
  }

  # ---- V-S3: enums ------------------------------------------------------------------
  check_enum <- function(table, x, name, col, keys, allow = character(0)) {
    bad <- which(has(x) & !(x %in% c(enum(name), allow)))
    add("V-S3", table, bad, keys[bad], sprintf("%s '%s' is not a value of enum %s", col, x[bad], name))
  }
  check_enum("targets", tg$block, "block", "block", tg$target)
  check_enum("targets", tg$type, "target_type", "type", tg$target)
  check_enum("targets", tg$target_timing, "target_timing", "target_timing", tg$target)
  check_enum("targets", tg$jurisdiction, "jurisdiction", "jurisdiction", tg$target)
  check_enum("targets", tg$election_ref_rule, "election_ref_rule", "election_ref_rule", tg$target)
  check_enum("targets", tg$status, "target_status", "status", tg$target)
  xkeys <- vapply(seq_len(nrow(xw)), xkey, character(1))
  rule_base <- ifelse(has(xw$rule) & startsWith(xw$rule, "fn:"), "fn", xw$rule)
  check_enum("crosswalk", rule_base, "rule", "rule", xkeys)
  bad <- which(rule_base == "fn" & !grepl("^fn:[a-z][a-z0-9_]*$", xw$rule))
  add("V-S3", "crosswalk", bad, xkeys[bad], "rule fn:<name> needs a function name")
  check_enum("crosswalk", xw$grade, "grade", "grade", xkeys)
  check_enum("crosswalk", xw$dk_offered, "dk_offered", "dk_offered", xkeys)
  check_enum("crosswalk", xw$status, "row_status", "status", xkeys)
  mode_ok <- function(m) !has(m) | m %in% enum("mode") | grepl("^var:[A-Za-z][A-Za-z0-9_.]*$", m)
  bad <- which(!mode_ok(xw$mode))
  add("V-S3", "crosswalk", bad, xkeys[bad], "mode must be web, phone, mixed or var:<name>")
  for (i in seq_len(nrow(xw))) {
    nac <- .qes_parse_kv(xw$na_codes[i]) %||% character(0)
    bad <- setdiff(unname(nac), na_reasons)
    if (length(bad) > 0L) {
      add("V-S3", "crosswalk", i, xkeys[i], sprintf("na_codes reason(s) %s not in the spec NA vocabulary", paste(bad, collapse = ", ")))
    }
    if (identical(rule_base[i], "coalesce")) {
      bad <- setdiff(.qes_hz_coalesce_fallthrough(xw, i), c(na_reasons, "sysmis"))
      if (length(bad) > 0L) {
        add("V-S3", "crosswalk", i, xkeys[i], sprintf("args fallthrough reason(s) %s not in the spec NA vocabulary", paste(bad, collapse = ", ")))
      }
    }
    to <- .qes_parse_kv(xw$gate_to[i]) %||% character(0)
    lv_names <- target_set(xw$target[i])$name %||% character(0)
    bad <- setdiff(unname(to), c(na_reasons, lv_names))
    if (length(bad) > 0L) {
      add("V-S3", "crosswalk", i, xkeys[i], sprintf("gate_to outcome(s) %s are neither NA reasons nor levels of the target", paste(bad, collapse = ", ")))
    }
  }
  vkeys <- paste(vm$map_id, vm$source_code, sep = "/")
  bad <- which(has(vm$na_reason) & !vm$na_reason %in% na_reasons)
  add("V-S3", "valuemaps", bad, vkeys[bad], "na_reason is not in the spec NA vocabulary")
  check_enum("valuemaps", vm$source_label_origin, "label_source", "source_label_origin", vkeys)
  bad <- which(vm$source_label_origin %in% c("none", "file_malformed"))
  add("V-S3", "valuemaps", bad, vkeys[bad], "source_label_origin must say where the label comes from")
  wkeys <- paste(wv$study, wv$wave, sep = "/")
  check_enum("waves", wv$wave_timing, "wave_timing", "wave_timing", wkeys)
  check_enum("waves", wv$wave_design, "wave_design", "wave_design", wkeys)
  check_enum("waves", wv$date_format, "date_format", "date_format", wkeys)
  bad <- which(!mode_ok(wv$mode))
  add("V-S3", "waves", bad, wkeys[bad], "mode must be web, phone, mixed or var:<name>")
  tkeys <- paste(wt$study, wt$wave, wt$weight_var, sep = "/")
  check_enum("weights", wt$role, "weight_role", "role", tkeys)
  check_enum("weights", wt$scale, "weight_scale", "scale", tkeys)
  check_enum("weights", wt$status, "weight_status", "status", tkeys)
  check_enum("changes", ch$kind, "spec_change", "kind", ch$spec_version)

  # ---- V-S4: referential integrity ----------------------------------------------------
  bad <- which(!xw$study %in% study_codes)
  add("V-S4", "crosswalk", bad, xkeys[bad], "study is not in the catalog")
  # wave "*" names every wave of a study: in a study whose waves are all
  # poll waves (each respondent answered in one poll), or, in a crosswalk
  # row, for a target that is not tied to one election period (timing
  # "static" or "any"), asked in whichever wave the respondent took part
  # (V-S9 checks the timing against every wave)
  star_ok <- function(study, wave, target = NULL) {
    timing <- if (is.null(target)) rep(NA_character_, length(study)) else tg$target_timing[match(target, tg$target)]
    vapply(seq_along(study), function(k) {
      identical(wave[k], .qes_all_waves) && any(wv$study %in% study[k]) &&
        (.qes_poll_study(wv, study[k]) || timing[k] %in% c("static", "any"))
    }, logical(1))
  }
  star_bad <- function(study, wave, target = NULL) wave %in% .qes_all_waves & !star_ok(study, wave, target)
  bad <- which(!paste(xw$study, xw$wave) %in% paste(wv$study, wv$wave) & !star_ok(xw$study, xw$wave, xw$target))
  add("V-S4", "crosswalk", bad, xkeys[bad], "(study, wave) is not in waves.csv")
  bad <- which(star_bad(xw$study, xw$wave, xw$target))
  add("V-S4", "crosswalk", bad, xkeys[bad], "wave * is allowed only in a study whose waves are all poll waves, or for a target of timing static or any")
  bad <- which(!xw$target %in% tg$target)
  add("V-S4", "crosswalk", bad, xkeys[bad], "target is not in targets.csv")
  bad <- which(xw$rule %in% c("map", "coalesce") & !has(xw$map_id))
  add("V-S4", "crosswalk", bad, xkeys[bad], "rules map and coalesce need a map_id")
  bad <- which(!xw$rule %in% c("map", "coalesce") & has(xw$map_id))
  add("V-S4", "crosswalk", bad, xkeys[bad], "only rules map and coalesce take a map_id")
  # every map a row reads: its map_id, and the maps of a coalesce row's
  # other variables (args then)
  row_maps <- lapply(seq_len(nrow(xw)), function(i) .qes_hz_row_maps(xw, i))
  map_rows <- data.frame(map_id = unlist(row_maps), row = rep(seq_len(nrow(xw)), lengths(row_maps)),
                         stringsAsFactors = FALSE)
  bad <- unique(map_rows$row[!map_rows$map_id %in% vm$map_id])
  add("V-S4", "crosswalk", bad, xkeys[bad], "map_id (or a map of args then) is not in valuemaps.csv")
  # the rx_ maps are read by the relaxed rows (V-R8 checks them)
  unused <- setdiff(unique(vm$map_id), map_rows$map_id)
  unused <- unused[!startsWith(unused, "rx_")]
  add("V-S4", "valuemaps", NA, unused, "map_id is not used by any crosswalk row")
  bad <- which(has(xw$election_ref) & !xw$election_ref %in% elections$election_id)
  add("V-S4", "crosswalk", bad, xkeys[bad], "election_ref is not in catalog/elections.csv")
  files <- cat_$files
  for (i in which(has(xw$wording_ref))) {
    refs <- split(xw$wording_ref[i])
    ids <- sub(":.*$", "", refs)
    bad_form <- refs[!grepl("^[0-9]+:.+$", refs)]
    if (length(bad_form) > 0L) {
      add("V-S4", "crosswalk", i, xkeys[i], sprintf("wording_ref entry %s is not <file_id>:<ref>", paste(bad_form, collapse = ", ")))
    }
    ids <- unique(ids[grepl("^[0-9]+:.+$", refs)])
    unknown <- ids[!ids %in% as.character(files$file_id)]
    if (length(unknown) > 0L) {
      add("V-S4", "crosswalk", i, xkeys[i], sprintf("wording_ref file(s) %s are not in the catalog", paste(unknown, collapse = ", ")))
    }
    other <- setdiff(ids, unknown)
    other <- other[files$study[match(other, as.character(files$file_id))] != xw$study[i]]
    if (length(other) > 0L) {
      add("V-S4", "crosswalk", i, xkeys[i], sprintf("wording_ref file(s) %s belong to another study", paste(other, collapse = ", ")))
    }
  }
  map_target <- xw$target[map_rows$row]
  for (id in unique(map_rows$map_id)) {
    ids <- unique(tg$levels_id[match(map_target[map_rows$map_id %in% id], tg$target)])
    if (length(ids) > 1L) {
      add("V-S4", "valuemaps", NA, id, sprintf("map used by targets with different level sets (%s)", paste(ids, collapse = ", ")))
    }
    set <- target_set(map_target[match(id, map_rows$map_id)])
    rows <- which(vm$map_id == id & !is.na(vm$target_code))
    if (!is.null(set)) {
      bad <- rows[!vm$target_code[rows] %in% set$code]
      add("V-S4", "valuemaps", bad, vkeys[bad], "target_code is not a code of the target's level set")
    }
  }
  needs_set <- tg$type %in% c("categorical", "ordinal")
  bad <- which(needs_set & !has(tg$levels_id))
  add("V-S4", "targets", bad, tg$target[bad], "a categorical or ordinal target needs levels_id")
  bad <- which(!needs_set & has(tg$levels_id))
  add("V-S4", "targets", bad, tg$target[bad], "only categorical and ordinal targets take levels_id")
  bad <- which(has(tg$levels_id) & !tg$levels_id %in% set_ids)
  add("V-S4", "targets", bad, tg$target[bad], "levels_id is not in levels.csv")
  broken <- names(sets)[vapply(sets, is.null, logical(1))]
  add("V-S4", "levels", NA, broken, "inherits= names a missing level set, or inheritance cycles")
  anchors <- paste(xw$study, xw$wave, xw$source_var, sep = ":")
  for (j in seq_len(nrow(tg))) {
    rows <- which(xw$target == tg$target[j])
    if (length(rows) == 0L) next
    if (!has(tg$anchor_row[j])) {
      add("V-S4", "targets", j, tg$target[j], "a target with crosswalk rows needs anchor_row")
    } else if (!tg$anchor_row[j] %in% anchors[rows]) {
      add("V-S4", "targets", j, tg$target[j], "anchor_row is not a crosswalk row of the target")
    }
  }
  bad <- which(has(tg$replaced_by) & !tg$replaced_by %in% tg$target)
  add("V-S4", "targets", bad, tg$target[bad], "replaced_by is not a target")
  bad <- which(has(tg$derive_rule) & !tg$derive_rule %in% .qes_hz_derivations)
  add("V-S4", "targets", bad, tg$target[bad], "derive_rule is not a registered derivation")
  bad <- which(has(tg$derive_rule) != has(tg$derive_from))
  add("V-S4", "targets", bad, tg$target[bad], "derive_rule and derive_from go together")
  for (j in which(has(tg$derive_from))) {
    miss <- setdiff(split(tg$derive_from[j]), tg$target)
    if (length(miss) > 0L) {
      add("V-S4", "targets", j, tg$target[j], sprintf("derive_from names unknown target(s) %s", paste(miss, collapse = ", ")))
    }
  }
  bad <- which(!wv$study %in% study_codes)
  add("V-S4", "waves", bad, wkeys[bad], "study is not in the catalog")
  bad <- which(has(wv$election_ref) & !wv$election_ref %in% elections$election_id)
  add("V-S4", "waves", bad, wkeys[bad], "election_ref is not in catalog/elections.csv")
  bad <- which(!paste(wt$study, wt$wave) %in% paste(wv$study, wv$wave) & !star_ok(wt$study, wt$wave))
  add("V-S4", "weights", bad, tkeys[bad], "(study, wave) is not in waves.csv")
  bad <- which(star_bad(wt$study, wt$wave))
  add("V-S4", "weights", bad, tkeys[bad], "wave * is allowed only in a study whose waves are all poll waves")
  both <- intersect(wt$study[wt$wave %in% .qes_all_waves], wt$study[!wt$wave %in% .qes_all_waves])
  add("V-S4", "weights", NA, both, "a study's weights are registered for wave * or wave by wave, not both")
  # a mode that varies by respondent (var:<name>) is read through the
  # wave's survey_mode crosswalk row on that variable
  for (w in which(has(wv$mode) & grepl("^var:", wv$mode))) {
    v <- sub("^var:", "", wv$mode[w])
    ok <- any(xw$study == wv$study[w] & xw$wave %in% c(wv$wave[w], .qes_all_waves) & xw$target %in% "survey_mode" &
                xw$source_var %in% v & xw$rule %in% "map")
    if (!ok) {
      add("V-S4", "waves", w, wkeys[w], sprintf("mode var:%s needs a survey_mode crosswalk row (rule map) on %s in this wave", v, v))
    }
  }
  # targets of the id and design blocks are leading columns: never in a set
  lead <- which(tg$block %in% c("id", "design") & has(tg$sets))
  add("V-S4", "targets", lead, tg$target[lead], "a target of the id or design block is a leading column and cannot be in a set")
  if (!is.null(gt) && nrow(gt) > 0L) {
    # the gate is part of the match: cells counted under a gate the row no
    # longer has are never read (.qes_hz_cells() filters on gate_var)
    gate_or_empty <- function(x) ifelse(is.na(x), "", x)
    xw_keys <- paste(xw$study, xw$wave, xw$source_var, gate_or_empty(xw$gate_var), sep = "\x1f")
    gt_keys <- paste(gt$study, gt$wave, gt$source_var, gate_or_empty(gt$gate_var), sep = "\x1f")
    bad <- which(!gt_keys %in% xw_keys)
    add("V-S4", "gates", bad, gkeys[bad], "(study, wave, source_var, gate_var) is not a crosswalk row")
    # cells counted under another membership rule than the wave's are stale
    # (once per study and wave)
    gw <- paste(gt$study, gt$wave, sep = "\x1f")
    first <- match(unique(gw), gw)
    rule_of <- vapply(first, function(k) {
      w <- .qes_wave_rows(wv, gt$wave[k], gt$study[k])
      if (length(w) == 1L || (length(w) > 1L && identical(gt$wave[k], .qes_all_waves))) {
        .qes_member_rule_rows(wv, w)
      } else {
        NA_character_
      }
    }, character(1))
    w_rule <- unname(rule_of[match(gw, gw[first])])
    bad <- which(!is.na(w_rule) & gate_or_empty(gt$member_rule) != w_rule)
    bad <- bad[!duplicated(gt_keys[bad])]
    add("V-S4", "gates", bad, gkeys[bad], sprintf(
      "counted under membership rule '%s', the wave has '%s'; run data-raw/build_sources.R",
      gate_or_empty(gt$member_rule[bad]), w_rule[bad]
    ))
  }
  if (!is.null(ex) && nrow(ex) > 0L) {
    bad <- which(!paste(ex$study, ex$wave, ex$target, ex$source_var) %in% paste(xw$study, xw$wave, xw$target, xw$source_var))
    add("V-S4", "expected", bad, ekeys[bad], "(study, wave, target, source_var) is not a crosswalk row")
  }
  if (!is.null(hs) && nrow(hs) > 0L) {
    # a pooled variable's hash (V-F9) is keyed by wave "*" and its member types
    pooled_hash <- hs$target %in% .qes_pool_names(spec, live = FALSE) & hs$wave %in% .qes_all_waves
    bad <- which(!pooled_hash & !paste(hs$study, hs$wave, hs$target, hs$source_var) %in%
                   paste(xw$study, xw$wave, xw$target, xw$source_var)[xw$primary %in% TRUE])
    add("V-S4", "hashes", bad, hkeys[bad], "(study, wave, target, source_var) is not a primary crosswalk row")
  }

  # ---- V-S5: one primary row per (study, target) -----------------------------------------
  prim <- which(xw$primary %in% TRUE)
  k <- paste(xw$study[prim], xw$target[prim], sep = "/")
  bad <- prim[duplicated(k)]
  add("V-S5", "crosswalk", bad, xkeys[bad], "more than one primary row for (study, target)")

  # ---- V-S6: English and French ------------------------------------------------------------
  pair <- function(table, x, en, fr, keys, need = TRUE, rows = seq_len(nrow(x))) {
    a <- has(x[[en]])
    b <- has(x[[fr]])
    bad <- if (need) which(!a | !b) else which(a != b)
    add("V-S6", table, rows[bad], keys[bad], sprintf("%s and %s must both be %s", en, fr, if (need) "present" else "present or both empty"))
  }
  pair("targets", tg, "label_en", "label_fr", tg$target)
  pair("targets", tg, "description_en", "description_fr", tg$target)
  pair("levels", lv[!inh, , drop = FALSE], "label_en", "label_fr", paste(lv$levels_id, lv$name, sep = "/")[!inh], rows = lv_rows)
  pair("crosswalk", xw, "grade_reason_en", "grade_reason_fr", xkeys)
  pair("crosswalk", xw, "notes_en", "notes_fr", xkeys, need = FALSE)
  pair("waves", wv, "target_population_en", "target_population_fr", wkeys, need = FALSE)
  pair("changes", ch, "change_en", "change_fr", ch$spec_version)

  # ---- V-S7: UTF-8, NFC, no replacement or control characters ---------------------------------
  bad_cp <- function(s) {
    cp <- utf8ToInt(s)
    if (anyNA(cp)) {
      return("invalid UTF-8")
    }
    if (any(cp == 0xFFFDL)) return("U+FFFD replacement character")
    if (any(cp >= 0x80L & cp <= 0x9FL)) return("C1 control character")
    if (any(cp < 0x20L | cp == 0x7FL)) return("control character")
    if (any(cp >= 0x300L & cp <= 0x36FL)) return("combining mark (text is not NFC)")
    NA_character_
  }
  for (tab in names(spec$tables)) {
    x <- spec$tables[[tab]]
    for (col in names(x)) {
      v <- x[[col]]
      if (!is.character(v)) next
      # printable ASCII needs no closer look
      plain <- !grepl("[^\x20-\x7E]", v, useBytes = TRUE)
      for (i in which(has(v) & !plain)) {
        why <- if (!validUTF8(v[i])) "invalid UTF-8" else bad_cp(v[i])
        if (!is.na(why)) add("V-S7", tab, i, col, why)
      }
    }
  }
  for (f in c("version", "date", "licence", "hash_recorded", "engine_min")) {
    v <- spec[[f]]
    if (has(v) && (!validUTF8(v) || !is.na(bad_cp(v)))) add("V-S7", "SPEC", NA, f, "invalid characters")
  }

  # ---- V-S8: alias contradictions ------------------------------------------------------------
  # the normalized aliases of each level set (and their md5), once per set
  alias_of <- list()
  alias_hash_of <- list()
  for (id in unique(vm$map_id)) {
    set_id <- tg$levels_id[match(map_target[match(id, map_rows$map_id)], tg$target)]
    set <- target_set(map_target[match(id, map_rows$map_id)])
    if (is.null(set)) next
    if (is.null(alias_of[[set_id]])) {
      alias_of[[set_id]] <- stats::setNames(lapply(set$aliases, function(a) .qes_norm_label(split(a))), set$code)
    }
    alias <- alias_of[[set_id]]
    rows <- which(vm$map_id == id)
    labs <- .qes_norm_label(vm$source_label[rows])
    for (i in rows) {
      mapped <- vm$target_code[i]
      if (has(vm$source_label[i])) {
        lab <- labs[match(i, rows)]
        hits <- names(alias)[vapply(alias, function(a) lab %in% a, logical(1))]
      } else if (has(vm$source_label_hash[i])) {
        if (is.null(alias_hash_of[[set_id]])) alias_hash_of[[set_id]] <- lapply(alias, .qes_md5_text)
        alias_hash <- alias_hash_of[[set_id]]
        hits <- names(alias_hash)[vapply(alias_hash, function(a) vm$source_label_hash[i] %in% a, logical(1))]
      } else {
        next
      }
      contradiction <- setdiff(hits, if (is.na(mapped)) character(0) else as.character(mapped))
      if (length(contradiction) > 0L && !has(vm$alias_exception[i])) {
        add("V-S8", "valuemaps", i, vkeys[i], sprintf(
          "source label matches an alias of level %s but maps to %s",
          paste(set$name[match(contradiction, set$code)], collapse = ", "),
          if (is.na(mapped)) vm$na_reason[i] else set$name[match(mapped, set$code)]
        ))
      }
    }
  }

  # ---- V-S9: timing and election reference ------------------------------------------------------
  for (i in seq_len(nrow(xw))) {
    j <- match(xw$target[i], tg$target)
    star <- identical(xw$wave[i], .qes_all_waves)
    w <- .qes_wave_rows(wv, xw$wave[i], xw$study[i])
    if (is.na(j) || length(w) == 0L || (length(w) > 1L && !star)) next
    tt <- tg$target_timing[j]
    for (wt_ in unique(wv$wave_timing[w])) {
      if (tt %in% c("pre", "post") && !wt_ %in% c(tt, "between")) {
        add("V-S9", "crosswalk", i, xkeys[i], sprintf("a %s-election target in a %s-election wave", tt, wt_))
      }
    }
    rule <- tg$election_ref_rule[j]
    ref <- xw$election_ref[i]
    s_ref <- studies$election_id[match(xw$study[i], studies$study)]
    if (star && !identical(rule, "none") && has(rule)) {
      # a row over every poll wave refers, poll by poll, to the election of
      # each wave (waves.csv), which must be set, in the target's jurisdiction
      if (has(ref)) {
        add("V-S9", "crosswalk", i, xkeys[i], "a row of wave * takes the election of each wave: leave election_ref empty")
      }
      w_ref <- wv$election_ref[w]
      if (!all(has(w_ref))) {
        add("V-S9", "crosswalk", i, xkeys[i], "a row of wave * needs election_ref on every wave of the study")
      }
      e <- match(w_ref[has(w_ref)], elections$election_id)
      if (has(tg$jurisdiction[j]) && any(!is.na(e) & elections$jurisdiction[e] != tg$jurisdiction[j])) {
        add("V-S9", "crosswalk", i, xkeys[i], "a wave's election_ref is in another jurisdiction than the target")
      }
    } else if (star) {
      if (has(ref)) add("V-S9", "crosswalk", i, xkeys[i], "the target has no election reference, but election_ref is set")
    } else if (identical(rule, "none") || !has(rule)) {
      if (has(ref)) add("V-S9", "crosswalk", i, xkeys[i], "the target has no election reference, but election_ref is set")
    } else if (!has(ref)) {
      add("V-S9", "crosswalk", i, xkeys[i], "the target needs election_ref")
    } else {
      e <- match(ref, elections$election_id)
      if (!is.na(e) && has(tg$jurisdiction[j]) && elections$jurisdiction[e] != tg$jurisdiction[j]) {
        add("V-S9", "crosswalk", i, xkeys[i], "election_ref is in another jurisdiction than the target")
      }
      if (identical(rule, "study") && has(s_ref) && ref != s_ref) {
        add("V-S9", "crosswalk", i, xkeys[i], sprintf("election_ref must be the study's election %s", s_ref))
      }
      if (identical(rule, "previous") && has(s_ref) && !is.na(e)) {
        s_date <- elections$election_date[match(s_ref, elections$election_id)]
        if (!is.na(s_date) && elections$election_date[e] >= s_date) {
          add("V-S9", "crosswalk", i, xkeys[i], "election_ref must be an election before the study's")
        }
      }
    }
  }
  s_ref <- studies$election_id[match(wv$study, studies$study)]
  bad <- which(has(wv$election_ref) & has(s_ref) & wv$election_ref != s_ref)
  add("V-S9", "waves", bad, wkeys[bad], "election_ref differs from the study's election in the catalog")

  # ---- V-S10: registered functions -----------------------------------------------------------
  fn_rows <- which(rule_base %in% "fn")
  fns <- sub("^fn:", "", xw$rule[fn_rows])
  bad <- fn_rows[!fns %in% names(.qes_hz_fns)]
  add("V-S10", "crosswalk", bad, xkeys[bad], "fn: names no registered function")
  n_active <- sum(active)
  if (n_active > 0L && length(fn_rows) > 0.1 * n_active) {
    add("V-S10", "crosswalk", NA, "fn:", sprintf("%d of %d mapped rows use fn: rules (at most 10%%)", length(fn_rows), n_active))
  }
  if (!is.null(source_dir)) {
    for (f in unique(fns)) {
      test <- file.path(source_dir, "tests", "testthat", sprintf("test-hz-fn-%s.R", f))
      if (!file.exists(test)) add("V-S10", "crosswalk", NA, paste0("fn:", f), "no test file tests/testthat/test-hz-fn-<name>.R")
    }
  }

  # ---- V-S11: review requirements and licence -------------------------------------------------
  stable <- which(xw$status %in% "stable")
  for (i in stable) {
    miss <- c(
      evidence = !has(xw$evidence[i]), reviewed_by = !has(xw$reviewed_by[i]),
      reviewed_on = is.na(xw$reviewed_on[i]),
      wording = !has(xw$wording_en[i]) && !has(xw$wording_fr[i]) && !has(xw$wording_ref[i])
    )
    if (any(miss)) {
      add("V-S11", "crosswalk", i, xkeys[i], sprintf("a stable row needs %s", paste(names(miss)[miss], collapse = ", ")))
    }
  }
  bad <- which(!xw$status %in% "draft" & !has(xw$evidence))
  add("V-S11", "crosswalk", bad, xkeys[bad], "a row in review or stable needs evidence")
  # a row a reviewer has seen (reviewed_by) but left in review says why in
  # review_note
  bad <- which(xw$status %in% "review" & has(xw$reviewed_by) & !has(xw$review_note))
  add("V-S11", "crosswalk", bad, xkeys[bad], "a reviewed row left in review needs review_note (why it is held)")
  for (i in which(vm$source_label_origin %in% "ddi")) {
    st <- xw$status[xw$map_id %in% vm$map_id[i]]
    if (any(st != "draft")) {
      add("V-S11", "valuemaps", i, vkeys[i], "a label from DDI metadata is allowed only in draft rows")
    }
  }
  if (isTRUE(release)) {
    bad <- which(xw$status %in% "draft")
    add("V-S11", "crosswalk", bad, xkeys[bad], "a release spec has no draft rows")
  }

  # ---- V-S12: rule constraints ---------------------------------------------------------------
  for (i in seq_len(nrow(xw))) {
    j <- match(xw$target[i], tg$target)
    rule <- rule_base[i]
    if (identical(xw$grade[i], "not_comparable") != identical(rule, "none")) {
      add("V-S12", "crosswalk", i, xkeys[i], "not_comparable rows, and only they, use rule none")
    }
    if (identical(rule, "none") && isTRUE(xw$primary[i])) {
      add("V-S12", "crosswalk", i, xkeys[i], "a documentation-only row (rule none) cannot be primary")
    }
    if (is.na(j)) next
    if (identical(rule, "constant") && !isTRUE(tg$allow_constant[j])) {
      add("V-S12", "crosswalk", i, xkeys[i], "rule constant on a target that does not allow it")
    }
    ok <- c(.qes_type_rules[[tg$type[j]]] %||% character(0), "fn", "none")
    if (has(rule) && !rule %in% ok) {
      add("V-S12", "crosswalk", i, xkeys[i], sprintf("rule %s does not fit a %s target", rule, tg$type[j]))
    }
    if (identical(rule, "numeric")) {
      args <- .qes_parse_kv(xw$args[i]) %||% character(0)
      lo <- suppressWarnings(as.numeric(unname(args["min"])))
      hi <- suppressWarnings(as.numeric(unname(args["max"])))
      if ((is.na(lo) || is.na(hi)) && (is.na(tg$valid_min[j]) || is.na(tg$valid_max[j]))) {
        add("V-S12", "crosswalk", i, xkeys[i], "a numeric row needs min and max (in args or in the target)")
      }
      if ((!is.na(lo) && !is.na(tg$valid_min[j]) && lo < tg$valid_min[j]) ||
          (!is.na(hi) && !is.na(tg$valid_max[j]) && hi > tg$valid_max[j])) {
        add("V-S12", "crosswalk", i, xkeys[i], "args min/max lie outside the target's valid range")
      }
    }
  }

  # ---- V-S13: weights ---------------------------------------------------------------------------
  rec <- wt$recommended %in% TRUE
  bad <- which(rec & wt$role %in% c("vote_calibrated", "turnout_calibrated"))
  add("V-S13", "weights", bad, tkeys[bad], "a recommended weight cannot be calibrated on vote or turnout")
  # a study-wave whose registered weights are all calibrated on vote or
  # turnout (qes2008) has none to recommend: its harmonized weights are NA
  calibrated <- wt$role %in% c("vote_calibrated", "turnout_calibrated")
  for (k in unique(paste(wt$study, wt$wave, sep = "/"))) {
    rows <- which(paste(wt$study, wt$wave, sep = "/") == k)
    n_rec <- sum(rec[rows])
    if (n_rec > 1L || (n_rec == 0L && !all(calibrated[rows]))) {
      add("V-S13", "weights", NA, k, sprintf("%d recommended weights; each study-wave with a weight not calibrated on vote or turnout has exactly one", n_rec))
    }
  }
  weighted <- c(paste(wt$study, wt$wave, sep = "/"), wkeys[wv$study %in% wt$study[wt$wave %in% .qes_all_waves]])
  no_weight <- setdiff(wkeys, weighted)
  add("V-S13", "waves", NA, no_weight, "no registered weight: harmonized weights will be NA", severity = "note")
  # A weight that needs review does not hold the crosswalk rows of its waves
  # (spec 4.1.0): the sign-off of a row is about its content, and the weight
  # is applied only once it is reviewed (its harmonized values are NA, with
  # qesR_message_weight_review, until then).

  # ---- V-S14: ordinal maps are monotone -------------------------------------------------------
  for (i in which(xw$rule %in% "map" & xw$target %in% tg$target[tg$type %in% "ordinal"])) {
    set <- target_set(xw$target[i])
    rows <- which(vm$map_id == xw$map_id[i] & !is.na(vm$target_code))
    src <- suppressWarnings(as.numeric(vm$source_code[rows]))
    if (is.null(set) || length(rows) < 2L || anyNA(src)) next
    ord <- set$order[match(vm$target_code[rows], set$code)][order(src)]
    # a code outside the set is reported by V-S4; the order cannot be judged
    if (anyNA(ord)) next
    d <- diff(ord)
    if (!(all(d >= 0) || all(d <= 0))) {
      args <- .qes_parse_kv(xw$args[i]) %||% character(0)
      explained <- identical(unname(args["nonmonotone"]), "TRUE") && has(xw$notes_en[i]) && has(xw$notes_fr[i])
      if (!explained) {
        add("V-S14", "crosswalk", i, xkeys[i], "ordinal map is not monotone in source order (set args nonmonotone=TRUE and explain in notes)")
      }
    }
  }

  # ---- V-S15: names --------------------------------------------------------------------------
  set_names <- unique(unlist(lapply(tg$sets, split)))
  spaces <- list(target = tg$target, family = unique(tg$family[has(tg$family)]), set = set_names)
  for (sp in names(spaces)) {
    nm <- spaces[[sp]]
    bad <- nm[!grepl(.qes_name_pattern, nm)]
    add("V-S15", "targets", NA, bad, sprintf("%s name does not match %s", sp, .qes_name_pattern))
    bad <- intersect(nm, study_names)
    add("V-S15", "targets", NA, bad, sprintf("%s name equals a study code", sp))
  }
  for (a in 1:2) for (b in (a + 1L):3) {
    both <- intersect(spaces[[a]], spaces[[b]])
    add("V-S15", "targets", NA, both, sprintf("name is both a %s and a %s", names(spaces)[a], names(spaces)[b]))
  }
  # a target that is not a leading column (id and design blocks) must not
  # take the name of one
  plain <- tg$target[!tg$block %in% c("id", "design")]
  bad <- intersect(plain, .qes_hz_leading_columns)
  add("V-S15", "targets", NA, bad, "target name is the name of a leading column of harmonized data")

  # ---- V-S16: offered levels ---------------------------------------------------------------------
  for (i in seq_len(nrow(xw))) {
    set <- target_set(xw$target[i])
    lo <- split(xw$levels_offered[i])
    if (is.null(set)) {
      if (length(lo) > 0L) add("V-S16", "crosswalk", i, xkeys[i], "levels_offered on a target without levels")
      next
    }
    extra <- setdiff(lo, set$name)
    if (length(extra) > 0L) {
      add("V-S16", "crosswalk", i, xkeys[i], sprintf("levels_offered has level(s) %s not in the target's set", paste(extra, collapse = ", ")))
    }
    if (!xw$rule[i] %in% c("map", "coalesce")) next
    if (length(lo) == 0L) {
      if (!xw$status[i] %in% "draft") add("V-S16", "crosswalk", i, xkeys[i], "a map row in review or stable needs levels_offered")
      next
    }
    codes <- vm$target_code[vm$map_id %in% row_maps[[i]] & !is.na(vm$target_code)]
    names_ <- set$name[match(codes, set$code)]
    extra <- setdiff(names_, c(lo, "other"))
    if (length(extra) > 0L) {
      add("V-S16", "crosswalk", i, xkeys[i], sprintf("mapped level(s) %s are not in levels_offered", paste(unique(extra), collapse = ", ")))
    }
  }

  # ---- V-S17: identical rows match their anchor ----------------------------------------------------
  mode_family <- function(i) {
    m <- xw$mode[i]
    if (!has(m)) {
      m <- wv$mode[.qes_wave_rows(wv, xw$wave[i], xw$study[i])[1]]
    }
    if (is.na(m)) return(NA_character_)
    if (m %in% c("web", "phone")) m else "mixed"
  }
  for (j in seq_len(nrow(tg))) {
    # the anchor row of this target (two targets may share a source, e.g.
    # the three and six age bands of one question)
    a <- which(anchors == tg$anchor_row[j] & xw$target == tg$target[j])[1]
    if (is.na(a)) next
    if (!identical(xw$grade[a], "identical")) {
      add("V-S17", "crosswalk", a, xkeys[a], "the anchor row must be graded identical")
    }
    for (i in which(xw$target == tg$target[j] & xw$grade %in% "identical" & seq_len(nrow(xw)) != a)) {
      diff_ <- c(
        levels_offered = !setequal(split(xw$levels_offered[i]), split(xw$levels_offered[a])),
        dk_offered = !identical(xw$dk_offered[i], xw$dk_offered[a]) || xw$dk_offered[i] %in% "unknown",
        instrument = !identical(xw$instrument[i], xw$instrument[a]),
        mode = !identical(mode_family(i), mode_family(a))
      )
      if (any(diff_)) {
        add("V-S17", "crosswalk", i, xkeys[i], sprintf("graded identical but differs from the anchor in %s", paste(names(diff_)[diff_], collapse = ", ")))
      }
    }
  }

  # ---- V-S18: the legacy renderer (legacy.csv) ----------------------------------------------------
  lg <- spec$tables$legacy
  if (!is.null(lg) && nrow(lg) > 0L) {
    out <- c(out, list(.qes_legacy_check(lg, tg, sets, study_codes, target_set)))
    # V-S19, the legacy freeze (spec 4.3.0): the legacy renderers never read a row
    # nobody has reviewed yet (reviewed_by empty, not stable). A note lists
    # the rows that a legacy column would read once signed off, so that the
    # sign-off is a decision about that column too
    fresh <- which(is.na(xw$reviewed_by) & !xw$status %in% "stable" & has(xw$rule) & xw$rule != "none" &
                     xw$primary %in% TRUE)
    for (i in fresh) {
      rows <- .qes_legacy_rows(lg, xw$study[i])
      hit <- rows$column[vapply(rows$target, function(x) xw$target[i] %in% .qes_split_list(x), logical(1))]
      if (length(hit) > 0L) {
        add("V-S19", "crosswalk", i, xkeys[i], sprintf(
          "not reviewed: the legacy renderers ignore it; signing it off changes the legacy column(s) %s of %s",
          paste(unique(hit), collapse = ", "), xw$study[i]), severity = "note")
      }
    }
  }

  # ---- V-F1 to V-F7: pooled variables (pooled.csv, pooled_members.csv) ------------------------
  out <- c(out, list(.qes_pool_check(spec, tg, lv, sets, target_set, enum, study_names)))

  # ---- V-R1 to V-R8, V-R10 to V-R12: the relaxed layer (relaxed.csv, relaxed_maps.csv) ---------
  out <- c(out, list(.qes_rx_check(spec, sets, enum, study_names, na_reasons)))

  # ---- V-P2 (hash half): SPEC records the content hash and the version has a CHANGES row ----
  sev <- if (isTRUE(spec$custom)) "warning" else "error"
  if (!identical(spec$hash, spec$hash_recorded)) {
    add("V-P2", "SPEC", NA, "Hash", sprintf("SPEC Hash %s is not the content hash %s", spec$hash_recorded, spec$hash), severity = sev)
  }
  if (!spec$version %in% ch$spec_version) {
    add("V-P2", "changes", NA, spec$version, "no CHANGES row for the SPEC version", severity = sev)
  }

  res <- if (length(out) > 0L) do.call(rbind, out) else .qes_spec_problems()
  rownames(res) <- NULL
  res
}

# V-F1 to V-F7: the pooled variables of the spec (R/hz-pool.R) against the
# targets and level sets. Returns a problems table.
.qes_pool_check <- function(spec, tg, lv, sets, target_set, enum, study_names) {
  pt <- .qes_pool_tables(spec)
  pl <- pt$pooled
  pm <- pt$members
  out <- list()
  add <- function(rule, table, row, key, detail, severity = "error") {
    n <- max(length(row), length(key))
    if (length(row) == 0L || length(key) == 0L) return(invisible())
    out[[length(out) + 1L]] <<- .qes_spec_problems(
      rep_len(rule, n), rep_len(severity, n), rep_len(table, n),
      rep_len(as.integer(row), n), rep_len(key, n), rep_len(detail, n)
    )
    invisible()
  }
  has <- function(x) !is.na(x) & nzchar(x)
  if (nrow(pl) == 0L && nrow(pm) == 0L) return(.qes_spec_problems())
  mkey <- paste(pm$pooled, pm$member, sep = "/")

  # ---- V-F1: form and keys ------------------------------------------------------
  for (col in c("pooled", "type", "label_en", "label_fr", "description_en", "description_fr", "status", "added_in")) {
    bad <- which(!has(pl[[col]]))
    add("V-F1", "pooled", bad, ifelse(is.na(pl$pooled[bad]), "", pl$pooled[bad]), sprintf("column '%s' is empty", col))
  }
  for (col in c("pooled", "member", "type_name", "transform", "added_in")) {
    bad <- which(!has(pm[[col]]))
    add("V-F1", "pooled_members", bad, mkey[bad], sprintf("column '%s' is empty", col))
  }
  bad <- which(is.na(pm$precedence) | is.na(pm$default))
  add("V-F1", "pooled_members", bad, mkey[bad], "precedence and default are required")
  bad <- which(duplicated(pl$pooled))
  add("V-F1", "pooled", bad, pl$pooled[bad], "duplicate pooled variable")
  bad <- which(duplicated(mkey))
  add("V-F1", "pooled_members", bad, mkey[bad], "duplicate (pooled, member)")
  bad <- which(duplicated(paste(pm$pooled, pm$type_name)))
  add("V-F1", "pooled_members", bad, mkey[bad], "duplicate (pooled, type_name)")
  bad <- which(duplicated(paste(pm$pooled, pm$precedence)))
  add("V-F1", "pooled_members", bad, mkey[bad], "precedence repeats within the pooled variable (a strict order is required)")
  bad <- which(has(pl$type) & !pl$type %in% c("categorical", "ordinal", "numeric"))
  add("V-F1", "pooled", bad, pl$pooled[bad], "type must be categorical, ordinal or numeric")
  bad <- which(has(pl$status) & !pl$status %in% enum("target_status"))
  add("V-F1", "pooled", bad, pl$pooled[bad], "status is not a value of enum target_status")
  bad <- which(has(pl$added_in) & !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", pl$added_in))
  add("V-F1", "pooled", bad, pl$pooled[bad], "added_in must be a spec version")
  bad <- which(has(pm$added_in) & !grepl("^[0-9]+\\.[0-9]+\\.[0-9]+$", pm$added_in))
  add("V-F1", "pooled_members", bad, mkey[bad], "added_in must be a spec version")
  bad <- which(!pm$pooled %in% pl$pooled)
  add("V-F1", "pooled_members", bad, mkey[bad], "pooled is not in pooled.csv")
  none <- pl$pooled[!pl$pooled %in% pm$pooled]
  add("V-F1", "pooled", match(none, pl$pooled), none, "a pooled variable needs members")

  # ---- V-F2: names -----------------------------------------------------------------
  bad <- which(has(pl$pooled) & !grepl(.qes_name_pattern, pl$pooled))
  add("V-F2", "pooled", bad, pl$pooled[bad], sprintf("pooled name does not match %s", .qes_name_pattern))
  bad <- which(has(pm$type_name) & !grepl(.qes_name_pattern, pm$type_name))
  add("V-F2", "pooled_members", bad, mkey[bad], sprintf("type_name does not match %s", .qes_name_pattern))
  set_names <- unique(c(unlist(lapply(tg$sets, .qes_split_list)), unlist(lapply(pl$sets, .qes_split_list))))
  bad <- set_names[!grepl(.qes_name_pattern, set_names)]
  add("V-F2", "pooled", NA, bad, sprintf("set name does not match %s", .qes_name_pattern))
  spaces <- list(target = tg$target, family = unique(tg$family[has(tg$family)]),
                 set = set_names, study = study_names, `leading column` = .qes_hz_leading_columns)
  for (sp in names(spaces)) {
    both <- intersect(pl$pooled, spaces[[sp]])
    add("V-F2", "pooled", match(both, pl$pooled), both, sprintf("pooled name is also a %s name", sp))
  }

  # ---- V-F3, V-F4: members ------------------------------------------------------------
  bad <- which(!pm$member %in% tg$target)
  add("V-F3", "pooled_members", bad, mkey[bad], "member is not a target of targets.csv")
  pool_live <- !pl$status[match(pm$pooled, pl$pooled)] %in% "retired"
  bad <- which(pool_live & tg$status[match(pm$member, tg$target)] %in% "retired")
  add("V-F3", "pooled_members", bad, mkey[bad], "member is a retired target (set the pooled variable's replaced_by, or retire it)")
  lead <- tg$target[tg$block %in% c("id", "design")]
  bad <- which(pm$member %in% lead)
  add("V-F3", "pooled_members", bad, mkey[bad], "member is a leading-column target (id or design block)")
  for (j in seq_len(nrow(pl))) {
    m <- pm[pm$pooled == pl$pooled[j], , drop = FALSE]
    if (has(pl$anchor_member[j]) && !pl$anchor_member[j] %in% m$member) {
      add("V-F3", "pooled", j, pl$pooled[j], "anchor_member is not a member")
    }
    if (!has(pl$anchor_member[j])) add("V-F3", "pooled", j, pl$pooled[j], "anchor_member is empty")
    if (nrow(m) > 0L && !any(m$default %in% TRUE)) {
      add("V-F4", "pooled", j, pl$pooled[j], "no member is used by default (default = TRUE)")
    }
    if (has(pl$replaced_by[j]) && !pl$replaced_by[j] %in% pl$pooled) {
      add("V-F3", "pooled", j, pl$pooled[j], "replaced_by is not a pooled variable")
    }
  }
  # type and member are one to one (unique type_name and member per pool,
  # V-F1): a member in two pools is allowed, a member twice in one is not

  # ---- V-F5, V-F6: levels, transforms and grade caps ------------------------------------------
  for (k in seq_len(nrow(pm))) {
    j <- match(pm$pooled[k], pl$pooled)
    t <- match(pm$member[k], tg$target)
    if (is.na(j) || is.na(t)) next
    tr <- .qes_pool_transform(pm$transform[k])
    if (is.null(tr)) {
      add("V-F5", "pooled_members", k, mkey[k], "transform must be identity, affine:<a*x+b>, recode:<level>=<level>,... or score:<level>=<number>,...")
      next
    }
    ptype <- pl$type[j]
    mtype <- tg$type[t]
    mset <- target_set(pm$member[k])
    if (ptype %in% c("categorical", "ordinal")) {
      pset <- if (has(pl$levels_id[j])) sets[[pl$levels_id[j]]] else NULL
      if (is.null(pset)) {
        add("V-F5", "pooled", j, pl$pooled[j], "a categorical or ordinal pooled variable needs a levels_id of levels.csv")
        next
      }
      if (!mtype %in% c("categorical", "ordinal") || is.null(mset)) {
        add("V-F5", "pooled_members", k, mkey[k], sprintf("a %s member cannot join a %s pooled variable", mtype, ptype))
        next
      }
      if (identical(tr$kind, "identity")) {
        extra <- setdiff(mset$name, pset$name)
        if (length(extra) > 0L) {
          add("V-F5", "pooled_members", k, mkey[k], sprintf("identity: member level(s) %s are not levels of the pooled variable", paste(extra, collapse = ", ")))
        }
      } else if (identical(tr$kind, "recode")) {
        miss <- setdiff(mset$name, names(tr$map))
        extra <- setdiff(names(tr$map), mset$name)
        out_bad <- setdiff(unname(tr$map), pset$name)
        if (length(miss) > 0L) add("V-F5", "pooled_members", k, mkey[k], sprintf("recode is not total: member level(s) %s have no image", paste(miss, collapse = ", ")))
        if (length(extra) > 0L) add("V-F5", "pooled_members", k, mkey[k], sprintf("recode names level(s) %s the member does not have", paste(extra, collapse = ", ")))
        if (length(out_bad) > 0L) add("V-F5", "pooled_members", k, mkey[k], sprintf("recode gives level(s) %s that the pooled variable does not have", paste(out_bad, collapse = ", ")))
      } else {
        add("V-F5", "pooled_members", k, mkey[k], sprintf("transform %s does not fit a %s pooled variable", tr$kind, ptype))
      }
    } else if (identical(ptype, "numeric")) {
      lo <- pl$valid_min[j]
      hi <- pl$valid_max[j]
      if (is.na(lo) || is.na(hi)) {
        add("V-F5", "pooled", j, pl$pooled[j], "a numeric pooled variable needs valid_min and valid_max")
        next
      }
      if (identical(mtype, "numeric")) {
        if (!tr$kind %in% c("identity", "affine")) {
          add("V-F5", "pooled_members", k, mkey[k], "a numeric member takes transform identity or affine")
          next
        }
        a <- if (identical(tr$kind, "affine")) tr$affine else c(a = 1, b = 0)
        mlo <- tg$valid_min[t]
        mhi <- tg$valid_max[t]
        if (is.na(mlo) || is.na(mhi)) {
          add("V-F5", "pooled_members", k, mkey[k], "a numeric member needs valid_min and valid_max in targets.csv")
          next
        }
        ends <- a[["a"]] * c(mlo, mhi) + a[["b"]]
        if (min(ends) < lo - 1e-9 || max(ends) > hi + 1e-9) {
          add("V-F5", "pooled_members", k, mkey[k], sprintf("the member's range maps to %s-%s, outside the pooled range %s-%s",
                                                            .qes_code_chr(min(ends)), .qes_code_chr(max(ends)), .qes_code_chr(lo), .qes_code_chr(hi)))
        }
      } else if (mtype %in% c("categorical", "ordinal") && !is.null(mset)) {
        if (!identical(tr$kind, "score")) {
          add("V-F5", "pooled_members", k, mkey[k], "an ordinal member of a numeric pooled variable takes transform score")
          next
        }
        miss <- setdiff(mset$name[mset$substantive %in% TRUE], names(tr$map))
        extra <- setdiff(names(tr$map), mset$name)
        num <- as.numeric(tr$map)
        if (length(miss) > 0L) add("V-F5", "pooled_members", k, mkey[k], sprintf("score is not total: member level(s) %s have no score", paste(miss, collapse = ", ")))
        if (length(extra) > 0L) add("V-F5", "pooled_members", k, mkey[k], sprintf("score names level(s) %s the member does not have", paste(extra, collapse = ", ")))
        if (any(num < lo | num > hi)) add("V-F5", "pooled_members", k, mkey[k], "a score lies outside the pooled range")
      } else {
        add("V-F5", "pooled_members", k, mkey[k], sprintf("a %s member cannot join a numeric pooled variable", mtype))
      }
    }
    cap <- pm$grade_cap[k]
    if (has(cap) && !cap %in% .qes_hz_grades) {
      add("V-F6", "pooled_members", k, mkey[k], "grade_cap must be identical, comparable or approximate (or empty)")
    }
    if (.qes_pool_lossy(tr) && !identical(cap, "approximate")) {
      add("V-F6", "pooled_members", k, mkey[k], "a transform that merges levels or scores an ordinal scale needs grade_cap = approximate")
    }
  }

  # ---- V-F7: English and French --------------------------------------------------------------
  pair <- function(table, x, en, fr, keys, need = TRUE) {
    a <- has(x[[en]])
    b <- has(x[[fr]])
    bad <- if (need) which(!a | !b) else which(a != b)
    add("V-F7", table, bad, keys[bad], sprintf("%s and %s must both be %s", en, fr, if (need) "present" else "present or both empty"))
  }
  pair("pooled", pl, "label_en", "label_fr", pl$pooled)
  pair("pooled", pl, "description_en", "description_fr", pl$pooled)
  pair("pooled_members", pm, "type_label_en", "type_label_fr", mkey)
  pair("pooled_members", pm, "note_en", "note_fr", mkey, need = FALSE)

  res <- if (length(out) > 0L) do.call(rbind, out) else .qes_spec_problems()
  rownames(res) <- NULL
  res
}
