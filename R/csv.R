# The one CSV loader (design.md sections 5.1 and P10).
#
# Every shipped table (catalog, and later the dictionary and the harmonization
# spec) is read here, as UTF-8 text:
#   * every column is read as character (no type guessing, no stripped
#     zeros), with no NA strings: the literal "NA" stays "NA" so that code
#     columns can use it as a token for system missing;
#   * `encoding = "UTF-8"` marks the strings; there is deliberately no
#     `fileEncoding`, which re-encodes to the native locale and truncated
#     strings under a C locale set inside the session;
#   * every value must be valid UTF-8 and is passed through enc2utf8();
#   * a schema then checks the column names, converts types, and turns ""
#     into NA except in columns listed as `keep_empty`.
# Lists use ";" and arguments "key=value;key=value". There is no JSON and no
# R expression in any table.

# Column types of every shipped table. "chr" character, "int" integer,
# "num" double, "lgl" TRUE/FALSE, "date" ISO date.
.qes_schemas <- list(
  studies = c(
    study = "chr", aliases = "chr", family = "chr", title_deposit = "chr",
    title_en = "chr", title_fr = "chr", authors = "chr", year = "int",
    year_end = "int", election_id = "chr", study_design = "chr",
    default_member = "lgl", target_population_en = "chr",
    target_population_fr = "chr", server = "chr", doi = "chr",
    dataset_version = "chr", data_file_id = "chr", label_file_id = "chr",
    source_lang = "chr", licence = "chr", licence_url = "chr",
    metadata_shipped = "lgl", publisher = "chr", citation_year = "int",
    dataset_unf = "chr", notes_en = "chr", notes_fr = "chr"
  ),
  files = c(
    study = "chr", file_id = "chr", role = "chr", lang = "chr",
    file_name = "chr", original_file_name = "chr", format = "chr",
    ingested = "lgl", bytes = "num", md5 = "chr", checksum_type = "chr",
    unf = "chr", n_rows = "int", n_cols = "int", encoding = "chr",
    id_vars = "chr", is_default = "lgl", dataset_version = "chr"
  ),
  elections = c(
    election_id = "chr", jurisdiction = "chr", election_type = "chr",
    election_date = "date", label_en = "chr", label_fr = "chr",
    source_url = "chr"
  ),
  name_map = c(
    file_id = "chr", source_name = "chr", name = "chr", evidence = "chr"
  ),
  text_fixes = c(
    file_id = "chr", from = "chr", to = "chr", evidence = "chr"
  ),
  type_fixes = c(
    file_id = "chr", variables = "chr", to = "chr", evidence = "chr"
  ),
  enums = c(
    enum = "chr", value = "chr", order = "int", code = "chr", scope = "chr",
    label_en = "chr", label_fr = "chr"
  ),
  # the dictionary (design.md section 6.1): inst/extdata/dict/*.csv.gz for the
  # shipped studies, and the same tables for a metadata shard in the cache
  dict_variables = c(
    study = "chr", variable = "chr", position = "int", source_name = "chr",
    type = "chr", measure = "chr", var_timing = "chr", label = "chr",
    question_en = "chr", question_fr = "chr", question_truncated = "lgl",
    universe_en = "chr", universe_fr = "chr", na_values = "chr",
    derived_from = "chr", label_source = "chr", question_source = "chr",
    doc_ref = "chr", reviewed = "lgl"
  ),
  dict_values = c(
    study = "chr", variable = "chr", value = "chr", label = "chr",
    label_source = "chr", label_lang = "chr", label_en = "chr",
    label_fr = "chr", missing_type = "chr", n = "int", label_flag = "chr"
  ),
  # missing-code rules for studies whose metadata is built at runtime (OD3):
  # no label text, only (study, variable, value) keys; variable "*" is any
  dict_shard_rules = c(
    study = "chr", variable = "chr", value = "chr", missing_type = "chr",
    evidence = "chr"
  ),
  # the interim legacy builders (inst/extdata/legacy/, R/legacy.R)
  legacy_sources = c(
    profile = "chr", column = "chr", study = "chr", source_variable = "chr"
  ),
  legacy_blanks = c(
    profile = "chr", column = "chr", study = "chr", codes = "chr",
    cause = "chr", basis = "chr"
  ),
  legacy_studies = c(
    study = "chr", vote_choice_timing = "chr", sovereignty_item = "chr",
    decon_vote_timing = "chr"
  ),
  legacy_columns = c(
    column = "chr", target = "chr", definition = "chr", flag = "chr",
    note = "chr"
  ),
  legacy_removed = c(
    column = "chr", studies = "chr", source_variables = "chr"
  ),
  # the harmonization spec (inst/extdata/harmonize/, design.md section 5,
  # R/hz-spec.R). Code columns (source_code, na_codes, gate_codes) are text:
  # the literal "NA" in them is the token for system missing.
  spec_targets = c(
    target = "chr", family = "chr", block = "chr", type = "chr",
    target_timing = "chr", jurisdiction = "chr", election_ref_rule = "chr",
    levels_id = "chr", valid_min = "num", valid_max = "num", anchor_row = "chr",
    derive_rule = "chr", derive_from = "chr", allow_constant = "lgl",
    label_en = "chr", label_fr = "chr", description_en = "chr",
    description_fr = "chr", sets = "chr", status = "chr", replaced_by = "chr",
    added_in = "chr"
  ),
  spec_levels = c(
    levels_id = "chr", code = "int", name = "chr", label_en = "chr",
    label_fr = "chr", order = "int", substantive = "lgl", aliases = "chr"
  ),
  spec_crosswalk = c(
    study = "chr", wave = "chr", target = "chr", rule = "chr",
    source_var = "chr", map_id = "chr", args = "chr", na_codes = "chr",
    gate_var = "chr", gate_codes = "chr", gate_to = "chr", primary = "lgl",
    grade = "chr", grade_reason_en = "chr", grade_reason_fr = "chr",
    instrument = "chr", election_ref = "chr", mode = "chr", dk_offered = "chr",
    levels_offered = "chr", wording_en = "chr", wording_fr = "chr",
    wording_ref = "chr", evidence = "chr", notes_en = "chr", notes_fr = "chr",
    reviewed_by = "chr", reviewed_on = "date", status = "chr"
  ),
  spec_valuemaps = c(
    map_id = "chr", source_code = "chr", source_label = "chr",
    source_label_hash = "chr", source_label_origin = "chr", target_code = "int",
    na_reason = "chr", alias_exception = "chr", note = "chr"
  ),
  spec_waves = c(
    study = "chr", wave = "chr", wave_order = "int", wave_timing = "chr",
    wave_design = "chr", election_ref = "chr", member_var = "chr",
    member_codes = "chr", n_cases = "int", target_population_en = "chr",
    target_population_fr = "chr", subsample_var = "chr", strata_var = "chr",
    fieldwork_start = "date", fieldwork_end = "date", date_var = "chr",
    date_format = "chr", mode = "chr", notes = "chr"
  ),
  spec_weights = c(
    study = "chr", wave = "chr", weight_var = "chr", role = "chr",
    scale = "chr", trim = "chr", calibrated_on = "chr", population = "chr",
    recommended = "lgl", status = "chr", source_ref = "chr"
  ),
  spec_changes = c(
    spec_version = "chr", date = "date", kind = "chr", targets = "chr",
    studies = "chr", change_en = "chr", change_fr = "chr"
  ),
  # aggregates of the pinned files that the offline checks read with the
  # dictionary (R/hz-data.R): joint counts of gate code and source code among
  # a wave's members, and the projected marginals (V-P1). gate_code and
  # source_code are code text ("NA" is system missing).
  spec_gates = c(
    study = "chr", wave = "chr", source_var = "chr", member_rule = "chr",
    gate_var = "chr", gate_code = "chr", source_code = "chr", n = "int"
  ),
  spec_expected = c(
    study = "chr", wave = "chr", target = "chr", source_var = "chr",
    value = "chr", na_reason = "chr", n = "int"
  ),
  # the md5 of each harmonized column on the pinned file (V-L1): the value
  # or NA reason of every row of the study, in file order (R/hz-engine.R,
  # .qes_hz_column_md5())
  spec_hashes = c(
    study = "chr", wave = "chr", target = "chr", source_var = "chr",
    n = "int", md5 = "chr"
  ),
  # the same aggregates for a study whose metadata cannot ship (OD3), kept in
  # the build-ignored data-raw/nc/ for CI: variable types and missing-code
  # declarations, and per-code counts with label hashes instead of labels
  hz_variables = c(
    study = "chr", variable = "chr", type = "chr", na_values = "chr"
  ),
  hz_values = c(
    study = "chr", variable = "chr", value = "chr", label_hash = "chr",
    label_number = "num", missing_type = "chr", n = "int"
  )
)

# Columns whose "" is a real value, not a missing one (per table). A value
# label may be "" in its file (qes2012 q64 = 96); an unlabelled code has
# label_source "none" and its label is set to NA after reading.
.qes_keep_empty <- list(dict_values = "label")

.qes_csv_error <- function(path, reason) {
  .qes_abort(
    "catalog_invalid",
    class = "qesR_error_source",
    args = list(basename(path), reason),
    data = list(study = NA_character_, file_id = NA_character_, path = path, reason = reason)
  )
}

# Read one shipped CSV. `table` names its schema in .qes_schemas; NULL reads
# every column as character with no conversion.
.qes_read_csv <- function(path, table = NULL) {
  x <- utils::read.csv(
    path,
    colClasses = "character",
    na.strings = character(),
    encoding = "UTF-8",
    check.names = FALSE,
    strip.white = FALSE
  )
  for (nm in names(x)) {
    v <- x[[nm]]
    if (!all(validUTF8(v))) {
      .qes_csv_error(path, sprintf("column '%s' is not valid UTF-8", nm))
    }
    x[[nm]] <- enc2utf8(v)
  }
  names(x) <- enc2utf8(names(x))
  if (is.null(table)) {
    return(x)
  }
  .qes_apply_schema(x, table, path)
}

.qes_apply_schema <- function(x, table, path = table) {
  schema <- .qes_schemas[[table]]
  if (is.null(schema)) {
    stop(sprintf("qesR internal error: no schema for table '%s'.", table), call. = FALSE)
  }
  if (!identical(names(x), names(schema))) {
    .qes_csv_error(
      path,
      sprintf("columns are not the %s schema (%s)", table, paste(names(schema), collapse = ", "))
    )
  }
  keep <- .qes_keep_empty[[table]] %||% character(0)
  for (nm in names(schema)) {
    v <- x[[nm]]
    if (!(nm %in% keep)) {
      v[v == ""] <- NA_character_
    }
    x[[nm]] <- switch(
      schema[[nm]],
      chr = v,
      int = .qes_as_type(v, nm, path, function(z) {
        out <- suppressWarnings(as.integer(z))
        out[!grepl("^-?[0-9]+$", z)] <- NA_integer_
        out
      }),
      num = .qes_as_type(v, nm, path, function(z) suppressWarnings(as.numeric(z))),
      lgl = .qes_as_type(v, nm, path, function(z) {
        out <- rep(NA, length(z))
        out[z == "TRUE"] <- TRUE
        out[z == "FALSE"] <- FALSE
        out
      }),
      date = .qes_as_type(v, nm, path, function(z) {
        out <- as.Date(rep(NA_character_, length(z)))
        ok <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", z)
        out[ok] <- as.Date(z[ok], format = "%Y-%m-%d")
        out
      })
    )
  }
  x
}

# Convert a column, failing on any non-missing value that does not convert.
.qes_as_type <- function(v, nm, path, convert) {
  out <- convert(v)
  bad <- !is.na(v) & is.na(out)
  if (any(bad)) {
    .qes_csv_error(path, sprintf("column '%s' has an invalid value '%s'", nm, v[which(bad)[1]]))
  }
  out
}

# Split a ";"-list cell into a character vector (NA gives character(0)).
.qes_split_list <- function(x) {
  if (length(x) != 1L || is.na(x) || !nzchar(x)) {
    return(character(0))
  }
  strsplit(x, ";", fixed = TRUE)[[1]]
}

# Write a table as UTF-8 CSV with LF line endings, whatever the locale: every
# field is written as text, quoted when it is character, NA as "" (the reader
# turns "" back into NA except in keep_empty columns). `path` ending in ".gz"
# is gzip-compressed. The file is written under a temporary name and renamed,
# so a reader never sees a partial file.
.qes_write_csv <- function(x, path) {
  field <- function(v) {
    if (is.logical(v)) {
      out <- ifelse(v, "TRUE", "FALSE")
    } else if (is.numeric(v)) {
      out <- .qes_code_chr(v)
    } else {
      out <- enc2utf8(as.character(v))
      out <- paste0("\"", gsub("\"", "\"\"", out, fixed = TRUE), "\"")
      out[is.na(v)] <- ""
      return(out)
    }
    out[is.na(v)] <- ""
    out
  }
  cols <- lapply(x, field)
  header <- paste0("\"", enc2utf8(names(x)), "\"", collapse = ",")
  body <- if (nrow(x) > 0L) do.call(paste, c(cols, sep = ",")) else character(0)
  text <- paste0(paste(c(header, body), collapse = "\n"), "\n")
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".part")
  con <- if (grepl("\\.gz$", path)) gzfile(tmp, open = "wb", compression = 9) else file(tmp, open = "wb")
  ok <- FALSE
  on.exit(if (!ok) unlink(tmp), add = TRUE)
  writeBin(charToRaw(text), con)
  close(con)
  if (!file.rename(tmp, path)) {
    ok <- file.copy(tmp, path, overwrite = TRUE)
    unlink(tmp)
  } else {
    ok <- TRUE
  }
  invisible(ok)
}
