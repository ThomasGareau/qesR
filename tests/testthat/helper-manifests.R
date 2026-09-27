# Contract data for the 14 v0.4.4 exports (slice S0b, design.md sections 2 and 8.1).
#
# Everything here was taken from qesR 0.4.4 at commit d1faad6: the formals from
# the installed d1faad6 namespace, the legacy column names from its outputs.
# These are frozen: change them only together with a documented, allowed
# difference in design.md section 1.2 (constraint 1).

# formals() of every v0.4.4 export at d1faad6.
v044_formals <- list(
  download_codebook = alist(srvy = , dest_dir = tempdir(), file = NULL, quiet = FALSE, refresh = FALSE, overwrite = FALSE),
  format_codebook = alist(codebook = , layout = c("compact", "wide", "long")),
  get_codebook = alist(srvy = , file = NULL, assign_global = FALSE, quiet = FALSE, refresh = FALSE, layout = c("compact", "wide", "long")),
  get_codebook_files = alist(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE),
  get_decon = alist(srvy = "qes2022", assign_global = TRUE, quiet = FALSE),
  get_preview = alist(srvy = , obs = 6L, file = NULL),
  get_qes = alist(srvy = , file = NULL, assign_global = TRUE, with_codebook = TRUE, quiet = FALSE),
  get_qes_codebook = alist(srvy = , file = NULL, assign_global = FALSE, quiet = FALSE, refresh = FALSE, layout = c("compact", "wide", "long")),
  get_qes_codebook_files = alist(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE),
  get_qes_master = alist(surveys = NULL, assign_global = TRUE, object_name = "qes_master", quiet = FALSE, strict = FALSE, save_path = NULL),
  get_qescodes = alist(detailed = FALSE),
  get_question = alist(do = , q = , full = TRUE),
  get_value_labels = alist(codebook = , variable = NULL, long = FALSE),
  qes_codebook = alist(srvy = , file = NULL, assign_global = FALSE, quiet = FALSE, refresh = FALSE, layout = c("compact", "wide", "long"))
)

# The only allowed differences (design.md section 1.2, constraint 1):
#   * assign_global defaults TRUE -> FALSE in get_qes, get_qes_master, get_decon;
#   * qes_codebook may append `variables = NULL, lang = NULL` (slice S3).
allowed_formals <- function() {
  out <- v044_formals
  for (f in c("get_qes", "get_qes_master", "get_decon")) {
    out[[f]]$assign_global <- FALSE
  }
  out
}
allowed_appended_formals <- list(qes_codebook = alist(variables = NULL, lang = NULL))

# Export sets (design.md section 2.2, OD17).
v044_exports <- names(v044_formals)
legacy_exports <- c(
  "get_codebook", "get_qes_codebook", "format_codebook", "get_value_labels",
  "get_question", "get_codebook_files", "get_qes_codebook_files",
  "download_codebook", "get_preview", "get_decon", "get_qescodes"
)
canonical_existing_exports <- c("get_qes", "get_qes_master", "qes_codebook")
new_exports <- c(
  "qes_studies", "qes_search", "qes_missing", "qes_download", "qes_question",
  "qes_docs", "qes_harmonize", "qes_spec", "qes_design", "qes_provenance",
  "qes_cite", "qes_cache_info", "qes_cache_clear"
)
final_exports <- c(canonical_existing_exports, new_exports, legacy_exports)
# Implemented internally but never exported until the owner decides (OD17).
internal_only <- c("qes_splice", "qes_join_raw", "qes_targets", "qes_crosswalk")

# Exports that can assign on opt-in, with the name they assign.
assigning_exports <- c(
  "get_qes", "get_qes_master", "get_decon",
  "get_codebook", "get_qes_codebook", "qes_codebook"
)

# ---- Legacy name manifests (column names only, v0.4.4 outputs) ----------------

v044_qescodes <- c(
  "qes2022", "qes2018", "qes2018_panel", "qes2014", "qes2012", "qes2012_panel",
  "qes_crop_2007_2010", "qes2008", "qes2007", "qes2007_panel", "qes1998"
)
v044_qescodes_cols <- c("index", "qes_survey_code", "get_qes_call_char")
v044_qescodes_detailed_cols <- c(
  v044_qescodes_cols,
  "year", "name_en", "name_fr", "doi", "doi_url", "documentation"
)

# get_qes_master(): the 30 documented legacy columns, in order, with their type.
v044_master_cols <- c(
  qes_code = "character", qes_year = "character", qes_name_en = "character",
  respondent_id = "character", interview_start = "character",
  interview_end = "character", interview_recorded = "character",
  language = "character", citizenship = "character", year_of_birth = "numeric",
  age = "numeric", age_group = "character", gender = "character",
  province_territory = "character", education = "character",
  income = "character", religion = "character", born_canada = "character",
  political_interest = "numeric", ideology = "numeric", turnout = "numeric",
  vote_choice = "character", vote_choice_text = "character",
  party_best = "character", party_lean = "character",
  sovereignty_support = "numeric", sovereignty = "numeric",
  federal_pid = "character", provincial_pid = "character",
  survey_weight = "numeric"
)
# Attribute names of the v0.4.4 master (without save_path).
v044_master_attrs <- c(
  "names", "row.names", "class", "source_map", "loaded_surveys",
  "failed_surveys", "duplicates_removed", "empty_rows_removed",
  "harmonized_variables", "crossstudy_variables_added", "variable_name_map"
)

v044_decon_cols <- c(
  "qes_code", "citizenship", "yob", "age", "gender", "province_territory",
  "education", "political_interest", "turnout", "votechoice",
  "votechoice_text", "party_best", "partylean", "fed_pid", "prov_pid",
  "ideology", "income", "religion", "born_canada"
)

v044_codebook_cols <- list(
  compact = c("variable", "label", "question", "n_value_labels"),
  wide = c("variable", "label", "question", "n_value_labels", "value_labels"),
  long = c("variable", "value", "value_label", "label", "question")
)
v044_value_labels_long_cols <- c("variable", "value", "value_label")
v044_codebook_files_cols <- c("file_id", "filename", "extension", "size", "download_url")
v044_download_codebook_cols <- c(v044_codebook_files_cols, "local_path", "downloaded")

# get_qes() column names per study: tests/testthat/fixtures/v044-get-qes-names.csv
# (study, position, name, type, n_na). Built by running d1faad6 against the
# Dataverse originals: each column's name, its storage ("numeric", "character"
# or "logical") and its is.na() count (added in slice S2b from the same R9
# baseline); no labels or values. tests/testthat/fixtures/v044-get-decon-classes.csv
# holds the class of every get_decon() column of d1faad6 per study, from the
# same baseline. The file is UTF-8. It is read
# with `encoding = "UTF-8"` only (no `fileEncoding`), so the bytes are kept and
# marked UTF-8 instead of being re-encoded to the native locale; re-encoding
# stops at the first accented name in a C / non-UTF-8 locale.
v044_get_qes_names <- function() {
  path <- testthat::test_path("fixtures", "v044-get-qes-names.csv")
  utils::read.csv(path, stringsAsFactors = FALSE, encoding = "UTF-8")
}

# Mark literal strings as UTF-8. A "\u00c9" escape yields UTF-8 bytes, but in a
# non-UTF-8 locale R may leave them marked "unknown", and then they do not
# match the UTF-8-marked strings read from the fixtures.
as_utf8 <- function(x) {
  Encoding(x) <- "UTF-8"
  x
}
