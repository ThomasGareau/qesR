# Offline study catalog (design.md section 3).
#
# The catalog is a set of UTF-8 CSV files in inst/extdata/catalog/, built by
# data-raw/build_catalog.R from the Dataverse JSON of each deposit plus
# hand-curated overlays, and read here once per session:
#   studies.csv    one row per study code;
#   files.csv      every Dataverse file a study uses: data, label donor and
#                  documents, with md5, size, n_rows and n_cols;
#   elections.csv  public election dates;
#   name_map.csv   renames that restore v0.4.4 column names;
#   text_fixes.csv characters of a file's labels that its own encoding
#                  decodes wrongly (CP850 letters in Windows-1252 labels);
#   type_fixes.csv text columns that hold whole-number codes, read as numbers
#                  (as qesR 0.4.4 had them: its text reader converted the
#                  quoted codes of Dataverse's .tab files);
#   enums.csv      every closed vocabulary of the package (P10).
# inst/extdata/VERSIONS records the catalog version and the md5 of each CSV.
#
# .qes_catalog() is the only function that reads the catalog. It is a test
# seam: tests replace it (testthat::local_mocked_bindings) to serve a fixture
# catalog. The synthetic study `qes_demo` lives in its own tree
# (inst/extdata/demo/) and is merged only when asked for.

# ---- loading -----------------------------------------------------------------

.qes_extdata <- function(...) {
  system.file("extdata", ..., package = "qesR", mustWork = TRUE)
}

.qes_catalog_tables <- c(
  "studies", "files", "elections", "name_map", "text_fixes", "type_fixes", "enums"
)

# Read a catalog directory (and optionally a demo catalog directory, whose
# studies and files are appended). Used by .qes_catalog() and by tests.
.qes_load_catalog <- function(dir, demo_dir = NULL) {
  out <- lapply(.qes_catalog_tables, function(tab) {
    .qes_read_csv(file.path(dir, paste0(tab, ".csv")), tab)
  })
  names(out) <- .qes_catalog_tables
  if (!is.null(demo_dir)) {
    for (tab in c("studies", "files")) {
      extra <- .qes_read_csv(file.path(demo_dir, paste0(tab, ".csv")), tab)
      out[[tab]] <- rbind(out[[tab]], extra)
    }
  }
  out$studies$demo <- out$studies$family %in% "demo"
  out
}

.qes_catalog_cache <- new.env(parent = emptyenv())

# The catalog as a list of data frames. `demo = TRUE` adds the synthetic
# study qes_demo. Read once per session.
.qes_catalog <- function(demo = FALSE) {
  key <- if (isTRUE(demo)) "demo" else "main"
  if (is.null(.qes_catalog_cache[[key]])) {
    .qes_catalog_cache[[key]] <- .qes_load_catalog(
      .qes_extdata("catalog"),
      demo_dir = if (isTRUE(demo)) .qes_extdata("demo", "catalog") else NULL
    )
  }
  .qes_catalog_cache[[key]]
}

# The rows of one closed vocabulary from enums.csv, in their order.
.qes_enum <- function(name) {
  enums <- .qes_catalog()$enums
  rows <- enums[enums$enum == name, , drop = FALSE]
  if (nrow(rows) == 0L) {
    stop(sprintf("qesR internal error: unknown enum '%s'.", name), call. = FALSE)
  }
  rows <- rows[order(rows$order), , drop = FALSE]
  rownames(rows) <- NULL
  rows
}

# The NA-reason vocabulary (design.md section 5.2): every reason a missing
# value can have, its one-letter tag for qes_missing(action = "tagged") (empty
# for reasons only the engine sets), and where it is set.
.qes_na_reasons <- function() {
  rows <- .qes_enum("missing_type")
  rows[, c("value", "code", "scope", "label_en", "label_fr")]
}

# ---- study codes ---------------------------------------------------------------

# The 11 study codes of qesR 0.4.4, in their get_qescodes() order. Frozen: the
# legacy master builds these by default, and get_qescodes() lists them first.
.qes_legacy_codes <- c(
  "qes2022", "qes2018", "qes2018_panel", "qes2014", "qes2012", "qes2012_panel",
  "qes_crop_2007_2010", "qes2008", "qes2007", "qes2007_panel", "qes1998"
)

# Catalog study codes, in catalog order (qes_demo only when `demo = TRUE`).
.qes_study_codes <- function(demo = FALSE) {
  studies <- .qes_catalog(demo = demo)$studies
  studies$study
}

# Study codes are trimmed and case-folded to the canonical catalog code, never
# matched fuzzily: " QES2018 " is qes2018, "2018" is an error that suggests
# qes2018 (design.md section 2.1). A code may also match an exact alias.
.qes_canon_code <- function(x) {
  tolower(trimws(x))
}

.qes_match_codes <- function(x, studies) {
  key <- .qes_canon_code(x)
  idx <- match(key, studies$study)
  miss <- which(is.na(idx))
  for (i in miss) {
    hit <- which(vapply(studies$aliases, function(a) key[i] %in% tolower(.qes_split_list(a)), logical(1)))
    if (length(hit) == 1L) {
      idx[i] <- hit
    }
  }
  studies$study[idx]
}

# Near matches for an unknown code: catalog codes that contain it, then the
# codes at the smallest edit distance (utils::adist), if it is small.
.qes_suggest_codes <- function(x, codes = .qes_study_codes(), n = 3L) {
  key <- .qes_canon_code(x)
  if (is.na(key) || !nzchar(key)) {
    return(character(0))
  }
  contains <- codes[grepl(key, codes, fixed = TRUE)]
  d <- as.vector(utils::adist(key, codes))
  limit <- max(2L, ceiling(nchar(key) / 3))
  near <- if (min(d) <= limit) codes[d == min(d)] else character(0)
  utils::head(unique(c(contains, near)), n)
}

.qes_unknown_study <- function(study) {
  suggestions <- unique(unlist(lapply(study, .qes_suggest_codes), use.names = FALSE))
  suggestions <- suggestions %||% character(0)
  if (length(suggestions) > 0L) {
    .qes_abort(
      "unknown_study_suggest",
      class = "qesR_error_unknown_study",
      args = list(.qes_q(study), .qes_q(suggestions)),
      data = list(study = study, suggestions = suggestions)
    )
  }
  .qes_abort(
    "unknown_study",
    class = "qesR_error_unknown_study",
    args = list(.qes_q(study)),
    data = list(study = study, suggestions = character(0))
  )
}

# Canonical codes for a vector of study codes. "all" is valid only on its own
# and means every catalog study (never qes_demo). `demo = TRUE` also accepts
# the code qes_demo.
.qes_resolve_codes <- function(x, arg, demo = FALSE) {
  if (!is.character(x) || length(x) == 0L || anyNA(x) || !all(nzchar(trimws(x)))) {
    .qes_abort(
      "input_codes",
      class = "qesR_error_input",
      args = list(arg),
      data = list(arg = arg, value = x)
    )
  }
  canon <- .qes_canon_code(x)
  if ("all" %in% canon) {
    if (length(canon) > 1L) {
      .qes_abort(
        "input_all_mixed",
        class = "qesR_error_input",
        args = list(arg),
        data = list(arg = arg, value = x)
      )
    }
    return(.qes_study_codes())
  }
  matched <- .qes_match_codes(x, .qes_catalog(demo = demo)$studies)
  if (anyNA(matched)) {
    .qes_unknown_study(x[is.na(matched)])
  }
  unique(matched)
}

# The studies.csv row of one canonical code.
.qes_study_row <- function(code, demo = FALSE) {
  studies <- .qes_catalog(demo = demo)$studies
  studies[match(code, studies$study), , drop = FALSE]
}

# "2007-2010" when a study spans years, else "2018" (the v0.4.4 year string).
.qes_year_label <- function(year, year_end) {
  ifelse(
    is.na(year_end) | year_end == year,
    as.character(year),
    paste0(year, "-", year_end)
  )
}

.qes_doi_url <- function(doi) {
  ifelse(is.na(doi), NA_character_, paste0("https://doi.org/", doi))
}

# The study fields the v0.4.4 code paths use, under their v0.4.4 names.
.qes_legacy_view <- function(studies) {
  data.frame(
    qes_survey_code = studies$study,
    year = .qes_year_label(studies$year, studies$year_end),
    name_en = studies$title_en,
    name_fr = studies$title_fr,
    doi = studies$doi,
    doi_url = .qes_doi_url(studies$doi),
    documentation = .qes_doi_url(studies$doi),
    server = studies$server,
    data_file_id = studies$data_file_id,
    stringsAsFactors = FALSE
  )
}

# One study, as a one-row legacy view, from a single user-supplied code.
# `demo = TRUE` also accepts the synthetic study qes_demo.
.get_qes_study <- function(srvy, arg = "srvy", demo = FALSE) {
  .assert_single_string(srvy, arg)
  if (!nzchar(trimws(srvy))) {
    .qes_abort(
      "input_string",
      class = "qesR_error_input",
      args = list(arg),
      data = list(arg = arg, value = srvy)
    )
  }
  code <- .qes_resolve_codes(srvy, arg, demo = demo)
  .qes_legacy_view(.qes_study_row(code, demo = demo))
}

# ---- URLs ----------------------------------------------------------------------

# Dataverse URLs, built from catalog fields only (design.md sections 4.1 and
# 4.6): nothing about the user or the machine can enter a URL.
#   "original"  the original upload of an ingested data file;
#   "file"      a file as stored (documents);
#   "dataset"   one version of a dataset's metadata (":latest-published" for
#               update checks).
.qes_url <- function(server, kind = c("original", "file", "dataset"),
                     file_id = NULL, doi = NULL, version = NULL,
                     include_deaccessioned = FALSE) {
  kind <- match.arg(kind)
  ok_server <- is.character(server) && length(server) == 1L && !is.na(server) &&
    grepl("^https://[A-Za-z0-9.-]+$", server)
  if (!ok_server) {
    stop("qesR internal error: invalid Dataverse server in the catalog.", call. = FALSE)
  }
  if (kind %in% c("original", "file")) {
    if (!is.character(file_id) || length(file_id) != 1L || !grepl("^[0-9]+$", file_id)) {
      stop("qesR internal error: invalid Dataverse file id in the catalog.", call. = FALSE)
    }
    url <- sprintf("%s/api/access/datafile/%s", server, file_id)
    if (identical(kind, "original")) {
      url <- paste0(url, "?format=original")
    }
    return(url)
  }
  ok_doi <- is.character(doi) && length(doi) == 1L && grepl("^10\\.[0-9]+/[A-Za-z0-9./_-]+$", doi)
  ok_version <- is.character(version) && length(version) == 1L &&
    grepl("^(:latest-published|[0-9]+\\.[0-9]+)$", version)
  if (!ok_doi || !ok_version) {
    stop("qesR internal error: invalid DOI or dataset version in the catalog.", call. = FALSE)
  }
  url <- sprintf("%s/api/datasets/:persistentId/versions/%s?persistentId=doi:%s", server, version, doi)
  # Dataverse 6.1+ hides deaccessioned versions unless asked; older servers
  # ignore the parameter.
  if (isTRUE(include_deaccessioned)) {
    url <- paste0(url, "&includeDeaccessioned=true")
  }
  url
}

# ---- qes_studies() ---------------------------------------------------------------

#' List the studies qesR can load
#'
#' `qes_studies()` returns the offline study catalog: one row per study code,
#' with the deposit title and authors, design, target population, licence,
#' DOI and the pinned dataset version and data file. It reads only metadata
#' shipped with the package and makes no network request, unless
#' `check_updates = TRUE`.
#'
#' Studies that are not Quebec Election Studies (the three Durand panels,
#' the CROP polls and the 1998 polls) are listed under their own titles and
#' authors. The 1998 deposit
#' holds three surveys, each with its own code: `qes1998` (the combined
#' CROP-CREATEC panel file, as in qesR 0.4.4), `qes1998_crop` and
#' `qes1998_createc`. All three cover francophones only, each with its own
#' definition (see `notes_en`).
#'
#' @param family Optional character vector of study families to keep:
#'   `"qes"` (Quebec Election Studies), `"durand_panel"`, `"crop_polls"` or
#'   `"polls_1998"`.
#' @param check_updates If `TRUE`, ask Dataverse whether each deposit has a
#'   newer published version and whether the pinned data file changed. This
#'   makes one metadata request per deposit (the three 1998 studies share one),
#'   one at a time. A study that cannot be checked gets `status =
#'   "unreachable"`; the call never fails because of the network.
#' @param quiet If `TRUE`, do not print progress messages.
#'
#' @return A data frame with one row per study and the columns of the
#'   catalog's `studies.csv`: `study`, `aliases`, `family`, `title_deposit`
#'   (verbatim), `title_en`, `title_fr`, `authors` (`;`-separated), `year`,
#'   `year_end`, `election_id`, `study_design`, `default_member`,
#'   `target_population_en`, `target_population_fr`, `server`, `doi`,
#'   `dataset_version` (pinned), `data_file_id` (pinned), `label_file_id`,
#'   `source_lang`, `licence`, `licence_url`, `metadata_shipped`, `publisher`,
#'   `citation_year`, `dataset_unf`, `notes_en`, `notes_fr`; then `doi_url`
#'   and `waves`, the waves of the study in the harmonization spec
#'   (`;`-separated, in field order, such as `"cps;pes"`; `NA` for a study
#'   the spec does not cover yet). With `check_updates = TRUE`
#'   it adds `latest_version`, `latest_md5` (of the pinned data file in the
#'   latest version) and `status`: `"current"`, `"new_version_same_file"`,
#'   `"data_changed"`, `"deaccessioned"` or `"unreachable"`.
#'
#'   The text columns do not depend on the session language: `title_en` and
#'   `title_fr` (and `notes_en`, `notes_fr`) are both always there.
#'
#' @family studies and documents
#' @seealso [qes_docs()] for the documents of each study, [qes_cite()] to cite
#'   them.
#' @examples
#' studies <- qes_studies()
#' studies[, c("study", "year", "title_en", "licence")]
#'
#' # Quebec Election Studies only
#' qes_studies(family = "qes")$study
#'
#' \donttest{
#' # Ask Dataverse whether the pinned versions are still current (one
#' # metadata request per deposit). A deposit that cannot be reached is
#' # reported as "unreachable", never as an error.
#' if (curl::has_internet()) {
#'   tryCatch(
#'     qes_studies(check_updates = TRUE, quiet = TRUE)[, c("study", "status")],
#'     qesR_error_network = function(e) conditionMessage(e)
#'   )
#' }
#' }
#' @export
qes_studies <- function(family = NULL, check_updates = FALSE, quiet = FALSE) {
  catalog <- .qes_catalog()
  studies <- catalog$studies[!catalog$studies$demo, , drop = FALSE]
  if (!is.null(family)) {
    families <- .qes_enum("family")$value
    families <- setdiff(families, "demo")
    if (!is.character(family) || length(family) == 0L || anyNA(family) || !all(family %in% families)) {
      .qes_abort(
        "input_choice",
        class = "qesR_error_input",
        args = list("family", .qes_q(families)),
        data = list(arg = "family", value = family)
      )
    }
    studies <- studies[studies$family %in% family, , drop = FALSE]
  }
  if (!is.logical(check_updates) || length(check_updates) != 1L || is.na(check_updates)) {
    .qes_abort(
      "input_flag",
      class = "qesR_error_input",
      args = list("check_updates"),
      data = list(arg = "check_updates", value = check_updates)
    )
  }
  studies$demo <- NULL
  studies$doi_url <- .qes_doi_url(studies$doi)
  studies$waves <- .qes_study_waves(studies$study)
  if (isTRUE(check_updates)) {
    studies <- .qes_check_updates(studies, catalog$files, quiet = quiet)
  }
  rownames(studies) <- NULL
  studies
}

# The waves of each study in the shipped harmonization spec, ";"-separated in
# wave order; NA for a study without waves.
.qes_study_waves <- function(codes) {
  wv <- .qes_spec_get(NULL, "none")$tables$waves
  wv <- wv[order(wv$study, wv$wave_order), , drop = FALSE]
  vapply(codes, function(s) {
    w <- wv$wave[wv$study == s]
    if (length(w) == 0L) NA_character_ else paste(w, collapse = ";")
  }, character(1), USE.NAMES = FALSE)
}

# One metadata request per deposit, compared with the pinned version and the
# pinned data file's md5. Any failure is recorded as "unreachable".
.qes_check_updates <- function(studies, files, quiet = FALSE) {
  n <- nrow(studies)
  latest_version <- rep(NA_character_, n)
  latest_md5 <- rep(NA_character_, n)
  status <- rep(NA_character_, n)
  seen <- list()
  for (i in seq_len(n)) {
    key <- paste(studies$server[i], studies$doi[i])
    if (is.null(seen[[key]])) {
      .qes_inform(
        "check_updates",
        class = "qesR_message_download",
        args = list(studies$doi[i]),
        data = list(study = studies$study[i]),
        quiet = quiet
      )
      seen[[key]] <- .qes_fetch_latest(studies$server[i], studies$doi[i])
    }
    res <- seen[[key]]
    if (inherits(res, "condition")) {
      status[i] <- "unreachable"
      next
    }
    latest_version[i] <- res$version
    pinned <- files$md5[files$study == studies$study[i] & files$file_id == studies$data_file_id[i]]
    latest_md5[i] <- unname(res$md5[studies$data_file_id[i]]) %||% NA_character_
    status[i] <- if (identical(res$state, "DEACCESSIONED")) {
      "deaccessioned"
    } else if (is.na(latest_md5[i]) || !identical(latest_md5[i], pinned[1])) {
      "data_changed"
    } else if (identical(res$version, studies$dataset_version[i])) {
      "current"
    } else {
      "new_version_same_file"
    }
  }
  studies$latest_version <- latest_version
  studies$latest_md5 <- latest_md5
  studies$status <- status
  studies
}

# The latest published version of a deposit: list(version, state, md5), where
# md5 is named by file id; or the condition that prevented reading it.
# Memoized for the session (qes_cache_clear() forgets it); a failure is not
# memoized. Two attempts at most, so that the check does not stall.
.qes_fetch_latest <- function(server, doi) {
  url <- tryCatch(
    .qes_url(server, "dataset", doi = doi, version = ":latest-published", include_deaccessioned = TRUE),
    error = function(e) e
  )
  if (inherits(url, "error")) {
    return(url)
  }
  if (!is.null(.qes_latest_memo[[url]])) {
    return(.qes_latest_memo[[url]])
  }
  tryCatch(
    {
      json <- .qes_fetch_json(url, max_tries = 2L)
      d <- json$data
      if (!identical(json$status, "OK") || is.null(d$versionNumber)) {
        stop("unexpected Dataverse response", call. = FALSE)
      }
      files <- .qes_latest_files(d$files)
      out <- list(
        version = sprintf("%s.%s", d$versionNumber, d$versionMinorNumber %||% 0L),
        state = d$versionState %||% NA_character_,
        md5 = stats::setNames(files$md5, files$id),
        files = files
      )
      .qes_latest_memo[[url]] <- out
      out
    },
    error = function(e) e
  )
}

# The files of a dataset version (the `files` list of the Dataverse JSON) as a
# data frame: id, md5 (of the original upload for an ingested file; NA when
# the server records another checksum type), the id of the file it replaced
# (`previous`) and of the first file of that chain (`root`), its name (the
# original upload's name for an ingested file), whether it was ingested, its
# size (of the original upload) and whether it is restricted.
.qes_latest_files <- function(files) {
  field <- function(f, name) {
    v <- f$dataFile[[name]]
    if (is.null(v) || length(v) != 1L) NA_character_ else as.character(v)
  }
  one <- function(f) {
    md5 <- field(f, "md5")
    checksum <- f$dataFile$checksum
    if (is.na(md5) && is.list(checksum) && identical(toupper(checksum$type %||% ""), "MD5")) {
      md5 <- as.character(checksum$value)
    }
    tabular <- isTRUE(f$dataFile$tabularData) || !is.na(field(f, "originalFileName"))
    name <- if (tabular && !is.na(field(f, "originalFileName"))) field(f, "originalFileName") else field(f, "filename")
    bytes <- if (tabular && !is.na(field(f, "originalFileSize"))) field(f, "originalFileSize") else field(f, "filesize")
    data.frame(
      id = field(f, "id"),
      md5 = tolower(md5),
      previous = field(f, "previousDataFileId"),
      root = field(f, "rootDataFileId"),
      name = name %||% NA_character_,
      ingested = tabular,
      bytes = suppressWarnings(as.numeric(bytes)),
      restricted = isTRUE(f$restricted),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, lapply(files, one))
  if (is.null(out)) {
    out <- data.frame(
      id = character(0), md5 = character(0), previous = character(0), root = character(0),
      name = character(0), ingested = logical(0), bytes = numeric(0), restricted = logical(0),
      stringsAsFactors = FALSE
    )
  }
  out
}

# ---- get_qescodes() (legacy) ------------------------------------------------------

#' List Quebec Election Study Survey Codes
#'
#' Returns a data frame of qesR survey call codes, with optional detailed
#' metadata.
#'
#' Soft-deprecated: use [qes_studies()], which returns the full catalog.
#' `get_qescodes()` keeps working and will not be removed; it prints a
#' one-time notice (see [qesR-deprecated]). Its first 11 rows are the qesR
#' 0.4.4 codes, in the same order and with the same `index`; codes added
#' since (the 1998 CROP and CREATEC surveys) follow. Names now carry their
#' accents and the Durand panels, CROP and 1998 surveys are listed under their
#' own titles; `documentation` is the DOI link.
#'
#' @param detailed If TRUE, include year, names, DOI, and documentation columns.
#'
#' @return A data frame of qesR survey codes (cesR-style by default).
#' @seealso [qes_studies()], and [qesR-deprecated] for the legacy functions
#'   and their replacements.
#' @examples
#' get_qescodes()
#' get_qescodes(detailed = TRUE)
#' @export
get_qescodes <- function(detailed = FALSE) {
  .qes_deprecate("get_qescodes")
  .get_qescodes_impl(detailed = detailed)
}

.get_qescodes_impl <- function(detailed = FALSE) {
  studies <- qes_studies(quiet = TRUE)
  view <- .qes_legacy_view(studies)
  out <- data.frame(
    index = seq_len(nrow(view)),
    qes_survey_code = view$qes_survey_code,
    get_qes_call_char = sprintf('"%s"', view$qes_survey_code),
    stringsAsFactors = FALSE
  )
  if (isTRUE(detailed)) {
    out <- cbind(
      out,
      view[, c("year", "name_en", "name_fr", "doi", "doi_url", "documentation")],
      stringsAsFactors = FALSE
    )
  }
  rownames(out) <- NULL
  out
}
