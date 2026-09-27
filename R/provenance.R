# qes_provenance() (design.md sections 2.2 and 5.9, slice S2c: study level).
#
# Data read by get_qes() (and files saved by qes_download()) carry the
# attribute qes_provenance: one row per file, built by .qes_provenance_row()
# (R/read.R). qes_provenance() returns it with class "qes_provenance", whose
# print() method writes a citable paragraph per file. For study codes it
# shows what get_qes() would read, before anything is downloaded.
# Data harmonized by qes_harmonize() (slice HZ3) also carry cell- and
# spec-level records, as attributes "cell" and "spec" of the study-level
# table (design.md section 5.9).

.qes_provenance_levels <- c("study", "cell", "spec")

# What get_qes() would read for each code: the pinned data file, its
# expected md5, label donor, renames and reader. Nothing is checked or
# retrieved yet, so md5_observed, md5_verified, retrieved_via, retrieved_at
# and label_source are NA.
.qes_provenance_codes <- function(codes) {
  catalog <- .qes_catalog(demo = TRUE)
  rows <- lapply(codes, function(code) {
    study_row <- catalog$studies[match(code, catalog$studies$study), , drop = FALSE]
    file_row <- .qes_default_data_file(code, demo = TRUE)
    donor <- .qes_label_donor(study_row, file_row, demo = TRUE)
    .qes_provenance_row(
      study_row, file_row,
      pinned = TRUE,
      label_file_id = if (is.null(donor)) NA_character_ else donor$file_id,
      name_map_applied = sum(catalog$name_map$file_id == file_row$file_id),
      reader = .qes_reader_call(file_row$format),
      haven_version = as.character(utils::packageVersion("haven"))
    )
  })
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Where a study's data came from
#'
#' `qes_provenance()` returns the record of which file a result was read
#' from: the study, its DOI and dataset version, the Dataverse file id and
#' name, the md5 checksum expected and observed, the file's UNF and size,
#' whether the file is the one qesR pins, where and when it was retrieved,
#' its licence, where its labels came from, the reader and the versions of
#' haven and of the qesR catalog. Printing it gives one paragraph per file,
#' ready for a replication log or a methods section.
#'
#' Data returned by [get_qes()] (and files saved by [qes_download()])
#' carry this record in their attribute `qes_provenance`. R drops such
#' attributes in some operations, notably [merge()]; `qes_provenance()` then
#' raises an error of class `qesR_error_no_provenance`. Record the provenance
#' before merging, or pass the study codes.
#'
#' @section En français:
#' `qes_provenance()` indique de quel fichier provient un résultat : l'étude,
#' son DOI et la version du jeu de données, l'identifiant et le nom du
#' fichier Dataverse, la somme md5 attendue et observée, l'UNF et la taille
#' du fichier, s'il s'agit du fichier retenu par qesR, où et quand il a été
#' obtenu, sa licence, la source des étiquettes, le lecteur et les versions de
#' haven et du catalogue de qesR. L'affichage donne un paragraphe par
#' fichier, dans la langue des messages. Avec des codes d'étude, la fonction
#' montre ce que [get_qes()] lirait, sans rien télécharger. Un objet qui a
#' perdu ses attributs (par exemple après [merge()]) produit une erreur de
#' classe `qesR_error_no_provenance`.
#'
#' @param x A data frame returned by [get_qes()] (or [get_preview()]), the
#'   result of [qes_download()], or a character vector of study codes (see
#'   [qes_studies()]). For study codes, the record shows what [get_qes()]
#'   would read: nothing has been retrieved or checked yet, so
#'   `md5_observed`, `md5_verified`, `retrieved_via`, `retrieved_at` and
#'   `label_source` are `NA`.
#' @param level `"study"` (default): one row per file. `"cell"` and
#'   `"spec"` describe data harmonized by [qes_harmonize()]; for other
#'   objects they are an error of class `qesR_error_no_provenance`.
#'
#' @return A data frame of class `qes_provenance`. For `level = "study"`,
#'   one row per file, with
#'   the columns `study`, `doi`, `dataset_version`, `file_id`, `file_name`,
#'   `format`, `md5_expected`, `md5_observed`, `md5_verified`, `unf`,
#'   `n_rows`, `n_cols`, `pinned`, `retrieved_via` (`"network"`,
#'   `"session_cache"`, `"disk_cache"`, `"local_demo"` for the shipped demo
#'   study, or `"user_data"` for a file [qes_download()] found already in
#'   place), `retrieved_at` (UTC), `licence`, `label_source`,
#'   `label_file_id`, `name_map_applied`, `reader`, `haven_version`,
#'   `catalog_version` and `dict_version`. Columns that do not apply (the
#'   reader of a document, say) are `NA`. For data read from frames given
#'   to [qes_harmonize()] in `data`, `md5_verified` is `FALSE` and
#'   `retrieved_via` is `"user_data"`.
#'
#'   For `level = "cell"` (harmonized data), one row per study and target:
#'   `study`, `wave`, `target`, `source_var`, `rule`, `map_id`, `grade`,
#'   `status` (of the crosswalk row: `stable`, or `review` and `draft` for
#'   rows not yet signed off), `instrument`, `weight_var` (the wave's
#'   recommended weight), `weight_status` (`reviewed`, or `needs_review`
#'   when the weight is not documented yet and is `NA` in the data),
#'   `weight_mean_raw` (the mean of the raw weight over the wave's members),
#'   `levels_not_offered` (structural zeros, `;`-separated; empty when
#'   every level was offered, `NA` for targets without levels), `included`
#'   (`FALSE` when no row was applied), `excluded` (why not: `no_row`, the
#'   study has no question for the target; `not_reviewed`, its row is not
#'   yet signed off and `include_draft = FALSE`; `not_in_data`, the
#'   variable is not in the demonstration data; `below_grade`, its grade is
#'   below `min_grade`; `NA` when included), `n_valid`, one count
#'   `n_<reason>` per NA reason (they sum with `n_valid` to the study's
#'   rows), `n_outside_universe` (for targets about an election: the
#'   wave's members who could not vote in it, `eligible_voter` `FALSE`; `NA`
#'   for other targets, and `NA` when eligibility is not available for any
#'   member of the wave, for example when its age rows are not signed off
#'   and `include_draft = FALSE`) and `note`.
#'
#'   For `level = "spec"`, one row (one per [qes_harmonize()] call for
#'   results combined with [rbind()]): `spec_version`, `spec_hash`,
#'   `spec_custom`, `qesR_version`, `qesR_sha` (the commit of a GitHub
#'   installation, else `NA`), `args` (the arguments of the call, with a
#'   fingerprint of any data given in `data`) and `created` (UTC).
#'
#' @family reproducibility
#' @seealso [qes_cite()] to cite the studies, [qes_studies()] for the pinned
#'   versions.
#' @examples
#' demo <- get_qes("qes_demo", assign_global = FALSE, quiet = TRUE)
#' qes_provenance(demo)
#'
#' prov <- as.data.frame(qes_provenance(demo))
#' prov[, c("study", "file_id", "md5_verified", "retrieved_via")]
#'
#' # what get_qes() would read, before anything is downloaded
#' qes_provenance(c("qes2018", "qes2014"))
#'
#' # merge() drops the record: keep it first
#' prov <- qes_provenance(demo)
#' merged <- merge(demo, data.frame(extra_column = 1))
#' try(qes_provenance(merged))
#' @export
qes_provenance <- function(x, level = c("study", "cell", "spec")) {
  level <- .qes_check_one(level, "level", .qes_provenance_levels)
  if (is.character(x) && !inherits(x, "qes_provenance")) {
    codes <- .qes_resolve_codes(x, "x", demo = TRUE)
    if (!identical(level, "study")) {
      .qes_abort(
        "no_provenance_level",
        class = "qesR_error_no_provenance",
        args = list("x", level),
        data = list(arg = "x", level = level)
      )
    }
    return(structure(.qes_provenance_codes(codes), class = c("qes_provenance", "data.frame")))
  }
  prov <- if (inherits(x, "qes_provenance")) x else attr(x, "qes_provenance", exact = TRUE)
  if (!is.data.frame(prov) || !("study" %in% names(prov))) {
    .qes_abort(
      "no_provenance",
      class = "qesR_error_no_provenance",
      args = list("x"),
      data = list(arg = "x")
    )
  }
  # harmonized data (qes_harmonize()) also carry cell- and spec-level records
  if (!identical(level, "study") && is.data.frame(attr(prov, level, exact = TRUE))) {
    out <- as.data.frame(attr(prov, level, exact = TRUE), stringsAsFactors = FALSE)
    rownames(out) <- NULL
    return(structure(out, class = c("qes_provenance", "data.frame")))
  }
  if (!identical(level, "study")) {
    .qes_abort(
      "no_provenance_level",
      class = "qesR_error_no_provenance",
      args = list("x", level),
      data = list(arg = "x", level = level)
    )
  }
  prov <- as.data.frame(prov, stringsAsFactors = FALSE)
  rownames(prov) <- NULL
  attr(prov, "cell") <- NULL
  attr(prov, "spec") <- NULL
  structure(prov, class = c("qes_provenance", "data.frame"))
}

# One paragraph per row, in the message language (printing is output, not a
# returned value: design rule P4).
.qes_provenance_text <- function(x, lang = .qes_lang()) {
  msg <- function(id, ...) .qes_msg(id, list(...), lang = lang)
  known <- function(v) length(v) == 1L && !is.na(v) && nzchar(as.character(v))
  vapply(seq_len(nrow(x)), function(i) {
    r <- x[i, , drop = FALSE]
    parts <- if (known(r$doi)) {
      msg("prov_head", r$study, r$file_id, r$file_name, r$doi, r$dataset_version)
    } else {
      msg("prov_head_demo", r$study, r$file_id, r$file_name)
    }
    if (isFALSE(r$pinned)) {
      parts <- c(parts, msg("prov_unpinned"))
    }
    parts <- c(parts, if (isTRUE(r$md5_verified)) {
      msg("prov_md5_verified", r$md5_observed)
    } else {
      msg("prov_md5_expected", r$md5_expected)
    })
    if (known(r$n_rows) && known(r$n_cols)) {
      parts <- c(parts, msg("prov_dims", r$n_rows, r$n_cols))
    }
    if (known(r$retrieved_at)) {
      when <- format(r$retrieved_at, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      parts <- c(parts, msg("prov_retrieved", when, if (known(r$retrieved_via)) r$retrieved_via else "?"))
    }
    if (known(r$reader) && known(r$retrieved_at)) {
      parts <- c(parts, msg("prov_reader", r$reader, r$haven_version))
    }
    parts <- c(parts, msg("prov_licence", if (known(r$licence)) r$licence else "-", r$catalog_version))
    paste(parts, collapse = " ")
  }, character(1))
}

#' @export
print.qes_provenance <- function(x, ...) {
  needed <- c(
    "study", "doi", "dataset_version", "file_id", "file_name", "md5_expected",
    "md5_observed", "md5_verified", "n_rows", "n_cols", "pinned", "retrieved_via",
    "retrieved_at", "licence", "reader", "haven_version", "catalog_version"
  )
  if (!all(needed %in% names(x)) || nrow(x) == 0L) {
    print.data.frame(x, ...)
    return(invisible(x))
  }
  lang <- .qes_lang()
  paragraphs <- .qes_provenance_text(x, lang = lang)
  for (p in paragraphs) {
    cat(strwrap(p, width = max(40L, getOption("width", 80L) - 2L)), sep = "\n")
    cat("\n")
  }
  cat(.qes_msg("prov_footer", lang = lang), "\n", sep = "")
  invisible(x)
}
