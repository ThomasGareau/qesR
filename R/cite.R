# Citations (design.md sections 2.2 and 9): qes_cite() and the static
# inst/CITATION, both built from .qes_citation_fields.

# The qesR citation. data-raw/make_citation.R writes inst/CITATION from these
# fields; a test checks that citation("qesR") and qes_cite(NULL, "bibentry")
# agree. inst/CITATION never calls package code, because readCitationFile()
# can run without the namespace loaded.
.qes_citation_fields <- list(
  key = "qesR",
  title = "{qesR}: Access Quebec Election Study Datasets",
  given = "Thomas",
  family = "Gareau-Paquette",
  year = "2026",
  url = "https://github.com/ThomasGareau/qesR"
)

.qes_cite_package <- function(version = as.character(utils::packageVersion("qesR")), spec = NULL) {
  f <- .qes_citation_fields
  note <- paste("R package version", version)
  if (!is.null(spec)) {
    note <- paste0(note, "; ", .qes_cite_spec_text(spec, "en"))
  }
  utils::bibentry(
    bibtype = "Manual",
    key = f$key,
    title = f$title,
    author = utils::person(f$given, f$family),
    year = f$year,
    note = note,
    url = f$url
  )
}

# "Family, Given" -> person(given, family). A name without a comma is kept
# whole as a family name.
.qes_parse_author <- function(x) {
  parts <- strsplit(x, ",", fixed = TRUE)[[1]]
  if (length(parts) < 2L) {
    return(utils::person(family = trimws(x)))
  }
  utils::person(given = trimws(paste(parts[-1], collapse = ",")), family = trimws(parts[1]))
}

# "V1" for version 1.0, "V1.1" for 1.1: the Dataverse citation convention.
.qes_version_label <- function(v) {
  paste0("V", sub("\\.0$", "", v))
}

# The data file a study reads, named in its citation when several catalog
# studies share one deposit (the three 1998 surveys).
.qes_shared_deposit_file <- function(code, catalog) {
  studies <- catalog$studies
  row <- studies[studies$study == code, , drop = FALSE]
  if (sum(studies$doi %in% row$doi) < 2L) {
    return(NA_character_)
  }
  files <- catalog$files
  f <- files[files$study == code & files$file_id == row$data_file_id, , drop = FALSE]
  f$original_file_name[1]
}

.qes_cite_dataset <- function(code, catalog) {
  s <- catalog$studies[catalog$studies$study == code, , drop = FALSE]
  authors <- .qes_split_list(s$authors)
  file <- .qes_shared_deposit_file(code, catalog)
  utils::bibentry(
    bibtype = "Misc",
    key = code,
    title = s$title_deposit,
    author = do.call(c, lapply(authors, .qes_parse_author)),
    year = as.character(s$citation_year),
    publisher = s$publisher,
    version = .qes_version_label(s$dataset_version),
    doi = s$doi,
    url = .qes_doi_url(s$doi),
    note = if (is.na(file)) s$dataset_unf else paste0(s$dataset_unf, "; file ", file)
  )
}

# The Dataverse citation: authors, year, "title", DOI link, publisher,
# version, UNF; for a study that is one file of a shared deposit, the file.
.qes_cite_dataset_text <- function(s, authors, file, lang = "en") {
  text <- sprintf(
    "%s, %s, \"%s\", %s, %s, %s, %s",
    paste(authors, collapse = "; "),
    s$citation_year,
    s$title_deposit,
    .qes_doi_url(s$doi),
    s$publisher,
    .qes_version_label(s$dataset_version),
    s$dataset_unf
  )
  if (!is.null(file)) {
    text <- paste0(text, " [", .qes_cite_word("file", lang), file, "]")
  }
  # a licence that asks more than CC0 (qes2022, CC BY-NC 4.0) is named with
  # its URL, since its data and the metadata qesR ships are under it
  if (!is.null(s$licence) && isTRUE(s$licence %in% names(.qes_metadata_licences))) {
    text <- paste0(text, " [", .qes_cite_word("licence", lang), s$licence, ", ",
                   .qes_metadata_licences[[s$licence]], "]")
  }
  text
}

.qes_cite_word <- function(word, lang) {
  words <- list(
    file = c(en = "file: ", fr = "fichier\u00a0: "),
    licence = c(en = "licence: ", fr = "licence\u00a0: "),
    package = c(en = "R package version ", fr = "package R, version "),
    spec = c(en = "harmonization spec %s (content hash %s)",
             fr = "sp\u00e9cification d'harmonisation %s (empreinte du contenu %s)")
  )
  words[[word]][[lang]]
}

.qes_cite_package_text <- function(lang, version = as.character(utils::packageVersion("qesR")), spec = NULL) {
  f <- .qes_citation_fields
  text <- sprintf(
    "%s, %s, %s, \"%s\", %s%s, %s",
    f$family, f$given, f$year,
    gsub("[{}]", "", f$title),
    .qes_cite_word("package", lang), version,
    f$url
  )
  if (!is.null(spec)) {
    text <- paste0(text, "; ", .qes_cite_spec_text(spec, lang))
  }
  text
}

# "harmonization spec 0.1.2 (content hash ...)", for data from
# qes_harmonize(), whose values depend on the spec version and content.
.qes_cite_spec_text <- function(spec, lang) {
  sprintf(.qes_cite_word("spec", lang), spec$version, spec$hash)
}

# The spec record of harmonized data (attr qes_spec), or NULL.
.qes_cite_spec <- function(x) {
  sp <- if (is.null(x) || is.character(x)) NULL else attr(x, "qes_spec", exact = TRUE)
  if (is.list(sp) && length(sp$version) == 1L && length(sp$hash) == 1L) sp else NULL
}

# Study codes named by `x`: a character vector of codes, or an object that
# records where it came from (attr qes_provenance or qes_survey_code).
.qes_cite_codes <- function(x) {
  if (is.character(x)) {
    return(.qes_resolve_codes(x, "x", demo = TRUE))
  }
  prov <- if (inherits(x, "qes_provenance")) x else attr(x, "qes_provenance", exact = TRUE)
  if (is.data.frame(prov) && "study" %in% names(prov)) {
    # an empty record (qes_download() with no matching file): no study
    if (nrow(prov) == 0L) {
      return(character(0))
    }
    return(.qes_resolve_codes(unique(prov$study), "x", demo = TRUE))
  }
  code <- attr(x, "qes_survey_code", exact = TRUE)
  if (is.character(code) && length(code) >= 1L && !anyNA(code)) {
    return(.qes_resolve_codes(code, "x", demo = TRUE))
  }
  .qes_abort(
    "no_provenance",
    class = "qesR_error_no_provenance",
    args = list("x"),
    data = list(arg = "x")
  )
}

#' Cite qesR and the studies it loads
#'
#' `qes_cite()` returns the citation of qesR and, for each study you name or
#' load, the citation of its Dataverse deposit: authors, year, the verbatim
#' deposit title, the DOI link, the publisher, the pinned dataset version and
#' the dataset UNF, as Dataverse writes it. Everything comes from the catalog
#' shipped with qesR; no network request is made.
#'
#' The three 1998 surveys (`qes1998`, `qes1998_crop`, `qes1998_createc`)
#' share one deposit, so their citations add the data file used.
#'
#' A study released under a licence other than CC0 has its licence and the
#' licence's URL at the end of its text citation: `qes2022` is licensed CC
#' BY-NC 4.0, which also covers the description of it that qesR ships (see
#' the file `COPYRIGHTS` of the installed package).
#'
#' For data from [qes_harmonize()], the qesR citation also gives the
#' harmonization spec version and content hash the values depend on.
#'
#' @param x `NULL` to cite qesR only; a character vector of study codes (see
#'   [qes_studies()]); a data frame returned by [get_qes()] or
#'   [qes_harmonize()], or the result of [qes_download()] or
#'   [qes_provenance()], whose studies are read from its provenance record. A data frame that has lost those attributes
#'   (for example after [merge()]) is an error of class
#'   `qesR_error_no_provenance`. The synthetic study `qes_demo` has no
#'   deposit and adds nothing.
#' @param style `"text"` (default) for plain-text citations, `"bibtex"` for
#'   BibTeX entries, or `"bibentry"` for a [utils::bibentry()] object.
#' @param lang Language of the few words qesR adds to the text citations
#'   (`"en"` or `"fr"`). Titles and author names are always as deposited. It
#'   never follows the session language.
#'
#' @return For `"text"` and `"bibtex"`, a character vector with one element
#'   per citation: qesR first, then each study in the order given. For
#'   `"bibentry"`, a `bibentry` object with the same entries; its keys are
#'   `qesR` and the study codes. `qes_cite(style = "bibentry")` with no study
#'   is the same object as `citation("qesR")`.
#'
#' @section En français:
#' `qes_cite()` donne la citation de qesR et, pour chaque étude nommée ou
#' chargée, celle de son dépôt Dataverse. Une étude diffusée sous une autre
#' licence que CC0 termine sa citation texte par sa licence et l'adresse de
#' celle-ci : `qes2022` est sous licence CC BY-NC 4.0, qui couvre aussi la
#' description de l'étude livrée avec qesR (voir le fichier `COPYRIGHTS` du
#' package installé).
#'
#' @family reproducibility
#' @seealso [qes_studies()] for the catalog, and `citation("qesR")`.
#' @examples
#' qes_cite()
#' qes_cite(c("qes2018", "qes2014"))
#' qes_cite("qes1998_crop", lang = "fr")
#' cat(qes_cite("qes2022", style = "bibtex"), sep = "\n\n")
#' @export
qes_cite <- function(x = NULL, style = c("text", "bibtex", "bibentry"), lang = "en") {
  style <- .qes_check_one(style, "style", c("text", "bibtex", "bibentry"))
  if (!is.character(lang) || length(lang) != 1L || !(lang %in% c("en", "fr"))) {
    .qes_abort(
      "input_choice",
      class = "qesR_error_input",
      args = list("lang", .qes_q(c("en", "fr"))),
      data = list(arg = "lang", value = lang)
    )
  }
  codes <- if (is.null(x)) character(0) else .qes_cite_codes(x)
  catalog <- .qes_catalog()
  codes <- codes[codes %in% catalog$studies$study]

  spec <- .qes_cite_spec(x)
  datasets <- lapply(codes, .qes_cite_dataset, catalog = catalog)
  entries <- c(list(.qes_cite_package(spec = spec)), datasets)

  if (identical(style, "bibentry")) {
    out <- do.call(c, entries)
    if (is.null(x)) {
      # the same object as citation("qesR")
      out <- structure(out, package = "qesR", class = c("citation", "bibentry"))
    }
    return(out)
  }
  if (identical(style, "bibtex")) {
    return(vapply(entries, function(e) paste(utils::toBibtex(e), collapse = "\n"), character(1)))
  }
  texts <- vapply(codes, function(code) {
    s <- catalog$studies[catalog$studies$study == code, , drop = FALSE]
    file <- .qes_shared_deposit_file(code, catalog)
    .qes_cite_dataset_text(s, .qes_split_list(s$authors), if (is.na(file)) NULL else file, lang)
  }, character(1), USE.NAMES = FALSE)
  c(.qes_cite_package_text(lang, spec = spec), texts)
}

# ---- the licence notice of shipped metadata ----------------------------------------------

# The licences under which a study's metadata ships besides CC0, with the URL
# of each (inst/COPYRIGHTS gives the attribution each requires).
.qes_metadata_licences <- c("CC BY-NC 4.0" = "https://creativecommons.org/licenses/by-nc/4.0/")

# The attribution and licence notice of the metadata qesR ships for `study`
# (labels, question text, counts), in `lang`; NULL for a study whose metadata
# is CC0 or does not ship. Printed by print.qes_codebook(), print.qes_search()
# and the harmonization reference, and kept as the attribute "licence_notice"
# of codebooks, qes_question() and qes_search() results
# (.qes_with_licence_notice()); inst/COPYRIGHTS (section 2) gives it in full.
.qes_licence_notice <- function(study, lang = .qes_lang()) {
  if (!is.character(study) || length(study) != 1L || is.na(study)) {
    return(NULL)
  }
  demo <- .qes_is_demo_code(study)
  s <- tryCatch(.qes_catalog(demo = demo)$studies, error = function(e) NULL)
  s <- if (is.null(s)) NULL else s[s$study == study, , drop = FALSE]
  if (is.null(s) || nrow(s) != 1L || !isTRUE(s$metadata_shipped) || !s$licence %in% names(.qes_metadata_licences)) {
    return(NULL)
  }
  family <- sub(",.*$", "", .qes_split_list(s$authors))
  and <- if (identical(lang, "fr")) " et " else " and "
  authors <- if (length(family) > 1L) {
    paste0(paste(family[-length(family)], collapse = ", "), and, family[length(family)])
  } else {
    family
  }
  .qes_msg("metadata_licence_notice", list(
    study, s$title_deposit, authors, s$citation_year, paste0("https://doi.org/", s$doi),
    s$licence, .qes_metadata_licences[[s$licence]]
  ), lang)
}

# `x` with the attribute "licence_notice": the notices of the studies of
# `studies` whose metadata is not CC0 (a character vector named by study), so
# that a codebook, a qes_question() or a qes_search() result keeps its
# attribution when it is saved or exported; no attribute when there is none.
.qes_with_licence_notice <- function(x, studies, lang = .qes_lang()) {
  studies <- unique(as.character(studies[!is.na(studies)]))
  notices <- lapply(studies, .qes_licence_notice, lang = lang)
  keep <- !vapply(notices, is.null, logical(1))
  attr(x, "licence_notice") <- if (any(keep)) stats::setNames(unlist(notices[keep]), studies[keep]) else NULL
  x
}
