# Build the offline study catalog (design.md section 3) from Dataverse JSON.
#
# Usage (from the package root):
#   Rscript data-raw/build_catalog.R [--check]
#   QESR_DV_JSON_DIR=<dir> Rscript data-raw/build_catalog.R [--check]
#
# Without QESR_DV_JSON_DIR it reads the committed snapshots in
# data-raw/inputs/dataverse/: the JSON of the pinned version of every deposit,
#   <server>/api/datasets/:persistentId/versions/<version>?persistentId=doi:<doi>
# and their index.csv, which records the pin (dataset_version), the publisher
# (the installation's name, from the dataset endpoint), the md5 of each
# snapshot and any edit made to it. A snapshot whose md5 differs from
# index.csv stops the script. So the catalog builds from a clean clone with
# no request.
#
# With QESR_DV_JSON_DIR, <dir> holds the current dataset JSON of every
# deposit, as returned by
#   <server>/api/datasets/:persistentId/?persistentId=doi:<doi>
# one file per deposit, any file name ending in .json. Fetch them with plain
# requests (User-Agent "qesR/<ver> R/<ver>", one at a time, at least 1 s
# apart); this script makes no network request itself. If the latestVersion
# of any deposit is not its pinned version (index.csv), the script stops and
# lists them. To move a pin, fetch the new version's JSON into
# data-raw/inputs/dataverse/, update index.csv (version, url, md5), review the
# curated overlays, and rebuild.
#
# Every fact that Dataverse publishes (titles, authors, versions, licences,
# file names, sizes, md5, UNF, citation year) is copied from the JSON. The
# judgments are curated by hand in data-raw/catalog/:
#   studies_curated.csv  codes, families, display names, design, population,
#                        pinned data file, notes (EN/FR);
#   files_curated.csv    which deposit files each study lists, their role and
#                        language, and the facts read from the files
#                        themselves (n_rows, n_cols, id_vars), which were
#                        checked with haven against the md5-verified originals.
#
# Writes inst/extdata/catalog/studies.csv and files.csv and updates
# inst/extdata/VERSIONS. With --check it writes nothing and fails if the
# shipped files differ from what it would write.

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args

json_dir <- Sys.getenv("QESR_DV_JSON_DIR")
if (nzchar(json_dir) && !dir.exists(json_dir)) {
  stop(sprintf("QESR_DV_JSON_DIR (%s) is not a directory. Unset it to use the committed snapshots.", json_dir),
       call. = FALSE)
}

# Licences under which qesR ships a study's metadata (dictionary, wording,
# labels and aggregates), besides CC0: each has its section, with the
# attribution it requires, in inst/COPYRIGHTS. A study under another licence
# gets metadata_shipped = FALSE, and its metadata is built from the data at
# runtime.
shippable_licences <- c("CC BY-NC 4.0")

root <- "."
cat_dir <- file.path(root, "inst", "extdata", "catalog")
cur_dir <- file.path(root, "data-raw", "catalog")

read_utf8_csv <- function(path) {
  utils::read.csv(path, colClasses = "character", na.strings = character(),
                  encoding = "UTF-8", check.names = FALSE, strip.white = FALSE)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x

# ---- deposits ----------------------------------------------------------------

snap_dir <- file.path(root, "data-raw", "inputs", "dataverse")
index <- read_utf8_csv(file.path(snap_dir, "index.csv"))
version_of <- function(v) sprintf("%s.%s", v$versionNumber, v$versionMinorNumber)
# a snapshot of one version, in the shape of the dataset endpoint: the
# dataset-level fields build_catalog reads come from index.csv
as_dataset <- function(v, row) {
  list(latestVersion = v, publisher = row$publisher, persistentUrl = row$persistent_url,
       publicationDate = v$publicationDate)
}

deposits <- list()
if (nzchar(json_dir)) {
  input <- json_dir
  moved <- character(0)
  for (f in list.files(json_dir, pattern = "\\.json$", full.names = TRUE)) {
    j <- jsonlite::fromJSON(f, simplifyVector = FALSE)
    d <- j$data
    if (is.null(d$latestVersion) && !is.null(d$versionNumber)) {
      # a version JSON (as the snapshots are) rather than a dataset JSON
      row <- index[index$doi == sub("^doi:", "", d$datasetPersistentId %||% ""), , drop = FALSE]
      if (nrow(row) != 1L) stop(sprintf("%s: doi not in %s/index.csv.", basename(f), snap_dir), call. = FALSE)
      d <- as_dataset(d, row)
    }
    doi <- sub("^doi:", "", d$latestVersion$datasetPersistentId %||% "")
    if (!nzchar(doi)) {
      doi <- sub("^https://doi.org/", "", d$persistentUrl)
    }
    pin <- index$dataset_version[index$doi == doi]
    if (length(pin) != 1L) {
      stop(sprintf("doi:%s (%s) has no pinned version in %s/index.csv.", doi, basename(f), snap_dir), call. = FALSE)
    }
    if (!identical(version_of(d$latestVersion), pin)) {
      moved <- c(moved, sprintf("doi:%s latest %s, pinned %s", doi, version_of(d$latestVersion), pin))
    }
    deposits[[doi]] <- d
  }
  if (length(moved) > 0L) {
    stop(sprintf(paste0("The latest version on Dataverse is not the pinned version for %d deposit(s):\n  %s\n",
                        "The catalog is built from the pinned versions only. To move a pin, snapshot the new ",
                        "version in %s, update index.csv and review the curated overlays."),
                 length(moved), paste(moved, collapse = "\n  "), snap_dir), call. = FALSE)
  }
} else {
  input <- snap_dir
  for (i in seq_len(nrow(index))) {
    row <- index[i, , drop = FALSE]
    f <- file.path(snap_dir, row$file)
    if (!file.exists(f)) stop(sprintf("Missing snapshot %s.", f), call. = FALSE)
    if (!identical(unname(tools::md5sum(f)), row$md5)) {
      stop(sprintf("%s does not have the md5 recorded in index.csv (%s).", f, row$md5), call. = FALSE)
    }
    v <- jsonlite::fromJSON(f, simplifyVector = FALSE)$data
    if (!identical(sub("^doi:", "", v$datasetPersistentId), row$doi) || !identical(version_of(v), row$dataset_version)) {
      stop(sprintf("%s is not version %s of doi:%s.", f, row$dataset_version, row$doi), call. = FALSE)
    }
    deposits[[row$doi]] <- as_dataset(v, row)
  }
}

citation_field <- function(d, name) {
  for (fld in d$latestVersion$metadataBlocks$citation$fields) {
    if (identical(fld$typeName, name)) return(fld$value)
  }
  NULL
}

format_of <- function(df) {
  type <- df$originalFileFormat %||% df$contentType %||% ""
  if (grepl("spss-sav", type)) return("sav")
  if (grepl("spss-por", type)) return("por")
  if (grepl("x-stata", type)) return("dta")
  if (identical(type, "application/pdf")) return("pdf")
  if (identical(type, "application/msword")) return("doc")
  if (grepl("wordprocessingml", type)) return("docx")
  stop(sprintf("Unknown content type '%s'.", type), call. = FALSE)
}

# ---- studies.csv -----------------------------------------------------------

sc <- read_utf8_csv(file.path(cur_dir, "studies_curated.csv"))
studies <- do.call(rbind, lapply(seq_len(nrow(sc)), function(i) {
  s <- sc[i, , drop = FALSE]
  d <- deposits[[s$doi]]
  if (is.null(d)) stop(sprintf("No JSON for doi:%s (%s).", s$doi, s$study), call. = FALSE)
  lv <- d$latestVersion
  authors <- vapply(citation_field(d, "author"), function(a) a$authorName$value, character(1))
  licence <- lv$license$name %||% ""
  data.frame(
    study = s$study,
    aliases = s$aliases,
    family = s$family,
    title_deposit = citation_field(d, "title"),
    title_en = s$title_en,
    title_fr = s$title_fr,
    authors = paste(authors, collapse = ";"),
    year = s$year,
    year_end = s$year_end,
    election_id = s$election_id,
    study_design = s$study_design,
    default_member = s$default_member,
    target_population_en = s$target_population_en,
    target_population_fr = s$target_population_fr,
    server = s$server,
    doi = s$doi,
    dataset_version = sprintf("%s.%s", lv$versionNumber, lv$versionMinorNumber),
    data_file_id = s$data_file_id,
    label_file_id = s$label_file_id,
    source_lang = s$source_lang,
    licence = licence,
    licence_url = lv$license$uri %||% "",
    metadata_shipped = if (grepl("^CC0", licence) || licence %in% shippable_licences) "TRUE" else "FALSE",
    publisher = d$publisher,
    citation_year = substr(lv$citationDate %||% d$publicationDate, 1, 4),
    dataset_unf = lv$UNF %||% "",
    notes_en = s$notes_en,
    notes_fr = s$notes_fr,
    stringsAsFactors = FALSE
  )
}))

# ---- files.csv ---------------------------------------------------------------

fc <- read_utf8_csv(file.path(cur_dir, "files_curated.csv"))
files <- do.call(rbind, lapply(seq_len(nrow(fc)), function(i) {
  r <- fc[i, , drop = FALSE]
  s <- studies[studies$study == r$study, , drop = FALSE]
  if (nrow(s) != 1L) stop(sprintf("Unknown study '%s' in files_curated.csv.", r$study), call. = FALSE)
  d <- deposits[[s$doi]]
  hit <- Filter(function(x) identical(as.character(x$dataFile$id), r$file_id), d$latestVersion$files)
  if (length(hit) != 1L) {
    stop(sprintf("File %s is not in the pinned version of doi:%s.", r$file_id, s$doi), call. = FALSE)
  }
  x <- hit[[1]]
  df <- x$dataFile
  ingested <- isTRUE(df$tabularData)
  checksum <- df$checksum
  data.frame(
    study = r$study,
    file_id = r$file_id,
    role = r$role,
    lang = r$lang,
    file_name = x$label %||% df$filename,
    original_file_name = df$originalFileName %||% (x$label %||% df$filename),
    format = format_of(df),
    ingested = if (ingested) "TRUE" else "FALSE",
    bytes = format(if (ingested) df$originalFileSize else df$filesize, scientific = FALSE),
    md5 = df$md5,
    checksum_type = checksum$type %||% "MD5",
    unf = df$UNF %||% "",
    n_rows = r$n_rows,
    n_cols = r$n_cols,
    encoding = r$encoding,
    id_vars = r$id_vars,
    is_default = r$is_default,
    dataset_version = s$dataset_version,
    stringsAsFactors = FALSE
  )
}))

# ---- writing -----------------------------------------------------------------

csv_field <- function(x) {
  x[is.na(x)] <- ""
  needs <- grepl("[\",\n\r]", x) | grepl("^\\s|\\s$", x)
  x[needs] <- paste0("\"", gsub("\"", "\"\"", x[needs], fixed = TRUE), "\"")
  x
}

csv_text <- function(df) {
  lines <- c(
    paste(csv_field(names(df)), collapse = ","),
    vapply(seq_len(nrow(df)), function(i) {
      paste(csv_field(vapply(df[i, , drop = FALSE], as.character, character(1))), collapse = ",")
    }, character(1))
  )
  enc2utf8(paste0(paste(lines, collapse = "\n"), "\n"))
}

write_utf8 <- function(text, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
}

out <- list(
  "studies.csv" = csv_text(studies),
  "files.csv" = csv_text(files)
)

if (check_only) {
  bad <- character(0)
  for (nm in names(out)) {
    path <- file.path(cat_dir, nm)
    shipped <- rawToChar(readBin(path, "raw", file.info(path)$size))
    Encoding(shipped) <- "UTF-8"
    if (!identical(shipped, out[[nm]])) bad <- c(bad, nm)
  }
  if (length(bad) > 0L) {
    stop(sprintf("Out of date: %s. Re-run without --check.", paste(bad, collapse = ", ")), call. = FALSE)
  }
  cat(sprintf("Catalog matches the Dataverse JSON (%s) and the curated overlays.\n", input))
  quit(save = "no", status = 0)
}

for (nm in names(out)) {
  write_utf8(out[[nm]], file.path(cat_dir, nm))
}

# ---- VERSIONS ----------------------------------------------------------------

# catalog_version and dict_version are kept (bumped by hand); built_from and
# the md5 of every catalog and dictionary table are recomputed.
source(file.path(root, "data-raw", "versions.R"))
write_versions(root)
cat(sprintf("Wrote %d studies and %d files; VERSIONS updated.\n", nrow(studies), nrow(files)))
