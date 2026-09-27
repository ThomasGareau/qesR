# inst/extdata/VERSIONS (design.md section 3.1), shared by
# data-raw/build_catalog.R and data-raw/build_dictionary.R.
#
# Fields:
#   catalog_version, dict_version  semver, bumped by hand (MAJOR when a pin
#                                  changes, MINOR when rows are added, PATCH
#                                  for text); kept from the current file;
#   schema_version                 "1";
#   built_from                     file_id:md5 of every catalog file;
#   csv_md5                        md5 of every shipped catalog and
#                                  dictionary table (catalog/*.csv,
#                                  dict/*.csv and dict/*.csv.gz).

write_versions <- function(root = ".") {
  ext <- file.path(root, "inst", "extdata")
  path <- file.path(ext, "VERSIONS")
  old <- if (file.exists(path)) read.dcf(path) else NULL
  keep <- function(field, default) {
    if (!is.null(old) && field %in% colnames(old)) unname(old[1, field]) else default
  }
  files <- utils::read.csv(file.path(ext, "catalog", "files.csv"), colClasses = "character",
                           encoding = "UTF-8")
  built_from <- sort(unique(sprintf("%s:%s", files$file_id, files$md5)))
  tables <- c(
    file.path("catalog", sort(list.files(file.path(ext, "catalog"), pattern = "\\.csv$"))),
    file.path("dict", sort(list.files(file.path(ext, "dict"), pattern = "\\.csv(\\.gz)?$")))
  )
  md5s <- unname(tools::md5sum(file.path(ext, tables)))
  fields <- c(
    catalog_version = keep("catalog_version", "1.0.0"),
    dict_version = keep("dict_version", "1.0.0"),
    schema_version = "1",
    built_from = paste(built_from, collapse = ",\n "),
    csv_md5 = paste(sprintf("%s:%s", tables, md5s), collapse = ",\n ")
  )
  text <- paste0(paste(sprintf("%s: %s", names(fields), fields), collapse = "\n"), "\n")
  con <- file(path, open = "wb")
  on.exit(close(con))
  writeBin(charToRaw(enc2utf8(text)), con)
  invisible(fields)
}
