# Reader fixtures (slice S2b, design.md section 8.1).
#
# local_reader_catalog() writes small synthetic data files at test time
# (haven::write_sav() / write_dta(), a few KB each, no real respondent), pins
# them in a fixture catalog (md5, size, rows, columns) and replaces the two
# test seams for the calling test: .qes_catalog() serves the fixture catalog
# and .qes_transport() serves the files, as a Dataverse server would serve
# originals (<server>/api/access/datafile/<id>?format=original). The download
# cache is off and so is the in-memory memo, unless `memo = TRUE`.

# One fixture file. `data` is written in `format`; the other fields become its
# files.csv row. `n_rows` / `n_cols` / `md5` override what is written (to test
# the checks).
reader_file <- function(study, file_id, data, format = "sav", role = "data",
                        is_default = role == "data", unf = paste0("UNF:6:", study),
                        encoding = NA_character_, n_rows = NULL, n_cols = NULL,
                        md5 = NULL) {
  list(
    study = study, file_id = file_id, data = data, format = format, role = role,
    is_default = is_default, unf = unf, encoding = encoding,
    n_rows = n_rows, n_cols = n_cols, md5 = md5
  )
}

reader_study_row <- function(template, study, data_file_id, label_file_id = NA_character_) {
  row <- template
  row$study <- study
  row$aliases <- NA_character_
  row$doi <- paste0("10.9999/FIX/", toupper(gsub("[^a-z0-9]", "", study)))
  row$data_file_id <- data_file_id
  row$label_file_id <- label_file_id
  row$demo <- FALSE
  row
}

local_reader_catalog <- function(files, name_map = NULL, text_fixes = NULL,
                                 type_fixes = NULL, memo = FALSE,
                                 serve = NULL, .env = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = .env)
  base <- fixture_catalog()
  template <- base$studies[1, , drop = FALSE]
  file_template <- base$files[1, , drop = FALSE]

  paths <- character(0)
  rows <- list()
  for (f in files) {
    path <- file.path(dir, paste0(f$file_id, ".", f$format))
    if (identical(f$format, "dta")) {
      haven::write_dta(f$data, path)
    } else {
      haven::write_sav(f$data, path)
    }
    row <- file_template
    row$study <- f$study
    row$file_id <- f$file_id
    row$role <- f$role
    row$lang <- NA_character_
    row$file_name <- paste0(f$file_id, ".tab")
    row$original_file_name <- basename(path)
    row$format <- f$format
    row$ingested <- TRUE
    row$bytes <- file.size(path)
    row$md5 <- f$md5 %||% unname(tools::md5sum(path))
    row$unf <- f$unf
    row$n_rows <- f$n_rows %||% nrow(f$data)
    row$n_cols <- f$n_cols %||% ncol(f$data)
    row$encoding <- f$encoding
    row$id_vars <- ".row"
    row$is_default <- f$is_default
    rows[[length(rows) + 1L]] <- row
    paths[[f$file_id]] <- path
  }
  file_rows <- do.call(rbind, rows)
  rownames(file_rows) <- NULL

  codes <- unique(file_rows$study)
  study_rows <- do.call(rbind, lapply(codes, function(code) {
    mine <- file_rows[file_rows$study == code, , drop = FALSE]
    donor <- mine$file_id[mine$role == "label_donor"]
    reader_study_row(
      template, code,
      data_file_id = mine$file_id[mine$role == "data" & mine$is_default][1],
      label_file_id = if (length(donor) == 1L) donor else NA_character_
    )
  }))
  rownames(study_rows) <- NULL

  catalog <- base
  catalog$studies <- study_rows
  catalog$files <- file_rows
  catalog$name_map <- name_map %||% base$name_map[0, , drop = FALSE]
  catalog$text_fixes <- text_fixes %||% base$text_fixes[0, , drop = FALSE]
  catalog$type_fixes <- type_fixes %||% base$type_fixes[0, , drop = FALSE]

  log <- new.env(parent = emptyenv())
  log$urls <- character(0)
  log$catalog <- catalog
  log$paths <- paths
  transport <- function(url, dest = NULL, handle = NULL) {
    log$urls <- c(log$urls, url)
    id <- sub("^.*/api/access/datafile/([0-9]+)\\?format=original$", "\\1", url)
    if (!(id %in% names(paths))) {
      stop(sprintf("fixture server: unexpected request '%s'", url), call. = FALSE)
    }
    if (!is.null(serve)) {
      return(fake_response(url, dest, body = serve(id)))
    }
    fake_response(url, dest, write = function(p) file.copy(paths[[id]], p, overwrite = TRUE))
  }

  withr::local_options(qesR.cache = "none", qesR.memo = isTRUE(memo), .local_envir = .env)
  if (isTRUE(memo)) {
    local_clean_memo(.env = .env)
  }
  testthat::local_mocked_bindings(
    .qes_catalog = function(demo = FALSE) catalog,
    .qes_transport = transport,
    .qes_sleep = function(seconds) invisible(NULL),
    .package = "qesR",
    .env = .env
  )
  invisible(log)
}

# A catalog table row set, built from named vectors (all character).
catalog_rows <- function(...) {
  data.frame(..., stringsAsFactors = FALSE)
}

read_fixture <- function(study, ...) {
  getFromNamespace(".qes_read", "qesR")(study, ...)
}

# Empty the in-session memo (parsed data and metadata) now and when the
# calling test ends.
local_clean_memo <- function(.env = parent.frame()) {
  memo_env <- getFromNamespace(".qes_memo", "qesR")
  rm(list = ls(memo_env, all.names = TRUE), envir = memo_env)
  withr::defer(rm(list = ls(memo_env, all.names = TRUE), envir = memo_env), envir = .env)
  invisible()
}
