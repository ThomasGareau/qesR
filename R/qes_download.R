`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) {
    y
  } else {
    x
  }
}

.squish_ws <- function(x) {
  gsub("\\s+", " ", trimws(x))
}

.assert_single_string <- function(x, arg_name) {
  if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x)) {
    stop(sprintf("`%s` must be a single non-empty character string.", arg_name), call. = FALSE)
  }
}

.get_qes_study <- function(srvy) {
  .assert_single_string(srvy, "srvy")

  idx <- match(srvy, .qes_catalog$qes_survey_code)
  if (is.na(idx)) {
    stop(
      sprintf(
        "Unknown survey code '%s'. Use get_qescodes() to see valid codes.",
        srvy
      ),
      call. = FALSE
    )
  }

  .qes_catalog[idx, , drop = FALSE]
}

.download_file_with_fallback <- function(url, destfile, quiet = TRUE, allow_insecure_retry = FALSE) {
  initial_error <- NULL

  downloaded <- tryCatch(
    {
      utils::download.file(url, destfile, mode = "wb", quiet = quiet)
      TRUE
    },
    error = function(e) {
      initial_error <<- conditionMessage(e)
      FALSE
    }
  )

  if (isTRUE(downloaded)) {
    return(invisible(destfile))
  }

  should_retry_insecure <- isTRUE(allow_insecure_retry) ||
    grepl("ssl|certificate", initial_error %||% "", ignore.case = TRUE)

  if (!should_retry_insecure) {
    stop(
      sprintf("Failed to download from '%s': %s", url, initial_error %||% "unknown error"),
      call. = FALSE
    )
  }

  insecure_error <- NULL
  old_method <- getOption("download.file.method")
  old_extra <- getOption("download.file.extra")
  on.exit(
    options(download.file.method = old_method, download.file.extra = old_extra),
    add = TRUE
  )

  options(
    download.file.method = "libcurl",
    download.file.extra = "--insecure --location --retry 3 --retry-delay 1"
  )

  downloaded_insecure <- tryCatch(
    {
      utils::download.file(url, destfile, mode = "wb", quiet = quiet)
      TRUE
    },
    error = function(e) {
      insecure_error <<- conditionMessage(e)
      FALSE
    }
  )

  if (isTRUE(downloaded_insecure)) {
    return(invisible(destfile))
  }

  if (nzchar(Sys.which("curl"))) {
    status <- suppressWarnings(system2(
      "curl",
      args = c(
        "-fsSL",
        "--retry", "3",
        "--retry-delay", "1",
        "--insecure",
        "--location",
        url,
        "-o", destfile
      ),
      stdout = if (isTRUE(quiet)) FALSE else "",
      stderr = if (isTRUE(quiet)) FALSE else ""
    ))

    if (identical(status, 0L)) {
      return(invisible(destfile))
    }
  }

  stop(
    sprintf(
      paste0(
        "Failed to download from '%s'. Initial error: %s. ",
        "Insecure retry error: %s"
      ),
      url,
      initial_error %||% "unknown error",
      insecure_error %||% "curl fallback failed"
    ),
    call. = FALSE
  )
}

.fetch_qes_metadata <- function(study, quiet = TRUE) {
  persistent_id <- paste0("doi:", study$doi)
  metadata_url <- sprintf(
    "%s/api/datasets/:persistentId/?persistentId=%s",
    study$server,
    utils::URLencode(persistent_id, reserved = TRUE)
  )

  json_file <- tempfile(fileext = ".json")
  on.exit(unlink(json_file), add = TRUE)

  .download_file_with_fallback(
    metadata_url,
    json_file,
    quiet = quiet,
    allow_insecure_retry = isTRUE(study$allow_insecure_retry)
  )
  metadata <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)

  if (!identical(metadata$status, "OK")) {
    stop(
      sprintf("Unable to fetch metadata for %s (%s).", study$qes_survey_code, study$doi),
      call. = FALSE
    )
  }

  metadata
}

.extract_qes_files <- function(metadata, srvy_code) {
  files <- metadata$data$latestVersion$files

  if (is.null(files) || length(files) == 0L) {
    stop(sprintf("No files were found for survey code '%s'.", srvy_code), call. = FALSE)
  }

  data.frame(
    id = vapply(files, function(x) as.character(x$dataFile$id %||% ""), character(1)),
    filename = vapply(files, function(x) x$dataFile$filename %||% "", character(1)),
    extension = vapply(files, function(x) {
      tolower(tools::file_ext(x$dataFile$filename %||% ""))
    }, character(1)),
    size = as.numeric(vapply(files, function(x) x$dataFile$filesize %||% NA_real_, numeric(1))),
    stringsAsFactors = FALSE
  )
}

.select_file_by_preference <- function(df) {
  preferred_extensions <- c(
    "sav", "zsav", "dta", "por", "sas7bdat", "xpt",
    "csv", "tsv", "tab", "txt", "rds", "zip"
  )

  for (ext in preferred_extensions) {
    idx <- which(df$extension == ext)
    if (length(idx) > 0L) {
      candidates <- df[idx, , drop = FALSE]
      return(candidates[which.max(candidates$size), , drop = FALSE])
    }
  }

  df[which.max(df$size), , drop = FALSE]
}

.choose_remote_file <- function(files_df, file = NULL) {
  if (!is.null(file)) {
    .assert_single_string(file, "file")

    matches <- grepl(file, files_df$filename, ignore.case = TRUE)
    if (!any(matches)) {
      stop(
        sprintf(
          "No files matched regex '%s'. Available files: %s",
          file,
          paste(files_df$filename, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    return(files_df[which(matches)[1], , drop = FALSE])
  }

  non_data_pattern <- "(codebook|questionnaire|instrument|readme|documentation|syntax|\\.pdf$)"
  data_candidates <- files_df[!grepl(non_data_pattern, files_df$filename, ignore.case = TRUE), , drop = FALSE]

  if (nrow(data_candidates) == 0L) {
    data_candidates <- files_df
  }

  .select_file_by_preference(data_candidates)
}

.find_qes_codebook_files <- function(files_df) {
  codebook_pattern <- "(codebook|questionnaire|instrument|readme|documentation|syntax|dictionary|metadata|ddi|\\.pdf$)"
  files_df[grepl(codebook_pattern, files_df$filename, ignore.case = TRUE), , drop = FALSE]
}

.download_qes_datafile <- function(study, file_id, filename, quiet = TRUE) {
  ext <- tolower(tools::file_ext(filename))
  tmp <- tempfile(fileext = if (nzchar(ext)) paste0(".", ext) else "")
  access_url <- sprintf("%s/api/access/datafile/%s", study$server, file_id)

  .download_file_with_fallback(
    access_url,
    tmp,
    quiet = quiet,
    allow_insecure_retry = isTRUE(study$allow_insecure_retry)
  )
  tmp
}

.fetch_qes_ddi <- function(study, file_id, quiet = TRUE) {
  ddi_urls <- c(
    sprintf("%s/api/access/datafile/%s/metadata/ddi", study$server, file_id),
    sprintf("%s/api/files/%s/metadata/ddi", study$server, file_id)
  )

  for (url in ddi_urls) {
    xml_file <- tempfile(fileext = ".xml")
    on.exit(unlink(xml_file), add = TRUE)

    downloaded <- tryCatch(
      {
        .download_file_with_fallback(
          url,
          xml_file,
          quiet = quiet,
          allow_insecure_retry = isTRUE(study$allow_insecure_retry)
        )
        TRUE
      },
      error = function(e) FALSE
    )

    if (!downloaded) {
      next
    }

    ddi <- tryCatch(xml2::read_xml(xml_file), error = function(e) NULL)
    if (!is.null(ddi)) {
      return(ddi)
    }
  }

  NULL
}

.ddi_text <- function(node, xpath) {
  out <- xml2::xml_find_first(node, xpath)
  if (inherits(out, "xml_missing")) {
    return(NA_character_)
  }

  txt <- trimws(xml2::xml_text(out))
  if (nzchar(txt)) txt else NA_character_
}

.ddi_question_text <- function(var_node) {
  qstn_nodes <- xml2::xml_find_all(var_node, ".//*[local-name()='qstn']")
  text_nodes <- xml2::xml_find_all(
    var_node,
    ".//*[local-name()='qstnLit' or local-name()='ivuInstr' or local-name()='preQTxt' or local-name()='postQTxt']"
  )

  chunks <- character(0)

  if (length(text_nodes) > 0L) {
    chunks <- xml2::xml_text(text_nodes)
  } else if (length(qstn_nodes) > 0L) {
    chunks <- xml2::xml_text(qstn_nodes)
  }

  if (length(chunks) == 0L) {
    return(NA_character_)
  }

  chunks <- .squish_ws(chunks)
  chunks <- chunks[nzchar(chunks)]

  if (length(chunks) == 0L) {
    return(NA_character_)
  }

  .squish_ws(paste(chunks, collapse = " "))
}

.parse_qes_ddi <- function(ddi_xml) {
  if (is.null(ddi_xml)) {
    return(NULL)
  }

  vars <- xml2::xml_find_all(ddi_xml, ".//*[local-name()='var']")
  if (length(vars) == 0L) {
    return(NULL)
  }

  parsed <- lapply(vars, function(var_node) {
    variable <- xml2::xml_attr(var_node, "name") %||%
      xml2::xml_attr(var_node, "ID") %||%
      xml2::xml_attr(var_node, "id")

    label <- .ddi_text(var_node, "./*[local-name()='labl'][1]")
    question <- .ddi_question_text(var_node)

    cat_nodes <- xml2::xml_find_all(var_node, ".//*[local-name()='catgry']")
    value_labels <- character(0)
    if (length(cat_nodes) > 0L) {
      for (cat_node in cat_nodes) {
        value <- .ddi_text(cat_node, "./*[local-name()='catValu'][1]")
        value_label <- .ddi_text(cat_node, "./*[local-name()='labl'][1]")

        if (!is.na(value) && !is.na(value_label)) {
          value_labels[value] <- value_label
        }
      }
    }

    list(
      variable = variable %||% NA_character_,
      label = label,
      question = question,
      value_labels = value_labels
    )
  })

  variable <- vapply(parsed, function(x) x$variable %||% NA_character_, character(1))
  keep <- !is.na(variable) & nzchar(variable)
  if (!any(keep)) {
    return(NULL)
  }

  parsed <- parsed[keep]
  variable <- variable[keep]

  codebook_df <- data.frame(
    variable = variable,
    label = vapply(parsed, function(x) x$label %||% NA_character_, character(1)),
    question = vapply(parsed, function(x) x$question %||% NA_character_, character(1)),
    n_value_labels = as.integer(vapply(parsed, function(x) length(x$value_labels), integer(1))),
    stringsAsFactors = FALSE
  )

  codebook_df <- codebook_df[!duplicated(codebook_df$variable), , drop = FALSE]
  parsed <- parsed[match(codebook_df$variable, variable)]

  value_labels <- lapply(parsed, function(x) x$value_labels)
  names(value_labels) <- codebook_df$variable

  list(
    data = codebook_df,
    value_labels = value_labels
  )
}

.has_pdf_text_extractor <- function() {
  nzchar(Sys.which("pdftotext")) || nzchar(Sys.which("gs"))
}

.parse_variable_question_line <- function(line, variables) {
  m <- regexec("^([A-Za-z][A-Za-z0-9_\\.]+)\\s+(.*)$", line, perl = TRUE)
  hit <- regmatches(line, m)[[1]]

  if (length(hit) < 3L) {
    return(NULL)
  }

  variable <- hit[2]
  if (!(variable %in% variables)) {
    return(NULL)
  }

  list(
    variable = variable,
    question_chunk = .squish_ws(hit[3])
  )
}

.clean_pdf_question_candidate <- function(x) {
  x <- .squish_ws(x)
  if (!nzchar(x)) {
    return(NA_character_)
  }

  x <- sub("\\s*\\u25BC.*$", "", x)
  x <- sub("\\s+(If|Si)\\b.*$", "", x)
  x <- .squish_ws(x)

  if (!nzchar(x)) NA_character_ else x
}

.extract_codebook_questions_from_lines <- function(lines, variables) {
  lines <- .squish_ws(lines)
  lines <- lines[!is.na(lines) & nzchar(lines)]
  variables <- unique(variables[!is.na(variables) & nzchar(variables)])

  if (length(lines) == 0L || length(variables) == 0L) {
    return(list())
  }

  out <- list()
  i <- 1L

  while (i <= length(lines)) {
    parsed <- .parse_variable_question_line(lines[i], variables)
    if (is.null(parsed)) {
      i <- i + 1L
      next
    }

    var <- parsed$variable
    if (!is.null(out[[var]])) {
      i <- i + 1L
      next
    }

    chunks <- parsed$question_chunk
    j <- i + 1L

    while (j <= length(lines)) {
      next_line <- lines[j]
      next_parsed <- .parse_variable_question_line(next_line, variables)

      if (!is.null(next_parsed)) {
        break
      }

      if (grepl("^[0-9]+$", next_line)) {
        j <- j + 1L
        next
      }

      if (grepl("^(If|Si)\\b", next_line)) {
        break
      }

      if (grepl("^\\u25BC", next_line)) {
        break
      }

      chunks <- c(chunks, next_line)

      if (grepl("\\?$", next_line)) {
        j <- j + 1L
        break
      }

      if (length(chunks) >= 8L) {
        j <- j + 1L
        break
      }

      j <- j + 1L
    }

    candidate <- .clean_pdf_question_candidate(paste(chunks, collapse = " "))
    if (!is.na(candidate) && nzchar(candidate)) {
      out[[var]] <- candidate
    }

    i <- j
  }

  out
}

.extract_codebook_questions_from_pdf <- function(study, codebook_df, file_manifest, quiet = TRUE) {
  if (!is.data.frame(codebook_df) || nrow(codebook_df) == 0L) {
    return(list())
  }

  if (!.has_pdf_text_extractor()) {
    return(list())
  }

  codebook_files <- .find_qes_codebook_files(file_manifest)
  if (nrow(codebook_files) == 0L || !("extension" %in% names(codebook_files))) {
    return(list())
  }

  pdf_idx <- which(tolower(codebook_files$extension) == "pdf")
  if (length(pdf_idx) == 0L) {
    return(list())
  }

  pdf_row <- codebook_files[pdf_idx[1], , drop = FALSE]
  if (!("download_url" %in% names(pdf_row)) || is.na(pdf_row$download_url[1])) {
    return(list())
  }

  pdf_file <- tempfile(fileext = ".pdf")
  on.exit(unlink(pdf_file), add = TRUE)

  downloaded <- tryCatch(
    {
      .download_file_with_fallback(
        pdf_row$download_url[1],
        pdf_file,
        quiet = quiet,
        allow_insecure_retry = isTRUE(study$allow_insecure_retry)
      )
      TRUE
    },
    error = function(e) FALSE
  )

  if (!downloaded) {
    return(list())
  }

  lines <- .extract_pdf_text_lines(pdf_file, quiet = quiet)
  if (length(lines) == 0L) {
    return(list())
  }

  .extract_codebook_questions_from_lines(lines, variables = codebook_df$variable)
}

.enrich_codebook_questions <- function(out, study, file_manifest, quiet = TRUE) {
  if (!is.data.frame(out) || nrow(out) == 0L || !all(c("label", "question", "variable") %in% names(out))) {
    return(out)
  }

  missing_question <- is.na(out$question) | !nzchar(out$question)
  has_label <- !is.na(out$label) & nzchar(out$label)
  out$question[missing_question & has_label] <- out$label[missing_question & has_label]

  parsed_questions <- .extract_codebook_questions_from_pdf(
    study = study,
    codebook_df = out,
    file_manifest = file_manifest,
    quiet = quiet
  )

  if (length(parsed_questions) == 0L) {
    return(out)
  }

  for (var in names(parsed_questions)) {
    idx <- match(var, out$variable)
    if (is.na(idx)) {
      next
    }

    candidate <- .clean_pdf_question_candidate(parsed_questions[[var]])
    if (is.na(candidate) || !nzchar(candidate)) {
      next
    }

    current_question <- out$question[idx]
    current_label <- out$label[idx]

    should_replace_question <- is.na(current_question) ||
      !nzchar(current_question) ||
      .is_likely_truncated_question(current_question) ||
      (nchar(candidate) > nchar(current_question) + 10L)

    if (should_replace_question) {
      out$question[idx] <- candidate
    }

    should_replace_label <- is.na(current_label) ||
      !nzchar(current_label) ||
      .is_likely_truncated_question(current_label)

    if (should_replace_label) {
      out$label[idx] <- candidate
    }
  }

  out
}

.coerce_label_values <- function(values, x) {
  if (is.numeric(x) || is.integer(x)) {
    cast <- suppressWarnings(as.numeric(values))
    keep <- !is.na(cast)
    return(list(values = cast[keep], keep = keep, target_type = "numeric"))
  }

  if (is.logical(x)) {
    numeric_cast <- suppressWarnings(as.numeric(values))
    numeric_keep <- !is.na(numeric_cast)
    if (any(numeric_keep)) {
      return(list(values = numeric_cast[numeric_keep], keep = numeric_keep, target_type = "numeric"))
    }

    lower <- tolower(as.character(values))
    logical_cast <- rep(NA, length(lower))
    logical_cast[lower %in% c("true", "t")] <- TRUE
    logical_cast[lower %in% c("false", "f")] <- FALSE
    logical_keep <- !is.na(logical_cast)
    return(list(values = as.logical(logical_cast[logical_keep]), keep = logical_keep, target_type = "logical"))
  }

  cast <- as.character(values)
  keep <- !is.na(cast)
  list(values = cast[keep], keep = keep, target_type = "character")
}

.apply_ddi_labels <- function(data, ddi_parsed) {
  if (is.null(ddi_parsed) || !is.data.frame(data)) {
    return(data)
  }

  codebook <- ddi_parsed$data
  vars <- intersect(names(data), codebook$variable)
  if (length(vars) == 0L) {
    return(data)
  }

  for (var in vars) {
    idx <- match(var, codebook$variable)
    x <- data[[var]]

    label <- codebook$label[idx]
    question <- codebook$question[idx]

    if (!is.na(label) && nzchar(label)) {
      attr(x, "label") <- label
    }

    if (!is.na(question) && nzchar(question)) {
      attr(x, "qes_question") <- question
    }

    map <- ddi_parsed$value_labels[[var]]
    if (!is.null(map) && length(map) > 0L) {
      coerced <- .coerce_label_values(names(map), x)
      if (length(coerced$values) > 0L) {
        if (is.logical(x) && identical(coerced$target_type, "numeric")) {
          x <- as.numeric(x)
        }
        if (is.logical(x) && identical(coerced$target_type, "character")) {
          x <- as.character(x)
        }
        if (is.logical(x) && identical(coerced$target_type, "logical")) {
          x <- as.logical(x)
        }

        map_labels <- unname(map[coerced$keep])
        map_labels <- make.unique(map_labels)
        labels_vec <- stats::setNames(coerced$values, map_labels)

        x <- haven::labelled(
          x,
          labels = labels_vec,
          label = attr(x, "label", exact = TRUE)
        )

        if (!is.na(question) && nzchar(question)) {
          attr(x, "qes_question") <- question
        }
      }
    }

    data[[var]] <- x
  }

  data
}

.build_qes_codebook <- function(study, files_df, selected_file, ddi_parsed = NULL, quiet = TRUE) {
  if (!is.null(ddi_parsed)) {
    out <- ddi_parsed$data
  } else {
    out <- data.frame(
      variable = character(0),
      label = character(0),
      question = character(0),
      n_value_labels = integer(0),
      stringsAsFactors = FALSE
    )
  }

  file_manifest <- data.frame(
    file_id = files_df$id,
    filename = files_df$filename,
    extension = files_df$extension,
    size = files_df$size,
    download_url = sprintf("%s/api/access/datafile/%s", study$server, files_df$id),
    stringsAsFactors = FALSE
  )

  out <- .enrich_codebook_questions(out, study = study, file_manifest = file_manifest, quiet = quiet)

  attr(out, "survey_code") <- study$qes_survey_code
  attr(out, "doi") <- study$doi
  attr(out, "doi_url") <- study$doi_url
  attr(out, "selected_data_file") <- selected_file$filename
  attr(out, "files") <- file_manifest
  attr(out, "codebook_files") <- .find_qes_codebook_files(file_manifest)
  attr(out, "value_labels_map") <- if (!is.null(ddi_parsed) && !is.null(ddi_parsed$value_labels)) {
    ddi_parsed$value_labels
  } else {
    list()
  }
  class(out) <- unique(c("qes_codebook", class(out)))

  out
}

.read_text_table <- function(path) {
  tabular <- tryCatch(
    utils::read.delim(path, stringsAsFactors = FALSE, check.names = FALSE),
    error = function(e) NULL
  )

  if (!is.null(tabular) && ncol(tabular) > 1L) {
    return(tabular)
  }

  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

.read_qes_zip <- function(path) {
  unzip_dir <- tempfile("qes_zip_")
  dir.create(unzip_dir)
  on.exit(unlink(unzip_dir, recursive = TRUE), add = TRUE)

  utils::unzip(path, exdir = unzip_dir)
  files <- list.files(unzip_dir, recursive = TRUE, full.names = TRUE)

  if (length(files) == 0L) {
    stop("Zip file did not contain any files.", call. = FALSE)
  }

  local_df <- data.frame(
    path = files,
    filename = basename(files),
    extension = tolower(tools::file_ext(files)),
    size = as.numeric(file.info(files)$size),
    stringsAsFactors = FALSE
  )

  supported <- c("sav", "zsav", "dta", "por", "sas7bdat", "xpt", "csv", "tsv", "tab", "txt", "rds")
  local_df <- local_df[local_df$extension %in% supported, , drop = FALSE]

  if (nrow(local_df) == 0L) {
    stop("No supported data files were found in the zip archive.", call. = FALSE)
  }

  non_data_pattern <- "(codebook|questionnaire|instrument|readme|documentation|syntax|\\.pdf$)"
  local_candidates <- local_df[!grepl(non_data_pattern, local_df$filename, ignore.case = TRUE), , drop = FALSE]

  if (nrow(local_candidates) == 0L) {
    local_candidates <- local_df
  }

  chosen <- .select_file_by_preference(local_candidates)
  .read_qes_file(chosen$path[1])
}

.read_qes_file <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("sav", "zsav")) {
    return(haven::read_sav(path))
  }

  if (ext == "dta") {
    return(haven::read_dta(path))
  }

  if (ext == "por") {
    return(haven::read_por(path))
  }

  if (ext == "sas7bdat") {
    return(haven::read_sas(path))
  }

  if (ext == "xpt") {
    return(haven::read_xpt(path))
  }

  if (ext == "csv") {
    return(utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE))
  }

  if (ext %in% c("tsv", "tab", "txt")) {
    return(.read_text_table(path))
  }

  if (ext == "rds") {
    return(readRDS(path))
  }

  if (ext == "zip") {
    return(.read_qes_zip(path))
  }

  stop(
    sprintf("Unsupported file extension '%s'.", ext),
    call. = FALSE
  )
}

.download_and_read_qes <- function(study, file = NULL, quiet = TRUE, read_data = TRUE) {
  metadata <- .fetch_qes_metadata(study, quiet = quiet)
  files_df <- .extract_qes_files(metadata, srvy_code = study$qes_survey_code)
  selected <- .choose_remote_file(files_df, file = file)
  ddi <- .fetch_qes_ddi(study, selected$id, quiet = quiet)
  ddi_parsed <- .parse_qes_ddi(ddi)
  codebook <- .build_qes_codebook(
    study,
    files_df,
    selected_file = selected,
    ddi_parsed = ddi_parsed,
    quiet = quiet
  )

  data <- NULL
  if (isTRUE(read_data)) {
    local_path <- .download_qes_datafile(study, selected$id, selected$filename, quiet = quiet)
    on.exit(unlink(local_path), add = TRUE)

    data <- .read_qes_file(local_path)
    data <- .apply_ddi_labels(data, ddi_parsed)
  }

  list(
    data = data,
    codebook = codebook,
    selected_file = selected,
    files = files_df
  )
}
