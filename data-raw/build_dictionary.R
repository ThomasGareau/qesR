# Build the variable dictionary shipped with qesR (design.md section 6.1).
#
# Usage (from the package root):
#   QESR_CACHE_DIR=<dir> Rscript data-raw/build_dictionary.R [--check]
#
# <dir> is a qesR download cache (see ?qes_cache_info) holding the pinned
# original data files; a file missing there is downloaded through qesR's own
# client (User-Agent "qesR/<ver> R/<ver>", md5-verified). The package is
# loaded from the source tree with pkgload, so the tables are built by the
# same code that builds a codebook at runtime (.qes_read() and
# .qes_dict_build() in R/read.R and R/metadata.R).
#
# Writes
#   inst/extdata/dict/variables.csv.gz, values.csv.gz
#       every study whose catalog row has metadata_shipped = TRUE (the CC0
#       studies), from its pinned data file (with its label donor) and the
#       curated files below;
#   inst/extdata/demo/dict/variables.csv.gz, values.csv.gz
#       the synthetic qes_demo, whose variables are a subset of qes2014's
#       (same names and labels), with qes2014's curated wording;
#   inst/extdata/dict/shard_rules.csv
#       missing-code rules for the studies whose metadata is not shipped
#       (qes2022, OD3): variable names and codes only, no label text. Built
#       from the cached qes2022 file and data-raw/nc/; kept as is when that
#       file is not in the cache;
# and updates inst/extdata/VERSIONS (data-raw/versions.R). With --check it
# writes nothing and fails if a shipped table differs from what it would
# write.
#
# Curated inputs (UTF-8 CSV, one row per decision, reviewed in PR diffs):
#   data-raw/questions/<study>.csv
#       variable, label (a variable label used only where the file has
#       none), question_en, question_fr, question_truncated, universe_en,
#       universe_fr, question_source, doc_ref (<file_id>:<question>),
#       measure, var_timing, derived_from, reviewed, notes. Drafted by
#       data-raw/extract_questions.R from the deposited questionnaires;
#       reviewed = TRUE once the wording was checked by hand against the
#       document.
#   data-raw/questions/<study>_values.csv
#       variable, value, label, label_lang, label_en, label_fr,
#       missing_type, label_flag, notes. A label is used only for a
#       variable whose file has no value labels at all (label_source
#       "supplement"); missing_type, label_flag and the translations apply
#       to file labels too. label_flag "declared_substantive" marks a code
#       the file declares missing that is a real answer (it then has no
#       missing type); "shifted_in_source" marks labels that do not match
#       their codes.
#   data-raw/questions/missing_labels.csv
#       study, label, missing_type, notes: the missing type of every code
#       whose value label is exactly `label` (after squishing white space)
#       in that study, e.g. qes2014 "Je ne sais pas" -> dk. Codes that a
#       file declares missing (SPSS user-missing) and that no rule types
#       are "user_na".
#   data-raw/nc/missing_labels_qes2022.csv
#       the same for qes2022; used only to write shard_rules.csv.
# Precedence of a code's missing type: <study>_values.csv (including
# label_flag "declared_substantive", which clears it), then
# missing_labels.csv, then the file's declaration (user_na).

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (nzchar(cache_dir)) {
  options(qesR.cache_dir = cache_dir, qesR.cache = "disk")
}
options(qesR.memo = TRUE)

root <- "."
q_dir <- file.path(root, "data-raw", "questions")
dict_dir <- file.path(root, "inst", "extdata", "dict")
demo_dir <- file.path(root, "inst", "extdata", "demo", "dict")

read_curated <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  x <- utils::read.csv(path, colClasses = "character", na.strings = character(),
                       encoding = "UTF-8", check.names = FALSE, strip.white = FALSE)
  for (nm in names(x)) {
    stopifnot(all(validUTF8(x[[nm]])))
    x[[nm]] <- enc2utf8(x[[nm]])
  }
  x
}

nz <- function(x) !is.na(x) & nzchar(x)
enum <- function(name) .qes_enum(name)$value
missing_types <- .qes_missing_types()
# label_flag: "declared_substantive", a code the file declares missing that is
# an answer ("Un autre parti", "ne voterait pas/annulerait" in an intention
# item): it gets no missing type and qes_missing() leaves it alone;
# "shifted_in_source", value labels that do not match the codes (1998
# intvote2, design.md section 5.2).
label_flags <- c("declared_substantive", "shifted_in_source")
missing_rules <- read_curated(file.path(q_dir, "missing_labels.csv"))
stopifnot(all(missing_rules$missing_type %in% missing_types))

# Apply the curated files of `curated_study` to the tables `d` of `study`
# (built from `data`).
curate <- function(d, study, data, curated_study = study) {
  v <- d$variables
  values <- d$values

  q <- read_curated(file.path(q_dir, paste0(curated_study, ".csv")))
  if (!is.null(q)) {
    if (!identical(curated_study, study)) {
      q <- q[q$variable %in% v$variable, , drop = FALSE]
    }
    unknown <- setdiff(q$variable, v$variable)
    if (length(unknown) > 0L) {
      stop(sprintf("%s.csv names variables not in the data: %s", curated_study, paste(unknown, collapse = ", ")), call. = FALSE)
    }
    stopifnot(!anyDuplicated(q$variable))
    stopifnot(all(q$reviewed %in% c("TRUE", "FALSE")))
    stopifnot(all(q$measure[nz(q$measure)] %in% enum("measure")))
    stopifnot(all(q$question_truncated %in% c("", "TRUE", "FALSE")))
    i <- match(q$variable, v$variable)
    set <- function(col, from = col) {
      has <- nz(q[[from]])
      v[[col]][i[has]] <<- q[[from]][has]
    }
    set("question_en")
    set("question_fr")
    set("universe_en")
    set("universe_fr")
    set("question_source")
    set("doc_ref")
    set("measure")
    set("var_timing")
    set("derived_from")
    has_q <- nz(q$question_en) | nz(q$question_fr)
    v$question_truncated[i[has_q]] <- q$question_truncated[has_q] %in% "TRUE"
    v$reviewed[i] <- q$reviewed == "TRUE"
    sup <- nz(q$label) & is.na(v$label[i])
    v$label[i[sup]] <- q$label[sup]
    v$label_source[i[sup]] <- "supplement"
    bad <- v$reviewed & is.na(v$question_en) & is.na(v$question_fr)
    if (any(bad)) {
      stop(sprintf("%s: reviewed rows without question text: %s", study, paste(v$variable[bad], collapse = ", ")), call. = FALSE)
    }
  }

  # label rules, below the file's own declarations only where they apply
  rules <- missing_rules[missing_rules$study == curated_study, , drop = FALSE]
  if (nrow(rules) > 0L) {
    hit <- match(values$label, rules$label)
    typed <- !is.na(hit) & values$label_source != "none"
    values$missing_type[typed] <- rules$missing_type[hit[typed]]
  }

  sv <- read_curated(file.path(q_dir, paste0(curated_study, "_values.csv")))
  if (!is.null(sv)) {
    if (!identical(curated_study, study)) {
      sv <- sv[sv$variable %in% v$variable, , drop = FALSE]
    }
    unknown <- setdiff(sv$variable, v$variable)
    if (length(unknown) > 0L) {
      stop(sprintf("%s_values.csv names variables not in the data: %s", curated_study, paste(unknown, collapse = ", ")), call. = FALSE)
    }
    stopifnot(all(sv$missing_type[nz(sv$missing_type)] %in% missing_types))
    stopifnot(all(sv$label_flag[nz(sv$label_flag)] %in% label_flags))
    stopifnot(!any(sv$label_flag %in% "declared_substantive" & nz(sv$missing_type)))
    stopifnot(!anyDuplicated(sv[c("variable", "value")]))
    for (var in unique(sv$variable)) {
      rows <- sv[sv$variable == var, , drop = FALSE]
      mine <- values$variable == var
      file_labelled <- any(values$label_source[mine] != "none")
      if (!file_labelled && any(nz(rows$label))) {
        x <- data[[var]]
        key <- .qes_code_chr(.qes_plain(x))
        for (k in which(nz(rows$label))) {
          j <- which(mine & values$value == rows$value[k])
          if (length(j) == 0L) {
            values <- rbind(values, data.frame(
              study = study, variable = var, value = rows$value[k], label = NA_character_,
              label_source = "none", label_lang = NA_character_, label_en = NA_character_,
              label_fr = NA_character_, missing_type = NA_character_,
              n = sum(key == rows$value[k], na.rm = TRUE), label_flag = NA_character_,
              stringsAsFactors = FALSE
            ))
            mine <- values$variable == var
            j <- nrow(values)
          }
          values$label[j] <- rows$label[k]
          values$label_source[j] <- "supplement"
          values$label_lang[j] <- rows$label_lang[k]
        }
        vi <- match(var, v$variable)
        if (v$label_source[vi] == "none") {
          v$label_source[vi] <- "supplement"
        }
        if (v$measure[vi] == "interval" && !nz(q$measure[match(var, q$variable)] %||% "")) {
          v$measure[vi] <- "nominal"
        }
        # missing types of the supplement's own labels
        if (nrow(rules) > 0L) {
          hit <- match(values$label, rules$label)
          typed <- values$variable == var & !is.na(hit) & values$label_source == "supplement"
          values$missing_type[typed] <- rules$missing_type[hit[typed]]
        }
      }
      for (k in seq_len(nrow(rows))) {
        j <- which(values$variable == var & values$value == rows$value[k])
        if (length(j) != 1L) {
          if (nz(rows$missing_type[k]) || nz(rows$label_flag[k])) {
            stop(sprintf("%s_values.csv: %s = %s is not a code of the data", curated_study, var, rows$value[k]), call. = FALSE)
          }
          next
        }
        if (nz(rows$label_en[k])) values$label_en[j] <- rows$label_en[k]
        if (nz(rows$label_fr[k])) values$label_fr[j] <- rows$label_fr[k]
        if (nz(rows$missing_type[k])) values$missing_type[j] <- rows$missing_type[k]
        if (nz(rows$label_flag[k])) values$label_flag[j] <- rows$label_flag[k]
        # a declared missing code that is an answer: no missing type
        if (rows$label_flag[k] %in% "declared_substantive") values$missing_type[j] <- NA_character_
      }
    }
  }
  values$label_en[values$label_lang %in% "en" & is.na(values$label_en)] <- values$label[values$label_lang %in% "en" & is.na(values$label_en)]
  values$label_fr[values$label_lang %in% "fr" & is.na(values$label_fr)] <- values$label[values$label_lang %in% "fr" & is.na(values$label_fr)]
  ord <- order(match(values$variable, v$variable), suppressWarnings(as.numeric(values$value)), values$value)
  values <- values[ord, , drop = FALSE]
  rownames(values) <- NULL
  list(variables = v, values = values)
}

cat_all <- .qes_catalog(demo = TRUE)
studies <- cat_all$studies
shipped <- studies$study[studies$metadata_shipped %in% TRUE & !studies$demo]

tables <- list()
for (study in shipped) {
  file_row <- .qes_default_data_file(study)
  data <- .qes_read(study, file_row$file_id, quiet = TRUE)
  d <- .qes_dict_build(data, .qes_study_row(study), file_row)
  tables[[study]] <- curate(d, study, data)
  v <- tables[[study]]$variables
  cat(sprintf("%-20s %4d variables, %5d values, %4d with question text, %3d reviewed\n",
    study, nrow(v), nrow(tables[[study]]$values),
    sum(!is.na(v$question_en) | !is.na(v$question_fr)), sum(v$reviewed)))
}
variables <- do.call(rbind, lapply(tables, `[[`, "variables"))
values <- do.call(rbind, lapply(tables, `[[`, "values"))
rownames(variables) <- NULL
rownames(values) <- NULL

demo_data <- .qes_read("qes_demo", quiet = TRUE)
demo <- curate(
  .qes_dict_build(demo_data, .qes_study_row("qes_demo", demo = TRUE), .qes_default_data_file("qes_demo")),
  "qes_demo", demo_data, curated_study = "qes2014"
)

# ---- shard rules (qes2022) --------------------------------------------------------------

rules_path <- file.path(dict_dir, "shard_rules.csv")
shard_rules <- NULL
nc_rules <- read_curated(file.path(root, "data-raw", "nc", "missing_labels_qes2022.csv"))
f22 <- .qes_default_data_file("qes2022")
cached <- qes_cache_info()
if (f22$md5 %in% cached$md5) {
  d22 <- .qes_read("qes2022", f22$file_id, quiet = TRUE)
  b22 <- .qes_dict_build(d22, .qes_study_row("qes2022"), f22)
  vals <- b22$values
  hit <- match(vals$label, nc_rules$label)
  typed <- vals[!is.na(hit) & vals$label_source != "none", , drop = FALSE]
  typed$missing_type <- nc_rules$missing_type[hit[!is.na(hit) & vals$label_source != "none"]]
  # -99 is item nonresponse, except in the "select all that apply" items,
  # whose columns hold 1 (selected) or -99 (not selected)
  has99 <- unique(vals$variable[vals$value == "-99"])
  binary <- has99[vapply(has99, function(x) {
    codes <- unique(stats::na.omit(.qes_code_chr(.qes_plain(d22[[x]]))))
    all(codes %in% c("-99", "1"))
  }, logical(1))]
  shard_rules <- rbind(
    data.frame(study = "qes2022", variable = "*", value = "-99", missing_type = "no_answer",
      evidence = "-99 marks a question left unanswered in the web survey", stringsAsFactors = FALSE),
    data.frame(study = "qes2022", variable = binary, value = rep("-99", length(binary)),
      missing_type = rep("not_selected", length(binary)),
      evidence = rep("select-all item: codes 1 and -99 only", length(binary)), stringsAsFactors = FALSE),
    data.frame(study = "qes2022", variable = typed$variable, value = typed$value,
      missing_type = typed$missing_type,
      evidence = rep("value label (data-raw/nc/missing_labels_qes2022.csv)", nrow(typed)),
      stringsAsFactors = FALSE)
  )
  shard_rules <- shard_rules[!duplicated(shard_rules[c("variable", "value")]), , drop = FALSE]
  rownames(shard_rules) <- NULL
  cat(sprintf("qes2022 shard rules: %d (%d select-all items)\n", nrow(shard_rules), length(binary)))
} else {
  cat("qes2022 is not in the cache: shard_rules.csv is kept as it is.\n")
}

# ---- lints -------------------------------------------------------------------------------

lint <- function(v, values) {
  stopifnot(!anyDuplicated(v[c("study", "variable")]))
  stopifnot(!anyDuplicated(values[c("study", "variable", "value")]))
  stopifnot(all(paste(values$study, values$variable) %in% paste(v$study, v$variable)))
  stopifnot(all(v$label_source %in% enum("label_source")))
  stopifnot(all(values$label_source %in% enum("label_source")))
  stopifnot(all(values$missing_type[!is.na(values$missing_type)] %in% missing_types))
  stopifnot(all(v$measure %in% enum("measure")))
  stopifnot(all(values$label_flag[!is.na(values$label_flag)] %in% label_flags))
  stopifnot(!any(values$label_flag %in% "declared_substantive" & !is.na(values$missing_type)))
  # derived_from names variables of the same study
  for (i in which(!is.na(v$derived_from))) {
    stopifnot(all(.qes_split_list(v$derived_from[i]) %in% v$variable[v$study == v$study[i]]))
  }
  stopifnot(!any(v$study == "qes2022"), !any(values$study == "qes2022"))
}
lint(variables, values)
lint(demo$variables, demo$values)

# ---- write -------------------------------------------------------------------------------

targets <- list(
  list(variables, file.path(dict_dir, "variables.csv.gz")),
  list(values, file.path(dict_dir, "values.csv.gz")),
  list(demo$variables, file.path(demo_dir, "variables.csv.gz")),
  list(demo$values, file.path(demo_dir, "values.csv.gz"))
)
if (!is.null(shard_rules)) {
  targets[[length(targets) + 1L]] <- list(shard_rules, rules_path)
}

if (check_only) {
  bad <- character(0)
  for (t in targets) {
    tmp <- tempfile(fileext = if (grepl("\\.gz$", t[[2]])) ".csv.gz" else ".csv")
    .qes_write_csv(t[[1]], tmp)
    read_text <- function(p) paste(readLines(p, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
    if (!file.exists(t[[2]]) || !identical(read_text(tmp), read_text(t[[2]]))) {
      bad <- c(bad, t[[2]])
    }
  }
  if (length(bad) > 0L) {
    stop(sprintf("Out of date: %s. Re-run without --check.", paste(bad, collapse = ", ")), call. = FALSE)
  }
  cat("The dictionary matches the pinned files and the curated tables.\n")
  quit(save = "no", status = 0)
}

dir.create(dict_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(demo_dir, recursive = TRUE, showWarnings = FALSE)
for (t in targets) {
  .qes_write_csv(t[[1]], t[[2]])
}
source(file.path(root, "data-raw", "versions.R"))
write_versions(root)
sizes <- file.size(vapply(targets, `[[`, "", 2L))
cat(sprintf("Wrote %d variables and %d values of %d studies (%s bytes); VERSIONS updated.\n",
  nrow(variables), nrow(values), length(shipped), format(sum(sizes), big.mark = ",")))
