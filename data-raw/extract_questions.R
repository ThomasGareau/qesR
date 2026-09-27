# Draft question wording for data-raw/questions/<study>.csv from the deposited
# questionnaires (design.md section 6.1, curation phase b).
#
# Usage (from the package root):
#   QESR_DOCS_TXT_DIR=<dir> QESR_CACHE_DIR=<dir> Rscript data-raw/extract_questions.R
#
# <dir> of QESR_DOCS_TXT_DIR holds plain-text copies of the deposited
# documents, one per file, named "<study>__<file_id>__<anything>.txt". They
# were made with macOS `textutil -convert txt` (.doc, .docx) from the
# md5-verified files listed by qes_docs(); this script makes no conversion and
# no network request. QESR_CACHE_DIR is a qesR download cache holding the
# pinned data files (read through qesR's own reader).
#
# The script only drafts. For each questionnaire it finds the question tags
# ("Q52:" on the question's own line, or "Q12A:" followed by the text on the
# next lines), keeps a tag only when it names a variable of the pinned data
# file exactly (ignoring case), and takes the question stem: the text up to
# the answer options, without interviewer notes. Grid items (a stem shared by
# q33a, q33b, ...) are not drafted. A drafted row is added to
# data-raw/questions/<study>.csv only when that variable has no row yet, and
# always with reviewed = FALSE; rows already in the file (reviewed or not)
# are never changed. The wording is then checked by hand against the
# document before a row is marked reviewed = TRUE.
#
# qes2018: the file has no value labels for most variables, so the French
# questionnaire "with programmed answer values" (file 367181), which lists
# each answer with its code, drafts data-raw/questions/qes2018_values.csv:
# only for variables whose file has no value labels, and only when every
# code observed in the data is among the drafted codes.

pkgload::load_all(".", quiet = TRUE, export_all = TRUE)

txt_dir <- Sys.getenv("QESR_DOCS_TXT_DIR")
cache_dir <- Sys.getenv("QESR_CACHE_DIR")
if (!nzchar(txt_dir) || !dir.exists(txt_dir)) {
  stop("Set QESR_DOCS_TXT_DIR to the directory of plain-text documents.", call. = FALSE)
}
if (nzchar(cache_dir)) {
  options(qesR.cache_dir = cache_dir, qesR.cache = "disk")
}
q_dir <- file.path("data-raw", "questions")
dir.create(q_dir, showWarnings = FALSE, recursive = TRUE)

squish <- function(x) gsub("\\s+", " ", trimws(x))

read_txt <- function(study, file_id) {
  f <- list.files(txt_dir, pattern = sprintf("^%s__%s__.*\\.txt$", study, file_id), full.names = TRUE)
  if (length(f) != 1L) {
    stop(sprintf("No text copy of %s file %s in %s.", study, file_id, txt_dir), call. = FALSE)
  }
  x <- readLines(f, warn = FALSE, encoding = "UTF-8")
  x <- gsub("\u00a0", " ", x, fixed = TRUE)
  x
}

# Lines that are instructions to the interviewer or programmer, not wording.
is_instruction <- function(x) {
  grepl(
    paste0(
      "^\\s*(\\(|\\[|NOTE|LIRE|READ|INTERVIEWER|INTERVIEWEUR|PROGRAMM|else\\b|if\\b|",
      "permutation|=>|ROTATE|RANDOM|ALTERNER|INSCRIRE|ACCEPT|NE PAS|DO NOT|SI\\s+Q)"
    ),
    x, ignore.case = FALSE
  ) || grepl("^[A-Z]\\) ", x) ||
    # programming notes of the CATI scripts (2007, 2008): routing, rotation,
    # numeric ranges ("$E 00 10") and eliminations
    grepl("->|\\$E\\b|#|^(si|if)\\b[^?]*\\bQ[0-9]|^(rotation|permutation)\\b|^selon Q[0-9]|do not read|ne pas lire|limination", x, ignore.case = TRUE)
}

# Format A: "Q52: How ...?" or "QSEXE: What ...?" on one line (2012, 2014 and
# 2018 questionnaires).
extract_same_line <- function(lines) {
  m <- regmatches(lines, regexec("^\\s*(Q[0-9A-Z][A-Za-z0-9_]*)\\s*:\\s*(\\S.*)$", lines))
  hits <- which(lengths(m) == 3L)
  data.frame(
    tag = vapply(m[hits], `[`, "", 2L),
    text = squish(vapply(m[hits], `[`, "", 3L)),
    item = hits,
    stringsAsFactors = FALSE
  )
}

# Format B: "Q12A:" alone, the question on the next lines, then answer lines
# ending in a code ("Oui<TAB>1") (2007 and 2008 questionnaires).
extract_next_lines <- function(lines) {
  tags <- which(grepl("^\\s*Q[0-9]+[A-Za-z]?:\\s*$", lines))
  out <- lapply(tags, function(i) {
    tag <- sub("^\\s*(Q[0-9]+[A-Za-z]?):.*$", "\\1", lines[i])
    body <- character(0)
    j <- i + 1L
    while (j <= length(lines) && j < i + 15L) {
      ln <- lines[j]
      if (grepl("\t[0-9]{1,3}(\t|\\s*$)", ln) || grepl("^\\s*Q[0-9]+[A-Za-z]?:", ln)) {
        break
      }
      if (nzchar(trimws(ln)) && !is_instruction(trimws(ln))) {
        body <- c(body, trimws(ln))
      }
      j <- j + 1L
    }
    data.frame(tag = tag, text = squish(paste(body, collapse = " ")), item = i, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# The questionnaires drafted, by study: file id, language and format.
sources <- list(
  qes2012 = list(c("196367", "en", "A"), c("196370", "fr", "A")),
  qes2014 = list(c("352010", "en", "A"), c("352009", "fr", "A")),
  qes2018 = list(c("361049", "en", "A"), c("361050", "fr", "A")),
  qes2007 = list(c("192423", "en", "B"), c("192422", "fr", "B")),
  qes2008 = list(c("196358", "fr", "B"))
)

cols <- c(
  "variable", "label", "question_en", "question_fr", "question_truncated",
  "universe_en", "universe_fr", "question_source", "doc_ref", "measure",
  "var_timing", "derived_from", "reviewed", "notes"
)

read_curated <- function(path, columns) {
  if (!file.exists(path)) {
    out <- as.data.frame(stats::setNames(replicate(length(columns), character(0), simplify = FALSE), columns))
    return(out)
  }
  utils::read.csv(path, colClasses = "character", na.strings = character(),
                  encoding = "UTF-8", check.names = FALSE, strip.white = FALSE)
}

write_curated <- function(x, path) {
  con <- file(path, open = "wb")
  on.exit(close(con))
  utils::write.csv(x, con, row.names = FALSE, na = "", fileEncoding = "UTF-8", eol = "\n")
}

for (study in names(sources)) {
  data <- .qes_read(study)
  vars <- names(data)
  drafts <- list()
  rejected <- character(0)
  for (src in sources[[study]]) {
    lines <- read_txt(study, src[1])
    found <- if (identical(src[3], "A")) extract_same_line(lines) else extract_next_lines(lines)
    found <- found[nzchar(found$text), , drop = FALSE]
    found$variable <- vars[match(tolower(found$tag), tolower(vars))]
    found <- found[!is.na(found$variable) & !duplicated(found$variable), , drop = FALSE]
    # a tag can name another item than the variable of the same name (the
    # 2012 grid q34b-d): where the file's own label is a question, keep the
    # draft only when the two share at least half of their words
    ok <- vapply(seq_len(nrow(found)), function(k) {
      lab <- attr(data[[found$variable[k]]], "label", exact = TRUE)
      if (!is.character(lab) || length(lab) != 1L || tolower(lab) == tolower(found$variable[k])) {
        return(TRUE)
      }
      same_lang <- (identical(src[2], "en") && identical(study, "qes2012")) ||
        (identical(src[2], "fr") && !identical(study, "qes2012"))
      if (!same_lang) {
        return(TRUE)
      }
      words <- function(x) {
        w <- strsplit(gsub("[^a-z0-9 ]", " ", .qes_fold(x)), " +")[[1]]
        unique(w[nchar(w) > 2L])
      }
      a <- words(found$text[k])
      b <- words(lab)
      length(a) == 0L || length(b) == 0L || length(intersect(a, b)) / min(length(a), length(b)) >= 0.5
    }, logical(1))
    if (any(!ok)) {
      cat(sprintf("%s %s: not drafted (the file label is another question): %s\n",
        study, src[1], paste(found$variable[!ok], collapse = ", ")))
    }
    rejected <- c(rejected, found$variable[!ok])
    found <- found[ok, , drop = FALSE]
    # a routing condition written before the question goes to the universe
    pre <- regmatches(found$text, regexec("^\\[([^]]+)\\]\\s*(.*)$", found$text))
    found$universe <- vapply(pre, function(m) if (length(m) == 3L) m[2] else "", "")
    found$text <- vapply(seq_along(pre), function(k) if (length(pre[[k]]) == 3L) pre[[k]][3] else found$text[k], "")
    found$lang <- src[2]
    found$file_id <- src[1]
    drafts[[length(drafts) + 1L]] <- found
  }
  drafts <- do.call(rbind, drafts)
  # a tag rejected in one language is dropped in the other too
  drafts <- drafts[!(drafts$variable %in% rejected), , drop = FALSE]
  path <- file.path(q_dir, paste0(study, ".csv"))
  cur <- read_curated(path, cols)
  new_vars <- setdiff(unique(drafts$variable), cur$variable)
  add <- lapply(new_vars, function(v) {
    d <- drafts[drafts$variable == v, , drop = FALSE]
    en <- d[d$lang == "en", , drop = FALSE]
    fr <- d[d$lang == "fr", , drop = FALSE]
    refs <- sprintf("%s:%s", d$file_id, d$tag)
    data.frame(
      variable = v, label = "",
      question_en = if (nrow(en)) en$text[1] else "",
      question_fr = if (nrow(fr)) fr$text[1] else "",
      question_truncated = "",
      universe_en = if (nrow(en)) en$universe[1] else "",
      universe_fr = if (nrow(fr)) fr$universe[1] else "",
      question_source = "questionnaire", doc_ref = paste(refs, collapse = ";"),
      measure = "", var_timing = "", derived_from = "", reviewed = "FALSE",
      notes = "drafted by data-raw/extract_questions.R",
      stringsAsFactors = FALSE
    )
  })
  if (length(add) > 0L) {
    cur <- rbind(cur[, cols, drop = FALSE], do.call(rbind, add))
  }
  cur <- cur[order(match(cur$variable, vars)), cols, drop = FALSE]
  write_curated(cur, path)
  cat(sprintf("%s: %d drafted, %d rows in %s\n", study, length(new_vars), nrow(cur), path))
}

# ---- qes2018 value labels from the programmed questionnaire --------------------------

lines <- read_txt("qes2018", "367181")
heads <- which(grepl("^[A-Za-z][A-Za-z0-9_]* - ", lines))
data <- .qes_read("qes2018")

# routing ("Q6 - Q6 - POSER SI Q5=...") fills the French universe of drafted
# rows that have none
qpath <- file.path(q_dir, "qes2018.csv")
q18 <- read_curated(qpath, cols)
for (i in heads) {
  parts <- strsplit(lines[i], " - ", fixed = TRUE)[[1]]
  cond <- trimws(parts[length(parts)])
  if (!grepl("^(POSER )?SI\\b", cond, ignore.case = TRUE)) {
    next
  }
  cond <- sub("^POSER ", "", cond, ignore.case = TRUE)
  cond <- paste0(toupper(substr(cond, 1, 1)), tolower(substr(cond, 2, 2)), substr(cond, 3, nchar(cond)))
  k <- which(tolower(q18$variable) == tolower(parts[1]))
  if (length(k) == 1L && !nzchar(q18$universe_fr[k]) && identical(q18$reviewed[k], "FALSE")) {
    q18$universe_fr[k] <- cond
  }
}
write_curated(q18, qpath)
vcols <- c("variable", "value", "label", "label_lang", "label_en", "label_fr",
           "missing_type", "label_flag", "notes")
vpath <- file.path(q_dir, "qes2018_values.csv")
vcur <- read_curated(vpath, vcols)
added <- 0L
for (k in seq_along(heads)) {
  i <- heads[k]
  end <- if (k < length(heads)) heads[k + 1L] - 1L else length(lines)
  tag <- sub(" - .*$", "", lines[i])
  v <- names(data)[tolower(names(data)) == tolower(tag)]
  if (length(v) != 1L || v %in% vcur$variable) {
    next
  }
  x <- data[[v]]
  if (length(attr(x, "labels", exact = TRUE)) > 0L || !is.numeric(x)) {
    next
  }
  block <- lines[(i + 1L):end]
  opt <- regmatches(block, regexec("^\\s*(?:\u2022\\s*)?(.*\\S)\\s*\\((-?[0-9]+)\\)(?:\\s*_+)?(?:\\s*\\[.*\\])?\\s*$", block, perl = TRUE))
  opt <- opt[lengths(opt) == 3L]
  if (length(opt) == 0L) {
    next
  }
  codes <- vapply(opt, `[`, "", 3L)
  labels <- squish(vapply(opt, `[`, "", 2L))
  keep <- !duplicated(codes)
  codes <- codes[keep]
  labels <- labels[keep]
  observed <- unique(stats::na.omit(as.numeric(unclass(x))))
  if (!all(observed %in% as.numeric(codes))) {
    cat(sprintf("qes2018 %s: observed codes not all in the questionnaire; skipped\n", v))
    next
  }
  vcur <- rbind(vcur[, vcols, drop = FALSE], data.frame(
    variable = v, value = codes, label = labels, label_lang = "fr", label_en = "",
    label_fr = labels, missing_type = "", label_flag = "",
    notes = "drafted from file 367181 by data-raw/extract_questions.R",
    stringsAsFactors = FALSE
  ))
  added <- added + 1L
}
vcur <- vcur[order(match(vcur$variable, names(data)), suppressWarnings(as.numeric(vcur$value))), vcols, drop = FALSE]
write_curated(vcur, vpath)
cat(sprintf("qes2018 values: %d variables drafted, %d rows in %s\n", added, nrow(vcur), vpath))
