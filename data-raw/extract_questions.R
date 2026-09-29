# Draft question wording for data-raw/questions/<study>.csv from the deposited
# questionnaires (design.md section 6.1, curation phase b).
#
# Usage (from the package root):
#   QESR_DOCS_TXT_DIR=<dir> QESR_CACHE_DIR=<dir> Rscript data-raw/extract_questions.R [<study> ...]
#
# With study codes, only those studies are drafted (default: every study
# below). <dir> of QESR_DOCS_TXT_DIR holds plain-text copies of the deposited
# documents, one per file, named "<study>__<file_id>__<anything>.txt". They
# were made with macOS `textutil -convert txt` (.doc, .docx) and, for the
# qes2022 codebook (.pdf), with `pdftotext -layout` (poppler), from the
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

run <- commandArgs(trailingOnly = TRUE)
if (length(run) == 0L) {
  run <- c("qes2012", "qes2014", "qes2018", "qes2007", "qes2008", "qes2022")
}
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

squish <- function(x) trimws(gsub("\\s+", " ", x))

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

for (study in intersect(names(sources), run)) {
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

if ("qes2018" %in% run) {
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
}

# ---- qes2022: question text and French answer labels from the codebook ----------------
#
# The 2022 codebook (file 7449514, a PDF) words each question twice, in
# English then in French, each time on a line that starts with the variable
# name ("cps_turnout The Quebec election is scheduled for ..."), followed by
# its answer options ("• Certain to vote (1)"). A grid words its stem once,
# under the grid's name, then lists its items ("- cps_leadertherm_1 Dominique
# Anglade"): an item's question is the stem followed by the item in brackets.
# A routing condition in English ("If cps_turnout = ..., display
# cps_votechoice1.") on the lines just above the English question drafts
# universe_en. Names inside a note ("Note: ... to pes_maileasy so ...") start
# no question. An option can wrap onto the next lines, lose its ")" ("Peu (3")
# or carry a footnote marker ("(1)1"). A typed-text box ("... (8):
# cps_votechoice1_8_TEXT") is worded as the question followed by its option
# in brackets. A name the codebook misspells ("cps__Duration_in_seconds_") is
# read under the file's name (qes2022_aliases). A text longer than 600
# characters (the consent letter) is cut at a word and marked
# question_truncated. This drafts
#   data-raw/questions/qes2022.csv         question_en, question_fr and
#                                          universe_en of each variable the
#                                          codebook words;
#   data-raw/questions/qes2022_values.csv  label_fr of each labelled code
#                                          whose English option in the
#                                          codebook is the file's own label
#                                          (the file's labels are English),
#                                          and missing_type not_selected for
#                                          the -99 of each "select all that
#                                          apply" item (a column holding
#                                          codes 1 and -99 only).
# As for the other studies, rows already in these files are never changed.

# the codebook's spelling = the file's name
qes2022_aliases <- c(cps__Duration_in_seconds_ = "cps_Duration__in_seconds_")

parse_qes2022_codebook <- function(lines, vars) {
  lines <- gsub(" ", " ", lines, fixed = TRUE)
  # names the codebook misspells, at the start of a line
  for (a in names(qes2022_aliases)) {
    lines <- sub(sprintf("^(\\s{0,2})%s\\b", a), sprintf("\\1%s", qes2022_aliases[[a]]), lines, perl = TRUE)
  }
  # page numbers (a number alone, far right) become page breaks
  lines[grepl("^\\s{20,}[0-9]{1,3}\\s*$", lines)] <- "\f"
  low <- tolower(vars)
  stems <- unique(sub("_[^_]+$", "", vars))
  m <- regmatches(lines, regexec("^\\s{0,2}([A-Za-z][A-Za-z0-9_]*)(?:\\s+(\\S.*))?$", lines, perl = TRUE))
  tag <- vapply(m, function(z) if (length(z) == 3L) z[2] else NA_character_, "")
  rest <- vapply(m, function(z) if (length(z) == 3L && nzchar(z[3])) z[3] else NA_character_, "")
  is_var <- !is.na(tag) & tolower(tag) %in% low
  is_stem <- !is.na(tag) & !is_var & tag %in% stems & !tag %in% c("If", "Note")
  starts <- which(is_var | is_stem)
  blank <- function(x) !nzchar(trimws(x)) | x == "\f"
  # lines of one text joined with a space, except after a line that ends in
  # a hyphen after a letter ("ci-" / "dessous" gives "ci-dessous")
  join_lines <- function(x) {
    x <- squish(x)
    x <- x[nzchar(x)]
    if (length(x) == 0L) return("")
    sep <- ifelse(grepl("[[:alpha:]]-$", x[-length(x)]), "", " ")
    squish(paste0(c(x[1], paste0(sep, x[-1L])), collapse = ""))
  }
  para_start <- function(i) {
    p <- i
    while (p > 1L && !blank(lines[p - 1L])) p <- p - 1L
    p
  }
  # a name that continues a paragraph starts a question only after a routing
  # condition ("If ... display <name>.")
  starts <- starts[vapply(starts, function(i) {
    p <- para_start(i)
    p == i || grepl("^\\s*(If|Si) ", lines[p])
  }, logical(1))]
  bullet <- function(x) grepl("^\\s*(•|▼|-\\s+[A-Za-z][A-Za-z0-9_]*\\s|each coded)", x)
  stop_line <- function(x) grepl("^\\s*(If |Note:|Si |Remarque)", x)
  fr_words <- c("vous", "votre", "vos", "les", "des", "est", "une", "quel", "quelle", "pour", "dans",
                "avez", "êtes", "le", "la", "du", "de", "qui", "que", "sur", "au")
  en_words <- c("you", "your", "the", "of", "are", "is", "what", "which", "how", "in", "to", "do",
                "have", "for", "on", "and", "did", "would")
  lang_of <- function(txt) {
    w <- strsplit(tolower(gsub("[^[:alpha:]' ]", " ", txt)), "[ ']+")[[1]]
    accents <- lengths(regmatches(txt, gregexpr("[àâçèéêîôû]", txt)))
    if (sum(w %in% fr_words) + accents > sum(w %in% en_words)) "fr" else "en"
  }
  blocks <- list()
  for (k in seq_along(starts)) {
    i <- starts[k]
    end <- if (k < length(starts)) starts[k + 1L] - 1L else length(lines)
    body <- lines[i:end]
    stem <- if (is.na(rest[i])) character(0) else rest[i]
    j <- 2L
    while (j <= length(body)) {
      ln <- body[j]
      if (bullet(ln) || stop_line(ln)) break
      if (blank(ln)) {
        run <- j
        while (run <= length(body) && blank(body[run])) run <- run + 1L
        so_far <- squish(paste(stem, collapse = " "))
        more <- run <= length(body) && !bullet(body[run]) && !stop_line(body[run])
        # a sentence cut by a page break, or a name alone on its line whose
        # text starts after a blank line: go on
        if (more && ((any(body[j:(run - 1L)] == "\f") && nzchar(so_far) && !grepl("[?.!:)…]$", so_far)) ||
                     length(stem) == 0L)) {
          j <- run
          next
        }
        break
      }
      stem <- c(stem, ln)
      j <- j + 1L
    }
    stem <- join_lines(stem)
    items <- list()
    opts <- list()
    # a typed-text box, by its variable: the text of its option or item
    texts <- list()
    # an option ends in its code: "(2)", "(2" when the PDF lost the ")",
    # "(1)1" when a footnote marker follows, or "(7): pes_q8_7_TEXT"
    code_end <- "\\((-?[0-9]+)(?:\\)[0-9]{0,2})?\\s*(?::\\s*([A-Za-z0-9_]+_TEXT))?\\s*$"
    is_opt <- function(x) grepl("^\\s*\u2022", x)
    indent <- function(x) nchar(sub("\\S.*$", "", x))
    used <- integer(0)
    for (jj in seq_along(body)[-1L]) {
      if (jj %in% used) next
      ln <- body[jj]
      it <- regmatches(ln, regexec("^(\\s*)-\\s+([A-Za-z][A-Za-z0-9_]*)\\s+(.*\\S)\\s*$", ln))[[1]]
      if (length(it) == 4L) {
        # an item that wraps goes on on the next lines, indented deeper
        # than its dash ("- pes_voteoptions_5 Voter ... de votre" /
        # "circonscription")
        txt <- it[4]
        nx <- jj + 1L
        while (nx <= length(body) && !blank(body[nx]) && !bullet(body[nx]) && !stop_line(body[nx]) &&
               nchar(sub("\\S.*$", "", body[nx])) > nchar(it[2])) {
          txt <- c(txt, body[nx])
          nx <- nx + 1L
        }
        txt <- join_lines(txt)
        box <- regmatches(txt, regexec(":\\s*([A-Za-z0-9_]+_TEXT)\\s*$", txt))[[1]]
        items[[it[3]]] <- sub("\\s*:\\s*[A-Za-z0-9_]+_TEXT\\s*$", "", txt)
        if (length(box) == 2L) texts[[box[2]]] <- items[[it[3]]]
        next
      }
      if (is_opt(ln) && !grepl(code_end, ln)) {
        # an option that wraps goes on on the next lines, indented deeper
        # than its bullet ("\u2022 I consent ... about the" / "study. (1)")
        nx <- jj + 1L
        while (nx <= length(body) && nx <= jj + 3L && !blank(body[nx]) && !is_opt(body[nx]) &&
               !stop_line(body[nx]) && indent(body[nx]) > indent(ln)) {
          ln <- paste(ln, trimws(body[nx]))
          used <- c(used, nx)
          if (grepl(code_end, body[nx])) break
          nx <- nx + 1L
        }
      } else if (!is_opt(ln) && !blank(ln) && !stop_line(ln) && jj > 2L && is_opt(body[jj - 1L]) &&
                 grepl(code_end, body[jj - 1L]) && indent(ln) >= indent(body[jj - 1L]) &&
                 grepl(code_end, ln)) {
        # an option whose bullet the PDF lost, right under a complete option
        # ("Je ne suis pas certain(e) (5)" under "\u2022 Tr\u00e8s difficile (4)")
        ln <- paste("\u2022", trimws(ln))
      }
      op <- regmatches(ln, regexec(paste0("^\\s*\u2022\\s*(.*\\S)\\s*", code_end), ln, perl = TRUE))[[1]]
      # a slider's ends ("No interest at all (0) - A great deal of interest (10)") are no options
      if (length(op) == 4L && !grepl("\\([0-9]+\\)\\s*-\\s", op[2])) {
        opts[[op[3]]] <- squish(op[2])
        if (nzchar(op[4])) texts[[op[4]]] <- sub("\\s*:$", "", squish(op[2]))
      }
    }
    p <- para_start(i)
    routing <- if (p < i) squish(paste(lines[p:(i - 1L)], collapse = " ")) else ""
    blocks[[length(blocks) + 1L]] <- list(
      tag = tag[i], stem = stem, items = items, opts = opts, texts = texts, routing = routing,
      lang = lang_of(paste(stem, paste(unlist(items), collapse = " ")))
    )
  }
  # a name worded twice: English first, then French (the codebook's order)
  tags <- vapply(blocks, `[[`, "", "tag")
  worded <- vapply(blocks, function(b) nzchar(b$stem), logical(1))
  for (t in unique(tags)) {
    k <- which(tags == t & worded)
    if (length(k) == 2L) {
      blocks[[k[1]]]$lang <- "en"
      blocks[[k[2]]]$lang <- "fr"
    }
  }
  blocks[worded]
}

if ("qes2022" %in% run) {
  cb_id <- "7449514"
  data <- .qes_read("qes2022")
  vars <- names(data)
  blocks <- parse_qes2022_codebook(read_txt("qes2022", cb_id), vars)
  cap <- function(x) {
    if (nchar(x) <= 600L) return(list(text = x, cut = FALSE))
    list(text = paste0(sub("\\s+\\S*$", "", substr(x, 1L, 600L)), " …"), cut = TRUE)
  }
  # per variable and language: text, universe, answer options, and for an
  # item of a grid, its own text (the label of code 1 of a select-all item)
  found <- list()
  put <- function(v, lang, text, block, item = NULL) {
    if (!is.null(found[[v]][[lang]])) return(invisible())
    found[[v]][[lang]] <<- list(text = text, tag = block$tag, ref = paste0(cb_id, ":", block$tag),
                                opts = block$opts, item = item, routing = block$routing)
  }
  for (b in blocks) {
    for (iv in names(b$items)) {
      v <- vars[tolower(vars) == tolower(iv)]
      if (length(v) == 1L) put(v, b$lang, sprintf("%s [%s]", b$stem, b$items[[iv]]), b, item = b$items[[iv]])
    }
    v <- vars[tolower(vars) == tolower(b$tag)]
    if (length(v) == 1L) put(v, b$lang, b$stem, b)
    # a typed-text box is worded as the question and its option, like a
    # grid item: "Which party ...? [Another party (please specify)]"
    for (tv in names(b$texts)) {
      v <- vars[tolower(vars) == tolower(tv)]
      if (length(v) == 1L) put(v, b$lang, sprintf("%s [%s]", b$stem, b$texts[[tv]]), b)
    }
  }

  path <- file.path(q_dir, "qes2022.csv")
  cur <- read_curated(path, cols)
  new_vars <- setdiff(vars[vars %in% names(found)], cur$variable)
  add <- lapply(new_vars, function(v) {
    en <- found[[v]]$en
    fr <- found[[v]]$fr
    en_t <- if (is.null(en)) list(text = "", cut = FALSE) else cap(en$text)
    fr_t <- if (is.null(fr)) list(text = "", cut = FALSE) else cap(fr$text)
    universe <- if (!is.null(en) && grepl(sprintf("display,?\\s+%s\\b", en$tag), en$routing, ignore.case = TRUE)) en$routing else ""
    data.frame(
      variable = v, label = "", question_en = en_t$text, question_fr = fr_t$text,
      question_truncated = if (en_t$cut || fr_t$cut) "TRUE" else "",
      universe_en = universe, universe_fr = "", question_source = "codebook",
      doc_ref = paste(unique(c(en$ref, fr$ref)), collapse = ";"),
      measure = "", var_timing = "", derived_from = "", reviewed = "FALSE",
      notes = sprintf("drafted from the codebook (%s) by data-raw/extract_questions.R", cb_id),
      stringsAsFactors = FALSE
    )
  })
  if (length(add) > 0L) {
    cur <- rbind(cur[, cols, drop = FALSE], do.call(rbind, add))
  }
  cur <- cur[order(match(cur$variable, vars)), cols, drop = FALSE]
  write_curated(cur, path)
  cat(sprintf("qes2022: %d drafted, %d rows in %s\n", length(new_vars), nrow(cur), path))

  vcols <- c("variable", "value", "label", "label_lang", "label_en", "label_fr",
             "missing_type", "label_flag", "notes")
  vpath <- file.path(q_dir, "qes2022_values.csv")
  vcur <- read_curated(vpath, vcols)
  rows <- list()
  for (v in vars) {
    x <- data[[v]]
    if (!is.numeric(unclass(x))) next
    labels <- attr(x, "labels", exact = TRUE)
    codes <- .qes_code_chr(unclass(unname(labels)))
    file_lab <- .squish_ws(names(labels))
    observed <- unique(stats::na.omit(.qes_code_chr(.qes_plain(x))))
    select_all <- "-99" %in% observed && all(observed %in% c("-99", "1"))
    en <- found[[v]]$en
    fr <- found[[v]]$fr
    fr_of <- stats::setNames(rep(NA_character_, length(codes)), codes)
    if (!is.null(en) && !is.null(fr)) {
      for (k in seq_along(codes)) {
        cd <- codes[k]
        same <- function(a, b) !is.null(a) && !is.null(b) && identical(.qes_norm_label(a), .qes_norm_label(b))
        if (same(en$opts[[cd]], file_lab[k]) && !is.null(fr$opts[[cd]])) {
          fr_of[cd] <- fr$opts[[cd]]
        } else if (cd == "1" && same(en$item, file_lab[k]) && !is.null(fr$item)) {
          fr_of[cd] <- fr$item
        }
      }
    }
    for (cd in union(codes[!is.na(fr_of)], if (select_all) "-99")) {
      rows[[length(rows) + 1L]] <- data.frame(
        variable = v, value = cd, label = "", label_lang = "", label_en = "",
        label_fr = if (cd %in% names(fr_of) && !is.na(fr_of[cd])) fr_of[cd] else "",
        missing_type = if (select_all && cd == "-99") "not_selected" else "",
        label_flag = "",
        notes = paste(c(if (!is.na(fr_of[cd] %||% NA)) sprintf("label_fr drafted from the codebook (%s) by data-raw/extract_questions.R", cb_id),
                        if (select_all && cd == "-99") "select-all item: codes 1 and -99 only"), collapse = "; "),
        stringsAsFactors = FALSE
      )
    }
  }
  drafted <- if (length(rows)) do.call(rbind, rows) else vcur[0, ]
  drafted <- drafted[!paste(drafted$variable, drafted$value) %in% paste(vcur$variable, vcur$value), , drop = FALSE]
  vcur <- rbind(vcur[, vcols, drop = FALSE], drafted[, vcols, drop = FALSE])
  vcur <- vcur[order(match(vcur$variable, vars), suppressWarnings(as.numeric(vcur$value))), vcols, drop = FALSE]
  write_curated(vcur, vpath)
  cat(sprintf("qes2022 values: %d rows drafted, %d rows in %s\n", nrow(drafted), nrow(vcur), vpath))
}
