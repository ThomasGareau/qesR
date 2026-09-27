# Harmonization primitives shared by the spec validator and the engine
# (design.md sections 5.1, 5.4 and 5.6, slice HZ1).
#
#   .canon()          a source value as its canonical code text;
#   .qes_norm_label() the one label normalization (aliases, label hashes);
#   .qes_md5_text()   md5 of UTF-8 text (label hashes of CC BY-NC studies);
#   .qes_affine()     the affine grammar of `args`, parsed, never evaluated;
#   .qes_parse_*()    the `key=value;...` and `code=outcome;...` cells.
# Nothing here evaluates text as R code (design rule P2).

# ---- codes -------------------------------------------------------------------

# A source value as code text, the form used by valuemaps.csv, na_codes and
# gate_codes. unclass() comes before any NA test: haven's is.na() is TRUE for
# SPSS user-missing values (labelled_spss), whose codes must survive [A:N3].
# Numbers use .qes_code_chr(): whole numbers without decimals or exponent
# (1e5 is "100000", never "1e+05"), others with up to 15 significant digits.
# Text is trimmed and marked UTF-8. Factors give their level text. Missing
# values stay NA (the spec writes them as the token "NA"). A logical column
# (haven gives one for a column with no value) reads as 0/1.
.canon <- function(x) {
  if (is.factor(x)) {
    x <- as.character(x)
  }
  x <- unclass(x)
  attributes(x) <- NULL
  if (is.numeric(x) || is.logical(x)) {
    return(.qes_code_chr(as.numeric(x)))
  }
  out <- enc2utf8(trimws(as.character(x)))
  out[is.na(x)] <- NA_character_
  out
}

# Is `code` (text) in canonical form? Codes that read as numbers must be
# written as .canon() writes them ("1", not "01", "1.0" or "1e5"); other text
# must be trimmed. "NA" is the token for system missing.
.qes_is_canon_code <- function(code) {
  out <- !is.na(code) & nzchar(code) & code == trimws(code)
  num <- out & code != "NA" & grepl("^[-+]?[0-9.]+([eE][-+]?[0-9]+)?$", code)
  val <- suppressWarnings(as.numeric(code[num]))
  out[num] <- !is.na(val) & .qes_code_chr(val) == code[num]
  out
}

# ---- labels --------------------------------------------------------------------

# The one label normalization (design.md section 5.4): accents folded with the
# fixed code-point table of .qes_fold() (built from integers, so it works when
# a C locale is set inside the session, and never through iconv TRANSLIT),
# lower-cased, apostrophes unified and whitespace squished. Combining marks
# (U+0300 to U+036F) are dropped, so a decomposed (NFD) "e + acute" folds like
# the precomposed letter: this is the NFC step for the Latin letters used here.
.qes_norm_label <- function(x) {
  out <- .qes_fold(x)
  vapply(out, function(s) {
    if (is.na(s)) {
      return(NA_character_)
    }
    cp <- utf8ToInt(s)
    if (length(cp) == 0L || anyNA(cp)) {
      return(s)
    }
    keep <- !(cp >= 0x300L & cp <= 0x36FL)
    if (all(keep)) s else .squish_ws(intToUtf8(cp[keep]))
  }, character(1), USE.NAMES = FALSE)
}

# md5 of the UTF-8 bytes of each string (no newline added). tools::md5sum()
# reads files, so each string goes through a temporary file.
.qes_md5_text <- function(x) {
  x <- enc2utf8(as.character(x))
  out <- rep(NA_character_, length(x))
  ok <- !is.na(x)
  if (!any(ok)) {
    return(out)
  }
  paths <- vapply(which(ok), function(i) tempfile("qesR-md5-"), character(1))
  on.exit(unlink(paths), add = TRUE)
  for (k in seq_along(paths)) {
    writeBin(charToRaw(x[which(ok)[k]]), paths[k])
  }
  out[ok] <- unname(tools::md5sum(paths))
  out
}

# ---- rule arguments ------------------------------------------------------------

# The affine grammar of `args` (design.md section 5.6): "x", "x-1", "0.5*x+2".
# The text is matched by one regular expression and its numbers are read; it
# is never evaluated. Returns c(a, b) for a*x + b, or NULL when the text is not
# in the grammar.
.qes_affine_pattern <- "^(?:(-?[0-9]+(?:\\.[0-9]+)?)\\*)?x(?:([+-])([0-9]+(?:\\.[0-9]+)?))?$"

.qes_affine <- function(text) {
  if (!is.character(text) || length(text) != 1L || is.na(text)) {
    return(NULL)
  }
  m <- regmatches(text, regexec(.qes_affine_pattern, text, perl = TRUE))[[1]]
  if (length(m) == 0L) {
    return(NULL)
  }
  a <- if (nzchar(m[2])) as.numeric(m[2]) else 1
  b <- if (nzchar(m[4])) as.numeric(m[4]) * (if (m[3] == "-") -1 else 1) else 0
  c(a = a, b = b)
}

# "key=value;key=value" as a named character vector. Returns NULL when a part
# has no "=" or an empty key, or a key repeats.
.qes_parse_kv <- function(text) {
  if (length(text) != 1L || is.na(text) || !nzchar(text)) {
    return(stats::setNames(character(0), character(0)))
  }
  parts <- strsplit(text, ";", fixed = TRUE)[[1]]
  if (!all(grepl("^[^=]+=", parts))) {
    return(NULL)
  }
  keys <- sub("=.*$", "", parts)
  if (anyDuplicated(keys) > 0L) {
    return(NULL)
  }
  stats::setNames(sub("^[^=]+=", "", parts), keys)
}

# The keys `args` may hold, and the rules they belong to.
.qes_arg_keys <- list(
  numeric = c("min", "max", "affine", "from_label", "nonmonotone"),
  map = c("from_label", "nonmonotone"),
  date = "format",
  weight = character(0),
  string = "from_label",
  constant = "value",
  none = character(0)
)
