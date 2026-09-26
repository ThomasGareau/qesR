# Gate check for slice S0a (roxygen2 migration), design.md section 11.
#
# Compares the roxygen-generated NAMESPACE and man/*.Rd in a package tree
# against the hand-written versions at a git ref (default a52cd9f, the last
# commit before the migration). The gate is:
#   1. NAMESPACE declares the same exports, imports and S3 methods;
#   2. the same set of Rd topics and aliases exists;
#   3. every \usage entry parses to the same calls (same arguments, same
#      defaults, same order); roxygen's line wrapping is ignored.
# It also reports, without failing, any whitespace-normalised difference in
# title, description, arguments, value and examples.
#
# Usage (from a scratch copy that has been roxygenised; never build in the repo):
#   Rscript dev/check_roxygen_migration.R <pkg_dir> [<repo_dir>] [<git_ref>]

args <- commandArgs(trailingOnly = TRUE)
pkg_dir <- if (length(args) >= 1L) args[[1L]] else "."
repo_dir <- if (length(args) >= 2L) args[[2L]] else pkg_dir
ref <- if (length(args) >= 3L) args[[3L]] else "a52cd9f"

git_show <- function(path) {
  out <- suppressWarnings(system2(
    "git",
    c("-C", shQuote(repo_dir), "-c", "core.fileMode=false", "show",
      shQuote(paste0(ref, ":", path))),
    stdout = TRUE, stderr = FALSE
  ))
  if (!is.null(attr(out, "status"))) stop("git show failed for ", path)
  out
}

git_ls <- function(path) {
  suppressWarnings(system2(
    "git",
    c("-C", shQuote(repo_dir), "ls-tree", "--name-only", ref, paste0(path, "/")),
    stdout = TRUE, stderr = FALSE
  ))
}

old_dir <- tempfile("s0a_old_")
dir.create(file.path(old_dir, "man"), recursive = TRUE)
writeLines(git_show("NAMESPACE"), file.path(old_dir, "NAMESPACE"))
old_rd <- grep("\\.Rd$", git_ls("man"), value = TRUE)
for (f in old_rd) writeLines(git_show(f), file.path(old_dir, f))

failures <- character(0)
fail <- function(...) failures <<- c(failures, paste0(...))

# 1. NAMESPACE --------------------------------------------------------------
ns_norm <- function(dir) {
  ns <- parseNamespaceFile(basename(dir), dirname(dir))
  list(
    exports = sort(ns$exports),
    exportPatterns = sort(ns$exportPatterns),
    imports = ns$imports,
    importClasses = ns$importClasses,
    importMethods = ns$importMethods,
    S3methods = ns$S3methods[order(ns$S3methods[, 1], ns$S3methods[, 2]), , drop = FALSE],
    dynlibs = ns$dynlibs
  )
}
# parseNamespaceFile wants <lib>/<pkg>/NAMESPACE
stage <- function(src) {
  d <- file.path(tempfile("ns_"), "qesR")
  dir.create(d, recursive = TRUE)
  file.copy(file.path(src, "NAMESPACE"), file.path(d, "NAMESPACE"))
  d
}
if (!identical(ns_norm(stage(old_dir)), ns_norm(stage(pkg_dir)))) {
  fail("NAMESPACE directives differ")
}

# 2 and 3. Rd ---------------------------------------------------------------
rd_parts <- function(file) {
  rd <- tools::parse_Rd(file)
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
  txt <- function(tag) {
    hits <- rd[tags == tag]
    if (!length(hits)) return(NA_character_)
    # Compare what the reader sees: render the section to plain text.
    frag <- structure(hits, class = "Rd", Rd_tag = "Rd")
    s <- utils::capture.output(tools::Rd2txt(frag, out = "", fragment = TRUE))
    trimws(gsub("[[:space:]]+", " ", paste(s, collapse = " ")))
  }
  usage_src <- if (any(tags == "\\usage")) {
    paste(as.character(rd[[which(tags == "\\usage")]]), collapse = "")
  } else ""
  usage_calls <- if (nzchar(trimws(usage_src))) {
    vapply(as.list(parse(text = usage_src, keep.source = FALSE)),
           function(e) paste(deparse(e, width.cutoff = 500L), collapse = ""), "")
  } else character(0)
  list(
    name = txt("\\name"),
    aliases = sort(vapply(rd[tags == "\\alias"], function(a) paste(as.character(a), collapse = ""), "")),
    usage = usage_calls,
    title = txt("\\title"),
    description = txt("\\description"),
    arguments = txt("\\arguments"),
    value = txt("\\value"),
    examples = txt("\\examples")
  )
}

new_rd <- file.path("man", list.files(file.path(pkg_dir, "man"), pattern = "\\.Rd$"))
if (!setequal(old_rd, new_rd)) {
  fail("Rd file set differs: removed {", paste(setdiff(old_rd, new_rd), collapse = ", "),
       "} added {", paste(setdiff(new_rd, old_rd), collapse = ", "), "}")
}

notes <- character(0)
for (f in intersect(old_rd, new_rd)) {
  a <- rd_parts(file.path(old_dir, f))
  b <- rd_parts(file.path(pkg_dir, f))
  if (!identical(a$usage, b$usage)) fail(f, ": \\usage differs")
  missing_alias <- setdiff(a$aliases, b$aliases)
  if (length(missing_alias)) fail(f, ": lost aliases ", paste(missing_alias, collapse = ", "))
  for (part in c("title", "description", "arguments", "value", "examples")) {
    if (!identical(a[[part]], b[[part]])) notes <- c(notes, paste0(f, ": ", part, " text differs"))
  }
}

cat("S0a roxygen migration check against", ref, "\n")
cat("Rd topics compared:", length(intersect(old_rd, new_rd)), "\n")
if (length(notes)) cat("Text differences (informational):\n", paste0("  ", notes, "\n"), sep = "")
if (length(failures)) {
  cat("FAIL:\n", paste0("  ", failures, "\n"), sep = "")
  quit(status = 1L)
}
cat("PASS: NAMESPACE identical, same topics, \\usage unchanged\n")
