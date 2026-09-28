#!/usr/bin/env Rscript

# Checks of the pkgdown website (design.md section 9.1, slice W).
#
# Usage, from the package root:
#   Rscript data-raw/check_site.R [<package root> [<built site directory>]]
#
# Without a site directory it checks the configuration only:
#   - pkgdown::check_pkgdown() passes;
#   - every help page is in the reference index, and every vignette or
#     article in the articles index;
#   - the "Guides" and "Guides (FR)" menus are parallel: same length, the same
#     separators and headings at the same places, and each French entry is the
#     partner of the English entry at its position (the page it links to as
#     its translation; qesR-package pairs with qesR-fr);
#   - the English and French sections of the articles index are parallel in
#     the same way.
# With a site directory (the output of pkgdown::build_site()) it also checks
# the built pages:
#   - every page named in the configuration was built, and the search index;
#   - every internal link and image resolves to a file of the site, and every
#     link to an anchor ("page.html#id") to an id on that page;
#   - every article links to its partner in the other language;
#   - the tables of each EN/FR article pair have the same shape, and no cell
#     is NA on one page but not on its partner;
#   - no page but the changelog mentions the old 40,606-row master file, and
#     none reads a master file shipped in the package or the working
#     directory.
# It uses yaml and xml2, which pkgdown itself needs. Pairing of the Rmd
# sources and their identical code are checked by check_vignette_pairs.R.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1]] else "."
site <- if (length(args) >= 2L) args[[2]] else NA_character_

`%||%` <- function(a, b) if (is.null(a)) b else a
problems <- character(0)
problem <- function(...) problems <<- c(problems, sprintf(...))

config <- yaml::read_yaml(file.path(root, "_pkgdown.yml"))

# ---- pkgdown's own check ---------------------------------------------------------

tryCatch(
  pkgdown::check_pkgdown(root),
  error = function(e) problem("pkgdown::check_pkgdown(): %s", conditionMessage(e))
)

# ---- reference and articles indexes ----------------------------------------------------

rd <- list.files(file.path(root, "man"), pattern = "^[^.].*\\.Rd$")
internal <- vapply(rd, function(f) {
  any(grepl("\\\\keyword\\{internal\\}", readLines(file.path(root, "man", f), warn = FALSE)))
}, logical(1))
topics <- sub("\\.Rd$", "", rd[!internal])
indexed <- unlist(lapply(config$reference, `[[`, "contents"))
for (t in setdiff(topics, indexed)) problem("reference index: help page %s is not listed", t)
for (t in setdiff(indexed, topics)) problem("reference index: %s is not a help page", t)

# One reference section per @family (design.md section 9.1): the pages of a
# family are all in one section, and a section holds one family. Only the
# package overviews and ?qesR-deprecated have no family.
concept_of <- vapply(topics, function(t) {
  lines <- readLines(file.path(root, "man", paste0(t, ".Rd")), warn = FALSE)
  hit <- sub("^\\\\concept\\{(.*)\\}$", "\\1", grep("^\\\\concept\\{", lines, value = TRUE))
  if (length(hit)) hit[[1]] else NA_character_
}, character(1))
no_family <- c("qesR-package", "qesR-fr", "qesR-deprecated")
for (t in setdiff(names(concept_of)[is.na(concept_of)], no_family)) {
  problem("reference index: help page %s has no @family", t)
}
section_of <- rep(vapply(config$reference, `[[`, "", "title"),
                  lengths(lapply(config$reference, `[[`, "contents")))
names(section_of) <- indexed
for (f in unique(stats::na.omit(concept_of))) {
  in_sections <- unique(section_of[intersect(names(concept_of)[concept_of %in% f], indexed)])
  if (length(in_sections) > 1L) {
    problem("reference index: family '%s' is split across sections %s", f,
            paste(in_sections, collapse = ", "))
  }
}
for (sec in config$reference) {
  fams <- unique(stats::na.omit(concept_of[intersect(unlist(sec$contents), names(concept_of))]))
  if (length(fams) > 1L) {
    problem("reference index: section '%s' mixes the families %s", sec$title,
            paste(fams, collapse = ", "))
  }
}

rmd_stems <- function(dir, prefix = "") {
  f <- list.files(file.path(root, dir), pattern = "^[^.].*\\.Rmd$")
  paste0(prefix, sub("\\.Rmd$", "", f))
}
articles <- c(rmd_stems("vignettes"), rmd_stems(file.path("vignettes", "articles"), "articles/"))
listed <- unlist(lapply(config$articles, `[[`, "contents"))
for (a in setdiff(articles, listed)) problem("articles index: %s is not listed", a)
for (a in setdiff(listed, articles)) problem("articles index: %s is not a vignette or article", a)

# ---- EN/FR parity of menus and article sections -----------------------------------------

# The partner of a page: the page its first lines link to as its translation.
partner_of <- function(stem) {
  base <- sub("^articles/", "", stem)
  path <- c(file.path(root, "vignettes", paste0(base, ".Rmd")),
            file.path(root, "vignettes", "articles", paste0(base, ".Rmd")))
  path <- path[file.exists(path)][1]
  if (is.na(path)) return(NA_character_)
  lines <- enc2utf8(readLines(path, n = 40L, encoding = "UTF-8", warn = FALSE))
  hit <- regmatches(lines, regexpr("\\[(English version|Version française)\\]\\(([^)]+)\\.html\\)", lines))
  if (!length(hit)) return(NA_character_)
  sub("^.*\\(([^)]+)\\.html\\).*$", "\\1", hit[[1]])
}
page_of <- function(href) sub("\\.html$", "", basename(href))
pair_of <- function(href) {
  if (grepl("^reference/", href)) {
    return(c("qesR-package" = "qesR-fr")[page_of(href)])
  }
  partner_of(page_of(href))
}

menu <- function(name) config$navbar$components[[name]]$menu
kind <- function(item) {
  if (!is.null(item$href)) "link" else if (grepl("^-+$", item$text)) "separator" else "heading"
}
en_menu <- menu("guides")
fr_menu <- menu("guides_fr")
if (length(en_menu) != length(fr_menu)) {
  problem("navbar: Guides has %d entries and Guides (FR) %d", length(en_menu), length(fr_menu))
} else {
  for (i in seq_along(en_menu)) {
    k_en <- kind(en_menu[[i]])
    k_fr <- kind(fr_menu[[i]])
    if (k_en != k_fr) {
      problem("navbar: entry %d is a %s in Guides but a %s in Guides (FR)", i, k_en, k_fr)
    } else if (k_en == "link") {
      want <- pair_of(en_menu[[i]]$href)
      got <- page_of(fr_menu[[i]]$href)
      if (is.na(want) || !identical(unname(want), got)) {
        problem("navbar: Guides entry %d (%s) is paired with %s in Guides (FR), not %s",
                i, en_menu[[i]]$href, got, want)
      }
    }
  }
}
for (href in c(vapply(en_menu, function(x) x$href %||% NA_character_, ""),
               vapply(fr_menu, function(x) x$href %||% NA_character_, ""))) {
  if (!is.na(href) && grepl("^articles/", href) && !page_of(href) %in% sub("^articles/", "", articles)) {
    problem("navbar: %s is not an article", href)
  }
}

sections <- vapply(config$articles, `[[`, "", "title")
en_sections <- c("Guides", "Harmonization (experimental)", "Examples")
fr_sections <- c("Guides en français", "Harmonisation en français (expérimental)",
                 "Exemples en français")
for (i in seq_along(en_sections)) {
  en <- config$articles[[match(en_sections[i], sections)]]$contents
  fr <- config$articles[[match(fr_sections[i], sections)]]$contents
  if (length(en) != length(fr)) {
    problem("articles index: '%s' has %d pages and '%s' %d", en_sections[i], length(en),
            fr_sections[i], length(fr))
    next
  }
  for (j in seq_along(en)) {
    want <- partner_of(en[[j]])
    if (!identical(want, sub("^articles/", "", fr[[j]]))) {
      problem("articles index: %s is paired with %s, not %s", en[[j]], fr[[j]], want)
    }
  }
}

# ---- the built site --------------------------------------------------------------------

if (!is.na(site)) {
  if (!dir.exists(site)) stop("no site directory ", site)
  site <- normalizePath(site)
  html <- list.files(site, pattern = "\\.html$", recursive = TRUE)
  html <- html[!grepl("(^|/)\\._", html)]

  expected <- c(
    "index.html", "news/index.html", "reference/index.html", "articles/index.html",
    paste0("reference/", topics, ".html"),
    paste0("articles/", sub("^articles/", "", articles), ".html")
  )
  for (f in setdiff(expected, html)) problem("site: %s was not built", f)
  if (!file.exists(file.path(site, "search.json"))) problem("site: no search index (search.json)")

  ids <- list()
  ids_of <- function(file) {
    if (is.null(ids[[file]])) {
      doc <- xml2::read_html(file.path(site, file))
      ids[[file]] <<- xml2::xml_attr(xml2::xml_find_all(doc, "//*[@id]"), "id")
    }
    ids[[file]]
  }
  n_links <- 0L
  for (file in html) {
    doc <- xml2::read_html(file.path(site, file))
    nodes <- xml2::xml_find_all(doc, "//a[@href] | //img[@src] | //link[@href] | //script[@src]")
    urls <- ifelse(is.na(xml2::xml_attr(nodes, "href")), xml2::xml_attr(nodes, "src"),
                   xml2::xml_attr(nodes, "href"))
    urls <- urls[!grepl("^([a-z][a-z0-9+.-]*:|//)", urls, ignore.case = TRUE) & nzchar(urls)]
    for (u in unique(urls)) {
      n_links <- n_links + 1L
      target <- sub("[?#].*$", "", u)
      anchor <- if (grepl("#", u)) sub("^[^#]*#", "", u) else NA_character_
      dest <- if (nzchar(target)) {
        normalizePath(file.path(site, dirname(file), target), mustWork = FALSE)
      } else {
        file.path(site, file)
      }
      if (dir.exists(dest)) dest <- file.path(dest, "index.html")
      if (!file.exists(dest)) {
        problem("site: %s links to %s, which does not exist", file, u)
        next
      }
      if (!is.na(anchor) && nzchar(anchor) && grepl("\\.html$", dest) &&
          startsWith(dest, site)) {
        rel <- substring(dest, nchar(site) + 2L)
        if (!utils::URLdecode(anchor) %in% ids_of(rel)) {
          problem("site: %s links to %s, but that page has no id '%s'", file, u, anchor)
        }
      }
    }

    text <- xml2::xml_text(doc)
    if (!startsWith(file, "news/") && grepl("40[,  ]?606", text)) {
      problem("site: %s mentions the old 40,606-row master file", file)
    }
    if (grepl("system.file\\(\"extdata\", \"qes_master|\\.\\./qes_master", text)) {
      problem("site: %s reads a master file from the package or the working directory", file)
    }
  }

  for (a in sub("^articles/", "", articles)) {
    file <- file.path(site, "articles", paste0(a, ".html"))
    if (!file.exists(file)) next
    partner <- partner_of(a)
    doc <- xml2::read_html(file)
    hrefs <- xml2::xml_attr(xml2::xml_find_all(doc, "//main//a[@href]"), "href")
    if (is.na(partner) || !paste0(partner, ".html") %in% hrefs) {
      problem("site: articles/%s.html has no link to its translation", a)
    }
  }
  # Content parity of the tables of each EN/FR pair: the pages run the same
  # code, so a cell that is NA on one page and not on its partner means the
  # data differ by language (e.g. a question documented in one language only).
  cells_of <- function(file) {
    doc <- xml2::read_html(file)
    lapply(xml2::xml_find_all(doc, "//main//table"), function(tb) {
      lapply(xml2::xml_find_all(tb, ".//tr"), function(tr) {
        trimws(xml2::xml_text(xml2::xml_find_all(tr, "./td")))
      })
    })
  }
  n_pairs <- 0L
  for (a in sub("^articles/", "", articles)) {
    partner <- partner_of(a)
    if (startsWith(a, "fr-") || is.na(partner)) next
    en_file <- file.path(site, "articles", paste0(a, ".html"))
    fr_file <- file.path(site, "articles", paste0(partner, ".html"))
    if (!file.exists(en_file) || !file.exists(fr_file)) next
    n_pairs <- n_pairs + 1L
    en <- cells_of(en_file)
    fr <- cells_of(fr_file)
    if (length(en) != length(fr)) {
      problem("site: articles/%s.html has %d tables and %s.html %d", a, length(en),
              partner, length(fr))
      next
    }
    for (k in seq_along(en)) {
      if (length(en[[k]]) != length(fr[[k]]) ||
          !identical(lengths(en[[k]]), lengths(fr[[k]]))) {
        problem("site: table %d of articles/%s.html and %s.html differ in shape", k, a, partner)
        next
      }
      for (i in seq_along(en[[k]])) {
        na_en <- en[[k]][[i]] == "NA"
        na_fr <- fr[[k]][[i]] == "NA"
        for (j in which(na_en != na_fr)) {
          problem("site: table %d, row %d, column %d is NA in %s.html but not in %s.html",
                  k, i, j, if (na_en[j]) a else partner, if (na_en[j]) partner else a)
        }
      }
    }
  }
  cat(sprintf("Site: %d pages, %d internal links checked; tables of %d EN/FR article pairs compared.\n",
              length(html), n_links, n_pairs))
}

if (length(problems)) {
  cat("Website:\n", paste0("- ", problems, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 1L)
}
cat("Website: configuration and pages consistent; every help page and article indexed; EN/FR menus and articles paired.\n")
