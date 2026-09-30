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
#   - the navbar is English only: every entry is an English page (an
#     article with a French partner, the page it links to as its
#     translation, or a page of the reference or the changelog), and the
#     FR/EN button's script (template.includes.before_body) has a fallback
#     map to pages of the site;
#   - the section "En français" of the articles index lists the French home
#     (fr-accueil), then the partner of every page of the English sections,
#     in their order;
#   - the French home links to every French page and to ?qesR-fr.
# With a site directory (the output of pkgdown::build_site()) it also checks
# the built pages:
#   - every page named in the configuration was built, and the search index;
#   - every internal link and image resolves to a file of the site, and every
#     link to an anchor ("page.html#id") to an id on that page;
#   - every article links to its partner in the other language;
#   - on every page, the FR/EN button (as its script resolves it: the page's
#     "Version française" / "English version" link, else the fallback map,
#     else the other language's home) points to a page that was built;
#   - the tables of each EN/FR article pair have the same shape, and no cell
#     is NA on one page but not on its partner;
#   - no page but the changelog mentions the old 40,606-row master file, and
#     none reads a master file shipped in the package or the working
#     directory;
#   - no article prints a warning or a ggplot2 diagnostic ("#> Warning",
#     "#> `geom_...`");
#   - every redirect of the configuration (pages of an earlier site that
#     moved) was written, and points to a page that was built.
# It uses yaml and xml2, which pkgdown itself needs. Pairing of the Rmd
# sources and their identical code are checked by check_vignette_pairs.R.

args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args) >= 1L) args[[1]] else "."
site <- if (length(args) >= 2L) args[[2]] else NA_character_

`%||%` <- function(a, b) if (is.null(a)) b else a
`%|NA|%` <- function(a, b) if (length(a) == 0L || is.na(a[1])) b else a
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

# The navbar: English entries only, each an article with a French partner
# (or a page of the reference or the changelog).
nav_items <- unlist(lapply(config$navbar$components, function(comp) {
  if (!is.null(comp$menu)) lapply(comp$menu, `[[`, "href") else list(comp$href)
}))
nav_items <- nav_items[!is.na(nav_items) & !grepl("^https?://", nav_items)]
article_stems <- sub("^articles/", "", articles)
for (href in nav_items) {
  if (grepl("^articles/", href)) {
    stem <- page_of(href)
    if (!stem %in% article_stems) {
      problem("navbar: %s is not an article", href)
    } else if (startsWith(stem, "fr-")) {
      problem("navbar: %s is a French page; the navbar is English, the FR/EN button leads to French", href)
    } else {
      partner <- partner_of(stem)
      if (is.na(partner) || !partner %in% article_stems) {
        problem("navbar: %s has no French partner", href)
      }
    }
  } else if (!grepl("^(reference|news)/", href)) {
    problem("navbar: %s is not an article, a reference page or the changelog", href)
  }
}
for (name in names(config$navbar$components)) {
  if (grepl("_fr$|\\(FR\\)", paste(name, config$navbar$components[[name]]$text %||% ""))) {
    problem("navbar: the menu '%s' is French; the FR/EN button replaces the French menus", name)
  }
}

# The FR/EN button: its script's fallback map ("page": "partner") and the
# pages it counts as French besides articles/fr-*.
toggle_js <- config$template$includes$before_body %||% ""
toggle_map <- local({
  body <- regmatches(toggle_js, regexpr("var fallback = \\{[^}]*\\}", toggle_js))
  pairs <- regmatches(body, gregexpr('"[^"]+"\\s*:\\s*"[^"]+"', body))[[1]]
  stats::setNames(sub('^.*:\\s*"([^"]+)"$', "\\1", pairs), sub('^"([^"]+)".*$', "\\1", pairs))
})
toggle_french <- local({
  body <- regmatches(toggle_js, regexpr("var french = \\[[^]]*\\]", toggle_js))
  gsub('"', "", regmatches(body, gregexpr('"[^"]+"', body))[[1]])
})
if (!grepl("qesr-lang-switch", toggle_js, fixed = TRUE) || length(toggle_map) == 0L) {
  problem("template.includes.before_body: no FR/EN button script with a fallback map")
}
# pkgdown passes the script through the pandoc template of each article,
# where "$" starts a template variable
if (grepl("$", toggle_js, fixed = TRUE)) {
  problem("template.includes.before_body: the script has a \"$\", which breaks the pandoc template of the articles")
}
# the pages of the site the map may name: articles, the home page, the
# reference pages and the indexes
site_pages <- c("index.html", "news/index.html", "reference/index.html", "articles/index.html",
                paste0("reference/", topics, ".html"), paste0("articles/", article_stems, ".html"))
for (p in unique(c(names(toggle_map), toggle_map, "articles/fr-accueil.html"))) {
  if (!p %in% site_pages) problem("FR/EN button: its map names %s, which is not a page of the site", p)
}
if (!identical(unname(toggle_map["index.html"]), "articles/fr-accueil.html")) {
  problem("FR/EN button: the home page should switch to articles/fr-accueil.html")
}

# The articles index: every English page, section by section, then the
# section "En français" with the French home and the partners in the same
# order.
sections <- vapply(config$articles, `[[`, "", "title")
fr_section <- match("En français", sections)
if (is.na(fr_section)) {
  problem("articles index: no section 'En français'")
} else {
  en <- unlist(lapply(config$articles[-fr_section], `[[`, "contents"))
  fr <- unlist(config$articles[[fr_section]]$contents)
  en_fr <- en[startsWith(sub("^articles/", "", en), "fr-")]
  for (a in en_fr) problem("articles index: the French page %s is outside the section 'En français'", a)
  want <- c("fr-accueil", vapply(setdiff(en, en_fr), function(a) partner_of(sub("^articles/", "", a)) %|NA|% NA_character_, ""))
  got <- sub("^articles/", "", fr)
  if (!identical(unname(want), got)) {
    problem("articles index: 'En français' should list %s, in that order (got %s)",
            paste(want, collapse = ", "), paste(got, collapse = ", "))
  }
}

# The French home links to every French page and to ?qesR-fr.
home_fr <- file.path(root, "vignettes", "articles", "fr-accueil.Rmd")
if (!file.exists(home_fr)) {
  problem("articles: no French home page (vignettes/articles/fr-accueil.Rmd)")
} else {
  text <- paste(enc2utf8(readLines(home_fr, encoding = "UTF-8", warn = FALSE)), collapse = "\n")
  linked <- unique(sub("^\\]\\(([A-Za-z0-9._-]+)\\.html.*$", "\\1",
                       regmatches(text, gregexpr("\\]\\(([A-Za-z0-9._-]+)\\.html[^)]*\\)", text))[[1]]))
  for (a in setdiff(article_stems[startsWith(article_stems, "fr-")], c("fr-accueil", linked))) {
    problem("articles/fr-accueil.Rmd does not link to %s.html", a)
  }
  if (!grepl("](../reference/qesR-fr.html)", text, fixed = TRUE)) {
    problem("articles/fr-accueil.Rmd does not link to ../reference/qesR-fr.html")
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
    # an example that prints a warning or a ggplot2 diagnostic looks broken
    if (startsWith(file, "articles/") && grepl("#> (Warning|`geom_|`stat_)", text)) {
      problem("site: %s prints a warning or a ggplot2 diagnostic in its output", file)
    }
  }

  # the pages of an earlier site that moved keep a redirect to their new place
  for (r in config$redirects %||% list()) {
    if (!file.exists(file.path(site, r[[1]]))) problem("site: no redirect page %s (to %s)", r[[1]], r[[2]])
    if (!file.exists(file.path(site, r[[2]]))) problem("site: redirect %s points to %s, which was not built", r[[1]], r[[2]])
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
  # The FR/EN button on every page, resolved as its script does: the page's
  # own link to its translation, else the fallback map, else the other
  # language's home.
  n_toggle <- 0L
  for (file in html) {
    doc <- xml2::read_html(file.path(site, file))
    if (length(xml2::xml_find_all(doc, "//nav//a[contains(@class, 'navbar-brand')]")) == 0L) next
    n_toggle <- n_toggle + 1L
    french <- startsWith(file, "articles/fr-") || file %in% toggle_french
    want <- if (french) "English version" else "Version fran\u00e7aise"
    links <- xml2::xml_find_all(doc, "//main//a[@href]")
    text <- trimws(gsub("\\s+", " ", xml2::xml_text(links)))
    hit <- which(text == want)
    target <- if (length(hit)) {
      file.path(dirname(file), sub("[?#].*$", "", xml2::xml_attr(links[hit[1]], "href")))
    } else if (!is.na(toggle_map[file])) {
      toggle_map[[file]]
    } else if (french) "index.html" else "articles/fr-accueil.html"
    dest <- normalizePath(file.path(site, target), mustWork = FALSE)
    if (!file.exists(dest)) {
      problem("site: the FR/EN button of %s leads to %s, which was not built", file, target)
    } else if (startsWith(dest, site)) {
      rel <- substring(dest, nchar(site) + 2L)
      if ((startsWith(rel, "articles/fr-") || rel %in% toggle_french) == french) {
        problem("site: the FR/EN button of %s leads to %s, a page in the same language", file, rel)
      }
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
  cat(sprintf("Site: %d pages, %d internal links checked; FR/EN button of %d pages resolved; tables of %d EN/FR article pairs compared.\n",
              length(html), n_links, n_toggle, n_pairs))
}

if (length(problems)) {
  cat("Website:\n", paste0("- ", problems, collapse = "\n"), "\n", sep = "")
  quit(save = "no", status = 1L)
}
cat("Website: configuration and pages consistent; every help page and article indexed; navbar English, French pages paired and reachable.\n")
