# What the website builds from the installed package (design.md section 9.1,
# slice W). The site itself is checked by data-raw/check_site.R, which needs
# the source tree; these tests guard the parts the site reads from the
# package: the function overviews of ?qesR and ?qesR-fr, and the catalog
# fields that the study catalog pages print for every study.

rd_text <- function(name) {
  rd <- qesR_rd_db()[[name]]
  expect_false(is.null(rd), info = name)
  paste(as.character(rd), collapse = "")
}

# The functions linked from the overview section of a help page (from its
# \section{<title>} to the next section).
overview_functions <- function(name, title) {
  text <- rd_text(name)
  start <- regexpr(sprintf("\\section{%s}", title), text, fixed = TRUE)
  expect_gt(start, 0L)
  text <- substring(text, start + 1L)
  end <- regexpr("\\section{", text, fixed = TRUE)
  if (end > 0L) text <- substring(text, 1L, end)
  sort(unique(regmatches(text, gregexpr("(?<=\\\\link\\[=)[a-z_]+(?=\\])", text, perl = TRUE))[[1]]))
}

test_that("?qesR and ?qesR-fr list the same functions in their overviews", {
  en <- overview_functions("qesR-package.Rd", "Functions")
  fr <- overview_functions("qesR-fr.Rd", "Fonctions")
  expect_identical(en, fr)
  canonical <- intersect(c(canonical_existing_exports, new_exports), getNamespaceExports("qesR"))
  expect_true(all(canonical %in% en), info = paste(setdiff(canonical, en), collapse = ", "))
})

# The groups of the reference index (design.md section 9.1, slice W.2): one
# section per @family, in this order, named alike in the two overviews.
site_groups <- data.frame(
  family = c("data", "studies and documents", "codebooks and search",
             "harmonization", "reproducibility", "cache", "legacy"),
  en = c("Data", "Studies and documents", "Codebooks and search",
         "Harmonization", "Reproducibility", "Cache", NA),
  fr = c("Données", "Études et documents", "Codebooks et recherche",
         "Harmonisation", "Reproductibilité", "Cache", NA),
  stringsAsFactors = FALSE
)

# The group given to each function in the overview table of a help page.
overview_groups <- function(name) {
  rows <- strsplit(rd_text(name), "\\cr", fixed = TRUE)[[1]]
  out <- character(0)
  for (row in rows[grepl("\\tab", rows, fixed = TRUE)]) {
    cells <- strsplit(row, "\\tab", fixed = TRUE)[[1]]
    group <- trimws(sub("^.*[{\n]", "", cells[1]))
    fns <- regmatches(cells[2], gregexpr("(?<=\\\\link\\[=)[a-z_]+(?=\\])", cells[2], perl = TRUE))[[1]]
    out[fns] <- as_utf8(group)
  }
  out
}

# The @family of each help page, from its \concept.
rd_family <- function(name) {
  rd <- qesR_rd_db()[[name]]
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), "")
  if (!any(tags == "\\concept")) return(NA_character_)
  trimws(paste(unlist(rd[[which(tags == "\\concept")[1]]]), collapse = ""))
}

test_that("every exported help page has a family, as the website reference groups them", {
  db <- qesR_rd_db()
  exported <- intersect(final_exports, getNamespaceExports("qesR"))
  aliases <- lapply(db, function(rd) {
    tags <- vapply(rd, function(x) attr(x, "Rd_tag"), "")
    vapply(rd[tags == "\\alias"], function(x) paste(unlist(x), collapse = ""), "")
  })
  for (f in exported) {
    page <- names(aliases)[vapply(aliases, function(a) f %in% a, logical(1))][1]
    expect_false(is.na(page), info = f)
    fam <- rd_family(page)
    expect_true(fam %in% site_groups$family, info = paste(f, page, fam))
    if (f %in% legacy_exports) expect_identical(fam, "legacy", info = f)
  }
  for (page in c("qesR-package.Rd", "qesR-fr.Rd", "qesR-deprecated.Rd")) {
    expect_true(is.na(rd_family(page)), info = page)
  }
})

test_that("the overviews group functions as the website reference does", {
  en <- rd_text("qesR-package.Rd")
  fr <- rd_text("qesR-fr.Rd")
  for (group in stats::na.omit(site_groups$en)) {
    expect_true(grepl(group, en, fixed = TRUE), info = group)
  }
  for (group in stats::na.omit(site_groups$fr)) {
    expect_true(grepl(group, as_utf8(fr), fixed = TRUE), info = group)
  }
  # each function is in the group of its family, in both overviews
  db <- qesR_rd_db()
  page_of <- function(f) {
    hit <- names(db)[vapply(db, function(rd) {
      tags <- vapply(rd, function(x) attr(x, "Rd_tag"), "")
      f %in% vapply(rd[tags == "\\alias"], function(x) paste(unlist(x), collapse = ""), "")
    }, logical(1))]
    hit[1]
  }
  for (lang in c("en", "fr")) {
    groups <- overview_groups(if (lang == "en") "qesR-package.Rd" else "qesR-fr.Rd")
    expect_gt(length(groups), 10L)
    for (f in names(groups)) {
      fam <- rd_family(page_of(f))
      expect_identical(unname(groups[f]), site_groups[[lang]][match(fam, site_groups$family)],
                       info = paste(lang, f))
    }
  }
})

test_that("the catalog gives every study what its catalog page prints", {
  studies <- qes_studies()
  cat <- shipped_catalog()
  files <- cat$files
  data_file <- files[match(paste(studies$study, studies$data_file_id),
                           paste(files$study, files$file_id)), ]
  expect_false(anyNA(data_file$file_id))
  for (col in c("original_file_name", "format", "n_rows", "n_cols", "md5")) {
    expect_false(anyNA(data_file[[col]]) || any(!nzchar(as.character(data_file[[col]]))), info = col)
  }
  for (col in c("title_en", "title_fr", "target_population_en", "target_population_fr",
                "doi_url", "publisher", "dataset_version", "licence", "licence_url")) {
    expect_false(anyNA(studies[[col]]) || any(!nzchar(studies[[col]])), info = col)
  }
  expect_false(anyNA(as.logical(studies$metadata_shipped)))
  # a note is given in both languages or in neither
  has_en <- !is.na(studies$notes_en) & nzchar(studies$notes_en)
  has_fr <- !is.na(studies$notes_fr) & nzchar(studies$notes_fr)
  expect_identical(has_en, has_fr)
})

test_that("every label a catalog page prints exists in English and French", {
  studies <- qes_studies()
  docs <- qes_docs()
  files <- shipped_catalog()$files
  enums <- shipped_catalog()$enums
  used <- list(
    family = studies$family,
    study_design = studies$study_design,
    licence = studies$licence,
    role = docs$role,
    lang = docs$lang[!is.na(docs$lang)],
    format = files$format[files$file_id %in% studies$data_file_id]
  )
  for (enum in names(used)) {
    e <- enums[enums$enum == enum, , drop = FALSE]
    values <- unique(used[[enum]])
    expect_true(all(values %in% e$value), info = enum)
    labels <- e[match(values, e$value), c("label_en", "label_fr")]
    expect_false(anyNA(labels) || any(!nzchar(unlist(labels))), info = enum)
  }
})

test_that("every document the catalog page links has a name and an https URL", {
  docs <- qes_docs()
  expect_gt(nrow(docs), 0L)
  expect_false(anyNA(docs$file_name) || any(!nzchar(docs$file_name)))
  expect_true(all(grepl("^https://", docs$url)))
})
