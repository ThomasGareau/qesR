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

test_that("the overviews group functions as the website reference does", {
  en <- rd_text("qesR-package.Rd")
  fr <- rd_text("qesR-fr.Rd")
  for (group in c("Discover", "Get data", "Metadata and search", "Reproducibility", "Cache")) {
    expect_true(grepl(group, en, fixed = TRUE), info = group)
  }
  for (group in c("Découvrir", "Obtenir les données", "Métadonnées et recherche",
                  "Reproductibilité", "Cache")) {
    expect_true(grepl(group, fr, fixed = TRUE), info = group)
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
