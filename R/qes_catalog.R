.qes_catalog <- local({
  catalog <- data.frame(
    index = 1:11,
    qes_survey_code = c(
      "qes2022",
      "qes2018",
      "qes2018_panel",
      "qes2014",
      "qes2012",
      "qes2012_panel",
      "qes_crop_2007_2010",
      "qes2008",
      "qes2007",
      "qes2007_panel",
      "qes1998"
    ),
    year = c(
      "2022",
      "2018",
      "2018",
      "2014",
      "2012",
      "2012",
      "2007-2010",
      "2008",
      "2007",
      "2007",
      "1998"
    ),
    name_en = c(
      "Quebec Election Study 2022",
      "Quebec Election Study 2018",
      "Quebec Election Study 2018 Panel",
      "Quebec Election Study 2014",
      "Quebec Election Study 2012",
      "Quebec Election Study 2012 Panel",
      "CROP Quebec Opinion Polls (2007-2010)",
      "Quebec Election Study 2008",
      "Quebec Election Study 2007",
      "Quebec Election Study 2007 Panel",
      "Quebec Elections 1998"
    ),
    name_fr = c(
      "Etude electorale quebecoise 2022",
      "Etude electorale quebecoise 2018",
      "Panel de l'etude electorale quebecoise 2018",
      "Etude electorale quebecoise 2014",
      "Etude electorale quebecoise 2012",
      "Panel de l'etude electorale quebecoise 2012",
      "Sondages CROP (2007-2010)",
      "Etude electorale quebecoise 2008",
      "Etude electorale quebecoise 2007",
      "Panel de l'etude electorale quebecoise 2007",
      "Elections quebecoises de 1998"
    ),
    doi = c(
      "10.7910/DVN/PAQBDR",
      "10.5683/SP3/NWTGWS",
      "10.5683/SP3/XP3XVW",
      "10.5683/SP3/2SWEAF",
      "10.5683/SP3/UMFEAI",
      "10.5683/SP3/TMRQRE",
      "10.5683/SP3/O2PI1F",
      "10.5683/SP3/0GWQPY",
      "10.5683/SP3/OCD8PY",
      "10.5683/SP3/KNOAFD",
      "10.5683/SP3/TGWQXJ"
    ),
    server = c(
      "https://dataverse.harvard.edu",
      rep("https://borealisdata.ca", 10)
    ),
    allow_insecure_retry = c(TRUE, rep(FALSE, 10)),
    stringsAsFactors = FALSE
  )

  catalog$doi_url <- paste0("https://doi.org/", catalog$doi)

  catalog$documentation <- ifelse(
    catalog$server == "https://dataverse.harvard.edu",
    paste0("https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:", catalog$doi),
    paste0("https://borealisdata.ca/dataset.xhtml?persistentId=doi:", catalog$doi)
  )

  catalog
})

#' List Quebec Election Study survey codes
#'
#' Returns available qesR survey codes, with optional extended metadata.
#'
#' @param detailed If `TRUE`, include year, names, DOI, and documentation
#'   columns.
#' @return A data frame with qesR survey codes.
#' @export
get_qescodes <- function(detailed = FALSE) {
  out <- .qes_catalog[, c(
    "index",
    "qes_survey_code"
  )]
  out$get_qes_call_char <- sprintf('"%s"', out$qes_survey_code)
  out <- out[, c("index", "qes_survey_code", "get_qes_call_char")]

  if (isTRUE(detailed)) {
    extra <- .qes_catalog[, c("year", "name_en", "name_fr", "doi", "doi_url", "documentation")]
    out <- cbind(out, extra, stringsAsFactors = FALSE)
  }

  rownames(out) <- NULL
  out
}
