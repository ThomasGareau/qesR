# Sourced by every example page (website only; see _theme.R). Loads the
# packages, the language helpers, the chart system and the data helpers.

suppressPackageStartupMessages({
  library(qesR)
  library(ggplot2)
  library(survey)
})

# the language of the page, from its `params`
qz_lang <- if (exists("params") && identical(params$lang, "fr")) "fr" else "en"
tr <- function(en, fr) if (identical(qz_lang, "fr")) fr else en

# numbers in the page's language: 12.5 / 12,5; percentages 12.5% / 12,5 %
qz_num <- function(x, digits = 1) {
  out <- formatC(x, format = "f", digits = digits, decimal.mark = tr(".", ","))
  out[is.na(x)] <- ""
  out
}
qz_pct <- function(x, digits = 1) {
  out <- paste0(qz_num(x, digits), tr("%", " %"))
  out[is.na(x)] <- ""
  out
}
# a signed difference in points; rounded before the sign is chosen, so a
# difference that rounds to zero is "0 pts", never "−0 pts"
qz_pts <- function(x, digits = 0) {
  r <- round(x, digits)
  paste0(ifelse(r > 0, "+", ifelse(r < 0, "−", "")),
         formatC(abs(r), format = "f", digits = digits, decimal.mark = tr(".", ",")),
         tr(" pts", " pts"))
}

options(qesR.lang = qz_lang)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", message = FALSE, warning = FALSE)

source("_theme.R", encoding = "UTF-8")
source("_data.R", encoding = "UTF-8")
