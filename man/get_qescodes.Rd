\name{get_qescodes}
\alias{get_qescodes}
\title{List Quebec Election Study Survey Codes}
\description{Returns a data frame of qesR survey call codes, with optional detailed metadata.}
\usage{
get_qescodes(detailed = FALSE)
}
\arguments{
  \item{detailed}{If TRUE, include year, names, DOI, and documentation columns.}
}
\value{A data frame of qesR survey codes (cesR-style by default).}
