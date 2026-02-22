\name{get_codebook}
\alias{get_codebook}
\title{Get a Quebec Election Study Codebook}
\description{Downloads and returns study codebook metadata; results are cached per session.}
\usage{
get_codebook(
  srvy,
  file = NULL,
  assign_global = FALSE,
  quiet = FALSE,
  refresh = FALSE,
  layout = c("compact", "wide", "long")
)
}
\arguments{
  \item{srvy}{A qesR survey code from \code{get_qescodes()}.}
  \item{file}{Optional regular expression for choosing one file in multi-file datasets.}
  \item{assign_global}{If TRUE, assign the codebook to \code{<srvy>_codebook} in the global environment.}
  \item{quiet}{If TRUE, suppress informational output.}
  \item{refresh}{If TRUE, force a fresh download instead of using cache.}
  \item{layout}{One of \code{"compact"}, \code{"wide"}, or \code{"long"}.}
}
\value{A \code{qes_codebook} data frame with variable metadata and codebook file manifest attributes.}
