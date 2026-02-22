\name{get_qes_codebook}
\alias{get_qes_codebook}
\title{Alias for get_codebook}
\description{Backward-compatible alias for \code{get_codebook()}.}
\usage{
get_qes_codebook(
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
\value{A \code{qes_codebook} data frame.}
