\name{get_preview}
\alias{get_preview}
\title{Preview a Quebec Election Study}
\description{Loads a study and returns the first observations.}
\usage{
get_preview(srvy, obs = 6L, file = NULL)
}
\arguments{
  \item{srvy}{A qesR survey code from \code{get_qescodes()}.}
  \item{obs}{Number of observations to return.}
  \item{file}{Optional regular expression for choosing one file in multi-file datasets.}
}
\value{A data frame/tibble preview (with attached codebook metadata).}
