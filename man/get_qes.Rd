\name{get_qes}
\alias{get_qes}
\title{Download and Load a Quebec Election Study}
\description{Downloads a study file from Dataverse, applies labels when available, and attaches a codebook.}
\usage{
get_qes(srvy, file = NULL, assign_global = TRUE, with_codebook = TRUE, quiet = FALSE)
}
\arguments{
  \item{srvy}{A qesR survey code from \code{get_qescodes()}.}
  \item{file}{Optional regular expression for choosing one file in multi-file datasets.}
  \item{assign_global}{If TRUE, assign the result into the global environment using \code{srvy}.}
  \item{with_codebook}{If TRUE, attach codebook metadata and assign \code{<srvy>_codebook} when \code{assign_global = TRUE}.}
  \item{quiet}{If TRUE, suppress informational output.}
}
\value{A labelled data frame/tibble for the selected survey.}
