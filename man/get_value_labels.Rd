\name{get_value_labels}
\alias{get_value_labels}
\title{Get Value Labels from a Codebook}
\description{Extracts value-label mappings from a \code{qes_codebook} object.}
\usage{
get_value_labels(codebook, variable = NULL, long = FALSE)
}
\arguments{
  \item{codebook}{A \code{qes_codebook} object from \code{get_codebook()}.}
  \item{variable}{Optional variable name. If \code{NULL}, returns mappings for all variables.}
  \item{long}{If TRUE, return a long data frame with \code{variable}, \code{value}, and \code{value_label} columns.}
}
\value{A named list of value-label vectors, or a long data frame when \code{long = TRUE}.}
