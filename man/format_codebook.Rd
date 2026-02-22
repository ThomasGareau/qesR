\name{format_codebook}
\alias{format_codebook}
\title{Reformat a qesR Codebook}
\description{Reformats a \code{qes_codebook} object into compact, wide, or long layout.}
\usage{
format_codebook(codebook, layout = c("compact", "wide", "long"))
}
\arguments{
  \item{codebook}{A \code{qes_codebook} object from \code{get_codebook()}.}
  \item{layout}{One of \code{"compact"}, \code{"wide"}, or \code{"long"}.}
}
\value{A reformatted codebook data frame.}
