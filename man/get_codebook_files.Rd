\name{get_codebook_files}
\alias{get_codebook_files}
\title{Get Codebook Files}
\description{Returns codebook/support files (PDFs, questionnaires, metadata files) associated with a study.}
\usage{
get_codebook_files(srvy = NULL, codebook = NULL, file = NULL, quiet = FALSE, refresh = FALSE)
}
\arguments{
  \item{srvy}{A qesR survey code. Required if \code{codebook} is NULL.}
  \item{codebook}{A \code{qes_codebook} object.}
  \item{file}{Optional file selector passed to \code{get_codebook()} when \code{codebook} is not provided.}
  \item{quiet}{If TRUE, suppress informational output when downloading.}
  \item{refresh}{If TRUE, force a fresh codebook download when \code{codebook} is not provided.}
}
\value{A data frame of codebook/support files.}
