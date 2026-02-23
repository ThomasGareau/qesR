\name{download_codebook}
\alias{download_codebook}
\title{Download Codebook Files}
\description{Downloads codebook/support files (PDFs, questionnaires, metadata files) into a local directory.}
\usage{
download_codebook(
  srvy,
  dest_dir = tempdir(),
  file = NULL,
  quiet = FALSE,
  refresh = FALSE,
  overwrite = FALSE
)
}
\arguments{
  \item{srvy}{A qesR survey code.}
  \item{dest_dir}{Directory where files should be downloaded.}
  \item{file}{Optional file selector passed to \code{get_codebook()}.}
  \item{quiet}{If TRUE, suppress informational output.}
  \item{refresh}{If TRUE, force a fresh codebook metadata download first.}
  \item{overwrite}{If TRUE, overwrite existing files in \code{dest_dir}.}
}
\value{A data frame containing source file metadata and local download paths.}
