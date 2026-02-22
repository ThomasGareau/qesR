\name{get_question}
\alias{get_question}
\title{Get Survey Question Text}
\description{Returns question text from variable labels or an attached/paired codebook, with optional full-question recovery.}
\usage{
get_question(do, q, full = TRUE)
}
\arguments{
  \item{do}{A data.frame or the name of one in the global environment.}
  \item{q}{Column name whose question text should be returned.}
  \item{full}{If TRUE, try to recover full question text when metadata appears truncated.}
}
\value{A character scalar with question text, or \code{NA_character_} when none exists.}
