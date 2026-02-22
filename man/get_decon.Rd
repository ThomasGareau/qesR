\name{get_decon}
\alias{get_decon}
\title{Create a Prepared Non-Exhaustive qesR Dataset}
\description{Builds a deconstructed teaching/testing dataset with standardized columns from a selected Quebec election study.}
\usage{
get_decon(srvy = "qes2022", assign_global = TRUE, quiet = FALSE)
}
\arguments{
  \item{srvy}{A qesR survey code. Defaults to \code{"qes2022"}.}
  \item{assign_global}{If TRUE, assign the result as \code{decon} in the global environment.}
  \item{quiet}{If TRUE, suppress informational output while downloading.}
}
\value{A data frame named \code{decon} when assigned globally.}
