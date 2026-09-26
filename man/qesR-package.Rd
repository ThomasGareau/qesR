\name{qesR-package}
\alias{qesR-package}
\title{qesR: Access Quebec Election Study Datasets}
\description{
The \code{qesR} package provides survey-code based access to Quebec election
study datasets available via Dataverse repositories.
}
\docType{package}
\examples{
get_qescodes()
\donttest{
  qes2022 <- get_qes("qes2022")
}
}
