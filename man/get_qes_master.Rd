\name{get_qes_master}
\alias{get_qes_master}
\title{Build a Harmonized Stacked Master QES Dataset}
\description{
Downloads multiple Quebec Election Study datasets, harmonizes shared variables,
stacks all rows into one master data frame, de-duplicates respondents within
the same survey code, and drops rows that are empty across harmonized variables.
}
\usage{
get_qes_master(
  surveys = NULL,
  assign_global = TRUE,
  object_name = "qes_master",
  quiet = FALSE,
  strict = FALSE,
  save_path = NULL
)
}
\arguments{
  \item{surveys}{
Character vector of qesR survey codes. Defaults to all studies from
\code{get_qescodes()}.
}
  \item{assign_global}{
If TRUE, assign the result to \code{.GlobalEnv} using \code{object_name}.
}
  \item{object_name}{
Object name used when \code{assign_global = TRUE}. Defaults to
\code{"qes_master"}.
}
  \item{quiet}{
If TRUE, suppress informational output while downloading.
}
  \item{strict}{
If TRUE, stop when any study fails. If FALSE, return partial results and
record failures in attributes.
}
  \item{save_path}{
Optional output path for writing the master file. Use \code{.rds} for RDS
output; otherwise CSV is written.
}
}
\value{
A harmonized stacked data frame with attributes:
\code{source_map}, \code{loaded_surveys}, \code{failed_surveys}, and
\code{harmonized_variables}. Additional attributes
\code{duplicates_removed} and \code{empty_rows_removed} report row filtering.
When \code{save_path} is provided, the output path is stored in \code{saved_to}.
}
