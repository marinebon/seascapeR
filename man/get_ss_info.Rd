% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read.R
\name{get_ss_info}
\alias{get_ss_info}
\title{Get Seascape dataset information}
\usage{
get_ss_info(dataset = "global_monthly")
}
\arguments{
\item{dataset}{\verb{\{region\}_\{frequency\}} of dataset. Valid values (so far): "global_8day" or "global_monthly" (default).}
}
\value{
ERDDAP \code{\link[rerddap]{info}} object
}
\description{
Get Seascape dataset information from ERDDAP server.
}
\examples{
get_ss_info() # default: dataset = "global_monthly"
get_ss_info("global_8day")
}
\concept{read}
