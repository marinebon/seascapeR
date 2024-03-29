% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viz.R
\name{tbl_ss_class}
\alias{tbl_ss_class}
\title{Table of Seascape Class with plots per variable}
\usage{
tbl_ss_class(class, dir_svg, web_svg, tbl_classes = ss_gl_classes, ...)
}
\arguments{
\item{class}{integer identifier for Seascape Class}

\item{dir_svg}{local filesystem directory where to store SVG files}

\item{web_svg}{web prefix for referencing the SVG files}

\item{tbl_classes}{table of values from which to extract \code{var}; default =
\code{\link{ss_gl_classes}}}

\item{...}{other parameters to pass onto \code{\link{plot_ss_class_var}}}
}
\description{
Produce a table of variable average values for the Seascape Class with a
distribution plot relative to all Classes. This function wraps around
\code{\link{plot_ss_class_var}}.
}
\examples{
dir_svg  <- here::here("inst/svg")
web_svg  <- "../svg"
tbl_ss_class(1, dir_svg, web_svg)
}
\concept{viz}
