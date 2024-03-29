% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/viz.R
\name{plot_ss_class_var}
\alias{plot_ss_class_var}
\title{Plot Seascape Class Variable}
\usage{
plot_ss_class_var(
  var,
  val,
  n_bins = NULL,
  ply_color = "black",
  ply_alpha = 0.5,
  ln_color = "red",
  ln_size = 5,
  tbl_classes = ss_gl_classes
)
}
\arguments{
\item{var}{variable to plot, for getting distribution of all Class averages from \code{\link{ss_gl_classes}}}

\item{val}{average value to plot as a vertical line}

\item{n_bins}{number of bins for generating histogram. If NULL (default), then perform a density histogram curve.}

\item{ply_color}{color of polygon representing distribution of all average values for \code{var}; default = \code{"black"}}

\item{ply_alpha}{color of polygon representing distribution of all average values for \code{var}; default = \code{0.5}}

\item{ln_color}{color of vertical line; default = \code{"red"}}

\item{ln_size}{width of vertical line; default = \code{5}}

\item{tbl_classes}{table of values from which to extract \code{var}; default = \code{\link{ss_gl_classes}}}
}
\description{
Plot the provided value relative to all average values for the
given variable across Seascape Classes.
}
\concept{viz}
