\name{plot_ns_term_structure}
\alias{plot_ns_term_structure}
\title{Plot Price-Based NS Term Structure}
\usage{
plot_ns_term_structure(
  x,
  date = NULL,
  observed_yields = NULL,
  maturity_grid = NULL,
  n_grid = 200,
  curve_col = yc_plot_palette()$curve,
  point_col = yc_plot_palette()$point,
  lwd = 2,
  pch = 21,
  ...
)
}
\arguments{
\item{x}{An object of class \code{"yc_ns_estimate"} or \code{"yc_ns_price"}.}

\item{date}{Quote date to plot. If omitted, the last available date is used.}

\item{observed_yields}{Optional data frame with columns \code{date}, \code{bond_id}, \code{time_to_maturity}, and \code{ytm}. This is useful when plotting a \code{"yc_ns_price"} object directly.}

\item{maturity_grid}{Optional numeric vector of maturities in years used to draw the smooth NS curve.}

\item{n_grid}{Number of grid points used when \code{maturity_grid} is \code{NULL}.}

\item{curve_col}{Color for the fitted NS term structure.}

\item{point_col}{Color for observed bond points.}

\item{lwd}{Line width for the fitted NS curve.}

\item{pch}{Plotting character for observed bond points.}

\item{...}{Additional named styling arguments such as \code{xlab}, \code{ylab}, \code{main}, \code{ylim}, \code{col}, or \code{lwd}.}
}
\value{
A \code{ggplot} object containing the term-structure plot. The curve data frame is stored in attribute \code{"curve"} and observed points in attribute \code{"observed"}. The plot is also printed.
}
\description{
Plots the estimated Nelson-Siegel zero-rate term structure for one quote
date and overlays observed bond-level values used in the estimation. When
\code{x} is the object returned by \code{\link[=estimate_ns_from_quotes]{estimate_ns_from_quotes()}}, observed points
are taken from its yield-to-maturity diagnostics. When \code{x} is a
\code{"yc_ns_price"} object, pass a compatible \code{observed_yields} data frame if
observed points should be displayed.
}
