\name{plot_dl_term_structure}
\alias{plot_dl_term_structure}
\title{Plot Diebold-Li Term Structure}
\usage{
plot_dl_term_structure(
  x,
  date = NULL,
  lambda = 0.0609,
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
\item{x}{An object of class \code{"yc_dl_estimate"} or a data frame returned by \code{\link[=fit_dl_factors]{fit_dl_factors()}}.}

\item{date}{Quote date to plot. If omitted, the last available date is used.}

\item{lambda}{Positive fixed Diebold-Li decay parameter. Required when \code{x} is a factor data frame unless the default \code{0.0609} should be used.}

\item{observed_yields}{Optional data frame with columns \code{date}, \code{bond_id}, \code{maturity}, and \code{yield}. This is useful when plotting a factor data frame directly.}

\item{maturity_grid}{Optional numeric vector of maturities in years used to draw the smooth DL curve.}

\item{n_grid}{Number of grid points used when \code{maturity_grid} is \code{NULL}.}

\item{curve_col}{Color for the fitted DL term structure.}

\item{point_col}{Color for observed bond points.}

\item{lwd}{Line width for the fitted DL curve.}

\item{pch}{Plotting character for observed bond points.}

\item{...}{Additional named styling arguments such as \code{xlab}, \code{ylab}, \code{main}, \code{ylim}, \code{col}, or \code{lwd}.}
}
\value{
A \code{ggplot} object containing the term-structure plot. The curve data frame is stored in attribute \code{"curve"} and observed points in attribute \code{"observed"}. The plot is also printed.
}
\description{
Plots the estimated Diebold-Li yield curve for one quote date and overlays
observed bond-level yield-to-maturity values used in factor estimation.
}
