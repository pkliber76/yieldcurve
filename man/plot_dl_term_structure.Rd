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
  curve_col = "#2563EB",
  point_col = "#111827",
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

\item{...}{Additional arguments passed to \code{\link[graphics:plot]{graphics::plot()}}.}
}
\value{
Invisibly returns a data frame with the curve values and, as an
attribute named \code{"observed"}, the observed points used in the plot.
}
\description{
Plots the estimated Diebold-Li yield curve for one quote date and overlays
observed bond-level yield-to-maturity values used in factor estimation.
}
