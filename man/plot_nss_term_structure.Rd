\name{plot_nss_term_structure}
\alias{plot_nss_term_structure}
\title{Plot Price-Based NSS Term Structure}
\usage{
plot_nss_term_structure(
  x,
  date = NULL,
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
\item{x}{An object of class \code{"yc_nss_estimate"} or \code{"yc_nss_price"}.}

\item{date}{Quote date to plot. If omitted, the last available date is used.}

\item{observed_yields}{Optional data frame with columns \code{date}, \code{bond_id}, \code{time_to_maturity}, and \code{ytm}. This is useful when plotting a \code{"yc_nss_price"} object directly.}

\item{maturity_grid}{Optional numeric vector of maturities in years used to draw the smooth NSS curve.}

\item{n_grid}{Number of grid points used when \code{maturity_grid} is \code{NULL}.}

\item{curve_col}{Color for the fitted NSS term structure.}

\item{point_col}{Color for observed bond points.}

\item{lwd}{Line width for the fitted NSS curve.}

\item{pch}{Plotting character for observed bond points.}

\item{...}{Additional arguments passed to \code{\link[graphics:plot]{graphics::plot()}}.}
}
\value{
Invisibly returns a data frame with the curve values and, as an
attribute named \code{"observed"}, the observed points used in the plot.
}
\description{
Plots the estimated Nelson-Siegel-Svensson zero-rate term structure for one
quote date and overlays observed bond-level values used in the estimation.
When \code{x} is the object returned by \code{\link[=estimate_nss_from_quotes]{estimate_nss_from_quotes()}}, observed
points are taken from its yield-to-maturity diagnostics. When \code{x} is a
\code{"yc_nss_price"} object, pass a compatible \code{observed_yields} data frame if
observed points should be displayed.
}
\examples{
bonds <- data.frame(
  bond_id = paste0("Z", 1:6),
  issue_date = as.Date(rep("2026-01-01", 6)),
  maturity = as.Date(c(
    "2026-07-01", "2027-01-01", "2028-01-01",
    "2030-01-01", "2033-01-01", "2036-01-01"
  )),
  coupon_rate = 0,
  coupon_frequency = 1L
)
time <- year_fraction(as.Date("2026-01-01"), bonds$maturity)
pars <- c(beta0 = 0.035, beta1 = -0.015, beta2 = 0.01,
  beta3 = 0.005, lambda1 = 0.6, lambda2 = 1.8)
rates <- nss_yield(time, pars[1], pars[2], pars[3], pars[4], pars[5], pars[6])
quotes <- data.frame(
  date = as.Date("2026-01-01"),
  stats::setNames(as.list(100 * (1 + rates)^(-time)), bonds$bond_id)
)
fit <- estimate_nss_from_quotes(bonds, quotes, start = pars)
plot_nss_term_structure(fit, as.Date("2026-01-01"))
}
