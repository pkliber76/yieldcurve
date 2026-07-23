\name{plot.yc_ns_price}
\alias{plot.yc_ns_price}
\title{Plot Price-Based Nelson-Siegel Results}
\usage{
\method{plot}{yc_ns_price}(x, date = NULL, type = c("term_structure"), observed_yields = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{"yc_ns_price"}.}
\item{date}{Quote date to plot. If omitted, the last available date is used.}
\item{type}{Plot type. Currently only \code{"term_structure"} is supported.}
\item{observed_yields}{Optional data frame with columns \code{date}, \code{bond_id}, \code{time_to_maturity}, and \code{ytm}.}
\item{...}{Additional arguments passed to \code{plot_ns_term_structure()}.}
}
\value{Invisibly returns the curve data frame returned by \code{plot_ns_term_structure()}.}
\description{
Plots a \code{"yc_ns_price"} object returned by \code{fit_ns_dirty_prices()} or
\code{fit_ns_prices()}. This is a generic \code{plot()} method that draws the
estimated Nelson-Siegel term structure for one quote date. Observed
yield-to-maturity points can be supplied with \code{observed_yields}.
}
