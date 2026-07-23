\name{plot.yc_nss_price}
\alias{plot.yc_nss_price}
\title{Plot Price-Based Nelson-Siegel-Svensson Results}
\usage{
\method{plot}{yc_nss_price}(x, date = NULL, type = c("term_structure"), observed_yields = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{"yc_nss_price"}.}
\item{date}{Quote date to plot. If omitted, the last available date is used.}
\item{type}{Plot type. Currently only \code{"term_structure"} is supported.}
\item{observed_yields}{Optional data frame with columns \code{date}, \code{bond_id}, \code{time_to_maturity}, and \code{ytm}.}
\item{...}{Additional arguments passed to \code{plot_nss_term_structure()}.}
}
\value{Invisibly returns the curve data frame returned by \code{plot_nss_term_structure()}.}
\description{
Plots a \code{"yc_nss_price"} object returned by \code{fit_nss_dirty_prices()} or
\code{fit_nss_prices()}. This is a generic \code{plot()} method that draws the
estimated Nelson-Siegel-Svensson term structure for one quote date. Observed
yield-to-maturity points can be supplied with \code{observed_yields}.
}
