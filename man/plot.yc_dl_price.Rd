\name{plot.yc_dl_price}
\alias{plot.yc_dl_price}
\title{Plot Price-Based Diebold-Li Results}
\usage{
\method{plot}{yc_dl_price}(x, date = NULL, type = c("term_structure"), observed_yields = NULL, ...)
}
\arguments{
\item{x}{An object of class \code{"yc_dl_price"}.}
\item{date}{Quote date to plot. If omitted, the last available date is used.}
\item{type}{Plot type. Currently only \code{"term_structure"} is supported.}
\item{observed_yields}{Optional data frame with columns \code{date}, \code{bond_id}, \code{maturity}, and \code{yield}.}
\item{...}{Additional arguments passed to \code{plot_dl_term_structure()}.}
}
\value{Invisibly returns the curve data frame returned by \code{plot_dl_term_structure()}.}
\description{
Plots a \code{"yc_dl_price"} object returned by the price-based Diebold-Li
fitting routine. Currently this draws the estimated Diebold-Li term structure
for one quote date.
}
