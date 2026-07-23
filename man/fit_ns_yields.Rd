\name{fit_ns_yields}
\alias{fit_ns_yields}
\title{Fit Nelson-Siegel Model to Observed Yields}
\usage{fit_ns_yields(maturity, yield, start = NULL)}
\arguments{
\item{maturity}{Numeric vector of maturities in years. Values must be strictly positive for fitting.}
\item{yield}{Numeric vector of observed yields expressed as decimals.}
\item{start}{Optional named numeric vector with starting values for \code{beta0}, \code{beta1}, \code{beta2}, and \code{lambda}.}
}
\value{An S3 object of class \code{"yc_ns"}.}
\description{Estimates Nelson-Siegel parameters from observed yields using \code{stats::optim()} with the \code{"L-BFGS-B"} method.}
