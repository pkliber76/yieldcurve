\name{fit_nss_yields}
\alias{fit_nss_yields}
\title{Fit Nelson-Siegel-Svensson Model to Observed Yields}
\usage{fit_nss_yields(maturity, yield, start = NULL)}
\arguments{
\item{maturity}{Numeric vector of maturities in years. Values must be strictly positive for fitting.}
\item{yield}{Numeric vector of observed yields expressed as decimals.}
\item{start}{Optional named numeric vector with starting values for \code{beta0}, \code{beta1}, \code{beta2}, \code{beta3}, \code{lambda1}, and \code{lambda2}.}
}
\value{An S3 object of class \code{"yc_nss"}.}
\description{Estimates Nelson-Siegel-Svensson parameters from observed yields using \code{stats::optim()} with the \code{"L-BFGS-B"} method.}
