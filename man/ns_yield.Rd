\name{ns_yield}
\alias{ns_yield}
\title{Nelson-Siegel Yield Curve}
\usage{ns_yield(maturity, beta0, beta1, beta2, lambda)}
\arguments{
\item{maturity}{Numeric vector of maturities in years.}
\item{beta0}{Level parameter.}
\item{beta1}{Slope parameter.}
\item{beta2}{Curvature parameter.}
\item{lambda}{Positive decay parameter.}
}
\value{A numeric vector of fitted yields.}
\description{Evaluates the Nelson-Siegel yield curve at one or more maturities.}
