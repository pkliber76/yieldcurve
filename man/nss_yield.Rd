\name{nss_yield}
\alias{nss_yield}
\title{Nelson-Siegel-Svensson Yield Curve}
\usage{nss_yield(maturity, beta0, beta1, beta2, beta3, lambda1, lambda2)}
\arguments{
\item{maturity}{Numeric vector of maturities in years.}
\item{beta0}{Level parameter.}
\item{beta1}{Slope parameter.}
\item{beta2}{First curvature parameter.}
\item{beta3}{Second curvature parameter.}
\item{lambda1}{Positive first decay parameter.}
\item{lambda2}{Positive second decay parameter.}
}
\value{A numeric vector of fitted yields.}
\description{Evaluates the Nelson-Siegel-Svensson yield curve at one or more maturities.}
