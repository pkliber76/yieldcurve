\name{dl_loadings}
\alias{dl_loadings}
\title{Diebold-Li Factor Loadings}
\usage{dl_loadings(maturity, lambda)}
\arguments{
\item{maturity}{Numeric vector of maturities in years.}
\item{lambda}{Positive fixed decay parameter.}
}
\value{A numeric matrix with columns \code{level}, \code{slope}, and \code{curvature}.}
\description{Computes the level, slope, and curvature loadings used by the dynamic Nelson-Siegel model.}
