\name{fit_dl_factors}
\alias{fit_dl_factors}
\title{Fit Diebold-Li Factors by Date}
\usage{fit_dl_factors(data, lambda = 0.0609, progress = FALSE)}
\arguments{
\item{data}{A data frame with columns \code{date}, \code{maturity}, and \code{yield}.}
\item{lambda}{Positive fixed decay parameter. Defaults to \code{0.0609}.}
\item{progress}{Logical. If \code{TRUE}, show a text progress bar while factors are estimated.}
}
\value{A data frame with columns \code{date}, \code{level}, \code{slope}, and \code{curvature}.}
\description{For each quotation date, estimates level, slope, and curvature by ordinary least squares using a fixed Nelson-Siegel decay parameter.}
