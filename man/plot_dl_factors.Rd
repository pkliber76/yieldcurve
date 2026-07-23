\name{plot_dl_factors}
\alias{plot_dl_factors}
\title{Plot Diebold-Li Factors}
\usage{plot_dl_factors(factors, ...)}
\arguments{
\item{factors}{A data frame returned by \code{fit_dl_factors()}.}

\item{...}{Additional graphical arguments passed to \code{\link[graphics:matplot]{graphics::matplot()}}, such as \code{col}, \code{main}, \code{xlab}, \code{ylab}, \code{lwd}, or \code{lty}.}
}
\value{Invisibly returns \code{factors}.}
\description{Plots level, slope, and curvature factor time series.}
