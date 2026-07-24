\name{plot_dl_factors}
\alias{plot_dl_factors}
\title{Plot Diebold-Li Factors}
\usage{plot_dl_factors(factors, ...)}
\arguments{
\item{factors}{A data frame returned by \code{fit_dl_factors()}.}

\item{...}{Additional named styling arguments such as \code{col}, \code{main}, \code{xlab}, \code{ylab}, \code{ylim}, \code{lwd}, or \code{lty}.}
}
\value{A \code{ggplot} object containing the factor time-series plot. The plot is also printed.}
\description{Plots level, slope, and curvature factor time series.}
