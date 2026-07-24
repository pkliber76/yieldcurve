\name{plot.yc_dl_estimate}
\alias{plot.yc_dl_estimate}
\title{Plot Diebold-Li Estimation Results}
\usage{
\method{plot}{yc_dl_estimate}(
  x,
  date = NULL,
  type = c("factors", "term_structure"),
  ...
)
}
\arguments{
\item{x}{An object of class \code{"yc_dl_estimate"}.}

\item{date}{Quote date used when \code{type = "term_structure"}. If omitted, the last available date is used.}

\item{type}{Plot type. \code{"factors"} plots the factor time series; \code{"term_structure"} plots the yield curve for one date.}

\item{...}{Additional arguments passed to \code{\link[=plot_dl_factors]{plot_dl_factors()}} or \code{\link[=plot_dl_term_structure]{plot_dl_term_structure()}}.}
}
\value{
A \code{ggplot} object containing the requested Diebold-Li plot. The plot is also printed.
}
\description{
Plots results from \code{\link[=estimate_dl_from_quotes]{estimate_dl_from_quotes()}}. By default it plots the
level, slope, and curvature factor time series. Use \code{type = "term_structure"}
to plot the estimated Diebold-Li curve for a specific date together with the
observed bond yield-to-maturity points used in factor estimation.
}
\examples{
\dontrun{
dl_estimate <- estimate_dl_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

plot(dl_estimate)
plot(dl_estimate, type = "term_structure", date = as.Date("2027-01-01"))
}
}
