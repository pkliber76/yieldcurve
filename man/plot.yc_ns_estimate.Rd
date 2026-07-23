\name{plot.yc_ns_estimate}
\alias{plot.yc_ns_estimate}
\title{Plot Nelson-Siegel Estimation Results}
\usage{
\method{plot}{yc_ns_estimate}(
  x,
  date = NULL,
  type = c("term_structure"),
  ...
)
}
\arguments{
\item{x}{An object of class \code{"yc_ns_estimate"}.}

\item{date}{Quote date to plot. If omitted, the last available date is used.}

\item{type}{Plot type. Currently only \code{"term_structure"} is supported.}

\item{...}{Additional arguments passed to \code{\link[=plot_ns_term_structure]{plot_ns_term_structure()}}.}
}
\value{
Invisibly returns the curve data frame returned by
\code{\link[=plot_ns_term_structure]{plot_ns_term_structure()}}.
}
\description{
Plots results from \code{\link[=estimate_ns_from_quotes]{estimate_ns_from_quotes()}}. Currently this draws the
estimated Nelson-Siegel term structure for one quote date together with the
observed bond yield-to-maturity points used in estimation.
}
\examples{
\dontrun{
ns_estimate <- estimate_ns_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

plot(ns_estimate, date = as.Date("2027-01-01"))
}
}
