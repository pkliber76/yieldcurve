\name{plot.yc_nss_estimate}
\alias{plot.yc_nss_estimate}
\title{Plot Nelson-Siegel-Svensson Estimation Results}
\usage{
\method{plot}{yc_nss_estimate}(
  x,
  date = NULL,
  type = c("term_structure"),
  ...
)
}
\arguments{
\item{x}{An object of class \code{"yc_nss_estimate"}.}

\item{date}{Quote date to plot. If omitted, the last available date is used.}

\item{type}{Plot type. Currently only \code{"term_structure"} is supported.}

\item{...}{Additional arguments passed to \code{\link[=plot_nss_term_structure]{plot_nss_term_structure()}}.}
}
\value{
A \code{ggplot} object returned by \code{\link[=plot_nss_term_structure]{plot_nss_term_structure()}}. The plot is also printed.
}
\description{
Plots results from \code{\link[=estimate_nss_from_quotes]{estimate_nss_from_quotes()}}. Currently this draws the
estimated Nelson-Siegel-Svensson term structure for one quote date together
with the observed bond yield-to-maturity points used in estimation.
}
\examples{
\dontrun{
nss_estimate <- estimate_nss_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

plot(nss_estimate, date = as.Date("2027-01-01"))
}
}
