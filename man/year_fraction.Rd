\name{year_fraction}
\alias{year_fraction}
\title{Year Fraction}
\usage{year_fraction(start_date, end_date, day_count = "ACT/365.25")}
\arguments{
\item{start_date}{A \code{Date} vector of start dates.}
\item{end_date}{A \code{Date} vector of end dates.}
\item{day_count}{Day-count convention. Currently \code{"ACT/365.25"} and \code{"ACT/365"} are supported.}
}
\value{A numeric vector of year fractions.}
\description{Computes year fractions between two dates using a simple day-count convention. The default is ACT/365.25, matching the package's current preliminary time-to-maturity convention.}
\examples{
year_fraction(as.Date("2026-01-01"), as.Date("2027-01-01"))
}
