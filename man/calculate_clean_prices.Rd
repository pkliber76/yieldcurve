\name{calculate_clean_prices}
\alias{calculate_clean_prices}
\title{Calculate Clean Prices from Dirty Prices}
\usage{
calculate_clean_prices(
  bonds,
  dirty_prices,
  face_value = 100,
  day_count = "ACT/ACT"
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}

\item{dirty_prices}{A wide dirty-price data frame. The first column must be \code{date}; each remaining column is one bond. Missing values mean that the bond was not quoted on that date.}

\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}

\item{day_count}{Day-count convention for accrued interest. Currently \code{"ACT/ACT"}, \code{"ACT/365.25"}, and \code{"ACT/365"} are supported. \code{"ACT/ACT"} uses the actual number of days in the coupon period.}
}
\value{
A wide clean-price data frame with the same date rows and bond columns as the
dirty-price panel.
}
\description{
Calculates clean prices from quoted dirty prices by subtracting accrued
interest. Accrued interest is calculated from each bond's coupon schedule using
the issue date, maturity date, coupon rate, and coupon frequency.
}
\examples{
bonds <- data.frame(
  bond_id = "B1",
  issue_date = as.Date("2026-01-01"),
  maturity = as.Date("2027-01-01"),
  coupon_rate = 0.04,
  coupon_frequency = 2L
)
dirty_prices <- data.frame(
  date = as.Date("2026-04-01"),
  B1 = 100
)
calculate_clean_prices(bonds, dirty_prices)
}
