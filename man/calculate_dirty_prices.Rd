\name{calculate_dirty_prices}
\alias{calculate_dirty_prices}
\title{Calculate Dirty Prices from Clean Prices}
\usage{
calculate_dirty_prices(
  bonds,
  quotes,
  face_value = 100,
  day_count = "ACT/ACT"
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}
\item{quotes}{A wide clean-price data frame. The first column must be \code{date}; each remaining column is one bond. Missing values mean that the bond was not quoted on that date.}
\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}
\item{day_count}{Day-count convention for accrued interest. Currently \code{"ACT/ACT"}, \code{"ACT/365.25"}, and \code{"ACT/365"} are supported. \code{"ACT/ACT"} uses the actual number of days in the coupon period.}
}
\value{A wide dirty-price data frame with the same date rows and bond columns as the clean-price panel.}
\description{Calculates dirty prices from quoted clean prices by adding accrued interest. Accrued interest is calculated from each bond's coupon schedule using the issue date, maturity date, coupon rate, and coupon frequency.}
\examples{
bonds <- data.frame(
  bond_id = c("B1", "B2"),
  issue_date = as.Date(c("2026-01-01", "2026-01-01")),
  maturity = as.Date(c("2028-01-01", "2031-01-01")),
  coupon_rate = c(0.04, 0.055),
  coupon_frequency = c(2L, 2L)
)

quotes <- data.frame(
  date = as.Date("2026-07-01"),
  B1 = 100.25,
  B2 = 98.75
)

calculate_dirty_prices(
  bonds = bonds,
  quotes = quotes,
  face_value = c(100, 100)
)
}
