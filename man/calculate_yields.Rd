\name{calculate_yields}
\alias{calculate_yields}
\title{Calculate Bond Yields from Clean Prices}
\usage{
calculate_yields(
  bonds,
  quotes,
  face_value = 100,
  accrued_day_count = "ACT/ACT",
  discount_day_count = "ACT/365.25",
  compounding = "annual",
  progress = FALSE
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}
\item{quotes}{A wide clean-price data frame accepted by \code{validate_quotes()}.}
\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}
\item{accrued_day_count}{Day-count convention for accrued interest passed to \code{calculate_dirty_prices()}.}
\item{discount_day_count}{Day-count convention for discounting remaining cash flows. Currently \code{"ACT/365.25"} and \code{"ACT/365"} are supported.}
\item{compounding}{Compounding convention. Currently \code{"annual"} and \code{"continuous"} are supported.}
\item{progress}{Logical. If \code{TRUE}, show a text progress bar while yields are calculated.}
}
\value{A data frame containing dirty-price data plus \code{ytm}.}
\description{
Calculates bond-level yield-to-maturity values from quoted clean prices. This
is a convenience wrapper around \code{calculate_ytm()} intended for workflows
where the same yields are reused across several estimation functions.
}
\examples{
bonds <- data.frame(
  bond_id = "B1",
  issue_date = as.Date("2026-01-01"),
  maturity = as.Date("2028-01-01"),
  coupon_rate = 0.04,
  coupon_frequency = 2L
)
quotes <- data.frame(date = as.Date("2026-04-01"), B1 = 99)
yields <- calculate_yields(bonds, quotes)
}
