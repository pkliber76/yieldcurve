\name{prepare_bond_data}
\alias{prepare_bond_data}
\title{Prepare Bond and Quote Data}
\usage{prepare_bond_data(bonds, quotes)}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}
\item{quotes}{A wide clean-price data frame accepted by \code{\link[=validate_quotes]{validate_quotes()}}.}
}
\value{A data frame containing quote and bond columns plus \code{time_to_maturity}, with expired observations removed.}
\description{Joins quote observations to bond reference data, computes time to maturity in years, and removes expired bonds. This function intentionally does not compute yields or price-based quantities yet.}
\examples{
bonds <- data.frame(
  bond_id = "B1",
  issue_date = as.Date("2020-01-01"),
  maturity = as.Date("2030-01-01"),
  coupon_rate = 0.05,
  coupon_frequency = 2L
)
quotes <- data.frame(
  date = as.Date("2026-01-01"),
  B1 = 101
)
prepare_bond_data(bonds, quotes)
}
