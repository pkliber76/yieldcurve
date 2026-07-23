\name{generate_cash_flows}
\alias{generate_cash_flows}
\title{Generate Bond Cash Flows}
\usage{generate_cash_flows(bonds, face_value = 100, settlement_date = NULL)}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}
\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}
\item{settlement_date}{Optional \code{Date} scalar. If supplied, cash flows on or before this date are removed.}
}
\value{A data frame with columns \code{bond_id}, \code{cash_flow_date}, \code{period_number}, \code{cash_flow_type}, \code{coupon_amount}, \code{principal_amount}, and \code{cash_flow}.}
\description{Creates contractual coupon and principal cash flows from bond reference data. Coupon dates are generated on a regular schedule anchored at maturity and filtered to dates after each bond's \code{issue_date}. If \code{settlement_date} is supplied, only cash flows after that date are returned.}
\examples{
bonds <- data.frame(
  bond_id = c("GOV_2Y", "CORP_5Y", "MUNI_7Y", "ZERO_3Y"),
  issue_date = as.Date(c(
    "2026-01-15",
    "2025-06-30",
    "2024-03-31",
    "2026-07-01"
  )),
  maturity = as.Date(c(
    "2028-01-15",
    "2030-06-30",
    "2031-03-31",
    "2029-07-01"
  )),
  coupon_rate = c(0.035, 0.0525, 0.041, 0),
  coupon_frequency = c(2L, 4L, 1L, 1L)
)

face_values <- c(100, 1000, 5000, 100)

all_cash_flows <- generate_cash_flows(
  bonds = bonds,
  face_value = face_values
)
all_cash_flows

future_cash_flows <- generate_cash_flows(
  bonds = bonds,
  face_value = face_values,
  settlement_date = as.Date("2027-01-01")
)
future_cash_flows

aggregate(cash_flow ~ bond_id, data = future_cash_flows, FUN = sum)

future_cash_flows[
  future_cash_flows$cash_flow_type \%in\% c("principal", "coupon_principal"),
]
}
