\name{calculate_accrued_interest}
\alias{calculate_accrued_interest}
\title{Calculate Accrued Interest Panel}
\usage{
calculate_accrued_interest(
  bonds,
  prices,
  face_value = 100,
  day_count = "ACT/ACT"
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}

\item{prices}{A wide price panel with first column \code{date}. The function uses the dates and bond columns from this panel.}

\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}

\item{day_count}{Day-count convention for accrued interest. Currently \code{"ACT/ACT"}, \code{"ACT/365.25"}, and \code{"ACT/365"} are supported.}
}
\value{
A wide data frame with first column \code{date} and accrued-interest columns for each bond.
}
\description{
Calculates accrued interest for every date and bond in a clean-price panel.
Values are returned in the same wide layout as prices. Cells outside a bond's
life are \code{NA}.
}
