\name{fit_ns_prices}
\alias{fit_ns_prices}
\title{Fit Nelson-Siegel Model from Clean Prices}
\usage{
fit_ns_prices(
  bonds,
  quotes,
  start = NULL,
  face_value = 100,
  accrued_day_count = "ACT/ACT",
  discount_day_count = "ACT/365.25",
  compounding = "annual",
  weighting = c("none", "inverse_duration"),
  progress = FALSE
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}

\item{quotes}{A wide clean-price data frame accepted by \code{\link[=validate_quotes]{validate_quotes()}}.}

\item{start}{Optional named numeric vector with starting values for \code{beta0}, \code{beta1}, \code{beta2}, and \code{lambda}.}

\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}

\item{accrued_day_count}{Day-count convention for accrued interest passed to \code{\link[=calculate_dirty_prices]{calculate_dirty_prices()}}.}

\item{discount_day_count}{Day-count convention for discounting cash flows.}

\item{compounding}{Compounding convention. Currently \code{"annual"} and \code{"continuous"} are supported.}

\item{weighting}{Objective weighting scheme. Use \code{"none"} for the ordinary sum of squared dirty-price errors or \code{"inverse_duration"} for weights proportional to the inverse bond duration within each quote date.}

\item{progress}{Logical. If \code{TRUE}, show a text progress bar while dates are estimated.}
}
\value{
An S3 object of class \code{"yc_ns_price"}.
}
\description{
Estimates Nelson-Siegel parameters directly from quoted clean prices. The
function first converts clean prices to dirty prices, then minimizes
dirty-price residuals using cash-flow present values under the NS zero curve.
For direct fitting to precomputed dirty prices, use
\code{\link[=fit_ns_dirty_prices]{fit_ns_dirty_prices()}}.
}
