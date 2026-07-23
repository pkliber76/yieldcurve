\name{fit_nss_prices}
\alias{fit_nss_prices}
\title{Fit Nelson-Siegel-Svensson Model from Clean Prices}
\usage{
fit_nss_prices(
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
\item{quotes}{A wide clean-price data frame accepted by \code{validate_quotes()}.}
\item{start}{Optional named numeric vector with starting values for \code{beta0}, \code{beta1}, \code{beta2}, \code{beta3}, \code{lambda1}, and \code{lambda2}.}
\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}
\item{accrued_day_count}{Day-count convention for accrued interest passed to \code{calculate_dirty_prices()}.}
\item{discount_day_count}{Day-count convention for discounting cash flows.}
\item{compounding}{Compounding convention. Currently \code{"annual"} and \code{"continuous"} are supported.}
\item{weighting}{Objective weighting scheme. Use \code{"none"} for the ordinary sum of squared dirty-price errors or \code{"inverse_duration"} for weights proportional to the inverse bond duration within each quote date.}
\item{progress}{Logical. If \code{TRUE}, show a text progress bar while dates are estimated.}
}
\value{An S3 object of class \code{"yc_nss_price"}.}
\description{Estimates Nelson-Siegel-Svensson parameters directly from quoted clean prices. The objective minimizes dirty-price errors after converting clean prices to dirty prices and pricing each bond from NSS zero rates applied to its remaining cash flows. For direct fitting to precomputed dirty prices, use \code{fit_nss_dirty_prices()}. Each quote date needs at least six bond price observations.}
