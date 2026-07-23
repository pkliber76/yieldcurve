\name{estimate_nss_from_quotes}
\alias{estimate_nss_from_quotes}
\title{Estimate NSS Curve from Clean Price Quotes}
\usage{
estimate_nss_from_quotes(
  bonds,
  quotes,
  start = NULL,
  face_value = 100,
  accrued_day_count = "ACT/ACT",
  discount_day_count = "ACT/365.25",
  compounding = "annual",
  include_ytm = TRUE,
  yields = NULL,
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
\item{include_ytm}{Logical. If \code{TRUE}, include yield-to-maturity diagnostics in the returned object.}
\item{yields}{Optional data frame previously returned by \code{calculate_yields()} or \code{calculate_ytm()}. If supplied and \code{include_ytm = TRUE}, these values are reused instead of being calculated again.}
\item{weighting}{Objective weighting scheme. Use \code{"none"} for the ordinary sum of squared dirty-price errors or \code{"inverse_duration"} for weights proportional to the inverse bond duration within each quote date.}
\item{progress}{Logical. If \code{TRUE}, show text progress bars while date-by-date NSS price estimation and optional yield-to-maturity diagnostics are running.}
}
\value{An S3 object of class \code{"yc_nss_estimate"} with elements \code{dirty_prices}, \code{fit}, and optionally \code{ytm}.}
\description{Runs the standard price-based NSS estimation: validate inputs, convert clean prices to dirty prices, optionally calculate yield-to-maturity diagnostics, and fit NSS parameters directly to dirty prices.}
