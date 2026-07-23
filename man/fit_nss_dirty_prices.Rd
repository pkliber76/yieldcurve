\name{fit_nss_dirty_prices}
\alias{fit_nss_dirty_prices}
\title{Fit Nelson-Siegel-Svensson Model from Dirty Prices}
\usage{
fit_nss_dirty_prices(
  bonds,
  dirty_prices,
  start = NULL,
  face_value = 100,
  discount_day_count = "ACT/365.25",
  compounding = "annual",
  weighting = c("none", "inverse_duration"),
  progress = FALSE
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}
\item{dirty_prices}{A wide dirty-price data frame. The first column must be \code{date}; each remaining column is one bond.}
\item{start}{Optional named numeric vector with starting values for \code{beta0}, \code{beta1}, \code{beta2}, \code{beta3}, \code{lambda1}, and \code{lambda2}.}
\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}
\item{discount_day_count}{Day-count convention for discounting cash flows.}
\item{compounding}{Compounding convention. Currently \code{"annual"} and \code{"continuous"} are supported.}
\item{weighting}{Objective weighting scheme. Use \code{"none"} for the ordinary sum of squared dirty-price errors or \code{"inverse_duration"} for weights proportional to the inverse bond duration within each quote date.}
\item{progress}{Logical. If \code{TRUE}, show a text progress bar while dates are estimated.}
}
\value{An S3 object of class \code{"yc_nss_price"}.}
\description{Estimates Nelson-Siegel-Svensson parameters directly from dirty prices. For each quote date, theoretical prices are computed as discounted contractual cash flows. Discount rates are NSS zero rates evaluated at each cash-flow maturity. The objective minimizes the sum of squared dirty-price errors.}
\details{Dates are fitted sequentially. The first date uses \code{start} or a default derived from same-day yield-to-maturity values; each next date uses the fitted NSS coefficients from the previous date as its starting values. For robustness, the optimizer also checks the same-day yield-based start and keeps the solution with the lower price objective.}
