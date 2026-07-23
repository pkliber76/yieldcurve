\name{estimate_dl_from_quotes}
\alias{estimate_dl_from_quotes}
\title{Estimate Diebold-Li Factors from Clean Price Quotes}
\usage{
estimate_dl_from_quotes(
  bonds,
  quotes,
  lambda = 0.0609,
  face_value = 100,
  accrued_day_count = "ACT/ACT",
  discount_day_count = "ACT/365.25",
  compounding = "annual",
  yields = NULL,
  weighting = c("none", "inverse_duration"),
  progress = FALSE
)
}
\arguments{
\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}

\item{quotes}{A wide clean-price data frame accepted by \code{\link[=validate_quotes]{validate_quotes()}}.}

\item{lambda}{Positive fixed Diebold-Li decay parameter. Defaults to \code{0.0609}.}

\item{face_value}{Numeric face value. Use a single value for all bonds or one value per row of \code{bonds}. Defaults to \code{100}.}

\item{accrued_day_count}{Day-count convention for accrued interest passed to \code{\link[=calculate_dirty_prices]{calculate_dirty_prices()}}.}

\item{discount_day_count}{Day-count convention for discounting remaining cash flows.}

\item{compounding}{Compounding convention. Currently \code{"annual"} and \code{"continuous"} are supported.}

\item{yields}{Optional data frame previously returned by \code{\link[=calculate_yields]{calculate_yields()}} or \code{\link[=calculate_ytm]{calculate_ytm()}}. If supplied, these values are reused instead of being calculated again.}

\item{weighting}{Objective weighting scheme. Use \code{"none"} for the ordinary sum of squared dirty-price errors or \code{"inverse_duration"} for weights proportional to the inverse bond duration within each quote date.}

\item{progress}{Logical. If \code{TRUE}, show text progress bars while date-by-date Diebold-Li price estimation and yield-to-maturity diagnostics are running.}
}
\value{
An S3 object of class \code{"yc_dl_estimate"} with elements
\code{dirty_prices}, \code{ytm}, \code{factor_input}, \code{factors},
\code{fitted_values}, \code{residuals}, \code{convergence}, \code{lambda}, and \code{settings}.
}
\description{
Runs a Diebold-Li estimation workflow from the package's bond and quote structures:
validate inputs, convert clean prices to dirty prices, and estimate daily
Diebold-Li level, slope, and curvature factors by minimizing dirty-price
errors using discounted bond cash flows. Yield-to-maturity observations are
still calculated as diagnostics and for plotting observed bond points.
}
\examples{
\dontrun{
dl_estimate <- estimate_dl_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  face_value = face_values
)

dl_estimate_custom <- estimate_dl_from_quotes(
  bonds = bonds_complex,
  quotes = quotes_complex,
  lambda = 0.1,
  face_value = face_values
)
}
}
