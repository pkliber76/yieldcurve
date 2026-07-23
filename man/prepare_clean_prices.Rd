\name{prepare_clean_prices}
\alias{prepare_clean_prices}
\title{Prepare Clean Price Panel from Wide Data}
\usage{
prepare_clean_prices(prices, bonds = NULL, keep_empty_dates = FALSE)
}
\arguments{
\item{prices}{A data frame with column \code{date} and one numeric clean-price column per bond.}

\item{bonds}{Optional bond reference data frame used to validate bond identifiers and remove observations outside the bond life.}

\item{keep_empty_dates}{Logical. If \code{TRUE}, retain dates with no non-missing valid prices. Defaults to \code{FALSE}.}
}
\value{
A wide clean-price data frame accepted by \code{\link[=validate_quotes]{validate_quotes()}}.
}
\description{
Validates and cleans a wide clean-price data frame. The input must contain a
\code{date} column and one column per bond, where
each bond column name is the bond identifier and values are clean prices.
Missing values are treated as non-quoted bonds.
}
\details{
If \code{bonds} is supplied, bond columns are checked against \code{bonds$bond_id} and
observations outside each bond's life are set to \code{NA}: quote dates before
\code{issue_date} and on or after \code{maturity}.
}
\examples{
prices_wide <- data.frame(
  date = as.Date(c("2027-01-01", "2027-01-02")),
  B1 = c(99.5, 99.6),
  B2 = c(101.2, NA)
)
prepare_clean_prices(prices_wide)
}
