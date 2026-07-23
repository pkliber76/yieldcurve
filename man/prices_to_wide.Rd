\name{prices_to_wide}
\alias{prices_to_wide}
\title{Reorder Wide Price Panel}
\usage{
prices_to_wide(prices, price_column = NULL, bond_id = NULL)
}
\arguments{
\item{prices}{A wide price data frame.}

\item{price_column}{Ignored. Kept only to avoid breaking calls that pass the argument by name.}

\item{bond_id}{Optional character vector defining the order of bond columns. If \code{NULL}, the existing column order is retained.}
}
\value{
A wide data frame with column \code{date} followed by one column per bond.
}
\description{
Validates and optionally reorders a wide price panel. The first column is
\code{date}; each following column is one bond identifier and contains prices
for that bond. Missing bond-date observations are returned as \code{NA}.
}
\examples{
clean_prices <- data.frame(
  date = as.Date(c("2027-01-01", "2027-01-02")),
  B1 = c(99, 99.2),
  B2 = c(101, NA)
)
prices_to_wide(clean_prices)
}
