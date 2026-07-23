\name{validate_price_panel}
\alias{validate_price_panel}
\title{Validate Wide Price Panel}
\usage{
validate_price_panel(prices, bonds = NULL, price_type = "price")
}
\arguments{
\item{prices}{A data frame with first column \code{date} and one numeric column per bond.}

\item{bonds}{Optional bond reference data frame. If supplied, price column names are checked against \code{bonds$bond_id}.}

\item{price_type}{Label used in error messages, for example \code{"clean"} or \code{"dirty"}.}
}
\value{
Invisibly returns \code{TRUE} if validation succeeds.
}
\description{
Checks a wide price panel. The first column must be \code{date}; each remaining
column must contain prices for one bond. Missing values are allowed and mean
that the bond was not quoted on that date.
}
