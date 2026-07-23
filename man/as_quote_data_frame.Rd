\name{as_quote_data_frame}
\alias{as_quote_data_frame}
\alias{quote_to_data_frame}
\title{Convert Wide Quote Panel to a Long Data Frame}
\usage{
as_quote_data_frame(quotes)

quote_to_data_frame(quotes)
}
\arguments{
\item{quotes}{A quote object accepted by \code{validate_quotes()}.}
}
\value{A data frame with columns \code{date}, \code{bond_id}, and \code{clean_price}.}
\description{Converts a wide clean-price panel to a long data frame with columns \code{date}, \code{bond_id}, and \code{clean_price}.}
