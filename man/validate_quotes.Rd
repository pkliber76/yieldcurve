\name{validate_quotes}
\alias{validate_quotes}
\title{Validate Bond Quote Data}
\usage{validate_quotes(quotes)}
\arguments{\item{quotes}{A wide clean-price data frame.}}
\value{Invisibly returns \code{TRUE} if validation succeeds.}
\description{Checks the structure and basic values of clean-price quote data. The canonical structure is a wide price panel: a data frame whose first column is \code{date} and whose remaining columns are bond identifiers. Missing values represent bonds that were not quoted on a given date.}
