\name{validate_inputs}
\alias{validate_inputs}
\title{Validate Bond and Quote Inputs Together}
\usage{validate_inputs(bonds, quotes)}
\arguments{
\item{bonds}{A bond reference data frame.}
\item{quotes}{A quote object accepted by \code{validate_quotes()}.}
}
\value{Invisibly returns \code{TRUE} if validation succeeds.}
\description{Checks \code{bonds} and \code{quotes} individually and verifies that all quoted bond identifiers exist in the bond reference data.}
