\name{validate_bonds}
\alias{validate_bonds}
\title{Validate Bond Reference Data}
\usage{validate_bonds(bonds)}
\arguments{\item{bonds}{A data frame with columns \code{bond_id}, \code{issue_date}, \code{maturity}, \code{coupon_rate}, and \code{coupon_frequency}.}}
\value{Invisibly returns \code{TRUE} if validation succeeds.}
\description{Checks the structure and basic values of a bond reference data set.}
