if (getRversion() >= "2.15.1") {
  utils::globalVariables(c("factor", "maturity", "series", "value", "yield"))
}

.onAttach <- function(libname, pkgname) {
  description <- utils::packageDescription(pkgname, lib.loc = libname)
  package <- description[["Package"]]
  version <- description[["Version"]]
  creation_date <- description[["Date"]]
  author <- description[["Maintainer"]]
  package_description <- description[["Description"]]

  if (is.null(creation_date) || is.na(creation_date)) {
    creation_date <- "unknown"
  }
  if (is.null(author) || is.na(author)) {
    author <- "unknown"
  }
  if (is.null(package_description) || is.na(package_description)) {
    package_description <- "unknown"
  }

  packageStartupMessage(
    package,
    " ",
    version,
    "\nCreated: ",
    creation_date,
    "\nAuthor: ",
    author,
    "\nDescription: ",
    package_description
  )
}
