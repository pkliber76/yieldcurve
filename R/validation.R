#' Validate Bond Reference Data
#'
#' Checks the structure and basic values of a bond reference data set.
#'
#' @param bonds A data frame with columns `bond_id`, `issue_date`, `maturity`,
#'   `coupon_rate`, and `coupon_frequency`.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @export
validate_bonds <- function(bonds) {
  if (!is.data.frame(bonds)) {
    stop("bonds must be a data.frame.", call. = FALSE)
  }

  check_required_columns(
    bonds,
    c("bond_id", "issue_date", "maturity", "coupon_rate", "coupon_frequency"),
    "bonds"
  )

  if (!is.character(bonds$bond_id)) {
    stop("bonds$bond_id must be character.", call. = FALSE)
  }
  if (any(is.na(bonds$bond_id)) || any(bonds$bond_id == "")) {
    stop("bonds$bond_id must not contain missing or empty values.", call. = FALSE)
  }
  if (anyDuplicated(bonds$bond_id)) {
    stop("bonds$bond_id must be unique.", call. = FALSE)
  }
  if (!inherits(bonds$issue_date, "Date")) {
    stop("bonds$issue_date must be a Date column.", call. = FALSE)
  }
  if (any(is.na(bonds$issue_date))) {
    stop("bonds$issue_date must not contain missing values.", call. = FALSE)
  }
  if (!inherits(bonds$maturity, "Date")) {
    stop("bonds$maturity must be a Date column.", call. = FALSE)
  }
  if (any(is.na(bonds$maturity))) {
    stop("bonds$maturity must not contain missing values.", call. = FALSE)
  }
  if (any(bonds$issue_date >= bonds$maturity)) {
    stop("bonds$issue_date must be earlier than bonds$maturity.", call. = FALSE)
  }

  check_numeric_vector(bonds$coupon_rate, "bonds$coupon_rate", nonnegative = TRUE)

  if (!is.integer(bonds$coupon_frequency) && !is.numeric(bonds$coupon_frequency)) {
    stop("bonds$coupon_frequency must be integer-like.", call. = FALSE)
  }
  if (any(!is.finite(bonds$coupon_frequency)) ||
      any(bonds$coupon_frequency <= 0) ||
      any(bonds$coupon_frequency != floor(bonds$coupon_frequency))) {
    stop("bonds$coupon_frequency must contain positive integer values.", call. = FALSE)
  }

  invisible(TRUE)
}

#' Validate Bond Quote Data
#'
#' Checks the structure and basic values of clean-price quote data. The
#' canonical structure is a wide price panel: a data frame whose first column is
#' `date` and whose remaining columns are bond identifiers. Missing values
#' represent bonds that were not quoted on a given date.
#'
#' @param quotes A wide clean-price data frame.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @export
validate_quotes <- function(quotes) {
  validate_price_panel(quotes, price_type = "clean")
  invisible(TRUE)
}

#' Validate Wide Price Panel
#'
#' Checks a wide price panel. The first column must be `date`; each remaining
#' column must contain prices for one bond. Missing values are allowed and mean
#' that the bond was not quoted on that date.
#'
#' @param prices A data frame with first column `date` and one numeric column
#'   per bond.
#' @param bonds Optional bond reference data frame. If supplied, price column
#'   names are checked against `bonds$bond_id`.
#' @param price_type Label used in error messages, for example `"clean"` or
#'   `"dirty"`.
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @export
validate_price_panel <- function(prices, bonds = NULL, price_type = "price") {
  if (!is.data.frame(prices)) {
    stop("prices must be a data.frame.", call. = FALSE)
  }
  if (!identical(names(prices)[1], "date")) {
    stop("The first column of prices must be named date.", call. = FALSE)
  }
  if (!inherits(prices$date, "Date")) {
    stop("prices$date must be a Date column.", call. = FALSE)
  }
  if (any(is.na(prices$date))) {
    stop("prices$date must not contain missing values.", call. = FALSE)
  }
  if (anyDuplicated(prices$date)) {
    stop("prices$date must be unique.", call. = FALSE)
  }

  bond_columns <- setdiff(names(prices), "date")
  if (length(bond_columns) == 0) {
    stop("prices must contain at least one bond price column.", call. = FALSE)
  }
  if (any(is.na(bond_columns)) || any(bond_columns == "")) {
    stop("Bond price column names must not be missing or empty.", call. = FALSE)
  }
  if (anyDuplicated(bond_columns)) {
    stop("Bond price column names must be unique.", call. = FALSE)
  }

  for (bond_column in bond_columns) {
    if (!is.numeric(prices[[bond_column]])) {
      stop("prices$", bond_column, " must be numeric.", call. = FALSE)
    }
    observed <- !is.na(prices[[bond_column]])
    if (any(!is.finite(prices[[bond_column]][observed])) ||
        any(prices[[bond_column]][observed] <= 0)) {
      stop(
        "prices$",
        bond_column,
        " must contain positive finite ",
        price_type,
        " prices or NA.",
        call. = FALSE
      )
    }
  }

  if (!is.null(bonds)) {
    validate_bonds(bonds)
    unknown_ids <- setdiff(bond_columns, bonds$bond_id)
    if (length(unknown_ids) > 0) {
      stop(
        "prices contains bond column(s) not present in bonds: ",
        paste(unknown_ids, collapse = ", "),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}

#' Convert Wide Quote Panel to a Long Data Frame
#'
#' Converts a wide clean-price panel to a long data frame with columns `date`,
#' `bond_id`, and `clean_price`.
#'
#' @param quotes A quote object accepted by [validate_quotes()].
#'
#' @return A data frame with columns `date`, `bond_id`, and `clean_price`.
#' @export
as_quote_data_frame <- function(quotes) {
  validate_quotes(quotes)
  price_panel_to_long(quotes, "clean_price")
}

#' @rdname as_quote_data_frame
#' @export
quote_to_data_frame <- function(quotes) {
  as_quote_data_frame(quotes)
}

#' Reorder Wide Price Panel
#'
#' Validates and optionally reorders a wide price panel. The first column is
#' `date`; each following column is one bond identifier and contains prices for
#' that bond. Missing bond-date observations are returned as `NA`.
#'
#' @param prices A wide price data frame.
#' @param price_column Ignored. Kept only to avoid breaking calls that pass the
#'   argument by name.
#' @param bond_id Optional character vector defining the order of bond columns.
#'   If `NULL`, the existing column order is retained.
#'
#' @return A wide data frame with column `date` followed by one column per bond.
#' @export
#'
#' @examples
#' clean_prices <- data.frame(
#'   date = as.Date(c("2027-01-01", "2027-01-02")),
#'   B1 = c(99, 99.2),
#'   B2 = c(101, NA)
#' )
#' prices_to_wide(clean_prices)
prices_to_wide <- function(prices, price_column = NULL, bond_id = NULL) {
  validate_price_panel(prices)
  if (!is.null(bond_id)) {
    if (!is.character(bond_id) || any(is.na(bond_id)) || any(bond_id == "")) {
      stop("bond_id must be a character vector without missing or empty values.", call. = FALSE)
    }
    if (anyDuplicated(bond_id)) {
      stop("bond_id must not contain duplicates.", call. = FALSE)
    }
    missing_ids <- setdiff(bond_id, names(prices))
    for (id in missing_ids) {
      prices[[id]] <- NA_real_
    }
    prices <- prices[, c("date", bond_id), drop = FALSE]
  }
  prices
}

#' Prepare Clean Price Panel from Wide Data
#'
#' Validates and cleans a wide clean-price data frame. The input must contain a
#' `date` column and one column per bond, where each bond column name is the
#' bond identifier and values are clean prices. Missing values are treated as
#' non-quoted bonds.
#'
#' If `bonds` is supplied, bond columns are checked against `bonds$bond_id` and
#' observations outside each bond's life are set to `NA`: quote dates before
#' `issue_date` and on or after `maturity`.
#'
#' @param prices A data frame with column `date` and one numeric clean-price
#'   column per bond.
#' @param bonds Optional bond reference data frame used to validate bond
#'   identifiers and remove observations outside the bond life.
#' @param keep_empty_dates Logical. If `TRUE`, retain dates with no non-missing
#'   valid prices. Defaults to `FALSE`.
#'
#' @return A wide clean-price data frame accepted by [validate_quotes()].
#' @export
#'
#' @examples
#' prices_wide <- data.frame(
#'   date = as.Date(c("2027-01-01", "2027-01-02")),
#'   B1 = c(99.5, 99.6),
#'   B2 = c(101.2, NA)
#' )
#' prepare_clean_prices(prices_wide)
prepare_clean_prices <- function(prices, bonds = NULL, keep_empty_dates = FALSE) {
  validate_price_panel(prices, bonds = bonds, price_type = "clean")
  prices <- prices[order(prices$date), , drop = FALSE]
  bond_columns <- setdiff(names(prices), "date")

  if (!is.null(bonds)) {
    for (id in bond_columns) {
      bond <- bonds[match(id, bonds$bond_id), , drop = FALSE]
      in_life <- prices$date >= bond$issue_date & prices$date < bond$maturity
      prices[[id]][!in_life] <- NA_real_
    }
  }

  if (!isTRUE(keep_empty_dates)) {
    has_price <- rowSums(!is.na(prices[, bond_columns, drop = FALSE])) > 0
    prices <- prices[has_price, , drop = FALSE]
  }
  if (nrow(prices) == 0 || all(is.na(as.matrix(prices[, bond_columns, drop = FALSE])))) {
    stop("No non-missing valid clean prices are available.", call. = FALSE)
  }

  rownames(prices) <- NULL
  validate_price_panel(prices, bonds = bonds, price_type = "clean")
  prices
}

validate_quote_data_frame <- function(quotes_df) {
  if (!is.data.frame(quotes_df)) {
    stop("quotes_df must be a data.frame.", call. = FALSE)
  }
  check_required_columns(quotes_df, c("date", "bond_id", "clean_price"), "quotes_df")
  invisible(TRUE)
}

#' Validate Bond and Quote Inputs Together
#'
#' Checks `bonds` and `quotes` individually and verifies that all quoted bond
#' identifiers exist in the bond reference data.
#'
#' @param bonds A bond reference data frame.
#' @param quotes A quote object accepted by [validate_quotes()].
#'
#' @return Invisibly returns `TRUE` if validation succeeds.
#' @export
validate_inputs <- function(bonds, quotes) {
  validate_bonds(bonds)
  validate_quotes(quotes)
  quotes_df <- as_quote_data_frame(quotes)

  unknown_ids <- setdiff(unique(quotes_df$bond_id), bonds$bond_id)
  if (length(unknown_ids) > 0) {
    stop(
      "quotes contains bond_id value(s) not present in bonds: ",
      paste(unknown_ids, collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

price_panel_to_long <- function(prices, value_name) {
  bond_columns <- setdiff(names(prices), "date")
  rows <- lapply(bond_columns, function(id) {
    data.frame(
      date = prices$date,
      bond_id = id,
      value = prices[[id]],
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[!is.na(out$value), , drop = FALSE]
  names(out)[names(out) == "value"] <- value_name
  out <- out[order(out$date, out$bond_id), , drop = FALSE]
  rownames(out) <- NULL
  out
}
